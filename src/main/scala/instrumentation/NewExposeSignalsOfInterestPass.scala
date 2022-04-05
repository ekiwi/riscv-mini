package instrumentation

import firrtl._
import firrtl.analyses.InstanceKeyGraph
import firrtl.annotations.CircuitTarget
import firrtl.options.Dependency
import firrtl.stage.Forms

import scala.collection.mutable

/** Exposes all signals of interest in a top-level module without the use of the wiring pass. */
object NewExposeSignalsOfInterestPass extends Transform with DependencyAPIMigration {
  val SignalTrackerDefaultName = "SignalTracker"
  val SignalTrackerDefaultInstanceName = "tracker"
  /** the delimiter needs to be a valid identifier character. "_" does not work because it breaks the uniquify assumption */
  val Delim = "__"

  override def prerequisites = Seq(
    Dependency[firrtl.transforms.RemoveWires],
    Dependency(passes.ExpandWhens),
    Dependency(passes.LowerTypes)
  )
  override def invalidates(a: Transform): Boolean = false

  val Optimizations = Seq(
    Dependency[firrtl.transforms.ConstantPropagation],
    Dependency(passes.CommonSubexpressionElimination),
    Dependency[firrtl.transforms.DeadCodeElimination]
  )

  // need to run _before_ optimizations, because otherwise, the whole circuit might be dead code eliminated
  override def optionalPrerequisiteOf = Forms.LowEmitters ++ Optimizations
  override def optionalPrerequisites = Seq()

  override def execute(state: CircuitState): CircuitState = {
    // determine the name of the tracker module
    val moduleNames = Namespace(state.circuit.modules.map(_.name))
    val signalTrackerName = moduleNames.newName(SignalTrackerDefaultName)

    // visit modules from bottom to top and wire up the signals of interest
    val iGraph = InstanceKeyGraph(state.circuit)
    val moduleOrderBottomUp = iGraph.moduleOrder.reverseIterator
    val modInfo = mutable.HashMap[String, ModuleInfo]()
    val modules = moduleOrderBottomUp.flatMap { dm =>
      if (dm.name == state.circuit.main) {
        onMainModule(dm.asInstanceOf[ir.Module], modInfo, signalTrackerName)
      } else {
        val (m, info) = onModule(dm, modInfo)
        modInfo(m.name) = info
        Seq(m)
      }
    }

    val circuit = state.circuit.copy(modules = modules.toSeq)

    // TODO: add annotations
    val newAnnos = Seq(
      SignalTrackerAnnotation(CircuitTarget(circuit.main).module(signalTrackerName))
    )

    println(circuit.serialize)

    state.copy(circuit = circuit, annotations = newAnnos ++: state.annotations)
  }

  private def onMainModule(
    m:                 ir.Module,
    getInfo:           String => ModuleInfo,
    signalTrackerName: String
  ): Seq[ir.DefModule] = {
    val namespace = Namespace(m)
    val (body, signals) = gatherSignals(m, getInfo, namespace)

    // create ports for the signal tracker
    val trackerPorts = Seq(
      ir.Port(ir.NoInfo, "clock", ir.Input, ir.ClockType),
      ir.Port(ir.NoInfo, "reset", ir.Input, Utils.BoolType)
    ) ++ signals.map(r => ir.Port(ir.NoInfo, r.serialize.replace(".", Delim), ir.Input, r.tpe))

    // create signal tracker
    val trackerMod = ir.ExtModule(ir.NoInfo, signalTrackerName, trackerPorts, signalTrackerName, Seq())

    // create signal tracker instance
    val trackerInstance = ir.DefInstance(
      ir.NoInfo,
      namespace.newName(SignalTrackerDefaultInstanceName),
      signalTrackerName,
      ir.BundleType(trackerPorts.map(p => ir.Field(p.name, ir.Flip, p.tpe)))
    )
    val trackerRef = ir.Reference(trackerInstance).copy(flow = SourceFlow)
    val trackerCons = trackerPorts.zip(signals).map {
      case (p, r) =>
        ir.Connect(ir.NoInfo, ir.SubField(trackerRef, p.name, p.tpe, SinkFlow), r)
    }
    val mainBody = ir.Block(body +: trackerInstance +: trackerCons)

    Seq(m.copy(body = mainBody), trackerMod)
  }

  private case class ModuleInfo(signals: Seq[(String, ir.GroundType)])

  private def onModule(dm: ir.DefModule, getInfo: String => ModuleInfo): (ir.DefModule, ModuleInfo) = dm match {
    case m: ir.Module =>
      val namespace = Namespace(dm)
      val (body, signals) = gatherSignals(m, getInfo, namespace)

      // create ports
      val refsAndPorts = signals.map { r =>
        val name = namespace.newName(r.serialize.replace(".", Delim))
        r -> ir.Port(ir.NoInfo, name, ir.Output, r.tpe)
      }
      val ports = m.ports ++ refsAndPorts.map(_._2)
      val portCons = refsAndPorts.map {
        case (ref, port) =>
          ir.Connect(ir.NoInfo, ir.Reference(port.name, port.tpe, PortKind, SinkFlow), ref)
      }
      val mod = m.copy(ports = ports, body = ir.Block(body +: portCons))

      val signalInfo = refsAndPorts.map(_._2).map(p => p.name -> p.tpe.asInstanceOf[ir.GroundType])
      (mod, ModuleInfo(signalInfo))

    case other => (other, ModuleInfo(List()))
  }

  private def gatherSignals(
    m:         ir.Module,
    getInfo:   String => ModuleInfo,
    namespace: Namespace
  ): (ir.Statement, Seq[ir.RefLikeExpression]) = {
    // add extra ports to instances
    val instanceSignals = mutable.ListBuffer[ir.RefLikeExpression]()
    val body = patchInstances(getInfo, instanceSignals)(m.body)

    // find local signals of interest
    val localSignals = findMuxConditions(m)
    val (nodes, localSignalRefs) = expressionsToRefs(namespace, "mux", localSignals)

    (ir.Block(body, nodes), instanceSignals.toList ++ localSignalRefs)
  }

  private def patchInstances(
    getInfo:         String => ModuleInfo,
    instanceSignals: mutable.ListBuffer[ir.RefLikeExpression]
  )(stmt:            ir.Statement
  ): ir.Statement = stmt match {
    case ir.DefInstance(info, name, module, oldTpe) =>
      val signals = getInfo(module).signals
      val fields = oldTpe.asInstanceOf[ir.BundleType].fields ++
        signals.map { case (name, tpe) => ir.Field(name, ir.Default, tpe) }
      val tpe = ir.BundleType(fields)
      val ref = ir.Reference(name, tpe, InstanceKind, SourceFlow)
      signals.foreach {
        case (name, tpe) =>
          instanceSignals.append(ir.SubField(ref, name, tpe, SourceFlow))
      }
      ir.DefInstance(info, name, module, tpe)
    case other => other.mapStmt(patchInstances(getInfo, instanceSignals))
  }

  private def expressionsToRefs(
    namespace: Namespace,
    prefix:    String,
    exprs:     Seq[ir.Expression]
  ): (ir.Statement, Seq[ir.RefLikeExpression]) = {
    val refsAndStmts = exprs.map {
      case ref: ir.RefLikeExpression => (ref, None)
      case other =>
        val node = ir.DefNode(ir.NoInfo, namespace.newName(prefix), other)
        (ir.Reference(node.name, node.value.tpe, NodeKind, SourceFlow), Some(node))
    }
    (ir.Block(refsAndStmts.flatMap(_._2)), refsAndStmts.map(_._1))
  }

  // returns a list of unique (at least structurally unique!) mux conditions used in the module
  private def findMuxConditions(m: ir.Module): List[ir.Expression] = {
    val conds = mutable.LinkedHashMap[String, ir.Expression]()

    def onStmt(s: ir.Statement): Unit = s match {
      case ir.Block(stmts) => stmts.foreach(onStmt)
      case other           => other.foreachExpr(onExpr)
    }
    def onExpr(e: ir.Expression): Unit = {
      e.foreachExpr(onExpr)
      e match {
        case ir.Mux(cond, _, _, _) =>
          val key = cond.serialize
          // ignore reset signal
          if (key != "reset") {
            conds(key) = cond
          }
        case _ =>
      }
    }
    onStmt(m.body)
    conds.values.toList
  }
}
