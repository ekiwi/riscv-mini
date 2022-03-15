package instrumentation

import firrtl._
import firrtl.analyses.InstanceKeyGraph
import firrtl.analyses.InstanceKeyGraph.InstanceKey
import firrtl.annotations.TargetToken.{Clock, Index, Init, Instance, OfModule, Ref, Reset}
import firrtl.annotations._
import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.passes.wiring
import firrtl.transforms.DontTouchAnnotation

import scala.collection.mutable

/** Specifies the module which contains all the signals we are tracking. */
case class SignalTrackerAnnotation(target: ModuleTarget) extends SingleTargetAnnotation[ModuleTarget] {
  override def duplicate(n: ModuleTarget): SignalTrackerAnnotation = SignalTrackerAnnotation(n)
}

/** Adds a signal tracker module to the top-level module and wires all mux toggle signals to this module. */
object ExposeSignalsOfInterestPass extends Transform with DependencyAPIMigration {
  val SignalTrackerDefaultName = "SignalTracker"
  val SignalTrackerDefaultInstanceName = "tracker"

  override def prerequisites = Seq(
    Dependency[firrtl.transforms.RemoveWires],
    Dependency(passes.ExpandWhens),
    Dependency(passes.LowerTypes)
  )
  override def invalidates(a: Transform): Boolean = a match {
    // we are going to emit annotations for the wiring transform
    case _: wiring.WiringTransform => true
    case _ => false
  }

  val Optimizations = Seq(
    Dependency[firrtl.transforms.ConstantPropagation],
    Dependency(passes.CommonSubexpressionElimination),
    Dependency[firrtl.transforms.DeadCodeElimination]
  )

  // need to run _before_ optimizations, because otherwise, the whole circuit might be dead code eliminated
  override def optionalPrerequisiteOf = Forms.LowEmitters ++ Optimizations
  override def optionalPrerequisites = Seq()

  override def execute(state: CircuitState): CircuitState = {
    // identify the mux toggle signals in each module
    val modulesAndSignals = state.circuit.modules.map(getMuxConditionSignals)
    val signals = modulesAndSignals.map { case (mod, signals) => mod.name -> signals }
    val modules = modulesAndSignals.map(_._1)
    val circuit1 = state.circuit.copy(modules = modules)

    // visit all modules and expose the mux toggle signals
    val newAnnos = mutable.ListBuffer[Annotation]()
    val iGraph = InstanceKeyGraph(state.circuit)
    val childInstances = iGraph.getChildInstances.toMap
    val c = CircuitTarget(state.circuit.main)
    val (signalSources, _) = wireSignals(childInstances, signals.toMap, newAnnos, c.module(state.circuit.main))

    // create a signal tracker module
    val circuit2 = createSignalTracker(circuit1, signalSources, newAnnos)

    // println(circuit2.serialize)

    state.copy(circuit = circuit2, annotations = newAnnos.toList ++: state.annotations)
  }

  private def createSignalTracker(
    circuit:       ir.Circuit,
    signalSources: Seq[(String, Int)],
    annos:         mutable.ListBuffer[Annotation]
  ): ir.Circuit = {
    // create module
    val moduleNames = Namespace(circuit.modules.map(_.name))
    val signalTrackerName = moduleNames.newName(SignalTrackerDefaultName)
    val ports = Seq(
      ir.Port(ir.NoInfo, "clock", ir.Input, ir.ClockType),
      ir.Port(ir.NoInfo, "reset", ir.Input, Utils.BoolType)
    ) ++ signalSources.map(s => ir.Port(ir.NoInfo, s._1, ir.Input, Utils.BoolType))

    // mark all ports as dont touch in order to preserve them through optimizations
    val m = CircuitTarget(circuit.main).module(signalTrackerName)
    ports.map(p => DontTouchAnnotation(m.ref(p.name))).foreach(annos.append(_))

    val trackerModule = ir.Module(ir.NoInfo, signalTrackerName, ports, ir.EmptyStmt)

    // create instance in main module
    val (mainModDef, otherModules) = circuit.modules.partition(_.name == circuit.main)
    val mainMod = mainModDef.head.asInstanceOf[ir.Module]
    val mainNames = Namespace(mainMod)
    val instanceName = mainNames.newName(SignalTrackerDefaultInstanceName)
    val instanceTpe = ir.BundleType(ports.map(p => ir.Field(p.name, Utils.to_flip(p.direction), p.tpe)))
    val instanceRef = ir.Reference(instanceName, instanceTpe, InstanceKind, SourceFlow)
    val mainM = CircuitTarget(circuit.main).module(circuit.main)
    val instanceStmts = Seq(
      ir.DefInstance(ir.NoInfo, instanceName, signalTrackerName, instanceTpe),
      // connect clock and reset
      ir.Connect(
        ir.NoInfo,
        ir.SubField(instanceRef, "clock", ir.ClockType, SinkFlow),
        ir.Reference("clock", ir.ClockType, PortKind, SourceFlow)
      ),
      ir.Connect(
        ir.NoInfo,
        ir.SubField(instanceRef, "reset", Utils.BoolType, SinkFlow),
        ir.Reference("reset", Utils.BoolType, PortKind, SourceFlow)
      )
    ) ++
      // create wires for each signal input
      signalSources.flatMap {
        case (name, cc) =>
          val wireName = mainNames.newName(name)
          val target = mainM.ref(wireName)
          annos.append(wiring.SinkAnnotation(target.toNamed, s"$WiringPrefix$cc"))
          Seq(
            ir.DefWire(ir.NoInfo, wireName, Utils.BoolType),
            ir.Connect(
              ir.NoInfo,
              ir.SubField(instanceRef, name, Utils.BoolType, SinkFlow),
              ir.Reference(wireName, Utils.BoolType, WireKind, SourceFlow)
            )
          )
      }
    val newMain = mainMod.copy(body = ir.Block(mainMod.body +: instanceStmts))

    circuit.copy(modules = newMain +: trackerModule +: otherModules)
  }

  private val WiringPrefix = "ESOIP_"

  private def wireSignals(
    childInstances: Map[String, Seq[InstanceKey]],
    signals:        Map[String, Seq[String]],
    annos:          mutable.ListBuffer[Annotation],
    target:         IsModule,
    count:          Int = 0
  ): (Seq[(String, Int)], Int) = {
    var cc = count
    val localSignals = signals.getOrElse(target.leafModule, Seq()).map { signal =>
      val id = cc; cc += 1
      // val targ = target.ref(signal)
      // turns out we can only use local (not full path) references
      val targ = CircuitTarget(target.circuit).module(target.leafModule).ref(signal)
      val source = wiring.SourceAnnotation(targ.toNamed, s"$WiringPrefix$id")
      annos.append(source)
      val fullName = targetToName(targ).replace('.', '_')
      fullName -> id
    }
    val children = childInstances(target.leafModule)
    val childSignals = children.flatMap {
      case InstanceKey(name, module) =>
        val (sigs, newCC) = wireSignals(childInstances, signals, annos, target.instOf(name, module), cc)
        cc = newCC
        sigs
    }
    (localSignals ++ childSignals, cc)
  }

  private def targetToName(target: ReferenceTarget): String = {
    val moduleString = target.moduleOpt.getOrElse("")
    val tokensString = target.tokens.map {
      case Ref(r)               => s".$r"
      case Instance(i)          => s".$i"
      case TargetToken.Field(f) => s".$f"
      case Index(v)             => s"[$v]"
      case _                    => ""
    }.mkString("")
    if (moduleString.isEmpty) {
      tokensString
    } else {
      moduleString + "." + tokensString
    }
  }

  private def getMuxConditionSignals(m: ir.DefModule): (ir.DefModule, Seq[String]) = m match {
    case mod: ir.Module =>
      val conds = findMuxConditions(mod)
      lazy val namespace = Namespace(mod)
      val namesAndNodes = conds.map {
        case ir.Reference(name, _, _, _) => name -> None
        case other                       =>
          // create a reference
          val node = ir.DefNode(ir.NoInfo, namespace.newName("mux_cond"), other)
          node.name -> Some(node)

      }
      val nodes = namesAndNodes.flatMap(_._2)
      val modWithNodes = mod.copy(body = ir.Block(mod.body +: nodes))
      (modWithNodes, namesAndNodes.map(_._1))
    case other => (other, Seq())
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
