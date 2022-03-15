package instrumentation

import firrtl._
import firrtl.analyses.InstanceKeyGraph
import firrtl.annotations._
import firrtl.options.Dependency
import firrtl.passes.wiring.WiringTransform
import firrtl.stage.Forms

import scala.collection.mutable

/** Specifies the module which contains all the signals we are tracking. */
case class SignalTrackerAnnotation(target: ModuleTarget) extends SingleTargetAnnotation[ModuleTarget] {
  override def duplicate(n: ModuleTarget): SignalTrackerAnnotation = SignalTrackerAnnotation(n)
}

/** Adds a signal tracker module to the top-level module and wires all mux toggle signals to this module. */
object ExposeSignalsOfInterestPass extends Transform with DependencyAPIMigration {
  val SignalTrackerDefaultName = "SignalTracker"

  override def prerequisites = Seq(
    Dependency[firrtl.transforms.RemoveWires],
    Dependency(passes.ExpandWhens),
    Dependency(passes.LowerTypes),
  )
  override def invalidates(a: Transform): Boolean = a match {
    // we are going to emit annotations for the wiring transform
    case _: WiringTransform => true
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
    val signals = modulesAndSignals.map{ case (mod, signals) => mod.name -> signals }
    val modules = modulesAndSignals.map(_._1)

    // visit all modules and expose the mux toggle signals
    val iGraph = InstanceKeyGraph(state.circuit)
    val c = CircuitTarget(state.circuit.main)
    val _ = wireSignals(iGraph, signals.toMap, c.module(state.circuit.main), 0)


    // create a signal tracker module
    val moduleNames = Namespace(state.circuit.modules.map(_.name))



    val newAnnos = mutable.ListBuffer[Annotation]()
    val circuit = state.circuit.mapModule(onModule(_, collectModulesToIgnore(state), c, newAnnos))
    //println(circuit.serialize)
    state.copy(circuit = circuit, annotations = newAnnos.toList ++: state.annotations)
  }

  private def wireSignals(iGraph: InstanceKeyGraph, signals: Map[String, Seq[String]], target: IsModule, count: Int) = {
    val localSignals = signals(target.leafModule)
    val children = iGraph.getChildInstances(target.leafModule)

  }


  private def getMuxConditionSignals(m: ir.DefModule): (ir.DefModule, Seq[String]) = m match {
    case mod: ir.Module =>
      val conds = findMuxConditions(mod)
      lazy val namespace = Namespace(mod)
      val namesAndNodes = conds.map {
        case ir.Reference(name, _, _, _) => name -> None
        case other =>
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
          conds(key) = cond
        case _ =>
      }
    }
    onStmt(m.body)
    conds.values.toList
  }
}
