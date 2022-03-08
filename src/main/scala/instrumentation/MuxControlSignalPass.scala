// Copyright 2017-2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Jack Koenig <koenig@sifive.com>, Kevin Laeufer <laeufer@cs.berkeley.edu>,

package instrumentation

import firrtl._
import firrtl.annotations._
import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.transforms.DontTouchAnnotation

import scala.collection.mutable

/** Tags a module that should not have any coverage added.
  *  This annotation should be respected by all automated coverage passes.
  */
case class DoNotCoverAnnotation(target: ModuleTarget) extends SingleTargetAnnotation[ModuleTarget] {
  override def duplicate(n: ModuleTarget) = copy(target = n)
}

// exposes mux control signals in the VCD
// based on ideas in: https://people.eecs.berkeley.edu/~laeufer/papers/rfuzz_kevin_laeufer_iccad2018.pdf
object MuxControlSignalPass extends Transform with DependencyAPIMigration {
  val TraceSuffix = "_trace"

  override def prerequisites = Seq(
    Dependency[firrtl.transforms.RemoveWires],
    Dependency(passes.ExpandWhens),
    Dependency(passes.LowerTypes),
    Dependency(KeepClockAndResetPass),
  )
  override def invalidates(a: Transform) = false

  val Optimizations = Seq(
    Dependency[firrtl.transforms.ConstantPropagation],
    Dependency(passes.CommonSubexpressionElimination),
    Dependency[firrtl.transforms.DeadCodeElimination]
  )

  // need to run _before_ optimizations, because otherwise, the whole circuit might be dead code eliminated
  override def optionalPrerequisiteOf = Forms.LowEmitters ++ Optimizations
  override def optionalPrerequisites = Seq()

  override def execute(state: CircuitState): CircuitState = {
    val c = CircuitTarget(state.circuit.main)

    val newAnnos = mutable.ListBuffer[Annotation]()
    val circuit = state.circuit.mapModule(onModule(_, collectModulesToIgnore(state), c, newAnnos))
    //println(circuit.serialize)
    state.copy(circuit = circuit, annotations = newAnnos.toList ++: state.annotations)
  }

  private val AddCover = true

  private def onModule(
    m:         ir.DefModule,
    ignoreSet: Set[String],
    c:         CircuitTarget,
    newAnnos:  mutable.ListBuffer[Annotation]
  ): ir.DefModule = m match {
    case mod: ir.Module if !ignoreSet.contains(mod.name) =>
      val mTarget = c.module(mod.name)
      val namespace = Namespace(mod)
      val conds = findMuxConditions(mod)
      // we create nodes for all mux conditions because that will allow us to have them identified through the
      // trace suffix
      var count = 0
      lazy val clock = Builder.findClock(mod)
      lazy val reset = Builder.findReset(mod)
      val nodes = conds.map { cond =>
        val prefix = cond match {
          case ir.Reference(name, _, _, _) => removeLeadingUnderscore(name)
          case _ =>
            count += 1
            s"mux_cond_$count"
        }
        val name = namespace.newName(prefix + TraceSuffix)
        newAnnos.append(DontTouchAnnotation(mTarget.ref(name)))
        val node = ir.DefNode(ir.NoInfo, name, cond)
        if(!AddCover) { node } else {
          val nodeRef = ir.Reference(node)
          val pos = ir.Verification(ir.Formal.Cover, ir.NoInfo, clock, nodeRef, Utils.not(reset), ir.StringLit(""), namespace.newName("cover_" + node.name))
          val neg = ir.Verification(ir.Formal.Cover, ir.NoInfo, clock, Utils.not(nodeRef), Utils.not(reset), ir.StringLit(""), namespace.newName("cover_not_" + node.name))
          ir.Block(node, pos, neg)
        }
      }
      mod.copy(body = ir.Block(mod.body +: nodes))
    case other => other
  }

  private def removeLeadingUnderscore(name: String): String =
    if (name.startsWith("_")) { removeLeadingUnderscore(name.drop(1)) }
    else { name }

  private def collectModulesToIgnore(state: CircuitState): Set[String] = {
    val main = state.circuit.main
    state.annotations.collect { case DoNotCoverAnnotation(target) if target.circuit == main => target.module }.toSet
  }

  private case class ModuleCtx(
    m:         ModuleTarget,
    namespace: Namespace,
    newAnnos:  mutable.ListBuffer[Annotation])

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
