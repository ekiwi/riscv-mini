// Copyright 2017-2022 The Regents of the University of California
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

// adds a cover statement for every possible mux control signal
// based on ideas in: https://people.eecs.berkeley.edu/~laeufer/papers/rfuzz_kevin_laeufer_iccad2018.pdf
object MuxControlSignalPass extends Transform with DependencyAPIMigration {
  override def prerequisites = Forms.HighForm
  override def invalidates(a: Transform) = false
  override def optionalPrerequisiteOf = Forms.LowEmitters
  override def optionalPrerequisites = Seq()

  override def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit.mapModule(onModule(_, collectModulesToIgnore(state)))
    state.copy(circuit = circuit)
  }

  private val Prefix = "mux"

  private def onModule(
    m:         ir.DefModule,
    ignoreSet: Set[String],
  ): ir.DefModule = m match {
    case mod: ir.Module if !ignoreSet.contains(mod.name) =>
      val namespace = Namespace(mod)
      namespace.newName(Prefix)
      val conds = findMuxConditions(mod)

      lazy val clock = Builder.findClock(mod)
      val covers = conds.map { cond =>
        ir.Verification(
          ir.Formal.Cover,
          ir.NoInfo,
          clock,
          cond,
          Utils.True(),
          ir.StringLit(""),
          namespace.newName(Prefix)
        )
      }
      mod.copy(body = ir.Block(mod.body +: covers))
    case other => other
  }

  private def collectModulesToIgnore(state: CircuitState): Set[String] = {
    val main = state.circuit.main
    state.annotations.collect { case DoNotCoverAnnotation(target) if target.circuit == main => target.module }.toSet
  }

  // returns a list of unique (at least structurally unique!) mux conditions used in the module
  private def findMuxConditions(m: ir.Module): List[ir.Expression] = {
    val conds = mutable.LinkedHashMap[String, ir.Expression]()

    def onCond(cond: ir.Expression): Unit = {
      val key = cond.serialize
      conds(key) = cond
    }
    def onStmt(s: ir.Statement): Unit = {
      s.foreachStmt(onStmt)
      s.foreachExpr(onExpr)
      s match {
        case ir.Conditionally(_, pred, _, _) => onCond(pred)
        case _ =>
      }
    }
    def onExpr(e: ir.Expression): Unit = {
      e.foreachExpr(onExpr)
      e match {
        case ir.Mux(cond, _, _, _) => onCond(cond)
        case ir.ValidIf(cond, _, _) => onCond(cond)
        case _ =>
      }
    }
    onStmt(m.body)
    conds.values.toList
  }
}
