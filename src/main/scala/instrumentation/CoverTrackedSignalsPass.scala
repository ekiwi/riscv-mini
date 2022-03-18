package instrumentation

import firrtl._
import firrtl.options.Dependency

object CoverTrackedSignalsPass extends Transform with DependencyAPIMigration {
  override def prerequisites = Seq(
    Dependency(NewExposeSignalsOfInterestPass)
  )
  override def invalidates(a: Transform): Boolean = false

  override def execute(state: CircuitState): CircuitState = {
    val trackerName = state.annotations.collectFirst { case SignalTrackerAnnotation(t) => t }.get.module
    val (tracker, otherModules) = state.circuit.modules.partition(_.name == trackerName)
    val newTracker = addCovers(tracker.head.asInstanceOf[ir.Module])
    val circuit = state.circuit.copy(modules = newTracker +: otherModules)
    state.copy(circuit = circuit)
  }

  private def addCovers(m: ir.Module): ir.DefModule = {
    val clock = Builder.findClock(m)
    val reset = Builder.findReset(m)
    val namespace = Namespace(m)

    val signals = m.ports
      .filterNot(p => p.name == "clock" || p.name == "reset")
      .map(p => ir.Reference(p.name, p.tpe, PortKind, SourceFlow))
    val stmts = signals.map { signal =>
      val pos = ir.Verification(
        ir.Formal.Cover,
        ir.NoInfo,
        clock,
        signal,
        Utils.not(reset),
        ir.StringLit(""),
        namespace.newName("cover_" + signal.name)
      )
      val neg = ir.Verification(
        ir.Formal.Cover,
        ir.NoInfo,
        clock,
        Utils.not(signal),
        Utils.not(reset),
        ir.StringLit(""),
        namespace.newName("cover_not_" + signal.name)
      )
      ir.Block(pos, neg)
    }

    m.copy(body = ir.Block(m.body +: stmts))
  }
}
