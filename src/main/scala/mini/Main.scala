// See LICENSE for license details.

package mini

import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.options.{Dependency, TargetDirAnnotation}
import firrtl.passes.memlib.VerilogMemDelays
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.transforms.NoCircuitDedupAnnotation
import instrumentation._

object Main extends App {
  val targetDirectory = args.head
  val config = MiniConfig()
  new chisel3.stage.ChiselStage().execute(
    Array("-ll", "info", "-E", "sverilog"),
    Seq(
      ChiselGeneratorAnnotation(() => new TileAndMemTop(config)),
      TargetDirAnnotation(targetDirectory),
      // we do not want to deduplicate modules because their signals might be covered differently
      NoCircuitDedupAnnotation,
      // expose all mux toggle signals
      RunFirrtlTransformAnnotation(Dependency(ExposeSignalsOfInterestPass)),
      RunFirrtlTransformAnnotation(Dependency(CoverTrackedSignalsPass)),
      // wire up signals
      RunFirrtlTransformAnnotation(Dependency[firrtl.passes.wiring.WiringTransform]),
      // make sure that all memories and registers are initialized to zero
      RunFirrtlTransformAnnotation(Dependency(InitialStateValuePass)),
      // we have to schedule this pass explicitly in order to make sure that the initial state value pass can run _after_
      RunFirrtlTransformAnnotation(Dependency(VerilogMemDelays))
    )
  )
}
