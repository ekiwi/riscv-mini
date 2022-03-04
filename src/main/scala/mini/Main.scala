// See LICENSE for license details.

package mini

import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.options.{Dependency, TargetDirAnnotation}
import firrtl.passes.memlib.VerilogMemDelays
import firrtl.stage.RunFirrtlTransformAnnotation
import instrumentation.{InitialStateValuePass, MuxControlSignalPass}

object Main extends App {
  val targetDirectory = args.head
  val config = MiniConfig()
  new chisel3.stage.ChiselStage().execute(
    Array("-ll", "info"),
    Seq(
      ChiselGeneratorAnnotation(() =>
        new Tile(
          coreParams = config.core,
          nastiParams = config.nasti,
          cacheParams = config.cache
        )
      ),
      TargetDirAnnotation(targetDirectory),
      RunFirrtlTransformAnnotation(Dependency(MuxControlSignalPass)),
      RunFirrtlTransformAnnotation(Dependency(InitialStateValuePass)),
      // we have to schedule this pass explicitly in order to make sure that the initial state value pass can run _after_
      RunFirrtlTransformAnnotation(Dependency(VerilogMemDelays))
    )
  )
}
