// See LICENSE for license details.

package mini

import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.options.TargetDirAnnotation

object Main extends App {
  val targetDirectory = args.head
  val config = MiniConfig()
  new chisel3.stage.ChiselStage().execute(
    args,
    Seq(
      ChiselGeneratorAnnotation(() => Tile(config)
      ),
      TargetDirAnnotation(targetDirectory)
    )
  )
}
