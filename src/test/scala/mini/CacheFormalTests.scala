package mini

import junctions._
import chisel3._
import chiseltest._
import chiseltest.formal._
import org.scalatest.freespec.AnyFreeSpec


class CacheFormalTests extends AnyFreeSpec with ChiselScalatestTester with Formal {
  val p = MiniConfig()
  "check cache against nasti properties" in {
    verify(new CacheTestBench(new Cache(p.cache, p.nasti, p.core.xlen)), Seq(BoundedCheck(4), BtormcEngineAnnotation))
  }
}


class CacheTestBench(makeCache: => Cache) extends Module {
  val dut = Module(makeCache)
  val io = IO(chiselTypeOf(dut.io))
  dut.io <> io
  NastiMonitor(dut.io.nasti)
}


object NastiMonitor {
  def apply(io: NastiBundle): NastiMonitor = {
    val monitor = Module(new NastiMonitor(chiselTypeOf(io)))
    monitor.io := io
    monitor
  }
}

class NastiMonitor(bundleType: NastiBundle) extends Module {
  val io = IO(Input(bundleType))

  // properties from:
  // https://raw.githubusercontent.com/ZipCPU/wb2axip/master/doc/chexpo-2021.pdf

  val channels = Seq(io.r, io.w, io.ar, io.aw, io.b)

  // Handshaking
  // 1. Reset clears all requests
  // TODO
  // 2. If a channel is stalled, nothing should change
  channels.foreach { ch =>
    when(past(ch.valid && !ch.ready)) {
      assert(ch.valid, s"valid was revoked on channel: $ch")
      assert(stable(ch.bits), s"bits changed while channel was stalled: $ch")
    }
  }

}