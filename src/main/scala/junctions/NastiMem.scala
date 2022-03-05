// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package junctions

import chisel3._
import chisel3.util._

class NastiMem(address: AddressSet, nastiParams: NastiBundleParameters) extends Module {
  val io = IO(new NastiBundle(nastiParams))
  val laneDataBits = 8
  val beatBytes = nastiParams.dataBits / laneDataBits

  private def bigBits(x: BigInt, tail: List[Boolean] = Nil): List[Boolean] =
    if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)

  val mask = bigBits(address.mask >> log2Ceil(beatBytes))
  val mem = SyncReadMem(
    BigInt(1) << mask.count(b => b),
    Vec(beatBytes, UInt(laneDataBits.W))
  )

  val r_addr = Cat((mask.zip((io.ar.bits.addr >> log2Ceil(beatBytes)).asBools)).filter(_._1).map(_._2).reverse)
  val w_addr = Cat((mask.zip((io.aw.bits.addr >> log2Ceil(beatBytes)).asBools)).filter(_._1).map(_._2).reverse)
  val r_sel0 = address.contains(io.ar.bits.addr)
  val w_sel0 = address.contains(io.aw.bits.addr)

  val w_full = RegInit(false.B)
  val w_id = Reg(UInt(nastiParams.idBits.W))
  val r_sel1 = Reg(r_sel0)
  val w_sel1 = Reg(w_sel0)

  when(io.b.fire) { w_full := false.B }
  when(io.aw.fire) { w_full := true.B }

  when(io.aw.fire) {
    w_id := io.aw.bits.id
    w_sel1 := w_sel0
  }

  val wdata = VecInit.tabulate(beatBytes) { i => io.w.bits.data(8 * (i + 1) - 1, 8 * i) }
  when(io.aw.fire && w_sel0) {
    mem.write(w_addr, wdata, io.w.bits.strb.asBools)
  }

  io.b.valid := w_full
  io.aw.ready := io.w.valid && (io.b.ready || !w_full)
  io.w.ready := io.aw.valid && (io.b.ready || !w_full)

  io.b.bits.id := w_id
  io.b.bits.resp := Mux(w_sel1, NastiConstants.RespOkay, NastiConstants.RespDecErr)

  val r_full = RegInit(false.B)
  val r_id = Reg(UInt(nastiParams.idBits.W))

  when(io.r.fire) { r_full := false.B }
  when(io.ar.fire) { r_full := true.B }

  when(io.ar.fire) {
    r_id := io.ar.bits.id
    r_sel1 := r_sel0
  }

  val ren = io.ar.fire
  val rdata = readAndHold(mem, r_addr, ren)

  io.r.valid := r_full
  io.ar.ready := io.r.ready || !r_full

  io.r.bits.id := r_id
  io.r.bits.resp := Mux(r_sel1, NastiConstants.RespOkay, NastiConstants.RespDecErr)
  io.r.bits.data := Cat(rdata.reverse)
  io.r.bits.last := true.B
}

private object readAndHold {
  def apply[D <: Data](mem: SyncReadMem[D], addr: UInt, ren: Bool): D = {
    val data = mem.read(addr, ren)
    val enable = RegNext(ren)
    Mux(enable, data, RegEnable(data, enable))
  }
}
