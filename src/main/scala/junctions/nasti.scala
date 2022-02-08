// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package junctions
import chisel3._
import config._
import util._

object Nasti {
  val XUserBits = 1
  val AWUserBits = XUserBits
  val WUserBits = XUserBits
  val BUserBits = XUserBits
  val ARUserBits = XUserBits
  val RUserBits = XUserBits
  val XLenBits = 8
  val XSizeBits = 3
  val XBurstBits = 2
  val XCacheBits = 4
  val XProtBits = 3
  val XQosBits = 4
  val XRegionBits = 4
  val XRespBits = 2
}

case class NastiParameters(dataBits: Int, addrBits: Int, idBits: Int)


class NastiReadIO(val p: NastiParameters) extends Bundle {
  val ar = Decoupled(new NastiReadAddressChannel(p))
  val r = Flipped(Decoupled(new NastiReadDataChannel(p)))
}

class NastiWriteIO(val p: NastiParameters) extends Bundle {
  val aw = Decoupled(new NastiWriteAddressChannel(p))
  val w = Decoupled(new NastiWriteDataChannel(p))
  val b = Flipped(Decoupled(new NastiWriteResponseChannel(p)))
}

class NastiIO(val p: NastiParameters) extends Bundle {
  val aw = Decoupled(new NastiWriteAddressChannel(p))
  val w = Decoupled(new NastiWriteDataChannel(p))
  val b = Flipped(Decoupled(new NastiWriteResponseChannel(p)))
  val ar = Decoupled(new NastiReadAddressChannel(p))
  val r = Flipped(Decoupled(new NastiReadDataChannel(p)))
}

class NastiResponseChannel extends Bundle {
  val resp = UInt(Nasti.XRespBits.W)
}

class NastiWriteAddressChannel(p: NastiParameters) extends Bundle {
  val data = UInt(p.dataBits.W)
  val last = Bool()
  val id = UInt(p.idBits.W)
  val user = UInt(Nasti.AWUserBits.W)
}

class NastiWriteDataChannel(p: NastiParameters) extends Bundle {
  val id = UInt(p.idBits.W)
  val strb = UInt((p.dataBits / 8).W)
  val user = UInt(Nasti.WUserBits.W)
}

class NastiWriteResponseChannel(p: NastiParameters) extends NastiResponseChannel {
  val id = UInt(p.idBits.W)
  val user = UInt(Nasti.BUserBits.W)
}

class NastiReadAddressChannel(p: NastiParameters) extends Bundle {
  val id = UInt(p.idBits.W)
  val user = UInt(Nasti.ARUserBits.W)
}

class NastiReadDataChannel(p: NastiParameters) extends NastiResponseChannel {
  val id = UInt(p.idBits.W)
  val user = UInt(Nasti.RUserBits.W)
}

object NastiConstants {
  def BURST_FIXED = "b00".U
  def BURST_INCR = "b01".U
  def BURST_WRAP = "b10".U

  def RESP_OKAY = "b00".U
  def RESP_EXOKAY = "b01".U
  def RESP_SLVERR = "b10".U
  def RESP_DECERR = "b11".U

  def CACHE_DEVICE_NOBUF = "b0000".U
  def CACHE_DEVICE_BUF = "b0001".U
  def CACHE_NORMAL_NOCACHE_NOBUF = "b0010".U
  def CACHE_NORMAL_NOCACHE_BUF = "b0011".U

  def AXPROT(instruction: Bool, nonsecure: Bool, privileged: Bool): UInt =
    Cat(instruction, nonsecure, privileged)

  def AXPROT(instruction: Boolean, nonsecure: Boolean, privileged: Boolean): UInt =
    AXPROT(instruction.B, nonsecure.B, privileged.B)
}

import NastiConstants._

object NastiWriteAddressChannel {
  def apply(p: NastiParameters, id: UInt, addr: UInt, size: UInt, len: UInt = 0.U, burst: UInt = BURST_INCR) = {
    val aw = Wire(new NastiWriteAddressChannel(p))
    aw.id := id
    aw.addr := addr
    aw.len := len
    aw.size := size
    aw.burst := burst
    aw.lock := false.B
    aw.cache := CACHE_DEVICE_NOBUF
    aw.prot := AXPROT(false, false, false)
    aw.qos := "b0000".U
    aw.region := "b0000".U
    aw.user := 0.U
    aw
  }
}

object NastiReadAddressChannel {
  def apply(id: UInt, addr: UInt, size: UInt, len: UInt = 0.U, burst: UInt = BURST_INCR)(implicit p: Parameters) = {
    val ar = Wire(new NastiReadAddressChannel)
    ar.id := id
    ar.addr := addr
    ar.len := len
    ar.size := size
    ar.burst := burst
    ar.lock := false.B
    ar.cache := CACHE_DEVICE_NOBUF
    ar.prot := AXPROT(false, false, false)
    ar.qos := 0.U
    ar.region := 0.U
    ar.user := 0.U
    ar
  }
}

object NastiWriteDataChannel {
  def apply(
    p: NastiParameters,
    data: UInt,
    strb: Option[UInt] = None,
    last: Bool = true.B,
    id:   UInt = 0.U
  ): NastiWriteDataChannel = {
    val w = Wire(new NastiWriteDataChannel(p))
    w.strb := strb.getOrElse(Fill((p.dataBits / 8), 1.U))
    w.data := data
    w.last := last
    w.id := id
    w.user := 0.U
    w
  }
}

object NastiReadDataChannel {
  def apply(
    p: NastiParameters,
    id:   UInt,
    data: UInt,
    last: Bool = true.B,
    resp: UInt = 0.U
  )(
    implicit p: Parameters
  ) = {
    val r = Wire(new NastiReadDataChannel)
    r.id := id
    r.data := data
    r.last := last
    r.resp := resp
    r.user := 0.U
    r
  }
}

object NastiWriteResponseChannel {
  def apply(id: UInt, resp: UInt = 0.U)(implicit p: Parameters) = {
    val b = Wire(new NastiWriteResponseChannel)
    b.id := id
    b.resp := resp
    b.user := 0.U
    b
  }
}

class NastiArbiterIO(arbN: Int)(implicit p: Parameters) extends Bundle {
  val master = Flipped(Vec(arbN, new NastiIO))
  val slave = new NastiIO
}

/** Arbitrate among arbN masters requesting to a single slave */
class NastiArbiter(val arbN: Int)(implicit p: Parameters) extends NastiModule {
  val io = new NastiArbiterIO(arbN)

  if (arbN > 1) {
    val arbIdBits = log2Up(arbN)

    val ar_arb = Module(new RRArbiter(new NastiReadAddressChannel, arbN))
    val aw_arb = Module(new RRArbiter(new NastiWriteAddressChannel, arbN))

    val slave_r_arb_id = io.slave.r.bits.id(arbIdBits - 1, 0)
    val slave_b_arb_id = io.slave.b.bits.id(arbIdBits - 1, 0)

    val w_chosen = Reg(UInt(arbIdBits.W))
    val w_done = RegInit(true.B)

    when(aw_arb.io.out.fire) {
      w_chosen := aw_arb.io.chosen
      w_done := false.B
    }

    when(io.slave.w.fire && io.slave.w.bits.last) {
      w_done := true.B
    }

    for (i <- 0 until arbN) {
      val m_ar = io.master(i).ar
      val m_aw = io.master(i).aw
      val m_r = io.master(i).r
      val m_b = io.master(i).b
      val a_ar = ar_arb.io.in(i)
      val a_aw = aw_arb.io.in(i)
      val m_w = io.master(i).w

      a_ar <> m_ar
      a_ar.bits.id := Cat(m_ar.bits.id, i.U(arbIdBits.W))

      a_aw <> m_aw
      a_aw.bits.id := Cat(m_aw.bits.id, i.U(arbIdBits.W))

      m_r.valid := io.slave.r.valid && slave_r_arb_id === i.U
      m_r.bits := io.slave.r.bits
      m_r.bits.id := io.slave.r.bits.id >> arbIdBits.U

      m_b.valid := io.slave.b.valid && slave_b_arb_id === i.U
      m_b.bits := io.slave.b.bits
      m_b.bits.id := io.slave.b.bits.id >> arbIdBits.U

      m_w.ready := io.slave.w.ready && w_chosen === i.U && !w_done
    }

    io.slave.r.ready := io.master(slave_r_arb_id).r.ready
    io.slave.b.ready := io.master(slave_b_arb_id).b.ready

    io.slave.w.bits := io.master(w_chosen).w.bits
    io.slave.w.valid := io.master(w_chosen).w.valid && !w_done

    io.slave.ar <> ar_arb.io.out

    io.slave.aw.bits <> aw_arb.io.out.bits
    io.slave.aw.valid := aw_arb.io.out.valid && w_done
    aw_arb.io.out.ready := io.slave.aw.ready && w_done

  } else { io.slave <> io.master.head }
}
