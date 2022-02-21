// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package axi4
import chisel3._
import chisel3.util._


case class NastiBundleParameters(
  addrBits: Int,
  dataBits: Int,
  idBits:   Int) {
  require(dataBits >= 8, s"AXI4 data bits must be >= 8 (got $dataBits)")
  require(addrBits >= 1, s"AXI4 addr bits must be >= 1 (got $addrBits)")
  require(idBits >= 1, s"AXI4 id bits must be >= 1 (got $idBits)")
  require(isPow2(dataBits), s"AXI4 data bits must be pow2 (got $dataBits)")
}

/** aka the AW/AR channel */
class Axi4AddressBundle(params: Axi4BundleParameters, userBits: Int = 0) extends Bundle {
  val id = UInt(params.idBits.W)
  val addr = UInt(params.addrBits.W)
  val len = UInt(Axi4Constants.LenBits.W) // number of beats - 1
  val size = UInt(Axi4Constants.SizeBits.W) // bytes in beat = 2^size
  val burst = UInt(Axi4Constants.BurstBits.W)
  val lock = UInt(Axi4Constants.LockBits.W)
  val cache = UInt(Axi4Constants.CacheBits.W)
  val prot = UInt(Axi4Constants.ProtBits.W)
  val qos = UInt(Axi4Constants.QosBits.W) // 0=no QoS, bigger = higher priority
  val user = UInt(userBits.W)
}

object Axi4AddressBundle {
  def apply(params: NastiBundleParameters)(id: UInt, addr: UInt, size: UInt, len: UInt = 0.U): Axi4AddressBundle = {
    val aw = Wire(new Axi4AddressBundle(params))
    aw.id := id
    aw.addr := addr
    aw.len := len
    aw.size := size
    aw.burst := NastiConstants.BurstIncr
    aw.lock := false.B
    aw.cache := 0.U
    aw.prot := 0.U
    aw.qos := 0.U
    aw
  }
}

/** aka the W-channel */
class NastiWriteDataBundle(params: NastiBundleParameters) extends Bundle {
  // id removed
  val data = UInt(params.dataBits.W)
  val strb = UInt((params.dataBits / 8).W)
  val last = Bool()
}

object NastiWriteDataBundle {
  def apply(
    params: NastiBundleParameters
  )(data:   UInt,
    strb:   Option[UInt] = None,
    last:   Bool = true.B
  ): NastiWriteDataBundle = {
    val w = Wire(new NastiWriteDataBundle(params))
    w.strb := strb.getOrElse(Fill(params.dataBits / 8, 1.U))
    w.data := data
    w.last := last
    w
  }
}

/** aka the R-channel */
class NastiReadDataBundle(params: NastiBundleParameters) extends Bundle {
  val id = UInt(params.idBits.W)
  val data = UInt(params.dataBits.W)
  val resp = UInt(NastiConstants.RespBits.W)
  val last = Bool()
}

object NastiReadDataBundle {
  def apply(
    params: NastiBundleParameters
  )(id:     UInt,
    data:   UInt,
    last:   Bool = true.B,
    resp:   UInt = 0.U
  ): NastiReadDataBundle = {
    val r = Wire(new NastiReadDataBundle(params))
    r.id := id
    r.data := data
    r.last := last
    r.resp := resp
    r
  }
}

/** aka the B-channel */
class NastiWriteResponseBundle(params: NastiBundleParameters) extends Bundle {
  val id = UInt(params.idBits.W)
  val resp = UInt(NastiConstants.RespBits.W)
}

object NastiWriteResponseBundle {
  def apply(params: NastiBundleParameters)(id: UInt, resp: UInt = 0.U): NastiWriteResponseBundle = {
    val b = Wire(new NastiWriteResponseBundle(params))
    b.id := id
    b.resp := resp
    b
  }
}

class NastiBundle(params: NastiBundleParameters) extends Bundle {
  val aw = Decoupled(new Axi4AddressBundle(params))
  val w = Decoupled(new NastiWriteDataBundle(params))
  val b = Flipped(Decoupled(new NastiWriteResponseBundle(params)))
  val ar = Decoupled(new Axi4AddressBundle(params))
  val r = Flipped(Decoupled(new NastiReadDataBundle(params)))
}
