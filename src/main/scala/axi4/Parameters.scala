// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package axi4

import chisel3.util.isPow2

case class Axi4BundleParameters(
  addrBits: Int,
  dataBits: Int,
  idBits:   Int,
  userBits: Axi4UserBitParameters = Axi4UserBitParameters()) {
  require(dataBits >= 8, s"AXI4 data bits must be >= 8 (got $dataBits)")
  require(addrBits >= 1, s"AXI4 addr bits must be >= 1 (got $addrBits)")
  require(idBits >= 1, s"AXI4 id bits must be >= 1 (got $idBits)")
  require(isPow2(dataBits), s"AXI4 data bits must be pow2 (got $dataBits)")
}

case class Axi4UserBitParameters(aw: Int = 0, ar: Int = 0, w: Int = 0, r: Int = 0, b: Int = 0) {
  require(aw >= 0, "Number of user bits on the aw channel must be non-negative.")
  require(ar >= 0, "Number of user bits on the ar channel must be non-negative.")
  require(w >= 0, "Number of user bits on the w channel must be non-negative.")
  require(r >= 0, "Number of user bits on the r channel must be non-negative.")
  require(b >= 0, "Number of user bits on the b channel must be non-negative.")
}