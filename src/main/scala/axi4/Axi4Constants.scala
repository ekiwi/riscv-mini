// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package axi4

import chisel3._

object Axi4Constants {
  // These are all fixed by the standard:
  val LenBits = 8
  val SizeBits = 3
  val BurstBits = 2
  val LockBits = 1
  val CacheBits = 4
  val ProtBits = 3
  val QosBits = 4
  val RespBits = 2

  def CacheReadAllocate: UInt = 8.U(CacheBits.W)
  def CacheWriteAllocate: UInt = 4.U(CacheBits.W)
  def CacheModifiable: UInt = 2.U(CacheBits.W)
  def CacheBufferable: UInt = 1.U(CacheBits.W)

  def ProtPrivileged: UInt = 1.U(ProtBits.W)
  def ProtInsecure: UInt = 2.U(ProtBits.W)
  def ProtInstruction: UInt = 4.U(ProtBits.W)

  def BurstFixed: UInt = 0.U(BurstBits.W)
  def BurstIncr: UInt = 1.U(BurstBits.W)
  def BurstWrap: UInt = 2.U(BurstBits.W)

  def RespOkay: UInt = 0.U(RespBits.W)
  def RespExOkay: UInt = 1.U(RespBits.W)
  def RespSlvErr: UInt = 2.U(RespBits.W)
  def RespDevErr: UInt = 3.U(RespBits.W)
}