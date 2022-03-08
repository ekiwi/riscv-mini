package mini

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import junctions._

object TileAndMemTop extends ChiselEnum {
  val sIdle, sWrite, sWrAck, sRead = Value
}

class TileAndMemTop(config: Config) extends Module {
  val trace = false

  val dut = Module(Tile(config))
  // extract parameters from design under test
  val nasti = dut.nastiParams

  dut.io.host.fromhost.bits := 0.U
  dut.io.host.fromhost.valid := false.B

  val _mem = Mem(1 << 20, UInt(nasti.dataBits.W))
  import TileAndMemTop._
  val state = RegInit(sIdle)
  val cycle = RegInit(0.U(32.W))

  val id = Reg(UInt(nasti.idBits.W))
  val addr = Reg(UInt(nasti.addrBits.W))
  val len = Reg(UInt(NastiConstants.LenBits.W))
  val off = Reg(UInt(NastiConstants.LenBits.W))
  val write = (0 until (nasti.dataBits / 8)).foldLeft(0.U(nasti.dataBits.W)) { (write, i) =>
    write |
      (Mux(dut.io.nasti.w.bits.strb(i), dut.io.nasti.w.bits.data, _mem(addr))(
        8 * (i + 1) - 1,
        8 * i
      ) << (8 * i).U).asUInt
  }

  dut.reset := reset.asBool
  dut.io.nasti.aw.ready := state === sIdle
  dut.io.nasti.ar.ready := state === sIdle
  dut.io.nasti.w.ready := state === sWrite
  dut.io.nasti.b.bits := NastiWriteResponseBundle(nasti)(id)
  dut.io.nasti.b.valid := state === sWrAck
  dut.io.nasti.r.bits := NastiReadDataBundle(nasti)(id, _mem(addr + off), off === len)
  dut.io.nasti.r.valid := state === sRead

  val isDone = WireInit(false.B)
  val setDone = WireInit(false.B)

  cycle := cycle + 1.U
  when(dut.io.host.tohost =/= 0.U) {
    isDone := true.B
  }

  setDone := isDone
  when(setDone) {
//    printf("cycles: %d\n", cycle)
//    assert((dut.io.host.tohost >> 1.U) === 0.U, "* tohost: %d *\n", dut.io.host.tohost)
//    stop()
  }

  switch(state) {
    is(sIdle) {
      when(dut.io.nasti.aw.valid) {
        // assert((1.U << dut.io.nasti.aw.bits.size).asUInt === (nasti.dataBits / 8).U)
        addr := dut.io.nasti.aw.bits.addr / (nasti.dataBits / 8).U
        id := dut.io.nasti.aw.bits.id
        len := dut.io.nasti.aw.bits.len
        off := 0.U
        state := sWrite
      }.elsewhen(dut.io.nasti.ar.valid) {
        // assert((1.U << dut.io.nasti.ar.bits.size).asUInt === (nasti.dataBits / 8).U)
        addr := dut.io.nasti.ar.bits.addr / (nasti.dataBits / 8).U
        id := dut.io.nasti.aw.bits.id
        len := dut.io.nasti.ar.bits.len
        off := 0.U
        state := sRead
      }
    }
    is(sWrite) {
      when(dut.io.nasti.w.valid) {
        _mem(addr + off) := write
        if (trace) printf("MEM[%x] <= %x\n", (addr + off) * (nasti.dataBits / 8).U, write)
        when(off === len) {
          // assert(dut.io.nasti.w.bits.last)
          state := sWrAck
        }.otherwise {
          off := off + 1.U
        }
      }
    }
    is(sWrAck) {
      when(dut.io.nasti.b.fire) {
        state := sIdle
      }
    }
    is(sRead) {
      when(dut.io.nasti.r.fire) {
        when(off === len) {
          state := sIdle
        }.otherwise {
          off := off + 1.U
        }
      }
    }
  }
}
