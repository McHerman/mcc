package mcc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import mcc.common._
import mcc.common.Constants._
import freechips.rocketchip.rocket.CoreInterrupts

object MccHexReader {
  def apply(path: String): Map[Int, BigInt] = {
    val src = scala.io.Source.fromFile(path)
    val result = scala.collection.mutable.Map[Int, BigInt]()
    var addr = 0
    for (line <- src.getLines()) {
      val trimmed = line.trim
      if (trimmed.startsWith("@")) {
        addr = Integer.parseInt(trimmed.drop(1), 16)
      } else if (trimmed.nonEmpty && !trimmed.startsWith("//")) {
        result(addr) = BigInt(trimmed, 16)
        addr += 1
      }
    }
    src.close()
    result.toMap
  }
}

// Test harness for the mcc 5-stage RV32I core.
// Provides a simple dual-port async scratchpad and tohost detection.
class MccTestHarness(
  memBytes:   Int              = 65536,           // 64 KB scratchpad
  baseAddr:   Long             = 0x80000000L,
  tohostAddr: Long             = 0x80001000L,
  initData:   Map[Int, BigInt] = Map.empty        // word-address → 32-bit word
)(implicit p: Parameters) extends Module {

  implicit val conf: MccCoreParams = MccCoreParams(xprlen = 32)

  val io = IO(new Bundle {
    val success = Output(Bool())
    val tohost  = Output(UInt(32.W))
    val cycles  = Output(UInt(64.W))
  })

  val core = Module(new mcc.stage5.Core())

  // Cycle counter
  val cycleCount = RegInit(0.U(64.W))
  cycleCount := cycleCount + 1.U
  io.cycles := cycleCount

  // Tie off debug and interrupt ports
  core.io.ddpath := DontCare
  core.io.dcpath := DontCare
  core.io.interrupt := 0.U.asTypeOf(new CoreInterrupts(false))
  core.io.hartid := 0.U
  core.io.reset_vector := baseAddr.U

  // ========================================================================
  // Async byte-addressed scratchpad memory
  // ========================================================================
  // Indexed by 32-bit word address derived from the lower addrWidth bits of
  // the byte address.

  val addrWidth = log2Ceil(memBytes)
  val numWords  = memBytes / 4

  // Word index helper: strips base address bits and divides by 4
  def wordIdx(addr: UInt): UInt = addr(addrWidth - 1, 2)

  // Byte-lane memory as Vec(4, UInt(8.W)) words
  val mem = Mem(numWords, Vec(4, UInt(8.W)))

  // ROM init: write initData words on the first cycles after reset
  val initDone: Bool = if (initData.nonEmpty) {
    val entries   = initData.toSeq.sortBy(_._1)
    val romAddrs  = VecInit(entries.map { case (a, _) => a.U(log2Ceil(numWords).W) })
    val romData   = VecInit(entries.map { case (_, d) =>
      VecInit(Seq(
        ((d >>  0) & 0xFF).U(8.W),
        ((d >>  8) & 0xFF).U(8.W),
        ((d >> 16) & 0xFF).U(8.W),
        ((d >> 24) & 0xFF).U(8.W),
      ))
    })
    val idx  = RegInit(0.U(log2Ceil(entries.size + 1).W))
    val done = idx === entries.size.U
    when(!reset.asBool && !done) {
      mem.write(romAddrs(idx), romData(idx))
      idx := idx + 1.U
    }
    done
  } else {
    true.B
  }

  // ========================================================================
  // Instruction memory port (imem) — async combinational read, no writes
  // ========================================================================
  core.io.imem.req.ready := true.B

  val imem_widx   = wordIdx(core.io.imem.req.bits.addr)
  val imem_rdata  = mem.read(imem_widx)
  val imem_word   = Cat(imem_rdata(3), imem_rdata(2), imem_rdata(1), imem_rdata(0))

  core.io.imem.resp.valid      := initDone && core.io.imem.req.valid
  core.io.imem.resp.bits.data  := imem_word

  // ========================================================================
  // Data memory port (dmem) — async read, combinational write
  // ========================================================================
  core.io.dmem.req.ready := true.B

  val dmem_req  = core.io.dmem.req.bits
  val dmem_wen  = core.io.dmem.req.valid && dmem_req.fcn === M_XWR
  val dmem_widx = wordIdx(dmem_req.addr)

  // Byte offset within word
  val dmem_offset = dmem_req.addr(1, 0)

  // Byte mask from typ field (TL size: typ - 1 gives TL size, lower 2 bits)
  val tlSize    = dmem_req.getTLSize
  val byteMask  = MuxLookup(tlSize, "b1111".U(4.W))(Seq(
    0.U -> ("b0001".U(4.W) << dmem_offset)(3, 0),
    1.U -> ("b0011".U(4.W) << dmem_offset)(3, 0),
    2.U -> "b1111".U(4.W),
  ))

  // Shifted write data
  val wdata_shifted = VecInit(Seq(
    (dmem_req.data >>  0)(7, 0),
    (dmem_req.data >>  8)(7, 0),
    (dmem_req.data >> 16)(7, 0),
    (dmem_req.data >> 24)(7, 0),
  ))

  when(dmem_wen) {
    mem.write(dmem_widx, wdata_shifted, byteMask.asBools)
  }

  // Async read for load response
  val dmem_rraw = mem.read(dmem_widx)
  val dmem_word = Cat(dmem_rraw(3), dmem_rraw(2), dmem_rraw(1), dmem_rraw(0))

  // Shift and sign-extend read data: shift right by byte offset, mask to size
  val dmem_shifted = (dmem_word >> (dmem_offset << 3))(31, 0)
  val dmem_signed  = dmem_req.getTLSigned
  val dmem_rdata   = MuxLookup(dmem_req.getTLSize, dmem_shifted)(Seq(
    0.U -> Cat(Fill(24, dmem_signed && dmem_shifted(7)),  dmem_shifted(7,  0)),
    1.U -> Cat(Fill(16, dmem_signed && dmem_shifted(15)), dmem_shifted(15, 0)),
  ))

  core.io.dmem.resp.valid     := core.io.dmem.req.valid
  core.io.dmem.resp.bits.data := dmem_rdata

  // ========================================================================
  // tohost detection
  // ========================================================================
  val tohostWordIdx = ((tohostAddr - baseAddr) / 4).toInt
  val tohostReg = RegInit(0.U(32.W))
  when(dmem_wen && dmem_widx === tohostWordIdx.U) {
    tohostReg := Cat(
      Mux(byteMask(3), wdata_shifted(3), 0.U),
      Mux(byteMask(2), wdata_shifted(2), 0.U),
      Mux(byteMask(1), wdata_shifted(1), 0.U),
      Mux(byteMask(0), wdata_shifted(0), 0.U),
    )
  }

  io.tohost  := tohostReg
  io.success := tohostReg === 1.U
}
