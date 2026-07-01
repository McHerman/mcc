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
  // Data memory port (dmem) — TileLink A/D, async scratchpad
  // ========================================================================
  core.io.dmem.a.ready := true.B

  val tl_a         = core.io.dmem.a.bits
  val tl_req_valid = core.io.dmem.a.valid
  val tl_is_put    = tl_a.opcode === TilelinkOpcodes.PutFullData ||
                     tl_a.opcode === TilelinkOpcodes.PutPartialData
  val dmem_widx    = wordIdx(tl_a.address)

  // Write: mask and data are already byte-lane aligned (shifted by memory stage)
  val wdata_bytes = VecInit(Seq(
    tl_a.data( 7,  0),
    tl_a.data(15,  8),
    tl_a.data(23, 16),
    tl_a.data(31, 24),
  ))
  when(tl_req_valid && tl_is_put) {
    mem.write(dmem_widx, wdata_bytes, tl_a.mask.asBools)
  }

  // Read: shift right by byte offset within word, then sign/zero-extend
  // size[1:0] = TL access size (0=byte, 1=half, 2=word)
  // size[2]   = 1 if signed, 0 if unsigned (custom encoding from memory stage)
  val dmem_rraw    = mem.read(dmem_widx)
  val dmem_word    = Cat(dmem_rraw(3), dmem_rraw(2), dmem_rraw(1), dmem_rraw(0))
  val byte_off     = tl_a.address(1, 0)
  val tl_size      = tl_a.size(1, 0)
  val tl_signed    = tl_a.size(2)
  val dmem_shifted = (dmem_word >> Cat(byte_off, 0.U(3.W)))(31, 0)
  val dmem_rdata   = MuxLookup(tl_size, dmem_shifted)(Seq(
    0.U -> Cat(Fill(24, tl_signed && dmem_shifted(7)),  dmem_shifted(7,  0)),
    1.U -> Cat(Fill(16, tl_signed && dmem_shifted(15)), dmem_shifted(15, 0)),
  ))

  // D channel response (combinational, same cycle as request)
  core.io.dmem.d.valid        := tl_req_valid
  core.io.dmem.d.bits.opcode  := Mux(tl_is_put, TilelinkOpcodes.AccessAck, TilelinkOpcodes.AccessAckData)
  core.io.dmem.d.bits.param   := 0.U
  core.io.dmem.d.bits.size    := tl_a.size
  core.io.dmem.d.bits.source  := tl_a.source
  core.io.dmem.d.bits.sink    := 0.U
  core.io.dmem.d.bits.denied  := 0.U
  core.io.dmem.d.bits.data    := dmem_rdata
  core.io.dmem.d.bits.corrupt := 0.U

  // ========================================================================
  // tohost detection
  // ========================================================================
  val tohostWordIdx = ((tohostAddr - baseAddr) / 4).toInt
  val tohostReg = RegInit(0.U(32.W))
  when(tl_req_valid && tl_is_put && dmem_widx === tohostWordIdx.U) {
    tohostReg := Cat(
      Mux(tl_a.mask(3), wdata_bytes(3), 0.U),
      Mux(tl_a.mask(2), wdata_bytes(2), 0.U),
      Mux(tl_a.mask(1), wdata_bytes(1), 0.U),
      Mux(tl_a.mask(0), wdata_bytes(0), 0.U),
    )
  }

  io.tohost  := tohostReg
  io.success := tohostReg === 1.U
}
