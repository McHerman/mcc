package mcc

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.chipsalliance.cde.config.Parameters

// Drives mcc.ImemTL's external TileLink `host` port directly (the same port
// a real SoC host would use to load a program), replacing the old
// constructor-time ROM preload. See mcc.ImemTL: a PutFullData per word into
// the loadable range, then a doorbell write (bit 0 of the data beat) to the
// sentinel address 0xF000 to latch initDone.
object MccImemLoader {
  import chisel3.simulator.PeekPokeAPI._

  private val InitDoneAddr = 0xF000L

  def load(dut: MccTestHarness, initData: Map[Int, BigInt], maxWaitCycles: Int = 1000): Unit = {
    dut.io.hostIn.d.ready.poke(true.B)
    dut.io.hostIn.a.bits.opcode.poke(ATA8.TilelinkOpcodes.PutFullData)
    dut.io.hostIn.a.bits.param.poke(0.U)
    dut.io.hostIn.a.bits.size.poke(4.U)
    dut.io.hostIn.a.bits.source.poke(0.U)
    dut.io.hostIn.a.bits.mask.poke("b1111".U)
    dut.io.hostIn.a.bits.corrupt.poke(0.U)

    def waitUntil(cond: => Boolean, msg: => String): Unit = {
      var cycles = 0
      while (!cond) {
        require(cycles < maxWaitCycles, s"MccImemLoader timeout waiting for $msg")
        dut.clock.step(1)
        cycles += 1
      }
    }

    def put(addr: Long, data: BigInt): Unit = {
      dut.io.hostIn.a.bits.address.poke(addr.U)
      dut.io.hostIn.a.bits.data.poke(data.U(32.W))
      dut.io.hostIn.a.valid.poke(true.B)
      waitUntil(dut.io.hostIn.a.ready.peek().litToBoolean, s"hostIn.a.ready (addr=$addr)")
      dut.clock.step(1)
      dut.io.hostIn.a.valid.poke(false.B)
      waitUntil(dut.io.hostIn.d.valid.peek().litToBoolean, s"hostIn.d.valid (addr=$addr)")
      dut.clock.step(1)
    }

    for ((wordIdx, word) <- initData.toSeq.sortBy(_._1)) {
      put(wordIdx.toLong * 4, word)
    }

    put(InitDoneAddr, BigInt(1))
  }
}

class MccElaborationTest extends AnyFreeSpec with Matchers with ChiselSim {
  implicit val p: Parameters = Parameters.empty

  "MccTestHarness should elaborate and run 100 cycles" in {
    simulate(new MccTestHarness()) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step(5)
      dut.reset.poke(false.B)
      dut.clock.step(100)
    }
  }
}

class MccProgramTest extends AnyFreeSpec with Matchers with ChiselSim {
  val hexFile  = sys.env.getOrElse("MCC_TEST_HEX", "")
  val maxCycles = sys.env.getOrElse("MCC_MAX_CYCLES", "50000").toInt

  implicit val p: Parameters = Parameters.empty

  "Mcc should execute test program" in {
    assume(hexFile.nonEmpty, "Set MCC_TEST_HEX=<path> to run program tests")

    val initData = MccHexReader(hexFile)
    info(s"Loaded ${initData.size} words from $hexFile")

    // dataInitData mirrors the same image onto dmem so any `.rodata` globals
    // the program references over dmem read back correctly (imem no longer
    // shares storage with dmem now that it's loaded over its own TileLink
    // host port below).
    simulate(new MccTestHarness(dataInitData = initData)) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step(5)
      dut.reset.poke(false.B)

      MccImemLoader.load(dut, initData)

      var done  = false
      var cycle = 0

      while (!done && cycle < maxCycles) {
        dut.clock.step(1)
        cycle += 1

        val tohost = dut.io.tohost.peek().litValue
        if (tohost != 0) {
          if (tohost == 1) {
            info(s"PASSED after $cycle cycles (tohost = $tohost)")
          } else {
            val testNum = tohost >> 1
            fail(s"FAILED after $cycle cycles (tohost = $tohost, test case = $testNum)")
          }
          done = true
        }
      }

      if (!done) fail(s"TIMEOUT after $maxCycles cycles")
    }
  }
}
