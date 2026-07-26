package mcc

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.chipsalliance.cde.config.Parameters

class MccLLVMTest extends AnyFreeSpec with Matchers with ChiselSim {
  //val hexFile  = sys.env.getOrElse("MCC_TEST_HEX", "")
  val maxCycles = sys.env.getOrElse("MCC_MAX_CYCLES", "50000").toInt

  implicit val p: Parameters = Parameters.empty

  "Mcc should execute test program" in {
    val hexFile  = "/home/karlhk/dtu/Thesis/hardware/mcc/test/programs_rv32/out.memhex"
    //assume(hexFile.nonEmpty, "Set MCC_TEST_HEX=<path> to run program tests")

    val initData = MccHexReader(hexFile)
    info(s"Loaded ${initData.size} words from $hexFile")

    simulate(new MccTestHarness(initData = initData)) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step(5)
      dut.reset.poke(false.B)
      dut.clock.step(initData.size + 2)

      var done  = false
      var cycle = 0

      while (!done && cycle < maxCycles) {
        dut.clock.step(1)
        cycle += 1
        /*
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
        */
        //if(dut.io.dmemMirror.a.valid.peek().litToBoolean && dut.io.dmemMirror.a.bits.address.peek() == 1.asUInt) {
        if(dut.io.dmemMirror.a.valid.peek().litToBoolean) {
          println(s"FOUND ACCESS, address: ${dut.io.dmemMirror.a.bits.address.peek().litValue}")
          
        }
      }

      if (!done) fail(s"TIMEOUT after $maxCycles cycles")
    }
  }


  def programSemaphore(dut: MccTestHarness, semIdx: Int, full: Int, empty: Int, gen: Int): Unit = {
    dut.io.semProgPort.valid.poke(true.B)
    dut.io.semProgPort.bits.addr.poke(semIdx.U)
    dut.io.semProgPort.bits.initFull.poke(full.U)
    dut.io.semProgPort.bits.initEmpty.poke(empty.U)
    dut.io.semProgPort.bits.generation.poke(gen.U)

    dut.clock.step()
    dut.io.semProgPort.valid.poke(false.B)
  }

  "Mcc should interface with sem" in {
    val hexFile  = "/home/karlhk/dtu/Thesis/hardware/mcc/test/programs_rv32/riscv-extrasmall.memhex"
    //assume(hexFile.nonEmpty, "Set MCC_TEST_HEX=<path> to run program tests")

    val initData = MccHexReader(hexFile)
    info(s"Loaded ${initData.size} words from $hexFile")

    simulate(new MccTestHarness(initData = initData)) { dut =>
      //dut.reset.poke(true.B)
      dut.clock.step(10)
      //dut.reset.poke(false.B)
      programSemaphore(dut,4,0,8,0)
      programSemaphore(dut,3,8,0,0)
      
      dut.clock.step(initData.size + 2)

      var done  = false
      var cycle = 0

      while (!done && cycle < maxCycles) {
        dut.clock.step(1)
        cycle += 1
      }

      if (!done) fail(s"TIMEOUT after $maxCycles cycles")
    }
  } 
}
