package mcc

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.chipsalliance.cde.config.Parameters

class MccLLVMTest extends AnyFreeSpec with Matchers with ChiselSim {
  //val hexFile  = sys.env.getOrElse("MCC_TEST_HEX", "")
  val hexFile  = "/home/karlhk/dtu/Thesis/hardware/mcc/test/programs_rv32/out.memhex"
  val maxCycles = sys.env.getOrElse("MCC_MAX_CYCLES", "50000").toInt

  implicit val p: Parameters = Parameters.empty

  "Mcc should execute test program" in {
    assume(hexFile.nonEmpty, "Set MCC_TEST_HEX=<path> to run program tests")

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
}
