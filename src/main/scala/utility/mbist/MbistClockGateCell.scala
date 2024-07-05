package utility.mbist
import chisel3._
import utility.ClockGate
import utility.sram.SramBroadcastBundle

class CgDftBundle extends Bundle {
  val ram_aux_clk = Input(Bool())
  val ram_aux_ckbp = Input(Bool())
  def fromBroadcast(brc: SramBroadcastBundle): Unit = {
    ram_aux_clk := brc.ram_aux_clk
    ram_aux_ckbp := brc.ram_aux_ckbp
  }
}

class MbistClockGateCell(mcpCtl:Boolean) extends Module {
  val mbist = IO(new Bundle {
    val writeen = Input(Bool())
    val readen = Input(Bool())
    val req = Input(Bool())
  })
  val E = IO(Input(Bool()))
  val TE = IO(Input(Bool()))
  val dft = if(mcpCtl) Some(IO(new CgDftBundle)) else None
  val out_clock = IO(Output(Clock()))

  private val CG = Module(new ClockGate)
  CG.io.E := Mux(mbist.req, mbist.readen | mbist.writeen, E)
  CG.io.TE := TE
  CG.io.CK := clock

  if(mcpCtl) {
    out_clock := Mux(dft.get.ram_aux_ckbp, dft.get.ram_aux_clk.asClock, CG.io.Q)
  } else {
    out_clock := CG.io.Q
  }
}
