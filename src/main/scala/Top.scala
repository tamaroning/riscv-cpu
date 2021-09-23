package fetch

import chisel3._
import chisel3.util._

class Top extends Module {
    val io = IO(new Bundle {
        val exit = Output(Bool())
    })

    val core = Module(new Core())
    val memory = Module(new Memory())

    // connect core IO with memory IO
    core.io.imem <> memory.io.imem
    io.exit := core.io.exit
}