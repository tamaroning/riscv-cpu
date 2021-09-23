package fetch

import chisel3._
import chisel3.util._
import common.Consts._

class Core extends Module {
    var io = IO(new Bundle {
        val imem = Flipped(new ImemPortIo())
        // output port "exit" turns true when the program finishes
        val exit = Output(Bool())
    })

    // create 32 registers the length of WORD_LEN(=32) bit
    val regfile = Mem(32, UInt(WORD_LEN.W))

    // ***** Instruction Fetch Stage: IF *****
    
    // create PC register initialized with START_ADDR(=0)
    val pc_reg = RegInit(START_ADDR)
    // increment PC register by 4 byte
    pc_reg := pc_reg + 4.U(WORD_LEN.W)

    // connect addr with pc_reg??
    io.imem.addr := pc_reg
    val inst = io.imem.inst

    // set true to "exit" when inst == (the last line of the program)
    io.exit := (inst === 0x34333231.U(WORD_LEN.W))

    // debug
    printf(p"pc_reg : 0x${Hexadecimal(pc_reg)}\n")
    printf(p"inst   : 0x${Hexadecimal(inst)}\n")
    printf("--------\n")
}