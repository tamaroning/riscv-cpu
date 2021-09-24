package cpu

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

    io.imem.addr := pc_reg
    val inst = io.imem.inst

    // ***** Instruction Decode Stage: ID *****

    val rs1_addr = inst(19, 15)
    val rs2_addr = inst(24, 20)
    val wb_addr = inst(11, 7)
    val rs1_data = Mux((rs1_addr =/= 0.U(WORD_LEN.W)), regfile(rs1_addr), 0.U(WORD_LEN.W))
    val rs2_data = Mux((rs2_addr =/= 0.U(WORD_LEN.W)), regfile(rs2_addr), 0.U(WORD_LEN.W))

    // set true to "exit" when inst == (the last line of the program)
    io.exit := (inst === 0x34333231.U(WORD_LEN.W))

    // debug
    printf(p"pc_reg  : 0x${Hexadecimal(pc_reg)}\n")
    printf(p"inst    : 0x${Hexadecimal(inst)}\n")
    printf(p"rs1_addr: 0x${Hexadecimal(rs1_addr)}\n")
    printf(p"rs2_addr: 0x${Hexadecimal(rs2_addr)}\n")
    printf(p"wb_addr : 0x${Hexadecimal(wb_addr)}\n")
    printf(p"rs1_data: 0x${Hexadecimal(rs1_data)}\n")
    printf(p"rs2_data: 0x${Hexadecimal(rs2_data)}\n")
    printf("--------\n")
}