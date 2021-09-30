package cpu

import chisel3._
import chisel3.util._
import common.Consts._
import common.Instructions._

class Core extends Module {
    var io = IO(new Bundle {
        val imem = Flipped(new ImemPortIo())
        val dmem = Flipped(new DmemPortIo())
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

    val imm_i = inst(31, 20)
    val imm_i_sext = Cat(Fill(20, imm_i(11)), imm_i) // sign extension

    val imm_s = Cat(inst(31, 25), inst(11,7))
    val imm_s_sext = Cat(Fill(20, imm_s(11)), imm_s)

    // ***** Execute Stage: EX *****

    val alu_out = MuxCase(0.U(WORD_LEN.W), Seq(
        (inst === LW || inst === ADDI) -> (rs1_data + imm_i_sext),
        (inst === SW) -> (rs1_data + imm_s_sext),
        (inst === ADD) -> (rs1_data + rs2_data),
        (inst === SUB) -> (rs1_data - rs2_data),
        (inst === AND) -> (rs1_data + rs2_data),
        (inst === OR) -> (rs1_data | rs2_data),
        (inst === XOR) -> (rs1_data ^ rs2_data),
        (inst === ANDI) -> (rs1_data & imm_i_sext),
        (inst === ORI) -> (rs1_data | imm_i_sext),
        (inst === XORI) -> (rs1_data ^ imm_i_sext),
    ))

    // ***** Memory Access Stage *****

    // specify address and  get data
    io.dmem.addr := alu_out
    io.dmem.wen := (inst === SW)
    io.dmem.wdata := rs2_data

    // ***** Write Back Stage: WB *****

    // write back data on the register wb_addr specifies
    val wb_data = io.dmem.rdata
    when(inst === LW || inst === ADD || inst === ADDI || inst === SUB || inst === AND || inst === OR || inst === XOR || inst === ANDI || inst === ORI || inst === XORI) {
        regfile(wb_addr) := wb_data
    }

    // set true to "exit" when inst == (the last line of the program)
    io.exit := (inst === 0x00602823.U(WORD_LEN.W))

    // debug
    printf(p"pc_reg    : 0x${Hexadecimal(pc_reg)}\n")
    printf(p"inst      : 0x${Hexadecimal(inst)}\n")
    printf(p"rs1_addr  : 0x${Hexadecimal(rs1_addr)}\n")
    printf(p"rs2_addr  : 0x${Hexadecimal(rs2_addr)}\n")
    printf(p"wb_addr   : 0x${Hexadecimal(wb_addr)}\n")
    printf(p"rs1_data  : 0x${Hexadecimal(rs1_data)}\n")
    printf(p"rs2_data  : 0x${Hexadecimal(rs2_data)}\n")
    printf(p"wb_data   : 0x${Hexadecimal(wb_data)}\n")
    printf(p"dmem.addr : ${io.dmem.addr}\n")
    printf(p"dmem.wen  : ${io.dmem.wen}\n")
    printf(p"dmem.wdata: 0x${Hexadecimal(io.dmem.wdata)}\n")
    printf("--------\n")
}