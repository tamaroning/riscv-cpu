package cpu

import chisel3._
import chisel3.util._
import common.Consts._
import common.Instructions._

class Core extends Module {
    var io = IO(new Bundle {
        val imem = Flipped(new ImemPortIo())
        val dmem = Flipped(new DmemPortIo())
        // output port that turns true when the program finishes
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
    val wb_addr = inst(11, 7) // write back register
    val rs1_data = Mux((rs1_addr =/= 0.U(WORD_LEN.W)), regfile(rs1_addr), 0.U(WORD_LEN.W))
    val rs2_data = Mux((rs2_addr =/= 0.U(WORD_LEN.W)), regfile(rs2_addr), 0.U(WORD_LEN.W))

    val imm_i = inst(31, 20)
    val imm_i_sext = Cat(Fill(20, imm_i(11)), imm_i) // sign extension

    val imm_s = Cat(inst(31, 25), inst(11,7))
    val imm_s_sext = Cat(Fill(20, imm_s(11)), imm_s)

    val csignals = ListLookup(inst, 
                    List(ALU_X, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X),
        Array(
            LW   -> List(ALU_ADD , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_MEM),
            SW   -> List(ALU_ADD , OP1_RS1, OP2_IMS, MEN_S, REN_X, WB_X  ),
            ADD  -> List(ALU_ADD , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
            ADDI -> List(ALU_ADD , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
            SUB  -> List(ALU_SUB , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
            AND  -> List(ALU_AND , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
            OR   -> List(ALU_OR  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
            XOR  -> List(ALU_XOR , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
            ANDI -> List(ALU_AND , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
            ORI  -> List(ALU_OR  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
            XORI -> List(ALU_XOR , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
            SLL  -> List(ALU_SLL , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
            SRL  -> List(ALU_SRL , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
            SRA  -> List(ALU_SRA , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
            SLLI -> List(ALU_SLL , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
            SRLI -> List(ALU_SRL , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
            SRAI -> List(ALU_SRA , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
            SLT  -> List(ALU_SLT , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
            SLTU -> List(ALU_SLTU, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
            SLTI -> List(ALU_SLT , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
            SLTIU-> List(ALU_SLTU, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
        )
    )
    //  ALU ops    oprand1    oprand2    mem_wrt?   wrt_bck?  wrt_bck_location
    val exe_fun :: op1_sel :: op2_sel :: mem_wen :: rf_wen :: wb_sel :: Nil = csignals
    // mem_wen: MEN_S: dont write memory, MEN_X: write memory
    // rf_wen : REN_S: write back to reg, REN_X dont write back to reg
    // wb_sel : WB_X: dont write back, WB_MEM: write back memory to reg, WB_ALU: write back ALU to reg

    val op1_data = MuxCase(0.U(WORD_LEN.W), Seq(
        (op1_sel === OP1_RS1) -> rs1_data,
    ))

    val op2_data = MuxCase(0.U(WORD_LEN.W), Seq(
        (op2_sel === OP2_RS2) -> rs2_data,
        (op2_sel === OP2_IMI) -> imm_i_sext,
        (op2_sel === OP2_IMS) -> imm_s_sext,
    ))


    // ***** Execute Stage: EX *****

    val alu_out = MuxCase(0.U(WORD_LEN.W), Seq(
        (exe_fun === ALU_ADD) -> (op1_data + op2_data),
        (exe_fun === ALU_SUB) -> (op1_data - op2_data),
        (exe_fun === ALU_AND) -> (op1_data & op2_data),
        (exe_fun === ALU_OR) -> (op1_data | op2_data),
        (exe_fun === ALU_XOR) -> (op1_data ^ op2_data),
        (exe_fun === ALU_SLL) -> (op1_data << op2_data(4, 0))(31, 0),
        (exe_fun === ALU_SRL) -> (op1_data >> op2_data(4, 0)).asUInt(),
        (exe_fun === ALU_SRA) -> (op1_data.asSInt() >> op2_data(4, 0)).asUInt(),
        (exe_fun === ALU_SLT) -> (op1_data.asSInt() < op2_data.asSInt()).asUInt(),
        (exe_fun === ALU_SLTU)-> (op1_data < op2_data).asUInt(),
    ))


    // ***** Memory Access Stage *****

    // set wdata to addr only when wen is true
    io.dmem.addr := alu_out
    io.dmem.wen := mem_wen
    io.dmem.wdata := rs2_data


    // ***** Write Back Stage: WB *****

    // get write back data only when WB_MEM
    val wb_data = MuxCase(alu_out, Seq(
        (wb_sel === WB_MEM) -> io.dmem.rdata,
    ))
    // do write back only when REN_S
    when(rf_wen === REN_S) {
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