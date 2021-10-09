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
    io.imem.addr := pc_reg
    val inst = io.imem.inst
    val pc_plus4 = pc_reg + 4.U(WORD_LEN.W)
    val br_flg = Wire(Bool())
    val br_target = Wire(UInt(WORD_LEN.W))
    val jmp_flg = (inst === JAL || inst === JALR)
    val alu_out = Wire(UInt(WORD_LEN.W))

    val pc_next = MuxCase(pc_plus4, Seq(
        br_flg -> br_target,
        jmp_flg -> alu_out,
    ))
    pc_reg := pc_next


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
    val imm_b = Cat(inst(31), inst(7), inst(30, 25), inst(11, 8))
    val imm_b_sext = Cat(Fill(19, imm_b(11)), imm_b, 0.U(1.U))
    val imm_j = Cat(inst(31), inst(19, 12), inst(20), inst(30, 21))
    val imm_j_sext = Cat(Fill(11, imm_j(19)), imm_j, 0.U(1.U))

    val csignals = ListLookup(inst, 
                    List(ALU_X, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X),
        Array(
            LW    -> List(ALU_ADD  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_MEM, CSR_X),
            SW    -> List(ALU_ADD  , OP1_RS1, OP2_IMS, MEN_S, REN_X, WB_X  , CSR_X),
            ADD   -> List(ALU_ADD  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
            ADDI  -> List(ALU_ADD  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
            SUB   -> List(ALU_SUB  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
            AND   -> List(ALU_AND  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
            OR    -> List(ALU_OR   , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
            XOR   -> List(ALU_XOR  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
            ANDI  -> List(ALU_AND  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
            ORI   -> List(ALU_OR   , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
            XORI  -> List(ALU_XOR  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
            SLL   -> List(ALU_SLL  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
            SRL   -> List(ALU_SRL  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
            SRA   -> List(ALU_SRA  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
            SLLI  -> List(ALU_SLL  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
            SRLI  -> List(ALU_SRL  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
            SRAI  -> List(ALU_SRA  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
            SLT   -> List(ALU_SLT  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
            SLTU  -> List(ALU_SLTU , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
            SLTI  -> List(ALU_SLT  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
            SLTIU -> List(ALU_SLTU , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
            BEQ   -> List(BR_BEQ   , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X  , CSR_X),
            BNE   -> List(BR_BNE   , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X  , CSR_X),
            BGE   -> List(BR_BGE   , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X  , CSR_X),
            BGEU  -> List(BR_BGEU  , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X  , CSR_X),
            BLT   -> List(BR_BLT   , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X  , CSR_X),
            BLTU  -> List(BR_BLTU  , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X  , CSR_X),
            JAL   -> List(ALU_ADD  , OP1_PC , OP2_IMJ, MEN_X, REN_S, WB_PC , CSR_X),
            JALR  -> List(ALU_JALR , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_PC , CSR_X),
        )
    )
    //  ALU ops    oprand1    oprand2    mem_wrt?   wrt_bck?  wrt_bck_location
    val exe_fun :: op1_sel :: op2_sel :: mem_wen :: rf_wen :: wb_sel :: Nil = csignals
    // mem_wen: MEN_S: dont write memory, MEN_X: write memory
    // rf_wen : REN_S: write back to reg, REN_X dont write back to reg
    // wb_sel : WB_X: dont write back, WB_MEM: write back memory to reg, WB_ALU: write back ALU to reg

    val op1_data = MuxCase(0.U(WORD_LEN.W), Seq(
        (op1_sel === OP1_RS1) -> rs1_data,
        (op1_sel === OP1_PC)  -> pc_reg,
    ))

    val op2_data = MuxCase(0.U(WORD_LEN.W), Seq(
        (op2_sel === OP2_RS2) -> rs2_data,
        (op2_sel === OP2_IMI) -> imm_i_sext,
        (op2_sel === OP2_IMS) -> imm_s_sext,
        (op2_sel === OP2_IMJ) -> imm_j_sext,
    ))


    // ***** Execute Stage: EX *****

    alu_out := MuxCase(0.U(WORD_LEN.W), Seq(
        (exe_fun === ALU_ADD)  -> (op1_data + op2_data),
        (exe_fun === ALU_SUB)  -> (op1_data - op2_data),
        (exe_fun === ALU_AND)  -> (op1_data & op2_data),
        (exe_fun === ALU_OR)   -> (op1_data | op2_data),
        (exe_fun === ALU_XOR)  -> (op1_data ^ op2_data),
        (exe_fun === ALU_SLL)  -> (op1_data << op2_data(4, 0))(31, 0),
        (exe_fun === ALU_SRL)  -> (op1_data >> op2_data(4, 0)).asUInt(),
        (exe_fun === ALU_SRA)  -> (op1_data.asSInt() >> op2_data(4, 0)).asUInt(),
        (exe_fun === ALU_SLT)  -> (op1_data.asSInt() < op2_data.asSInt()).asUInt(),
        (exe_fun === ALU_SLTU) -> (op1_data < op2_data).asUInt(),
        (exe_fun === ALU_JALR) -> ((op1_data + op2_data) & ~1.U(WORD_LEN.W)),
    ))

    br_flg := MuxCase(false.B, Seq(
        (exe_fun === BR_BEQ)  ->  (op1_data === op2_data),
        (exe_fun === BR_BNE)  -> !(op1_data === op2_data),
        (exe_fun === BR_BLT)  ->  (op1_data.asSInt() < op2_data.asSInt()),
        (exe_fun === BR_BGE)  -> !(op1_data.asSInt() < op2_data.asSInt()),
        (exe_fun === BR_BLTU) ->  (op1_data < op2_data),
        (exe_fun === BR_BGEU) -> !(op1_data < op2_data),
    ))
    br_target := pc_reg + imm_b_sext


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