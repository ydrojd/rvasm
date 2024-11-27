#ifndef RVLIB_H
#define RVLIB_H

#include <assert.h>
#include <cstdint>

const uint8_t LOAD_OPCODE = 0b00000011;
const uint8_t STORE_OPCODE = 0b00100011;
const uint8_t MADD_OPCODE = 0b01000011;
const uint8_t BRANCH_OPCODE = 0b01100011;

const uint8_t LOAD_FP_OPCODE = 0b00000111;
const uint8_t STORE_FP_OPCODE = 0b00100111;
const uint8_t MSUB_OPCODE = 0b01000111;
const uint8_t JALR_OPCODE = 0b01100111;

const uint8_t NMSUB_OPCODE = 0b01001011;

const uint8_t MISC_MEM_OPCODE = 0b00001111;
const uint8_t AMO_OPCODE = 0b00101111;
const uint8_t NMADD_OPCODE = 0b01001111;
const uint8_t JAL_OPCODE = 0b01101111;

const uint8_t OP_IMM_OPCODE = 0b00010011;
const uint8_t OP_OPCODE = 0b00110011;
const uint8_t OP_FP_OPCODE = 0b01010011;
const uint8_t SYSTEM_OPCODE = 0b01110011;

const uint8_t AUIPC_OPCODE = 0b00010111;
const uint8_t LUI_OPCODE = 0b00110111;
const uint8_t OP_IMM_32 = 0b00011011;

const int OPCODE_POS = 0;
const int OPCODE_SIZE = 7;
const int FUNCT3_POS = 12;
const int FUNCT3_SIZE = 3;
const int FUNCT7_POS = 25;
const int FUNCT7_SIZE = 7;
const int RS_SIZE = 5;
const int RD_POS = 7;
const int RS1_POS = 15;
const int RS2_POS = 20;
const int IMM_ITYPE_POS = 20;
const int IMM_ITYPE_SIZE = 12;

inline uint32_t bitwise_select(uint32_t val, uint8_t position, uint8_t size) {
    return (val >> position) & ((1 << size) - 1);
}

inline uint32_t bitwise_range_select(uint32_t val, uint32_t a, uint32_t b) {
    auto size = (a - b) + 1;
    assert(size > 0);
    return bitwise_select(val, b, size);
}

inline uint32_t bitwise_place(uint32_t x, uint32_t val, uint8_t position, uint8_t size) {
    return (x & ~(((1 << size) - 1) << position)) | (val << position);
}

inline uint32_t bitwise_range_place(uint32_t x, uint32_t val, uint8_t a, uint8_t b) {
    auto size = (a - b) + 1;
    assert(size > 0);
    return bitwise_place(x, val, b, size);
}

inline uint32_t encode_function(uint32_t funct7, uint32_t funct3, uint32_t opcode) {
    uint32_t inst = 0;

    inst = bitwise_place(inst, opcode, OPCODE_POS, OPCODE_SIZE);
    inst = bitwise_place(inst, funct3, FUNCT3_POS, FUNCT3_SIZE);
    inst = bitwise_place(inst, funct7, FUNCT7_POS, FUNCT7_SIZE);

    return inst;
}

inline uint32_t encode_syscall_function(uint32_t immediate, uint32_t opcode) {
    uint32_t inst = 0;

    inst = bitwise_place(inst, opcode, OPCODE_POS, OPCODE_SIZE);
    inst = bitwise_place(inst, immediate, IMM_ITYPE_POS, IMM_ITYPE_SIZE);

    return inst;
}

//---upper format instructions---//
const uint32_t LUI = encode_function(0, 0, LUI_OPCODE);
const uint32_t AUIPC = encode_function(0, 0, AUIPC_OPCODE);

//---jump format instructions---//
const uint32_t JAL = encode_function(0, 0, JAL_OPCODE);

//---branch format instructions---//
const uint32_t BEQ = encode_function(0, 0, BRANCH_OPCODE);
const uint32_t BNE = encode_function(0, 1, BRANCH_OPCODE);
const uint32_t BLT = encode_function(0, 4, BRANCH_OPCODE);
const uint32_t BGE = encode_function(0, 5, BRANCH_OPCODE);
const uint32_t BLTU = encode_function(0, 6, BRANCH_OPCODE);
const uint32_t BGEU = encode_function(0, 7, BRANCH_OPCODE);

//---store format instructions---//
const uint32_t SB = encode_function(0, 0, STORE_OPCODE);
const uint32_t SH = encode_function(0, 1, STORE_OPCODE);
const uint32_t SW = encode_function(0, 2, STORE_OPCODE);

//---immediate format instructions---//
const uint32_t JALR = encode_function(0, 0, JALR_OPCODE);

const uint32_t LB = encode_function(0, 0, LOAD_OPCODE);
const uint32_t LH = encode_function(0, 1, LOAD_OPCODE);
const uint32_t LW = encode_function(0, 2, LOAD_OPCODE);
const uint32_t LBU = encode_function(0, 4, LOAD_OPCODE);
const uint32_t LHU = encode_function(0, 5, LOAD_OPCODE);

const uint32_t ADDI = encode_function(0, 0, OP_IMM_OPCODE);
const uint32_t SLTI = encode_function(0, 2, OP_IMM_OPCODE);
const uint32_t SLTIU = encode_function(0, 3, OP_IMM_OPCODE);
const uint32_t XORI = encode_function(0, 4, OP_IMM_OPCODE);
const uint32_t ORI = encode_function(0, 6, OP_IMM_OPCODE);
const uint32_t ANDI = encode_function(0, 7, OP_IMM_OPCODE);

//---register (shamt) format  instructions---//
const uint32_t SLLI = encode_function(0, 1, OP_IMM_OPCODE);
const uint32_t SRLI = encode_function(0, 5, OP_IMM_OPCODE);
const uint32_t SRAI = encode_function(32, 5, OP_IMM_OPCODE);

//---register format instructions---//
const uint32_t ADD = encode_function(0, 0, OP_OPCODE);
const uint32_t SUB = encode_function(32, 0, OP_OPCODE);
const uint32_t SLL = encode_function(0, 1, OP_OPCODE);
const uint32_t SLT = encode_function(0, 2, OP_OPCODE);
const uint32_t SLTU = encode_function(0, 3, OP_OPCODE);
const uint32_t XOR = encode_function(0, 4, OP_OPCODE);
const uint32_t SRL = encode_function(0, 5, OP_OPCODE);
const uint32_t SRA = encode_function(32, 5, OP_OPCODE);
const uint32_t OR = encode_function(0, 6, OP_OPCODE);
const uint32_t AND = encode_function(0, 7, OP_OPCODE);

//---register format M instructions---//
const uint32_t MUL = encode_function(1, 0, OP_OPCODE);
const uint32_t MULH = encode_function(1, 1, OP_OPCODE);
const uint32_t MULHSU = encode_function(1, 2, OP_OPCODE);
const uint32_t MULHU = encode_function(1, 3, OP_OPCODE);
const uint32_t DIV = encode_function(1, 4, OP_OPCODE);
const uint32_t DIVU = encode_function(1, 5, OP_OPCODE);
const uint32_t REM = encode_function(1, 6, OP_OPCODE);
const uint32_t REMU = encode_function(1, 7, OP_OPCODE);

const uint32_t FENCE = encode_function(0, 0, MISC_MEM_OPCODE);
const uint32_t FENCEI = encode_function(0, 1, MISC_MEM_OPCODE);
const uint32_t CSRRW = encode_function(0, 1, SYSTEM_OPCODE);
const uint32_t CSRRS = encode_function(0, 2, SYSTEM_OPCODE);
const uint32_t CSRRC = encode_function(0, 3, SYSTEM_OPCODE);
const uint32_t CSRRWI = encode_function(0, 5, SYSTEM_OPCODE);
const uint32_t CSRRSI = encode_function(0, 6, SYSTEM_OPCODE);
const uint32_t CSRRCI = encode_function(0, 7, SYSTEM_OPCODE);

//---system instructions---//
const uint32_t ECALL = encode_syscall_function(0, SYSTEM_OPCODE);
const uint32_t EBREAK = encode_syscall_function(1, SYSTEM_OPCODE);
const uint32_t SRET = encode_syscall_function(0b000100000010, SYSTEM_OPCODE);
const uint32_t MRET = encode_syscall_function(0b001100000010, SYSTEM_OPCODE);
const uint32_t MNRET = encode_syscall_function(0b011100000010, SYSTEM_OPCODE);
const uint32_t WFI = encode_syscall_function(0b000100000101, SYSTEM_OPCODE);
const uint32_t SFENCEVMA = encode_syscall_function(0b000100100000, SYSTEM_OPCODE);

uint32_t place_rs1(uint32_t sr1, uint32_t inst);

uint32_t encode_register_inst(uint32_t inst, uint32_t rs1, uint32_t rs2, uint32_t rd);

uint32_t encode_branch_inst(uint32_t inst, uint32_t rs1, uint32_t rs2, uint32_t imm);

uint32_t encode_immediate_inst(uint32_t inst, uint32_t rs1, uint32_t imm, uint32_t rd);

uint32_t encode_store_inst(uint32_t inst, uint32_t rs1, uint32_t rs2, uint32_t imm);

uint32_t encode_upper_inst(uint32_t inst, uint32_t imm, uint32_t rd);

uint32_t encode_branch_inst(uint32_t inst, uint32_t rs1, uint32_t rs2, uint32_t imm);

uint32_t encode_jump_inst(uint32_t inst, uint32_t imm, uint32_t rd);

#endif
