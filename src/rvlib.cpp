#include "rvlib.hpp"
#include <cstdint>
#include <iostream>

uint32_t place_rs1(uint32_t rs1, uint32_t inst) {
    return bitwise_place(inst, rs1, RS1_POS, RS_SIZE);
}

uint32_t place_rs2(uint32_t rs2, uint32_t inst) {
    return bitwise_place(inst, rs2, RS2_POS, RS_SIZE);
}

uint32_t place_rd(uint32_t rd, uint32_t inst) { return bitwise_place(inst, rd, RD_POS, RS_SIZE); }

uint32_t place_imm_itype(uint32_t imm, uint32_t inst) {
    return bitwise_place(inst, imm, IMM_ITYPE_POS, IMM_ITYPE_SIZE);
}

uint32_t place_imm_stype(uint32_t imm, uint32_t inst) {
    const uint32_t imm40 = bitwise_range_select(imm, 4, 0);
    inst = bitwise_range_place(inst, imm40, 11, 7);

    const uint32_t imm115 = bitwise_range_select(imm, 11, 5);
    inst = bitwise_range_place(inst, imm115, 31, 25);

    return inst;
}

uint32_t place_imm_btype(uint32_t imm, uint32_t inst) {
    const uint32_t imm11 = bitwise_range_select(imm, 11, 11);
    inst = bitwise_range_place(inst, imm11, 7, 7);

    const uint32_t imm41 = bitwise_range_select(imm, 4, 1);
    inst = bitwise_range_place(inst, imm41, 11, 8);

    const uint32_t imm105 = bitwise_range_select(imm, 10, 5);
    inst = bitwise_range_place(inst, imm105, 30, 25);

    const uint32_t imm12 = bitwise_range_select(imm, 12, 12);
    inst = bitwise_range_place(inst, imm12, 31, 31);

    return inst;
}

uint32_t place_imm_utype(uint32_t imm, uint32_t inst) {
    inst = bitwise_range_place(inst, imm, 31, 12);
    return inst;
}

uint32_t place_imm_jtype(uint32_t imm, uint32_t inst) {
    const uint32_t imm1912 = bitwise_range_select(imm, 19, 12);
    inst = bitwise_range_place(inst, imm1912, 19, 12);

    const uint32_t imm11 = bitwise_range_select(imm, 11, 11);
    inst = bitwise_range_place(inst, imm11, 20, 20);

    const uint32_t imm101 = bitwise_range_select(imm, 10, 1);
    inst = bitwise_range_place(inst, imm101, 30, 21);

    const uint32_t imm20 = bitwise_range_select(imm, 20, 20);
    inst = bitwise_range_place(inst, imm20, 31, 31);

    return inst;
}

uint32_t encode_register_inst(uint32_t base, uint32_t rs1, uint32_t rs2, uint32_t rd) {
    uint32_t inst;

    inst = place_rs1(rs1, base);
    inst = place_rs2(rs2, inst);
    inst = place_rd(rd, inst);

    return inst;
}

uint32_t encode_immediate_inst(uint32_t base, uint32_t rs1, uint32_t imm, uint32_t rd) {
    uint32_t inst;

    inst = place_rs1(rs1, base);
    inst = place_rd(rd, inst);
    inst = place_imm_itype(imm, inst);

    return inst;
}

uint32_t encode_store_inst(uint32_t base, uint32_t rs1, uint32_t rs2, uint32_t imm) {
    uint32_t inst;

    inst = place_rs1(rs1, base);
    inst = place_rs2(rs2, inst);
    inst = place_imm_stype(imm, inst);

    return inst;
}

uint32_t encode_upper_inst(uint32_t base, uint32_t imm, uint32_t rd) {
    uint32_t inst;

    inst = place_rd(rd, base);
    inst = place_imm_utype(imm, inst);

    return inst;
}

uint32_t encode_branch_inst(uint32_t base, uint32_t rs1, uint32_t rs2, uint32_t imm) {
    uint32_t inst;

    inst = place_rs1(rs1, base);
    inst = place_rs2(rs2, inst);
    inst = place_imm_btype(imm, inst);

    return inst;
}

uint32_t encode_jump_inst(uint32_t base, uint32_t imm, uint32_t rd) {
    uint32_t inst;

    inst = place_rd(rd, base);
    inst = place_imm_jtype(imm, inst);

    return inst;
}
