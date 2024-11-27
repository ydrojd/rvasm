#include "assembler.hpp"
#include "elf_file.hpp"
#include "error_logger.hpp"
#include "program_options.hpp"
#include "rvlib.hpp"
#include "statement.hpp"
#include "unordered_map"
#include <array>
#include <cassert>
#include <cstdint>
#include <elf_types.hpp>
#include <sys/types.h>
#include <tuple>
#include <unordered_map>
#include <vector>

static std::tuple<uint32_t, syntax_error> assemble_instruction(statement stmnt, uint32_t instruction_address,
                                                               const program_options &options,
                                                               elf_file &object_file);
enum struct instruction_format_group {
    REGISTER = 0,
    REGISTER_SHAMT,
    IMMEDIATE,
    IMMEDIATE_LOAD,
    STORE,
    BRANCH,
    UPPER,
    JUMP,
    FENCE,
    SFENCE,
    SYS_CALL,
    CSR,
    CSRI,
};

const static std::unordered_map<instruction_id, std::tuple<instruction_format_group, uint32_t>>
        instruction_info_map{
                {instruction_id::LUI, {instruction_format_group::UPPER, LUI}},
                {instruction_id::AUIPC, {instruction_format_group::UPPER, AUIPC}},

                {instruction_id::JAL, {instruction_format_group::JUMP, JAL}},

                {instruction_id::BEQ, {instruction_format_group::BRANCH, BEQ}},
                {instruction_id::BNE, {instruction_format_group::BRANCH, BNE}},
                {instruction_id::BLT, {instruction_format_group::BRANCH, BLT}},
                {instruction_id::BGE, {instruction_format_group::BRANCH, BGE}},
                {instruction_id::BLTU, {instruction_format_group::BRANCH, BLTU}},
                {instruction_id::BGEU, {instruction_format_group::BRANCH, BGEU}},

                {instruction_id::SB, {instruction_format_group::STORE, SB}},
                {instruction_id::SH, {instruction_format_group::STORE, SH}},
                {instruction_id::SW, {instruction_format_group::STORE, SW}},

                {instruction_id::JALR, {instruction_format_group::IMMEDIATE, JALR}},
                {instruction_id::LB, {instruction_format_group::IMMEDIATE_LOAD, LB}},
                {instruction_id::LH, {instruction_format_group::IMMEDIATE_LOAD, LH}},
                {instruction_id::LW, {instruction_format_group::IMMEDIATE_LOAD, LW}},
                {instruction_id::LBU, {instruction_format_group::IMMEDIATE_LOAD, LBU}},
                {instruction_id::LHU, {instruction_format_group::IMMEDIATE_LOAD, LHU}},
                {instruction_id::ADDI, {instruction_format_group::IMMEDIATE, ADDI}},
                {instruction_id::SLTI, {instruction_format_group::IMMEDIATE, SLTI}},
                {instruction_id::SLTIU, {instruction_format_group::IMMEDIATE, SLTIU}},
                {instruction_id::XORI, {instruction_format_group::IMMEDIATE, XORI}},
                {instruction_id::ORI, {instruction_format_group::IMMEDIATE, ORI}},
                {instruction_id::ANDI, {instruction_format_group::IMMEDIATE, ANDI}},

                {instruction_id::SLLI, {instruction_format_group::REGISTER_SHAMT, SLLI}},
                {instruction_id::SRLI, {instruction_format_group::REGISTER_SHAMT, SRLI}},
                {instruction_id::SRAI, {instruction_format_group::REGISTER_SHAMT, SRAI}},

                {instruction_id::ADD, {instruction_format_group::REGISTER, ADD}},
                {instruction_id::SUB, {instruction_format_group::REGISTER, SUB}},
                {instruction_id::SLL, {instruction_format_group::REGISTER, SLL}},
                {instruction_id::SLT, {instruction_format_group::REGISTER, SLT}},
                {instruction_id::SLTU, {instruction_format_group::REGISTER, SLTU}},
                {instruction_id::XOR, {instruction_format_group::REGISTER, XOR}},
                {instruction_id::SRL, {instruction_format_group::REGISTER, SRL}},
                {instruction_id::SRA, {instruction_format_group::REGISTER, SRA}},
                {instruction_id::AND, {instruction_format_group::REGISTER, AND}},
                {instruction_id::OR, {instruction_format_group::REGISTER, OR}},
                {instruction_id::MUL, {instruction_format_group::REGISTER, MUL}},
                {instruction_id::MULH, {instruction_format_group::REGISTER, MULH}},
                {instruction_id::MULHSU, {instruction_format_group::REGISTER, MULHSU}},
                {instruction_id::DIV, {instruction_format_group::REGISTER, DIV}},
                {instruction_id::DIVU, {instruction_format_group::REGISTER, DIVU}},
                {instruction_id::REM, {instruction_format_group::REGISTER, REM}},
                {instruction_id::REMU, {instruction_format_group::REGISTER, REMU}},

                {instruction_id::FENCE, {instruction_format_group::FENCE, FENCE}},
                {instruction_id::FENCEI, {instruction_format_group::FENCE, FENCEI}},
                {instruction_id::SFENCEVMA, {instruction_format_group::SFENCE, SFENCEVMA}},

                {instruction_id::ECALL, {instruction_format_group::SYS_CALL, ECALL}},
                {instruction_id::EBREAK, {instruction_format_group::SYS_CALL, EBREAK}},
                {instruction_id::SRET, {instruction_format_group::SYS_CALL, SRET}},
                {instruction_id::MRET, {instruction_format_group::SYS_CALL, MRET}},
                {instruction_id::MNRET, {instruction_format_group::SYS_CALL, MNRET}},
                {instruction_id::WFI, {instruction_format_group::SYS_CALL, WFI}},

                {instruction_id::CSRRW, {instruction_format_group::CSR, CSRRW}},
                {instruction_id::CSRRS, {instruction_format_group::CSR, CSRRS}},
                {instruction_id::CSRRC, {instruction_format_group::CSR, CSRRC}},
                {instruction_id::CSRRWI, {instruction_format_group::CSRI, CSRRWI}},
                {instruction_id::CSRRSI, {instruction_format_group::CSRI, CSRRSI}},
                {instruction_id::CSRRCI, {instruction_format_group::CSRI, CSRRCI}},
        };

void assemble(const std::vector<statement> &stmnts, elf_file &object_file, const program_options &options,
              std::vector<syntax_error> &errors) {
    uint32_t active_section_idx = 0;
    std::vector<std::string> globals;
    std::unordered_map<std::string, counter> counters;
    std::unordered_map<std::string, counter>::iterator active_counter;

    //---insert .text first---//
    active_section_idx = object_file.add_section(".text");
    active_counter = counters.insert({".text", {}}).first;

    //---pass 1 assemble: build symbol table---//
    for (auto &stmnt: stmnts) {
        memory_allocation allocation{.p2allign = 0, .nbytes = 0};
        symbol_type label_type = symbol_type::undefined;
        switch (stmnt.type) {
            case statement::types::directive:
                label_type = symbol_type::data;
                switch (stmnt.directive) {
                    case directive_id::SECTION: {
                        if (stmnt.args.size() == 1 && stmnt.args[0].is_identifier()) {
                            const auto &section_identifier = stmnt.args[0].str_val;
                            active_section_idx = object_file.get_section_id(section_identifier);
                            if (active_section_idx == 0) {
                                active_section_idx = object_file.add_section(section_identifier);
                                active_counter = counters.insert({section_identifier, {}}).first;
                            } else {
                                active_counter = counters.find(section_identifier);
                            }

                        } else {
                            errors.push_back(make_error(syntax_error::ids::illegal_operands, stmnt.line));
                        }
                    } break;

                    case directive_id::GLOBL: {
                        if (stmnt.args.size() == 1 && stmnt.args[0].is_identifier()) {
                            const auto &identifier = stmnt.args[0].str_val;
                            globals.push_back(identifier);
                        } else {
                            errors.push_back(make_error(syntax_error::ids::illegal_operands, stmnt.line));
                        }
                        break;
                    }

                    case directive_id::P2ALIGN:
                        if (stmnt.args.size() == 1 && stmnt.args[0].is_integer() == false) {
                            errors.push_back(make_error(syntax_error::ids::illegal_operands, stmnt.line));
                            break;
                        }

                        allocation = {.p2allign = stmnt.args[0].int_val, .nbytes = 0};
                        break;

                    case directive_id::WORD: {
                        const uint32_t nwords = stmnt.args.size() == 0 ? 1 : stmnt.args.size();
                        allocation = {.p2allign = 2, .nbytes = 4 * nwords};
                        break;
                    }

                    case directive_id::HALF: {
                        const uint32_t nhalfwords = stmnt.args.size() == 0 ? 1 : stmnt.args.size();
                        allocation = {.p2allign = 1, .nbytes = 2 * nhalfwords};
                        break;
                    }

                    case directive_id::ZERO: {
                        uint32_t n = 1;
                        if (stmnt.args.empty() == false) {
                            if (stmnt.args.size() != 1 || stmnt.args[0].is_integer() == false) {
                                errors.push_back(make_error(syntax_error::ids::illegal_operands, stmnt.line));
                                break;
                            }

                            n = stmnt.args[0].int_val;
                        }

                        allocation = {.p2allign = 0, .nbytes = n};
                    } break;

                    case directive_id::BYTE: {
                        const uint32_t nbytes = stmnt.args.size() == 0 ? 1 : stmnt.args.size();
                        allocation = {.p2allign = 0, .nbytes = nbytes};
                        break;
                    }

                    case directive_id::STRING: {
                        if (stmnt.args.size() == 1 && stmnt.args[0].is_string() == false) {
                            errors.push_back(make_error(syntax_error::ids::illegal_operands, stmnt.line));
                            break;
                        }

                        const uint32_t nbytes = stmnt.args[0].str_val.size() + 1;// add 1 for terminating zero
                        allocation = {.p2allign = 0, .nbytes = nbytes};
                    } break;

                    case directive_id::DATA:
                    case directive_id::TEXT:
                    case directive_id::BSS:
                    case directive_id::RODATA:
                    case directive_id::MACRO:
                    case directive_id::ENDM:
                        assert(!"unreachable");
                        break;

                    case directive_id::EQU:
                        break;
                }

                break;

            case statement::types::instruction:
                label_type = symbol_type::fun;
                allocation = {.p2allign = 2, .nbytes = 4};
                break;

            case statement::types::unkown_operation:
            case statement::types::pseudo:
                assert(!"unreachable");
                break;
        }

        uint32_t placement_address = 0;
        if (active_counter != counters.end())
            placement_address = active_counter->second.increment(allocation);

        for (auto &label: stmnt.labels) {
            const symbol sym = {.identifier = label.name,
                                .index = 0,
                                .section_id = active_section_idx,
                                .address = placement_address,
                                .scope = symbol_scope::local,
                                .type = label_type,
                                .size = allocation.nbytes};
            const bool insert_success = object_file.insert_symbol(sym);
            if (insert_success == false)
                errors.push_back(make_error(syntax_error::ids::duplicate_identifier, stmnt.line));
        }
    }

    for (auto &global_identifier: globals)
        object_file.set_global(global_identifier);

    //---pass 2 assemble: compile---//
    for (auto &counter: counters)
        counter.second = {};//reset counters

    active_counter = counters.find(".text");
    object_file.set_active_section(".text");

    for (auto &stmnt: stmnts) {
        memory_allocation allocation{.p2allign = 0, .nbytes = 0};
        switch (stmnt.type) {
            case statement::types::directive:
                switch (stmnt.directive) {
                    case directive_id::SECTION: {
                        const auto &section_identifier = stmnt.args[0].str_val;
                        object_file.set_active_section(section_identifier);
                        active_counter = counters.find(section_identifier);
                        assert(active_counter != counters.end());
                    } break;

                    case directive_id::P2ALIGN:
                        assert(stmnt.args.size() == 1 && stmnt.args[0].is_integer());
                        allocation = {.p2allign = stmnt.args[0].int_val, .nbytes = 0};
                        break;

                    case directive_id::WORD: {
                        object_file.p2align(2);

                        if (stmnt.args.empty())
                            object_file.insert_data((uint32_t) 0);

                        for (auto &arg: stmnt.args) {
                            if (arg.is_integer() == false) {
                                errors.push_back(
                                        make_error(syntax_error::ids::duplicate_identifier, stmnt.line));
                                break;
                            }
                            object_file.insert_data((uint32_t) arg.int_val);
                        }

                        const uint32_t n = stmnt.args.size() == 0 ? 1 : stmnt.args.size();
                        allocation = {.p2allign = 2, .nbytes = 4 * n};
                    } break;

                    case directive_id::HALF: {
                        object_file.p2align(1);

                        if (stmnt.args.empty())
                            object_file.insert_data((uint16_t) 0);

                        for (auto &arg: stmnt.args) {
                            if (arg.is_integer() == false) {
                                errors.push_back(
                                        make_error(syntax_error::ids::duplicate_identifier, stmnt.line));
                                break;
                            }
                            object_file.insert_data((uint16_t) arg.int_val);
                        }

                        const uint32_t n = stmnt.args.size() == 0 ? 1 : stmnt.args.size();
                        allocation = {.p2allign = 1, .nbytes = 2 * n};
                    } break;

                    case directive_id::BYTE: {
                        object_file.p2align(0);

                        if (stmnt.args.empty())
                            object_file.insert_data((uint8_t) 0);

                        for (auto &arg: stmnt.args) {
                            if (arg.is_integer() == false) {
                                errors.push_back(
                                        make_error(syntax_error::ids::duplicate_identifier, stmnt.line));
                                break;
                            }

                            object_file.insert_data((uint8_t) arg.int_val);
                        }

                        const uint32_t n = stmnt.args.size() == 0 ? 1 : stmnt.args.size();
                        allocation = {.p2allign = 0, .nbytes = n};
                    } break;

                    case directive_id::ZERO: {
                        object_file.p2align(0);

                        uint32_t n = 1;
                        if (stmnt.args.empty() == false) {
                            assert(stmnt.args[0].is_integer());
                            n = stmnt.args[0].int_val;
                        }

                        for (uint32_t i = 0; i < n; ++i)
                            object_file.insert_data((uint8_t) 0);

                        allocation = {.p2allign = 0, .nbytes = n};
                    } break;

                    case directive_id::STRING: {
                        assert(stmnt.args.size() == 1 && stmnt.args[0].is_string());

                        for (char c: stmnt.args[0].str_val)
                            object_file.insert_data((uint8_t) c);

                        object_file.insert_data((uint32_t) 0);

                        const uint32_t n = stmnt.args[0].str_val.size() + 1;// add 1 for terminating zero
                        allocation = {.p2allign = 0, .nbytes = n};
                    } break;

                    case directive_id::DATA:
                    case directive_id::TEXT:
                    case directive_id::BSS:
                    case directive_id::RODATA:
                    case directive_id::MACRO:
                    case directive_id::ENDM:
                        assert(!"unreachable");
                        break;

                    case directive_id::EQU:
                        break;

                    default:
                        break;
                } break;

            case statement::types::instruction: {
                allocation = {.p2allign = 2, .nbytes = 4};

                const auto placement_address = active_counter->second.allign(2);
                const auto [instruction, encode_error] =
                        assemble_instruction(stmnt, placement_address, options, object_file);

                if (encode_error.is_error()) {
                    errors.push_back(encode_error);
                    break;
                }

                object_file.push_data((uint32_t) instruction);
            } break;

            case statement::types::unkown_operation:
            case statement::types::pseudo:
                assert(!"unknown operation!");
		break;
        }

        active_counter->second.increment(allocation);
    }
}

static uint8_t parse_fence_options(const std::string &str, bool &error) {
    uint8_t val = 0;
    for (const auto c: str)
        switch (c) {
            case 'i':
                val += 1 << 3;
                break;
            case 'o':
                val += 1 << 2;
                break;
            case 'r':
                val += 1 << 1;
                break;
            case 'w':
                val += 1 << 0;
                break;
            default:
                error = true;
        }

    return val;
};

static std::pair<uint32_t, bool> try_parse_csr(const std::string &str) {
    const static std::vector<std::pair<std::string, uint32_t>> csr_names = {
            {"cycle", 0x0C00},  {"time", 0x0C01},  {"instret", 0x0C02},
            {"cycleh", 0x0C80}, {"timeh", 0x0C81}, {"instreth", 0x0C82}};

    for (const auto &csr: csr_names)
        if (csr.first == str)
            return {csr.second, true};

    return {0, false};
}

static std::tuple<uint32_t, syntax_error> assemble_instruction(statement stmnt, uint32_t instruction_address,
                                                               const program_options &options,
                                                               elf_file &object_file) {
    const static expression zero_imm = {.type = expression::types::integer, .int_val = (uint32_t) 0};

    assert(stmnt.type == statement::types::instruction);
    auto &args = stmnt.args;

    const auto it = instruction_info_map.find(stmnt.instruction);
    assert(it != instruction_info_map.end());
    const auto [format_group, operation_encoding] = it->second;

    uint32_t instruction = 0;
    switch (format_group) {
        case instruction_format_group::STORE: {
            if (args.size() == 2)
                stmnt.args = {args[0], zero_imm, args[1]};

            if (args.size() != 3 || !args[0].is_reg() || !args[2].is_reg())
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

            uint32_t int_val = 0;
            if (args[1].is_relocated_addressing()) {
                const auto [reloc, symbol_identifier, addend] = args[1].get_relocated_addressing();
                int_val = addend;

                riscv_relocation relocation;
                if (reloc == expression::reloc_functions::lo) {
                    relocation = riscv_relocation::R_RISCV_LO12_S;
                } else if (reloc == expression::reloc_functions::pcrel_lo) {
                    relocation = riscv_relocation::R_RISCV_PCREL_LO12_S;
                } else {
                    return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};
                }

                if (symbol_identifier.empty() == false) {
                    object_file.insert_relocation(symbol_identifier, instruction_address, relocation, addend);
                    if (options.use_relaxation)
                        object_file.insert_relaxation(instruction_address, addend);
                }

            } else if (args[1].is_integer()) {
                int_val = args[1].int_val;
            } else {
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};
            }

            instruction = encode_store_inst(operation_encoding, args[2].int_val, args[0].int_val, int_val);
        } break;

        case instruction_format_group::IMMEDIATE: {
            if (stmnt.instruction == instruction_id::JALR)
                if (args.size() == 3 && args[0].is_reg() && args[2].is_reg())
                    args = {args[0], args[2], args[1]};

            if (args.size() != 3 || !args[0].is_reg() || !args[1].is_reg())
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

            uint32_t immediate = 0;
            if (args[2].is_integer()) {
                immediate = args[2].int_val;
            } else if (args[2].is_relocated_addressing()) {
                const auto [reloc, symbol_identifier, addend] = args[2].get_relocated_addressing();
                immediate = addend;

                riscv_relocation relocation;
                if (reloc == expression::reloc_functions::lo) {
                    relocation = riscv_relocation::R_RISCV_LO12_I;
                } else if (reloc == expression::reloc_functions::pcrel_lo) {
                    relocation = riscv_relocation::R_RISCV_PCREL_LO12_I;
                } else
                    return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

                if (symbol_identifier.empty() == false) {
                    object_file.insert_relocation(symbol_identifier, instruction_address, relocation, addend);
                    if (options.use_relaxation)
                        object_file.insert_relaxation(instruction_address, addend);
                }
            }

            instruction =
                    encode_immediate_inst(operation_encoding, args[1].int_val, immediate, args[0].int_val);
        } break;

        case instruction_format_group::IMMEDIATE_LOAD: {
            if (args.size() == 2)
                stmnt.args = {args[0], zero_imm, args[1]};

            if (args.size() != 3 || !args[0].is_reg() || !args[2].is_reg())
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

            uint32_t immediate = 0;
            if (args[1].is_integer()) {
                immediate = args[1].int_val;
            } else if (args[1].is_relocated_addressing()) {
                const auto [reloc, symbol_identifier, addend] = args[1].get_relocated_addressing();
                immediate = addend;

                riscv_relocation relocation;
                if (reloc == expression::reloc_functions::lo) {
                    relocation = riscv_relocation::R_RISCV_LO12_I;
                } else if (reloc == expression::reloc_functions::pcrel_lo) {
                    relocation = riscv_relocation::R_RISCV_PCREL_LO12_I;
                } else
                    return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

                if (symbol_identifier.empty() == false) {
                    object_file.insert_relocation(symbol_identifier, instruction_address, relocation, addend);
                    if (options.use_relaxation)
                        object_file.insert_relaxation(instruction_address, addend);
                }
            } else
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

            instruction =
                    encode_immediate_inst(operation_encoding, args[2].int_val, immediate, args[0].int_val);
        } break;

        case instruction_format_group::UPPER: {
            if (args.size() != 2 || !args[0].is_reg())
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

            uint32_t immediate = 0;
            if (args[1].is_integer()) {
                immediate = args[1].int_val;
            } else if (args[1].is_relocated_addressing()) {
                const auto [reloc, symbol_identifier, addend] = args[1].get_relocated_addressing();

                riscv_relocation relocation;
                if (reloc == expression::reloc_functions::hi) {
                    const bool lo_is_negative = addend & (1 << 11);
                    immediate = (addend >> 12) + lo_is_negative;
                    relocation = riscv_relocation::R_RISCV_HI20;

                } else if (reloc == expression::reloc_functions::pcrel_hi) {
                    relocation = riscv_relocation::R_RISCV_PCREL_HI20;
                    if (symbol_identifier.empty())
                        object_file.insert_relocation(instruction_address, relocation, addend);

                } else if (reloc == expression::reloc_functions::call) {
                    relocation = riscv_relocation::R_RISCV_CALL_PLT;
                    if (symbol_identifier.empty())
                        object_file.insert_relocation(instruction_address, relocation, addend);

                } else {
                    return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};
                }

                if (symbol_identifier.empty() == false) {
                    object_file.insert_relocation(symbol_identifier, instruction_address, relocation, addend);
                    if (options.use_relaxation)
                        object_file.insert_relaxation(instruction_address, addend);
                }
            } else
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

            instruction = encode_upper_inst(operation_encoding, immediate, args[0].int_val);
        } break;

        case instruction_format_group::JUMP: {
            if (args.size() != 2 || !args[0].is_reg() || !args[1].is_addressing())
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

            const auto [symbol_identifier, addend] = args[1].get_addressing();
            uint32_t immediate = 0;
            if (symbol_identifier.empty()) {
                object_file.insert_relocation(instruction_address, riscv_relocation::R_RISCV_JAL, addend);
            } else {
                auto sym = object_file.insert_relocation(symbol_identifier, instruction_address,
                                                         riscv_relocation::R_RISCV_JAL, addend);

                immediate = (sym.address + addend) - instruction_address;
            }

            instruction = encode_jump_inst(operation_encoding, immediate, args[0].int_val);
        } break;

        case instruction_format_group::BRANCH: {
            if (args.size() != 3 || !args[0].is_reg() || !args[1].is_reg() || !args[2].is_addressing())
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

            const auto [symbol_identifier, addend] = args[2].get_addressing();

            uint32_t immediate = 0;
            if (symbol_identifier.empty()) {
                object_file.insert_relocation(instruction_address, riscv_relocation::R_RISCV_BRANCH, addend);
            } else {
                const auto &sym = object_file.insert_relocation(symbol_identifier, instruction_address,
                                                                riscv_relocation::R_RISCV_BRANCH, addend);
                immediate = (sym.address + addend) - instruction_address;
            }

            instruction = encode_branch_inst(operation_encoding, args[0].int_val, args[1].int_val, immediate);
        } break;

        case instruction_format_group::REGISTER: {
            if (args.size() != 3 || !args[0].is_reg() || !args[1].is_reg() || !args[2].is_reg())
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

            instruction = encode_register_inst(operation_encoding, args[1].int_val, args[2].int_val,
                                               args[0].int_val);
        } break;

        case instruction_format_group::REGISTER_SHAMT: {
            if (args.size() != 3 || !args[0].is_reg() || !args[1].is_reg() || !args[2].is_integer())
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};
            instruction = encode_register_inst(operation_encoding, args[1].int_val, args[2].int_val,
                                               args[0].int_val);
        } break;

        case instruction_format_group::SYS_CALL: {
            if (args.size() != 0)
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

            instruction = operation_encoding;
        } break;

        case instruction_format_group::FENCE: {
            uint32_t immediate = 0;
            if (args.size() == 2 && args[0].is_identifier() && args[1].is_identifier()) {
                bool error = false;
                immediate = parse_fence_options(args[1].str_val, error) |
                            parse_fence_options(args[0].str_val, error) << 4;
                if (error)
                    return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};
            } else if (args.empty()) {
                immediate = 0x0FF;
                if (stmnt.instruction == instruction_id::FENCEI)
                    immediate = 0;

            } else {
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};
            }

            instruction = encode_immediate_inst(operation_encoding, 0, immediate, 0);
        } break;

        case instruction_format_group::SFENCE: {
            if (args.size() != 2 || !args[0].is_reg() || !args[1].is_reg())
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

            instruction = encode_register_inst(operation_encoding, args[0].int_val, args[1].int_val, 0);
        } break;

        case instruction_format_group::CSR: {
            if (args.size() != 3 || !args[0].is_reg() || !args[2].is_reg())
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

            uint32_t immediate = 0;
            if (args[1].is_identifier()) {
                const auto [addr, success] = try_parse_csr(args[1].str_val);
                if (success == false)
                    return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

                immediate = addr;
            } else if (args[1].is_integer()) {
                immediate = args[1].int_val;
            } else {
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};
            }

            instruction =
                    encode_immediate_inst(operation_encoding, args[2].int_val, immediate, args[0].int_val);
        } break;

        case instruction_format_group::CSRI: {
            if (args.size() != 3 || !args[0].is_reg() || !args[2].is_integer() || args[2].int_val >= (1 << 5))
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

            uint32_t immediate = 0;
            if (args[1].is_identifier()) {
                auto [addr, success] = try_parse_csr(args[1].str_val);
                if (success == false)
                    return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

                immediate = addr;
            } else if (args[1].is_integer()) {
                immediate = args[1].int_val;
            } else {
                return {0, make_error(syntax_error::ids::illegal_operands, stmnt.line)};
            }

            instruction =
                    encode_immediate_inst(operation_encoding, args[2].int_val, immediate, args[0].int_val);
        } break;

        default:
            break;
    }
    return {instruction, {}};
};
