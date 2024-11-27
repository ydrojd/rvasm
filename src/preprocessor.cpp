#include "preprocessor.hpp"
#include "error_logger.hpp"
#include "statement.hpp"
#include <cassert>
#include <cstdint>
#include <elf_types.hpp>
#include <format>
#include <string>
#include <unordered_map>
#include <vector>

const expression zero_reg = {.type = expression::types::reg, .int_val = 0};
const expression ra_reg = {.type = expression::types::reg, .int_val = 1};
const expression six_reg = {.type = expression::types::reg, .int_val = 6};
const expression zero_imm = {.type = expression::types::integer, .int_val = (uint32_t) 0};
const expression one_imm = {.type = expression::types::integer, .int_val = (uint32_t) 1};
const expression minone_imm = {.type = expression::types::integer, .int_val = (uint32_t) -1};

static syntax_error convert_pseudo_instruction(statement &stmnt, std::vector<statement> &output_statements,
                                               uint32_t &L0_counter);

static std::vector<statement> macro_processor(std::vector<statement> &input_stmnts,
                                              std::vector<syntax_error> &errors);

std::vector<statement> preprocess(std::vector<statement> &&input_stmnts, std::vector<syntax_error> &errors) {
    std::vector<statement> output_stmnts;
    std::unordered_map<std::string_view, expression> constants;

    std::unordered_map<std::string, int> local_labels;
    uint32_t L0_counter = 0;

    input_stmnts = macro_processor(input_stmnts, errors);

    for (auto &stmnt: input_stmnts) {
        //---forward and backward labels---//
        for (auto &label: stmnt.labels)
            if (label.is_int_label) {
                auto it = local_labels.find(label.name);
                if (it != local_labels.end()) {
                    it->second++;
                } else {
                    it = local_labels.insert({label.name, 1}).first;
                }

                const auto num = it->second;
                label.name = std::format(".L{}^B{}", label.name, num);
            }

        //---identifier exprs---//
        itererate_expressions(stmnt.args, [&](expression &expr) {
            if (expr.is_identifier()) {
                //---macro constant---//
                const auto it = constants.find(expr.str_val);
                if (it != constants.end())
                    expr = it->second;

            } else if (expr.is_integer_identifier()) {
                assert(expr.str_val.size() >= 2);
                if (expr.str_val.back() == 'b') {
                    //---backward reference---//
                    expr.str_val = expr.str_val.substr(0, expr.str_val.length() - 1);
                    auto it = local_labels.find(expr.str_val);
                    if (it == local_labels.end()) {
                        errors.push_back(make_error(syntax_error::ids::unknown_backward_ref, stmnt.line));
                    } else {
                        const int id_num = it->second;
                        expr.str_val = std::format(".L{}^B{}", expr.str_val, id_num);
                        expr.type = expression::types::identifier;
                    }

                } else if (expr.str_val.back() == 'f') {
                    //---forward reference---//
                    expr.str_val = expr.str_val.substr(0, expr.str_val.length() - 1);
                    int id_num = 1;
                    auto it = local_labels.find(expr.str_val);
                    if (it != local_labels.end())
                        id_num = it->second + 1;

                    expr.str_val = std::format(".L{}^B{}", expr.str_val, id_num);
                    expr.type = expression::types::identifier;
                }
            }
        });

        switch (stmnt.type) {
            case statement::types::directive:
                switch (stmnt.directive) {
                    case directive_id::TEXT: {
                        stmnt.directive = directive_id::SECTION;
                        stmnt.args = {{.type = expression::types::identifier, .str_val = ".text"}};
                    } break;

                    case directive_id::DATA:
                        stmnt.directive = directive_id::SECTION;
                        stmnt.args = {{.type = expression::types::identifier, .str_val = ".data"}};
                        break;

                    case directive_id::BSS:
                        stmnt.directive = directive_id::SECTION;
                        stmnt.args = {{.type = expression::types::identifier, .str_val = ".bss"}};
                        break;

                    case directive_id::RODATA:
                        stmnt.directive = directive_id::SECTION;
                        stmnt.args = {{.type = expression::types::identifier, .str_val = ".rodata"}};
                        break;

                    case directive_id::EQU:
                        if (stmnt.args.size() == 2 && stmnt.args[0].is_identifier()) {
                            const auto &symbol = stmnt.args[0].str_val;
                            const auto &value = stmnt.args[1];
                            constants.insert({symbol, value});
                        } else {
                            errors.push_back(make_error(syntax_error::ids::illegal_operands, stmnt.line));
                        }
                        break;

                    default:
                        break;
                }

                output_stmnts.push_back(std::move(stmnt));
                break;

            case statement::types::pseudo: {
                const auto error = convert_pseudo_instruction(stmnt, output_stmnts, L0_counter);
                if (error.is_error())
                    errors.push_back(error);

            } break;

            case statement::types::instruction: {
                switch (stmnt.instruction) {
                    case (instruction_id::JAL):
                        if (stmnt.args.size() == 1 && stmnt.args[0].is_identifier())
                            stmnt.args = {ra_reg, stmnt.args[0]};
                        break;

                    case (instruction_id::JALR):
                        if (stmnt.args.size() == 1 && stmnt.args[0].is_reg())
                            stmnt.args = {ra_reg, stmnt.args[0], zero_imm};
                        else if (stmnt.args.size() == 2 && stmnt.args[0].is_reg() && stmnt.args[1].is_reg())
                            stmnt.args = {stmnt.args[0], stmnt.args[1], zero_imm};
                        break;

                    case (instruction_id::SW):
                    case (instruction_id::SH):
                    case (instruction_id::SB): {
                        if (stmnt.args.size() == 3 && stmnt.args[0].is_reg() &&
                            stmnt.args[1].is_identifier_addressing() && stmnt.args[2].is_reg()) {

                            statement first_stmnt;
                            first_stmnt.line = stmnt.line;
                            first_stmnt.labels = std::move(stmnt.labels);
                            first_stmnt.type = statement::types::instruction;
                            first_stmnt.instruction = instruction_id::AUIPC;

                            expression hi_expr = {.sub_expressions = {stmnt.args[1]},
                                                  .type = expression::types::reloc,
                                                  .reloc_val = expression::reloc_functions::pcrel_hi};

                            first_stmnt.args = {stmnt.args[2], std::move(hi_expr)};
                            std::string label_name = std::format(".L0^{}", ++L0_counter);
                            first_stmnt.labels.push_back(label_name);
                            output_stmnts.push_back(std::move(first_stmnt));

                            expression lo_expr = {.sub_expressions = {{.type = expression::types::identifier,
                                                                       .str_val = std::move(label_name)}},
                                                  .type = expression::types::reloc,
                                                  .reloc_val = expression::reloc_functions::pcrel_lo};

                            stmnt.args = {stmnt.args[0], lo_expr, stmnt.args[2]};
                            stmnt.labels = {};
                        }
                    } break;

                    case (instruction_id::LW):
                    case (instruction_id::LH):
                    case (instruction_id::LHU):
                    case (instruction_id::LB):
                    case (instruction_id::LBU): {
                        if (stmnt.args.size() == 2 && stmnt.args[0].is_reg() &&
                            stmnt.args[1].is_identifier_addressing()) {

                            statement first_stmnt;
                            first_stmnt.line = stmnt.line;
                            first_stmnt.labels = std::move(stmnt.labels);
                            first_stmnt.type = statement::types::instruction;
                            first_stmnt.instruction = instruction_id::AUIPC;

                            expression hi_expr = {.sub_expressions = {stmnt.args[1]},
                                                  .type = expression::types::reloc,
                                                  .reloc_val = expression::reloc_functions::pcrel_hi};

                            first_stmnt.args = {stmnt.args[0], hi_expr};
                            std::string label_name = std::format(".L0^{}", ++L0_counter);
                            first_stmnt.labels.push_back(label_name);

                            output_stmnts.push_back(std::move(first_stmnt));

                            expression lo_expr = {.sub_expressions = {{.type = expression::types::identifier,
                                                                       .str_val = std::move(label_name)}},
                                                  .type = expression::types::reloc,
                                                  .reloc_val = expression::reloc_functions::pcrel_lo};

                            stmnt.args = {stmnt.args[0], lo_expr, stmnt.args[0]};
                        }
                    } break;

                    default:
                        break;
                }

                output_stmnts.push_back(std::move(stmnt));
            } break;

            default:
                output_stmnts.push_back(std::move(stmnt));
                break;
        }
    }

    return output_stmnts;
}

struct macro_definition {
    std::vector<statement>::iterator begin;
    std::vector<statement>::iterator end;
    std::vector<std::string_view> paramater_names;
};

struct macro_reference {
    std::vector<statement>::iterator current;
    std::vector<statement>::iterator end;
    std::vector<std::pair<std::string_view, expression>> arguments;
    std::vector<label> labels;
    bool inserted_labels = false;
};

static std::vector<statement> macro_processor(std::vector<statement> &input_stmnts,
                                              std::vector<syntax_error> &errors) {

    std::vector<statement> output_statements;
    std::unordered_map<std::string_view, macro_definition> macro_definitions;
    std::vector<macro_reference> macro_stack;
    auto input_stmnts_it = input_stmnts.begin();

    auto forward_stmnt = [&]() {
        if (macro_stack.empty()) {
            ++input_stmnts_it;
        } else {
            ++macro_stack.back().current;
            if (macro_stack.back().current == macro_stack.back().end)
                macro_stack.pop_back();
        }
    };

    auto get_stmnt_it = [&]() {
        if (macro_stack.empty())
            return input_stmnts_it;
        else
            return macro_stack.back().current;
    };

    do {
        auto stmnt_it = get_stmnt_it();

        //---macro definition---//
        if (stmnt_it->type == statement::types::directive && stmnt_it->directive == directive_id::MACRO) {
            macro_definition new_macro;
            bool has_identifier_args = true;
            for (auto &arg: stmnt_it->args)
                if (arg.is_identifier() == false)
                    has_identifier_args = false;

            if (stmnt_it->args.empty() || !has_identifier_args) {
                errors.push_back(make_error(syntax_error::ids::illegal_operands, stmnt_it->line));
                return {};
            }

            auto &macro_name = stmnt_it->args[0].str_val;
            for (unsigned long i = 1; i < stmnt_it->args.size(); ++i)
                new_macro.paramater_names.push_back(stmnt_it->args[i].str_val);

            forward_stmnt();
            new_macro.begin = get_stmnt_it();

            int macro_def_count = 1;
            do {
                stmnt_it = get_stmnt_it();
                if (stmnt_it == input_stmnts.end()) {
                    errors.push_back(make_error(syntax_error::ids::illegal_operands, stmnt_it->line));
                    return {};
                }

                if (stmnt_it->type == statement::types::directive) {
                    if (stmnt_it->directive == directive_id::ENDM)
                        --macro_def_count;
                    if (stmnt_it->directive == directive_id::MACRO)
                        ++macro_def_count;
                }

                if (macro_def_count == 0)
                    break;

                forward_stmnt();
            } while (true);

            new_macro.end = stmnt_it;
            macro_definitions.insert({macro_name, new_macro});
            forward_stmnt();

        } else {
            auto stmnt = *get_stmnt_it();

            //---macro identifiers---//
            if (macro_stack.empty() == false) {
                itererate_expressions(stmnt.args, expression::types::macro_identifier, [&](expression &expr) {
                    bool found = false;
                    for (auto &macro_arg: macro_stack.back().arguments)
                        if (macro_arg.first == expr.str_val) {
                            expr = std::get<expression>(macro_arg);
                            found = true;
                            break;
                        }

                    if (found == false)
                        errors.push_back(make_error(syntax_error::ids::illegal_operands, stmnt.line));
                });

                if (macro_stack.back().inserted_labels == false) {
                    auto &macro_ref = macro_stack.back();
                    stmnt.labels.insert(stmnt.labels.end(), macro_ref.labels.begin(), macro_ref.labels.end());
                    macro_ref.inserted_labels = true;
                }
            }

            //---macro reference---//
            if (stmnt_it->type == statement::types::unkown_operation) {
                auto it = macro_definitions.find(stmnt_it->operation_identifier);

                if (it == macro_definitions.end()) {
                    errors.push_back(make_error(syntax_error::ids::unrecognized_instruction, stmnt_it->line));
                    forward_stmnt();

                } else if (stmnt_it->args.size() != it->second.paramater_names.size()) {
                    errors.push_back(make_error(syntax_error::ids::illegal_operands, stmnt_it->line));
                    forward_stmnt();

                } else {
                    const auto &macro_def = it->second;

                    macro_reference new_macro_ref;
                    new_macro_ref.current = macro_def.begin;
                    new_macro_ref.labels = stmnt.labels;
                    new_macro_ref.end = macro_def.end;

                    for (unsigned long i = 0; i < macro_def.paramater_names.size(); ++i)
                        new_macro_ref.arguments.push_back({macro_def.paramater_names[i], stmnt_it->args[i]});

                    forward_stmnt();
                    macro_stack.push_back(new_macro_ref);
                }

            } else {
                output_statements.push_back(stmnt);
                forward_stmnt();
            }
        }

    } while (get_stmnt_it() != input_stmnts.end());

    return output_statements;
}

static syntax_error convert_pseudo_instruction(statement &stmnt, std::vector<statement> &output_statements,
                                               uint32_t &L0_counter) {
    assert(stmnt.type == statement::types::pseudo);

    //---verify parameters---//
    bool operands_are_valid = false;
    switch (stmnt.pseudo) {
        case (pseudo_id::LA):
            operands_are_valid =
                    (stmnt.args.size() == 2) && stmnt.args[0].is_reg() && stmnt.args[1].is_addressing();
            break;

        case (pseudo_id::RET):
        case (pseudo_id::NOP):
            operands_are_valid = (stmnt.args.size() == 0);
            break;

        case (pseudo_id::LI):
            operands_are_valid =
                    (stmnt.args.size() == 2) && stmnt.args[0].is_reg() && stmnt.args[1].is_integer();
            break;

        case (pseudo_id::MV):
        case (pseudo_id::NOT):
        case (pseudo_id::NEG):
        case (pseudo_id::SEQZ):
        case (pseudo_id::SNEZ):
        case (pseudo_id::SLTZ):
        case (pseudo_id::SGTZ):
            operands_are_valid = (stmnt.args.size() == 2) && stmnt.args[0].is_reg() && stmnt.args[1].is_reg();
            break;

        case (pseudo_id::BEQZ):
        case (pseudo_id::BNEZ):
        case (pseudo_id::BLEZ):
        case (pseudo_id::BLTZ):
        case (pseudo_id::BGEZ):
        case (pseudo_id::BGTZ):
            operands_are_valid =
                    (stmnt.args.size() == 2) && stmnt.args[0].is_reg() && stmnt.args[1].is_addressing();
            break;

        case (pseudo_id::BGT):
        case (pseudo_id::BLE):
        case (pseudo_id::BGTU):
        case (pseudo_id::BLEU):
            operands_are_valid = (stmnt.args.size() == 3) && stmnt.args[0].is_reg() &&
                                 stmnt.args[1].is_reg() && stmnt.args[2].is_addressing();
            break;

        case (pseudo_id::J):
            operands_are_valid = (stmnt.args.size() == 1) && stmnt.args[0].is_addressing();
            break;

        case (pseudo_id::CALL):
        case (pseudo_id::TAIL):
            operands_are_valid = (stmnt.args.size() == 1) && (stmnt.args[0].is_addressing());
            break;

        case (pseudo_id::JR):
            operands_are_valid = (stmnt.args.size() == 1) && stmnt.args[0].is_reg();
            break;

        case (pseudo_id::RDINSTRETH):
        case (pseudo_id::RDINSTRET):
        case (pseudo_id::RDCYCLE):
        case (pseudo_id::RDCYCLEH):
        case (pseudo_id::RDTIME):
        case (pseudo_id::RDTIMEH):
            operands_are_valid = (stmnt.args.size() == 1) && stmnt.args[0].is_reg();
            break;

        case (pseudo_id::CSRR):
            operands_are_valid = (stmnt.args.size() == 2) && stmnt.args[0].is_reg() &&
                                 (stmnt.args[1].is_integer() || stmnt.args[1].is_identifier());
            break;

        case (pseudo_id::CSRW):
        case (pseudo_id::CSRS):
        case (pseudo_id::CSRC):
            operands_are_valid = (stmnt.args.size() == 2) &&
                                 (stmnt.args[0].is_integer() || stmnt.args[0].is_identifier()) &&
                                 stmnt.args[1].is_reg();
            break;

        case (pseudo_id::CSRWI):
        case (pseudo_id::CSRSI):
        case (pseudo_id::CSRCI):
            operands_are_valid = (stmnt.args.size() == 2) &&
                                 (stmnt.args[0].is_integer() || stmnt.args[0].is_identifier()) &&
                                 stmnt.args[1].is_integer();
            break;
    }

    if (operands_are_valid == false)
        return (make_error(syntax_error::ids::illegal_operands, stmnt.line));

    bool has_two_statements = false;
    statement first_stmnt;
    first_stmnt.line = stmnt.line;
    first_stmnt.labels = stmnt.labels;
    first_stmnt.type = statement::types::instruction;

    statement second_stmnt;
    second_stmnt.line = stmnt.line;
    second_stmnt.type = statement::types::instruction;

    const auto make_li = [&]() {
        const uint32_t immediate = stmnt.args[1].int_val;
        const uint32_t lo12 = ((1 << 12) - 1) & immediate;
        const uint32_t hi20 = ~((1 << 12) - 1) & immediate;
        const bool lo12_is_negative = lo12 >> 11;

        if (hi20 == 0) {
            if (lo12_is_negative) {
                first_stmnt.instruction = instruction_id::LUI;
                first_stmnt.args = {stmnt.args[0], zero_imm};
                first_stmnt.args[1].int_val = 1;

                second_stmnt.instruction = instruction_id::ADDI;
                second_stmnt.args = {stmnt.args[0], stmnt.args[0], stmnt.args[1]};
                has_two_statements = true;

            } else {
                first_stmnt.instruction = instruction_id::ADDI;
                first_stmnt.args = {stmnt.args[0], zero_reg, stmnt.args[1]};
            }

        } else if (lo12 == 0) {
            first_stmnt.instruction = instruction_id::LUI;
            first_stmnt.args = {stmnt.args[0], stmnt.args[1]};
            first_stmnt.args[1].int_val = first_stmnt.args[1].int_val >> 12;

        } else if (hi20 == (uint32_t) ~((1 << 12) - 1)) {
            first_stmnt.instruction = instruction_id::ADDI;
            first_stmnt.args = {stmnt.args[0], zero_reg, stmnt.args[1]};

        } else {
            first_stmnt.instruction = instruction_id::LUI;
            first_stmnt.args = {stmnt.args[0], stmnt.args[1]};
            first_stmnt.args[1].int_val = (first_stmnt.args[1].int_val >> 12);
            first_stmnt.args[1].int_val += lo12_is_negative;

            second_stmnt.instruction = instruction_id::ADDI;
            second_stmnt.args = {stmnt.args[0], stmnt.args[0], stmnt.args[1]};
            has_two_statements = true;
        }
    };

    switch (stmnt.pseudo) {
        case (pseudo_id::NOP):
            first_stmnt.instruction = instruction_id::ADDI;
            first_stmnt.args = {zero_reg, zero_reg, zero_imm};
            break;

        case (pseudo_id::LA): {
            if (stmnt.args[1].is_integer()) {
                make_li();
            } else {
                expression hi_expr = {.sub_expressions = {stmnt.args[1]},
                                      .type = expression::types::reloc,
                                      .reloc_val = expression::reloc_functions::pcrel_hi};

                first_stmnt.instruction = instruction_id::AUIPC;
                first_stmnt.args = {stmnt.args[0], std::move(hi_expr)};

                std::string label_name = std::format(".L0^{}", ++L0_counter);
                first_stmnt.labels.push_back(label_name);

                expression lo_expr = {.sub_expressions = {{.type = expression::types::identifier,
                                                           .str_val = std::move(label_name)}},
                                      .type = expression::types::reloc,
                                      .reloc_val = expression::reloc_functions::pcrel_lo};

                second_stmnt.args = {stmnt.args[0], stmnt.args[0], lo_expr};
                second_stmnt.instruction = instruction_id::ADDI;

                has_two_statements = true;
            }
        } break;

        case (pseudo_id::LI): {
            make_li();
        } break;

        case (pseudo_id::MV):
            first_stmnt.instruction = instruction_id::ADDI;
            first_stmnt.args = {stmnt.args[0], stmnt.args[1], zero_imm};
            break;

        case (pseudo_id::NOT):
            first_stmnt.instruction = instruction_id::XORI;
            first_stmnt.args = {stmnt.args[0], stmnt.args[1], minone_imm};
            break;

        case (pseudo_id::NEG):
            first_stmnt.instruction = instruction_id::SUB;
            first_stmnt.args = {stmnt.args[0], zero_reg, stmnt.args[1]};
            break;

        case (pseudo_id::SEQZ):
            first_stmnt.instruction = instruction_id::SLTIU;
            first_stmnt.args = {stmnt.args[0], stmnt.args[1], one_imm};
            break;

        case (pseudo_id::SNEZ):
            first_stmnt.instruction = instruction_id::SLTU;
            first_stmnt.args = {stmnt.args[0], zero_reg, stmnt.args[1]};
            break;

        case (pseudo_id::SLTZ):
            first_stmnt.instruction = instruction_id::SLT;
            first_stmnt.args = {stmnt.args[0], stmnt.args[1], zero_reg};
            break;

        case (pseudo_id::SGTZ):
            first_stmnt.instruction = instruction_id::SLT;
            first_stmnt.args = {stmnt.args[0], zero_reg, stmnt.args[1]};
            break;

        case (pseudo_id::BEQZ):
            first_stmnt.instruction = instruction_id::BEQ;
            first_stmnt.args = {stmnt.args[0], zero_reg, stmnt.args[1]};
            break;

        case (pseudo_id::BNEZ):
            first_stmnt.instruction = instruction_id::BNE;
            first_stmnt.args = {stmnt.args[0], zero_reg, stmnt.args[1]};
            break;

        case (pseudo_id::BLEZ):
            first_stmnt.instruction = instruction_id::BGE;
            first_stmnt.args = {zero_reg, stmnt.args[0], stmnt.args[1]};
            break;

        case (pseudo_id::BGEZ):
            first_stmnt.instruction = instruction_id::BGE;
            first_stmnt.args = {stmnt.args[0], zero_reg, stmnt.args[1]};
            break;

        case (pseudo_id::BLTZ):
            first_stmnt.instruction = instruction_id::BLT;
            first_stmnt.args = {stmnt.args[0], zero_reg, stmnt.args[1]};
            break;

        case (pseudo_id::BGTZ):
            first_stmnt.instruction = instruction_id::BLT;
            first_stmnt.args = {zero_reg, stmnt.args[0], stmnt.args[1]};
            break;

        case (pseudo_id::BGT):
            first_stmnt.instruction = instruction_id::BLT;
            first_stmnt.args = {stmnt.args[1], stmnt.args[0], stmnt.args[2]};
            break;

        case (pseudo_id::BLE):
            first_stmnt.instruction = instruction_id::BGE;
            first_stmnt.args = {stmnt.args[1], stmnt.args[0], stmnt.args[2]};
            break;

        case (pseudo_id::BGTU):
            first_stmnt.instruction = instruction_id::BLTU;
            first_stmnt.args = {stmnt.args[1], stmnt.args[0], stmnt.args[2]};
            break;

        case (pseudo_id::BLEU):
            first_stmnt.instruction = instruction_id::BGEU;
            first_stmnt.args = {stmnt.args[1], stmnt.args[0], stmnt.args[2]};
            break;

        case (pseudo_id::J):
            first_stmnt.instruction = instruction_id::JAL;
            first_stmnt.args = {zero_reg, stmnt.args[0]};
            break;

        case (pseudo_id::JR):
            first_stmnt.instruction = instruction_id::JALR;
            first_stmnt.args = {zero_reg, stmnt.args[0], zero_imm};
            break;

        case (pseudo_id::RET):
            first_stmnt.instruction = instruction_id::JALR;
            first_stmnt.args = {zero_reg, ra_reg, zero_imm};
            break;

        case (pseudo_id::TAIL):
        case (pseudo_id::CALL): {
            expression hi_expr = {.sub_expressions = {stmnt.args[0]},
                                  .type = expression::types::reloc,
                                  .reloc_val = expression::reloc_functions::call};

            first_stmnt.instruction = instruction_id::AUIPC;
            first_stmnt.args = {ra_reg, std::move(hi_expr)};

            second_stmnt.args = {ra_reg, ra_reg, zero_imm};
            second_stmnt.instruction = instruction_id::JALR;
            if (stmnt.pseudo == pseudo_id::TAIL) {
                first_stmnt.args[0] = six_reg;
                second_stmnt.args[0] = zero_reg;
                second_stmnt.args[1] = six_reg;
            }

            has_two_statements = true;
        } break;

        case (pseudo_id::RDINSTRETH): {
            expression imm = {.type = expression::types::integer, .int_val = 0x0C82};
            first_stmnt.instruction = instruction_id::CSRRS;
            first_stmnt.args = {stmnt.args[0], imm, zero_reg};
        } break;

        case (pseudo_id::RDINSTRET): {
            expression imm = {.type = expression::types::integer, .int_val = 0x0C02};
            first_stmnt.instruction = instruction_id::CSRRS;
            first_stmnt.args = {stmnt.args[0], imm, zero_reg};
        } break;

        case (pseudo_id::RDCYCLE): {
            expression imm = {.type = expression::types::integer, .int_val = 0x0C00};
            first_stmnt.instruction = instruction_id::CSRRS;
            first_stmnt.args = {stmnt.args[0], imm, zero_reg};
        } break;

        case (pseudo_id::RDCYCLEH): {
            expression imm = {.type = expression::types::integer, .int_val = 0x0C80};
            first_stmnt.instruction = instruction_id::CSRRS;
            first_stmnt.args = {stmnt.args[0], imm, zero_reg};
        } break;

        case (pseudo_id::RDTIME): {
            expression imm = {.type = expression::types::integer, .int_val = 0x0C01};
            first_stmnt.instruction = instruction_id::CSRRS;
            first_stmnt.args = {stmnt.args[0], imm, zero_reg};
        } break;

        case (pseudo_id::RDTIMEH): {
            expression imm = {.type = expression::types::integer, .int_val = 0x0C81};
            first_stmnt.instruction = instruction_id::CSRRS;
            first_stmnt.args = {stmnt.args[0], imm, zero_reg};
        } break;

        case (pseudo_id::CSRR): {
            first_stmnt.instruction = instruction_id::CSRRS;
            first_stmnt.args = {stmnt.args[0], stmnt.args[1], zero_reg};
        } break;

        case (pseudo_id::CSRW): {
            first_stmnt.instruction = instruction_id::CSRRW;
            first_stmnt.args = {zero_reg, stmnt.args[0], stmnt.args[1]};
        } break;

        case (pseudo_id::CSRS): {
            first_stmnt.instruction = instruction_id::CSRRS;
            first_stmnt.args = {zero_reg, stmnt.args[0], stmnt.args[1]};
        } break;

        case (pseudo_id::CSRC): {
            first_stmnt.instruction = instruction_id::CSRRC;
            first_stmnt.args = {zero_reg, stmnt.args[0], stmnt.args[1]};
        } break;

        case (pseudo_id::CSRWI): {
            first_stmnt.instruction = instruction_id::CSRRWI;
            first_stmnt.args = {zero_reg, stmnt.args[0], stmnt.args[1]};
        } break;

        case (pseudo_id::CSRSI): {
            first_stmnt.instruction = instruction_id::CSRRSI;
            first_stmnt.args = {zero_reg, stmnt.args[0], stmnt.args[1]};
        } break;

        case (pseudo_id::CSRCI): {
            first_stmnt.instruction = instruction_id::CSRRCI;
            first_stmnt.args = {zero_reg, stmnt.args[0], stmnt.args[1]};
        } break;

        default:
            break;
    }

    output_statements.push_back(std::move(first_stmnt));

    if (has_two_statements)
        output_statements.push_back(std::move(second_stmnt));

    return {};
}
