#include "parser.hpp"
#include "error_logger.hpp"
#include "lexer.hpp"
#include "statement.hpp"
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>
#include <tuple>
#include <unordered_map>
#include <vector>

std::tuple<std::string, bool> parse_string(std::string_view str) {
    const auto get_hex = [](char c) -> std::pair<uint8_t, bool> {
        if (c >= '0' && c <= '9')
            return {c - '0', true};
        else if (c >= 'a' && c <= 'f')
            return {10 + c - 'a', true};
        else if (c >= 'A' && c <= 'F')
            return {10 + c - 'A', true};

        return {0, false};
    };

    std::string new_string;

    assert(str.size() >= 2);
    str.remove_prefix(1);
    str.remove_suffix(1);

    for (auto it = str.begin(); it != str.end(); ++it) {
        if (*it == '\\') {
            ++it;
            if (it == str.end())
                return {"", false};

            const char c = *it;

            //---octal---//
            if (c >= '0' && c <= '8') {
                uint8_t b = c - '0';

                for (int i = 0; i < 2; ++i) {
                    const auto next_it = it + 1;
                    if (next_it == str.end())
                        break;

                    const char c = *next_it;
                    if (c >= '0' && c <= '8') {
                        b = b * 8 + (c - '0');
                        it = next_it;
                    } else {
                        break;
                    }
                }

                new_string.push_back(b);

            } else if (c == 'x') {

                //---hex---//
                ++it;
                if (it == str.end())
                    return {"", false};

                const auto [hex1, is_hex] = get_hex(*it);
                if (is_hex == false)
                    return {"", false};

                const auto next_it = it + 1;
                if (next_it == str.end()) {
                    new_string.push_back(hex1);
                } else {
                    const auto [hex2, is_hex2] = get_hex(*next_it);
                    if (is_hex2) {
                        const uint8_t num = hex2 + 16 * hex1;
                        new_string.push_back(num);
                        it = next_it;
                    } else {
                        new_string.push_back(hex1);
                    }
                }
            } else {

                //---escape character---//
                switch (c) {
                    case 'a':
                        new_string.push_back('\a');
                        break;
                    case 'b':
                        new_string.push_back('\b');
                        break;
                    case 'f':
                        new_string.push_back('\f');
                        break;
                    case 'n':
                        new_string.push_back('\n');
                        break;
                    case 'r':
                        new_string.push_back('\r');
                        break;
                    case 't':
                        new_string.push_back('\t');
                        break;
                    case 'v':
                        new_string.push_back('\v');
                        break;
                    case '\\':
                        new_string.push_back('\\');
                        break;
                    case '\'':
                        new_string.push_back('\'');
                        break;
                    case '\"':
                        new_string.push_back('\"');
                        break;
                    case '\?':
                        new_string.push_back('\?');
                        break;
                    default:
                        return {"", false};
                }
            }
        } else {
            new_string.push_back(*it);
        }
    }

    return {std::move(new_string), true};
}

const static std::unordered_map<std::string, register_id> string_to_reg_id{
        {"zero", register_id::zero}, {"ra", register_id::ra}, {"sp", register_id::sp},
        {"gp", register_id::gp},     {"tp", register_id::tp}, {"t0", register_id::t0},
        {"t1", register_id::t1},     {"t2", register_id::t2}, {"s0", register_id::s0},
        {"s1", register_id::s1},     {"a0", register_id::a0}, {"a1", register_id::a1},
        {"a2", register_id::a2},     {"a3", register_id::a3}, {"a4", register_id::a4},
        {"a5", register_id::a5},     {"a6", register_id::a6}, {"a7", register_id::a7},
        {"s2", register_id::s2},     {"s3", register_id::s3}, {"s4", register_id::s4},
        {"s5", register_id::s5},     {"s6", register_id::s6}, {"s7", register_id::s7},
        {"s8", register_id::s8},     {"s9", register_id::s9}, {"s10", register_id::s10},
        {"s11", register_id::s11},   {"t3", register_id::t3}, {"t4", register_id::t4},
        {"t5", register_id::t5},     {"t6", register_id::t6}};

const static std::unordered_map<std::string, expression::reloc_functions> string_to_reloc_fun{
        {"%hi", expression::reloc_functions::hi},
        {"%lo", expression::reloc_functions::lo},
        {"%pcrel_hi", expression::reloc_functions::pcrel_hi},
        {"%pcrel_lo", expression::reloc_functions::pcrel_lo}};

struct operation_type {
    statement::types type;
    union {
        directive_id directive;
        pseudo_id pseudo;
        instruction_id instruction;
    };

    operation_type(directive_id id) : type(statement::types::directive), directive(id){};
    operation_type(pseudo_id id) : type(statement::types::pseudo), pseudo(id){};
    operation_type(instruction_id id) : type(statement::types::instruction), instruction(id){};
};

const static std::unordered_map<std::string, operation_type> string_to_operation_map{
        {".rodata", directive_id::RODATA},
        {".data", directive_id::DATA},
        {".text", directive_id::TEXT},
        {".bss", directive_id::BSS},
        {".word", directive_id::WORD},
        {".half", directive_id::HALF},
        {".byte", directive_id::BYTE},
        {".p2align", directive_id::P2ALIGN},
        {".equ", directive_id::EQU},
        {".global", directive_id::GLOBL},
        {".globl", directive_id::GLOBL},
        {".section", directive_id::SECTION},
        {".string", directive_id::STRING},
        {".macro", directive_id::MACRO},
        {".endm", directive_id::ENDM},
        {".zero", directive_id::ZERO},

        {"lui", instruction_id::LUI},
        {"auipc", instruction_id::AUIPC},
        {"jal", instruction_id::JAL},
        {"beq", instruction_id::BEQ},
        {"bne", instruction_id::BNE},
        {"blt", instruction_id::BLT},
        {"bge", instruction_id::BGE},
        {"bltu", instruction_id::BLTU},
        {"bgeu", instruction_id::BGEU},
        {"sb", instruction_id::SB},
        {"sh", instruction_id::SH},
        {"sw", instruction_id::SW},
        {"jalr", instruction_id::JALR},
        {"lb", instruction_id::LB},
        {"lh", instruction_id::LH},
        {"lw", instruction_id::LW},
        {"lbu", instruction_id::LBU},
        {"lhu", instruction_id::LHU},
        {"addi", instruction_id::ADDI},
        {"slti", instruction_id::SLTI},
        {"sltiu", instruction_id::SLTIU},
        {"xori", instruction_id::XORI},
        {"ori", instruction_id::ORI},
        {"andi", instruction_id::ANDI},
        {"slli", instruction_id::SLLI},
        {"srli", instruction_id::SRLI},
        {"srai", instruction_id::SRAI},
        {"add", instruction_id::ADD},
        {"sub", instruction_id::SUB},
        {"sll", instruction_id::SLL},
        {"slt", instruction_id::SLT},
        {"sltu", instruction_id::SLTU},
        {"xor", instruction_id::XOR},
        {"srl", instruction_id::SRL},
        {"sra", instruction_id::SRA},
        {"or", instruction_id::OR},
        {"and", instruction_id::AND},

        {"fence", instruction_id::FENCE},
        {"fence.i", instruction_id::FENCEI},
        {"sfence.vma", instruction_id::SFENCEVMA},

        {"ecall", instruction_id::ECALL},
        {"ebreak", instruction_id::EBREAK},
        {"sret", instruction_id::SRET},
        {"mret", instruction_id::MRET},
        {"mnret", instruction_id::MNRET},
        {"wfi", instruction_id::WFI},

        {"csrrw", instruction_id::CSRRW},
        {"csrrs", instruction_id::CSRRS},
        {"csrrc", instruction_id::CSRRC},
        {"csrrwi", instruction_id::CSRRWI},
        {"csrrsi", instruction_id::CSRRSI},
        {"csrrci", instruction_id::CSRRCI},

        {"mul", instruction_id::MUL},
        {"mulh", instruction_id::MULH},
        {"mulhsu", instruction_id::MULHSU},
        {"div", instruction_id::DIV},
        {"divu", instruction_id::DIVU},
        {"rem", instruction_id::REM},
        {"remu", instruction_id::REMU},

        {"la", pseudo_id::LA},
        {"nop", pseudo_id::NOP},
        {"li", pseudo_id::LI},
        {"mv", pseudo_id::MV},
        {"not", pseudo_id::NOT},
        {"neg", pseudo_id::NEG},
        {"seqz", pseudo_id::SEQZ},
        {"snez", pseudo_id::SNEZ},
        {"sltz", pseudo_id::SLTZ},
        {"sgtz", pseudo_id::SGTZ},
        {"beqz", pseudo_id::BEQZ},
        {"bnez", pseudo_id::BNEZ},
        {"blez", pseudo_id::BLEZ},
        {"bgez", pseudo_id::BGEZ},
        {"bltz", pseudo_id::BLTZ},
        {"bgtz", pseudo_id::BGTZ},
        {"bgt", pseudo_id::BGT},
        {"ble", pseudo_id::BLE},
        {"bgtu", pseudo_id::BGTU},
        {"bleu", pseudo_id::BLEU},
        {"j", pseudo_id::J},
        {"jr", pseudo_id::JR},
        {"ret", pseudo_id::RET},
        {"call", pseudo_id::CALL},
        {"tail", pseudo_id::TAIL},

        {"rdinstret", pseudo_id::RDINSTRET},
        {"rdinstreth", pseudo_id::RDINSTRETH},
        {"rdcycle", pseudo_id::RDCYCLE},
        {"rdcycleh", pseudo_id::RDCYCLEH},
        {"rdtime", pseudo_id::RDTIME},
        {"rdtimeh", pseudo_id::RDTIMEH},
        {"csrr", pseudo_id::CSRR},
        {"csrw", pseudo_id::CSRW},
        {"csrc", pseudo_id::CSRC},
        {"csrs", pseudo_id::CSRS},
        {"csrwi", pseudo_id::CSRWI},
        {"csrsi", pseudo_id::CSRSI},
        {"csrci", pseudo_id::CSRCI}};

static register_id parse_register(const std::string &reg) {
    assert(reg.length() >= 2);

    register_id reg_id = register_id::zero;

    if (reg[0] == 'x') {
        if (reg.size() == 2)
            reg_id = (register_id) (reg[1] - '0');
        else
            reg_id = (register_id) ((reg[1] - '0') * 10 + (reg[2] - '0'));
    } else {
        auto it = string_to_reg_id.find(reg);
        if (it == string_to_reg_id.end()) {
            std::cout << reg << std::endl << std::flush;
        }

        assert(it != string_to_reg_id.end());
        reg_id = it->second;
    }

    return reg_id;
}

static std::tuple<expression, bool> try_parse_expression(lexer &lex) {
    expression expr;

    //---try relocation---//
    if (lex.get_type() == token_type::reloc) {
        expr.type = expression::types::reloc;
        auto it = string_to_reloc_fun.find(lex.get_string());
        assert(it != string_to_reloc_fun.end());
        expr.reloc_val = it->second;
        lex++;

        //expecting opening brace
        if (lex.get_type() != token_type::lbracket)
            return {{std::move(expr)}, false};

        lex++;
        //expecting a subexpression
        auto [sub_expr, success] = try_parse_expression(lex);
        if (success == false)
            return {{std::move(expr)}, false};

        expr.sub_expressions = {std::move(sub_expr)};

        //expecting closing brace
        if (lex.get_type() != token_type::rbracket)
            return {{std::move(expr)}, false};

        lex++;
        return {expr, true};
    };

    bool is_subscript = false;
    if (lex.get_type() == token_type::lbracket) {
        is_subscript = true;
        lex++;
    }

    switch (lex.get_type()) {
        case token_type::integer_identifier:
            expr.type = expression::types::integer_identifier;
            expr.str_val = lex.get_string();
            break;
        case token_type::identifier:
        case token_type::directive:
            expr.str_val = lex.get_string();
            expr.type = expression::types::identifier;
            break;
        case token_type::macro_identifier:
            expr.str_val = lex.get_string();
            expr.type = expression::types::macro_identifier;
            assert(expr.str_val.size() >= 1);
            expr.str_val = expr.str_val.substr(1, expr.str_val.size() - 1);
            break;

        case token_type::reg:
            expr.reg_val = parse_register(lex.get_string());
            expr.type = expression::types::reg;
            break;

        case token_type::string: {
            auto [str, success] = parse_string(lex.get_string());
            if (success == false)
                return {std::move(expr), false};

            expr.type = expression::types::string;
            expr.str_val = std::move(str);
        } break;

        case token_type::character: {
            std::string_view cstr = lex.get_string();
            assert(cstr.size() >= 3);

            const bool has_sign = cstr[0] == '-';
            if (has_sign)
                cstr.remove_prefix(1);

            auto [str, success] = parse_string(cstr);
            if (success == false || str.size() != 1) {
                std::cout << "not ONE" << std::endl << std::flush;
                return {std::move(expr), false};
            }

            expr.type = expression::types::integer;
            expr.int_val = has_sign ? -str[0] : str[0];
        } break;

        case token_type::binary_integer:
            expr.int_val = std::stoi(lex.get_string(), nullptr, 2);
            expr.type = expression::types::integer;
            break;
        case token_type::decimal_integer:
            expr.int_val = std::stoi(lex.get_string(), nullptr, 10);
            expr.type = expression::types::integer;
            break;
        case token_type::hex_integer:
            expr.int_val = std::stoi(lex.get_string(), nullptr, 16);
            expr.type = expression::types::integer;
            break;

        default:
            return {std::move(expr), false};
    }

    lex++;

    //---try addend---//
    if (lex.get_type() == token_type::add) {
        expression sub_expr1 = std::move(expr);
        expr = {};
        expr.type = expression::types::addend;

        //try to parse addend value
        lex++;
        auto [sub_expr2, success] = try_parse_expression(lex);
        if (success == false)
            return {std::move(expr), false};

        expr.sub_expressions = {std::move(sub_expr1), std::move(sub_expr2)};
    }

    if (is_subscript) {
        if (lex.get_type() != token_type::rbracket)
            return {std::move(expr), false};

        lex++;
    }

    return {std::move(expr), true};
}

static void eat_newlines(lexer &lex) {
    while (lex.get_type() == token_type::newline)
        lex++;
}

static std::tuple<statement, bool, syntax_error> parse_statement(lexer &lex) {
    statement stmnt;

    //---while lines---//
    eat_newlines(lex);

    //check for end of file
    const bool eof = lex.get_type() == token_type::eof;
    if (eof)
        return {std::move(stmnt), eof, {}};

    //---labels---//
    while (lex.get_type() == token_type::label || lex.get_type() == token_type::integer_label) {
        std::string label = lex.get_string();
        assert(label.size() >= 1);
        label = label.substr(0, label.length() - 1);//todo
        stmnt.labels.push_back(label);
        stmnt.labels.back().is_int_label = lex.get_type() == token_type::integer_label;
        lex++;
        eat_newlines(lex);
    }

    //---white lines---//
    eat_newlines(lex);

    //---statement operation---//
    stmnt.line = lex.get_line();
    if (lex.get_type() != token_type::identifier) {
        return {stmnt, false, make_error(syntax_error::ids::unrecognized_instruction, stmnt.line)};
    }

    auto it = string_to_operation_map.find(lex.get_string());
    if (it == string_to_operation_map.end()) {
        //may be macro
        stmnt.type = statement::types::unkown_operation;
        stmnt.operation_identifier = lex.get_string();
    } else {
        stmnt.type = it->second.type;
        switch (stmnt.type) {
            case statement::types::instruction: {
                stmnt.instruction = it->second.instruction;
            } break;

            case statement::types::pseudo: {
                stmnt.pseudo = it->second.pseudo;
            } break;

            case statement::types::directive: {
                stmnt.directive = it->second.directive;
            } break;

            default:
                assert(!"unreachable");
        }
    }

    lex++;

    //---arguments---//
    if (lex.get_type() == token_type::newline || lex.get_type() == token_type::eof)
        return {std::move(stmnt), false, {}};

    //---try first argument ---///
    auto [expr, success] = try_parse_expression(lex);
    if (success == false)
        return {stmnt, false, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

    stmnt.args.push_back(std::move(expr));

    //---argument list---//
    while (lex.get_type() != token_type::newline) {
        //---comma seperator---//
        if (lex.get_type() == token_type::comma)
            lex++;
        else if (lex.get_type() != token_type::lbracket)
            return {stmnt, false, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

        //---argument---//
        auto [expr, success] = try_parse_expression(lex);
        if (success == false)
            return {stmnt, false, make_error(syntax_error::ids::illegal_operands, stmnt.line)};

        stmnt.args.push_back(std::move(expr));
    }

    lex++;

    return {std::move(stmnt), false, {}};
}

std::vector<statement> parse_statements(lexer &lex, std::vector<syntax_error> &errors) {
    std::vector<statement> stmnts;

    bool is_eof = false;
    lex++;
    while (is_eof == false) {
        auto [stmnt, eof, error] = parse_statement(lex);
        is_eof = eof;
        if (is_eof == false)
            stmnts.push_back(stmnt);

        if (error.is_error()) {
            //eat all the remaining tokens of the current line
            for (; lex.get_type() != token_type::newline && lex.get_type() != token_type::eof; lex++)
                ;
            eat_newlines(lex);//seek the first token of the next statement by eating all whitelines
            errors.push_back(error);
        }
    }

    return stmnts;
}
