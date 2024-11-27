#ifndef STATEMENT_HPP
#define STATEMENT_HPP

#include <cassert>
#include <cstdint>
#include <stdint.h>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

// clang-format off
enum struct directive_id {
    DATA,
    TEXT,
    BSS,
    RODATA,
    WORD,
    HALF,
    BYTE,
    P2ALIGN,
    EQU,
    GLOBL,
    SECTION,
    STRING,
    MACRO,
    ENDM,
    ZERO
};

enum struct pseudo_id {
    LA = 0,
    NOP,

    LI,

    MV,
    NOT,
    NEG,
    SEQZ,
    SNEZ,
    SLTZ,
    SGTZ,

    BEQZ,
    BNEZ,
    BLEZ,
    BGEZ,
    BLTZ,
    BGTZ,

    BGT,
    BLE,
    BGTU,
    BLEU,
    J,
    JR,
    RET,
    CALL,
    TAIL,

    RDINSTRET,
    RDINSTRETH,
    RDCYCLE,
    RDCYCLEH,
    RDTIME,
    RDTIMEH,

    CSRR,
    CSRW,
    CSRS,
    CSRC,
    
    CSRWI,
    CSRSI,
    CSRCI,
};

enum struct instruction_id {
    LUI = 0,
    AUIPC,
    JAL,
    BEQ,
    BNE,
    BLT,
    BGE,
    BLTU,
    BGEU,
    SB,
    SH,
    SW,
    JALR,

    LB,
    LH,
    LW,
    LBU,
    LHU,

    ADDI,
    SLTI,
    SLTIU,
    XORI,
    ORI,
    ANDI,
    SLLI,
    SRLI,
    SRAI,
    ADD,
    SUB,
    SLL,
    SLT,
    SLTU,
    XOR,
    SRL,
    SRA,
    OR,
    AND,
    MUL,
    MULH,
    MULHSU,
    DIV,
    DIVU,
    REM,
    REMU,

    FENCE,
    FENCEI,

    ECALL,
    EBREAK,
    SRET,
    MRET,
    MNRET,
    WFI,
    SFENCEVMA,

    CSRRW,
    CSRRS,
    CSRRC,
    CSRRWI,
    CSRRSI,
    CSRRCI,
};

enum struct register_id { 
    x0	= 0,    zero = 0,  
    x1	= 1,	ra= 1,  
    x2	= 2,	sp= 2,  
    x3	= 3,	gp= 3,  
    x4	= 4,	tp= 4,  
    x5	= 5,	t0= 5,  
    x6	= 6,	t1= 6,  
    x7	= 7,	t2= 7,  
    x8	= 8,	s0= 8,  
    x9	= 9,	s1= 9,  
		      
    x10 = 10,	 a0= 10,
    x11 = 11,	 a1= 11,
    x12 = 12,	 a2= 12,
    x13 = 13,	 a3= 13,
    x14 = 14,	 a4= 14,
    x15 = 15,	 a5= 15,
    x16 = 16,	 a6= 16,
    x17 = 17,	 a7= 17,
    x18 = 18,	 s2= 18,
    x19 = 19,	 s3= 19,
		      
    x20 = 20,	 s4= 20,
    x21 = 21,	 s5= 21,
    x22 = 22,	 s6= 22,
    x23 = 23,	 s7= 23,
    x24 = 24,	 s8= 24,
    x25 = 25,	 s9= 25,
    x26 = 26,	 s10= 26,
    x27 = 27,	 s11= 27,
    x28 = 28,	 t3= 28,
    x29 = 29,	 t4= 29,
    x30 = 30,    t5 = 30,
    x31 = 31,    t6 = 31,
};
// clang-format on

enum struct operand_type { reg, string, character, identifier };

struct expression {
    std::vector<expression> sub_expressions = {};

    enum struct types {
        integer,
        reg,
        string,
        identifier,
        integer_identifier,
        macro_identifier,
        addend,
        reloc
    } type;

    enum struct reloc_functions {
        none = 0,
        hi,
        lo,
        pcrel_hi,
        pcrel_lo,
        call
    } reloc_val = reloc_functions::none;

    union {
        register_id reg_val;
        uint32_t int_val = 0;
    };

    std::string str_val = "";

    bool is_reloc() const { return type == types::reloc; };
    bool is_integer() const { return type == types::integer; };
    bool is_reg() const { return type == types::reg; };
    bool is_addend() const { return type == types::addend; };
    bool is_string() const { return type == types::string; };
    bool is_identifier() const { return type == types::identifier; };
    bool is_integer_identifier() const { return type == types::integer_identifier; };
    bool is_macro_identifier() const { return type == types::macro_identifier; };
    bool has_reloc_hi() const { return reloc_val == reloc_functions::hi; }
    bool has_reloc_lo() const { return reloc_val == reloc_functions::lo; }
    bool has_reloc_pcrel_lo() const { return reloc_val == reloc_functions::pcrel_lo; }
    bool has_reloc_pcrel_hi() const { return reloc_val == reloc_functions::pcrel_hi; }
    bool has_reloc() const { return reloc_val != reloc_functions::none; }

    bool is_identifier_addressing() const {
        if (is_identifier())
            return true;

        if (is_addend() && sub_expressions.size() == 2)
            return sub_expressions[0].is_identifier() && sub_expressions[1].is_integer();

        return false;
    };

     bool is_addressing() const {
        return (is_identifier_addressing() || is_integer());
    };

     bool is_relocated_addressing() const {
        if (is_reloc() && sub_expressions.size() == 1)
            return sub_expressions[0].is_addressing();

        return false;
    }

    std::tuple<const std::string, uint32_t> get_addressing() const {
        static std::string dummy;
        assert(is_addressing());

        if (is_identifier())
            return {str_val, 0};

        if (is_integer())
            return {dummy, int_val};

        if (is_addend())
            return {sub_expressions[0].str_val, sub_expressions[1].int_val};

        assert("unreachable");
        return {dummy, 0};
    }

    std::tuple<reloc_functions, const std::string, uint32_t> get_relocated_addressing() const {
        assert(is_reloc());
        assert(sub_expressions.empty() == false);
        auto [str, immediate] = sub_expressions[0].get_addressing();
        return {reloc_val, str, immediate};
    }
};

struct label {
    std::string name;
    bool is_int_label = false;

    label(std::string name) : name(name), is_int_label(false) {}
};

struct statement {
    int line = 0;
    std::vector<label> labels = {};

    enum struct types { directive, instruction, pseudo, unkown_operation } type;

    union {
        directive_id directive;
        pseudo_id pseudo;
        instruction_id instruction;
    };

    std::string operation_identifier = "";


    std::vector<expression> args = {};
};

template<typename T>
void itererate_expressions(std::vector<expression> &expressions, expression::types expr_type, T func) {
    for (auto &expr: expressions) {
        if (expr.type == expr_type)
            func(expr);

        itererate_expressions(expr.sub_expressions, expr_type, func);
    }
}

template<typename T>
void itererate_expressions(std::vector<expression> &expressions, T func) {
    for (auto &expr: expressions) {
        func(expr);
        itererate_expressions(expr.sub_expressions, func);
    }
}

#endif
