#ifndef LEXER_H
#define LEXER_H

#include "F.h"
#include <iostream>
#include <random>
#include <string>
#include <string_view>
#include <vector>
#include <iostream>

class flex : public yyFlexLexer {
public:
    bool is_eof() { return yyin.eof(); }

    using yyFlexLexer::yyFlexLexer;
    explicit flex(std::istream &str) : flex(str, std::cout){};
    explicit flex(std::istream *str) : flex(str, &std::cout){};
};

enum struct token_type {
    eof = 0,
    none,
    label,
    directive,
    mnemonic,
    pseudo_mnemonic,
    identifier,
    hex_integer,
    decimal_integer,
    binary_integer,
    character,
    reg,
    string,
    newline,
    lbracket,
    rbracket,
    comma,
    reloc,
    integer_label,
    integer_identifier,
    add,
    macro_identifier,
    lsft
};

class lexer {
    flex flex_lexer;
    token_type type;

public:
    lexer(std::istream *str) : flex_lexer(str) {};
    lexer(std::istream &str) : flex_lexer(str) {};
    void operator++(int) {
        if (flex_lexer.is_eof())
	    type = token_type::eof;
        else {
	    type = (token_type) flex_lexer.yylex();
	    if (type == token_type::none)
		this->operator++(1);
        }
    }
    
    int get_line(void) const {return flex_lexer.lineno();}
    token_type get_type(void) const {return type;}
    const char *get_string(void) const {
	return flex_lexer.YYText();
    }
};

#endif
