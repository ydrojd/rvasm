#ifndef PARSER_H
#define PARSER_H

#include "error_logger.hpp"
#include "lexer.hpp"
#include "statement.hpp"
#include <vector>
std::vector<statement> parse_statements(lexer &lex, std::vector<syntax_error> &errors);
#endif
