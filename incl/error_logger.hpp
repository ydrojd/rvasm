#ifndef ERROR_LOGGER_HPP
#define ERROR_LOGGER_HPP

#include "program_options.hpp"
#include <fstream>
#include <iostream>
#include <source_location>
#include <string>
#include <vector>

#define make_error(id, line)                                                                                 \
    syntax_error { id, line, std::source_location::current() }

struct syntax_error {
    std::source_location src_location;

    enum struct ids {
        none = 0,
        illegal_operands,
        unrecognized_instruction,
        duplicate_identifier,
        unknown_backward_ref,
    };

    int line = 0;
    ids id = ids::none;

    syntax_error() = default;
    syntax_error(ids type, int line, std::source_location location)
        : src_location(location), line(line), id(type) {}

    syntax_error(ids type, int line) : line(line), id(type) {}
    bool is_error() const { return id != ids::none; }
};

void print_errors(std::vector<syntax_error> errors, std::fstream &asm_file, const program_options &poptions,
                  std::ostream &output_stream);

void print_error(const syntax_error &error, const std::string &linestr, const std::string &file_name);

#endif
