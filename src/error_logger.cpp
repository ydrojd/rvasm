#include "error_logger.hpp"
#include "program_options.hpp"
#include <algorithm>
#include <format>
#include <iostream>
#include <ostream>
#include <source_location>
#include <string>

static std::string make_template(int linenum, const std::string &file_name_m, const std::string &message,
                                 const std::string &value) {
    return std::format("{}:{}: error: {}: {}", file_name_m, linenum, message, value);
}

void print_error(const syntax_error &error, const std::string &linestr, const std::string &file_name,
                 std::ostream &output_stream) {
    std::string message;
    switch (error.id) {
        case syntax_error::ids::duplicate_identifier:
            message = make_template(error.line, file_name, "Duplicate identifier", linestr);
            break;
        case syntax_error::ids::illegal_operands:
            message = make_template(error.line, file_name, "Illegal operands", linestr);
            break;
        case syntax_error::ids::unrecognized_instruction:
            message = make_template(error.line, file_name, "Unrecognized instruction", linestr);
            break;
        case syntax_error::ids::unknown_backward_ref:
            message = make_template(error.line, file_name, "Backward ref to unknown label", linestr);
            break;

        default:
            break;
    }

    output_stream << message << std::endl;
}

std::string make_source_location_string(const std::source_location &src) {
    return std::format("{}:{}: {}", src.file_name(), src.line(), src.function_name());
}

void print_errors(std::vector<syntax_error> errors, std::fstream &asm_file, const program_options &poptions,
                  std::ostream &output_stream) {

    std::sort(errors.begin(), errors.end(), [](syntax_error a, syntax_error b) { return a.line < b.line; });

    auto error_it = errors.begin();

    std::string linestr;
    for (int linenum = 1; std::getline(asm_file, linestr); linenum++) {
        if (error_it == errors.end())
            return;

        if (linenum == error_it->line) {
            for (; error_it != errors.end(); error_it++) {
                if (error_it->line != linenum)
                    break;

                print_error(*error_it, linestr, poptions.input_filename, output_stream);
                if (poptions.print_error_source)
                    output_stream << make_source_location_string(error_it->src_location) << std::endl
                                  << std::endl;
            }
        }
    }

    output_stream << std::flush;
}
