#ifndef PROGRAM_OPTIONS_HPP
#define PROGRAM_OPTIONS_HPP

#include <string>

struct program_options {
    std::string input_filename = "";
    std::string output_filename = "";
    bool use_relaxation = false;
    bool print_error_source = false;
};

#endif //PROGRAM_OPTIONS_HPP
