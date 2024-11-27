#include <filesystem>
#include <fstream>
#include <getopt.h>
#include <iostream>
#include <string>
#include <unistd.h>
#include <vector>

#include "assembler.hpp"
#include "elf_file.hpp"
#include "error_logger.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "preprocessor.hpp"
#include "program_options.hpp"

int main(int argc, char **argv) {
    //---parse input arguments---//
    program_options options;
    options.print_error_source = true;
    options.use_relaxation = true;

    if (argc >= 2 && argv[1][0] != '-')
        options.input_filename = argv[1];

    char c;
    while ((c = getopt(argc, argv, "o:")) != -1) {
        switch (c) {
            case 'o':
                options.output_filename = optarg;
                break;
        }
    }

    if (options.input_filename.empty()) {
        std::cout << "Error: missing filename" << std::endl << std::flush;
        return 0;
    }

    if (std::filesystem::exists(options.input_filename) == false) {
        std::cout << "Error: can't open " << options.input_filename
                  << " for reading: No such file or directory" << std::endl
                  << std::flush;
        return 0;
    }

    if (options.output_filename.empty())
        options.output_filename = options.input_filename + ".elf";

    //---lexer---//
    auto asm_file = std::fstream();
    asm_file.open(options.input_filename);
    lexer lex(asm_file);

    //---parser---//
    std::vector<syntax_error> errors;
    auto statements = parse_statements(lex, errors);

    //---preprocessor---//
    if (errors.empty())
        statements = preprocess(std::move(statements), errors);

    //---assembler---//
    elf_file object_file;
    if (errors.empty())
        assemble(statements, object_file, options, errors);

    //---write object file---//
    if (errors.empty()) {
        std::ofstream output_file;
        output_file.open(options.output_filename);
        object_file.write(output_file);
        output_file.close();
    }

    //---print errors---//
    if (errors.empty() == false) {
        auto file = std::fstream();
        file.open(options.input_filename);
        print_errors(errors, file, options, std::cout);
        file.close();
    }

    return 0;
}
