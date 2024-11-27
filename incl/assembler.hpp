#ifndef ASSEMBLER_H
#define ASSEMBLER_H

#include "elf_file.hpp"
#include "error_logger.hpp"
#include "statement.hpp"
#include <assert.h>
#include <cstdint>

void assemble(const std::vector<statement> &stmnts, elf_file &object_file, const program_options &options,
              std::vector<syntax_error> &errors);

struct memory_allocation {
    uint32_t p2allign = 0;
    uint32_t nbytes = 0;
};

class counter {
public:
    uint32_t address = 0;
    uint32_t allign(uint32_t p2) {
        assert(p2 < 32);
        if ((address & ((1 << p2) - 1)) == 0)
            return address;

        const uint32_t right_shift = address >> p2;
        address = (right_shift + 1) << p2;
        return address;
    };

    uint32_t increment(memory_allocation mem) {
        const auto placement = allign(mem.p2allign);
        address += mem.nbytes;
        return placement;
    }
};

#endif
