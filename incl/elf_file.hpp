#ifndef ASSEMBLER_ELF_GENERATOR_H
#define ASSEMBLER_ELF_GENERATOR_H

#include "elfio.hpp"
#include <cstdint>
#include <elfio_section.hpp>
#include <string>
#include <sys/types.h>
#include <unordered_map>
#include <vector>

enum struct riscv_relocation {
    NONE = 0,
    R_RISCV_BRANCH = 16,
    R_RISCV_JAL = 17,
    R_RISCV_CALL_PLT = 19,

    R_RISCV_PCREL_HI20 = 23,
    R_RISCV_PCREL_LO12_I = 24,
    R_RISCV_PCREL_LO12_S = 25,
    R_RISCV_HI20 = 26,
    R_RISCV_LO12_I = 27,
    R_RISCV_LO12_S = 28,

    R_RISCV_RELAX = 51,
};

enum struct symbol_type { undefined = 0, data, fun, sec };

enum struct symbol_scope { local, global };

struct symbol {
    std::string identifier;
    uint32_t index;
    uint32_t section_id;
    uint32_t address;
    symbol_scope scope;
    symbol_type type;
    u_int32_t size;
};

struct relocation {
    uint32_t address;
    riscv_relocation type;
    std::string symbol_identifier;
    uint32_t addend;
};

struct section_t {
    std::string data;
    ELFIO::section *elf_sec;
    ELFIO::section *elf_rela_sec;
    std::vector<relocation> relocations;
};

class elf_file {
public:
private:
    ELFIO::elfio writer;
    void initialise();

    ELFIO::Elf_Half get_sec_index(const std::string &section);
    std::string &get_working_data();

    ELFIO::section *symbol_sec;
    ELFIO::section *str_sec;

    std::unordered_map<std::string, section_t> sections;
    std::unordered_map<std::string, section_t>::iterator active_section;

    std::unordered_map<std::string, symbol> symbols;

    uint32_t entry_point = 0;
    std::string attributes;

    uint32_t write_symbol(const symbol &sym);
    section_t &get_active_section();

public:
    explicit elf_file() { initialise(); }
    uint32_t p2align(uint32_t p2);

    void insert_data(uint8_t val);
    void insert_data(uint16_t val);
    void insert_data(uint32_t val);

    uint32_t push_data(uint8_t val);
    uint32_t push_data(uint16_t val);
    uint32_t push_data(uint32_t val);
    uint32_t add_string(const std::string &str);

    uint32_t add_section(const std::string &section);
    bool has_section(const std::string &section);
    uint32_t set_active_section(const std::string &section);
    uint32_t get_section_id(const std::string &section);

    void set_global(const std::string &identifier);
    bool insert_symbol(const symbol &sym);
    void insert_relaxation(uint32_t address, uint32_t addend);
    const symbol &insert_relocation(const std::string &symbol_identifier, uint32_t address,
                                    riscv_relocation type, uint32_t addend);

    void insert_relocation(uint32_t address, riscv_relocation type, uint32_t addend);
    void set_entrypoint(uint32_t address);
    void write(std::ostream &fstream);
};

#endif
