#include "elf_file.hpp"
#include "elfio.hpp"
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iostream>

uint32_t elf_file::add_string(const std::string &str) {
    ELFIO::string_section_accessor str_writer(str_sec);
    return str_writer.add_string(str);
}

std::string &elf_file::get_working_data() {
    assert(active_section != sections.end());
    return active_section->second.data;
}

uint32_t elf_file::p2align(uint32_t p2) {
    auto &working_data = get_working_data();
    const auto size = working_data.size();

    if ((size & ((1 << p2) - 1)) == 0)
        return size;

    const uint32_t right_shift = size >> p2;
    const uint32_t aligned_size = (right_shift + 1) << p2;
    const uint32_t diff = aligned_size - size;

    working_data.insert(size, diff, 0);

    return aligned_size;
}

void elf_file::insert_data(uint8_t val) { get_working_data().push_back(val); }

void elf_file::insert_data(uint16_t val) {
    insert_data(static_cast<uint8_t>(val));     //least significant first
    insert_data(static_cast<uint8_t>(val >> 8));//most significant second
}

void elf_file::insert_data(uint32_t val) {
    insert_data(static_cast<uint16_t>(val));      //least significant first
    insert_data(static_cast<uint16_t>(val >> 16));//most significant second
}

uint32_t elf_file::push_data(uint32_t val) {
    const auto placement_address = p2align(2);
    insert_data(val);
    return placement_address;
}

uint32_t elf_file::push_data(uint16_t val) {
    const auto placement_address = p2align(1);
    insert_data(val);
    return placement_address;
}

uint32_t elf_file::push_data(uint8_t val) {
    const auto placement_address = get_working_data().size();
    insert_data(val);
    return placement_address;
}

uint32_t elf_file::write_symbol(const symbol &sym) {
    assert(sym.identifier.length() != 0);

    const ELFIO::Elf32_Word name = add_string(sym.identifier);
    const ELFIO::Elf32_Addr address_value = sym.address;
    const ELFIO::Elf32_Word size = sym.size;
    unsigned char info = ELFIO::STT_NOTYPE;
    ELFIO::Elf32_Word section_index = sym.section_id;

    //---determine info (type)---//
    switch (sym.type) {
        case symbol_type::data:
            info = ELFIO::STT_OBJECT;
            break;

        case symbol_type::fun:
            info = ELFIO::STT_FUNC;
            break;

        case symbol_type::undefined:
            info = ELFIO::STT_NOTYPE;
            break;

        default:
            assert(false);
    }

    //---determine info (visibility)---//
    if (sym.scope == symbol_scope::global)
        info |= ELFIO::STB_GLOBAL << 4;
    else
        info |= ELFIO::STB_LOCAL << 4;

    ELFIO::symbol_section_accessor symbol_writer(writer, symbol_sec);

    const auto idx = symbol_writer.add_symbol(name, address_value, size, info, 0, section_index);
    if (sym.scope == symbol_scope::local)
        symbol_sec->set_info((idx + 1));

    return idx;
}

bool elf_file::insert_symbol(const symbol &sym) {
    assert(sym.identifier.empty() == false);
    const bool unique = symbols.insert({sym.identifier, sym}).second;
    return unique;
}

section_t &elf_file::get_active_section() {
    assert(active_section != sections.end());
    return active_section->second;
}

void elf_file::insert_relocation(uint32_t address, riscv_relocation type, uint32_t addend) {
    const relocation reloc = {.address = address, .type = type, .symbol_identifier = "", .addend = addend};
    assert(get_active_section().elf_rela_sec != NULL);
    get_active_section().relocations.push_back(reloc);
}

void elf_file::insert_relaxation(uint32_t address, uint32_t addend) {
    const relocation reloc = {.address = address,
                              .type = riscv_relocation::R_RISCV_RELAX,
                              .symbol_identifier = "",
                              .addend = addend};

    assert(get_active_section().elf_rela_sec != NULL);
    get_active_section().relocations.push_back(reloc);
}

const symbol &elf_file::insert_relocation(const std::string &symbol_identifier, uint32_t address,
                                          riscv_relocation type, uint32_t addend) {
    //---find symbol---//
    assert(symbol_identifier.empty() == false);
    auto symbol_it = symbols.find(symbol_identifier);
    if (symbol_it == symbols.end()) {
        const symbol global_symbol = {.identifier = symbol_identifier,
                                      .index = 0,
                                      .section_id = 0,
                                      .address = 0,
                                      .scope = symbol_scope::global,
                                      .type = symbol_type::undefined,
                                      .size = 0};

        symbol_it = symbols.insert({symbol_identifier, global_symbol}).first;
    }

    const relocation reloc = {.address = address,
                              .type = type,
                              .symbol_identifier = symbol_identifier,
                              .addend = addend};

    assert(get_active_section().elf_rela_sec != NULL);
    get_active_section().relocations.push_back(reloc);

    return symbol_it->second;
}

void elf_file::set_global(const std::string &identifier) {
    auto it = symbols.find(identifier);
    if (it != symbols.end()) {
        it->second.scope = symbol_scope::global;
    } else {
        const symbol global_symbol = {.identifier = identifier,
                                      .index = 0,
                                      .section_id = 0,
                                      .address = 0,
                                      .scope = symbol_scope::global,
                                      .type = symbol_type::undefined,
                                      .size = 0};

        symbols.insert({identifier, global_symbol});
    }
}

void elf_file::initialise() {
    writer.create(ELFIO::ELFCLASS32,
                  ELFIO::ELFDATA2LSB);  //set 32 bit and little endian 2s compliment
    writer.set_os_abi(0);               //UNIX - System V ABI
    writer.set_machine(ELFIO::EM_RISCV);//riscv cpu architecture
    writer.set_flags(0);
    writer.set_type(ELFIO::ET_REL);

    //---string section---//
    str_sec = writer.sections.add(".strtab");
    str_sec->set_type(ELFIO::SHT_STRTAB);
    str_sec->set_addr_align(1);

    //---symbtab section---//
    symbol_sec = writer.sections.add(".symtab");
    symbol_sec->set_type(ELFIO::SHT_SYMTAB);//set symbol table type
    symbol_sec->set_addr_align(0x4);        //set allignment to 4 bytes (word)
    symbol_sec->set_entry_size(writer.get_default_entry_size(ELFIO::SHT_SYMTAB));
    symbol_sec->set_link(str_sec->get_index());//link to the string table

    //---atributes section---//
    const ELFIO::Elf_Word SHT_RISCV_ATTRIBUTES = 0x070000003;
    ELFIO::section *attributes_sec = writer.sections.add(".riscv.attributes");
    attributes_sec->set_type(SHT_RISCV_ATTRIBUTES);
    attributes_sec->set_addr_align(1);

    std::string added_str = "_m2p0_zmmul1p0";
    attributes = {'A', 0x019 + 14, 0, 0, 0};
    attributes += std::string{"riscv"};
    attributes += {0, 0x01, 0x0f + 14, 0, 0, 0, 5};
    attributes += std::string{"rv32i2p1_m2p0_zmmul1p0"};
    attributes += {0};

    attributes_sec->set_data(attributes.c_str(), attributes.size());
    ELFIO::symbol_section_accessor sym_writer(writer, symbol_sec);

    const auto idx = sym_writer.add_symbol(0, 0, 0, ELFIO::STT_SECTION | ELFIO::STB_LOCAL << 4, 0,
                                           attributes_sec->get_index());
    symbol_sec->set_info((idx + 1));
}

uint32_t elf_file::get_section_id(const std::string &section_name) {
    auto it = sections.find(section_name);
    if (it == sections.end())
        return 0;

    return it->second.elf_sec->get_index();
}

uint32_t elf_file::set_active_section(const std::string &section_name) {
    active_section = sections.find(section_name);
    assert(active_section != sections.end());
    return active_section->second.elf_sec->get_index();
}

bool elf_file::has_section(const std::string &section) { return sections.find(section) != sections.end(); }

uint32_t elf_file::add_section(const std::string &section_name) {
    assert(section_name.length());
    assert(!has_section(section_name));

    ELFIO::symbol_section_accessor sym_writer(writer, symbol_sec);
    section_t section;

    section.elf_sec = writer.sections.add(std::string{section_name});
    section.elf_sec->set_addr_align(4);//set allignment to 4 bytes (word)
    section.elf_rela_sec = NULL;
    if (section_name.compare(0, 5, ".text") == 0) {
        /*---text section--*/
        section.elf_sec->set_type(ELFIO::SHT_PROGBITS);//set section type to program data
        section.elf_sec->set_flags(
                ELFIO::SHF_EXECINSTR |//executable section
                ELFIO::SHF_ALLOC);    //section needs to be allocated in memory during execution

        sym_writer.add_symbol(add_string("$xrv32i2p1"), 0, 0, ELFIO::STT_NOTYPE | ELFIO::STB_LOCAL << 4, 0,
                              section.elf_sec->get_index());

        const std::string rela_section_name = std::string{".rela"} + std::string{section_name};
        section.elf_rela_sec = writer.sections.add(rela_section_name);//relocates for text section

        section.elf_rela_sec->set_type(ELFIO::SHT_RELA);//relocate table type
        section.elf_rela_sec->set_flags(ELFIO::SHF_INFO_LINK);
        section.elf_rela_sec->set_info(section.elf_sec->get_index());//in context of text section
        section.elf_rela_sec->set_link(symbol_sec->get_index());     //link to symbol table section
        section.elf_rela_sec->set_entry_size(writer.get_default_entry_size(ELFIO::SHT_RELA));
        section.elf_rela_sec->set_addr_align(0x4);//set allignment to 4 bytes (word)

        sym_writer.add_symbol(0, 0, 0, ELFIO::STT_SECTION | ELFIO::STB_LOCAL << 4, 0,
                              section.elf_sec->get_index());

    } else if (section_name.compare(0, 5, ".data") == 0) {
        /*---data section--*/
        section.elf_sec->set_type(ELFIO::SHT_PROGBITS);//set section type to program data
        section.elf_sec->set_flags(
                ELFIO::SHF_WRITE |//writable datab
                ELFIO::SHF_ALLOC);//section needs to be allocated in memory during execution

        section.elf_sec->set_addr_align(1);//set allignment to 4 bytes (word)

    } else if (section_name.compare(0, 4, ".bss") == 0) {
        /*---bss section---*/
        section.elf_sec->set_type(ELFIO::SHT_NOBITS);// Program space with no data
        section.elf_sec->set_flags(
                ELFIO::SHF_WRITE |//writable data
                ELFIO::SHF_ALLOC);//section needs to be allocated in memory during execution

        section.elf_sec->set_addr_align(1);//set allignment to 4 bytes (word)

    } else if (section_name.compare(0, 7, ".rodata") == 0) {
        /*---rodata section---*/
        section.elf_sec->set_type(ELFIO::SHT_PROGBITS);//set section type to program data
        section.elf_sec->set_flags(
                ELFIO::SHF_ALLOC);//section needs to be allocated in memory during execution

    } else {
        /*---generic section---*/
        section.elf_sec->set_type(ELFIO::STT_NOTYPE);//set section type to program data
    }

    const int idx = sym_writer.add_symbol(0, 0, 0, ELFIO::STT_SECTION | ELFIO::STB_LOCAL << 4, 0,
                                          section.elf_sec->get_index());
    symbol_sec->set_info(idx + 1);

    sections.insert({section_name, section});

    return section.elf_sec->get_index();
}

void elf_file::write(std::ostream &fstream) {
    //---write local symbols---//
    for (auto &[dummy, sym]: symbols)
        if (sym.scope == symbol_scope::local)
            sym.index = write_symbol(sym);

    //---write global symbols---//
    for (auto &[dummy, sym]: symbols)
        if (sym.scope == symbol_scope::global)
            sym.index = write_symbol(sym);

    //---write sections---//
    for (auto &[dummy, sec]: sections) {
        if (sec.elf_rela_sec != NULL) {
            //---write relocations---//
            ELFIO::relocation_section_accessor rel_writer(writer, sec.elf_rela_sec);
            for (auto &relocation: sec.relocations) {
                if (relocation.symbol_identifier.empty()) {
                    rel_writer.add_entry(relocation.address, 0, (int) relocation.type, relocation.addend);
                } else {
                    auto symbol_it = symbols.find(relocation.symbol_identifier);
                    assert(symbol_it != symbols.end());
                    const ELFIO::Elf_Word symbol_index = symbol_it->second.index;
                    rel_writer.add_entry(relocation.address, symbol_index, (int) relocation.type,
                                         relocation.addend);
                }
            }
        }

        //---set section data---//
        if (sec.elf_sec->get_type() == ELFIO::SHT_NOBITS)
            sec.elf_sec->set_size(sec.data.size());
        else
            sec.elf_sec->set_data(sec.data.c_str(), sec.data.size());
    }

    writer.set_entry(entry_point);
    writer.save(fstream);
}
