// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import os
import arrays

enum PeCharacteristics {
	// 1: relocation info stripped
	// 2: file is executable
	// 4: line numbers stripped
	// 8: local symbols stripped
	// 20: app can handle > 2GB
	executable_image = 0x2f
}

const image_base = i64(0x400000)

const pe_section_align = 0x1000
const pe_file_align = 0x0200

const pe_coff_hdr_size = 0x18
const pe_opt_hdr_size = 0xf0
const pe32_plus_opt_hdr_size = 0x70
const pe_header_size = pe_file_align
const pe_section_header_size = 0x28
const pe_stack_size = 0x200000 // gcc default on windows

const pe_heap_size = 0x100000 // gcc default on windows

// tcc defaults
const pe_major_linker_version = 6
const pe_minor_linker_version = 0
const pe_major_os_version = 4
const pe_minor_os_version = 0
const pe_major_subsystem_version = 4
const pe_minor_subsystem_version = 0

const pe_header_machine_offset = 4
const pe_number_of_sections_offset = 6

const pe_num_data_dirs = 0x10

const dos_stub_end = 0x80
const optdr_location = 0x98

// reference: https://learn.microsoft.com/en-us/windows/win32/debug/pe-format?redirectedfrom=MSDN#section-flags
const pe_scn_type_no_pad = 0x00000008
const pe_scn_cnt_code = 0x00000020
const pe_scn_cnt_initialized_data = 0x00000040
const pe_scn_cnt_uninitialized_data = 0x00000080
const pe_scn_lnk_info = 0x00000200
const pe_scn_lnk_remove = 0x00000400
const pe_scn_lnk_comdat = 0x00001000
const pe_scn_gprel = 0x00008000
const pe_scn_lnk_nreloc_ovfl = 0x01000000
const pe_scn_mem_discardable = 0x02000000
const pe_scn_mem_not_cached = 0x04000000
const pe_scn_mem_not_paged = 0x08000000
const pe_scn_mem_shared = 0x10000000
const pe_scn_mem_execute = 0x20000000
const pe_scn_mem_read = 0x40000000
const pe_scn_mem_write = i32(u32(0x80000000))
// obj files only:
const pe_scn_align_1bytes = 0x00100000
const pe_scn_align_2bytes = 0x00200000
const pe_scn_align_4bytes = 0x00300000
const pe_scn_align_8bytes = 0x00400000
const pe_scn_align_16bytes = 0x00500000
const pe_scn_align_32bytes = 0x00600000
const pe_scn_align_64bytes = 0x00700000
const pe_scn_align_128bytes = 0x00800000
const pe_scn_align_256bytes = 0x00900000
const pe_scn_align_512bytes = 0x00a00000
const pe_scn_align_1024bytes = 0x00b00000
const pe_scn_align_2048bytes = 0x00c00000
const pe_scn_align_4096bytes = 0x00d00000
const pe_scn_align_8192bytes = 0x00e00000

enum PeMachine as u16 {
	i386  = 0x014c
	amd64 = 0x8664
	arm64 = 0xaa64
}

enum PeMagic as u16 {
	mz       = 0x5a4d // dos(mz)-header magic number
	pe       = 0x4550 // pe-header magic number
	pe32     = 0x010b // PE32 optional header magic number
	pe32plus = 0x020b // PE32+ optional header magic number
}

// reference: https://learn.microsoft.com/en-us/windows/win32/debug/pe-format?redirectedfrom=MSDN#windows-subsystem
enum PeSubsystem as u16 {
	unknown = 0
	native
	windows_gui
	windows_cui
	os2_cui
	posix_cui
	native_windows
	windows_ce_gui
	efi_application
	efi_boot_service_driver
	efi_runtime_driver
	efi_rom
	xbox
	windows_boot_application
}

// reference: https://learn.microsoft.com/en-us/windows/win32/debug/pe-format?redirectedfrom=MSDN#dll-characteristics
enum DllCharacteristics as u16 {
	high_entropy_va       = 0x0020
	dynamic_base          = 0x0040
	force_integrity       = 0x0080
	nx_compat             = 0x0100
	no_isolation          = 0x0200
	no_seh                = 0x0400
	no_bind               = 0x0800
	appcontainer          = 0x1000
	wdm_driver            = 0x2000
	guard_cf              = 0x4000
	terminal_server_aware = 0x8000
}

fn (mut g Gen) get_pe_machine() PeMachine {
	return match g.pref.arch {
		.amd64 {
			PeMachine.amd64
		}
		.arm64 {
			PeMachine.arm64
		}
		else {
			g.n_error('arch ${g.pref.arch} is not supported with PE right now')
		}
	}
}

pub fn (mut g Gen) gen_pe_header() {
	pe_header := [
		i32(PeMagic.pe),
		0,
		i32(g.get_pe_machine()), // machine
		2, // number of sections
		0,
		0, // timestamp
		0,
		0, // symbol table
		0, // number of symbols
		0,
		pe_opt_hdr_size, // 40 // size of optional header
		i32(PeCharacteristics.executable_image),
	]

	// debug descriptions when `-v` is used
	mut pe_header_description := map[int]string{}
	if g.pref.is_verbose {
		pe_header_description = {
			0x04: '; mMagic'
			0x06: '; mMachine'
			0x08: '; mNumberOfSections'
			0x0c: '; mTimeDateStamp'
			0x10: '; mPointerToSymbolTable'
			0x14: '; mNumberOfSymbols'
			0x16: '; mSizeOfOptionalHeader'
			0x18: '; mCharacteristics'
		}
		assert pe_coff_hdr_size == pe_header.len * 2
	}

	g.pe_coff_hdr_pos = g.pos()
	for i, b in pe_header {
		end_addr := i * 2 + 2 // align correctly with description table
		g.write16(b)
		if g.pref.is_verbose && pe_header_description[end_addr] != '' {
			g.println(pe_header_description[end_addr])
		}
	}

	g.println('^^^ PE Header')

	// optional here comes here
	p_opthdr := g.pos() // should be 0x98
	if p_opthdr != optdr_location {
		g.n_error('Invalid optdr location ${p_opthdr} != 0x98')
	}

	opt_hdr := g.get_pe_optional_header()
	g.gen_pe_optional_header(opt_hdr)
}

struct Pe32OptionalHeader {
	// TODO: potential support for 32-bit
}

struct Pe32PlusOptionalHeader {
	// standard fields
	// refenrence: https://learn.microsoft.com/en-us/windows/win32/debug/pe-format?redirectedfrom=MSDN#optional-header-standard-fields-image-only
	magic                      PeMagic
	major_linker_version       u8
	minor_linker_version       u8
	size_of_code               i32
	size_of_initialized_data   i32
	size_of_uninitialized_data i32
	address_of_entry_point     i32
	base_of_code               i32
	// PE32 ONLY:
	// base_of_data u32
	// NT fields
	// reference: https://learn.microsoft.com/en-us/windows/win32/debug/pe-format?redirectedfrom=MSDN#optional-header-windows-specific-fields-image-only
	image_base              i64 // u32 for PE32
	section_alignment       i32
	file_alignment          i32
	major_os_version        i16
	minor_os_version        i16
	major_image_version     i16
	minor_image_version     i16
	major_subsystem_version i16
	minor_subsystem_version i16
	win32_version_value     i32
	size_of_image           i32
	size_of_headers         i32
	checksum                i32
	subsystem               PeSubsystem
	dll_characteristics     i16
	size_of_stack_reserve   i64 // u32 for PE32
	size_of_stack_commit    i64 // u32 for PE32
	size_of_heap_reserve    i64 // u32 for PE32
	size_of_heap_commit     i64 // u32 for PE32
	loader_flags            i32 // reserved, MUST be zero
	number_of_rva_and_sizes i32
}

fn (mut g Gen) get_pe32_plus_optional_header() Pe32PlusOptionalHeader {
	return Pe32PlusOptionalHeader{
		magic:                   .pe32plus
		major_linker_version:    pe_major_linker_version
		minor_linker_version:    pe_minor_linker_version
		image_base:              image_base
		section_alignment:       pe_section_align
		file_alignment:          pe_file_align
		major_os_version:        pe_major_os_version
		minor_os_version:        pe_minor_os_version
		major_subsystem_version: pe_major_subsystem_version
		minor_subsystem_version: pe_minor_subsystem_version
		size_of_headers:         pe_header_size
		subsystem:               .windows_cui
		size_of_stack_reserve:   pe_stack_size
		size_of_stack_commit:    pe_stack_size
		size_of_heap_reserve:    pe_heap_size
		number_of_rva_and_sizes: pe_num_data_dirs
	}
}

enum Pe32PlusOPtionalHeaderField {
	size_of_code             = 4
	size_of_initialized_data = 8
	address_of_entry_point   = 16
	base_of_code             = 20
	size_of_image            = 56
	number_of_rva_and_sizes  = 108
}

// implemented because __offsetof() + [packed] structs wasn't consistent across OSs
@[inline]
fn pe32_plus_optional_header_offsetof(field Pe32PlusOPtionalHeaderField) i64 {
	return i64(field)
}

// for later expandability
type PeOptionalHeader = Pe32OptionalHeader | Pe32PlusOptionalHeader

fn optional_header_arch(opt_hdr PeOptionalHeader) string {
	return match opt_hdr {
		Pe32PlusOptionalHeader { 'PE32+' }
		Pe32OptionalHeader { 'PE32' }
	}
}

fn (mut g Gen) get_pe_optional_header() PeOptionalHeader {
	match g.pref.arch {
		.amd64 {
			return g.get_pe32_plus_optional_header()
		}
		// TODO: arm64
		else {
			g.n_error('unsupported PE platform')
		}
	}
}

fn (mut g Gen) gen_pe_optional_header(opt_hdr PeOptionalHeader) {
	if opt_hdr !is Pe32PlusOptionalHeader {
		g.n_error('unsupported optional header architecture ${optional_header_arch(opt_hdr)}')
	}

	h := opt_hdr as Pe32PlusOptionalHeader
	g.pe_opt_hdr_pos = g.pos()

	// standard fields
	g.write16(i32(h.magic))
	g.println('; mMagic')
	g.write8(h.major_linker_version)
	g.println('; mMajorLinkerVersion')
	g.write8(h.minor_linker_version)
	g.println('; mMinorLinkerVersion')

	g.write32(h.size_of_code)
	g.println('; mSizeOfCode')

	g.write32(h.size_of_initialized_data)
	g.println('; mSizeOfInitializedData')
	g.write32(h.size_of_uninitialized_data)
	g.println('; mSizeOfUninitializedData')
	g.write32(h.address_of_entry_point)
	g.println('; mAddressOfEntryPoint')
	g.write32(h.base_of_code)
	g.println('; mBaseOfCode')

	// NT fields
	g.write64(h.image_base)
	g.println('; mImageBase')
	g.write32(h.section_alignment)
	g.println('; mSectionAlignment')
	g.write32(h.file_alignment)
	g.println('; mFileAlignment')
	g.write16(i32(h.major_os_version))
	g.println('; mMajorOperatingSystemVersion')
	g.write16(i32(h.minor_os_version))
	g.println('; mMinorOperatingSystemVersion')
	g.write16(i32(h.major_image_version))
	g.println('; mMajorImageVersion')
	g.write16(i32(h.minor_image_version))
	g.println('; mMinorImageVersion')
	g.write16(i32(h.major_subsystem_version))
	g.println('; mMajorSubsystemVersion')
	g.write16(i32(h.minor_subsystem_version))
	g.println('; mMinorSubsystemVersion')
	g.write32(h.win32_version_value)
	g.println('; mWin32VersionValue')
	g.write32(h.size_of_image)
	g.println('; mSizeOfImage')
	g.write32(h.size_of_headers)
	g.println('; mSizeOfHeaders')
	g.write32(h.checksum)
	g.println('; mCeckSum')
	g.write16(i32(h.subsystem))
	g.println('; mSubsystem')
	g.write16(i32(h.dll_characteristics))
	g.println('; mDllCharacteristics')
	g.write64(h.size_of_stack_reserve)
	g.println('; mSizeOfStackReserve')
	g.write64(h.size_of_stack_commit)
	g.println('; mSizeOfStackCommit')
	g.write64(h.size_of_heap_reserve)
	g.println('; mSizeOfHeapReserve')
	g.write64(h.size_of_heap_commit)
	g.println('; mSizeOfHeapCommit')
	g.write32(h.loader_flags)
	g.println('; mLoaderFlags')
	g.write32(h.number_of_rva_and_sizes)
	g.println('; mNumberOfRvaAndSizes')

	g.println('^^^ PE Optional Header')
}

struct PeDataDir {
	rva  i32
	size i32
}

struct PeDataDirs {
mut:
	debugnames [pe_num_data_dirs]string
	dirs       [pe_num_data_dirs]PeDataDir
	base_addr  i64
}

const pe_data_dir_names = [
	'export table',
	'import table',
	'resource table',
	'exception table',
	'attribute certificate table',
	'base relocation table',
	'debug data',
	'architecture',
	'global ptr',
	'thread local storage table',
	'load config table',
	'bound import table',
	'import address table',
	'delay import descriptors',
	'COM runtime descriptor',
	'reserved',
]

const pe_default_data_dirs = [
	PeDataDir{},
	PeDataDir{
		rva:  0x1000
		size: 0x2000
	},
	// remaining filled in implicitly
]

fn get_pe_data_dirs() PeDataDirs {
	assert pe_data_dir_names.len == pe_num_data_dirs
	assert pe_default_data_dirs.len <= pe_num_data_dirs

	mut dd := PeDataDirs{}
	for i in 0 .. pe_num_data_dirs {
		dd.dirs[i] = pe_default_data_dirs[i] or { PeDataDir{} }
		dd.debugnames[i] = pe_data_dir_names[i]
	}
	return dd
}

fn (mut g Gen) gen_pe_data_dirs() {
	g.pe_data_dirs.base_addr = g.pos()
	for i, dd in g.pe_data_dirs.dirs {
		g.write32(dd.rva)
		g.write32(dd.size)
		g.println('; ${g.pe_data_dirs.debugnames[i]}')
	}
	g.println('^^^ data directories (PE)')
}

// reference: https://learn.microsoft.com/en-us/windows/win32/debug/pe-format?redirectedfrom=MSDN#section-table-section-headers
struct PeSectionHeader {
	name [8]u8
mut:
	virtual_size           i32
	virtual_address        i32
	size_of_raw_data       i32
	pointer_to_raw_data    i32
	pointer_to_relocations i32
	pointer_to_linenumbers i32
	number_of_relocations  i16
	number_of_linenumbers  i16
	characteristics        i32
}

enum PeSectionHeaderField {
	virtual_size        = 8
	virtual_address     = 12
	size_of_raw_data    = 16
	pointer_to_raw_data = 20
}

// implemented because __offsetof() + [packed] structs wasn't consistent across OSs
@[inline]
fn pe_section_header_offsetof(field PeSectionHeaderField) i64 {
	return i64(field)
}

struct PeSection {
	name string
mut:
	header     PeSectionHeader
	header_pos i64
}

fn (mut s PeSection) set_pointer_to_raw_data(mut g Gen, pointer i32) {
	s.header.pointer_to_raw_data = pointer
	g.write32_at(s.header_pos + pe_section_header_offsetof(.pointer_to_raw_data), pointer)
}

fn (mut s PeSection) set_size_of_raw_data(mut g Gen, size i32) {
	if size < s.header.virtual_size {
		s.set_virtual_size(mut g, size)
	}

	s.header.pointer_to_raw_data = size
	g.write32_at(s.header_pos + pe_section_header_offsetof(.size_of_raw_data), size)
}

fn (mut s PeSection) set_virtual_address(mut g Gen, addr i32) {
	aligned := (addr + pe_section_align - 1) & ~(pe_section_align - 1)

	s.header.virtual_address = aligned
	g.write32_at(s.header_pos + pe_section_header_offsetof(.virtual_address), aligned)
}

fn (mut s PeSection) set_virtual_size(mut g Gen, size i32) {
	aligned := (size + pe_section_align - 1) & ~(pe_section_align - 1)

	s.header.virtual_size = aligned
	g.write32_at(s.header_pos + pe_section_header_offsetof(.virtual_size), aligned)
}

fn (mut g Gen) create_pe_section(name string, header PeSectionHeader) PeSection {
	return PeSection{
		name:   name
		header: header
	}
}

fn (mut g Gen) gen_pe_section_header(mut section PeSection) {
	sh := &section.header
	section.header_pos = g.pos()

	assert sh.name.len <= 8
	g.write_string_with_padding(section.name, 8)
	g.println('"${section.name}"')

	g.write32(sh.virtual_size)
	g.println('; VirtualSize')
	g.write32(sh.virtual_address)
	g.println('; VirtualAddress')
	g.write32(sh.size_of_raw_data)
	g.println('; SizeOfRawData')
	g.write32(sh.pointer_to_raw_data)
	g.println('; PointerToRawData')
	g.write32(sh.pointer_to_relocations)
	g.println('; PointerToRelocations')
	g.write32(sh.pointer_to_linenumbers)
	g.println('; PointerToLinenumbers')
	g.write16(i32(sh.number_of_relocations))
	g.println('; NumberOfRelocations')
	g.write16(i32(sh.number_of_linenumbers))
	g.println('; NumberOfLinenumbers')
	g.write32(i32(sh.characteristics))
}

fn (mut g Gen) gen_pe_sections() {
	for mut section in g.pe_sections {
		g.gen_pe_section_header(mut section)
		g.println('; ^^^ section header (pe) "${section.name}"')
	}
}

fn (mut g Gen) get_pe_section(name string) ?PeSection {
	return arrays.find_first(g.pe_sections, fn [name] (s PeSection) bool {
		return s.name == name
	})
}

fn (mut g Gen) get_pe_section_index(name string) ?int {
	for i, section in g.pe_sections {
		if section.name == name {
			return i
		}
	}
	return none
}

struct PeImportDirectoryTable {
	import_lookup_table_rva i32
	time_date_stamp         i32
	forwarder_chain         i32
	name_rva                i32
mut:
	import_address_table_rva i32
}

enum PeImportDirectoryTableField {
	name_rva                 = 12
	import_address_table_rva = 16
}

// implemented because __offsetof() + [packed] structs wasn't consistent across OSs
@[inline]
fn pe_idt_offsetof(field PeImportDirectoryTableField) i64 {
	return i64(field)
}

fn default_pe_idt() PeImportDirectoryTable {
	return PeImportDirectoryTable{
		forwarder_chain: i32(u32(0xffffffff))
		time_date_stamp: i32(u32(0xffffffff))
	}
}

const pe_idt_field_description = ['Import Lookup Table RVA', 'Time/Date Stamp', 'Forwarder Chain',
	'Name RVA', 'Import Address Table RVA']

fn (mut g Gen) gen_pe_idt(idt &PeImportDirectoryTable, dll_name string) {
	fields := [idt.import_lookup_table_rva, idt.time_date_stamp, idt.forwarder_chain, idt.name_rva,
		idt.import_address_table_rva]

	assert fields.len == pe_idt_field_description.len
	for i, field in fields {
		g.write32(field)
		g.println('; ' + pe_idt_field_description[i])
	}
	g.println('^^^ Import Directory Table (${dll_name})')
}

struct PeDllImport {
	name string
mut:
	functions []string
	idt_pos   i64
}

// reference: https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#the-idata-section
fn (mut g Gen) gen_pe_idata() {
	idata_section_index := g.get_pe_section_index('.idata') or {
		g.n_error('no ".idata" section header generated')
	}
	mut idata_section := &mut g.pe_sections[idata_section_index]

	g.align_to(pe_file_align)
	g.println('; padding to 0x${g.pos().hex()}')

	idata_pos := g.pos()
	idata_section.set_pointer_to_raw_data(mut g, i32(idata_pos))

	// Allow for adding more search paths for DLLs, through setting VNATIVE_PE_DLL_PATH:
	dll_paths := os.getenv('VNATIVE_PE_DLL_PATH').split(':').filter(it != '')
	if dll_paths.len > 0 {
		g.linker_include_paths << dll_paths
	}

	wine_system32_folder := os.join_path(os.home_dir(), '.wine/drive_c/windows/system32')
	if os.exists(wine_system32_folder) {
		g.linker_include_paths << wine_system32_folder
	}

	g.linker_include_paths << '.'

	// keep only the unique paths:
	mut upaths := map[string]bool{}
	for path in g.linker_include_paths {
		upaths[path] = true
	}
	g.linker_include_paths = upaths.keys()

	mut dll_files := ['kernel32.dll', 'user32.dll', 'msvcrt.dll']
	dll_files << g.linker_libs.map(it + '.dll')
	mut dlls := dll_files
		.map(g.lookup_system_dll(it) or { g.n_error('${it}: ${err}') })
		.map(index_dll(it) or { g.n_error('${it}: ${err}') })

	g.extern_symbols << [
		'GetStdHandle',
		'ExitProcess',
		'WriteFile',
	]

	for symbol in g.extern_symbols {
		mut sym := symbol.all_after('C.')
		mut found := false
		for mut dll in dlls {
			if sym in dll.exports {
				found = true
				dll.exports[sym] = true
				break
			}
		}

		if !found {
			if sym == 'CaptureStackBackTrace' {
				sym = 'RtlCaptureStackBackTrace'
			} else if sym == '__debugbreak' {
				sym = 'DebugBreak'
			}
			for mut dll in dlls {
				if sym in dll.exports {
					found = true
					dll.exports[sym] = true
					break
				}
			}
		}

		if !found {
			eprintln('could not find symbol `${sym}` in ${dll_files}')
		}
	}

	mut imports := dlls.map(it.to_import())

	// import directory table
	for mut imp in imports {
		// generate idt
		idt := default_pe_idt()
		imp.idt_pos = g.pos()
		g.gen_pe_idt(&idt, imp.name)
	}
	g.gen_pe_idt(&PeImportDirectoryTable{}, 'null entry') // null entry

	for imp in imports {
		g.write32_at(imp.idt_pos + pe_idt_offsetof(.import_address_table_rva),
			i32(g.pos() - idata_pos) + idata_section.header.virtual_address + 4)

		for func in imp.functions {
			g.pe_dll_relocations[func] = g.pos()
			g.write64(i64(0)) // filled in later
			g.println('; name lookup addr to "${func}"')
		}
		g.write64(i64(0)) // null entry
		g.println('; null entry')
		g.println('^^^ import lookup table ("${imp.name}")')
	}

	// null entry
	g.zeroes(4)
	g.println('; null entry')

	// dll names
	for imp in imports {
		g.write32_at(imp.idt_pos + pe_idt_offsetof(.name_rva), i32(g.pos() - idata_pos) +
			idata_section.header.virtual_address)
		g.write_string(imp.name)
		g.println('"${imp.name}"')
	}

	for imp in imports {
		for func in imp.functions {
			g.write64_at(g.pe_dll_relocations[func], i64(u64(g.pos() - idata_pos +
				i64(idata_section.header.virtual_address)) << 32))
			g.pe_dll_relocations[func] = g.pe_dll_relocations[func] - idata_pos +
				i64(idata_section.header.virtual_address) + 4 // set correct lookup address for function calls

			g.write16(0) // export pointer index; we go via names, so 0
			g.write_string(func)
			g.println('"${func}"')
		}

		// null entry
		g.zeroes(4)
		g.println('; null entry')
		g.println('^^^ hint/name table (${imp.name})')
	}

	// write the section size
	idata_size := i32(g.pos() - idata_pos)
	idata_section.set_size_of_raw_data(mut g, idata_size)
	idata_section.set_virtual_size(mut g, idata_size)
}

pub fn (mut g Gen) generate_pe_header() {
	g.gen_dos_header()
	g.gen_dos_stub()
	g.gen_pe_header()

	g.gen_pe_data_dirs()

	g.pe_sections = [
		g.create_pe_section('.idata',
			virtual_address: 0x1000
			characteristics: pe_scn_cnt_initialized_data | pe_scn_mem_read | pe_scn_mem_write
		),
		g.create_pe_section('.text',
			characteristics: pe_scn_cnt_initialized_data | pe_scn_cnt_code | pe_scn_mem_execute | pe_scn_mem_read
		),
	]

	g.gen_pe_sections()
	g.gen_pe_idata()

	g.align_to(pe_file_align)
	g.println('')
	g.println('^^^ padding to addr 0x${g.pos().hex()}')

	// patch virtual addr of .text section
	g.patch_section_virtual_addrs()

	text_section_index := g.get_pe_section_index('.text') or {
		g.n_error('no ".text" section generated')
	}
	mut text_section := &mut g.pe_sections[text_section_index]
	g.code_start_pos = g.pos()
	text_section.set_pointer_to_raw_data(mut g, i32(g.code_start_pos))

	g.code_gen.call(0)
	g.code_gen.ret()
	g.main_fn_addr = g.pos()
}

fn (mut g Gen) patch_section_virtual_addrs() {
	if g.pe_sections.len == 0 {
		return
	}

	mut addr := g.pe_sections[0].header.virtual_address
	for mut section in g.pe_sections {
		section.set_virtual_address(mut g, addr)
		addr += section.header.virtual_size

		match section.name {
			'.text' {
				g.write32_at(g.pe_opt_hdr_pos + pe32_plus_optional_header_offsetof(.base_of_code),
					section.header.virtual_address)
				g.write32_at(g.pe_opt_hdr_pos +
					pe32_plus_optional_header_offsetof(.address_of_entry_point), section.header.virtual_address)
			}
			else {}
		}
	}
}

fn (mut g Gen) patch_pe_code_size() {
	code_size := i32(g.file_size_pos - g.code_start_pos)

	g.write32_at(g.pe_opt_hdr_pos + pe32_plus_optional_header_offsetof(.size_of_code),
		code_size)
	g.write32_at(g.pe_opt_hdr_pos + pe32_plus_optional_header_offsetof(.size_of_initialized_data),
		code_size)

	text_section_index := g.get_pe_section_index('.text') or {
		g.n_error('no ".text" section generated')
	}
	mut text_section := &mut g.pe_sections[text_section_index]
	text_section.set_size_of_raw_data(mut g, code_size)
	text_section.set_virtual_size(mut g, code_size)
}

fn (mut g Gen) patch_pe_image_size() {
	last_section := g.pe_sections.last()
	image_size := (last_section.header.virtual_address + last_section.header.virtual_size +
		pe_section_align - 1) & ~(pe_section_align - 1)
	g.write32_at(g.pe_opt_hdr_pos + pe32_plus_optional_header_offsetof(.size_of_image),
		image_size)
}

pub fn (mut g Gen) generate_pe_footer() {
	g.sym_string_table()

	g.align_to(pe_file_align)
	g.file_size_pos = g.pos()

	if g.pe_opt_hdr_pos == 0 {
		g.n_error('no PE optional header generated')
	}

	g.patch_pe_code_size()
	g.patch_pe_image_size()

	// patch call main
	match g.pref.arch {
		.arm64 {
			bl_next := u32(0x94000001)
			g.write32_at(g.code_start_pos, i32(bl_next))
		}
		.amd64 {
			// +1 is for "e8"
			// -5 is for "e8 00 00 00 00"
			g.write32_at(g.code_start_pos + 1, i32(g.main_fn_addr - g.code_start_pos) - 5)
		}
		else {
			g.n_error('unsupported platform ${g.pref.arch}')
		}
	}

	g.println('')
	g.println('=== end ===')
	g.create_executable()
}
