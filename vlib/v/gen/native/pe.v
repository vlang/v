// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

enum PeCharacteristics {
	// 1: relocation info stripped
	// 2: file is executable
	// 4: line numbers stripped
	// 8: local symbols stripped
	// 20: app can handle > 2GB
	executable_image = 0x2f
}

const (
	image_base        = i64(0x400000)
	pe_sections_start = 0x0180
	pe_code_start     = 0x400

	pe_section_align  = 0x1000
	pe_file_align     = 0x0200

	pe_opt_hdr_size   = 0xf0
	pe_header_size	  = pe_file_align
	pe_stack_size	  = 0x1000
	pe_heap_size 	  = 0x10000

	dos_stub_end      = 0x80
	optdr_location    = 0x98
)

enum PeMachine as u16 {
	i386 = 0x014c
	amd64 = 0x8664
	arm64 = 0xaa64
}

enum PeMagic as u16 {
	mz = 0x5a4d // dos(mz)-header magic number
	pe = 0x4550 // pe-header magic number
	pe32 = 0x010b // PE32 optional header magic number
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
	high_entropy_va = 0x0020
	dynamic_base = 0x0040
	force_integrity = 0x0080
	nx_compat = 0x0100
	no_isolation = 0x0200
	no_seh = 0x0400
	no_bind = 0x0800
	appcontainer = 0x1000
	wdm_driver = 0x2000
	guard_cf = 0x4000
	terminal_server_aware = 0x8000
}

// reference: https://learn.microsoft.com/en-us/windows/win32/debug/pe-format?redirectedfrom=MSDN#section-flags
enum PeSectionFlags as u32 {
	type_no_pad = 0x00000008
	cnt_code = 0x00000020
	cnt_initialized_data = 0x00000040
	cnt_uninitialized_data = 0x00000080
	lnk_info = 0x00000200
	lnk_remove = 0x00000400
	lnk_comdat = 0x00001000
	gprel = 0x00008000
	lnk_nreloc_ovfl = 0x01000000
	mem_discardable = 0x02000000
	mem_not_cached = 0x04000000
	mem_not_paged = 0x08000000
	mem_shared = 0x10000000
	mem_execute = 0x20000000
	mem_read = 0x40000000
	mem_write = 0x80000000
	// obj files only:
	align_1bytes = 0x00100000
	align_2bytes = 0x00200000
	align_4bytes = 0x00300000
	align_8bytes = 0x00400000
	align_16bytes = 0x00500000
	align_32bytes = 0x00600000
	align_64bytes = 0x00700000
	align_128bytes = 0x00800000
	align_256bytes = 0x00900000
	align_512bytes = 0x00a00000
	align_1024bytes = 0x00b00000
	align_2048bytes = 0x00c00000
	align_4096bytes = 0x00d00000
	align_8192bytes = 0x00e00000
}

struct PeDataDirectory {
	virtual_addr u32
	size         u32
}

pub fn (mut g Gen) write_pe_header() {
	pe_header := [
		int(PeMagic.pe),
		0,
		int(PeMachine.amd64), // machine
		2, // number of sections
		0,
		0, // timestamp
		0,
		0, // symbol table
		0, // number of symbols
		0,
		native.pe_opt_hdr_size, // 40 // size of optional header
		int(PeCharacteristics.executable_image),
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
		assert 0x18 == pe_header.len * 2
	}

	for i, b in pe_header {
		end_addr := i * 2 + 2 // allign correctly with description table
		g.write16(b)
		if g.pref.is_verbose && pe_header_description[end_addr] != '' {
			g.println(pe_header_description[end_addr])
		}
	}

	g.println('^^^ PE Header')

	// optional here comes here
	p_opthdr := g.pos() // should be 0x98
	if p_opthdr != native.optdr_location {
		g.n_error('Invalid optdr location ${p_opthdr} != 0x98')
	}

	opt_hdr := g.get_pe_optional_header()
	g.write_pe_optional_header(opt_hdr)

	g.write32(0)
	g.write32(0)
	g.write32(0x1000)
	g.write32(0x2000) // size of code
	g.println('')
}

[packed]
struct Pe32OptionalHeader {
	// TODO: potential support for 32-bit
}

[packed]
struct Pe32PlusOptionalHeader {
	// standard fields
	// refenrence: https://learn.microsoft.com/en-us/windows/win32/debug/pe-format?redirectedfrom=MSDN#optional-header-standard-fields-image-only
	magic                      PeMagic
	major_linker_version       u8
	minor_linker_version       u8
	size_of_code               int
	size_of_initialized_data   int
	size_of_uninitialized_data int
	address_of_entry_point     int
	base_of_code               int
	// PE32 ONLY:
	// base_of_data u32
	// NT fields
	// reference: https://learn.microsoft.com/en-us/windows/win32/debug/pe-format?redirectedfrom=MSDN#optional-header-windows-specific-fields-image-only
	image_base              i64 // u32 for PE32
	section_alignment       int
	file_alignment          int
	major_os_version        i16
	minor_os_version        i16
	major_image_version     i16
	minor_image_version     i16
	major_subsystem_version i16
	minor_subsystem_version i16
	win32_version_value     int
	size_of_image           int
	size_of_headers         int
	checksum                int
	subsystem               PeSubsystem
	dll_characteristics     i16
	size_of_stack_reserve   i64 // u32 for PE32
	size_of_stack_commit    i64 // u32 for PE32
	size_of_heap_reserve    i64 // u32 for PE32
	size_of_heap_commit     i64 // u32 for PE32
	loader_flags            int // reserved, MUST be zero
	number_of_rva_and_sizes int
}

fn (mut g Gen) get_pe32_plus_optional_header() Pe32PlusOptionalHeader {
	return Pe32PlusOptionalHeader{
		magic: .pe32plus
		major_linker_version: 0x01
		minor_linker_version: 0x49
		size_of_code: 0 // filled in when the footer is generated
		size_of_initialized_data: 0
		size_of_uninitialized_data: 0x00
		address_of_entry_point: 0x2000
		base_of_code: 0x2000
		image_base: native.image_base
		section_alignment: native.pe_section_align
		file_alignment: native.pe_file_align
		major_os_version: 1
		minor_os_version: 0
		major_image_version: 0
		minor_image_version: 0
		major_subsystem_version: 5
		minor_subsystem_version: 0
		win32_version_value: 0
		size_of_image: 0x3000 // filled in later // header size + code size
		size_of_headers: native.pe_header_size
		checksum: 0
		subsystem: .windows_cui
		dll_characteristics: 0
		size_of_stack_reserve: native.pe_stack_size
		size_of_stack_commit: native.pe_stack_size
		size_of_heap_reserve: native.pe_heap_size
		size_of_heap_commit: 0
		loader_flags: 0
		number_of_rva_and_sizes: 0x10
	}
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

fn (mut g Gen) write_pe_optional_header(opt_hdr PeOptionalHeader) {
	if opt_hdr !is Pe32PlusOptionalHeader {
		g.n_error('unsupported optional header architecture ${optional_header_arch(opt_hdr)}')
	}

	h := opt_hdr as Pe32PlusOptionalHeader
	g.pe_opt_hdr_pos = g.pos()

	// standard fields
	g.write16(i16(h.magic))
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
	g.write16(h.major_os_version)
	g.println('; mMajorOperatingSystemVersion')
	g.write16(h.minor_os_version)
	g.println('; mMinorOperatingSystemVersion')
	g.write16(h.major_image_version)
	g.println('; mMajorImageVersion')
	g.write16(h.minor_image_version)
	g.println('; mMinorImageVersion')
	g.write16(h.major_subsystem_version)
	g.println('; mMajorSubsystemVersion')
	g.write16(h.minor_subsystem_version)
	g.println('; mMinorSubsystemVersion')
	g.write32(h.win32_version_value)
	g.println('; mWin32VersionValue')
	g.write32(h.size_of_image)
	g.println('; mSizeOfImage')
	g.write32(h.size_of_headers)
	g.println('; mSizeOfHeaders')
	g.write32(h.checksum)
	g.println('; mCeckSum')
	g.write16(int(h.subsystem))
	g.println('; mSubsystem')
	g.write16(h.dll_characteristics)
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

// reference: https://learn.microsoft.com/en-us/windows/win32/debug/pe-format?redirectedfrom=MSDN#section-table-section-headers
struct PeSectionHeader {
	name                   string
	virtual_size           int
	virtual_address        int
	size_of_raw_data       int
	pointer_to_raw_data    int
	pointer_to_relocations int
	pointer_to_linenumbers int
	number_of_relocations  i16
	number_of_linenumbers  i16
	characteristics        PeSectionFlags
}

fn (mut g Gen) gen_pe_section_header(sh PeSectionHeader) {
	assert sh.name.len <= 8
	g.write(sh.name.bytes())
	for _ in sh.name.len .. 8 { // pad to 8 bytes
		g.write8(0)
	}
	g.println('"${sh.name}"')

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
	g.write32(sh.number_of_relocations)
	g.println('; NumberOfRelocations')
	g.write32(sh.number_of_linenumbers)
	g.println('; NumberOfLinenumbers')
	g.write32(int(sh.characteristics))
}

fn (mut g Gen) write_pe_sections() {
	g.pad_to(native.pe_sections_start)
	g.println('')
	g.println('^^^ padding to addr 0x${native.pe_sections_start.hex()}')

	g.write64(0)
	g.write_string_with_padding('.idata', 8)
	g.println('; ".idata"')
	//
	g.write32(0x89) // 137
	g.write16(0x1000)
	g.write32(0x02000000)
	g.write32(0x02000000)
	g.zeroes(14)
	g.write16(64)
	g.write8(0)
	g.write8(192)
	g.println('')

	g.write_string_with_padding('.text', 8)
	g.println('".text"')

	g.write32(75)
	g.write8(0)
	g.write32(0x20)
	g.write32(0x00002)
	g.write32(4)
	g.write32(0)
	g.write32(0)
	g.write32(0x20000000) // 0, 0, 0, 32,
	g.write([u8(0), 0, 96])
	g.zeroes(52)
	g.write([u8(72), 16, 0, 0])
	g.write([u8(40), 16, 0, 0])
	g.zeroes(20)
	g.write([u8(96), 16, 0, 0])
	g.write32(0)
	g.write([u8(110), 16, 0, 0])
	g.write32(0)
	g.write([u8(125), 16, 0, 0])
	g.println('')
	g.zeroes(12)
	g.println('')
	g.write_string_with_padding('KERNEL32.DLL', 13)
	g.println('\t"KERNEL32.DLL"')
	g.write_string_with_padding('USER32.DLL', 13)
	g.println('\t"USER32.DLL"')
	g.write_string_with_padding('ExitProcess', 14)
	g.println('\t"ExitProcess" (function)')
	g.write_string_with_padding('GetStdHandle', 15)
	g.println('\t"GetStdHandle" (function)')
	g.write_string_with_padding('WriteFile', 13)
	g.println('\t"WriteFile" (function)')

	g.println('^^^ PE Sections')
}

pub fn (mut g Gen) generate_pe_header() {
	g.write_dos_header()
	g.write_dos_stub()
	g.write_pe_header()
	g.write_pe_sections()

	g.pad_to(native.pe_code_start)
	g.println('')
	g.println('^^^ padding to addr 0x${native.pe_code_start.hex()}')

	g.code_start_pos = g.pos()

	g.code_gen.call(0)
	g.code_gen.ret()
	g.main_fn_addr = g.pos()
}

pub fn (mut g Gen) generate_pe_footer() {
	g.sym_string_table()

	g.align_to(native.pe_file_align)
	g.file_size_pos = g.pos()

	if g.pe_opt_hdr_pos == 0 {
		g.n_error('no PE optional header generated')
	}

	code_size := int(g.file_size_pos - g.code_start_pos)

	size_of_code_pos := g.pe_opt_hdr_pos + __offsetof(Pe32PlusOptionalHeader, size_of_code)
	g.write32_at(size_of_code_pos, code_size)

	size_of_initialized_data_pos := g.pe_opt_hdr_pos +
		__offsetof(Pe32PlusOptionalHeader, size_of_initialized_data)
	g.write32_at(size_of_initialized_data_pos, code_size)

	//size_of_image := (int(g.file_size_pos) + native.pe_section_align - 1) & ~(native.pe_section_align - 1)
	//size_of_image_pos := g.pe_opt_hdr_pos +
	//	__offsetof(Pe32PlusOptionalHeader, size_of_image)
	//g.write32_at(size_of_image_pos, size_of_image)
//
	//g.pad_to(size_of_image)
	//g.println('fs: ${size_of_image.hex()}')

	// patch call main
	match g.pref.arch {
		.arm64 {
			bl_next := u32(0x94000001)
			g.write32_at(g.code_start_pos, int(bl_next))
		}
		.amd64 {
			// +1 is for "e8"
			// -5 is for "e8 00 00 00 00"
			g.write32_at(g.code_start_pos + 1, int(g.main_fn_addr - g.code_start_pos) - 5)
		}
		else {
			g.n_error('unsupported platform ${g.pref.arch}')
		}
	}

	g.println('')
	g.println('=== end ===')
	g.create_executable()
}
