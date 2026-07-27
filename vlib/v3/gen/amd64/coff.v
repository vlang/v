// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module amd64

const coff64_file_header_size = u64(20)
const coff64_section_header_size = u64(40)
const coff64_relocation_size = u64(10)
const coff64_symbol_size = u64(18)
const coff64_runtime_function_size = u64(12)
const coff64_unwind_info_size = u64(8)
const coff64_max_section_relocations = u64(0xffff)

const coff64_image_file_machine_amd64 = u16(0x8664)
const coff64_image_rel_amd64_addr64 = u16(0x0001)
const coff64_image_rel_amd64_addr32 = u16(0x0002)
const coff64_image_rel_amd64_addr32nb = u16(0x0003)
const coff64_image_rel_amd64_rel32 = u16(0x0004)
const coff64_image_rel_amd64_rel32_1 = u16(0x0005)
const coff64_image_rel_amd64_rel32_2 = u16(0x0006)
const coff64_image_rel_amd64_rel32_3 = u16(0x0007)
const coff64_image_rel_amd64_rel32_4 = u16(0x0008)
const coff64_image_rel_amd64_rel32_5 = u16(0x0009)

const coff64_image_sym_class_external = u8(2)
const coff64_image_sym_class_static = u8(3)
const coff64_image_sym_class_label = u8(6)
const coff64_image_sym_type_function = u16(0x20)

const coff64_text_characteristics = u32(0x60500020)
const coff64_data_characteristics = u32(0x40300040)
const coff64_private_data_characteristics = u32(0xc0400040)
const coff64_rdata_base_characteristics = u32(0x40000040)
const coff64_writable_data_base_characteristics = u32(0xc0000040)
const coff64_bss_base_characteristics = u32(0xc0000080)

struct Coff64IndexedFunction {
	public_index u32
	offset       u64
	size         u64
}

struct Coff64Nonleaf {
	public_index         u32
	end                  u32
	unwind               u32
	end_name             string
	unwind_name          string
	windows_unwind_bytes []u8
}

struct Coff64Relocation {
	offset       u32
	symbol_index u32
	typ          u16
}

struct Coff64Counts {
	text_relocation_count u64
	nonleaf_count         u64
}

struct Coff64StandardSymbol {
	index          u32
	name           string
	value          u32
	section_number i16
	typ            u16
	storage_class  u8
	aux_count      u8
}

struct Coff64Placement {
	offset u64
	end    u64
}

struct Coff64Layout {
	section_count     u16
	header_end        u64
	text_size         u64
	pdata_size        u64
	xdata_size        u64
	text_raw          u64
	pdata_raw         u64
	xdata_raw         u64
	text_reloc        u64
	pdata_reloc       u64
	symbol_table      u64
	strings           u64
	string_size       u64
	final_extent      u64
	text_reloc_count  u64
	pdata_reloc_count u64
}

struct Coff64PrivateDataLayout {
	section_count     u16
	header_end        u64
	text_size         u64
	pdata_size        u64
	xdata_size        u64
	data_size         u64
	text_raw          u64
	pdata_raw         u64
	xdata_raw         u64
	data_raw          u64
	text_reloc        u64
	pdata_reloc       u64
	symbol_table      u64
	strings           u64
	string_size       u64
	final_extent      u64
	text_reloc_count  u64
	pdata_reloc_count u64
}

struct Coff64Preflight {
	layout              Coff64Layout
	output_capacity     int
	text_relocations    []Coff64Relocation
	pdata_relocations   []Coff64Relocation
	nonleafs            []Coff64Nonleaf
	standard_symbols    []Coff64StandardSymbol
	symbol_name_offsets []u32
	symbol_count        u32
	xdata_symbol_index  u32
}

struct Coff64PrivateDataPreflight {
	layout              Coff64PrivateDataLayout
	output_capacity     int
	text_relocations    []Coff64Relocation
	pdata_relocations   []Coff64Relocation
	nonleafs            []Coff64Nonleaf
	standard_symbols    []Coff64StandardSymbol
	symbol_name_offsets []u32
	symbol_count        u32
	xdata_symbol_index  u32
}

struct Coff64ObjectDataLegacyPreflight {
	text_relocations   []Coff64Relocation
	pdata_relocations  []Coff64Relocation
	nonleafs           []Coff64Nonleaf
	standard_symbols   []Coff64StandardSymbol
	symbol_count       u32
	xdata_symbol_index u32
}

struct Coff64ObjectDataRelocationEncoding {
	typ   u16
	width u64
}

struct Coff64ObjectDataPhysicalSection {
	name            string
	semantic_size   u64
	characteristics u32
mut:
	bytes              []u8
	relocations        []Coff64Relocation
	raw_pointer        u64
	relocation_pointer u64
}

struct Coff64ObjectDataPreflight {
	sections            []Coff64ObjectDataPhysicalSection
	output_capacity     int
	standard_symbols    []Coff64StandardSymbol
	symbol_name_offsets []u32
	symbol_count        u32
	symbol_table        u64
	strings             u64
	string_size         u64
	final_extent        u64
	xdata_symbol_index  u32
	xdata_size          u64
}

fn coff64_checked_add(left u64, right u64, label string) !u64 {
	if left > max_u64 - right {
		return error('COFF64 ${label} overflows u64')
	}
	return left + right
}

fn coff64_checked_mul(left u64, right u64, label string) !u64 {
	if left != 0 && right > max_u64 / left {
		return error('COFF64 ${label} overflows u64')
	}
	return left * right
}

fn coff64_align4(value u64, label string) !u64 {
	remainder := value % 4
	if remainder == 0 {
		return value
	}
	return coff64_checked_add(value, 4 - remainder, label)
}

fn coff64_align_to(value u64, alignment u64, label string) !u64 {
	if alignment == 0 || alignment & (alignment - 1) != 0 {
		return error('COFF64 ${label} alignment ${alignment} is invalid')
	}
	remainder := value % alignment
	if remainder == 0 {
		return value
	}
	return coff64_checked_add(value, alignment - remainder, label)
}

fn coff64_alignment_characteristic(alignment u64) !u32 {
	return match alignment {
		1 {
			u32(0x00100000)
		}
		2 {
			u32(0x00200000)
		}
		4 {
			u32(0x00300000)
		}
		8 {
			u32(0x00400000)
		}
		16 {
			u32(0x00500000)
		}
		32 {
			u32(0x00600000)
		}
		64 {
			u32(0x00700000)
		}
		128 {
			u32(0x00800000)
		}
		256 {
			u32(0x00900000)
		}
		512 {
			u32(0x00a00000)
		}
		1024 {
			u32(0x00b00000)
		}
		2048 {
			u32(0x00c00000)
		}
		4096 {
			u32(0x00d00000)
		}
		8192 {
			u32(0x00e00000)
		}
		else {
			error('COFF64 object section alignment ${alignment} is unsupported')
		}
	}
}

fn coff64_validate_physical_relocation_count(name string, count u64) ! {
	if count > coff64_max_section_relocations {
		return error('COFF64 ${name} has ${count} relocations; extended relocations are unsupported')
	}
}

fn coff64_validate_combined_text_relocation_count(call_count u64, object_count u64) !u64 {
	total := coff64_checked_add(call_count, object_count, '.text relocation count')!
	coff64_validate_physical_relocation_count('.text', total)!
	return total
}

fn coff64_require_u32(value u64, label string) !u32 {
	if value > u64(max_u32) {
		return error('COFF64 ${label} exceeds u32')
	}
	return u32(value)
}

fn coff64_checked_host_size(value u64) !int {
	if value > u64(max_int) {
		return error('COFF64 output exceeds the host array limit')
	}
	return int(value)
}

fn coff64_validate_counts(text_relocation_count u64, nonleaf_count u64) ! {
	if text_relocation_count > coff64_max_section_relocations {
		return error('COFF64 .text has ${text_relocation_count} relocations; extended relocations are unsupported')
	}
	pdata_relocation_count := coff64_checked_mul(nonleaf_count, 3, '.pdata relocation count')!
	if pdata_relocation_count > coff64_max_section_relocations {
		return error('COFF64 .pdata has ${pdata_relocation_count} relocations; extended relocations are unsupported')
	}
}

fn coff64_symbol_count(public_count u64, nonleaf_count u64) !u64 {
	if public_count > u64(max_u32) {
		return error('COFF64 public symbol count exceeds u32')
	}
	if nonleaf_count == 0 {
		return public_count
	}
	private_count := coff64_checked_add(coff64_checked_mul(nonleaf_count, 2, 'private symbol count')!,
		1, 'private symbol count')!
	total := coff64_checked_add(public_count, private_count, 'symbol count')!
	if total > u64(max_u32) {
		return error('COFF64 symbol count exceeds u32')
	}
	return total
}

fn coff64_private_data_symbol_count(base_count u64, data_count u64) !u64 {
	symbol_count := coff64_checked_add(base_count, data_count, 'symbol count')!
	if symbol_count > u64(max_u32) {
		return error('COFF64 symbol count exceeds u32')
	}
	return symbol_count
}

fn coff64_checked_string_size(current u64, name_size u64) !u64 {
	growth := coff64_checked_add(name_size, 1, 'string entry size')!
	next := coff64_checked_add(current, growth, 'string table size')!
	if next > u64(max_u32) {
		return error('COFF64 string table exceeds u32')
	}
	return next
}

fn coff64_validate_relocation_symbol_index(index u64, symbol_count u64, forbidden_aux_index u64) !u32 {
	if index >= symbol_count {
		if symbol_count == 0 {
			return error('COFF64 relocation symbol index ${index} has no symbol table')
		}
		return error('COFF64 relocation symbol index ${index} is outside 0..${symbol_count - 1}')
	}
	if index == forbidden_aux_index {
		return error('COFF64 relocation targets the .xdata auxiliary record')
	}
	return coff64_require_u32(index, 'relocation symbol index')
}

fn coff64_place(cursor u64, size u64, label string) !Coff64Placement {
	if size == 0 {
		return Coff64Placement{
			end: cursor
		}
	}
	offset := coff64_align4(cursor, '${label} offset')!
	end := coff64_checked_add(offset, size, '${label} extent')!
	return Coff64Placement{
		offset: offset
		end:    end
	}
}

fn coff64_build_layout(text_size u64, text_relocation_count u64, nonleaf_count u64, symbol_count u64, string_size u64) !Coff64Layout {
	coff64_validate_counts(text_relocation_count, nonleaf_count)!
	_ = coff64_require_u32(text_size, '.text size')!
	_ = coff64_require_u32(symbol_count, 'symbol count')!
	if symbol_count == 0 {
		if string_size != 0 {
			return error('COFF64 empty symbol table must omit the string table')
		}
	} else if string_size < 4 {
		return error('COFF64 nonempty symbol table requires a string table header')
	}
	_ = coff64_require_u32(string_size, 'string table size')!

	section_count := if nonleaf_count == 0 { u16(1) } else { u16(3) }
	section_headers_size := coff64_checked_mul(u64(section_count), coff64_section_header_size,
		'section header table size')!
	header_end :=
		coff64_checked_add(coff64_file_header_size, section_headers_size, 'header extent')!
	pdata_size := coff64_checked_mul(nonleaf_count, coff64_runtime_function_size, '.pdata size')!
	xdata_size := coff64_checked_mul(nonleaf_count, coff64_unwind_info_size, '.xdata size')!
	text_reloc_size := coff64_checked_mul(text_relocation_count, coff64_relocation_size,
		'.text relocation table size')!
	pdata_reloc_count := coff64_checked_mul(nonleaf_count, 3, '.pdata relocation count')!
	pdata_reloc_size := coff64_checked_mul(pdata_reloc_count, coff64_relocation_size,
		'.pdata relocation table size')!
	_ = coff64_require_u32(pdata_size, '.pdata size')!
	_ = coff64_require_u32(xdata_size, '.xdata size')!

	text := coff64_place(header_end, text_size, '.text')!
	pdata := coff64_place(text.end, pdata_size, '.pdata')!
	xdata := coff64_place(pdata.end, xdata_size, '.xdata')!
	text_relocations := coff64_place(xdata.end, text_reloc_size, '.text relocations')!
	pdata_relocations := coff64_place(text_relocations.end, pdata_reloc_size, '.pdata relocations')!

	mut symbol_table := u64(0)
	mut strings := u64(0)
	mut final_extent := pdata_relocations.end
	if symbol_count != 0 {
		symbol_table = coff64_align4(final_extent, 'symbol table offset')!
		symbol_bytes := coff64_checked_mul(symbol_count, coff64_symbol_size, 'symbol table size')!
		strings = coff64_checked_add(symbol_table, symbol_bytes, 'string table offset')!
		final_extent = coff64_checked_add(strings, string_size, 'file extent')!
	}
	_ = coff64_require_u32(text.offset, '.text raw pointer')!
	_ = coff64_require_u32(pdata.offset, '.pdata raw pointer')!
	_ = coff64_require_u32(xdata.offset, '.xdata raw pointer')!
	_ = coff64_require_u32(text_relocations.offset, '.text relocation pointer')!
	_ = coff64_require_u32(pdata_relocations.offset, '.pdata relocation pointer')!
	_ = coff64_require_u32(symbol_table, 'symbol table pointer')!
	_ = coff64_require_u32(strings, 'string table pointer')!
	_ = coff64_require_u32(final_extent, 'file extent')!
	_ = coff64_checked_host_size(final_extent)!

	return Coff64Layout{
		section_count:     section_count
		header_end:        header_end
		text_size:         text_size
		pdata_size:        pdata_size
		xdata_size:        xdata_size
		text_raw:          text.offset
		pdata_raw:         pdata.offset
		xdata_raw:         xdata.offset
		text_reloc:        text_relocations.offset
		pdata_reloc:       pdata_relocations.offset
		symbol_table:      symbol_table
		strings:           strings
		string_size:       string_size
		final_extent:      final_extent
		text_reloc_count:  text_relocation_count
		pdata_reloc_count: pdata_reloc_count
	}
}

fn coff64_build_private_data_layout(text_size u64, data_size u64, text_relocation_count u64, nonleaf_count u64, symbol_count u64, string_size u64) !Coff64PrivateDataLayout {
	coff64_validate_counts(text_relocation_count, nonleaf_count)!
	_ = coff64_require_u32(text_size, '.text size')!
	_ = coff64_require_u32(data_size, '.data size')!
	_ = coff64_require_u32(symbol_count, 'symbol count')!
	if string_size < 4 {
		return error('COFF64 private data requires a string table header')
	}
	_ = coff64_require_u32(string_size, 'string table size')!
	section_count := if nonleaf_count == 0 { u16(2) } else { u16(4) }
	section_headers_size := coff64_checked_mul(u64(section_count), coff64_section_header_size,
		'section header table size')!
	header_end :=
		coff64_checked_add(coff64_file_header_size, section_headers_size, 'header extent')!
	pdata_size := coff64_checked_mul(nonleaf_count, coff64_runtime_function_size, '.pdata size')!
	xdata_size := coff64_checked_mul(nonleaf_count, coff64_unwind_info_size, '.xdata size')!
	text_reloc_size := coff64_checked_mul(text_relocation_count, coff64_relocation_size,
		'.text relocation table size')!
	pdata_reloc_count := coff64_checked_mul(nonleaf_count, 3, '.pdata relocation count')!
	pdata_reloc_size := coff64_checked_mul(pdata_reloc_count, coff64_relocation_size,
		'.pdata relocation table size')!
	text := coff64_place(header_end, text_size, '.text')!
	pdata := coff64_place(text.end, pdata_size, '.pdata')!
	xdata := coff64_place(pdata.end, xdata_size, '.xdata')!
	data := coff64_place(xdata.end, data_size, '.data')!
	text_relocations := coff64_place(data.end, text_reloc_size, '.text relocations')!
	pdata_relocations := coff64_place(text_relocations.end, pdata_reloc_size, '.pdata relocations')!
	symbol_table := coff64_align4(pdata_relocations.end, 'symbol table offset')!
	symbol_bytes := coff64_checked_mul(symbol_count, coff64_symbol_size, 'symbol table size')!
	strings := coff64_checked_add(symbol_table, symbol_bytes, 'string table offset')!
	final_extent := coff64_checked_add(strings, string_size, 'file extent')!
	_ = coff64_require_u32(text.offset, '.text raw pointer')!
	_ = coff64_require_u32(pdata.offset, '.pdata raw pointer')!
	_ = coff64_require_u32(xdata.offset, '.xdata raw pointer')!
	_ = coff64_require_u32(data.offset, '.data raw pointer')!
	_ = coff64_require_u32(text_relocations.offset, '.text relocation pointer')!
	_ = coff64_require_u32(pdata_relocations.offset, '.pdata relocation pointer')!
	_ = coff64_require_u32(symbol_table, 'symbol table pointer')!
	_ = coff64_require_u32(strings, 'string table pointer')!
	_ = coff64_require_u32(final_extent, 'file extent')!
	_ = coff64_checked_host_size(final_extent)!
	return Coff64PrivateDataLayout{
		section_count:     section_count
		header_end:        header_end
		text_size:         text_size
		pdata_size:        pdata_size
		xdata_size:        xdata_size
		data_size:         data_size
		text_raw:          text.offset
		pdata_raw:         pdata.offset
		xdata_raw:         xdata.offset
		data_raw:          data.offset
		text_reloc:        text_relocations.offset
		pdata_reloc:       pdata_relocations.offset
		symbol_table:      symbol_table
		strings:           strings
		string_size:       string_size
		final_extent:      final_extent
		text_reloc_count:  text_relocation_count
		pdata_reloc_count: pdata_reloc_count
	}
}

fn coff64_private_name(mut used map[string]bool, kind string, id u64) !string {
	mut salt := u64(0)
	for {
		candidate := '.v3\$coff\$${kind}\$${id}\$${salt}'
		if !used[candidate] {
			used[candidate] = true
			return candidate
		}
		if salt == max_u64 {
			break
		}
		salt++
	}
	return error('COFF64 private symbol salt exhausted')
}

fn coff64_scan_counts(o &Object) !Coff64Counts {
	text_relocation_count := u64(o.call_relocations.len)
	mut owned_relocation_count := u64(0)
	mut nonleaf_count := u64(0)
	for symbol in o.symbols {
		if symbol.intentional_external {
			continue
		}
		function_end := coff64_checked_add(symbol.offset, symbol.size, 'function end')!
		mut owns_call := false
		for relocation in o.call_relocations {
			call_start := relocation.offset - 1
			field_end := coff64_checked_add(relocation.offset, 4, 'CALL relocation field end')!
			if symbol.offset <= call_start && field_end <= function_end {
				owns_call = true
				owned_relocation_count = coff64_checked_add(owned_relocation_count, 1,
					'owned relocation count')!
			}
		}
		if owns_call {
			nonleaf_count = coff64_checked_add(nonleaf_count, 1, 'nonleaf count')!
		}
	}
	if owned_relocation_count != text_relocation_count {
		return error('COFF64 ownership count did not consume every CALL relocation')
	}
	return Coff64Counts{
		text_relocation_count: text_relocation_count
		nonleaf_count:         nonleaf_count
	}
}

fn coff64_validate_explicit_m7_frame(o &Object, function Coff64IndexedFunction, frame &ObjectFunctionFrame, relocation TextCallRelocation) ! {
	expected_prologue := [u8(0x48), 0x83, 0xec, 0x28]
	expected_epilogue := [u8(0x48), 0x83, 0xc4, 0x28]
	expected_unwind := [u8(0x01), 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00]
	expected_text := [u8(0x48), 0x83, 0xec, 0x28, 0xe8, 0x00, 0x00, 0x00, 0x00, 0x31, 0xc0, 0x48,
		0x83, 0xc4, 0x28, 0xc3]
	if frame.prologue_bytes != expected_prologue || frame.epilogue_bytes != expected_epilogue
		|| frame.windows_unwind_bytes != expected_unwind {
		return error('COFF64 explicit function frame is outside the M7 Windows CALL32 contract')
	}
	if function.size != u64(expected_text.len)
		|| relocation.offset != function.offset + u64(expected_prologue.len) + 1 {
		return error('COFF64 explicit function frame has noncanonical M7 text geometry')
	}
	start := int(function.offset)
	end := start + expected_text.len
	if o.text[start..end] != expected_text {
		return error('COFF64 explicit function frame has noncanonical M7 text bytes')
	}
}

fn coff64_prepare_nonleafs(o &Object, sorted_relocations []TextCallRelocation) ![]Coff64Nonleaf {
	mut functions := []Coff64IndexedFunction{cap: o.symbols.len}
	for public_index, symbol in o.symbols {
		if symbol.intentional_external {
			continue
		}
		functions << Coff64IndexedFunction{
			public_index: coff64_require_u32(u64(public_index), 'public symbol index')!
			offset:       symbol.offset
			size:         symbol.size
		}
	}
	functions.sort(a.offset < b.offset)

	explicit_frames := o.function_frames.len != 0
	mut frame_indices := map[int]int{}
	for frame_index, frame in o.function_frames {
		frame_indices[int(frame.function_symbol)] = frame_index + 1
	}
	mut used_frames := []bool{len: o.function_frames.len}
	mut bases := []Coff64Nonleaf{cap: functions.len}
	mut relocation_index := 0
	for function in functions {
		function_end := coff64_checked_add(function.offset, function.size, 'function end')!
		first_owned_relocation := relocation_index
		for relocation_index < sorted_relocations.len {
			relocation := sorted_relocations[relocation_index]
			call_start := relocation.offset - 1
			field_end := coff64_checked_add(relocation.offset, 4, 'CALL relocation field end')!
			if call_start >= function_end {
				break
			}
			if call_start < function.offset || field_end > function_end {
				return error('COFF64 CALL relocation is not owned by the ordered function')
			}
			relocation_index++
		}
		owned_call_count := relocation_index - first_owned_relocation
		frame_index := frame_indices[int(function.public_index)] - 1
		if explicit_frames && owned_call_count == 0 && frame_index >= 0 {
			return error('COFF64 explicit function frame is attached to a leaf function')
		}
		if explicit_frames && owned_call_count != 0 && frame_index < 0 {
			return error('COFF64 explicit frame mode is missing a caller frame')
		}
		mut unwind_bytes := [u8(0x01), 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00]
		if explicit_frames && owned_call_count != 0 {
			if owned_call_count != 1 {
				return error('COFF64 explicit M7 caller must own exactly one CALL relocation')
			}
			frame := o.function_frames[frame_index]
			coff64_validate_explicit_m7_frame(o, function, &frame,
				sorted_relocations[first_owned_relocation])!
			used_frames[frame_index] = true
			unwind_bytes = frame.windows_unwind_bytes.clone()
		}
		if owned_call_count != 0 {
			bases << Coff64Nonleaf{
				public_index:         function.public_index
				end:                  coff64_require_u32(function_end, 'function end')!
				windows_unwind_bytes: unwind_bytes
			}
		}
	}
	if relocation_index != sorted_relocations.len {
		return error('COFF64 CALL relocation ownership scan did not consume every relocation')
	}
	if explicit_frames {
		for used in used_frames {
			if !used {
				return error('COFF64 explicit frame mode did not consume every function frame')
			}
		}
	}

	mut used := map[string]bool{}
	for symbol in o.symbols {
		if !symbol.intentional_external {
			used[symbol.name] = true
		}
	}
	for symbol in o.private_data_symbols {
		used[symbol.name] = true
	}
	for symbol in o.object_data.symbols {
		if symbol.kind == .named {
			used[symbol.name] = true
		}
	}
	used['.text'] = true
	used['.pdata'] = true
	used['.xdata'] = true
	nonleafs := coff64_materialize_nonleafs(mut used, bases)!
	mut external_names := []string{}
	for symbol in o.symbols {
		if symbol.intentional_external {
			external_names << symbol.name
		}
	}
	coff64_validate_external_generated_names(external_names, nonleafs)!
	return nonleafs
}

fn coff64_materialize_nonleafs(mut used map[string]bool, bases []Coff64Nonleaf) ![]Coff64Nonleaf {
	mut nonleafs := []Coff64Nonleaf{cap: bases.len}
	for nonleaf_index, base in bases {
		end_name := coff64_private_name(mut used, 'end', u64(base.public_index))!
		unwind_offset := coff64_checked_mul(u64(nonleaf_index), coff64_unwind_info_size,
			'unwind offset')!
		unwind_name := if nonleaf_index == 0 {
			''
		} else {
			coff64_private_name(mut used, 'uw', u64(nonleaf_index))!
		}
		nonleafs << Coff64Nonleaf{
			public_index:         base.public_index
			end:                  base.end
			unwind:               coff64_require_u32(unwind_offset, 'unwind offset')!
			end_name:             end_name
			unwind_name:          unwind_name
			windows_unwind_bytes: base.windows_unwind_bytes.clone()
		}
	}
	return nonleafs
}

fn coff64_validate_external_generated_names(external_names []string, nonleafs []Coff64Nonleaf) ! {
	mut generated := map[string]bool{}
	if nonleafs.len != 0 {
		generated['.xdata'] = true
	}
	for nonleaf in nonleafs {
		generated[nonleaf.end_name] = true
		if nonleaf.unwind_name.len != 0 {
			generated[nonleaf.unwind_name] = true
		}
	}
	for name in external_names {
		if generated[name] {
			return error('COFF64 external symbol `${name}` collides with a generated symbol')
		}
	}
}

fn coff64_validate_plan_external_names(plan &LoweringPlan) ! {
	mut used := map[string]bool{}
	for function in plan.functions {
		used[function.name] = true
	}
	for symbol in plan.private_data.symbols {
		used[symbol.name] = true
	}
	used['.text'] = true
	used['.pdata'] = true
	used['.xdata'] = true
	mut bases := []Coff64Nonleaf{}
	for function_index, function in plan.functions {
		mut has_calls := function.calls.len != 0
		for block in function.blocks {
			has_calls = has_calls || block.calls.len != 0
		}
		if has_calls {
			bases << Coff64Nonleaf{
				public_index: coff64_require_u32(u64(function_index), 'public symbol index')!
			}
		}
	}
	nonleafs := coff64_materialize_nonleafs(mut used, bases)!
	mut external_names := []string{cap: plan.externals.len}
	for external in plan.externals {
		external_names << external.name
	}
	coff64_validate_external_generated_names(external_names, nonleafs)!
}

fn coff64_prepare_symbols(o &Object, nonleafs []Coff64Nonleaf, symbol_count u64) ![]Coff64StandardSymbol {
	standard_count := if nonleafs.len == 0 { symbol_count } else { symbol_count - 1 }
	if standard_count > u64(max_int) {
		return error('COFF64 standard symbol count exceeds the host array limit')
	}
	mut symbols := []Coff64StandardSymbol{cap: int(standard_count)}
	for public_index, symbol in o.symbols {
		mut value := u32(0)
		mut section_number := i16(0)
		mut typ := coff64_image_sym_type_function
		if !symbol.intentional_external {
			value = coff64_require_u32(symbol.offset, 'public symbol value')!
			section_number = 1
			typ = 0
		}
		symbols << Coff64StandardSymbol{
			index:          coff64_require_u32(u64(public_index), 'public symbol index')!
			name:           symbol.name.clone()
			value:          value
			section_number: section_number
			typ:            typ
			storage_class:  coff64_image_sym_class_external
		}
	}
	public_count := u64(o.symbols.len)
	for nonleaf_index, nonleaf in nonleafs {
		index := coff64_checked_add(public_count, u64(nonleaf_index), 'end symbol index')!
		symbols << Coff64StandardSymbol{
			index:          coff64_require_u32(index, 'end symbol index')!
			name:           nonleaf.end_name
			value:          nonleaf.end
			section_number: 1
			storage_class:  coff64_image_sym_class_label
		}
	}
	if nonleafs.len != 0 {
		xdata_index := coff64_checked_add(public_count, u64(nonleafs.len), '.xdata symbol index')!
		symbols << Coff64StandardSymbol{
			index:          coff64_require_u32(xdata_index, '.xdata symbol index')!
			name:           '.xdata'
			section_number: 3
			storage_class:  coff64_image_sym_class_static
			aux_count:      1
		}
		for nonleaf_index in 1 .. nonleafs.len {
			index_delta := coff64_checked_add(u64(nonleaf_index), 1, 'unwind symbol index delta')!
			index := coff64_checked_add(xdata_index, index_delta, 'unwind symbol index')!
			symbols << Coff64StandardSymbol{
				index:          coff64_require_u32(index, 'unwind symbol index')!
				name:           nonleafs[nonleaf_index].unwind_name
				value:          nonleafs[nonleaf_index].unwind
				section_number: 3
				storage_class:  coff64_image_sym_class_static
			}
		}
	}
	return symbols
}

fn coff64_string_size(symbols []Coff64StandardSymbol, symbol_count u64) !u64 {
	if symbol_count == 0 {
		return 0
	}
	mut size := u64(4)
	for symbol in symbols {
		if symbol.name.len > 8 {
			size = coff64_checked_string_size(size, u64(symbol.name.len))!
		}
	}
	return size
}

fn coff64_preflight(o &Object) !Coff64Preflight {
	o.validate_with_coff_function_frames(false)!
	counts := coff64_scan_counts(o)!
	coff64_validate_counts(counts.text_relocation_count, counts.nonleaf_count)!
	symbol_count_u64 := coff64_symbol_count(u64(o.symbols.len), counts.nonleaf_count)!
	mut text_relocations := o.call_relocations.clone()
	text_relocations.sort(a.offset < b.offset)
	nonleafs := coff64_prepare_nonleafs(o, text_relocations)!
	if u64(nonleafs.len) != counts.nonleaf_count {
		return error('COFF64 materialized nonleaf count does not match ownership scan')
	}
	standard_symbols := coff64_prepare_symbols(o, nonleafs, symbol_count_u64)!
	expected_standard_count := if nonleafs.len == 0 {
		symbol_count_u64
	} else {
		symbol_count_u64 - 1
	}
	if u64(standard_symbols.len) != expected_standard_count {
		return error('COFF64 internal standard symbol count mismatch')
	}
	string_size := coff64_string_size(standard_symbols, symbol_count_u64)!
	layout := coff64_build_layout(u64(o.text.len), counts.text_relocation_count,
		counts.nonleaf_count, symbol_count_u64, string_size)!

	xdata_symbol_index_u64 := if nonleafs.len == 0 {
		u64(max_u32)
	} else {
		coff64_checked_add(u64(o.symbols.len), u64(nonleafs.len), '.xdata symbol index')!
	}
	xdata_symbol_index := if nonleafs.len == 0 {
		max_u32
	} else {
		coff64_require_u32(xdata_symbol_index_u64, '.xdata symbol index')!
	}
	xdata_aux_index := if nonleafs.len == 0 {
		u64(max_u32)
	} else {
		coff64_checked_add(xdata_symbol_index_u64, 1, '.xdata auxiliary index')!
	}
	mut physical_text_relocations := []Coff64Relocation{cap: text_relocations.len}
	for relocation in text_relocations {
		physical_text_relocations << Coff64Relocation{
			offset:       coff64_require_u32(relocation.offset, '.text relocation offset')!
			symbol_index: coff64_validate_relocation_symbol_index(u64(relocation.symbol_id),
				symbol_count_u64, xdata_aux_index)!
			typ:          coff64_image_rel_amd64_rel32
		}
	}
	pdata_relocation_capacity := coff64_checked_host_size(layout.pdata_reloc_count)!
	mut pdata_relocations := []Coff64Relocation{cap: pdata_relocation_capacity}
	for nonleaf_index, nonleaf in nonleafs {
		begin_index := u64(nonleaf.public_index)
		end_index := coff64_checked_add(u64(o.symbols.len), u64(nonleaf_index), 'end symbol index')!
		unwind_index := if nonleaf_index == 0 {
			xdata_symbol_index_u64
		} else {
			unwind_delta := coff64_checked_add(u64(nonleaf_index), 1, 'unwind symbol index delta')!
			coff64_checked_add(xdata_symbol_index_u64, unwind_delta, 'unwind symbol index')!
		}
		record_offset := coff64_checked_mul(u64(nonleaf_index), coff64_runtime_function_size,
			'.pdata record offset')!
		pdata_relocations << Coff64Relocation{
			offset:       coff64_require_u32(record_offset, '.pdata begin relocation offset')!
			symbol_index: coff64_validate_relocation_symbol_index(begin_index, symbol_count_u64,
				xdata_aux_index)!
			typ:          coff64_image_rel_amd64_addr32nb
		}
		pdata_relocations << Coff64Relocation{
			offset:       coff64_require_u32(coff64_checked_add(record_offset, 4,
				'.pdata end relocation offset')!, '.pdata end relocation offset')!
			symbol_index: coff64_validate_relocation_symbol_index(end_index, symbol_count_u64,
				xdata_aux_index)!
			typ:          coff64_image_rel_amd64_addr32nb
		}
		pdata_relocations << Coff64Relocation{
			offset:       coff64_require_u32(coff64_checked_add(record_offset, 8,
				'.pdata unwind relocation offset')!, '.pdata unwind relocation offset')!
			symbol_index: coff64_validate_relocation_symbol_index(unwind_index, symbol_count_u64,
				xdata_aux_index)!
			typ:          coff64_image_rel_amd64_addr32nb
		}
	}
	if u64(physical_text_relocations.len) != layout.text_reloc_count {
		return error('COFF64 internal .text relocation count mismatch')
	}
	if u64(pdata_relocations.len) != layout.pdata_reloc_count {
		return error('COFF64 internal .pdata relocation count mismatch')
	}

	mut symbol_name_offsets := []u32{cap: standard_symbols.len}
	mut string_cursor := u64(4)
	for symbol in standard_symbols {
		if symbol.name.len <= 8 {
			symbol_name_offsets << u32(0)
			continue
		}
		symbol_name_offsets << coff64_require_u32(string_cursor, 'string offset')!
		string_cursor = coff64_checked_string_size(string_cursor, u64(symbol.name.len))!
	}
	if symbol_count_u64 != 0 && string_cursor != string_size {
		return error('COFF64 internal string size mismatch')
	}
	if symbol_name_offsets.len != standard_symbols.len {
		return error('COFF64 internal symbol name offset count mismatch')
	}

	return Coff64Preflight{
		layout:              layout
		output_capacity:     coff64_checked_host_size(layout.final_extent)!
		text_relocations:    physical_text_relocations
		pdata_relocations:   pdata_relocations
		nonleafs:            nonleafs
		standard_symbols:    standard_symbols
		symbol_name_offsets: symbol_name_offsets
		symbol_count:        u32(symbol_count_u64)
		xdata_symbol_index:  xdata_symbol_index
	}
}

fn coff64_private_data_preflight(o &Object) !Coff64PrivateDataPreflight {
	base := coff64_preflight(o)!
	if o.private_data_symbols.len == 0 {
		return error('COFF64 private data has no symbols')
	}
	new_symbol_count_u64 := coff64_private_data_symbol_count(u64(base.symbol_count),
		u64(o.private_data_symbols.len))!
	data_section_number := if base.nonleafs.len == 0 { i16(2) } else { i16(4) }
	mut standard_symbols := base.standard_symbols.clone()
	for data_index, symbol in o.private_data_symbols {
		physical_index := coff64_checked_add(u64(base.symbol_count), u64(data_index),
			'private data symbol index')!
		standard_symbols << Coff64StandardSymbol{
			index:          coff64_require_u32(physical_index, 'private data symbol index')!
			name:           symbol.name.clone()
			value:          coff64_require_u32(symbol.offset, 'private data symbol value')!
			section_number: data_section_number
			storage_class:  coff64_image_sym_class_static
		}
	}
	string_size := coff64_string_size(standard_symbols, new_symbol_count_u64)!
	layout := coff64_build_private_data_layout(u64(o.text.len), u64(o.private_data.len),
		u64(base.text_relocations.len), u64(base.nonleafs.len), new_symbol_count_u64, string_size)!
	mut symbol_name_offsets := []u32{cap: standard_symbols.len}
	mut string_cursor := u64(4)
	for symbol in standard_symbols {
		if symbol.name.len <= 8 {
			symbol_name_offsets << u32(0)
			continue
		}
		symbol_name_offsets << coff64_require_u32(string_cursor, 'string offset')!
		string_cursor = coff64_checked_string_size(string_cursor, u64(symbol.name.len))!
	}
	if string_cursor != string_size {
		return error('COFF64 internal private-data string size mismatch')
	}
	return Coff64PrivateDataPreflight{
		layout:              layout
		output_capacity:     coff64_checked_host_size(layout.final_extent)!
		text_relocations:    base.text_relocations
		pdata_relocations:   base.pdata_relocations
		nonleafs:            base.nonleafs
		standard_symbols:    standard_symbols
		symbol_name_offsets: symbol_name_offsets
		symbol_count:        u32(new_symbol_count_u64)
		xdata_symbol_index:  base.xdata_symbol_index
	}
}

fn coff64_object_data_prepare_legacy(o &Object) !Coff64ObjectDataLegacyPreflight {
	counts := coff64_scan_counts(o)!
	coff64_validate_counts(counts.text_relocation_count, counts.nonleaf_count)!
	symbol_count_u64 := coff64_symbol_count(u64(o.symbols.len), counts.nonleaf_count)!
	mut text_relocations := o.call_relocations.clone()
	text_relocations.sort(a.offset < b.offset)
	nonleafs := coff64_prepare_nonleafs(o, text_relocations)!
	if u64(nonleafs.len) != counts.nonleaf_count {
		return error('COFF64 materialized nonleaf count does not match ownership scan')
	}
	standard_symbols := coff64_prepare_symbols(o, nonleafs, symbol_count_u64)!
	expected_standard_count := if nonleafs.len == 0 {
		symbol_count_u64
	} else {
		symbol_count_u64 - 1
	}
	if u64(standard_symbols.len) != expected_standard_count {
		return error('COFF64 internal standard symbol count mismatch')
	}

	xdata_symbol_index_u64 := if nonleafs.len == 0 {
		u64(max_u32)
	} else {
		coff64_checked_add(u64(o.symbols.len), u64(nonleafs.len), '.xdata symbol index')!
	}
	xdata_symbol_index := if nonleafs.len == 0 {
		max_u32
	} else {
		coff64_require_u32(xdata_symbol_index_u64, '.xdata symbol index')!
	}
	xdata_aux_index := if nonleafs.len == 0 {
		u64(max_u32)
	} else {
		coff64_checked_add(xdata_symbol_index_u64, 1, '.xdata auxiliary index')!
	}

	mut physical_text_relocations := []Coff64Relocation{cap: text_relocations.len}
	for relocation in text_relocations {
		physical_text_relocations << Coff64Relocation{
			offset:       coff64_require_u32(relocation.offset, '.text relocation offset')!
			symbol_index: coff64_validate_relocation_symbol_index(u64(relocation.symbol_id),
				symbol_count_u64, xdata_aux_index)!
			typ:          coff64_image_rel_amd64_rel32
		}
	}
	pdata_relocation_count := coff64_checked_mul(u64(nonleafs.len), 3, '.pdata relocation count')!
	mut pdata_relocations := []Coff64Relocation{cap: coff64_checked_host_size(pdata_relocation_count)!}
	for nonleaf_index, nonleaf in nonleafs {
		begin_index := u64(nonleaf.public_index)
		end_index := coff64_checked_add(u64(o.symbols.len), u64(nonleaf_index), 'end symbol index')!
		unwind_index := if nonleaf_index == 0 {
			xdata_symbol_index_u64
		} else {
			unwind_delta := coff64_checked_add(u64(nonleaf_index), 1, 'unwind symbol index delta')!
			coff64_checked_add(xdata_symbol_index_u64, unwind_delta, 'unwind symbol index')!
		}
		record_offset := coff64_checked_mul(u64(nonleaf_index), coff64_runtime_function_size,
			'.pdata record offset')!
		pdata_relocations << Coff64Relocation{
			offset:       coff64_require_u32(record_offset, '.pdata begin relocation offset')!
			symbol_index: coff64_validate_relocation_symbol_index(begin_index, symbol_count_u64,
				xdata_aux_index)!
			typ:          coff64_image_rel_amd64_addr32nb
		}
		pdata_relocations << Coff64Relocation{
			offset:       coff64_require_u32(coff64_checked_add(record_offset, 4,
				'.pdata end relocation offset')!, '.pdata end relocation offset')!
			symbol_index: coff64_validate_relocation_symbol_index(end_index, symbol_count_u64,
				xdata_aux_index)!
			typ:          coff64_image_rel_amd64_addr32nb
		}
		pdata_relocations << Coff64Relocation{
			offset:       coff64_require_u32(coff64_checked_add(record_offset, 8,
				'.pdata unwind relocation offset')!, '.pdata unwind relocation offset')!
			symbol_index: coff64_validate_relocation_symbol_index(unwind_index, symbol_count_u64,
				xdata_aux_index)!
			typ:          coff64_image_rel_amd64_addr32nb
		}
	}
	if u64(physical_text_relocations.len) != counts.text_relocation_count {
		return error('COFF64 internal .text relocation count mismatch')
	}
	if u64(pdata_relocations.len) != pdata_relocation_count {
		return error('COFF64 internal .pdata relocation count mismatch')
	}

	return Coff64ObjectDataLegacyPreflight{
		text_relocations:   physical_text_relocations
		pdata_relocations:  pdata_relocations
		nonleafs:           nonleafs
		standard_symbols:   standard_symbols
		symbol_count:       u32(symbol_count_u64)
		xdata_symbol_index: xdata_symbol_index
	}
}

fn coff64_object_data_relocation_encoding(mapped ObjectDataFormatRelocation) !Coff64ObjectDataRelocationEncoding {
	return match mapped {
		.coff_addr64 {
			Coff64ObjectDataRelocationEncoding{
				typ:   coff64_image_rel_amd64_addr64
				width: 8
			}
		}
		.coff_addr32 {
			Coff64ObjectDataRelocationEncoding{
				typ:   coff64_image_rel_amd64_addr32
				width: 4
			}
		}
		.coff_addr32nb {
			Coff64ObjectDataRelocationEncoding{
				typ:   coff64_image_rel_amd64_addr32nb
				width: 4
			}
		}
		.coff_rel32 {
			Coff64ObjectDataRelocationEncoding{
				typ:   coff64_image_rel_amd64_rel32
				width: 4
			}
		}
		.coff_rel32_1 {
			Coff64ObjectDataRelocationEncoding{
				typ:   coff64_image_rel_amd64_rel32_1
				width: 4
			}
		}
		.coff_rel32_2 {
			Coff64ObjectDataRelocationEncoding{
				typ:   coff64_image_rel_amd64_rel32_2
				width: 4
			}
		}
		.coff_rel32_3 {
			Coff64ObjectDataRelocationEncoding{
				typ:   coff64_image_rel_amd64_rel32_3
				width: 4
			}
		}
		.coff_rel32_4 {
			Coff64ObjectDataRelocationEncoding{
				typ:   coff64_image_rel_amd64_rel32_4
				width: 4
			}
		}
		.coff_rel32_5 {
			Coff64ObjectDataRelocationEncoding{
				typ:   coff64_image_rel_amd64_rel32_5
				width: 4
			}
		}
		else {
			error('COFF64 object data relocation ${mapped} is unsupported')
		}
	}
}

fn coff64_validate_object_data_addend(mapped ObjectDataFormatRelocation, addend i64) ! {
	if mapped == .coff_addr64 {
		return
	}
	if addend < i64(min_i32) || addend > i64(max_i32) {
		return error('COFF64 object data relocation ${mapped} addend ${addend} is outside signed i32')
	}
}

fn coff64_stage_object_data_addend(mut bytes []u8, offset u64, width u64, addend i64) ! {
	if width !in [u64(4), 8] {
		return error('COFF64 object data addend width ${width} is unsupported')
	}
	end := coff64_checked_add(offset, width, 'object data addend extent')!
	if end > u64(bytes.len) {
		return error('COFF64 object data addend field exceeds staged section bytes')
	}
	start := int(offset)
	for byte_index in 0 .. int(width) {
		if bytes[start + byte_index] != 0 {
			return error('COFF64 object data addend field is not a zero placeholder')
		}
	}
	raw := u64(addend)
	for byte_index in 0 .. int(width) {
		bytes[start + byte_index] = u8(raw >> (byte_index * 8))
	}
}

fn coff64_object_data_physical_section_index(sections []Coff64ObjectDataPhysicalSection, name string) !int {
	for index, section in sections {
		if section.name == name {
			return index
		}
	}
	return error('COFF64 required physical section ${name} is absent')
}

fn coff64_object_data_section_name(kind ObjectDataSectionKind) !string {
	return match kind {
		.text { '.text' }
		.rodata { '.rdata' }
		.data { '.data' }
		.bss { '.bss' }
		.unknown { error('COFF64 object data section kind is missing') }
	}
}

fn coff64_object_data_section_number(sections []Coff64ObjectDataPhysicalSection, kind ObjectDataSectionKind) !i16 {
	name := coff64_object_data_section_name(kind)!
	index := coff64_object_data_physical_section_index(sections, name)!
	number := index + 1
	if number > int(max_i16) {
		return error('COFF64 section number exceeds i16')
	}
	return i16(number)
}

fn coff64_object_data_preflight(o &Object) !Coff64ObjectDataPreflight {
	o.validate_with_coff_function_frames(true)!
	if object_data_is_empty(&o.object_data) {
		return error('COFF64 object-data preflight requires object data')
	}
	for section in o.object_data.sections {
		_ = coff64_alignment_characteristic(section.alignment)!
	}

	legacy := coff64_object_data_prepare_legacy(o)!
	mut object_text_relocation_count := u64(0)
	for relocation in o.object_data.relocations {
		if relocation.source_section == .text {
			object_text_relocation_count = coff64_checked_add(object_text_relocation_count, 1,
				'object-data .text relocation count')!
		}
	}
	_ = coff64_validate_combined_text_relocation_count(u64(legacy.text_relocations.len),
		object_text_relocation_count)!
	mut sections := []Coff64ObjectDataPhysicalSection{cap: 6}
	sections << Coff64ObjectDataPhysicalSection{
		name:            '.text'
		semantic_size:   u64(o.text.len)
		characteristics: coff64_text_characteristics
		bytes:           o.text.clone()
		relocations:     legacy.text_relocations.clone()
	}

	xdata_size := coff64_checked_mul(u64(legacy.nonleafs.len), coff64_unwind_info_size,
		'.xdata size')!
	if legacy.nonleafs.len != 0 {
		pdata_size := coff64_checked_mul(u64(legacy.nonleafs.len), coff64_runtime_function_size,
			'.pdata size')!
		sections << Coff64ObjectDataPhysicalSection{
			name:            '.pdata'
			semantic_size:   pdata_size
			characteristics: coff64_data_characteristics
			bytes:           []u8{len: coff64_checked_host_size(pdata_size)!}
			relocations:     legacy.pdata_relocations.clone()
		}
		mut xdata_bytes := []u8{cap: coff64_checked_host_size(xdata_size)!}
		for nonleaf in legacy.nonleafs {
			xdata_bytes << nonleaf.windows_unwind_bytes
		}
		sections << Coff64ObjectDataPhysicalSection{
			name:            '.xdata'
			semantic_size:   xdata_size
			characteristics: coff64_data_characteristics
			bytes:           xdata_bytes
		}
	}

	rodata_index := object_data_find_section(o.object_data.sections, .rodata)
	if rodata_index >= 0 {
		rodata := o.object_data.sections[rodata_index]
		sections << Coff64ObjectDataPhysicalSection{
			name:            '.rdata'
			semantic_size:   rodata.size
			characteristics: coff64_rdata_base_characteristics | coff64_alignment_characteristic(rodata.alignment)!
			bytes:           rodata.bytes.clone()
		}
	}

	object_data_index := object_data_find_section(o.object_data.sections, .data)
	mut object_data_offset := u64(0)
	if o.private_data.len != 0 || object_data_index >= 0 {
		private_size := u64(o.private_data.len)
		mut data_alignment := u64(8)
		mut data_bytes := o.private_data.clone()
		if object_data_index >= 0 {
			object_section := o.object_data.sections[object_data_index]
			object_data_offset = coff64_align_to(private_size, object_section.alignment,
				'merged .data object offset')!
			if o.private_data.len == 0 {
				data_alignment = object_section.alignment
			} else if object_section.alignment > data_alignment {
				data_alignment = object_section.alignment
			}
			merged_size := coff64_checked_add(object_data_offset, object_section.size,
				'merged .data size')!
			_ = coff64_require_u32(merged_size, '.data size')!
			_ = coff64_checked_host_size(merged_size)!
			for u64(data_bytes.len) < object_data_offset {
				data_bytes << u8(0)
			}
			data_bytes << object_section.bytes
		}
		sections << Coff64ObjectDataPhysicalSection{
			name:            '.data'
			semantic_size:   u64(data_bytes.len)
			characteristics: coff64_writable_data_base_characteristics | coff64_alignment_characteristic(data_alignment)!
			bytes:           data_bytes
		}
	}

	bss_index := object_data_find_section(o.object_data.sections, .bss)
	if bss_index >= 0 {
		bss := o.object_data.sections[bss_index]
		sections << Coff64ObjectDataPhysicalSection{
			name:            '.bss'
			semantic_size:   bss.size
			characteristics: coff64_bss_base_characteristics | coff64_alignment_characteristic(bss.alignment)!
		}
	}
	if sections.len > int(max_i16) {
		return error('COFF64 section count exceeds i16')
	}

	mut standard_symbols := legacy.standard_symbols.clone()
	with_private_count := coff64_private_data_symbol_count(u64(legacy.symbol_count),
		u64(o.private_data_symbols.len))!
	total_symbol_count := coff64_private_data_symbol_count(with_private_count,
		u64(o.object_data.symbols.len))!
	if o.private_data_symbols.len != 0 {
		data_section_number := coff64_object_data_section_number(sections, .data)!
		for data_index, symbol in o.private_data_symbols {
			physical_index := coff64_checked_add(u64(legacy.symbol_count), u64(data_index),
				'private data symbol index')!
			standard_symbols << Coff64StandardSymbol{
				index:          coff64_require_u32(physical_index, 'private data symbol index')!
				name:           symbol.name.clone()
				value:          coff64_require_u32(symbol.offset, 'private data symbol value')!
				section_number: data_section_number
				storage_class:  coff64_image_sym_class_static
			}
		}
	}

	mut legacy_names := map[string]bool{}
	for symbol in standard_symbols {
		legacy_names[symbol.name] = true
	}
	for symbol in o.object_data.symbols {
		if symbol.kind == .named && legacy_names[symbol.name] {
			return error('COFF64 object data symbol `${symbol.name}` collides with an existing physical symbol')
		}
	}
	mut used_names := legacy_names.clone()
	for section in sections {
		used_names[section.name] = true
	}
	for symbol in o.object_data.symbols {
		if symbol.kind == .named {
			used_names[symbol.name] = true
		}
	}

	object_symbol_base := with_private_count
	for symbol_index, symbol in o.object_data.symbols {
		mut name := symbol.name.clone()
		if symbol.kind == .internal {
			name = coff64_private_name(mut used_names, 'obj', u64(symbol_index))!
		}
		mut value := symbol.offset
		if symbol.section == .data {
			value = coff64_checked_add(object_data_offset, value, 'object data symbol value')!
		}
		physical_index := coff64_checked_add(object_symbol_base, u64(symbol_index),
			'object data symbol index')!
		standard_symbols << Coff64StandardSymbol{
			index:          coff64_require_u32(physical_index, 'object data symbol index')!
			name:           name
			value:          coff64_require_u32(value, 'object data symbol value')!
			section_number: coff64_object_data_section_number(sections, symbol.section)!
			storage_class:  coff64_image_sym_class_static
		}
	}

	xdata_aux_index := if legacy.nonleafs.len == 0 {
		u64(max_u32)
	} else {
		coff64_checked_add(u64(legacy.xdata_symbol_index), 1, '.xdata auxiliary index')!
	}
	for relocation in o.object_data.relocations {
		mapped := object_data_map_relocation(&relocation, .coff_amd64)!
		encoding := coff64_object_data_relocation_encoding(mapped)!
		coff64_validate_object_data_addend(mapped, relocation.addend)!
		source_name := coff64_object_data_section_name(relocation.source_section)!
		source_index := coff64_object_data_physical_section_index(sections, source_name)!
		mut physical_offset := relocation.offset
		if relocation.source_section == .data {
			physical_offset = coff64_checked_add(object_data_offset, physical_offset,
				'merged .data relocation offset')!
		}
		coff64_stage_object_data_addend(mut sections[source_index].bytes, physical_offset,
			encoding.width, relocation.addend)!
		target_index := coff64_checked_add(object_symbol_base, u64(relocation.target_symbol.id),
			'object data relocation symbol index')!
		sections[source_index].relocations << Coff64Relocation{
			offset:       coff64_require_u32(physical_offset, 'object data relocation offset')!
			symbol_index: coff64_validate_relocation_symbol_index(target_index, total_symbol_count,
				xdata_aux_index)!
			typ:          encoding.typ
		}
	}
	for section_index in 0 .. sections.len {
		sections[section_index].relocations.sort(a.offset < b.offset)
		coff64_validate_physical_relocation_count(sections[section_index].name,
			u64(sections[section_index].relocations.len))!
		if sections[section_index].name == '.bss' && sections[section_index].relocations.len != 0 {
			return error('COFF64 .bss cannot originate relocations')
		}
	}

	string_size := coff64_string_size(standard_symbols, total_symbol_count)!
	mut symbol_name_offsets := []u32{cap: standard_symbols.len}
	mut string_cursor := u64(4)
	for symbol in standard_symbols {
		if symbol.name.len <= 8 {
			symbol_name_offsets << u32(0)
			continue
		}
		symbol_name_offsets << coff64_require_u32(string_cursor, 'string offset')!
		string_cursor = coff64_checked_string_size(string_cursor, u64(symbol.name.len))!
	}
	if total_symbol_count == 0 {
		string_cursor = 0
	}
	if string_cursor != string_size {
		return error('COFF64 internal object-data string size mismatch')
	}

	section_headers_size := coff64_checked_mul(u64(sections.len), coff64_section_header_size,
		'section header table size')!
	header_end :=
		coff64_checked_add(coff64_file_header_size, section_headers_size, 'header extent')!
	mut cursor := header_end
	for section_index in 0 .. sections.len {
		section := sections[section_index]
		_ = coff64_require_u32(section.semantic_size, '${section.name} size')!
		if section.name == '.bss' || section.bytes.len == 0 {
			continue
		}
		placement := coff64_place(cursor, u64(section.bytes.len), section.name)!
		sections[section_index].raw_pointer = placement.offset
		cursor = placement.end
	}
	for section_index in 0 .. sections.len {
		relocation_count := u64(sections[section_index].relocations.len)
		if relocation_count == 0 {
			continue
		}
		relocation_size := coff64_checked_mul(relocation_count, coff64_relocation_size,
			'${sections[section_index].name} relocation table size')!
		placement := coff64_place(cursor, relocation_size,
			'${sections[section_index].name} relocations')!
		sections[section_index].relocation_pointer = placement.offset
		cursor = placement.end
	}

	mut symbol_table := u64(0)
	mut strings := u64(0)
	mut final_extent := cursor
	if total_symbol_count != 0 {
		symbol_table = coff64_align4(cursor, 'symbol table offset')!
		symbol_bytes := coff64_checked_mul(total_symbol_count, coff64_symbol_size,
			'symbol table size')!
		strings = coff64_checked_add(symbol_table, symbol_bytes, 'string table offset')!
		final_extent = coff64_checked_add(strings, string_size, 'file extent')!
	}
	for section in sections {
		_ = coff64_require_u32(section.raw_pointer, '${section.name} raw pointer')!
		_ = coff64_require_u32(section.relocation_pointer, '${section.name} relocation pointer')!
	}
	_ = coff64_require_u32(symbol_table, 'symbol table pointer')!
	_ = coff64_require_u32(strings, 'string table pointer')!
	_ = coff64_require_u32(final_extent, 'file extent')!

	return Coff64ObjectDataPreflight{
		sections:            sections
		output_capacity:     coff64_checked_host_size(final_extent)!
		standard_symbols:    standard_symbols
		symbol_name_offsets: symbol_name_offsets
		symbol_count:        u32(total_symbol_count)
		symbol_table:        symbol_table
		strings:             strings
		string_size:         string_size
		final_extent:        final_extent
		xdata_symbol_index:  legacy.xdata_symbol_index
		xdata_size:          xdata_size
	}
}

fn coff64_write_u16(mut output []u8, value u16) {
	output << u8(value)
	output << u8(value >> 8)
}

fn coff64_write_u32(mut output []u8, value u32) {
	output << u8(value)
	output << u8(value >> 8)
	output << u8(value >> 16)
	output << u8(value >> 24)
}

fn coff64_write_short_name(mut output []u8, name string) ! {
	if name.len > 8 {
		return error('COFF64 section name `${name}` exceeds eight bytes')
	}
	output << name.bytes()
	for _ in name.len .. 8 {
		output << u8(0)
	}
}

fn coff64_pad_to(mut output []u8, target u64) ! {
	if target > u64(max_int) {
		return error('COFF64 output offset exceeds the host array limit')
	}
	if u64(output.len) > target {
		return error('COFF64 internal layout moved backwards')
	}
	for u64(output.len) < target {
		output << u8(0)
	}
}

fn coff64_write_section_header(mut output []u8, name string, raw_size u64, raw_pointer u64, relocation_pointer u64, relocation_count u64, characteristics u32) ! {
	coff64_write_short_name(mut output, name)!
	coff64_write_u32(mut output, 0)
	coff64_write_u32(mut output, 0)
	coff64_write_u32(mut output, coff64_require_u32(raw_size, '${name} raw size')!)
	coff64_write_u32(mut output, coff64_require_u32(raw_pointer, '${name} raw pointer')!)
	coff64_write_u32(mut output, coff64_require_u32(relocation_pointer,
		'${name} relocation pointer')!)
	coff64_write_u32(mut output, 0)
	coff64_write_u16(mut output, u16(relocation_count))
	coff64_write_u16(mut output, 0)
	coff64_write_u32(mut output, characteristics)
}

fn coff64_write_symbol(mut output []u8, symbol Coff64StandardSymbol, name_offset u32) ! {
	if symbol.name.len <= 8 {
		coff64_write_short_name(mut output, symbol.name)!
	} else {
		coff64_write_u32(mut output, 0)
		coff64_write_u32(mut output, name_offset)
	}
	coff64_write_u32(mut output, symbol.value)
	coff64_write_u16(mut output, u16(symbol.section_number))
	coff64_write_u16(mut output, symbol.typ)
	output << symbol.storage_class
	output << symbol.aux_count
}

fn coff64_write_xdata_aux(mut output []u8, xdata_size u64) ! {
	coff64_write_u32(mut output, coff64_require_u32(xdata_size, '.xdata auxiliary length')!)
	coff64_write_u16(mut output, 0)
	coff64_write_u16(mut output, 0)
	coff64_write_u32(mut output, 0)
	coff64_write_u16(mut output, 0)
	output << u8(0)
	output << [u8(0), 0, 0]
}

fn coff64_object_data_relocatable_bytes(o &Object) ![]u8 {
	preflight := coff64_object_data_preflight(o)!
	mut output := []u8{cap: preflight.output_capacity}

	coff64_write_u16(mut output, coff64_image_file_machine_amd64)
	coff64_write_u16(mut output, u16(preflight.sections.len))
	coff64_write_u32(mut output, 0)
	coff64_write_u32(mut output,
		coff64_require_u32(preflight.symbol_table, 'symbol table pointer')!)
	coff64_write_u32(mut output, preflight.symbol_count)
	coff64_write_u16(mut output, 0)
	coff64_write_u16(mut output, 0)
	for section in preflight.sections {
		coff64_write_section_header(mut output, section.name, section.semantic_size,
			section.raw_pointer, section.relocation_pointer, u64(section.relocations.len),
			section.characteristics)!
	}
	expected_header_end := coff64_checked_add(coff64_file_header_size, coff64_checked_mul(u64(preflight.sections.len),
		coff64_section_header_size, 'section header table size')!, 'header extent')!
	if u64(output.len) != expected_header_end {
		return error('COFF64 internal object-data header size mismatch')
	}

	for section in preflight.sections {
		if section.bytes.len == 0 {
			continue
		}
		coff64_pad_to(mut output, section.raw_pointer)!
		output << section.bytes
	}
	for section in preflight.sections {
		if section.relocations.len == 0 {
			continue
		}
		coff64_pad_to(mut output, section.relocation_pointer)!
		for relocation in section.relocations {
			coff64_write_u32(mut output, relocation.offset)
			coff64_write_u32(mut output, relocation.symbol_index)
			coff64_write_u16(mut output, relocation.typ)
		}
	}

	if preflight.symbol_count != 0 {
		coff64_pad_to(mut output, preflight.symbol_table)!
		mut physical_index := u64(0)
		for symbol_index, symbol in preflight.standard_symbols {
			if u64(symbol.index) != physical_index {
				return error('COFF64 object-data symbol order does not match physical indices')
			}
			coff64_write_symbol(mut output, symbol, preflight.symbol_name_offsets[symbol_index])!
			physical_index++
			if symbol.aux_count == 1 {
				if symbol.index != preflight.xdata_symbol_index {
					return error('COFF64 unexpected auxiliary symbol owner')
				}
				coff64_write_xdata_aux(mut output, preflight.xdata_size)!
				physical_index++
			}
		}
		if physical_index != u64(preflight.symbol_count) {
			return error('COFF64 object-data physical symbol count mismatch')
		}
		if u64(output.len) != preflight.strings {
			return error('COFF64 internal object-data symbol table size mismatch')
		}
		coff64_write_u32(mut output, u32(preflight.string_size))
		for symbol in preflight.standard_symbols {
			if symbol.name.len > 8 {
				output << symbol.name.bytes()
				output << u8(0)
			}
		}
	}

	if u64(output.len) != preflight.final_extent {
		return error('COFF64 internal object-data layout size mismatch')
	}
	return output
}

fn coff64_relocatable_bytes(o &Object) ![]u8 {
	if !object_data_is_empty(&o.object_data) {
		return coff64_object_data_relocatable_bytes(o)
	}
	if o.private_data.len != 0 {
		return coff64_private_data_relocatable_bytes(o)
	}
	preflight := coff64_preflight(o)!
	layout := preflight.layout
	mut output := []u8{cap: preflight.output_capacity}

	coff64_write_u16(mut output, coff64_image_file_machine_amd64)
	coff64_write_u16(mut output, layout.section_count)
	coff64_write_u32(mut output, 0)
	coff64_write_u32(mut output, coff64_require_u32(layout.symbol_table, 'symbol table pointer')!)
	coff64_write_u32(mut output, preflight.symbol_count)
	coff64_write_u16(mut output, 0)
	coff64_write_u16(mut output, 0)

	coff64_write_section_header(mut output, '.text', layout.text_size, layout.text_raw,
		layout.text_reloc, layout.text_reloc_count, coff64_text_characteristics)!
	if preflight.nonleafs.len != 0 {
		coff64_write_section_header(mut output, '.pdata', layout.pdata_size, layout.pdata_raw,
			layout.pdata_reloc, layout.pdata_reloc_count, coff64_data_characteristics)!
		coff64_write_section_header(mut output, '.xdata', layout.xdata_size, layout.xdata_raw, 0,
			0, coff64_data_characteristics)!
	}
	if u64(output.len) != layout.header_end {
		return error('COFF64 internal header size mismatch')
	}

	if layout.text_size != 0 {
		coff64_pad_to(mut output, layout.text_raw)!
		output << o.text
	}
	if preflight.nonleafs.len != 0 {
		coff64_pad_to(mut output, layout.pdata_raw)!
		for _ in 0 .. int(layout.pdata_size) {
			output << u8(0)
		}
		coff64_pad_to(mut output, layout.xdata_raw)!
		for nonleaf in preflight.nonleafs {
			output << nonleaf.windows_unwind_bytes
		}
	}

	if preflight.text_relocations.len != 0 {
		coff64_pad_to(mut output, layout.text_reloc)!
		for relocation in preflight.text_relocations {
			coff64_write_u32(mut output, relocation.offset)
			coff64_write_u32(mut output, relocation.symbol_index)
			coff64_write_u16(mut output, relocation.typ)
		}
	}
	if preflight.nonleafs.len != 0 {
		coff64_pad_to(mut output, layout.pdata_reloc)!
		for relocation in preflight.pdata_relocations {
			coff64_write_u32(mut output, relocation.offset)
			coff64_write_u32(mut output, relocation.symbol_index)
			coff64_write_u16(mut output, relocation.typ)
		}
	}

	if preflight.symbol_count != 0 {
		coff64_pad_to(mut output, layout.symbol_table)!
		for symbol_index, symbol in preflight.standard_symbols {
			coff64_write_symbol(mut output, symbol, preflight.symbol_name_offsets[symbol_index])!
			if symbol.aux_count == 1 {
				if symbol.index != preflight.xdata_symbol_index {
					return error('COFF64 unexpected auxiliary symbol owner')
				}
				coff64_write_xdata_aux(mut output, layout.xdata_size)!
			}
		}
		if u64(output.len) != layout.strings {
			return error('COFF64 internal symbol table size mismatch')
		}
		coff64_write_u32(mut output, u32(layout.string_size))
		for symbol in preflight.standard_symbols {
			if symbol.name.len > 8 {
				output << symbol.name.bytes()
				output << u8(0)
			}
		}
	}

	if u64(output.len) != layout.final_extent {
		return error('COFF64 internal layout size mismatch')
	}
	return output
}

fn coff64_private_data_relocatable_bytes(o &Object) ![]u8 {
	if !object_data_is_empty(&o.object_data) {
		return coff64_object_data_relocatable_bytes(o)
	}
	preflight := coff64_private_data_preflight(o)!
	layout := preflight.layout
	mut output := []u8{cap: preflight.output_capacity}
	coff64_write_u16(mut output, coff64_image_file_machine_amd64)
	coff64_write_u16(mut output, layout.section_count)
	coff64_write_u32(mut output, 0)
	coff64_write_u32(mut output, coff64_require_u32(layout.symbol_table, 'symbol table pointer')!)
	coff64_write_u32(mut output, preflight.symbol_count)
	coff64_write_u16(mut output, 0)
	coff64_write_u16(mut output, 0)
	coff64_write_section_header(mut output, '.text', layout.text_size, layout.text_raw,
		layout.text_reloc, layout.text_reloc_count, coff64_text_characteristics)!
	if preflight.nonleafs.len != 0 {
		coff64_write_section_header(mut output, '.pdata', layout.pdata_size, layout.pdata_raw,
			layout.pdata_reloc, layout.pdata_reloc_count, coff64_data_characteristics)!
		coff64_write_section_header(mut output, '.xdata', layout.xdata_size, layout.xdata_raw, 0,
			0, coff64_data_characteristics)!
	}
	coff64_write_section_header(mut output, '.data', layout.data_size, layout.data_raw, 0, 0,
		coff64_private_data_characteristics)!
	if u64(output.len) != layout.header_end {
		return error('COFF64 internal private-data header size mismatch')
	}
	if layout.text_size != 0 {
		coff64_pad_to(mut output, layout.text_raw)!
		output << o.text
	}
	if preflight.nonleafs.len != 0 {
		coff64_pad_to(mut output, layout.pdata_raw)!
		for _ in 0 .. int(layout.pdata_size) {
			output << u8(0)
		}
		coff64_pad_to(mut output, layout.xdata_raw)!
		for nonleaf in preflight.nonleafs {
			output << nonleaf.windows_unwind_bytes
		}
	}
	coff64_pad_to(mut output, layout.data_raw)!
	output << o.private_data
	if preflight.text_relocations.len != 0 {
		coff64_pad_to(mut output, layout.text_reloc)!
		for relocation in preflight.text_relocations {
			coff64_write_u32(mut output, relocation.offset)
			coff64_write_u32(mut output, relocation.symbol_index)
			coff64_write_u16(mut output, relocation.typ)
		}
	}
	if preflight.nonleafs.len != 0 {
		coff64_pad_to(mut output, layout.pdata_reloc)!
		for relocation in preflight.pdata_relocations {
			coff64_write_u32(mut output, relocation.offset)
			coff64_write_u32(mut output, relocation.symbol_index)
			coff64_write_u16(mut output, relocation.typ)
		}
	}
	coff64_pad_to(mut output, layout.symbol_table)!
	for symbol_index, symbol in preflight.standard_symbols {
		coff64_write_symbol(mut output, symbol, preflight.symbol_name_offsets[symbol_index])!
		if symbol.aux_count == 1 {
			if symbol.index != preflight.xdata_symbol_index {
				return error('COFF64 unexpected auxiliary symbol owner')
			}
			coff64_write_xdata_aux(mut output, layout.xdata_size)!
		}
	}
	if u64(output.len) != layout.strings {
		return error('COFF64 internal private-data symbol table size mismatch')
	}
	coff64_write_u32(mut output, u32(layout.string_size))
	for symbol in preflight.standard_symbols {
		if symbol.name.len > 8 {
			output << symbol.name.bytes()
			output << u8(0)
		}
	}
	if u64(output.len) != layout.final_extent {
		return error('COFF64 internal private-data layout size mismatch')
	}
	return output
}
