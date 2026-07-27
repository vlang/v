// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module amd64

type SymbolID = u32

struct FunctionSymbol {
	name string
mut:
	defined              bool
	intentional_external bool
	offset               u64
	size                 u64
}

struct TextCallRelocation {
	offset    u64
	symbol_id SymbolID
}

struct ObjectFunctionFrame {
	function_symbol      SymbolID
	prologue_bytes       []u8
	epilogue_bytes       []u8
	windows_unwind_bytes []u8
}

struct PrivateDataDefinition {
	name        string
	value       i64
	width       int
	is_unsigned bool
	alignment   u64
}

struct PrivateDataSymbol {
	name      string
	offset    u64
	size      u64
	alignment u64
}

struct PrivateDataPlan {
	data_size int
	symbols   []PrivateDataSymbol
	values    []i64
}

type ObjectDataSymbolID = u32

struct ObjectDataSymbolRef {
	id     ObjectDataSymbolID
	is_set bool
}

enum ObjectDataSectionKind {
	unknown
	text
	rodata
	data
	bss
}

enum ObjectDataSymbolKind {
	unknown
	internal
	named
}

enum ObjectDataRelocationKind {
	unknown
	absolute
	pc_relative
	got_relative
}

enum ObjectDataRelocationSignedness {
	unknown
	unsigned
	signed
}

enum ObjectDataAddressIntent {
	unknown
	virtual_address
	image_relative
}

enum ObjectDataPcBias {
	unknown
	zero
	one
	two
	three
	four
	five
}

enum ObjectDataGotAccessIntent {
	unknown
	none
	load
	address
}

enum ObjectDataRelocationFormat {
	unknown
	elf_x86_64
	coff_amd64
	macho_x86_64
}

enum ObjectDataFormatRelocation {
	unknown
	elf_64
	elf_32
	elf_32s
	elf_pc32
	elf_gotpcrel
	coff_addr64
	coff_addr32
	coff_addr32nb
	coff_rel32
	coff_rel32_1
	coff_rel32_2
	coff_rel32_3
	coff_rel32_4
	coff_rel32_5
	macho_unsigned
	macho_signed
	macho_signed_1
	macho_signed_2
	macho_signed_4
	macho_got_load
	macho_got
}

struct ObjectDataSection {
mut:
	kind      ObjectDataSectionKind
	bytes     []u8
	size      u64
	alignment u64
}

struct ObjectDataSymbol {
mut:
	kind     ObjectDataSymbolKind
	name     string
	section  ObjectDataSectionKind
	offset   u64
	size     u64
	alias_of ObjectDataSymbolRef
}

struct ObjectDataRelocation {
mut:
	source_section ObjectDataSectionKind
	offset         u64
	target_symbol  ObjectDataSymbolRef
	width          int
	kind           ObjectDataRelocationKind
	signedness     ObjectDataRelocationSignedness
	address_intent ObjectDataAddressIntent
	pc_bias        ObjectDataPcBias
	got_access     ObjectDataGotAccessIntent
	addend         i64
}

struct ObjectDataDefinition {
mut:
	sections    []ObjectDataSection
	symbols     []ObjectDataSymbol
	relocations []ObjectDataRelocation
}

struct ObjectDataPlan {
mut:
	sections    []ObjectDataSection
	symbols     []ObjectDataSymbol
	relocations []ObjectDataRelocation
}

struct Object {
mut:
	text                 []u8
	symbols              []FunctionSymbol
	call_relocations     []TextCallRelocation
	function_frames      []ObjectFunctionFrame
	private_data         []u8
	private_data_symbols []PrivateDataSymbol
	object_data          ObjectDataPlan
}

fn Object.new() Object {
	return Object{
		text:                 []u8{}
		symbols:              []FunctionSymbol{}
		call_relocations:     []TextCallRelocation{}
		function_frames:      []ObjectFunctionFrame{}
		private_data:         []u8{}
		private_data_symbols: []PrivateDataSymbol{}
		object_data:          ObjectDataPlan{
			sections:    []ObjectDataSection{}
			symbols:     []ObjectDataSymbol{}
			relocations: []ObjectDataRelocation{}
		}
	}
}

fn private_data_width_size(width int) !u64 {
	return match width {
		1, 8 { u64(1) }
		16 { u64(2) }
		32 { u64(4) }
		64 { u64(8) }
		else { error('AMD64 private data integer width ${width} is unsupported') }
	}
}

fn private_data_value_in_range(width int, is_unsigned bool, value i64) bool {
	if width == 1 {
		return value == 0 || value == 1
	}
	if is_unsigned {
		if width == 64 {
			return true
		}
		if value < 0 {
			return false
		}
		return match width {
			8 { value <= 255 }
			16 { value <= 65_535 }
			32 { value <= 4_294_967_295 }
			else { false }
		}
	}
	return match width {
		8 { value >= -128 && value <= 127 }
		16 { value >= -32_768 && value <= 32_767 }
		32 { value >= -2_147_483_648 && value <= 2_147_483_647 }
		64 { true }
		else { false }
	}
}

fn private_data_checked_add(left u64, right u64, label string) !u64 {
	if left > max_u64 - right {
		return error('AMD64 private data ${label} overflows u64')
	}
	return left + right
}

fn private_data_checked_host_size(value u64) !int {
	if value > u64(max_int) {
		return error('AMD64 private data exceeds the host array limit')
	}
	return int(value)
}

fn private_data_align_up(value u64, alignment u64) !u64 {
	if alignment == 0 || alignment > 8 || alignment & (alignment - 1) != 0 {
		return error('AMD64 private data alignment ${alignment} is invalid')
	}
	remainder := value % alignment
	if remainder == 0 {
		return value
	}
	return private_data_checked_add(value, alignment - remainder, 'aligned offset')
}

fn private_data_validate_name(name string) ! {
	if name.len == 0 {
		return error('AMD64 private data symbol name must not be empty')
	}
	if name.index_u8(0) >= 0 {
		return error('AMD64 private data symbol name must not contain NUL')
	}
}

fn private_data_preflight(definitions []PrivateDataDefinition, function_names []string) !PrivateDataPlan {
	mut names := map[string]bool{}
	for name in function_names {
		names[name] = true
	}
	mut cursor := u64(0)
	mut symbols := []PrivateDataSymbol{cap: definitions.len}
	mut values := []i64{cap: definitions.len}
	for definition in definitions {
		private_data_validate_name(definition.name)!
		if names[definition.name] {
			return error('AMD64 private data symbol `${definition.name}` collides with an existing symbol')
		}
		names[definition.name] = true
		size := private_data_width_size(definition.width)!
		if definition.alignment != size {
			return error('AMD64 private data symbol `${definition.name}` alignment ${definition.alignment} does not match natural alignment ${size}')
		}
		if !private_data_value_in_range(definition.width, definition.is_unsigned, definition.value) {
			return error('AMD64 private data symbol `${definition.name}` value ${definition.value} is outside ${definition.width}-bit range')
		}
		offset := private_data_align_up(cursor, definition.alignment)!
		end := private_data_checked_add(offset, size, 'symbol extent')!
		_ = private_data_checked_host_size(end)!
		symbols << PrivateDataSymbol{
			name:      definition.name.clone()
			offset:    offset
			size:      size
			alignment: definition.alignment
		}
		values << definition.value
		cursor = end
	}
	return PrivateDataPlan{
		data_size: private_data_checked_host_size(cursor)!
		symbols:   symbols
		values:    values
	}
}

fn private_data_validate_layout(symbols []PrivateDataSymbol, data []u8) ! {
	mut names := map[string]bool{}
	mut cursor := u64(0)
	for symbol in symbols {
		private_data_validate_name(symbol.name)!
		if names[symbol.name] {
			return error('AMD64 private data contains duplicate symbol `${symbol.name}`')
		}
		names[symbol.name] = true
		if symbol.size !in [u64(1), 2, 4, 8] {
			return error('AMD64 private data symbol `${symbol.name}` has invalid size ${symbol.size}')
		}
		if symbol.alignment != symbol.size {
			return error('AMD64 private data symbol `${symbol.name}` alignment ${symbol.alignment} does not match size ${symbol.size}')
		}
		expected_offset := private_data_align_up(cursor, symbol.alignment)!
		if symbol.offset != expected_offset {
			return error('AMD64 private data symbol `${symbol.name}` offset ${symbol.offset} does not match canonical offset ${expected_offset}')
		}
		end := private_data_checked_add(symbol.offset, symbol.size, 'symbol extent')!
		if end > u64(data.len) {
			return error('AMD64 private data symbol `${symbol.name}` exceeds data size ${data.len}')
		}
		for index in int(cursor) .. int(symbol.offset) {
			if data[index] != 0 {
				return error('AMD64 private data padding before `${symbol.name}` is not zero')
			}
		}
		cursor = end
	}
	if cursor != u64(data.len) {
		return error('AMD64 private data symbols cover ${cursor} bytes but data contains ${data.len}')
	}
}

fn (mut o Object) install_private_data(plan &PrivateDataPlan) ! {
	if o.private_data.len != 0 || o.private_data_symbols.len != 0 {
		return error('AMD64 object private data is already installed')
	}
	if plan.symbols.len != plan.values.len {
		return error('AMD64 private data plan symbol/value count mismatch')
	}
	if plan.data_size < 0 {
		return error('AMD64 private data plan size must not be negative')
	}
	mut function_names := map[string]bool{}
	for symbol in o.symbols {
		function_names[symbol.name] = true
	}
	for symbol in o.object_data.symbols {
		if symbol.kind == .named {
			function_names[symbol.name] = true
		}
	}
	mut expected_cursor := u64(0)
	for symbol in plan.symbols {
		private_data_validate_name(symbol.name)!
		if function_names[symbol.name] {
			return error('AMD64 private data symbol `${symbol.name}` collides with an existing symbol')
		}
		function_names[symbol.name] = true
		if symbol.size !in [u64(1), 2, 4, 8] || symbol.alignment != symbol.size {
			return error('AMD64 private data plan symbol `${symbol.name}` has invalid size or alignment')
		}
		expected_offset := private_data_align_up(expected_cursor, symbol.alignment)!
		if symbol.offset != expected_offset {
			return error('AMD64 private data plan symbol `${symbol.name}` has noncanonical offset')
		}
		expected_cursor = private_data_checked_add(symbol.offset, symbol.size, 'symbol extent')!
	}
	if expected_cursor != u64(plan.data_size) {
		return error('AMD64 private data plan size does not match symbol extents')
	}
	mut data := []u8{len: plan.data_size}
	mut symbols := []PrivateDataSymbol{cap: plan.symbols.len}
	for index, symbol in plan.symbols {
		bits := u64(plan.values[index])
		for byte_index in 0 .. int(symbol.size) {
			data[int(symbol.offset) + byte_index] = u8(bits >> (byte_index * 8))
		}
		symbols << PrivateDataSymbol{
			name:      symbol.name.clone()
			offset:    symbol.offset
			size:      symbol.size
			alignment: symbol.alignment
		}
	}
	o.private_data = data
	o.private_data_symbols = symbols
}

fn object_validate_symbol_name(name string) ! {
	if name.len == 0 {
		return error('AMD64 object function name must not be empty')
	}
	if name.index_u8(0) >= 0 {
		return error('AMD64 object function name must not contain NUL')
	}
}

fn object_symbol_index(o &Object, id SymbolID) !int {
	if u64(id) >= u64(o.symbols.len) {
		return error('AMD64 object symbol ' + u64(id).str() + ' is out of range')
	}
	return int(id)
}

fn object_ranges_overlap(start_a u64, end_a u64, start_b u64, end_b u64) bool {
	return start_a < end_b && start_b < end_a
}

fn object_data_is_empty(plan &ObjectDataPlan) bool {
	return plan.sections.len == 0 && plan.symbols.len == 0 && plan.relocations.len == 0
}

fn object_data_checked_add(left u64, right u64, label string) !u64 {
	if left > max_u64 - right {
		return error('AMD64 object data ${label} overflows u64')
	}
	return left + right
}

fn object_data_checked_add_signed(base u64, addend i64, label string) !u64 {
	if addend >= 0 {
		return object_data_checked_add(base, u64(addend), label)
	}
	magnitude := u64(-(addend + 1)) + 1
	if base < magnitude {
		return error('AMD64 object data ${label} is below section offset zero')
	}
	return base - magnitude
}

fn object_data_validate_alignment(alignment u64) ! {
	if alignment == 0 || alignment & (alignment - 1) != 0 {
		return error('AMD64 object data section alignment ${alignment} is invalid')
	}
}

fn object_data_validate_name(name string) ! {
	if name.len == 0 {
		return error('AMD64 object data named symbol name must not be empty')
	}
	if name.index_u8(0) >= 0 {
		return error('AMD64 object data named symbol name must not contain NUL')
	}
}

fn object_data_section_order(kind ObjectDataSectionKind) !int {
	if int(kind) < int(ObjectDataSectionKind.unknown) || int(kind) > int(ObjectDataSectionKind.bss) {
		return error('AMD64 object data section kind ${int(kind)} is invalid')
	}
	return match kind {
		.rodata { 0 }
		.data { 1 }
		.bss { 2 }
		.unknown { error('AMD64 object data section kind is missing') }
		.text { error('AMD64 object data must not redefine .text') }
	}
}

fn object_data_find_section(sections []ObjectDataSection, kind ObjectDataSectionKind) int {
	for index, section in sections {
		if section.kind == kind {
			return index
		}
	}
	return -1
}

fn object_data_symbol_ref(id ObjectDataSymbolID) ObjectDataSymbolRef {
	return ObjectDataSymbolRef{
		id:     id
		is_set: true
	}
}

fn object_data_relocation_width_size(kind ObjectDataRelocationKind, width int) !u64 {
	if int(kind) < int(ObjectDataRelocationKind.unknown)
		|| int(kind) > int(ObjectDataRelocationKind.got_relative) {
		return error('AMD64 object data relocation kind ${int(kind)} is invalid')
	}
	if kind == .unknown {
		return error('AMD64 object data relocation kind is missing')
	}
	if kind == .absolute {
		if width == 32 {
			return u64(4)
		}
		if width == 64 {
			return u64(8)
		}
		return error('AMD64 object data absolute relocation width ${width} is unsupported')
	}
	if width != 32 {
		return error('AMD64 object data ${kind} relocation width ${width} is unsupported')
	}
	return u64(4)
}

fn object_data_pc_bias_bytes(bias ObjectDataPcBias) !int {
	if int(bias) < int(ObjectDataPcBias.unknown) || int(bias) > int(ObjectDataPcBias.five) {
		return error('AMD64 object data PC bias ${int(bias)} is invalid')
	}
	return match bias {
		.zero { 0 }
		.one { 1 }
		.two { 2 }
		.three { 3 }
		.four { 4 }
		.five { 5 }
		.unknown { error('AMD64 object data PC bias is missing') }
	}
}

fn object_data_validate_relocation_intent(relocation &ObjectDataRelocation) ! {
	if int(relocation.signedness) < int(ObjectDataRelocationSignedness.unknown)
		|| int(relocation.signedness) > int(ObjectDataRelocationSignedness.signed) {
		return error('AMD64 object data relocation signedness ${int(relocation.signedness)} is invalid')
	}
	if relocation.signedness == .unknown {
		return error('AMD64 object data relocation signedness is missing')
	}
	if int(relocation.address_intent) < int(ObjectDataAddressIntent.unknown)
		|| int(relocation.address_intent) > int(ObjectDataAddressIntent.image_relative) {
		return error('AMD64 object data relocation address intent ${int(relocation.address_intent)} is invalid')
	}
	if relocation.address_intent == .unknown {
		return error('AMD64 object data relocation address intent is missing')
	}
	pc_bias := object_data_pc_bias_bytes(relocation.pc_bias)!
	if int(relocation.got_access) < int(ObjectDataGotAccessIntent.unknown)
		|| int(relocation.got_access) > int(ObjectDataGotAccessIntent.address) {
		return error('AMD64 object data relocation GOT access intent ${int(relocation.got_access)} is invalid')
	}
	if relocation.got_access == .unknown {
		return error('AMD64 object data relocation GOT access intent is missing')
	}
	match relocation.kind {
		.absolute {
			if relocation.got_access != .none || pc_bias != 0 {
				return error('AMD64 object data absolute relocation intent is inconsistent')
			}
			if relocation.address_intent == .image_relative {
				if relocation.width != 32 || relocation.signedness != .unsigned {
					return error('AMD64 object data image-relative relocation must be unsigned width 32')
				}
			} else if relocation.width == 64 && relocation.signedness != .unsigned {
				return error('AMD64 object data absolute width-64 relocation must be unsigned')
			}
		}
		.pc_relative {
			if relocation.signedness != .signed || relocation.address_intent != .virtual_address
				|| relocation.got_access != .none {
				return error('AMD64 object data PC-relative relocation intent is inconsistent')
			}
		}
		.got_relative {
			if relocation.signedness != .signed || relocation.address_intent != .virtual_address
				|| pc_bias != 0 || relocation.got_access !in [.load, .address] {
				return error('AMD64 object data GOT-relative relocation intent is inconsistent')
			}
		}
		.unknown {
			return error('AMD64 object data relocation kind is missing')
		}
	}
}

fn object_data_map_relocation(relocation &ObjectDataRelocation, format ObjectDataRelocationFormat) !ObjectDataFormatRelocation {
	_ = object_data_relocation_width_size(relocation.kind, relocation.width)!
	object_data_validate_relocation_intent(relocation)!
	if int(format) < int(ObjectDataRelocationFormat.unknown)
		|| int(format) > int(ObjectDataRelocationFormat.macho_x86_64) {
		return error('AMD64 object data relocation format ${int(format)} is invalid')
	}
	if format == .unknown {
		return error('AMD64 object data relocation format is missing')
	}
	match format {
		.elf_x86_64 {
			if relocation.kind == .absolute && relocation.address_intent == .virtual_address {
				if relocation.width == 64 && relocation.signedness == .unsigned {
					return .elf_64
				}
				if relocation.width == 32 {
					return if relocation.signedness == .signed { .elf_32s } else { .elf_32 }
				}
			}
			if relocation.kind == .pc_relative && relocation.pc_bias == .zero {
				return .elf_pc32
			}
			if relocation.kind == .got_relative {
				return .elf_gotpcrel
			}
		}
		.coff_amd64 {
			if relocation.kind == .absolute && relocation.signedness == .unsigned {
				if relocation.address_intent == .image_relative {
					return .coff_addr32nb
				}
				if relocation.width == 64 {
					return .coff_addr64
				}
				return .coff_addr32
			}
			if relocation.kind == .pc_relative {
				return match relocation.pc_bias {
					.zero { .coff_rel32 }
					.one { .coff_rel32_1 }
					.two { .coff_rel32_2 }
					.three { .coff_rel32_3 }
					.four { .coff_rel32_4 }
					.five { .coff_rel32_5 }
					.unknown { error('AMD64 object data PC bias is missing') }
				}
			}
		}
		.macho_x86_64 {
			if relocation.kind == .absolute && relocation.address_intent == .virtual_address
				&& relocation.signedness == .unsigned {
				return .macho_unsigned
			}
			if relocation.kind == .pc_relative {
				return match relocation.pc_bias {
					.zero { .macho_signed }
					.one { .macho_signed_1 }
					.two { .macho_signed_2 }
					.four { .macho_signed_4 }
					else { error('AMD64 object data relocation has no macho_x86_64 mapping') }
				}
			}
			if relocation.kind == .got_relative {
				return if relocation.got_access == .load {
					.macho_got_load
				} else {
					.macho_got
				}
			}
		}
		.unknown {
			return error('AMD64 object data relocation format is missing')
		}
	}

	return error('AMD64 object data relocation has no ${format} mapping')
}

fn object_data_clone(sections []ObjectDataSection, symbols []ObjectDataSymbol, relocations []ObjectDataRelocation) ObjectDataPlan {
	mut cloned_sections := []ObjectDataSection{cap: sections.len}
	for section in sections {
		cloned_sections << ObjectDataSection{
			kind:      section.kind
			bytes:     section.bytes.clone()
			size:      section.size
			alignment: section.alignment
		}
	}
	mut cloned_symbols := []ObjectDataSymbol{cap: symbols.len}
	for symbol in symbols {
		cloned_symbols << ObjectDataSymbol{
			kind:     symbol.kind
			name:     symbol.name.clone()
			section:  symbol.section
			offset:   symbol.offset
			size:     symbol.size
			alias_of: symbol.alias_of
		}
	}
	return ObjectDataPlan{
		sections:    cloned_sections
		symbols:     cloned_symbols
		relocations: relocations.clone()
	}
}

fn object_data_validate_parts(sections []ObjectDataSection, data_symbols []ObjectDataSymbol, relocations []ObjectDataRelocation, o &Object) ! {
	mut previous_order := -1
	for section in sections {
		order := object_data_section_order(section.kind)!
		if order <= previous_order {
			return error('AMD64 object data sections must be unique and ordered .rodata, .data, .bss')
		}
		previous_order = order
		if section.size == 0 {
			return error('AMD64 object data section ${section.kind} must not be empty')
		}
		object_data_validate_alignment(section.alignment)!
		if section.kind == .bss {
			if section.bytes.len != 0 {
				return error('AMD64 object data .bss must not contain file bytes')
			}
		} else if section.size != u64(section.bytes.len) {
			return error('AMD64 object data section ${section.kind} size ${section.size} does not match payload size ${section.bytes.len}')
		}
	}

	if u64(data_symbols.len) > u64(max_u32) {
		return error('AMD64 object data has too many symbols')
	}
	mut reserved_names := map[string]bool{}
	for symbol in o.symbols {
		reserved_names[symbol.name] = true
	}
	for symbol in o.private_data_symbols {
		reserved_names[symbol.name] = true
	}
	for index, symbol in data_symbols {
		if int(symbol.kind) < int(ObjectDataSymbolKind.unknown)
			|| int(symbol.kind) > int(ObjectDataSymbolKind.named) {
			return error('AMD64 object data symbol kind ${int(symbol.kind)} is invalid')
		}
		match symbol.kind {
			.named {
				object_data_validate_name(symbol.name)!
				if reserved_names[symbol.name] {
					return error('AMD64 object data named symbol `${symbol.name}` collides with an existing symbol')
				}
			}
			.internal {
				if symbol.name.len != 0 {
					return error('AMD64 object data internal symbol must not have a name')
				}
			}
			.unknown {
				return error('AMD64 object data symbol kind is missing')
			}
		}

		if int(symbol.section) < int(ObjectDataSectionKind.unknown)
			|| int(symbol.section) > int(ObjectDataSectionKind.bss) {
			return error('AMD64 object data symbol ${index} section kind ${int(symbol.section)} is invalid')
		}
		match symbol.section {
			.rodata, .data, .bss {}
			.unknown {
				return error('AMD64 object data symbol ${index} section is missing')
			}
			.text {
				return error('AMD64 object data symbol ${index} must not own a .text range')
			}
		}

		section_index := object_data_find_section(sections, symbol.section)
		if section_index < 0 {
			return error('AMD64 object data symbol ${index} references absent section ${symbol.section}')
		}
		section := sections[section_index]
		if symbol.offset > section.size || symbol.size > section.size - symbol.offset {
			return error('AMD64 object data symbol ${index} range exceeds section ${symbol.section} size ${section.size}')
		}
		symbol_end := symbol.offset + symbol.size
		mut alias_target_index := -1
		if symbol.alias_of.is_set {
			if symbol.size == 0 {
				return error('AMD64 object data alias symbol ${index} must own a non-empty range')
			}
			if u64(symbol.alias_of.id) >= u64(index) {
				return error('AMD64 object data alias target ${u64(symbol.alias_of.id)} must precede symbol ${index}')
			}
			alias_target_index = int(symbol.alias_of.id)
			alias_target := data_symbols[alias_target_index]
			if alias_target.section != symbol.section || alias_target.offset != symbol.offset
				|| alias_target.size != symbol.size {
				return error('AMD64 object data alias symbol ${index} does not exactly match target ${alias_target_index}')
			}
		}
		for previous_index in 0 .. index {
			previous := data_symbols[previous_index]
			if previous.section != symbol.section {
				continue
			}
			previous_end := previous.offset + previous.size
			if object_ranges_overlap(symbol.offset, symbol_end, previous.offset, previous_end) {
				if alias_target_index >= 0 && symbol.offset == previous.offset
					&& symbol_end == previous_end {
					continue
				}
				return error('AMD64 object data symbol ${index} overlaps symbol ${previous_index}')
			}
		}
	}

	for index, relocation in relocations {
		width_size := object_data_relocation_width_size(relocation.kind, relocation.width)!
		object_data_validate_relocation_intent(&relocation)!
		mut source_size := u64(0)
		mut source_section_index := -1
		if int(relocation.source_section) < int(ObjectDataSectionKind.unknown)
			|| int(relocation.source_section) > int(ObjectDataSectionKind.bss) {
			return error('AMD64 object data relocation ${index} source section kind ${int(relocation.source_section)} is invalid')
		}
		match relocation.source_section {
			.text {
				source_size = u64(o.text.len)
			}
			.rodata, .data {
				section_index := object_data_find_section(sections, relocation.source_section)
				if section_index < 0 {
					return error('AMD64 object data relocation ${index} references absent source section ${relocation.source_section}')
				}
				source_section_index = section_index
				source_size = sections[section_index].size
			}
			.bss {
				return error('AMD64 object data relocation ${index} cannot originate in .bss')
			}
			.unknown {
				return error('AMD64 object data relocation ${index} source section is missing')
			}
		}

		field_end := object_data_checked_add(relocation.offset, width_size,
			'relocation ${index} field extent')!
		if relocation.offset > source_size || field_end > source_size {
			return error('AMD64 object data relocation ${index} field exceeds source section ${relocation.source_section} size ${source_size}')
		}
		for byte_index in int(relocation.offset) .. int(field_end) {
			source_byte := if relocation.source_section == .text {
				o.text[byte_index]
			} else {
				sections[source_section_index].bytes[byte_index]
			}
			if source_byte != 0 {
				return error('AMD64 object data relocation ${index} field is not a zero placeholder')
			}
		}
		if !relocation.target_symbol.is_set {
			return error('AMD64 object data relocation ${index} target symbol is missing')
		}
		if u64(relocation.target_symbol.id) >= u64(data_symbols.len) {
			return error('AMD64 object data relocation ${index} target symbol ${u64(relocation.target_symbol.id)} is out of range')
		}
		target := data_symbols[int(relocation.target_symbol.id)]
		target_section_index := object_data_find_section(sections, target.section)
		if target_section_index < 0 {
			return error('AMD64 object data relocation ${index} target section ${target.section} is absent')
		}
		effective_target := object_data_checked_add_signed(target.offset, relocation.addend,
			'relocation ${index} effective target')!
		target_section_size := sections[target_section_index].size
		if effective_target > target_section_size {
			return error('AMD64 object data relocation ${index} effective target ${effective_target} exceeds section ${target.section} size ${target_section_size}')
		}
		if relocation.source_section == .text {
			mut owners := 0
			for function_symbol in o.symbols {
				if function_symbol.defined && !function_symbol.intentional_external {
					function_end := function_symbol.offset + function_symbol.size
					if function_symbol.offset <= relocation.offset && field_end <= function_end {
						owners++
					}
				}
			}
			if owners != 1 {
				return error('AMD64 object data relocation ${index} field is not contained in exactly one function')
			}
			for call in o.call_relocations {
				call_end := object_data_checked_add(call.offset, 4, 'CALL relocation field extent')!
				if object_ranges_overlap(relocation.offset, field_end, call.offset, call_end) {
					return error('AMD64 object data relocation ${index} overlaps a CALL relocation')
				}
			}
		}
		for previous_index in 0 .. index {
			previous := relocations[previous_index]
			if previous.source_section != relocation.source_section {
				continue
			}
			previous_size := object_data_relocation_width_size(previous.kind, previous.width)!
			previous_end := object_data_checked_add(previous.offset, previous_size,
				'relocation ${previous_index} field extent')!
			if object_ranges_overlap(relocation.offset, field_end, previous.offset, previous_end) {
				return error('AMD64 object data relocation ${index} overlaps relocation ${previous_index}')
			}
		}
	}
}

fn object_data_preflight(definition &ObjectDataDefinition, o &Object) !ObjectDataPlan {
	if !object_data_is_empty(&o.object_data) {
		return error('AMD64 object data is already installed')
	}
	object_data_validate_parts(definition.sections, definition.symbols, definition.relocations, o)!
	return object_data_clone(definition.sections, definition.symbols, definition.relocations)
}

fn (mut o Object) install_object_data(plan &ObjectDataPlan) ! {
	if !object_data_is_empty(&o.object_data) {
		return error('AMD64 object data is already installed')
	}
	object_data_validate_parts(plan.sections, plan.symbols, plan.relocations, &o)!
	o.object_data = object_data_clone(plan.sections, plan.symbols, plan.relocations)
}

fn (mut o Object) append_text(bytes []u8) !u64 {
	if bytes.len > max_int - o.text.len {
		return error('AMD64 object .text exceeds the host array limit')
	}
	offset := u64(o.text.len)
	o.text << bytes
	return offset
}

fn (mut o Object) intern_function_symbol(name string) !SymbolID {
	object_validate_symbol_name(name)!
	for symbol in o.private_data_symbols {
		if symbol.name == name {
			return error('AMD64 object function `${name}` collides with private data symbol')
		}
	}
	for symbol in o.object_data.symbols {
		if symbol.kind == .named && symbol.name == name {
			return error('AMD64 object function `${name}` collides with object data symbol')
		}
	}
	mut saw_external := false
	for index, symbol in o.symbols {
		if symbol.name == name {
			if symbol.intentional_external {
				return error('AMD64 object symbol `${name}` is both defined and external')
			}
			return SymbolID(index)
		}
		if symbol.intentional_external {
			saw_external = true
		}
	}
	if saw_external {
		return error('AMD64 object defined symbols must precede external symbols')
	}
	if u64(o.symbols.len) >= u64(max_u32) {
		return error('AMD64 object has too many function symbols')
	}
	id := SymbolID(o.symbols.len)
	o.symbols << FunctionSymbol{
		name: name
	}
	return id
}

fn (mut o Object) intern_external_function_symbol(name string) !SymbolID {
	object_validate_symbol_name(name)!
	for symbol in o.private_data_symbols {
		if symbol.name == name {
			return error('AMD64 object external function `${name}` collides with private data symbol')
		}
	}
	for symbol in o.object_data.symbols {
		if symbol.kind == .named && symbol.name == name {
			return error('AMD64 object external function `${name}` collides with object data symbol')
		}
	}
	for index, symbol in o.symbols {
		if symbol.name != name {
			continue
		}
		if !symbol.intentional_external {
			return error('AMD64 object symbol `${name}` is both defined and external')
		}
		return SymbolID(index)
	}
	if u64(o.symbols.len) >= u64(max_u32) {
		return error('AMD64 object has too many function symbols')
	}
	id := SymbolID(o.symbols.len)
	o.symbols << FunctionSymbol{
		name:                 name
		intentional_external: true
	}
	return id
}

fn (mut o Object) define_text_function(id SymbolID, offset u64, size u64) ! {
	index := object_symbol_index(&o, id)!
	symbol := o.symbols[index]
	if symbol.intentional_external {
		return error('AMD64 object external function ' + symbol.name + ' must not be defined')
	}
	if symbol.defined {
		return error('AMD64 object function ' + symbol.name + ' is already defined')
	}
	if size == 0 {
		return error('AMD64 object function ' + symbol.name + ' must not be empty')
	}
	text_size := u64(o.text.len)
	if offset > text_size || size > text_size - offset {
		return error('AMD64 object function ' + symbol.name + ' offset ' + offset.str() + ' size ' +
			size.str() + ' exceeds .text size ' + text_size.str())
	}
	end := offset + size
	for other in o.symbols {
		if !other.defined {
			continue
		}
		other_end := other.offset + other.size
		if object_ranges_overlap(offset, end, other.offset, other_end) {
			return error('AMD64 object function ' + symbol.name + ' overlaps function ' + other.name)
		}
	}
	o.symbols[index].defined = true
	o.symbols[index].offset = offset
	o.symbols[index].size = size
}

fn object_validate_call_placeholder(o &Object, offset u64) ! {
	text_size := u64(o.text.len)
	if offset == 0 || offset > text_size || u64(4) > text_size - offset {
		return error('AMD64 object CALL relocation field ' + offset.str() +
			' is outside .text size ' + text_size.str())
	}
	field_index := int(offset)
	if o.text[field_index - 1] != 0xe8 {
		return error('AMD64 object CALL relocation field ' + offset.str() + ' is not preceded by E8')
	}
	for index in field_index .. field_index + 4 {
		if o.text[index] != 0 {
			return error('AMD64 object CALL relocation field ' + offset.str() +
				' is not a zero rel32 placeholder')
		}
	}
}

fn (mut o Object) add_text_call_relocation(offset u64, id SymbolID) ! {
	_ = object_symbol_index(&o, id)!
	object_validate_call_placeholder(&o, offset)!
	field_end := offset + 4
	for relocation in o.call_relocations {
		if relocation.offset > max_u64 - 4 {
			return error('AMD64 object contains an overflowing CALL relocation field')
		}
		existing_end := relocation.offset + 4
		if object_ranges_overlap(offset, field_end, relocation.offset, existing_end) {
			return error('AMD64 object CALL relocation field ' + offset.str() +
				' overlaps an existing relocation')
		}
	}
	for relocation in o.object_data.relocations {
		if relocation.source_section != .text {
			continue
		}
		existing_size := object_data_relocation_width_size(relocation.kind, relocation.width)!
		existing_end := object_data_checked_add(relocation.offset, existing_size,
			'object data relocation field extent')!
		if object_ranges_overlap(offset, field_end, relocation.offset, existing_end) {
			return error('AMD64 object CALL relocation field ' + offset.str() +
				' overlaps an existing object data relocation')
		}
	}
	o.call_relocations << TextCallRelocation{
		offset:    offset
		symbol_id: id
	}
}

fn object_validate_function_frame(o &Object, frame &ObjectFunctionFrame) ! {
	index := object_symbol_index(o, frame.function_symbol)!
	symbol := o.symbols[index]
	if symbol.intentional_external {
		return error('AMD64 object external function ${symbol.name} must not own a frame')
	}
	if !symbol.defined {
		return error('AMD64 object function ${symbol.name} must be defined before its frame')
	}
	if frame.prologue_bytes.len == 0 || frame.epilogue_bytes.len == 0 {
		return error('AMD64 object function ${symbol.name} frame bytes must not be empty')
	}
	if frame.windows_unwind_bytes.len == 0 {
		return error('AMD64 object function ${symbol.name} Windows unwind bytes must not be empty')
	}
	required_size := u64(frame.prologue_bytes.len) + u64(frame.epilogue_bytes.len) + 1
	if required_size > symbol.size {
		return error('AMD64 object function ${symbol.name} frame bytes exceed its text extent')
	}
	start := int(symbol.offset)
	end := int(symbol.offset + symbol.size)
	prologue_end := start + frame.prologue_bytes.len
	epilogue_start := end - 1 - frame.epilogue_bytes.len
	if o.text[start..prologue_end] != frame.prologue_bytes {
		return error('AMD64 object function ${symbol.name} prologue does not match its frame')
	}
	if o.text[epilogue_start..end - 1] != frame.epilogue_bytes {
		return error('AMD64 object function ${symbol.name} epilogue does not match its frame')
	}
	if o.text[end - 1] != 0xc3 {
		return error('AMD64 object function ${symbol.name} frame is not followed by RET')
	}
}

fn (mut o Object) add_function_frame(id SymbolID, prologue_bytes []u8, epilogue_bytes []u8, windows_unwind_bytes []u8) ! {
	for frame in o.function_frames {
		if frame.function_symbol == id {
			index := object_symbol_index(&o, id)!
			return error('AMD64 object function ${o.symbols[index].name} already owns a frame')
		}
	}
	frame := ObjectFunctionFrame{
		function_symbol:      id
		prologue_bytes:       prologue_bytes.clone()
		epilogue_bytes:       epilogue_bytes.clone()
		windows_unwind_bytes: windows_unwind_bytes.clone()
	}
	object_validate_function_frame(&o, &frame)!
	o.function_frames << frame
}

fn (o &Object) validate() ! {
	o.validate_with_capabilities(false, false)!
}

fn (o &Object) validate_with_object_data() ! {
	o.validate_with_capabilities(true, false)!
}

fn (o &Object) validate_with_coff_function_frames(allow_object_data bool) ! {
	o.validate_with_capabilities(allow_object_data, true)!
}

fn (o &Object) validate_with_capabilities(allow_object_data bool, allow_function_frames bool) ! {
	private_data_validate_layout(o.private_data_symbols, o.private_data)!
	for data_symbol in o.private_data_symbols {
		for function_symbol in o.symbols {
			if data_symbol.name == function_symbol.name {
				return error('AMD64 object symbol `${data_symbol.name}` is both function and private data')
			}
		}
	}
	text_size := u64(o.text.len)
	if u64(o.symbols.len) > u64(max_u32) {
		return error('AMD64 object has too many function symbols')
	}
	mut covered_size := u64(0)
	mut saw_external := false
	for index, symbol in o.symbols {
		object_validate_symbol_name(symbol.name)!
		for previous_index in 0 .. index {
			previous := o.symbols[previous_index]
			if previous.name == symbol.name {
				return error('AMD64 object contains duplicate function symbol ' + symbol.name)
			}
		}
		if symbol.intentional_external {
			saw_external = true
			if symbol.defined {
				return error('AMD64 object external function ' + symbol.name +
					' must not be defined')
			}
			if symbol.offset != 0 || symbol.size != 0 {
				return error('AMD64 object external function ' + symbol.name +
					' must have zero offset and size')
			}
			continue
		}
		if saw_external {
			return error('AMD64 object defined symbols must precede external symbols')
		}
		if !symbol.defined {
			return error('AMD64 object function ' + symbol.name + ' is not defined')
		}
		if symbol.size == 0 {
			return error('AMD64 object function ' + symbol.name + ' must not be empty')
		}
		if symbol.offset > text_size || symbol.size > text_size - symbol.offset {
			return error('AMD64 object function ' + symbol.name + ' exceeds .text')
		}
		symbol_end := symbol.offset + symbol.size
		for previous_index in 0 .. index {
			previous := o.symbols[previous_index]
			if previous.intentional_external {
				continue
			}
			previous_end := previous.offset + previous.size
			if object_ranges_overlap(symbol.offset, symbol_end, previous.offset, previous_end) {
				return error('AMD64 object function ' + symbol.name + ' overlaps function ' +
					previous.name)
			}
		}
		if covered_size > max_u64 - symbol.size {
			return error('AMD64 object function coverage overflows u64')
		}
		covered_size += symbol.size
	}
	if covered_size != text_size {
		return error('AMD64 object function definitions cover ' + covered_size.str() +
			' bytes but .text contains ' + text_size.str())
	}
	mut external_referenced := []bool{len: o.symbols.len}
	for index, relocation in o.call_relocations {
		target_index := object_symbol_index(o, relocation.symbol_id)!
		if o.symbols[target_index].intentional_external {
			external_referenced[target_index] = true
		}
		object_validate_call_placeholder(o, relocation.offset)!
		field_end := relocation.offset + 4
		call_start := relocation.offset - 1
		mut owners := 0
		for symbol in o.symbols {
			if symbol.intentional_external {
				continue
			}
			symbol_end := symbol.offset + symbol.size
			if symbol.offset <= call_start && field_end <= symbol_end {
				owners++
			}
		}
		if owners != 1 {
			return error('AMD64 object CALL relocation field ' + relocation.offset.str() +
				' is not contained in exactly one function')
		}
		for previous_index in 0 .. index {
			previous := o.call_relocations[previous_index]
			if previous.offset > max_u64 - 4 {
				return error('AMD64 object contains an overflowing CALL relocation field')
			}
			previous_end := previous.offset + 4
			if object_ranges_overlap(relocation.offset, field_end, previous.offset, previous_end) {
				return error('AMD64 object contains overlapping CALL relocations')
			}
		}
	}
	for index, symbol in o.symbols {
		if symbol.intentional_external && !external_referenced[index] {
			return error('AMD64 object external function ' + symbol.name + ' has no CALL relocation')
		}
	}
	mut framed_symbols := map[int]bool{}
	for frame in o.function_frames {
		if int(frame.function_symbol) in framed_symbols {
			return error('AMD64 object function symbol ${frame.function_symbol} owns duplicate frames')
		}
		object_validate_function_frame(o, &frame)!
		framed_symbols[int(frame.function_symbol)] = true
	}
	object_data_validate_parts(o.object_data.sections, o.object_data.symbols,
		o.object_data.relocations, o)!
	if !allow_object_data && !object_data_is_empty(&o.object_data) {
		return error('AMD64 object data requires explicit object-format writer support')
	}
	if !allow_function_frames && o.function_frames.len != 0 {
		return error('AMD64 object function frames require explicit object-format writer support')
	}
}
