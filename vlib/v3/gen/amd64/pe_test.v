module amd64

import crypto.sha256
import os
import time

const pe64_test_dumpbin_guard = 'VTEST_AMD64_PE_CORE_DUMPBIN'
const pe64_test_dumpbin_path = 'VTEST_AMD64_PE_CORE_DUMPBIN_PATH'
const pe64_test_process_exit_llvm_guard = 'V3_PE_PROCESS_EXIT_LLVM_ORACLE'
const pe64_test_malloc_free_llvm_guard = 'V3_PE_MALLOC_FREE_LLVM_ORACLE'
const pe64_test_timeout_ms = 10_000
const pe64_test_output_limit = 64 * 1024

struct Pe64TestSection {
	name            string
	virtual_size    u32
	virtual_address u32
	raw_size        u32
	raw_pointer     u32
	reloc_pointer   u32
	reloc_count     u16
	characteristics u32
}

struct Pe64TestDirectory {
	rva  u32
	size u32
}

struct Pe64TestImport {
	dll         string
	export_name string
	iat_rva     u32
}

struct Pe64TestProcessResult {
	exit_code      int
	stdout         string
	stderr         string
	timed_out      bool
	output_limited bool
}

struct Pe64TestCapture {
mut:
	stdout string
	stderr string
}

struct Pe64TestDisassemblyInstruction {
	address  u64
	bytes    []u8
	mnemonic string
	operands string
}

fn pe64_test_read_u16(data []u8, offset int) u16 {
	assert offset >= 0
	assert offset <= data.len - 2
	return u16(data[offset]) | (u16(data[offset + 1]) << 8)
}

fn pe64_test_read_u32(data []u8, offset int) u32 {
	assert offset >= 0
	assert offset <= data.len - 4
	return u32(data[offset]) | (u32(data[offset + 1]) << 8) | (u32(data[offset + 2]) << 16) | (u32(data[
		offset + 3]) << 24)
}

fn pe64_test_read_u64(data []u8, offset int) u64 {
	return u64(pe64_test_read_u32(data, offset)) | (u64(pe64_test_read_u32(data, offset + 4)) << 32)
}

fn pe64_test_optional_offset(data []u8) int {
	pe_offset := int(pe64_test_read_u32(data, 0x3c))
	assert pe_offset >= 0x40
	assert pe_offset <= data.len - 24
	assert pe64_test_read_u32(data, pe_offset) == pe64_signature
	return pe_offset + 24
}

fn pe64_test_directory(data []u8, index int) Pe64TestDirectory {
	assert index >= 0 && index < pe64_directory_count
	offset := pe64_test_optional_offset(data) + 112 + index * 8
	return Pe64TestDirectory{
		rva:  pe64_test_read_u32(data, offset)
		size: pe64_test_read_u32(data, offset + 4)
	}
}

fn pe64_test_fixed_name(data []u8, offset int) string {
	assert offset >= 0 && offset <= data.len - 8
	mut end := offset
	for end < offset + 8 && data[end] != 0 {
		end++
	}
	return data[offset..end].bytestr()
}

fn pe64_test_sections(data []u8) []Pe64TestSection {
	pe_offset := int(pe64_test_read_u32(data, 0x3c))
	count := int(pe64_test_read_u16(data, pe_offset + 6))
	optional_size := int(pe64_test_read_u16(data, pe_offset + 20))
	section_table := pe_offset + 24 + optional_size
	assert count >= 1 && count <= int(pe64_max_sections)
	assert section_table <= data.len - count * 40
	mut sections := []Pe64TestSection{cap: count}
	for index in 0 .. count {
		offset := section_table + index * 40
		sections << Pe64TestSection{
			name:            pe64_test_fixed_name(data, offset)
			virtual_size:    pe64_test_read_u32(data, offset + 8)
			virtual_address: pe64_test_read_u32(data, offset + 12)
			raw_size:        pe64_test_read_u32(data, offset + 16)
			raw_pointer:     pe64_test_read_u32(data, offset + 20)
			reloc_pointer:   pe64_test_read_u32(data, offset + 24)
			reloc_count:     pe64_test_read_u16(data, offset + 32)
			characteristics: pe64_test_read_u32(data, offset + 36)
		}
	}
	return sections
}

fn pe64_test_section(data []u8, name string) Pe64TestSection {
	for section in pe64_test_sections(data) {
		if section.name == name {
			return section
		}
	}
	assert false, 'PE section `${name}` was not emitted'
	return Pe64TestSection{}
}

fn pe64_test_rva_offset(data []u8, rva u32) int {
	for section in pe64_test_sections(data) {
		if rva < section.virtual_address {
			continue
		}
		delta := u64(rva) - u64(section.virtual_address)
		if delta < u64(section.virtual_size) {
			assert delta < u64(section.raw_size)
			offset := u64(section.raw_pointer) + delta
			assert offset < u64(data.len)
			return int(offset)
		}
	}
	assert false, 'RVA 0x${rva:x} is not contained in an emitted section'
	return -1
}

fn pe64_test_imports(data []u8) []Pe64TestImport {
	directory := pe64_test_directory(data, pe64_import_directory_index)
	if directory.rva == 0 {
		assert directory.size == 0
		return []Pe64TestImport{}
	}
	assert directory.size >= u32(pe64_import_descriptor_size * 2)
	mut imports := []Pe64TestImport{}
	mut descriptor_rva := directory.rva
	mut descriptor_count := 0
	for {
		descriptor_offset := pe64_test_rva_offset(data, descriptor_rva)
		ilt_rva := pe64_test_read_u32(data, descriptor_offset)
		name_rva := pe64_test_read_u32(data, descriptor_offset + 12)
		iat_rva := pe64_test_read_u32(data, descriptor_offset + 16)
		if ilt_rva == 0 && name_rva == 0 && iat_rva == 0 {
			break
		}
		assert ilt_rva != 0 && name_rva != 0 && iat_rva != 0
		name_offset := pe64_test_rva_offset(data, name_rva)
		dll := pe64_test_cstring(data, name_offset, data.len)
		mut local_index := 0
		for {
			ilt_offset := pe64_test_rva_offset(data, ilt_rva + u32(local_index * 8))
			hint_name_rva := pe64_test_read_u64(data, ilt_offset)
			if hint_name_rva == 0 {
				break
			}
			assert hint_name_rva <= u64(max_u32)
			hint_name_offset := pe64_test_rva_offset(data, u32(hint_name_rva))
			export_name := pe64_test_cstring(data, hint_name_offset + 2, data.len)
			imports << Pe64TestImport{
				dll:         dll
				export_name: export_name
				iat_rva:     iat_rva + u32(local_index * 8)
			}
			local_index++
		}
		assert local_index > 0
		descriptor_count++
		descriptor_rva += u32(pe64_import_descriptor_size)
	}
	assert directory.size == u32((descriptor_count + 1) * int(pe64_import_descriptor_size))
	return imports
}

fn pe64_test_cstring(data []u8, offset int, limit int) string {
	assert offset >= 0 && offset < limit && limit <= data.len
	mut end := offset
	for end < limit && data[end] != 0 {
		end++
	}
	assert end < limit
	return data[offset..end].bytestr()
}

fn pe64_test_assert_zero_range(data []u8, start int, end int) {
	assert start >= 0 && start <= end && end <= data.len
	for byte in data[start..end] {
		assert byte == 0
	}
}

fn pe64_test_definition(entry SymbolID, imports []Pe64ImportBinding) Pe64ImageDefinition {
	return Pe64ImageDefinition{
		target_abi:   .windows_x64_microsoft
		subsystem:    .windows_cui
		image_policy: .fixed_base
		entry:        Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         .raw_noreturn_process_entry
		}
		imports:      imports
	}
}

fn pe64_test_error(o &Object, definition Pe64ImageDefinition) string {
	if _ := pe64_image_bytes(o, definition) {
		assert false, 'PE64 serialization unexpectedly succeeded'
	} else {
		return err.msg()
	}
	return ''
}

fn pe64_test_leaf_object(name string) (Object, SymbolID) {
	mut object := Object.new()
	entry := object.intern_function_symbol(name) or { panic(err) }
	assert object.append_text([u8(0xeb), 0xfe]) or { panic(err) } == 0
	object.define_text_function(entry, 0, 2) or { panic(err) }
	return object, entry
}

fn pe64_test_nonleaf_body() []u8 {
	return [
		u8(0x48),
		0x83,
		0xec,
		0x28,
		0xe8,
		0,
		0,
		0,
		0,
		0x48,
		0x83,
		0xc4,
		0x28,
		0xeb,
		0xfe,
	]
}

fn pe64_test_runtime_definition(entry SymbolID, imports []Pe64ImportBinding, runtime_helpers []Pe64RuntimeBinding) Pe64ImageDefinition {
	return Pe64ImageDefinition{
		target_abi:      .windows_x64_microsoft
		subsystem:       .windows_cui
		image_policy:    .fixed_base
		entry:           Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         .raw_noreturn_process_entry
		}
		imports:         imports
		runtime_helpers: runtime_helpers
	}
}

fn pe64_test_process_exit_definition(entry SymbolID, imports []Pe64ImportBinding,
	runtime_helpers []Pe64RuntimeBinding,
	runtime_imports []Pe64RuntimeImportBinding) Pe64ImageDefinition {
	return Pe64ImageDefinition{
		target_abi:      .windows_x64_microsoft
		subsystem:       .windows_cui
		image_policy:    .fixed_base
		entry:           Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         .raw_noreturn_process_entry
		}
		imports:         imports
		runtime_helpers: runtime_helpers
		runtime_imports: runtime_imports
	}
}

fn pe64_test_process_exit_import() Pe64RuntimeImportBinding {
	return Pe64RuntimeImportBinding{
		helper:      .process_exit
		dll:         pe64_runtime_process_exit_dll
		export_name: pe64_runtime_process_exit_export
	}
}

fn pe64_test_runtime_fixture(entry_name string, external_name string) (Object, SymbolID, SymbolID) {
	mut object := Object.new()
	entry := object.intern_function_symbol(entry_name) or { panic(err) }
	external := object.intern_external_function_symbol(external_name) or { panic(err) }
	body := pe64_test_nonleaf_body()
	assert object.append_text(body) or { panic(err) } == 0
	object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
	object.add_text_call_relocation(5, external) or { panic(err) }
	return object, entry, external
}

fn pe64_test_runtime_order_fixture(reverse_relocations bool) (Object, SymbolID, SymbolID, SymbolID, SymbolID, SymbolID, SymbolID) {
	mut object := Object.new()
	entry := object.intern_function_symbol('runtime_order_entry') or { panic(err) }
	strlen_a := object.intern_external_function_symbol('runtime_order_strlen_a') or { panic(err) }
	strlen_b := object.intern_external_function_symbol('runtime_order_strlen_b') or { panic(err) }
	wcslen_a := object.intern_external_function_symbol('runtime_order_wcslen_a') or { panic(err) }
	wcslen_b := object.intern_external_function_symbol('runtime_order_wcslen_b') or { panic(err) }
	imported := object.intern_external_function_symbol('runtime_order_import') or { panic(err) }
	body := [
		u8(0x48),
		0x83,
		0xec,
		0x28,
		0xe8,
		0,
		0,
		0,
		0,
		0xe8,
		0,
		0,
		0,
		0,
		0xe8,
		0,
		0,
		0,
		0,
		0xe8,
		0,
		0,
		0,
		0,
		0xe8,
		0,
		0,
		0,
		0,
		0x48,
		0x83,
		0xc4,
		0x28,
		0xeb,
		0xfe,
	]
	assert object.append_text(body) or { panic(err) } == 0
	object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
	if reverse_relocations {
		object.add_text_call_relocation(25, imported) or { panic(err) }
		object.add_text_call_relocation(20, wcslen_b) or { panic(err) }
		object.add_text_call_relocation(15, wcslen_a) or { panic(err) }
		object.add_text_call_relocation(10, strlen_b) or { panic(err) }
		object.add_text_call_relocation(5, strlen_a) or { panic(err) }
	} else {
		object.add_text_call_relocation(5, strlen_a) or { panic(err) }
		object.add_text_call_relocation(10, strlen_b) or { panic(err) }
		object.add_text_call_relocation(15, wcslen_a) or { panic(err) }
		object.add_text_call_relocation(20, wcslen_b) or { panic(err) }
		object.add_text_call_relocation(25, imported) or { panic(err) }
	}
	return object, entry, strlen_a, strlen_b, wcslen_a, wcslen_b, imported
}

fn pe64_test_runtime_memset_order_fixture(reverse_relocations bool) (Object, SymbolID, SymbolID, SymbolID, SymbolID, SymbolID, SymbolID, SymbolID, SymbolID) {
	mut object := Object.new()
	entry := object.intern_function_symbol('runtime_memset_order_entry') or { panic(err) }
	strlen_a := object.intern_external_function_symbol('runtime_memset_order_strlen_a') or {
		panic(err)
	}
	strlen_b := object.intern_external_function_symbol('runtime_memset_order_strlen_b') or {
		panic(err)
	}
	wcslen_a := object.intern_external_function_symbol('runtime_memset_order_wcslen_a') or {
		panic(err)
	}
	wcslen_b := object.intern_external_function_symbol('runtime_memset_order_wcslen_b') or {
		panic(err)
	}
	memset_a := object.intern_external_function_symbol('runtime_memset_order_memset_a') or {
		panic(err)
	}
	memset_b := object.intern_external_function_symbol('runtime_memset_order_memset_b') or {
		panic(err)
	}
	imported := object.intern_external_function_symbol('runtime_memset_order_import') or {
		panic(err)
	}
	body := [
		u8(0x48),
		0x83,
		0xec,
		0x28,
		0xe8,
		0,
		0,
		0,
		0,
		0xe8,
		0,
		0,
		0,
		0,
		0xe8,
		0,
		0,
		0,
		0,
		0xe8,
		0,
		0,
		0,
		0,
		0xe8,
		0,
		0,
		0,
		0,
		0xe8,
		0,
		0,
		0,
		0,
		0xe8,
		0,
		0,
		0,
		0,
		0x48,
		0x83,
		0xc4,
		0x28,
		0xeb,
		0xfe,
	]
	assert object.append_text(body) or { panic(err) } == 0
	object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
	if reverse_relocations {
		object.add_text_call_relocation(35, imported) or { panic(err) }
		object.add_text_call_relocation(30, memset_b) or { panic(err) }
		object.add_text_call_relocation(25, memset_a) or { panic(err) }
		object.add_text_call_relocation(20, wcslen_b) or { panic(err) }
		object.add_text_call_relocation(15, wcslen_a) or { panic(err) }
		object.add_text_call_relocation(10, strlen_b) or { panic(err) }
		object.add_text_call_relocation(5, strlen_a) or { panic(err) }
	} else {
		object.add_text_call_relocation(5, strlen_a) or { panic(err) }
		object.add_text_call_relocation(10, strlen_b) or { panic(err) }
		object.add_text_call_relocation(15, wcslen_a) or { panic(err) }
		object.add_text_call_relocation(20, wcslen_b) or { panic(err) }
		object.add_text_call_relocation(25, memset_a) or { panic(err) }
		object.add_text_call_relocation(30, memset_b) or { panic(err) }
		object.add_text_call_relocation(35, imported) or { panic(err) }
	}
	return object, entry, strlen_a, strlen_b, wcslen_a, wcslen_b, memset_a, memset_b, imported
}

fn pe64_test_runtime_memcmp_order_fixture(reverse_relocations bool) (Object, SymbolID, SymbolID, SymbolID, SymbolID, SymbolID, SymbolID, SymbolID) {
	mut object := Object.new()
	entry := object.intern_function_symbol('runtime_memcmp_order_entry') or { panic(err) }
	strlen := object.intern_external_function_symbol('runtime_memcmp_order_strlen') or {
		panic(err)
	}
	wcslen := object.intern_external_function_symbol('runtime_memcmp_order_wcslen') or {
		panic(err)
	}
	memset := object.intern_external_function_symbol('runtime_memcmp_order_memset') or {
		panic(err)
	}
	memcmp_a := object.intern_external_function_symbol('runtime_memcmp_order_memcmp_a') or {
		panic(err)
	}
	memcmp_b := object.intern_external_function_symbol('runtime_memcmp_order_memcmp_b') or {
		panic(err)
	}
	imported := object.intern_external_function_symbol('runtime_memcmp_order_import') or {
		panic(err)
	}
	mut body := [u8(0x48), 0x83, 0xec, 0x28]
	for _ in 0 .. 6 {
		body << [u8(0xe8), 0, 0, 0, 0]
	}
	body << [u8(0x48), 0x83, 0xc4, 0x28, 0xeb, 0xfe]
	assert object.append_text(body) or { panic(err) } == 0
	object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
	if reverse_relocations {
		object.add_text_call_relocation(30, imported) or { panic(err) }
		object.add_text_call_relocation(25, memcmp_b) or { panic(err) }
		object.add_text_call_relocation(20, memcmp_a) or { panic(err) }
		object.add_text_call_relocation(15, memset) or { panic(err) }
		object.add_text_call_relocation(10, wcslen) or { panic(err) }
		object.add_text_call_relocation(5, strlen) or { panic(err) }
	} else {
		object.add_text_call_relocation(5, strlen) or { panic(err) }
		object.add_text_call_relocation(10, wcslen) or { panic(err) }
		object.add_text_call_relocation(15, memset) or { panic(err) }
		object.add_text_call_relocation(20, memcmp_a) or { panic(err) }
		object.add_text_call_relocation(25, memcmp_b) or { panic(err) }
		object.add_text_call_relocation(30, imported) or { panic(err) }
	}
	return object, entry, strlen, wcslen, memset, memcmp_a, memcmp_b, imported
}

struct Pe64TestMoveOrderFixture {
	object    Object
	entry     SymbolID
	strlen    SymbolID
	wcslen    SymbolID
	memset    SymbolID
	memcmp    SymbolID
	memmove_a SymbolID
	memmove_b SymbolID
	memcpy_a  SymbolID
	memcpy_b  SymbolID
	imported  SymbolID
}

struct Pe64TestProcessExitAliasFixture {
	object   Object
	entry    SymbolID
	exit_a   SymbolID
	exit_b   SymbolID
	imported SymbolID
}

fn pe64_test_process_exit_alias_fixture(reverse_relocations bool) Pe64TestProcessExitAliasFixture {
	mut object := Object.new()
	entry := object.intern_function_symbol('process_exit_alias_entry') or { panic(err) }
	exit_a := object.intern_external_function_symbol('process_exit_alias_a') or { panic(err) }
	exit_b := object.intern_external_function_symbol('process_exit_alias_b') or { panic(err) }
	imported := object.intern_external_function_symbol('ordinary_exit_process') or { panic(err) }
	mut body := [u8(0x48), 0x83, 0xec, 0x28]
	for _ in 0 .. 3 {
		body << [u8(0xe8), 0, 0, 0, 0]
	}
	body << [u8(0x48), 0x83, 0xc4, 0x28, 0xeb, 0xfe]
	assert object.append_text(body) or { panic(err) } == 0
	object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
	relocations := [
		TextCallRelocation{
			offset:    5
			symbol_id: exit_a
		},
		TextCallRelocation{
			offset:    10
			symbol_id: exit_b
		},
		TextCallRelocation{
			offset:    15
			symbol_id: imported
		},
	]
	if reverse_relocations {
		for index := relocations.len - 1; index >= 0; index-- {
			relocation := relocations[index]
			object.add_text_call_relocation(relocation.offset, relocation.symbol_id) or {
				panic(err)
			}
		}
	} else {
		for relocation in relocations {
			object.add_text_call_relocation(relocation.offset, relocation.symbol_id) or {
				panic(err)
			}
		}
	}
	return Pe64TestProcessExitAliasFixture{
		object:   object
		entry:    entry
		exit_a:   exit_a
		exit_b:   exit_b
		imported: imported
	}
}

fn pe64_test_runtime_move_order_fixture(reverse_relocations bool) Pe64TestMoveOrderFixture {
	mut object := Object.new()
	entry := object.intern_function_symbol('runtime_move_order_entry') or { panic(err) }
	strlen := object.intern_external_function_symbol('runtime_move_order_strlen') or { panic(err) }
	wcslen := object.intern_external_function_symbol('runtime_move_order_wcslen') or { panic(err) }
	memset := object.intern_external_function_symbol('runtime_move_order_memset') or { panic(err) }
	memcmp := object.intern_external_function_symbol('runtime_move_order_memcmp') or { panic(err) }
	memmove_a := object.intern_external_function_symbol('runtime_move_order_a') or { panic(err) }
	memmove_b := object.intern_external_function_symbol('runtime_move_order_b') or { panic(err) }
	memcpy_a := object.intern_external_function_symbol('runtime_copy_order_a') or { panic(err) }
	memcpy_b := object.intern_external_function_symbol('runtime_copy_order_b') or { panic(err) }
	imported := object.intern_external_function_symbol('runtime_move_order_import') or {
		panic(err)
	}
	mut body := [u8(0x48), 0x83, 0xec, 0x28]
	for _ in 0 .. 9 {
		body << [u8(0xe8), 0, 0, 0, 0]
	}
	body << [u8(0x48), 0x83, 0xc4, 0x28, 0xeb, 0xfe]
	assert object.append_text(body) or { panic(err) } == 0
	object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
	relocations := [
		TextCallRelocation{
			offset:    5
			symbol_id: strlen
		},
		TextCallRelocation{
			offset:    10
			symbol_id: wcslen
		},
		TextCallRelocation{
			offset:    15
			symbol_id: memset
		},
		TextCallRelocation{
			offset:    20
			symbol_id: memcmp
		},
		TextCallRelocation{
			offset:    25
			symbol_id: memmove_a
		},
		TextCallRelocation{
			offset:    30
			symbol_id: memmove_b
		},
		TextCallRelocation{
			offset:    35
			symbol_id: memcpy_a
		},
		TextCallRelocation{
			offset:    40
			symbol_id: memcpy_b
		},
		TextCallRelocation{
			offset:    45
			symbol_id: imported
		},
	]
	if reverse_relocations {
		for index := relocations.len - 1; index >= 0; index-- {
			relocation := relocations[index]
			object.add_text_call_relocation(relocation.offset, relocation.symbol_id) or {
				panic(err)
			}
		}
	} else {
		for relocation in relocations {
			object.add_text_call_relocation(relocation.offset, relocation.symbol_id) or {
				panic(err)
			}
		}
	}
	return Pe64TestMoveOrderFixture{
		object:    object
		entry:     entry
		strlen:    strlen
		wcslen:    wcslen
		memset:    memset
		memcmp:    memcmp
		memmove_a: memmove_a
		memmove_b: memmove_b
		memcpy_a:  memcpy_a
		memcpy_b:  memcpy_b
		imported:  imported
	}
}

fn pe64_test_clone_object(o &Object) Object {
	mut function_frames := []ObjectFunctionFrame{cap: o.function_frames.len}
	for frame in o.function_frames {
		function_frames << ObjectFunctionFrame{
			function_symbol:      frame.function_symbol
			prologue_bytes:       frame.prologue_bytes.clone()
			epilogue_bytes:       frame.epilogue_bytes.clone()
			windows_unwind_bytes: frame.windows_unwind_bytes.clone()
		}
	}
	return Object{
		text:                 o.text.clone()
		symbols:              o.symbols.clone()
		call_relocations:     o.call_relocations.clone()
		function_frames:      function_frames
		private_data:         o.private_data.clone()
		private_data_symbols: o.private_data_symbols.clone()
		object_data:          object_data_clone(o.object_data.sections, o.object_data.symbols,
			o.object_data.relocations)
	}
}

fn pe64_test_clone_image_definition(definition &Pe64ImageDefinition) Pe64ImageDefinition {
	mut imports := []Pe64ImportBinding{cap: definition.imports.len}
	for item in definition.imports {
		imports << Pe64ImportBinding{
			symbol_id:   item.symbol_id
			dll:         item.dll.clone()
			export_name: item.export_name.clone()
		}
	}
	mut runtime_helpers := []Pe64RuntimeBinding{cap: definition.runtime_helpers.len}
	for item in definition.runtime_helpers {
		runtime_helpers << Pe64RuntimeBinding{
			symbol_id: item.symbol_id
			helper:    item.helper
		}
	}
	mut runtime_imports := []Pe64RuntimeImportBinding{cap: definition.runtime_imports.len}
	for item in definition.runtime_imports {
		runtime_imports << Pe64RuntimeImportBinding{
			helper:      item.helper
			dll:         item.dll.clone()
			export_name: item.export_name.clone()
		}
	}
	return Pe64ImageDefinition{
		target_abi:      definition.target_abi
		subsystem:       definition.subsystem
		image_policy:    definition.image_policy
		entry:           definition.entry
		imports:         imports
		runtime_helpers: runtime_helpers
		runtime_imports: runtime_imports
	}
}

fn pe64_test_assert_object_snapshot(actual &Object, expected &Object) {
	assert actual.text == expected.text
	assert actual.symbols == expected.symbols
	assert actual.call_relocations == expected.call_relocations
	assert actual.function_frames == expected.function_frames
	assert actual.private_data == expected.private_data
	assert actual.private_data_symbols == expected.private_data_symbols
	assert actual.object_data.sections == expected.object_data.sections
	assert actual.object_data.symbols == expected.object_data.symbols
	assert actual.object_data.relocations == expected.object_data.relocations
}

fn pe64_test_assert_image_definition_snapshot(actual &Pe64ImageDefinition,
	expected &Pe64ImageDefinition) {
	assert actual.target_abi == expected.target_abi
	assert actual.subsystem == expected.subsystem
	assert actual.image_policy == expected.image_policy
	assert actual.entry == expected.entry
	assert actual.imports == expected.imports
	assert actual.runtime_helpers == expected.runtime_helpers
	assert actual.runtime_imports == expected.runtime_imports
}

fn pe64_test_malloc_free_image_bytes(o &Object, definition Pe64ImageDefinition) ![]u8 {
	object_before := pe64_test_clone_object(o)
	definition_before := pe64_test_clone_image_definition(&definition)
	bytes := pe64_image_bytes(o, definition) or {
		pe64_test_assert_object_snapshot(o, &object_before)
		pe64_test_assert_image_definition_snapshot(&definition, &definition_before)
		return err
	}
	pe64_test_assert_object_snapshot(o, &object_before)
	pe64_test_assert_image_definition_snapshot(&definition, &definition_before)
	return bytes
}

fn pe64_test_malloc_free_publish_error(o &Object, definition Pe64ImageDefinition,
	final_path string) string {
	object_before := pe64_test_clone_object(o)
	definition_before := pe64_test_clone_image_definition(&definition)
	message := pe64_test_build_then_publish_error(o, definition, final_path)
	pe64_test_assert_object_snapshot(o, &object_before)
	pe64_test_assert_image_definition_snapshot(&definition, &definition_before)
	return message
}

fn pe64_test_runtime_strlen_body() []u8 {
	return [u8(0x48), 0x89, 0xc8, 0x80, 0x38, 0x00, 0x74, 0x05, 0x48, 0xff, 0xc0, 0xeb, 0xf6, 0x48,
		0x29, 0xc8, 0xc3]
}

fn pe64_test_runtime_wcslen_body() []u8 {
	return [u8(0x48), 0x89, 0xc8, 0x66, 0x83, 0x38, 0x00, 0x74, 0x06, 0x48, 0x83, 0xc0, 0x02, 0xeb,
		0xf4, 0x48, 0x29, 0xc8, 0x48, 0xd1, 0xe8, 0xc3]
}

fn pe64_test_runtime_memset_body() []u8 {
	return [u8(0x48), 0x89, 0xc8, 0x4d, 0x85, 0xc0, 0x74, 0x0e, 0x49, 0x89, 0xca, 0x41, 0x88, 0x12,
		0x49, 0xff, 0xc2, 0x49, 0xff, 0xc8, 0x75, 0xf5, 0xc3]
}

fn pe64_test_runtime_memcmp_body() []u8 {
	return [u8(0x4d), 0x85, 0xc0, 0x74, 0x16, 0x44, 0x8a, 0x09, 0x44, 0x8a, 0x12, 0x45, 0x38, 0xd1,
		0x75, 0x0e, 0x48, 0xff, 0xc1, 0x48, 0xff, 0xc2, 0x49, 0xff, 0xc8, 0x75, 0xea, 0x29, 0xc0,
		0xc3, 0x41, 0x0f, 0xb6, 0xc1, 0x45, 0x0f, 0xb6, 0xd2, 0x44, 0x29, 0xd0, 0xc3]
}

fn pe64_test_runtime_move_body() []u8 {
	return [u8(0x48), 0x89, 0xc8, 0x4d, 0x85, 0xc0, 0x74, 0x41, 0x48, 0x39, 0xd1, 0x76, 0x25, 0x4e,
		0x8d, 0x0c, 0x02, 0x4c, 0x39, 0xc9, 0x73, 0x1c, 0x4e, 0x8d, 0x54, 0x01, 0xff, 0x4e, 0x8d,
		0x5c, 0x02, 0xff, 0x45, 0x8a, 0x0b, 0x45, 0x88, 0x0a, 0x49, 0xff, 0xca, 0x49, 0xff, 0xcb,
		0x49, 0xff, 0xc8, 0x75, 0xef, 0xc3, 0x49, 0x89, 0xca, 0x49, 0x89, 0xd3, 0x45, 0x8a, 0x0b,
		0x45, 0x88, 0x0a, 0x49, 0xff, 0xc2, 0x49, 0xff, 0xc3, 0x49, 0xff, 0xc8, 0x75, 0xef, 0xc3]
}

fn pe64_test_assert_runtime_process_exit_body(body []u8, displacement u32) {
	assert body.len == int(pe64_runtime_process_exit_size)
	assert body[0..5] == [u8(0x48), 0x83, 0xec, 0x28, 0xe8]
	assert pe64_test_read_u32(body, 5) == displacement
	assert body[9] == 0xcc
	assert body[0] == 0x48 && body[1] == 0x83
	sub_mode, sub_opcode, sub_rm := pe64_test_modrm_fields(body[2])
	assert sub_mode == 3 && sub_opcode == 5 && sub_rm == 4
	assert body[3] == 40
	assert body[4] == 0xe8
	assert !body.contains(u8(0xc3))
}

struct Pe64TestMemsetAbiState {
mut:
	rax                  u64
	rcx                  u64
	rdx                  u64
	r8                   u64
	r10                  u64
	rsp                  u64
	nonvolatile_gprs     []u64
	nonvolatile_vectors  []u64
	df                   bool
	status_flags_written bool
	memory               []u8
}

struct Pe64TestMemcmpAbiState {
mut:
	eax                  i32
	rax_written          bool
	rcx                  u64
	rdx                  u64
	r8                   u64
	r9                   u64
	r10                  u64
	r11                  u64
	rsp                  u64
	nonvolatile_gprs     []u64
	nonvolatile_vectors  []u64
	df                   bool
	status_flags_written bool
	memory               []u8
	stack                []u8
	read_offsets         []u64
}

struct Pe64TestMoveAbiState {
mut:
	rax                  u64
	rcx                  u64
	rdx                  u64
	r8                   u64
	r9                   u64
	r10                  u64
	r11                  u64
	rsp                  u64
	nonvolatile_gprs     []u64
	nonvolatile_vectors  []u64
	df                   bool
	status_flags_written bool
	memory               []u8
	stack                []u8
	read_offsets         []u64
	write_offsets        []u64
}

fn pe64_test_modrm_fields(value u8) (u8, u8, u8) {
	return value >> 6, (value >> 3) & 7, value & 7
}

fn pe64_test_sib_fields(value u8) (u8, u8, u8) {
	return value >> 6, (value >> 3) & 7, value & 7
}

struct Pe64TestModrmOracle {
	offset int
	mode   u8
	reg    u8
	rm     u8
}

fn pe64_test_rel8_target(next_offset int, encoded u8) int {
	if encoded < 0x80 {
		return next_offset + int(encoded)
	}
	return next_offset + int(encoded) - 0x100
}

fn pe64_test_hex_nibble(value u8) int {
	if value >= `0` && value <= `9` {
		return int(value - `0`)
	}
	if value >= `a` && value <= `f` {
		return int(value - `a`) + 10
	}
	if value >= `A` && value <= `F` {
		return int(value - `A`) + 10
	}
	return -1
}

fn pe64_test_parse_hex_u64(value string) (bool, u64) {
	if value.len == 0 || value.len > 16 {
		return false, 0
	}
	mut parsed := u64(0)
	for byte in value.bytes() {
		nibble := pe64_test_hex_nibble(byte)
		if nibble < 0 {
			return false, 0
		}
		parsed = (parsed << 4) | u64(nibble)
	}
	return true, parsed
}

fn pe64_test_parse_hex_byte(value string) (bool, u8) {
	if value.len != 2 {
		return false, 0
	}
	high := pe64_test_hex_nibble(value[0])
	low := pe64_test_hex_nibble(value[1])
	if high < 0 || low < 0 {
		return false, 0
	}
	return true, u8(high * 16 + low)
}

fn pe64_test_parse_dumpbin_instruction(line string) (bool, Pe64TestDisassemblyInstruction) {
	fields := line.fields()
	if fields.len < 3 || fields[0].len < 2 || fields[0][fields[0].len - 1] != `:` {
		return false, Pe64TestDisassemblyInstruction{}
	}
	valid_address, address := pe64_test_parse_hex_u64(fields[0][..fields[0].len - 1])
	if !valid_address {
		return false, Pe64TestDisassemblyInstruction{}
	}
	mut bytes := []u8{}
	mut cursor := 1
	for cursor < fields.len {
		valid_byte, byte := pe64_test_parse_hex_byte(fields[cursor])
		if !valid_byte {
			break
		}
		bytes << byte
		cursor++
	}
	if bytes.len == 0 || cursor >= fields.len {
		return false, Pe64TestDisassemblyInstruction{}
	}
	mut operands := ''
	if cursor + 1 < fields.len {
		operands = fields[cursor + 1..].join(' ').to_lower()
	}
	return true, Pe64TestDisassemblyInstruction{
		address:  address
		bytes:    bytes
		mnemonic: fields[cursor].to_lower()
		operands: operands
	}
}

fn pe64_test_dumpbin_instructions(output string) []Pe64TestDisassemblyInstruction {
	mut instructions := []Pe64TestDisassemblyInstruction{}
	for line in output.split_into_lines() {
		valid, instruction := pe64_test_parse_dumpbin_instruction(line)
		if valid {
			instructions << instruction
		}
	}
	return instructions
}

struct Pe64TestDumpbinImportRecord {
	dll         string
	export_name string
}

struct Pe64TestDumpbinRuntimeFunction {
	begin  u64
	end    u64
	unwind u64
}

fn pe64_test_dumpbin_raw_bytes(output string) []u8 {
	mut result := []u8{}
	mut next_address := u64(0)
	mut found := false
	for line in output.split_into_lines() {
		fields := line.fields()
		if fields.len < 2 || fields[0].len < 2 || fields[0][fields[0].len - 1] != `:` {
			continue
		}
		valid_address, address := pe64_test_parse_hex_u64(fields[0][..fields[0].len - 1])
		if !valid_address {
			continue
		}
		mut line_bytes := []u8{}
		for field in fields[1..] {
			valid_byte, byte := pe64_test_parse_hex_byte(field)
			if !valid_byte {
				break
			}
			line_bytes << byte
		}
		if line_bytes.len == 0 {
			continue
		}
		if found {
			assert address == next_address, 'DUMPBIN raw-data offsets are not contiguous'
		}
		result << line_bytes
		next_address = address + u64(line_bytes.len)
		found = true
	}
	assert found, 'DUMPBIN raw-data output contained no byte rows'
	return result
}

fn pe64_test_dumpbin_import_records(output string) ([]string, []Pe64TestDumpbinImportRecord) {
	mut dlls := []string{}
	mut records := []Pe64TestDumpbinImportRecord{}
	mut current_dll := ''
	for line in output.split_into_lines() {
		fields := line.fields()
		if fields.len == 1 && fields[0].to_lower().ends_with('.dll') {
			current_dll = fields[0]
			dlls << current_dll
			continue
		}
		if current_dll.len == 0 || fields.len != 2 || fields[1].starts_with('.') {
			continue
		}
		valid_hint, _ := pe64_test_parse_hex_u64(fields[0])
		if valid_hint {
			records << Pe64TestDumpbinImportRecord{
				dll:         current_dll
				export_name: fields[1]
			}
		}
	}
	return dlls, records
}

fn pe64_test_dumpbin_runtime_functions(output string) []Pe64TestDumpbinRuntimeFunction {
	mut rows := []Pe64TestDumpbinRuntimeFunction{}
	for line in output.split_into_lines() {
		fields := line.fields()
		if fields.len != 4 {
			continue
		}
		valid_table, _ := pe64_test_parse_hex_u64(fields[0])
		valid_begin, begin := pe64_test_parse_hex_u64(fields[1])
		valid_end, end := pe64_test_parse_hex_u64(fields[2])
		valid_unwind, unwind := pe64_test_parse_hex_u64(fields[3])
		if valid_table && valid_begin && valid_end && valid_unwind {
			rows << Pe64TestDumpbinRuntimeFunction{
				begin:  begin
				end:    end
				unwind: unwind
			}
		}
	}
	return rows
}

fn pe64_test_dumpbin_image_rva(value u64) u64 {
	if value >= pe64_image_base {
		return value - pe64_image_base
	}
	return value
}

fn pe64_test_compact_operands(operands string) string {
	return operands.replace(' ', '').replace('\t', '')
}

fn pe64_test_dumpbin_operand_has_address(operands string, expected u64) bool {
	for raw_token in operands.fields() {
		mut token := raw_token.trim(' ,[]()')
		if token.starts_with('0x') {
			token = token[2..]
		}
		if token.ends_with('h') {
			token = token[..token.len - 1]
		}
		valid, parsed := pe64_test_parse_hex_u64(token)
		if valid && parsed == expected {
			return true
		}
	}
	return false
}

fn pe64_test_assert_runtime_strlen_body(body []u8) {
	expected := pe64_test_runtime_strlen_body()
	assert body.len == int(pe64_runtime_strlen_size)
	assert body == expected
	assert body[0] == 0x48
	assert body[0] >> 4 == 4
	assert (body[0] >> 3) & 1 == 1
	assert body[0] & 7 == 0
	mov_mode, mov_reg, mov_rm := pe64_test_modrm_fields(body[2])
	assert mov_mode == 3 && mov_reg == 1 && mov_rm == 0
	cmp_mode, cmp_opcode, cmp_rm := pe64_test_modrm_fields(body[4])
	assert cmp_mode == 0 && cmp_opcode == 7 && cmp_rm == 0
	assert body[5] == 0
	assert body[6] == 0x74 && body[7] == 5
	assert pe64_test_rel8_target(8, body[7]) == 13
	assert body[8] == 0x48
	inc_mode, inc_opcode, inc_rm := pe64_test_modrm_fields(body[10])
	assert inc_mode == 3 && inc_opcode == 0 && inc_rm == 0
	assert body[11] == 0xeb && body[12] == 0xf6
	assert pe64_test_rel8_target(13, body[12]) == 3
	assert body[13] == 0x48
	sub_mode, sub_reg, sub_rm := pe64_test_modrm_fields(body[15])
	assert sub_mode == 3 && sub_reg == 1 && sub_rm == 0
	assert body[16] == 0xc3
}

fn pe64_test_assert_runtime_wcslen_body(body []u8) {
	expected := pe64_test_runtime_wcslen_body()
	assert body.len == int(pe64_runtime_wcslen_size)
	assert body == expected
	assert body[0] == 0x48
	mov_mode, mov_reg, mov_rm := pe64_test_modrm_fields(body[2])
	assert mov_mode == 3 && mov_reg == 1 && mov_rm == 0
	assert body[3] == 0x66 && body[4] == 0x83
	cmp_mode, cmp_opcode, cmp_rm := pe64_test_modrm_fields(body[5])
	assert cmp_mode == 0 && cmp_opcode == 7 && cmp_rm == 0
	assert body[6] == 0
	assert body[7] == 0x74 && body[8] == 0x06
	assert pe64_test_rel8_target(9, body[8]) == 15
	assert body[9] == 0x48 && body[10] == 0x83
	add_mode, add_opcode, add_rm := pe64_test_modrm_fields(body[11])
	assert add_mode == 3 && add_opcode == 0 && add_rm == 0
	assert body[12] == 2
	assert body[13] == 0xeb && body[14] == 0xf4
	assert pe64_test_rel8_target(15, body[14]) == 3
	assert body[15] == 0x48 && body[16] == 0x29
	sub_mode, sub_reg, sub_rm := pe64_test_modrm_fields(body[17])
	assert sub_mode == 3 && sub_reg == 1 && sub_rm == 0
	assert body[18] == 0x48 && body[19] == 0xd1
	shr_mode, shr_opcode, shr_rm := pe64_test_modrm_fields(body[20])
	assert shr_mode == 3 && shr_opcode == 5 && shr_rm == 0
	assert body[21] == 0xc3
}

fn pe64_test_assert_runtime_memset_body(body []u8) {
	expected := pe64_test_runtime_memset_body()
	assert body.len == int(pe64_runtime_memset_size)
	assert body == expected
	assert body[0] == 0x48 && body[1] == 0x89
	mov_return_mode, mov_return_reg, mov_return_rm := pe64_test_modrm_fields(body[2])
	assert mov_return_mode == 3 && mov_return_reg == 1 && mov_return_rm == 0
	assert body[3] == 0x4d && body[4] == 0x85
	test_mode, test_reg, test_rm := pe64_test_modrm_fields(body[5])
	assert test_mode == 3 && test_reg == 0 && test_rm == 0
	assert body[6] == 0x74 && body[7] == 0x0e
	assert pe64_test_rel8_target(8, body[7]) == 22
	assert body[8] == 0x49 && body[9] == 0x89
	mov_cursor_mode, mov_cursor_reg, mov_cursor_rm := pe64_test_modrm_fields(body[10])
	assert mov_cursor_mode == 3 && mov_cursor_reg == 1 && mov_cursor_rm == 2
	assert body[11] == 0x41 && body[12] == 0x88
	store_mode, store_reg, store_rm := pe64_test_modrm_fields(body[13])
	assert store_mode == 0 && store_reg == 2 && store_rm == 2
	assert body[14] == 0x49 && body[15] == 0xff
	inc_mode, inc_opcode, inc_rm := pe64_test_modrm_fields(body[16])
	assert inc_mode == 3 && inc_opcode == 0 && inc_rm == 2
	assert body[17] == 0x49 && body[18] == 0xff
	dec_mode, dec_opcode, dec_rm := pe64_test_modrm_fields(body[19])
	assert dec_mode == 3 && dec_opcode == 1 && dec_rm == 0
	assert body[20] == 0x75 && body[21] == 0xf5
	assert pe64_test_rel8_target(22, body[21]) == 11
	assert body[22] == 0xc3
	assert 0xfc !in body
	assert 0xfd !in body
}

fn pe64_test_execute_runtime_memset_manifest(body []u8, memory_base u64, mut state Pe64TestMemsetAbiState) {
	pe64_test_assert_runtime_memset_body(body)
	state.rax = state.rcx
	state.status_flags_written = true
	if state.r8 == 0 {
		return
	}
	state.r10 = state.rcx
	for state.r8 != 0 {
		assert state.r10 >= memory_base
		index := state.r10 - memory_base
		assert index < u64(state.memory.len)
		state.memory[int(index)] = u8(state.rdx & 0xff)
		state.r10++
		state.r8--
	}
}

fn pe64_test_assert_runtime_memcmp_body(body []u8) {
	expected := pe64_test_runtime_memcmp_body()
	assert body.len == int(pe64_runtime_memcmp_size)
	assert body == expected
	assert body[0] == 0x4d && body[1] == 0x85
	test_mode, test_reg, test_rm := pe64_test_modrm_fields(body[2])
	assert test_mode == 3 && test_reg == 0 && test_rm == 0
	assert body[3] == 0x74 && body[4] == 0x16
	assert pe64_test_rel8_target(5, body[4]) == 27
	assert body[5] == 0x44 && body[6] == 0x8a
	left_mode, left_reg, left_rm := pe64_test_modrm_fields(body[7])
	assert left_mode == 0 && left_reg == 1 && left_rm == 1
	assert body[8] == 0x44 && body[9] == 0x8a
	right_mode, right_reg, right_rm := pe64_test_modrm_fields(body[10])
	assert right_mode == 0 && right_reg == 2 && right_rm == 2
	assert body[11] == 0x45 && body[12] == 0x38
	cmp_mode, cmp_reg, cmp_rm := pe64_test_modrm_fields(body[13])
	assert cmp_mode == 3 && cmp_reg == 2 && cmp_rm == 1
	assert body[14] == 0x75 && body[15] == 0x0e
	assert pe64_test_rel8_target(16, body[15]) == 30
	assert body[16] == 0x48 && body[17] == 0xff
	left_inc_mode, left_inc_opcode, left_inc_rm := pe64_test_modrm_fields(body[18])
	assert left_inc_mode == 3 && left_inc_opcode == 0 && left_inc_rm == 1
	assert body[19] == 0x48 && body[20] == 0xff
	right_inc_mode, right_inc_opcode, right_inc_rm := pe64_test_modrm_fields(body[21])
	assert right_inc_mode == 3 && right_inc_opcode == 0 && right_inc_rm == 2
	assert body[22] == 0x49 && body[23] == 0xff
	dec_mode, dec_opcode, dec_rm := pe64_test_modrm_fields(body[24])
	assert dec_mode == 3 && dec_opcode == 1 && dec_rm == 0
	assert body[25] == 0x75 && body[26] == 0xea
	assert pe64_test_rel8_target(27, body[26]) == 5
	assert body[27] == 0x29
	zero_mode, zero_reg, zero_rm := pe64_test_modrm_fields(body[28])
	assert zero_mode == 3 && zero_reg == 0 && zero_rm == 0
	assert body[29] == 0xc3
	assert body[30] == 0x41 && body[31] == 0x0f && body[32] == 0xb6
	left_extend_mode, left_extend_reg, left_extend_rm := pe64_test_modrm_fields(body[33])
	assert left_extend_mode == 3 && left_extend_reg == 0 && left_extend_rm == 1
	assert body[34] == 0x45 && body[35] == 0x0f && body[36] == 0xb6
	right_extend_mode, right_extend_reg, right_extend_rm := pe64_test_modrm_fields(body[37])
	assert right_extend_mode == 3 && right_extend_reg == 2 && right_extend_rm == 2
	assert body[38] == 0x44 && body[39] == 0x29
	diff_mode, diff_reg, diff_rm := pe64_test_modrm_fields(body[40])
	assert diff_mode == 3 && diff_reg == 2 && diff_rm == 0
	assert body[41] == 0xc3
	assert 0xfc !in body
	assert 0xfd !in body
}

fn pe64_test_execute_runtime_memcmp_manifest(body []u8, memory_base u64, mut state Pe64TestMemcmpAbiState) {
	pe64_test_assert_runtime_memcmp_body(body)
	state.status_flags_written = true
	for state.r8 != 0 {
		assert state.rcx >= memory_base
		assert state.rdx >= memory_base
		left_index := state.rcx - memory_base
		right_index := state.rdx - memory_base
		assert left_index < u64(state.memory.len)
		assert right_index < u64(state.memory.len)
		state.read_offsets << left_index
		state.read_offsets << right_index
		left := state.memory[int(left_index)]
		right := state.memory[int(right_index)]
		state.r9 = (state.r9 & u64(0xffff_ffff_ffff_ff00)) | u64(left)
		state.r10 = (state.r10 & u64(0xffff_ffff_ffff_ff00)) | u64(right)
		if left != right {
			state.r10 = u64(right)
			state.eax = i32(left) - i32(right)
			state.rax_written = true
			return
		}
		state.rcx++
		state.rdx++
		state.r8--
	}
	state.eax = 0
	state.rax_written = true
}

fn pe64_test_assert_runtime_memcmp_preserved(before &Pe64TestMemcmpAbiState, after &Pe64TestMemcmpAbiState) {
	assert after.r11 == before.r11
	assert after.rsp == before.rsp
	assert after.nonvolatile_gprs == before.nonvolatile_gprs
	assert after.nonvolatile_vectors == before.nonvolatile_vectors
	assert after.df == before.df
	assert after.memory == before.memory
	assert after.stack == before.stack
}

fn pe64_test_runtime_memcmp_state(rcx u64, rdx u64, count u64, memory []u8) Pe64TestMemcmpAbiState {
	return Pe64TestMemcmpAbiState{
		eax:                 -123_456
		rcx:                 rcx
		rdx:                 rdx
		r8:                  count
		r9:                  0x1122_3344_5566_7788
		r10:                 0x8877_6655_4433_2211
		r11:                 0xaabb_ccdd_eeff_0011
		rsp:                 0x7fff_ffff_d000
		nonvolatile_gprs:    [u64(0x21), 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28]
		nonvolatile_vectors: [u64(0x306), 0x307, 0x308, 0x309, 0x310, 0x311, 0x312, 0x313, 0x314,
			0x315]
		df:                  true
		memory:              memory.clone()
		stack:               [u8(0x71), 0x72, 0x73, 0x74, 0x75]
	}
}

fn pe64_test_clone_runtime_memcmp_state(state &Pe64TestMemcmpAbiState) Pe64TestMemcmpAbiState {
	return Pe64TestMemcmpAbiState{
		eax:                  state.eax
		rax_written:          state.rax_written
		rcx:                  state.rcx
		rdx:                  state.rdx
		r8:                   state.r8
		r9:                   state.r9
		r10:                  state.r10
		r11:                  state.r11
		rsp:                  state.rsp
		nonvolatile_gprs:     state.nonvolatile_gprs.clone()
		nonvolatile_vectors:  state.nonvolatile_vectors.clone()
		df:                   state.df
		status_flags_written: state.status_flags_written
		memory:               state.memory.clone()
		stack:                state.stack.clone()
		read_offsets:         state.read_offsets.clone()
	}
}

fn pe64_test_assert_runtime_move_body(body []u8) {
	expected := pe64_test_runtime_move_body()
	assert body.len == int(pe64_runtime_move_size)
	assert body == expected
	offsets := [0, 3, 6, 8, 11, 13, 17, 20, 22, 27, 32, 35, 38, 41, 44, 47, 49, 50, 53, 56, 59,
		62, 65, 68, 71, 73]
	lengths := [3, 3, 2, 3, 2, 4, 3, 2, 5, 5, 3, 3, 3, 3, 3, 2, 1, 3, 3, 3, 3, 3, 3, 3, 2, 1]
	assert offsets.len == 26
	assert lengths.len == offsets.len
	for index in 0 .. offsets.len - 1 {
		assert offsets[index] + lengths[index] == offsets[index + 1]
	}
	assert offsets[offsets.len - 1] + lengths[lengths.len - 1] == body.len
	modrm_offsets := [2, 5, 10, 15, 19, 24, 29, 34, 37, 40, 43, 46, 52, 55, 58, 61, 64, 67, 70]
	mut modrm := []u8{cap: modrm_offsets.len}
	for offset in modrm_offsets {
		modrm << body[offset]
	}
	assert modrm == [u8(0xc8), 0xc0, 0xd1, 0x0c, 0xc9, 0x54, 0x5c, 0x0b, 0x0a, 0xca, 0xcb, 0xc8,
		0xca, 0xd3, 0x0b, 0x0a, 0xc2, 0xc3, 0xc8]
	assert body[16] == 0x02
	assert body[25] == 0x01 && body[26] == 0xff
	assert body[30] == 0x02 && body[31] == 0xff
	assert body[6] == 0x74 && pe64_test_rel8_target(8, body[7]) == 73
	assert body[11] == 0x76 && pe64_test_rel8_target(13, body[12]) == 50
	assert body[20] == 0x73 && pe64_test_rel8_target(22, body[21]) == 50
	assert body[47] == 0x75 && pe64_test_rel8_target(49, body[48]) == 32
	assert body[71] == 0x75 && pe64_test_rel8_target(73, body[72]) == 56
	assert body[3] == 0x4d && body[4] == 0x85 && body[5] == 0xc0
	assert body[13] == 0x4e && body[22] == 0x4e && body[27] == 0x4e
	assert body[44..47] == [u8(0x49), 0xff, 0xc8]
	assert body[68..71] == [u8(0x49), 0xff, 0xc8]
	assert body[49] == 0xc3 && body[73] == 0xc3
	assert 0xfc !in body
	assert 0xfd !in body
}

fn pe64_test_execute_runtime_move_manifest(body []u8, memory_base u64, mut state Pe64TestMoveAbiState) {
	pe64_test_assert_runtime_move_body(body)
	state.rax = state.rcx
	state.status_flags_written = true
	if state.r8 == 0 {
		return
	}
	assert state.rcx <= max_u64 - state.r8
	assert state.rdx <= max_u64 - state.r8
	if state.rcx > state.rdx {
		state.r9 = state.rdx + state.r8
		if state.rcx < state.r9 {
			state.r10 = state.rcx + state.r8 - 1
			state.r11 = state.rdx + state.r8 - 1
			for state.r8 != 0 {
				assert state.r11 >= memory_base
				assert state.r10 >= memory_base
				read_index := state.r11 - memory_base
				write_index := state.r10 - memory_base
				assert read_index < u64(state.memory.len)
				assert write_index < u64(state.memory.len)
				state.read_offsets << read_index
				state.write_offsets << write_index
				byte := state.memory[int(read_index)]
				state.r9 = (state.r9 & u64(0xffff_ffff_ffff_ff00)) | u64(byte)
				state.memory[int(write_index)] = byte
				state.r10--
				state.r11--
				state.r8--
			}
			return
		}
	}
	state.r10 = state.rcx
	state.r11 = state.rdx
	for state.r8 != 0 {
		assert state.r11 >= memory_base
		assert state.r10 >= memory_base
		read_index := state.r11 - memory_base
		write_index := state.r10 - memory_base
		assert read_index < u64(state.memory.len)
		assert write_index < u64(state.memory.len)
		state.read_offsets << read_index
		state.write_offsets << write_index
		byte := state.memory[int(read_index)]
		state.r9 = (state.r9 & u64(0xffff_ffff_ffff_ff00)) | u64(byte)
		state.memory[int(write_index)] = byte
		state.r10++
		state.r11++
		state.r8--
	}
}

fn pe64_test_assert_runtime_move_preserved(before &Pe64TestMoveAbiState, after &Pe64TestMoveAbiState) {
	assert after.rcx == before.rcx
	assert after.rdx == before.rdx
	assert after.rsp == before.rsp
	assert after.nonvolatile_gprs == before.nonvolatile_gprs
	assert after.nonvolatile_vectors == before.nonvolatile_vectors
	assert after.df == before.df
	assert after.stack == before.stack
}

fn pe64_test_runtime_move_state(rcx u64, rdx u64, count u64, memory []u8) Pe64TestMoveAbiState {
	return Pe64TestMoveAbiState{
		rax:                 0xdead_beef_cafe_babe
		rcx:                 rcx
		rdx:                 rdx
		r8:                  count
		r9:                  0x1122_3344_5566_7788
		r10:                 0x8877_6655_4433_2211
		r11:                 0xaabb_ccdd_eeff_0011
		rsp:                 0x7fff_ffff_c000
		nonvolatile_gprs:    [u64(0x41), 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48]
		nonvolatile_vectors: [u64(0x506), 0x507, 0x508, 0x509, 0x510, 0x511, 0x512, 0x513, 0x514,
			0x515]
		df:                  true
		memory:              memory.clone()
		stack:               [u8(0x81), 0x82, 0x83, 0x84, 0x85]
	}
}

fn pe64_test_clone_runtime_move_state(state &Pe64TestMoveAbiState) Pe64TestMoveAbiState {
	return Pe64TestMoveAbiState{
		rax:                  state.rax
		rcx:                  state.rcx
		rdx:                  state.rdx
		r8:                   state.r8
		r9:                   state.r9
		r10:                  state.r10
		r11:                  state.r11
		rsp:                  state.rsp
		nonvolatile_gprs:     state.nonvolatile_gprs.clone()
		nonvolatile_vectors:  state.nonvolatile_vectors.clone()
		df:                   state.df
		status_flags_written: state.status_flags_written
		memory:               state.memory.clone()
		stack:                state.stack.clone()
		read_offsets:         state.read_offsets.clone()
		write_offsets:        state.write_offsets.clone()
	}
}

fn pe64_test_build_then_publish_error(o &Object, definition Pe64ImageDefinition, final_path string) string {
	stage_path := publication_stage_path(final_path)
	assert !pe64_test_path_present(final_path)
	assert !pe64_test_path_present(stage_path)
	bytes := pe64_image_bytes(o, definition) or {
		assert !pe64_test_path_present(final_path)
		assert !pe64_test_path_present(stage_path)
		return err.msg()
	}
	publish_object(final_path, bytes) or { assert false, err.msg() }
	assert false, 'PE64 image construction and publication unexpectedly succeeded'
	return ''
}

fn test_pe64_core_leaf_image_has_exact_headers_sections_entry_and_zero_directories() {
	object, entry := pe64_test_leaf_object('not_main')
	data := pe64_image_bytes(&object, pe64_test_definition(entry, []Pe64ImportBinding{})) or {
		panic(err)
	}
	assert data.len == 1024
	assert data[0..2] == [u8(`M`), `Z`]
	pe_offset := int(pe64_test_read_u32(data, 0x3c))
	assert pe_offset == 0x80
	assert pe64_test_read_u32(data, pe_offset) == pe64_signature
	assert pe64_test_read_u16(data, pe_offset + 4) == pe64_machine_amd64
	assert pe64_test_read_u16(data, pe_offset + 6) == 1
	assert pe64_test_read_u32(data, pe_offset + 8) == 0
	assert pe64_test_read_u32(data, pe_offset + 12) == 0
	assert pe64_test_read_u32(data, pe_offset + 16) == 0
	assert pe64_test_read_u16(data, pe_offset + 20) == u16(pe64_optional_header_size)
	assert pe64_test_read_u16(data, pe_offset + 22) == pe64_image_file_relocs_stripped | pe64_image_file_executable_image | pe64_image_file_large_address_aware
	optional := pe64_test_optional_offset(data)
	assert pe64_test_read_u16(data, optional) == pe64_optional_header_magic
	assert data[optional + 2] == pe64_linker_major_version
	assert data[optional + 3] == pe64_linker_minor_version
	assert pe64_test_read_u32(data, optional + 4) == 0x200
	assert pe64_test_read_u32(data, optional + 8) == 0
	assert pe64_test_read_u32(data, optional + 12) == 0
	assert pe64_test_read_u32(data, optional + 16) == 0x1000
	assert pe64_test_read_u32(data, optional + 20) == 0x1000
	assert pe64_test_read_u64(data, optional + 24) == pe64_image_base
	assert pe64_test_read_u32(data, optional + 32) == 0x1000
	assert pe64_test_read_u32(data, optional + 36) == 0x200
	assert pe64_test_read_u16(data, optional + 40) == 6
	assert pe64_test_read_u16(data, optional + 42) == 0
	assert pe64_test_read_u16(data, optional + 48) == 6
	assert pe64_test_read_u16(data, optional + 50) == 0
	assert pe64_test_read_u32(data, optional + 56) == 0x2000
	assert pe64_test_read_u32(data, optional + 60) == 0x200
	assert pe64_test_read_u32(data, optional + 64) == 0
	assert pe64_test_read_u16(data, optional + 68) == pe64_subsystem_windows_cui
	assert pe64_test_read_u16(data, optional + 70) == pe64_dll_characteristics_nx_compat
	assert pe64_test_read_u64(data, optional + 72) == pe64_size_of_stack_reserve
	assert pe64_test_read_u64(data, optional + 80) == pe64_size_of_stack_commit
	assert pe64_test_read_u64(data, optional + 88) == pe64_size_of_heap_reserve
	assert pe64_test_read_u64(data, optional + 96) == pe64_size_of_heap_commit
	assert pe64_test_read_u32(data, optional + 104) == 0
	assert pe64_test_read_u32(data, optional + 108) == u32(pe64_directory_count)
	for index in 0 .. pe64_directory_count {
		assert pe64_test_directory(data, index) == Pe64TestDirectory{}
	}
	sections := pe64_test_sections(data)
	assert sections == [
		Pe64TestSection{
			name:            '.text'
			virtual_size:    2
			virtual_address: 0x1000
			raw_size:        0x200
			raw_pointer:     0x200
			characteristics: pe64_section_contains_code | pe64_section_mem_execute | pe64_section_mem_read
		},
	]
	pe64_test_assert_zero_range(data, optional + int(pe64_optional_header_size) + 40, 0x200)
	assert data[0x200..0x202] == [u8(0xeb), 0xfe]
	pe64_test_assert_zero_range(data, 0x202, data.len)
	assert object.text == [u8(0xeb), 0xfe]
}

fn test_pe64_core_internal_rel32_resolves_forward_backward_and_checks_overflow() {
	mut object := Object.new()
	forward := object.intern_function_symbol('forward') or { panic(err) }
	backward := object.intern_function_symbol('backward') or { panic(err) }
	body := pe64_test_nonleaf_body()
	assert object.append_text(body) or { panic(err) } == 0
	assert object.append_text(body) or { panic(err) } == 15
	object.define_text_function(forward, 0, 15) or { panic(err) }
	object.define_text_function(backward, 15, 15) or { panic(err) }
	object.add_text_call_relocation(20, forward) or { panic(err) }
	object.add_text_call_relocation(5, backward) or { panic(err) }
	before := object.text.clone()
	data := pe64_image_bytes(&object, pe64_test_definition(forward, []Pe64ImportBinding{})) or {
		panic(err)
	}
	text := pe64_test_section(data, '.text')
	assert pe64_test_read_u32(data, int(text.raw_pointer) + 5) == u32(6)
	assert pe64_test_read_u32(data, int(text.raw_pointer) + 20) == u32(0xffff_ffe8)
	assert pe64_test_directory(data, pe64_base_relocation_directory_index) == Pe64TestDirectory{}
	assert object.text == before
	assert object.text[5..9] == [u8(0), 0, 0, 0]
	assert object.text[20..24] == [u8(0), 0, 0, 0]
	assert pe64_checked_rel32(0, u64(2_147_483_651)) or { panic(err) } == u32(0x7fff_ffff)
	assert pe64_checked_rel32(u64(2_147_483_644), 0) or { panic(err) } == u32(0x8000_0000)
	if _ := pe64_checked_rel32(0, u64(2_147_483_652)) {
		assert false, 'positive REL32 overflow was accepted'
	} else {
		assert err.msg() == 'PE64 REL32 overflow'
	}
	if _ := pe64_checked_rel32(u64(2_147_483_645), 0) {
		assert false, 'negative REL32 overflow was accepted'
	} else {
		assert err.msg() == 'PE64 REL32 overflow'
	}
	if _ := pe64_checked_rel32(max_u64 - 3, 0) {
		assert false, 'overflowing REL32 place was accepted'
	} else {
		assert err.msg() == 'PE64 REL32 overflow'
	}
}

fn test_pe64_core_nonleaf_preserves_canonical_unwind_in_pdata_xdata_exception_directory() {
	mut object := Object.new()
	later := object.intern_function_symbol('later') or { panic(err) }
	early := object.intern_function_symbol('early') or { panic(err) }
	leaf := object.intern_function_symbol('leaf') or { panic(err) }
	body := pe64_test_nonleaf_body()
	assert object.append_text(body) or { panic(err) } == 0
	assert object.append_text(body) or { panic(err) } == 15
	assert object.append_text([u8(0xeb), 0xfe]) or { panic(err) } == 30
	object.define_text_function(later, 15, 15) or { panic(err) }
	object.define_text_function(early, 0, 15) or { panic(err) }
	object.define_text_function(leaf, 30, 2) or { panic(err) }
	object.add_text_call_relocation(20, early) or { panic(err) }
	object.add_text_call_relocation(5, later) or { panic(err) }
	data := pe64_image_bytes(&object, pe64_test_definition(early, []Pe64ImportBinding{})) or {
		panic(err)
	}
	sections := pe64_test_sections(data)
	assert sections.map(it.name) == ['.text', '.pdata', '.xdata']
	pdata := sections[1]
	xdata := sections[2]
	assert pdata.virtual_size == 24
	assert xdata.virtual_size == 16
	assert pdata.virtual_address % 4 == 0
	assert xdata.virtual_address % 4 == 0
	assert pe64_test_directory(data, pe64_exception_directory_index) == Pe64TestDirectory{
		rva:  pdata.virtual_address
		size: pdata.virtual_size
	}
	pdata_offset := int(pdata.raw_pointer)
	assert pe64_test_read_u32(data, pdata_offset) == sections[0].virtual_address
	assert pe64_test_read_u32(data, pdata_offset + 4) == sections[0].virtual_address + 15
	assert pe64_test_read_u32(data, pdata_offset + 8) == xdata.virtual_address
	assert pe64_test_read_u32(data, pdata_offset + 12) == sections[0].virtual_address + 15
	assert pe64_test_read_u32(data, pdata_offset + 16) == sections[0].virtual_address + 30
	assert pe64_test_read_u32(data, pdata_offset + 20) == xdata.virtual_address + 8
	expected_unwind := [u8(0x01), 0x04, 0x01, 0, 0x04, 0x42, 0, 0]
	mut two_unwind := expected_unwind.clone()
	two_unwind << expected_unwind
	assert data[int(xdata.raw_pointer)..int(xdata.raw_pointer + xdata.virtual_size)] == two_unwind
	assert pdata.virtual_size / 12 == 2
}

fn test_pe64_core_private_data_layout_sections_directories_and_wx_are_exact() {
	mut object, entry := pe64_test_leaf_object('private_entry')
	plan := private_data_preflight([
		PrivateDataDefinition{ name: 'flag', value: 7, width: 8, alignment: 1 },
		PrivateDataDefinition{
			name:      'counter'
			value:     0x0102_0304_0506_0708
			width:     64
			alignment: 8
		},
	], ['private_entry']) or { panic(err) }
	object.install_private_data(&plan) or { panic(err) }
	before := object.private_data.clone()
	data := pe64_image_bytes(&object, pe64_test_definition(entry, []Pe64ImportBinding{})) or {
		panic(err)
	}
	sections := pe64_test_sections(data)
	assert sections.map(it.name) == ['.text', '.data']
	text := sections[0]
	private_data := sections[1]
	assert private_data.virtual_address == text.virtual_address + 0x1000
	assert private_data.raw_pointer == text.raw_pointer + text.raw_size
	assert private_data.virtual_size == u32(before.len)
	assert private_data.characteristics == pe64_section_contains_initialized_data | pe64_section_mem_read | pe64_section_mem_write
	assert private_data.characteristics & pe64_section_mem_execute == 0
	assert text.characteristics & pe64_section_mem_write == 0
	assert data[int(private_data.raw_pointer)..int(private_data.raw_pointer +
		private_data.virtual_size)] == before
	assert before == [u8(7), 0, 0, 0, 0, 0, 0, 0, 8, 7, 6, 5, 4, 3, 2, 1]
	pe64_test_assert_zero_range(data, int(private_data.raw_pointer + private_data.virtual_size), int(
		private_data.raw_pointer + private_data.raw_size))
	for index in 0 .. pe64_directory_count {
		assert pe64_test_directory(data, index) == Pe64TestDirectory{}
	}
	assert object.private_data == before
}

fn test_pe64_core_explicit_imports_emit_descriptors_ilt_iat_names_and_thunks() {
	mut object := Object.new()
	entry := object.intern_function_symbol('explicit_raw_entry') or { panic(err) }
	message := object.intern_external_function_symbol('semantic_external_a') or { panic(err) }
	exit := object.intern_external_function_symbol('semantic_external_b') or { panic(err) }
	tick := object.intern_external_function_symbol('semantic_external_c') or { panic(err) }
	body := [
		u8(0x48),
		0x83,
		0xec,
		0x28,
		0xe8,
		0,
		0,
		0,
		0,
		0xe8,
		0,
		0,
		0,
		0,
		0xe8,
		0,
		0,
		0,
		0,
		0x48,
		0x83,
		0xc4,
		0x28,
		0xeb,
		0xfe,
	]
	assert object.append_text(body) or { panic(err) } == 0
	object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
	object.add_text_call_relocation(5, message) or { panic(err) }
	object.add_text_call_relocation(10, exit) or { panic(err) }
	object.add_text_call_relocation(15, tick) or { panic(err) }
	definition := pe64_test_definition(entry, [
		Pe64ImportBinding{
			symbol_id:   message
			dll:         'user32.dll'
			export_name: 'MessageBoxA'
		},
		Pe64ImportBinding{
			symbol_id:   exit
			dll:         'kernel32.dll'
			export_name: 'ExitProcess'
		},
		Pe64ImportBinding{
			symbol_id:   tick
			dll:         'kernel32.dll'
			export_name: 'GetTickCount'
		},
	])
	data := pe64_image_bytes(&object, definition) or { panic(err) }
	sections := pe64_test_sections(data)
	assert sections.map(it.name) == ['.text', '.pdata', '.xdata', '.idata']
	text := sections[0]
	pdata := sections[1]
	idata := sections[3]
	assert text.virtual_size == u32(body.len + 18)
	assert pdata.virtual_size == 12
	assert idata.characteristics == pe64_section_contains_initialized_data | pe64_section_mem_read | pe64_section_mem_write
	assert pe64_test_read_u32(data, int(text.raw_pointer) + 5) == u32(28)
	assert pe64_test_read_u32(data, int(text.raw_pointer) + 10) == u32(11)
	assert pe64_test_read_u32(data, int(text.raw_pointer) + 15) == u32(12)
	assert data[int(text.raw_pointer) + body.len..int(text.raw_pointer) + body.len + 2] == [
		u8(0xff),
		0x25,
	]
	assert data[int(text.raw_pointer) + body.len + 6..int(text.raw_pointer) + body.len + 8] == [
		u8(0xff),
		0x25,
	]
	assert data[int(text.raw_pointer) + body.len + 12..int(text.raw_pointer) + body.len + 14] == [
		u8(0xff),
		0x25,
	]

	import_directory := pe64_test_directory(data, pe64_import_directory_index)
	iat_directory := pe64_test_directory(data, pe64_iat_directory_index)
	assert import_directory == Pe64TestDirectory{
		rva:  idata.virtual_address
		size: 60
	}
	assert iat_directory.size == 40
	idata_offset := int(idata.raw_pointer)
	first_ilt := pe64_test_read_u32(data, idata_offset)
	first_name := pe64_test_read_u32(data, idata_offset + 12)
	first_iat := pe64_test_read_u32(data, idata_offset + 16)
	second_ilt := pe64_test_read_u32(data, idata_offset + 20)
	second_name := pe64_test_read_u32(data, idata_offset + 32)
	second_iat := pe64_test_read_u32(data, idata_offset + 36)
	pe64_test_assert_zero_range(data, idata_offset + 40, idata_offset + 60)
	assert pe64_test_cstring(data, pe64_test_rva_offset(data, first_name), data.len) == 'kernel32.dll'
	assert pe64_test_cstring(data, pe64_test_rva_offset(data, second_name), data.len) == 'user32.dll'
	assert iat_directory.rva == first_iat
	first_hint_rva := pe64_test_read_u64(data, pe64_test_rva_offset(data, first_ilt))
	first_second_hint_rva := pe64_test_read_u64(data, pe64_test_rva_offset(data, first_ilt) + 8)
	second_hint_rva := pe64_test_read_u64(data, pe64_test_rva_offset(data, second_ilt))
	assert first_hint_rva % 2 == 0
	assert first_second_hint_rva % 2 == 0
	assert second_hint_rva % 2 == 0
	assert first_hint_rva == pe64_test_read_u64(data, pe64_test_rva_offset(data, first_iat))
	assert first_second_hint_rva == pe64_test_read_u64(data,

		pe64_test_rva_offset(data, first_iat) + 8)
	assert second_hint_rva == pe64_test_read_u64(data, pe64_test_rva_offset(data, second_iat))
	assert pe64_test_read_u16(data, pe64_test_rva_offset(data, u32(first_hint_rva))) == 0
	assert pe64_test_read_u16(data, pe64_test_rva_offset(data, u32(first_second_hint_rva))) == 0
	assert pe64_test_read_u16(data, pe64_test_rva_offset(data, u32(second_hint_rva))) == 0
	assert pe64_test_cstring(data, pe64_test_rva_offset(data, u32(first_hint_rva)) + 2, data.len) == 'ExitProcess'
	assert pe64_test_cstring(data, pe64_test_rva_offset(data, u32(first_second_hint_rva)) + 2,
		data.len) == 'GetTickCount'
	assert pe64_test_cstring(data, pe64_test_rva_offset(data, u32(second_hint_rva)) + 2, data.len) == 'MessageBoxA'
	assert pe64_test_read_u64(data, pe64_test_rva_offset(data, first_ilt) + 16) == 0
	assert pe64_test_read_u64(data, pe64_test_rva_offset(data, second_ilt) + 8) == 0
	assert pe64_test_read_u64(data, pe64_test_rva_offset(data, first_iat) + 16) == 0
	assert pe64_test_read_u64(data, pe64_test_rva_offset(data, second_iat) + 8) == 0
	first_thunk_field_rva := text.virtual_address + u32(body.len) + 2
	second_thunk_field_rva := text.virtual_address + u32(body.len) + 8
	third_thunk_field_rva := text.virtual_address + u32(body.len) + 14
	first_thunk_displacement := pe64_checked_rel32(u64(first_thunk_field_rva), u64(first_iat)) or {
		panic(err)
	}
	second_thunk_displacement := pe64_checked_rel32(u64(second_thunk_field_rva), u64(first_iat + 8)) or {
		panic(err)
	}
	third_thunk_displacement := pe64_checked_rel32(u64(third_thunk_field_rva), u64(second_iat)) or {
		panic(err)
	}
	assert pe64_test_read_u32(data, int(text.raw_pointer) + body.len + 2) == first_thunk_displacement
	assert pe64_test_read_u32(data, int(text.raw_pointer) + body.len + 8) == second_thunk_displacement
	assert pe64_test_read_u32(data, int(text.raw_pointer) + body.len + 14) == third_thunk_displacement
	assert pdata.virtual_size / 12 == 1
}

fn pe64_test_alias_fixture(reverse_relocations bool,
	external_names []string) (Object, SymbolID, SymbolID, SymbolID, SymbolID) {
	assert external_names.len == 3
	mut object := Object.new()
	entry := object.intern_function_symbol('alias_entry') or { panic(err) }
	first := object.intern_external_function_symbol(external_names[0]) or { panic(err) }
	second := object.intern_external_function_symbol(external_names[1]) or { panic(err) }
	third := object.intern_external_function_symbol(external_names[2]) or { panic(err) }
	body := [
		u8(0x48),
		0x83,
		0xec,
		0x28,
		0xe8,
		0,
		0,
		0,
		0,
		0xe8,
		0,
		0,
		0,
		0,
		0xe8,
		0,
		0,
		0,
		0,
		0x48,
		0x83,
		0xc4,
		0x28,
		0xeb,
		0xfe,
	]
	assert object.append_text(body) or { panic(err) } == 0
	object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
	if reverse_relocations {
		object.add_text_call_relocation(15, third) or { panic(err) }
		object.add_text_call_relocation(10, second) or { panic(err) }
		object.add_text_call_relocation(5, first) or { panic(err) }
	} else {
		object.add_text_call_relocation(5, first) or { panic(err) }
		object.add_text_call_relocation(10, second) or { panic(err) }
		object.add_text_call_relocation(15, third) or { panic(err) }
	}
	return object, entry, first, second, third
}

fn test_pe64_core_import_aliases_deduplicate_and_names_and_input_order_are_irrelevant() {
	first_object, first_entry, first_a, first_b, first_c := pe64_test_alias_fixture(false, [
		'first_semantic_external',
		'second_semantic_external',
		'third_semantic_external',
	])
	second_object, second_entry, second_a, second_b, second_c := pe64_test_alias_fixture(true, [
		'first_semantic_external',
		'second_semantic_external',
		'third_semantic_external',
	])
	name_object, name_entry, name_a, name_b, name_c := pe64_test_alias_fixture(false, [
		'.xdata',
		'.v3\$coff\$end\$0\$0',
		'third_semantic_external',
	])
	first_definition := pe64_test_definition(first_entry, [
		Pe64ImportBinding{ symbol_id: first_c, dll: 'user32.dll', export_name: 'ExitProcess' },
		Pe64ImportBinding{ symbol_id: first_b, dll: 'kernel32.dll', export_name: 'ExitProcess' },
		Pe64ImportBinding{ symbol_id: first_a, dll: 'kernel32.dll', export_name: 'ExitProcess' },
	])
	second_definition := pe64_test_definition(second_entry, [
		Pe64ImportBinding{ symbol_id: second_a, dll: 'kernel32.dll', export_name: 'ExitProcess' },
		Pe64ImportBinding{ symbol_id: second_b, dll: 'kernel32.dll', export_name: 'ExitProcess' },
		Pe64ImportBinding{ symbol_id: second_c, dll: 'user32.dll', export_name: 'ExitProcess' },
	])
	name_definition := pe64_test_definition(name_entry, [
		Pe64ImportBinding{ symbol_id: name_c, dll: 'user32.dll', export_name: 'ExitProcess' },
		Pe64ImportBinding{ symbol_id: name_b, dll: 'kernel32.dll', export_name: 'ExitProcess' },
		Pe64ImportBinding{ symbol_id: name_a, dll: 'kernel32.dll', export_name: 'ExitProcess' },
	])
	first := pe64_image_bytes(&first_object, first_definition) or { panic(err) }
	second := pe64_image_bytes(&second_object, second_definition) or { panic(err) }
	name_neutral := pe64_image_bytes(&name_object, name_definition) or { panic(err) }
	assert first == second
	assert first == name_neutral
	text := pe64_test_section(first, '.text')
	assert text.virtual_size == u32(first_object.text.len + 12)
	import_directory := pe64_test_directory(first, pe64_import_directory_index)
	assert import_directory.size == 60
	idata := pe64_test_section(first, '.idata')
	idata_offset := int(idata.raw_pointer)
	assert pe64_test_cstring(first, pe64_test_rva_offset(first, pe64_test_read_u32(first,

		idata_offset + 12)), first.len) == 'kernel32.dll'
	assert pe64_test_cstring(first, pe64_test_rva_offset(first, pe64_test_read_u32(first,

		idata_offset + 32)), first.len) == 'user32.dll'
	assert pe64_test_read_u32(first, int(text.raw_pointer) + 5) == u32(16)
	assert pe64_test_read_u32(first, int(text.raw_pointer) + 10) == u32(11)
	assert pe64_test_read_u32(first, int(text.raw_pointer) + 15) == u32(12)
}

fn test_pe64_layout_accepts_maximum_size_of_image_and_rejects_first_aligned_value_above() {
	maximum := pe64_build_layout(u64(0x7fff_f000), 0, 0, 0) or { panic(err) }
	assert maximum.size_of_image == pe64_max_size_of_image
	if _ := pe64_build_layout(u64(0x7fff_f001), 0, 0, 0) {
		assert false, 'PE layout above the maximum SizeOfImage was accepted'
	} else {
		assert err.msg() == 'PE64 layout: SizeOfImage 2147487744 exceeds 2147483648'
	}
}

fn test_pe64_core_refuses_bad_definition_unbound_external_malformed_object_and_overflow() {
	leaf, entry := pe64_test_leaf_object('entry')
	assert pe64_test_error(&leaf, Pe64ImageDefinition{
		subsystem:    .windows_cui
		image_policy: .fixed_base
		entry:        Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         .raw_noreturn_process_entry
		}
	}) == 'PE64 requires Microsoft x64 ABI'
	assert pe64_test_error(&leaf, Pe64ImageDefinition{
		target_abi:   unsafe { Pe64TargetAbi(255) }
		subsystem:    .windows_cui
		image_policy: .fixed_base
		entry:        Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         .raw_noreturn_process_entry
		}
	}) == 'PE64 requires Microsoft x64 ABI'
	assert pe64_test_error(&leaf, Pe64ImageDefinition{
		target_abi:   .windows_x64_microsoft
		image_policy: .fixed_base
		entry:        Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         .raw_noreturn_process_entry
		}
	}).contains('PE64 image definition: subsystem')
	assert pe64_test_error(&leaf, Pe64ImageDefinition{
		target_abi:   .windows_x64_microsoft
		subsystem:    unsafe { Pe64Subsystem(255) }
		image_policy: .fixed_base
		entry:        Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         .raw_noreturn_process_entry
		}
	}).contains('PE64 image definition: subsystem 255')
	assert pe64_test_error(&leaf, Pe64ImageDefinition{
		target_abi: .windows_x64_microsoft
		subsystem:  .windows_cui
		entry:      Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         .raw_noreturn_process_entry
		}
	}).contains('PE64 image definition: image policy')
	assert pe64_test_error(&leaf, Pe64ImageDefinition{
		target_abi:   .windows_x64_microsoft
		subsystem:    .windows_cui
		image_policy: unsafe { Pe64ImagePolicy(255) }
		entry:        Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         .raw_noreturn_process_entry
		}
	}).contains('PE64 image definition: image policy 255')
	assert pe64_test_error(&leaf, Pe64ImageDefinition{
		target_abi:   .windows_x64_microsoft
		subsystem:    .windows_cui
		image_policy: .fixed_base
		entry:        Pe64EntryDefinition{
			function_index: u32(entry)
		}
	}).contains('PE64 entry policy')
	assert pe64_test_error(&leaf, Pe64ImageDefinition{
		target_abi:   .windows_x64_microsoft
		subsystem:    .windows_cui
		image_policy: .fixed_base
		entry:        Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         unsafe { Pe64EntryPolicy(255) }
		}
	}).contains('PE64 entry policy 255')
	assert pe64_test_error(&leaf, Pe64ImageDefinition{
		target_abi:   .windows_x64_microsoft
		subsystem:    .windows_cui
		image_policy: .fixed_base
		entry:        Pe64EntryDefinition{
			function_index:  u32(entry)
			parameter_count: 1
			policy:          .raw_noreturn_process_entry
		}
	}).contains('zero-parameter')
	assert pe64_test_error(&leaf, Pe64ImageDefinition{
		target_abi:   .windows_x64_microsoft
		subsystem:    .windows_cui
		image_policy: .fixed_base
		entry:        Pe64EntryDefinition{
			function_index: 99
			policy:         .raw_noreturn_process_entry
		}
	}).contains('entry function index 99 is out of range')

	mut external_object := Object.new()
	caller := external_object.intern_function_symbol('caller') or { panic(err) }
	foreign := external_object.intern_external_function_symbol('foreign') or { panic(err) }
	body := pe64_test_nonleaf_body()
	assert external_object.append_text(body) or { panic(err) } == 0
	external_object.define_text_function(caller, 0, u64(body.len)) or { panic(err) }
	external_object.add_text_call_relocation(5, foreign) or { panic(err) }
	assert pe64_test_error(&external_object, pe64_test_definition(caller, []Pe64ImportBinding{})).contains('PE64 import binding missing for SymbolID')
	assert pe64_test_error(&external_object, pe64_test_definition(foreign, [
		Pe64ImportBinding{ symbol_id: foreign, dll: 'kernel32.dll', export_name: 'ExitProcess' },
	])).contains('is not a defined function')
	assert pe64_test_error(&external_object, pe64_test_definition(caller, [
		Pe64ImportBinding{ symbol_id: foreign, dll: '', export_name: 'ExitProcess' },
	])).contains('DLL name must not be empty')
	assert pe64_test_error(&external_object, pe64_test_definition(caller, [
		Pe64ImportBinding{ symbol_id: foreign, dll: 'kernel\x00.dll', export_name: 'ExitProcess' },
	])).contains('DLL name must not contain NUL')
	assert pe64_test_error(&external_object, pe64_test_definition(caller, [
		Pe64ImportBinding{ symbol_id: foreign, dll: 'kernel32.dll', export_name: '\xff' },
	])).contains('export name must be ASCII')
	assert pe64_test_error(&external_object, pe64_test_definition(caller, [
		Pe64ImportBinding{ symbol_id: foreign, dll: 'kernel32.dll', export_name: 'ExitProcess' },
		Pe64ImportBinding{ symbol_id: foreign, dll: 'kernel32.dll', export_name: 'ExitProcess' },
	])).contains('duplicate binding')
	assert pe64_test_error(&leaf, pe64_test_definition(entry, [
		Pe64ImportBinding{ symbol_id: entry, dll: 'kernel32.dll', export_name: 'ExitProcess' },
	])).contains('is not an external function')
	assert pe64_test_error(&leaf, pe64_test_definition(entry, [
		Pe64ImportBinding{ symbol_id: SymbolID(99), dll: 'kernel32.dll', export_name: 'ExitProcess' },
	])).contains('SymbolID 99 is out of range')

	mut ambiguous := Object.new()
	ambiguous_entry := ambiguous.intern_function_symbol('ambiguous_entry') or { panic(err) }
	first_external := ambiguous.intern_external_function_symbol('first') or { panic(err) }
	second_external := ambiguous.intern_external_function_symbol('second') or { panic(err) }
	ambiguous_body := [u8(0x48), 0x83, 0xec, 0x28, 0xe8, 0, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0x48, 0x83,
		0xc4, 0x28, 0xeb, 0xfe]
	assert ambiguous.append_text(ambiguous_body) or { panic(err) } == 0
	ambiguous.define_text_function(ambiguous_entry, 0, u64(ambiguous_body.len)) or { panic(err) }
	ambiguous.add_text_call_relocation(5, first_external) or { panic(err) }
	ambiguous.add_text_call_relocation(10, second_external) or { panic(err) }
	assert pe64_test_error(&ambiguous, pe64_test_definition(ambiguous_entry, [
		Pe64ImportBinding{ symbol_id: first_external, dll: 'KERNEL32.dll', export_name: 'One' },
		Pe64ImportBinding{ symbol_id: second_external, dll: 'kernel32.dll', export_name: 'Two' },
	])).contains('differ only by ASCII case')

	mut malformed_prolog := Object.new()
	bad_caller := malformed_prolog.intern_function_symbol('bad_caller') or { panic(err) }
	bad_target := malformed_prolog.intern_function_symbol('bad_target') or { panic(err) }
	assert malformed_prolog.append_text([u8(0xe8), 0, 0, 0, 0, 0xeb, 0xfe]) or { panic(err) } == 0
	assert malformed_prolog.append_text([u8(0xeb), 0xfe]) or { panic(err) } == 7
	malformed_prolog.define_text_function(bad_caller, 0, 7) or { panic(err) }
	malformed_prolog.define_text_function(bad_target, 7, 2) or { panic(err) }
	malformed_prolog.add_text_call_relocation(1, bad_target) or { panic(err) }
	assert pe64_test_error(&malformed_prolog, pe64_test_definition(bad_caller,
		[]Pe64ImportBinding{})).contains('PE64 unwind contract')

	mut malformed_object := external_object
	malformed_object.text = external_object.text.clone()
	malformed_object.text[6] = 1
	assert pe64_test_error(&malformed_object, pe64_test_definition(caller, [
		Pe64ImportBinding{ symbol_id: foreign, dll: 'kernel32.dll', export_name: 'ExitProcess' },
	])).contains('PE64 object contract')
	if _ := pe64_checked_add(max_u64, 1, 'test') {
		assert false, 'u64 layout overflow was accepted'
	} else {
		assert err.msg().contains('PE64 layout')
	}
	if _ := pe64_build_layout(max_u64, 0, 0, 0) {
		assert false, 'oversized PE layout was accepted'
	} else {
		assert err.msg().contains('PE64 layout')
	}
}

fn test_pe64_core_is_fresh_deterministic_and_does_not_mutate_object() {
	mut object := Object.new()
	entry := object.intern_function_symbol('immutable_entry') or { panic(err) }
	foreign := object.intern_external_function_symbol('opaque_external') or { panic(err) }
	body := pe64_test_nonleaf_body()
	assert object.append_text(body) or { panic(err) } == 0
	object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
	object.add_text_call_relocation(5, foreign) or { panic(err) }
	plan := private_data_preflight([
		PrivateDataDefinition{ name: 'slot', value: 23, width: 32, alignment: 4 },
	], ['immutable_entry', 'opaque_external']) or { panic(err) }
	object.install_private_data(&plan) or { panic(err) }
	definition := pe64_test_definition(entry, [
		Pe64ImportBinding{ symbol_id: foreign, dll: 'kernel32.dll', export_name: 'ExitProcess' },
	])
	before_text := object.text.clone()
	before_symbols := object.symbols.clone()
	before_relocations := object.call_relocations.clone()
	before_data := object.private_data.clone()
	before_data_symbols := object.private_data_symbols.clone()
	before_imports := definition.imports.clone()
	mut first := pe64_image_bytes(&object, definition) or { panic(err) }
	second := pe64_image_bytes(&object, definition) or { panic(err) }
	assert first == second
	first[0] = 0
	assert second[0] == `M`
	assert object.text == before_text
	assert object.symbols == before_symbols
	assert object.call_relocations == before_relocations
	assert object.private_data == before_data
	assert object.private_data_symbols == before_data_symbols
	assert definition.imports == before_imports
	message := pe64_test_error(&object, pe64_test_definition(entry, []Pe64ImportBinding{}))
	assert message.contains('PE64 import binding missing')
	assert object.text == before_text
	assert object.symbols == before_symbols
	assert object.call_relocations == before_relocations
	assert object.private_data == before_data
	assert object.private_data_symbols == before_data_symbols
}

fn test_pe64_core_fixed_base_headers_and_security_flags_are_coherent() {
	mut object := Object.new()
	entry := object.intern_function_symbol('security_entry') or { panic(err) }
	foreign := object.intern_external_function_symbol('security_external') or { panic(err) }
	body := pe64_test_nonleaf_body()
	assert object.append_text(body) or { panic(err) } == 0
	object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
	object.add_text_call_relocation(5, foreign) or { panic(err) }
	data := pe64_image_bytes(&object, pe64_test_definition(entry, [
		Pe64ImportBinding{ symbol_id: foreign, dll: 'kernel32.dll', export_name: 'ExitProcess' },
	])) or { panic(err) }
	pe_offset := int(pe64_test_read_u32(data, 0x3c))
	characteristics := pe64_test_read_u16(data, pe_offset + 22)
	assert characteristics & pe64_image_file_relocs_stripped != 0
	assert characteristics & pe64_image_file_executable_image != 0
	assert characteristics & pe64_image_file_large_address_aware != 0
	assert pe64_test_read_u32(data, pe_offset + 8) == 0
	assert pe64_test_read_u32(data, pe_offset + 12) == 0
	assert pe64_test_read_u32(data, pe_offset + 16) == 0
	optional := pe64_test_optional_offset(data)
	assert pe64_test_read_u64(data, optional + 24) == pe64_image_base
	dll_characteristics := pe64_test_read_u16(data, optional + 70)
	assert dll_characteristics & pe64_dll_characteristics_nx_compat != 0
	assert dll_characteristics & pe64_dll_characteristics_dynamic_base == 0
	assert dll_characteristics & pe64_dll_characteristics_high_entropy_va == 0
	assert pe64_test_directory(data, pe64_base_relocation_directory_index) == Pe64TestDirectory{}
	size_of_image := pe64_test_read_u32(data, optional + 56)
	for directory_index in 0 .. pe64_directory_count {
		directory := pe64_test_directory(data, directory_index)
		if directory.size != 0 {
			assert directory.rva < size_of_image
			assert u64(directory.rva) + u64(directory.size) <= u64(size_of_image)
		}
	}
	for section in pe64_test_sections(data) {
		assert section.name != '.reloc'
		assert section.reloc_pointer == 0
		assert section.reloc_count == 0
		assert section.characteristics & pe64_section_mem_execute == 0
			|| section.characteristics & pe64_section_mem_write == 0
	}
}

fn pe64_test_path_present(path string) bool {
	return os.exists(path) || os.is_link(path)
}

fn pe64_test_root(name string) string {
	root := os.join_path(os.temp_dir(), 'v3_amd64_pe64_${name}_${os.getpid()}')
	assert !pe64_test_path_present(root), 'test root `${root}` was stale'
	os.mkdir(root) or { assert false, 'create `${root}`: ${err.msg()}' }
	return root
}

fn pe64_test_cleanup(root string) {
	if !pe64_test_path_present(root) {
		return
	}
	assert os.is_dir(root) && !os.is_link(root), 'test root `${root}` changed type'
	for entry in os.ls(root) or { panic(err) } {
		assert !entry.ends_with('.amd64-stage'), 'publication stage leaked: `${entry}`'
	}
	os.rmdir_all(root) or { assert false, 'remove `${root}`: ${err.msg()}' }
	assert !pe64_test_path_present(root), 'test root `${root}` survived cleanup'
}

fn pe64_test_publication_error(path string, bytes []u8) string {
	mut message := ''
	publish_object(path, bytes) or { message = err.msg() }
	assert message.len != 0, 'PE publication unexpectedly succeeded for `${path}`'
	return message
}

fn test_pe64_core_publishes_with_existing_no_clobber_byte_helper() {
	object, entry := pe64_test_leaf_object('publication_entry')
	bytes := pe64_image_bytes(&object, pe64_test_definition(entry, []Pe64ImportBinding{})) or {
		panic(err)
	}
	root := pe64_test_root('publication')
	defer {
		pe64_test_cleanup(root)
	}
	output := os.join_path(root, 'core.exe')
	stage := publication_stage_path(output)
	publish_object(output, bytes) or { assert false, err.msg() }
	assert os.is_file(output) && !os.is_link(output)
	published := os.read_bytes(output) or { panic(err) }
	assert published == bytes
	assert !pe64_test_path_present(stage)

	blocked := os.join_path(root, 'blocked.exe')
	final_sentinel := 'final-sentinel\n'
	os.write_file(blocked, final_sentinel) or { panic(err) }
	assert pe64_test_publication_error(blocked, bytes) == 'final output `${blocked}` already exists'
	blocked_contents := os.read_file(blocked) or { panic(err) }
	assert blocked_contents == final_sentinel
	assert !pe64_test_path_present(publication_stage_path(blocked))

	stage_blocked := os.join_path(root, 'stage-blocked.exe')
	blocked_stage := publication_stage_path(stage_blocked)
	stage_sentinel := 'stage-sentinel\n'
	os.write_file(blocked_stage, stage_sentinel) or { panic(err) }
	assert pe64_test_publication_error(stage_blocked, bytes) == 'stage `${blocked_stage}` already exists'
	stage_contents := os.read_file(blocked_stage) or { panic(err) }
	assert stage_contents == stage_sentinel
	assert !pe64_test_path_present(stage_blocked)
	os.rm(blocked_stage) or { assert false, err.msg() }
	assert !pe64_test_path_present(blocked_stage)
}

fn (mut capture Pe64TestCapture) append_bounded(pipe os.ChildProcessPipeKind, chunk string) bool {
	match pipe {
		.stdout {
			if chunk.len > pe64_test_output_limit - capture.stdout.len {
				return false
			}
			capture.stdout += chunk
		}
		.stderr {
			if chunk.len > pe64_test_output_limit - capture.stderr.len {
				return false
			}
			capture.stderr += chunk
		}
		else {
			return false
		}
	}

	return true
}

fn pe64_test_drain_process(mut process os.Process, mut capture Pe64TestCapture) bool {
	for {
		mut read_any := false
		if chunk := process.pipe_read(.stdout) {
			read_any = true
			if !capture.append_bounded(.stdout, chunk) {
				return false
			}
		}
		if chunk := process.pipe_read(.stderr) {
			read_any = true
			if !capture.append_bounded(.stderr, chunk) {
				return false
			}
		}
		if !read_any {
			return true
		}
	}
	return true
}

fn pe64_test_run_process(command string, args []string, environment map[string]string, timeout_ms int) Pe64TestProcessResult {
	mut process := os.new_process(command)
	process.use_pgroup = true
	process.set_args(args)
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.run()
	mut elapsed_ms := 0
	mut capture := Pe64TestCapture{}
	mut output_limited := false
	for process.is_alive() && elapsed_ms < timeout_ms && !output_limited {
		if !pe64_test_drain_process(mut process, mut capture) {
			output_limited = true
			break
		}
		time.sleep(20 * time.millisecond)
		elapsed_ms += 20
	}
	still_alive := process.is_alive()
	timed_out := still_alive && elapsed_ms >= timeout_ms
	if still_alive {
		process.signal_pgkill()
	}
	if !output_limited && !pe64_test_drain_process(mut process, mut capture) {
		output_limited = true
	}
	if process.status in [.running, .stopped] {
		process.wait()
	}
	exit_code := if timed_out {
		124
	} else if output_limited {
		125
	} else {
		process.code
	}
	process.close()
	return Pe64TestProcessResult{
		exit_code:      exit_code
		stdout:         capture.stdout
		stderr:         capture.stderr
		timed_out:      timed_out
		output_limited: output_limited
	}
}

fn pe64_test_dumpbin_environment() map[string]string {
	mut environment := {
		'LC_ALL': 'C'
		'LANG':   'C'
	}
	for key in ['PATH', 'SystemRoot', 'SYSTEMROOT', 'TEMP', 'TMP'] {
		value := os.getenv(key)
		if value.len != 0 {
			environment[key] = value
		}
	}
	return environment
}

fn test_pe64_core_windows_dumpbin_headers_and_imports_when_guarded() {
	$if windows && amd64 {
		if os.getenv(pe64_test_dumpbin_guard) != '1' {
			return
		}
		dumpbin := os.getenv(pe64_test_dumpbin_path)
		assert dumpbin.len != 0, '${pe64_test_dumpbin_path} must name an absolute Microsoft DUMPBIN path'
		assert os.is_abs_path(dumpbin), 'DUMPBIN path must be absolute'
		assert os.is_file(dumpbin) && !os.is_link(dumpbin), 'DUMPBIN path must be a regular file'
		mut object := Object.new()
		entry := object.intern_function_symbol('dumpbin_entry') or { panic(err) }
		exit := object.intern_external_function_symbol('dumpbin_external') or { panic(err) }
		body := pe64_test_nonleaf_body()
		assert object.append_text(body) or { panic(err) } == 0
		object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
		object.add_text_call_relocation(5, exit) or { panic(err) }
		bytes := pe64_image_bytes(&object, pe64_test_definition(entry, [
			Pe64ImportBinding{
				symbol_id:   exit
				dll:         'kernel32.dll'
				export_name: 'ExitProcess'
			},
		])) or { panic(err) }
		root := pe64_test_root('dumpbin')
		defer {
			pe64_test_cleanup(root)
		}
		output := os.join_path(root, 'core.exe')
		assert !pe64_test_path_present(output)
		assert !pe64_test_path_present(publication_stage_path(output))
		publish_object(output, bytes) or { assert false, err.msg() }
		result := pe64_test_run_process(dumpbin, ['/HEADERS', '/IMPORTS', output],
			pe64_test_dumpbin_environment(), pe64_test_timeout_ms)
		assert !result.timed_out, 'DUMPBIN timed out'
		assert !result.output_limited, 'DUMPBIN exceeded the output limit'
		assert result.exit_code == 0, result.stderr
		assert result.stdout.contains('PE32+')
		assert result.stdout.contains('machine (x64)') || result.stdout.contains('8664 machine')
		assert result.stdout.contains('kernel32.dll')
		assert result.stdout.contains('ExitProcess')
		assert result.stdout.contains('.pdata')
		assert result.stdout.contains('.xdata')
	} $else {
		return
	}
}

fn test_pe64_runtime_strlen_explicit_binding_resolves_without_import_or_name_inference() {
	object, entry, helper_symbol := pe64_test_runtime_fixture('runtime_entry',
		'opaque_runtime_subject')
	assert object.symbols[int(helper_symbol)].name == 'opaque_runtime_subject'
	assert !object.symbols[int(helper_symbol)].name.contains('strlen')
	definition := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{
			symbol_id: helper_symbol
			helper:    .strlen
		},
	])
	before_text := object.text.clone()
	before_symbols := object.symbols.clone()
	before_relocations := object.call_relocations.clone()
	before_runtime := definition.runtime_helpers.clone()
	data := pe64_image_bytes(&object, definition) or { panic(err) }
	sections := pe64_test_sections(data)
	assert sections.map(it.name) == ['.text', '.pdata', '.xdata']
	text := sections[0]
	assert text.virtual_size == u32(object.text.len) + u32(pe64_runtime_strlen_size)
	assert pe64_test_read_u32(data, int(text.raw_pointer) + 5) == u32(6)
	helper_offset := int(text.raw_pointer) + object.text.len
	pe64_test_assert_runtime_strlen_body(data[helper_offset..helper_offset +
		int(pe64_runtime_strlen_size)])
	assert pe64_test_directory(data, pe64_import_directory_index) == Pe64TestDirectory{}
	assert pe64_test_directory(data, pe64_iat_directory_index) == Pe64TestDirectory{}
	assert object.text == before_text
	assert object.symbols == before_symbols
	assert object.call_relocations == before_relocations
	assert definition.runtime_helpers == before_runtime
}

fn test_pe64_runtime_strlen_leaf_preserves_stage0_unwind_and_is_deterministic() {
	first_object, first_entry, first_a, first_b, first_c := pe64_test_alias_fixture(false, [
		'runtime_alias_a',
		'runtime_alias_b',
		'runtime_alias_c',
	])
	second_object, second_entry, second_a, second_b, second_c := pe64_test_alias_fixture(true, [
		'runtime_alias_a',
		'runtime_alias_b',
		'runtime_alias_c',
	])
	first_definition := pe64_test_runtime_definition(first_entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: first_c, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: first_b, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: first_a, helper: .strlen },
	])
	second_definition := pe64_test_runtime_definition(second_entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: second_a, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: second_b, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: second_c, helper: .strlen },
	])
	before_text := first_object.text.clone()
	before_symbols := first_object.symbols.clone()
	before_relocations := first_object.call_relocations.clone()
	before_runtime := first_definition.runtime_helpers.clone()
	second_before_text := second_object.text.clone()
	second_before_symbols := second_object.symbols.clone()
	second_before_relocations := second_object.call_relocations.clone()
	second_before_runtime := second_definition.runtime_helpers.clone()
	mut first := pe64_image_bytes(&first_object, first_definition) or { panic(err) }
	second := pe64_image_bytes(&second_object, second_definition) or { panic(err) }
	assert first == second
	text := pe64_test_section(second, '.text')
	assert text.virtual_size == u32(first_object.text.len) + u32(pe64_runtime_strlen_size)
	assert pe64_test_read_u32(second, int(text.raw_pointer) + 5) == u32(16)
	assert pe64_test_read_u32(second, int(text.raw_pointer) + 10) == u32(11)
	assert pe64_test_read_u32(second, int(text.raw_pointer) + 15) == u32(6)
	helper_offset := int(text.raw_pointer) + first_object.text.len
	pe64_test_assert_runtime_strlen_body(second[helper_offset..helper_offset +
		int(pe64_runtime_strlen_size)])
	pdata := pe64_test_section(second, '.pdata')
	xdata := pe64_test_section(second, '.xdata')
	assert pdata.virtual_size == u32(pe64_runtime_function_size)
	assert xdata.virtual_size == u32(pe64_unwind_info_size)
	assert pe64_test_read_u32(second, int(pdata.raw_pointer)) == text.virtual_address
	assert pe64_test_read_u32(second, int(pdata.raw_pointer) + 4) == text.virtual_address +
		u32(first_object.text.len)
	assert first_object.text == before_text
	assert first_object.symbols == before_symbols
	assert first_object.call_relocations == before_relocations
	assert first_definition.runtime_helpers == before_runtime
	assert second_object.text == second_before_text
	assert second_object.symbols == second_before_symbols
	assert second_object.call_relocations == second_before_relocations
	assert second_definition.runtime_helpers == second_before_runtime
	first[0] = 0
	assert second[0] == `M`
}

fn test_pe64_runtime_strlen_coexists_with_explicit_import_and_deduplicates_aliases() {
	object, entry, first_runtime, second_runtime, imported := pe64_test_alias_fixture(false, [
		'first_runtime_subject',
		'second_runtime_subject',
		'import_subject',
	])
	definition := pe64_test_runtime_definition(entry, [
		Pe64ImportBinding{
			symbol_id:   imported
			dll:         'kernel32.dll'
			export_name: 'ExitProcess'
		},
	], [
		Pe64RuntimeBinding{ symbol_id: second_runtime, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: first_runtime, helper: .strlen },
	])
	data := pe64_image_bytes(&object, definition) or { panic(err) }
	sections := pe64_test_sections(data)
	assert sections.map(it.name) == ['.text', '.pdata', '.xdata', '.idata']
	text := sections[0]
	helper_start := object.text.len
	thunk_start := helper_start + int(pe64_runtime_strlen_size)
	assert text.virtual_size == u32(thunk_start + int(pe64_import_thunk_size))
	assert pe64_test_read_u32(data, int(text.raw_pointer) + 5) == u32(16)
	assert pe64_test_read_u32(data, int(text.raw_pointer) + 10) == u32(11)
	assert pe64_test_read_u32(data, int(text.raw_pointer) + 15) == u32(23)
	pe64_test_assert_runtime_strlen_body(data[int(text.raw_pointer) + helper_start..
		int(text.raw_pointer) + thunk_start])
	assert data[int(text.raw_pointer) + thunk_start..int(text.raw_pointer) + thunk_start + 2] == [
		u8(0xff),
		0x25,
	]
	idata := sections[3]
	iat_rva := pe64_test_read_u32(data, int(idata.raw_pointer) + 16)
	thunk_field_offset := thunk_start + 2
	thunk_field_rva := text.virtual_address + u32(thunk_field_offset)
	expected_iat_displacement := pe64_checked_rel32(u64(thunk_field_rva), u64(iat_rva)) or {
		panic(err)
	}
	assert pe64_test_read_u32(data, int(text.raw_pointer) + thunk_field_offset) == expected_iat_displacement
	assert pe64_test_directory(data, pe64_import_directory_index).size == 40
	assert pe64_test_directory(data, pe64_iat_directory_index).size == 16
}

fn test_pe64_runtime_strlen_refuses_missing_duplicate_conflicting_and_invalid_symbol_bindings() {
	object, entry, runtime_symbol := pe64_test_runtime_fixture('refusal_entry', 'strlen')
	before_text := object.text.clone()
	before_symbols := object.symbols.clone()
	before_relocations := object.call_relocations.clone()
	assert pe64_test_error(&object, pe64_test_definition(entry, []Pe64ImportBinding{})).contains('PE64 import binding missing for SymbolID')
	assert pe64_test_error(&object, pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: SymbolID(99), helper: .strlen },
	])).contains('PE64 runtime binding: SymbolID 99 is out of range')
	assert pe64_test_error(&object, pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: entry, helper: .strlen },
	])).contains('PE64 runtime binding: SymbolID ${u64(entry)} is not an external function')
	assert pe64_test_error(&object, pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .strlen },
	])).contains('PE64 runtime binding: duplicate binding')
	conflicting_definition := pe64_test_runtime_definition(entry, [
		Pe64ImportBinding{
			symbol_id:   runtime_symbol
			dll:         'kernel32.dll'
			export_name: 'ExitProcess'
		},
	], [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .strlen },
	])
	before_conflicting_imports := conflicting_definition.imports.clone()
	before_conflicting_runtime := conflicting_definition.runtime_helpers.clone()
	assert pe64_test_error(&object, conflicting_definition).contains('has both import and runtime bindings')
	assert conflicting_definition.imports == before_conflicting_imports
	assert conflicting_definition.runtime_helpers == before_conflicting_runtime

	mut unreferenced_object, unreferenced_entry, referenced_symbol := pe64_test_runtime_fixture('unreferenced_entry',
		'referenced_runtime')
	unreferenced_symbol := unreferenced_object.intern_external_function_symbol('unreferenced_runtime') or {
		panic(err)
	}
	assert pe64_test_error(&unreferenced_object, pe64_test_runtime_definition(unreferenced_entry,
		[]Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: referenced_symbol, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: unreferenced_symbol, helper: .strlen },
	])).contains('PE64 object contract: AMD64 object external function unreferenced_runtime has no CALL relocation')

	mut malformed_object := pe64_test_clone_object(&object)
	malformed_object.text[6] = 1
	assert pe64_test_error(&malformed_object, pe64_test_runtime_definition(entry,
		[]Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .strlen },
	])).contains('PE64 object contract')
	assert object.text == before_text
	assert object.symbols == before_symbols
	assert object.call_relocations == before_relocations
}

fn test_pe64_runtime_strlen_non_microsoft_abi_precedes_invalid_helper_and_never_publishes() {
	object, entry, runtime_symbol := pe64_test_runtime_fixture('abi_precedence_entry',
		'abi_precedence_runtime')
	definition := Pe64ImageDefinition{
		target_abi:      .unknown
		subsystem:       .windows_cui
		image_policy:    .fixed_base
		entry:           Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         .raw_noreturn_process_entry
		}
		runtime_helpers: [
			Pe64RuntimeBinding{
				symbol_id: runtime_symbol
				helper:    .unknown
			},
		]
	}
	root := pe64_test_root('runtime-abi-precedence')
	defer {
		pe64_test_cleanup(root)
	}
	final_path := os.join_path(root, 'abi-precedence.exe')
	stage_path := publication_stage_path(final_path)
	assert pe64_test_build_then_publish_error(&object, definition, final_path) == 'PE64 requires Microsoft x64 ABI'
	assert !pe64_test_path_present(final_path)
	assert !pe64_test_path_present(stage_path)
}

fn test_pe64_runtime_strlen_refuses_unknown_helper_without_publication() {
	object, entry, runtime_symbol := pe64_test_runtime_fixture('unknown_helper_entry',
		'unknown_helper_runtime')
	definition := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .unknown },
	])
	root := pe64_test_root('runtime-unknown-helper')
	defer {
		pe64_test_cleanup(root)
	}
	final_path := os.join_path(root, 'unknown-helper.exe')
	stage_path := publication_stage_path(final_path)
	assert pe64_test_build_then_publish_error(&object, definition, final_path) == 'PE64 runtime binding: helper 0 is unsupported'
	assert !pe64_test_path_present(final_path)
	assert !pe64_test_path_present(stage_path)
}

fn test_pe64_runtime_strlen_refuses_out_of_domain_helper_without_publication() {
	object, entry, runtime_symbol := pe64_test_runtime_fixture('forged_helper_entry',
		'forged_helper_runtime')
	definition := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{
			symbol_id: runtime_symbol
			helper:    unsafe { Pe64RuntimeHelperKind(255) }
		},
	])
	root := pe64_test_root('runtime-forged-helper')
	defer {
		pe64_test_cleanup(root)
	}
	final_path := os.join_path(root, 'forged-helper.exe')
	stage_path := publication_stage_path(final_path)
	assert pe64_test_build_then_publish_error(&object, definition, final_path) == 'PE64 runtime binding: helper 255 is unsupported'
	assert !pe64_test_path_present(final_path)
	assert !pe64_test_path_present(stage_path)
}

fn test_pe64_runtime_strlen_dumpbin_disassembly_when_guarded() {
	$if windows && amd64 {
		if os.getenv(pe64_test_dumpbin_guard) != '1' {
			return
		}
		dumpbin := os.getenv(pe64_test_dumpbin_path)
		assert dumpbin.len != 0, '${pe64_test_dumpbin_path} must name an absolute Microsoft DUMPBIN path'
		assert os.is_abs_path(dumpbin), 'DUMPBIN path must be absolute'
		assert os.is_file(dumpbin) && !os.is_link(dumpbin), 'DUMPBIN path must be a regular file'
		object, entry, runtime_symbol := pe64_test_runtime_fixture('dumpbin_runtime_entry',
			'dumpbin_runtime_subject')
		bytes := pe64_image_bytes(&object, pe64_test_runtime_definition(entry,
			[]Pe64ImportBinding{}, [
			Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .strlen },
		])) or { panic(err) }
		text := pe64_test_section(bytes, '.text')
		assert pe64_test_read_u32(bytes, int(text.raw_pointer) + 5) == u32(6)
		helper_rva := text.virtual_address + u32(object.text.len)
		helper_offset := pe64_test_rva_offset(bytes, helper_rva)
		pe64_test_assert_runtime_strlen_body(bytes[helper_offset..helper_offset +
			int(pe64_runtime_strlen_size)])
		root := pe64_test_root('runtime-dumpbin')
		defer {
			pe64_test_cleanup(root)
		}
		output := os.join_path(root, 'runtime.exe')
		publish_object(output, bytes) or { assert false, err.msg() }
		result := pe64_test_run_process(dumpbin, ['/DISASM:BYTES', output],
			pe64_test_dumpbin_environment(), pe64_test_timeout_ms)
		assert !result.timed_out, 'DUMPBIN timed out'
		assert !result.output_limited, 'DUMPBIN exceeded the output limit'
		assert result.exit_code == 0, result.stderr
		instructions := pe64_test_dumpbin_instructions(result.stdout)
		helper_address := pe64_checked_add(pe64_image_base, u64(helper_rva),
			'DUMPBIN helper address') or { panic(err) }
		caller_address := pe64_checked_add(pe64_image_base, u64(text.virtual_address),
			'DUMPBIN caller address') or { panic(err) }
		assert helper_address == caller_address + u64(object.text.len)
		assert helper_address != caller_address
		mut helper_index := -1
		for index, instruction in instructions {
			if instruction.address == helper_address {
				assert helper_index == -1, 'DUMPBIN repeated the helper start address'
				helper_index = index
			}
		}
		assert helper_index >= 0, 'DUMPBIN omitted the addressed helper start'
		assert helper_index + 7 <= instructions.len, 'DUMPBIN truncated the addressed helper'
		helper := instructions[helper_index..helper_index + 7]
		assert helper[0].address == helper_address
		assert helper[0].bytes == [u8(0x48), 0x89, 0xc8]
		assert helper[0].mnemonic == 'mov'
		assert pe64_test_compact_operands(helper[0].operands) == 'rax,rcx'
		assert helper[1].address == helper_address + 3
		assert helper[1].bytes == [u8(0x80), 0x38, 0x00]
		assert helper[1].mnemonic == 'cmp'
		cmp_operands := pe64_test_compact_operands(helper[1].operands)
		assert cmp_operands in ['byteptr[rax],0', 'byteptr[rax],0h', 'byteptr[rax],00h']
		assert helper[2].address == helper_address + 6
		assert helper[2].bytes == [u8(0x74), 0x05]
		assert helper[2].mnemonic in ['je', 'jz']
		assert pe64_test_dumpbin_operand_has_address(helper[2].operands, helper_address + 13)
		assert helper[3].address == helper_address + 8
		assert helper[3].bytes == [u8(0x48), 0xff, 0xc0]
		assert helper[3].mnemonic == 'inc'
		assert pe64_test_compact_operands(helper[3].operands) == 'rax'
		assert helper[4].address == helper_address + 11
		assert helper[4].bytes == [u8(0xeb), 0xf6]
		assert helper[4].mnemonic == 'jmp'
		assert pe64_test_dumpbin_operand_has_address(helper[4].operands, helper_address + 3)
		assert helper[5].address == helper_address + 13
		assert helper[5].bytes == [u8(0x48), 0x29, 0xc8]
		assert helper[5].mnemonic == 'sub'
		assert pe64_test_compact_operands(helper[5].operands) == 'rax,rcx'
		assert helper[6].address == helper_address + 16
		assert helper[6].bytes == [u8(0xc3)]
		assert helper[6].mnemonic == 'ret'
		assert helper[6].operands == ''
	} $else {
		return
	}
}

fn test_pe64_runtime_wcslen_explicit_binding_emits_exact_leaf_without_name_inference() {
	object, entry, runtime_symbol := pe64_test_runtime_fixture('wcslen_entry',
		'opaque_wide_runtime')
	assert !object.symbols[int(runtime_symbol)].name.contains('wcslen')
	definition := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .wcslen },
	])
	before_text := object.text.clone()
	before_symbols := object.symbols.clone()
	before_relocations := object.call_relocations.clone()
	before_runtime := definition.runtime_helpers.clone()
	data := pe64_image_bytes(&object, definition) or { panic(err) }
	sections := pe64_test_sections(data)
	assert sections.map(it.name) == ['.text', '.pdata', '.xdata']
	text := sections[0]
	helper_start := object.text.len
	assert text.virtual_size == u32(helper_start + int(pe64_runtime_wcslen_size))
	expected_call := pe64_checked_rel32(5, u64(helper_start)) or { panic(err) }
	assert pe64_test_read_u32(data, int(text.raw_pointer) + 5) == expected_call
	helper_offset := int(text.raw_pointer) + helper_start
	pe64_test_assert_runtime_wcslen_body(data[helper_offset..helper_offset +
		int(pe64_runtime_wcslen_size)])
	pdata := sections[1]
	xdata := sections[2]
	assert pdata.virtual_size == u32(pe64_runtime_function_size)
	assert xdata.virtual_size == u32(pe64_unwind_info_size)
	assert pe64_test_read_u32(data, int(pdata.raw_pointer)) == text.virtual_address
	assert pe64_test_read_u32(data, int(pdata.raw_pointer) + 4) == text.virtual_address +
		u32(object.text.len)
	assert pe64_test_directory(data, pe64_import_directory_index) == Pe64TestDirectory{}
	assert pe64_test_directory(data, pe64_iat_directory_index) == Pe64TestDirectory{}
	assert object.text == before_text
	assert object.symbols == before_symbols
	assert object.call_relocations == before_relocations
	assert definition.runtime_helpers == before_runtime
}

fn test_pe64_runtime_wcslen_deduplicates_and_orders_after_strlen_deterministically() {
	first_object, first_entry, first_strlen_a, first_strlen_b, first_wcslen_a, first_wcslen_b, first_imported :=
		pe64_test_runtime_order_fixture(false)
	second_object, second_entry, second_strlen_a, second_strlen_b, second_wcslen_a, second_wcslen_b, second_imported :=
		pe64_test_runtime_order_fixture(true)
	first_definition := pe64_test_runtime_definition(first_entry, [
		Pe64ImportBinding{
			symbol_id:   first_imported
			dll:         'kernel32.dll'
			export_name: 'ExitProcess'
		},
	], [
		Pe64RuntimeBinding{ symbol_id: first_wcslen_b, helper: .wcslen },
		Pe64RuntimeBinding{ symbol_id: first_strlen_b, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: first_wcslen_a, helper: .wcslen },
		Pe64RuntimeBinding{ symbol_id: first_strlen_a, helper: .strlen },
	])
	second_definition := pe64_test_runtime_definition(second_entry, [
		Pe64ImportBinding{
			symbol_id:   second_imported
			dll:         'kernel32.dll'
			export_name: 'ExitProcess'
		},
	], [
		Pe64RuntimeBinding{ symbol_id: second_strlen_a, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: second_wcslen_a, helper: .wcslen },
		Pe64RuntimeBinding{ symbol_id: second_strlen_b, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: second_wcslen_b, helper: .wcslen },
	])
	first_before_text := first_object.text.clone()
	first_before_symbols := first_object.symbols.clone()
	first_before_relocations := first_object.call_relocations.clone()
	first_before_imports := first_definition.imports.clone()
	first_before_runtime := first_definition.runtime_helpers.clone()
	second_before_text := second_object.text.clone()
	second_before_symbols := second_object.symbols.clone()
	second_before_relocations := second_object.call_relocations.clone()
	second_before_imports := second_definition.imports.clone()
	second_before_runtime := second_definition.runtime_helpers.clone()
	mut first := pe64_image_bytes(&first_object, first_definition) or { panic(err) }
	second := pe64_image_bytes(&second_object, second_definition) or { panic(err) }
	assert first == second
	sections := pe64_test_sections(second)
	assert sections.map(it.name) == ['.text', '.pdata', '.xdata', '.idata']
	text := sections[0]
	strlen_start := first_object.text.len
	wcslen_start := strlen_start + int(pe64_runtime_strlen_size)
	thunk_start := wcslen_start + int(pe64_runtime_wcslen_size)
	assert text.virtual_size == u32(thunk_start + int(pe64_import_thunk_size))
	pe64_test_assert_runtime_strlen_body(second[int(text.raw_pointer) + strlen_start..
		int(text.raw_pointer) + wcslen_start])
	pe64_test_assert_runtime_wcslen_body(second[int(text.raw_pointer) + wcslen_start..
		int(text.raw_pointer) + thunk_start])
	for field in [5, 10] {
		expected := pe64_checked_rel32(u64(field), u64(strlen_start)) or { panic(err) }
		assert pe64_test_read_u32(second, int(text.raw_pointer) + field) == expected
	}
	for field in [15, 20] {
		expected := pe64_checked_rel32(u64(field), u64(wcslen_start)) or { panic(err) }
		assert pe64_test_read_u32(second, int(text.raw_pointer) + field) == expected
	}
	expected_import_call := pe64_checked_rel32(25, u64(thunk_start)) or { panic(err) }
	assert pe64_test_read_u32(second, int(text.raw_pointer) + 25) == expected_import_call
	assert second[int(text.raw_pointer) + thunk_start..int(text.raw_pointer) + thunk_start + 2] == [
		u8(0xff),
		0x25,
	]
	idata := sections[3]
	iat_rva := pe64_test_read_u32(second, int(idata.raw_pointer) + 16)
	thunk_field_offset := thunk_start + 2
	thunk_field_rva := text.virtual_address + u32(thunk_field_offset)
	expected_iat_displacement := pe64_checked_rel32(u64(thunk_field_rva), u64(iat_rva)) or {
		panic(err)
	}
	assert pe64_test_read_u32(second, int(text.raw_pointer) + thunk_field_offset) == expected_iat_displacement
	pdata := sections[1]
	assert pdata.virtual_size == u32(pe64_runtime_function_size)
	assert pe64_test_read_u32(second, int(pdata.raw_pointer) + 4) == text.virtual_address +
		u32(first_object.text.len)
	assert first_object.text == first_before_text
	assert first_object.symbols == first_before_symbols
	assert first_object.call_relocations == first_before_relocations
	assert first_definition.imports == first_before_imports
	assert first_definition.runtime_helpers == first_before_runtime
	assert second_object.text == second_before_text
	assert second_object.symbols == second_before_symbols
	assert second_object.call_relocations == second_before_relocations
	assert second_definition.imports == second_before_imports
	assert second_definition.runtime_helpers == second_before_runtime
	first[0] = 0
	assert second[0] == `M`
}

fn test_pe64_runtime_wcslen_requires_explicit_binding_and_preserves_refusal_precedence() {
	object, entry, runtime_symbol := pe64_test_runtime_fixture('wcslen_refusal_entry', 'wcslen')
	before_text := object.text.clone()
	before_symbols := object.symbols.clone()
	before_relocations := object.call_relocations.clone()
	root := pe64_test_root('runtime-wcslen-refusal')
	defer {
		pe64_test_cleanup(root)
	}

	unbound_path := os.join_path(root, 'unbound.exe')
	unbound_error := pe64_test_build_then_publish_error(&object, pe64_test_definition(entry,
		[]Pe64ImportBinding{}), unbound_path)
	assert unbound_error.contains('PE64 import binding missing for SymbolID')

	non_microsoft := Pe64ImageDefinition{
		target_abi:      .unknown
		subsystem:       .windows_cui
		image_policy:    .fixed_base
		entry:           Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         .raw_noreturn_process_entry
		}
		runtime_helpers: [
			Pe64RuntimeBinding{
				symbol_id: runtime_symbol
				helper:    .wcslen
			},
		]
	}
	non_microsoft_before := non_microsoft.runtime_helpers.clone()
	abi_path := os.join_path(root, 'abi.exe')
	assert pe64_test_build_then_publish_error(&object, non_microsoft, abi_path) == 'PE64 requires Microsoft x64 ABI'
	assert non_microsoft.runtime_helpers == non_microsoft_before

	conflicting := pe64_test_runtime_definition(entry, [
		Pe64ImportBinding{
			symbol_id:   runtime_symbol
			dll:         'kernel32.dll'
			export_name: 'ExitProcess'
		},
	], [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .wcslen },
	])
	conflicting_imports_before := conflicting.imports.clone()
	conflicting_runtime_before := conflicting.runtime_helpers.clone()
	conflict_path := os.join_path(root, 'conflict.exe')
	assert pe64_test_build_then_publish_error(&object, conflicting, conflict_path).contains('has both import and runtime bindings')
	assert conflicting.imports == conflicting_imports_before
	assert conflicting.runtime_helpers == conflicting_runtime_before

	mut malformed_object := pe64_test_clone_object(&object)
	malformed_object.text[6] = 1
	malformed_definition := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .wcslen },
	])
	malformed_runtime_before := malformed_definition.runtime_helpers.clone()
	malformed_path := os.join_path(root, 'malformed.exe')
	assert pe64_test_build_then_publish_error(&malformed_object, malformed_definition,
		malformed_path).contains('PE64 object contract')
	assert malformed_definition.runtime_helpers == malformed_runtime_before
	assert object.text == before_text
	assert object.symbols == before_symbols
	assert object.call_relocations == before_relocations
}

fn test_pe64_runtime_wcslen_dumpbin_disassembly_when_guarded() {
	$if windows && amd64 {
		if os.getenv(pe64_test_dumpbin_guard) != '1' {
			return
		}
		dumpbin := os.getenv(pe64_test_dumpbin_path)
		assert dumpbin.len != 0, '${pe64_test_dumpbin_path} must name an absolute Microsoft DUMPBIN path'
		assert os.is_abs_path(dumpbin), 'DUMPBIN path must be absolute'
		assert os.is_file(dumpbin) && !os.is_link(dumpbin), 'DUMPBIN path must be a regular file'
		object, entry, runtime_symbol := pe64_test_runtime_fixture('dumpbin_wcslen_entry',
			'dumpbin_wide_runtime')
		bytes := pe64_image_bytes(&object, pe64_test_runtime_definition(entry,
			[]Pe64ImportBinding{}, [
			Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .wcslen },
		])) or { panic(err) }
		text := pe64_test_section(bytes, '.text')
		helper_rva := text.virtual_address + u32(object.text.len)
		helper_offset := pe64_test_rva_offset(bytes, helper_rva)
		pe64_test_assert_runtime_wcslen_body(bytes[helper_offset..helper_offset +
			int(pe64_runtime_wcslen_size)])
		root := pe64_test_root('runtime-wcslen-dumpbin')
		defer {
			pe64_test_cleanup(root)
		}
		output := os.join_path(root, 'runtime-wcslen.exe')
		publish_object(output, bytes) or { assert false, err.msg() }
		result := pe64_test_run_process(dumpbin, ['/DISASM:BYTES', output],
			pe64_test_dumpbin_environment(), pe64_test_timeout_ms)
		assert !result.timed_out, 'DUMPBIN timed out'
		assert !result.output_limited, 'DUMPBIN exceeded the output limit'
		assert result.exit_code == 0, result.stderr
		instructions := pe64_test_dumpbin_instructions(result.stdout)
		helper_address := pe64_checked_add(pe64_image_base, u64(helper_rva),
			'DUMPBIN wcslen helper address') or { panic(err) }
		mut helper_index := -1
		for index, instruction in instructions {
			if instruction.address == helper_address {
				assert helper_index == -1, 'DUMPBIN repeated the wcslen helper start address'
				helper_index = index
			}
		}
		assert helper_index >= 0, 'DUMPBIN omitted the addressed wcslen helper start'
		assert helper_index + 8 <= instructions.len, 'DUMPBIN truncated the addressed wcslen helper'
		helper := instructions[helper_index..helper_index + 8]
		assert helper[0].address == helper_address
		assert helper[0].bytes == [u8(0x48), 0x89, 0xc8]
		assert helper[0].mnemonic == 'mov'
		assert pe64_test_compact_operands(helper[0].operands) == 'rax,rcx'
		assert helper[1].address == helper_address + 3
		assert helper[1].bytes == [u8(0x66), 0x83, 0x38, 0x00]
		assert helper[1].mnemonic == 'cmp'
		cmp_operands := pe64_test_compact_operands(helper[1].operands)
		assert cmp_operands in ['wordptr[rax],0', 'wordptr[rax],0h', 'wordptr[rax],00h']
		assert helper[2].address == helper_address + 7
		assert helper[2].bytes == [u8(0x74), 0x06]
		assert helper[2].mnemonic in ['je', 'jz']
		assert pe64_test_dumpbin_operand_has_address(helper[2].operands, helper_address + 15)
		assert helper[3].address == helper_address + 9
		assert helper[3].bytes == [u8(0x48), 0x83, 0xc0, 0x02]
		assert helper[3].mnemonic == 'add'
		assert pe64_test_compact_operands(helper[3].operands) in ['rax,2', 'rax,2h', 'rax,02h']
		assert helper[4].address == helper_address + 13
		assert helper[4].bytes == [u8(0xeb), 0xf4]
		assert helper[4].mnemonic == 'jmp'
		assert pe64_test_dumpbin_operand_has_address(helper[4].operands, helper_address + 3)
		assert helper[5].address == helper_address + 15
		assert helper[5].bytes == [u8(0x48), 0x29, 0xc8]
		assert helper[5].mnemonic == 'sub'
		assert pe64_test_compact_operands(helper[5].operands) == 'rax,rcx'
		assert helper[6].address == helper_address + 18
		assert helper[6].bytes == [u8(0x48), 0xd1, 0xe8]
		assert helper[6].mnemonic == 'shr'
		assert pe64_test_compact_operands(helper[6].operands) in ['rax,1', 'rax,1h', 'rax,01h']
		assert helper[7].address == helper_address + 21
		assert helper[7].bytes == [u8(0xc3)]
		assert helper[7].mnemonic == 'ret'
		assert helper[7].operands == ''
	} $else {
		return
	}
}

fn test_pe64_runtime_memset_explicit_binding_emits_exact_leaf_without_name_inference() {
	object, entry, runtime_symbol := pe64_test_runtime_fixture('memset_entry',
		'opaque_byte_fill_runtime')
	assert !object.symbols[int(runtime_symbol)].name.contains('memset')
	definition := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memset },
	])
	before_text := object.text.clone()
	before_symbols := object.symbols.clone()
	before_relocations := object.call_relocations.clone()
	before_runtime := definition.runtime_helpers.clone()
	data := pe64_image_bytes(&object, definition) or { panic(err) }
	sections := pe64_test_sections(data)
	assert sections.map(it.name) == ['.text', '.pdata', '.xdata']
	text := sections[0]
	helper_start := object.text.len
	assert text.virtual_size == u32(helper_start + int(pe64_runtime_memset_size))
	expected_call := pe64_checked_rel32(5, u64(helper_start)) or { panic(err) }
	assert pe64_test_read_u32(data, int(text.raw_pointer) + 5) == expected_call
	helper_offset := int(text.raw_pointer) + helper_start
	helper_body := data[helper_offset..helper_offset + int(pe64_runtime_memset_size)]
	pe64_test_assert_runtime_memset_body(helper_body)

	memory_base := u64(0x1000)
	mut nonzero := Pe64TestMemsetAbiState{
		rax:                 0xaaaa_aaaa_aaaa_aaaa
		rcx:                 memory_base + 2
		rdx:                 0x8877_6655_4433_22a5
		r8:                  3
		r10:                 0xbbbb_bbbb_bbbb_bbbb
		rsp:                 0x7fff_ffff_f000
		nonvolatile_gprs:    [u64(0x11), 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88]
		nonvolatile_vectors: [u64(0x106), 0x107, 0x108, 0x109, 0x110, 0x111, 0x112, 0x113, 0x114,
			0x115]
		df:                  true
		memory:              [u8(0x31), 0x32, 0x33, 0x34, 0x35, 0x36, 0x37]
	}
	nonzero_initial_rax := nonzero.rax
	nonzero_initial_rcx := nonzero.rcx
	nonzero_initial_rdx := nonzero.rdx
	nonzero_initial_r8 := nonzero.r8
	nonzero_initial_r10 := nonzero.r10
	nonzero_initial_rsp := nonzero.rsp
	nonzero_initial_gprs := nonzero.nonvolatile_gprs.clone()
	nonzero_initial_vectors := nonzero.nonvolatile_vectors.clone()
	nonzero_initial_df := nonzero.df
	pe64_test_execute_runtime_memset_manifest(helper_body, memory_base, mut nonzero)
	assert nonzero.rax == nonzero_initial_rcx
	assert nonzero.rax != nonzero_initial_rax
	assert nonzero.r8 == 0 && nonzero.r8 != nonzero_initial_r8
	assert nonzero.r10 == nonzero_initial_rcx + nonzero_initial_r8
	assert nonzero.r10 != nonzero_initial_r10
	assert nonzero.status_flags_written
	assert nonzero.rcx == nonzero_initial_rcx
	assert nonzero.rdx == nonzero_initial_rdx
	assert u8(nonzero.rdx & 0xff) == 0xa5
	assert nonzero.rsp == nonzero_initial_rsp
	assert nonzero.nonvolatile_gprs == nonzero_initial_gprs
	assert nonzero.nonvolatile_vectors == nonzero_initial_vectors
	assert nonzero.df == nonzero_initial_df
	assert nonzero.memory == [u8(0x31), 0x32, 0xa5, 0xa5, 0xa5, 0x36, 0x37]

	mut zero := Pe64TestMemsetAbiState{
		rax:                 0xcccc_cccc_cccc_cccc
		rcx:                 memory_base + 1
		rdx:                 0xffee_ddcc_bbaa_995a
		r8:                  0
		r10:                 0xdddd_dddd_dddd_dddd
		rsp:                 0x7fff_ffff_e000
		nonvolatile_gprs:    [u64(0x91), 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98]
		nonvolatile_vectors: [u64(0x206), 0x207, 0x208, 0x209, 0x210, 0x211, 0x212, 0x213, 0x214,
			0x215]
		df:                  true
		memory:              [u8(0x41), 0x42, 0x43]
	}
	zero_initial_rax := zero.rax
	zero_initial_rcx := zero.rcx
	zero_initial_rdx := zero.rdx
	zero_initial_r10 := zero.r10
	zero_initial_rsp := zero.rsp
	zero_initial_gprs := zero.nonvolatile_gprs.clone()
	zero_initial_vectors := zero.nonvolatile_vectors.clone()
	zero_initial_df := zero.df
	zero_initial_memory := zero.memory.clone()
	pe64_test_execute_runtime_memset_manifest(helper_body, memory_base, mut zero)
	assert zero.rax == zero_initial_rcx
	assert zero.rax != zero_initial_rax
	assert zero.r8 == 0
	assert zero.r10 == zero_initial_r10
	assert zero.status_flags_written
	assert zero.rcx == zero_initial_rcx
	assert zero.rdx == zero_initial_rdx
	assert zero.rsp == zero_initial_rsp
	assert zero.nonvolatile_gprs == zero_initial_gprs
	assert zero.nonvolatile_vectors == zero_initial_vectors
	assert zero.df == zero_initial_df
	assert zero.memory == zero_initial_memory

	pdata := sections[1]
	xdata := sections[2]
	assert pdata.virtual_size == u32(pe64_runtime_function_size)
	assert xdata.virtual_size == u32(pe64_unwind_info_size)
	assert pe64_test_read_u32(data, int(pdata.raw_pointer)) == text.virtual_address
	assert pe64_test_read_u32(data, int(pdata.raw_pointer) + 4) == text.virtual_address +
		u32(object.text.len)
	assert pe64_test_directory(data, pe64_import_directory_index) == Pe64TestDirectory{}
	assert pe64_test_directory(data, pe64_iat_directory_index) == Pe64TestDirectory{}
	assert object.text == before_text
	assert object.symbols == before_symbols
	assert object.call_relocations == before_relocations
	assert definition.runtime_helpers == before_runtime
}

fn test_pe64_runtime_memset_deduplicates_and_orders_after_string_helpers() {
	first_object, first_entry, first_strlen_a, first_strlen_b, first_wcslen_a, first_wcslen_b, first_memset_a, first_memset_b, first_imported :=
		pe64_test_runtime_memset_order_fixture(false)
	second_object, second_entry, second_strlen_a, second_strlen_b, second_wcslen_a, second_wcslen_b, second_memset_a, second_memset_b, second_imported :=
		pe64_test_runtime_memset_order_fixture(true)
	first_definition := pe64_test_runtime_definition(first_entry, [
		Pe64ImportBinding{
			symbol_id:   first_imported
			dll:         'kernel32.dll'
			export_name: 'ExitProcess'
		},
	], [
		Pe64RuntimeBinding{ symbol_id: first_memset_b, helper: .memset },
		Pe64RuntimeBinding{ symbol_id: first_wcslen_b, helper: .wcslen },
		Pe64RuntimeBinding{ symbol_id: first_strlen_b, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: first_memset_a, helper: .memset },
		Pe64RuntimeBinding{ symbol_id: first_wcslen_a, helper: .wcslen },
		Pe64RuntimeBinding{ symbol_id: first_strlen_a, helper: .strlen },
	])
	second_definition := pe64_test_runtime_definition(second_entry, [
		Pe64ImportBinding{
			symbol_id:   second_imported
			dll:         'kernel32.dll'
			export_name: 'ExitProcess'
		},
	], [
		Pe64RuntimeBinding{ symbol_id: second_strlen_a, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: second_memset_a, helper: .memset },
		Pe64RuntimeBinding{ symbol_id: second_wcslen_a, helper: .wcslen },
		Pe64RuntimeBinding{ symbol_id: second_strlen_b, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: second_memset_b, helper: .memset },
		Pe64RuntimeBinding{ symbol_id: second_wcslen_b, helper: .wcslen },
	])
	first_before_text := first_object.text.clone()
	first_before_symbols := first_object.symbols.clone()
	first_before_relocations := first_object.call_relocations.clone()
	first_before_imports := first_definition.imports.clone()
	first_before_runtime := first_definition.runtime_helpers.clone()
	second_before_text := second_object.text.clone()
	second_before_symbols := second_object.symbols.clone()
	second_before_relocations := second_object.call_relocations.clone()
	second_before_imports := second_definition.imports.clone()
	second_before_runtime := second_definition.runtime_helpers.clone()
	mut first := pe64_image_bytes(&first_object, first_definition) or { panic(err) }
	second := pe64_image_bytes(&second_object, second_definition) or { panic(err) }
	assert first == second
	sections := pe64_test_sections(second)
	assert sections.map(it.name) == ['.text', '.pdata', '.xdata', '.idata']
	text := sections[0]
	strlen_start := first_object.text.len
	wcslen_start := strlen_start + int(pe64_runtime_strlen_size)
	memset_start := wcslen_start + int(pe64_runtime_wcslen_size)
	thunk_start := memset_start + int(pe64_runtime_memset_size)
	assert text.virtual_size == u32(thunk_start + int(pe64_import_thunk_size))
	pe64_test_assert_runtime_strlen_body(second[int(text.raw_pointer) + strlen_start..
		int(text.raw_pointer) + wcslen_start])
	pe64_test_assert_runtime_wcslen_body(second[int(text.raw_pointer) + wcslen_start..
		int(text.raw_pointer) + memset_start])
	pe64_test_assert_runtime_memset_body(second[int(text.raw_pointer) + memset_start..
		int(text.raw_pointer) + thunk_start])
	for field in [5, 10] {
		expected := pe64_checked_rel32(u64(field), u64(strlen_start)) or { panic(err) }
		assert pe64_test_read_u32(second, int(text.raw_pointer) + field) == expected
	}
	for field in [15, 20] {
		expected := pe64_checked_rel32(u64(field), u64(wcslen_start)) or { panic(err) }
		assert pe64_test_read_u32(second, int(text.raw_pointer) + field) == expected
	}
	for field in [25, 30] {
		expected := pe64_checked_rel32(u64(field), u64(memset_start)) or { panic(err) }
		assert pe64_test_read_u32(second, int(text.raw_pointer) + field) == expected
	}
	expected_import_call := pe64_checked_rel32(35, u64(thunk_start)) or { panic(err) }
	assert pe64_test_read_u32(second, int(text.raw_pointer) + 35) == expected_import_call
	assert second[int(text.raw_pointer) + thunk_start..int(text.raw_pointer) + thunk_start + 2] == [
		u8(0xff),
		0x25,
	]
	idata := sections[3]
	iat_rva := pe64_test_read_u32(second, int(idata.raw_pointer) + 16)
	thunk_field_offset := thunk_start + 2
	thunk_field_rva := text.virtual_address + u32(thunk_field_offset)
	expected_iat_displacement := pe64_checked_rel32(u64(thunk_field_rva), u64(iat_rva)) or {
		panic(err)
	}
	assert pe64_test_read_u32(second, int(text.raw_pointer) + thunk_field_offset) == expected_iat_displacement
	pdata := sections[1]
	assert pdata.virtual_size == u32(pe64_runtime_function_size)
	assert pe64_test_read_u32(second, int(pdata.raw_pointer) + 4) == text.virtual_address +
		u32(first_object.text.len)
	assert first_object.text == first_before_text
	assert first_object.symbols == first_before_symbols
	assert first_object.call_relocations == first_before_relocations
	assert first_definition.imports == first_before_imports
	assert first_definition.runtime_helpers == first_before_runtime
	assert second_object.text == second_before_text
	assert second_object.symbols == second_before_symbols
	assert second_object.call_relocations == second_before_relocations
	assert second_definition.imports == second_before_imports
	assert second_definition.runtime_helpers == second_before_runtime
	first[0] = 0
	assert second[0] == `M`
}

fn test_pe64_runtime_memset_preserves_binding_and_error_precedence() {
	object, entry, runtime_symbol := pe64_test_runtime_fixture('memset_refusal_entry', 'memset')
	before_text := object.text.clone()
	before_symbols := object.symbols.clone()
	before_relocations := object.call_relocations.clone()
	root := pe64_test_root('runtime-memset-refusal')
	defer {
		pe64_test_cleanup(root)
	}

	unbound_path := os.join_path(root, 'unbound.exe')
	assert pe64_test_build_then_publish_error(&object, pe64_test_definition(entry,
		[]Pe64ImportBinding{}), unbound_path).contains('PE64 import binding missing for SymbolID')

	non_microsoft := Pe64ImageDefinition{
		target_abi:      .unknown
		subsystem:       .windows_cui
		image_policy:    .fixed_base
		entry:           Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         .raw_noreturn_process_entry
		}
		runtime_helpers: [
			Pe64RuntimeBinding{
				symbol_id: runtime_symbol
				helper:    .memset
			},
		]
	}
	non_microsoft_before := non_microsoft.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, non_microsoft, os.join_path(root, 'abi.exe')) == 'PE64 requires Microsoft x64 ABI'
	assert non_microsoft.runtime_helpers == non_microsoft_before

	invalid := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: SymbolID(99), helper: .memset },
	])
	invalid_before := invalid.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, invalid, os.join_path(root, 'invalid.exe')).contains('PE64 runtime binding: SymbolID 99 is out of range')
	assert invalid.runtime_helpers == invalid_before

	defined := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: entry, helper: .memset },
	])
	defined_before := defined.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, defined, os.join_path(root, 'defined.exe')).contains('is not an external function')
	assert defined.runtime_helpers == defined_before

	duplicate := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memset },
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memset },
	])
	duplicate_before := duplicate.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, duplicate, os.join_path(root,
		'duplicate.exe')).contains('PE64 runtime binding: duplicate binding')
	assert duplicate.runtime_helpers == duplicate_before

	conflicting := pe64_test_runtime_definition(entry, [
		Pe64ImportBinding{
			symbol_id:   runtime_symbol
			dll:         'kernel32.dll'
			export_name: 'ExitProcess'
		},
	], [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memset },
	])
	conflicting_imports_before := conflicting.imports.clone()
	conflicting_runtime_before := conflicting.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, conflicting, os.join_path(root,
		'conflict.exe')).contains('has both import and runtime bindings')
	assert conflicting.imports == conflicting_imports_before
	assert conflicting.runtime_helpers == conflicting_runtime_before

	unknown := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .unknown },
	])
	unknown_before := unknown.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, unknown, os.join_path(root, 'unknown.exe')) == 'PE64 runtime binding: helper 0 is unsupported'
	assert unknown.runtime_helpers == unknown_before

	forged := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{
			symbol_id: runtime_symbol
			helper:    unsafe { Pe64RuntimeHelperKind(255) }
		},
	])
	forged_before := forged.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, forged, os.join_path(root, 'forged.exe')) == 'PE64 runtime binding: helper 255 is unsupported'
	assert forged.runtime_helpers == forged_before

	mut malformed_object := pe64_test_clone_object(&object)
	malformed_object.text[6] = 1
	malformed_definition := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memset },
	])
	malformed_runtime_before := malformed_definition.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&malformed_object, malformed_definition, os.join_path(root,
		'malformed.exe')).contains('PE64 object contract')
	assert malformed_definition.runtime_helpers == malformed_runtime_before
	assert object.text == before_text
	assert object.symbols == before_symbols
	assert object.call_relocations == before_relocations
}

fn test_pe64_runtime_memset_dumpbin_disassembly_when_guarded() {
	$if windows && amd64 {
		if os.getenv(pe64_test_dumpbin_guard) != '1' {
			return
		}
		dumpbin := os.getenv(pe64_test_dumpbin_path)
		assert dumpbin.len != 0, '${pe64_test_dumpbin_path} must name an absolute Microsoft DUMPBIN path'
		assert os.is_abs_path(dumpbin), 'DUMPBIN path must be absolute'
		assert os.is_file(dumpbin) && !os.is_link(dumpbin), 'DUMPBIN path must be a regular file'
		object, entry, runtime_symbol := pe64_test_runtime_fixture('dumpbin_memset_entry',
			'dumpbin_byte_fill_runtime')
		bytes := pe64_image_bytes(&object, pe64_test_runtime_definition(entry,
			[]Pe64ImportBinding{}, [
			Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memset },
		])) or { panic(err) }
		text := pe64_test_section(bytes, '.text')
		helper_rva := text.virtual_address + u32(object.text.len)
		helper_offset := pe64_test_rva_offset(bytes, helper_rva)
		pe64_test_assert_runtime_memset_body(bytes[helper_offset..helper_offset +
			int(pe64_runtime_memset_size)])
		root := pe64_test_root('runtime-memset-dumpbin')
		defer {
			pe64_test_cleanup(root)
		}
		output := os.join_path(root, 'runtime-memset.exe')
		publish_object(output, bytes) or { assert false, err.msg() }
		result := pe64_test_run_process(dumpbin, ['/DISASM:BYTES', output],
			pe64_test_dumpbin_environment(), pe64_test_timeout_ms)
		assert !result.timed_out, 'DUMPBIN timed out'
		assert !result.output_limited, 'DUMPBIN exceeded the output limit'
		assert result.exit_code == 0, result.stderr
		instructions := pe64_test_dumpbin_instructions(result.stdout)
		helper_address := pe64_checked_add(pe64_image_base, u64(helper_rva),
			'DUMPBIN memset helper address') or { panic(err) }
		mut helper_index := -1
		for index, instruction in instructions {
			if instruction.address == helper_address {
				assert helper_index == -1, 'DUMPBIN repeated the memset helper start address'
				helper_index = index
			}
		}
		assert helper_index >= 0, 'DUMPBIN omitted the addressed memset helper start'
		assert helper_index + 9 <= instructions.len, 'DUMPBIN truncated the addressed memset helper'
		helper := instructions[helper_index..helper_index + 9]
		assert helper[0].address == helper_address
		assert helper[0].bytes == [u8(0x48), 0x89, 0xc8]
		assert helper[0].mnemonic == 'mov'
		assert pe64_test_compact_operands(helper[0].operands) == 'rax,rcx'
		assert helper[1].address == helper_address + 3
		assert helper[1].bytes == [u8(0x4d), 0x85, 0xc0]
		assert helper[1].mnemonic == 'test'
		assert pe64_test_compact_operands(helper[1].operands) == 'r8,r8'
		assert helper[2].address == helper_address + 6
		assert helper[2].bytes == [u8(0x74), 0x0e]
		assert helper[2].mnemonic in ['je', 'jz']
		assert pe64_test_dumpbin_operand_has_address(helper[2].operands, helper_address + 22)
		assert helper[3].address == helper_address + 8
		assert helper[3].bytes == [u8(0x49), 0x89, 0xca]
		assert helper[3].mnemonic == 'mov'
		assert pe64_test_compact_operands(helper[3].operands) == 'r10,rcx'
		assert helper[4].address == helper_address + 11
		assert helper[4].bytes == [u8(0x41), 0x88, 0x12]
		assert helper[4].mnemonic == 'mov'
		assert pe64_test_compact_operands(helper[4].operands) == 'byteptr[r10],dl'
		assert helper[5].address == helper_address + 14
		assert helper[5].bytes == [u8(0x49), 0xff, 0xc2]
		assert helper[5].mnemonic == 'inc'
		assert pe64_test_compact_operands(helper[5].operands) == 'r10'
		assert helper[6].address == helper_address + 17
		assert helper[6].bytes == [u8(0x49), 0xff, 0xc8]
		assert helper[6].mnemonic == 'dec'
		assert pe64_test_compact_operands(helper[6].operands) == 'r8'
		assert helper[7].address == helper_address + 20
		assert helper[7].bytes == [u8(0x75), 0xf5]
		assert helper[7].mnemonic in ['jne', 'jnz']
		assert pe64_test_dumpbin_operand_has_address(helper[7].operands, helper_address + 11)
		assert helper[8].address == helper_address + 22
		assert helper[8].bytes == [u8(0xc3)]
		assert helper[8].mnemonic == 'ret'
		assert helper[8].operands == ''
	} $else {
		return
	}
}

fn test_pe64_runtime_memcmp_explicit_binding_emits_exact_leaf_and_semantics() {
	object, entry, runtime_symbol := pe64_test_runtime_fixture('memcmp_entry',
		'opaque_byte_compare_runtime')
	assert int(Pe64RuntimeHelperKind.memcmp) == 4
	assert !object.symbols[int(runtime_symbol)].name.contains('memcmp')
	definition := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memcmp },
	])
	before_text := object.text.clone()
	before_symbols := object.symbols.clone()
	before_relocations := object.call_relocations.clone()
	before_runtime := definition.runtime_helpers.clone()
	data := pe64_image_bytes(&object, definition) or { panic(err) }
	sections := pe64_test_sections(data)
	assert sections.map(it.name) == ['.text', '.pdata', '.xdata']
	text := sections[0]
	helper_start := object.text.len
	assert text.virtual_size == u32(helper_start + int(pe64_runtime_memcmp_size))
	expected_call := pe64_checked_rel32(5, u64(helper_start)) or { panic(err) }
	assert pe64_test_read_u32(data, int(text.raw_pointer) + 5) == expected_call
	helper_offset := int(text.raw_pointer) + helper_start
	helper_body := data[helper_offset..helper_offset + int(pe64_runtime_memcmp_size)]
	pe64_test_assert_runtime_memcmp_body(helper_body)

	memory_base := u64(0x2000)
	mut zero := pe64_test_runtime_memcmp_state(memory_base + 0x100, memory_base + 0x200, 0, [
		u8(0xaa),
	])
	zero_before := pe64_test_clone_runtime_memcmp_state(&zero)
	pe64_test_execute_runtime_memcmp_manifest(helper_body, memory_base, mut zero)
	assert zero.rax_written
	assert zero.eax == 0 && zero.eax != zero_before.eax
	assert zero.rcx == zero_before.rcx
	assert zero.rdx == zero_before.rdx
	assert zero.r8 == 0
	assert zero.r9 == zero_before.r9
	assert zero.r10 == zero_before.r10
	assert zero.status_flags_written
	assert zero.read_offsets.len == 0
	pe64_test_assert_runtime_memcmp_preserved(&zero_before, &zero)

	mut equal := pe64_test_runtime_memcmp_state(memory_base, memory_base + 1, 3, [
		u8(0x5a),
		0x5a,
		0x5a,
		0x5a,
		0x99,
	])
	equal_before := pe64_test_clone_runtime_memcmp_state(&equal)
	pe64_test_execute_runtime_memcmp_manifest(helper_body, memory_base, mut equal)
	assert equal.rax_written
	assert equal.eax == 0
	assert equal.rcx == equal_before.rcx + 3
	assert equal.rdx == equal_before.rdx + 3
	assert equal.r8 == 0
	assert equal.r9 == (equal_before.r9 & u64(0xffff_ffff_ffff_ff00)) | u64(0x5a)
	assert equal.r10 == (equal_before.r10 & u64(0xffff_ffff_ffff_ff00)) | u64(0x5a)
	assert equal.status_flags_written
	assert equal.read_offsets == [u64(0), 1, 1, 2, 2, 3]
	pe64_test_assert_runtime_memcmp_preserved(&equal_before, &equal)

	mut positive := pe64_test_runtime_memcmp_state(memory_base, memory_base + 4, 3, [
		u8(0x20),
		0xff,
		0x77,
		0xaa,
		0x20,
		0x00,
		0x88,
		0xbb,
	])
	positive_before := pe64_test_clone_runtime_memcmp_state(&positive)
	pe64_test_execute_runtime_memcmp_manifest(helper_body, memory_base, mut positive)
	assert positive.rax_written
	assert positive.eax == 255
	assert positive.rcx == positive_before.rcx + 1
	assert positive.rdx == positive_before.rdx + 1
	assert positive.r8 == positive_before.r8 - 1
	assert positive.r9 == (positive_before.r9 & u64(0xffff_ffff_ffff_ff00)) | u64(0xff)
	assert positive.r10 == 0
	assert positive.status_flags_written
	assert positive.read_offsets == [u64(0), 4, 1, 5]
	pe64_test_assert_runtime_memcmp_preserved(&positive_before, &positive)

	mut negative := pe64_test_runtime_memcmp_state(memory_base, memory_base + 2, 2, [
		u8(0x00),
		0x33,
		0xff,
		0x44,
	])
	negative_before := pe64_test_clone_runtime_memcmp_state(&negative)
	pe64_test_execute_runtime_memcmp_manifest(helper_body, memory_base, mut negative)
	assert negative.rax_written
	assert negative.eax == -255
	assert negative.rcx == negative_before.rcx
	assert negative.rdx == negative_before.rdx
	assert negative.r8 == negative_before.r8
	assert negative.r9 == negative_before.r9 & u64(0xffff_ffff_ffff_ff00)
	assert negative.r10 == 0xff
	assert negative.status_flags_written
	assert negative.read_offsets == [u64(0), 2]
	pe64_test_assert_runtime_memcmp_preserved(&negative_before, &negative)

	pdata := sections[1]
	xdata := sections[2]
	assert pdata.virtual_size == u32(pe64_runtime_function_size)
	assert xdata.virtual_size == u32(pe64_unwind_info_size)
	assert pe64_test_read_u32(data, int(pdata.raw_pointer)) == text.virtual_address
	assert pe64_test_read_u32(data, int(pdata.raw_pointer) + 4) == text.virtual_address +
		u32(object.text.len)
	assert pe64_test_directory(data, pe64_import_directory_index) == Pe64TestDirectory{}
	assert pe64_test_directory(data, pe64_iat_directory_index) == Pe64TestDirectory{}
	assert object.text == before_text
	assert object.symbols == before_symbols
	assert object.call_relocations == before_relocations
	assert definition.runtime_helpers == before_runtime
}

fn test_pe64_runtime_memcmp_deduplicates_and_orders_after_memset() {
	first_object, first_entry, first_strlen, first_wcslen, first_memset, first_memcmp_a, first_memcmp_b, first_imported :=
		pe64_test_runtime_memcmp_order_fixture(false)
	second_object, second_entry, second_strlen, second_wcslen, second_memset, second_memcmp_a, second_memcmp_b, second_imported :=
		pe64_test_runtime_memcmp_order_fixture(true)
	first_definition := pe64_test_runtime_definition(first_entry, [
		Pe64ImportBinding{
			symbol_id:   first_imported
			dll:         'kernel32.dll'
			export_name: 'ExitProcess'
		},
	], [
		Pe64RuntimeBinding{ symbol_id: first_memcmp_b, helper: .memcmp },
		Pe64RuntimeBinding{ symbol_id: first_memset, helper: .memset },
		Pe64RuntimeBinding{ symbol_id: first_wcslen, helper: .wcslen },
		Pe64RuntimeBinding{ symbol_id: first_strlen, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: first_memcmp_a, helper: .memcmp },
	])
	second_definition := pe64_test_runtime_definition(second_entry, [
		Pe64ImportBinding{
			symbol_id:   second_imported
			dll:         'kernel32.dll'
			export_name: 'ExitProcess'
		},
	], [
		Pe64RuntimeBinding{ symbol_id: second_strlen, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: second_memcmp_a, helper: .memcmp },
		Pe64RuntimeBinding{ symbol_id: second_wcslen, helper: .wcslen },
		Pe64RuntimeBinding{ symbol_id: second_memcmp_b, helper: .memcmp },
		Pe64RuntimeBinding{ symbol_id: second_memset, helper: .memset },
	])
	first_before_text := first_object.text.clone()
	first_before_symbols := first_object.symbols.clone()
	first_before_relocations := first_object.call_relocations.clone()
	first_before_imports := first_definition.imports.clone()
	first_before_runtime := first_definition.runtime_helpers.clone()
	second_before_text := second_object.text.clone()
	second_before_symbols := second_object.symbols.clone()
	second_before_relocations := second_object.call_relocations.clone()
	second_before_imports := second_definition.imports.clone()
	second_before_runtime := second_definition.runtime_helpers.clone()
	mut first := pe64_image_bytes(&first_object, first_definition) or { panic(err) }
	second := pe64_image_bytes(&second_object, second_definition) or { panic(err) }
	assert first == second
	sections := pe64_test_sections(second)
	assert sections.map(it.name) == ['.text', '.pdata', '.xdata', '.idata']
	text := sections[0]
	strlen_start := first_object.text.len
	wcslen_start := strlen_start + int(pe64_runtime_strlen_size)
	memset_start := wcslen_start + int(pe64_runtime_wcslen_size)
	memcmp_start := memset_start + int(pe64_runtime_memset_size)
	thunk_start := memcmp_start + int(pe64_runtime_memcmp_size)
	assert text.virtual_size == u32(thunk_start + int(pe64_import_thunk_size))
	pe64_test_assert_runtime_strlen_body(second[int(text.raw_pointer) + strlen_start..
		int(text.raw_pointer) + wcslen_start])
	pe64_test_assert_runtime_wcslen_body(second[int(text.raw_pointer) + wcslen_start..
		int(text.raw_pointer) + memset_start])
	pe64_test_assert_runtime_memset_body(second[int(text.raw_pointer) + memset_start..
		int(text.raw_pointer) + memcmp_start])
	pe64_test_assert_runtime_memcmp_body(second[int(text.raw_pointer) + memcmp_start..
		int(text.raw_pointer) + thunk_start])
	expected_strlen_call := pe64_checked_rel32(5, u64(strlen_start)) or { panic(err) }
	assert pe64_test_read_u32(second, int(text.raw_pointer) + 5) == expected_strlen_call
	expected_wcslen_call := pe64_checked_rel32(10, u64(wcslen_start)) or { panic(err) }
	assert pe64_test_read_u32(second, int(text.raw_pointer) + 10) == expected_wcslen_call
	expected_memset_call := pe64_checked_rel32(15, u64(memset_start)) or { panic(err) }
	assert pe64_test_read_u32(second, int(text.raw_pointer) + 15) == expected_memset_call
	for field in [20, 25] {
		expected := pe64_checked_rel32(u64(field), u64(memcmp_start)) or { panic(err) }
		assert pe64_test_read_u32(second, int(text.raw_pointer) + field) == expected
	}
	expected_import_call := pe64_checked_rel32(30, u64(thunk_start)) or { panic(err) }
	assert pe64_test_read_u32(second, int(text.raw_pointer) + 30) == expected_import_call
	assert second[int(text.raw_pointer) + thunk_start..int(text.raw_pointer) + thunk_start + 2] == [
		u8(0xff),
		0x25,
	]
	idata := sections[3]
	iat_rva := pe64_test_read_u32(second, int(idata.raw_pointer) + 16)
	thunk_field_offset := thunk_start + 2
	thunk_field_rva := text.virtual_address + u32(thunk_field_offset)
	expected_iat_displacement := pe64_checked_rel32(u64(thunk_field_rva), u64(iat_rva)) or {
		panic(err)
	}
	assert pe64_test_read_u32(second, int(text.raw_pointer) + thunk_field_offset) == expected_iat_displacement
	pdata := sections[1]
	assert pdata.virtual_size == u32(pe64_runtime_function_size)
	assert pe64_test_read_u32(second, int(pdata.raw_pointer) + 4) == text.virtual_address +
		u32(first_object.text.len)
	assert first_object.text == first_before_text
	assert first_object.symbols == first_before_symbols
	assert first_object.call_relocations == first_before_relocations
	assert first_definition.imports == first_before_imports
	assert first_definition.runtime_helpers == first_before_runtime
	assert second_object.text == second_before_text
	assert second_object.symbols == second_before_symbols
	assert second_object.call_relocations == second_before_relocations
	assert second_definition.imports == second_before_imports
	assert second_definition.runtime_helpers == second_before_runtime
	first[0] = 0
	assert second[0] == `M`
}

fn test_pe64_runtime_memcmp_preserves_binding_and_error_precedence() {
	object, entry, runtime_symbol := pe64_test_runtime_fixture('memcmp_refusal_entry', 'memcmp')
	before_text := object.text.clone()
	before_symbols := object.symbols.clone()
	before_relocations := object.call_relocations.clone()
	root := pe64_test_root('runtime-memcmp-refusal')
	defer {
		pe64_test_cleanup(root)
	}

	unbound_path := os.join_path(root, 'unbound.exe')
	assert pe64_test_build_then_publish_error(&object, pe64_test_definition(entry,
		[]Pe64ImportBinding{}), unbound_path).contains('PE64 import binding missing for SymbolID')

	non_microsoft := Pe64ImageDefinition{
		target_abi:      .unknown
		subsystem:       .windows_cui
		image_policy:    .fixed_base
		entry:           Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         .raw_noreturn_process_entry
		}
		runtime_helpers: [
			Pe64RuntimeBinding{
				symbol_id: runtime_symbol
				helper:    .memcmp
			},
		]
	}
	non_microsoft_before := non_microsoft.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, non_microsoft, os.join_path(root, 'abi.exe')) == 'PE64 requires Microsoft x64 ABI'
	assert non_microsoft.runtime_helpers == non_microsoft_before

	invalid := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: SymbolID(99), helper: .memcmp },
	])
	invalid_before := invalid.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, invalid, os.join_path(root, 'invalid.exe')).contains('PE64 runtime binding: SymbolID 99 is out of range')
	assert invalid.runtime_helpers == invalid_before

	defined := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: entry, helper: .memcmp },
	])
	defined_before := defined.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, defined, os.join_path(root, 'defined.exe')).contains('is not an external function')
	assert defined.runtime_helpers == defined_before

	duplicate := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memcmp },
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memcmp },
	])
	duplicate_before := duplicate.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, duplicate, os.join_path(root,
		'duplicate.exe')).contains('PE64 runtime binding: duplicate binding')
	assert duplicate.runtime_helpers == duplicate_before

	conflicting := pe64_test_runtime_definition(entry, [
		Pe64ImportBinding{
			symbol_id:   runtime_symbol
			dll:         'kernel32.dll'
			export_name: 'ExitProcess'
		},
	], [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memcmp },
	])
	conflicting_imports_before := conflicting.imports.clone()
	conflicting_runtime_before := conflicting.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, conflicting, os.join_path(root,
		'conflict.exe')).contains('has both import and runtime bindings')
	assert conflicting.imports == conflicting_imports_before
	assert conflicting.runtime_helpers == conflicting_runtime_before

	unknown := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .unknown },
	])
	unknown_before := unknown.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, unknown, os.join_path(root, 'unknown.exe')) == 'PE64 runtime binding: helper 0 is unsupported'
	assert unknown.runtime_helpers == unknown_before

	forged := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{
			symbol_id: runtime_symbol
			helper:    unsafe { Pe64RuntimeHelperKind(255) }
		},
	])
	forged_before := forged.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, forged, os.join_path(root, 'forged.exe')) == 'PE64 runtime binding: helper 255 is unsupported'
	assert forged.runtime_helpers == forged_before

	mut malformed_object := pe64_test_clone_object(&object)
	malformed_object.text[6] = 1
	malformed_definition := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memcmp },
	])
	malformed_runtime_before := malformed_definition.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&malformed_object, malformed_definition, os.join_path(root,
		'malformed.exe')).contains('PE64 object contract')
	assert malformed_definition.runtime_helpers == malformed_runtime_before
	assert object.text == before_text
	assert object.symbols == before_symbols
	assert object.call_relocations == before_relocations
}

fn test_pe64_runtime_memcmp_dumpbin_disassembly_when_guarded() {
	$if windows && amd64 {
		if os.getenv(pe64_test_dumpbin_guard) != '1' {
			return
		}
		dumpbin := os.getenv(pe64_test_dumpbin_path)
		assert dumpbin.len != 0, '${pe64_test_dumpbin_path} must name an absolute Microsoft DUMPBIN path'
		assert os.is_abs_path(dumpbin), 'DUMPBIN path must be absolute'
		assert os.is_file(dumpbin) && !os.is_link(dumpbin), 'DUMPBIN path must be a regular file'
		object, entry, runtime_symbol := pe64_test_runtime_fixture('dumpbin_memcmp_entry',
			'dumpbin_byte_compare_runtime')
		bytes := pe64_image_bytes(&object, pe64_test_runtime_definition(entry,
			[]Pe64ImportBinding{}, [
			Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memcmp },
		])) or { panic(err) }
		text := pe64_test_section(bytes, '.text')
		helper_rva := text.virtual_address + u32(object.text.len)
		helper_offset := pe64_test_rva_offset(bytes, helper_rva)
		pe64_test_assert_runtime_memcmp_body(bytes[helper_offset..helper_offset +
			int(pe64_runtime_memcmp_size)])
		root := pe64_test_root('runtime-memcmp-dumpbin')
		defer {
			pe64_test_cleanup(root)
		}
		output := os.join_path(root, 'runtime-memcmp.exe')
		publish_object(output, bytes) or { assert false, err.msg() }
		result := pe64_test_run_process(dumpbin, ['/DISASM:BYTES', output],
			pe64_test_dumpbin_environment(), pe64_test_timeout_ms)
		assert !result.timed_out, 'DUMPBIN timed out'
		assert !result.output_limited, 'DUMPBIN exceeded the output limit'
		assert result.exit_code == 0, result.stderr
		instructions := pe64_test_dumpbin_instructions(result.stdout)
		helper_address := pe64_checked_add(pe64_image_base, u64(helper_rva),
			'DUMPBIN memcmp helper address') or { panic(err) }
		mut helper_index := -1
		for index, instruction in instructions {
			if instruction.address == helper_address {
				assert helper_index == -1, 'DUMPBIN repeated the memcmp helper start address'
				helper_index = index
			}
		}
		assert helper_index >= 0, 'DUMPBIN omitted the addressed memcmp helper start'
		assert helper_index + 16 <= instructions.len, 'DUMPBIN truncated the addressed memcmp helper'
		helper := instructions[helper_index..helper_index + 16]
		assert helper[0].address == helper_address
		assert helper[0].bytes == [u8(0x4d), 0x85, 0xc0]
		assert helper[0].mnemonic == 'test'
		assert pe64_test_compact_operands(helper[0].operands) == 'r8,r8'
		assert helper[1].address == helper_address + 3
		assert helper[1].bytes == [u8(0x74), 0x16]
		assert helper[1].mnemonic in ['je', 'jz']
		assert pe64_test_dumpbin_operand_has_address(helper[1].operands, helper_address + 27)
		assert helper[2].address == helper_address + 5
		assert helper[2].bytes == [u8(0x44), 0x8a, 0x09]
		assert helper[2].mnemonic == 'mov'
		assert pe64_test_compact_operands(helper[2].operands) == 'r9b,byteptr[rcx]'
		assert helper[3].address == helper_address + 8
		assert helper[3].bytes == [u8(0x44), 0x8a, 0x12]
		assert helper[3].mnemonic == 'mov'
		assert pe64_test_compact_operands(helper[3].operands) == 'r10b,byteptr[rdx]'
		assert helper[4].address == helper_address + 11
		assert helper[4].bytes == [u8(0x45), 0x38, 0xd1]
		assert helper[4].mnemonic == 'cmp'
		assert pe64_test_compact_operands(helper[4].operands) == 'r9b,r10b'
		assert helper[5].address == helper_address + 14
		assert helper[5].bytes == [u8(0x75), 0x0e]
		assert helper[5].mnemonic in ['jne', 'jnz']
		assert pe64_test_dumpbin_operand_has_address(helper[5].operands, helper_address + 30)
		assert helper[6].address == helper_address + 16
		assert helper[6].bytes == [u8(0x48), 0xff, 0xc1]
		assert helper[6].mnemonic == 'inc'
		assert pe64_test_compact_operands(helper[6].operands) == 'rcx'
		assert helper[7].address == helper_address + 19
		assert helper[7].bytes == [u8(0x48), 0xff, 0xc2]
		assert helper[7].mnemonic == 'inc'
		assert pe64_test_compact_operands(helper[7].operands) == 'rdx'
		assert helper[8].address == helper_address + 22
		assert helper[8].bytes == [u8(0x49), 0xff, 0xc8]
		assert helper[8].mnemonic == 'dec'
		assert pe64_test_compact_operands(helper[8].operands) == 'r8'
		assert helper[9].address == helper_address + 25
		assert helper[9].bytes == [u8(0x75), 0xea]
		assert helper[9].mnemonic in ['jne', 'jnz']
		assert pe64_test_dumpbin_operand_has_address(helper[9].operands, helper_address + 5)
		assert helper[10].address == helper_address + 27
		assert helper[10].bytes == [u8(0x29), 0xc0]
		assert helper[10].mnemonic == 'sub'
		assert pe64_test_compact_operands(helper[10].operands) == 'eax,eax'
		assert helper[11].address == helper_address + 29
		assert helper[11].bytes == [u8(0xc3)]
		assert helper[11].mnemonic == 'ret'
		assert helper[11].operands == ''
		assert helper[12].address == helper_address + 30
		assert helper[12].bytes == [u8(0x41), 0x0f, 0xb6, 0xc1]
		assert helper[12].mnemonic == 'movzx'
		assert pe64_test_compact_operands(helper[12].operands) == 'eax,r9b'
		assert helper[13].address == helper_address + 34
		assert helper[13].bytes == [u8(0x45), 0x0f, 0xb6, 0xd2]
		assert helper[13].mnemonic == 'movzx'
		assert pe64_test_compact_operands(helper[13].operands) == 'r10d,r10b'
		assert helper[14].address == helper_address + 38
		assert helper[14].bytes == [u8(0x44), 0x29, 0xd0]
		assert helper[14].mnemonic == 'sub'
		assert pe64_test_compact_operands(helper[14].operands) == 'eax,r10d'
		assert helper[15].address == helper_address + 41
		assert helper[15].bytes == [u8(0xc3)]
		assert helper[15].mnemonic == 'ret'
		assert helper[15].operands == ''
	} $else {
		return
	}
}

fn test_pe64_runtime_memmove_explicit_binding_emits_exact_shared_leaf_and_overlap_semantics() {
	object, entry, runtime_symbol := pe64_test_runtime_fixture('move_entry', 'opaque_move_runtime')
	assert int(Pe64RuntimeHelperKind.memmove) == 5
	assert int(Pe64RuntimeHelperKind.memcpy) == 6
	assert !object.symbols[int(runtime_symbol)].name.contains('memmove')
	assert !object.symbols[int(runtime_symbol)].name.contains('memcpy')
	definition := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memmove },
	])
	before_object := pe64_test_clone_object(&object)
	before_runtime := definition.runtime_helpers.clone()
	data := pe64_image_bytes(&object, definition) or { panic(err) }
	sections := pe64_test_sections(data)
	assert sections.map(it.name) == ['.text', '.pdata', '.xdata']
	text := sections[0]
	helper_start := object.text.len
	helper_offset := int(text.raw_pointer) + helper_start
	assert text.virtual_size == u32(helper_start + int(pe64_runtime_move_size))
	helper_body := data[helper_offset..helper_offset + int(pe64_runtime_move_size)]
	pe64_test_assert_runtime_move_body(helper_body)
	expected_call := pe64_checked_rel32(5, u64(helper_start)) or { panic(err) }
	assert pe64_test_read_u32(data, int(text.raw_pointer) + 5) == expected_call
	pdata := sections[1]
	assert pdata.virtual_size == u32(pe64_runtime_function_size)
	assert pe64_test_read_u32(data, int(pdata.raw_pointer) + 4) == text.virtual_address +
		u32(object.text.len)
	assert pe64_test_directory(data, pe64_import_directory_index) == Pe64TestDirectory{}
	assert pe64_test_directory(data, pe64_iat_directory_index) == Pe64TestDirectory{}

	memory_base := u64(0x1000_0000)
	mut zero := pe64_test_runtime_move_state(memory_base + 0x100, memory_base + 0x200, 0, [
		u8(0x10),
		0x11,
	])
	zero_before := pe64_test_clone_runtime_move_state(&zero)
	pe64_test_execute_runtime_move_manifest(helper_body, memory_base, mut zero)
	assert zero.rax == zero_before.rcx
	assert zero.r8 == 0
	assert zero.r9 == zero_before.r9
	assert zero.r10 == zero_before.r10
	assert zero.r11 == zero_before.r11
	assert zero.memory == zero_before.memory
	assert zero.read_offsets.len == 0
	assert zero.write_offsets.len == 0
	assert zero.status_flags_written
	pe64_test_assert_runtime_move_preserved(&zero_before, &zero)

	mut equal := pe64_test_runtime_move_state(memory_base, memory_base, 4, [
		u8(0x10),
		0x20,
		0x30,
		0x40,
	])
	equal_before := pe64_test_clone_runtime_move_state(&equal)
	pe64_test_execute_runtime_move_manifest(helper_body, memory_base, mut equal)
	assert equal.rax == equal_before.rcx
	assert equal.r8 == 0
	assert equal.r9 == (equal_before.r9 & u64(0xffff_ffff_ffff_ff00)) | u64(0x40)
	assert equal.r10 == memory_base + 4
	assert equal.r11 == memory_base + 4
	assert equal.memory == equal_before.memory
	assert equal.read_offsets == [u64(0), 1, 2, 3]
	assert equal.write_offsets == [u64(0), 1, 2, 3]
	pe64_test_assert_runtime_move_preserved(&equal_before, &equal)

	mut before_source := pe64_test_runtime_move_state(memory_base, memory_base + 2, 4, [
		u8(0xa0),
		0xa1,
		0xb0,
		0xb1,
		0xb2,
		0xb3,
	])
	before_source_before := pe64_test_clone_runtime_move_state(&before_source)
	pe64_test_execute_runtime_move_manifest(helper_body, memory_base, mut before_source)
	assert before_source.rax == before_source_before.rcx
	assert before_source.r8 == 0
	assert before_source.r9 == (before_source_before.r9 & u64(0xffff_ffff_ffff_ff00)) | u64(0xb3)
	assert before_source.r10 == memory_base + 4
	assert before_source.r11 == memory_base + 6
	assert before_source.memory == [u8(0xb0), 0xb1, 0xb2, 0xb3, 0xb2, 0xb3]
	assert before_source.read_offsets == [u64(2), 3, 4, 5]
	assert before_source.write_offsets == [u64(0), 1, 2, 3]
	pe64_test_assert_runtime_move_preserved(&before_source_before, &before_source)

	mut after_nonoverlap := pe64_test_runtime_move_state(memory_base + 4, memory_base, 2, [
		u8(0xc0),
		0xc1,
		0xd0,
		0xd1,
		0xe0,
		0xe1,
	])
	after_nonoverlap_before := pe64_test_clone_runtime_move_state(&after_nonoverlap)
	pe64_test_execute_runtime_move_manifest(helper_body, memory_base, mut after_nonoverlap)
	assert after_nonoverlap.rax == after_nonoverlap_before.rcx
	assert after_nonoverlap.r8 == 0
	assert after_nonoverlap.r9 == ((memory_base + 2) & u64(0xffff_ffff_ffff_ff00)) | u64(0xc1)
	assert after_nonoverlap.r10 == memory_base + 6
	assert after_nonoverlap.r11 == memory_base + 2
	assert after_nonoverlap.memory == [u8(0xc0), 0xc1, 0xd0, 0xd1, 0xc0, 0xc1]
	assert after_nonoverlap.read_offsets == [u64(0), 1]
	assert after_nonoverlap.write_offsets == [u64(4), 5]
	pe64_test_assert_runtime_move_preserved(&after_nonoverlap_before, &after_nonoverlap)

	mut backward := pe64_test_runtime_move_state(memory_base + 2, memory_base, 4, [
		u8(0xd0),
		0xd1,
		0xd2,
		0xd3,
		0xd4,
		0xd5,
	])
	backward_before := pe64_test_clone_runtime_move_state(&backward)
	pe64_test_execute_runtime_move_manifest(helper_body, memory_base, mut backward)
	assert backward.rax == backward_before.rcx
	assert backward.r8 == 0
	assert backward.r9 == ((memory_base + 4) & u64(0xffff_ffff_ffff_ff00)) | u64(0xd0)
	assert backward.r10 == memory_base + 1
	assert backward.r11 == memory_base - 1
	assert backward.memory == [u8(0xd0), 0xd1, 0xd0, 0xd1, 0xd2, 0xd3]
	assert backward.read_offsets == [u64(3), 2, 1, 0]
	assert backward.write_offsets == [u64(5), 4, 3, 2]
	pe64_test_assert_runtime_move_preserved(&backward_before, &backward)

	high_count := u64(0x1_0000_0001)
	assert high_count > u64(max_u32)
	mut symbolic_r8 := high_count
	assert helper_body[3..6] == [u8(0x4d), 0x85, 0xc0]
	test_mode, test_reg, test_rm := pe64_test_modrm_fields(helper_body[5])
	assert test_mode == 3 && test_reg == 0 && test_rm == 0
	assert helper_body[3] & 0x0d == 0x0d
	symbolic_test_result := symbolic_r8 & symbolic_r8
	assert symbolic_test_result == high_count
	assert symbolic_r8 == high_count
	lea_offsets := [13, 22, 27]
	lea_sib_offsets := [16, 25, 30]
	for index, lea_offset in lea_offsets {
		assert helper_body[lea_offset] & 0x0a == 0x0a
		assert helper_body[lea_offset + 1] == 0x8d
		sib := helper_body[lea_sib_offsets[index]]
		assert (sib >> 3) & 7 == 0
	}
	symbolic_source := u64(0x1_0000_0000)
	symbolic_destination := u64(0x2_0000_0000)
	assert symbolic_source <= max_u64 - symbolic_r8
	assert symbolic_destination <= max_u64 - symbolic_r8
	symbolic_r9 := symbolic_source + symbolic_r8
	symbolic_r10 := symbolic_destination + symbolic_r8 - 1
	symbolic_r11 := symbolic_source + symbolic_r8 - 1
	assert symbolic_r9 == u64(0x2_0000_0001)
	assert symbolic_r10 == u64(0x3_0000_0000)
	assert symbolic_r11 == u64(0x2_0000_0000)
	assert symbolic_r9 > u64(max_u32)
	assert symbolic_r10 > u64(max_u32)
	assert symbolic_r11 > u64(max_u32)
	assert helper_body[44..47] == [u8(0x49), 0xff, 0xc8]
	dec_mode, dec_opcode, dec_rm := pe64_test_modrm_fields(helper_body[46])
	assert dec_mode == 3 && dec_opcode == 1 && dec_rm == 0
	assert helper_body[44] & 0x09 == 0x09
	symbolic_r8--
	assert symbolic_r8 == high_count - 1
	assert symbolic_r8 == u64(0x1_0000_0000)
	assert symbolic_r8 > u64(max_u32)
	assert helper_body[68..71] == [u8(0x49), 0xff, 0xc8]
	assert object.text == before_object.text
	assert object.symbols == before_object.symbols
	assert object.call_relocations == before_object.call_relocations
	assert definition.runtime_helpers == before_runtime
}

fn test_pe64_runtime_memcpy_explicit_binding_uses_shared_move_body_for_defined_cases() {
	object, entry, runtime_symbol := pe64_test_runtime_fixture('copy_entry', 'opaque_copy_runtime')
	memmove_definition := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memmove },
	])
	memcpy_definition := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memcpy },
	])
	before_object := pe64_test_clone_object(&object)
	before_memmove := memmove_definition.runtime_helpers.clone()
	before_memcpy := memcpy_definition.runtime_helpers.clone()
	memmove_plan := pe64_prepare_runtime_helpers(&object, memmove_definition.runtime_helpers) or {
		panic(err)
	}
	memcpy_plan := pe64_prepare_runtime_helpers(&object, memcpy_definition.runtime_helpers) or {
		panic(err)
	}
	assert memmove_plan.physical == [Pe64RuntimeHelperKind.memmove]
	assert memcpy_plan.physical == [Pe64RuntimeHelperKind.memmove]
	assert memmove_plan.physical_offsets == memcpy_plan.physical_offsets
	assert memmove_plan.symbol_physical_index == memcpy_plan.symbol_physical_index
	assert memmove_plan.size == pe64_runtime_move_size
	assert memcpy_plan.size == pe64_runtime_move_size
	memmove_image := pe64_image_bytes(&object, memmove_definition) or { panic(err) }
	memcpy_image := pe64_image_bytes(&object, memcpy_definition) or { panic(err) }
	assert memcpy_image == memmove_image
	text := pe64_test_section(memcpy_image, '.text')
	helper_start := object.text.len
	helper_offset := int(text.raw_pointer) + helper_start
	helper_body := memcpy_image[helper_offset..helper_offset + int(pe64_runtime_move_size)]
	pe64_test_assert_runtime_move_body(helper_body)
	expected_call := pe64_checked_rel32(5, u64(helper_start)) or { panic(err) }
	assert pe64_test_read_u32(memcpy_image, int(text.raw_pointer) + 5) == expected_call

	memory_base := u64(0x2000_0000)
	mut forward := pe64_test_runtime_move_state(memory_base, memory_base + 4, 2, [
		u8(0x90),
		0x91,
		0xa0,
		0xa1,
		0xb0,
		0xb1,
	])
	forward_before := pe64_test_clone_runtime_move_state(&forward)
	pe64_test_execute_runtime_move_manifest(helper_body, memory_base, mut forward)
	assert forward.rax == forward_before.rcx
	assert forward.memory == [u8(0xb0), 0xb1, 0xa0, 0xa1, 0xb0, 0xb1]
	assert forward.read_offsets == [u64(4), 5]
	assert forward.write_offsets == [u64(0), 1]
	pe64_test_assert_runtime_move_preserved(&forward_before, &forward)

	mut after := pe64_test_runtime_move_state(memory_base + 4, memory_base, 2, [
		u8(0xc0),
		0xc1,
		0xd0,
		0xd1,
		0xe0,
		0xe1,
	])
	after_before := pe64_test_clone_runtime_move_state(&after)
	pe64_test_execute_runtime_move_manifest(helper_body, memory_base, mut after)
	assert after.rax == after_before.rcx
	assert after.memory == [u8(0xc0), 0xc1, 0xd0, 0xd1, 0xc0, 0xc1]
	assert after.read_offsets == [u64(0), 1]
	assert after.write_offsets == [u64(4), 5]
	pe64_test_assert_runtime_move_preserved(&after_before, &after)

	mut zero := pe64_test_runtime_move_state(memory_base + 0x100, memory_base + 0x200, 0, [
		u8(0xee),
	])
	zero_before := pe64_test_clone_runtime_move_state(&zero)
	pe64_test_execute_runtime_move_manifest(helper_body, memory_base, mut zero)
	assert zero.rax == zero_before.rcx
	assert zero.memory == zero_before.memory
	assert zero.read_offsets.len == 0 && zero.write_offsets.len == 0
	pe64_test_assert_runtime_move_preserved(&zero_before, &zero)
	assert object.text == before_object.text
	assert object.symbols == before_object.symbols
	assert object.call_relocations == before_object.call_relocations
	assert memmove_definition.runtime_helpers == before_memmove
	assert memcpy_definition.runtime_helpers == before_memcpy
}

fn test_pe64_runtime_move_family_cross_kind_deduplicates_and_orders_after_memcmp() {
	first := pe64_test_runtime_move_order_fixture(false)
	second := pe64_test_runtime_move_order_fixture(true)
	first_definition := pe64_test_runtime_definition(first.entry, [
		Pe64ImportBinding{
			symbol_id:   first.imported
			dll:         'kernel32.dll'
			export_name: 'ExitProcess'
		},
	], [
		Pe64RuntimeBinding{ symbol_id: first.memcpy_b, helper: .memcpy },
		Pe64RuntimeBinding{ symbol_id: first.strlen, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: first.memmove_b, helper: .memmove },
		Pe64RuntimeBinding{ symbol_id: first.memcmp, helper: .memcmp },
		Pe64RuntimeBinding{ symbol_id: first.memset, helper: .memset },
		Pe64RuntimeBinding{ symbol_id: first.memcpy_a, helper: .memcpy },
		Pe64RuntimeBinding{ symbol_id: first.wcslen, helper: .wcslen },
		Pe64RuntimeBinding{ symbol_id: first.memmove_a, helper: .memmove },
	])
	second_definition := pe64_test_runtime_definition(second.entry, [
		Pe64ImportBinding{
			symbol_id:   second.imported
			dll:         'kernel32.dll'
			export_name: 'ExitProcess'
		},
	], [
		Pe64RuntimeBinding{ symbol_id: second.memmove_a, helper: .memmove },
		Pe64RuntimeBinding{ symbol_id: second.wcslen, helper: .wcslen },
		Pe64RuntimeBinding{ symbol_id: second.memcpy_a, helper: .memcpy },
		Pe64RuntimeBinding{ symbol_id: second.memset, helper: .memset },
		Pe64RuntimeBinding{ symbol_id: second.memcmp, helper: .memcmp },
		Pe64RuntimeBinding{ symbol_id: second.memmove_b, helper: .memmove },
		Pe64RuntimeBinding{ symbol_id: second.strlen, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: second.memcpy_b, helper: .memcpy },
	])
	first_before := pe64_test_clone_object(&first.object)
	second_before := pe64_test_clone_object(&second.object)
	first_imports_before := first_definition.imports.clone()
	first_runtime_before := first_definition.runtime_helpers.clone()
	second_imports_before := second_definition.imports.clone()
	second_runtime_before := second_definition.runtime_helpers.clone()
	first_plan := pe64_prepare_runtime_helpers(&first.object, first_definition.runtime_helpers) or {
		panic(err)
	}
	second_plan := pe64_prepare_runtime_helpers(&second.object, second_definition.runtime_helpers) or {
		panic(err)
	}
	assert first_plan.physical == [
		Pe64RuntimeHelperKind.strlen,
		.wcslen,
		.memset,
		.memcmp,
		.memmove,
	]
	assert second_plan.physical == first_plan.physical
	assert first_plan.physical_offsets == second_plan.physical_offsets
	assert first_plan.size == pe64_runtime_strlen_size + pe64_runtime_wcslen_size +
		pe64_runtime_memset_size + pe64_runtime_memcmp_size + pe64_runtime_move_size
	assert first_plan.symbol_physical_index[int(first.strlen)] == 0
	assert first_plan.symbol_physical_index[int(first.wcslen)] == 1
	assert first_plan.symbol_physical_index[int(first.memset)] == 2
	assert first_plan.symbol_physical_index[int(first.memcmp)] == 3
	for symbol_id in [first.memmove_a, first.memmove_b, first.memcpy_a, first.memcpy_b] {
		assert first_plan.symbol_physical_index[int(symbol_id)] == 4
	}
	mut first_image := pe64_image_bytes(&first.object, first_definition) or { panic(err) }
	second_image := pe64_image_bytes(&second.object, second_definition) or { panic(err) }
	assert first_image == second_image
	sections := pe64_test_sections(second_image)
	assert sections.map(it.name) == ['.text', '.pdata', '.xdata', '.idata']
	text := sections[0]
	strlen_start := first.object.text.len
	wcslen_start := strlen_start + int(pe64_runtime_strlen_size)
	memset_start := wcslen_start + int(pe64_runtime_wcslen_size)
	memcmp_start := memset_start + int(pe64_runtime_memset_size)
	move_start := memcmp_start + int(pe64_runtime_memcmp_size)
	thunk_start := move_start + int(pe64_runtime_move_size)
	assert first_plan.physical_offsets == [
		u64(strlen_start),
		u64(wcslen_start),
		u64(memset_start),
		u64(memcmp_start),
		u64(move_start),
	]
	assert text.virtual_size == u32(thunk_start + int(pe64_import_thunk_size))
	raw := int(text.raw_pointer)
	pe64_test_assert_runtime_strlen_body(second_image[raw + strlen_start..raw + wcslen_start])
	pe64_test_assert_runtime_wcslen_body(second_image[raw + wcslen_start..raw + memset_start])
	pe64_test_assert_runtime_memset_body(second_image[raw + memset_start..raw + memcmp_start])
	pe64_test_assert_runtime_memcmp_body(second_image[raw + memcmp_start..raw + move_start])
	pe64_test_assert_runtime_move_body(second_image[raw + move_start..raw + thunk_start])
	for field, target in {
		5:  strlen_start
		10: wcslen_start
		15: memset_start
		20: memcmp_start
		25: move_start
		30: move_start
		35: move_start
		40: move_start
		45: thunk_start
	} {
		expected := pe64_checked_rel32(u64(field), u64(target)) or { panic(err) }
		assert pe64_test_read_u32(second_image, raw + field) == expected
	}
	assert second_image[raw + thunk_start..raw + thunk_start + 2] == [u8(0xff), 0x25]
	idata := sections[3]
	iat_rva := pe64_test_read_u32(second_image, int(idata.raw_pointer) + 16)
	thunk_field_offset := thunk_start + 2
	thunk_field_rva := text.virtual_address + u32(thunk_field_offset)
	expected_iat := pe64_checked_rel32(u64(thunk_field_rva), u64(iat_rva)) or { panic(err) }
	assert pe64_test_read_u32(second_image, raw + thunk_field_offset) == expected_iat
	pdata := sections[1]
	assert pdata.virtual_size == u32(pe64_runtime_function_size)
	assert pe64_test_read_u32(second_image, int(pdata.raw_pointer) + 4) == text.virtual_address +
		u32(first.object.text.len)
	assert first.object.text == first_before.text
	assert first.object.symbols == first_before.symbols
	assert first.object.call_relocations == first_before.call_relocations
	assert second.object.text == second_before.text
	assert second.object.symbols == second_before.symbols
	assert second.object.call_relocations == second_before.call_relocations
	assert first_definition.imports == first_imports_before
	assert first_definition.runtime_helpers == first_runtime_before
	assert second_definition.imports == second_imports_before
	assert second_definition.runtime_helpers == second_runtime_before
	first_image[0] = 0
	assert second_image[0] == `M`
}

fn test_pe64_runtime_move_family_preserves_binding_and_error_precedence() {
	object, entry, runtime_symbol := pe64_test_runtime_fixture('move_refusal_entry', 'memmove')
	copy_named_object, copy_named_entry, _ := pe64_test_runtime_fixture('copy_refusal_entry',
		'memcpy')
	before_object := pe64_test_clone_object(&object)
	copy_named_before := pe64_test_clone_object(&copy_named_object)
	root := pe64_test_root('runtime-move-refusal')
	defer {
		pe64_test_cleanup(root)
	}

	assert pe64_test_build_then_publish_error(&object, pe64_test_definition(entry,
		[]Pe64ImportBinding{}), os.join_path(root, 'unbound-memmove.exe')).contains('PE64 import binding missing for SymbolID')
	assert pe64_test_build_then_publish_error(&copy_named_object, pe64_test_definition(copy_named_entry,
		[]Pe64ImportBinding{}), os.join_path(root, 'unbound-memcpy.exe')).contains('PE64 import binding missing for SymbolID')

	non_microsoft := Pe64ImageDefinition{
		target_abi:      .unknown
		subsystem:       .windows_cui
		image_policy:    .fixed_base
		entry:           Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         .raw_noreturn_process_entry
		}
		runtime_helpers: [
			Pe64RuntimeBinding{
				symbol_id: runtime_symbol
				helper:    .unknown
			},
		]
	}
	non_microsoft_before := non_microsoft.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, non_microsoft, os.join_path(root, 'abi.exe')) == 'PE64 requires Microsoft x64 ABI'
	assert non_microsoft.runtime_helpers == non_microsoft_before

	invalid := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: SymbolID(99), helper: .memcpy },
	])
	invalid_before := invalid.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, invalid, os.join_path(root, 'invalid.exe')).contains('PE64 runtime binding: SymbolID 99 is out of range')
	assert invalid.runtime_helpers == invalid_before

	defined := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: entry, helper: .memmove },
	])
	defined_before := defined.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, defined, os.join_path(root, 'defined.exe')).contains('is not an external function')
	assert defined.runtime_helpers == defined_before

	memmove_duplicate := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memmove },
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memmove },
	])
	memmove_duplicate_before := memmove_duplicate.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, memmove_duplicate, os.join_path(root,
		'memmove-duplicate.exe')).contains('PE64 runtime binding: duplicate binding')
	assert memmove_duplicate.runtime_helpers == memmove_duplicate_before

	memcpy_duplicate := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memcpy },
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memcpy },
	])
	memcpy_duplicate_before := memcpy_duplicate.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, memcpy_duplicate, os.join_path(root,
		'memcpy-duplicate.exe')).contains('PE64 runtime binding: duplicate binding')
	assert memcpy_duplicate.runtime_helpers == memcpy_duplicate_before

	cross_kind_duplicate := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memmove },
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memcpy },
	])
	cross_kind_before := cross_kind_duplicate.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, cross_kind_duplicate, os.join_path(root,
		'cross-kind-duplicate.exe')).contains('PE64 runtime binding: duplicate binding')
	assert cross_kind_duplicate.runtime_helpers == cross_kind_before

	conflicting := pe64_test_runtime_definition(entry, [
		Pe64ImportBinding{
			symbol_id:   runtime_symbol
			dll:         'kernel32.dll'
			export_name: 'ExitProcess'
		},
	], [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memcpy },
	])
	conflicting_imports_before := conflicting.imports.clone()
	conflicting_runtime_before := conflicting.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, conflicting, os.join_path(root,
		'conflict.exe')).contains('has both import and runtime bindings')
	assert conflicting.imports == conflicting_imports_before
	assert conflicting.runtime_helpers == conflicting_runtime_before

	unknown := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .unknown },
	])
	unknown_before := unknown.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, unknown, os.join_path(root, 'unknown.exe')) == 'PE64 runtime binding: helper 0 is unsupported'
	assert unknown.runtime_helpers == unknown_before

	forged := pe64_test_runtime_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{
			symbol_id: runtime_symbol
			helper:    unsafe { Pe64RuntimeHelperKind(255) }
		},
	])
	forged_before := forged.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&object, forged, os.join_path(root, 'forged.exe')) == 'PE64 runtime binding: helper 255 is unsupported'
	assert forged.runtime_helpers == forged_before

	mut malformed_object := pe64_test_clone_object(&object)
	malformed_object.text[6] = 1
	malformed_definition := Pe64ImageDefinition{
		target_abi:      .unknown
		subsystem:       .windows_cui
		image_policy:    .fixed_base
		entry:           Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         .raw_noreturn_process_entry
		}
		runtime_helpers: [
			Pe64RuntimeBinding{
				symbol_id: runtime_symbol
				helper:    .memmove
			},
		]
	}
	malformed_before := malformed_definition.runtime_helpers.clone()
	assert pe64_test_build_then_publish_error(&malformed_object, malformed_definition, os.join_path(root,
		'malformed.exe')).contains('PE64 object contract')
	assert malformed_definition.runtime_helpers == malformed_before
	assert object.text == before_object.text
	assert object.symbols == before_object.symbols
	assert object.call_relocations == before_object.call_relocations
	assert copy_named_object.text == copy_named_before.text
	assert copy_named_object.symbols == copy_named_before.symbols
	assert copy_named_object.call_relocations == copy_named_before.call_relocations
}

fn test_pe64_runtime_move_family_dumpbin_disassembly_when_guarded() {
	$if windows && amd64 {
		if os.getenv(pe64_test_dumpbin_guard) != '1' {
			return
		}
		dumpbin := os.getenv(pe64_test_dumpbin_path)
		assert dumpbin.len != 0, '${pe64_test_dumpbin_path} must name an absolute Microsoft DUMPBIN path'
		assert os.is_abs_path(dumpbin), 'DUMPBIN path must be absolute'
		assert os.is_file(dumpbin) && !os.is_link(dumpbin), 'DUMPBIN path must be a regular file'
		object, entry, runtime_symbol := pe64_test_runtime_fixture('dumpbin_move_entry',
			'dumpbin_move_runtime')
		bytes := pe64_image_bytes(&object, pe64_test_runtime_definition(entry,
			[]Pe64ImportBinding{}, [
			Pe64RuntimeBinding{ symbol_id: runtime_symbol, helper: .memmove },
		])) or { panic(err) }
		text := pe64_test_section(bytes, '.text')
		helper_rva := text.virtual_address + u32(object.text.len)
		helper_offset := pe64_test_rva_offset(bytes, helper_rva)
		pe64_test_assert_runtime_move_body(bytes[helper_offset..helper_offset +
			int(pe64_runtime_move_size)])
		root := pe64_test_root('runtime-move-dumpbin')
		defer {
			pe64_test_cleanup(root)
		}
		output := os.join_path(root, 'runtime-move.exe')
		publish_object(output, bytes) or { assert false, err.msg() }
		result := pe64_test_run_process(dumpbin, ['/DISASM:BYTES', output],
			pe64_test_dumpbin_environment(), pe64_test_timeout_ms)
		assert !result.timed_out, 'DUMPBIN timed out'
		assert !result.output_limited, 'DUMPBIN exceeded the output limit'
		assert result.exit_code == 0, result.stderr
		instructions := pe64_test_dumpbin_instructions(result.stdout)
		helper_address := pe64_checked_add(pe64_image_base, u64(helper_rva),
			'DUMPBIN move helper address') or { panic(err) }
		mut helper_index := -1
		for index, instruction in instructions {
			if instruction.address == helper_address {
				assert helper_index == -1, 'DUMPBIN repeated the move helper start address'
				helper_index = index
			}
		}
		assert helper_index >= 0, 'DUMPBIN omitted the addressed move helper start'
		expected_offsets := [u64(0), 3, 6, 8, 11, 13, 17, 20, 22, 27, 32, 35, 38, 41, 44, 47, 49,
			50, 53, 56, 59, 62, 65, 68, 71, 73]
		expected_bytes := [
			[u8(0x48), 0x89, 0xc8],
			[u8(0x4d), 0x85, 0xc0],
			[u8(0x74), 0x41],
			[u8(0x48), 0x39, 0xd1],
			[u8(0x76), 0x25],
			[u8(0x4e), 0x8d, 0x0c, 0x02],
			[u8(0x4c), 0x39, 0xc9],
			[u8(0x73), 0x1c],
			[u8(0x4e), 0x8d, 0x54, 0x01, 0xff],
			[u8(0x4e), 0x8d, 0x5c, 0x02, 0xff],
			[u8(0x45), 0x8a, 0x0b],
			[u8(0x45), 0x88, 0x0a],
			[u8(0x49), 0xff, 0xca],
			[u8(0x49), 0xff, 0xcb],
			[u8(0x49), 0xff, 0xc8],
			[u8(0x75), 0xef],
			[u8(0xc3)],
			[u8(0x49), 0x89, 0xca],
			[u8(0x49), 0x89, 0xd3],
			[u8(0x45), 0x8a, 0x0b],
			[u8(0x45), 0x88, 0x0a],
			[u8(0x49), 0xff, 0xc2],
			[u8(0x49), 0xff, 0xc3],
			[u8(0x49), 0xff, 0xc8],
			[u8(0x75), 0xef],
			[u8(0xc3)],
		]
		expected_mnemonics := ['mov', 'test', '', 'cmp', 'jbe', 'lea', 'cmp', 'jae', 'lea', 'lea',
			'mov', 'mov', 'dec', 'dec', 'dec', '', 'ret', 'mov', 'mov', 'mov', 'mov', 'inc', 'inc',
			'dec', '', 'ret']
		expected_operands := ['rax,rcx', 'r8,r8', '', 'rcx,rdx', '', 'r9,[rdx+r8]', 'rcx,r9', '',
			'r10,[rcx+r8-1]', 'r11,[rdx+r8-1]', 'r9b,byteptr[r11]', 'byteptr[r10],r9b', 'r10',
			'r11', 'r8', '', '', 'r10,rcx', 'r11,rdx', 'r9b,byteptr[r11]', 'byteptr[r10],r9b',
			'r10', 'r11', 'r8', '', '']
		assert expected_offsets.len == 26
		assert expected_bytes.len == expected_offsets.len
		assert expected_mnemonics.len == expected_offsets.len
		assert expected_operands.len == expected_offsets.len
		assert helper_index + expected_offsets.len <= instructions.len
		helper := instructions[helper_index..helper_index + expected_offsets.len]
		for index in 0 .. expected_offsets.len {
			assert helper[index].address == helper_address + expected_offsets[index]
			assert helper[index].bytes == expected_bytes[index]
			if expected_mnemonics[index].len != 0 {
				assert helper[index].mnemonic == expected_mnemonics[index]
			}
			if expected_operands[index].len != 0 {
				assert pe64_test_compact_operands(helper[index].operands) == expected_operands[index]
			}
		}
		assert helper[2].mnemonic in ['je', 'jz']
		assert pe64_test_dumpbin_operand_has_address(helper[2].operands, helper_address + 73)
		assert pe64_test_dumpbin_operand_has_address(helper[4].operands, helper_address + 50)
		assert pe64_test_dumpbin_operand_has_address(helper[7].operands, helper_address + 50)
		assert helper[15].mnemonic in ['jne', 'jnz']
		assert pe64_test_dumpbin_operand_has_address(helper[15].operands, helper_address + 32)
		assert helper[24].mnemonic in ['jne', 'jnz']
		assert pe64_test_dumpbin_operand_has_address(helper[24].operands, helper_address + 56)
		assert helper[16].operands == ''
		assert helper[25].operands == ''
	} $else {
		return
	}
}

fn test_pe64_runtime_process_exit_explicit_binding_emits_exact_body_import_and_unwind() {
	object, entry, exit_symbol := pe64_test_runtime_fixture('process_exit_entry',
		'opaque_process_terminator')
	definition := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{
			symbol_id: exit_symbol
			helper:    .process_exit
		},
	], [pe64_test_process_exit_import()])
	before := pe64_test_clone_object(&object)
	runtime_before := definition.runtime_helpers.clone()
	runtime_imports_before := definition.runtime_imports.clone()
	runtime_plan := pe64_prepare_runtime_helpers(&object, definition.runtime_helpers) or {
		panic(err)
	}
	assert runtime_plan.physical == [Pe64RuntimeHelperKind.process_exit]
	assert runtime_plan.physical_offsets == [u64(object.text.len)]
	assert runtime_plan.symbol_physical_index[int(exit_symbol)] == 0
	assert runtime_plan.process_exit_physical_index == 0
	assert runtime_plan.size == pe64_runtime_process_exit_size
	import_plan := pe64_prepare_imports(&object, definition.imports, definition.runtime_imports,
		&runtime_plan) or { panic(err) }
	assert import_plan.physical == [
		Pe64PhysicalImport{
			dll:         pe64_runtime_process_exit_dll
			export_name: pe64_runtime_process_exit_export
		},
	]
	assert import_plan.symbol_physical_index[int(exit_symbol)] == -1
	assert import_plan.process_exit_physical_index == 0

	data := pe64_image_bytes(&object, definition) or { panic(err) }
	sections := pe64_test_sections(data)
	assert sections.map(it.name) == ['.text', '.pdata', '.xdata', '.idata']
	text := sections[0]
	pdata := sections[1]
	xdata := sections[2]
	idata := sections[3]
	helper_start := object.text.len
	thunk_start := helper_start + int(pe64_runtime_process_exit_size)
	assert text.virtual_size == u32(thunk_start + int(pe64_import_thunk_size))
	raw := int(text.raw_pointer)
	helper_rva := text.virtual_address + u32(helper_start)
	thunk_rva := text.virtual_address + u32(thunk_start)
	object_call := pe64_checked_rel32(u64(text.virtual_address + 5), u64(helper_rva)) or {
		panic(err)
	}
	helper_call := pe64_checked_rel32(u64(helper_rva + 5), u64(thunk_rva)) or { panic(err) }
	assert object_call == 6
	assert helper_call == 1
	assert pe64_test_read_u32(data, raw + 5) == object_call
	pe64_test_assert_runtime_process_exit_body(data[raw + helper_start..raw + thunk_start],
		helper_call)
	assert data[raw + thunk_start..raw + thunk_start + 2] == [u8(0xff), 0x25]
	imports := pe64_test_imports(data)
	assert imports == [
		Pe64TestImport{
			dll:         pe64_runtime_process_exit_dll
			export_name: pe64_runtime_process_exit_export
			iat_rva:     imports[0].iat_rva
		},
	]
	thunk_field_rva := thunk_rva + 2
	thunk_displacement := pe64_checked_rel32(u64(thunk_field_rva), u64(imports[0].iat_rva)) or {
		panic(err)
	}
	assert pe64_test_read_u32(data, raw + thunk_start + 2) == thunk_displacement

	assert pdata.virtual_size == u32(pe64_runtime_function_size * 2)
	assert xdata.virtual_size == u32(pe64_unwind_info_size * 2)
	pdata_raw := int(pdata.raw_pointer)
	assert pe64_test_read_u32(data, pdata_raw) == text.virtual_address
	assert pe64_test_read_u32(data, pdata_raw + 4) == text.virtual_address + u32(object.text.len)
	assert pe64_test_read_u32(data, pdata_raw + 8) == xdata.virtual_address
	assert pe64_test_read_u32(data, pdata_raw + 12) == helper_rva
	assert pe64_test_read_u32(data, pdata_raw + 16) == helper_rva +
		u32(pe64_runtime_process_exit_size)
	assert pe64_test_read_u32(data, pdata_raw + 20) == xdata.virtual_address +
		u32(pe64_unwind_info_size)
	xdata_raw := int(xdata.raw_pointer)
	expected_unwind := [u8(0x01), 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00]
	assert data[xdata_raw..xdata_raw + 8] == expected_unwind
	assert data[xdata_raw + 8..xdata_raw + 16] == expected_unwind
	assert pe64_test_directory(data, pe64_exception_directory_index) == Pe64TestDirectory{
		rva:  pdata.virtual_address
		size: pdata.virtual_size
	}
	assert pe64_test_directory(data, pe64_import_directory_index).rva == idata.virtual_address
	assert pe64_test_directory(data, pe64_base_relocation_directory_index) == Pe64TestDirectory{}
	assert 40 == 32 + 8
	assert (8 + 16 - 40 % 16) % 16 == 0
	for status in [u64(0), 1, 0x7fff_ffff, 0x8000_0000, 0xffff_ffff] {
		rcx_at_entry := 0xa5a5_a5a5_0000_0000 | status
		rcx_at_call := rcx_at_entry
		assert u32(rcx_at_call) == u32(status)
	}
	assert object.text == before.text
	assert object.symbols == before.symbols
	assert object.call_relocations == before.call_relocations
	assert definition.runtime_helpers == runtime_before
	assert definition.runtime_imports == runtime_imports_before
}

fn test_pe64_runtime_process_exit_import_deduplicates_without_conflating_owners() {
	first := pe64_test_process_exit_alias_fixture(false)
	second := pe64_test_process_exit_alias_fixture(true)
	first_definition := pe64_test_process_exit_definition(first.entry, [
		Pe64ImportBinding{
			symbol_id:   first.imported
			dll:         pe64_runtime_process_exit_dll
			export_name: pe64_runtime_process_exit_export
		},
	], [
		Pe64RuntimeBinding{ symbol_id: first.exit_b, helper: .process_exit },
		Pe64RuntimeBinding{ symbol_id: first.exit_a, helper: .process_exit },
	], [pe64_test_process_exit_import()])
	second_definition := pe64_test_process_exit_definition(second.entry, [
		Pe64ImportBinding{
			symbol_id:   second.imported
			dll:         pe64_runtime_process_exit_dll
			export_name: pe64_runtime_process_exit_export
		},
	], [
		Pe64RuntimeBinding{ symbol_id: second.exit_a, helper: .process_exit },
		Pe64RuntimeBinding{ symbol_id: second.exit_b, helper: .process_exit },
	], [pe64_test_process_exit_import()])
	first_before := pe64_test_clone_object(&first.object)
	second_before := pe64_test_clone_object(&second.object)
	first_helpers_before := first_definition.runtime_helpers.clone()
	first_imports_before := first_definition.imports.clone()
	first_runtime_imports_before := first_definition.runtime_imports.clone()
	first_runtime_plan := pe64_prepare_runtime_helpers(&first.object,
		first_definition.runtime_helpers) or { panic(err) }
	first_import_plan := pe64_prepare_imports(&first.object, first_definition.imports,
		first_definition.runtime_imports, &first_runtime_plan) or { panic(err) }
	assert first_runtime_plan.physical == [Pe64RuntimeHelperKind.process_exit]
	assert first_runtime_plan.symbol_physical_index[int(first.exit_a)] == 0
	assert first_runtime_plan.symbol_physical_index[int(first.exit_b)] == 0
	assert first_import_plan.physical.len == 1
	assert first_import_plan.process_exit_physical_index == 0
	assert first_import_plan.symbol_physical_index[int(first.imported)] == 0
	assert first_import_plan.symbol_physical_index[int(first.exit_a)] == -1
	assert first_import_plan.symbol_physical_index[int(first.exit_b)] == -1

	first_image := pe64_image_bytes(&first.object, first_definition) or { panic(err) }
	second_image := pe64_image_bytes(&second.object, second_definition) or { panic(err) }
	assert first_image == second_image
	assert pe64_test_imports(first_image).len == 1
	text := pe64_test_section(first_image, '.text')
	raw := int(text.raw_pointer)
	helper_start := first.object.text.len
	thunk_start := helper_start + int(pe64_runtime_process_exit_size)
	assert text.virtual_size == u32(thunk_start + int(pe64_import_thunk_size))
	for field in [5, 10] {
		expected := pe64_checked_rel32(u64(text.virtual_address + u32(field)), u64(
			text.virtual_address + u32(helper_start))) or { panic(err) }
		assert pe64_test_read_u32(first_image, raw + field) == expected
	}
	ordinary_call := pe64_checked_rel32(u64(text.virtual_address + 15), u64(text.virtual_address +
		u32(thunk_start))) or { panic(err) }
	assert pe64_test_read_u32(first_image, raw + 15) == ordinary_call
	helper_call := pe64_checked_rel32(u64(text.virtual_address + u32(helper_start + 5)), u64(
		text.virtual_address + u32(thunk_start))) or { panic(err) }
	pe64_test_assert_runtime_process_exit_body(first_image[raw + helper_start..raw + thunk_start],
		helper_call)
	assert pe64_test_section(first_image, '.pdata').virtual_size == u32(pe64_runtime_function_size * 2)
	assert first.object.text == first_before.text
	assert first.object.symbols == first_before.symbols
	assert first.object.call_relocations == first_before.call_relocations
	assert second.object.text == second_before.text
	assert second.object.symbols == second_before.symbols
	assert second.object.call_relocations == second_before.call_relocations
	assert first_definition.runtime_helpers == first_helpers_before
	assert first_definition.imports == first_imports_before
	assert first_definition.runtime_imports == first_runtime_imports_before
}

fn test_pe64_runtime_process_exit_orders_after_move_family_and_preserves_directories() {
	mut object := Object.new()
	entry := object.intern_function_symbol('process_exit_order_entry') or { panic(err) }
	strlen := object.intern_external_function_symbol('process_exit_order_strlen') or { panic(err) }
	wcslen := object.intern_external_function_symbol('process_exit_order_wcslen') or { panic(err) }
	memset := object.intern_external_function_symbol('process_exit_order_memset') or { panic(err) }
	memcmp := object.intern_external_function_symbol('process_exit_order_memcmp') or { panic(err) }
	memmove := object.intern_external_function_symbol('process_exit_order_memmove') or {
		panic(err)
	}
	memcpy := object.intern_external_function_symbol('process_exit_order_memcpy') or { panic(err) }
	process_exit := object.intern_external_function_symbol('process_exit_order_opaque') or {
		panic(err)
	}
	imported := object.intern_external_function_symbol('process_exit_order_import') or {
		panic(err)
	}
	mut body := [u8(0x48), 0x83, 0xec, 0x28]
	for _ in 0 .. 8 {
		body << [u8(0xe8), 0, 0, 0, 0]
	}
	body << [u8(0x48), 0x83, 0xc4, 0x28, 0xeb, 0xfe]
	assert object.append_text(body) or { panic(err) } == 0
	object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
	targets := [strlen, wcslen, memset, memcmp, memmove, memcpy, process_exit, imported]
	for index, target in targets {
		object.add_text_call_relocation(u64(5 + index * 5), target) or { panic(err) }
	}
	definition := pe64_test_process_exit_definition(entry, [
		Pe64ImportBinding{
			symbol_id:   imported
			dll:         'User32.dll'
			export_name: 'MessageBoxA'
		},
	], [
		Pe64RuntimeBinding{ symbol_id: memcpy, helper: .memcpy },
		Pe64RuntimeBinding{ symbol_id: process_exit, helper: .process_exit },
		Pe64RuntimeBinding{ symbol_id: strlen, helper: .strlen },
		Pe64RuntimeBinding{ symbol_id: memmove, helper: .memmove },
		Pe64RuntimeBinding{ symbol_id: memcmp, helper: .memcmp },
		Pe64RuntimeBinding{ symbol_id: wcslen, helper: .wcslen },
		Pe64RuntimeBinding{ symbol_id: memset, helper: .memset },
	], [pe64_test_process_exit_import()])
	before := pe64_test_clone_object(&object)
	imports_before := definition.imports.clone()
	helpers_before := definition.runtime_helpers.clone()
	runtime_imports_before := definition.runtime_imports.clone()
	runtime_plan := pe64_prepare_runtime_helpers(&object, definition.runtime_helpers) or {
		panic(err)
	}
	assert runtime_plan.physical == [
		Pe64RuntimeHelperKind.strlen,
		.wcslen,
		.memset,
		.memcmp,
		.memmove,
		.process_exit,
	]
	strlen_start := object.text.len
	wcslen_start := strlen_start + int(pe64_runtime_strlen_size)
	memset_start := wcslen_start + int(pe64_runtime_wcslen_size)
	memcmp_start := memset_start + int(pe64_runtime_memset_size)
	move_start := memcmp_start + int(pe64_runtime_memcmp_size)
	process_exit_start := move_start + int(pe64_runtime_move_size)
	runtime_end := process_exit_start + int(pe64_runtime_process_exit_size)
	assert runtime_plan.physical_offsets == [
		u64(strlen_start),
		u64(wcslen_start),
		u64(memset_start),
		u64(memcmp_start),
		u64(move_start),
		u64(process_exit_start),
	]
	import_plan := pe64_prepare_imports(&object, definition.imports, definition.runtime_imports,
		&runtime_plan) or { panic(err) }
	assert import_plan.physical == [
		Pe64PhysicalImport{
			dll:         pe64_runtime_process_exit_dll
			export_name: pe64_runtime_process_exit_export
		},
		Pe64PhysicalImport{
			dll:         'User32.dll'
			export_name: 'MessageBoxA'
		},
	]
	assert import_plan.process_exit_physical_index == 0
	assert import_plan.symbol_physical_index[int(imported)] == 1

	data := pe64_image_bytes(&object, definition) or { panic(err) }
	sections := pe64_test_sections(data)
	assert sections.map(it.name) == ['.text', '.pdata', '.xdata', '.idata']
	for section in sections {
		assert section.characteristics & pe64_section_mem_execute == 0
			|| section.characteristics & pe64_section_mem_write == 0
	}
	text := sections[0]
	pdata := sections[1]
	xdata := sections[2]
	idata := sections[3]
	first_thunk := runtime_end
	second_thunk := first_thunk + int(pe64_import_thunk_size)
	assert text.virtual_size == u32(second_thunk + int(pe64_import_thunk_size))
	raw := int(text.raw_pointer)
	pe64_test_assert_runtime_strlen_body(data[raw + strlen_start..raw + wcslen_start])
	pe64_test_assert_runtime_wcslen_body(data[raw + wcslen_start..raw + memset_start])
	pe64_test_assert_runtime_memset_body(data[raw + memset_start..raw + memcmp_start])
	pe64_test_assert_runtime_memcmp_body(data[raw + memcmp_start..raw + move_start])
	pe64_test_assert_runtime_move_body(data[raw + move_start..raw + process_exit_start])
	process_call := pe64_checked_rel32(u64(text.virtual_address + u32(process_exit_start + 5)), u64(
		text.virtual_address + u32(first_thunk))) or { panic(err) }
	pe64_test_assert_runtime_process_exit_body(data[raw + process_exit_start..raw + runtime_end],
		process_call)
	object_targets := [strlen_start, wcslen_start, memset_start, memcmp_start, move_start, move_start,
		process_exit_start, second_thunk]
	for index, target in object_targets {
		field := 5 + index * 5
		expected := pe64_checked_rel32(u64(text.virtual_address + u32(field)), u64(
			text.virtual_address + u32(target))) or { panic(err) }
		assert pe64_test_read_u32(data, raw + field) == expected
	}
	assert data[raw + first_thunk..raw + first_thunk + 2] == [u8(0xff), 0x25]
	assert data[raw + second_thunk..raw + second_thunk + 2] == [u8(0xff), 0x25]
	assert pe64_test_imports(data) == [
		Pe64TestImport{
			dll:         pe64_runtime_process_exit_dll
			export_name: pe64_runtime_process_exit_export
			iat_rva:     pe64_test_imports(data)[0].iat_rva
		},
		Pe64TestImport{
			dll:         'User32.dll'
			export_name: 'MessageBoxA'
			iat_rva:     pe64_test_imports(data)[1].iat_rva
		},
	]
	assert pdata.virtual_size == u32(pe64_runtime_function_size * 2)
	assert xdata.virtual_size == u32(pe64_unwind_info_size * 2)
	pdata_raw := int(pdata.raw_pointer)
	assert pe64_test_read_u32(data, pdata_raw + 12) == text.virtual_address +
		u32(process_exit_start)
	assert pe64_test_read_u32(data, pdata_raw + 16) == text.virtual_address + u32(runtime_end)
	assert pe64_test_read_u32(data, pdata_raw + 20) == xdata.virtual_address +
		u32(pe64_unwind_info_size)
	assert pe64_test_directory(data, pe64_exception_directory_index) == Pe64TestDirectory{
		rva:  pdata.virtual_address
		size: pdata.virtual_size
	}
	assert pe64_test_directory(data, pe64_import_directory_index).rva == idata.virtual_address
	iat_directory := pe64_test_directory(data, pe64_iat_directory_index)
	assert iat_directory.rva >= idata.virtual_address
	assert u64(iat_directory.rva) + u64(iat_directory.size) <= u64(idata.virtual_address) +
		u64(idata.virtual_size)
	assert pe64_test_directory(data, pe64_base_relocation_directory_index) == Pe64TestDirectory{}
	assert object.text == before.text
	assert object.symbols == before.symbols
	assert object.call_relocations == before.call_relocations
	assert definition.imports == imports_before
	assert definition.runtime_helpers == helpers_before
	assert definition.runtime_imports == runtime_imports_before
}

fn test_pe64_runtime_process_exit_refuses_invalid_ownership_and_preserves_error_precedence() {
	object, entry, exit_symbol := pe64_test_runtime_fixture('process_exit_refusal_entry',
		'process_exit_refusal_opaque')
	before := pe64_test_clone_object(&object)
	root := pe64_test_root('runtime-process-exit-refusal')
	defer {
		pe64_test_cleanup(root)
	}
	helper := Pe64RuntimeBinding{
		symbol_id: exit_symbol
		helper:    .process_exit
	}
	exact_import := pe64_test_process_exit_import()
	missing := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		helper,
	], []Pe64RuntimeImportBinding{})
	assert pe64_test_build_then_publish_error(&object, missing, os.join_path(root, 'missing.exe')) == 'PE64 runtime import binding: process_exit binding is missing'

	orphan := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{},
		[]Pe64RuntimeBinding{}, [exact_import])
	assert pe64_test_build_then_publish_error(&object, orphan, os.join_path(root, 'orphan.exe')) == 'PE64 runtime import binding: process_exit binding is orphaned'

	for wrong in [
		Pe64RuntimeImportBinding{
			helper:      .process_exit
			dll:         'kernel32.dll'
			export_name: pe64_runtime_process_exit_export
		},
		Pe64RuntimeImportBinding{
			helper:      .process_exit
			dll:         pe64_runtime_process_exit_dll
			export_name: 'exitprocess'
		},
		Pe64RuntimeImportBinding{
			helper:      .process_exit
			dll:         pe64_runtime_process_exit_dll
			export_name: 'TerminateProcess'
		},
	] {
		definition := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
			helper,
		], [wrong])
		assert pe64_test_build_then_publish_error(&object, definition, os.join_path(root,
			'wrong-${wrong.export_name}.exe')).contains('requires exact `Kernel32.dll` / `ExitProcess`')
	}

	duplicate := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		helper,
	], [
		exact_import,
		exact_import,
	])
	assert pe64_test_build_then_publish_error(&object, duplicate, os.join_path(root,
		'duplicate.exe')) == 'PE64 runtime import binding: duplicate binding for process_exit'

	invalid_symbol := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{
			symbol_id: SymbolID(99)
			helper:    .process_exit
		},
	], [exact_import])
	assert pe64_test_build_then_publish_error(&object, invalid_symbol, os.join_path(root,
		'invalid-symbol.exe')).contains('PE64 runtime binding: SymbolID 99 is out of range')

	defined_symbol := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{
			symbol_id: entry
			helper:    .process_exit
		},
	], [exact_import])
	assert pe64_test_build_then_publish_error(&object, defined_symbol, os.join_path(root,
		'defined-symbol.exe')).contains('is not an external function')

	duplicate_helper := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		helper,
		helper,
	], [exact_import])
	assert pe64_test_build_then_publish_error(&object, duplicate_helper, os.join_path(root,
		'duplicate-helper.exe')).contains('PE64 runtime binding: duplicate binding')

	for unsupported in [
		Pe64RuntimeHelperKind.unknown,
		unsafe { Pe64RuntimeHelperKind(255) },
	] {
		definition := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
			Pe64RuntimeBinding{
				symbol_id: exit_symbol
				helper:    unsupported
			},
		], [exact_import])
		assert pe64_test_build_then_publish_error(&object, definition, os.join_path(root,
			'unsupported-helper-${int(unsupported)}.exe')) == 'PE64 runtime binding: helper ${int(unsupported)} is unsupported'
	}

	for unsupported in [
		Pe64RuntimeHelperKind.unknown,
		.strlen,
		unsafe { Pe64RuntimeHelperKind(255) },
	] {
		definition := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
			helper,
		], [
			Pe64RuntimeImportBinding{
				helper:      unsupported
				dll:         pe64_runtime_process_exit_dll
				export_name: pe64_runtime_process_exit_export
			},
		])
		assert pe64_test_build_then_publish_error(&object, definition, os.join_path(root,
			'unsupported-${int(unsupported)}.exe')) == 'PE64 runtime import binding: helper ${int(unsupported)} is unsupported'
	}

	conflict := pe64_test_process_exit_definition(entry, [
		Pe64ImportBinding{
			symbol_id:   exit_symbol
			dll:         pe64_runtime_process_exit_dll
			export_name: pe64_runtime_process_exit_export
		},
	], [helper], [exact_import])
	assert pe64_test_build_then_publish_error(&object, conflict, os.join_path(root, 'conflict.exe')).contains('has both import and runtime bindings')

	aliases := pe64_test_process_exit_alias_fixture(false)
	case_collision := pe64_test_process_exit_definition(aliases.entry, [
		Pe64ImportBinding{
			symbol_id:   aliases.imported
			dll:         'kernel32.dll'
			export_name: pe64_runtime_process_exit_export
		},
	], [
		Pe64RuntimeBinding{ symbol_id: aliases.exit_a, helper: .process_exit },
		Pe64RuntimeBinding{ symbol_id: aliases.exit_b, helper: .process_exit },
	], [exact_import])
	assert pe64_test_build_then_publish_error(&aliases.object, case_collision, os.join_path(root,
		'case-collision.exe')).contains('differ only by ASCII case')

	non_microsoft := Pe64ImageDefinition{
		target_abi:      .unknown
		subsystem:       .windows_cui
		image_policy:    .fixed_base
		entry:           Pe64EntryDefinition{
			function_index: u32(entry)
			policy:         .raw_noreturn_process_entry
		}
		runtime_helpers: [helper]
		runtime_imports: [
			Pe64RuntimeImportBinding{
				helper:      .unknown
				dll:         ''
				export_name: ''
			},
		]
	}
	assert pe64_test_build_then_publish_error(&object, non_microsoft, os.join_path(root,
		'abi-precedence.exe')) == 'PE64 requires Microsoft x64 ABI'

	mut malformed := pe64_test_clone_object(&object)
	malformed.text[6] = 1
	assert pe64_test_build_then_publish_error(&malformed, non_microsoft, os.join_path(root,
		'object-precedence.exe')).contains('PE64 object contract')

	mut unreferenced := Object.new()
	unreferenced_entry := unreferenced.intern_function_symbol('unreferenced_entry') or {
		panic(err)
	}
	unreferenced_exit := unreferenced.intern_external_function_symbol('unreferenced_exit') or {
		panic(err)
	}
	assert unreferenced.append_text([u8(0xeb), 0xfe]) or { panic(err) } == 0
	unreferenced.define_text_function(unreferenced_entry, 0, 2) or { panic(err) }
	unreferenced_definition := pe64_test_process_exit_definition(unreferenced_entry,
		[]Pe64ImportBinding{}, [
		Pe64RuntimeBinding{
			symbol_id: unreferenced_exit
			helper:    .process_exit
		},
	], [exact_import])
	assert pe64_test_build_then_publish_error(&unreferenced, unreferenced_definition, os.join_path(root,
		'unreferenced.exe')).contains('PE64 object contract')

	assert pe64_checked_rel32(0, u64(2_147_483_651)) or { panic(err) } == u32(0x7fff_ffff)
	assert pe64_checked_rel32(u64(2_147_483_644), 0) or { panic(err) } == u32(0x8000_0000)
	if _ := pe64_checked_rel32(0, u64(2_147_483_652)) {
		assert false, 'positive process_exit rel32 overflow was accepted'
	} else {
		assert err.msg() == 'PE64 REL32 overflow'
	}
	if _ := pe64_checked_rel32(u64(2_147_483_645), 0) {
		assert false, 'negative process_exit rel32 overflow was accepted'
	} else {
		assert err.msg() == 'PE64 REL32 overflow'
	}
	assert object.text == before.text
	assert object.symbols == before.symbols
	assert object.call_relocations == before.call_relocations
	assert missing.runtime_helpers == [helper]
	assert missing.runtime_imports.len == 0
	assert duplicate.runtime_imports == [exact_import, exact_import]
}

fn test_pe64_runtime_process_exit_does_not_infer_activation_from_entry_or_names() {
	exit_entry_object, exit_entry := pe64_test_leaf_object('ExitProcess')
	opaque_entry_object, opaque_entry := pe64_test_leaf_object('opaque_entry')
	exit_entry_bytes := pe64_image_bytes(&exit_entry_object, pe64_test_definition(exit_entry,
		[]Pe64ImportBinding{})) or { panic(err) }
	opaque_entry_bytes := pe64_image_bytes(&opaque_entry_object, pe64_test_definition(opaque_entry,
		[]Pe64ImportBinding{})) or { panic(err) }
	assert exit_entry_bytes == opaque_entry_bytes
	assert pe64_test_sections(exit_entry_bytes).map(it.name) == ['.text']
	assert pe64_test_imports(exit_entry_bytes).len == 0
	assert pe64_test_directory(exit_entry_bytes, pe64_exception_directory_index) == Pe64TestDirectory{}

	named_object, named_entry, named_external := pe64_test_runtime_fixture('named_exit_entry',
		'ExitProcess')
	assert pe64_test_error(&named_object, pe64_test_definition(named_entry, []Pe64ImportBinding{})).contains('PE64 import binding missing for SymbolID')
	ordinary_definition := pe64_test_definition(named_entry, [
		Pe64ImportBinding{
			symbol_id:   named_external
			dll:         pe64_runtime_process_exit_dll
			export_name: pe64_runtime_process_exit_export
		},
	])
	ordinary := pe64_image_bytes(&named_object, ordinary_definition) or { panic(err) }
	ordinary_text := pe64_test_section(ordinary, '.text')
	assert ordinary_text.virtual_size == u32(named_object.text.len) + u32(pe64_import_thunk_size)
	assert pe64_test_section(ordinary, '.pdata').virtual_size == u32(pe64_runtime_function_size)
	assert pe64_test_imports(ordinary).len == 1
	assert ordinary[int(ordinary_text.raw_pointer) + named_object.text.len..
		int(ordinary_text.raw_pointer) + named_object.text.len + 2] == [u8(0xff), 0x25]

	explicit_empty := Pe64ImageDefinition{
		target_abi:      .windows_x64_microsoft
		subsystem:       .windows_cui
		image_policy:    .fixed_base
		entry:           Pe64EntryDefinition{
			function_index: u32(opaque_entry)
			policy:         .raw_noreturn_process_entry
		}
		runtime_imports: []Pe64RuntimeImportBinding{}
	}
	assert pe64_image_bytes(&opaque_entry_object, explicit_empty) or { panic(err) } == opaque_entry_bytes

	opaque_object, explicit_entry, opaque_external := pe64_test_runtime_fixture('explicit_exit_entry',
		'opaque_runtime_subject')
	explicit := pe64_image_bytes(&opaque_object, pe64_test_process_exit_definition(explicit_entry,
		[]Pe64ImportBinding{}, [
		Pe64RuntimeBinding{
			symbol_id: opaque_external
			helper:    .process_exit
		},
	], [pe64_test_process_exit_import()])) or { panic(err) }
	explicit_text := pe64_test_section(explicit, '.text')
	helper_start := int(explicit_text.raw_pointer) + opaque_object.text.len
	assert explicit[helper_start..helper_start + 5] == [u8(0x48), 0x83, 0xec, 0x28, 0xe8]
	assert pe64_test_imports(explicit)[0].dll == pe64_runtime_process_exit_dll
}

fn pe64_test_find_process_exit_llvm_objdump() string {
	for candidate in [
		'/usr/bin/llvm-objdump-21',
		'/usr/lib/llvm-21/bin/llvm-objdump',
		'llvm-objdump-21',
		'llvm-objdump',
	] {
		if candidate.starts_with('/') {
			if os.is_executable(candidate) {
				return candidate
			}
			continue
		}
		path := os.find_abs_path_of_executable(candidate) or { '' }
		if path.len > 0 && os.is_executable(path) {
			return path
		}
	}
	return ''
}

fn pe64_test_assert_process_exit_disassembly(instructions []Pe64TestDisassemblyInstruction,
	helper_address u64, thunk_address u64) {
	mut helper_index := -1
	for index, instruction in instructions {
		if instruction.address == helper_address {
			assert helper_index == -1, 'disassembler repeated the process_exit helper address'
			helper_index = index
		}
	}
	assert helper_index >= 0, 'disassembler omitted the process_exit helper'
	assert helper_index + 4 <= instructions.len
	decoded := instructions[helper_index..helper_index + 4]
	assert decoded[0].address == helper_address
	assert decoded[0].bytes == [u8(0x48), 0x83, 0xec, 0x28]
	assert decoded[0].mnemonic in ['sub', 'subq']
	assert pe64_test_compact_operands(decoded[0].operands).contains('rsp')
	assert pe64_test_compact_operands(decoded[0].operands).contains('28')
	assert decoded[1].address == helper_address + 4
	assert decoded[1].bytes == [u8(0xe8), 0x01, 0x00, 0x00, 0x00]
	assert decoded[1].mnemonic in ['call', 'callq']
	assert pe64_test_dumpbin_operand_has_address(decoded[1].operands, thunk_address)
	assert decoded[2].address == helper_address + 9
	assert decoded[2].bytes == [u8(0xcc)]
	assert decoded[2].mnemonic in ['int', 'int3']
	assert decoded[3].address == thunk_address
	assert decoded[3].bytes.len == int(pe64_import_thunk_size)
	assert decoded[3].bytes[0..2] == [u8(0xff), 0x25]
	assert decoded[3].mnemonic in ['jmp', 'jmpq']
}

fn test_pe64_runtime_process_exit_llvm_and_dumpbin_disassembly_when_guarded() {
	mandatory_llvm := os.getenv(pe64_test_process_exit_llvm_guard) == '1'
	object, entry, exit_symbol := pe64_test_runtime_fixture('process_exit_oracle_entry',
		'process_exit_oracle_opaque')
	bytes := pe64_image_bytes(&object, pe64_test_process_exit_definition(entry,
		[]Pe64ImportBinding{}, [
		Pe64RuntimeBinding{
			symbol_id: exit_symbol
			helper:    .process_exit
		},
	], [pe64_test_process_exit_import()])) or { panic(err) }
	text := pe64_test_section(bytes, '.text')
	helper_rva := text.virtual_address + u32(object.text.len)
	thunk_rva := helper_rva + u32(pe64_runtime_process_exit_size)
	helper_offset := pe64_test_rva_offset(bytes, helper_rva)
	pe64_test_assert_runtime_process_exit_body(bytes[helper_offset..helper_offset +
		int(pe64_runtime_process_exit_size)], 1)
	root := pe64_test_root('runtime process_exit ; oracle')
	defer {
		pe64_test_cleanup(root)
	}
	output := os.join_path(root, 'process exit ; oracle.exe')
	publish_object(output, bytes) or { assert false, err.msg() }

	llvm_objdump := pe64_test_find_process_exit_llvm_objdump()
	if llvm_objdump.len == 0 {
		assert !mandatory_llvm, 'mandatory LLVM 21 process_exit oracle is unavailable'
	} else {
		version := pe64_test_run_process(llvm_objdump, ['--version'],
			pe64_test_dumpbin_environment(), pe64_test_timeout_ms)
		compatible := version.exit_code == 0 && !version.timed_out && !version.output_limited
			&& version.stdout.contains('LLVM version 21.1.8')
		if !compatible {
			assert !mandatory_llvm, 'mandatory LLVM process_exit oracle fingerprint mismatch:\n${version.stdout}\n${version.stderr}'
		} else {
			result := pe64_test_run_process(llvm_objdump, [
				'--disassemble',
				'--x86-asm-syntax=intel',
				output,
			], pe64_test_dumpbin_environment(), pe64_test_timeout_ms)
			assert !result.timed_out, 'LLVM process_exit oracle timed out'
			assert !result.output_limited, 'LLVM process_exit oracle exceeded the output limit'
			assert result.exit_code == 0, result.stderr
			instructions := pe64_test_dumpbin_instructions(result.stdout)
			helper_address := pe64_image_base + u64(helper_rva)
			thunk_address := pe64_image_base + u64(thunk_rva)
			pe64_test_assert_process_exit_disassembly(instructions, helper_address, thunk_address)
		}
	}

	$if windows && amd64 {
		if os.getenv(pe64_test_dumpbin_guard) == '1' {
			dumpbin := os.getenv(pe64_test_dumpbin_path)
			assert dumpbin.len != 0, '${pe64_test_dumpbin_path} must name an absolute Microsoft DUMPBIN path'
			assert os.is_abs_path(dumpbin), 'DUMPBIN path must be absolute'
			assert os.is_file(dumpbin) && !os.is_link(dumpbin), 'DUMPBIN path must be a regular file'
			result := pe64_test_run_process(dumpbin, ['/DISASM:BYTES', output],
				pe64_test_dumpbin_environment(), pe64_test_timeout_ms)
			assert !result.timed_out, 'DUMPBIN process_exit oracle timed out'
			assert !result.output_limited, 'DUMPBIN process_exit oracle exceeded the output limit'
			assert result.exit_code == 0, result.stderr
			pe64_test_assert_process_exit_disassembly(pe64_test_dumpbin_instructions(result.stdout),

				pe64_image_base + u64(helper_rva), pe64_image_base + u64(thunk_rva))
		}
	}
}

fn pe64_test_heap_runtime_import(helper Pe64RuntimeHelperKind,
	export_name string) Pe64RuntimeImportBinding {
	return Pe64RuntimeImportBinding{
		helper:      helper
		dll:         pe64_runtime_heap_dll
		export_name: export_name
	}
}

fn pe64_test_malloc_runtime_imports() []Pe64RuntimeImportBinding {
	return [
		pe64_test_heap_runtime_import(.malloc, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.malloc, pe64_runtime_heap_alloc_export),
	]
}

fn pe64_test_free_runtime_imports() []Pe64RuntimeImportBinding {
	return [
		pe64_test_heap_runtime_import(.free, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.free, pe64_runtime_heap_free_export),
	]
}

fn pe64_test_calloc_runtime_imports() []Pe64RuntimeImportBinding {
	return [
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_heap_alloc_export),
	]
}

fn pe64_test_runtime_malloc_template() []u8 {
	return [u8(0x48), 0x83, 0xec, 0x28, 0x48, 0x89, 0x4c, 0x24, 0x20, 0xe8, 0, 0, 0, 0, 0x48, 0x85,
		0xc0, 0x74, 0x31, 0x48, 0x89, 0xc1, 0x31, 0xd2, 0x4c, 0x8b, 0x44, 0x24, 0x20, 0x49, 0x83,
		0xc0, 0x18, 0x72, 0x21, 0xe8, 0, 0, 0, 0, 0x48, 0x85, 0xc0, 0x74, 0x17, 0x49, 0x89, 0xc3,
		0x49, 0x83, 0xc3, 0x17, 0x49, 0x83, 0xe3, 0xf0, 0x49, 0x89, 0x43, 0xf8, 0x4c, 0x89, 0xd8,
		0x48, 0x83, 0xc4, 0x28, 0xc3, 0x31, 0xc0, 0x48, 0x83, 0xc4, 0x28, 0xc3]
}

fn pe64_test_runtime_free_template() []u8 {
	return [u8(0x48), 0x83, 0xec, 0x28, 0x48, 0x85, 0xc9, 0x74, 0x22, 0x48, 0x8b, 0x41, 0xf8, 0x48,
		0x89, 0x44, 0x24, 0x20, 0xe8, 0, 0, 0, 0, 0x48, 0x85, 0xc0, 0x74, 0x0f, 0x48, 0x89, 0xc1,
		0x31, 0xd2, 0x4c, 0x8b, 0x44, 0x24, 0x20, 0xe8, 0, 0, 0, 0, 0x48, 0x83, 0xc4, 0x28, 0xc3]
}

fn pe64_test_runtime_calloc_template() []u8 {
	return [u8(0x48), 0x83, 0xec, 0x28, 0x48, 0x89, 0xc8, 0x48, 0xf7, 0xe2, 0x48, 0x85, 0xd2, 0x75,
		0x43, 0x48, 0x89, 0x44, 0x24, 0x20, 0xe8, 0, 0, 0, 0, 0x48, 0x85, 0xc0, 0x74, 0x34, 0x48,
		0x89, 0xc1, 0xba, 0x08, 0, 0, 0, 0x4c, 0x8b, 0x44, 0x24, 0x20, 0x49, 0x83, 0xc0, 0x18,
		0x72, 0x21, 0xe8, 0, 0, 0, 0, 0x48, 0x85, 0xc0, 0x74, 0x17, 0x49, 0x89, 0xc3, 0x49, 0x83,
		0xc3, 0x17, 0x49, 0x83, 0xe3, 0xf0, 0x49, 0x89, 0x43, 0xf8, 0x4c, 0x89, 0xd8, 0x48, 0x83,
		0xc4, 0x28, 0xc3, 0x31, 0xc0, 0x48, 0x83, 0xc4, 0x28, 0xc3]
}

fn pe64_test_put_u32(mut data []u8, offset int, value u32) {
	assert offset >= 0 && offset <= data.len - 4
	data[offset] = u8(value)
	data[offset + 1] = u8(value >> 8)
	data[offset + 2] = u8(value >> 16)
	data[offset + 3] = u8(value >> 24)
}

fn pe64_test_rel32_bits(field_rva u64, target_rva u64) u32 {
	assert field_rva <= u64(max_i64) - 4
	assert target_rva <= u64(max_i64)
	delta := i64(target_rva) - i64(field_rva + 4)
	assert delta >= i64(-2_147_483_648) && delta <= i64(2_147_483_647)
	return u32(i32(delta))
}

fn pe64_test_rel32_target(field_rva u64, bits u32) u64 {
	assert field_rva <= u64(max_i64) - 4
	target := i64(field_rva + 4) + i64(i32(bits))
	assert target >= 0
	return u64(target)
}

fn pe64_test_runtime_body(data []u8, text Pe64TestSection, offset int, size int) []u8 {
	assert offset >= 0 && size >= 0
	start := int(text.raw_pointer) + offset
	assert start >= 0 && start <= data.len - size
	return data[start..start + size].clone()
}

struct Pe64TestMallocOutcome {
mut:
	result                 u64
	allocation_size        u64
	cookie_address         u64
	cookie_value           u64
	get_process_heap_calls int
	heap_alloc_calls       int
}

fn pe64_test_model_malloc(size u64, heap u64, raw u64) Pe64TestMallocOutcome {
	mut outcome := Pe64TestMallocOutcome{
		get_process_heap_calls: 1
	}
	if heap == 0 || size > max_u64 - 24 {
		return outcome
	}
	outcome.allocation_size = size + 24
	outcome.heap_alloc_calls = 1
	if raw == 0 {
		return outcome
	}
	assert raw <= max_u64 - 23
	outcome.result = (raw + 23) & ~u64(15)
	assert outcome.result >= 8
	outcome.cookie_address = outcome.result - 8
	outcome.cookie_value = raw
	return outcome
}

struct Pe64TestCallocOutcome {
mut:
	result                  u64
	product                 u64
	allocation_size         u64
	cookie_address          u64
	cookie_value            u64
	multiplication_overflow bool
	addition_overflow       bool
	payload_zeroed          bool
	get_process_heap_calls  int
	heap_alloc_calls        int
	heap_alloc_flags        u64
}

fn pe64_test_model_calloc(number u64, size u64, heap u64, raw u64) Pe64TestCallocOutcome {
	mut outcome := Pe64TestCallocOutcome{}
	if number != 0 && size > max_u64 / number {
		outcome.multiplication_overflow = true
		return outcome
	}
	outcome.product = number * size
	outcome.get_process_heap_calls = 1
	if heap == 0 {
		return outcome
	}
	if outcome.product > max_u64 - 24 {
		outcome.addition_overflow = true
		return outcome
	}
	outcome.allocation_size = outcome.product + 24
	outcome.heap_alloc_calls = 1
	outcome.heap_alloc_flags = 8
	if raw == 0 {
		return outcome
	}
	assert raw <= max_u64 - 23
	outcome.result = (raw + 23) & ~u64(15)
	assert outcome.result >= 8
	outcome.cookie_address = outcome.result - 8
	outcome.cookie_value = raw
	outcome.payload_zeroed = true
	return outcome
}

struct Pe64TestFreeOutcome {
mut:
	cookie_read            bool
	raw                    u64
	get_process_heap_calls int
	heap_free_calls        int
	heap_free_heap         u64
	heap_free_flags        u64
	heap_free_pointer      u64
}

fn pe64_test_model_free(pointer u64, cookie u64, heap u64) Pe64TestFreeOutcome {
	if pointer == 0 {
		return Pe64TestFreeOutcome{}
	}
	mut outcome := Pe64TestFreeOutcome{
		cookie_read:            true
		raw:                    cookie
		get_process_heap_calls: 1
	}
	if heap == 0 {
		return outcome
	}
	outcome.heap_free_calls = 1
	outcome.heap_free_heap = heap
	outcome.heap_free_flags = 0
	outcome.heap_free_pointer = cookie
	return outcome
}

struct Pe64TestHeapAliasFixture {
	object   Object
	entry    SymbolID
	malloc_a SymbolID
	malloc_b SymbolID
	free_a   SymbolID
	free_b   SymbolID
	calloc_a SymbolID
	calloc_b SymbolID
	imported SymbolID
}

fn pe64_test_heap_alias_fixture(reverse_relocations bool) Pe64TestHeapAliasFixture {
	mut object := Object.new()
	entry := object.intern_function_symbol('heap_alias_entry') or { panic(err) }
	malloc_a := object.intern_external_function_symbol('heap_alias_malloc_a') or { panic(err) }
	malloc_b := object.intern_external_function_symbol('heap_alias_malloc_b') or { panic(err) }
	free_a := object.intern_external_function_symbol('heap_alias_free_a') or { panic(err) }
	free_b := object.intern_external_function_symbol('heap_alias_free_b') or { panic(err) }
	calloc_a := object.intern_external_function_symbol('heap_alias_calloc_a') or { panic(err) }
	calloc_b := object.intern_external_function_symbol('heap_alias_calloc_b') or { panic(err) }
	imported := object.intern_external_function_symbol('heap_alias_import') or { panic(err) }
	mut body := [u8(0x48), 0x83, 0xec, 0x28]
	for _ in 0 .. 7 {
		body << [u8(0xe8), 0, 0, 0, 0]
	}
	body << [u8(0x48), 0x83, 0xc4, 0x28, 0xeb, 0xfe]
	assert object.append_text(body) or { panic(err) } == 0
	object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
	relocations := [
		TextCallRelocation{
			offset:    5
			symbol_id: malloc_a
		},
		TextCallRelocation{
			offset:    10
			symbol_id: malloc_b
		},
		TextCallRelocation{
			offset:    15
			symbol_id: free_a
		},
		TextCallRelocation{
			offset:    20
			symbol_id: free_b
		},
		TextCallRelocation{
			offset:    25
			symbol_id: calloc_a
		},
		TextCallRelocation{
			offset:    30
			symbol_id: calloc_b
		},
		TextCallRelocation{
			offset:    35
			symbol_id: imported
		},
	]
	if reverse_relocations {
		for index := relocations.len - 1; index >= 0; index-- {
			relocation := relocations[index]
			object.add_text_call_relocation(relocation.offset, relocation.symbol_id) or {
				panic(err)
			}
		}
	} else {
		for relocation in relocations {
			object.add_text_call_relocation(relocation.offset, relocation.symbol_id) or {
				panic(err)
			}
		}
	}
	return Pe64TestHeapAliasFixture{
		object:   object
		entry:    entry
		malloc_a: malloc_a
		malloc_b: malloc_b
		free_a:   free_a
		free_b:   free_b
		calloc_a: calloc_a
		calloc_b: calloc_b
		imported: imported
	}
}

fn pe64_test_assert_calloc_exact_body_abi_imports_and_unwind() {
	assert int(Pe64RuntimeHelperKind.calloc) == 10
	object, entry, calloc_symbol := pe64_test_runtime_fixture('calloc_entry',
		'opaque_zeroed_heap_allocator')
	definition := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{
			symbol_id: calloc_symbol
			helper:    .calloc
		},
	], [
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_heap_alloc_export),
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_get_process_heap_export),
	])
	object_before := pe64_test_clone_object(&object)
	definition_before := pe64_test_clone_image_definition(&definition)
	runtime_plan := pe64_prepare_runtime_helpers(&object, definition.runtime_helpers) or {
		panic(err)
	}
	assert runtime_plan.physical == [Pe64RuntimeHelperKind.calloc]
	assert runtime_plan.physical_offsets == [u64(object.text.len)]
	assert runtime_plan.symbol_physical_index[int(calloc_symbol)] == 0
	assert runtime_plan.process_exit_physical_index == -1
	assert runtime_plan.malloc_physical_index == -1
	assert runtime_plan.free_physical_index == -1
	assert runtime_plan.calloc_physical_index == 0
	assert runtime_plan.size == pe64_runtime_calloc_size
	import_plan := pe64_prepare_imports(&object, definition.imports, definition.runtime_imports,
		&runtime_plan) or { panic(err) }
	assert import_plan.physical.map('${it.dll}/${it.export_name}') == [
		'Kernel32.dll/GetProcessHeap',
		'Kernel32.dll/HeapAlloc',
	]
	assert import_plan.get_process_heap_physical_index == 0
	assert import_plan.heap_alloc_physical_index == 1
	assert import_plan.heap_free_physical_index == -1

	data := pe64_test_malloc_free_image_bytes(&object, definition) or { panic(err) }
	sections := pe64_test_sections(data)
	assert sections.map(it.name) == ['.text', '.pdata', '.xdata', '.idata']
	text := sections[0]
	pdata := sections[1]
	xdata := sections[2]
	helper_offset := object.text.len
	helper_rva := u64(text.virtual_address) + u64(helper_offset)
	thunk_base_offset := helper_offset + int(pe64_runtime_calloc_size)
	get_process_heap_rva := u64(text.virtual_address) + u64(thunk_base_offset)
	heap_alloc_rva := get_process_heap_rva + pe64_import_thunk_size
	mut expected := pe64_test_runtime_calloc_template()
	assert expected.len == int(pe64_runtime_calloc_size)
	assert pe64_runtime_helper_bytes(.calloc) or { panic(err) } == expected
	pe64_test_put_u32(mut expected, 21, pe64_test_rel32_bits(helper_rva + 21, get_process_heap_rva))
	pe64_test_put_u32(mut expected, 50, pe64_test_rel32_bits(helper_rva + 50, heap_alloc_rva))
	body := pe64_test_runtime_body(data, text, helper_offset, int(pe64_runtime_calloc_size))
	assert body == expected
	assert body[0..4] == [u8(0x48), 0x83, 0xec, 0x28]
	assert body[4..10] == [u8(0x48), 0x89, 0xc8, 0x48, 0xf7, 0xe2]
	assert body[10..15] == [u8(0x48), 0x85, 0xd2, 0x75, 0x43]
	modrm_oracles := [
		Pe64TestModrmOracle{
			offset: 2
			mode:   3
			reg:    5
			rm:     4
		},
		Pe64TestModrmOracle{
			offset: 6
			mode:   3
			reg:    1
			rm:     0
		},
		Pe64TestModrmOracle{
			offset: 9
			mode:   3
			reg:    4
			rm:     2
		},
		Pe64TestModrmOracle{
			offset: 12
			mode:   3
			reg:    2
			rm:     2
		},
		Pe64TestModrmOracle{
			offset: 17
			mode:   1
			reg:    0
			rm:     4
		},
		Pe64TestModrmOracle{
			offset: 27
			mode:   3
			reg:    0
			rm:     0
		},
		Pe64TestModrmOracle{
			offset: 32
			mode:   3
			reg:    0
			rm:     1
		},
		Pe64TestModrmOracle{
			offset: 40
			mode:   1
			reg:    0
			rm:     4
		},
		Pe64TestModrmOracle{
			offset: 45
			mode:   3
			reg:    0
			rm:     0
		},
		Pe64TestModrmOracle{
			offset: 56
			mode:   3
			reg:    0
			rm:     0
		},
		Pe64TestModrmOracle{
			offset: 61
			mode:   3
			reg:    0
			rm:     3
		},
		Pe64TestModrmOracle{
			offset: 64
			mode:   3
			reg:    0
			rm:     3
		},
		Pe64TestModrmOracle{
			offset: 68
			mode:   3
			reg:    4
			rm:     3
		},
		Pe64TestModrmOracle{
			offset: 72
			mode:   1
			reg:    0
			rm:     3
		},
		Pe64TestModrmOracle{
			offset: 76
			mode:   3
			reg:    3
			rm:     0
		},
		Pe64TestModrmOracle{
			offset: 79
			mode:   3
			reg:    0
			rm:     4
		},
		Pe64TestModrmOracle{
			offset: 83
			mode:   3
			reg:    0
			rm:     0
		},
		Pe64TestModrmOracle{
			offset: 86
			mode:   3
			reg:    0
			rm:     4
		},
	]
	assert modrm_oracles.map(it.offset) == [2, 6, 9, 12, 17, 27, 32, 40, 45, 56, 61, 64, 68, 72,
		76, 79, 83, 86]
	for oracle in modrm_oracles {
		mode, reg, rm := pe64_test_modrm_fields(body[oracle.offset])
		assert mode == oracle.mode
		assert reg == oracle.reg
		assert rm == oracle.rm
	}
	for sib_offset in [18, 41] {
		scale, index, base := pe64_test_sib_fields(body[sib_offset])
		assert scale == 0
		assert index == 4
		assert base == 4
	}
	assert body[20] == 0xe8 && body[49] == 0xe8
	assert pe64_test_rel32_target(helper_rva + 21, pe64_test_read_u32(body, 21)) == get_process_heap_rva
	assert pe64_test_rel32_target(helper_rva + 50, pe64_test_read_u32(body, 50)) == heap_alloc_rva
	assert pe64_test_rel8_target(15, body[14]) == 82
	assert pe64_test_rel8_target(30, body[29]) == 82
	assert pe64_test_rel8_target(49, body[48]) == 82
	assert pe64_test_rel8_target(59, body[58]) == 82
	assert body[15..20] == [u8(0x48), 0x89, 0x44, 0x24, 0x20]
	assert body[30..38] == [u8(0x48), 0x89, 0xc1, 0xba, 0x08, 0, 0, 0]
	assert body[38..49] == [u8(0x4c), 0x8b, 0x44, 0x24, 0x20, 0x49, 0x83, 0xc0, 0x18, 0x72, 0x21]
	assert body[59..77] == [u8(0x49), 0x89, 0xc3, 0x49, 0x83, 0xc3, 0x17, 0x49, 0x83, 0xe3, 0xf0,
		0x49, 0x89, 0x43, 0xf8, 0x4c, 0x89, 0xd8]
	assert body[77..89] == [u8(0x48), 0x83, 0xc4, 0x28, 0xc3, 0x31, 0xc0, 0x48, 0x83, 0xc4, 0x28,
		0xc3]
	assert body[..4] != [u8(0x48), 0x89, 0xc8, 0x48]
	object_call_bits := pe64_test_read_u32(data, int(text.raw_pointer) + 5)
	assert pe64_test_rel32_target(u64(text.virtual_address) + 5, object_call_bits) == helper_rva
	assert object.text != body[..object.text.len]

	assert pdata.virtual_size == u32(pe64_runtime_function_size * 2)
	assert xdata.virtual_size == u32(pe64_unwind_info_size * 2)
	pdata_raw := int(pdata.raw_pointer)
	assert pe64_test_read_u32(data, pdata_raw + 12) == u32(helper_rva)
	assert pe64_test_read_u32(data, pdata_raw + 16) == u32(helper_rva + pe64_runtime_calloc_size)
	assert pe64_test_read_u32(data, pdata_raw + 20) == xdata.virtual_address +
		u32(pe64_unwind_info_size)
	unwind := [u8(0x01), 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00]
	xdata_raw := int(xdata.raw_pointer)
	assert data[xdata_raw..xdata_raw + 8] == unwind
	assert data[xdata_raw + 8..xdata_raw + 16] == unwind
	pe64_test_assert_object_snapshot(&object, &object_before)
	pe64_test_assert_image_definition_snapshot(&definition, &definition_before)
}

fn test_pe64_runtime_malloc_exact_body_abi_imports_and_unwind() {
	assert int(Pe64RuntimeHelperKind.malloc) == 8
	object, entry, malloc_symbol := pe64_test_runtime_fixture('malloc_entry',
		'opaque_heap_allocator')
	definition := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{
			symbol_id: malloc_symbol
			helper:    .malloc
		},
	], [
		pe64_test_heap_runtime_import(.malloc, pe64_runtime_heap_alloc_export),
		pe64_test_heap_runtime_import(.malloc, pe64_runtime_get_process_heap_export),
	])
	before := pe64_test_clone_object(&object)
	definition_before := pe64_test_clone_image_definition(&definition)
	helpers_before := definition.runtime_helpers.clone()
	imports_before := definition.runtime_imports.clone()
	runtime_plan := pe64_prepare_runtime_helpers(&object, definition.runtime_helpers) or {
		panic(err)
	}
	assert runtime_plan.physical == [Pe64RuntimeHelperKind.malloc]
	assert runtime_plan.physical_offsets == [u64(object.text.len)]
	assert runtime_plan.symbol_physical_index[int(malloc_symbol)] == 0
	assert runtime_plan.process_exit_physical_index == -1
	assert runtime_plan.malloc_physical_index == 0
	assert runtime_plan.free_physical_index == -1
	assert runtime_plan.size == pe64_runtime_malloc_size
	import_plan := pe64_prepare_imports(&object, definition.imports, definition.runtime_imports,
		&runtime_plan) or { panic(err) }
	assert import_plan.physical == [
		Pe64PhysicalImport{
			dll:         pe64_runtime_heap_dll
			export_name: pe64_runtime_get_process_heap_export
		},
		Pe64PhysicalImport{
			dll:         pe64_runtime_heap_dll
			export_name: pe64_runtime_heap_alloc_export
		},
	]
	assert import_plan.process_exit_physical_index == -1
	assert import_plan.get_process_heap_physical_index == 0
	assert import_plan.heap_alloc_physical_index == 1
	assert import_plan.heap_free_physical_index == -1

	data := pe64_test_malloc_free_image_bytes(&object, definition) or { panic(err) }
	sections := pe64_test_sections(data)
	assert sections.map(it.name) == ['.text', '.pdata', '.xdata', '.idata']
	text := sections[0]
	pdata := sections[1]
	xdata := sections[2]
	helper_offset := object.text.len
	helper_rva := u64(text.virtual_address) + u64(helper_offset)
	thunk_base_offset := helper_offset + int(pe64_runtime_malloc_size)
	get_process_heap_rva := u64(text.virtual_address) + u64(thunk_base_offset)
	heap_alloc_rva := get_process_heap_rva + pe64_import_thunk_size
	mut expected := pe64_test_runtime_malloc_template()
	assert expected.len == int(pe64_runtime_malloc_size)
	assert pe64_runtime_helper_bytes(.malloc) or { panic(err) } == expected
	pe64_test_put_u32(mut expected, 10, pe64_test_rel32_bits(helper_rva + 10, get_process_heap_rva))
	pe64_test_put_u32(mut expected, 36, pe64_test_rel32_bits(helper_rva + 36, heap_alloc_rva))
	body := pe64_test_runtime_body(data, text, helper_offset, int(pe64_runtime_malloc_size))
	assert body == expected
	assert body[0..9] == [u8(0x48), 0x83, 0xec, 0x28, 0x48, 0x89, 0x4c, 0x24, 0x20]
	assert body[9] == 0xe8 && body[35] == 0xe8
	assert pe64_test_rel32_target(helper_rva + 10, pe64_test_read_u32(body, 10)) == get_process_heap_rva
	assert pe64_test_rel32_target(helper_rva + 36, pe64_test_read_u32(body, 36)) == heap_alloc_rva
	assert body[17] == 0x74 && pe64_test_rel8_target(19, body[18]) == 68
	assert body[33] == 0x72 && pe64_test_rel8_target(35, body[34]) == 68
	assert body[43] == 0x74 && pe64_test_rel8_target(45, body[44]) == 68
	assert body[24..35] == [u8(0x4c), 0x8b, 0x44, 0x24, 0x20, 0x49, 0x83, 0xc0, 0x18, 0x72, 0x21]
	assert body[45..60] == [u8(0x49), 0x89, 0xc3, 0x49, 0x83, 0xc3, 0x17, 0x49, 0x83, 0xe3, 0xf0,
		0x49, 0x89, 0x43, 0xf8]
	assert body[68..75] == [u8(0x31), 0xc0, 0x48, 0x83, 0xc4, 0x28, 0xc3]
	object_call_bits := pe64_test_read_u32(data, int(text.raw_pointer) + 5)
	assert pe64_test_rel32_target(u64(text.virtual_address) + 5, object_call_bits) == helper_rva

	assert pe64_test_imports(data).map('${it.dll}/${it.export_name}') == [
		'Kernel32.dll/GetProcessHeap',
		'Kernel32.dll/HeapAlloc',
	]
	assert pdata.virtual_size == u32(pe64_runtime_function_size * 2)
	assert xdata.virtual_size == u32(pe64_unwind_info_size * 2)
	pdata_raw := int(pdata.raw_pointer)
	assert pe64_test_read_u32(data, pdata_raw) == text.virtual_address
	assert pe64_test_read_u32(data, pdata_raw + 4) == text.virtual_address + u32(object.text.len)
	assert pe64_test_read_u32(data, pdata_raw + 8) == xdata.virtual_address
	assert pe64_test_read_u32(data, pdata_raw + 12) == u32(helper_rva)
	assert pe64_test_read_u32(data, pdata_raw + 16) == u32(helper_rva + pe64_runtime_malloc_size)
	assert pe64_test_read_u32(data, pdata_raw + 20) == xdata.virtual_address +
		u32(pe64_unwind_info_size)
	unwind := [u8(0x01), 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00]
	xdata_raw := int(xdata.raw_pointer)
	assert data[xdata_raw..xdata_raw + 8] == unwind
	assert data[xdata_raw + 8..xdata_raw + 16] == unwind

	success := pe64_test_model_malloc(13, 0x7777, 0x1000)
	assert success.get_process_heap_calls == 1
	assert success.heap_alloc_calls == 1
	assert success.allocation_size == 37
	assert success.result == 0x1010
	assert success.result & 15 == 0
	assert success.cookie_address == 0x1008
	assert success.cookie_value == 0x1000
	assert object.text == before.text
	assert object.symbols == before.symbols
	assert object.call_relocations == before.call_relocations
	assert definition.runtime_helpers == helpers_before
	assert definition.runtime_imports == imports_before
	pe64_test_assert_object_snapshot(&object, &before)
	pe64_test_assert_image_definition_snapshot(&definition, &definition_before)
	pe64_test_assert_calloc_exact_body_abi_imports_and_unwind()
}

fn test_pe64_runtime_free_exact_v3_body_null_path_cookie_and_unwind() {
	assert int(Pe64RuntimeHelperKind.free) == 9
	object, entry, free_symbol := pe64_test_runtime_fixture('free_entry', 'opaque_heap_release')
	definition := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{
			symbol_id: free_symbol
			helper:    .free
		},
	], [
		pe64_test_heap_runtime_import(.free, pe64_runtime_heap_free_export),
		pe64_test_heap_runtime_import(.free, pe64_runtime_get_process_heap_export),
	])
	before := pe64_test_clone_object(&object)
	definition_before := pe64_test_clone_image_definition(&definition)
	helpers_before := definition.runtime_helpers.clone()
	imports_before := definition.runtime_imports.clone()
	runtime_plan := pe64_prepare_runtime_helpers(&object, definition.runtime_helpers) or {
		panic(err)
	}
	assert runtime_plan.physical == [Pe64RuntimeHelperKind.free]
	assert runtime_plan.physical_offsets == [u64(object.text.len)]
	assert runtime_plan.malloc_physical_index == -1
	assert runtime_plan.free_physical_index == 0
	assert runtime_plan.size == pe64_runtime_free_size
	import_plan := pe64_prepare_imports(&object, definition.imports, definition.runtime_imports,
		&runtime_plan) or { panic(err) }
	assert import_plan.physical.map('${it.dll}/${it.export_name}') == [
		'Kernel32.dll/GetProcessHeap',
		'Kernel32.dll/HeapFree',
	]
	assert import_plan.get_process_heap_physical_index == 0
	assert import_plan.heap_alloc_physical_index == -1
	assert import_plan.heap_free_physical_index == 1

	data := pe64_test_malloc_free_image_bytes(&object, definition) or { panic(err) }
	text := pe64_test_section(data, '.text')
	pdata := pe64_test_section(data, '.pdata')
	xdata := pe64_test_section(data, '.xdata')
	helper_offset := object.text.len
	helper_rva := u64(text.virtual_address) + u64(helper_offset)
	thunk_base_offset := helper_offset + int(pe64_runtime_free_size)
	get_process_heap_rva := u64(text.virtual_address) + u64(thunk_base_offset)
	heap_free_rva := get_process_heap_rva + pe64_import_thunk_size
	mut expected := pe64_test_runtime_free_template()
	assert expected.len == int(pe64_runtime_free_size)
	assert pe64_runtime_helper_bytes(.free) or { panic(err) } == expected
	pe64_test_put_u32(mut expected, 19, pe64_test_rel32_bits(helper_rva + 19, get_process_heap_rva))
	pe64_test_put_u32(mut expected, 39, pe64_test_rel32_bits(helper_rva + 39, heap_free_rva))
	body := pe64_test_runtime_body(data, text, helper_offset, int(pe64_runtime_free_size))
	assert body == expected
	assert body[0..4] == [u8(0x48), 0x83, 0xec, 0x28]
	assert body[4..9] == [u8(0x48), 0x85, 0xc9, 0x74, 0x22]
	assert pe64_test_rel8_target(9, body[8]) == 43
	assert body[9..18] == [u8(0x48), 0x8b, 0x41, 0xf8, 0x48, 0x89, 0x44, 0x24, 0x20]
	assert body[18] == 0xe8 && body[38] == 0xe8
	assert pe64_test_rel32_target(helper_rva + 19, pe64_test_read_u32(body, 19)) == get_process_heap_rva
	assert body[26] == 0x74 && pe64_test_rel8_target(28, body[27]) == 43
	assert pe64_test_rel32_target(helper_rva + 39, pe64_test_read_u32(body, 39)) == heap_free_rva
	assert body[43..48] == [u8(0x48), 0x83, 0xc4, 0x28, 0xc3]
	assert pe64_test_imports(data).map('${it.dll}/${it.export_name}') == [
		'Kernel32.dll/GetProcessHeap',
		'Kernel32.dll/HeapFree',
	]

	assert pdata.virtual_size == u32(pe64_runtime_function_size * 2)
	assert xdata.virtual_size == u32(pe64_unwind_info_size * 2)
	pdata_raw := int(pdata.raw_pointer)
	assert pe64_test_read_u32(data, pdata_raw + 12) == u32(helper_rva)
	assert pe64_test_read_u32(data, pdata_raw + 16) == u32(helper_rva + pe64_runtime_free_size)
	assert pe64_test_read_u32(data, pdata_raw + 20) == xdata.virtual_address +
		u32(pe64_unwind_info_size)
	unwind := [u8(0x01), 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00]
	xdata_raw := int(xdata.raw_pointer)
	assert data[xdata_raw..xdata_raw + 8] == unwind
	assert data[xdata_raw + 8..xdata_raw + 16] == unwind

	null_path := pe64_test_model_free(0, 0xaaaa, 0xbbbb)
	assert !null_path.cookie_read
	assert null_path.get_process_heap_calls == 0
	assert null_path.heap_free_calls == 0
	success := pe64_test_model_free(0x1010, 0x1000, 0x7777)
	assert success.cookie_read
	assert success.raw == 0x1000
	assert success.get_process_heap_calls == 1
	assert success.heap_free_calls == 1
	assert success.heap_free_heap == 0x7777
	assert success.heap_free_flags == 0
	assert success.heap_free_pointer == 0x1000
	assert object.text == before.text
	assert object.symbols == before.symbols
	assert object.call_relocations == before.call_relocations
	assert definition.runtime_helpers == helpers_before
	assert definition.runtime_imports == imports_before
	pe64_test_assert_object_snapshot(&object, &before)
	pe64_test_assert_image_definition_snapshot(&definition, &definition_before)
}

fn test_pe64_runtime_malloc_free_cookie_family_aliases_stable_ids_and_ordering() {
	first := pe64_test_heap_alias_fixture(false)
	second := pe64_test_heap_alias_fixture(true)
	first_before := pe64_test_clone_object(&first.object)
	second_before := pe64_test_clone_object(&second.object)
	first_helpers := [
		Pe64RuntimeBinding{
			symbol_id: first.free_b
			helper:    .free
		},
		Pe64RuntimeBinding{
			symbol_id: first.calloc_b
			helper:    .calloc
		},
		Pe64RuntimeBinding{
			symbol_id: first.malloc_b
			helper:    .malloc
		},
		Pe64RuntimeBinding{
			symbol_id: first.free_a
			helper:    .free
		},
		Pe64RuntimeBinding{
			symbol_id: first.calloc_a
			helper:    .calloc
		},
		Pe64RuntimeBinding{
			symbol_id: first.malloc_a
			helper:    .malloc
		},
	]
	second_helpers := [
		Pe64RuntimeBinding{
			symbol_id: second.malloc_a
			helper:    .malloc
		},
		Pe64RuntimeBinding{
			symbol_id: second.calloc_a
			helper:    .calloc
		},
		Pe64RuntimeBinding{
			symbol_id: second.free_a
			helper:    .free
		},
		Pe64RuntimeBinding{
			symbol_id: second.malloc_b
			helper:    .malloc
		},
		Pe64RuntimeBinding{
			symbol_id: second.calloc_b
			helper:    .calloc
		},
		Pe64RuntimeBinding{
			symbol_id: second.free_b
			helper:    .free
		},
	]
	first_runtime_imports := [
		pe64_test_heap_runtime_import(.free, pe64_runtime_heap_free_export),
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_heap_alloc_export),
		pe64_test_heap_runtime_import(.malloc, pe64_runtime_heap_alloc_export),
		pe64_test_heap_runtime_import(.free, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.malloc, pe64_runtime_get_process_heap_export),
	]
	second_runtime_imports := [
		pe64_test_heap_runtime_import(.malloc, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.free, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.malloc, pe64_runtime_heap_alloc_export),
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_heap_alloc_export),
		pe64_test_heap_runtime_import(.free, pe64_runtime_heap_free_export),
	]
	first_definition := pe64_test_process_exit_definition(first.entry, [
		Pe64ImportBinding{
			symbol_id:   first.imported
			dll:         pe64_runtime_heap_dll
			export_name: pe64_runtime_get_process_heap_export
		},
	], first_helpers, first_runtime_imports)
	second_definition := pe64_test_process_exit_definition(second.entry, [
		Pe64ImportBinding{
			symbol_id:   second.imported
			dll:         pe64_runtime_heap_dll
			export_name: pe64_runtime_get_process_heap_export
		},
	], second_helpers, second_runtime_imports)
	first_definition_before := pe64_test_clone_image_definition(&first_definition)
	second_definition_before := pe64_test_clone_image_definition(&second_definition)
	first_plan := pe64_prepare_runtime_helpers(&first.object, first_definition.runtime_helpers) or {
		panic(err)
	}
	second_plan := pe64_prepare_runtime_helpers(&second.object, second_definition.runtime_helpers) or {
		panic(err)
	}
	assert first_plan.physical == [.malloc, .free, .calloc]
	assert second_plan.physical == first_plan.physical
	assert first_plan.physical_offsets == [u64(first.object.text.len),
		u64(first.object.text.len) + pe64_runtime_malloc_size,
		u64(first.object.text.len) +
			pe64_runtime_malloc_size + pe64_runtime_free_size]
	assert second_plan.physical_offsets == first_plan.physical_offsets
	assert first_plan.symbol_physical_index[int(first.malloc_a)] == 0
	assert first_plan.symbol_physical_index[int(first.malloc_b)] == 0
	assert first_plan.symbol_physical_index[int(first.free_a)] == 1
	assert first_plan.symbol_physical_index[int(first.free_b)] == 1
	assert first_plan.symbol_physical_index[int(first.calloc_a)] == 2
	assert first_plan.symbol_physical_index[int(first.calloc_b)] == 2
	assert second_plan.symbol_physical_index == first_plan.symbol_physical_index
	assert second_plan.process_exit_physical_index == first_plan.process_exit_physical_index
	assert second_plan.malloc_physical_index == first_plan.malloc_physical_index
	assert second_plan.free_physical_index == first_plan.free_physical_index
	assert second_plan.calloc_physical_index == first_plan.calloc_physical_index
	assert second_plan.size == first_plan.size
	first_import_plan := pe64_prepare_imports(&first.object, first_definition.imports,
		first_definition.runtime_imports, &first_plan) or { panic(err) }
	second_import_plan := pe64_prepare_imports(&second.object, second_definition.imports,
		second_definition.runtime_imports, &second_plan) or { panic(err) }
	assert first_import_plan.physical.map('${it.dll}/${it.export_name}') == [
		'Kernel32.dll/GetProcessHeap',
		'Kernel32.dll/HeapAlloc',
		'Kernel32.dll/HeapFree',
	]
	assert first_import_plan.symbol_physical_index[int(first.imported)] == 0
	assert first_import_plan.get_process_heap_physical_index == 0
	assert first_import_plan.heap_alloc_physical_index == 1
	assert first_import_plan.heap_free_physical_index == 2
	assert second_import_plan.physical == first_import_plan.physical
	assert second_import_plan.symbol_physical_index == first_import_plan.symbol_physical_index
	assert second_import_plan.process_exit_physical_index == first_import_plan.process_exit_physical_index
	assert second_import_plan.get_process_heap_physical_index == first_import_plan.get_process_heap_physical_index
	assert second_import_plan.heap_alloc_physical_index == first_import_plan.heap_alloc_physical_index
	assert second_import_plan.heap_free_physical_index == first_import_plan.heap_free_physical_index

	first_image := pe64_test_malloc_free_image_bytes(&first.object, first_definition) or {
		panic(err)
	}
	second_image := pe64_test_malloc_free_image_bytes(&second.object, second_definition) or {
		panic(err)
	}
	assert first_image == second_image
	first_imports := pe64_test_imports(first_image)
	second_imports := pe64_test_imports(second_image)
	assert first_imports.map('${it.dll}/${it.export_name}') == [
		'Kernel32.dll/GetProcessHeap',
		'Kernel32.dll/HeapAlloc',
		'Kernel32.dll/HeapFree',
	]
	assert second_imports == first_imports
	text := pe64_test_section(first_image, '.text')
	malloc_rva := u64(text.virtual_address) + u64(first.object.text.len)
	free_rva := malloc_rva + pe64_runtime_malloc_size
	calloc_rva := free_rva + pe64_runtime_free_size
	for field_offset in [5, 10] {
		bits := pe64_test_read_u32(first_image, int(text.raw_pointer) + field_offset)
		assert pe64_test_rel32_target(u64(text.virtual_address) + u64(field_offset), bits) == malloc_rva
	}
	for field_offset in [15, 20] {
		bits := pe64_test_read_u32(first_image, int(text.raw_pointer) + field_offset)
		assert pe64_test_rel32_target(u64(text.virtual_address) + u64(field_offset), bits) == free_rva
	}
	for field_offset in [25, 30] {
		bits := pe64_test_read_u32(first_image, int(text.raw_pointer) + field_offset)
		assert pe64_test_rel32_target(u64(text.virtual_address) + u64(field_offset), bits) == calloc_rva
	}
	thunk_base_rva := calloc_rva + pe64_runtime_calloc_size
	import_bits := pe64_test_read_u32(first_image, int(text.raw_pointer) + 35)
	assert pe64_test_rel32_target(u64(text.virtual_address) + 35, import_bits) == thunk_base_rva
	malloc_body := pe64_test_runtime_body(first_image, text, first.object.text.len,
		int(pe64_runtime_malloc_size))
	free_body := pe64_test_runtime_body(first_image, text, first.object.text.len +
		int(pe64_runtime_malloc_size), int(pe64_runtime_free_size))
	calloc_body := pe64_test_runtime_body(first_image, text, first.object.text.len +
		int(pe64_runtime_malloc_size + pe64_runtime_free_size), int(pe64_runtime_calloc_size))
	assert malloc_body[56..60] == [u8(0x49), 0x89, 0x43, 0xf8]
	assert free_body[9..13] == [u8(0x48), 0x8b, 0x41, 0xf8]
	assert calloc_body[70..74] == [u8(0x49), 0x89, 0x43, 0xf8]
	assert pe64_test_section(first_image, '.pdata').virtual_size == u32(pe64_runtime_function_size * 4)
	assert pe64_test_section(first_image, '.xdata').virtual_size == u32(pe64_unwind_info_size * 4)
	assert first.object.text == first_before.text
	assert first.object.symbols == first_before.symbols
	assert first.object.call_relocations == first_before.call_relocations
	assert second.object.text == second_before.text
	assert second.object.symbols == second_before.symbols
	assert second.object.call_relocations == second_before.call_relocations
	assert first_definition.runtime_helpers == first_helpers
	assert first_definition.runtime_imports == first_runtime_imports
	assert second_definition.runtime_helpers == second_helpers
	assert second_definition.runtime_imports == second_runtime_imports
	pe64_test_assert_object_snapshot(&first.object, &first_before)
	pe64_test_assert_object_snapshot(&second.object, &second_before)
	pe64_test_assert_image_definition_snapshot(&first_definition, &first_definition_before)
	pe64_test_assert_image_definition_snapshot(&second_definition, &second_definition_before)
}

fn pe64_test_assert_calloc_publish_refusal(root string, name string, o &Object,
	definition Pe64ImageDefinition, expected string) {
	final_path := os.join_path(root, '${name}.exe')
	message := pe64_test_malloc_free_publish_error(o, definition, final_path)
	if expected.len == 0 {
		assert message.contains('calloc requires exact')
	} else {
		assert message == expected
	}
	assert !pe64_test_path_present(final_path)
	assert !pe64_test_path_present(publication_stage_path(final_path))
}

fn pe64_test_assert_calloc_import_ownership_matrix(root string) {
	object, entry, calloc_symbol := pe64_test_runtime_fixture('calloc_import_entry',
		'calloc_import_opaque')
	calloc_helper := Pe64RuntimeBinding{
		symbol_id: calloc_symbol
		helper:    .calloc
	}
	valid_imports := pe64_test_calloc_runtime_imports()
	valid_definition := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		calloc_helper,
	], valid_imports)
	valid_image := pe64_test_malloc_free_image_bytes(&object, valid_definition) or { panic(err) }
	assert pe64_test_imports(valid_image).map('${it.dll}/${it.export_name}') == [
		'Kernel32.dll/GetProcessHeap',
		'Kernel32.dll/HeapAlloc',
	]
	assert 'HeapFree' !in pe64_test_imports(valid_image).map(it.export_name)
	assert pe64_test_imports(valid_image).all(it.dll != 'ucrtbase.dll' && it.dll != 'msvcrt.dll')

	missing_get := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		calloc_helper,
	], [pe64_test_heap_runtime_import(.calloc, pe64_runtime_heap_alloc_export)])
	pe64_test_assert_calloc_publish_refusal(root, 'calloc-missing-get', &object, missing_get,
		'PE64 runtime import binding: calloc GetProcessHeap binding is missing')
	missing_alloc := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		calloc_helper,
	], [pe64_test_heap_runtime_import(.calloc, pe64_runtime_get_process_heap_export)])
	pe64_test_assert_calloc_publish_refusal(root, 'calloc-missing-alloc', &object, missing_alloc,
		'PE64 runtime import binding: calloc HeapAlloc binding is missing')

	duplicate_get := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		calloc_helper,
	], [
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_heap_alloc_export),
	])
	pe64_test_assert_calloc_publish_refusal(root, 'calloc-duplicate-get', &object, duplicate_get,
		'PE64 runtime import binding: duplicate calloc GetProcessHeap binding')
	duplicate_alloc := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		calloc_helper,
	], [
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_heap_alloc_export),
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_heap_alloc_export),
	])
	pe64_test_assert_calloc_publish_refusal(root, 'calloc-duplicate-alloc', &object,
		duplicate_alloc, 'PE64 runtime import binding: duplicate calloc HeapAlloc binding')

	wrong_case := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		calloc_helper,
	], [
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_heap_alloc_export),
		Pe64RuntimeImportBinding{
			helper:      .calloc
			dll:         'kernel32.dll'
			export_name: pe64_runtime_get_process_heap_export
		},
	])
	pe64_test_assert_calloc_publish_refusal(root, 'calloc-wrong-case', &object, wrong_case, '')
	wrong_dll := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		calloc_helper,
	], [
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_heap_alloc_export),
		Pe64RuntimeImportBinding{
			helper:      .calloc
			dll:         'OtherHeap.dll'
			export_name: pe64_runtime_get_process_heap_export
		},
	])
	pe64_test_assert_calloc_publish_refusal(root, 'calloc-wrong-dll', &object, wrong_dll, '')
	wrong_export := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		calloc_helper,
	], [
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_heap_alloc_export),
		Pe64RuntimeImportBinding{
			helper:      .calloc
			dll:         pe64_runtime_heap_dll
			export_name: 'heapalloc'
		},
	])
	pe64_test_assert_calloc_publish_refusal(root, 'calloc-wrong-export', &object, wrong_export, '')
	extra_heap_free := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		calloc_helper,
	], [
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_heap_alloc_export),
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_heap_free_export),
	])
	pe64_test_assert_calloc_publish_refusal(root, 'calloc-extra-heap-free', &object,
		extra_heap_free, '')

	for name, orphan_import in {
		'calloc-orphan-get':   pe64_test_heap_runtime_import(.calloc,
			pe64_runtime_get_process_heap_export)
		'calloc-orphan-alloc': pe64_test_heap_runtime_import(.calloc,
			pe64_runtime_heap_alloc_export)
	} {
		orphan := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{},
			[]Pe64RuntimeBinding{}, [orphan_import])
		pe64_test_assert_calloc_publish_refusal(root, name, &object, orphan,
			'PE64 runtime import binding: calloc binding is orphaned')
	}

	ordinary_substitution := pe64_test_process_exit_definition(entry, [
		Pe64ImportBinding{
			symbol_id:   calloc_symbol
			dll:         pe64_runtime_heap_dll
			export_name: pe64_runtime_get_process_heap_export
		},
	], [calloc_helper], [
		pe64_test_heap_runtime_import(.calloc, pe64_runtime_heap_alloc_export),
	])
	pe64_test_assert_calloc_publish_refusal(root, 'calloc-ordinary-substitution', &object,
		ordinary_substitution,
		'PE64 runtime import binding: calloc GetProcessHeap binding is missing')

	mut shared_object := Object.new()
	shared_entry := shared_object.intern_function_symbol('malloc_calloc_shared_entry') or {
		panic(err)
	}
	shared_malloc := shared_object.intern_external_function_symbol('malloc_calloc_shared_malloc') or {
		panic(err)
	}
	shared_calloc := shared_object.intern_external_function_symbol('malloc_calloc_shared_calloc') or {
		panic(err)
	}
	shared_body := [u8(0x48), 0x83, 0xec, 0x28, 0xe8, 0, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0x48, 0x83,
		0xc4, 0x28, 0xeb, 0xfe]
	assert shared_object.append_text(shared_body) or { panic(err) } == 0
	shared_object.define_text_function(shared_entry, 0, u64(shared_body.len)) or { panic(err) }
	shared_object.add_text_call_relocation(5, shared_malloc) or { panic(err) }
	shared_object.add_text_call_relocation(10, shared_calloc) or { panic(err) }
	mut shared_runtime_imports := pe64_test_malloc_runtime_imports()
	shared_runtime_imports << pe64_test_calloc_runtime_imports()
	shared_definition := pe64_test_process_exit_definition(shared_entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: shared_malloc, helper: .malloc },
		Pe64RuntimeBinding{ symbol_id: shared_calloc, helper: .calloc },
	], shared_runtime_imports)
	shared_image := pe64_test_malloc_free_image_bytes(&shared_object, shared_definition) or {
		panic(err)
	}
	assert pe64_test_imports(shared_image).map(it.export_name) == ['GetProcessHeap', 'HeapAlloc']

	alias_fixture := pe64_test_heap_alias_fixture(false)
	mut aliases := [
		Pe64RuntimeBinding{
			symbol_id: alias_fixture.malloc_a
			helper:    .malloc
		},
		Pe64RuntimeBinding{
			symbol_id: alias_fixture.malloc_b
			helper:    .malloc
		},
		Pe64RuntimeBinding{
			symbol_id: alias_fixture.free_a
			helper:    .free
		},
		Pe64RuntimeBinding{
			symbol_id: alias_fixture.free_b
			helper:    .free
		},
		Pe64RuntimeBinding{
			symbol_id: alias_fixture.calloc_a
			helper:    .calloc
		},
		Pe64RuntimeBinding{
			symbol_id: alias_fixture.calloc_b
			helper:    .calloc
		},
	]
	mut family_runtime_imports := pe64_test_malloc_runtime_imports()
	family_runtime_imports << pe64_test_free_runtime_imports()
	family_runtime_imports << pe64_test_calloc_runtime_imports()
	aliases.reverse_in_place()
	family_runtime_imports.reverse_in_place()
	family_definition := pe64_test_process_exit_definition(alias_fixture.entry, [
		Pe64ImportBinding{
			symbol_id:   alias_fixture.imported
			dll:         pe64_runtime_heap_dll
			export_name: pe64_runtime_get_process_heap_export
		},
	], aliases, family_runtime_imports)
	family_image := pe64_test_malloc_free_image_bytes(&alias_fixture.object, family_definition) or {
		panic(err)
	}
	assert pe64_test_imports(family_image).map('${it.dll}/${it.export_name}') == [
		'Kernel32.dll/GetProcessHeap',
		'Kernel32.dll/HeapAlloc',
		'Kernel32.dll/HeapFree',
	]
}

fn test_pe64_runtime_malloc_free_import_ownership_dedup_and_refusal_matrix() {
	root := pe64_test_root('runtime malloc free import matrix')
	defer {
		pe64_test_cleanup(root)
	}
	malloc_object, malloc_entry, malloc_symbol := pe64_test_runtime_fixture('malloc_import_entry',
		'malloc_import_opaque')
	malloc_definition := pe64_test_process_exit_definition(malloc_entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: malloc_symbol, helper: .malloc },
	], pe64_test_malloc_runtime_imports())
	malloc_image := pe64_test_malloc_free_image_bytes(&malloc_object, malloc_definition) or {
		panic(err)
	}
	assert pe64_test_imports(malloc_image).map(it.export_name) == ['GetProcessHeap', 'HeapAlloc']
	assert 'HeapFree' !in pe64_test_imports(malloc_image).map(it.export_name)

	free_object, free_entry, free_symbol := pe64_test_runtime_fixture('free_import_entry',
		'free_import_opaque')
	free_definition := pe64_test_process_exit_definition(free_entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: free_symbol, helper: .free },
	], pe64_test_free_runtime_imports())
	free_image := pe64_test_malloc_free_image_bytes(&free_object, free_definition) or { panic(err) }
	assert pe64_test_imports(free_image).map(it.export_name) == ['GetProcessHeap', 'HeapFree']
	assert 'HeapAlloc' !in pe64_test_imports(free_image).map(it.export_name)
	assert 'ucrtbase.dll' !in pe64_test_imports(malloc_image).map(it.dll)
	assert 'msvcrt.dll' !in pe64_test_imports(malloc_image).map(it.dll)

	malloc_helper := Pe64RuntimeBinding{
		symbol_id: malloc_symbol
		helper:    .malloc
	}
	missing_get := pe64_test_process_exit_definition(malloc_entry, []Pe64ImportBinding{}, [
		malloc_helper,
	], [pe64_test_heap_runtime_import(.malloc, pe64_runtime_heap_alloc_export)])
	assert pe64_test_malloc_free_publish_error(&malloc_object, missing_get, os.join_path(root,
		'missing-get.exe')) == 'PE64 runtime import binding: malloc GetProcessHeap binding is missing'
	missing_alloc := pe64_test_process_exit_definition(malloc_entry, []Pe64ImportBinding{}, [
		malloc_helper,
	], [pe64_test_heap_runtime_import(.malloc, pe64_runtime_get_process_heap_export)])
	assert pe64_test_malloc_free_publish_error(&malloc_object, missing_alloc, os.join_path(root,
		'missing-alloc.exe')) == 'PE64 runtime import binding: malloc HeapAlloc binding is missing'
	duplicate_malloc := pe64_test_process_exit_definition(malloc_entry, []Pe64ImportBinding{}, [
		malloc_helper,
	], [
		pe64_test_heap_runtime_import(.malloc, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.malloc, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.malloc, pe64_runtime_heap_alloc_export),
	])
	assert pe64_test_malloc_free_publish_error(&malloc_object, duplicate_malloc, os.join_path(root,
		'duplicate-malloc.exe')) == 'PE64 runtime import binding: duplicate malloc GetProcessHeap binding'
	wrong_case := pe64_test_process_exit_definition(malloc_entry, []Pe64ImportBinding{}, [
		malloc_helper,
	], [
		Pe64RuntimeImportBinding{
			helper:      .malloc
			dll:         'kernel32.dll'
			export_name: pe64_runtime_get_process_heap_export
		},
		pe64_test_heap_runtime_import(.malloc, pe64_runtime_heap_alloc_export),
	])
	assert pe64_test_malloc_free_publish_error(&malloc_object, wrong_case, os.join_path(root,
		'wrong-case.exe')).contains('malloc requires exact')

	free_helper := Pe64RuntimeBinding{
		symbol_id: free_symbol
		helper:    .free
	}
	missing_free := pe64_test_process_exit_definition(free_entry, []Pe64ImportBinding{}, [
		free_helper,
	], [pe64_test_heap_runtime_import(.free, pe64_runtime_get_process_heap_export)])
	assert pe64_test_malloc_free_publish_error(&free_object, missing_free, os.join_path(root,
		'missing-free.exe')) == 'PE64 runtime import binding: free HeapFree binding is missing'
	duplicate_free := pe64_test_process_exit_definition(free_entry, []Pe64ImportBinding{}, [
		free_helper,
	], [
		pe64_test_heap_runtime_import(.free, pe64_runtime_get_process_heap_export),
		pe64_test_heap_runtime_import(.free, pe64_runtime_heap_free_export),
		pe64_test_heap_runtime_import(.free, pe64_runtime_heap_free_export),
	])
	assert pe64_test_malloc_free_publish_error(&free_object, duplicate_free, os.join_path(root,
		'duplicate-free.exe')) == 'PE64 runtime import binding: duplicate free HeapFree binding'
	orphan_malloc := pe64_test_process_exit_definition(malloc_entry, []Pe64ImportBinding{},
		[]Pe64RuntimeBinding{}, pe64_test_malloc_runtime_imports())
	assert pe64_test_malloc_free_publish_error(&malloc_object, orphan_malloc, os.join_path(root,
		'orphan-malloc.exe')) == 'PE64 runtime import binding: malloc binding is orphaned'
	orphan_free := pe64_test_process_exit_definition(free_entry, []Pe64ImportBinding{},
		[]Pe64RuntimeBinding{}, pe64_test_free_runtime_imports())
	assert pe64_test_malloc_free_publish_error(&free_object, orphan_free, os.join_path(root,
		'orphan-free.exe')) == 'PE64 runtime import binding: free binding is orphaned'
	for index, orphan_imports in [
		[pe64_test_heap_runtime_import(.malloc, pe64_runtime_get_process_heap_export)],
		[pe64_test_heap_runtime_import(.malloc, pe64_runtime_heap_alloc_export)],
	] {
		orphan := pe64_test_process_exit_definition(malloc_entry, []Pe64ImportBinding{},
			[]Pe64RuntimeBinding{}, orphan_imports)
		assert pe64_test_malloc_free_publish_error(&malloc_object, orphan, os.join_path(root,
			'orphan-malloc-${index}.exe')) == 'PE64 runtime import binding: malloc binding is orphaned'
	}
	orphan_heap_free := pe64_test_process_exit_definition(free_entry, []Pe64ImportBinding{},
		[]Pe64RuntimeBinding{}, [
		pe64_test_heap_runtime_import(.free, pe64_runtime_heap_free_export),
	])
	assert pe64_test_malloc_free_publish_error(&free_object, orphan_heap_free, os.join_path(root,
		'orphan-heap-free.exe')) == 'PE64 runtime import binding: free binding is orphaned'
	conflicting_duplicate := pe64_test_process_exit_definition(malloc_entry, []Pe64ImportBinding{}, [
		malloc_helper,
	], [
		pe64_test_heap_runtime_import(.malloc, pe64_runtime_get_process_heap_export),
		Pe64RuntimeImportBinding{
			helper:      .malloc
			dll:         'kernel32.dll'
			export_name: pe64_runtime_get_process_heap_export
		},
		pe64_test_heap_runtime_import(.malloc, pe64_runtime_heap_alloc_export),
	])
	assert pe64_test_malloc_free_publish_error(&malloc_object, conflicting_duplicate, os.join_path(root,
		'conflicting-duplicate.exe')).contains('malloc requires exact')
	wrong_export := pe64_test_process_exit_definition(malloc_entry, []Pe64ImportBinding{}, [
		malloc_helper,
	], [
		pe64_test_heap_runtime_import(.malloc, pe64_runtime_get_process_heap_export),
		Pe64RuntimeImportBinding{
			helper:      .malloc
			dll:         pe64_runtime_heap_dll
			export_name: 'heapalloc'
		},
	])
	assert pe64_test_malloc_free_publish_error(&malloc_object, wrong_export, os.join_path(root,
		'wrong-export.exe')).contains('malloc requires exact')

	substitution := pe64_test_process_exit_definition(malloc_entry, [
		Pe64ImportBinding{
			symbol_id:   malloc_symbol
			dll:         pe64_runtime_heap_dll
			export_name: pe64_runtime_get_process_heap_export
		},
	], [malloc_helper], [
		pe64_test_heap_runtime_import(.malloc, pe64_runtime_heap_alloc_export),
	])
	assert pe64_test_malloc_free_publish_error(&malloc_object, substitution, os.join_path(root,
		'ordinary-substitution.exe')) == 'PE64 runtime import binding: malloc GetProcessHeap binding is missing'
	pe64_test_assert_calloc_import_ownership_matrix(root)
}

fn pe64_test_malloc_free_fixture(entry_name string, malloc_name string, free_name string,
	calloc_name string) (Object, SymbolID, SymbolID, SymbolID, SymbolID) {
	mut object := Object.new()
	entry := object.intern_function_symbol(entry_name) or { panic(err) }
	malloc_symbol := object.intern_external_function_symbol(malloc_name) or { panic(err) }
	free_symbol := object.intern_external_function_symbol(free_name) or { panic(err) }
	calloc_symbol := object.intern_external_function_symbol(calloc_name) or { panic(err) }
	body := [u8(0x48), 0x83, 0xec, 0x28, 0xe8, 0, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0xe8, 0, 0, 0, 0,
		0x48, 0x83, 0xc4, 0x28, 0xeb, 0xfe]
	assert object.append_text(body) or { panic(err) } == 0
	object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
	object.add_text_call_relocation(5, malloc_symbol) or { panic(err) }
	object.add_text_call_relocation(10, free_symbol) or { panic(err) }
	object.add_text_call_relocation(15, calloc_symbol) or { panic(err) }
	return object, entry, malloc_symbol, free_symbol, calloc_symbol
}

fn pe64_test_malloc_free_definition(entry SymbolID, malloc_symbol SymbolID,
	free_symbol SymbolID, calloc_symbol SymbolID) Pe64ImageDefinition {
	mut runtime_imports := pe64_test_malloc_runtime_imports()
	runtime_imports << pe64_test_free_runtime_imports()
	runtime_imports << pe64_test_calloc_runtime_imports()
	return pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: malloc_symbol, helper: .malloc },
		Pe64RuntimeBinding{ symbol_id: free_symbol, helper: .free },
		Pe64RuntimeBinding{ symbol_id: calloc_symbol, helper: .calloc },
	], runtime_imports)
}

struct Pe64TestHeapPredecessorFixture {
	object       Object
	entry        SymbolID
	strlen       SymbolID
	wcslen       SymbolID
	memset       SymbolID
	memcmp       SymbolID
	memmove      SymbolID
	memcpy       SymbolID
	process_exit SymbolID
	imported     SymbolID
	malloc       SymbolID
	free         SymbolID
}

fn pe64_test_heap_predecessor_fixture(reverse_relocations bool) Pe64TestHeapPredecessorFixture {
	mut object := Object.new()
	entry := object.intern_function_symbol('heap_predecessor_entry') or { panic(err) }
	strlen_symbol := object.intern_external_function_symbol('heap_predecessor_strlen') or {
		panic(err)
	}
	wcslen_symbol := object.intern_external_function_symbol('heap_predecessor_wcslen') or {
		panic(err)
	}
	memset_symbol := object.intern_external_function_symbol('heap_predecessor_memset') or {
		panic(err)
	}
	memcmp_symbol := object.intern_external_function_symbol('heap_predecessor_memcmp') or {
		panic(err)
	}
	memmove_symbol := object.intern_external_function_symbol('heap_predecessor_memmove') or {
		panic(err)
	}
	memcpy_symbol := object.intern_external_function_symbol('heap_predecessor_memcpy') or {
		panic(err)
	}
	process_exit_symbol := object.intern_external_function_symbol('heap_predecessor_exit') or {
		panic(err)
	}
	imported := object.intern_external_function_symbol('heap_predecessor_import') or { panic(err) }
	malloc_symbol := object.intern_external_function_symbol('heap_predecessor_malloc') or {
		panic(err)
	}
	free_symbol := object.intern_external_function_symbol('heap_predecessor_free') or { panic(err) }
	mut body := [u8(0x48), 0x83, 0xec, 0x28]
	for _ in 0 .. 10 {
		body << [u8(0xe8), 0, 0, 0, 0]
	}
	body << [u8(0x48), 0x83, 0xc4, 0x28, 0xeb, 0xfe]
	assert object.append_text(body) or { panic(err) } == 0
	object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
	targets := [strlen_symbol, wcslen_symbol, memset_symbol, memcmp_symbol, memmove_symbol,
		memcpy_symbol, process_exit_symbol, imported, malloc_symbol, free_symbol]
	if reverse_relocations {
		for index := targets.len - 1; index >= 0; index-- {
			object.add_text_call_relocation(u64(5 + index * 5), targets[index]) or { panic(err) }
		}
	} else {
		for index, target in targets {
			object.add_text_call_relocation(u64(5 + index * 5), target) or { panic(err) }
		}
	}
	return Pe64TestHeapPredecessorFixture{
		object:       object
		entry:        entry
		strlen:       strlen_symbol
		wcslen:       wcslen_symbol
		memset:       memset_symbol
		memcmp:       memcmp_symbol
		memmove:      memmove_symbol
		memcpy:       memcpy_symbol
		process_exit: process_exit_symbol
		imported:     imported
		malloc:       malloc_symbol
		free:         free_symbol
	}
}

fn pe64_test_heap_predecessor_definition(fixture &Pe64TestHeapPredecessorFixture,
	with_heap_helpers bool, reverse_inputs bool) Pe64ImageDefinition {
	mut imports := [
		Pe64ImportBinding{
			symbol_id:   fixture.imported
			dll:         'zeta.dll'
			export_name: 'OrdinaryCall'
		},
	]
	if !with_heap_helpers {
		imports << Pe64ImportBinding{
			symbol_id:   fixture.malloc
			dll:         'allocator.dll'
			export_name: 'AllocateOpaque'
		}
		imports << Pe64ImportBinding{
			symbol_id:   fixture.free
			dll:         'allocator.dll'
			export_name: 'ReleaseOpaque'
		}
	}
	mut helpers := [
		Pe64RuntimeBinding{
			symbol_id: fixture.strlen
			helper:    .strlen
		},
		Pe64RuntimeBinding{
			symbol_id: fixture.wcslen
			helper:    .wcslen
		},
		Pe64RuntimeBinding{
			symbol_id: fixture.memset
			helper:    .memset
		},
		Pe64RuntimeBinding{
			symbol_id: fixture.memcmp
			helper:    .memcmp
		},
		Pe64RuntimeBinding{
			symbol_id: fixture.memmove
			helper:    .memmove
		},
		Pe64RuntimeBinding{
			symbol_id: fixture.memcpy
			helper:    .memcpy
		},
		Pe64RuntimeBinding{
			symbol_id: fixture.process_exit
			helper:    .process_exit
		},
	]
	mut runtime_imports := [pe64_test_process_exit_import()]
	if with_heap_helpers {
		helpers << Pe64RuntimeBinding{
			symbol_id: fixture.malloc
			helper:    .malloc
		}
		helpers << Pe64RuntimeBinding{
			symbol_id: fixture.free
			helper:    .free
		}
		runtime_imports << pe64_test_malloc_runtime_imports()
		runtime_imports << pe64_test_free_runtime_imports()
	}
	if reverse_inputs {
		imports.reverse_in_place()
		helpers.reverse_in_place()
		runtime_imports.reverse_in_place()
	}
	return pe64_test_process_exit_definition(fixture.entry, imports, helpers, runtime_imports)
}

fn pe64_test_mask_rel32_fields(mut bytes []u8, fields []int) {
	for field in fields {
		assert field >= 0 && field <= bytes.len - 4
		for index in field .. field + 4 {
			bytes[index] = 0
		}
	}
}

fn pe64_test_import_thunk_rva(data []u8, text Pe64TestSection, runtime_end int, dll string,
	export_name string) u64 {
	for index, item in pe64_test_imports(data) {
		if item.dll == dll && item.export_name == export_name {
			return u64(text.virtual_address) + u64(runtime_end) +
				u64(index) * pe64_import_thunk_size
		}
	}
	assert false, 'missing import `${dll}` / `${export_name}`'
	return 0
}

struct Pe64TestCallocPredecessorFixture {
	object       Object
	entry        SymbolID
	strlen       SymbolID
	wcslen       SymbolID
	memset       SymbolID
	memcmp       SymbolID
	memmove      SymbolID
	memcpy       SymbolID
	process_exit SymbolID
	imported     SymbolID
	malloc       SymbolID
	free         SymbolID
	calloc       SymbolID
}

struct Pe64TestHeapCallOracle {
	export_name  string
	field_offset int
}

fn pe64_test_calloc_predecessor_fixture(reverse_relocations bool) Pe64TestCallocPredecessorFixture {
	mut object := Object.new()
	entry := object.intern_function_symbol('calloc_predecessor_entry') or { panic(err) }
	strlen_symbol := object.intern_external_function_symbol('calloc_predecessor_strlen') or {
		panic(err)
	}
	wcslen_symbol := object.intern_external_function_symbol('calloc_predecessor_wcslen') or {
		panic(err)
	}
	memset_symbol := object.intern_external_function_symbol('calloc_predecessor_memset') or {
		panic(err)
	}
	memcmp_symbol := object.intern_external_function_symbol('calloc_predecessor_memcmp') or {
		panic(err)
	}
	memmove_symbol := object.intern_external_function_symbol('calloc_predecessor_memmove') or {
		panic(err)
	}
	memcpy_symbol := object.intern_external_function_symbol('calloc_predecessor_memcpy') or {
		panic(err)
	}
	process_exit_symbol := object.intern_external_function_symbol('calloc_predecessor_exit') or {
		panic(err)
	}
	imported := object.intern_external_function_symbol('calloc_predecessor_import') or {
		panic(err)
	}
	malloc_symbol := object.intern_external_function_symbol('calloc_predecessor_malloc') or {
		panic(err)
	}
	free_symbol := object.intern_external_function_symbol('calloc_predecessor_free') or {
		panic(err)
	}
	calloc_symbol := object.intern_external_function_symbol('calloc_predecessor_calloc') or {
		panic(err)
	}
	mut body := [u8(0x48), 0x83, 0xec, 0x28]
	for _ in 0 .. 11 {
		body << [u8(0xe8), 0, 0, 0, 0]
	}
	body << [u8(0x48), 0x83, 0xc4, 0x28, 0xeb, 0xfe]
	assert object.append_text(body) or { panic(err) } == 0
	object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
	targets := [strlen_symbol, wcslen_symbol, memset_symbol, memcmp_symbol, memmove_symbol,
		memcpy_symbol, process_exit_symbol, imported, malloc_symbol, free_symbol, calloc_symbol]
	if reverse_relocations {
		for index := targets.len - 1; index >= 0; index-- {
			object.add_text_call_relocation(u64(5 + index * 5), targets[index]) or { panic(err) }
		}
	} else {
		for index, target in targets {
			object.add_text_call_relocation(u64(5 + index * 5), target) or { panic(err) }
		}
	}
	return Pe64TestCallocPredecessorFixture{
		object:       object
		entry:        entry
		strlen:       strlen_symbol
		wcslen:       wcslen_symbol
		memset:       memset_symbol
		memcmp:       memcmp_symbol
		memmove:      memmove_symbol
		memcpy:       memcpy_symbol
		process_exit: process_exit_symbol
		imported:     imported
		malloc:       malloc_symbol
		free:         free_symbol
		calloc:       calloc_symbol
	}
}

fn pe64_test_calloc_predecessor_definition(fixture &Pe64TestCallocPredecessorFixture,
	with_calloc bool, reverse_inputs bool) Pe64ImageDefinition {
	mut imports := [
		Pe64ImportBinding{
			symbol_id:   fixture.imported
			dll:         'zeta.dll'
			export_name: 'OrdinaryCall'
		},
	]
	mut helpers := [
		Pe64RuntimeBinding{
			symbol_id: fixture.strlen
			helper:    .strlen
		},
		Pe64RuntimeBinding{
			symbol_id: fixture.wcslen
			helper:    .wcslen
		},
		Pe64RuntimeBinding{
			symbol_id: fixture.memset
			helper:    .memset
		},
		Pe64RuntimeBinding{
			symbol_id: fixture.memcmp
			helper:    .memcmp
		},
		Pe64RuntimeBinding{
			symbol_id: fixture.memmove
			helper:    .memmove
		},
		Pe64RuntimeBinding{
			symbol_id: fixture.memcpy
			helper:    .memcpy
		},
		Pe64RuntimeBinding{
			symbol_id: fixture.process_exit
			helper:    .process_exit
		},
		Pe64RuntimeBinding{
			symbol_id: fixture.malloc
			helper:    .malloc
		},
		Pe64RuntimeBinding{
			symbol_id: fixture.free
			helper:    .free
		},
	]
	mut runtime_imports := [pe64_test_process_exit_import()]
	runtime_imports << pe64_test_malloc_runtime_imports()
	runtime_imports << pe64_test_free_runtime_imports()
	if with_calloc {
		helpers << Pe64RuntimeBinding{
			symbol_id: fixture.calloc
			helper:    .calloc
		}
		runtime_imports << pe64_test_calloc_runtime_imports()
	} else {
		imports << Pe64ImportBinding{
			symbol_id:   fixture.calloc
			dll:         'allocator.dll'
			export_name: 'CallocOpaque'
		}
	}
	if reverse_inputs {
		imports.reverse_in_place()
		helpers.reverse_in_place()
		runtime_imports.reverse_in_place()
	}
	return pe64_test_process_exit_definition(fixture.entry, imports, helpers, runtime_imports)
}

fn pe64_test_linked_call_target(data []u8, text Pe64TestSection, field_offset int) u64 {
	bits := pe64_test_read_u32(data, int(text.raw_pointer) + field_offset)
	return pe64_test_rel32_target(u64(text.virtual_address) + u64(field_offset), bits)
}

fn pe64_test_assert_runtime_plan_equal(actual &Pe64RuntimePlan, expected &Pe64RuntimePlan) {
	assert actual.physical == expected.physical
	assert actual.physical_offsets == expected.physical_offsets
	assert actual.symbol_physical_index == expected.symbol_physical_index
	assert actual.process_exit_physical_index == expected.process_exit_physical_index
	assert actual.malloc_physical_index == expected.malloc_physical_index
	assert actual.free_physical_index == expected.free_physical_index
	assert actual.calloc_physical_index == expected.calloc_physical_index
	assert actual.size == expected.size
}

fn pe64_test_assert_import_plan_equal(actual &Pe64ImportPlan, expected &Pe64ImportPlan) {
	assert actual.physical == expected.physical
	assert actual.symbol_physical_index == expected.symbol_physical_index
	assert actual.process_exit_physical_index == expected.process_exit_physical_index
	assert actual.get_process_heap_physical_index == expected.get_process_heap_physical_index
	assert actual.heap_alloc_physical_index == expected.heap_alloc_physical_index
	assert actual.heap_free_physical_index == expected.heap_free_physical_index
}

fn pe64_test_assert_calloc_predecessor_calls(data []u8,
	fixture &Pe64TestCallocPredecessorFixture, plan &Pe64RuntimePlan, with_calloc bool) {
	text := pe64_test_section(data, '.text')
	runtime_end := fixture.object.text.len + int(plan.size)
	for field_and_index in [
		[5, 0],
		[10, 1],
		[15, 2],
		[20, 3],
		[25, 4],
		[30, 4],
		[35, 5],
		[45, 6],
		[50, 7],
	] {
		field := field_and_index[0]
		physical_index := field_and_index[1]
		assert pe64_test_linked_call_target(data, text, field) == u64(text.virtual_address) +
			plan.physical_offsets[physical_index]
	}
	assert pe64_test_linked_call_target(data, text, 40) == pe64_test_import_thunk_rva(data, text,
		runtime_end, 'zeta.dll', 'OrdinaryCall')
	if with_calloc {
		assert plan.calloc_physical_index == 8
		assert pe64_test_linked_call_target(data, text, 55) == u64(text.virtual_address) +
			plan.physical_offsets[plan.calloc_physical_index]
	} else {
		assert plan.calloc_physical_index == -1
		assert pe64_test_linked_call_target(data, text, 55) == pe64_test_import_thunk_rva(data,
			text, runtime_end, 'allocator.dll', 'CallocOpaque')
	}

	process_exit_offset := int(plan.physical_offsets[plan.process_exit_physical_index])
	malloc_offset := int(plan.physical_offsets[plan.malloc_physical_index])
	free_offset := int(plan.physical_offsets[plan.free_physical_index])
	for target in [
		Pe64TestHeapCallOracle{
			export_name:  pe64_runtime_process_exit_export
			field_offset: process_exit_offset + 5
		},
		Pe64TestHeapCallOracle{
			export_name:  pe64_runtime_get_process_heap_export
			field_offset: malloc_offset + 10
		},
		Pe64TestHeapCallOracle{
			export_name:  pe64_runtime_heap_alloc_export
			field_offset: malloc_offset + 36
		},
		Pe64TestHeapCallOracle{
			export_name:  pe64_runtime_get_process_heap_export
			field_offset: free_offset + 19
		},
		Pe64TestHeapCallOracle{
			export_name:  pe64_runtime_heap_free_export
			field_offset: free_offset + 39
		},
	] {
		dll := if target.export_name == pe64_runtime_process_exit_export {
			pe64_runtime_process_exit_dll
		} else {
			pe64_runtime_heap_dll
		}
		assert pe64_test_linked_call_target(data, text, target.field_offset) == pe64_test_import_thunk_rva(data,
			text, runtime_end, dll, target.export_name)
	}
}

fn pe64_test_assert_calloc_predecessor_unwind(data []u8,
	fixture &Pe64TestCallocPredecessorFixture, plan &Pe64RuntimePlan, with_calloc bool) {
	text := pe64_test_section(data, '.text')
	pdata := pe64_test_section(data, '.pdata')
	xdata := pe64_test_section(data, '.xdata')
	mut physical_indices := [plan.process_exit_physical_index, plan.malloc_physical_index,
		plan.free_physical_index]
	mut helper_sizes := [pe64_runtime_process_exit_size, pe64_runtime_malloc_size,
		pe64_runtime_free_size]
	if with_calloc {
		physical_indices << plan.calloc_physical_index
		helper_sizes << pe64_runtime_calloc_size
	}
	row_count := 1 + physical_indices.len
	assert pdata.virtual_size == u32(row_count * int(pe64_runtime_function_size))
	assert xdata.virtual_size == u32(row_count * int(pe64_unwind_info_size))
	assert pe64_test_directory(data, pe64_exception_directory_index) == Pe64TestDirectory{
		rva:  pdata.virtual_address
		size: pdata.virtual_size
	}
	pdata_raw := int(pdata.raw_pointer)
	assert pe64_test_read_u32(data, pdata_raw) == text.virtual_address
	assert pe64_test_read_u32(data, pdata_raw + 4) == text.virtual_address +
		u32(fixture.object.text.len)
	assert pe64_test_read_u32(data, pdata_raw + 8) == xdata.virtual_address
	for index, physical_index in physical_indices {
		row := pdata_raw + (index + 1) * int(pe64_runtime_function_size)
		begin := text.virtual_address + u32(plan.physical_offsets[physical_index])
		assert pe64_test_read_u32(data, row) == begin
		assert pe64_test_read_u32(data, row + 4) == begin + u32(helper_sizes[index])
		assert pe64_test_read_u32(data, row + 8) == xdata.virtual_address + u32((index +
			1) * int(pe64_unwind_info_size))
	}
	unwind := [u8(0x01), 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00]
	xdata_raw := int(xdata.raw_pointer)
	for index in 0 .. row_count {
		offset := xdata_raw + index * int(pe64_unwind_info_size)
		assert data[offset..offset + int(pe64_unwind_info_size)] == unwind
	}
}

fn pe64_test_assert_calloc_predecessor_continuity() {
	first := pe64_test_calloc_predecessor_fixture(false)
	second := pe64_test_calloc_predecessor_fixture(true)
	first_before := pe64_test_clone_object(&first.object)
	second_before := pe64_test_clone_object(&second.object)
	empty_definition := pe64_test_calloc_predecessor_definition(&first, false, false)
	reversed_empty_definition := pe64_test_calloc_predecessor_definition(&second, false, true)
	mixed_definition := pe64_test_calloc_predecessor_definition(&first, true, true)
	reversed_mixed_definition := pe64_test_calloc_predecessor_definition(&second, true, false)
	empty_definition_before := pe64_test_clone_image_definition(&empty_definition)
	reversed_empty_definition_before := pe64_test_clone_image_definition(&reversed_empty_definition)
	mixed_definition_before := pe64_test_clone_image_definition(&mixed_definition)
	reversed_mixed_definition_before := pe64_test_clone_image_definition(&reversed_mixed_definition)

	empty_plan := pe64_prepare_runtime_helpers(&first.object, empty_definition.runtime_helpers) or {
		panic(err)
	}
	reversed_empty_plan := pe64_prepare_runtime_helpers(&second.object,
		reversed_empty_definition.runtime_helpers) or { panic(err) }
	mixed_plan := pe64_prepare_runtime_helpers(&first.object, mixed_definition.runtime_helpers) or {
		panic(err)
	}
	reversed_mixed_plan := pe64_prepare_runtime_helpers(&second.object,
		reversed_mixed_definition.runtime_helpers) or { panic(err) }
	predecessor_helpers := [Pe64RuntimeHelperKind.strlen, .wcslen, .memset, .memcmp, .memmove,
		.process_exit, .malloc, .free]
	mut all_helpers := predecessor_helpers.clone()
	all_helpers << Pe64RuntimeHelperKind.calloc
	assert empty_plan.physical == predecessor_helpers
	assert mixed_plan.physical == all_helpers
	pe64_test_assert_runtime_plan_equal(&reversed_empty_plan, &empty_plan)
	pe64_test_assert_runtime_plan_equal(&reversed_mixed_plan, &mixed_plan)
	assert mixed_plan.physical_offsets[..empty_plan.physical_offsets.len] == empty_plan.physical_offsets
	assert empty_plan.size == pe64_runtime_strlen_size + pe64_runtime_wcslen_size +
		pe64_runtime_memset_size + pe64_runtime_memcmp_size + pe64_runtime_move_size +
		pe64_runtime_process_exit_size + pe64_runtime_malloc_size + pe64_runtime_free_size
	assert mixed_plan.size == empty_plan.size + pe64_runtime_calloc_size
	assert mixed_plan.physical_offsets[mixed_plan.calloc_physical_index] ==
		u64(first.object.text.len) + empty_plan.size
	mut expected_offset := u64(first.object.text.len)
	for index, helper in all_helpers {
		assert mixed_plan.physical_offsets[index] == expected_offset
		if index < predecessor_helpers.len {
			assert empty_plan.physical_offsets[index] == expected_offset
		}
		helper_size := pe64_runtime_helper_size(helper) or { panic(err) }
		expected_offset += helper_size
		if index + 1 < all_helpers.len {
			assert mixed_plan.physical_offsets[index + 1] == expected_offset
		}
	}
	assert expected_offset == u64(first.object.text.len) + mixed_plan.size
	for symbol_and_index in [
		[int(first.strlen), 0],
		[int(first.wcslen), 1],
		[int(first.memset), 2],
		[int(first.memcmp), 3],
		[int(first.memmove), 4],
		[int(first.memcpy), 4],
		[int(first.process_exit), 5],
		[int(first.malloc), 6],
		[int(first.free), 7],
	] {
		assert empty_plan.symbol_physical_index[symbol_and_index[0]] == symbol_and_index[1]
		assert mixed_plan.symbol_physical_index[symbol_and_index[0]] == symbol_and_index[1]
	}
	assert empty_plan.symbol_physical_index[int(first.calloc)] == -1
	assert mixed_plan.symbol_physical_index[int(first.calloc)] == 8

	empty_import_plan := pe64_prepare_imports(&first.object, empty_definition.imports,
		empty_definition.runtime_imports, &empty_plan) or { panic(err) }
	reversed_empty_import_plan := pe64_prepare_imports(&second.object,
		reversed_empty_definition.imports, reversed_empty_definition.runtime_imports,
		&reversed_empty_plan) or { panic(err) }
	mixed_import_plan := pe64_prepare_imports(&first.object, mixed_definition.imports,
		mixed_definition.runtime_imports, &mixed_plan) or { panic(err) }
	reversed_mixed_import_plan := pe64_prepare_imports(&second.object,
		reversed_mixed_definition.imports, reversed_mixed_definition.runtime_imports,
		&reversed_mixed_plan) or { panic(err) }
	pe64_test_assert_import_plan_equal(&reversed_empty_import_plan, &empty_import_plan)
	pe64_test_assert_import_plan_equal(&reversed_mixed_import_plan, &mixed_import_plan)
	assert empty_import_plan.physical.map('${it.dll}/${it.export_name}') == [
		'Kernel32.dll/ExitProcess',
		'Kernel32.dll/GetProcessHeap',
		'Kernel32.dll/HeapAlloc',
		'Kernel32.dll/HeapFree',
		'allocator.dll/CallocOpaque',
		'zeta.dll/OrdinaryCall',
	]
	assert mixed_import_plan.physical.map('${it.dll}/${it.export_name}') == [
		'Kernel32.dll/ExitProcess',
		'Kernel32.dll/GetProcessHeap',
		'Kernel32.dll/HeapAlloc',
		'Kernel32.dll/HeapFree',
		'zeta.dll/OrdinaryCall',
	]
	assert empty_import_plan.symbol_physical_index[int(first.calloc)] == 4
	assert empty_import_plan.symbol_physical_index[int(first.imported)] == 5
	assert mixed_import_plan.symbol_physical_index[int(first.calloc)] == -1
	assert mixed_import_plan.symbol_physical_index[int(first.imported)] == 4
	assert empty_import_plan.process_exit_physical_index == 0
	assert empty_import_plan.get_process_heap_physical_index == 1
	assert empty_import_plan.heap_alloc_physical_index == 2
	assert empty_import_plan.heap_free_physical_index == 3
	assert mixed_import_plan.process_exit_physical_index == 0
	assert mixed_import_plan.get_process_heap_physical_index == 1
	assert mixed_import_plan.heap_alloc_physical_index == 2
	assert mixed_import_plan.heap_free_physical_index == 3

	empty_image := pe64_test_malloc_free_image_bytes(&first.object, empty_definition) or {
		panic(err)
	}
	reversed_empty_image := pe64_test_malloc_free_image_bytes(&second.object,
		reversed_empty_definition) or { panic(err) }
	assert empty_image == reversed_empty_image
	assert sha256.sum(empty_image).hex() == '3c1b6171dd66fdc826a7305a808cf4d809a6d7cf518009d68770280d42af39ba'
	mixed_image := pe64_test_malloc_free_image_bytes(&first.object, mixed_definition) or {
		panic(err)
	}
	reversed_mixed_image := pe64_test_malloc_free_image_bytes(&second.object,
		reversed_mixed_definition) or { panic(err) }
	assert mixed_image == reversed_mixed_image
	assert pe64_test_imports(empty_image).map('${it.dll}/${it.export_name}') == empty_import_plan.physical.map('${it.dll}/${it.export_name}')
	assert pe64_test_imports(reversed_empty_image) == pe64_test_imports(empty_image)
	assert pe64_test_imports(mixed_image).map('${it.dll}/${it.export_name}') == mixed_import_plan.physical.map('${it.dll}/${it.export_name}')
	assert pe64_test_imports(reversed_mixed_image) == pe64_test_imports(mixed_image)

	empty_text := pe64_test_section(empty_image, '.text')
	reversed_empty_text := pe64_test_section(reversed_empty_image, '.text')
	mixed_text := pe64_test_section(mixed_image, '.text')
	reversed_mixed_text := pe64_test_section(reversed_mixed_image, '.text')
	assert empty_text.virtual_address == mixed_text.virtual_address
	predecessor_end := first.object.text.len + int(empty_plan.size)
	mut empty_prefix := pe64_test_runtime_body(empty_image, empty_text, 0, predecessor_end)
	mut reversed_empty_prefix := pe64_test_runtime_body(reversed_empty_image, reversed_empty_text,
		0, predecessor_end)
	mut mixed_prefix := pe64_test_runtime_body(mixed_image, mixed_text, 0, predecessor_end)
	mut reversed_mixed_prefix := pe64_test_runtime_body(reversed_mixed_image, reversed_mixed_text,
		0, predecessor_end)
	process_exit_offset := int(empty_plan.physical_offsets[empty_plan.process_exit_physical_index])
	malloc_offset := int(empty_plan.physical_offsets[empty_plan.malloc_physical_index])
	free_offset := int(empty_plan.physical_offsets[empty_plan.free_physical_index])
	mut recorded_fields := []int{}
	for index in 0 .. 11 {
		recorded_fields << 5 + index * 5
	}
	recorded_fields << [
		process_exit_offset + 5,
		malloc_offset + 10,
		malloc_offset + 36,
		free_offset + 19,
		free_offset + 39,
	]
	pe64_test_mask_rel32_fields(mut empty_prefix, recorded_fields)
	pe64_test_mask_rel32_fields(mut reversed_empty_prefix, recorded_fields)
	pe64_test_mask_rel32_fields(mut mixed_prefix, recorded_fields)
	pe64_test_mask_rel32_fields(mut reversed_mixed_prefix, recorded_fields)
	assert empty_prefix == mixed_prefix
	assert reversed_empty_prefix == empty_prefix
	assert reversed_mixed_prefix == mixed_prefix

	pe64_test_assert_calloc_predecessor_calls(empty_image, &first, &empty_plan, false)
	pe64_test_assert_calloc_predecessor_calls(reversed_empty_image, &second, &reversed_empty_plan,
		false)
	pe64_test_assert_calloc_predecessor_calls(mixed_image, &first, &mixed_plan, true)
	pe64_test_assert_calloc_predecessor_calls(reversed_mixed_image, &second, &reversed_mixed_plan,
		true)
	for field in [5, 10, 15, 20, 25, 30, 35, 45, 50] {
		assert pe64_test_read_u32(empty_image, int(empty_text.raw_pointer) + field) == pe64_test_read_u32(mixed_image,

			int(mixed_text.raw_pointer) + field)
		assert pe64_test_read_u32(reversed_empty_image,
			int(reversed_empty_text.raw_pointer) + field) == pe64_test_read_u32(reversed_mixed_image,

			int(reversed_mixed_text.raw_pointer) + field)
	}
	for field in [
		40,
		55,
		process_exit_offset + 5,
		malloc_offset + 10,
		malloc_offset + 36,
		free_offset + 19,
		free_offset + 39,
	] {
		assert pe64_test_read_u32(empty_image, int(empty_text.raw_pointer) + field) != pe64_test_read_u32(mixed_image,

			int(mixed_text.raw_pointer) + field)
		assert pe64_test_read_u32(reversed_empty_image,
			int(reversed_empty_text.raw_pointer) + field) != pe64_test_read_u32(reversed_mixed_image,

			int(reversed_mixed_text.raw_pointer) + field)
	}

	empty_free := pe64_test_runtime_body(empty_image, empty_text, free_offset,
		int(pe64_runtime_free_size))
	mixed_free := pe64_test_runtime_body(mixed_image, mixed_text, free_offset,
		int(pe64_runtime_free_size))
	calloc_body := pe64_test_runtime_body(mixed_image, mixed_text,
		int(mixed_plan.physical_offsets[mixed_plan.calloc_physical_index]),
		int(pe64_runtime_calloc_size))
	assert empty_free[9..13] == [u8(0x48), 0x8b, 0x41, 0xf8]
	assert mixed_free[9..13] == empty_free[9..13]
	assert calloc_body[70..74] == [u8(0x49), 0x89, 0x43, 0xf8]

	pe64_test_assert_calloc_predecessor_unwind(empty_image, &first, &empty_plan, false)
	pe64_test_assert_calloc_predecessor_unwind(reversed_empty_image, &second, &reversed_empty_plan,
		false)
	pe64_test_assert_calloc_predecessor_unwind(mixed_image, &first, &mixed_plan, true)
	pe64_test_assert_calloc_predecessor_unwind(reversed_mixed_image, &second, &reversed_mixed_plan,
		true)
	empty_pdata := pe64_test_section(empty_image, '.pdata')
	mixed_pdata := pe64_test_section(mixed_image, '.pdata')
	empty_xdata := pe64_test_section(empty_image, '.xdata')
	mixed_xdata := pe64_test_section(mixed_image, '.xdata')
	assert empty_image[int(empty_pdata.raw_pointer)..int(empty_pdata.raw_pointer +
		empty_pdata.virtual_size)] == mixed_image[int(mixed_pdata.raw_pointer)..int(
		mixed_pdata.raw_pointer + empty_pdata.virtual_size)]
	assert empty_image[int(empty_xdata.raw_pointer)..int(empty_xdata.raw_pointer +
		empty_xdata.virtual_size)] == mixed_image[int(mixed_xdata.raw_pointer)..int(
		mixed_xdata.raw_pointer + empty_xdata.virtual_size)]
	mixed_pdata_raw := int(mixed_pdata.raw_pointer)
	calloc_row := mixed_pdata_raw + 4 * int(pe64_runtime_function_size)
	calloc_rva := u64(mixed_text.virtual_address) +
		mixed_plan.physical_offsets[mixed_plan.calloc_physical_index]
	assert pe64_test_read_u32(mixed_image, calloc_row) == u32(calloc_rva)
	assert pe64_test_read_u32(mixed_image, calloc_row + 4) == u32(calloc_rva +
		pe64_runtime_calloc_size)
	assert pe64_test_read_u32(mixed_image, calloc_row + 8) == mixed_xdata.virtual_address +
		u32(4 * int(pe64_unwind_info_size))

	pe64_test_assert_object_snapshot(&first.object, &first_before)
	pe64_test_assert_object_snapshot(&second.object, &second_before)
	pe64_test_assert_image_definition_snapshot(&empty_definition, &empty_definition_before)
	pe64_test_assert_image_definition_snapshot(&reversed_empty_definition,
		&reversed_empty_definition_before)
	pe64_test_assert_image_definition_snapshot(&mixed_definition, &mixed_definition_before)
	pe64_test_assert_image_definition_snapshot(&reversed_mixed_definition,
		&reversed_mixed_definition_before)
}

fn test_pe64_runtime_malloc_free_preserves_process_exit_and_layout_order() {
	first := pe64_test_heap_predecessor_fixture(false)
	second := pe64_test_heap_predecessor_fixture(true)
	first_before := pe64_test_clone_object(&first.object)
	second_before := pe64_test_clone_object(&second.object)
	empty_definition := pe64_test_heap_predecessor_definition(&first, false, false)
	reversed_empty_definition := pe64_test_heap_predecessor_definition(&second, false, true)
	mixed_definition := pe64_test_heap_predecessor_definition(&first, true, true)
	reversed_mixed_definition := pe64_test_heap_predecessor_definition(&second, true, false)
	empty_definition_before := pe64_test_clone_image_definition(&empty_definition)
	reversed_empty_definition_before := pe64_test_clone_image_definition(&reversed_empty_definition)
	mixed_definition_before := pe64_test_clone_image_definition(&mixed_definition)
	reversed_mixed_definition_before := pe64_test_clone_image_definition(&reversed_mixed_definition)

	empty_plan := pe64_prepare_runtime_helpers(&first.object, empty_definition.runtime_helpers) or {
		panic(err)
	}
	mixed_plan := pe64_prepare_runtime_helpers(&first.object, mixed_definition.runtime_helpers) or {
		panic(err)
	}
	predecessor_helpers := [Pe64RuntimeHelperKind.strlen, .wcslen, .memset, .memcmp, .memmove,
		.process_exit]
	mut all_helpers := predecessor_helpers.clone()
	all_helpers << [Pe64RuntimeHelperKind.malloc, .free]
	assert empty_plan.physical == predecessor_helpers
	assert mixed_plan.physical == all_helpers
	assert mixed_plan.physical_offsets[..empty_plan.physical_offsets.len] == empty_plan.physical_offsets
	assert empty_plan.size == pe64_runtime_strlen_size + pe64_runtime_wcslen_size +
		pe64_runtime_memset_size + pe64_runtime_memcmp_size + pe64_runtime_move_size +
		pe64_runtime_process_exit_size
	assert mixed_plan.size == empty_plan.size + pe64_runtime_malloc_size + pe64_runtime_free_size

	empty_image := pe64_test_malloc_free_image_bytes(&first.object, empty_definition) or {
		panic(err)
	}
	reversed_empty_image := pe64_test_malloc_free_image_bytes(&second.object,
		reversed_empty_definition) or { panic(err) }
	assert empty_image == reversed_empty_image
	assert sha256.sum(empty_image).hex() == '48b5af13f6fb1c8d1fd73fb0adb38773b154e8859cdb0a852b81184724a08256'

	mixed_image := pe64_test_malloc_free_image_bytes(&first.object, mixed_definition) or {
		panic(err)
	}
	reversed_mixed_image := pe64_test_malloc_free_image_bytes(&second.object,
		reversed_mixed_definition) or { panic(err) }
	assert mixed_image == reversed_mixed_image

	empty_text := pe64_test_section(empty_image, '.text')
	mixed_text := pe64_test_section(mixed_image, '.text')
	assert empty_text.virtual_address == mixed_text.virtual_address
	predecessor_end := first.object.text.len + int(empty_plan.size)
	mut empty_prefix := pe64_test_runtime_body(empty_image, empty_text, 0, predecessor_end)
	mut mixed_prefix := pe64_test_runtime_body(mixed_image, mixed_text, 0, predecessor_end)
	mut predecessor_fields := []int{}
	for index in 0 .. 10 {
		predecessor_fields << 5 + index * 5
	}
	process_exit_offset := int(empty_plan.physical_offsets[empty_plan.process_exit_physical_index])
	process_exit_field := process_exit_offset + 5
	predecessor_fields << process_exit_field
	pe64_test_mask_rel32_fields(mut empty_prefix, predecessor_fields)
	pe64_test_mask_rel32_fields(mut mixed_prefix, predecessor_fields)
	assert empty_prefix == mixed_prefix

	predecessor_physical_indices := [0, 1, 2, 3, 4, 4, 5]
	for index, physical_index in predecessor_physical_indices {
		field := u64(5 + index * 5)
		empty_bits := pe64_test_read_u32(empty_image, int(empty_text.raw_pointer) + int(field))
		mixed_bits := pe64_test_read_u32(mixed_image, int(mixed_text.raw_pointer) + int(field))
		assert empty_bits == mixed_bits
		assert pe64_test_rel32_target(u64(empty_text.virtual_address) + field, empty_bits) ==
			u64(empty_text.virtual_address) + empty_plan.physical_offsets[physical_index]
	}
	empty_stable_thunk := pe64_test_import_thunk_rva(empty_image, empty_text, predecessor_end,
		'zeta.dll', 'OrdinaryCall')
	mixed_runtime_end := first.object.text.len + int(mixed_plan.size)
	mixed_stable_thunk := pe64_test_import_thunk_rva(mixed_image, mixed_text, mixed_runtime_end,
		'zeta.dll', 'OrdinaryCall')
	empty_stable_bits := pe64_test_read_u32(empty_image, int(empty_text.raw_pointer) + 40)
	mixed_stable_bits := pe64_test_read_u32(mixed_image, int(mixed_text.raw_pointer) + 40)
	assert pe64_test_rel32_target(u64(empty_text.virtual_address) + 40, empty_stable_bits) == empty_stable_thunk
	assert pe64_test_rel32_target(u64(mixed_text.virtual_address) + 40, mixed_stable_bits) == mixed_stable_thunk
	assert empty_stable_bits != mixed_stable_bits

	empty_exit_thunk := pe64_test_import_thunk_rva(empty_image, empty_text, predecessor_end,
		pe64_runtime_process_exit_dll, pe64_runtime_process_exit_export)
	mixed_exit_thunk := pe64_test_import_thunk_rva(mixed_image, mixed_text, mixed_runtime_end,
		pe64_runtime_process_exit_dll, pe64_runtime_process_exit_export)
	empty_exit_bits := pe64_test_read_u32(empty_image, int(empty_text.raw_pointer) +
		process_exit_field)
	mixed_exit_bits := pe64_test_read_u32(mixed_image, int(mixed_text.raw_pointer) +
		process_exit_field)
	process_exit_rva := u64(empty_text.virtual_address) + u64(process_exit_offset)
	assert pe64_test_rel32_target(process_exit_rva + 5, empty_exit_bits) == empty_exit_thunk
	assert pe64_test_rel32_target(process_exit_rva + 5, mixed_exit_bits) == mixed_exit_thunk
	assert empty_exit_bits != mixed_exit_bits

	malloc_rva := u64(mixed_text.virtual_address) +
		mixed_plan.physical_offsets[mixed_plan.malloc_physical_index]
	free_rva := u64(mixed_text.virtual_address) +
		mixed_plan.physical_offsets[mixed_plan.free_physical_index]
	for field_and_target in [[u64(45), malloc_rva], [u64(50), free_rva]] {
		field := field_and_target[0]
		bits := pe64_test_read_u32(mixed_image, int(mixed_text.raw_pointer) + int(field))
		assert pe64_test_rel32_target(u64(mixed_text.virtual_address) + field, bits) == field_and_target[1]
	}

	empty_pdata := pe64_test_section(empty_image, '.pdata')
	mixed_pdata := pe64_test_section(mixed_image, '.pdata')
	empty_xdata := pe64_test_section(empty_image, '.xdata')
	mixed_xdata := pe64_test_section(mixed_image, '.xdata')
	assert empty_pdata.virtual_size == u32(pe64_runtime_function_size * 2)
	assert mixed_pdata.virtual_size == u32(pe64_runtime_function_size * 4)
	assert empty_xdata.virtual_size == u32(pe64_unwind_info_size * 2)
	assert mixed_xdata.virtual_size == u32(pe64_unwind_info_size * 4)
	assert empty_image[int(empty_pdata.raw_pointer)..int(empty_pdata.raw_pointer +
		empty_pdata.virtual_size)] == mixed_image[int(mixed_pdata.raw_pointer)..int(
		mixed_pdata.raw_pointer + empty_pdata.virtual_size)]
	assert empty_image[int(empty_xdata.raw_pointer)..int(empty_xdata.raw_pointer +
		empty_xdata.virtual_size)] == mixed_image[int(mixed_xdata.raw_pointer)..int(
		mixed_xdata.raw_pointer + empty_xdata.virtual_size)]
	assert pe64_test_imports(mixed_image).map('${it.dll}/${it.export_name}') == [
		'Kernel32.dll/ExitProcess',
		'Kernel32.dll/GetProcessHeap',
		'Kernel32.dll/HeapAlloc',
		'Kernel32.dll/HeapFree',
		'zeta.dll/OrdinaryCall',
	]

	pe64_test_assert_object_snapshot(&first.object, &first_before)
	pe64_test_assert_object_snapshot(&second.object, &second_before)
	pe64_test_assert_image_definition_snapshot(&empty_definition, &empty_definition_before)
	pe64_test_assert_image_definition_snapshot(&reversed_empty_definition,
		&reversed_empty_definition_before)
	pe64_test_assert_image_definition_snapshot(&mixed_definition, &mixed_definition_before)
	pe64_test_assert_image_definition_snapshot(&reversed_mixed_definition,
		&reversed_mixed_definition_before)
	pe64_test_assert_calloc_predecessor_continuity()
}

fn pe64_test_model_xor_eax_eax(initial_rax u64, instruction []u8) u64 {
	assert instruction == [u8(0x31), 0xc0]
	assert initial_rax >> 32 != 0
	mode, source, destination := pe64_test_modrm_fields(instruction[1])
	assert mode == 3
	assert source == 0
	assert destination == 0
	eax := u32(initial_rax)
	result := eax ^ eax
	without_architectural_zero_extension := (initial_rax & u64(0xffff_ffff_0000_0000)) | u64(result)
	assert without_architectural_zero_extension != 0
	return u64(result)
}

fn pe64_test_model_mov_edx_imm32(initial_rdx u64, instruction []u8) u64 {
	assert instruction == [u8(0xba), 0x08, 0, 0, 0]
	assert initial_rdx >> 32 != 0
	immediate := u32(instruction[1]) | (u32(instruction[2]) << 8) | (u32(instruction[3]) << 16) | (u32(instruction[4]) << 24)
	return u64(immediate)
}

fn test_pe64_runtime_malloc_free_full_width_boundaries_and_modeled_paths() {
	malloc_template := pe64_test_runtime_malloc_template()
	free_template := pe64_test_runtime_free_template()
	calloc_template := pe64_test_runtime_calloc_template()
	assert malloc_template.len == 75
	assert free_template.len == 48
	assert calloc_template.len == 89
	assert malloc_template[4..9] == [u8(0x48), 0x89, 0x4c, 0x24, 0x20]
	assert malloc_template[24..29] == [u8(0x4c), 0x8b, 0x44, 0x24, 0x20]
	assert malloc_template[29..33] == [u8(0x49), 0x83, 0xc0, 0x18]
	assert malloc_template[33..35] == [u8(0x72), 0x21]
	assert malloc_template[22..24] == [u8(0x31), 0xd2]
	assert free_template[31..33] == [u8(0x31), 0xd2]
	assert [u8(0xba), 0x08, 0, 0, 0] != malloc_template[22..27]
	assert malloc_template[68..70] == [u8(0x31), 0xc0]
	dirty_rax := u64(0xffff_ffff_dead_beef)
	assert dirty_rax >> 32 == 0xffff_ffff
	cleared_rax := pe64_test_model_xor_eax_eax(dirty_rax, malloc_template[68..70])
	assert cleared_rax == 0
	assert cleared_rax >> 32 == 0
	assert calloc_template[4..10] == [u8(0x48), 0x89, 0xc8, 0x48, 0xf7, 0xe2]
	assert calloc_template[10..15] == [u8(0x48), 0x85, 0xd2, 0x75, 0x43]
	assert calloc_template[15..20] == [u8(0x48), 0x89, 0x44, 0x24, 0x20]
	assert calloc_template[33..38] == [u8(0xba), 0x08, 0, 0, 0]
	assert calloc_template[38..47] == [u8(0x4c), 0x8b, 0x44, 0x24, 0x20, 0x49, 0x83, 0xc0, 0x18]
	assert calloc_template[47..49] == [u8(0x72), 0x21]
	assert calloc_template[82..84] == [u8(0x31), 0xc0]
	dirty_rdx := u64(0xffff_ffff_dead_beef)
	zero_extended_flags := pe64_test_model_mov_edx_imm32(dirty_rdx, calloc_template[33..38])
	assert zero_extended_flags == 8
	assert zero_extended_flags >> 32 == 0
	calloc_cleared_rax := pe64_test_model_xor_eax_eax(dirty_rax, calloc_template[82..84])
	assert calloc_cleared_rax == 0
	assert calloc_cleared_rax >> 32 == 0
	assert pe64_test_rel8_target(15, calloc_template[14]) == 82
	assert pe64_test_rel8_target(30, calloc_template[29]) == 82
	assert pe64_test_rel8_target(49, calloc_template[48]) == 82
	assert pe64_test_rel8_target(59, calloc_template[58]) == 82

	no_heap := pe64_test_model_malloc(99, 0, 0x1000)
	assert no_heap.result == 0
	assert no_heap.get_process_heap_calls == 1
	assert no_heap.heap_alloc_calls == 0
	zero := pe64_test_model_malloc(0, 0x1111, 0x2000)
	assert zero.allocation_size == 24
	assert zero.heap_alloc_calls == 1
	assert zero.result == 0x2010
	alloc_failure := pe64_test_model_malloc(17, 0x1111, 0)
	assert alloc_failure.allocation_size == 41
	assert alloc_failure.heap_alloc_calls == 1
	assert alloc_failure.result == 0
	maximum := pe64_test_model_malloc(max_u64 - 24, 0x1111, 0)
	assert maximum.allocation_size == max_u64
	assert maximum.heap_alloc_calls == 1
	assert maximum.result == 0
	for rejected in [max_u64 - 23, max_u64 - 1, max_u64] {
		outcome := pe64_test_model_malloc(rejected, 0x1111, 0x2000)
		assert outcome.result == 0
		assert outcome.get_process_heap_calls == 1
		assert outcome.heap_alloc_calls == 0
		assert outcome.allocation_size == 0
	}
	for residue in 0 .. 16 {
		raw := u64(0x4000 + residue)
		outcome := pe64_test_model_malloc(31, 0x1111, raw)
		assert outcome.result & 15 == 0
		assert outcome.result >= raw
		assert outcome.result - raw >= 8
		assert outcome.result - raw <= 23
		assert outcome.cookie_address == outcome.result - 8
		assert outcome.cookie_value == raw
	}

	multiplication_overflow := pe64_test_model_calloc(max_u64, 2, 0x1111, 0x2000)
	assert multiplication_overflow.multiplication_overflow
	assert !multiplication_overflow.addition_overflow
	assert multiplication_overflow.get_process_heap_calls == 0
	assert multiplication_overflow.heap_alloc_calls == 0
	assert multiplication_overflow.result == 0
	full_width := pe64_test_model_calloc(1, max_u64 - 24, 0x1111, 0)
	assert !full_width.multiplication_overflow
	assert !full_width.addition_overflow
	assert full_width.product == max_u64 - 24
	assert full_width.allocation_size == max_u64
	assert full_width.get_process_heap_calls == 1
	assert full_width.heap_alloc_calls == 1
	addition_overflow := pe64_test_model_calloc(1, max_u64 - 23, 0x1111, 0x2000)
	assert !addition_overflow.multiplication_overflow
	assert addition_overflow.addition_overflow
	assert addition_overflow.product == max_u64 - 23
	assert addition_overflow.get_process_heap_calls == 1
	assert addition_overflow.heap_alloc_calls == 0
	assert addition_overflow.result == 0
	zero_factor_cases := [[u64(0), u64(8)], [u64(8), u64(0)],
		[u64(0), u64(0)]]
	for factors in zero_factor_cases {
		outcome := pe64_test_model_calloc(factors[0], factors[1], 0x1111, 0x3000)
		assert outcome.product == 0
		assert outcome.allocation_size == 24
		assert outcome.get_process_heap_calls == 1
		assert outcome.heap_alloc_calls == 1
		assert outcome.heap_alloc_flags == 8
		assert outcome.result == 0x3010
		assert outcome.result & 15 == 0
	}
	calloc_no_heap := pe64_test_model_calloc(4, 8, 0, 0x3000)
	assert calloc_no_heap.product == 32
	assert calloc_no_heap.get_process_heap_calls == 1
	assert calloc_no_heap.heap_alloc_calls == 0
	assert calloc_no_heap.result == 0
	calloc_failure := pe64_test_model_calloc(4, 8, 0x1111, 0)
	assert calloc_failure.product == 32
	assert calloc_failure.allocation_size == 56
	assert calloc_failure.heap_alloc_calls == 1
	assert calloc_failure.heap_alloc_flags == 8
	assert calloc_failure.result == 0
	calloc_success := pe64_test_model_calloc(4, 8, 0x1111, 0x4001)
	assert calloc_success.product == 32
	assert calloc_success.allocation_size == 56
	assert calloc_success.heap_alloc_calls == 1
	assert calloc_success.heap_alloc_flags == 8
	assert calloc_success.result == 0x4010
	assert calloc_success.result & 15 == 0
	assert calloc_success.result - calloc_success.cookie_value >= 8
	assert calloc_success.result - calloc_success.cookie_value <= 23
	assert calloc_success.cookie_address + 8 == calloc_success.result
	assert calloc_success.payload_zeroed

	null_free := pe64_test_model_free(0, 0x4000, 0x1111)
	assert !null_free.cookie_read
	assert null_free.get_process_heap_calls == 0
	assert null_free.heap_free_calls == 0
	no_heap_free := pe64_test_model_free(0x4010, 0x4000, 0)
	assert no_heap_free.cookie_read
	assert no_heap_free.get_process_heap_calls == 1
	assert no_heap_free.heap_free_calls == 0
	successful_free := pe64_test_model_free(0x4010, 0x4000, 0x1111)
	assert successful_free.cookie_read
	assert successful_free.heap_free_calls == 1
	assert successful_free.heap_free_heap == 0x1111
	assert successful_free.heap_free_flags == 0
	assert successful_free.heap_free_pointer == 0x4000
}

struct Pe64TestHeapRefusalCase {
	name       string
	definition Pe64ImageDefinition
	expected   string
}

fn test_pe64_runtime_malloc_free_validation_order_transactionality_and_no_name_inference() {
	root := pe64_test_root('runtime malloc free validation')
	defer {
		pe64_test_cleanup(root)
	}
	object, entry, external := pe64_test_runtime_fixture('heap_validation_entry',
		'heap_validation_external')
	object_before := pe64_test_clone_object(&object)
	malloc_helper := Pe64RuntimeBinding{
		symbol_id: external
		helper:    .malloc
	}
	calloc_helper := Pe64RuntimeBinding{
		symbol_id: external
		helper:    .calloc
	}
	valid_runtime_imports := pe64_test_malloc_runtime_imports()
	valid_calloc_runtime_imports := pe64_test_calloc_runtime_imports()
	mut runtime_imports_with_unsupported := valid_runtime_imports.clone()
	runtime_imports_with_unsupported << Pe64RuntimeImportBinding{
		helper:      .strlen
		dll:         pe64_runtime_heap_dll
		export_name: pe64_runtime_get_process_heap_export
	}
	ordinary_owner := Pe64ImportBinding{
		symbol_id:   external
		dll:         'ordinary.dll'
		export_name: 'OrdinaryOwner'
	}
	cases := [
		Pe64TestHeapRefusalCase{
			name:       'abi-before-ordinary'
			definition: Pe64ImageDefinition{
				target_abi:      .unknown
				subsystem:       .windows_cui
				image_policy:    .fixed_base
				entry:           Pe64EntryDefinition{
					function_index: u32(entry)
					policy:         .raw_noreturn_process_entry
				}
				imports:         [
					Pe64ImportBinding{
						symbol_id:   external
						dll:         ''
						export_name: 'invalid'
					},
				]
				runtime_helpers: [
					Pe64RuntimeBinding{
						symbol_id: external
						helper:    .unknown
					},
				]
				runtime_imports: [
					Pe64RuntimeImportBinding{
						helper:      .unknown
						dll:         ''
						export_name: ''
					},
				]
			}
			expected:   'PE64 requires Microsoft x64 ABI'
		},
		Pe64TestHeapRefusalCase{
			name:       'ordinary-before-helper'
			definition: pe64_test_process_exit_definition(entry, [
				Pe64ImportBinding{
					symbol_id:   external
					dll:         ''
					export_name: 'invalid'
				},
			], [
				Pe64RuntimeBinding{ symbol_id: external, helper: .unknown },
			], [
				Pe64RuntimeImportBinding{
					helper:      .unknown
					dll:         ''
					export_name: ''
				},
			])
			expected:   'PE64 import binding: DLL name must not be empty'
		},
		Pe64TestHeapRefusalCase{
			name:       'abi-before-calloc-runtime-import'
			definition: Pe64ImageDefinition{
				target_abi:      .unknown
				subsystem:       .windows_cui
				image_policy:    .fixed_base
				entry:           Pe64EntryDefinition{
					function_index: u32(entry)
					policy:         .raw_noreturn_process_entry
				}
				runtime_helpers: [
					calloc_helper,
				]
				runtime_imports: [
					Pe64RuntimeImportBinding{
						helper:      .calloc
						dll:         'kernel32.dll'
						export_name: pe64_runtime_heap_alloc_export
					},
				]
			}
			expected:   'PE64 requires Microsoft x64 ABI'
		},
		Pe64TestHeapRefusalCase{
			name:       'helper-before-runtime-import'
			definition: pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
				Pe64RuntimeBinding{ symbol_id: external, helper: .unknown },
			], [
				Pe64RuntimeImportBinding{
					helper:      .unknown
					dll:         ''
					export_name: ''
				},
			])
			expected:   'PE64 runtime binding: helper 0 is unsupported'
		},
		Pe64TestHeapRefusalCase{
			name:       'runtime-import-before-ownership'
			definition: pe64_test_process_exit_definition(entry, [
				ordinary_owner,
			], [
				malloc_helper,
			], [
				Pe64RuntimeImportBinding{
					helper:      .malloc
					dll:         'kernel32.dll'
					export_name: pe64_runtime_get_process_heap_export
				},
				pe64_test_heap_runtime_import(.malloc, pe64_runtime_heap_alloc_export),
			])
			expected:   'PE64 runtime import binding: malloc requires exact `Kernel32.dll` / `GetProcessHeap` or `HeapAlloc`'
		},
		Pe64TestHeapRefusalCase{
			name:       'ownership-after-runtime-import'
			definition: pe64_test_process_exit_definition(entry, [
				ordinary_owner,
			], [
				malloc_helper,
			], valid_runtime_imports)
			expected:   'PE64 resolution: SymbolID ${u64(external)} has both import and runtime bindings'
		},
		Pe64TestHeapRefusalCase{
			name:       'helper-out-of-range'
			definition: pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
				Pe64RuntimeBinding{ symbol_id: SymbolID(999), helper: .malloc },
			], valid_runtime_imports)
			expected:   'PE64 runtime binding: SymbolID 999 is out of range'
		},
		Pe64TestHeapRefusalCase{
			name:       'helper-not-external'
			definition: pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
				Pe64RuntimeBinding{ symbol_id: entry, helper: .malloc },
			], valid_runtime_imports)
			expected:   'PE64 runtime binding: SymbolID ${u64(entry)} is not an external function'
		},
		Pe64TestHeapRefusalCase{
			name:       'duplicate-helper'
			definition: pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
				malloc_helper,
				malloc_helper,
			], valid_runtime_imports)
			expected:   'PE64 runtime binding: duplicate binding for SymbolID ${u64(external)}'
		},
		Pe64TestHeapRefusalCase{
			name:       'calloc-malloc-alias'
			definition: pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
				malloc_helper,
				calloc_helper,
			], [
				pe64_test_heap_runtime_import(.malloc, pe64_runtime_get_process_heap_export),
				pe64_test_heap_runtime_import(.malloc, pe64_runtime_heap_alloc_export),
				pe64_test_heap_runtime_import(.calloc, pe64_runtime_get_process_heap_export),
				pe64_test_heap_runtime_import(.calloc, pe64_runtime_heap_alloc_export),
			])
			expected:   'PE64 runtime binding: duplicate binding for SymbolID ${u64(external)}'
		},
		Pe64TestHeapRefusalCase{
			name:       'unknown-helper'
			definition: pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
				Pe64RuntimeBinding{ symbol_id: external, helper: .unknown },
			], []Pe64RuntimeImportBinding{})
			expected:   'PE64 runtime binding: helper 0 is unsupported'
		},
		Pe64TestHeapRefusalCase{
			name:       'forged-helper'
			definition: pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
				Pe64RuntimeBinding{
					symbol_id: external
					helper:    unsafe { Pe64RuntimeHelperKind(255) }
				},
			], []Pe64RuntimeImportBinding{})
			expected:   'PE64 runtime binding: helper 255 is unsupported'
		},
		Pe64TestHeapRefusalCase{
			name:       'adjacent-forged-helper'
			definition: pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
				Pe64RuntimeBinding{
					symbol_id: external
					helper:    unsafe { Pe64RuntimeHelperKind(11) }
				},
			], []Pe64RuntimeImportBinding{})
			expected:   'PE64 runtime binding: helper 11 is unsupported'
		},
		Pe64TestHeapRefusalCase{
			name:       'unsupported-runtime-import-owner'
			definition: pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, [
				malloc_helper,
			], runtime_imports_with_unsupported)
			expected:   'PE64 runtime import binding: helper 1 is unsupported'
		},
	]
	for item in cases {
		final_path := os.join_path(root, '${item.name}.exe')
		assert pe64_test_malloc_free_publish_error(&object, item.definition, final_path) == item.expected
		assert !pe64_test_path_present(final_path)
		assert !pe64_test_path_present(publication_stage_path(final_path))
	}
	pe64_test_assert_object_snapshot(&object, &object_before)

	mut malformed_object := pe64_test_clone_object(&object)
	malformed_before := pe64_test_clone_object(&malformed_object)
	malformed_object.text[6] = 1
	malformed_definition := cases[0].definition
	malformed_path := os.join_path(root, 'object-before-abi.exe')
	assert pe64_test_malloc_free_publish_error(&malformed_object, malformed_definition,
		malformed_path).contains('PE64 object contract')
	assert !pe64_test_path_present(malformed_path)
	assert !pe64_test_path_present(publication_stage_path(malformed_path))
	assert malformed_before.text != malformed_object.text

	mut unreferenced_object := Object.new()
	unreferenced_entry := unreferenced_object.intern_function_symbol('heap_unreferenced_entry') or {
		panic(err)
	}
	unreferenced := unreferenced_object.intern_external_function_symbol('heap_unreferenced') or {
		panic(err)
	}
	assert unreferenced_object.append_text([u8(0xeb), 0xfe]) or { panic(err) } == 0
	unreferenced_object.define_text_function(unreferenced_entry, 0, 2) or { panic(err) }
	unreferenced_definition := pe64_test_process_exit_definition(unreferenced_entry,
		[]Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: unreferenced, helper: .malloc },
	], pe64_test_malloc_runtime_imports())
	unreferenced_path := os.join_path(root, 'unreferenced-helper.exe')
	assert pe64_test_malloc_free_publish_error(&unreferenced_object, unreferenced_definition,
		unreferenced_path).contains('has no CALL relocation')
	assert !pe64_test_path_present(unreferenced_path)
	assert !pe64_test_path_present(publication_stage_path(unreferenced_path))

	mut named_object := Object.new()
	named_entry := named_object.intern_function_symbol('heap_named_entry') or { panic(err) }
	named_malloc := named_object.intern_external_function_symbol('malloc') or { panic(err) }
	named_free := named_object.intern_external_function_symbol('free') or { panic(err) }
	named_calloc := named_object.intern_external_function_symbol('calloc') or { panic(err) }
	mut named_body := [u8(0x48), 0x83, 0xec, 0x28]
	for _ in 0 .. 3 {
		named_body << [u8(0xe8), 0, 0, 0, 0]
	}
	named_body << [u8(0x48), 0x83, 0xc4, 0x28, 0xeb, 0xfe]
	assert named_object.append_text(named_body) or { panic(err) } == 0
	named_object.define_text_function(named_entry, 0, u64(named_body.len)) or { panic(err) }
	for index, symbol in [named_malloc, named_free, named_calloc] {
		named_object.add_text_call_relocation(u64(5 + index * 5), symbol) or { panic(err) }
	}
	named_definition := pe64_test_process_exit_definition(named_entry, [
		Pe64ImportBinding{
			symbol_id:   named_malloc
			dll:         'ordinary.dll'
			export_name: 'OrdinaryMalloc'
		},
		Pe64ImportBinding{
			symbol_id:   named_free
			dll:         'ordinary.dll'
			export_name: 'OrdinaryFree'
		},
		Pe64ImportBinding{
			symbol_id:   named_calloc
			dll:         'ordinary.dll'
			export_name: 'OrdinaryCalloc'
		},
	], []Pe64RuntimeBinding{}, []Pe64RuntimeImportBinding{})
	named_plan := pe64_prepare_runtime_helpers(&named_object, named_definition.runtime_helpers) or {
		panic(err)
	}
	assert named_plan.physical.len == 0
	named_image := pe64_test_malloc_free_image_bytes(&named_object, named_definition) or {
		panic(err)
	}
	named_text := pe64_test_section(named_image, '.text')
	assert named_text.virtual_size == u32(named_object.text.len) +
		u32(3 * int(pe64_import_thunk_size))
	assert pe64_test_imports(named_image).map(it.export_name) == [
		'OrdinaryCalloc',
		'OrdinaryFree',
		'OrdinaryMalloc',
	]
	assert pe64_test_imports(named_image).all(it.dll == 'ordinary.dll')

	calloc_binding_object, calloc_binding_entry, calloc_binding_symbol := pe64_test_runtime_fixture('calloc_binding_entry',
		'opaque_calloc_activation')
	calloc_binding_definition := pe64_test_process_exit_definition(calloc_binding_entry,
		[]Pe64ImportBinding{}, [
		Pe64RuntimeBinding{
			symbol_id: calloc_binding_symbol
			helper:    .calloc
		},
	], valid_calloc_runtime_imports)
	calloc_binding_plan := pe64_prepare_runtime_helpers(&calloc_binding_object,
		calloc_binding_definition.runtime_helpers) or { panic(err) }
	assert calloc_binding_plan.physical == [Pe64RuntimeHelperKind.calloc]
	assert calloc_binding_plan.calloc_physical_index == 0
	calloc_binding_image := pe64_test_malloc_free_image_bytes(&calloc_binding_object,
		calloc_binding_definition) or { panic(err) }
	calloc_binding_text := pe64_test_section(calloc_binding_image, '.text')
	calloc_binding_offset := calloc_binding_object.text.len
	calloc_binding_rva := u64(calloc_binding_text.virtual_address) + u64(calloc_binding_offset)
	calloc_binding_runtime_end := calloc_binding_offset + int(pe64_runtime_calloc_size)
	calloc_get_process_heap_rva := pe64_test_import_thunk_rva(calloc_binding_image,
		calloc_binding_text, calloc_binding_runtime_end, pe64_runtime_heap_dll,
		pe64_runtime_get_process_heap_export)
	calloc_heap_alloc_rva := pe64_test_import_thunk_rva(calloc_binding_image, calloc_binding_text,
		calloc_binding_runtime_end, pe64_runtime_heap_dll, pe64_runtime_heap_alloc_export)
	mut expected_calloc_binding_body := pe64_test_runtime_calloc_template()
	pe64_test_put_u32(mut expected_calloc_binding_body, 21, pe64_test_rel32_bits(
		calloc_binding_rva + 21, calloc_get_process_heap_rva))
	pe64_test_put_u32(mut expected_calloc_binding_body, 50, pe64_test_rel32_bits(
		calloc_binding_rva + 50, calloc_heap_alloc_rva))
	assert pe64_test_runtime_body(calloc_binding_image, calloc_binding_text, calloc_binding_offset,
		int(pe64_runtime_calloc_size)) == expected_calloc_binding_body
	assert pe64_test_imports(calloc_binding_image).map(it.export_name) == [
		'GetProcessHeap',
		'HeapAlloc',
	]

	binding_object, binding_entry, binding_symbol := pe64_test_runtime_fixture('heap_binding_entry',
		'free')
	binding_definition := pe64_test_process_exit_definition(binding_entry, []Pe64ImportBinding{}, [
		Pe64RuntimeBinding{ symbol_id: binding_symbol, helper: .malloc },
	], pe64_test_malloc_runtime_imports())
	binding_image := pe64_test_malloc_free_image_bytes(&binding_object, binding_definition) or {
		panic(err)
	}
	binding_text := pe64_test_section(binding_image, '.text')
	assert pe64_test_runtime_body(binding_image, binding_text, binding_object.text.len,
		int(pe64_runtime_malloc_size))[..10] == pe64_test_runtime_malloc_template()[..10]
	assert pe64_test_imports(binding_image).map(it.export_name) == [
		'GetProcessHeap',
		'HeapAlloc',
	]

	mut framed_object := Object.new()
	framed_entry := framed_object.intern_function_symbol('heap_framed_entry') or { panic(err) }
	framed_body := [u8(0x48), 0x83, 0xec, 0x28, 0x31, 0xc0, 0x48, 0x83, 0xc4, 0x28, 0xc3]
	assert framed_object.append_text(framed_body) or { panic(err) } == 0
	framed_object.define_text_function(framed_entry, 0, u64(framed_body.len)) or { panic(err) }
	framed_object.add_function_frame(framed_entry, [u8(0x48), 0x83, 0xec, 0x28], [
		u8(0x48),
		0x83,
		0xc4,
		0x28,
	], [u8(0x01), 0x04, 0x01, 0, 0x04, 0x42, 0, 0]) or { panic(err) }
	framed_before := pe64_test_clone_object(&framed_object)
	framed_definition := pe64_test_process_exit_definition(framed_entry, []Pe64ImportBinding{},
		[]Pe64RuntimeBinding{}, []Pe64RuntimeImportBinding{})
	framed_path := os.join_path(root, 'function-frame-object.exe')
	assert pe64_test_malloc_free_publish_error(&framed_object, framed_definition, framed_path) == 'PE64 object contract: AMD64 object function frames require explicit object-format writer support'
	pe64_test_assert_object_snapshot(&framed_object, &framed_before)
	assert !pe64_test_path_present(framed_path)
	assert !pe64_test_path_present(publication_stage_path(framed_path))

	final_sentinel_path := os.join_path(root, 'existing-final.exe')
	final_sentinel := 'pe4g-final-sentinel\n'
	os.write_file(final_sentinel_path, final_sentinel) or { panic(err) }
	assert pe64_test_publication_error(final_sentinel_path, calloc_binding_image) == 'final output `${final_sentinel_path}` already exists'
	final_sentinel_after := os.read_file(final_sentinel_path) or { panic(err) }
	assert final_sentinel_after == final_sentinel
	assert !pe64_test_path_present(publication_stage_path(final_sentinel_path))

	stage_final_path := os.join_path(root, 'existing-stage.exe')
	stage_sentinel_path := publication_stage_path(stage_final_path)
	stage_sentinel := 'pe4g-stage-sentinel\n'
	os.write_file(stage_sentinel_path, stage_sentinel) or { panic(err) }
	assert pe64_test_publication_error(stage_final_path, calloc_binding_image) == 'stage `${stage_sentinel_path}` already exists'
	stage_sentinel_after := os.read_file(stage_sentinel_path) or { panic(err) }
	assert stage_sentinel_after == stage_sentinel
	assert !pe64_test_path_present(stage_final_path)

	assert pe64_checked_rel32(0, u64(4) + u64(max_i32)) or { panic(err) } == u32(max_i32)
	assert pe64_checked_rel32(u64(2_147_483_644), 0) or { panic(err) } == u32(0x8000_0000)
	for field_and_target in [
		[u64(0), u64(4) + u64(max_i32) + 1],
		[u64(2_147_483_645), u64(0)],
		[max_u64 - 3, max_u64],
	] {
		if _ := pe64_checked_rel32(field_and_target[0], field_and_target[1]) {
			assert false, 'PE64 rel32 overflow unexpectedly succeeded'
		} else {
			assert err.msg() == 'PE64 REL32 overflow'
		}
	}
	if _ := pe64_checked_add(max_u64, 1, 'heap helper extent') {
		assert false, 'PE64 heap helper extent overflow unexpectedly succeeded'
	} else {
		assert err.msg() == 'PE64 layout: heap helper extent overflows u64'
	}
	final_sentinel_end := os.read_file(final_sentinel_path) or { panic(err) }
	stage_sentinel_end := os.read_file(stage_sentinel_path) or { panic(err) }
	assert final_sentinel_end == final_sentinel_after
	assert stage_sentinel_end == stage_sentinel_after
	os.rm(stage_sentinel_path) or { panic(err) }
}

fn pe64_test_unique_instruction_index(instructions []Pe64TestDisassemblyInstruction,
	address u64, label string) int {
	mut found := -1
	for index, instruction in instructions {
		if instruction.address == address {
			assert found == -1, '${label}: disassembler repeated address ${address:016x}'
			found = index
		}
	}
	assert found >= 0, '${label}: disassembler omitted address ${address:016x}'
	return found
}

fn pe64_test_assert_complete_disassembly_body(instructions []Pe64TestDisassemblyInstruction,
	address u64, expected []u8, label string) {
	mut index := pe64_test_unique_instruction_index(instructions, address, label)
	mut cursor := 0
	for cursor < expected.len {
		assert index < instructions.len, '${label}: disassembly ended after ${cursor} bytes'
		instruction := instructions[index]
		assert instruction.address == address + u64(cursor), '${label}: disassembly is not contiguous at byte ${cursor}'

		assert instruction.bytes.len > 0
		assert instruction.bytes.len <= expected.len - cursor, '${label}: final instruction crosses the helper boundary'

		assert instruction.bytes == expected[cursor..cursor + instruction.bytes.len], '${label}: instruction bytes differ at byte ${cursor}'

		cursor += instruction.bytes.len
		index++
	}
	assert cursor == expected.len
}

fn pe64_test_assert_disassembly_target(instructions []Pe64TestDisassemblyInstruction,
	address u64, mnemonics []string, target u64, label string) {
	index := pe64_test_unique_instruction_index(instructions, address, label)
	instruction := instructions[index]
	assert instruction.mnemonic in mnemonics, '${label}: unexpected mnemonic `${instruction.mnemonic}`'

	assert pe64_test_dumpbin_operand_has_address(instruction.operands, target), '${label}: `${instruction.operands}` does not name ${target:016x}'
}

fn pe64_test_assert_heap_disassembly(instructions []Pe64TestDisassemblyInstruction,
	malloc_address u64, free_address u64, calloc_address u64, get_process_heap_address u64,
	heap_alloc_address u64, heap_free_address u64, malloc_body []u8, free_body []u8,
	calloc_body []u8) {
	pe64_test_assert_complete_disassembly_body(instructions, malloc_address, malloc_body,
		'malloc helper')
	pe64_test_assert_complete_disassembly_body(instructions, free_address, free_body, 'free helper')
	pe64_test_assert_complete_disassembly_body(instructions, calloc_address, calloc_body,
		'calloc helper')

	pe64_test_assert_disassembly_target(instructions, malloc_address + 9, ['call', 'callq'],
		get_process_heap_address, 'malloc GetProcessHeap call')
	pe64_test_assert_disassembly_target(instructions, malloc_address + 35, ['call', 'callq'],
		heap_alloc_address, 'malloc HeapAlloc call')
	pe64_test_assert_disassembly_target(instructions, free_address + 18, ['call', 'callq'],
		get_process_heap_address, 'free GetProcessHeap call')
	pe64_test_assert_disassembly_target(instructions, free_address + 38, ['call', 'callq'],
		heap_free_address, 'free HeapFree call')
	pe64_test_assert_disassembly_target(instructions, calloc_address + 20, ['call', 'callq'],
		get_process_heap_address, 'calloc GetProcessHeap call')
	pe64_test_assert_disassembly_target(instructions, calloc_address + 49, ['call', 'callq'],
		heap_alloc_address, 'calloc HeapAlloc call')

	pe64_test_assert_disassembly_target(instructions, malloc_address + 17, ['je', 'jz'],

		malloc_address + 68, 'malloc null-heap branch')
	pe64_test_assert_disassembly_target(instructions, malloc_address + 33, [
		'jb',
		'jc',
		'jnae',
	], malloc_address + 68, 'malloc carry branch')
	pe64_test_assert_disassembly_target(instructions, malloc_address + 43, ['je', 'jz'],

		malloc_address + 68, 'malloc null-allocation branch')
	pe64_test_assert_disassembly_target(instructions, free_address + 7, ['je', 'jz'],

		free_address + 43, 'free null-pointer branch')
	pe64_test_assert_disassembly_target(instructions, free_address + 26, ['je', 'jz'],

		free_address + 43, 'free null-heap branch')
	pe64_test_assert_disassembly_target(instructions, calloc_address + 13, ['jne', 'jnz'],

		calloc_address + 82, 'calloc multiplication-overflow branch')
	pe64_test_assert_disassembly_target(instructions, calloc_address + 28, ['je', 'jz'],

		calloc_address + 82, 'calloc null-heap branch')
	pe64_test_assert_disassembly_target(instructions, calloc_address + 47, [
		'jb',
		'jc',
		'jnae',
	], calloc_address + 82, 'calloc carry branch')
	pe64_test_assert_disassembly_target(instructions, calloc_address + 57, ['je', 'jz'],

		calloc_address + 82, 'calloc null-allocation branch')

	for address, label in {
		get_process_heap_address: 'GetProcessHeap thunk'
		heap_alloc_address:       'HeapAlloc thunk'
		heap_free_address:        'HeapFree thunk'
	} {
		index := pe64_test_unique_instruction_index(instructions, address, label)
		instruction := instructions[index]
		assert instruction.bytes.len == int(pe64_import_thunk_size)
		assert instruction.bytes[0..2] == [u8(0xff), 0x25]
		assert instruction.mnemonic in ['jmp', 'jmpq']
	}
}

fn pe64_test_calloc_canary_image(name string, body []u8, calloc_fields []u64, free_fields []u64,
	process_exit_fields []u64) []u8 {
	assert calloc_fields.len > 0
	assert process_exit_fields.len > 0
	mut object := Object.new()
	entry := object.intern_function_symbol('${name}_entry') or { panic(err) }
	calloc_symbol := object.intern_external_function_symbol('${name}_calloc') or { panic(err) }
	mut free_symbol := SymbolID(0)
	if free_fields.len > 0 {
		free_symbol = object.intern_external_function_symbol('${name}_free') or { panic(err) }
	}
	process_exit_symbol := object.intern_external_function_symbol('${name}_exit') or { panic(err) }
	assert object.append_text(body) or { panic(err) } == 0
	object.define_text_function(entry, 0, u64(body.len)) or { panic(err) }
	for field in calloc_fields {
		object.add_text_call_relocation(field, calloc_symbol) or { panic(err) }
	}
	for field in free_fields {
		object.add_text_call_relocation(field, free_symbol) or { panic(err) }
	}
	for field in process_exit_fields {
		object.add_text_call_relocation(field, process_exit_symbol) or { panic(err) }
	}
	mut helpers := [
		Pe64RuntimeBinding{
			symbol_id: process_exit_symbol
			helper:    .process_exit
		},
		Pe64RuntimeBinding{
			symbol_id: calloc_symbol
			helper:    .calloc
		},
	]
	mut runtime_imports := [pe64_test_process_exit_import()]
	runtime_imports << pe64_test_calloc_runtime_imports()
	if free_fields.len > 0 {
		helpers << Pe64RuntimeBinding{
			symbol_id: free_symbol
			helper:    .free
		}
		runtime_imports << pe64_test_free_runtime_imports()
	}
	definition := pe64_test_process_exit_definition(entry, []Pe64ImportBinding{}, helpers,
		runtime_imports)
	return pe64_test_malloc_free_image_bytes(&object, definition) or { panic(err) }
}

fn pe64_test_run_calloc_canary(root string, name string, body []u8, calloc_fields []u64,
	free_fields []u64, process_exit_fields []u64) {
	image := pe64_test_calloc_canary_image(name, body, calloc_fields, free_fields,
		process_exit_fields)
	output := os.join_path(root, '${name}.exe')
	publish_object(output, image) or { assert false, err.msg() }
	result := pe64_test_run_process(output, []string{}, pe64_test_dumpbin_environment(),
		pe64_test_timeout_ms)
	assert !result.timed_out, '${name} timed out'
	assert !result.output_limited, '${name} exceeded the output limit'
	assert result.exit_code == 0, '${name} exited ${result.exit_code}: ${result.stderr}'
	assert result.stdout.len == 0, '${name} wrote stdout: ${result.stdout}'
	assert result.stderr.len == 0, '${name} wrote stderr: ${result.stderr}'
}

fn test_pe64_runtime_malloc_free_llvm_and_dumpbin_complete_oracles_when_guarded() {
	mandatory_llvm := os.getenv(pe64_test_malloc_free_llvm_guard) == '1'
	object, entry, malloc_symbol, free_symbol, calloc_symbol := pe64_test_malloc_free_fixture('heap_oracle_entry',
		'heap_oracle_malloc', 'heap_oracle_free', 'heap_oracle_calloc')
	definition := pe64_test_malloc_free_definition(entry, malloc_symbol, free_symbol, calloc_symbol)
	object_before := pe64_test_clone_object(&object)
	definition_before := pe64_test_clone_image_definition(&definition)
	image := pe64_test_malloc_free_image_bytes(&object, definition) or { panic(err) }
	text := pe64_test_section(image, '.text')
	pdata := pe64_test_section(image, '.pdata')
	xdata := pe64_test_section(image, '.xdata')
	imports := pe64_test_imports(image)
	assert imports.map('${it.dll}/${it.export_name}') == [
		'Kernel32.dll/GetProcessHeap',
		'Kernel32.dll/HeapAlloc',
		'Kernel32.dll/HeapFree',
	]

	malloc_offset := object.text.len
	free_offset := malloc_offset + int(pe64_runtime_malloc_size)
	calloc_offset := free_offset + int(pe64_runtime_free_size)
	thunk_offset := calloc_offset + int(pe64_runtime_calloc_size)
	malloc_rva := u64(text.virtual_address) + u64(malloc_offset)
	free_rva := u64(text.virtual_address) + u64(free_offset)
	calloc_rva := u64(text.virtual_address) + u64(calloc_offset)
	get_process_heap_rva := u64(text.virtual_address) + u64(thunk_offset)
	heap_alloc_rva := get_process_heap_rva + pe64_import_thunk_size
	heap_free_rva := heap_alloc_rva + pe64_import_thunk_size
	mut malloc_body := pe64_test_runtime_malloc_template()
	pe64_test_put_u32(mut malloc_body, 10, pe64_test_rel32_bits(malloc_rva + 10,
		get_process_heap_rva))
	pe64_test_put_u32(mut malloc_body, 36, pe64_test_rel32_bits(malloc_rva + 36, heap_alloc_rva))
	mut free_body := pe64_test_runtime_free_template()
	pe64_test_put_u32(mut free_body, 19, pe64_test_rel32_bits(free_rva + 19, get_process_heap_rva))
	pe64_test_put_u32(mut free_body, 39, pe64_test_rel32_bits(free_rva + 39, heap_free_rva))
	mut calloc_body := pe64_test_runtime_calloc_template()
	pe64_test_put_u32(mut calloc_body, 21, pe64_test_rel32_bits(calloc_rva + 21,
		get_process_heap_rva))
	pe64_test_put_u32(mut calloc_body, 50, pe64_test_rel32_bits(calloc_rva + 50, heap_alloc_rva))
	assert pe64_test_runtime_body(image, text, malloc_offset, int(pe64_runtime_malloc_size)) == malloc_body
	assert pe64_test_runtime_body(image, text, free_offset, int(pe64_runtime_free_size)) == free_body
	assert pe64_test_runtime_body(image, text, calloc_offset, int(pe64_runtime_calloc_size)) == calloc_body
	assert object.text != malloc_body[..object.text.len]
	assert object.text != free_body[..object.text.len]
	assert object.text != calloc_body[..object.text.len]

	for index, thunk_rva in [get_process_heap_rva, heap_alloc_rva, heap_free_rva] {
		thunk := pe64_test_runtime_body(image, text, thunk_offset +
			index * int(pe64_import_thunk_size), int(pe64_import_thunk_size))
		assert thunk[0..2] == [u8(0xff), 0x25]
		assert pe64_test_rel32_target(thunk_rva + 2, pe64_test_read_u32(thunk, 2)) == u64(imports[index].iat_rva)
	}

	assert pdata.virtual_size == u32(pe64_runtime_function_size * 4)
	assert xdata.virtual_size == u32(pe64_unwind_info_size * 4)
	assert pe64_test_directory(image, pe64_exception_directory_index) == Pe64TestDirectory{
		rva:  pdata.virtual_address
		size: pdata.virtual_size
	}
	pdata_raw := int(pdata.raw_pointer)
	expected_rows := [
		[u32(text.virtual_address), text.virtual_address + u32(object.text.len), xdata.virtual_address],
		[u32(malloc_rva), u32(malloc_rva + pe64_runtime_malloc_size),
			xdata.virtual_address + u32(pe64_unwind_info_size)],
		[u32(free_rva), u32(free_rva + pe64_runtime_free_size),
			xdata.virtual_address + u32(2 * int(pe64_unwind_info_size))],
		[u32(calloc_rva), u32(calloc_rva + pe64_runtime_calloc_size),
			xdata.virtual_address + u32(3 * int(pe64_unwind_info_size))],
	]
	for index, row in expected_rows {
		row_offset := pdata_raw + index * int(pe64_runtime_function_size)
		assert pe64_test_read_u32(image, row_offset) == row[0]
		assert pe64_test_read_u32(image, row_offset + 4) == row[1]
		assert pe64_test_read_u32(image, row_offset + 8) == row[2]
	}
	unwind := [u8(0x01), 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00]
	xdata_raw := int(xdata.raw_pointer)
	for index in 0 .. 4 {
		offset := xdata_raw + index * int(pe64_unwind_info_size)
		assert image[offset..offset + int(pe64_unwind_info_size)] == unwind
	}

	root := pe64_test_root('runtime malloc free ; complete oracle')
	defer {
		pe64_test_cleanup(root)
	}
	output := os.join_path(root, 'malloc free ; complete oracle.exe')
	publish_object(output, image) or { assert false, err.msg() }
	malloc_address := pe64_image_base + malloc_rva
	free_address := pe64_image_base + free_rva
	calloc_address := pe64_image_base + calloc_rva
	get_process_heap_address := pe64_image_base + get_process_heap_rva
	heap_alloc_address := pe64_image_base + heap_alloc_rva
	heap_free_address := pe64_image_base + heap_free_rva

	llvm_objdump := pe64_test_find_process_exit_llvm_objdump()
	if llvm_objdump.len == 0 {
		assert !mandatory_llvm, 'mandatory LLVM 21 malloc/free oracle is unavailable'
	} else {
		version := pe64_test_run_process(llvm_objdump, ['--version'],
			pe64_test_dumpbin_environment(), pe64_test_timeout_ms)
		compatible := version.exit_code == 0 && !version.timed_out && !version.output_limited
			&& version.stdout.contains('LLVM version 21.1.8')
		if !compatible {
			assert !mandatory_llvm, 'mandatory LLVM malloc/free oracle fingerprint mismatch:\n${version.stdout}\n${version.stderr}'
		} else {
			result := pe64_test_run_process(llvm_objdump, [
				'--disassemble',
				'--x86-asm-syntax=intel',
				output,
			], pe64_test_dumpbin_environment(), pe64_test_timeout_ms)
			assert !result.timed_out, 'LLVM malloc/free oracle timed out'
			assert !result.output_limited, 'LLVM malloc/free oracle exceeded the output limit'
			assert result.exit_code == 0, result.stderr
			pe64_test_assert_heap_disassembly(pe64_test_dumpbin_instructions(result.stdout),
				malloc_address, free_address, calloc_address, get_process_heap_address,
				heap_alloc_address, heap_free_address, malloc_body, free_body, calloc_body)
		}
	}

	$if windows && amd64 {
		if os.getenv(pe64_test_dumpbin_guard) == '1' {
			dumpbin := os.getenv(pe64_test_dumpbin_path)
			assert dumpbin.len != 0, '${pe64_test_dumpbin_path} must name an absolute Microsoft DUMPBIN path'
			assert os.is_abs_path(dumpbin), 'DUMPBIN path must be absolute'
			assert os.is_file(dumpbin) && !os.is_link(dumpbin), 'DUMPBIN path must be a regular file'

			disassembly := pe64_test_run_process(dumpbin, ['/DISASM:BYTES', output],
				pe64_test_dumpbin_environment(), pe64_test_timeout_ms)
			assert !disassembly.timed_out, 'DUMPBIN malloc/free disassembly timed out'
			assert !disassembly.output_limited, 'DUMPBIN malloc/free disassembly exceeded the output limit'

			assert disassembly.exit_code == 0, disassembly.stderr
			pe64_test_assert_heap_disassembly(pe64_test_dumpbin_instructions(disassembly.stdout),
				malloc_address, free_address, calloc_address, get_process_heap_address,
				heap_alloc_address, heap_free_address, malloc_body, free_body, calloc_body)

			headers := pe64_test_run_process(dumpbin, ['/HEADERS', '/IMPORTS', output],
				pe64_test_dumpbin_environment(), pe64_test_timeout_ms)
			assert !headers.timed_out, 'DUMPBIN malloc/free headers/imports timed out'
			assert !headers.output_limited, 'DUMPBIN malloc/free headers/imports exceeded the output limit'

			assert headers.exit_code == 0, headers.stderr
			assert headers.stdout.contains('PE32+')

			assert
				headers.stdout.contains('machine (x64)') || headers.stdout.contains('8664 machine')
			for expected in ['.pdata', '.xdata'] {
				assert headers.stdout.contains(expected), 'DUMPBIN omitted `${expected}` from headers/imports'
			}
			dumpbin_dlls, dumpbin_imports := pe64_test_dumpbin_import_records(headers.stdout)
			assert dumpbin_dlls == ['Kernel32.dll']
			assert dumpbin_imports.map('${it.dll}/${it.export_name}') == [
				'Kernel32.dll/GetProcessHeap',
				'Kernel32.dll/HeapAlloc',
				'Kernel32.dll/HeapFree',
			]

			runtime_functions := pe64_test_run_process(dumpbin, ['/PDATA', output],
				pe64_test_dumpbin_environment(), pe64_test_timeout_ms)
			assert !runtime_functions.timed_out, 'DUMPBIN runtime-function oracle timed out'
			assert !runtime_functions.output_limited, 'DUMPBIN runtime-function oracle exceeded the output limit'
			assert runtime_functions.exit_code == 0, runtime_functions.stderr
			dumpbin_runtime_functions :=
				pe64_test_dumpbin_runtime_functions(runtime_functions.stdout)
			assert dumpbin_runtime_functions.len == expected_rows.len
			for index, row in dumpbin_runtime_functions {
				assert pe64_test_dumpbin_image_rva(row.begin) == u64(expected_rows[index][0])
				assert pe64_test_dumpbin_image_rva(row.end) == u64(expected_rows[index][1])
				assert pe64_test_dumpbin_image_rva(row.unwind) == u64(expected_rows[index][2])
			}
			assert pe64_test_dumpbin_image_rva(dumpbin_runtime_functions[3].begin) == calloc_rva
			assert pe64_test_dumpbin_image_rva(dumpbin_runtime_functions[3].end) == calloc_rva +
				pe64_runtime_calloc_size
			assert pe64_test_dumpbin_image_rva(dumpbin_runtime_functions[3].unwind) ==
				u64(xdata.virtual_address) + u64(3 * int(pe64_unwind_info_size))

			pdata_dump := pe64_test_run_process(dumpbin, [
				'/RAWDATA:1',
				'/SECTION:.pdata',
				output,
			], pe64_test_dumpbin_environment(), pe64_test_timeout_ms)
			assert !pdata_dump.timed_out, 'DUMPBIN .pdata raw-data oracle timed out'
			assert !pdata_dump.output_limited, 'DUMPBIN .pdata raw-data oracle exceeded the output limit'
			assert pdata_dump.exit_code == 0, pdata_dump.stderr
			dumpbin_pdata := pe64_test_dumpbin_raw_bytes(pdata_dump.stdout)
			expected_pdata := image[int(pdata.raw_pointer)..int(pdata.raw_pointer + pdata.raw_size)]
			assert dumpbin_pdata == expected_pdata
			calloc_pdata_offset := 3 * int(pe64_runtime_function_size)
			assert dumpbin_pdata[calloc_pdata_offset..calloc_pdata_offset +
				int(pe64_runtime_function_size)] == image[pdata_raw + calloc_pdata_offset..
				pdata_raw + calloc_pdata_offset + int(pe64_runtime_function_size)]

			xdata_dump := pe64_test_run_process(dumpbin, [
				'/RAWDATA:1',
				'/SECTION:.xdata',
				output,
			], pe64_test_dumpbin_environment(), pe64_test_timeout_ms)
			assert !xdata_dump.timed_out, 'DUMPBIN .xdata raw-data oracle timed out'
			assert !xdata_dump.output_limited, 'DUMPBIN .xdata raw-data oracle exceeded the output limit'
			assert xdata_dump.exit_code == 0, xdata_dump.stderr
			dumpbin_xdata := pe64_test_dumpbin_raw_bytes(xdata_dump.stdout)
			expected_xdata := image[int(xdata.raw_pointer)..int(xdata.raw_pointer + xdata.raw_size)]
			assert dumpbin_xdata == expected_xdata
			calloc_xdata_offset := 3 * int(pe64_unwind_info_size)
			assert dumpbin_xdata[calloc_xdata_offset..calloc_xdata_offset +
				int(pe64_unwind_info_size)] == image[xdata_raw + calloc_xdata_offset..xdata_raw +
				calloc_xdata_offset + int(pe64_unwind_info_size)]

			canary_a := [u8(0x48), 0x83, 0xec, 0x28, 0xb9, 0x04, 0, 0, 0, 0xba, 0x08, 0, 0, 0,
				0xe8, 0, 0, 0, 0, 0x48, 0x85, 0xc0, 0x74, 0x2c, 0x49, 0x89, 0xc2, 0xb9, 0x20, 0,
				0, 0, 0x31, 0xd2, 0x41, 0x80, 0x3c, 0x12, 0, 0x75, 0x1b, 0x48, 0xff, 0xc2, 0x48,
				0x39, 0xca, 0x72, 0xf1, 0xc6, 0x40, 0x1f, 0x5a, 0x48, 0x89, 0xc1, 0xe8, 0, 0, 0,
				0, 0x31, 0xc9, 0xe8, 0, 0, 0, 0, 0xb9, 0x01, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0xcc]
			pe64_test_run_calloc_canary(root, 'calloc-zero-write-free', canary_a, [
				u64(15),
			], [u64(57)], [u64(64), 74])

			canary_b := [u8(0x48), 0x83, 0xec, 0x28, 0x48, 0xc7, 0xc1, 0xff, 0xff, 0xff, 0xff,
				0xba, 0x02, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0x48, 0x85, 0xc0, 0x75, 0x07, 0x31, 0xc9,
				0xe8, 0, 0, 0, 0, 0xb9, 0x01, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0xcc]
			pe64_test_run_calloc_canary(root, 'calloc-multiply-overflow', canary_b, [
				u64(17),
			], []u64{}, [u64(29), 39])

			canary_c := [u8(0x48), 0x83, 0xec, 0x28, 0x31, 0xc9, 0xba, 0x08, 0, 0, 0, 0xe8, 0,
				0, 0, 0, 0x48, 0x85, 0xc0, 0x74, 0x0f, 0x48, 0x89, 0xc1, 0xe8, 0, 0, 0, 0, 0x31,
				0xc9, 0xe8, 0, 0, 0, 0, 0xb9, 0x01, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0xcc]
			pe64_test_run_calloc_canary(root, 'calloc-zero-factor', canary_c, [
				u64(12),
			], [u64(25)], [u64(32), 42])
		}
	}
	pe64_test_assert_object_snapshot(&object, &object_before)
	pe64_test_assert_image_definition_snapshot(&definition, &definition_before)
}
