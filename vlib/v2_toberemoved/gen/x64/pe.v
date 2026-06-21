// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module x64

import os

const pe_dos_stub_size = 0x80
const pe_signature = u32(0x00004550)
const pe_optional_header64_magic = u16(0x20b)
const pe_file_alignment = 0x200
const pe_section_alignment = 0x1000
const pe_image_base = u64(0x140000000)
const pe_size_of_optional_header64 = u16(0xf0)
const pe_number_of_rva_and_sizes = 16

// These optional-header values are local policy defaults for the current
// minimal linker, not requirements imposed by the Microsoft PE/COFF spec.
const pe_linker_major_version = u8(0)
const pe_linker_minor_version = u8(1)
const pe_major_operating_system_version = u16(6)
const pe_minor_operating_system_version = u16(0)
const pe_major_subsystem_version = u16(6)
const pe_minor_subsystem_version = u16(0)
const pe_size_of_stack_reserve = u64(0x100000)
const pe_size_of_stack_commit = u64(0x1000)
const pe_size_of_heap_reserve = u64(0x100000)
const pe_size_of_heap_commit = u64(0x1000)

const pe_image_file_relocs_stripped = u16(0x0001)
const pe_image_file_executable_image = u16(0x0002)
const pe_image_file_large_address_aware = u16(0x0020)
const pe_image_subsystem_windows_cui = u16(3)
const pe_dll_characteristics_nx_compat = u16(0x0100)

const pe_image_scn_cnt_code = u32(0x00000020)
const pe_image_scn_cnt_initialized_data = u32(0x00000040)
const pe_image_scn_mem_execute = u32(0x20000000)
const pe_image_scn_mem_read = u32(0x40000000)
const pe_image_scn_mem_write = u32(0x80000000)

const pe_import_directory_index = 1
const pe_exception_directory_index = 3
const pe_base_reloc_directory_index = 5
const pe_iat_directory_index = 12
const pe_runtime_data_alignment = 16
const pe_wgetenv_buffer_wchars = 32768
const pe_wgetenv_buffer_bytes = pe_wgetenv_buffer_wchars * 2
const pe_atexit_callback_capacity = 64
const pe_run_atexit_symbol = '__v2_pe_run_atexit'

const pe_kernel32_dll = 'kernel32.dll'
const pe_shell32_dll = 'shell32.dll'
const pe_ucrtbase_dll = 'ucrtbase.dll'
const pe_msvcrt_dll = 'msvcrt.dll'
const pe_kernel32_imports = ['ExitProcess', 'GetCommandLineW', 'GetStdHandle', 'GetConsoleMode',
	'MultiByteToWideChar', 'WideCharToMultiByte', 'GetCurrentDirectoryW', 'GetEnvironmentVariableW',
	'ReadConsoleW', 'ReadFile', 'WriteConsoleW', 'WriteFile', 'GetProcessHeap', 'HeapAlloc',
	'HeapReAlloc', 'HeapFree', 'GetCurrentThreadId', 'GetSystemTimeAsFileTime',
	'FileTimeToSystemTime', 'SystemTimeToTzSpecificLocalTime', 'QueryPerformanceFrequency',
	'QueryPerformanceCounter']
const pe_shell32_imports = ['CommandLineToArgvW']
const pe_ucrtbase_imports = ['log', 'ldexp', 'sqrt', '_time64', '_localtime64']
const pe_msvcrt_imports = ['_scprintf', '_snprintf']

struct PeImport {
	dll  string
	name string
}

struct PeRuntimeCallPatch {
	field_off int
	dll       string
	name      string
}

struct PeRuntimeDataPatch {
	field_off int
	data_off  u32
}

struct PeRuntimeText {
mut:
	bytes          []u8
	data           []u8
	symbols        map[string]u32
	import_patches []PeRuntimeCallPatch
	data_patches   []PeRuntimeDataPatch
}

struct PeSection {
mut:
	name            string
	data            []u8
	virtual_address u32
	virtual_size    u32
	raw_pointer     u32
	raw_size        u32
	characteristics u32
}

struct PeIdata {
	data        []u8
	import_rva  u32
	import_size u32
	iat_rva     u32
	iat_size    u32
	iat_rvas    map[string]u32
}

struct PeImportGroup {
	dll   string
	start int
mut:
	len int
}

struct PeTextBuild {
	data                 []u8
	argc_global_disp_off int
	argv_global_disp_off int
}

pub struct PeLinker {
	coff &CoffObject
mut:
	imports []PeImport
}

pub fn PeLinker.new(coff &CoffObject) &PeLinker {
	return unsafe {
		&PeLinker{
			coff: coff
		}
	}
}

pub fn (mut l PeLinker) write(path string) ! {
	image := l.image()!
	os.write_file_array(path, image)!
	pe_check_written_image(path, image.len)!
}

pub fn (mut l PeLinker) image() ![]u8 {
	mut runtime_text := l.build_runtime_text()
	l.imports = l.required_pe_imports(runtime_text)!
	l.patch_runtime_import_calls(mut runtime_text)!
	text_build := l.build_text_section(runtime_text)!
	mut text := text_build.data.clone()
	mut data_data := l.coff.data_data.clone()
	runtime_data_base := if runtime_text.data.len > 0 {
		base := align_int(data_data.len, pe_runtime_data_alignment)
		for data_data.len < base {
			data_data << u8(0)
		}
		data_data << runtime_text.data
		base
	} else {
		data_data.len
	}
	section_count := 2 + if l.coff.rodata.len > 0 { 1 } else { 0 } +
		if data_data.len > 0 { 1 } else { 0 }
	header_size := pe_headers_size(section_count)
	mut raw_pointer := header_size
	mut next_rva := u32(pe_section_alignment)
	mut sections := []PeSection{cap: section_count}

	text_section, next_text_raw := pe_make_section('.text', text, next_rva, raw_pointer,
		pe_image_scn_cnt_code | pe_image_scn_mem_execute | pe_image_scn_mem_read)
	raw_pointer = next_text_raw
	next_rva = pe_next_section_rva(text_section.virtual_address, int(text_section.virtual_size))
	sections << text_section

	mut rdata_rva := u32(0)
	if l.coff.rodata.len > 0 {
		rdata_section, next_rdata_raw := pe_make_section('.rdata', l.coff.rodata, next_rva,
			raw_pointer, pe_image_scn_cnt_initialized_data | pe_image_scn_mem_read)
		raw_pointer = next_rdata_raw
		next_rva = pe_next_section_rva(rdata_section.virtual_address,
			int(rdata_section.virtual_size))
		rdata_rva = rdata_section.virtual_address
		sections << rdata_section
	}

	mut data_rva := u32(0)
	if data_data.len > 0 {
		data_section, next_data_raw := pe_make_section('.data', data_data, next_rva, raw_pointer,
			pe_image_scn_cnt_initialized_data | pe_image_scn_mem_read | pe_image_scn_mem_write)
		raw_pointer = next_data_raw
		next_rva = pe_next_section_rva(data_section.virtual_address, int(data_section.virtual_size))
		data_rva = data_section.virtual_address
		sections << data_section
	}

	idata := l.build_idata(next_rva)
	idata_section, next_idata_raw := pe_make_section('.idata', idata.data, next_rva, raw_pointer,
		pe_image_scn_cnt_initialized_data | pe_image_scn_mem_read | pe_image_scn_mem_write)
	raw_pointer = next_idata_raw
	sections << idata_section

	import_thunks := l.import_thunk_rvas(text_section.virtual_address, runtime_text.bytes.len)
	import_iats := idata.iat_rvas.clone()
	runtime_thunks := runtime_text.symbol_rvas(text_section.virtual_address)
	l.patch_entry_argv_bootstrap_data_refs(mut text, text_section.virtual_address, data_rva,
		text_build)!
	l.apply_text_relocations(mut text, text_section.virtual_address, rdata_rva, data_rva,
		runtime_text.bytes.len, runtime_thunks, import_thunks, import_iats)!
	l.apply_runtime_data_patches(mut text, text_section.virtual_address, data_rva, runtime_text,
		runtime_data_base)!
	sections[0].data = text

	size_of_image := pe_size_of_image(sections)
	size_of_code := text_section.raw_size
	mut size_of_initialized_data := u32(0)
	for section in sections {
		if section.characteristics & pe_image_scn_cnt_initialized_data != 0 {
			size_of_initialized_data += section.raw_size
		}
	}

	mut buf := []u8{cap: int(raw_pointer)}
	write_pe_dos_stub(mut buf)
	for buf.len < pe_dos_stub_size {
		buf << 0
	}

	write_u32_le(mut buf, pe_signature)
	write_u16_le(mut buf, coff_image_file_machine_amd64)
	write_u16_le(mut buf, u16(sections.len))
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u16_le(mut buf, pe_size_of_optional_header64)
	// No .reloc section is emitted yet, so mark the image fixed-base.
	write_u16_le(mut buf,
		pe_image_file_relocs_stripped | pe_image_file_executable_image | pe_image_file_large_address_aware)

	l.write_optional_header(mut buf, size_of_code, size_of_initialized_data, size_of_image,
		header_size, idata, text_section.virtual_address)

	for section in sections {
		write_pe_section_header(mut buf, section)
	}

	for buf.len < header_size {
		buf << 0
	}
	for section in sections {
		if section.raw_size == 0 {
			continue
		}
		for buf.len < int(section.raw_pointer) {
			buf << 0
		}
		buf << section.data
		for buf.len < int(section.raw_pointer + section.raw_size) {
			buf << 0
		}
	}

	return buf
}

fn (l PeLinker) build_text_section(runtime_text PeRuntimeText) !PeTextBuild {
	main_rva := l.defined_symbol_offset('main') or {
		return error('PE linker requires a defined main symbol')
	}
	mut text := []u8{}
	vinit_offset := l.defined_symbol_offset('_vinit') or { u32(0xffff_ffff) }
	exit_offset := runtime_text.symbols['exit'] or {
		return error('PE linker internal error: missing runtime exit helper')
	}
	text << [u8(0x48), 0x83, 0xec, 0x28] // sub rsp, 40
	mut get_command_line_field := -1
	mut command_line_to_argv_field := -1
	mut argv_failure_exit_field := -1
	mut argc_global_disp_off := -1
	mut argv_global_disp_off := -1
	if l.needs_windows_argv_bootstrap() {
		text << [u8(0xc7), 0x44, 0x24, 0x20, 0, 0, 0, 0] // mov dword ptr [rsp+32], 0
		get_command_line_field = pe_emit_call_placeholder(mut text)
		text << [u8(0x48), 0x89, 0xc1] // mov rcx, rax
		text << [u8(0x48), 0x8d, 0x54, 0x24, 0x20] // lea rdx, [rsp+32]
		command_line_to_argv_field = pe_emit_call_placeholder(mut text)
		text << [u8(0x48), 0x85, 0xc0] // test rax, rax
		text << [u8(0x75), 0x0a] // jne argv parsed
		text << [u8(0xb9), 1, 0, 0, 0] // mov ecx, 1
		argv_failure_exit_field = pe_emit_call_placeholder(mut text)
		if l.has_main_argv_data_symbol() {
			argv_global_disp_off = pe_emit_lea_r10_rip_placeholder(mut text)
			text << [u8(0x49), 0x89, 0x02] // mov [r10], rax
		}
		if l.has_main_argc_data_symbol() {
			argc_global_disp_off = pe_emit_lea_r10_rip_placeholder(mut text)
			text << [u8(0x8b), 0x4c, 0x24, 0x20] // mov ecx, [rsp+32]
			text << [u8(0x41), 0x89, 0x0a] // mov [r10], ecx
		}
	}
	if vinit_offset != 0xffff_ffff {
		pe_emit_call_placeholder(mut text)
	}
	pe_emit_call_placeholder(mut text)
	text << [u8(0x31), 0xc9] // xor ecx, ecx
	pe_emit_call_placeholder(mut text)
	text << 0xcc

	entry_stub_len := text.len
	if entry_stub_len != l.entry_stub_size() {
		return error('PE linker internal error: entry stub size mismatch (${entry_stub_len} != ${l.entry_stub_size()})')
	}
	text << l.coff.text_data
	text << runtime_text.bytes
	for _ in l.imports {
		text << [u8(0xff), 0x25, 0, 0, 0, 0] // jmp qword ptr [rip + disp32]
	}

	import_offsets := l.import_thunk_offsets(runtime_text.bytes.len)
	if get_command_line_field >= 0 {
		get_command_line_thunk_off := import_offsets[pe_kernel32_import_key('GetCommandLineW')] or {
			return error('PE linker internal error: missing GetCommandLineW import thunk')
		}
		command_line_to_argv_thunk_off := import_offsets[pe_shell32_import_key('CommandLineToArgvW')] or {
			return error('PE linker internal error: missing CommandLineToArgvW import thunk')
		}
		pe_patch_rel32(mut text, get_command_line_field, get_command_line_thunk_off, 0)
		pe_patch_rel32(mut text, command_line_to_argv_field, command_line_to_argv_thunk_off, 0)
		pe_patch_rel32(mut text, argv_failure_exit_field, exit_offset, 0)
	}

	mut cursor := 4 + l.windows_argv_bootstrap_size()
	if vinit_offset != 0xffff_ffff {
		pe_patch_rel32(mut text, cursor + 1, u32(entry_stub_len) + vinit_offset, 0)
		cursor += 5
	}
	pe_patch_rel32(mut text, cursor + 1, u32(entry_stub_len) + main_rva, 0)
	cursor += 5
	cursor += 2
	pe_patch_rel32(mut text, cursor + 1, exit_offset, 0)

	return PeTextBuild{
		data:                 text
		argc_global_disp_off: argc_global_disp_off
		argv_global_disp_off: argv_global_disp_off
	}
}

fn (l PeLinker) build_runtime_text() PeRuntimeText {
	runtime_base := u32(l.entry_stub_size() + l.coff.text_data.len)
	mut rt := PeRuntimeText{
		symbols: map[string]u32{}
	}
	mut run_atexit_offset := -1
	if l.uses_undefined_symbol('atexit') {
		count_off := pe_runtime_data_alloc(mut rt, 8, 8)
		callbacks_off := pe_runtime_data_alloc(mut rt, pe_atexit_callback_capacity * 8, 8)
		run_atexit_offset = rt.bytes.len
		rt.symbols[pe_run_atexit_symbol] = runtime_base + u32(run_atexit_offset)
		pe_emit_runtime_run_atexit(mut rt, count_off, callbacks_off)
		rt.symbols['atexit'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_atexit(mut rt, count_off, callbacks_off)
	}
	if l.uses_undefined_symbol('_aligned_malloc') {
		rt.symbols['_aligned_malloc'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_aligned_malloc(mut rt)
	}
	if l.uses_undefined_symbol('_aligned_free') {
		rt.symbols['_aligned_free'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_aligned_free(mut rt)
	}
	if l.uses_undefined_symbol('_aligned_realloc') {
		rt.symbols['_aligned_realloc'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_aligned_realloc(mut rt)
	}
	needs_memmove := l.uses_undefined_symbol('memmove') || l.uses_undefined_symbol('memcpy')
	if needs_memmove {
		move_off := runtime_base + u32(rt.bytes.len)
		if l.uses_undefined_symbol('memmove') {
			rt.symbols['memmove'] = move_off
		}
		if l.uses_undefined_symbol('memcpy') {
			rt.symbols['memcpy'] = move_off
		}
		pe_emit_runtime_memmove(mut rt)
	}
	if l.uses_undefined_symbol('memset') {
		rt.symbols['memset'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_memset(mut rt)
	}
	if l.uses_undefined_symbol('strlen') {
		rt.symbols['strlen'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_strlen(mut rt)
	}
	if l.uses_undefined_symbol('wcslen') {
		rt.symbols['wcslen'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_wcslen(mut rt)
	}
	if l.uses_undefined_symbol('_wgetcwd') {
		rt.symbols['_wgetcwd'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_wgetcwd(mut rt)
	}
	if l.uses_undefined_symbol('_wgetenv') {
		rt.symbols['_wgetenv'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_wgetenv(mut rt)
	}
	if l.uses_undefined_symbol('_errno') {
		rt.symbols['_errno'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_errno(mut rt)
	}
	if l.uses_undefined_symbol('malloc') {
		rt.symbols['malloc'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_malloc(mut rt)
	}
	if l.uses_undefined_symbol('calloc') {
		rt.symbols['calloc'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_calloc(mut rt)
	}
	if l.uses_undefined_symbol('free') {
		rt.symbols['free'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_free(mut rt)
	}
	if l.uses_undefined_symbol('memcmp') {
		rt.symbols['memcmp'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_memcmp(mut rt)
	}
	rt.symbols['exit'] = runtime_base + u32(rt.bytes.len)
	pe_emit_runtime_exit(mut rt, run_atexit_offset)
	if l.uses_undefined_symbol('builtin__i64__str') {
		rt.symbols['builtin__i64__str'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_i64_str(mut rt)
	}
	if l.uses_undefined_symbol('builtin__string__+') {
		rt.symbols['builtin__string__+'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_string_plus(mut rt)
	}
	if l.uses_undefined_symbol('builtin__Array_rune__string') {
		rt.symbols['builtin__Array_rune__string'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_array_rune_string(mut rt)
	}
	if l.uses_undefined_symbol('builtin__new_array_from_c_array_noscan') {
		rt.symbols['builtin__new_array_from_c_array_noscan'] = runtime_base + u32(rt.bytes.len)
		pe_emit_runtime_new_array_from_c_array_noscan(mut rt)
	}
	if l.uses_undefined_symbol('builtin____new_array_noscan')
		|| l.uses_undefined_symbol('__new_array_noscan') {
		start := runtime_base + u32(rt.bytes.len)
		if l.uses_undefined_symbol('builtin____new_array_noscan') {
			rt.symbols['builtin____new_array_noscan'] = start
		}
		if l.uses_undefined_symbol('__new_array_noscan') {
			rt.symbols['__new_array_noscan'] = start
		}
		pe_emit_runtime_new_array_noscan(mut rt)
	}
	return rt
}

fn (l PeLinker) required_pe_imports(runtime_text PeRuntimeText) ![]PeImport {
	mut required_kernel32 := map[string]bool{}
	mut required_shell32 := map[string]bool{}
	mut required_ucrtbase := map[string]bool{}
	mut required_msvcrt := map[string]bool{}
	pe_require_kernel32_import(mut required_kernel32, 'ExitProcess')!
	if l.needs_windows_argv_bootstrap() {
		pe_require_kernel32_import(mut required_kernel32, 'GetCommandLineW')!
		pe_require_shell32_import(mut required_shell32, 'CommandLineToArgvW')!
	}
	for patch in runtime_text.import_patches {
		if patch.dll == pe_kernel32_dll {
			pe_require_kernel32_import(mut required_kernel32, patch.name)!
		} else if patch.dll == pe_shell32_dll {
			pe_require_shell32_import(mut required_shell32, patch.name)!
		} else {
			return error('PE linker internal error: unknown import DLL `${patch.dll}` for `${patch.name}`')
		}
	}
	for reloc in l.coff.text_relocs {
		if reloc.sym_idx < 0 || reloc.sym_idx >= l.coff.symbols.len {
			continue
		}
		sym := l.coff.symbols[reloc.sym_idx]
		if sym.section != 0 {
			continue
		}
		if _ := runtime_text.symbols[sym.name] {
			continue
		}
		if kernel32_name := pe_kernel32_import_name_for_external_symbol(sym.name) {
			required_kernel32[kernel32_name] = true
		} else if pe_shell32_import_is_known(sym.name) {
			required_shell32[sym.name] = true
		} else if ucrt_name := pe_ucrtbase_import_name_for_external_symbol(sym.name) {
			pe_require_ucrtbase_import(mut required_ucrtbase, ucrt_name)!
		} else if pe_msvcrt_import_is_known(sym.name) {
			pe_require_msvcrt_import(mut required_msvcrt, sym.name)!
		}
	}

	mut imports := []PeImport{cap: required_kernel32.len + required_shell32.len +
		required_ucrtbase.len + required_msvcrt.len}
	for name in pe_kernel32_imports {
		if required_kernel32[name] {
			imports << PeImport{
				dll:  pe_kernel32_dll
				name: name
			}
		}
	}
	for name in pe_shell32_imports {
		if required_shell32[name] {
			imports << PeImport{
				dll:  pe_shell32_dll
				name: name
			}
		}
	}
	for name in pe_ucrtbase_imports {
		if required_ucrtbase[name] {
			imports << PeImport{
				dll:  pe_ucrtbase_dll
				name: name
			}
		}
	}
	for name in pe_msvcrt_imports {
		if required_msvcrt[name] {
			imports << PeImport{
				dll:  pe_msvcrt_dll
				name: name
			}
		}
	}
	return imports
}

fn (l PeLinker) patch_runtime_import_calls(mut rt PeRuntimeText) ! {
	runtime_base := u32(l.entry_stub_size() + l.coff.text_data.len)
	import_offsets := l.import_thunk_offsets(rt.bytes.len)
	for patch in rt.import_patches {
		key := pe_import_key(patch.dll, patch.name)
		target := import_offsets[key] or {
			return error('PE linker internal error: missing import thunk for `${key}`')
		}
		pe_patch_rel32(mut rt.bytes, patch.field_off, target, runtime_base)
	}
}

fn pe_import_key(dll string, name string) string {
	return '${dll}:${name}'
}

fn pe_kernel32_import_key(name string) string {
	return pe_import_key(pe_kernel32_dll, name)
}

fn pe_shell32_import_key(name string) string {
	return pe_import_key(pe_shell32_dll, name)
}

fn pe_ucrtbase_import_key(name string) string {
	return pe_import_key(pe_ucrtbase_dll, name)
}

fn pe_msvcrt_import_key(name string) string {
	return pe_import_key(pe_msvcrt_dll, name)
}

fn pe_require_kernel32_import(mut required map[string]bool, name string) ! {
	if !pe_kernel32_import_is_known(name) {
		return error('PE linker internal error: unknown Kernel32 import `${name}`')
	}
	required[name] = true
}

fn pe_kernel32_import_is_known(name string) bool {
	for known in pe_kernel32_imports {
		if known == name {
			return true
		}
	}
	return false
}

fn pe_kernel32_import_name_for_external_symbol(name string) ?string {
	return match name {
		'ReadConsole' {
			'ReadConsoleW'
		}
		else {
			if pe_kernel32_import_is_known(name) {
				name
			} else {
				none
			}
		}
	}
}

fn pe_require_ucrtbase_import(mut required map[string]bool, name string) ! {
	if !pe_ucrtbase_import_is_known(name) {
		return error('PE linker internal error: unknown UCRT import `${name}`')
	}
	required[name] = true
}

fn pe_ucrtbase_import_is_known(name string) bool {
	for known in pe_ucrtbase_imports {
		if known == name {
			return true
		}
	}
	return false
}

fn pe_ucrtbase_import_name_for_external_symbol(name string) ?string {
	return match name {
		'time' {
			'_time64'
		}
		'localtime' {
			'_localtime64'
		}
		else {
			if pe_ucrtbase_import_is_known(name) {
				name
			} else {
				none
			}
		}
	}
}

fn pe_require_msvcrt_import(mut required map[string]bool, name string) ! {
	if !pe_msvcrt_import_is_known(name) {
		return error('PE linker internal error: unknown MSVCRT import `${name}`')
	}
	required[name] = true
}

fn pe_msvcrt_import_is_known(name string) bool {
	for known in pe_msvcrt_imports {
		if known == name {
			return true
		}
	}
	return false
}

fn pe_require_shell32_import(mut required map[string]bool, name string) ! {
	if !pe_shell32_import_is_known(name) {
		return error('PE linker internal error: unknown Shell32 import `${name}`')
	}
	required[name] = true
}

fn pe_shell32_import_is_known(name string) bool {
	for known in pe_shell32_imports {
		if known == name {
			return true
		}
	}
	return false
}

fn (rt PeRuntimeText) symbol_rvas(text_rva u32) map[string]u32 {
	mut out := map[string]u32{}
	for name, off in rt.symbols {
		out[name] = text_rva + off
	}
	return out
}

fn (l PeLinker) build_idata(idata_rva u32) PeIdata {
	descriptor_size := 20
	groups := pe_import_groups(l.imports)
	descriptor_count := groups.len + 1
	mut cursor := descriptor_size * descriptor_count
	mut ilt_offsets := []int{cap: groups.len}
	for group in groups {
		ilt_offsets << cursor
		cursor += (group.len + 1) * 8
	}
	mut iat_offsets := []int{cap: groups.len}
	for group in groups {
		iat_offsets << cursor
		cursor += (group.len + 1) * 8
	}
	hint_name_off := cursor

	mut data := []u8{len: hint_name_off}
	mut hint_name_rvas := []u32{len: l.imports.len}
	for i, imp in l.imports {
		hint_name_rvas[i] = idata_rva + u32(data.len)
		write_u16_le(mut data, 0)
		data << imp.name.bytes()
		data << 0
		if data.len % 2 != 0 {
			data << 0
		}
	}
	mut dll_name_rvas := []u32{len: groups.len}
	for i, group in groups {
		dll_name_rvas[i] = idata_rva + u32(data.len)
		data << group.dll.bytes()
		data << 0
		if data.len % 2 != 0 {
			data << 0
		}
	}

	mut iat_rvas := map[string]u32{}
	for group_idx, group in groups {
		descriptor_off := group_idx * descriptor_size
		ilt_off := ilt_offsets[group_idx]
		iat_off := iat_offsets[group_idx]
		pe_put_u32_le(mut data, descriptor_off, idata_rva + u32(ilt_off))
		pe_put_u32_le(mut data, descriptor_off + 12, dll_name_rvas[group_idx])
		pe_put_u32_le(mut data, descriptor_off + 16, idata_rva + u32(iat_off))
		for local_i in 0 .. group.len {
			import_idx := group.start + local_i
			rva := hint_name_rvas[import_idx]
			pe_put_u64_le(mut data, ilt_off + local_i * 8, u64(rva))
			pe_put_u64_le(mut data, iat_off + local_i * 8, u64(rva))
			imp := l.imports[import_idx]
			iat_rvas[pe_import_key(imp.dll, imp.name)] = idata_rva + u32(iat_off + local_i * 8)
		}
	}
	iat_rva := if groups.len == 0 { idata_rva } else { idata_rva + u32(iat_offsets[0]) }
	iat_size := if groups.len == 0 {
		u32(0)
	} else {
		last_group := groups[groups.len - 1]
		last_iat_off := iat_offsets[iat_offsets.len - 1]
		u32(last_iat_off + (last_group.len + 1) * 8 - iat_offsets[0])
	}

	return PeIdata{
		data:        data
		import_rva:  idata_rva
		import_size: u32(descriptor_size * descriptor_count)
		iat_rva:     iat_rva
		iat_size:    iat_size
		iat_rvas:    iat_rvas
	}
}

fn pe_import_groups(imports []PeImport) []PeImportGroup {
	mut groups := []PeImportGroup{}
	for i, imp in imports {
		if groups.len == 0 || groups[groups.len - 1].dll != imp.dll {
			groups << PeImportGroup{
				dll:   imp.dll
				start: i
				len:   1
			}
		} else {
			groups[groups.len - 1].len++
		}
	}
	return groups
}

fn (l PeLinker) apply_text_relocations(mut text []u8,
	text_rva u32,
	rdata_rva u32,
	data_rva u32,
	runtime_text_size int,
	runtime_thunks map[string]u32,
	import_thunks map[string]u32,
	import_iats map[string]u32) ! {
	entry_stub_len := l.entry_stub_size()
	for reloc in l.coff.text_relocs {
		if reloc.type_ != coff_image_rel_amd64_rel32 {
			return error('PE linker does not support COFF relocation type 0x${reloc.type_:04x}')
		}
		if reloc.sym_idx < 0 || reloc.sym_idx >= l.coff.symbols.len {
			return error('PE linker relocation references invalid symbol index ${reloc.sym_idx}')
		}
		sym := l.coff.symbols[reloc.sym_idx]
		target_rva := match sym.section {
			1 {
				text_rva + u32(entry_stub_len) + sym.value
			}
			2 {
				if rdata_rva == 0 {
					return error('PE linker relocation references .rdata, but no .rdata section was emitted')
				}
				rdata_rva + sym.value
			}
			3 {
				if data_rva == 0 || l.coff.data_data.len == 0 {
					return error('PE linker relocation references .data, but no .data section was emitted')
				}
				data_rva + sym.value
			}
			0 {
				if runtime_rva := runtime_thunks[sym.name] {
					runtime_rva
				} else {
					import_key := l.import_key_for_external_symbol(sym.name) or {
						return error(l.unresolved_external_symbol_message(sym.name, reloc))
					}
					import_thunks[import_key] or {
						return error(l.unresolved_external_symbol_message(sym.name, reloc))
					}
				}
			}
			else {
				return error('PE linker cannot resolve symbol `${sym.name}` in COFF section ${sym.section}')
			}
		}

		field_off := entry_stub_len + int(reloc.offset)
		old_disp := pe_read_i32_le(text, field_off)
		field_rva := text_rva + u32(field_off)
		new_disp := old_disp + i32(int(target_rva) - (int(field_rva) + 4))
		pe_put_u32_le(mut text, field_off, u32(new_disp))
	}

	thunk_base := l.import_thunk_base(runtime_text_size)
	for i, imp in l.imports {
		thunk_off := thunk_base + i * 6
		import_key := pe_import_key(imp.dll, imp.name)
		iat_entry_rva := import_iats[import_key] or {
			return error('PE linker internal error: missing IAT entry for `${import_key}`')
		}
		thunk_rva := text_rva + u32(thunk_off)
		disp := i32(int(iat_entry_rva) - (int(thunk_rva) + 6))
		pe_put_u32_le(mut text, thunk_off + 2, u32(disp))
	}
}

fn (l PeLinker) apply_runtime_data_patches(mut text []u8,
	text_rva u32,
	data_rva u32,
	runtime_text PeRuntimeText,
	runtime_data_base int) ! {
	if runtime_text.data_patches.len == 0 {
		return
	}
	if data_rva == 0 {
		return error('PE linker runtime data patch requires a .data section')
	}
	runtime_base := l.entry_stub_size() + l.coff.text_data.len
	for patch in runtime_text.data_patches {
		field_off := runtime_base + patch.field_off
		target_rva := data_rva + u32(runtime_data_base) + patch.data_off
		pe_patch_rel32(mut text, field_off, target_rva, text_rva)
	}
}

fn (l PeLinker) unresolved_external_symbol_message(name string, reloc CoffRelocation) string {
	context := l.text_relocation_context(reloc)
	return x64_unresolved_external_symbol_message(.coff, name, context)
}

fn (l PeLinker) text_relocation_context(reloc CoffRelocation) string {
	mut has_symbol := false
	mut nearest_name := ''
	mut nearest_offset := u32(0)
	for sym in l.coff.symbols {
		if sym.section != 1 || sym.name.len == 0 || sym.value > reloc.offset {
			continue
		}
		if !has_symbol || sym.value >= nearest_offset {
			has_symbol = true
			nearest_name = sym.name
			nearest_offset = sym.value
		}
	}
	mut context := 'referenced from .text relocation offset 0x${reloc.offset:08x}'
	if has_symbol {
		context += ' near `${nearest_name}`+0x${reloc.offset - nearest_offset:08x}'
	}
	return context
}

fn (l PeLinker) import_thunk_base(runtime_text_size int) int {
	return l.entry_stub_size() + l.coff.text_data.len + runtime_text_size
}

fn (l PeLinker) import_thunk_offsets(runtime_text_size int) map[string]u32 {
	thunk_base := l.import_thunk_base(runtime_text_size)
	mut out := map[string]u32{}
	for i, imp in l.imports {
		out[pe_import_key(imp.dll, imp.name)] = u32(thunk_base + i * 6)
	}
	return out
}

fn (l PeLinker) import_thunk_rvas(text_rva u32, runtime_text_size int) map[string]u32 {
	offsets := l.import_thunk_offsets(runtime_text_size)
	mut out := map[string]u32{}
	for name, off in offsets {
		out[name] = text_rva + off
	}
	return out
}

fn (l PeLinker) import_key_for_external_symbol(name string) ?string {
	if kernel32_name := pe_kernel32_import_name_for_external_symbol(name) {
		return pe_kernel32_import_key(kernel32_name)
	}
	if pe_shell32_import_is_known(name) {
		return pe_shell32_import_key(name)
	}
	if ucrt_name := pe_ucrtbase_import_name_for_external_symbol(name) {
		return pe_ucrtbase_import_key(ucrt_name)
	}
	if pe_msvcrt_import_is_known(name) {
		return pe_msvcrt_import_key(name)
	}
	return none
}

fn (l PeLinker) entry_stub_size() int {
	vinit := l.defined_symbol_offset('_vinit') or { u32(0xffff_ffff) }
	base_size := if vinit == 0xffff_ffff { 17 } else { 22 }
	return base_size + l.windows_argv_bootstrap_size()
}

fn (l PeLinker) windows_argv_bootstrap_size() int {
	if !l.needs_windows_argv_bootstrap() {
		return 0
	}
	mut size := 8 + 5 + 3 + 5 + 5 + 15 // zero argc, argv calls, null check, ExitProcess(1)
	if l.has_main_argv_data_symbol() {
		size += 7 + 3 // lea g_main_argv; mov [g_main_argv], rax
	}
	if l.has_main_argc_data_symbol() {
		size += 7 + 4 + 3 // lea g_main_argc; load local argc; mov [g_main_argc], ecx
	}
	return size
}

fn (l PeLinker) defined_symbol_offset(name string) ?u32 {
	for sym in l.coff.symbols {
		if sym.name == name && sym.section == 1 {
			return sym.value
		}
	}
	return none
}

fn (l PeLinker) uses_undefined_symbol(name string) bool {
	for reloc in l.coff.text_relocs {
		if reloc.sym_idx < 0 || reloc.sym_idx >= l.coff.symbols.len {
			continue
		}
		sym := l.coff.symbols[reloc.sym_idx]
		if sym.section == 0 && sym.name == name {
			return true
		}
	}
	return false
}

fn (l PeLinker) needs_windows_argv_bootstrap() bool {
	for reloc in l.coff.text_relocs {
		if reloc.sym_idx < 0 || reloc.sym_idx >= l.coff.symbols.len {
			continue
		}
		sym := l.coff.symbols[reloc.sym_idx]
		if sym.section == 3
			&& (x64_main_argc_global_name(sym.name) || x64_main_argv_global_name(sym.name)) {
			return true
		}
	}
	return false
}

fn (l PeLinker) has_main_argc_data_symbol() bool {
	if _ := l.main_argc_data_offset() {
		return true
	}
	return false
}

fn (l PeLinker) has_main_argv_data_symbol() bool {
	if _ := l.main_argv_data_offset() {
		return true
	}
	return false
}

fn (l PeLinker) main_argc_data_offset() ?u32 {
	for sym in l.coff.symbols {
		if sym.section == 3 && x64_main_argc_global_name(sym.name) {
			return sym.value
		}
	}
	return none
}

fn (l PeLinker) main_argv_data_offset() ?u32 {
	for sym in l.coff.symbols {
		if sym.section == 3 && x64_main_argv_global_name(sym.name) {
			return sym.value
		}
	}
	return none
}

fn (l PeLinker) patch_entry_argv_bootstrap_data_refs(mut text []u8, text_rva u32, data_rva u32, text_build PeTextBuild) ! {
	if text_build.argc_global_disp_off < 0 && text_build.argv_global_disp_off < 0 {
		return
	}
	if data_rva == 0 {
		return error('PE linker Windows argv bootstrap requires a .data section for g_main_argc/g_main_argv')
	}
	if text_build.argc_global_disp_off >= 0 {
		argc_off := l.main_argc_data_offset() or {
			return error('PE linker Windows argv bootstrap requires g_main_argc data symbol')
		}
		pe_patch_rel32(mut text, text_build.argc_global_disp_off, data_rva + argc_off, text_rva)
	}
	if text_build.argv_global_disp_off >= 0 {
		argv_off := l.main_argv_data_offset() or {
			return error('PE linker Windows argv bootstrap requires g_main_argv data symbol')
		}
		pe_patch_rel32(mut text, text_build.argv_global_disp_off, data_rva + argv_off, text_rva)
	}
}

fn (mut l PeLinker) write_optional_header(mut buf []u8, size_of_code u32, size_of_initialized_data u32, size_of_image u32, size_of_headers int, idata PeIdata, entry_rva u32) {
	write_u16_le(mut buf, pe_optional_header64_magic)
	buf << pe_linker_major_version
	buf << pe_linker_minor_version
	write_u32_le(mut buf, size_of_code)
	write_u32_le(mut buf, size_of_initialized_data)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, entry_rva)
	write_u32_le(mut buf, entry_rva)
	write_u64_le(mut buf, pe_image_base)
	write_u32_le(mut buf, pe_section_alignment)
	write_u32_le(mut buf, pe_file_alignment)
	write_u16_le(mut buf, pe_major_operating_system_version)
	write_u16_le(mut buf, pe_minor_operating_system_version)
	write_u16_le(mut buf, 0)
	write_u16_le(mut buf, 0)
	write_u16_le(mut buf, pe_major_subsystem_version)
	write_u16_le(mut buf, pe_minor_subsystem_version)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, size_of_image)
	write_u32_le(mut buf, u32(size_of_headers))
	write_u32_le(mut buf, 0)
	write_u16_le(mut buf, pe_image_subsystem_windows_cui)
	write_u16_le(mut buf, pe_dll_characteristics_nx_compat)
	write_u64_le(mut buf, pe_size_of_stack_reserve)
	write_u64_le(mut buf, pe_size_of_stack_commit)
	write_u64_le(mut buf, pe_size_of_heap_reserve)
	write_u64_le(mut buf, pe_size_of_heap_commit)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, pe_number_of_rva_and_sizes)

	for i in 0 .. pe_number_of_rva_and_sizes {
		if i == pe_import_directory_index {
			write_u32_le(mut buf, idata.import_rva)
			write_u32_le(mut buf, idata.import_size)
		} else if i == pe_exception_directory_index || i == pe_base_reloc_directory_index {
			// Unwind and base relocation directories are not emitted yet.
			write_u32_le(mut buf, 0)
			write_u32_le(mut buf, 0)
		} else if i == pe_iat_directory_index {
			write_u32_le(mut buf, idata.iat_rva)
			write_u32_le(mut buf, idata.iat_size)
		} else {
			write_u32_le(mut buf, 0)
			write_u32_le(mut buf, 0)
		}
	}
}

fn write_pe_dos_stub(mut buf []u8) {
	buf << [u8(`M`), `Z`]
	for buf.len < 0x3c {
		buf << 0
	}
	write_u32_le(mut buf, pe_dos_stub_size)
}

fn write_pe_section_header(mut buf []u8, section PeSection) {
	write_fixed_string(mut buf, section.name, 8)
	write_u32_le(mut buf, section.virtual_size)
	write_u32_le(mut buf, section.virtual_address)
	write_u32_le(mut buf, section.raw_size)
	write_u32_le(mut buf, section.raw_pointer)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u16_le(mut buf, 0)
	write_u16_le(mut buf, 0)
	write_u32_le(mut buf, section.characteristics)
}

fn pe_make_section(name string, data []u8, virtual_address u32, raw_pointer int, characteristics u32) (PeSection, int) {
	raw_size := if data.len == 0 { 0 } else { align_int(data.len, pe_file_alignment) }
	raw_ptr := if raw_size == 0 { 0 } else { raw_pointer }
	mut next_raw_pointer := raw_pointer
	if raw_size > 0 {
		next_raw_pointer += raw_size
	}
	return PeSection{
		name:            name
		data:            data.clone()
		virtual_address: virtual_address
		virtual_size:    u32(data.len)
		raw_pointer:     u32(raw_ptr)
		raw_size:        u32(raw_size)
		characteristics: characteristics
	}, next_raw_pointer
}

fn pe_headers_size(section_count int) int {
	return align_int(pe_dos_stub_size + 4 + 20 + int(pe_size_of_optional_header64) +
		section_count * 40, pe_file_alignment)
}

fn pe_size_of_image(sections []PeSection) u32 {
	mut end := 0
	for section in sections {
		virtual_size := if section.virtual_size == 0 { 1 } else { int(section.virtual_size) }
		section_end := int(section.virtual_address) + align_int(virtual_size, pe_section_alignment)
		if section_end > end {
			end = section_end
		}
	}
	return u32(align_int(end, pe_section_alignment))
}

fn pe_next_section_rva(rva u32, size int) u32 {
	virtual_size := if size == 0 { 1 } else { size }
	return u32(align_int(int(rva) + virtual_size, pe_section_alignment))
}

fn pe_check_written_image(path string, expected_size int) ! {
	if !os.is_file(path) {
		return error('PE linker write reported success, but `${path}` was not created as a file')
	}
	size := os.file_size(path)
	if size != u64(expected_size) {
		return error('PE linker write reported success, but `${path}` has size ${size} bytes (expected ${expected_size} bytes)')
	}
}

fn pe_emit_call_placeholder(mut text []u8) int {
	text << u8(0xe8)
	field_off := text.len
	text << [u8(0), 0, 0, 0]
	return field_off
}

fn pe_emit_lea_r10_rip_placeholder(mut text []u8) int {
	text << [u8(0x4c), 0x8d, 0x15] // lea r10, [rip + disp32]
	field_off := text.len
	text << [u8(0), 0, 0, 0]
	return field_off
}

fn pe_emit_runtime_call_import(mut rt PeRuntimeText, import_name string) {
	rt.bytes << u8(0xe8)
	rt.import_patches << PeRuntimeCallPatch{
		field_off: rt.bytes.len
		dll:       pe_kernel32_dll
		name:      import_name
	}
	rt.bytes << [u8(0), 0, 0, 0]
}

fn pe_runtime_data_alloc(mut rt PeRuntimeText, size int, align int) u32 {
	aligned := align_int(rt.data.len, align)
	for rt.data.len < aligned {
		rt.data << u8(0)
	}
	off := rt.data.len
	for _ in 0 .. size {
		rt.data << u8(0)
	}
	return u32(off)
}

fn pe_emit_runtime_lea_rax_data(mut rt PeRuntimeText, data_off u32) {
	rt.bytes << [u8(0x48), 0x8d, 0x05] // lea rax, [rip + disp32]
	rt.data_patches << PeRuntimeDataPatch{
		field_off: rt.bytes.len
		data_off:  data_off
	}
	rt.bytes << [u8(0), 0, 0, 0]
}

fn pe_emit_runtime_lea_rdx_data(mut rt PeRuntimeText, data_off u32) {
	rt.bytes << [u8(0x48), 0x8d, 0x15] // lea rdx, [rip + disp32]
	rt.data_patches << PeRuntimeDataPatch{
		field_off: rt.bytes.len
		data_off:  data_off
	}
	rt.bytes << [u8(0), 0, 0, 0]
}

fn pe_emit_runtime_lea_r8_data(mut rt PeRuntimeText, data_off u32) {
	rt.bytes << [u8(0x4c), 0x8d, 0x05] // lea r8, [rip + disp32]
	rt.data_patches << PeRuntimeDataPatch{
		field_off: rt.bytes.len
		data_off:  data_off
	}
	rt.bytes << [u8(0), 0, 0, 0]
}

fn pe_emit_jcc8(mut data []u8, opcode u8) int {
	data << opcode
	field_off := data.len
	data << u8(0)
	return field_off
}

fn pe_emit_jcc32(mut data []u8, opcode u8) int {
	data << [u8(0x0f), opcode]
	field_off := data.len
	data << [u8(0), 0, 0, 0]
	return field_off
}

fn pe_emit_jmp32(mut data []u8) int {
	data << u8(0xe9)
	field_off := data.len
	data << [u8(0), 0, 0, 0]
	return field_off
}

fn pe_patch_rel32(mut text []u8, field_off int, target_off u32, text_rva u32) {
	field_rva := text_rva + u32(field_off)
	disp := i32(int(target_off) - (int(field_rva) + 4))
	pe_put_u32_le(mut text, field_off, u32(disp))
}

fn pe_patch_rel32_local(mut data []u8, field_off int, target_off int) {
	pe_patch_rel32(mut data, field_off, u32(target_off), 0)
}

fn pe_patch_rel8(mut data []u8, field_off int, target_off int) {
	disp := target_off - (field_off + 1)
	data[field_off] = u8(disp & 0xff)
}

fn pe_emit_runtime_aligned_malloc(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x48), 0x83, 0xec, 0x38] // sub rsp, 56
	rt.bytes << [u8(0x48), 0x83, 0xfa, 0x10] // cmp rdx, 16
	align_check := pe_emit_jcc8(mut rt.bytes, 0x77) // ja
	rt.bytes << [u8(0xba), 0x10, 0, 0, 0] // mov edx, 16
	align_ready_jump := pe_emit_jcc8(mut rt.bytes, 0xeb) // jmp
	align_check_target := rt.bytes.len
	rt.bytes << [u8(0x49), 0x89, 0xd2] // mov r10, rdx
	rt.bytes << [u8(0x49), 0xff, 0xca] // dec r10
	rt.bytes << [u8(0x4c), 0x85, 0xd2] // test rdx, r10
	invalid_align := pe_emit_jcc8(mut rt.bytes, 0x75) // jne
	align_ready := rt.bytes.len
	rt.bytes << [u8(0x48), 0x89, 0x4c, 0x24, 0x20] // mov [rsp+32], rcx
	rt.bytes << [u8(0x48), 0x89, 0x54, 0x24, 0x28] // mov [rsp+40], rdx
	pe_emit_runtime_call_import(mut rt, 'GetProcessHeap')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	no_heap := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x48), 0x89, 0xc1] // mov rcx, rax
	rt.bytes << [u8(0x31), 0xd2] // xor edx, edx
	rt.bytes << [u8(0x4c), 0x8b, 0x44, 0x24, 0x20] // mov r8, [rsp+32]
	rt.bytes << [u8(0x4c), 0x8b, 0x4c, 0x24, 0x28] // mov r9, [rsp+40]
	rt.bytes << [u8(0x4d), 0x01, 0xc8] // add r8, r9
	size_overflow := pe_emit_jcc8(mut rt.bytes, 0x72) // jc
	rt.bytes << [u8(0x49), 0x83, 0xc0, 0x08] // add r8, 8
	padding_overflow := pe_emit_jcc8(mut rt.bytes, 0x72) // jc
	pe_emit_runtime_call_import(mut rt, 'HeapAlloc')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	alloc_failed := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x4c), 0x8b, 0x54, 0x24, 0x28] // mov r10, [rsp+40]
	rt.bytes << [u8(0x49), 0x89, 0xc3] // mov r11, rax
	rt.bytes << [u8(0x49), 0x83, 0xc3, 0x08] // add r11, 8
	rt.bytes << [u8(0x4d), 0x01, 0xd3] // add r11, r10
	rt.bytes << [u8(0x49), 0xff, 0xcb] // dec r11
	rt.bytes << [u8(0x4c), 0x89, 0xd1] // mov rcx, r10
	rt.bytes << [u8(0x48), 0xf7, 0xd9] // neg rcx
	rt.bytes << [u8(0x49), 0x21, 0xcb] // and r11, rcx
	rt.bytes << [u8(0x49), 0x89, 0x43, 0xf8] // mov [r11-8], rax
	rt.bytes << [u8(0x4c), 0x89, 0xd8] // mov rax, r11
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x38] // add rsp, 56
	rt.bytes << u8(0xc3) // ret
	fail := rt.bytes.len
	rt.bytes << [u8(0x31), 0xc0] // xor eax, eax
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x38] // add rsp, 56
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel8(mut rt.bytes, align_check, align_check_target)
	pe_patch_rel8(mut rt.bytes, align_ready_jump, align_ready)
	pe_patch_rel8(mut rt.bytes, invalid_align, fail)
	pe_patch_rel8(mut rt.bytes, no_heap, fail)
	pe_patch_rel8(mut rt.bytes, size_overflow, fail)
	pe_patch_rel8(mut rt.bytes, padding_overflow, fail)
	pe_patch_rel8(mut rt.bytes, alloc_failed, fail)
}

fn pe_emit_runtime_aligned_free(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x48), 0x85, 0xc9] // test rcx, rcx
	null_ptr := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x48), 0x83, 0xec, 0x28] // sub rsp, 40
	rt.bytes << [u8(0x48), 0x8b, 0x41, 0xf8] // mov rax, [rcx-8]
	rt.bytes << [u8(0x48), 0x89, 0x44, 0x24, 0x20] // mov [rsp+32], rax
	pe_emit_runtime_call_import(mut rt, 'GetProcessHeap')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	no_heap := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x48), 0x89, 0xc1] // mov rcx, rax
	rt.bytes << [u8(0x31), 0xd2] // xor edx, edx
	rt.bytes << [u8(0x4c), 0x8b, 0x44, 0x24, 0x20] // mov r8, [rsp+32]
	pe_emit_runtime_call_import(mut rt, 'HeapFree')
	cleanup := rt.bytes.len
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x28] // add rsp, 40
	done := rt.bytes.len
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel8(mut rt.bytes, null_ptr, done)
	pe_patch_rel8(mut rt.bytes, no_heap, cleanup)
}

fn pe_emit_runtime_aligned_realloc(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x49), 0x83, 0xf8, 0x10] // cmp r8, 16
	supported_align := pe_emit_jcc8(mut rt.bytes, 0x76) // jbe
	rt.bytes << [u8(0x31), 0xc0] // xor eax, eax
	rt.bytes << u8(0xc3) // ret
	supported_align_target := rt.bytes.len
	rt.bytes << [u8(0x48), 0x85, 0xc9] // test rcx, rcx
	null_ptr := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x48), 0x83, 0xec, 0x38] // sub rsp, 56
	rt.bytes << [u8(0x48), 0x89, 0x54, 0x24, 0x20] // mov [rsp+32], rdx
	rt.bytes << [u8(0x4c), 0x8b, 0x51, 0xf8] // mov r10, [rcx-8]
	rt.bytes << [u8(0x4c), 0x89, 0x54, 0x24, 0x28] // mov [rsp+40], r10
	pe_emit_runtime_call_import(mut rt, 'GetProcessHeap')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	no_heap := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x48), 0x89, 0xc1] // mov rcx, rax
	rt.bytes << [u8(0x31), 0xd2] // xor edx, edx
	rt.bytes << [u8(0x4c), 0x8b, 0x44, 0x24, 0x28] // mov r8, [rsp+40]
	rt.bytes << [u8(0x4c), 0x8b, 0x4c, 0x24, 0x20] // mov r9, [rsp+32]
	rt.bytes << [u8(0x49), 0x83, 0xc1, 0x18] // add r9, 24
	size_overflow := pe_emit_jcc8(mut rt.bytes, 0x72) // jc
	pe_emit_runtime_call_import(mut rt, 'HeapReAlloc')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	realloc_failed := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x49), 0x89, 0xc2] // mov r10, rax
	rt.bytes << [u8(0x49), 0x83, 0xc2, 0x10] // add r10, 16
	rt.bytes << [u8(0x49), 0x89, 0x42, 0xf8] // mov [r10-8], rax
	rt.bytes << [u8(0x4c), 0x89, 0xd0] // mov rax, r10
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x38] // add rsp, 56
	rt.bytes << u8(0xc3) // ret
	fail_with_frame := rt.bytes.len
	rt.bytes << [u8(0x31), 0xc0] // xor eax, eax
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x38] // add rsp, 56
	rt.bytes << u8(0xc3) // ret
	null_alloc := rt.bytes.len
	rt.bytes << [u8(0x48), 0x83, 0xec, 0x28] // sub rsp, 40
	rt.bytes << [u8(0x48), 0x89, 0x54, 0x24, 0x20] // mov [rsp+32], rdx
	pe_emit_runtime_call_import(mut rt, 'GetProcessHeap')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	no_heap_null := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x48), 0x89, 0xc1] // mov rcx, rax
	rt.bytes << [u8(0x31), 0xd2] // xor edx, edx
	rt.bytes << [u8(0x4c), 0x8b, 0x44, 0x24, 0x20] // mov r8, [rsp+32]
	rt.bytes << [u8(0x49), 0x83, 0xc0, 0x18] // add r8, 24
	null_size_overflow := pe_emit_jcc8(mut rt.bytes, 0x72) // jc
	pe_emit_runtime_call_import(mut rt, 'HeapAlloc')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	alloc_failed := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x49), 0x89, 0xc2] // mov r10, rax
	rt.bytes << [u8(0x49), 0x83, 0xc2, 0x10] // add r10, 16
	rt.bytes << [u8(0x49), 0x89, 0x42, 0xf8] // mov [r10-8], rax
	rt.bytes << [u8(0x4c), 0x89, 0xd0] // mov rax, r10
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x28] // add rsp, 40
	rt.bytes << u8(0xc3) // ret
	fail_with_null_frame := rt.bytes.len
	rt.bytes << [u8(0x31), 0xc0] // xor eax, eax
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x28] // add rsp, 40
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel8(mut rt.bytes, supported_align, supported_align_target)
	pe_patch_rel8(mut rt.bytes, null_ptr, null_alloc)
	pe_patch_rel8(mut rt.bytes, no_heap, fail_with_frame)
	pe_patch_rel8(mut rt.bytes, size_overflow, fail_with_frame)
	pe_patch_rel8(mut rt.bytes, realloc_failed, fail_with_frame)
	pe_patch_rel8(mut rt.bytes, no_heap_null, fail_with_null_frame)
	pe_patch_rel8(mut rt.bytes, null_size_overflow, fail_with_null_frame)
	pe_patch_rel8(mut rt.bytes, alloc_failed, fail_with_null_frame)
}

fn pe_emit_runtime_align_heap_allocated_data(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x49), 0x89, 0xc3] // mov r11, rax
	rt.bytes << [u8(0x49), 0x83, 0xc3, 0x17] // add r11, 23
	rt.bytes << [u8(0x49), 0x83, 0xe3, 0xf0] // and r11, -16
	rt.bytes << [u8(0x49), 0x89, 0x43, 0xf8] // mov [r11-8], rax
}

fn pe_emit_runtime_memmove(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x48), 0x89, 0xc8] // mov rax, rcx
	rt.bytes << [u8(0x4d), 0x85, 0xc0] // test r8, r8
	zero_len := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x48), 0x39, 0xd1] // cmp rcx, rdx
	dest_before_src := pe_emit_jcc8(mut rt.bytes, 0x76) // jbe
	rt.bytes << [u8(0x4e), 0x8d, 0x0c, 0x02] // lea r9, [rdx+r8]
	rt.bytes << [u8(0x4c), 0x39, 0xc9] // cmp rcx, r9
	no_overlap := pe_emit_jcc8(mut rt.bytes, 0x73) // jae
	rt.bytes << [u8(0x4e), 0x8d, 0x54, 0x01, 0xff] // lea r10, [rcx+r8-1]
	rt.bytes << [u8(0x4e), 0x8d, 0x5c, 0x02, 0xff] // lea r11, [rdx+r8-1]
	backward_loop := rt.bytes.len
	rt.bytes << [u8(0x45), 0x8a, 0x0b] // mov r9b, [r11]
	rt.bytes << [u8(0x45), 0x88, 0x0a] // mov [r10], r9b
	rt.bytes << [u8(0x49), 0xff, 0xca] // dec r10
	rt.bytes << [u8(0x49), 0xff, 0xcb] // dec r11
	rt.bytes << [u8(0x49), 0xff, 0xc8] // dec r8
	backward_more := pe_emit_jcc8(mut rt.bytes, 0x75) // jne
	rt.bytes << u8(0xc3) // ret
	forward := rt.bytes.len
	rt.bytes << [u8(0x49), 0x89, 0xca] // mov r10, rcx
	rt.bytes << [u8(0x49), 0x89, 0xd3] // mov r11, rdx
	forward_loop := rt.bytes.len
	rt.bytes << [u8(0x45), 0x8a, 0x0b] // mov r9b, [r11]
	rt.bytes << [u8(0x45), 0x88, 0x0a] // mov [r10], r9b
	rt.bytes << [u8(0x49), 0xff, 0xc2] // inc r10
	rt.bytes << [u8(0x49), 0xff, 0xc3] // inc r11
	rt.bytes << [u8(0x49), 0xff, 0xc8] // dec r8
	forward_more := pe_emit_jcc8(mut rt.bytes, 0x75) // jne
	done := rt.bytes.len
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel8(mut rt.bytes, zero_len, done)
	pe_patch_rel8(mut rt.bytes, dest_before_src, forward)
	pe_patch_rel8(mut rt.bytes, no_overlap, forward)
	pe_patch_rel8(mut rt.bytes, backward_more, backward_loop)
	pe_patch_rel8(mut rt.bytes, forward_more, forward_loop)
}

fn pe_emit_runtime_memset(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x48), 0x89, 0xc8] // mov rax, rcx
	rt.bytes << [u8(0x4d), 0x85, 0xc0] // test r8, r8
	zero_len := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x49), 0x89, 0xca] // mov r10, rcx
	loop_start := rt.bytes.len
	rt.bytes << [u8(0x41), 0x88, 0x12] // mov [r10], dl
	rt.bytes << [u8(0x49), 0xff, 0xc2] // inc r10
	rt.bytes << [u8(0x49), 0xff, 0xc8] // dec r8
	more := pe_emit_jcc8(mut rt.bytes, 0x75) // jne
	done := rt.bytes.len
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel8(mut rt.bytes, zero_len, done)
	pe_patch_rel8(mut rt.bytes, more, loop_start)
}

fn pe_emit_runtime_strlen(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x48), 0x89, 0xc8] // mov rax, rcx
	loop_start := rt.bytes.len
	rt.bytes << [u8(0x80), 0x38, 0x00] // cmp byte ptr [rax], 0
	done := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x48), 0xff, 0xc0] // inc rax
	more := pe_emit_jcc8(mut rt.bytes, 0xeb) // jmp
	done_target := rt.bytes.len
	rt.bytes << [u8(0x48), 0x29, 0xc8] // sub rax, rcx
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel8(mut rt.bytes, done, done_target)
	pe_patch_rel8(mut rt.bytes, more, loop_start)
}

fn pe_emit_runtime_wcslen(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x48), 0x89, 0xc8] // mov rax, rcx
	loop_start := rt.bytes.len
	rt.bytes << [u8(0x66), 0x83, 0x38, 0x00] // cmp word ptr [rax], 0
	done := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x48), 0x83, 0xc0, 0x02] // add rax, 2
	more := pe_emit_jcc8(mut rt.bytes, 0xeb) // jmp
	done_target := rt.bytes.len
	rt.bytes << [u8(0x48), 0x29, 0xc8] // sub rax, rcx
	rt.bytes << [u8(0x48), 0xd1, 0xe8] // shr rax, 1
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel8(mut rt.bytes, done, done_target)
	pe_patch_rel8(mut rt.bytes, more, loop_start)
}

fn pe_emit_runtime_wgetcwd(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x48), 0x83, 0xec, 0x38] // sub rsp, 56
	rt.bytes << [u8(0x48), 0x89, 0x4c, 0x24, 0x20] // mov [rsp+32], rcx
	rt.bytes << [u8(0x89), 0x54, 0x24, 0x28] // mov [rsp+40], edx
	rt.bytes << [u8(0x89), 0xd1] // mov ecx, edx
	rt.bytes << [u8(0x48), 0x8b, 0x54, 0x24, 0x20] // mov rdx, [rsp+32]
	pe_emit_runtime_call_import(mut rt, 'GetCurrentDirectoryW')
	rt.bytes << [u8(0x85), 0xc0] // test eax, eax
	failed := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x3b), 0x44, 0x24, 0x28] // cmp eax, [rsp+40]
	too_small := pe_emit_jcc8(mut rt.bytes, 0x73) // jae
	rt.bytes << [u8(0x48), 0x8b, 0x44, 0x24, 0x20] // mov rax, [rsp+32]
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x38] // add rsp, 56
	rt.bytes << u8(0xc3) // ret
	fail_target := rt.bytes.len
	rt.bytes << [u8(0x31), 0xc0] // xor eax, eax
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x38] // add rsp, 56
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel8(mut rt.bytes, failed, fail_target)
	pe_patch_rel8(mut rt.bytes, too_small, fail_target)
}

fn pe_emit_runtime_wgetenv(mut rt PeRuntimeText) {
	buffer_off := pe_runtime_data_alloc(mut rt, pe_wgetenv_buffer_bytes, pe_runtime_data_alignment)
	rt.bytes << [u8(0x48), 0x83, 0xec, 0x28] // sub rsp, 40
	rt.bytes << [u8(0x48), 0x85, 0xc9] // test rcx, rcx
	null_name := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	pe_emit_runtime_lea_rdx_data(mut rt, buffer_off)
	rt.bytes << [u8(0x41), 0xb8] // mov r8d, pe_wgetenv_buffer_wchars
	write_u32_le(mut rt.bytes, u32(pe_wgetenv_buffer_wchars))
	pe_emit_runtime_call_import(mut rt, 'GetEnvironmentVariableW')
	rt.bytes << [u8(0x85), 0xc0] // test eax, eax
	missing := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << u8(0x3d) // cmp eax, pe_wgetenv_buffer_wchars
	write_u32_le(mut rt.bytes, u32(pe_wgetenv_buffer_wchars))
	too_small := pe_emit_jcc8(mut rt.bytes, 0x73) // jae
	pe_emit_runtime_lea_rax_data(mut rt, buffer_off)
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x28] // add rsp, 40
	rt.bytes << u8(0xc3) // ret
	fail_target := rt.bytes.len
	rt.bytes << [u8(0x31), 0xc0] // xor eax, eax
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x28] // add rsp, 40
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel8(mut rt.bytes, null_name, fail_target)
	pe_patch_rel8(mut rt.bytes, missing, fail_target)
	pe_patch_rel8(mut rt.bytes, too_small, fail_target)
}

fn pe_emit_runtime_calloc(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x48), 0x89, 0xc8] // mov rax, rcx
	rt.bytes << [u8(0x48), 0xf7, 0xe2] // mul rdx
	rt.bytes << [u8(0x48), 0x85, 0xd2] // test rdx, rdx
	overflow := pe_emit_jcc8(mut rt.bytes, 0x75) // jne
	rt.bytes << [u8(0x48), 0x83, 0xec, 0x28] // sub rsp, 40
	rt.bytes << [u8(0x48), 0x89, 0x44, 0x24, 0x20] // mov [rsp+32], rax
	pe_emit_runtime_call_import(mut rt, 'GetProcessHeap')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	no_heap := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x48), 0x89, 0xc1] // mov rcx, rax
	rt.bytes << [u8(0xba), 0x08, 0, 0, 0] // mov edx, HEAP_ZERO_MEMORY
	rt.bytes << [u8(0x4c), 0x8b, 0x44, 0x24, 0x20] // mov r8, [rsp+32]
	rt.bytes << [u8(0x49), 0x83, 0xc0, 0x18] // add r8, 24
	size_overflow := pe_emit_jcc8(mut rt.bytes, 0x72) // jc
	pe_emit_runtime_call_import(mut rt, 'HeapAlloc')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	alloc_failed := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	pe_emit_runtime_align_heap_allocated_data(mut rt)
	rt.bytes << [u8(0x4c), 0x89, 0xd8] // mov rax, r11
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x28] // add rsp, 40
	rt.bytes << u8(0xc3) // ret
	fail_with_frame := rt.bytes.len
	rt.bytes << [u8(0x31), 0xc0] // xor eax, eax
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x28] // add rsp, 40
	rt.bytes << u8(0xc3) // ret
	fail_without_frame := rt.bytes.len
	rt.bytes << [u8(0x31), 0xc0] // xor eax, eax
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel8(mut rt.bytes, overflow, fail_without_frame)
	pe_patch_rel8(mut rt.bytes, no_heap, fail_with_frame)
	pe_patch_rel8(mut rt.bytes, size_overflow, fail_with_frame)
	pe_patch_rel8(mut rt.bytes, alloc_failed, fail_with_frame)
}

fn pe_emit_runtime_malloc(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x48), 0x83, 0xec, 0x28] // sub rsp, 40
	rt.bytes << [u8(0x48), 0x89, 0x4c, 0x24, 0x20] // mov [rsp+32], rcx
	pe_emit_runtime_call_import(mut rt, 'GetProcessHeap')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	no_heap := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x48), 0x89, 0xc1] // mov rcx, rax
	rt.bytes << [u8(0x31), 0xd2] // xor edx, edx
	rt.bytes << [u8(0x4c), 0x8b, 0x44, 0x24, 0x20] // mov r8, [rsp+32]
	rt.bytes << [u8(0x49), 0x83, 0xc0, 0x18] // add r8, 24
	size_overflow := pe_emit_jcc8(mut rt.bytes, 0x72) // jc
	pe_emit_runtime_call_import(mut rt, 'HeapAlloc')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	alloc_failed := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	pe_emit_runtime_align_heap_allocated_data(mut rt)
	rt.bytes << [u8(0x4c), 0x89, 0xd8] // mov rax, r11
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x28] // add rsp, 40
	rt.bytes << u8(0xc3) // ret
	fail := rt.bytes.len
	rt.bytes << [u8(0x31), 0xc0] // xor eax, eax
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x28] // add rsp, 40
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel8(mut rt.bytes, no_heap, fail)
	pe_patch_rel8(mut rt.bytes, size_overflow, fail)
	pe_patch_rel8(mut rt.bytes, alloc_failed, fail)
}

fn pe_emit_runtime_free(mut rt PeRuntimeText) {
	pe_emit_runtime_aligned_free(mut rt)
}

fn pe_emit_runtime_memcmp(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x4d), 0x85, 0xc0] // test r8, r8
	zero_len := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	loop_start := rt.bytes.len
	rt.bytes << [u8(0x44), 0x8a, 0x09] // mov r9b, [rcx]
	rt.bytes << [u8(0x44), 0x8a, 0x12] // mov r10b, [rdx]
	rt.bytes << [u8(0x45), 0x38, 0xd1] // cmp r9b, r10b
	diff := pe_emit_jcc8(mut rt.bytes, 0x75) // jne
	rt.bytes << [u8(0x48), 0xff, 0xc1] // inc rcx
	rt.bytes << [u8(0x48), 0xff, 0xc2] // inc rdx
	rt.bytes << [u8(0x49), 0xff, 0xc8] // dec r8
	more := pe_emit_jcc8(mut rt.bytes, 0x75) // jne
	done_equal := rt.bytes.len
	rt.bytes << [u8(0x31), 0xc0] // xor eax, eax
	rt.bytes << u8(0xc3) // ret
	diff_target := rt.bytes.len
	rt.bytes << [u8(0x41), 0x0f, 0xb6, 0xc1] // movzx eax, r9b
	rt.bytes << [u8(0x45), 0x0f, 0xb6, 0xd2] // movzx r10d, r10b
	rt.bytes << [u8(0x44), 0x29, 0xd0] // sub eax, r10d
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel8(mut rt.bytes, zero_len, done_equal)
	pe_patch_rel8(mut rt.bytes, diff, diff_target)
	pe_patch_rel8(mut rt.bytes, more, loop_start)
}

fn pe_emit_runtime_atexit(mut rt PeRuntimeText, count_off u32, callbacks_off u32) {
	pe_emit_runtime_lea_rdx_data(mut rt, count_off)
	rt.bytes << [u8(0x48), 0x8b, 0x02] // mov rax, [rdx]
	rt.bytes << [u8(0x48), 0x83, 0xf8, pe_atexit_callback_capacity] // cmp rax, capacity
	overflow := pe_emit_jcc8(mut rt.bytes, 0x73) // jae
	pe_emit_runtime_lea_r8_data(mut rt, callbacks_off)
	rt.bytes << [u8(0x49), 0x89, 0x0c, 0xc0] // mov [r8 + rax * 8], rcx
	rt.bytes << [u8(0x48), 0xff, 0xc0] // inc rax
	rt.bytes << [u8(0x48), 0x89, 0x02] // mov [rdx], rax
	rt.bytes << [u8(0x31), 0xc0] // xor eax, eax
	rt.bytes << u8(0xc3) // ret
	overflow_target := rt.bytes.len
	rt.bytes << [u8(0xb8), 1, 0, 0, 0] // mov eax, 1
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel8(mut rt.bytes, overflow, overflow_target)
}

fn pe_emit_runtime_run_atexit(mut rt PeRuntimeText, count_off u32, callbacks_off u32) {
	rt.bytes << [u8(0x48), 0x83, 0xec, 0x28] // sub rsp, 40
	loop_start := rt.bytes.len
	pe_emit_runtime_lea_rdx_data(mut rt, count_off)
	rt.bytes << [u8(0x48), 0x8b, 0x02] // mov rax, [rdx]
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	done := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0x48), 0xff, 0xc8] // dec rax
	rt.bytes << [u8(0x48), 0x89, 0x02] // mov [rdx], rax
	pe_emit_runtime_lea_r8_data(mut rt, callbacks_off)
	rt.bytes << [u8(0x49), 0x8b, 0x0c, 0xc0] // mov rcx, [r8 + rax * 8]
	rt.bytes << [u8(0x48), 0x85, 0xc9] // test rcx, rcx
	skip_null := pe_emit_jcc8(mut rt.bytes, 0x74) // je
	rt.bytes << [u8(0xff), 0xd1] // call rcx
	more := pe_emit_jcc8(mut rt.bytes, 0xeb) // jmp
	done_target := rt.bytes.len
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x28] // add rsp, 40
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel8(mut rt.bytes, done, done_target)
	pe_patch_rel8(mut rt.bytes, skip_null, loop_start)
	pe_patch_rel8(mut rt.bytes, more, loop_start)
}

fn pe_emit_runtime_exit(mut rt PeRuntimeText, run_atexit_offset int) {
	rt.bytes << [u8(0x48), 0x83, 0xec, 0x28] // sub rsp, 40
	if run_atexit_offset >= 0 {
		rt.bytes << [u8(0x89), 0x4c, 0x24, 0x20] // mov [rsp+32], ecx
		call_field := pe_emit_call_placeholder(mut rt.bytes)
		rt.bytes << [u8(0x8b), 0x4c, 0x24, 0x20] // mov ecx, [rsp+32]
		pe_patch_rel32_local(mut rt.bytes, call_field, run_atexit_offset)
	}
	pe_emit_runtime_call_import(mut rt, 'ExitProcess')
	rt.bytes << u8(0xcc) // int3
}

fn pe_emit_runtime_exit_process_1(mut rt PeRuntimeText) {
	rt.bytes << [u8(0xb9), 0x01, 0, 0, 0] // mov ecx, 1
	pe_emit_runtime_call_import(mut rt, 'ExitProcess')
	rt.bytes << u8(0xcc) // int3
}

fn pe_emit_runtime_i64_str(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x48), 0x83, 0xec, 0x58] // sub rsp, 88
	rt.bytes << [u8(0x48), 0x89, 0x4c, 0x24, 0x20] // mov [rsp+32], rcx
	rt.bytes << [u8(0x48), 0x89, 0x54, 0x24, 0x28] // mov [rsp+40], rdx
	pe_emit_runtime_call_import(mut rt, 'GetProcessHeap')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	no_heap := pe_emit_jcc32(mut rt.bytes, 0x84) // je
	rt.bytes << [u8(0x48), 0x89, 0xc1] // mov rcx, rax
	rt.bytes << [u8(0xba), 0x08, 0, 0, 0] // mov edx, HEAP_ZERO_MEMORY
	rt.bytes << [u8(0x41), 0xb8, 0x20, 0, 0, 0] // mov r8d, 32
	pe_emit_runtime_call_import(mut rt, 'HeapAlloc')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	alloc_failed := pe_emit_jcc32(mut rt.bytes, 0x84) // je
	rt.bytes << [u8(0x48), 0x89, 0x44, 0x24, 0x30] // mov [rsp+48], rax
	rt.bytes << [u8(0x4c), 0x8d, 0x40, 0x1f] // lea r8, [rax+31]
	rt.bytes << [u8(0x41), 0xc6, 0x00, 0x00] // mov byte ptr [r8], 0
	rt.bytes << [u8(0x48), 0x8b, 0x44, 0x24, 0x28] // mov rax, [rsp+40]
	rt.bytes << [u8(0x45), 0x31, 0xc9] // xor r9d, r9d
	rt.bytes << [u8(0x45), 0x31, 0xd2] // xor r10d, r10d
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	non_negative := pe_emit_jcc32(mut rt.bytes, 0x89) // jns
	rt.bytes << [u8(0x41), 0xb2, 0x01] // mov r10b, 1
	rt.bytes << [u8(0x48), 0xf7, 0xd8] // neg rax
	non_negative_target := rt.bytes.len
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	non_zero := pe_emit_jcc32(mut rt.bytes, 0x85) // jne
	rt.bytes << [u8(0x49), 0xff, 0xc8] // dec r8
	rt.bytes << [u8(0x41), 0xc6, 0x00, 0x30] // mov byte ptr [r8], '0'
	rt.bytes << [u8(0x41), 0xb9, 0x01, 0, 0, 0] // mov r9d, 1
	maybe_sign_jump := pe_emit_jmp32(mut rt.bytes)
	loop_start := rt.bytes.len
	rt.bytes << [u8(0x31), 0xd2] // xor edx, edx
	rt.bytes << [u8(0xb9), 0x0a, 0, 0, 0] // mov ecx, 10
	rt.bytes << [u8(0x48), 0xf7, 0xf1] // div rcx
	rt.bytes << [u8(0x80), 0xc2, 0x30] // add dl, '0'
	rt.bytes << [u8(0x49), 0xff, 0xc8] // dec r8
	rt.bytes << [u8(0x41), 0x88, 0x10] // mov [r8], dl
	rt.bytes << [u8(0x49), 0xff, 0xc1] // inc r9
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	loop_more := pe_emit_jcc32(mut rt.bytes, 0x85) // jne
	maybe_sign := rt.bytes.len
	rt.bytes << [u8(0x45), 0x84, 0xd2] // test r10b, r10b
	done_digits := pe_emit_jcc32(mut rt.bytes, 0x84) // je
	rt.bytes << [u8(0x49), 0xff, 0xc8] // dec r8
	rt.bytes << [u8(0x41), 0xc6, 0x00, 0x2d] // mov byte ptr [r8], '-'
	rt.bytes << [u8(0x49), 0xff, 0xc1] // inc r9
	done_digits_target := rt.bytes.len
	rt.bytes << [u8(0x48), 0x8b, 0x54, 0x24, 0x20] // mov rdx, [rsp+32]
	rt.bytes << [u8(0x4c), 0x89, 0x02] // mov [rdx], r8
	rt.bytes << [u8(0x44), 0x89, 0x4a, 0x08] // mov [rdx+8], r9d
	rt.bytes << [u8(0xc7), 0x42, 0x0c, 0, 0, 0, 0] // mov dword ptr [rdx+12], 0
	rt.bytes << [u8(0x48), 0x89, 0xd0] // mov rax, rdx
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x58] // add rsp, 88
	rt.bytes << u8(0xc3) // ret
	fail := rt.bytes.len
	pe_emit_runtime_exit_process_1(mut rt)
	pe_patch_rel32_local(mut rt.bytes, no_heap, fail)
	pe_patch_rel32_local(mut rt.bytes, alloc_failed, fail)
	pe_patch_rel32_local(mut rt.bytes, non_negative, non_negative_target)
	pe_patch_rel32_local(mut rt.bytes, non_zero, loop_start)
	pe_patch_rel32_local(mut rt.bytes, maybe_sign_jump, maybe_sign)
	pe_patch_rel32_local(mut rt.bytes, loop_more, loop_start)
	pe_patch_rel32_local(mut rt.bytes, done_digits, done_digits_target)
}

fn pe_emit_runtime_string_plus(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x48), 0x83, 0xec, 0x68] // sub rsp, 104
	rt.bytes << [u8(0x48), 0x89, 0x4c, 0x24, 0x20] // mov [rsp+32], rcx
	rt.bytes << [u8(0x48), 0x89, 0x54, 0x24, 0x28] // mov [rsp+40], rdx
	rt.bytes << [u8(0x4c), 0x89, 0x44, 0x24, 0x30] // mov [rsp+48], r8
	rt.bytes << [u8(0x44), 0x8b, 0x4a, 0x08] // mov r9d, [rdx+8]
	rt.bytes << [u8(0x45), 0x03, 0x48, 0x08] // add r9d, [r8+8]
	len_overflow := pe_emit_jcc32(mut rt.bytes, 0x82) // jc
	rt.bytes << [u8(0x4c), 0x89, 0x4c, 0x24, 0x38] // mov [rsp+56], r9
	rt.bytes << [u8(0x45), 0x89, 0xc8] // mov r8d, r9d
	rt.bytes << [u8(0x49), 0x83, 0xc0, 0x01] // add r8, 1
	alloc_overflow := pe_emit_jcc32(mut rt.bytes, 0x82) // jc
	rt.bytes << [u8(0x4c), 0x89, 0x44, 0x24, 0x40] // mov [rsp+64], r8
	pe_emit_runtime_call_import(mut rt, 'GetProcessHeap')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	no_heap := pe_emit_jcc32(mut rt.bytes, 0x84) // je
	rt.bytes << [u8(0x48), 0x89, 0xc1] // mov rcx, rax
	rt.bytes << [u8(0xba), 0x08, 0, 0, 0] // mov edx, HEAP_ZERO_MEMORY
	rt.bytes << [u8(0x4c), 0x8b, 0x44, 0x24, 0x40] // mov r8, [rsp+64]
	pe_emit_runtime_call_import(mut rt, 'HeapAlloc')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	alloc_failed := pe_emit_jcc32(mut rt.bytes, 0x84) // je
	rt.bytes << [u8(0x48), 0x89, 0x44, 0x24, 0x48] // mov [rsp+72], rax
	rt.bytes << [u8(0x48), 0x8b, 0x54, 0x24, 0x28] // mov rdx, [rsp+40]
	rt.bytes << [u8(0x4c), 0x8b, 0x12] // mov r10, [rdx]
	rt.bytes << [u8(0x44), 0x8b, 0x5a, 0x08] // mov r11d, [rdx+8]
	rt.bytes << [u8(0x48), 0x89, 0xc1] // mov rcx, rax
	rt.bytes << [u8(0x4d), 0x85, 0xdb] // test r11, r11
	copy_a_done := pe_emit_jcc32(mut rt.bytes, 0x84) // je
	copy_a_loop := rt.bytes.len
	rt.bytes << [u8(0x45), 0x8a, 0x0a] // mov r9b, [r10]
	rt.bytes << [u8(0x44), 0x88, 0x09] // mov [rcx], r9b
	rt.bytes << [u8(0x49), 0xff, 0xc2] // inc r10
	rt.bytes << [u8(0x48), 0xff, 0xc1] // inc rcx
	rt.bytes << [u8(0x49), 0xff, 0xcb] // dec r11
	copy_a_more := pe_emit_jcc32(mut rt.bytes, 0x85) // jne
	copy_a_done_target := rt.bytes.len
	rt.bytes << [u8(0x4c), 0x8b, 0x44, 0x24, 0x30] // mov r8, [rsp+48]
	rt.bytes << [u8(0x4d), 0x8b, 0x10] // mov r10, [r8]
	rt.bytes << [u8(0x45), 0x8b, 0x58, 0x08] // mov r11d, [r8+8]
	rt.bytes << [u8(0x4d), 0x85, 0xdb] // test r11, r11
	copy_b_done := pe_emit_jcc32(mut rt.bytes, 0x84) // je
	copy_b_loop := rt.bytes.len
	rt.bytes << [u8(0x45), 0x8a, 0x0a] // mov r9b, [r10]
	rt.bytes << [u8(0x44), 0x88, 0x09] // mov [rcx], r9b
	rt.bytes << [u8(0x49), 0xff, 0xc2] // inc r10
	rt.bytes << [u8(0x48), 0xff, 0xc1] // inc rcx
	rt.bytes << [u8(0x49), 0xff, 0xcb] // dec r11
	copy_b_more := pe_emit_jcc32(mut rt.bytes, 0x85) // jne
	copy_b_done_target := rt.bytes.len
	rt.bytes << [u8(0xc6), 0x01, 0x00] // mov byte ptr [rcx], 0
	rt.bytes << [u8(0x48), 0x8b, 0x54, 0x24, 0x20] // mov rdx, [rsp+32]
	rt.bytes << [u8(0x48), 0x8b, 0x44, 0x24, 0x48] // mov rax, [rsp+72]
	rt.bytes << [u8(0x48), 0x89, 0x02] // mov [rdx], rax
	rt.bytes << [u8(0x8b), 0x44, 0x24, 0x38] // mov eax, [rsp+56]
	rt.bytes << [u8(0x89), 0x42, 0x08] // mov [rdx+8], eax
	rt.bytes << [u8(0xc7), 0x42, 0x0c, 0, 0, 0, 0] // mov dword ptr [rdx+12], 0
	rt.bytes << [u8(0x48), 0x89, 0xd0] // mov rax, rdx
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x68] // add rsp, 104
	rt.bytes << u8(0xc3) // ret
	fail := rt.bytes.len
	pe_emit_runtime_exit_process_1(mut rt)
	pe_patch_rel32_local(mut rt.bytes, len_overflow, fail)
	pe_patch_rel32_local(mut rt.bytes, alloc_overflow, fail)
	pe_patch_rel32_local(mut rt.bytes, no_heap, fail)
	pe_patch_rel32_local(mut rt.bytes, alloc_failed, fail)
	pe_patch_rel32_local(mut rt.bytes, copy_a_done, copy_a_done_target)
	pe_patch_rel32_local(mut rt.bytes, copy_a_more, copy_a_loop)
	pe_patch_rel32_local(mut rt.bytes, copy_b_done, copy_b_done_target)
	pe_patch_rel32_local(mut rt.bytes, copy_b_more, copy_b_loop)
}

fn pe_emit_runtime_errno(mut rt PeRuntimeText) {
	errno_off := pe_runtime_data_alloc(mut rt, 4, 4)
	pe_emit_runtime_lea_rax_data(mut rt, errno_off)
	rt.bytes << u8(0xc3) // ret
}

fn pe_emit_runtime_new_array_from_c_array_noscan(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x48), 0x83, 0xec, 0x58] // sub rsp, 88
	rt.bytes << [u8(0x48), 0x89, 0x4c, 0x24, 0x20] // mov [rsp+32], rcx
	rt.bytes << [u8(0x48), 0x89, 0x54, 0x24, 0x28] // mov [rsp+40], rdx
	rt.bytes << [u8(0x4c), 0x89, 0x44, 0x24, 0x30] // mov [rsp+48], r8
	rt.bytes << [u8(0x4c), 0x89, 0x4c, 0x24, 0x38] // mov [rsp+56], r9
	rt.bytes << [u8(0x48), 0x8b, 0x84, 0x24, 0x80, 0, 0, 0] // mov rax, [rsp+128]
	rt.bytes << [u8(0x48), 0x89, 0x44, 0x24, 0x40] // mov [rsp+64], rax
	rt.bytes << [u8(0x48), 0xc7, 0x01, 0, 0, 0, 0] // mov qword ptr [rcx], 0
	rt.bytes << [u8(0x48), 0xc7, 0x41, 0x08, 0, 0, 0, 0] // mov qword ptr [rcx+8], 0
	rt.bytes << [u8(0x48), 0xc7, 0x41, 0x10, 0, 0, 0, 0] // mov qword ptr [rcx+16], 0
	rt.bytes << [u8(0x48), 0xc7, 0x41, 0x18, 0, 0, 0, 0] // mov qword ptr [rcx+24], 0
	rt.bytes << [u8(0x83), 0xfa, 0x00] // cmp edx, 0
	negative_len := pe_emit_jcc32(mut rt.bytes, 0x8c) // jl
	rt.bytes << [u8(0x41), 0x83, 0xf8, 0x00] // cmp r8d, 0
	negative_cap := pe_emit_jcc32(mut rt.bytes, 0x8c) // jl
	rt.bytes << [u8(0x41), 0x83, 0xf9, 0x00] // cmp r9d, 0
	negative_elem_size := pe_emit_jcc32(mut rt.bytes, 0x8c) // jl
	rt.bytes << [u8(0x45), 0x89, 0xc2] // mov r10d, r8d
	rt.bytes << [u8(0x41), 0x39, 0xd2] // cmp r10d, edx
	cap_ready := pe_emit_jcc8(mut rt.bytes, 0x7d) // jge
	rt.bytes << [u8(0x41), 0x89, 0xd2] // mov r10d, edx
	cap_ready_target := rt.bytes.len
	rt.bytes << [u8(0x4c), 0x89, 0x54, 0x24, 0x48] // mov [rsp+72], r10
	rt.bytes << [u8(0x49), 0x63, 0xc2] // movsxd rax, r10d
	rt.bytes << [u8(0x4d), 0x63, 0xd9] // movsxd r11, r9d
	rt.bytes << [u8(0x49), 0x0f, 0xaf, 0xc3] // imul rax, r11
	rt.bytes << [u8(0x49), 0x89, 0xc0] // mov r8, rax
	rt.bytes << [u8(0x4d), 0x85, 0xc0] // test r8, r8
	payload_nonzero := pe_emit_jcc8(mut rt.bytes, 0x75) // jne
	rt.bytes << [u8(0x41), 0xb8, 0x01, 0, 0, 0] // mov r8d, 1
	payload_nonzero_target := rt.bytes.len
	rt.bytes << [u8(0x49), 0x83, 0xc0, 0x20] // add r8, 32
	allocation_overflow := pe_emit_jcc32(mut rt.bytes, 0x82) // jc
	rt.bytes << [u8(0x4c), 0x89, 0x44, 0x24, 0x50] // mov [rsp+80], r8
	pe_emit_runtime_call_import(mut rt, 'GetProcessHeap')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	no_heap := pe_emit_jcc32(mut rt.bytes, 0x84) // je
	rt.bytes << [u8(0x48), 0x89, 0xc1] // mov rcx, rax
	rt.bytes << [u8(0xba), 0x08, 0, 0, 0] // mov edx, HEAP_ZERO_MEMORY
	rt.bytes << [u8(0x4c), 0x8b, 0x44, 0x24, 0x50] // mov r8, [rsp+80]
	pe_emit_runtime_call_import(mut rt, 'HeapAlloc')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	alloc_failed := pe_emit_jcc32(mut rt.bytes, 0x84) // je
	pe_emit_runtime_align_heap_allocated_data(mut rt)
	rt.bytes << [u8(0x41), 0xc6, 0x03, 0] // mov byte ptr [r11], 0 ; ArrayDataHeader.has_slices = false
	rt.bytes << [u8(0x48), 0x8b, 0x4c, 0x24, 0x20] // mov rcx, [rsp+32]
	rt.bytes << [u8(0x49), 0x8d, 0x43, 0x08] // lea rax, [r11+8]
	rt.bytes << [u8(0x48), 0x89, 0x01] // mov [rcx], rax
	rt.bytes << [u8(0xc7), 0x41, 0x08, 0, 0, 0, 0] // mov dword ptr [rcx+8], 0
	rt.bytes << [u8(0x8b), 0x44, 0x24, 0x28] // mov eax, [rsp+40]
	rt.bytes << [u8(0x89), 0x41, 0x0c] // mov [rcx+12], eax
	rt.bytes << [u8(0x8b), 0x44, 0x24, 0x48] // mov eax, [rsp+72]
	rt.bytes << [u8(0x89), 0x41, 0x10] // mov [rcx+16], eax
	rt.bytes << [u8(0xc7), 0x41, 0x14, 0x30, 0, 0, 0] // mov dword ptr [rcx+20], 48
	rt.bytes << [u8(0x8b), 0x44, 0x24, 0x38] // mov eax, [rsp+56]
	rt.bytes << [u8(0x89), 0x41, 0x18] // mov [rcx+24], eax
	rt.bytes << [u8(0x48), 0x63, 0x44, 0x24, 0x28] // movsxd rax, dword ptr [rsp+40]
	rt.bytes << [u8(0x4c), 0x63, 0x54, 0x24, 0x38] // movsxd r10, dword ptr [rsp+56]
	rt.bytes << [u8(0x49), 0x0f, 0xaf, 0xc2] // imul rax, r10
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	no_copy := pe_emit_jcc32(mut rt.bytes, 0x8e) // jle
	rt.bytes << [u8(0x4c), 0x8b, 0x54, 0x24, 0x40] // mov r10, [rsp+64]
	rt.bytes << [u8(0x4d), 0x85, 0xd2] // test r10, r10
	null_source := pe_emit_jcc32(mut rt.bytes, 0x84) // je
	rt.bytes << [u8(0x4d), 0x8d, 0x43, 0x08] // lea r8, [r11+8]
	copy_loop := rt.bytes.len
	rt.bytes << [u8(0x41), 0x8a, 0x12] // mov dl, [r10]
	rt.bytes << [u8(0x41), 0x88, 0x10] // mov [r8], dl
	rt.bytes << [u8(0x49), 0xff, 0xc2] // inc r10
	rt.bytes << [u8(0x49), 0xff, 0xc0] // inc r8
	rt.bytes << [u8(0x48), 0xff, 0xc8] // dec rax
	copy_more := pe_emit_jcc8(mut rt.bytes, 0x75) // jne
	done := rt.bytes.len
	rt.bytes << [u8(0x48), 0x8b, 0x44, 0x24, 0x20] // mov rax, [rsp+32]
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x58] // add rsp, 88
	rt.bytes << u8(0xc3) // ret
	fail := rt.bytes.len
	rt.bytes << [u8(0x48), 0x8b, 0x44, 0x24, 0x20] // mov rax, [rsp+32]
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x58] // add rsp, 88
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel32_local(mut rt.bytes, negative_len, fail)
	pe_patch_rel32_local(mut rt.bytes, negative_cap, fail)
	pe_patch_rel32_local(mut rt.bytes, negative_elem_size, fail)
	pe_patch_rel8(mut rt.bytes, cap_ready, cap_ready_target)
	pe_patch_rel8(mut rt.bytes, payload_nonzero, payload_nonzero_target)
	pe_patch_rel32_local(mut rt.bytes, allocation_overflow, fail)
	pe_patch_rel32_local(mut rt.bytes, no_heap, fail)
	pe_patch_rel32_local(mut rt.bytes, alloc_failed, fail)
	pe_patch_rel32_local(mut rt.bytes, no_copy, done)
	pe_patch_rel32_local(mut rt.bytes, null_source, done)
	pe_patch_rel8(mut rt.bytes, copy_more, copy_loop)
}

fn pe_emit_runtime_new_array_noscan(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x48), 0x83, 0xec, 0x58] // sub rsp, 88
	rt.bytes << [u8(0x48), 0x89, 0x4c, 0x24, 0x20] // mov [rsp+32], rcx
	rt.bytes << [u8(0x48), 0x89, 0x54, 0x24, 0x28] // mov [rsp+40], rdx
	rt.bytes << [u8(0x4c), 0x89, 0x44, 0x24, 0x30] // mov [rsp+48], r8
	rt.bytes << [u8(0x4c), 0x89, 0x4c, 0x24, 0x38] // mov [rsp+56], r9
	rt.bytes << [u8(0x48), 0xc7, 0x01, 0, 0, 0, 0] // mov qword ptr [rcx], 0
	rt.bytes << [u8(0x48), 0xc7, 0x41, 0x08, 0, 0, 0, 0] // mov qword ptr [rcx+8], 0
	rt.bytes << [u8(0x48), 0xc7, 0x41, 0x10, 0, 0, 0, 0] // mov qword ptr [rcx+16], 0
	rt.bytes << [u8(0x48), 0xc7, 0x41, 0x18, 0, 0, 0, 0] // mov qword ptr [rcx+24], 0
	rt.bytes << [u8(0x83), 0xfa, 0x00] // cmp edx, 0
	negative_len := pe_emit_jcc32(mut rt.bytes, 0x8c) // jl
	rt.bytes << [u8(0x41), 0x83, 0xf8, 0x00] // cmp r8d, 0
	negative_cap := pe_emit_jcc32(mut rt.bytes, 0x8c) // jl
	rt.bytes << [u8(0x41), 0x83, 0xf9, 0x00] // cmp r9d, 0
	negative_elem_size := pe_emit_jcc32(mut rt.bytes, 0x8c) // jl
	rt.bytes << [u8(0x45), 0x89, 0xc2] // mov r10d, r8d
	rt.bytes << [u8(0x41), 0x39, 0xd2] // cmp r10d, edx
	cap_ready := pe_emit_jcc8(mut rt.bytes, 0x7d) // jge
	rt.bytes << [u8(0x41), 0x89, 0xd2] // mov r10d, edx
	cap_ready_target := rt.bytes.len
	rt.bytes << [u8(0x4c), 0x89, 0x54, 0x24, 0x40] // mov [rsp+64], r10
	rt.bytes << [u8(0x49), 0x63, 0xc2] // movsxd rax, r10d
	rt.bytes << [u8(0x4d), 0x63, 0xd9] // movsxd r11, r9d
	rt.bytes << [u8(0x49), 0x0f, 0xaf, 0xc3] // imul rax, r11
	rt.bytes << [u8(0x49), 0x89, 0xc0] // mov r8, rax
	rt.bytes << [u8(0x4d), 0x85, 0xc0] // test r8, r8
	payload_nonzero := pe_emit_jcc8(mut rt.bytes, 0x75) // jne
	rt.bytes << [u8(0x41), 0xb8, 0x01, 0, 0, 0] // mov r8d, 1
	payload_nonzero_target := rt.bytes.len
	rt.bytes << [u8(0x49), 0x83, 0xc0, 0x20] // add r8, 32
	allocation_overflow := pe_emit_jcc32(mut rt.bytes, 0x82) // jc
	rt.bytes << [u8(0x4c), 0x89, 0x44, 0x24, 0x48] // mov [rsp+72], r8
	pe_emit_runtime_call_import(mut rt, 'GetProcessHeap')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	no_heap := pe_emit_jcc32(mut rt.bytes, 0x84) // je
	rt.bytes << [u8(0x48), 0x89, 0xc1] // mov rcx, rax
	rt.bytes << [u8(0xba), 0x08, 0, 0, 0] // mov edx, HEAP_ZERO_MEMORY
	rt.bytes << [u8(0x4c), 0x8b, 0x44, 0x24, 0x48] // mov r8, [rsp+72]
	pe_emit_runtime_call_import(mut rt, 'HeapAlloc')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	alloc_failed := pe_emit_jcc32(mut rt.bytes, 0x84) // je
	pe_emit_runtime_align_heap_allocated_data(mut rt)
	rt.bytes << [u8(0x41), 0xc6, 0x03, 0] // mov byte ptr [r11], 0 ; ArrayDataHeader.has_slices = false
	rt.bytes << [u8(0x48), 0x8b, 0x4c, 0x24, 0x20] // mov rcx, [rsp+32]
	rt.bytes << [u8(0x49), 0x8d, 0x43, 0x08] // lea rax, [r11+8]
	rt.bytes << [u8(0x48), 0x89, 0x01] // mov [rcx], rax
	rt.bytes << [u8(0xc7), 0x41, 0x08, 0, 0, 0, 0] // mov dword ptr [rcx+8], 0
	rt.bytes << [u8(0x8b), 0x44, 0x24, 0x28] // mov eax, [rsp+40]
	rt.bytes << [u8(0x89), 0x41, 0x0c] // mov [rcx+12], eax
	rt.bytes << [u8(0x8b), 0x44, 0x24, 0x40] // mov eax, [rsp+64]
	rt.bytes << [u8(0x89), 0x41, 0x10] // mov [rcx+16], eax
	rt.bytes << [u8(0xc7), 0x41, 0x14, 0x30, 0, 0, 0] // mov dword ptr [rcx+20], 48
	rt.bytes << [u8(0x8b), 0x44, 0x24, 0x38] // mov eax, [rsp+56]
	rt.bytes << [u8(0x89), 0x41, 0x18] // mov [rcx+24], eax
	rt.bytes << [u8(0x48), 0x8b, 0x44, 0x24, 0x20] // mov rax, [rsp+32]
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x58] // add rsp, 88
	rt.bytes << u8(0xc3) // ret
	fail := rt.bytes.len
	rt.bytes << [u8(0x48), 0x8b, 0x44, 0x24, 0x20] // mov rax, [rsp+32]
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x58] // add rsp, 88
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel32_local(mut rt.bytes, negative_len, fail)
	pe_patch_rel32_local(mut rt.bytes, negative_cap, fail)
	pe_patch_rel32_local(mut rt.bytes, negative_elem_size, fail)
	pe_patch_rel8(mut rt.bytes, cap_ready, cap_ready_target)
	pe_patch_rel8(mut rt.bytes, payload_nonzero, payload_nonzero_target)
	pe_patch_rel32_local(mut rt.bytes, allocation_overflow, fail)
	pe_patch_rel32_local(mut rt.bytes, no_heap, fail)
	pe_patch_rel32_local(mut rt.bytes, alloc_failed, fail)
}

fn pe_emit_runtime_array_rune_string(mut rt PeRuntimeText) {
	rt.bytes << [u8(0x48), 0x83, 0xec, 0x58] // sub rsp, 88
	rt.bytes << [u8(0x48), 0x89, 0x4c, 0x24, 0x20] // mov [rsp+32], rcx
	rt.bytes << [u8(0x48), 0x89, 0x54, 0x24, 0x28] // mov [rsp+40], rdx
	rt.bytes << [u8(0x48), 0xc7, 0x01, 0, 0, 0, 0] // mov qword ptr [rcx], 0
	rt.bytes << [u8(0x48), 0xc7, 0x41, 0x08, 0, 0, 0, 0] // mov qword ptr [rcx+8], 0
	rt.bytes << [u8(0x48), 0x85, 0xd2] // test rdx, rdx
	null_array := pe_emit_jcc32(mut rt.bytes, 0x84) // je
	rt.bytes << [u8(0x48), 0x63, 0x42, 0x0c] // movsxd rax, dword ptr [rdx+12]
	rt.bytes << [u8(0x48), 0x89, 0x44, 0x24, 0x30] // mov [rsp+48], rax
	rt.bytes << [u8(0x48), 0x8b, 0x02] // mov rax, [rdx]
	rt.bytes << [u8(0x4c), 0x63, 0x52, 0x08] // movsxd r10, dword ptr [rdx+8]
	rt.bytes << [u8(0x4c), 0x01, 0xd0] // add rax, r10
	rt.bytes << [u8(0x48), 0x89, 0x44, 0x24, 0x38] // mov [rsp+56], rax
	rt.bytes << [u8(0x4c), 0x8b, 0x44, 0x24, 0x30] // mov r8, [rsp+48]
	rt.bytes << [u8(0x4d), 0x85, 0xc0] // test r8, r8
	positive_size := pe_emit_jcc32(mut rt.bytes, 0x8f) // jg
	rt.bytes << [u8(0x41), 0xb8, 0x01, 0, 0, 0] // mov r8d, 1
	size_ready_jump := pe_emit_jmp32(mut rt.bytes)
	positive_size_target := rt.bytes.len
	rt.bytes << [u8(0x49), 0xc1, 0xe0, 0x02] // shl r8, 2
	rt.bytes << [u8(0x49), 0x83, 0xc0, 0x01] // add r8, 1
	size_ready := rt.bytes.len
	rt.bytes << [u8(0x49), 0x83, 0xc0, 0x18] // add r8, 24
	allocation_overflow := pe_emit_jcc32(mut rt.bytes, 0x82) // jc
	rt.bytes << [u8(0x4c), 0x89, 0x44, 0x24, 0x50] // mov [rsp+80], r8
	pe_emit_runtime_call_import(mut rt, 'GetProcessHeap')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	no_heap := pe_emit_jcc32(mut rt.bytes, 0x84) // je
	rt.bytes << [u8(0x48), 0x89, 0xc1] // mov rcx, rax
	rt.bytes << [u8(0x31), 0xd2] // xor edx, edx
	rt.bytes << [u8(0x4c), 0x8b, 0x44, 0x24, 0x50] // mov r8, [rsp+80]
	pe_emit_runtime_call_import(mut rt, 'HeapAlloc')
	rt.bytes << [u8(0x48), 0x85, 0xc0] // test rax, rax
	alloc_failed := pe_emit_jcc32(mut rt.bytes, 0x84) // je
	pe_emit_runtime_align_heap_allocated_data(mut rt)
	rt.bytes << [u8(0x4c), 0x89, 0x5c, 0x24, 0x40] // mov [rsp+64], r11
	rt.bytes << [u8(0x48), 0x8b, 0x4c, 0x24, 0x30] // mov rcx, [rsp+48]
	rt.bytes << [u8(0x48), 0x85, 0xc9] // test rcx, rcx
	no_input := pe_emit_jcc32(mut rt.bytes, 0x8e) // jle
	rt.bytes << [u8(0x48), 0x8b, 0x54, 0x24, 0x38] // mov rdx, [rsp+56]
	rt.bytes << [u8(0x48), 0x85, 0xd2] // test rdx, rdx
	no_data := pe_emit_jcc32(mut rt.bytes, 0x84) // je
	loop_start := rt.bytes.len
	rt.bytes << [u8(0x8b), 0x02] // mov eax, [rdx]
	rt.bytes << [u8(0x48), 0x83, 0xc2, 0x04] // add rdx, 4
	rt.bytes << [u8(0x83), 0xf8, 0x7f] // cmp eax, 0x7f
	two_byte_check := pe_emit_jcc32(mut rt.bytes, 0x87) // ja
	rt.bytes << [u8(0x41), 0x88, 0x03] // mov [r11], al
	rt.bytes << [u8(0x49), 0xff, 0xc3] // inc r11
	after_one := pe_emit_jmp32(mut rt.bytes)
	two_byte_target := rt.bytes.len
	rt.bytes << [u8(0x3d), 0xff, 0x07, 0, 0] // cmp eax, 0x7ff
	three_byte_check := pe_emit_jcc32(mut rt.bytes, 0x87) // ja
	rt.bytes << [u8(0x41), 0x89, 0xc1, 0x41, 0xc1, 0xe9, 0x06, 0x41, 0x80, 0xc9, 0xc0, 0x45, 0x88,
		0x0b, 0x41, 0x89, 0xc1, 0x41, 0x80, 0xe1, 0x3f, 0x41, 0x80, 0xc9, 0x80, 0x45, 0x88, 0x4b,
		0x01, 0x49, 0x83, 0xc3, 0x02]
	after_two := pe_emit_jmp32(mut rt.bytes)
	three_byte_target := rt.bytes.len
	rt.bytes << [u8(0x3d), 0x00, 0xd8, 0, 0] // cmp eax, 0xd800
	valid_three := pe_emit_jcc32(mut rt.bytes, 0x82) // jb
	rt.bytes << [u8(0x3d), 0xff, 0xdf, 0, 0] // cmp eax, 0xdfff
	skip_surrogate := pe_emit_jcc32(mut rt.bytes, 0x86) // jbe
	valid_three_target := rt.bytes.len
	rt.bytes << [u8(0x3d), 0xff, 0xff, 0, 0] // cmp eax, 0xffff
	four_byte_check := pe_emit_jcc32(mut rt.bytes, 0x87) // ja
	rt.bytes << [u8(0x41), 0x89, 0xc1, 0x41, 0xc1, 0xe9, 0x0c, 0x41, 0x80, 0xc9, 0xe0, 0x45, 0x88,
		0x0b, 0x41, 0x89, 0xc1, 0x41, 0xc1, 0xe9, 0x06, 0x41, 0x80, 0xe1, 0x3f, 0x41, 0x80, 0xc9,
		0x80, 0x45, 0x88, 0x4b, 0x01, 0x41, 0x89, 0xc1, 0x41, 0x80, 0xe1, 0x3f, 0x41, 0x80, 0xc9,
		0x80, 0x45, 0x88, 0x4b, 0x02, 0x49, 0x83, 0xc3, 0x03]
	after_three := pe_emit_jmp32(mut rt.bytes)
	four_byte_target := rt.bytes.len
	rt.bytes << [u8(0x3d), 0xff, 0xff, 0x10, 0] // cmp eax, 0x10ffff
	skip_large := pe_emit_jcc32(mut rt.bytes, 0x87) // ja
	rt.bytes << [u8(0x41), 0x89, 0xc1, 0x41, 0xc1, 0xe9, 0x12, 0x41, 0x80, 0xc9, 0xf0, 0x45, 0x88,
		0x0b, 0x41, 0x89, 0xc1, 0x41, 0xc1, 0xe9, 0x0c, 0x41, 0x80, 0xe1, 0x3f, 0x41, 0x80, 0xc9,
		0x80, 0x45, 0x88, 0x4b, 0x01, 0x41, 0x89, 0xc1, 0x41, 0xc1, 0xe9, 0x06, 0x41, 0x80, 0xe1,
		0x3f, 0x41, 0x80, 0xc9, 0x80, 0x45, 0x88, 0x4b, 0x02, 0x41, 0x89, 0xc1, 0x41, 0x80, 0xe1,
		0x3f, 0x41, 0x80, 0xc9, 0x80, 0x45, 0x88, 0x4b, 0x03, 0x49, 0x83, 0xc3, 0x04]
	after_emit := rt.bytes.len
	rt.bytes << [u8(0x48), 0xff, 0xc9] // dec rcx
	more := pe_emit_jcc32(mut rt.bytes, 0x85) // jne
	finish := rt.bytes.len
	rt.bytes << [u8(0x41), 0xc6, 0x03, 0x00] // mov byte ptr [r11], 0
	rt.bytes << [u8(0x48), 0x8b, 0x4c, 0x24, 0x20] // mov rcx, [rsp+32]
	rt.bytes << [u8(0x48), 0x8b, 0x44, 0x24, 0x40] // mov rax, [rsp+64]
	rt.bytes << [u8(0x48), 0x89, 0x01] // mov [rcx], rax
	rt.bytes << [u8(0x4c), 0x89, 0xda] // mov rdx, r11
	rt.bytes << [u8(0x48), 0x29, 0xc2] // sub rdx, rax
	rt.bytes << [u8(0x89), 0x51, 0x08] // mov [rcx+8], edx
	rt.bytes << [u8(0xc7), 0x41, 0x0c, 0, 0, 0, 0] // mov dword ptr [rcx+12], 0
	rt.bytes << [u8(0x48), 0x89, 0xc8] // mov rax, rcx
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x58] // add rsp, 88
	rt.bytes << u8(0xc3) // ret
	fail := rt.bytes.len
	rt.bytes << [u8(0x48), 0x8b, 0x44, 0x24, 0x20] // mov rax, [rsp+32]
	rt.bytes << [u8(0x48), 0x83, 0xc4, 0x58] // add rsp, 88
	rt.bytes << u8(0xc3) // ret
	pe_patch_rel32_local(mut rt.bytes, null_array, fail)
	pe_patch_rel32_local(mut rt.bytes, positive_size, positive_size_target)
	pe_patch_rel32_local(mut rt.bytes, size_ready_jump, size_ready)
	pe_patch_rel32_local(mut rt.bytes, allocation_overflow, fail)
	pe_patch_rel32_local(mut rt.bytes, no_heap, fail)
	pe_patch_rel32_local(mut rt.bytes, alloc_failed, fail)
	pe_patch_rel32_local(mut rt.bytes, no_input, finish)
	pe_patch_rel32_local(mut rt.bytes, no_data, finish)
	pe_patch_rel32_local(mut rt.bytes, two_byte_check, two_byte_target)
	pe_patch_rel32_local(mut rt.bytes, after_one, after_emit)
	pe_patch_rel32_local(mut rt.bytes, three_byte_check, three_byte_target)
	pe_patch_rel32_local(mut rt.bytes, after_two, after_emit)
	pe_patch_rel32_local(mut rt.bytes, valid_three, valid_three_target)
	pe_patch_rel32_local(mut rt.bytes, skip_surrogate, after_emit)
	pe_patch_rel32_local(mut rt.bytes, four_byte_check, four_byte_target)
	pe_patch_rel32_local(mut rt.bytes, after_three, after_emit)
	pe_patch_rel32_local(mut rt.bytes, skip_large, after_emit)
	pe_patch_rel32_local(mut rt.bytes, more, loop_start)
}

fn pe_read_i32_le(data []u8, off int) i32 {
	return i32(u32(data[off]) | (u32(data[off + 1]) << 8) | (u32(data[off + 2]) << 16) | (u32(data[
		off + 3]) << 24))
}

fn pe_put_u32_le(mut data []u8, off int, v u32) {
	data[off] = u8(v)
	data[off + 1] = u8(v >> 8)
	data[off + 2] = u8(v >> 16)
	data[off + 3] = u8(v >> 24)
}

fn pe_put_u64_le(mut data []u8, off int, v u64) {
	pe_put_u32_le(mut data, off, u32(v))
	pe_put_u32_le(mut data, off + 4, u32(v >> 32))
}
