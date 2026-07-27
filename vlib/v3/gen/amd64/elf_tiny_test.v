module amd64

import crypto.sha256
import os
import time

const elf_tiny_runtime_test_guard = 'V3_ELF_TINY_RUNTIME_ORACLE'
const elf_tiny_runtime_test_timeout_ms = 10_000
const elf_tiny_runtime_test_output_limit = 64 * 1024

struct ElfTinyTestProgramHeader {
	type_     u32
	flags     u32
	offset    u64
	vaddr     u64
	paddr     u64
	file_size u64
	mem_size  u64
	alignment u64
}

struct ElfTinyRuntimeTestProcessResult {
	exit_code      int
	stdout         string
	stderr         string
	timed_out      bool
	output_limited bool
}

struct ElfTinyRuntimeTestCapture {
mut:
	stdout string
	stderr string
}

fn elf_tiny_test_read_u16(data []u8, offset int) u16 {
	assert offset >= 0
	assert offset <= data.len - 2
	return u16(data[offset]) | (u16(data[offset + 1]) << 8)
}

fn elf_tiny_test_read_u32(data []u8, offset int) u32 {
	assert offset >= 0
	assert offset <= data.len - 4
	return u32(data[offset]) | (u32(data[offset + 1]) << 8) | (u32(data[offset + 2]) << 16) | (u32(data[
		offset + 3]) << 24)
}

fn elf_tiny_test_read_u64(data []u8, offset int) u64 {
	return u64(elf_tiny_test_read_u32(data, offset)) | (u64(elf_tiny_test_read_u32(data, offset + 4)) << 32)
}

fn elf_tiny_test_program_headers(data []u8) []ElfTinyTestProgramHeader {
	offset := elf_tiny_test_read_u64(data, 32)
	entry_size := elf_tiny_test_read_u16(data, 54)
	count := elf_tiny_test_read_u16(data, 56)
	assert entry_size == 56
	assert offset <= u64(data.len)
	assert u64(count) <= (u64(data.len) - offset) / u64(entry_size)
	mut headers := []ElfTinyTestProgramHeader{cap: int(count)}
	for index in 0 .. int(count) {
		start := int(offset) + index * int(entry_size)
		headers << ElfTinyTestProgramHeader{
			type_:     elf_tiny_test_read_u32(data, start)
			flags:     elf_tiny_test_read_u32(data, start + 4)
			offset:    elf_tiny_test_read_u64(data, start + 8)
			vaddr:     elf_tiny_test_read_u64(data, start + 16)
			paddr:     elf_tiny_test_read_u64(data, start + 24)
			file_size: elf_tiny_test_read_u64(data, start + 32)
			mem_size:  elf_tiny_test_read_u64(data, start + 40)
			alignment: elf_tiny_test_read_u64(data, start + 48)
		}
	}
	return headers
}

fn elf_tiny_test_assert_zero_range(data []u8, start int, end int) {
	assert start >= 0
	assert start <= end
	assert end <= data.len
	for byte in data[start..end] {
		assert byte == 0
	}
}

fn elf_tiny_test_error(o &Object, entry ElfTinyEntryDefinition) string {
	if _ := elf_tiny_executable_bytes(o, entry) {
		assert false, 'ELF tiny serialization unexpectedly succeeded'
	} else {
		return err.msg()
	}
	return ''
}

fn elf_tiny_runtime_test_definition(entry SymbolID, policy ElfTinyEntryResultPolicy, bindings []ElfTinyRuntimeBinding) ElfTinyRuntimeDefinition {
	return ElfTinyRuntimeDefinition{
		entry:            ElfTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  policy
		}
		startup_policy:   .no_args_no_init
		runtime_bindings: bindings
	}
}

fn elf_tiny_runtime_test_error(o &Object, definition ElfTinyRuntimeDefinition) string {
	if _ := elf_tiny_runtime_executable_bytes(o, definition) {
		assert false, 'ELF tiny runtime serialization unexpectedly succeeded'
	} else {
		return err.msg()
	}
	return ''
}

fn elf_tiny_runtime_test_install_data(mut o Object, definition &ObjectDataDefinition) {
	plan := object_data_preflight(definition, &o) or { panic(err) }
	o.install_object_data(&plan) or { panic(err) }
}

fn elf_tiny_runtime_test_pc_relocation(offset u64, target ObjectDataSymbolID, addend i64) ObjectDataRelocation {
	return ObjectDataRelocation{
		source_section: .text
		offset:         offset
		target_symbol:  object_data_symbol_ref(target)
		width:          32
		kind:           .pc_relative
		signedness:     .signed
		address_intent: .virtual_address
		pc_bias:        .zero
		got_access:     .none
		addend:         addend
	}
}

fn elf_tiny_runtime_test_clone_object(o &Object) Object {
	return Object{
		text:                 o.text.clone()
		symbols:              o.symbols.clone()
		call_relocations:     o.call_relocations.clone()
		private_data:         o.private_data.clone()
		private_data_symbols: o.private_data_symbols.clone()
		object_data:          object_data_clone(o.object_data.sections, o.object_data.symbols,
			o.object_data.relocations)
	}
}

fn elf_tiny_runtime_test_leaf(name string) (Object, SymbolID) {
	mut o := Object.new()
	entry := o.intern_function_symbol(name) or { panic(err) }
	assert o.append_text([u8(0x31), 0xc0, 0xc3]) or { panic(err) } == 0
	o.define_text_function(entry, 0, 3) or { panic(err) }
	return o, entry
}

fn elf_tiny_runtime_test_helper_object(name string) (Object, SymbolID, SymbolID) {
	mut o := Object.new()
	entry := o.intern_function_symbol('runtime_entry') or { panic(err) }
	helper := o.intern_external_function_symbol(name) or { panic(err) }
	assert o.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	o.define_text_function(entry, 0, 6) or { panic(err) }
	o.add_text_call_relocation(1, helper) or { panic(err) }
	return o, entry, helper
}

fn elf_tiny_runtime_test_rel32_target(bytes []u8, field int) int {
	raw := elf_tiny_test_read_u32(bytes, field)
	return field + 4 + int(i32(raw))
}

fn elf_tiny_runtime_test_concat_allocation(left u64, right u64) !u64 {
	if left > u64(max_u32) || right > u64(max_u32) || left > u64(max_u32) - right {
		return error('length overflow')
	}
	return left + right + 1
}

fn (mut capture ElfTinyRuntimeTestCapture) append_bounded(pipe os.ChildProcessPipeKind, chunk string) bool {
	match pipe {
		.stdout {
			if chunk.len > elf_tiny_runtime_test_output_limit - capture.stdout.len {
				return false
			}
			capture.stdout += chunk
		}
		.stderr {
			if chunk.len > elf_tiny_runtime_test_output_limit - capture.stderr.len {
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

fn elf_tiny_runtime_test_drain_process(mut process os.Process, mut capture ElfTinyRuntimeTestCapture) bool {
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

fn elf_tiny_runtime_test_run_process(command string, args []string, timeout_ms int) ElfTinyRuntimeTestProcessResult {
	mut process := os.new_process(command)
	process.use_pgroup = true
	process.set_args(args)
	process.set_environment({
		'LC_ALL': 'C'
		'LANG':   'C'
		'PATH':   os.getenv('PATH')
		'HOME':   os.getenv('HOME')
		'TMPDIR': os.getenv('TMPDIR')
	})
	process.set_redirect_stdio()
	process.run()
	mut elapsed_ms := 0
	mut capture := ElfTinyRuntimeTestCapture{}
	mut output_limited := false
	for process.is_alive() && elapsed_ms < timeout_ms && !output_limited {
		if !elf_tiny_runtime_test_drain_process(mut process, mut capture) {
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
	if !output_limited && !elf_tiny_runtime_test_drain_process(mut process, mut capture) {
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
	return ElfTinyRuntimeTestProcessResult{
		exit_code:      exit_code
		stdout:         capture.stdout
		stderr:         capture.stderr
		timed_out:      timed_out
		output_limited: output_limited
	}
}

fn test_elf_tiny_leaf_et_exec_has_exact_entry_result_policies_one_rx_load_and_nx_stack() {
	mut object := Object.new()
	entry := object.intern_function_symbol('explicit_entry') or { panic(err) }
	assert object.append_text([u8(0x31), 0xc0, 0xc3]) or { panic(err) } == 0
	object.define_text_function(entry, 0, 3) or { panic(err) }
	before_text := object.text.clone()

	data := elf_tiny_executable_bytes(&object, ElfTinyEntryDefinition{
		function_index: u32(entry)
		result_policy:  .scalar
	}) or { panic(err) }
	void_data := elf_tiny_executable_bytes(&object, ElfTinyEntryDefinition{
		function_index: u32(entry)
		result_policy:  .void_
	}) or { panic(err) }
	assert data.len == 211
	assert void_data.len == 211
	assert data[0..16] == [
		u8(0x7f),
		0x45,
		0x4c,
		0x46,
		0x02,
		0x01,
		0x01,
		0x00,
		0x00,
		0x00,
		0x00,
		0x00,
		0x00,
		0x00,
		0x00,
		0x00,
	]
	assert elf_tiny_test_read_u16(data, 16) == elf_tiny_et_exec
	assert elf_tiny_test_read_u16(data, 18) == elf_tiny_em_x86_64
	assert elf_tiny_test_read_u32(data, 20) == 1
	assert elf_tiny_test_read_u64(data, 24) == elf_tiny_base_vaddr + 176
	assert elf_tiny_test_read_u64(data, 32) == 64
	assert elf_tiny_test_read_u64(data, 40) == 0
	assert elf_tiny_test_read_u32(data, 48) == 0
	assert elf_tiny_test_read_u16(data, 52) == 64
	assert elf_tiny_test_read_u16(data, 54) == 56
	assert elf_tiny_test_read_u16(data, 56) == 2
	assert elf_tiny_test_read_u16(data, 58) == 0
	assert elf_tiny_test_read_u16(data, 60) == 0
	assert elf_tiny_test_read_u16(data, 62) == 0

	headers := elf_tiny_test_program_headers(data)
	assert headers == [
		ElfTinyTestProgramHeader{
			type_:     elf_tiny_pt_load
			flags:     elf_tiny_pf_r | elf_tiny_pf_x
			offset:    0
			vaddr:     elf_tiny_base_vaddr
			paddr:     elf_tiny_base_vaddr
			file_size: 211
			mem_size:  211
			alignment: elf_tiny_page_align
		},
		ElfTinyTestProgramHeader{
			type_:     elf_tiny_pt_gnu_stack
			flags:     elf_tiny_pf_r | elf_tiny_pf_w
			alignment: 16
		},
	]
	for header in headers {
		assert (header.flags & (elf_tiny_pf_w | elf_tiny_pf_x)) != (elf_tiny_pf_w | elf_tiny_pf_x)
	}
	text_offset := 176
	assert data[text_offset..text_offset + 18] == [
		u8(0x31),
		0xed,
		0xe8,
		0x19,
		0x00,
		0x00,
		0x00,
		0x89,
		0xc7,
		0xb8,
		0xe7,
		0x00,
		0x00,
		0x00,
		0x0f,
		0x05,
		0x0f,
		0x0b,
	]
	assert void_data[text_offset..text_offset + 18] == [
		u8(0x31),
		0xed,
		0xe8,
		0x19,
		0x00,
		0x00,
		0x00,
		0x31,
		0xff,
		0xb8,
		0xe7,
		0x00,
		0x00,
		0x00,
		0x0f,
		0x05,
		0x0f,
		0x0b,
	]
	elf_tiny_test_assert_zero_range(data, text_offset + 18, 208)
	elf_tiny_test_assert_zero_range(void_data, text_offset + 18, 208)
	assert data[208..] == before_text
	assert void_data[208..] == before_text
	assert object.text == before_text
	assert object.call_relocations.len == 0
}

fn test_elf_tiny_private_data_adds_page_aligned_rw_load_without_wx() {
	mut object := Object.new()
	entry := object.intern_function_symbol('entry_with_private_data') or { panic(err) }
	plan := private_data_preflight([
		PrivateDataDefinition{ name: 'flag', value: 7, width: 8, alignment: 1 },
		PrivateDataDefinition{
			name:      'counter'
			value:     0x0102_0304_0506_0708
			width:     64
			alignment: 8
		},
	], ['entry_with_private_data']) or { panic(err) }
	object.install_private_data(&plan) or { panic(err) }
	assert object.append_text([u8(0x31), 0xc0, 0xc3]) or { panic(err) } == 0
	object.define_text_function(entry, 0, 3) or { panic(err) }
	before_data := object.private_data.clone()

	data := elf_tiny_executable_bytes(&object, ElfTinyEntryDefinition{
		function_index: u32(entry)
		result_policy:  .scalar
	}) or { panic(err) }
	assert data.len == 4096 + before_data.len
	assert elf_tiny_test_read_u64(data, 24) == elf_tiny_base_vaddr + 240
	assert elf_tiny_test_read_u16(data, 56) == 3
	headers := elf_tiny_test_program_headers(data)
	assert headers.len == 3
	assert headers[0] == ElfTinyTestProgramHeader{
		type_:     elf_tiny_pt_load
		flags:     elf_tiny_pf_r | elf_tiny_pf_x
		offset:    0
		vaddr:     elf_tiny_base_vaddr
		paddr:     elf_tiny_base_vaddr
		file_size: 275
		mem_size:  275
		alignment: elf_tiny_page_align
	}
	assert headers[1] == ElfTinyTestProgramHeader{
		type_:     elf_tiny_pt_load
		flags:     elf_tiny_pf_r | elf_tiny_pf_w
		offset:    4096
		vaddr:     elf_tiny_base_vaddr + 4096
		paddr:     elf_tiny_base_vaddr + 4096
		file_size: u64(before_data.len)
		mem_size:  u64(before_data.len)
		alignment: elf_tiny_page_align
	}
	assert headers[2] == ElfTinyTestProgramHeader{
		type_:     elf_tiny_pt_gnu_stack
		flags:     elf_tiny_pf_r | elf_tiny_pf_w
		alignment: 16
	}
	assert headers[0].vaddr < headers[1].vaddr
	assert headers[1].offset % headers[1].alignment == headers[1].vaddr % headers[1].alignment
	for header in headers {
		assert (header.flags & (elf_tiny_pf_w | elf_tiny_pf_x)) != (elf_tiny_pf_w | elf_tiny_pf_x)
	}
	elf_tiny_test_assert_zero_range(data, 232, 240)
	assert elf_tiny_test_read_u32(data, 243) == u32(25)
	elf_tiny_test_assert_zero_range(data, 258, 272)
	assert data[272..275] == [u8(0x31), 0xc0, 0xc3]
	elf_tiny_test_assert_zero_range(data, 275, 4096)
	assert data[4096..] == before_data
	assert object.private_data == before_data
}

fn test_elf_tiny_resolves_startup_and_internal_call_rel32_exactly() {
	mut object := Object.new()
	forward := object.intern_function_symbol('forward') or { panic(err) }
	backward := object.intern_function_symbol('backward') or { panic(err) }
	recursive := object.intern_function_symbol('recursive') or { panic(err) }
	body := [
		u8(0x48),
		0x83,
		0xec,
		0x08,
		0xe8,
		0x00,
		0x00,
		0x00,
		0x00,
		0x31,
		0xc0,
		0x48,
		0x83,
		0xc4,
		0x08,
		0xc3,
	]
	assert object.append_text(body) or { panic(err) } == 0
	assert object.append_text(body) or { panic(err) } == 16
	assert object.append_text(body) or { panic(err) } == 32
	object.define_text_function(forward, 0, 16) or { panic(err) }
	object.define_text_function(backward, 16, 16) or { panic(err) }
	object.define_text_function(recursive, 32, 16) or { panic(err) }
	object.add_text_call_relocation(5, backward) or { panic(err) }
	object.add_text_call_relocation(21, forward) or { panic(err) }
	object.add_text_call_relocation(37, recursive) or { panic(err) }
	before_text := object.text.clone()

	data := elf_tiny_executable_bytes(&object, ElfTinyEntryDefinition{
		function_index: u32(backward)
		result_policy:  .scalar
	}) or { panic(err) }
	entry_offset := 176
	object_text_offset := 208
	assert elf_tiny_test_read_u32(data, entry_offset + 3) == u32(41)
	elf_tiny_test_assert_zero_range(data, entry_offset + 18, object_text_offset)
	assert elf_tiny_test_read_u32(data, object_text_offset + 5) == u32(7)
	assert elf_tiny_test_read_u32(data, object_text_offset + 21) == u32(0xffff_ffe7)
	assert elf_tiny_test_read_u32(data, object_text_offset + 37) == u32(0xffff_fff7)
	assert object.text == before_text
	assert object.text[5..9] == [u8(0), 0, 0, 0]
	assert object.text[21..25] == [u8(0), 0, 0, 0]
	assert object.text[37..41] == [u8(0), 0, 0, 0]
}

fn test_elf_tiny_rejects_external_scalar_parameter_bad_entry_and_invalid_object() {
	mut leaf := Object.new()
	entry := leaf.intern_function_symbol('entry') or { panic(err) }
	assert leaf.append_text([u8(0x31), 0xc0, 0xc3]) or { panic(err) } == 0
	leaf.define_text_function(entry, 0, 3) or { panic(err) }
	assert elf_tiny_test_error(&leaf, ElfTinyEntryDefinition{
		function_index: 9
		result_policy:  .scalar
	}) == 'ELF tiny entry function index 9 is out of range'
	assert elf_tiny_test_error(&leaf, ElfTinyEntryDefinition{
		function_index:  u32(entry)
		parameter_count: 1
		result_policy:   .scalar
	}) == 'ELF tiny entry function must not accept scalar parameters'
	assert elf_tiny_test_error(&leaf, ElfTinyEntryDefinition{
		function_index: u32(entry)
		result_policy:  unsafe { ElfTinyEntryResultPolicy(255) }
	}) == 'ELF tiny entry result policy 255 is unsupported'

	mut external_object := Object.new()
	caller := external_object.intern_function_symbol('caller') or { panic(err) }
	foreign := external_object.intern_external_function_symbol('foreign') or { panic(err) }
	assert external_object.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	external_object.define_text_function(caller, 0, 6) or { panic(err) }
	external_object.add_text_call_relocation(1, foreign) or { panic(err) }
	assert elf_tiny_test_error(&external_object, ElfTinyEntryDefinition{
		function_index: u32(caller)
		result_policy:  .scalar
	}) == 'ELF tiny executable does not support external function `foreign`'

	mut malformed := Object.new()
	self := malformed.intern_function_symbol('self') or { panic(err) }
	assert malformed.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	malformed.define_text_function(self, 0, 6) or { panic(err) }
	malformed.add_text_call_relocation(1, self) or { panic(err) }
	malformed.text[2] = 1
	before := malformed.text.clone()
	assert elf_tiny_test_error(&malformed, ElfTinyEntryDefinition{
		function_index: u32(self)
		result_policy:  .scalar
	}) == 'AMD64 object CALL relocation field 1 is not a zero rel32 placeholder'
	assert malformed.text == before
}

fn test_elf_tiny_rel32_and_layout_checks_reject_overflow_without_large_allocations() {
	assert elf_tiny_checked_rel32(0, u64(2_147_483_651)) or { panic(err) } == u32(0x7fff_ffff)
	assert elf_tiny_checked_rel32(u64(2_147_483_644), 0) or { panic(err) } == u32(0x8000_0000)
	if _ := elf_tiny_checked_rel32(0, u64(2_147_483_652)) {
		assert false, 'positive out-of-range ELF tiny rel32 was accepted'
	} else {
		assert err.msg() == 'ELF tiny CALL displacement is outside signed 32-bit range'
	}
	if _ := elf_tiny_checked_rel32(u64(2_147_483_645), 0) {
		assert false, 'negative out-of-range ELF tiny rel32 was accepted'
	} else {
		assert err.msg() == 'ELF tiny CALL displacement is outside signed 32-bit range'
	}
	if _ := elf_tiny_checked_rel32(max_u64 - 3, 0) {
		assert false, 'overflowing ELF tiny CALL place was accepted'
	} else {
		assert err.msg() == 'ELF64 tiny CALL next instruction overflows u64'
	}
	if _ := elf_tiny_build_layout(max_u64, 0) {
		assert false, 'overflowing ELF tiny text layout was accepted'
	} else {
		assert err.msg() == 'ELF64 tiny RX extent overflows u64'
	}
	if _ := elf_tiny_build_layout(1, max_u64) {
		assert false, 'overflowing ELF tiny data layout was accepted'
	} else {
		assert err.msg() == 'ELF64 tiny file size overflows u64'
	}
	layout := elf_tiny_build_layout(3, 0) or { panic(err) }
	assert layout.entry_offset == 176
	assert layout.entry_offset % 16 == 0
	assert layout.entry_vaddr == elf_tiny_base_vaddr + 176
	assert layout.object_text_offset == 208
	assert layout.object_text_offset % 16 == 0
	assert layout.object_text_vaddr == elf_tiny_base_vaddr + 208
	malformed_layout := ElfTinyLayout{
		program_header_count: layout.program_header_count
		entry_offset:         layout.entry_offset + 1
		entry_vaddr:          layout.entry_vaddr
		object_text_offset:   layout.object_text_offset
		object_text_vaddr:    layout.object_text_vaddr
		rx_file_size:         layout.rx_file_size
		data_offset:          layout.data_offset
		data_vaddr:           layout.data_vaddr
		file_size:            layout.file_size
	}
	if _ := elf_tiny_validate_layout(&malformed_layout, 3, 0) {
		assert false, 'malformed ELF tiny layout was accepted'
	} else {
		assert err.msg() == 'ELF tiny layout entry offset 177 does not match 176'
	}
}

fn test_elf_tiny_output_is_fresh_deterministic_and_does_not_mutate_object() {
	mut object := Object.new()
	caller := object.intern_function_symbol('caller') or { panic(err) }
	callee := object.intern_function_symbol('callee') or { panic(err) }
	plan := private_data_preflight([
		PrivateDataDefinition{ name: 'slot', value: 23, width: 32, alignment: 4 },
	], ['caller', 'callee']) or { panic(err) }
	object.install_private_data(&plan) or { panic(err) }
	assert object.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	assert object.append_text([u8(0x31), 0xc0, 0xc3]) or { panic(err) } == 6
	object.define_text_function(caller, 0, 6) or { panic(err) }
	object.define_text_function(callee, 6, 3) or { panic(err) }
	object.add_text_call_relocation(1, callee) or { panic(err) }
	before_text := object.text.clone()
	before_data := object.private_data.clone()
	before_relocation := object.call_relocations[0]
	entry := ElfTinyEntryDefinition{
		function_index: u32(caller)
		result_policy:  .scalar
	}

	mut first := elf_tiny_executable_bytes(&object, entry) or { panic(err) }
	second := elf_tiny_executable_bytes(&object, entry) or { panic(err) }
	assert first == second
	first[0] = 0
	third := elf_tiny_executable_bytes(&object, entry) or { panic(err) }
	assert first != third
	assert second == third
	assert third[0..4] == [u8(0x7f), 0x45, 0x4c, 0x46]
	assert object.text == before_text
	assert object.private_data == before_data
	assert object.call_relocations.len == 1
	assert object.call_relocations[0] == before_relocation
}

fn test_elf_tiny_legacy_leaf_bytes_are_stable_across_fresh_equivalent_objects() {
	first, first_entry := elf_tiny_runtime_test_leaf('stable_entry')
	second, second_entry := elf_tiny_runtime_test_leaf('stable_entry')
	first_bytes := elf_tiny_executable_bytes(&first, ElfTinyEntryDefinition{
		function_index: u32(first_entry)
		result_policy:  .void_
	}) or { panic(err) }
	second_bytes := elf_tiny_executable_bytes(&second, ElfTinyEntryDefinition{
		function_index: u32(second_entry)
		result_policy:  .void_
	}) or { panic(err) }
	assert first_bytes == second_bytes
	assert first_bytes.len == 211
	assert elf_tiny_test_read_u16(first_bytes, 16) == elf_tiny_et_exec
	assert elf_tiny_test_read_u16(first_bytes, 56) == 2
	assert elf_tiny_test_read_u16(first_bytes, 60) == 0
	assert elf_tiny_test_read_u16(first_bytes, 62) == 0
}

fn test_elf_tiny_legacy_object_data_gate_refuses_without_mutation() {
	mut o := Object.new()
	entry := o.intern_function_symbol('gated_entry') or { panic(err) }
	assert o.append_text([u8(0xc3)]) or { panic(err) } == 0
	o.define_text_function(entry, 0, 1) or { panic(err) }
	definition := ObjectDataDefinition{
		sections: [
			ObjectDataSection{
				kind:      .rodata
				bytes:     [u8(1)]
				size:      1
				alignment: 1
			},
		]
		symbols:  [
			ObjectDataSymbol{
				kind:    .named
				name:    'gated_value'
				section: .rodata
				size:    1
			},
		]
	}
	elf_tiny_runtime_test_install_data(mut o, &definition)
	before := elf_tiny_runtime_test_clone_object(&o)
	assert elf_tiny_test_error(&o, ElfTinyEntryDefinition{
		function_index: u32(entry)
		result_policy:  .void_
	}).contains('requires explicit object-format writer support')
	assert o == before
}

fn test_elf_tiny_runtime_t01_startup_manifests_and_exit_mapping_are_exact() {
	void_bytes := elf_tiny_startup_bytes(.void_)
	scalar_bytes := elf_tiny_startup_bytes(.scalar)
	assert void_bytes.len == 18
	assert scalar_bytes.len == 18
	assert sha256.sum256(void_bytes).hex() == '4c9e9ca4afe7d571b9cadf861187a8a1b9035d1bae04701f58026d26e592b753'
	assert sha256.sum256(scalar_bytes).hex() == 'b10167d4a6eb1d5d1b54cafec27dd9bae34979e80a62eaaa5c6439033086bb0b'
	assert void_bytes[..7] == scalar_bytes[..7]
	assert void_bytes[7..9] == [u8(0x31), 0xff]
	assert scalar_bytes[7..9] == [u8(0x89), 0xc7]
	assert void_bytes[3..7] == [u8(0), 0, 0, 0]
	assert elf_tiny_test_read_u32(void_bytes, 10) == elf_tiny_sys_exit_group
	assert void_bytes[14..] == [u8(0x0f), 0x05, 0x0f, 0x0b]
}

fn test_elf_tiny_runtime_t02_helper_hashes_lengths_fixups_and_syscall_manifests_are_exact() {
	write := elf_tiny_runtime_write_manifest() or { panic(err) }
	flush := elf_tiny_runtime_flush_manifest() or { panic(err) }
	exit := elf_tiny_runtime_exit_manifest() or { panic(err) }
	mmap := elf_tiny_runtime_mmap_manifest() or { panic(err) }
	i64_decimal := elf_tiny_runtime_i64_manifest() or { panic(err) }
	i32_decimal := elf_tiny_runtime_i32_manifest() or { panic(err) }
	rune_utf8 := elf_tiny_runtime_rune_manifest() or { panic(err) }
	concat := elf_tiny_runtime_concat_manifest() or { panic(err) }
	assert write.bytes.len == 120
	assert flush.bytes.len == 3
	assert exit.bytes.len == 9
	assert mmap.bytes.len == 58
	assert i64_decimal.bytes.len == 149
	assert i32_decimal.bytes.len == 8
	assert rune_utf8.bytes.len == 265
	assert concat.bytes.len == 171
	assert sha256.sum256(write.bytes).hex() == 'f063033cfe65dc1a4d0c7f0c82295e50297a7af106d4b7376c0ddf0f65204f4c'
	assert sha256.sum256(flush.bytes).hex() == '251447ee91a9067dcd6ab96703133f617565974cd6c4819021760c4688c91abf'
	assert sha256.sum256(exit.bytes).hex() == 'b269e85e2ddba0398ab6b10028e4f7cf616f13b7ff9368fa986c4d4afd29ad28'
	assert sha256.sum256(mmap.bytes).hex() == '3416e1def64adda5898486bcb155275c5841ce0b4b5125a6d939e0adb666c61f'
	assert sha256.sum256(i64_decimal.bytes).hex() == '3fab08373610cf20916facc8048f8436a71ddc207513cc5bb6feadb974ba425a'
	assert sha256.sum256(i32_decimal.bytes).hex() == 'ecf7464ebdbc8a1be0d05805c253d696c06a535bedf163a8f125cd65fd475ead'
	assert sha256.sum256(rune_utf8.bytes).hex() == '6ee82b3368cb11e8ac0e89ff4e8c986e3d00bcc25887ff4c4cfd84ac4ebb7d2b'
	assert sha256.sum256(concat.bytes).hex() == '0cc430b9c27089d4983582a1db1f44958e5984523f372321bec2f7176b86756b'
	assert write.cross_fixups == [
		ElfTinyRuntimeCrossFixup{
			field:  114
			target: .exit_group
		},
	]
	assert i64_decimal.cross_fixups == [
		ElfTinyRuntimeCrossFixup{
			field:  7
			target: .mmap_alloc
		},
		ElfTinyRuntimeCrossFixup{
			field:  26
			target: .exit_group
		},
	]
	assert i32_decimal.cross_fixups == [
		ElfTinyRuntimeCrossFixup{
			field:  4
			target: .i64_decimal
		},
	]
	assert rune_utf8.cross_fixups == [
		ElfTinyRuntimeCrossFixup{
			field:  20
			target: .mmap_alloc
		},
		ElfTinyRuntimeCrossFixup{
			field:  39
			target: .exit_group
		},
	]
	rune_implementation := elf_tiny_runtime_role_implementation(.rune_utf8) or { panic(err) }
	assert rune_implementation == .rune_utf8
	assert concat.cross_fixups.map(it.field) == [u64(41), 60]
	assert elf_tiny_test_read_u32(write.bytes, 19) == elf_tiny_runtime_sys_write
	assert elf_tiny_test_read_u32(mmap.bytes, 27) == elf_tiny_runtime_sys_mmap
	assert elf_tiny_test_read_u32(exit.bytes, 1) == elf_tiny_sys_exit_group
}

fn test_elf_tiny_runtime_t03_rx_rw_rodata_private_data_and_bss_layout_formulas_are_exact() {
	mut o := Object.new()
	entry := o.intern_function_symbol('layout_entry') or { panic(err) }
	private_plan := private_data_preflight([
		PrivateDataDefinition{
			name:      'private_slot'
			value:     0x0102_0304_0506_0708
			width:     64
			alignment: 8
		},
	], ['layout_entry']) or { panic(err) }
	o.install_private_data(&private_plan) or { panic(err) }
	assert o.append_text([]u8{len: 13, init: if index == 12 { u8(0xc3) } else { u8(0) }}) or {
		panic(err)
	} == 0
	o.define_text_function(entry, 0, 13) or { panic(err) }
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     [u8(90), 91, 92, 93, 10, 11, 12, 13, 94, 95, 96, 97, 98, 99, 100, 101]
				size:      16
				alignment: 8
			},
			ObjectDataSection{
				kind:      .data
				bytes:     [u8(70), 71, 72, 73, 74, 75, 76, 77, 20, 21, 22, 23, 24, 25, 26, 27]
				size:      16
				alignment: 16
			},
			ObjectDataSection{
				kind:      .bss
				size:      32
				alignment: 32
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'ro_value'
				section: .rodata
				offset:  4
				size:    4
			},
			ObjectDataSymbol{
				kind:    .named
				name:    'rw_value'
				section: .data
				offset:  8
				size:    8
			},
			ObjectDataSymbol{
				kind:    .named
				name:    'zero_value'
				section: .bss
				offset:  16
				size:    8
			},
		]
		relocations: [
			elf_tiny_runtime_test_pc_relocation(0, 0, -4),
			elf_tiny_runtime_test_pc_relocation(4, 1, 0),
			elf_tiny_runtime_test_pc_relocation(8, 2, 0),
		]
	}
	elf_tiny_runtime_test_install_data(mut o, &definition)
	runtime_definition := elf_tiny_runtime_test_definition(entry, .void_, [])
	plan := elf_tiny_runtime_build_plan(&o, runtime_definition) or { panic(err) }
	assert plan.layout.program_header_count == 3
	assert plan.layout.entry_offset == 240
	assert plan.layout.function_offset == 272
	assert plan.layout.text_end == 285
	assert plan.layout.rodata_offset == 288
	assert plan.layout.rx_file_size == 296
	assert plan.layout.rw_offset == 4096
	assert plan.layout.object_data_base == 16
	assert plan.layout.bss_base == 32
	assert plan.layout.rw_file_size == 32
	assert plan.layout.rw_mem_size == 56
	assert plan.layout.file_size == 4128
	assert plan.data_plan.sections.map(it.kind) == [.rodata, .data, .bss]
	assert plan.data_plan.sections[0].bytes == [u8(0), 0, 0, 0, 10, 11, 12, 13]
	assert plan.data_plan.sections[1].bytes == [u8(0), 0, 0, 0, 0, 0, 0, 0, 20, 21, 22, 23, 24,
		25, 26, 27]
	assert plan.data_plan.sections[2].bytes.len == 0
	assert plan.data_plan.sections[2].size == 24

	bytes := elf_tiny_runtime_executable_bytes(&o, runtime_definition) or { panic(err) }
	assert bytes.len == 4128
	headers := elf_tiny_test_program_headers(bytes)
	assert headers.len == 3
	assert headers[0].type_ == elf_tiny_pt_load
	assert headers[0].flags == elf_tiny_pf_r | elf_tiny_pf_x
	assert headers[0].file_size == 296 && headers[0].mem_size == 296
	assert headers[1].type_ == elf_tiny_pt_load
	assert headers[1].flags == elf_tiny_pf_r | elf_tiny_pf_w
	assert headers[1].offset == 4096
	assert headers[1].vaddr == elf_tiny_base_vaddr + 4096
	assert headers[1].file_size == 32 && headers[1].mem_size == 56
	assert headers[2].type_ == elf_tiny_pt_gnu_stack
	assert headers[2].flags == elf_tiny_pf_r | elf_tiny_pf_w
	assert bytes[288..296] == plan.data_plan.sections[0].bytes
	assert bytes[4096..4104] == o.private_data
	elf_tiny_test_assert_zero_range(bytes, 4104, 4112)
	assert bytes[4112..4128] == plan.data_plan.sections[1].bytes
}

fn test_elf_tiny_runtime_t04_startup_entry_and_result_refusals_are_transactional() {
	o, entry := elf_tiny_runtime_test_leaf('entry')
	before := elf_tiny_runtime_test_clone_object(&o)
	assert elf_tiny_runtime_test_error(&o, ElfTinyRuntimeDefinition{
		entry: ElfTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .void_
		}
	}).contains('startup policy')
	assert elf_tiny_runtime_test_error(&o, ElfTinyRuntimeDefinition{
		entry:          ElfTinyEntryDefinition{
			function_index:  u32(entry)
			parameter_count: 1
			result_policy:   .void_
		}
		startup_policy: .no_args_no_init
	}).contains('must not accept parameters')
	assert elf_tiny_runtime_test_error(&o, ElfTinyRuntimeDefinition{
		entry:          ElfTinyEntryDefinition{
			function_index: 99
			result_policy:  .void_
		}
		startup_policy: .no_args_no_init
	}).contains('out of range')
	assert elf_tiny_runtime_test_error(&o, ElfTinyRuntimeDefinition{
		entry:          ElfTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  unsafe { ElfTinyEntryResultPolicy(255) }
		}
		startup_policy: .no_args_no_init
	}).contains('result policy 255')
	assert o == before
}

fn test_elf_tiny_runtime_t05_roles_require_unique_reachable_intentional_external_ids() {
	mut o := Object.new()
	entry := o.intern_function_symbol('entry') or { panic(err) }
	first := o.intern_external_function_symbol('arbitrary_alpha') or { panic(err) }
	second := o.intern_external_function_symbol('arbitrary_beta') or { panic(err) }
	assert o.append_text([u8(0xe8), 0, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	o.define_text_function(entry, 0, 11) or { panic(err) }
	o.add_text_call_relocation(1, first) or { panic(err) }
	o.add_text_call_relocation(6, second) or { panic(err) }
	valid := elf_tiny_runtime_test_definition(entry, .void_, [
		ElfTinyRuntimeBinding{
			external_function_index: u32(first)
			role:                    .i64_decimal
		},
		ElfTinyRuntimeBinding{
			external_function_index: u32(second)
			role:                    .string_concat
		},
	])
	plan := elf_tiny_runtime_build_plan(&o, valid) or { panic(err) }
	for implementation in [ElfTinyRuntimeImplementation.mmap_alloc, .exit_group, .i64_decimal,
		.string_concat] {
		assert plan.implementation_used[int(implementation)]
	}
	_ = elf_tiny_runtime_executable_bytes(&o, valid) or { panic(err) }
	duplicate_id := ElfTinyRuntimeDefinition{
		...valid
		runtime_bindings: [
			ElfTinyRuntimeBinding{
				external_function_index: u32(first)
				role:                    .i64_decimal
			},
			ElfTinyRuntimeBinding{
				external_function_index: u32(first)
				role:                    .string_concat
			},
		]
	}
	assert elf_tiny_runtime_test_error(&o, duplicate_id).contains('bound more than once')
	duplicate_role := ElfTinyRuntimeDefinition{
		...valid
		runtime_bindings: [
			ElfTinyRuntimeBinding{
				external_function_index: u32(first)
				role:                    .i64_decimal
			},
			ElfTinyRuntimeBinding{
				external_function_index: u32(second)
				role:                    .i64_decimal
			},
		]
	}
	assert elf_tiny_runtime_test_error(&o, duplicate_role).contains('role is bound more than once')
	missing := ElfTinyRuntimeDefinition{
		...valid
		runtime_bindings: [valid.runtime_bindings[0]]
	}
	assert elf_tiny_runtime_test_error(&o, missing).contains('has no binding')

	mut stale := Object.new()
	stale_entry := stale.intern_function_symbol('stale_entry') or { panic(err) }
	dead := stale.intern_function_symbol('dead') or { panic(err) }
	stale_external := stale.intern_external_function_symbol('stale_external') or { panic(err) }
	assert stale.append_text([u8(0xc3), 0xe8, 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	stale.define_text_function(stale_entry, 0, 1) or { panic(err) }
	stale.define_text_function(dead, 1, 6) or { panic(err) }
	stale.add_text_call_relocation(2, stale_external) or { panic(err) }
	assert elf_tiny_runtime_test_error(&stale, elf_tiny_runtime_test_definition(stale_entry,
		.void_, [
		ElfTinyRuntimeBinding{
			external_function_index: u32(stale_external)
			role:                    .write_all
		},
	])).contains('not reachable')
}

fn test_elf_tiny_runtime_t06_write_all_manifest_covers_retry_progress_and_fail_closed_paths() {
	manifest := elf_tiny_runtime_write_manifest() or { panic(err) }
	for field, target in {
		14: 100
		36: 61
		42: 104
		52: 18
		57: 104
		66: 104
		75: 104
		87: 104
		96: 18
	} {
		assert elf_tiny_runtime_test_rel32_target(manifest.bytes, field) == target
	}
	assert manifest.bytes[18..23] == [u8(0xb8), 1, 0, 0, 0]
	assert manifest.bytes[34..40] == [u8(0x0f), 0x8f, 0x15, 0, 0, 0]
	assert manifest.bytes[46..50] == [u8(0x48), 0x83, 0xf8, 0xfc]
	assert manifest.bytes[100..104] == [u8(0x4c), 0x89, 0xd0, 0xc3]
	assert manifest.bytes[104..113] == [u8(0xbf), 1, 0, 0, 0, 0x48, 0x83, 0xec, 0x08]
	o, entry, external := elf_tiny_runtime_test_helper_object('not_inferred_write')
	definition := elf_tiny_runtime_test_definition(entry, .void_, [
		ElfTinyRuntimeBinding{
			external_function_index: u32(external)
			role:                    .write_all
		},
	])
	plan := elf_tiny_runtime_build_plan(&o, definition) or { panic(err) }
	linked := elf_tiny_runtime_build_linked_text(&o, definition, &plan) or { panic(err) }
	write_offset := plan.layout.entry_offset +
		u64(linked.implementation_offsets[int(ElfTinyRuntimeImplementation.write_all)] -
		plan.layout.entry_offset)
	exit_offset := linked.implementation_offsets[int(ElfTinyRuntimeImplementation.exit_group)]
	field := int(write_offset - plan.layout.entry_offset + 114)
	assert elf_tiny_runtime_test_rel32_target(linked.bytes, field) == int(exit_offset - plan.layout.entry_offset)
}

fn test_elf_tiny_runtime_t07_integer_and_rune_helpers_cover_full_domains_and_dependencies() {
	assert min_i64.str() == '-9223372036854775808'
	assert i64(0).str() == '0'
	assert max_i64.str() == '9223372036854775807'
	i64_manifest := elf_tiny_runtime_i64_manifest() or { panic(err) }
	assert i64_manifest.bytes[30..34] == [u8(0x0f), 0x0b, 0x5f, 0x4c]
	assert i64_manifest.bytes[74..87] == [u8(0x49), 0xff, 0xc8, 0x41, 0xc6, 0x00, 0x30, 0x41, 0xb9,
		1, 0, 0, 0]
	i32_manifest := elf_tiny_runtime_i32_manifest() or { panic(err) }
	assert i32_manifest.bytes[..4] == [u8(0x48), 0x63, 0xff, 0xe9]
	rune_manifest := elf_tiny_runtime_rune_manifest() or { panic(err) }
	assert rune_manifest.bytes[..7] == [u8(0x89), 0xf8, 0x3d, 0xff, 0xff, 0x10, 0]
	assert elf_tiny_runtime_test_rel32_target(rune_manifest.bytes, 9) == 260
	assert rune_manifest.bytes.last() == 0xc3
	mut o := Object.new()
	entry := o.intern_function_symbol('entry') or { panic(err) }
	i32_external := o.intern_external_function_symbol('i32_role') or { panic(err) }
	rune_external := o.intern_external_function_symbol('rune_role') or { panic(err) }
	assert o.append_text([u8(0xe8), 0, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	o.define_text_function(entry, 0, 11) or { panic(err) }
	o.add_text_call_relocation(1, i32_external) or { panic(err) }
	o.add_text_call_relocation(6, rune_external) or { panic(err) }
	plan := elf_tiny_runtime_build_plan(&o, elf_tiny_runtime_test_definition(entry, .void_, [
		ElfTinyRuntimeBinding{
			external_function_index: u32(i32_external)
			role:                    .i32_decimal
		},
		ElfTinyRuntimeBinding{
			external_function_index: u32(rune_external)
			role:                    .rune_utf8
		},
	])) or { panic(err) }
	for implementation in [ElfTinyRuntimeImplementation.i32_decimal, .i64_decimal, .rune_utf8,
		.mmap_alloc, .exit_group] {
		assert plan.implementation_used[int(implementation)]
	}
}

fn test_elf_tiny_runtime_t08_concat_and_mmap_manifests_pin_length_overflow_null_and_errno_paths() {
	assert elf_tiny_runtime_test_concat_allocation(0, 0) or { panic(err) } == 1
	assert elf_tiny_runtime_test_concat_allocation(max_u32, 0) or { panic(err) } == u64(max_u32) + 1
	for pair in [[u64(max_u32), u64(1)], [u64(1), u64(max_u32)],
		[u64(max_u32) + 1, u64(0)]] {
		if _ := elf_tiny_runtime_test_concat_allocation(pair[0], pair[1]) {
			assert false, 'overflowing concat length was accepted'
		} else {
			assert err.msg() == 'length overflow'
		}
	}
	concat := elf_tiny_runtime_concat_manifest() or { panic(err) }
	assert elf_tiny_runtime_test_rel32_target(concat.bytes, 20) == 54
	assert elf_tiny_runtime_test_rel32_target(concat.bytes, 33) == 54
	assert elf_tiny_runtime_test_rel32_target(concat.bytes, 50) == 66
	assert concat.bytes[54..66] == [u8(0xbf), 1, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0x0f, 0x0b]
	mmap := elf_tiny_runtime_mmap_manifest() or { panic(err) }
	assert mmap.bytes[0..33] == [u8(0x48), 0x89, 0xfe, 0x31, 0xff, 0xba, 3, 0, 0, 0, 0x41, 0xba,
		0x22, 0, 0, 0, 0x49, 0xc7, 0xc0, 0xff, 0xff, 0xff, 0xff, 0x45, 0x31, 0xc9, 0xb8, 9, 0,
		0, 0, 0x0f, 0x05]
	assert elf_tiny_runtime_test_rel32_target(mmap.bytes, 41) == 55
	assert elf_tiny_runtime_test_rel32_target(mmap.bytes, 50) == 55
	assert mmap.bytes[55..] == [u8(0x31), 0xc0, 0xc3]
}

fn test_elf_tiny_runtime_t09_global_relocation_classification_precedes_reachability_pruning() {
	mut refused := Object.new()
	entry := refused.intern_function_symbol('entry') or { panic(err) }
	dead := refused.intern_function_symbol('dead') or { panic(err) }
	assert refused.append_text([u8(0xc3), 0, 0, 0, 0, 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	refused.define_text_function(entry, 0, 1) or { panic(err) }
	refused.define_text_function(dead, 1, 9) or { panic(err) }
	absolute_definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     []u8{len: 8}
				size:      8
				alignment: 8
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'dead_data'
				section: .rodata
				size:    8
			},
		]
		relocations: [
			ObjectDataRelocation{
				source_section: .text
				offset:         1
				target_symbol:  object_data_symbol_ref(0)
				width:          64
				kind:           .absolute
				signedness:     .unsigned
				address_intent: .virtual_address
				pc_bias:        .zero
				got_access:     .none
			},
		]
	}
	elf_tiny_runtime_test_install_data(mut refused, &absolute_definition)
	before := elf_tiny_runtime_test_clone_object(&refused)
	assert elf_tiny_runtime_test_error(&refused,
		elf_tiny_runtime_test_definition(entry, .void_, [])).contains('relocation 0 is unsupported')
	assert refused == before

	mut pruned := Object.new()
	live_entry := pruned.intern_function_symbol('live_entry') or { panic(err) }
	dead_data_user := pruned.intern_function_symbol('dead_data_user') or { panic(err) }
	assert pruned.append_text([u8(0xc3), 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	pruned.define_text_function(live_entry, 0, 1) or { panic(err) }
	pruned.define_text_function(dead_data_user, 1, 5) or { panic(err) }
	supported_definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     []u8{len: 8}
				size:      8
				alignment: 4
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'pruned_data'
				section: .rodata
				size:    8
			},
		]
		relocations: [elf_tiny_runtime_test_pc_relocation(1, 0, 0)]
	}
	elf_tiny_runtime_test_install_data(mut pruned, &supported_definition)
	plan := elf_tiny_runtime_build_plan(&pruned, elf_tiny_runtime_test_definition(live_entry,
		.void_, [])) or { panic(err) }
	assert plan.selected_functions == [int(live_entry)]
	assert plan.data_plan.sections.len == 0
	assert plan.data_plan.symbols.len == 0
	bytes := elf_tiny_runtime_executable_bytes(&pruned, elf_tiny_runtime_test_definition(live_entry,
		.void_, [])) or { panic(err) }
	assert u64(bytes.len) == plan.layout.rx_file_size
	assert elf_tiny_test_program_headers(bytes).len == 2
}

fn test_elf_tiny_runtime_t10_alias_pruning_keeps_only_target_ancestry_and_preserves_s_plus_a() {
	mut o := Object.new()
	entry := o.intern_function_symbol('alias_entry') or { panic(err) }
	assert o.append_text([u8(0), 0, 0, 0, 0xc3]) or { panic(err) } == 0
	o.define_text_function(entry, 0, 5) or { panic(err) }
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     [u8(90), 91, 92, 93, 1, 2, 3, 4, 5, 6, 7, 8, 94, 95, 96, 97]
				size:      16
				alignment: 4
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'root'
				section: .rodata
				offset:  4
				size:    4
			},
			ObjectDataSymbol{
				kind:     .named
				name:     'unrelated_sibling'
				section:  .rodata
				offset:   4
				size:     4
				alias_of: object_data_symbol_ref(0)
			},
			ObjectDataSymbol{
				kind:     .named
				name:     'parent'
				section:  .rodata
				offset:   4
				size:     4
				alias_of: object_data_symbol_ref(0)
			},
			ObjectDataSymbol{
				kind:     .named
				name:     'leaf'
				section:  .rodata
				offset:   4
				size:     4
				alias_of: object_data_symbol_ref(2)
			},
			ObjectDataSymbol{
				kind:    .named
				name:    'other_root'
				section: .rodata
				offset:  8
				size:    4
			},
			ObjectDataSymbol{
				kind:     .named
				name:     'other_child'
				section:  .rodata
				offset:   8
				size:     4
				alias_of: object_data_symbol_ref(4)
			},
		]
		relocations: [elf_tiny_runtime_test_pc_relocation(0, 3, -4)]
	}
	elf_tiny_runtime_test_install_data(mut o, &definition)
	runtime_definition := elf_tiny_runtime_test_definition(entry, .void_, [])
	plan := elf_tiny_runtime_build_plan(&o, runtime_definition) or { panic(err) }
	assert plan.data_plan.symbol_kept == [true, false, true, true, false, false]
	assert plan.data_plan.symbols.map(it.name) == ['root', 'parent', 'leaf']
	assert plan.data_plan.old_to_new[3] == ObjectDataSymbolID(2)
	assert !plan.data_plan.symbols[0].alias_of.is_set
	assert plan.data_plan.symbols[1].alias_of == object_data_symbol_ref(0)
	assert plan.data_plan.symbols[2].alias_of == object_data_symbol_ref(1)
	assert plan.data_plan.sections[0].bytes == [u8(1), 2, 3, 4]
	assert plan.data_plan.symbols.map(it.offset) == [u64(0), 0, 0]
	linked := elf_tiny_runtime_build_linked_text(&o, runtime_definition, &plan) or { panic(err) }
	field_offset := linked.function_offsets[int(entry)]
	field_index := int(field_offset - plan.layout.entry_offset)
	raw := elf_tiny_test_read_u32(linked.bytes, field_index)
	field_vaddr := elf_tiny_base_vaddr + field_offset
	target_vaddr := elf_tiny_runtime_data_symbol_vaddr(&plan.data_plan, &plan.layout,
		ObjectDataSymbolID(2)) or { panic(err) }
	assert i64(field_vaddr) + 4 + i64(i32(raw)) == i64(target_vaddr)
}

fn test_elf_tiny_runtime_t11_adjacent_intervals_coalesce_with_alignment_congruence() {
	mut o := Object.new()
	entry := o.intern_function_symbol('adjacent_entry') or { panic(err) }
	assert o.append_text([u8(0), 0, 0, 0, 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	o.define_text_function(entry, 0, 9) or { panic(err) }
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .data
				bytes:     [u8(90), 91, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 94, 95]
				size:      16
				alignment: 8
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'left'
				section: .data
				offset:  2
				size:    6
			},
			ObjectDataSymbol{
				kind:    .named
				name:    'right'
				section: .data
				offset:  8
				size:    6
			},
		]
		relocations: [
			elf_tiny_runtime_test_pc_relocation(0, 0, 0),
			elf_tiny_runtime_test_pc_relocation(4, 1, 0),
		]
	}
	elf_tiny_runtime_test_install_data(mut o, &definition)
	plan := elf_tiny_runtime_build_plan(&o, elf_tiny_runtime_test_definition(entry, .void_, [])) or {
		panic(err)
	}
	assert plan.data_plan.sections.len == 1
	assert plan.data_plan.sections[0].kind == .data
	assert plan.data_plan.sections[0].size == 14
	assert plan.data_plan.sections[0].bytes == [u8(0), 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]
	assert plan.data_plan.symbols.map(it.offset) == [u64(2), 8]
	assert plan.data_plan.symbols[0].offset % 8 == 2
	assert plan.data_plan.symbols[1].offset % 8 == 0
}

fn test_elf_tiny_runtime_t12_addends_are_bounded_to_the_target_half_open_interval() {
	mut base := Object.new()
	entry := base.intern_function_symbol('addend_entry') or { panic(err) }
	assert base.append_text([u8(0), 0, 0, 0, 0xc3]) or { panic(err) } == 0
	base.define_text_function(entry, 0, 5) or { panic(err) }
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     []u8{len: 8}
				size:      8
				alignment: 4
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'bounded'
				section: .rodata
				offset:  4
				size:    4
			},
		]
		relocations: [elf_tiny_runtime_test_pc_relocation(0, 0, -4)]
	}
	elf_tiny_runtime_test_install_data(mut base, &definition)
	runtime_definition := elf_tiny_runtime_test_definition(entry, .void_, [])
	for addend in [i64(-5), -4, -3, -1, 0, max_i64] {
		mut candidate := elf_tiny_runtime_test_clone_object(&base)
		candidate.object_data.relocations[0].addend = addend
		before := elf_tiny_runtime_test_clone_object(&candidate)
		if addend >= -4 && addend <= -1 {
			plan := elf_tiny_runtime_build_plan(&candidate, runtime_definition) or { panic(err) }
			linked := elf_tiny_runtime_build_linked_text(&candidate, runtime_definition, &plan) or {
				panic(err)
			}
			field_offset := linked.function_offsets[int(entry)]
			raw := elf_tiny_test_read_u32(linked.bytes,
				int(field_offset - plan.layout.entry_offset))
			target_vaddr := elf_tiny_runtime_data_symbol_vaddr(&plan.data_plan, &plan.layout,
				ObjectDataSymbolID(0)) or { panic(err) }
			assert i64(elf_tiny_base_vaddr + field_offset) + 4 + i64(i32(raw)) ==
				i64(target_vaddr) + addend + 4
		} else {
			message := elf_tiny_runtime_test_error(&candidate, runtime_definition)
			assert message.contains('effective target') || message.contains('escapes')
		}
		assert candidate == before
	}
}

fn test_elf_tiny_runtime_t13_only_signed_width32_zero_bias_elf_pc32_is_accepted() {
	mut base := Object.new()
	entry := base.intern_function_symbol('mapping_entry') or { panic(err) }
	assert base.append_text([u8(0), 0, 0, 0, 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	base.define_text_function(entry, 0, 9) or { panic(err) }
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     []u8{len: 8}
				size:      8
				alignment: 4
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'mapped'
				section: .rodata
				size:    8
			},
		]
		relocations: [elf_tiny_runtime_test_pc_relocation(0, 0, 0)]
	}
	elf_tiny_runtime_test_install_data(mut base, &definition)
	runtime_definition := elf_tiny_runtime_test_definition(entry, .void_, [])
	_ = elf_tiny_runtime_executable_bytes(&base, runtime_definition) or { panic(err) }

	mut absolute64 := elf_tiny_runtime_test_clone_object(&base)
	absolute64.object_data.relocations[0] = ObjectDataRelocation{
		source_section: .text
		offset:         0
		target_symbol:  object_data_symbol_ref(0)
		width:          64
		kind:           .absolute
		signedness:     .unsigned
		address_intent: .virtual_address
		pc_bias:        .zero
		got_access:     .none
	}
	assert elf_tiny_runtime_test_error(&absolute64, runtime_definition).contains('unsupported')
	mut absolute32 := elf_tiny_runtime_test_clone_object(&base)
	absolute32.object_data.relocations[0].kind = .absolute
	absolute32.object_data.relocations[0].signedness = .unsigned
	assert elf_tiny_runtime_test_error(&absolute32, runtime_definition).contains('unsupported')
	mut absolute32s := elf_tiny_runtime_test_clone_object(&base)
	absolute32s.object_data.relocations[0].kind = .absolute
	assert elf_tiny_runtime_test_error(&absolute32s, runtime_definition).contains('unsupported')
	mut got := elf_tiny_runtime_test_clone_object(&base)
	got.object_data.relocations[0].kind = .got_relative
	got.object_data.relocations[0].got_access = .load
	assert elf_tiny_runtime_test_error(&got, runtime_definition).contains('unsupported')
	mut biased := elf_tiny_runtime_test_clone_object(&base)
	biased.object_data.relocations[0].pc_bias = .one
	assert elf_tiny_runtime_test_error(&biased, runtime_definition).contains('no elf_x86_64 mapping')
	mut data_source := elf_tiny_runtime_test_clone_object(&base)
	data_source.object_data.relocations[0].source_section = .rodata
	assert elf_tiny_runtime_test_error(&data_source, runtime_definition).contains('originate in .text')
}

fn test_elf_tiny_runtime_t14_bfs_pruning_id_order_and_role_mapping_are_deterministic() {
	mut o := Object.new()
	entry := o.intern_function_symbol('entry') or { panic(err) }
	dead := o.intern_function_symbol('dead') or { panic(err) }
	callee := o.intern_function_symbol('callee') or { panic(err) }
	write_external := o.intern_external_function_symbol('not_named_write') or { panic(err) }
	dead_external := o.intern_external_function_symbol('dead_external') or { panic(err) }
	assert o.append_text([
		u8(0xe8),
		0,
		0,
		0,
		0,
		0xe8,
		0,
		0,
		0,
		0,
		0xc3,
		0xe8,
		0,
		0,
		0,
		0,
		0xc3,
		0x31,
		0xc0,
		0xc3,
	]) or { panic(err) } == 0
	o.define_text_function(entry, 0, 11) or { panic(err) }
	o.define_text_function(dead, 11, 6) or { panic(err) }
	o.define_text_function(callee, 17, 3) or { panic(err) }
	o.add_text_call_relocation(1, callee) or { panic(err) }
	o.add_text_call_relocation(6, write_external) or { panic(err) }
	o.add_text_call_relocation(12, dead_external) or { panic(err) }
	definition := elf_tiny_runtime_test_definition(entry, .scalar, [
		ElfTinyRuntimeBinding{
			external_function_index: u32(write_external)
			role:                    .write_all
		},
	])
	plan := elf_tiny_runtime_build_plan(&o, definition) or { panic(err) }
	assert plan.selected_functions == [int(entry), int(callee)]
	assert plan.reachability.functions == [true, false, true, false, false]
	assert plan.reachability.externals == [false, false, false, true, false]
	linked := elf_tiny_runtime_build_linked_text(&o, definition, &plan) or { panic(err) }
	assert linked.function_offsets[int(callee)] == linked.function_offsets[int(entry)] + 11
	first := elf_tiny_runtime_executable_bytes(&o, definition) or { panic(err) }
	second := elf_tiny_runtime_executable_bytes(&o, definition) or { panic(err) }
	assert first == second
	assert o.text[1..5] == [u8(0), 0, 0, 0]
	assert o.text[6..10] == [u8(0), 0, 0, 0]
	assert o.text[12..16] == [u8(0), 0, 0, 0]
}

fn test_elf_tiny_runtime_t15_caps_alignment_and_nobits_boundaries_refuse_before_allocation() {
	mut bss := Object.new()
	entry := bss.intern_function_symbol('bss_entry') or { panic(err) }
	assert bss.append_text([u8(0), 0, 0, 0, 0xc3]) or { panic(err) } == 0
	bss.define_text_function(entry, 0, 5) or { panic(err) }
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .bss
				size:      elf_tiny_runtime_alloc_cap
				alignment: elf_tiny_page_align
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'large_bss'
				section: .bss
				size:    elf_tiny_runtime_alloc_cap
			},
		]
		relocations: [elf_tiny_runtime_test_pc_relocation(0, 0, 0)]
	}
	elf_tiny_runtime_test_install_data(mut bss, &definition)
	runtime_definition := elf_tiny_runtime_test_definition(entry, .void_, [])
	bytes := elf_tiny_runtime_executable_bytes(&bss, runtime_definition) or { panic(err) }
	headers := elf_tiny_test_program_headers(bytes)
	assert headers.len == 3
	assert headers[1].file_size == 0
	assert headers[1].mem_size == elf_tiny_runtime_alloc_cap
	assert headers[1].offset == 4096
	assert u64(bytes.len) == headers[0].file_size
	assert bss.object_data.sections[0].bytes.len == 0

	mut too_large := elf_tiny_runtime_test_clone_object(&bss)
	too_large.object_data.sections[0].size = elf_tiny_runtime_alloc_cap + 1
	too_large.object_data.symbols[0].size = elf_tiny_runtime_alloc_cap + 1
	before := elf_tiny_runtime_test_clone_object(&too_large)
	assert elf_tiny_runtime_test_error(&too_large, runtime_definition).contains('exceeds 0x80000000')
	assert too_large == before
	mut over_aligned := elf_tiny_runtime_test_clone_object(&bss)
	over_aligned.object_data.sections[0].alignment = elf_tiny_page_align * 2
	assert elf_tiny_runtime_test_error(&over_aligned, runtime_definition).contains('alignment 8192 is unsupported')
	elf_tiny_runtime_check_cap(elf_tiny_runtime_alloc_cap, 'boundary') or { panic(err) }
	if _ := elf_tiny_runtime_check_cap(elf_tiny_runtime_alloc_cap + 1, 'boundary') {
		assert false, 'oversized allocation endpoint was accepted'
	}
}

fn test_elf_tiny_runtime_t16_legacy_bytes_and_closed_elf_object_adjacency_are_preserved() {
	leaf, entry := elf_tiny_runtime_test_leaf('legacy_entry')
	legacy := elf_tiny_executable_bytes(&leaf, ElfTinyEntryDefinition{
		function_index: u32(entry)
		result_policy:  .void_
	}) or { panic(err) }
	runtime := elf_tiny_runtime_executable_bytes(&leaf, elf_tiny_runtime_test_definition(entry,
		.void_, [])) or { panic(err) }
	assert runtime == legacy

	mut data_object := Object.new()
	data_entry := data_object.intern_function_symbol('data_entry') or { panic(err) }
	assert data_object.append_text([u8(0), 0, 0, 0, 0xc3]) or { panic(err) } == 0
	data_object.define_text_function(data_entry, 0, 5) or { panic(err) }
	data_definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     [u8(90), 91, 92, 93, 1, 2, 3, 4]
				size:      8
				alignment: 4
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'adjacent_value'
				section: .rodata
				offset:  4
				size:    4
			},
		]
		relocations: [elf_tiny_runtime_test_pc_relocation(0, 0, -4)]
	}
	elf_tiny_runtime_test_install_data(mut data_object, &data_definition)
	before := elf_tiny_runtime_test_clone_object(&data_object)
	assert elf_tiny_test_error(&data_object, ElfTinyEntryDefinition{
		function_index: u32(data_entry)
		result_policy:  .void_
	}).contains('requires explicit object-format writer support')
	relocatable := elf64_relocatable_bytes(&data_object) or { panic(err) }
	assert elf_tiny_test_read_u16(relocatable, 16) == 1
	direct := elf_tiny_runtime_executable_bytes(&data_object, elf_tiny_runtime_test_definition(data_entry,
		.void_, [])) or { panic(err) }
	assert elf_tiny_test_read_u16(direct, 16) == elf_tiny_et_exec
	assert elf_tiny_test_read_u16(direct, 60) == 0
	assert data_object == before
}

fn elf_tiny_runtime_test_emit_u32(mut bytes []u8, value u32) {
	bytes << u8(value)
	bytes << u8(value >> 8)
	bytes << u8(value >> 16)
	bytes << u8(value >> 24)
}

fn elf_tiny_runtime_test_emit_u64(mut bytes []u8, value u64) {
	for shift in 0 .. 8 {
		bytes << u8(value >> (shift * 8))
	}
}

fn elf_tiny_runtime_test_file_sha256(path string) string {
	bytes := os.read_bytes(path) or { return '' }
	return sha256.sum256(bytes).hex()
}

fn elf_tiny_runtime_test_exact_host() bool {
	$if linux && amd64 {
		uname := os.uname()
		if uname.sysname != 'Linux' || uname.release != '7.0.0-28-generic'
			|| uname.machine != 'x86_64' {
			return false
		}
		for path in ['/usr/bin/readelf', '/usr/bin/objdump', '/usr/bin/prlimit'] {
			if !os.is_executable(path) {
				return false
			}
		}
		if elf_tiny_runtime_test_file_sha256('/usr/bin/readelf') != 'c857339616bbbfa5eba32733e22365048903fbaf6ed2126b897dd138bcb741fc'
			|| elf_tiny_runtime_test_file_sha256('/usr/bin/objdump') != '44f07f8da860b15bd4dec909f229dec536595fb170a616fe3ab29c7b21c9736f'
			|| elf_tiny_runtime_test_file_sha256('/usr/bin/prlimit') != '00be18793391b9222a041277a088022cc58088690cb0e851383bd8ec73f0fefb' {
			return false
		}
		readelf_version := elf_tiny_runtime_test_run_process('/usr/bin/readelf', [
			'--version',
		], elf_tiny_runtime_test_timeout_ms)
		objdump_version := elf_tiny_runtime_test_run_process('/usr/bin/objdump', [
			'--version',
		], elf_tiny_runtime_test_timeout_ms)
		prlimit_version := elf_tiny_runtime_test_run_process('/usr/bin/prlimit', [
			'--version',
		], elf_tiny_runtime_test_timeout_ms)
		return readelf_version.exit_code == 0 && !readelf_version.timed_out
			&& !readelf_version.output_limited
			&& readelf_version.stdout.contains('GNU readelf (GNU Binutils for Ubuntu) 2.46')
			&& objdump_version.exit_code == 0 && !objdump_version.timed_out
			&& !objdump_version.output_limited
			&& objdump_version.stdout.contains('GNU objdump (GNU Binutils for Ubuntu) 2.46')
			&& prlimit_version.exit_code == 0 && !prlimit_version.timed_out
			&& !prlimit_version.output_limited
			&& prlimit_version.stdout.contains('prlimit from util-linux 2.41.3')
	} $else {
		return false
	}
}

fn elf_tiny_runtime_test_root(name string) string {
	root := os.join_path(os.temp_dir(), 'v3 elf tiny [${name}];${os.getpid()}')
	assert !os.exists(root), 'ELF tiny runtime test root was stale'
	os.mkdir(root) or { panic(err) }
	return root
}

fn elf_tiny_runtime_test_cleanup(root string) {
	if os.exists(root) {
		assert os.is_dir(root) && !os.is_link(root)
		os.rmdir_all(root) or { panic(err) }
	}
	assert !os.exists(root)
}

fn elf_tiny_runtime_test_publish_executable(root string, name string, bytes []u8) string {
	path := os.join_path(root, '${name} [direct];.elf')
	os.write_file_array(path, bytes) or { panic(err) }
	os.chmod(path, 0o700) or { panic(err) }
	return path
}

fn elf_tiny_runtime_test_run_executable(root string, name string, o &Object, definition ElfTinyRuntimeDefinition, expected_exit int, expected_stdout string) {
	bytes := elf_tiny_runtime_executable_bytes(o, definition) or { panic(err) }
	path := elf_tiny_runtime_test_publish_executable(root, name, bytes)
	result := elf_tiny_runtime_test_run_process('/usr/bin/prlimit', [
		'--as=67108864',
		'--',
		path,
	], elf_tiny_runtime_test_timeout_ms)
	assert !result.timed_out, '${name} timed out'
	assert !result.output_limited, '${name} exceeded output bound'
	assert result.exit_code == expected_exit, '${name}: exit ${result.exit_code}, stderr `${result.stderr}`'
	assert result.stdout == expected_stdout, '${name}: stdout `${result.stdout}`'
	assert result.stderr.len == 0, '${name}: stderr `${result.stderr}`'
}

fn elf_tiny_runtime_test_scalar_fixture(value u32, policy ElfTinyEntryResultPolicy) (Object, ElfTinyRuntimeDefinition) {
	mut o := Object.new()
	entry := o.intern_function_symbol('scalar_entry') or { panic(err) }
	mut text := [u8(0xb8)]
	elf_tiny_runtime_test_emit_u32(mut text, value)
	text << u8(0xc3)
	assert o.append_text(text) or { panic(err) } == 0
	o.define_text_function(entry, 0, u64(text.len)) or { panic(err) }
	return o, elf_tiny_runtime_test_definition(entry, policy, [])
}

fn elf_tiny_runtime_test_write_fixture(message []u8) (Object, ElfTinyRuntimeDefinition) {
	assert message.len > 0
	mut o := Object.new()
	entry := o.intern_function_symbol('write_entry') or { panic(err) }
	write_external := o.intern_external_function_symbol('declared_write') or { panic(err) }
	mut text := [u8(0x48), 0x83, 0xec, 0x08, 0xbf, 1, 0, 0, 0, 0x48, 0x8d, 0x35]
	data_field := u64(text.len)
	text << [u8(0), 0, 0, 0, 0xba]
	elf_tiny_runtime_test_emit_u32(mut text, u32(message.len))
	call_field := elf_tiny_runtime_emit_call(mut text)
	text << [u8(0x48), 0x83, 0xc4, 0x08, 0x31, 0xc0, 0xc3]
	assert o.append_text(text) or { panic(err) } == 0
	o.define_text_function(entry, 0, u64(text.len)) or { panic(err) }
	o.add_text_call_relocation(call_field, write_external) or { panic(err) }
	mut payload := [u8(0), 0, 0, 0]
	payload << message
	data_definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     payload
				size:      u64(payload.len)
				alignment: 4
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'message'
				section: .rodata
				offset:  4
				size:    u64(message.len)
			},
		]
		relocations: [elf_tiny_runtime_test_pc_relocation(data_field, 0, -4)]
	}
	elf_tiny_runtime_test_install_data(mut o, &data_definition)
	return o, elf_tiny_runtime_test_definition(entry, .void_, [
		ElfTinyRuntimeBinding{
			external_function_index: u32(write_external)
			role:                    .write_all
		},
	])
}

fn elf_tiny_runtime_test_i64_fixture(value i64) (Object, ElfTinyRuntimeDefinition) {
	mut o := Object.new()
	entry := o.intern_function_symbol('i64_entry') or { panic(err) }
	decimal := o.intern_external_function_symbol('declared_decimal') or { panic(err) }
	write_external := o.intern_external_function_symbol('declared_write') or { panic(err) }
	mut text := [u8(0x48), 0x83, 0xec, 0x08, 0x48, 0xbf]
	elf_tiny_runtime_test_emit_u64(mut text, u64(value))
	decimal_field := elf_tiny_runtime_emit_call(mut text)
	text << [u8(0x48), 0x89, 0xc6, 0xbf, 1, 0, 0, 0]
	write_field := elf_tiny_runtime_emit_call(mut text)
	text << [u8(0x48), 0x83, 0xc4, 0x08, 0x31, 0xc0, 0xc3]
	assert o.append_text(text) or { panic(err) } == 0
	o.define_text_function(entry, 0, u64(text.len)) or { panic(err) }
	o.add_text_call_relocation(decimal_field, decimal) or { panic(err) }
	o.add_text_call_relocation(write_field, write_external) or { panic(err) }
	return o, elf_tiny_runtime_test_definition(entry, .void_, [
		ElfTinyRuntimeBinding{
			external_function_index: u32(decimal)
			role:                    .i64_decimal
		},
		ElfTinyRuntimeBinding{
			external_function_index: u32(write_external)
			role:                    .write_all
		},
	])
}

fn elf_tiny_runtime_test_i32_fixture(value i32) (Object, ElfTinyRuntimeDefinition) {
	mut o := Object.new()
	entry := o.intern_function_symbol('i32_entry') or { panic(err) }
	decimal := o.intern_external_function_symbol('declared_i32') or { panic(err) }
	write_external := o.intern_external_function_symbol('declared_write') or { panic(err) }
	mut text := [u8(0x48), 0x83, 0xec, 0x08, 0xbf]
	elf_tiny_runtime_test_emit_u32(mut text, u32(value))
	decimal_field := elf_tiny_runtime_emit_call(mut text)
	text << [u8(0x48), 0x89, 0xc6, 0xbf, 1, 0, 0, 0]
	write_field := elf_tiny_runtime_emit_call(mut text)
	text << [u8(0x48), 0x83, 0xc4, 0x08, 0x31, 0xc0, 0xc3]
	assert o.append_text(text) or { panic(err) } == 0
	o.define_text_function(entry, 0, u64(text.len)) or { panic(err) }
	o.add_text_call_relocation(decimal_field, decimal) or { panic(err) }
	o.add_text_call_relocation(write_field, write_external) or { panic(err) }
	return o, elf_tiny_runtime_test_definition(entry, .void_, [
		ElfTinyRuntimeBinding{
			external_function_index: u32(decimal)
			role:                    .i32_decimal
		},
		ElfTinyRuntimeBinding{
			external_function_index: u32(write_external)
			role:                    .write_all
		},
	])
}

fn elf_tiny_runtime_test_emit_abi_checks(mut text []u8, mut failure_fields []int) {
	text << [u8(0x48), 0x3b, 0x24, 0x24]
	failure_fields << elf_tiny_runtime_emit_jcc(mut text, 0x85)
	for check in [
		[u8(0x48), 0x81, 0xfb, 0x44, 0x33, 0x22, 0x11],
		[u8(0x48), 0x81, 0xfd, 0x55, 0x44, 0x33, 0x22],
		[u8(0x49), 0x81, 0xfc, 0x66, 0x55, 0x44, 0x33],
		[u8(0x49), 0x81, 0xfd, 0x77, 0x66, 0x55, 0x44],
		[u8(0x49), 0x81, 0xfe, 0x11, 0x77, 0x66, 0x55],
		[u8(0x49), 0x81, 0xff, 0x22, 0x11, 0x77, 0x66],
	] {
		text << check
		failure_fields << elf_tiny_runtime_emit_jcc(mut text, 0x85)
	}
	text << [u8(0x9c), 0x41, 0x5a, 0x41, 0xf7, 0xc2, 0x00, 0x04, 0x00, 0x00]
	failure_fields << elf_tiny_runtime_emit_jcc(mut text, 0x85)
}

fn elf_tiny_runtime_test_abi_fixture() (Object, ElfTinyRuntimeDefinition) {
	mut o := Object.new()
	entry := o.intern_function_symbol('abi_entry') or { panic(err) }
	decimal := o.intern_external_function_symbol('declared_decimal') or { panic(err) }
	write_external := o.intern_external_function_symbol('declared_write') or { panic(err) }
	mut text := [u8(0x53), 0x55, 0x41, 0x54, 0x41, 0x55, 0x41, 0x56, 0x41, 0x57, 0xfc, 0xbb]
	elf_tiny_runtime_test_emit_u32(mut text, 0x1122_3344)
	text << u8(0xbd)
	elf_tiny_runtime_test_emit_u32(mut text, 0x2233_4455)
	text << [u8(0x41), 0xbc]
	elf_tiny_runtime_test_emit_u32(mut text, 0x3344_5566)
	text << [u8(0x41), 0xbd]
	elf_tiny_runtime_test_emit_u32(mut text, 0x4455_6677)
	text << [u8(0x41), 0xbe]
	elf_tiny_runtime_test_emit_u32(mut text, 0x5566_7711)
	text << [u8(0x41), 0xbf]
	elf_tiny_runtime_test_emit_u32(mut text, 0x6677_1122)
	text << [u8(0x48), 0x83, 0xec, 0x08, 0x48, 0x89, 0x24, 0x24, 0x48, 0xbf]
	elf_tiny_runtime_test_emit_u64(mut text, u64(i64(-42)))
	decimal_field := elf_tiny_runtime_emit_call(mut text)
	mut failure_fields := []int{}
	elf_tiny_runtime_test_emit_abi_checks(mut text, mut failure_fields)
	text << [u8(0x48), 0x89, 0xc6, 0xbf, 1, 0, 0, 0]
	write_field := elf_tiny_runtime_emit_call(mut text)
	elf_tiny_runtime_test_emit_abi_checks(mut text, mut failure_fields)
	text << [u8(0x31), 0xc0]
	success_field := elf_tiny_runtime_emit_jmp(mut text)
	failure_target := text.len
	text << [u8(0xb8), 91, 0, 0, 0]
	cleanup_target := text.len
	text << [u8(0x48), 0x83, 0xc4, 0x08, 0x41, 0x5f, 0x41, 0x5e, 0x41, 0x5d, 0x41, 0x5c, 0x5d,
		0x5b, 0xc3]
	for field in failure_fields {
		elf_tiny_runtime_patch_local_rel32(mut text, field, failure_target) or { panic(err) }
	}
	elf_tiny_runtime_patch_local_rel32(mut text, success_field, cleanup_target) or { panic(err) }
	assert o.append_text(text) or { panic(err) } == 0
	o.define_text_function(entry, 0, u64(text.len)) or { panic(err) }
	o.add_text_call_relocation(decimal_field, decimal) or { panic(err) }
	o.add_text_call_relocation(write_field, write_external) or { panic(err) }
	return o, elf_tiny_runtime_test_definition(entry, .scalar, [
		ElfTinyRuntimeBinding{
			external_function_index: u32(decimal)
			role:                    .i64_decimal
		},
		ElfTinyRuntimeBinding{
			external_function_index: u32(write_external)
			role:                    .write_all
		},
	])
}

fn elf_tiny_runtime_test_rune_fixture(value u32) (Object, ElfTinyRuntimeDefinition) {
	mut o := Object.new()
	entry := o.intern_function_symbol('rune_entry') or { panic(err) }
	encoder := o.intern_external_function_symbol('declared_rune') or { panic(err) }
	write_external := o.intern_external_function_symbol('declared_write') or { panic(err) }
	mut text := [u8(0x48), 0x83, 0xec, 0x08, 0xbf]
	elf_tiny_runtime_test_emit_u32(mut text, value)
	encode_field := elf_tiny_runtime_emit_call(mut text)
	text << [u8(0x48), 0x89, 0xc6, 0xbf, 1, 0, 0, 0]
	write_field := elf_tiny_runtime_emit_call(mut text)
	text << [u8(0x48), 0x83, 0xc4, 0x08, 0x31, 0xc0, 0xc3]
	assert o.append_text(text) or { panic(err) } == 0
	o.define_text_function(entry, 0, u64(text.len)) or { panic(err) }
	o.add_text_call_relocation(encode_field, encoder) or { panic(err) }
	o.add_text_call_relocation(write_field, write_external) or { panic(err) }
	return o, elf_tiny_runtime_test_definition(entry, .void_, [
		ElfTinyRuntimeBinding{
			external_function_index: u32(encoder)
			role:                    .rune_utf8
		},
		ElfTinyRuntimeBinding{
			external_function_index: u32(write_external)
			role:                    .write_all
		},
	])
}

fn elf_tiny_runtime_test_concat_fixture(left []u8, right []u8) (Object, ElfTinyRuntimeDefinition) {
	assert left.len > 0 && right.len > 0
	mut o := Object.new()
	entry := o.intern_function_symbol('concat_entry') or { panic(err) }
	concat := o.intern_external_function_symbol('declared_concat') or { panic(err) }
	write_external := o.intern_external_function_symbol('declared_write') or { panic(err) }
	mut text := [u8(0x48), 0x83, 0xec, 0x08, 0x48, 0x8d, 0x3d]
	left_field := u64(text.len)
	text << [u8(0), 0, 0, 0, 0xbe]
	elf_tiny_runtime_test_emit_u32(mut text, u32(left.len))
	text << [u8(0x48), 0x8d, 0x15]
	right_field := u64(text.len)
	text << [u8(0), 0, 0, 0, 0xb9]
	elf_tiny_runtime_test_emit_u32(mut text, u32(right.len))
	concat_field := elf_tiny_runtime_emit_call(mut text)
	text << [u8(0x48), 0x89, 0xc6, 0xbf, 1, 0, 0, 0]
	write_field := elf_tiny_runtime_emit_call(mut text)
	text << [u8(0x48), 0x83, 0xc4, 0x08, 0x31, 0xc0, 0xc3]
	assert o.append_text(text) or { panic(err) } == 0
	o.define_text_function(entry, 0, u64(text.len)) or { panic(err) }
	o.add_text_call_relocation(concat_field, concat) or { panic(err) }
	o.add_text_call_relocation(write_field, write_external) or { panic(err) }
	mut payload := [u8(0), 0, 0, 0]
	left_offset := u64(payload.len)
	payload << left
	right_offset := u64(payload.len)
	payload << right
	data_definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     payload
				size:      u64(payload.len)
				alignment: 4
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'left'
				section: .rodata
				offset:  left_offset
				size:    u64(left.len)
			},
			ObjectDataSymbol{
				kind:    .named
				name:    'right'
				section: .rodata
				offset:  right_offset
				size:    u64(right.len)
			},
		]
		relocations: [
			elf_tiny_runtime_test_pc_relocation(left_field, 0, -4),
			elf_tiny_runtime_test_pc_relocation(right_field, 1, -4),
		]
	}
	elf_tiny_runtime_test_install_data(mut o, &data_definition)
	return o, elf_tiny_runtime_test_definition(entry, .void_, [
		ElfTinyRuntimeBinding{
			external_function_index: u32(concat)
			role:                    .string_concat
		},
		ElfTinyRuntimeBinding{
			external_function_index: u32(write_external)
			role:                    .write_all
		},
	])
}

fn elf_tiny_runtime_test_concat_failure_fixture(right_length u32) (Object, ElfTinyRuntimeDefinition) {
	mut o := Object.new()
	entry := o.intern_function_symbol('concat_failure_entry') or { panic(err) }
	concat := o.intern_external_function_symbol('declared_concat') or { panic(err) }
	mut text := [
		u8(0x48),
		0x83,
		0xec,
		0x08,
		0x31,
		0xff,
		0xbe,
		0xff,
		0xff,
		0xff,
		0xff,
		0x31,
		0xd2,
		0xb9,
	]
	elf_tiny_runtime_test_emit_u32(mut text, right_length)
	call_field := elf_tiny_runtime_emit_call(mut text)
	text << [u8(0x0f), 0x0b]
	assert o.append_text(text) or { panic(err) } == 0
	o.define_text_function(entry, 0, u64(text.len)) or { panic(err) }
	o.add_text_call_relocation(call_field, concat) or { panic(err) }
	return o, elf_tiny_runtime_test_definition(entry, .void_, [
		ElfTinyRuntimeBinding{
			external_function_index: u32(concat)
			role:                    .string_concat
		},
	])
}

fn elf_tiny_runtime_test_single_role_fixture(role ElfTinyRuntimeRole) (Object, ElfTinyRuntimeDefinition) {
	mut o := Object.new()
	entry := o.intern_function_symbol('single_role_entry') or { panic(err) }
	external := o.intern_external_function_symbol('declared_role') or { panic(err) }
	mut text := [u8(0x48), 0x83, 0xec, 0x08]
	match role {
		.flush_noop {}
		.exit_group {
			text << [u8(0xbf), 37, 0, 0, 0]
		}
		.write_all {
			text << [u8(0xbf), 0xff, 0xff, 0xff, 0xff, 0xbe, 1, 0, 0, 0, 0xba, 1, 0, 0, 0]
		}
		else {
			panic('unsupported single-role fixture')
		}
	}

	call_field := elf_tiny_runtime_emit_call(mut text)
	if role == .flush_noop {
		text << [u8(0x48), 0x83, 0xc4, 0x08, 0xc3]
	} else {
		text << [u8(0x0f), 0x0b]
	}
	assert o.append_text(text) or { panic(err) } == 0
	o.define_text_function(entry, 0, u64(text.len)) or { panic(err) }
	o.add_text_call_relocation(call_field, external) or { panic(err) }
	return o, elf_tiny_runtime_test_definition(entry, if role == .flush_noop {
		.scalar
	} else {
		.void_
	}, [
		ElfTinyRuntimeBinding{
			external_function_index: u32(external)
			role:                    role
		},
	])
}

fn test_elf_tiny_runtime_t17_fingerprinted_readelf_and_objdump_raw_oracle_is_bounded() {
	if !elf_tiny_runtime_test_exact_host() {
		eprintln('ELF tiny runtime raw oracle: SKIPPED/UNPROVEN (host/tool fingerprint mismatch)')
		return
	}
	message := [u8(0x72), 0x61, 0x77, 0x0a]
	o, definition := elf_tiny_runtime_test_write_fixture(message)
	bytes := elf_tiny_runtime_executable_bytes(&o, definition) or { panic(err) }
	root := elf_tiny_runtime_test_root('raw oracle')
	defer {
		elf_tiny_runtime_test_cleanup(root)
	}
	path := elf_tiny_runtime_test_publish_executable(root, 'writer output', bytes)
	readelf := elf_tiny_runtime_test_run_process('/usr/bin/readelf', [
		'--file-header',
		'--program-headers',
		'--wide',
		path,
	], elf_tiny_runtime_test_timeout_ms)
	objdump := elf_tiny_runtime_test_run_process('/usr/bin/objdump', [
		'-f',
		'-p',
		path,
	], elf_tiny_runtime_test_timeout_ms)
	for result in [readelf, objdump] {
		assert !result.timed_out
		assert !result.output_limited
		assert result.exit_code == 0, result.stderr
	}
	assert readelf.stdout.contains('ELF64')
	assert readelf.stdout.contains('EXEC (Executable file)')
	assert readelf.stdout.contains('Advanced Micro Devices X86-64')
	assert readelf.stdout.contains('Entry point address:')
	assert readelf.stdout.contains('GNU_STACK')
	assert objdump.stdout.contains('file format elf64-x86-64')
	assert objdump.stdout.contains('EXEC_P')
	assert objdump.stdout.contains('start address 0x000000000040')
}

fn test_elf_tiny_runtime_t18_direct_linux_et_exec_semantics_are_mandatory_when_provisioned() {
	guard := os.getenv(elf_tiny_runtime_test_guard)
	assert guard in ['', '1'], '${elf_tiny_runtime_test_guard} must be empty or 1'
	mandatory := guard == '1'
	if !elf_tiny_runtime_test_exact_host() {
		assert !mandatory, 'mandatory ELF tiny runtime oracle requires the pinned Linux AMD64 host and tools'
		eprintln('ELF tiny runtime direct oracle: SKIPPED/UNPROVEN/UNCLOSED')
		return
	}
	root := elf_tiny_runtime_test_root('direct oracle')
	defer {
		elf_tiny_runtime_test_cleanup(root)
	}
	scalar, scalar_definition := elf_tiny_runtime_test_scalar_fixture(7, .scalar)
	elf_tiny_runtime_test_run_executable(root, 'scalar exit', &scalar, scalar_definition, 7, '')
	void_object, void_definition := elf_tiny_runtime_test_scalar_fixture(7, .void_)
	elf_tiny_runtime_test_run_executable(root, 'void exit', &void_object, void_definition, 0, '')
	write_object, write_definition := elf_tiny_runtime_test_write_fixture([
		u8(0x77),
		0x72,
		0x69,
		0x74,
		0x65,
		0x0a,
	])
	elf_tiny_runtime_test_run_executable(root, 'write all', &write_object, write_definition, 0,
		'write\n')
	for value in [min_i64, i64(0), max_i64] {
		decimal, decimal_definition := elf_tiny_runtime_test_i64_fixture(value)
		elf_tiny_runtime_test_run_executable(root, 'decimal ${value}', &decimal,
			decimal_definition, 0, value.str())
	}
	for value in [min_i32, i32(0), max_i32] {
		decimal, decimal_definition := elf_tiny_runtime_test_i32_fixture(value)
		elf_tiny_runtime_test_run_executable(root, 'i32 decimal ${value}', &decimal,
			decimal_definition, 0, value.str())
	}
	abi, abi_definition := elf_tiny_runtime_test_abi_fixture()
	elf_tiny_runtime_test_run_executable(root, 'sysv abi', &abi, abi_definition, 0, '-42')
	rune_object, rune_definition := elf_tiny_runtime_test_rune_fixture(0x20ac)
	elf_tiny_runtime_test_run_executable(root, 'rune utf8', &rune_object, rune_definition, 0, [
		u8(0xe2),
		0x82,
		0xac,
	].bytestr())
	d800_object, d800_definition := elf_tiny_runtime_test_rune_fixture(0xd800)
	elf_tiny_runtime_test_run_executable(root, 'rune U+D800', &d800_object, d800_definition, 0, [
		u8(0xed),
		0xa0,
		0x80,
	].bytestr())
	dfff_object, dfff_definition := elf_tiny_runtime_test_rune_fixture(0xdfff)
	elf_tiny_runtime_test_run_executable(root, 'rune U+DFFF', &dfff_object, dfff_definition, 0, [
		u8(0xed),
		0xbf,
		0xbf,
	].bytestr())
	concat, concat_definition := elf_tiny_runtime_test_concat_fixture([u8(0x61), 0x62], [
		u8(0x43),
		0x44,
		0x45,
	])
	elf_tiny_runtime_test_run_executable(root, 'concat success', &concat, concat_definition, 0,
		'abCDE')
	overflow, overflow_definition := elf_tiny_runtime_test_concat_failure_fixture(1)
	elf_tiny_runtime_test_run_executable(root, 'concat overflow', &overflow, overflow_definition,
		1, '')
	allocation_failure, allocation_failure_definition :=
		elf_tiny_runtime_test_concat_failure_fixture(0)
	elf_tiny_runtime_test_run_executable(root, 'allocator null', &allocation_failure,
		allocation_failure_definition, 1, '')
	flush, flush_definition := elf_tiny_runtime_test_single_role_fixture(.flush_noop)
	elf_tiny_runtime_test_run_executable(root, 'flush noop', &flush, flush_definition, 0, '')
	exit, exit_definition := elf_tiny_runtime_test_single_role_fixture(.exit_group)
	elf_tiny_runtime_test_run_executable(root, 'exit group', &exit, exit_definition, 37, '')
	write_error, write_error_definition := elf_tiny_runtime_test_single_role_fixture(.write_all)
	elf_tiny_runtime_test_run_executable(root, 'write terminal error', &write_error,
		write_error_definition, 1, '')
}
