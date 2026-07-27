module amd64

import crypto.sha256
import os
import time

const macho_tiny_test_runtime_guard = 'VTEST_AMD64_MACHO_TINY_RUNTIME'
const macho_tiny_test_timeout_ms = 10_000
const macho_tiny_test_output_limit = 64 * 1024
const macho_tiny_test_macos_minimum = '11.0'
const macho_tiny_runtime_raw_guard = 'V3_MACHO_TINY_RAW_ORACLE'
const macho_tiny_runtime_apple_guard = 'V3_MACHO_TINY_APPLE_ORACLE'

struct MachoTinyTestCommand {
	offset int
	cmd    u32
	size   u32
}

struct MachoTinyTestSection {
	offset u32
	size   u64
	reloff u32
	nreloc u32
	flags  u32
}

struct MachoTinyTestSymtab {
	symoff  u32
	nsyms   u32
	stroff  u32
	strsize u32
}

struct MachoTinyTestSymbol {
	name_offset u32
	type_       u8
	section     u8
	value       u64
}

struct MachoTinyTestRelocation {
	address      u32
	symbol_index u32
	pc_relative  bool
	length       u32
	external     bool
	type_        u32
	packed       u32
}

struct MachoTinyTestProcessResult {
	exit_code      int
	stdout         string
	stderr         string
	timed_out      bool
	output_limited bool
}

struct MachoTinyTestCapture {
mut:
	stdout string
	stderr string
}

struct MachoTinyTestMacTools {
	ld          string
	file_cmd    string
	otool       string
	nm          string
	codesign    string
	sdk_path    string
	sdk_version string
}

fn macho_tiny_test_read_u16(data []u8, offset int) u16 {
	assert offset >= 0
	assert offset <= data.len - 2
	return u16(data[offset]) | (u16(data[offset + 1]) << 8)
}

fn macho_tiny_test_read_u32(data []u8, offset int) u32 {
	assert offset >= 0
	assert offset <= data.len - 4
	return u32(data[offset]) | (u32(data[offset + 1]) << 8) | (u32(data[offset + 2]) << 16) |
		(u32(data[offset + 3]) << 24)
}

fn macho_tiny_test_read_u64(data []u8, offset int) u64 {
	return u64(macho_tiny_test_read_u32(data, offset)) |
		(u64(macho_tiny_test_read_u32(data, offset + 4)) << 32)
}

fn macho_tiny_test_commands(data []u8) []MachoTinyTestCommand {
	count := int(macho_tiny_test_read_u32(data, 16))
	command_bytes := int(macho_tiny_test_read_u32(data, 20))
	command_end := 32 + command_bytes
	assert command_end <= data.len
	mut commands := []MachoTinyTestCommand{cap: count}
	mut offset := 32
	for _ in 0 .. count {
		cmd := macho_tiny_test_read_u32(data, offset)
		size := macho_tiny_test_read_u32(data, offset + 4)
		assert size >= 8
		assert size % 8 == 0
		assert int(size) <= command_end - offset
		commands << MachoTinyTestCommand{
			offset: offset
			cmd:    cmd
			size:   size
		}
		offset += int(size)
	}
	assert offset == command_end
	return commands
}

fn macho_tiny_test_layout(data []u8) (MachoTinyTestSection, MachoTinyTestSymtab) {
	mut section := MachoTinyTestSection{}
	mut symtab := MachoTinyTestSymtab{}
	mut saw_segment := false
	mut saw_symtab := false
	for command in macho_tiny_test_commands(data) {
		if command.cmd == macho64_lc_segment_64 {
			assert !saw_segment
			assert command.size == 152
			assert macho_tiny_test_read_u32(data, command.offset + 64) == 1
			section_offset := command.offset + 72
			section = MachoTinyTestSection{
				offset: macho_tiny_test_read_u32(data, section_offset + 48)
				size:   macho_tiny_test_read_u64(data, section_offset + 40)
				reloff: macho_tiny_test_read_u32(data, section_offset + 56)
				nreloc: macho_tiny_test_read_u32(data, section_offset + 60)
				flags:  macho_tiny_test_read_u32(data, section_offset + 64)
			}
			saw_segment = true
		} else if command.cmd == macho64_lc_symtab {
			assert !saw_symtab
			assert command.size == 24
			symtab = MachoTinyTestSymtab{
				symoff:  macho_tiny_test_read_u32(data, command.offset + 8)
				nsyms:   macho_tiny_test_read_u32(data, command.offset + 12)
				stroff:  macho_tiny_test_read_u32(data, command.offset + 16)
				strsize: macho_tiny_test_read_u32(data, command.offset + 20)
			}
			saw_symtab = true
		}
	}
	assert saw_segment
	assert saw_symtab
	return section, symtab
}

fn macho_tiny_test_symbols(data []u8, symtab MachoTinyTestSymtab) []MachoTinyTestSymbol {
	assert u64(symtab.symoff) <= u64(data.len)
	assert u64(symtab.nsyms) <= (u64(data.len) - u64(symtab.symoff)) / 16
	mut symbols := []MachoTinyTestSymbol{cap: int(symtab.nsyms)}
	for index in 0 .. int(symtab.nsyms) {
		offset := int(symtab.symoff) + index * 16
		symbols << MachoTinyTestSymbol{
			name_offset: macho_tiny_test_read_u32(data, offset)
			type_:       data[offset + 4]
			section:     data[offset + 5]
			value:       macho_tiny_test_read_u64(data, offset + 8)
		}
	}
	return symbols
}

fn macho_tiny_test_symbol_name(data []u8, symtab MachoTinyTestSymtab, name_offset u32) string {
	assert name_offset < symtab.strsize
	start := u64(symtab.stroff) + u64(name_offset)
	limit := u64(symtab.stroff) + u64(symtab.strsize)
	assert limit <= u64(data.len)
	mut end := start
	for end < limit && data[int(end)] != 0 {
		end++
	}
	assert end < limit
	return data[int(start)..int(end)].bytestr()
}

fn macho_tiny_test_symbol_names(data []u8, symtab MachoTinyTestSymtab, symbols []MachoTinyTestSymbol) []string {
	mut names := []string{cap: symbols.len}
	for symbol in symbols {
		names << macho_tiny_test_symbol_name(data, symtab, symbol.name_offset)
	}
	return names
}

fn macho_tiny_test_relocations(data []u8, section MachoTinyTestSection) []MachoTinyTestRelocation {
	if section.nreloc == 0 {
		assert section.reloff == 0
		return []MachoTinyTestRelocation{}
	}
	assert u64(section.reloff) <= u64(data.len)
	assert u64(section.nreloc) <= (u64(data.len) - u64(section.reloff)) / 8
	mut relocations := []MachoTinyTestRelocation{cap: int(section.nreloc)}
	for index in 0 .. int(section.nreloc) {
		offset := int(section.reloff) + index * 8
		packed := macho_tiny_test_read_u32(data, offset + 4)
		relocations << MachoTinyTestRelocation{
			address:      macho_tiny_test_read_u32(data, offset)
			symbol_index: packed & 0x00ff_ffff
			pc_relative:  ((packed >> 24) & 1) == 1
			length:       (packed >> 25) & 3
			external:     ((packed >> 27) & 1) == 1
			type_:        packed >> 28
			packed:       packed
		}
	}
	return relocations
}

fn macho_tiny_test_text(data []u8, section MachoTinyTestSection) []u8 {
	start := int(section.offset)
	assert section.size <= u64(max_int)
	end := start + int(section.size)
	assert start >= 0
	assert end >= start
	assert end <= data.len
	return data[start..end].clone()
}

fn macho_tiny_test_error(o &Object, entry MachoTinyEntryDefinition) string {
	if _ := macho64_tiny_artifact(o, entry) {
		assert false, 'Mach-O tiny artifact unexpectedly succeeded'
	} else {
		return err.msg()
	}
	return ''
}

fn macho_tiny_test_leaf_object(name string) !Object {
	mut object := Object.new()
	entry := object.intern_function_symbol(name)!
	_ = object.append_text([u8(0x31), 0xc0, 0xc3])!
	object.define_text_function(entry, 0, 3)!
	return object
}

fn macho_tiny_test_present(path string) bool {
	return os.exists(path) || os.is_link(path)
}

fn macho_tiny_test_root(name string) string {
	root := os.join_path(os.temp_dir(), 'v3_amd64_macho_tiny_${name}_${os.getpid()}')
	assert !macho_tiny_test_present(root), 'test root `${root}` was stale'
	os.mkdir(root) or { assert false, 'create `${root}`: ${err.msg()}' }
	return root
}

fn macho_tiny_test_cleanup(root string) {
	if !macho_tiny_test_present(root) {
		return
	}
	assert os.is_dir(root) && !os.is_link(root), 'test root `${root}` changed type'
	os.rmdir_all(root) or { assert false, 'remove `${root}`: ${err.msg()}' }
	assert !macho_tiny_test_present(root), 'test root `${root}` survived cleanup'
}

fn macho_tiny_test_publication_error(path string, bytes []u8) string {
	mut message := ''
	publish_object(path, bytes) or { message = err.msg() }
	assert message.len > 0, 'publication unexpectedly succeeded for `${path}`'
	return message
}

fn macho_tiny_test_environment() map[string]string {
	mut environment := {
		'LC_ALL': 'C'
		'LANG':   'C'
	}
	for key in ['PATH', 'HOME', 'TMPDIR', 'DEVELOPER_DIR'] {
		value := os.getenv(key)
		if value.len != 0 {
			environment[key] = value
		}
	}
	return environment
}

fn (mut capture MachoTinyTestCapture) append_bounded(pipe os.ChildProcessPipeKind, chunk string) bool {
	match pipe {
		.stdout {
			if chunk.len > macho_tiny_test_output_limit - capture.stdout.len {
				return false
			}
			capture.stdout += chunk
		}
		.stderr {
			if chunk.len > macho_tiny_test_output_limit - capture.stderr.len {
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

fn macho_tiny_test_drain_process(mut process os.Process, mut capture MachoTinyTestCapture) bool {
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

fn macho_tiny_test_run_process(command string, args []string, environment map[string]string, timeout_ms int) MachoTinyTestProcessResult {
	mut process := os.new_process(command)
	process.use_pgroup = true
	process.set_args(args)
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.run()
	mut elapsed_ms := 0
	mut capture := MachoTinyTestCapture{}
	mut output_limited := false
	for process.is_alive() && elapsed_ms < timeout_ms && !output_limited {
		if !macho_tiny_test_drain_process(mut process, mut capture) {
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
	if !output_limited && !macho_tiny_test_drain_process(mut process, mut capture) {
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
	return MachoTinyTestProcessResult{
		exit_code: exit_code
		stdout:    capture.stdout
		stderr:    capture.stderr
		timed_out: timed_out
		output_limited: output_limited
	}
}

fn macho_tiny_test_command_output(command string, args []string, environment map[string]string) !string {
	result := macho_tiny_test_run_process(command, args, environment, macho_tiny_test_timeout_ms)
	if result.timed_out {
		return error('command `${command}` timed out')
	}
	if result.output_limited {
		return error('command `${command}` exceeded the output limit')
	}
	if result.exit_code != 0 {
		return error('command `${command}` failed with ${result.exit_code}: ${result.stderr}')
	}
	return result.stdout.trim_space()
}

fn macho_tiny_test_macos_tools(environment map[string]string) !MachoTinyTestMacTools {
	xcrun := os.find_abs_path_of_executable('xcrun')!
	ld := macho_tiny_test_command_output(xcrun, ['--sdk', 'macosx', '--find', 'ld'],
		environment)!
	file_cmd := os.find_abs_path_of_executable('file')!
	otool := macho_tiny_test_command_output(xcrun, ['--sdk', 'macosx', '--find', 'otool'],
		environment)!
	nm := macho_tiny_test_command_output(xcrun, ['--sdk', 'macosx', '--find', 'nm'],
		environment)!
	sdk_path := macho_tiny_test_command_output(xcrun, ['--sdk', 'macosx', '--show-sdk-path'],
		environment)!
	sdk_version := macho_tiny_test_command_output(xcrun, ['--sdk', 'macosx',
		'--show-sdk-version'], environment)!
	codesign := os.find_abs_path_of_executable('codesign')!
	return MachoTinyTestMacTools{
		ld:          ld
		file_cmd:    file_cmd
		otool:       otool
		nm:          nm
		codesign:    codesign
		sdk_path:    sdk_path
		sdk_version: sdk_version
	}
}

fn macho_tiny_test_link(tools MachoTinyTestMacTools, environment map[string]string, object_path string, output_path string, entry_link_symbol string) ! {
	if macho_tiny_test_present(output_path) {
		return error('owned link output `${output_path}` already exists')
	}
	args := [
		'-o',
		output_path,
		object_path,
		'-lSystem',
		'-syslibroot',
		tools.sdk_path,
		'-e',
		entry_link_symbol,
		'-arch',
		'x86_64',
		'-platform_version',
		'macos',
		macho_tiny_test_macos_minimum,
		tools.sdk_version,
		'-dead_strip',
		'-x',
		'-S',
	]
	result := macho_tiny_test_run_process(tools.ld, args, environment,
		macho_tiny_test_timeout_ms)
	if result.timed_out {
		return error('Apple ld timed out')
	}
	if result.output_limited {
		return error('Apple ld exceeded the output limit')
	}
	if result.exit_code != 0 {
		return error('Apple ld failed with ${result.exit_code}: ${result.stderr}')
	}
	if !os.is_file(output_path) || os.is_link(output_path) {
		return error('Apple ld did not create the owned regular output')
	}
}

fn test_macho_tiny_explicit_entry_prunes_unreachable_text_without_name_inference() {
	mut object := Object.new()
	main_symbol := object.intern_function_symbol('main') or { panic(err) }
	entry_symbol := object.intern_function_symbol('selected_entry') or { panic(err) }
	init_symbol := object.intern_function_symbol('module__init') or { panic(err) }
	helper_symbol := object.intern_function_symbol('helper') or { panic(err) }
	historical_helper := object.intern_function_symbol('_builtin__i64__str') or { panic(err) }
	assert object.append_text([u8(0xc3)]) or { panic(err) } == 0
	assert object.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or { panic(err) } == 1
	assert object.append_text([u8(0xc3)]) or { panic(err) } == 7
	assert object.append_text([u8(0xc3)]) or { panic(err) } == 8
	assert object.append_text([u8(0xc3)]) or { panic(err) } == 9
	object.define_text_function(main_symbol, 0, 1) or { panic(err) }
	object.define_text_function(entry_symbol, 1, 6) or { panic(err) }
	object.define_text_function(init_symbol, 7, 1) or { panic(err) }
	object.define_text_function(helper_symbol, 8, 1) or { panic(err) }
	object.define_text_function(historical_helper, 9, 1) or { panic(err) }
	object.add_text_call_relocation(2, helper_symbol) or { panic(err) }

	artifact := macho64_tiny_artifact(&object, MachoTinyEntryDefinition{
		function_index: u32(entry_symbol)
		result_policy:  .void_
	}) or { panic(err) }
	assert artifact.entry_link_symbol == '_selected_entry'
	data := artifact.object_bytes
	assert macho_tiny_test_read_u32(data, 0) == macho64_mh_magic_64
	assert macho_tiny_test_read_u32(data, 4) == macho64_cpu_type_x86_64
	assert macho_tiny_test_read_u32(data, 12) == macho64_mh_object
	section, symtab := macho_tiny_test_layout(data)
	assert section.flags == macho64_text_section_flags
	assert macho_tiny_test_text(data, section) == [u8(0xe8), 0, 0, 0, 0, 0xc3, 0xc3]
	symbols := macho_tiny_test_symbols(data, symtab)
	assert macho_tiny_test_symbol_names(data, symtab, symbols) == ['_selected_entry', '_helper']
	assert symbols[0].type_ == 0x0f
	assert symbols[0].section == 1
	assert symbols[0].value == 0
	assert symbols[1].type_ == 0x0f
	assert symbols[1].section == 1
	assert symbols[1].value == 6
	relocations := macho_tiny_test_relocations(data, section)
	assert relocations == [
		MachoTinyTestRelocation{
			address:      1
			symbol_index: 1
			pc_relative:  true
			length:       2
			external:     true
			type_:        2
			packed:       macho64_branch_relocation_bits | u32(1)
		},
	]
}

fn test_macho_tiny_remaps_internal_and_external_branch_relocations_exactly() {
	mut object := Object.new()
	entry := object.intern_function_symbol('entry') or { panic(err) }
	unreachable := object.intern_function_symbol('unreachable') or { panic(err) }
	callee := object.intern_function_symbol('callee') or { panic(err) }
	puts := object.intern_external_function_symbol('puts') or { panic(err) }
	unused := object.intern_external_function_symbol('unused_external') or { panic(err) }
	entry_body := [u8(0xe8), 0, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0xc3]
	call_body := [u8(0xe8), 0, 0, 0, 0, 0xc3]
	assert object.append_text(entry_body) or { panic(err) } == 0
	assert object.append_text(call_body) or { panic(err) } == 11
	assert object.append_text(call_body) or { panic(err) } == 17
	object.define_text_function(entry, 0, 11) or { panic(err) }
	object.define_text_function(unreachable, 11, 6) or { panic(err) }
	object.define_text_function(callee, 17, 6) or { panic(err) }
	object.add_text_call_relocation(12, unused) or { panic(err) }
	object.add_text_call_relocation(18, entry) or { panic(err) }
	object.add_text_call_relocation(6, puts) or { panic(err) }
	object.add_text_call_relocation(1, callee) or { panic(err) }
	before_text := object.text.clone()
	before_relocations := object.call_relocations.clone()

	artifact := macho64_tiny_artifact(&object, MachoTinyEntryDefinition{
		function_index: u32(entry)
		result_policy:  .scalar
	}) or { panic(err) }
	section, symtab := macho_tiny_test_layout(artifact.object_bytes)
	mut expected_text := entry_body.clone()
	expected_text << call_body
	assert macho_tiny_test_text(artifact.object_bytes, section) == expected_text
	symbols := macho_tiny_test_symbols(artifact.object_bytes, symtab)
	assert macho_tiny_test_symbol_names(artifact.object_bytes, symtab, symbols) == [
		'_entry',
		'_callee',
		'_puts',
	]
	relocations := macho_tiny_test_relocations(artifact.object_bytes, section)
	assert relocations.len == 3
	assert relocations[0] == MachoTinyTestRelocation{
		address:      1
		symbol_index: 1
		pc_relative:  true
		length:       2
		external:     true
		type_:        2
		packed:       macho64_branch_relocation_bits | u32(1)
	}
	assert relocations[1] == MachoTinyTestRelocation{
		address:      6
		symbol_index: 2
		pc_relative:  true
		length:       2
		external:     true
		type_:        2
		packed:       macho64_branch_relocation_bits | u32(2)
	}
	assert relocations[2] == MachoTinyTestRelocation{
		address:      12
		symbol_index: 0
		pc_relative:  true
		length:       2
		external:     true
		type_:        2
		packed:       macho64_branch_relocation_bits
	}
	text := macho_tiny_test_text(artifact.object_bytes, section)
	assert text[1..5] == [u8(0), 0, 0, 0]
	assert text[6..10] == [u8(0), 0, 0, 0]
	assert text[12..16] == [u8(0), 0, 0, 0]
	assert object.text == before_text
	assert object.call_relocations == before_relocations
}

fn test_macho_tiny_preserves_referenced_externals_and_serializer_physical_names() {
	mut object := Object.new()
	entry := object.intern_function_symbol('_entry') or { panic(err) }
	dead := object.intern_function_symbol('dead') or { panic(err) }
	zeta := object.intern_external_function_symbol('zeta') or { panic(err) }
	alpha := object.intern_external_function_symbol('alpha') or { panic(err) }
	unused := object.intern_external_function_symbol('unused') or { panic(err) }
	assert object.append_text([u8(0xe8), 0, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0xc3]) or {
		panic(err)
	} == 0
	assert object.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or { panic(err) } == 11
	object.define_text_function(entry, 0, 11) or { panic(err) }
	object.define_text_function(dead, 11, 6) or { panic(err) }
	object.add_text_call_relocation(1, zeta) or { panic(err) }
	object.add_text_call_relocation(6, alpha) or { panic(err) }
	object.add_text_call_relocation(12, unused) or { panic(err) }

	artifact := macho64_tiny_artifact(&object, MachoTinyEntryDefinition{
		function_index: u32(entry)
		result_policy:  .scalar
	}) or { panic(err) }
	assert artifact.entry_link_symbol == '__entry'
	section, symtab := macho_tiny_test_layout(artifact.object_bytes)
	symbols := macho_tiny_test_symbols(artifact.object_bytes, symtab)
	assert macho_tiny_test_symbol_names(artifact.object_bytes, symtab, symbols) == [
		'__entry',
		'_alpha',
		'_zeta',
	]
	assert symbols[0].type_ == 0x0f
	assert symbols[0].section == 1
	assert symbols[1].type_ == 0x01
	assert symbols[1].section == 0
	assert symbols[1].value == 0
	assert symbols[2].type_ == 0x01
	assert symbols[2].section == 0
	assert symbols[2].value == 0
	relocations := macho_tiny_test_relocations(artifact.object_bytes, section)
	assert relocations.len == 2
	assert relocations[0].address == 1
	assert relocations[0].symbol_index == 2
	assert relocations[0].packed == macho64_branch_relocation_bits | u32(2)
	assert relocations[1].address == 6
	assert relocations[1].symbol_index == 1
	assert relocations[1].packed == macho64_branch_relocation_bits | u32(1)
}

fn test_macho_tiny_refuses_bad_entry_signature_private_data_and_malformed_object() {
	leaf := macho_tiny_test_leaf_object('entry') or { panic(err) }
	assert macho_tiny_test_error(&leaf, MachoTinyEntryDefinition{
		function_index: 9
		result_policy:  .scalar
	}) == 'Mach-O tiny entry function index 9 is out of range'
	assert macho_tiny_test_error(&leaf, MachoTinyEntryDefinition{
		function_index:  0
		parameter_count: 1
		result_policy:   .scalar
	}) == 'Mach-O tiny entry function must not accept scalar parameters'
	assert macho_tiny_test_error(&leaf, MachoTinyEntryDefinition{
		function_index: 0
		result_policy:  unsafe { MachoTinyEntryResultPolicy(255) }
	}) == 'Mach-O tiny entry result policy 255 is unsupported'

	mut external_object := Object.new()
	caller := external_object.intern_function_symbol('caller') or { panic(err) }
	foreign := external_object.intern_external_function_symbol('foreign') or { panic(err) }
	assert external_object.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or {
		panic(err)
	} == 0
	external_object.define_text_function(caller, 0, 6) or { panic(err) }
	external_object.add_text_call_relocation(1, foreign) or { panic(err) }
	assert macho_tiny_test_error(&external_object, MachoTinyEntryDefinition{
		function_index: u32(foreign)
		result_policy:  .scalar
	}) == 'Mach-O tiny entry function index 1 is not a defined function'

	mut data_object := macho_tiny_test_leaf_object('data_entry') or { panic(err) }
	data_plan := private_data_preflight([
		PrivateDataDefinition{ name: 'slot', value: 7, width: 32, alignment: 4 },
	], ['data_entry']) or { panic(err) }
	data_object.install_private_data(&data_plan) or { panic(err) }
	assert macho_tiny_test_error(&data_object, MachoTinyEntryDefinition{
		function_index: 0
		result_policy:  .scalar
	}) == 'Mach-O tiny object does not support private data'

	mut malformed := Object.new()
	self := malformed.intern_function_symbol('self') or { panic(err) }
	assert malformed.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	malformed.define_text_function(self, 0, 6) or { panic(err) }
	malformed.add_text_call_relocation(1, self) or { panic(err) }
	malformed.text[2] = 1
	before := malformed.text.clone()
	assert macho_tiny_test_error(&malformed, MachoTinyEntryDefinition{
		function_index: u32(self)
		result_policy:  .scalar
	}) == 'AMD64 object CALL relocation field 1 is not a zero rel32 placeholder'
	assert malformed.text == before

	mut bad_target := Object.new()
	bad_target_entry := bad_target.intern_function_symbol('bad_target') or { panic(err) }
	assert bad_target.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	bad_target.define_text_function(bad_target_entry, 0, 6) or { panic(err) }
	bad_target.add_text_call_relocation(1, bad_target_entry) or { panic(err) }
	bad_target.call_relocations[0] = TextCallRelocation{
		offset:    1
		symbol_id: SymbolID(99)
	}
	assert macho_tiny_test_error(&bad_target, MachoTinyEntryDefinition{
		function_index: u32(bad_target_entry)
		result_policy:  .scalar
	}) == 'AMD64 object symbol 99 is out of range'

	mut bad_owner := Object.new()
	owner_left := bad_owner.intern_function_symbol('owner_left') or { panic(err) }
	owner_right := bad_owner.intern_function_symbol('owner_right') or { panic(err) }
	assert bad_owner.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	bad_owner.define_text_function(owner_left, 0, 5) or { panic(err) }
	bad_owner.define_text_function(owner_right, 5, 1) or { panic(err) }
	bad_owner.add_text_call_relocation(1, owner_left) or { panic(err) }
	bad_owner.symbols[int(owner_left)].size = 3
	bad_owner.symbols[int(owner_right)].offset = 3
	bad_owner.symbols[int(owner_right)].size = 3
	assert macho_tiny_test_error(&bad_owner, MachoTinyEntryDefinition{
		function_index: u32(owner_left)
		result_policy:  .scalar
	}) == 'AMD64 object CALL relocation field 1 is not contained in exactly one function'

	mut overlapping := Object.new()
	overlap_left := overlapping.intern_function_symbol('overlap_left') or { panic(err) }
	overlap_right := overlapping.intern_function_symbol('overlap_right') or { panic(err) }
	assert overlapping.append_text([u8(0xc3), 0x90, 0x90, 0xc3, 0x90, 0x90]) or {
		panic(err)
	} == 0
	overlapping.define_text_function(overlap_left, 0, 3) or { panic(err) }
	overlapping.define_text_function(overlap_right, 3, 3) or { panic(err) }
	overlapping.symbols[int(overlap_right)].offset = 2
	overlapping.symbols[int(overlap_right)].size = 4
	assert macho_tiny_test_error(&overlapping, MachoTinyEntryDefinition{
		function_index: u32(overlap_left)
		result_policy:  .scalar
	}) == 'AMD64 object function overlap_right overlaps function overlap_left'

	assert macho_tiny_link_symbol('_entry') or { panic(err) } == '__entry'
	if _ := macho_tiny_checked_add(max_u64, 1, 'test extent') {
		assert false, 'overflowing Mach-O tiny addition was accepted'
	} else {
		assert err.msg() == 'Mach-O tiny test extent overflows u64'
	}
	if _ := macho_tiny_checked_sub(0, 1, 'test relative offset') {
		assert false, 'underflowing Mach-O tiny subtraction was accepted'
	} else {
		assert err.msg() == 'Mach-O tiny test relative offset underflows u64'
	}
	if _ := macho_tiny_checked_host_index(u64(max_int) + 1, 'test index') {
		assert false, 'out-of-range Mach-O tiny host index was accepted'
	} else {
		assert err.msg() == 'Mach-O tiny test index exceeds the host array limit'
	}
}

fn macho_tiny_test_deterministic_object(reverse_relocations bool) !Object {
	mut object := Object.new()
	entry := object.intern_function_symbol('entry')!
	left := object.intern_function_symbol('left')!
	right := object.intern_function_symbol('right')!
	beta := object.intern_external_function_symbol('beta')!
	alpha := object.intern_external_function_symbol('alpha')!
	_ = object.append_text([
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
		0xc3,
	])!
	_ = object.append_text([u8(0xc3)])!
	_ = object.append_text([u8(0xc3)])!
	object.define_text_function(entry, 0, 21)!
	object.define_text_function(left, 21, 1)!
	object.define_text_function(right, 22, 1)!
	if reverse_relocations {
		object.add_text_call_relocation(16, alpha)!
		object.add_text_call_relocation(11, beta)!
		object.add_text_call_relocation(6, right)!
		object.add_text_call_relocation(1, left)!
	} else {
		object.add_text_call_relocation(1, left)!
		object.add_text_call_relocation(6, right)!
		object.add_text_call_relocation(11, beta)!
		object.add_text_call_relocation(16, alpha)!
	}
	return object
}

fn test_macho_tiny_is_fresh_deterministic_and_does_not_mutate_object() {
	object := macho_tiny_test_deterministic_object(false) or { panic(err) }
	reversed := macho_tiny_test_deterministic_object(true) or { panic(err) }
	before_text := object.text.clone()
	before_symbols := object.symbols.clone()
	before_relocations := object.call_relocations.clone()
	before_private_data := object.private_data.clone()
	before_private_data_symbols := object.private_data_symbols.clone()
	entry := MachoTinyEntryDefinition{
		function_index: 0
		result_policy:  .scalar
	}

	first := macho64_tiny_artifact(&object, entry) or { panic(err) }
	second := macho64_tiny_artifact(&object, entry) or { panic(err) }
	reordered := macho64_tiny_artifact(&reversed, entry) or { panic(err) }
	void_artifact := macho64_tiny_artifact(&object, MachoTinyEntryDefinition{
		function_index: 0
		result_policy:  .void_
	}) or { panic(err) }
	assert first.object_bytes == second.object_bytes
	assert first.object_bytes == reordered.object_bytes
	assert first.object_bytes == void_artifact.object_bytes
	assert first.entry_link_symbol == second.entry_link_symbol
	assert first.entry_link_symbol == void_artifact.entry_link_symbol
	failure := macho_tiny_test_error(&object, MachoTinyEntryDefinition{
		function_index:  0
		parameter_count: 1
		result_policy:   .scalar
	})
	assert failure == 'Mach-O tiny entry function must not accept scalar parameters'

	mut changed := first.object_bytes.clone()
	changed[0] = 0
	third := macho64_tiny_artifact(&object, entry) or { panic(err) }
	assert changed != third.object_bytes
	assert second.object_bytes == third.object_bytes
	assert object.text == before_text
	assert object.symbols == before_symbols
	assert object.call_relocations == before_relocations
	assert object.private_data == before_private_data
	assert object.private_data_symbols == before_private_data_symbols
	assert reversed.text == before_text
}

fn test_macho_tiny_publishes_object_no_clobber_with_existing_helper() {
	object := macho_tiny_test_leaf_object('published_entry') or { panic(err) }
	artifact := macho64_tiny_artifact(&object, MachoTinyEntryDefinition{
		function_index: 0
		result_policy:  .scalar
	}) or { panic(err) }
	root := macho_tiny_test_root('publication')
	defer {
		macho_tiny_test_cleanup(root)
	}
	output := os.join_path(root, 'tiny.o')
	stage := publication_stage_path(output)
	publish_object(output, artifact.object_bytes) or { assert false, err.msg() }
	assert os.is_file(output) && !os.is_link(output)
	published_bytes := os.read_bytes(output) or { panic(err) }
	assert published_bytes == artifact.object_bytes
	assert !macho_tiny_test_present(stage)
	$if !windows {
		attributes := os.stat(output) or { panic(err) }
		assert attributes.mode & u32(0o111) == 0
	}

	blocked := os.join_path(root, 'blocked.o')
	sentinel := 'preexisting-object\n'
	os.write_file(blocked, sentinel) or { panic(err) }
	message := macho_tiny_test_publication_error(blocked, artifact.object_bytes)
	assert message == 'final output `${blocked}` already exists'
	blocked_contents := os.read_file(blocked) or { panic(err) }
	assert blocked_contents == sentinel
	assert !macho_tiny_test_present(publication_stage_path(blocked))
}

fn test_macho_tiny_macos_system_ld_links_explicit_entry_when_guarded() {
	$if macos && amd64 {
		if os.getenv(macho_tiny_test_runtime_guard) != '1' {
			return
		}
		environment := macho_tiny_test_environment()
		tools := macho_tiny_test_macos_tools(environment) or { panic(err) }
		mut object := Object.new()
		entry := object.intern_function_symbol('tiny_entry') or { panic(err) }
		dead := object.intern_function_symbol('tiny_dead') or { panic(err) }
		exit_symbol := object.intern_external_function_symbol('exit') or { panic(err) }
		assert object.append_text([
			u8(0x48),
			0x83,
			0xe4,
			0xf0,
			0xbf,
			0,
			0,
			0,
			0,
			0xe8,
			0,
			0,
			0,
			0,
			0x0f,
			0x0b,
		]) or { panic(err) } == 0
		assert object.append_text([u8(0xc3)]) or { panic(err) } == 16
		object.define_text_function(entry, 0, 16) or { panic(err) }
		object.define_text_function(dead, 16, 1) or { panic(err) }
		object.add_text_call_relocation(10, exit_symbol) or { panic(err) }
		artifact := macho64_tiny_artifact(&object, MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .void_
		}) or { panic(err) }
		root := macho_tiny_test_root('macos_link')
		defer {
			macho_tiny_test_cleanup(root)
		}
		object_path := os.join_path(root, 'tiny.o')
		output_path := os.join_path(root, 'linked-stage')
		publish_object(object_path, artifact.object_bytes) or { panic(err) }
		macho_tiny_test_link(tools, environment, object_path, output_path,
			artifact.entry_link_symbol) or { panic(err) }

		file_result := macho_tiny_test_run_process(tools.file_cmd, [output_path], environment,
			macho_tiny_test_timeout_ms)
		assert !file_result.timed_out
		assert !file_result.output_limited
		assert file_result.exit_code == 0, file_result.stderr
		assert file_result.stdout.contains('Mach-O 64-bit executable x86_64')
		otool := macho_tiny_test_run_process(tools.otool, ['-hv', output_path], environment,
			macho_tiny_test_timeout_ms)
		assert !otool.timed_out
		assert !otool.output_limited
		assert otool.exit_code == 0, otool.stderr
		assert otool.stdout.contains('X86_64') || otool.stdout.contains('x86_64')
		assert otool.stdout.contains('EXECUTE')
		nm := macho_tiny_test_run_process(tools.nm, ['-m', output_path], environment,
			macho_tiny_test_timeout_ms)
		assert !nm.timed_out
		assert !nm.output_limited
		assert nm.exit_code == 0, nm.stderr
		assert nm.stdout.contains(artifact.entry_link_symbol)
		assert !nm.stdout.contains('_tiny_dead')
	}
}

fn test_macho_tiny_macos_runtime_internal_and_libsystem_calls_when_guarded() {
	$if macos && amd64 {
		if os.getenv(macho_tiny_test_runtime_guard) != '1' {
			return
		}
		environment := macho_tiny_test_environment()
		tools := macho_tiny_test_macos_tools(environment) or { panic(err) }
		mut object := Object.new()
		entry := object.intern_function_symbol('runtime_entry') or { panic(err) }
		inner := object.intern_function_symbol('runtime_inner') or { panic(err) }
		dead := object.intern_function_symbol('runtime_dead') or { panic(err) }
		exit_symbol := object.intern_external_function_symbol('exit') or { panic(err) }
		write_symbol := object.intern_external_function_symbol('write') or { panic(err) }
		entry_text := [
			u8(0x48),
			0x83,
			0xe4,
			0xf0,
			0xe8,
			0,
			0,
			0,
			0,
			0xbf,
			0x07,
			0,
			0,
			0,
			0xe8,
			0,
			0,
			0,
			0,
			0x0f,
			0x0b,
		]
		inner_text := [
			u8(0x48),
			0x83,
			0xec,
			0x18,
			0xc6,
			0x04,
			0x24,
			0x4d,
			0xbf,
			0x01,
			0,
			0,
			0,
			0x48,
			0x89,
			0xe6,
			0xba,
			0x01,
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
			0x18,
			0xc3,
		]
		assert object.append_text(entry_text) or { panic(err) } == 0
		assert object.append_text(inner_text) or { panic(err) } == 21
		assert object.append_text([u8(0xc3)]) or { panic(err) } == 52
		object.define_text_function(entry, 0, 21) or { panic(err) }
		object.define_text_function(inner, 21, 31) or { panic(err) }
		object.define_text_function(dead, 52, 1) or { panic(err) }
		object.add_text_call_relocation(5, inner) or { panic(err) }
		object.add_text_call_relocation(15, exit_symbol) or { panic(err) }
		object.add_text_call_relocation(43, write_symbol) or { panic(err) }
		artifact := macho64_tiny_artifact(&object, MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .void_
		}) or { panic(err) }
		artifact_section, artifact_symtab := macho_tiny_test_layout(artifact.object_bytes)
		artifact_symbols := macho_tiny_test_symbols(artifact.object_bytes, artifact_symtab)
		assert macho_tiny_test_text(artifact.object_bytes, artifact_section).len == 52
		assert !macho_tiny_test_symbol_names(artifact.object_bytes, artifact_symtab,
			artifact_symbols).contains('_runtime_dead')
		root := macho_tiny_test_root('macos_runtime')
		defer {
			macho_tiny_test_cleanup(root)
		}
		object_path := os.join_path(root, 'runtime.o')
		output_path := os.join_path(root, 'runtime-stage')
		publish_object(object_path, artifact.object_bytes) or { panic(err) }
		macho_tiny_test_link(tools, environment, object_path, output_path,
			artifact.entry_link_symbol) or { panic(err) }
		sign := macho_tiny_test_run_process(tools.codesign, ['--force', '--sign', '-',
			output_path], environment, macho_tiny_test_timeout_ms)
		assert !sign.timed_out
		assert !sign.output_limited
		assert sign.exit_code == 0, sign.stderr

		run := macho_tiny_test_run_process(output_path, []string{}, environment,
			macho_tiny_test_timeout_ms)
		assert !run.timed_out
		assert !run.output_limited
		assert run.exit_code == 7
		assert run.stdout.bytes() == [u8(0x4d)]
		assert run.stderr == ''
	}
}

struct MachoTinyRuntimeTestSection {
	sectname        string
	segname         string
	address         u64
	size            u64
	offset          u32
	alignment_power u32
	reloff          u32
	nreloc          u32
	flags           u32
}

fn macho_tiny_runtime_test_fixed_name(data []u8, offset int) string {
	assert offset >= 0 && offset <= data.len - 16
	mut end := offset
	for end < offset + 16 && data[end] != 0 {
		end++
	}
	return data[offset..end].bytestr()
}

fn macho_tiny_runtime_test_sections(data []u8) []MachoTinyRuntimeTestSection {
	mut sections := []MachoTinyRuntimeTestSection{}
	for command in macho_tiny_test_commands(data) {
		if command.cmd != macho64_lc_segment_64 {
			continue
		}
		count := int(macho_tiny_test_read_u32(data, command.offset + 64))
		assert command.size == u32(72 + count * 80)
		for index in 0 .. count {
			offset := command.offset + 72 + index * 80
			sections << MachoTinyRuntimeTestSection{
				sectname:        macho_tiny_runtime_test_fixed_name(data, offset)
				segname:         macho_tiny_runtime_test_fixed_name(data, offset + 16)
				address:         macho_tiny_test_read_u64(data, offset + 32)
				size:            macho_tiny_test_read_u64(data, offset + 40)
				offset:          macho_tiny_test_read_u32(data, offset + 48)
				alignment_power: macho_tiny_test_read_u32(data, offset + 52)
				reloff:          macho_tiny_test_read_u32(data, offset + 56)
				nreloc:          macho_tiny_test_read_u32(data, offset + 60)
				flags:           macho_tiny_test_read_u32(data, offset + 64)
			}
		}
	}
	return sections
}

fn macho_tiny_runtime_test_symtab(data []u8) MachoTinyTestSymtab {
	for command in macho_tiny_test_commands(data) {
		if command.cmd == macho64_lc_symtab {
			return MachoTinyTestSymtab{
				symoff:  macho_tiny_test_read_u32(data, command.offset + 8)
				nsyms:   macho_tiny_test_read_u32(data, command.offset + 12)
				stroff:  macho_tiny_test_read_u32(data, command.offset + 16)
				strsize: macho_tiny_test_read_u32(data, command.offset + 20)
			}
		}
	}
	assert false, 'Mach-O runtime artifact has no symbol table'
	return MachoTinyTestSymtab{}
}

fn macho_tiny_runtime_test_section(data []u8, name string) MachoTinyRuntimeTestSection {
	for section in macho_tiny_runtime_test_sections(data) {
		if section.sectname == name {
			return section
		}
	}
	assert false, 'Mach-O runtime artifact has no ${name} section'
	return MachoTinyRuntimeTestSection{}
}

fn macho_tiny_runtime_test_definition(entry SymbolID, policy MachoTinyEntryResultPolicy) MachoTinyRuntimeDefinition {
	return MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  policy
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'tiny_runtime_start'
		exit_symbol_name:   'exit'
	}
}

fn macho_tiny_runtime_test_error(o &Object, definition MachoTinyRuntimeDefinition) string {
	if _ := macho64_tiny_runtime_artifact(o, definition) {
		assert false, 'Mach-O tiny runtime artifact unexpectedly succeeded'
	} else {
		return err.msg()
	}
	return ''
}

fn macho_tiny_runtime_test_install_data(mut o Object, definition &ObjectDataDefinition) {
	plan := object_data_preflight(definition, &o) or { panic(err) }
	o.install_object_data(&plan) or { panic(err) }
}

fn macho_tiny_runtime_test_helper_object(helper_name string) (Object, SymbolID, SymbolID) {
	mut o := Object.new()
	entry := o.intern_function_symbol('runtime_entry') or { panic(err) }
	helper := o.intern_external_function_symbol(helper_name) or { panic(err) }
	assert o.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	o.define_text_function(entry, 0, 6) or { panic(err) }
	o.add_text_call_relocation(1, helper) or { panic(err) }
	return o, entry, helper
}

fn macho_tiny_runtime_test_single_data_object(text []u8, section ObjectDataSection, symbols []ObjectDataSymbol, relocations []ObjectDataRelocation) (Object, SymbolID) {
	mut o := Object.new()
	entry := o.intern_function_symbol('data_entry') or { panic(err) }
	assert o.append_text(text) or { panic(err) } == 0
	o.define_text_function(entry, 0, u64(text.len)) or { panic(err) }
	definition := ObjectDataDefinition{
		sections:    [section]
		symbols:     symbols
		relocations: relocations
	}
	macho_tiny_runtime_test_install_data(mut o, &definition)
	return o, entry
}

fn macho_tiny_runtime_test_pc_relocation(offset u64, target ObjectDataSymbolID, addend i64) ObjectDataRelocation {
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

fn macho_tiny_runtime_test_got_relocation(offset u64, target ObjectDataSymbolID, access ObjectDataGotAccessIntent) ObjectDataRelocation {
	return ObjectDataRelocation{
		source_section: .text
		offset:         offset
		target_symbol:  object_data_symbol_ref(target)
		width:          32
		kind:           .got_relative
		signedness:     .signed
		address_intent: .virtual_address
		pc_bias:        .zero
		got_access:     access
	}
}

fn macho_tiny_runtime_test_i64_model(value i64) string {
	return value.str()
}

fn macho_tiny_runtime_test_concat_allocation(left u64, right u64) !u64 {
	if left > u64(max_u32) || right > u64(max_u32) || left > u64(max_u32) - right {
		return error('length overflow')
	}
	return left + right + 1
}

fn macho_tiny_runtime_test_clone_object(o &Object) Object {
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

fn macho_tiny_runtime_test_rel32_target(bytes []u8, field int) int {
	raw := macho_tiny_test_read_u32(bytes, field)
	return field + 4 + int(i32(raw))
}

fn test_macho_tiny_runtime_t01_wrapper_manifests_and_exit_mapping_are_exact() {
	void_bytes := macho_tiny_runtime_wrapper_bytes(.void_) or { panic(err) }
	scalar_bytes := macho_tiny_runtime_wrapper_bytes(.scalar) or { panic(err) }
	assert void_bytes.len == 18
	assert scalar_bytes.len == 18
	assert sha256.sum256(void_bytes).hex() == 'da7a0ede8981dc0f606e8cb4ce3fc61d80e7ffc78cb4ec74f85c93554839ab99'
	assert sha256.sum256(scalar_bytes).hex() == '93323ed2fa890482b30f519d8ddc05f31c08c27fd09cd675e8fa64cebfbfdcb7'
	assert void_bytes[..9] == scalar_bytes[..9]
	assert void_bytes[9..11] == [u8(0x31), 0xff]
	assert scalar_bytes[9..11] == [u8(0x89), 0xc7]
	assert void_bytes[5..9] == [u8(0), 0, 0, 0]
	assert void_bytes[12..16] == [u8(0), 0, 0, 0]
	assert void_bytes[16..] == [u8(0x0f), 0x0b]
}

fn test_macho_tiny_runtime_t02_helper_byte_hashes_fixups_and_abi_manifests_are_exact() {
	i64_manifest := macho_tiny_runtime_i64_decimal_manifest() or { panic(err) }
	concat_manifest := macho_tiny_runtime_string_concat_manifest() or { panic(err) }
	assert i64_manifest.bytes.len == 149
	assert concat_manifest.bytes.len == 171
	assert sha256.sum256(i64_manifest.bytes).hex() == '3fab08373610cf20916facc8048f8436a71ddc207513cc5bb6feadb974ba425a'
	assert sha256.sum256(concat_manifest.bytes).hex() == '0cc430b9c27089d4983582a1db1f44958e5984523f372321bec2f7176b86756b'
	assert i64_manifest.allocator_call_fields == [u64(7)]
	assert i64_manifest.exit_call_fields == [u64(26)]
	assert concat_manifest.allocator_call_fields == [u64(41)]
	assert concat_manifest.exit_call_fields == [u64(60)]
	for field in [7, 26] {
		assert i64_manifest.bytes[field - 1] == 0xe8
		assert i64_manifest.bytes[field..field + 4] == [u8(0), 0, 0, 0]
	}
	for field in [41, 60] {
		assert concat_manifest.bytes[field - 1] == 0xe8
		assert concat_manifest.bytes[field..field + 4] == [u8(0), 0, 0, 0]
	}
	for field, target in {
		16: 32
		55: 65
		70: 92
		88: 123
		119: 92
		128: 142
	} {
		assert macho_tiny_runtime_test_rel32_target(i64_manifest.bytes, field) == target
	}
	for field, target in {
		20: 54
		33: 54
		50: 66
		86: 110
		106: 90
		124: 148
		144: 128
	} {
		assert macho_tiny_runtime_test_rel32_target(concat_manifest.bytes, field) == target
	}
	assert i64_manifest.bytes[0] == 0x57
	assert i64_manifest.bytes.last() == 0xc3
	assert concat_manifest.bytes[..8] == [u8(0x57), 0x56, 0x52, 0x51, 0x48, 0x83, 0xec,
		0x08]
	assert concat_manifest.bytes[166..] == [u8(0x48), 0x83, 0xc4, 0x28, 0xc3]
}

fn test_macho_tiny_runtime_t03_basic_artifact_has_wrapper_entry_and_stable_branch_symbols() {
	o := macho_tiny_test_leaf_object('runtime_entry') or { panic(err) }
	artifact := macho64_tiny_runtime_artifact(&o, macho_tiny_runtime_test_definition(0,
		.scalar)) or { panic(err) }
	assert artifact.entry_link_symbol == '_tiny_runtime_start'
	sections := macho_tiny_runtime_test_sections(artifact.object_bytes)
	assert sections.len == 1
	assert sections[0].sectname == '__text'
	text := macho_tiny_test_text(artifact.object_bytes, MachoTinyTestSection{
		offset: sections[0].offset
		size:   sections[0].size
		reloff: sections[0].reloff
		nreloc: sections[0].nreloc
		flags:  sections[0].flags
	})
	assert text[..18] == macho_tiny_runtime_wrapper_bytes(.scalar) or { panic(err) }
	assert text[18..] == [u8(0x31), 0xc0, 0xc3]
	symtab := macho_tiny_runtime_test_symtab(artifact.object_bytes)
	symbols := macho_tiny_test_symbols(artifact.object_bytes, symtab)
	assert macho_tiny_test_symbol_names(artifact.object_bytes, symtab, symbols) == [
		'_tiny_runtime_start',
		'_runtime_entry',
		'_exit',
	]
	assert symbols[0].type_ == 0x0f && symbols[0].section == 1 && symbols[0].value == 0
	assert symbols[1].type_ == 0x0f && symbols[1].section == 1 && symbols[1].value == 18
	assert symbols[2].type_ == 0x01 && symbols[2].section == 0 && symbols[2].value == 0
	relocations := macho_tiny_test_relocations(artifact.object_bytes, MachoTinyTestSection{
		offset: sections[0].offset
		size:   sections[0].size
		reloff: sections[0].reloff
		nreloc: sections[0].nreloc
		flags:  sections[0].flags
	})
	assert relocations.len == 2
	assert relocations[0].address == 5 && relocations[0].symbol_index == 1
	assert relocations[1].address == 12 && relocations[1].symbol_index == 2
	for relocation in relocations {
		assert relocation.pc_relative && relocation.length == 2 && relocation.external
		assert relocation.type_ == 2
	}
}

fn test_macho_tiny_runtime_t04_role_signature_name_and_startup_refusals_are_immutable() {
	o := macho_tiny_test_leaf_object('runtime_entry') or { panic(err) }
	before_text := o.text.clone()
	before_symbols := o.symbols.clone()
	assert macho_tiny_runtime_test_error(&o, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: 0
			result_policy:  .void_
		}
		entry_wrapper_name: 'tiny_runtime_start'
		exit_symbol_name:   'exit'
	}).contains('startup policy')
	assert macho_tiny_runtime_test_error(&o, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: 0
			parameter_count: 1
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'tiny_runtime_start'
		exit_symbol_name:   'exit'
	}).contains('must not accept parameters')
	assert macho_tiny_runtime_test_error(&o, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: 0
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'runtime_entry'
		exit_symbol_name:   'exit'
	}).contains('wrapper collides')
	assert macho_tiny_runtime_test_error(&o, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: 0
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'tiny_runtime_start'
		exit_symbol_name:   ''
	}).contains('exit symbol name must not be empty')
	assert macho_tiny_runtime_test_error(&o, MachoTinyRuntimeDefinition{
		entry:                 MachoTinyEntryDefinition{
			function_index: 0
			result_policy:  .void_
		}
		startup_policy:        .no_args_no_init
		entry_wrapper_name:    'tiny_runtime_start'
		allocator_symbol_name: 'malloc'
		exit_symbol_name:      'exit'
	}).contains('must be empty without helpers')
	mut private_object := macho_tiny_test_leaf_object('private_entry') or { panic(err) }
	private_plan := private_data_preflight([
		PrivateDataDefinition{
			name:      'private_value'
			value:     1
			width:     64
			alignment: 8
		},
	], ['private_entry']) or { panic(err) }
	private_object.install_private_data(&private_plan) or { panic(err) }
	assert macho_tiny_runtime_test_error(&private_object, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: 0
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'tiny_runtime_start'
		exit_symbol_name:   'exit'
	}).contains('does not support private data')
	assert o.text == before_text
	assert o.symbols == before_symbols
	assert o.call_relocations.len == 0
	assert object_data_is_empty(&o.object_data)
}

fn test_macho_tiny_runtime_t05_helper_roles_require_unique_reachable_intentional_externals() {
	mut o := Object.new()
	entry := o.intern_function_symbol('entry') or { panic(err) }
	i64_helper := o.intern_external_function_symbol('declared_i64') or { panic(err) }
	concat_helper := o.intern_external_function_symbol('declared_concat') or { panic(err) }
	assert o.append_text([u8(0xe8), 0, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0xc3]) or {
		panic(err)
	} == 0
	o.define_text_function(entry, 0, 11) or { panic(err) }
	o.add_text_call_relocation(1, concat_helper) or { panic(err) }
	o.add_text_call_relocation(6, i64_helper) or { panic(err) }
	definition := MachoTinyRuntimeDefinition{
		entry:                 MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .void_
		}
		startup_policy:        .no_args_no_init
		entry_wrapper_name:    'start'
		allocator_symbol_name: 'malloc'
		exit_symbol_name:      'exit'
		helper_bindings:       [
			MachoTinyRuntimeHelperBinding{
				external_function_index: u32(concat_helper)
				kind:                    .string_concat
			},
			MachoTinyRuntimeHelperBinding{
				external_function_index: u32(i64_helper)
				kind:                    .i64_decimal
			},
		]
	}
	artifact := macho64_tiny_runtime_artifact(&o, definition) or { panic(err) }
	symtab := macho_tiny_runtime_test_symtab(artifact.object_bytes)
	symbols := macho_tiny_test_symbols(artifact.object_bytes, symtab)
	assert macho_tiny_test_symbol_names(artifact.object_bytes, symtab, symbols) == [
		'_start',
		'_entry',
		'_declared_i64',
		'_declared_concat',
		'_exit',
		'_malloc',
	]
	assert symbols[2].type_ == 0x0f && symbols[3].type_ == 0x0f
	assert macho_tiny_runtime_test_error(&o, MachoTinyRuntimeDefinition{
		entry:                 definition.entry
		startup_policy:        .no_args_no_init
		entry_wrapper_name:    'start'
		allocator_symbol_name: 'malloc'
		exit_symbol_name:      'exit'
		helper_bindings:       [
			MachoTinyRuntimeHelperBinding{
				external_function_index: u32(i64_helper)
				kind:                    .i64_decimal
			},
			MachoTinyRuntimeHelperBinding{
				external_function_index: u32(i64_helper)
				kind:                    .string_concat
			},
		]
	}).contains('bound more than once')
	assert macho_tiny_runtime_test_error(&o, MachoTinyRuntimeDefinition{
		entry:              definition.entry
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'start'
		exit_symbol_name:   'exit'
		helper_bindings:    [MachoTinyRuntimeHelperBinding{
			external_function_index: u32(i64_helper)
			kind:                    .i64_decimal
		}]
	}).contains('allocator name is required')

	mut stale := Object.new()
	stale_entry := stale.intern_function_symbol('stale_entry') or { panic(err) }
	dead := stale.intern_function_symbol('dead') or { panic(err) }
	stale_helper := stale.intern_external_function_symbol('stale_helper') or { panic(err) }
	assert stale.append_text([u8(0xc3), 0xe8, 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	stale.define_text_function(stale_entry, 0, 1) or { panic(err) }
	stale.define_text_function(dead, 1, 6) or { panic(err) }
	stale.add_text_call_relocation(2, stale_helper) or { panic(err) }
	assert macho_tiny_runtime_test_error(&stale, MachoTinyRuntimeDefinition{
		entry:                 MachoTinyEntryDefinition{
			function_index: u32(stale_entry)
			result_policy:  .void_
		}
		startup_policy:        .no_args_no_init
		entry_wrapper_name:    'start'
		allocator_symbol_name: 'malloc'
		exit_symbol_name:      'exit'
		helper_bindings:       [MachoTinyRuntimeHelperBinding{
			external_function_index: u32(stale_helper)
			kind:                    .i64_decimal
		}]
	}).contains('not reachable')
}

fn test_macho_tiny_runtime_t06_i64_helper_handles_full_domain_and_failure_path_manifest() {
	o, entry, helper := macho_tiny_runtime_test_helper_object('declared_i64')
	artifact := macho64_tiny_runtime_artifact(&o, MachoTinyRuntimeDefinition{
		entry:                 MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .void_
		}
		startup_policy:        .no_args_no_init
		entry_wrapper_name:    'start'
		allocator_symbol_name: 'malloc'
		exit_symbol_name:      'exit'
		helper_bindings:       [MachoTinyRuntimeHelperBinding{
			external_function_index: u32(helper)
			kind:                    .i64_decimal
		}]
	}) or { panic(err) }
	section := macho_tiny_runtime_test_section(artifact.object_bytes, '__text')
	text := macho_tiny_test_text(artifact.object_bytes, MachoTinyTestSection{
		offset: section.offset
		size:   section.size
		reloff: section.reloff
		nreloc: section.nreloc
		flags:  section.flags
	})
	manifest := macho_tiny_runtime_i64_decimal_manifest() or { panic(err) }
	assert text[24..24 + manifest.bytes.len] == manifest.bytes
	assert macho_tiny_runtime_test_i64_model(min_i64) == '-9223372036854775808'
	assert macho_tiny_runtime_test_i64_model(0) == '0'
	assert macho_tiny_runtime_test_i64_model(max_i64) == '9223372036854775807'
	assert manifest.bytes[20..31] == [u8(0xbf), 1, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0x0f]
}

fn test_macho_tiny_runtime_t07_concat_helper_checks_u32_sum_allocation_and_null_failure() {
	o, entry, helper := macho_tiny_runtime_test_helper_object('declared_concat')
	artifact := macho64_tiny_runtime_artifact(&o, MachoTinyRuntimeDefinition{
		entry:                 MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .void_
		}
		startup_policy:        .no_args_no_init
		entry_wrapper_name:    'start'
		allocator_symbol_name: 'malloc'
		exit_symbol_name:      'exit'
		helper_bindings:       [MachoTinyRuntimeHelperBinding{
			external_function_index: u32(helper)
			kind:                    .string_concat
		}]
	}) or { panic(err) }
	section := macho_tiny_runtime_test_section(artifact.object_bytes, '__text')
	text := macho_tiny_test_text(artifact.object_bytes, MachoTinyTestSection{
		offset: section.offset
		size:   section.size
		reloff: section.reloff
		nreloc: section.nreloc
		flags:  section.flags
	})
	manifest := macho_tiny_runtime_string_concat_manifest() or { panic(err) }
	assert text[24..24 + manifest.bytes.len] == manifest.bytes
	assert macho_tiny_runtime_test_concat_allocation(0, 0) or { panic(err) } == 1
	assert macho_tiny_runtime_test_concat_allocation(max_u32, 0) or { panic(err) } == u64(max_u32) +
		1
	if _ := macho_tiny_runtime_test_concat_allocation(max_u32, 1) {
		assert false
	}
	if _ := macho_tiny_runtime_test_concat_allocation(u64(max_u32) + 1, 0) {
		assert false
	}
	assert manifest.bytes[54..65] == [u8(0xbf), 1, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0x0f]
}

fn test_macho_tiny_runtime_t08_global_relocation_classification_precedes_reachability_pruning() {
	mut valid := Object.new()
	entry := valid.intern_function_symbol('entry') or { panic(err) }
	dead := valid.intern_function_symbol('dead') or { panic(err) }
	assert valid.append_text([u8(0xc3), 0, 0, 0, 0, 0xc3]) or { panic(err) } == 0
	valid.define_text_function(entry, 0, 1) or { panic(err) }
	valid.define_text_function(dead, 1, 5) or { panic(err) }
	valid_definition := ObjectDataDefinition{
		sections: [ObjectDataSection{
			kind:      .rodata
			bytes:     [u8(1), 2, 3, 4]
			size:      4
			alignment: 4
		}]
		symbols:  [ObjectDataSymbol{
			kind:    .named
			name:    '_dead_data'
			section: .rodata
			size:    4
		}]
		relocations: [macho_tiny_runtime_test_pc_relocation(1, 0, 0)]
	}
	macho_tiny_runtime_test_install_data(mut valid, &valid_definition)
	artifact := macho64_tiny_runtime_artifact(&valid, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'start'
		exit_symbol_name:   'exit'
	}) or { panic(err) }
	assert macho_tiny_runtime_test_sections(artifact.object_bytes).map(it.sectname) == ['__text']

	mut refused := Object.new()
	refused_entry := refused.intern_function_symbol('entry') or { panic(err) }
	refused_dead := refused.intern_function_symbol('dead') or { panic(err) }
	assert refused.append_text([u8(0xc3), 0, 0, 0, 0, 0, 0, 0, 0, 0xc3]) or {
		panic(err)
	} == 0
	refused.define_text_function(refused_entry, 0, 1) or { panic(err) }
	refused.define_text_function(refused_dead, 1, 9) or { panic(err) }
	refused_definition := ObjectDataDefinition{
		sections: [ObjectDataSection{
			kind:      .data
			bytes:     []u8{len: 8}
			size:      8
			alignment: 8
		}]
		symbols:  [ObjectDataSymbol{
			kind:    .named
			name:    '_dead_absolute'
			section: .data
			size:    8
		}]
		relocations: [ObjectDataRelocation{
			source_section: .text
			offset:         1
			target_symbol:  object_data_symbol_ref(0)
			width:          64
			kind:           .absolute
			signedness:     .unsigned
			address_intent: .virtual_address
			pc_bias:        .zero
			got_access:     .none
		}]
	}
	macho_tiny_runtime_test_install_data(mut refused, &refused_definition)
	before := object_data_clone(refused.object_data.sections, refused.object_data.symbols,
		refused.object_data.relocations)
	assert macho_tiny_runtime_test_error(&refused, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: u32(refused_entry)
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'start'
		exit_symbol_name:   'exit'
	}).contains('unsupported')
	assert refused.object_data.sections == before.sections
	assert refused.object_data.symbols == before.symbols
	assert refused.object_data.relocations == before.relocations
}

fn test_macho_tiny_runtime_t09_alias_ancestry_closure_ids_and_external_form_targets_are_exact() {
	o, entry := macho_tiny_runtime_test_single_data_object([u8(0), 0, 0, 0, 0xc3],
		ObjectDataSection{
			kind:      .rodata
			bytes:     [u8(0xa0), 0xa1, 0xa2, 0xa3, 0xb0, 0xb1, 0xb2, 0xb3, 0xc0]
			size:      9
			alignment: 8
		}, [
		ObjectDataSymbol{
			kind:    .named
			name:    '_pruned_root'
			section: .rodata
			offset:  0
			size:    2
		},
		ObjectDataSymbol{
			kind:    .named
			name:    '_canonical'
			section: .rodata
			offset:  4
			size:    4
		},
		ObjectDataSymbol{
			kind:     .named
			name:     '_alias_parent'
			section:  .rodata
			offset:   4
			size:     4
			alias_of: object_data_symbol_ref(1)
		},
		ObjectDataSymbol{
			kind:     .named
			name:     '_alias_leaf'
			section:  .rodata
			offset:   4
			size:     4
			alias_of: object_data_symbol_ref(2)
		},
		ObjectDataSymbol{
			kind:     .named
			name:     '_pruned_sibling'
			section:  .rodata
			offset:   4
			size:     4
			alias_of: object_data_symbol_ref(1)
		},
		ObjectDataSymbol{
			kind:     .named
			name:     '_pruned_descendant'
			section:  .rodata
			offset:   4
			size:     4
			alias_of: object_data_symbol_ref(4)
		},
	], [macho_tiny_runtime_test_pc_relocation(0, 3, 3)])
	reachability := macho_tiny_collect_reachable(&o, int(entry), []) or { panic(err) }
	classified := macho_tiny_runtime_classify_data_relocations(&o) or { panic(err) }
	plan := macho_tiny_runtime_build_data_plan(&o, &reachability, classified) or {
		panic(err)
	}
	assert plan.definition.sections.len == 1
	assert plan.definition.sections[0].alignment == 8
	assert plan.definition.sections[0].bytes == [u8(0), 0, 0, 0, 0xb0, 0xb1, 0xb2, 0xb3]
	assert plan.definition.symbols.len == 3
	assert plan.definition.symbols.map(it.offset) == [u64(4), 4, 4]
	assert !plan.definition.symbols[0].alias_of.is_set
	assert plan.definition.symbols[1].alias_of == object_data_symbol_ref(0)
	assert plan.definition.symbols[2].alias_of == object_data_symbol_ref(1)
	assert plan.symbol_kept == [false, true, true, true, false, false]
	assert plan.old_to_new == [ObjectDataSymbolID(0), 0, 1, 2, 0, 0]
	assert plan.relocations[0].target_index == 3
	assert plan.relocations[0].relocation.addend == 3
	assert o.object_data.symbols[3].offset + u64(plan.relocations[0].relocation.addend) == 7
	assert plan.definition.symbols[2].offset + u64(plan.relocations[0].relocation.addend) == 7

	artifact := macho64_tiny_runtime_artifact(&o, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'start'
		exit_symbol_name:   'exit'
	}) or { panic(err) }
	symtab := macho_tiny_runtime_test_symtab(artifact.object_bytes)
	symbols := macho_tiny_test_symbols(artifact.object_bytes, symtab)
	assert macho_tiny_test_symbol_names(artifact.object_bytes, symtab, symbols) == [
		'_start',
		'_data_entry',
		'_exit',
		'_canonical',
		'_alias_parent',
		'_alias_leaf',
	]
	for index in 3 .. 6 {
		assert symbols[index].type_ == 0x0e
		assert symbols[index].section == 2
	}
	text_section := macho_tiny_runtime_test_section(artifact.object_bytes, '__text')
	text := macho_tiny_test_text(artifact.object_bytes, MachoTinyTestSection{
		offset: text_section.offset
		size:   text_section.size
		reloff: text_section.reloff
		nreloc: text_section.nreloc
		flags:  text_section.flags
	})
	assert text[18..22] == [u8(3), 0, 0, 0]
	relocations := macho_tiny_test_relocations(artifact.object_bytes, MachoTinyTestSection{
		offset: text_section.offset
		size:   text_section.size
		reloff: text_section.reloff
		nreloc: text_section.nreloc
		flags:  text_section.flags
	})
	assert relocations.len == 3
	assert relocations[2].address == 18
	assert relocations[2].symbol_index == 5
	assert relocations[2].external
	assert relocations[2].type_ == 1
}

fn test_macho_tiny_runtime_t10_adjacent_intervals_coalesce_with_alignment_congruence() {
	o, entry := macho_tiny_runtime_test_single_data_object([u8(0), 0, 0, 0, 0, 0, 0, 0,
		0xc3], ObjectDataSection{
		kind:      .data
		bytes:     [u8(10), 11, 12, 13, 14, 15, 16, 17]
		size:      8
		alignment: 8
	}, [
		ObjectDataSymbol{
			kind:    .named
			name:    '_left'
			section: .data
			offset:  2
			size:    2
		},
		ObjectDataSymbol{
			kind:    .named
			name:    '_right'
			section: .data
			offset:  4
			size:    2
		},
	], [
		macho_tiny_runtime_test_pc_relocation(0, 0, 1),
		macho_tiny_runtime_test_pc_relocation(4, 1, 1),
	])
	reachability := macho_tiny_collect_reachable(&o, int(entry), []) or { panic(err) }
	classified := macho_tiny_runtime_classify_data_relocations(&o) or { panic(err) }
	plan := macho_tiny_runtime_build_data_plan(&o, &reachability, classified) or {
		panic(err)
	}
	assert plan.definition.sections[0].bytes == [u8(0), 0, 12, 13, 14, 15]
	assert plan.definition.sections[0].size == 6
	assert plan.definition.symbols.map(it.offset) == [u64(2), 4]
	assert plan.definition.symbols[0].offset % 8 == 2
	assert plan.definition.symbols[1].offset % 8 == 4
	artifact := macho64_tiny_runtime_artifact(&o, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'start'
		exit_symbol_name:   'exit'
	}) or { panic(err) }
	data_section := macho_tiny_runtime_test_section(artifact.object_bytes, '__data')
	assert data_section.size == 6
	assert data_section.alignment_power == 3

	mut overlap := Object.new()
	overlap_entry := overlap.intern_function_symbol('entry') or { panic(err) }
	assert overlap.append_text([u8(0xc3)]) or { panic(err) } == 0
	overlap.define_text_function(overlap_entry, 0, 1) or { panic(err) }
	overlap_definition := ObjectDataDefinition{
		sections: [ObjectDataSection{
			kind:      .rodata
			bytes:     []u8{len: 8}
			size:      8
			alignment: 4
		}]
		symbols:  [
			ObjectDataSymbol{
				kind:    .internal
				section: .rodata
				offset:  2
				size:    3
			},
			ObjectDataSymbol{
				kind:    .internal
				section: .rodata
				offset:  4
				size:    2
			},
		]
	}
	if _ := object_data_preflight(&overlap_definition, &overlap) {
		assert false, 'unrelated partial overlap unexpectedly passed'
	}
}

fn test_macho_tiny_runtime_t11_addend_must_stay_inside_own_half_open_symbol_interval() {
	for addend in [i64(3), -1, 4, min_i64] {
		o, entry := macho_tiny_runtime_test_single_data_object([u8(0), 0, 0, 0, 0xc3],
			ObjectDataSection{
				kind:      .rodata
				bytes:     []u8{len: 12}
				size:      12
				alignment: 4
			}, [ObjectDataSymbol{
				kind:    .named
				name:    '_bounded'
				section: .rodata
				offset:  4
				size:    4
			}], [macho_tiny_runtime_test_pc_relocation(0, 0, 0)])
		mut candidate := macho_tiny_runtime_test_clone_object(&o)
		candidate.object_data.relocations[0].addend = addend
		before := object_data_clone(candidate.object_data.sections, candidate.object_data.symbols,
			candidate.object_data.relocations)
		definition := MachoTinyRuntimeDefinition{
			entry:              MachoTinyEntryDefinition{
				function_index: u32(entry)
				result_policy:  .void_
			}
			startup_policy:     .no_args_no_init
			entry_wrapper_name: 'start'
			exit_symbol_name:   'exit'
		}
		if addend == 3 {
			artifact := macho64_tiny_runtime_artifact(&candidate, definition) or { panic(err) }
			text_section := macho_tiny_runtime_test_section(artifact.object_bytes, '__text')
			text := macho_tiny_test_text(artifact.object_bytes, MachoTinyTestSection{
				offset: text_section.offset
				size:   text_section.size
				reloff: text_section.reloff
				nreloc: text_section.nreloc
				flags:  text_section.flags
			})
			assert text[18..22] == [u8(3), 0, 0, 0]
		} else {
			message := macho_tiny_runtime_test_error(&candidate, definition)
			assert message.contains('effective target') || message.contains('escapes')
		}
		assert candidate.object_data.sections == before.sections
		assert candidate.object_data.symbols == before.symbols
		assert candidate.object_data.relocations == before.relocations
	}
}

fn test_macho_tiny_runtime_t12_only_signed_pc32_and_explicit_got_intents_are_accepted() {
	o, entry := macho_tiny_runtime_test_single_data_object([
		u8(0),
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0x48,
		0x8b,
		0x05,
		0,
		0,
		0,
		0,
		0xc3,
	], ObjectDataSection{
		kind:      .rodata
		bytes:     [u8(1), 2, 3, 4]
		size:      4
		alignment: 4
	}, [ObjectDataSymbol{
		kind:    .named
		name:    '_mapped'
		section: .rodata
		size:    4
	}], [
		macho_tiny_runtime_test_pc_relocation(0, 0, 0),
		macho_tiny_runtime_test_got_relocation(4, 0, .address),
		macho_tiny_runtime_test_got_relocation(11, 0, .load),
	])
	artifact := macho64_tiny_runtime_artifact(&o, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'start'
		exit_symbol_name:   'exit'
	}) or { panic(err) }
	text_section := macho_tiny_runtime_test_section(artifact.object_bytes, '__text')
	relocations := macho_tiny_test_relocations(artifact.object_bytes, MachoTinyTestSection{
		offset: text_section.offset
		size:   text_section.size
		reloff: text_section.reloff
		nreloc: text_section.nreloc
		flags:  text_section.flags
	})
	assert relocations.map(it.type_) == [u32(2), 2, 1, 4, 3]

	mut biased := macho_tiny_runtime_test_clone_object(&o)
	biased.object_data.relocations[0].pc_bias = .one
	biased.object_data.relocations[0].addend = 1
	assert macho_tiny_runtime_test_error(&biased, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'start'
		exit_symbol_name:   'exit'
	}).contains('bias must be zero')

	data_source, data_entry := macho_tiny_runtime_test_single_data_object([u8(0xc3)],
		ObjectDataSection{
			kind:      .rodata
			bytes:     []u8{len: 8}
			size:      8
			alignment: 4
		}, [ObjectDataSymbol{
			kind:    .named
			name:    '_source_target'
			section: .rodata
			offset:  4
			size:    4
		}], [ObjectDataRelocation{
			source_section: .rodata
			offset:         0
			target_symbol:  object_data_symbol_ref(0)
			width:          32
			kind:           .pc_relative
			signedness:     .signed
			address_intent: .virtual_address
			pc_bias:        .zero
			got_access:     .none
		}])
	assert macho_tiny_runtime_test_error(&data_source, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: u32(data_entry)
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'start'
		exit_symbol_name:   'exit'
	}).contains('must originate in __text')

	bss_target, bss_entry := macho_tiny_runtime_test_single_data_object([u8(0), 0, 0, 0,
		0xc3], ObjectDataSection{
		kind:      .bss
		size:      4
		alignment: 4
	}, [ObjectDataSymbol{
		kind:    .named
		name:    '_bss_target'
		section: .bss
		size:    4
	}], [macho_tiny_runtime_test_pc_relocation(0, 0, 0)])
	assert macho_tiny_runtime_test_error(&bss_target, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: u32(bss_entry)
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'start'
		exit_symbol_name:   'exit'
	}).contains('must own non-empty rodata or data')
}

fn test_macho_tiny_runtime_t13_got_load_instruction_must_fit_one_unique_function() {
	canonical, canonical_entry := macho_tiny_runtime_test_single_data_object([u8(0x48),
		0x8b, 0x05, 0, 0, 0, 0, 0xc3], ObjectDataSection{
		kind:      .rodata
		bytes:     [u8(1)]
		size:      1
		alignment: 1
	}, [ObjectDataSymbol{
		kind:    .named
		name:    '_got_value'
		section: .rodata
		size:    1
	}], [macho_tiny_runtime_test_got_relocation(3, 0, .load)])
	_ = macho64_tiny_runtime_artifact(&canonical, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: u32(canonical_entry)
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'start'
		exit_symbol_name:   'exit'
	}) or { panic(err) }

	mut boundary := Object.new()
	prefix := boundary.intern_function_symbol('prefix') or { panic(err) }
	entry := boundary.intern_function_symbol('entry') or { panic(err) }
	assert boundary.append_text([u8(0x48), 0x8b, 0x05, 0, 0, 0, 0, 0xc3]) or {
		panic(err)
	} == 0
	boundary.define_text_function(prefix, 0, 2) or { panic(err) }
	boundary.define_text_function(entry, 2, 6) or { panic(err) }
	boundary_definition := ObjectDataDefinition{
		sections: [ObjectDataSection{
			kind:      .rodata
			bytes:     [u8(1)]
			size:      1
			alignment: 1
		}]
		symbols:  [ObjectDataSymbol{
			kind:    .named
			name:    '_boundary_got'
			section: .rodata
			size:    1
		}]
		relocations: [macho_tiny_runtime_test_got_relocation(3, 0, .load)]
	}
	macho_tiny_runtime_test_install_data(mut boundary, &boundary_definition)
	before_text := boundary.text.clone()
	assert macho_tiny_runtime_test_error(&boundary, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'start'
		exit_symbol_name:   'exit'
	}).contains('not contained in exactly one function')
	assert boundary.text == before_text

	mut bad_opcode := macho_tiny_runtime_test_clone_object(&canonical)
	bad_opcode.text[1] = 0x8d
	assert macho_tiny_runtime_test_error(&bad_opcode, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: u32(canonical_entry)
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'start'
		exit_symbol_name:   'exit'
	}).contains('canonical RIP-relative MOVQ')
}

fn test_macho_tiny_runtime_t14_external_order_role_reuse_and_wrapper_link_name_are_deterministic() {
	mut o := Object.new()
	entry := o.intern_function_symbol('entry') or { panic(err) }
	dead := o.intern_function_symbol('dead') or { panic(err) }
	zeta := o.intern_external_function_symbol('zeta') or { panic(err) }
	quit := o.intern_external_function_symbol('quit') or { panic(err) }
	assert o.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3, 0xe8, 0, 0, 0, 0, 0xc3]) or {
		panic(err)
	} == 0
	o.define_text_function(entry, 0, 6) or { panic(err) }
	o.define_text_function(dead, 6, 6) or { panic(err) }
	o.add_text_call_relocation(1, zeta) or { panic(err) }
	o.add_text_call_relocation(7, quit) or { panic(err) }
	definition := MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .scalar
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'returned_start'
		exit_symbol_name:   'quit'
	}
	first := macho64_tiny_runtime_artifact(&o, definition) or { panic(err) }
	second := macho64_tiny_runtime_artifact(&o, definition) or { panic(err) }
	assert first.object_bytes == second.object_bytes
	assert first.entry_link_symbol == '_returned_start'
	symtab := macho_tiny_runtime_test_symtab(first.object_bytes)
	symbols := macho_tiny_test_symbols(first.object_bytes, symtab)
	assert macho_tiny_test_symbol_names(first.object_bytes, symtab, symbols) == [
		'_returned_start',
		'_entry',
		'_quit',
		'_zeta',
	]
	assert symbols[2].type_ == 0x01 && symbols[3].type_ == 0x01
}

fn test_macho_tiny_runtime_t15_bounds_and_success_or_refusal_are_deeply_immutable() {
	o := macho_tiny_test_leaf_object('entry') or { panic(err) }
	before_text := o.text.clone()
	before_symbols := o.symbols.clone()
	definition := macho_tiny_runtime_test_definition(0, .void_)
	_ = macho64_tiny_runtime_artifact(&o, definition) or { panic(err) }
	assert o.text == before_text
	assert o.symbols == before_symbols
	assert o.call_relocations.len == 0
	assert object_data_is_empty(&o.object_data)
	assert macho_tiny_runtime_padding_for_residue(1, u64(1) << 63,
		(u64(1) << 63) - 1) or { panic(err) } == (u64(1) << 63) - 2
	if _ := macho_tiny_runtime_padding_for_residue(0, 3, 0) {
		assert false
	}
	mut bytes := []u8{len: 4}
	if _ := macho_tiny_runtime_patch_rel32(mut bytes, 1, 0) {
		assert false
	}

	mut bss := Object.new()
	bss_entry := bss.intern_function_symbol('entry') or { panic(err) }
	assert bss.append_text([u8(0xc3)]) or { panic(err) } == 0
	bss.define_text_function(bss_entry, 0, 1) or { panic(err) }
	bss_definition := ObjectDataDefinition{
		sections: [ObjectDataSection{
			kind:      .bss
			size:      max_u64
			alignment: 1
		}]
	}
	macho_tiny_runtime_test_install_data(mut bss, &bss_definition)
	artifact := macho64_tiny_runtime_artifact(&bss, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: u32(bss_entry)
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'start'
		exit_symbol_name:   'exit'
	}) or { panic(err) }
	assert macho_tiny_runtime_test_sections(artifact.object_bytes).map(it.sectname) == ['__text']
	assert bss.object_data.sections[0].size == max_u64
}

fn test_macho_tiny_runtime_t16_legacy_api_bytes_gate_and_closed_writer_adjacency_are_preserved() {
	leaf := macho_tiny_test_leaf_object('entry') or { panic(err) }
	legacy := macho64_tiny_artifact(&leaf, MachoTinyEntryDefinition{
		function_index: 0
		result_policy:  .void_
	}) or { panic(err) }
	direct := macho64_relocatable_bytes(&leaf) or { panic(err) }
	assert legacy.object_bytes == direct
	assert legacy.entry_link_symbol == '_entry'

	data_object, data_entry := macho_tiny_runtime_test_single_data_object([u8(0), 0, 0, 0,
		0xc3], ObjectDataSection{
		kind:      .rodata
		bytes:     [u8(1), 2, 3, 4]
		size:      4
		alignment: 4
	}, [ObjectDataSymbol{
		kind:    .named
		name:    '_adjacent'
		section: .rodata
		size:    4
	}], [macho_tiny_runtime_test_pc_relocation(0, 0, 0)])
	assert macho_tiny_test_error(&data_object, MachoTinyEntryDefinition{
		function_index: u32(data_entry)
		result_policy:  .void_
	}).contains('requires explicit object-format writer support')
	runtime := macho64_tiny_runtime_artifact(&data_object, MachoTinyRuntimeDefinition{
		entry:              MachoTinyEntryDefinition{
			function_index: u32(data_entry)
			result_policy:  .void_
		}
		startup_policy:     .no_args_no_init
		entry_wrapper_name: 'start'
		exit_symbol_name:   'exit'
	}) or { panic(err) }
	assert macho_tiny_runtime_test_sections(runtime.object_bytes).map(it.sectname) == [
		'__text',
		'__const',
	]
	assert macho_tiny_runtime_test_section(runtime.object_bytes, '__const').alignment_power == 2
}

fn macho_tiny_runtime_test_find_tool(candidates []string) string {
	for candidate in candidates {
		if os.is_abs_path(candidate) {
			if os.is_executable(candidate) {
				return candidate
			}
			continue
		}
		if path := os.find_abs_path_of_executable(candidate) {
			return path
		}
	}
	return ''
}

fn macho_tiny_runtime_test_fingerprint(path string, args []string, expected []string) bool {
	result := macho_tiny_test_run_process(path, args, macho_tiny_test_environment(),
		macho_tiny_test_timeout_ms)
	if result.timed_out || result.output_limited || result.exit_code != 0 {
		return false
	}
	output := result.stdout + result.stderr
	for needle in expected {
		if !output.contains(needle) {
			return false
		}
	}
	return true
}

fn macho_tiny_runtime_test_raw_oracle(mandatory bool) {
	$if !linux || !amd64 {
		assert !mandatory, 'mandatory Mach-O tiny raw oracle requires the provisioned Linux AMD64 host'
		eprintln('Mach-O tiny raw oracle: SKIPPED/UNPROVEN')
		return
	}
	$if linux && amd64 {
		clang := macho_tiny_runtime_test_find_tool([
			'/usr/lib/llvm-21/bin/clang',
			'clang-21',
		])
		readobj := macho_tiny_runtime_test_find_tool([
			'/usr/lib/llvm-21/bin/llvm-readobj',
			'llvm-readobj-21',
		])
		objdump := macho_tiny_runtime_test_find_tool([
			'/usr/lib/llvm-21/bin/llvm-objdump',
			'llvm-objdump-21',
		])
		if clang.len == 0 || readobj.len == 0 || objdump.len == 0 {
			assert !mandatory, 'mandatory Mach-O tiny raw oracle tools are unavailable'
			eprintln('Mach-O tiny raw oracle: SKIPPED/UNPROVEN (tools unavailable)')
			return
		}
		fingerprints_match :=
			macho_tiny_runtime_test_fingerprint(clang, ['--version'], [
				'Ubuntu clang version 21.1.8 (6ubuntu1)',
				'Target: x86_64-pc-linux-gnu',
			]) && macho_tiny_runtime_test_fingerprint(readobj, ['--version'], [
				'Ubuntu LLVM version 21.1.8',
			]) && macho_tiny_runtime_test_fingerprint(objdump, ['--version'], [
				'Ubuntu LLVM version 21.1.8',
			])
		if !fingerprints_match {
			assert !mandatory, 'mandatory Mach-O tiny raw oracle fingerprints do not match'
			eprintln('Mach-O tiny raw oracle: SKIPPED/UNPROVEN (fingerprint mismatch)')
			return
		}

		o, entry := macho_tiny_runtime_test_single_data_object([u8(0x48), 0x8b, 0x05,
			0, 0, 0, 0, 0xc3], ObjectDataSection{
			kind:      .rodata
			bytes:     [u8(0x2a)]
			size:      1
			alignment: 1
		}, [ObjectDataSymbol{
			kind:    .named
			name:    '_raw_value'
			section: .rodata
			size:    1
		}], [macho_tiny_runtime_test_got_relocation(3, 0, .load)])
		artifact := macho64_tiny_runtime_artifact(&o, MachoTinyRuntimeDefinition{
			entry:              MachoTinyEntryDefinition{
				function_index: u32(entry)
				result_policy:  .void_
			}
			startup_policy:     .no_args_no_init
			entry_wrapper_name: 'raw_start'
			exit_symbol_name:   'exit'
		}) or { panic(err) }
		root := macho_tiny_test_root('runtime_raw_oracle')
		defer {
			macho_tiny_test_cleanup(root)
		}
		writer_path := os.join_path(root, 'writer object.o')
		assembly_path := os.join_path(root, 'reference source.s')
		reference_path := os.join_path(root, 'reference object.o')
		publish_object(writer_path, artifact.object_bytes) or { panic(err) }
		os.write_file(assembly_path, '.text\n.globl _ref_entry\n_ref_entry:\n  movq _ref_data@GOTPCREL(%rip), %rax\n  retq\n.section __TEXT,__const\n_ref_data:\n  .byte 42\n') or {
			panic(err)
		}
		compile := macho_tiny_test_run_process(clang, [
			'-target',
			'x86_64-apple-macos11',
			'-c',
			assembly_path,
			'-o',
			reference_path,
		], macho_tiny_test_environment(), macho_tiny_test_timeout_ms)
		assert !compile.timed_out && !compile.output_limited
		assert compile.exit_code == 0, compile.stderr
		writer_read := macho_tiny_test_command_output(readobj, [
			'--file-headers',
			'--sections',
			'--symbols',
			'--relocations',
			'--section-data',
			writer_path,
		], macho_tiny_test_environment()) or { panic(err) }
		reference_read := macho_tiny_test_command_output(readobj, [
			'--sections',
			'--symbols',
			'--relocations',
			'--section-data',
			reference_path,
		], macho_tiny_test_environment()) or { panic(err) }
		writer_dump := macho_tiny_test_command_output(objdump, [
			'--macho',
			'--section-headers',
			'--reloc',
			'--syms',
			writer_path,
		], macho_tiny_test_environment()) or { panic(err) }
		for needle in ['Format: Mach-O 64-bit x86-64', 'Name: __text', 'Name: __const',
			'X86_64_RELOC_BRANCH', 'X86_64_RELOC_GOT_LOAD', '_raw_value'] {
			assert writer_read.contains(needle), 'writer raw oracle missing `${needle}`'
		}
		for needle in ['Name: __text', 'Name: __const', 'X86_64_RELOC_GOT_LOAD',
			'_ref_data'] {
			assert reference_read.contains(needle), 'Clang raw oracle missing `${needle}`'
		}
		assert writer_dump.contains('GOT_LOAD') || writer_dump.contains('GOT')
		assert writer_dump.contains('_raw_value')
	}
}

fn macho_tiny_runtime_test_file_sha256(path string) !string {
	return sha256.sum256(os.read_bytes(path)!).hex()
}

fn macho_tiny_runtime_test_decimal_version(value string) bool {
	parts := value.split('.')
	if parts.len < 2 || parts.len > 3 {
		return false
	}
	for part in parts {
		if part.len == 0 {
			return false
		}
		for character in part {
			if character < `0` || character > `9` {
				return false
			}
		}
	}
	return true
}

fn macho_tiny_runtime_test_apple_emit_u32(mut bytes []u8, value u32) {
	bytes << u8(value)
	bytes << u8(value >> 8)
	bytes << u8(value >> 16)
	bytes << u8(value >> 24)
}

fn macho_tiny_runtime_test_apple_emit_u64(mut bytes []u8, value u64) {
	for shift in 0 .. 8 {
		bytes << u8(value >> (shift * 8))
	}
}

fn macho_tiny_runtime_test_apple_emit_movabs(mut bytes []u8, prefix u8, opcode u8, value u64) {
	bytes << [prefix, opcode]
	macho_tiny_runtime_test_apple_emit_u64(mut bytes, value)
}

fn macho_tiny_runtime_test_apple_emit_checked_prologue(mut bytes []u8) {
	bytes << [u8(0x53), 0x55, 0x41, 0x54, 0x41, 0x55, 0x41, 0x56, 0x41, 0x57]
	macho_tiny_runtime_test_apple_emit_movabs(mut bytes, 0x48, 0xbb, 0x11223344)
	macho_tiny_runtime_test_apple_emit_movabs(mut bytes, 0x48, 0xbd, 0x22334455)
	macho_tiny_runtime_test_apple_emit_movabs(mut bytes, 0x49, 0xbc, 0x33445566)
	macho_tiny_runtime_test_apple_emit_movabs(mut bytes, 0x49, 0xbd, 0x44556677)
	macho_tiny_runtime_test_apple_emit_movabs(mut bytes, 0x49, 0xbe, 0x55667711)
	bytes << [u8(0x48), 0x83, 0xec, 0x08, 0x49, 0x89, 0xe7]
}

fn macho_tiny_runtime_test_apple_emit_failure_branch(mut bytes []u8, mut failures []int, opcode u8) {
	failures << macho_tiny_runtime_emit_jcc(mut bytes, opcode)
}

fn macho_tiny_runtime_test_apple_emit_preserved_check(mut bytes []u8, mut failures []int, prefix u8, modrm u8, value u32) {
	bytes << [prefix, u8(0x81), modrm]
	macho_tiny_runtime_test_apple_emit_u32(mut bytes, value)
	macho_tiny_runtime_test_apple_emit_failure_branch(mut bytes, mut failures, 0x85)
}

fn macho_tiny_runtime_test_apple_emit_abi_checks(mut bytes []u8, mut failures []int) {
	bytes << [u8(0x48), 0x83, 0xc4, 0x08]
	bytes << [u8(0x4c), 0x8d, 0x54, 0x24, 0xf8, 0x4d, 0x39, 0xfa]
	macho_tiny_runtime_test_apple_emit_failure_branch(mut bytes, mut failures, 0x85)
	macho_tiny_runtime_test_apple_emit_preserved_check(mut bytes, mut failures, 0x48, 0xfb,
		0x11223344)
	macho_tiny_runtime_test_apple_emit_preserved_check(mut bytes, mut failures, 0x48, 0xfd,
		0x22334455)
	macho_tiny_runtime_test_apple_emit_preserved_check(mut bytes, mut failures, 0x49, 0xfc,
		0x33445566)
	macho_tiny_runtime_test_apple_emit_preserved_check(mut bytes, mut failures, 0x49, 0xfd,
		0x44556677)
	macho_tiny_runtime_test_apple_emit_preserved_check(mut bytes, mut failures, 0x49, 0xfe,
		0x55667711)
	bytes << [u8(0x9c), 0x41, 0x5a, 0x41, 0xf7, 0xc2, 0x00, 0x04, 0x00, 0x00]
	macho_tiny_runtime_test_apple_emit_failure_branch(mut bytes, mut failures, 0x85)
}

fn macho_tiny_runtime_test_apple_finish_checked_entry(mut bytes []u8, failures []int, failure_code u32) {
	bytes << [u8(0x31), 0xc0]
	success_jump := macho_tiny_runtime_emit_jmp(mut bytes)
	failure_start := bytes.len
	bytes << u8(0xb8)
	macho_tiny_runtime_test_apple_emit_u32(mut bytes, failure_code)
	epilogue := bytes.len
	bytes << [u8(0x41), 0x5f, 0x41, 0x5e, 0x41, 0x5d, 0x41, 0x5c, 0x5d, 0x5b, 0xc3]
	macho_tiny_runtime_patch_rel32(mut bytes, success_jump, epilogue) or { panic(err) }
	for field in failures {
		macho_tiny_runtime_patch_rel32(mut bytes, field, failure_start) or { panic(err) }
	}
}

fn macho_tiny_runtime_test_apple_i64_success_fixture() (Object, MachoTinyRuntimeDefinition) {
	mut o := Object.new()
	entry := o.intern_function_symbol('apple_i64_entry') or { panic(err) }
	helper := o.intern_external_function_symbol('apple_i64_decimal') or { panic(err) }
	mut bytes := []u8{}
	mut failures := []int{}
	macho_tiny_runtime_test_apple_emit_checked_prologue(mut bytes)
	bytes << [u8(0x48), 0xc7, 0xc7, 0xd6, 0xff, 0xff, 0xff]
	call_field := macho_tiny_runtime_emit_call(mut bytes)
	macho_tiny_runtime_test_apple_emit_abi_checks(mut bytes, mut failures)
	bytes << [u8(0x48), 0x83, 0xfa, 0x03]
	macho_tiny_runtime_test_apple_emit_failure_branch(mut bytes, mut failures, 0x85)
	for displacement, expected in [u8(0x2d), 0x34, 0x32, 0x00] {
		if displacement == 0 {
			bytes << [u8(0x80), 0x38, expected]
		} else {
			bytes << [u8(0x80), 0x78, u8(displacement), expected]
		}
		macho_tiny_runtime_test_apple_emit_failure_branch(mut bytes, mut failures, 0x85)
	}
	macho_tiny_runtime_test_apple_finish_checked_entry(mut bytes, failures, 41)
	assert o.append_text(bytes) or { panic(err) } == 0
	o.define_text_function(entry, 0, u64(bytes.len)) or { panic(err) }
	o.add_text_call_relocation(call_field, helper) or { panic(err) }
	return o, MachoTinyRuntimeDefinition{
		entry:                 MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .scalar
		}
		startup_policy:        .no_args_no_init
		entry_wrapper_name:    'apple_i64_start'
		allocator_symbol_name: 'malloc'
		exit_symbol_name:      'exit'
		helper_bindings:       [MachoTinyRuntimeHelperBinding{
			external_function_index: u32(helper)
			kind:                    .i64_decimal
		}]
	}
}

fn macho_tiny_runtime_test_apple_concat_success_fixture() (Object, MachoTinyRuntimeDefinition) {
	mut o := Object.new()
	entry := o.intern_function_symbol('apple_concat_entry') or { panic(err) }
	helper := o.intern_external_function_symbol('apple_string_concat') or { panic(err) }
	mut bytes := []u8{}
	mut failures := []int{}
	macho_tiny_runtime_test_apple_emit_checked_prologue(mut bytes)
	bytes << [u8(0x48), 0x8d, 0x3d]
	left_field := u64(bytes.len)
	bytes << [u8(0), 0, 0, 0, 0xbe, 0x02, 0x00, 0x00, 0x00]
	bytes << [u8(0x48), 0x8d, 0x15]
	right_field := u64(bytes.len)
	bytes << [u8(0), 0, 0, 0, 0xb9, 0x03, 0x00, 0x00, 0x00]
	call_field := macho_tiny_runtime_emit_call(mut bytes)
	macho_tiny_runtime_test_apple_emit_abi_checks(mut bytes, mut failures)
	bytes << [u8(0x48), 0x85, 0xc0]
	macho_tiny_runtime_test_apple_emit_failure_branch(mut bytes, mut failures, 0x84)
	bytes << [u8(0x48), 0x83, 0xfa, 0x05]
	macho_tiny_runtime_test_apple_emit_failure_branch(mut bytes, mut failures, 0x85)
	for displacement, expected in [u8(`a`), `b`, `C`, `D`, `E`, 0] {
		if displacement == 0 {
			bytes << [u8(0x80), 0x38, expected]
		} else {
			bytes << [u8(0x80), 0x78, u8(displacement), expected]
		}
		macho_tiny_runtime_test_apple_emit_failure_branch(mut bytes, mut failures, 0x85)
	}
	macho_tiny_runtime_test_apple_finish_checked_entry(mut bytes, failures, 42)
	assert o.append_text(bytes) or { panic(err) } == 0
	o.define_text_function(entry, 0, u64(bytes.len)) or { panic(err) }
	o.add_text_call_relocation(call_field, helper) or { panic(err) }
	data_definition := ObjectDataDefinition{
		sections: [ObjectDataSection{
			kind:      .rodata
			bytes:     [u8(`a`), `b`, `C`, `D`, `E`]
			size:      5
			alignment: 1
		}]
		symbols:  [
			ObjectDataSymbol{
				kind:    .named
				name:    '_apple_left'
				section: .rodata
				size:    2
			},
			ObjectDataSymbol{
				kind:    .named
				name:    '_apple_right'
				section: .rodata
				offset:  2
				size:    3
			},
		]
		relocations: [
			macho_tiny_runtime_test_pc_relocation(left_field, 0, 0),
			macho_tiny_runtime_test_pc_relocation(right_field, 1, 0),
		]
	}
	macho_tiny_runtime_test_install_data(mut o, &data_definition)
	return o, MachoTinyRuntimeDefinition{
		entry:                 MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .scalar
		}
		startup_policy:        .no_args_no_init
		entry_wrapper_name:    'apple_concat_start'
		allocator_symbol_name: 'malloc'
		exit_symbol_name:      'exit'
		helper_bindings:       [MachoTinyRuntimeHelperBinding{
			external_function_index: u32(helper)
			kind:                    .string_concat
		}]
	}
}

fn macho_tiny_runtime_test_apple_i64_null_fixture() (Object, MachoTinyRuntimeDefinition) {
	mut o := Object.new()
	entry := o.intern_function_symbol('apple_i64_null_entry') or { panic(err) }
	helper := o.intern_external_function_symbol('apple_i64_null') or { panic(err) }
	mut bytes := [u8(0x48), 0x83, 0xec, 0x08, 0xbf, 0x7b, 0x00, 0x00, 0x00]
	call_field := macho_tiny_runtime_emit_call(mut bytes)
	bytes << [u8(0x48), 0x83, 0xc4, 0x08, 0xb8, 0x5b, 0x00, 0x00, 0x00, 0xc3]
	assert o.append_text(bytes) or { panic(err) } == 0
	o.define_text_function(entry, 0, u64(bytes.len)) or { panic(err) }
	o.add_text_call_relocation(call_field, helper) or { panic(err) }
	return o, MachoTinyRuntimeDefinition{
		entry:                 MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .scalar
		}
		startup_policy:        .no_args_no_init
		entry_wrapper_name:    'apple_i64_null_start'
		allocator_symbol_name: 'tiny_null_alloc'
		exit_symbol_name:      'exit'
		helper_bindings:       [MachoTinyRuntimeHelperBinding{
			external_function_index: u32(helper)
			kind:                    .i64_decimal
		}]
	}
}

fn macho_tiny_runtime_test_apple_concat_overflow_fixture() (Object, MachoTinyRuntimeDefinition) {
	mut o := Object.new()
	entry := o.intern_function_symbol('apple_concat_overflow_entry') or { panic(err) }
	helper := o.intern_external_function_symbol('apple_concat_overflow') or { panic(err) }
	mut bytes := [
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
		0x01,
		0x00,
		0x00,
		0x00,
	]
	call_field := macho_tiny_runtime_emit_call(mut bytes)
	bytes << [u8(0x48), 0x83, 0xc4, 0x08, 0xb8, 0x5c, 0x00, 0x00, 0x00, 0xc3]
	assert o.append_text(bytes) or { panic(err) } == 0
	o.define_text_function(entry, 0, u64(bytes.len)) or { panic(err) }
	o.add_text_call_relocation(call_field, helper) or { panic(err) }
	return o, MachoTinyRuntimeDefinition{
		entry:                 MachoTinyEntryDefinition{
			function_index: u32(entry)
			result_policy:  .scalar
		}
		startup_policy:        .no_args_no_init
		entry_wrapper_name:    'apple_concat_overflow_start'
		allocator_symbol_name: 'malloc'
		exit_symbol_name:      'exit'
		helper_bindings:       [MachoTinyRuntimeHelperBinding{
			external_function_index: u32(helper)
			kind:                    .string_concat
		}]
	}
}

fn macho_tiny_runtime_test_apple_run_fixture(clang string, ld string, objdump string, codesign string, sdk string, sdk_version string, root string, fixture_name string, o &Object, definition MachoTinyRuntimeDefinition, expected_exit int, extra_objects []string, expected_symbols []string) {
	artifact := macho64_tiny_runtime_artifact(o, definition) or { panic(err) }
	object_path := os.join_path(root, '${fixture_name} runtime.o')
	image_path := os.join_path(root, '${fixture_name} runtime image')
	publish_object(object_path, artifact.object_bytes) or { panic(err) }
	object_inspection := macho_tiny_test_command_output(objdump, [
		'--macho',
		'--section-headers',
		'--syms',
		'--reloc',
		object_path,
	], macho_tiny_test_environment()) or { panic(err) }
	assert object_inspection.contains(artifact.entry_link_symbol)
	for symbol in expected_symbols {
		assert object_inspection.contains(symbol), 'mandatory Apple fixture `${fixture_name}` is missing `${symbol}`'
	}
	mut link_args := [
		'-target',
		'x86_64-apple-macos${macho_tiny_test_macos_minimum}',
		'-isysroot',
		sdk,
		'-fuse-ld=${ld}',
		'-nostartfiles',
		'-Wl,-platform_version,macos,${macho_tiny_test_macos_minimum},${sdk_version}',
		object_path,
	]
	link_args << extra_objects
	link_args << [
		'-Wl,-e,${artifact.entry_link_symbol}',
		'-Wl,-dead_strip',
		'-lSystem',
		'-o',
		image_path,
	]
	link := macho_tiny_test_run_process(clang, link_args, macho_tiny_test_environment(),
		macho_tiny_test_timeout_ms)
	assert !link.timed_out && !link.output_limited
	assert link.exit_code == 0, 'mandatory Apple `${fixture_name}` link failed: ${link.stderr}'
	assert os.is_file(image_path) && !os.is_link(image_path)
	sign := macho_tiny_test_run_process(codesign, ['--force', '--sign', '-', image_path],
		macho_tiny_test_environment(), macho_tiny_test_timeout_ms)
	assert !sign.timed_out && !sign.output_limited
	assert sign.exit_code == 0, 'mandatory Apple `${fixture_name}` signing failed: ${sign.stderr}'
	image_inspection := macho_tiny_test_command_output(objdump, [
		'--macho',
		'--private-headers',
		'--syms',
		image_path,
	], macho_tiny_test_environment()) or { panic(err) }
	assert image_inspection.contains(artifact.entry_link_symbol)
	run := macho_tiny_test_run_process(image_path, []string{}, macho_tiny_test_environment(),
		macho_tiny_test_timeout_ms)
	assert !run.timed_out && !run.output_limited
	assert run.exit_code == expected_exit, 'mandatory Apple `${fixture_name}` returned ${run.exit_code}, expected ${expected_exit}'
}

fn macho_tiny_runtime_test_apple_oracle(mandatory bool) {
	i64_object, i64_definition := macho_tiny_runtime_test_apple_i64_success_fixture()
	_ = macho64_tiny_runtime_artifact(&i64_object, i64_definition) or { panic(err) }
	concat_object, concat_definition := macho_tiny_runtime_test_apple_concat_success_fixture()
	_ = macho64_tiny_runtime_artifact(&concat_object, concat_definition) or { panic(err) }
	null_object, null_definition := macho_tiny_runtime_test_apple_i64_null_fixture()
	_ = macho64_tiny_runtime_artifact(&null_object, null_definition) or { panic(err) }
	overflow_object, overflow_definition := macho_tiny_runtime_test_apple_concat_overflow_fixture()
	_ = macho64_tiny_runtime_artifact(&overflow_object, overflow_definition) or { panic(err) }
	if !mandatory {
		eprintln('Mach-O tiny Apple linked oracle: SKIPPED/UNPROVEN/UNCLOSED')
		return
	}
	$if !macos || !amd64 {
		assert false, 'mandatory Mach-O tiny Apple linked oracle requires a provisioned Apple AMD64 host'
	}
	$if macos && amd64 {
		clang := os.getenv('V3_MACHO_TINY_APPLE_CLANG')
		ld := os.getenv('V3_MACHO_TINY_APPLE_LD64')
		objdump := os.getenv('V3_MACHO_TINY_APPLE_LLVM_OBJDUMP')
		codesign := os.getenv('V3_MACHO_TINY_APPLE_CODESIGN')
		sdk := os.getenv('V3_MACHO_TINY_APPLE_SDK')
		sdk_version := os.getenv('V3_MACHO_TINY_APPLE_SDK_VERSION')
		clang_fingerprint := os.getenv('V3_MACHO_TINY_APPLE_CLANG_FINGERPRINT')
		ld_fingerprint := os.getenv('V3_MACHO_TINY_APPLE_LD_FINGERPRINT')
		objdump_fingerprint := os.getenv('V3_MACHO_TINY_APPLE_LLVM_FINGERPRINT')
		codesign_hash := os.getenv('V3_MACHO_TINY_APPLE_CODESIGN_SHA256')
		sdk_settings_hash := os.getenv('V3_MACHO_TINY_APPLE_SDK_SETTINGS_SHA256')
		libsystem_hash := os.getenv('V3_MACHO_TINY_APPLE_LIBSYSTEM_SHA256')
		assert clang.len != 0 && ld.len != 0 && objdump.len != 0 && codesign.len != 0
			&& sdk.len != 0, 'mandatory Mach-O tiny Apple tool/SDK paths are incomplete'
		for path in [clang, ld, objdump, codesign] {
			assert os.is_abs_path(path) && os.is_executable(path), 'mandatory Apple tool is not an absolute executable'
		}
		assert os.is_abs_path(sdk) && os.is_dir(sdk), 'mandatory Apple SDK is not an existing absolute directory'
		assert macho_tiny_runtime_test_decimal_version(sdk_version), 'mandatory Apple SDK version is invalid'
		assert clang_fingerprint.len != 0 && ld_fingerprint.len != 0
			&& objdump_fingerprint.len != 0 && codesign_hash.len == 64
			&& sdk_settings_hash.len == 64 && libsystem_hash.len == 64, 'mandatory Apple fingerprints are incomplete'
		assert macho_tiny_runtime_test_fingerprint(clang, ['--version'], [
			clang_fingerprint,
		]), 'mandatory Apple Clang fingerprint does not match'
		assert macho_tiny_runtime_test_fingerprint(ld, ['-v'], [
			ld_fingerprint,
		]), 'mandatory Apple ld64 fingerprint does not match'
		assert macho_tiny_runtime_test_fingerprint(objdump, ['--version'], [
			objdump_fingerprint,
		]), 'mandatory Apple llvm-objdump fingerprint does not match'
		assert macho_tiny_runtime_test_file_sha256(codesign) or {
			panic(err)
		} == codesign_hash, 'mandatory Apple codesign fingerprint does not match'
		sdk_settings := os.join_path(sdk, 'SDKSettings.json')
		libsystem := os.join_path(sdk, 'usr', 'lib', 'libSystem.tbd')
		assert os.is_file(sdk_settings) && os.is_file(libsystem), 'mandatory Apple SDK inputs are missing'
		assert macho_tiny_runtime_test_file_sha256(sdk_settings) or { panic(err) } == sdk_settings_hash
		assert macho_tiny_runtime_test_file_sha256(libsystem) or { panic(err) } == libsystem_hash

		root := macho_tiny_test_root('runtime_apple_oracle')
		defer {
			macho_tiny_test_cleanup(root)
		}
		null_allocator_source := os.join_path(root, 'null allocator.s')
		null_allocator_object := os.join_path(root, 'null allocator.o')
		os.write_file(null_allocator_source,
			'.text\n.p2align 4, 0x90\n.globl _tiny_null_alloc\n_tiny_null_alloc:\n  xorl %eax, %eax\n  retq\n') or {
			panic(err)
		}
		compile_support := macho_tiny_test_run_process(clang, [
			'-target',
			'x86_64-apple-macos${macho_tiny_test_macos_minimum}',
			'-isysroot',
			sdk,
			'-c',
			null_allocator_source,
			'-o',
			null_allocator_object,
		], macho_tiny_test_environment(), macho_tiny_test_timeout_ms)
		assert !compile_support.timed_out && !compile_support.output_limited
		assert compile_support.exit_code == 0, 'mandatory Apple support compile failed: ${compile_support.stderr}'
		assert os.is_file(null_allocator_object) && !os.is_link(null_allocator_object)

		macho_tiny_runtime_test_apple_run_fixture(clang, ld, objdump, codesign, sdk,
			sdk_version, root, 'i64 success', &i64_object, i64_definition, 0, []string{},
			['_apple_i64_decimal', '_malloc', '_exit'])

		macho_tiny_runtime_test_apple_run_fixture(clang, ld, objdump, codesign, sdk,
			sdk_version, root, 'concat success', &concat_object, concat_definition, 0,
			[]string{}, ['_apple_string_concat', '_malloc', '_exit'])

		macho_tiny_runtime_test_apple_run_fixture(clang, ld, objdump, codesign, sdk,
			sdk_version, root, 'allocator null', &null_object, null_definition, 1,
			[null_allocator_object], ['_apple_i64_null', '_tiny_null_alloc', '_exit'])

		macho_tiny_runtime_test_apple_run_fixture(clang, ld, objdump, codesign, sdk,
			sdk_version, root, 'concat overflow', &overflow_object, overflow_definition, 1,
			[]string{}, ['_apple_concat_overflow', '_malloc', '_exit'])
	}
}

fn test_macho_tiny_runtime_t17_fingerprinted_raw_clang_llvm_oracle_is_bounded() {
	guard := os.getenv(macho_tiny_runtime_raw_guard)
	assert guard in ['', '1'], '${macho_tiny_runtime_raw_guard} must be empty or 1'
	macho_tiny_runtime_test_raw_oracle(guard == '1')
}

fn test_macho_tiny_runtime_t18_apple_link_run_sign_and_inspection_is_mandatory_only_when_provisioned() {
	guard := os.getenv(macho_tiny_runtime_apple_guard)
	assert guard in ['', '1'], '${macho_tiny_runtime_apple_guard} must be empty or 1'
	macho_tiny_runtime_test_apple_oracle(guard == '1')
}
