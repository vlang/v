module driver

import os
import v.cmdexec
import v.markused
import v.parser
import v.pref
import v.transform
import v.types

// msvc_inline_asm_error_for parses `source` as a program for `ccompiler` and returns what
// msvc_inline_asm_error reports for it.
fn msvc_inline_asm_error_for(name string, ccompiler string, source string) ?string {
	path := os.join_path(os.vtmp_dir(), 'msvc_inline_asm_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('windows', 'amd64') or { panic(err) }
	prefs.ccompiler = ccompiler
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used := markused.mark_used(a, tc)
	return msvc_inline_asm_error(a, used)
}

const msvc_inline_asm_used_source = 'fn add_asm(a int) int {
	mut r := 0
	asm amd64 {
		mov r, a
		; =r (r)
		; r (a)
	}
	return r
}

fn main() {
	println(add_asm(42))
}
'

// write_msvc_inline_asm_source writes the program with a used asm block to a temporary file.
fn write_msvc_inline_asm_source(name string) string {
	path := os.join_path(os.vtmp_dir(), 'msvc_inline_asm_${name}_${os.getpid()}.v')
	os.write_file(path, msvc_inline_asm_used_source) or { panic(err) }
	return path
}

fn test_c_output_for_msvc_keeps_the_inline_asm_block() {
	// Text output is not compiled by V, and a portable snapshot resolves `$if msvc` only when
	// the C compiler runs, so only builds that V hands to `cl` are rejected.
	os.setenv('V_C_ERROR_BUG_REPORT_DISABLED', '1', true)
	path := write_msvc_inline_asm_source('c_output')
	c_path := path.all_before_last('.v') + '.c'
	defer {
		os.rm(path) or {}
		os.rm(c_path) or {}
	}
	result := cmdexec.run(os.join_path(@VMODROOT, 'v'), ['-os', 'windows', '-cc', 'msvc', '-o',
		c_path, path])
	assert result.exit_code == 0, result.output
	assert os.read_file(c_path)!.contains('__asm__')
}

fn test_a_build_for_cl_stops_with_a_located_error() {
	// Needs the real cl.exe: the lanes that build with `-cc msvc` have it, anywhere else this
	// test has nothing to run.
	$if !windows {
		return
	}
	os.find_abs_path_of_executable('cl') or { return }
	os.setenv('V_C_ERROR_BUG_REPORT_DISABLED', '1', true)
	path := write_msvc_inline_asm_source('build')
	exe_path := path.all_before_last('.v') + '.exe'
	defer {
		os.rm(path) or {}
		os.rm(exe_path) or {}
	}
	result := cmdexec.run(os.join_path(@VMODROOT, 'v'), ['-cc', 'msvc', '-o', exe_path, path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('.v:3:2: error: inline assembly is not supported when the C compiler is MSVC'), result.output
	assert !result.output.contains('C4013'), result.output
	assert !os.exists(exe_path)
}

fn test_a_used_inline_asm_block_is_reported_at_its_source_position() {
	msg := msvc_inline_asm_error_for('used', 'msvc', msvc_inline_asm_used_source) or {
		assert false, 'a used asm block was not reported'
		return
	}
	assert msg.contains('.v:3:2: error: inline assembly is not supported when the C compiler is MSVC'), msg
	assert msg.contains('`\$if !msvc`'), msg
}

fn test_inline_asm_in_an_unused_function_is_not_reported() {
	source := msvc_inline_asm_used_source.replace('println(add_asm(42))', 'println(42)')
	if msg := msvc_inline_asm_error_for('unused', 'msvc', source) {
		assert false, 'unused asm was reported: ${msg}'
	}
}

fn test_inline_asm_in_a_branch_guarded_with_not_msvc_is_not_reported() {
	source := 'fn add_asm(a int) int {
	mut r := 0
	\$if !msvc {
		asm amd64 {
			mov r, a
			; =r (r)
			; r (a)
		}
	} \$else {
		r = a
	}
	return r
}

fn main() {
	println(add_asm(42))
}
'
	if msg := msvc_inline_asm_error_for('guarded', 'msvc', source) {
		assert false, 'a block guarded by `\$if !msvc` was reported: ${msg}'
	}
	// The guard is what keeps the block out: under gcc the same program keeps it.
	msg := msvc_inline_asm_error_for('guarded_gcc', 'gcc', source) or {
		assert false, 'the unguarded branch was not scanned under gcc'
		return
	}
	assert msg.contains(': error: inline assembly is not supported'), msg
}

fn test_top_level_inline_asm_in_a_branch_guarded_with_not_msvc_is_not_reported() {
	source := '\$if !msvc {
	asm amd64 {
		.global msvc_asm_guarded_value
		msvc_asm_guarded_value:
		.quad 48321074923
	}
}

fn main() {
	println(42)
}
'
	if msg := msvc_inline_asm_error_for('top_level_guarded', 'msvc', source) {
		assert false, 'a top-level block guarded by `\$if !msvc` was reported: ${msg}'
	}
	msg := msvc_inline_asm_error_for('top_level_guarded_gcc', 'gcc', source) or {
		assert false, 'the unguarded top-level branch was not scanned under gcc'
		return
	}
	assert msg.contains(': error: inline assembly is not supported'), msg
}

fn test_inline_asm_in_a_closure_stored_in_a_const_is_reported() {
	source := 'const adder = fn (a int) int {
	mut r := 0
	asm amd64 {
		mov r, a
		; =r (r)
		; r (a)
	}
	return r
}

fn main() {
	println(adder(42))
}
'
	msg := msvc_inline_asm_error_for('const_closure', 'msvc', source) or {
		assert false, 'asm in a const closure was not reported'
		return
	}
	assert msg.contains('.v:3:2: error: inline assembly is not supported'), msg
}

fn test_top_level_inline_asm_is_reported() {
	source := 'asm amd64 {
	.global msvc_asm_value
	msvc_asm_value:
	.quad 48321074923
}

fn main() {
	println(42)
}
'
	msg := msvc_inline_asm_error_for('top_level', 'msvc', source) or {
		assert false, 'top-level asm was not reported'
		return
	}
	assert msg.contains('.v:1:1: error: inline assembly is not supported'), msg
}
