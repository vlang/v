module fastc

import os
import v.cmdexec

// Constructs that vlib's own modules and tests use: `&?T` parameters (json2), `continue`
// in a loop inside a closure inside a loop (arrays), escaped keyword identifiers (cbor,
// mysql, datatypes), and `#flag` target prefixes for other platforms (gg, iconv).
const vlib_constructs_source = 'module main

#flag wasm32_emscripten --embed-file missing@/missing
#flag termux -L/data/missing -lmissing
#flag -DFASTC_UNDEFINED_LATER=1
#flag linux -DFASTC_FLAG_VALUE=7 -UFASTC_UNDEFINED_LATER
#flag macos -DFASTC_FLAG_VALUE=7

#insert "@DIR/check.h"

fn C.fastc_flag_value() int
fn C.fastc_undefined_later() int

struct Value {
	@type  string
	@union int
}

fn describe(@none int) string {
	return "none=\${@none}"
}

fn set_option(field &?rune, is_null bool) {
	if is_null {
		unsafe {
			*field = none
		}
		return
	}
	value := `x`
	unsafe {
		*field = value
	}
}

fn main() {
	mut total := 0
	for i in 0 .. 3 {
		odd_sum := fn (n int) int {
			mut sum := 0
			for j in 0 .. n {
				if j % 2 == 0 {
					continue
				}
				sum += j
			}
			return sum
		}
		total += odd_sum(i + 4)
	}
	assert total == 17

	v := Value{
		@type:  "map"
		@union: 3
	}
	@lock := v.@union + 1
	assert v.@type == "map"
	assert @lock == 4
	assert describe(5) == "none=5"

	mut option := ?rune(none)
	set_option(&option, false)
	if r := option {
		assert r == `x`
	} else {
		assert false
	}
	set_option(&option, true)
	if _ := option {
		assert false
	}

	assert C.fastc_flag_value() == 7
	assert C.fastc_undefined_later() == 0
	println("vlib constructs work")
}
'

const vlib_constructs_header = '#ifdef FASTC_UNDEFINED_LATER
static int fastc_undefined_later(void) { return 1; }
#else
static int fastc_undefined_later(void) { return 0; }
#endif
static int fastc_flag_value(void) { return FASTC_FLAG_VALUE; }
'

// fastc_build_program builds `source` (plus a `check.h` beside it) with FastC into `output`
// in a fresh directory named after `name`, which the caller removes.
fn fastc_build_program(name string, source string, output string) (string, string) {
	root := os.join_path(os.vtmp_dir(), '${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	os.write_file(os.join_path(root, 'check.h'), vlib_constructs_header) or { panic(err) }
	program := os.join_path(root, output)
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root],
		os.real_path(os.join_path(@DIR, '../../../..')), {
			'VEXE':                   @VEXE
			'VFLAGS':                 ''
			'VOSARGS':                ''
			'V_MACOS_V3_NO_FALLBACK': '1'
		})
	assert build.exit_code == 0, build.output
	return root, program
}

fn test_fastc_vlib_constructs_runtime() {
	$if !macos && !linux {
		return
	}
	root, program := fastc_build_program('fastc_vlib_constructs', vlib_constructs_source, 'program')
	defer { os.rmdir_all(root) or {} }
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'vlib constructs work', run.output
}

// `!` after a method called on a call's result (`mk(4).raw()!`), as a comparison or
// arithmetic operand and before a member access, is lowered like any other propagation; the
// call-result renderer used to take the expression and leave the `!` in the C. So is the
// base of an index, `pick(4)![1]`.
const call_result_propagation_source = 'module main

struct Range {
	lo   int
	note string
}

struct Sig {
	v int
}

fn (s Sig) raw() !int {
	if s.v < 0 {
		return error("negative")
	}
	return s.v * 2
}

fn (s Sig) range() !Range {
	return Range{
		lo:   s.v
		note: "n\${s.v}"
	}
}

fn mk(v int) Sig {
	return Sig{v}
}

fn pick(n int) ![]int {
	return [n, n + 1]
}

fn lo_of(r &Range) int {
	return r.lo
}

fn run() ! {
	lo := lo_of(mk(9).range()!)
	assert lo == 9
	assert pick(4)![1] == 5
	x := mk(4).raw()!
	assert x == 8
	assert mk(3).raw()! == 6
	assert 6 == mk(3).raw()!
	assert mk(5).raw()! + 1 == 11
	assert mk(7).range()!.note == "n7"
	assert mk(7).range()!.lo + 1 == 8
	mk(-1).raw() or { return }
	assert false
}

fn main() {
	run() or { panic(err) }
	println("call result propagation works")
}
'

fn test_fastc_propagation_on_a_call_result_member_runtime() {
	$if !macos && !linux {
		return
	}
	root, program := fastc_build_program('fastc_call_result_propagation', call_result_propagation_source,
		'program')
	defer { os.rmdir_all(root) or {} }
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'call result propagation works', run.output
}

// A test file inside vlib/builtin is built with its module's files, minus the ones the
// FastC runtime leaves out (map.v, which v3_backend replaces, declared `DenseArray` twice).
fn test_fastc_builds_a_test_file_inside_builtin() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_builtin_test_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	program := os.join_path(root, 'hex_test')
	vroot := os.real_path(os.join_path(@DIR, '../../../..'))
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-o', program,
		os.join_path(vroot, 'vlib', 'builtin', 'hex_test.v')], vroot, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
