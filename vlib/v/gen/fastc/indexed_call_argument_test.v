module fastc

import os
import v.cmdexec

fn test_fastc_pointer_call_with_indexed_arguments_is_passed_by_value() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_indexed_call_argument_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, r'module main

struct Record {
	value int
}
type Value = Record | int

fn record(value int) &Record { return &Record{value: value} }
fn boxed(value &Value, expected int) &Value { assert expected == 42; return value }
fn read_record(value Record) int { return value.value }
fn read_value(value Value) int {
	return match value {
		Record { value.value }
		int { value }
	}
}
fn identity(value int) int { return value }
fn next(mut calls int) int { calls++; return 0 }
fn slice_len(values []int) int { return values.len }
fn forward(mut values []int) int { return slice_len(values[1..]) }
fn grouped_forward(mut values []int) int { return slice_len((values[1..])) }
fn main() {
	mut values := [42, 43]
	mut calls := 0
	assert read_record(record(values[next(mut calls)])) == 42
	assert calls == 1
	value := Value(Record{value: 42})
	assert read_value(boxed(&value, identity(values[next(mut calls)]))) == 42
	assert calls == 2
	assert read_record((record(values[next(mut calls)]))) == 42
	assert calls == 3
	assert forward(mut values) == 1
	assert grouped_forward(mut values) == 1
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-d', 'fastc_real_builtin',
		'-nocache', '-gc', 'none', '-o', program, source], @VEXEROOT, {
		'VEXE':                          @VEXE
		'VFLAGS':                        ''
		'VOSARGS':                       ''
		'V_MACOS_V3_NO_FALLBACK':        '1'
		'V_C_ERROR_BUG_REPORT_DISABLED': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
