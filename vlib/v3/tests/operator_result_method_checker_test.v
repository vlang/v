import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_operator_result_method_checker_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn write_clock_project(name string, main_src string) string {
	root := os.join_path(os.temp_dir(), 'v3_operator_result_method_${name}')
	os.rmdir_all(root) or {}
	clock_dir := os.join_path(root, 'clock')
	os.mkdir_all(clock_dir) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'operator_result_method' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(clock_dir, 'clock.v'), 'module clock

pub struct Moment {
	value int
}

pub type Span = i64

pub fn now() Moment {
	return Moment{
		value: 1
	}
}

pub fn (lhs Moment) - (rhs Moment) Span {
	return Span(lhs.value - rhs.value)
}

pub fn (span Span) milliseconds() i64 {
	return i64(span)
}
') or {
		panic(err)
	}
	main_path := os.join_path(root, 'main.v')
	os.write_file(main_path, main_src) or { panic(err) }
	return main_path
}

fn check_to_c(v3_bin string, name string, main_src string) os.Result {
	main_path := write_clock_project(name, main_src)
	c_path := os.join_path(os.temp_dir(), 'v3_operator_result_method_${name}.c')
	os.rm(c_path) or {}
	return os.execute('${v3_bin} ${main_path} -b c -o ${c_path}')
}

fn check_standalone_to_c(v3_bin string, name string, main_src string) os.Result {
	main_path := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(main_path, main_src) or { panic(err) }
	c_path := os.join_path(os.temp_dir(), 'v3_${name}.c')
	os.rm(c_path) or {}
	return os.execute('${v3_bin} ${main_path} -b c -o ${c_path}')
}

fn run_standalone(v3_bin string, name string, main_src string) string {
	main_path := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(main_path, main_src) or { panic(err) }
	bin_path := os.join_path(os.temp_dir(), 'v3_${name}')
	os.rm(bin_path) or {}
	os.rm(bin_path + '.c') or {}
	compile := os.execute('${v3_bin} ${main_path} -b c -o ${bin_path}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin_path)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_operator_result_type_is_used_before_method_resolution() {
	v3_bin := build_v3()
	good := 'module main

import clock

fn main() {
	println((clock.now() - clock.now()).milliseconds())
	println(clock.Span(123000000).milliseconds())
	span := clock.Span(10)
	_ := span.milliseconds()
}
'
	good_res := check_to_c(v3_bin, 'good', good)
	assert good_res.exit_code == 0, good_res.output
	assert !good_res.output.contains('unknown function `milliseconds`'), good_res.output

	time_good := 'module main

import time

fn main() {
	println((time.now() - time.now()).milliseconds())
	println(time.Duration(123000000).milliseconds())
	d := time.Duration(10)
	_ := d.milliseconds()
}
'
	time_res := check_to_c(v3_bin, 'time_good', time_good)
	assert time_res.exit_code == 0, time_res.output
	assert !time_res.output.contains('unknown function `milliseconds`'), time_res.output

	time_standalone := 'module main

import time

fn main() {
	d := time.Duration(10); _ := d.milliseconds()
}
'
	time_standalone_res := check_standalone_to_c(v3_bin, 'probe_time_duration_method_clean',
		time_standalone)
	assert time_standalone_res.exit_code == 0, time_standalone_res.output
	assert !time_standalone_res.output.contains('unknown function d.milliseconds'), time_standalone_res.output

	bad := 'module main

import clock

fn main() {
	println((clock.now() - clock.now()).bogus_method())
}
'
	bad_res := check_to_c(v3_bin, 'bad', bad)
	assert bad_res.exit_code != 0, bad_res.output
	assert bad_res.output.contains('unknown function'), bad_res.output
	assert bad_res.output.contains('bogus_method'), bad_res.output
}

fn test_string_alias_operator_overloads_are_preserved() {
	v3_bin := build_v3()
	out := run_standalone(v3_bin, 'string_alias_operator_overloads', "type S = string

fn (a S) == (b S) bool {
	return false
}

fn (a S) < (b S) bool {
	return false
}

fn main() {
	assert !(S('x') == S('x'))
	assert !(S('a') < S('b'))
	println('ok')
}
")
	assert out == 'ok'
}
