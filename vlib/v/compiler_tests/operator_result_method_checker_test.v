import os
import v.tempname

fn write_clock_project(root string, main_src string) string {
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

fn check_to_c(name string, main_src string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_operator_result_method_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_path := write_clock_project(os.join_path(dir, 'project'), main_src)
	c_path := os.join_path(dir, 'v3_operator_result_method_${name}.c')
	return os.exec([@VEXE, main_path, '-b', 'c', '-o', c_path])
}

fn check_standalone_to_c(name string, main_src string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_operator_result_method_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	base := os.join_path(dir, 'v3_${name}')
	main_path := base + '.v'
	os.write_file(main_path, main_src) or { panic(err) }
	c_path := base + '.c'
	return os.exec([@VEXE, main_path, '-b', 'c', '-o', c_path])
}

fn run_standalone(name string, main_src string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_operator_result_method_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	bin_path := os.join_path(dir, 'v3_${name}')
	main_path := bin_path + '.v'
	os.write_file(main_path, main_src) or { panic(err) }
	compile := os.exec([@VEXE, main_path, '-b', 'c', '-o', bin_path])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_operator_result_type_is_used_before_method_resolution() {
	good := 'module main

import clock
import time

fn main() {
	println((clock.now() - clock.now()).milliseconds())
	println(clock.Span(123000000).milliseconds())
	span := clock.Span(10)
	_ := span.milliseconds()

	println((time.now() - time.now()).milliseconds())
	println(time.Duration(123000000).milliseconds())
	d := time.Duration(10)
	_ := d.milliseconds()
}
'
	good_res := check_to_c('good', good)
	assert good_res.exit_code == 0, good_res.output
	assert !good_res.output.contains('unknown function `milliseconds`'), good_res.output

	time_standalone := 'module main

import time

fn main() {
	d := time.Duration(10); _ := d.milliseconds()
}
'
	time_standalone_res := check_standalone_to_c('probe_time_duration_method_clean',
		time_standalone)
	assert time_standalone_res.exit_code == 0, time_standalone_res.output
	assert !time_standalone_res.output.contains('unknown function d.milliseconds'), time_standalone_res.output

	bad := 'module main

import clock

fn main() {
	println((clock.now() - clock.now()).bogus_method())
}
'
	bad_res := check_to_c('bad', bad)
	assert bad_res.exit_code != 0, bad_res.output
	assert bad_res.output.contains('unknown method or field: `Span.bogus_method`'), bad_res.output
	assert bad_res.output.contains('bogus_method'), bad_res.output
}

fn test_operator_overloads_and_named_equality_method() {
	out := run_standalone('operator_overloads_and_named_eq', "type S = string

fn (a S) == (b S) bool {
	return false
}

fn (a S) < (b S) bool {
	return false
}

struct Rectangle {
	x int
}

fn (r Rectangle) eq(s Rectangle) bool {
	return r == s
}

fn main() {
	assert !(S('x') == S('x'))
	assert !(S('a') < S('b'))
	println('ok')
	println(Rectangle{x: 1}.eq(Rectangle{x: 1}))
	println(Rectangle{x: 1}.eq(Rectangle{x: 2}))
}
")
	assert out == 'ok\ntrue\nfalse'
}
