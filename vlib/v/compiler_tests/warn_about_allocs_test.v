import os
import v.cmdexec

const warn_allocs_vexe = @VEXE
const warn_allocs_tests_dir = os.dir(@FILE)
const warn_allocs_v3_dir = os.dir(warn_allocs_tests_dir)
const warn_allocs_vlib_dir = os.dir(warn_allocs_v3_dir)
const warn_allocs_v3_src = os.join_path(warn_allocs_v3_dir, 'v.v')

fn build_warn_allocs_v3(root string) string {
	bin := os.join_path(root, 'v3_warn_allocs')
	result := cmdexec.run(warn_allocs_vexe, ['-gc', 'none', '-path',
		'${warn_allocs_vlib_dir}|@vlib|@vmodules', '-o', bin, warn_allocs_v3_src])
	assert result.exit_code == 0, result.output
	return bin
}

fn test_warn_about_allocs_reports_v1_allocation_sites() {
	root := os.join_path(os.vtmp_dir(), 'v3_warn_allocs_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_warn_allocs_v3(root)
	source := os.join_path(root, 'main.v')
	os.write_file(source, "type Name = string

interface Speaker {
	speak()
}

type Expr = Speaker

struct Person {}

type PersonPtr = &Person

fn (p Person) speak() {}

fn (p PersonPtr) speak() {}

fn main() {
	name := 'V'
	array := [1, 2, 3]
	interpolation := 'hello \${name}'
	concatenation := 'hello ' + name
	alias_string_left := Name('a') + 'b'
	alias_string_right := 'a' + Name('b')
	speaker := Speaker(Person{})
	alias_speaker := Expr(Person{})
	person := Person{}
	pointer_alias_speaker := Speaker(PersonPtr(&person))
	freed := ['\${name}' + name] @[freed]
	println(array)
	println(interpolation)
	println(concatenation)
	println(alias_string_left)
	println(alias_string_right)
	speaker.speak()
	alias_speaker.speak()
	pointer_alias_speaker.speak()
	println(freed)
}
")!

	output := os.join_path(root, 'main.c')
	plain := cmdexec.run(v3_bin, ['-silent', '-nocache', '-o', output, source])
	assert plain.exit_code == 0, plain.output
	assert !plain.output.contains('allocation ('), plain.output

	warned := cmdexec.run(v3_bin, ['-silent', '-nocache', '-warn-about-allocs', '-o', output, source])
	assert warned.exit_code == 0, warned.output
	for description in ['array initialization', 'string interpolation', 'string concatenation',
		'cast to interface'] {
		message := 'allocation (${description})'
		expected_count := if description == 'string concatenation' {
			3
		} else if description == 'cast to interface' {
			3
		} else {
			1
		}
		assert warned.output.count(message) == expected_count, warned.output
		assert warned.output.contains('warning: ${message}'), warned.output
	}

	as_errors := cmdexec.run(v3_bin, ['-silent', '-nocache', '-W', '-warn-about-allocs', '-o',
		output, source])
	assert as_errors.exit_code != 0, as_errors.output
	for description in ['array initialization', 'string interpolation', 'string concatenation',
		'cast to interface'] {
		assert as_errors.output.contains('error: allocation (${description})'), as_errors.output
	}

	assignment_source := os.join_path(root, 'assignment_attribute.v')
	os.write_file(assignment_source, 'fn main() {
	freed := [1] @[freed]
	[2].map(it * 2)
	println(freed)
}
')!
	assignment := cmdexec.run(v3_bin, ['-silent', '-nocache', '-o',
		os.join_path(root, 'assignment_attribute.c'), assignment_source])
	assert assignment.exit_code == 0, assignment.output

	nonallocating_source := os.join_path(root, 'nonallocating.v')
	os.write_file(nonallocating_source, "import os

type Token = string

interface Speaker {
	speak()
}

struct Person {}

type PersonPtr = &Person

fn (p Person) speak() {}

fn (p PersonPtr) speak() {}

fn (a Token) + (b Token) Token {
	_ = b
	return a
}

fn box(p &Person) Speaker {
	return Speaker(p)
}

fn box_alias(p PersonPtr) Speaker {
	return Speaker(p)
}

fn fixed_array() [3]int {
	return [3]int[1, 2, 3]
}

fn empty_fixed_array() [3]int {
	return [3]int{}
}

fn initialized_fixed_array() [3]int {
	return [3]int{init: 1}
}

fn main() {
	println(os.args.len)
	println(Token('a') + Token('b'))
	person := Person{}
	speaker := box(&person)
	speaker.speak()
	converted := Speaker(speaker)
	converted.speak()
	println(fixed_array())
	println(empty_fixed_array())
	println(initialized_fixed_array())
}
")!
	nonallocating := cmdexec.run(v3_bin, ['-silent', '-nocache', '-W', '-warn-about-allocs', '-o',
		os.join_path(root, 'nonallocating.c'), nonallocating_source])
	assert nonallocating.exit_code == 0, nonallocating.output
	assert !nonallocating.output.contains('allocation ('), nonallocating.output

	core_root := os.join_path(root, 'core_module')
	os.mkdir_all(os.join_path(core_root, 'math'))!
	os.write_file(os.join_path(core_root, 'math', 'math.v'), "module math

pub fn allocated(name string) string {
	values := [name]
	return '\${values[0]}' + name
}
")!
	os.write_file(os.join_path(core_root, 'main.v'), "module main

import math

fn main() {
	println(math.allocated('V'))
}
")!

	result := cmdexec.run(v3_bin, ['-silent', '-nocache', '-warn-about-allocs', '-o',
		os.join_path(core_root, 'main.c'), os.join_path(core_root, 'main.v')])
	assert result.exit_code == 0, result.output
	assert !result.output.contains('allocation ('), result.output
}
