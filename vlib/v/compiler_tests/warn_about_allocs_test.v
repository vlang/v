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

fn run_warn_allocs_process(program string, args []string, environment map[string]string) os.Result {
	mut process := os.new_process(program)
	process.set_args(args)
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.run()
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	result := os.Result{
		exit_code: process.code
		output:    output
	}
	process.close()
	return result
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

fn box_pointer(p &Person) &Speaker {
	return &Speaker(p)
}

fn box_interface(s Speaker) &Speaker {
	return &Speaker(s)
}

fn main() {
	name := 'V'
	array := [1, 2, 3]
	reserved := []int{cap: 1}
	interpolation := 'hello \${name}'
	concatenation := 'hello ' + name
	alias_string_left := Name('a') + 'b'
	alias_string_right := 'a' + Name('b')
	speaker := Speaker(Person{})
	alias_speaker := Expr(Person{})
	person := Person{}
	pointer_alias_speaker := Speaker(PersonPtr(&person))
	pointer_speaker := box_pointer(&person)
	interface_speaker := box_interface(speaker)
	freed := ['\${name}' + name] @[freed]
	callback := fn () {
		values := [1, 2, 3]
		println(values)
	} @[freed]
	println(array)
	println(reserved)
	println(interpolation)
	println(concatenation)
	println(alias_string_left)
	println(alias_string_right)
	speaker.speak()
	alias_speaker.speak()
	pointer_alias_speaker.speak()
	pointer_speaker.speak()
	interface_speaker.speak()
	println(freed)
	callback()
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
			5
		} else if description == 'array initialization' {
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

	invalid_assignment_attribute_source := os.join_path(root, 'invalid_assignment_attribute.v')
	os.write_file(invalid_assignment_attribute_source, 'fn main() {
	values := [1] @[freed: false]
	println(values)
}
')!
	invalid_assignment_attribute := cmdexec.run(v3_bin, ['-silent', '-nocache',
		'-no-retry-compilation', '-warn-about-allocs', '-o',
		os.join_path(root, 'invalid_assignment_attribute.c'), invalid_assignment_attribute_source])
	assert invalid_assignment_attribute.exit_code != 0, invalid_assignment_attribute.output
	assert invalid_assignment_attribute.output.contains('assignment attribute `freed` does not accept an argument'), invalid_assignment_attribute.output

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

fn preserve_box(p &Speaker) &Speaker {
	return &Speaker(p)
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

fn inferred_fixed_array() [3]int {
	return [1, 2, 3]!
}

fn empty_dynamic_array() []int {
	return []int{}
}

fn zero_length_capacity_array() []int {
	return []int{len: 0, cap: 0}
}

fn main() {
	println(os.args.len)
	println(Token('a') + Token('b'))
	person := Person{}
	speaker := box(&person)
	speaker.speak()
	speaker_pointer := preserve_box(&speaker)
	speaker_pointer.speak()
	converted := Speaker(speaker)
	converted.speak()
	value := 42
	callback := fn [value] () int {
		return value
	}
	println(callback())
	println(fixed_array())
	println(empty_fixed_array())
	println(initialized_fixed_array())
	println(inferred_fixed_array())
	println(empty_dynamic_array())
	println(zero_length_capacity_array())
}
")!
	nonallocating := cmdexec.run(v3_bin, ['-silent', '-nocache', '-W', '-warn-about-allocs', '-o',
		os.join_path(root, 'nonallocating.c'), nonallocating_source])
	assert nonallocating.exit_code == 0, nonallocating.output
	assert !nonallocating.output.contains('allocation ('), nonallocating.output

	cache_project := os.join_path(root, 'cached_import')
	cache_module := os.join_path(cache_project, 'allocmod')
	os.mkdir_all(cache_module)!
	os.write_file(os.join_path(cache_project, 'v.mod'), "Module {
	name: 'warn_allocs_cache'
	subdirs: ['allocmod']
}
")!
	os.write_file(os.join_path(cache_module, 'allocmod.v'), 'module allocmod

pub fn first() int {
	values := [1, 2, 3]
	return values[0]
}
')!
	cache_main := os.join_path(cache_project, 'main.v')
	os.write_file(cache_main, 'module main

import allocmod

fn main() {
	println(allocmod.first())
}
')!
	cache_output := os.join_path(cache_project, 'program')
	mut cache_environment := os.environ()
	cache_environment['V3CACHE'] = os.join_path(root, 'module_cache')
	cache_environment['VTMP'] = os.join_path(root, 'cache_vtmp')
	uncached_import := run_warn_allocs_process(v3_bin, ['-silent', '-nocache', '-warn-about-allocs',
		'-o', cache_output, cache_project], cache_environment)
	assert uncached_import.exit_code == 0, uncached_import.output
	assert uncached_import.output.count('allocation (array initialization)') == 1, uncached_import.output
	warm_cache := run_warn_allocs_process(v3_bin, ['-silent', '-warn-about-allocs', '-o',
		cache_output, cache_project], cache_environment)
	assert warm_cache.exit_code == 0, warm_cache.output
	assert warm_cache.output.count('allocation (array initialization)') == 1, warm_cache.output
	os.write_file(cache_main, 'module main

import allocmod

fn main() {
	value := allocmod.first()
	println(value)
}
')!
	rebuilt_entry := run_warn_allocs_process(v3_bin, ['-silent', '-warn-about-allocs', '-o',
		cache_output, cache_project], cache_environment)
	assert rebuilt_entry.exit_code == 0, rebuilt_entry.output
	assert rebuilt_entry.output.count('allocation (array initialization)') == 1, rebuilt_entry.output

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
