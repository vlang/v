module fastc

import os
import v.cmdexec
import v.flat
import v.pref
import v.scanner
import v.token

fn test_fastc_qualified_test_source_identifies_its_entry_module() {
	mut prefs := pref.new_preferences()
	prefs.is_test = true
	prefs.backend = 'fastc'
	sources := [FastcSourceFile{
		path:   '/project/generic_test.c.v'
		header: FastcSourceHeader{
			module_name: 'sample'
		}
	}]
	assert fastc_is_entry_module(sources, 'sample', prefs)
	assert !fastc_is_entry_module(sources, 'different', prefs)
}

fn test_fastc_compound_generic_arguments_do_not_use_the_first_literal_type() {
	source := 'f(1 + 0.5, 2, Box{n: 1}, x)'
	mut prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('arguments.v', source.len), source)
	assert scan.scan() == .name
	assert scan.scan() == .lpar
	assert fastc_call_argument_simple_types(mut scan, {
		'x': flat.source_type_string
	}, map[string]bool{}) == [
		flat.source_type_f64,
		flat.source_type_int,
		flat.source_type_id('Box'),
		flat.source_type_string,
	]
}

fn test_fastc_qualified_test_modules_monomorphize_their_generics() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_qualified_generic_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'generic_test.c.v')
	os.write_file(os.join_path(root, 'sample.v'), r'module sample
struct Holder[T] {
	value T
}
fn echo[T](value T) T {
	return value
}
') or { panic(err) }
	os.write_file(source, r'module sample
fn test_generic() {
	item := Holder[int]{value: 42}
	assert echo(item.value) == 42
	println("qualified generic ran")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-gc', 'none', '-nocache', '-o', program, source])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.contains('qualified generic ran'), run.output
}

fn test_fastc_compound_generic_arguments_preserve_float_arithmetic() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_compound_generic_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	declaration := 'module main\nfn first[T, H](value T, hint H) T { return value }\n'
	os.write_file(os.join_path(root, 'main.v'), declaration + 'fn main() { println(first(1 + 0.5, 2)) }\n') or { panic(err) }
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'implicit_${backend}')
		build := cmdexec.run(@VEXE, ['-b', backend, '-gc', 'none', '-nocache', '-o', program, root])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '1.5', run.output
	}
	os.write_file(os.join_path(root, 'main.v'), declaration + 'fn main() { println(first[f64, int](1 + 0.5, 2)) }\n') or { panic(err) }
	for backend in ['c', 'fastc'] {
		explicit_program := os.join_path(root, 'explicit_${backend}')
		explicit_build := cmdexec.run(@VEXE, ['-b', backend, '-gc', 'none', '-nocache', '-o',
			explicit_program, root])
		assert explicit_build.exit_code == 0, explicit_build.output
		explicit_run := cmdexec.run(explicit_program, [])
		assert explicit_run.exit_code == 0, explicit_run.output
		assert explicit_run.output.trim_space() == '1.5', explicit_run.output
	}
}

fn test_fastc_unknown_compound_generic_argument_requires_explicit_types() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_unknown_generic_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main
fn first[T, H](value T, hint H) T { return value }
fn number() f64 { return 0.5 }
fn main() { println(first(1 + number(), 2)) }
') or { panic(err) }
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-gc', 'none', '-nocache', '-o',
		os.join_path(root, 'program'), root])
	assert build.exit_code != 0, build.output
	assert build.output.contains('cannot infer generic type arguments for `first`'), build.output
	assert build.output.contains('use explicit type arguments'), build.output
}

fn test_fastc_unknown_arguments_outside_generic_declarations_are_rejected() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_generic_context_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	for index, source in [
		'module main
struct T { value int }
fn unused[T](value T) T { return value }
fn first[A, B](value A, hint B) A { return value }
fn number() f64 { return 0.5 }
fn main() { marker := T{value: 1}; println(marker.value); println(first(1 + number(), 2)) }
',
		'module main
fn first[A, B](value A, hint B) A { return value }
fn number() f64 { return 0.5 }
fn unused[T](value T) T { return value }
const answer = first(1 + number(), 2)
fn main() { println(answer) }
',
	] {
		os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
		build := cmdexec.run(@VEXE, ['-b', 'fastc', '-gc', 'none', '-nocache', '-o',
			os.join_path(root, 'program_${index}'), root])
		assert build.exit_code != 0, build.output
		assert build.output.contains('cannot infer generic type arguments for `first`'), build.output
	}
}

fn test_fastc_generic_body_calls_are_deferred_until_specialization() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_deferred_generic_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main
fn first[A, B](value A, hint B) A { return value }
fn outer[T](value T) T { return first(value, 2) }
fn main() { println(outer(7)) }
') or { panic(err) }
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_${backend}')
		build := cmdexec.run(@VEXE, ['-b', backend, '-gc', 'none', '-nocache', '-o', program, root])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '7', run.output
	}
}
