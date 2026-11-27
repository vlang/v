import os
import v.tempname

fn new_generics_fixture_dir() string {
	root := os.join_path(os.vtmp_dir(), 'v3_generics_fixtures_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	return root
}

fn write_generics_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

struct SelfhostGenericRejection {
	name     string
	source   string
	expected string
}

fn run_selfhost_bad_batch(root string, cases []SelfhostGenericRejection, imported_module string) map[string][]string {
	mut imports := []string{cap: cases.len + 1}
	imports << 'import ${imported_module}'
	for i, case in cases {
		module_name := 'fixture_${i}'
		imports << 'import ${module_name} as _'
		write_generics_project_file(root, '${module_name}/v3_gen_${case.name}.v', 'module ${module_name}\n\n' +
			case.source)
	}
	write_generics_project_file(root, 'main.v', 'module main\n\n${imports.join('\n')}\n\nfn main() {}\n')
	// The combined fixtures exceed the default diagnostic limit.
	result := os.exec([@VEXE, '-selfhost', '-b', 'c', '-nocolor', '-message-limit', '100', '-o',
		os.join_path(root, 'program'), os.join_path(root, 'main.v')])
	assert result.exit_code != 0, result.output
	assert !result.output.contains('C compilation failed'), result.output
	mut errors := map[string][]string{}
	for line in result.output.split_into_lines() {
		location, message := line.split_once(': error: ') or { continue }
		// Drop the line and column from the right to preserve any colon in the path.
		file := location.all_before_last(':').all_before_last(':')
		errors[os.file_name(file)] << message
	}
	for case in cases {
		assert errors['v3_gen_${case.name}.v'].any(it.contains(case.expected)), '${case.name}: expected `${case.expected}` in\n${result.output}'
	}
	return errors
}

// run_no_generic_error supports run no generic error handling for v3 tests.
fn run_no_generic_error(fixture_dir string, name string, src string) {
	src_file := os.join_path(fixture_dir, 'v3_gen_${name}.v')
	os.write_file(src_file, src) or { panic(err) }
	bin_file := os.join_path(fixture_dir, 'v3_gen_${name}')
	result := os.exec([@VEXE, src_file, '-b', 'c', '-o', bin_file])
	assert !result.output.contains('unsupported generic'), '${name}: should not reject generics without -selfhost, got: ${result.output}'
}

fn run_generics_codegen_batch(fixture_dir string, cases map[string]string, main_source string) string {
	root := os.join_path(fixture_dir, 'codegen_batch')
	names := cases.keys()
	mut imports := []string{cap: names.len}
	mut calls := []string{cap: names.len}
	for i, name in names {
		module_name := 'fixture_${i}'
		imports << 'import ${module_name}'
		calls << '\t${module_name}.exercise()'
		fixture := 'module ${module_name}\n\n' + cases[name]
		write_generics_project_file(root, '${module_name}/v3_gen_${name}.v', fixture.replace_once('\nfn main()',
			'\npub fn exercise()'))
	}
	main_with_calls := main_source.replace_once('\nfn main() {\n', '\nfn main() {\n${calls.join('\n')}\n')
	write_generics_project_file(root, 'main.v', 'module main\n\n${imports.join('\n')}\n' + main_with_calls)
	c_file := os.join_path(root, 'program.c')
	compile := os.exec([@VEXE, '-b', 'c', '-o', c_file, os.join_path(root, 'main.v')])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('unsupported generic'), compile.output
	assert !compile.output.contains('type checker found'), compile.output
	c_content := os.read_file(c_file) or { panic(err) }
	for i, name in names {
		assert c_content.contains('void fixture_${i}__exercise(void) {'), '${name}: missing generated C function body'
	}
	return c_content
}

fn test_explicit_main_generic_arg_survives_imported_call_rewrite() {
	fixture_dir := new_generics_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	root := os.join_path(fixture_dir, 'v3_gen_explicit_main_generic_project')
	os.mkdir(root) or { panic(err) }
	write_generics_project_file(root, 'gate/gate.v', 'module gate

pub fn classify[T]() string {
	$if T is $struct {
		return "struct"
	} $else {
		return "other"
	}
}
')
	write_generics_project_file(root, 'main.v', 'module main

import gate

struct Payload[T] {
	value T
}

fn main() {
	println(gate.classify[Payload[string]]())
}
')
	bin_file := os.join_path(root, 'app')
	compile := os.exec([@VEXE, '-nocache', '-path', '${root}' + '|@vlib|@vmodules', '-b', 'c',
		'-o', bin_file, os.join_path(root, 'main.v')])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin_file])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'struct'
}

fn test_late_generic_reachability_runs_to_fixpoint() {
	$if windows {
		return
	}
	fixture_dir := new_generics_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	mut source := ''
	for i in 1 .. 41 {
		source += 'struct Reach${i}[T] {\n\tmarker T\n\tvalue int\n}\n\n'
		source += 'fn (_ Reach${i}[T]) + (_ Reach${i}[T]) Reach${i}[T] {\n\treturn Reach${i}[T]{\n\t\tvalue: helper_${i}()\n\t}\n}\n\n'
		if i < 40 {
			source += 'fn helper_${i}() int {\n\tif never() {\n\t\t_ = Reach${i + 1}[string]{} + Reach${i + 1}[string]{}\n\t}\n\tvalue := Reach${i + 1}[int]{} + Reach${i + 1}[int]{}\n\treturn value.value + 1\n}\n\n'
		} else {
			source += 'fn helper_${i}() int {\n\treturn 1\n}\n\n'
		}
	}
	source += 'fn never() bool {\n\treturn false\n}\n\nfn main() {\n\tif never() {\n\t\t_ = Reach1[string]{} + Reach1[string]{}\n\t}\n\tvalue := Reach1[int]{} + Reach1[int]{}\n\tprintln(value.value)\n}\n'
	src_file := os.join_path(fixture_dir, 'v3_gen_late_generic_reachability_fixpoint.v')
	os.write_file(src_file, source) or { panic(err) }
	bin_file := os.join_path(fixture_dir, 'v3_gen_late_generic_reachability_fixpoint')
	compile := os.exec(['env', 'VJOBS=4', @VEXE, src_file, '-b', 'c', '-o', bin_file])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin_file])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '40'
}

// test_generics_rejected_when_building_v validates this v3 regression case.
fn test_generics_rejected_when_building_v() {
	fixture_dir := new_generics_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	root := os.join_path(fixture_dir, 'selfhost_rejections')
	write_generics_project_file(root, 'badmod/badmod.v', 'module badmod

struct Phantom[T] {
	value int
}
')
	errors := run_selfhost_bad_batch(root, [
		// generic function
		SelfhostGenericRejection{
			name:     'generic_fn'
			source:   '
fn id[T](x T) T {
	return x
}
fn main() {
	println(id[int](1))
}
'
			expected: 'unsupported generic'
		},
		// generic struct
		SelfhostGenericRejection{
			name:     'generic_struct'
			source:   '
struct Box[T] {
	value T
}
fn main() {
	b := Box[int]{value: 7}
	println(b.value)
}
	'
			expected: 'unsupported generic'
		},
		// generic struct with no generic fields
		SelfhostGenericRejection{
			name:     'generic_struct_marker_only'
			source:   '
struct Phantom[T] {
	value int
}
fn main() {}
'
			expected: 'unsupported generic struct `Phantom`'
		},
		SelfhostGenericRejection{
			name:     'generic_fn_marker_only'
			source:   '
fn unused[T]() {}
fn main() {}
	'
			expected: 'unsupported generic declaration `unused`'
		},
		SelfhostGenericRejection{
			name:     'generic_c_fn_marker_only'
			source:   '
fn helper[T]()
fn main() {}
	'
			expected: 'unsupported generic declaration `helper`'
		},
		// Single-letter capital names are reserved for generic parameters.
		SelfhostGenericRejection{
			name:     'generic_interface_marker_only'
			source:   '
interface Marker[T] {}
fn main() {}
	'
			expected: 'unsupported generic declaration `Marker`'
		},
		SelfhostGenericRejection{
			name:     'generic_type_alias_marker_only'
			source:   '
type Alias[T] = int
fn main() {}
'
			expected: 'unsupported generic declaration `Alias`'
		},
		// generic method
		SelfhostGenericRejection{
			name:     'generic_method'
			source:   '
struct Box[T] {
	value T
}
fn (b Box[T]) get() T {
	return b.value
}
fn main() {
	b := Box[string]{value: "v3"}
	println(b.get())
}
'
			expected: 'unsupported generic'
		},
		// generic with option/result return
		SelfhostGenericRejection{
			name:     'generic_option'
			source:   '
fn maybe[T](x T) ?T {
	return x
}
fn main() {
	x := maybe[int](1) or { 0 }
	println(x)
}
'
			expected: 'unsupported generic'
		},
		// multiple type parameters
		SelfhostGenericRejection{
			name:     'generic_multi_param'
			source:   '
fn pair[A, B](a A, b B) A {
	return a
}
fn main() {
	_ := pair[int, string](1, "ok")
}
'
			expected: 'unsupported generic'
		},
		// generic type application in params
		SelfhostGenericRejection{
			name:     'generic_param_type'
			source:   '
fn takes_box(x Box[int]) {}
fn main() {}
'
			expected: 'unsupported generic type application `Box[int]`'
		},
		// generic struct field
		SelfhostGenericRejection{
			name:     'generic_field_type'
			source:   '
struct Wrapper {
	inner Box[string]
}
fn main() {}
'
			expected: 'unsupported generic type application `Box[string]`'
		},
		// generic return type
		SelfhostGenericRejection{
			name:     'generic_return_type'
			source:   '
fn make_box() Box[int] {
	return Box[int]{}
}
fn main() {}
'
			expected: 'unsupported generic type application `Box[int]`'
		},
		// generic sum type
		SelfhostGenericRejection{
			name:     'generic_type_decl'
			source:   '
type Result[T] = T | string
fn main() {}
'
			expected: 'unsupported generic'
		},
		// generic interface
		SelfhostGenericRejection{
			name:     'generic_interface'
			source:   '
interface Container[T] {
	get() T
}
fn main() {}
'
			expected: 'unsupported generic'
		},
	], 'badmod')
	assert errors['badmod.v'].any(it.contains('unsupported generic struct `Phantom`')), errors.str()
}

// test_generics_allowed_without_building_v validates this v3 regression case.
fn test_generics_allowed_without_building_v() {
	fixture_dir := new_generics_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	run_generics_codegen_batch(fixture_dir, {
		// generic function — no "unsupported generic" error
		'allow_generic_fn':        '
fn id[T](x T) T {
	return x
}
fn main() {
	_ := id[int](1)
}
'
		// generic struct
		'allow_generic_struct':    '
struct Box[T] {
	value T
}
fn main() {
	b := Box[int]{value: 7}
	_ := b
}
'
		// generic method
		'allow_generic_method':    '
struct Box[T] {
	value T
}
fn (b Box[T]) get() T {
	return b.value
}
fn main() {
	b := Box[string]{value: "v3"}
	_ := b
}
'
		// nested generic type
		'allow_generic_nested':    '
struct Box[T] {
	value T
}
fn main() {
	b := Box[[]int]{value: [1, 2, 3]}
	_ := b
}
'
		// option/result generic
		'allow_generic_option':    '
fn maybe[T](x T) ?T {
	return x
}
fn main() {
	x := maybe[int](1) or { 0 }
	_ := x
}
'
		// generic interface
		'allow_generic_interface': '
interface Container[T] {
	get() T
}
fn main() {}
'
	}, '\nfn main() {\n}\n')

	// generic type in params/fields/returns
	run_no_generic_error(fixture_dir, 'allow_generic_type_app', '
fn takes_box(x Box[int]) {}
fn main() {}
')
}

// test_generics_compile_and_run validates generics compile and run behavior in v3 tests.
fn test_generics_compile_and_run() {
	fixture_dir := new_generics_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }

	c_content := run_generics_codegen_batch(fixture_dir, {
		// identity function with int and string
		'run_id_fn':                     '
fn id[T](x T) T {
	return x
}
fn main() {
	println(id(123))
	println(id("ok"))
}
'
		// generic struct with field access
		'run_generic_struct':            '
struct Box[T] {
	value T
}
fn main() {
	b := Box[string]{value: "v3"}
	println(b.value)
}
'
		// transitive generic calls: outer[T] calls id[T]
		'run_transitive':                '
fn id[T](x T) T {
	return x
}
fn outer[T](x T) T {
	return id(x)
}
fn main() {
	println(outer(42))
}
'
		// multi-param generic
		'run_multi_param':               '
fn first[A, B](a A, b B) A {
	return a
}
fn main() {
	println(first(99, "ignored"))
}
'
		// multi-param generic struct fields
		'run_multi_param_struct_fields': '
struct Pair[X, Y] {
	left X
	right Y
}
fn main() {
	p := Pair[int, string]{left: 1, right: "ok"}
	println(p.right)
}
'
		// infer from array element type
		'run_array_infer':               '
fn length[T](xs []T) int {
	return xs.len
}
fn main() {
	println(length([1, 2, 3]))
}
'
	}, '
struct A {
	value int
}

struct Box[T] {
	value T
}

fn main() {
	b := Box[A]{
		value: A{
			value: 3
		}
	}
	_ := b
}
')
	// Keep the one-letter concrete argument in main's namespace.
	assert c_content.contains('struct main__Box_A'), c_content

	root := os.join_path(fixture_dir, 'runtime_batch')
	write_generics_project_file(root, 'selector_infer/v3_gen_selector_method_arg_infer.v', 'module selector_infer

struct Box[T] {
	value T
}

fn (b Box[T]) convert[U](value U) U {
	return value
}

pub fn exercise() {
	b := Box[int]{value: 1}
	println(b.convert("ok"))
}
')
	write_generics_project_file(root, 'explicit_args/v3_gen_explicit_generic_call_keeps_runtime_args.v', 'module explicit_args

fn second[T](a T, b T) T {
	return b
}

pub fn exercise() {
	println(second[int](1, 2))
}
')
	write_generics_project_file(root, 'array_for_in/v3_gen_generic_array_for_in_binding.v', 'module array_for_in

fn sum[T](data T) int {
	$if T is $array {
		mut total := 0
		for value in data {
			total += value
		}
		return total
	} $else {
		return 0
	}
}

pub fn exercise() {
	println(sum([1, 2, 3]))
}
')
	// Keep result propagation in main to exercise the program entry point.
	write_generics_project_file(root, 'main.v', "module main

import arrays { sum }
import os
import selector_infer
import explicit_args
import array_for_in

fn main() {
	match os.args[1] {
		'selector_method_arg_infer' {
			selector_infer.exercise()
			return
		}
		'explicit_generic_call_keeps_runtime_args' {
			explicit_args.exercise()
			return
		}
		'generic_array_for_in_binding' {
			array_for_in.exercise()
			return
		}
		'reused_generic_result_keeps_payload_type' {}
		else { panic('unknown fixture') }
	}
	mapped := [[1, 2], [3, 4]].map(sum(it)!)
	println(sum(mapped[..2])!)
}
")
	bin := os.join_path(root, 'program')
	compile := os.exec([@VEXE, '-b', 'c', '-o', bin, os.join_path(root, 'main.v')])
	assert compile.exit_code == 0, compile.output
	for name, expected in {
		'selector_method_arg_infer':                'ok'
		'explicit_generic_call_keeps_runtime_args': '2'
		'generic_array_for_in_binding':             '6'
		'reused_generic_result_keeps_payload_type': '10'
	} {
		run := os.exec([bin, name])
		assert run.exit_code == 0, '${name}: ${run.output}'
		assert run.output.trim_space() == expected, '${name}: expected ${expected} in\n${run.output}'
	}
}
