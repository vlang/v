import os

const veb_run_at_source = 'module main

import veb

struct Program {}

struct Context {
	veb.Context
}

@[get: "/"]
pub fn (mut p Program) index(mut ctx Context) !veb.Result {
	return ctx.text("hello")
}

fn main() {
	mut p := Program{}
	veb.run_at(mut p, port: 8080)!
}
'

fn veb_run_at_project(name string, source string) !string {
	root := os.join_path(os.vtmp_dir(), 'veb_run_at_${name}_${os.getpid()}')
	os.mkdir_all(root)!
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'veb_run_at_regression' }\n")!
	os.write_file(os.join_path(root, 'main.v'), source)!
	return root
}

fn test_veb_run_at_requires_an_explicit_context_type() {
	for module_name in ['veb', 'web'] {
		source := veb_run_at_source.replace('import veb', 'import veb as ${module_name}')
			.replace('veb.', '${module_name}.')
		root := veb_run_at_project('inference_${module_name}', source)!
		defer { os.rmdir_all(root) or {} }
		for flags in [[]string{}, ['-no-parallel']] {
			result := os.exec([@VEXE, '-new-compiler', '-nocache', '-nocolor', ...flags, '-o',
				os.join_path(root, 'app.c'), root])
			assert result.exit_code != 0, result.output
			assert result.output.contains('could not infer generic type `X` in call to `veb.run_at`'), result.output
			assert !result.output.contains('C compilation error'), result.output
		}
	}
}

fn test_veb_run_at_with_explicit_app_and_context_compiles() {
	source := veb_run_at_source.replace('veb.run_at(mut p, port: 8080)!',
		'veb.run_at[Program, Context](mut p, port: 8080)!')
	root := veb_run_at_project('explicit', source)!
	defer { os.rmdir_all(root) or {} }
	program := os.join_path(root, 'app' + $if windows { '.exe' } $else { '' })
	result := os.exec([@VEXE, '-new-compiler', '-nocache', '-o', program, root])
	assert result.exit_code == 0, result.output
	assert os.is_file(program), result.output
}

fn test_veb_run_at_with_explicit_types_preserves_errors_and_handler_response() {
	source := veb_run_at_source.replace('veb.run_at(mut p, port: 8080)!', '
	veb.run_at[Program, Context](mut p, port: 0) or {
		assert err.msg() == "invalid port number `0`, it should be between 1 and 65535"
		mut ctx := Context{}
		p.index(mut ctx)!
		assert ctx.res.body == "hello"
		println("ok")
		return
	}
	panic("expected an invalid port error")')
	root := veb_run_at_project('response', source)!
	defer { os.rmdir_all(root) or {} }
	program := os.join_path(root, 'app' + $if windows { '.exe' } $else { '' })
	compile := os.exec([@VEXE, '-new-compiler', '-nocache', '-o', program, root])
	assert compile.exit_code == 0, compile.output
	result := os.exec([program])
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == 'ok', result.output
}

fn test_generic_type_used_only_in_the_body_requires_explicit_arguments() {
	root := veb_run_at_project('unused_type', 'fn use_type[A, X](value A) {
	_ = X{}
}
fn main() {
	use_type(1)
}
')!
	defer { os.rmdir_all(root) or {} }
	result := os.exec([@VEXE, '-new-compiler', '-nocache', '-nocolor', '-o',
		os.join_path(root, 'app.c'), root])
	assert result.exit_code != 0, result.output
	assert result.output.contains('could not infer generic type `X` in call to `use_type`'), result.output
}

fn test_generic_return_and_constraint_inference_still_work() {
	root := veb_run_at_project('inference_sources', 'import arrays

interface Container[T] {
	get() T
}
struct Box {}
fn (b Box) get() int {
	return 7
}
fn read[C Container[T], T](value C) int {
	return value.get()
}
fn make_default[T](_ int) T {
	return T{}
}
fn make_box[C Container[T], T](_ int) C {
	return C{}
}
fn main() {
	assert arrays.uniq([1, 1, 2]) == [1, 2]
	assert (arrays.min([3, 1, 2]) or { panic(err) }) == 1
	assert read(Box{}) == 7
	mut box := Box{}
	box = make_box(0)
	assert box.get() == 7
	mut value := 1
	value = make_default(0)
	assert value == 0
	println("ok")
}
')!
	defer { os.rmdir_all(root) or {} }
	result := os.exec([@VEXE, '-new-compiler', '-nocache', 'run', root])
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == 'ok', result.output
}
