import os

const veb_run_at_source = 'module main

import veb

struct Program {}

struct Context {
	veb.Context
}

@["/"; get]
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

fn test_veb_run_at_rejects_invalid_generic_arguments() {
	invalid_calls := 'veb.run_at(mut p, port: 8080)!
	veb.run_at[Program](mut p, port: 8080)!
	veb.run_at[Program, Context, int](mut p, port: 8080)!
	veb.run_at[int, Context](mut p, port: 8080)!'
	source := veb_run_at_source.replace('veb.run_at(mut p, port: 8080)!', invalid_calls)
	root := veb_run_at_project('invalid_generic_args', source)!
	defer { os.rmdir_all(root) or {} }
	aliased_calls := invalid_calls.replace('veb.', 'web.')
	os.write_file(os.join_path(root, 'aliased.v'), 'module main

import veb as web

fn check_aliased_run_at() ! {
	mut p := Program{}
	${aliased_calls}
}
')!
	os.write_file(os.join_path(root, 'body_only.v'), 'fn use_type[A, X](value A) {
	_ = X{}
}
fn check_body_only_type() {
	use_type(1)
}
')!

	run_at_errors := [
		'could not infer generic type `X` in call to `veb.run_at`',
		'expected 2 generic parameters, got 1',
		'expected 2 generic parameters, got 3',
		'cannot use `&Program` as `&int` in argument 1 to `veb.run_at`',
	]
	for flags in [[]string{}, ['-no-parallel']] {
		result := os.exec([@VEXE, '-nocache', '-nocolor', ...flags, '-o', os.join_path(root, 'app.c'),
			root])
		assert result.exit_code != 0, result.output
		assert !result.output.contains('C compilation error'), result.output
		mut diagnostics := map[string][]string{}
		for line in result.output.split_into_lines() {
			location, message := line.split_once(': error: ') or { continue }
			file := location.all_before_last(':').all_before_last(':')
			diagnostics[os.file_name(file)] << message
		}
		for file, expected in {
			'main.v':      run_at_errors
			'aliased.v':   run_at_errors
			'body_only.v': ['could not infer generic type `X` in call to `use_type`']
		} {
			for message in expected {
				assert diagnostics[file].any(it.contains(message)), 'expected ${message} in ${file}\n${result.output}'
			}
		}
	}
}

fn test_veb_run_at_response_and_generic_inference() {
	source := veb_run_at_source.replace('import veb', 'import veb\nimport net.http\nimport os')
		.replace('veb.run_at(mut p, port: 8080)!', '
	// Compile the valid-port call with result propagation. The test runs without arguments.
	if os.args.contains("--serve") {
		veb.run_at[Program, Context](mut p, port: 8080)!
		return
	}
	check_generic_inference()
	veb.run_at[Program, Context](mut p, port: 0) or {
		assert err.msg() == "invalid port number `0`, it should be between 1 and 65535"
		mut ctx := Context{}
		p.index(mut ctx)!
		assert ctx.res.body == "hello"
		response := veb.render_request[Program, Context](mut p, http.Request{url: "/"})!
		assert response.res.status_code == 200
		assert response.res.body == "hello"
		println("response ok")
		return
	}
	panic("expected an invalid port error")')
	root := veb_run_at_project('response', source)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'inference.v'), 'module main

import arrays

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
fn check_generic_inference() {
	assert arrays.uniq([1, 1, 2]) == [1, 2]
	assert (arrays.min([3, 1, 2]) or { panic(err) }) == 1
	assert read(Box{}) == 7
	mut box := Box{}
	box = make_box(0)
	assert box.get() == 7
	mut value := 1
	value = make_default(0)
	assert value == 0
	println("inference ok")
}
')!
	program := os.join_path(root, 'app' + $if windows { '.exe' } $else { '' })
	compile := os.exec([@VEXE, '-nocache', '-o', program, root])
	assert compile.exit_code == 0, compile.output
	assert os.is_file(program), compile.output
	result := os.exec([program])
	assert result.exit_code == 0, result.output
	assert result.output.split_into_lines() == ['inference ok', 'response ok'], result.output
}
