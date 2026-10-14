module js

import os

// A js_run records what one V program compiled with the JS backend did:
// the exit status, and the combined stdout of the backend and of node.
struct JsRun {
	exit_code int
	output    string
}

// js_write_fixture writes `source` as `name` inside `root` and returns its path.
fn js_write_fixture(root string, name string, source string) !string {
	path := os.join_path(root, name)
	os.write_file(path, source)!
	return path
}

// js_new_fixture_root makes a private scratch directory for one test.
fn js_new_fixture_root(name string) !string {
	root := os.join_path(os.vtmp_dir(), 'js_backend_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	return root
}

fn js_child_environment() map[string]string {
	mut environment := os.environ()
	environment.delete('VFLAGS')
	environment.delete('VOSARGS')
	environment['V_MACOS_V3_NO_FALLBACK'] = '1'
	return environment
}

// js_compile runs the backend over `program` and returns the generated JS path
// together with what the backend printed. The compile step is kept separate
// from the run step so a failure names whether the backend or node is at fault.
fn js_compile(program string, output_js string) (int, string, string) {
	mut compiler := os.new_process(@VEXE)
	compiler.set_args(['-b', 'js', '-silent', '-o', output_js, program])
	compiler.set_environment(js_child_environment())
	compiler.set_redirect_stdio_merged()
	compiler.run()
	compile_output := compiler.stdout_slurp()
	compiler.wait()
	compile_code := compiler.code
	compiler.close()
	return compile_code, compile_output, output_js
}

// js_run_java_script executes an already generated JS file under node.
fn js_run_java_script(output_js string, program_args []string) JsRun {
	mut node := os.new_process('node')
	mut args := [output_js]
	args << program_args
	node.set_args(args)
	node.set_redirect_stdio_merged()
	node.run()
	node_output := node.stdout_slurp()
	node.wait()
	node_code := node.code
	node.close()
	return JsRun{
		exit_code: node_code
		output:    node_output
	}
}

fn js_run_program(root string, name string, source string, program_args []string) !JsRun {
	program := js_write_fixture(root, name, source)!
	output_js := os.join_path(root, '${name}_out.js')
	compile_code, compile_output, generated := js_compile(program, output_js)
	if compile_code != 0 || !os.exists(generated) {
		return JsRun{
			exit_code: 1
			output:    compile_output
		}
	}
	return js_run_java_script(generated, program_args)
}

fn test_js_backend_compiles_and_runs_a_program_under_node() {
	os.find_abs_path_of_executable('node') or {
		eprintln('Skipping Node.js execution: node is unavailable')
		return
	}
	root := js_new_fixture_root('hello')!
	defer {
		os.rmdir_all(root) or {}
	}
	run := js_run_program(root, 'hello.v', "fn main() {\n\tprintln('hello')\n}\n", [])!
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'hello'
}

fn test_js_backend_rejects_a_program_without_a_main_function() {
	root := js_new_fixture_root('nomains')!
	defer {
		os.rmdir_all(root) or {}
	}
	run := js_run_program(root, 'nomains.v', 'fn add(a int, b int) int {\n\treturn a + b\n}\n',
		[])!
	assert run.exit_code != 0, run.output
}

// The backend is a compatibility generator, not a full JS lowering: it reports
// what it does not support rather than generating code that would misbehave.
// `os.args` is outside its scope, and that is surfaced at compile time.
fn test_js_backend_reports_an_unsupported_feature() {
	root := js_new_fixture_root('unsupported')!
	defer {
		os.rmdir_all(root) or {}
	}
	program := js_write_fixture(root, 'unsupported.v', 'import os\n\nfn main() {\n\tprintln(os.args[1])\n}\n')!
	compile_code, compile_output, _ := js_compile(program, os.join_path(root, 'unsupported_out.js'))
	assert compile_code != 0, compile_output
	assert compile_output.contains('compatibility generator'), 'expected the backend to name its limitation, got: ${compile_output}'
}

fn test_js_backend_compiles_a_literal_println() {
	root := js_new_fixture_root('lit')!
	defer {
		os.rmdir_all(root) or {}
	}
	program := js_write_fixture(root, 'lit.v', "fn main() {\n\tprintln('literal')\n}\n")!
	compile_code, compile_output, generated := js_compile(program, os.join_path(root, 'lit_out.js'))
	assert compile_code == 0, compile_output
	assert os.exists(generated), 'compile reported: ${compile_output}'
}
