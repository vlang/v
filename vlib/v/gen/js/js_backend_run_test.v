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

fn test_js_backend_preserves_literal_and_eval_statement_order() {
	os.find_abs_path_of_executable('node') or {
		eprintln('Skipping Node.js execution: node is unavailable')
		return
	}
	root := js_new_fixture_root('order')!
	defer {
		os.rmdir_all(root) or {}
	}
	source := 'fn main() {
	println(\'first\')
	JS.eval("console.log(\'second\');".str)
	println(\'third\') println(\'fourth\')
}'
	run := js_run_program(root, 'order.js.v', source, [])!
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'first\nsecond\nthird\nfourth'
}

fn test_js_backend_ignores_calls_in_comments() {
	os.find_abs_path_of_executable('node') or {
		eprintln('Skipping Node.js execution: node is unavailable')
		return
	}
	root := js_new_fixture_root('comments')!
	defer {
		os.rmdir_all(root) or {}
	}
	source := 'fn main() {
	println(\'live\') /* println(\'ghost\') */
	println(\'https://example.test\') /* /* println(\'nested\') */ println(\'outer\') */ println(\'after\')
	/* JS.eval("console.log(\'hidden\');".str)
	   println(\'also hidden\')
	*/ JS.eval("console.log(\'from JS\');".str)
	println(\'last\') // println(\'line comment\')
}'
	run := js_run_program(root, 'comments.js.v', source.replace('\n', '\r\n'), [])!
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'live\nhttps://example.test\nafter\nfrom JS\nlast'
}

fn test_js_backend_ignores_calls_in_string_literals() {
	os.find_abs_path_of_executable('node') or {
		eprintln('Skipping Node.js execution: node is unavailable')
		return
	}
	root := js_new_fixture_root('string_literals')!
	defer {
		os.rmdir_all(root) or {}
	}
	source := 'fn main() {
	println(\'live\') _ = "println(\'ghost\')"
	_ = "https://example.test/println(\'url\')" println(\'after URL\')
	_ = "/* println(\'comment-like\') */"
	_ = r\'JS.eval("console.log(123);".str)\'
	_ = "\${\'}\'} println(\'interpolated\')"
	JS.eval("console.log(\'from JS\');".str) println(\'last\')
}'
	run := js_run_program(root, 'string_literals.js.v', source, [])!
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'live\nafter URL\nfrom JS\nlast'
}

fn test_js_backend_rejects_calls_in_unsupported_control_flow() {
	root := js_new_fixture_root('control_flow')!
	defer {
		os.rmdir_all(root) or {}
	}
	for i, source in [
		"fn main() { println('live') if false { println('dead') } }",
		"fn main() { println('live') \$if custom_flag ? { println('conditional') } }",
		'fn main() { println(\'live\') for { JS.eval("console.log(\'loop\');".str) } }',
		"fn main() { println('live') abort() println('dead') }\nfn abort() { panic('stop') }",
	] {
		output := os.join_path(root, 'control_flow_${i}.js')
		program := js_write_fixture(root, 'control_flow_${i}.js.v', source)!
		code, message, _ := js_compile(program, output)
		assert code != 0, message
		assert message.contains('supports only direct literal println and JS.eval statements'), message
		assert !os.exists(output)
	}
	output := os.join_path(root, 'init.js')
	program := js_write_fixture(root, 'init.js.v', "fn main() { println('live') } fn init() { panic('stop') }")!
	code, message, _ := js_compile(program, output)
	assert code != 0, message
	assert message.contains('does not support init functions'), message
	assert !os.exists(output)
}

fn test_js_backend_stops_at_return_and_ignores_unused_functions() {
	os.find_abs_path_of_executable('node') or {
		eprintln('Skipping Node.js execution: node is unavailable')
		return
	}
	root := js_new_fixture_root('return_and_unused')!
	defer {
		os.rmdir_all(root) or {}
	}
	source := "fn unused() { println('unused') } fn main() { println('live') return; println('dead') }"
	run := js_run_program(root, 'return_and_unused.js.v', source, [])!
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'live'
}

fn test_js_backend_rejects_unsupported_startup_initializers() {
	root := js_new_fixture_root('startup')!
	defer {
		os.rmdir_all(root) or {}
	}
	declarations := {
		'constant':       'const value = announce()'
		'constant_group': 'const (\n\tliteral = 1\n\tvalue = (announce())\n)'
		'global':         '__global value = announce()'
		'global_group':   '__global (\n\tliteral = 1\n\tvalue int = announce()\n)'
		'global_default': '__global value Startup\nstruct Startup { number int = announce() }'
	}
	for name, declaration in declarations {
		source := "@[has_globals]
module main

${declaration}

fn announce() int {
	println('init')
	return 1
}

fn main() { println('main') }"
		program := js_write_fixture(root, '${name}.v', source)!
		output := os.join_path(root, '${name}.js')
		code, message, _ := js_compile(program, output)
		kind := name.all_before('_')
		assert code != 0, message
		assert message.contains('${name}.v:'), message
		assert message.contains('does not support this ${kind} initializer for `value`'), message
		assert !os.exists(output)
	}
	// Top-level declarations can run before main even when written after it.
	program := js_write_fixture(root, 'conditional.v', "fn announce() int {
	println('init')
	return 1
}

fn main() {
	println('main')
	return
}

\$if enabled ? {
	const value = announce()
}")!
	output := os.join_path(root, 'conditional.js')
	code, message, _ := js_compile(program, output)
	assert code != 0, message
	assert message.contains('does not support top-level compile-time control flow'), message
	assert !os.exists(output)
}

fn test_js_backend_allows_inert_startup_declarations() {
	os.find_abs_path_of_executable('node') or {
		eprintln('Skipping Node.js execution: node is unavailable')
		return
	}
	root := js_new_fixture_root('inert_startup')!
	defer {
		os.rmdir_all(root) or {}
	}
	source := '@[has_globals]
module main

const literal = (-42)
__global flag bool
__global label = \'label\'

@[export: \'TIC\']
__global tic = fn (value int) int {
	return value + 1
}

fn main() {
	JS.eval("console.log(globalThis.TIC(4));".str)
	println(\'main\')
}'
	run := js_run_program(root, 'inert_startup.js.v', source, [])!
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '5\nmain'
}

fn test_js_backend_decodes_v_string_escapes_before_emission() {
	os.find_abs_path_of_executable('node') or {
		eprintln('Skipping Node.js execution: node is unavailable')
		return
	}
	root := js_new_fixture_root('literal_escapes')!
	defer { os.rmdir_all(root) or {} }
	source := 'fn main() {
	println(\'\\a\\U0001f600\\xc3\\xa9\')
	println(\'quote: " and slash: \\\\ out\')
	JS.eval("console.log(\\"from eval\\");".str)
}'
	run := js_run_program(root, 'literal_escapes.js.v', source, [])!
	assert run.exit_code == 0, run.output
	assert run.output == '\x07😀é\nquote: " and slash: \\ out\nfrom eval\n'
}

fn test_js_backend_rejects_invalid_utf8_literal_before_emission() {
	root := js_new_fixture_root('invalid_utf8')!
	defer { os.rmdir_all(root) or {} }
	program := js_write_fixture(root, 'invalid_utf8.js.v', "fn main() { println('\\xff') }")!
	output := os.join_path(root, 'invalid_utf8.js')
	code, message, _ := js_compile(program, output)
	assert code != 0, message
	assert message.contains('compatibility generator'), message
	assert !os.exists(output)
}
