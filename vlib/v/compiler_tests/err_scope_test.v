// Tests for where `err` exists. An `or {}` block names its error `err`, and so
// does the `else` block right after an `if x := call() {` guard; anywhere else
// `err` is an undefined identifier, unless the program declares something with
// that name. V3 used to accept `err` everywhere as an `IError`: the program
// passed `v -check` and then failed in the C compiler, where no `err` exists.
import json2
import os

const prelude = "module main

import os

fn fails() !int {
	return error('x')
}

fn maybe() ?int {
	return none
}
"

struct Case {
	name string
	top  string // top-level code before main
	body string // the body of main; «err» marks the use that is checked
	ok   bool   // whether `err` is defined at the marked use
}

struct ErrScopeDiagnostic {
	file     string
	line     int
	col      int
	severity string
	message  string
}

const undefined_err_messages = ['undefined ident: `err`', 'undefined variable: `err`']

const cases = [
	// outside any `or {}` block or guard `else`: an undefined identifier
	Case{'after_guard', '', 'if x := fails() {\n\t\tprintln(x)\n\t} else {\n\t}\n\tprintln(«err»)', false},
	Case{'no_block', '', 'println(«err»)', false},
	Case{'after_or', '', 'x := fails() or { 0 }\n\tprintln(x)\n\tprintln(«err»)', false},
	Case{'after_nested_or', '', 'x := fails() or {\n\t\ty := fails() or { 1 }\n\t\ty\n\t}\n\tprintln(x)\n\tprintln(«err»)', false},
	Case{'guard_then', '', 'if x := fails() {\n\t\tprintln(«err»)\n\t\tprintln(x)\n\t}', false},
	Case{'guard_else_if', '', 'if x := fails() {\n\t\tprintln(x)\n\t} else if os.args.len > 5 {\n\t\tprintln(«err»)\n\t}', false},
	Case{'guard_else_after_else_if', '', 'if x := fails() {\n\t\tprintln(x)\n\t} else if os.args.len > 5 {\n\t} else {\n\t\tprintln(«err»)\n\t}', false},
	Case{'plain_if_else', '', 'if os.args.len > 0 {\n\t\tprintln(1)\n\t} else {\n\t\tprintln(«err»)\n\t}', false},
	Case{'match_branch', '', 'match os.args.len {\n\t\t0 { println(«err») }\n\t\telse {}\n\t}', false},
	Case{'interpolation', '', "if x := fails() {\n\t\tprintln(x)\n\t} else {\n\t}\n\tprintln('error: \${«err»}')", false},
	// inside the blocks that name the error
	Case{'or_block', '', 'x := fails() or {\n\t\tprintln(«err»)\n\t\t0\n\t}\n\tprintln(x)', true},
	Case{'or_nested_block', '', 'x := fails() or {\n\t\tif os.args.len > 0 {\n\t\t\tprintln(«err»)\n\t\t}\n\t\t0\n\t}\n\tprintln(x)', true},
	Case{'or_closure_capture', '', 'x := fails() or {\n\t\tf := fn [err] () {\n\t\t\tprintln(«err»)\n\t\t}\n\t\tf()\n\t\t0\n\t}\n\tprintln(x)', true},
	Case{'guard_else', '', 'if x := fails() {\n\t\tprintln(x)\n\t} else {\n\t\tprintln(«err»)\n\t}', true},
	Case{'guard_else_nested', '', 'if x := fails() {\n\t\tprintln(x)\n\t} else {\n\t\tif os.args.len > 0 {\n\t\t\tprintln(«err»)\n\t\t}\n\t}', true},
	Case{'second_guard_else', '', 'if x := fails() {\n\t\tprintln(x)\n\t} else if y := maybe() {\n\t\tprintln(y)\n\t} else {\n\t\tprintln(«err»)\n\t}', false},
	Case{'option_guard_else', '', 'if x := maybe() {\n\t\tprintln(x)\n\t} else {\n\t\tprintln(«err»)\n\t}', false},
	Case{'option_or', '', 'x := maybe() or {\n\t\tprintln(«err»)\n\t\t0\n\t}\n\tprintln(x)', false},
	Case{'return_err_in_else', 'fn g() !int {\n\tif x := fails() {\n\t\treturn x\n\t} else {\n\t\treturn «err»\n\t}\n}\n', 'println(g() or { 0 })', true},
	Case{'mut_guard_else', '', 'if mut x := fails() {\n\t\tx++\n\t\tprintln(x)\n\t} else {\n\t\tprintln(«err»)\n\t}', true},
	Case{'map_guard_else', '', "m := {'a': 1}\n\tif v := m['b'] {\n\t\tprintln(v)\n\t} else {\n\t\tprintln(«err»)\n\t}", false},
	Case{'array_guard_else', '', 'a := [1]\n\tif v := a[3] {\n\t\tprintln(v)\n\t} else {\n\t\tprintln(«err»)\n\t}', false},
	Case{'chan_guard_else', '', 'ch := chan int{cap: 1}\n\tch.close()\n\tif v := <-ch {\n\t\tprintln(v)\n\t} else {\n\t\tprintln(«err»)\n\t}', true},
	// a call through a fn-typed value named like a function (`open`, `read`)
	Case{'fn_value_param_or', 'fn call(open fn () !int) !int {\n\tx := open() or { return «err» }\n\treturn x\n}\n', 'println(call(fails) or { 0 })', true},
	Case{'fn_value_local_or', '', 'read := fails\n\tx := read() or {\n\t\tprintln(«err»)\n\t\t0\n\t}\n\tprintln(x)', true},
	Case{'option_fn_value_param_or', 'fn call(open fn () ?int) ?int {\n\tx := open() or {\n\t\tprintln(«err»)\n\t\treturn none\n\t}\n\treturn x\n}\n', 'println(call(maybe) or { 0 })', false},
	Case{'closure_calls_fn_shadowed_outside', '', 'fails := maybe\n\tf := fn () {\n\t\tx := fails() or {\n\t\t\tprintln(«err»)\n\t\t\t0\n\t\t}\n\t\tprintln(x)\n\t}\n\tf()\n\t_ = fails', true},
	Case{'closure_lambda_calls_fn_shadowed_outside', '', 'fails := maybe\n\tf := fn () {\n\t\tr := [1].map(|x| fails() or {\n\t\t\tprintln(«err»)\n\t\t\tx\n\t\t})\n\t\tprintln(r)\n\t}\n\tf()\n\t_ = fails', true},
	// something the program declares with that name
	Case{'local_named_err', '', 'err := 5\n\tprintln(«err»)', true},
	Case{'param_named_err', 'fn show(err string) {\n\tprintln(«err»)\n}\n', "show('a')", true},
	Case{'const_named_err', 'const err = 5\n', 'println(«err» + 1)', true},
]

fn unmarked(text string) string {
	return text.replace('«', '').replace('»', '')
}

fn program(c Case) string {
	return prelude + '\n' + unmarked(c.top) + '\nfn main() {\n\t' + unmarked(c.body) + '\n}\n'
}

// marked_position returns the line and column, 1-based, of the marked `err`.
fn marked_position(c Case) (int, int) {
	text := if c.top.contains('«') {
		prelude + '\n' + c.top.all_before('«')
	} else {
		prelude + '\n' + unmarked(c.top) + '\nfn main() {\n\t' + c.body.all_before('«')
	}
	lines := text.split('\n')
	return lines.len, lines.last().runes().len + 1
}

// check_program runs `v -check` on the program of `c`.
fn check_program(c Case) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_err_scope_${c.name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'main.v'), program(c)) or { panic(err) }
	return check_err_scope_source(os.join_path(dir, 'main.v'))
}

fn check_err_scope_source(path string, flags ...string) os.Result {
	return os.exec([@VEXE, '-nocache', '-gc', 'none', '-nocolor', '-check', ...flags, path])
}

fn check_err_scope_batch(name string, batch []Case, expected_exit int) {
	dir := os.join_path(os.vtmp_dir(), 'v3_err_scope_batch_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	mut imports := []string{cap: batch.len}
	mut calls := []string{cap: batch.len}
	for c in batch {
		module_name := 'err_scope_${c.name}'
		module_dir := os.join_path(dir, module_name)
		os.mkdir(module_dir) or { panic(err) }
		// Keep the original entry point reachable and the marked use at its original location.
		source := program(c).replace_once('module main\n', 'module ${module_name}\n').replace_once('\nfn main()',
			'\npub fn exercise()')
		os.write_file(os.join_path(module_dir, '${module_name}.v'), source) or { panic(err) }
		imports << 'import ${module_name}'
		calls << '\t${module_name}.exercise()'
	}
	main_path := os.join_path(dir, 'main.v')
	os.write_file(main_path, 'module main\n\n${imports.join('\n')}\n\nfn main() {\n${calls.join('\n')}\n}\n') or {
		panic(err)
	}
	result := check_err_scope_source(main_path, '-json-errors')
	assert result.exit_code == expected_exit, result.output
	mut errors := map[string][]ErrScopeDiagnostic{}
	for line in result.output.split_into_lines() {
		if !line.starts_with('{') {
			continue
		}
		diagnostic := json2.decode[ErrScopeDiagnostic](line) or { panic(err) }
		if diagnostic.severity == 'error' {
			errors[os.file_name(diagnostic.file)] << diagnostic
		}
	}
	if expected_exit == 0 {
		assert errors.len == 0, result.output
		return
	}
	for c in batch {
		line, col := marked_position(c)
		assert errors['err_scope_${c.name}.v'].any(it.line == line && it.col == col
			&& it.message.all_before('\n') in undefined_err_messages), '${c.name}: expected undefined `err` at ${line}:${col}\n${result.output}'
	}
}

fn test_err_exists_only_where_an_error_is_handled() {
	// Custom flags can affect module lookup or limit diagnostics for the batch.
	if os.getenv('VFLAGS').trim_space() in ['', '-gc none'] {
		check_err_scope_batch('accepted', cases.filter(it.ok), 0)
		check_err_scope_batch('rejected', cases.filter(!it.ok), 1)
		return
	}
	mut wrong := []string{}
	for c in cases {
		result := check_program(c)
		errors := result.output.split_into_lines().filter(it.contains(': error: '))
		line, col := marked_position(c)
		position := 'main.v:${line}:${col}: error: '
		undefined := errors.any(it.contains(position) && it.all_after(position) in undefined_err_messages)
		if c.ok && (result.exit_code != 0 || errors.len > 0) {
			wrong << '${c.name}: expected a successful check\n${result.output}'
		} else if !c.ok && !undefined {
			wrong << '${c.name}: expected `undefined ident: \\`err\\`` at ${line}:${col}, got: ${errors}'
		}
	}
	assert wrong.len == 0, wrong.join('\n')
}

// build_program compiles `source` as `main.v` to check instantiated generic function bodies.
fn build_program(name string, source string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_err_scope_build_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'main.v'), source) or { panic(err) }
	return os.exec([@VEXE, '-nocache', '-gc', 'none', '-nocolor', '-o', os.join_path(dir, 'main'),
		os.join_path(dir, 'main.v')])
}

const generic_or_block = "module main

import io

struct Reader {}

struct App {
mut:
	id string
}

fn read[T](mut r T) !int {
	_ = r
	return error('x')
}

fn (mut app App) handle[T](mut reader T) {
	for {
		n := read(mut reader) or {
			if err is io.Eof {
				break
			}
			if err.msg().starts_with('x') {
				app.id = err.msg()
				break
			}
			break
		}
		println(n)
	}
}

fn main() {
	mut app := App{}
	mut r := Reader{}
	app.handle(mut r)
}
"

fn test_err_in_the_or_block_of_an_instance_of_a_generic_function() {
	result := build_program('generic_or', generic_or_block)
	assert result.exit_code == 0, result.output
	assert !result.output.contains(': error: '), result.output
}
