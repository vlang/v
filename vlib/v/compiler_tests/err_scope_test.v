// Tests for where `err` exists. An `or {}` block names its error `err`, and so
// does the `else` block right after an `if x := call() {` guard; anywhere else
// `err` is an undefined identifier, unless the program declares something with
// that name. V3 used to accept `err` everywhere as an `IError`: the program
// passed `v -check` and then failed in the C compiler, where no `err` exists.
import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v.v')
const err_scope_v3_bin = os.join_path(os.temp_dir(), 'v3_err_scope_test_${os.getpid()}')

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
	Case{'second_guard_else', '', 'if x := fails() {\n\t\tprintln(x)\n\t} else if y := maybe() {\n\t\tprintln(y)\n\t} else {\n\t\tprintln(«err»)\n\t}', true},
	Case{'option_guard_else', '', 'if x := maybe() {\n\t\tprintln(x)\n\t} else {\n\t\tprintln(«err»)\n\t}', true},
	Case{'option_or', '', 'x := maybe() or {\n\t\tprintln(«err»)\n\t\t0\n\t}\n\tprintln(x)', true},
	Case{'return_err_in_else', 'fn g() !int {\n\tif x := fails() {\n\t\treturn x\n\t} else {\n\t\treturn «err»\n\t}\n}\n', 'println(g() or { 0 })', true},
	Case{'mut_guard_else', '', 'if mut x := fails() {\n\t\tx++\n\t\tprintln(x)\n\t} else {\n\t\tprintln(«err»)\n\t}', true},
	Case{'map_guard_else', '', "m := {'a': 1}\n\tif v := m['b'] {\n\t\tprintln(v)\n\t} else {\n\t\tprintln(«err»)\n\t}", true},
	Case{'array_guard_else', '', 'a := [1]\n\tif v := a[3] {\n\t\tprintln(v)\n\t} else {\n\t\tprintln(«err»)\n\t}', true},
	Case{'chan_guard_else', '', 'ch := chan int{cap: 1}\n\tch.close()\n\tif v := <-ch {\n\t\tprintln(v)\n\t} else {\n\t\tprintln(«err»)\n\t}', true},
	// something the program declares with that name
	Case{'local_named_err', '', 'err := 5\n\tprintln(«err»)', true},
	Case{'param_named_err', 'fn show(err string) {\n\tprintln(«err»)\n}\n', "show('a')", true},
	Case{'const_named_err', 'const err = 5\n', 'println(«err» + 1)', true},
]

fn build_v3() string {
	if !os.is_executable(err_scope_v3_bin) {
		res := os.execute('${os.quoted_path(vexe)} -gc none -path ${os.quoted_path('${vlib_dir}|@vlib|@vmodules')} -o ${os.quoted_path(err_scope_v3_bin)} ${os.quoted_path(v3_src)}')
		assert res.exit_code == 0, res.output
	}
	return err_scope_v3_bin
}

fn testsuite_end() {
	os.rm(err_scope_v3_bin) or {}
}

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

// check_output runs `v -check` on the program of `c` and returns its output.
fn check_output(v3 string, c Case) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_err_scope_${c.name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'main.v'), program(c)) or { panic(err) }
	return os.execute('${os.quoted_path(v3)} -nocache -gc none -nocolor -check ${os.quoted_path(os.join_path(dir, 'main.v'))}').output
}

fn test_err_exists_only_where_an_error_is_handled() {
	v3 := build_v3()
	mut wrong := []string{}
	for c in cases {
		output := check_output(v3, c)
		errors := output.split_into_lines().filter(it.contains(': error: '))
		line, col := marked_position(c)
		undefined := errors.any(it.contains('main.v:${line}:${col}: error: undefined ident: `err`'))
		if c.ok && errors.len > 0 {
			wrong << '${c.name}: `err` is defined there, but: ${errors}'
		} else if !c.ok && !undefined {
			wrong << '${c.name}: expected `undefined ident: \\`err\\`` at ${line}:${col}, got: ${errors}'
		}
	}
	assert wrong.len == 0, wrong.join('\n')
}

// build_output builds `source` as `main.v` with `v3` and returns what it printed:
// a check leaves the bodies of generic functions out, a build checks each
// instance of them.
fn build_output(v3 string, name string, source string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_err_scope_build_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'main.v'), source) or { panic(err) }
	return os.execute('${os.quoted_path(v3)} -nocache -gc none -nocolor -o ${os.quoted_path(os.join_path(dir, 'main'))} ${os.quoted_path(os.join_path(dir, 'main.v'))}').output
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
	v3 := build_v3()
	output := build_output(v3, 'generic_or', generic_or_block)
	assert !output.contains(': error: '), output
}
