module fastc

import os
import v.cmdexec
import v.pref

// A main-module function can have the name of any libc function that FastC's
// prelude headers declare (`sleep`, `time`, `remove`; on macOS also `bind` and
// `accept`), not only one of naming.c_name's libc collisions. Main-module
// functions are named `main__<name>`, so none of them redeclares a libc
// function. A constant of the same name as a function keeps its value, and calls,
// function values, generic instances, spawned calls and module init still reach
// the function.
fn test_fastc_main_module_functions_named_like_libc_functions() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_main_libc_names_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

const answer = 40

struct Box {
	v int
}

fn bind(b Box) int {
	return b.v + 1
}

fn accept(x int) int {
	return x * 2
}

fn sleep(n int) int {
	return n + 100
}

fn time() string {
	return "now"
}

fn remove(s string) string {
	return s + "!"
}

fn read(b Box) int {
	return b.v - 1
}

fn answer() int {
	return answer + 2
}

fn twice[T](x T) T {
	return x + x
}

fn apply(f fn (int) int, x int) int {
	return f(x)
}

fn init() {
	println("init")
}

fn main() {
	println(bind(Box{41}))
	println(accept(4))
	println(sleep(1))
	println(time())
	println(remove("x"))
	println(read(Box{3}))
	println(answer())
	println(answer)
	println(twice(21))
	println(apply(accept, 5))
	t := spawn sleep(2)
	println(t.wait())
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	c_source := generate_files([root], prefs) or { panic(err) }
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-pthread', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output == 'init\n42\n8\n101\nnow\nx!\n2\n42\n40\n42\n10\n102\n', run.output
}
