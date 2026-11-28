module main

import os
import v.cmdexec

// Nested FastC builds select their own C compiler; a GCC choice inherited from the
// outer runner's VFLAGS (sanitizer and musl jobs) is not a FastC compiler.
fn testsuite_begin() {
	os.unsetenv('VFLAGS')
	os.unsetenv('VOSARGS')
}

struct FastcRemainingCase {
	name      string
	source    string
	expected  string
	bootstrap bool
}

fn test_fastc_remaining_pointer_array_and_default_regressions() {
	$if !macos && !linux {
		return
	}

	cases := [
		FastcRemainingCase{'voidptr_variant', r'module main
type Val = int | voidptr
fn main() {
 x := 5
 p := voidptr(&x)
 z := unsafe { nil }
 for v in [Val(1), Val(p), Val(z)] {
  if v is voidptr {
   println(if v == p { "p" } else if isnil(v) { "nil" } else { "other" })
  } else { println("int") }
 }
}
', 'int\np\nnil', false},
		FastcRemainingCase{'fixed_pointer_literal', r'module main
struct Named { mut: name string }
fn main() {
 mut x := &Named{"a"}
 y := &Named{"b"}
 arr := [x, y]!
 x.name = "changed"
 println(arr[0].name)
 assert arr[0] == x
 assert arr[1] == y
}
', 'changed', false},
		FastcRemainingCase{'bootstrap_defaults', r'module main
struct Cfg {
 n int = 7
 s int = 3
}
fn main() {
 c := Cfg{}
 println(c.n + c.s)
}
', '10', true},
		FastcRemainingCase{'empty_fixed_pointer_array', r'module main
struct Named { name string }
fn main() {
 mut a := [4]&Named{}
 assert a.len == 4
 for p in a { assert isnil(p) }
 a[0] = &Named{"first"}
 println(a[0].name)
}
', 'first', false},
		FastcRemainingCase{'function_literal_array', r'module main
fn main() {
 fs := [fn (x int) int { return x + 1 }, fn (x int) int { return x * 2 }]
 assert fs[0](41) == 42
 assert fs[1](21) == 42
 strings := [fn (x string) string { return x + "!" }]
 assert strings[0]("ok") == "ok!"
 println("functions work")
}
', 'functions work', false},
		FastcRemainingCase{'fixed_function_array', r'module main
fn main() {
 fs := [fn (x int) int { return x + 1 }]!
 assert fs[0](41) == 42
 println("fixed functions work")
}
', 'fixed functions work', false},
		FastcRemainingCase{'function_alias_signature', r'module main
type MyInt = int
fn main() {
 fs := [fn (x MyInt) MyInt { return x + 1 }]
 assert fs[0](MyInt(41)) == MyInt(42)
 println("alias works")
}
', 'alias works', false},
		FastcRemainingCase{'function_array_signature', r'module main
fn main() {
 fs := [fn (x []int) []int { return x }]
 assert fs[0]([1, 2]) == [1, 2]
 println("array works")
}
', 'array works', false},
		FastcRemainingCase{'function_fixed_array_signature', r'module main
fn main() {
 fs := [fn (x [2]int) [2]int { return x }]
 result := fs[0]([1, 2]!)
 assert result[0] == 1
 assert result[1] == 2
 println("fixed array works")
}
', 'fixed array works', false},
		FastcRemainingCase{'fixed_array_membership', r'module main
fn main() {
 assert [3, 4]! in [[1, 2]!, [3, 4]!]
 assert [5, 6]! !in [[1, 2]!, [3, 4]!]
 rows := [[1, 2]!, [3, 4]!]
 value := [3, 4]!
 assert value in rows
 println("membership works")
}
', 'membership works', false},
	]
	// The cases that FastC builds as projects are modules of one program; a bootstrap case
	// keeps a source file of its own.
	shared_root := os.join_path(os.vtmp_dir(), 'fastc_remaining_modules_${os.getpid()}')
	os.mkdir_all(shared_root)!
	defer { os.rmdir_all(shared_root) or {} }
	mut imports := []string{}
	mut calls := []string{}
	mut expected := []string{}
	for case in cases {
		if case.bootstrap {
			root := os.join_path(os.vtmp_dir(), 'fastc_remaining_${case.name}_${os.getpid()}')
			os.mkdir_all(root)!
			defer { os.rmdir_all(root) or {} }
			source := os.join_path(root, 'main.v')
			os.write_file(source, case.source)!
			check_fastc_remaining_project(case.name, root, source, case.expected)
			continue
		}
		module_name := 'case_${case.name}'
		assert case.source.starts_with('module main\n') && case.source.contains('\nfn main() {')
		os.mkdir_all(os.join_path(shared_root, module_name))!
		os.write_file(os.join_path(shared_root, module_name, '${case.name}.v'), 'module ${module_name}\n' +
			case.source.replace_once('\nfn main() {', '\npub fn exercise() {').all_after('module main\n'))!
		imports << 'import ${module_name}'
		calls << '\t${module_name}.exercise()'
		expected << case.expected
	}
	os.write_file(os.join_path(shared_root, 'main.v'), 'module main\n${imports.join('\n')}\n\nfn main() {\n${calls.join('\n')}\n}\n')!
	check_fastc_remaining_project('modules', shared_root, shared_root, expected.join('\n'))
}

// check_fastc_remaining_project builds `root` with both backends, FastC from `fastc_target`,
// and expects `expected` from the program.
fn check_fastc_remaining_project(name string, root string, fastc_target string, expected string) {
	for backend in ['c', 'fastc'] {
		executable := os.join_path(root, 'program_${backend}')
		target := if backend == 'fastc' { fastc_target } else { root }
		build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-gc', 'none', '-o', executable,
			target])
		assert build.exit_code == 0, '${name} ${backend}: ${build.output}'
		run := cmdexec.run(executable, [])
		assert run.exit_code == 0, '${name} ${backend}: ${run.output}'
		assert run.output.trim_space() == expected, '${name} ${backend}: ${run.output}'
	}
}
