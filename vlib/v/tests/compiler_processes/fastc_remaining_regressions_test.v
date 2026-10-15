module main

import os
import v.cmdexec

struct FastcRemainingCase {
	name      string
	source    string
	expected  string
	bootstrap bool
}

fn test_fastc_remaining_pointer_array_and_default_regressions() {
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
	for case in cases {
		root := os.join_path(os.vtmp_dir(), 'fastc_remaining_${case.name}_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		source := os.join_path(root, 'main.v')
		os.write_file(source, case.source)!
		for backend in ['c', 'fastc'] {
			executable := os.join_path(root, 'program_${backend}')
			target := if backend == 'fastc' && case.bootstrap { source } else { root }
			build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-gc', 'none', '-o', executable,
				target])
			assert build.exit_code == 0, '${case.name} ${backend}: ${build.output}'
			run := cmdexec.run(executable, [])
			assert run.exit_code == 0, '${case.name} ${backend}: ${run.output}'
			assert run.output.trim_space() == case.expected, '${case.name} ${backend}: ${run.output}'
		}
	}
}
