module fastc

import os
import v.cmdexec

fn test_fixed_array_predicates_and_temporary_equality_with_tcc() ! {
	root := os.join_path(os.vtmp_dir(), 'fastc_fixed_predicates_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
__global calls = 0
enum Destination { image style }
type RequestDestination = Destination
struct Preload { destination ?RequestDestination }
fn description(key Preload) string {
	return if value := key.destination { value.str() } else { "-" }
}
struct Values { items [4]f64 }
type NamedValues = []Values
fn optional(values &Values) ?[4]f64 { return values.items }
fn result(values &Values) ![4]f64 { return values.items }
struct Edges { top f64 right f64 bottom f64 left f64 }
fn filtered(edges Edges) int {
	sizes := [edges.top, edges.right, edges.bottom, edges.left]!
	return sizes.filter(it > 0).len
}
fn numbers() [4]f64 { calls++; return [1.0, 2.0, 3.0, 4.0]! }
fn main() {
	mut named := NamedValues{cap: 4}
	named << Values{[1.0, 2.0, 3.0, 4.0]!}
	assert named.len == 1 && named[0].items[3] == 4
	initialized := NamedValues{len: 2, init: Values{[1.0, 2.0, 3.0, 4.0]!}}
	assert initialized[1].items[2] == 3
	assert description(Preload{destination: RequestDestination(.image)}) == "image"
	assert description(Preload{}) == "-"
	values := Values{[1.0, 2.0, 3.0, 4.0]!}
	assert (optional(&values) or { panic("missing") }) == values.items
	assert (result(&values) or { panic(err) }) == values.items
	assert values.items == [1.0, 2.0, 3.0, 4.0]!
	assert [1.0, 2.0, 3.0, 4.0]! == values.items
	assert values.items != [1.0, 1.0, 1.0, 1.0]! && values.items.all(it >= 0)
	assert values.items.any(it == 3.0)
	assert !values.items.any(it < 0)
	assert values.items.count(it >= 3) == 2
	assert numbers() == numbers()
	assert calls == 2
	assert numbers().all(it > 0)
	assert calls == 3
	assert filtered(Edges{0, 2, 0, 0}) == 1
	assert values.items.map(it + 1) == [2.0, 3.0, 4.0, 5.0]
}
')!
	for compiler in ['tcc', 'clang'] {
		if compiler == 'clang' && os.find_abs_path_of_executable(compiler) == none { continue }
		program := os.join_path(root, compiler)
		built := cmdexec.run(@VEXE, ['-b', 'fastc', '-cc', compiler, '-enable-globals', '-nocache',
			'-o', program, root])
		assert built.exit_code == 0, built.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
}
