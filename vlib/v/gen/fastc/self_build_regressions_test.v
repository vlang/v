module fastc

import os
import v.cmdexec
import v.pref

// fastc_self_build_regression_program builds `main_source`, with `modules` as its
// imported module directories, as a selfhost program with FastC, with and without
// target libc headers. It compiles each C file with `cc` and returns the output,
// which must be the same for both, and the C of each mode (compact names first).
fn fastc_self_build_regression_program(name string, main_source string, modules map[string]string) (string, []string) {
	root := os.join_path(os.vtmp_dir(), 'fastc_self_build_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), main_source) or { panic(err) }
	for module_name, module_source in modules {
		os.mkdir_all(os.join_path(root, module_name)) or { panic(err) }
		os.write_file(os.join_path(root, module_name, '${module_name}.v'), module_source) or {
			panic(err)
		}
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.enable_globals = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	mut outputs := []string{}
	mut sources := []string{}
	for target_headers in [false, true] {
		prefs.target_libc_headers = target_headers
		source := generate_files([root], prefs) or { panic(err) }
		c_path := os.join_path(root, 'main.c')
		program := os.join_path(root, 'program')
		os.write_file(c_path, source) or { panic(err) }
		sources << source
		build := cmdexec.run('cc', ['-std=gnu11', '-w', '-Werror=incompatible-pointer-types',
			'-Werror=int-conversion', '-o', program, c_path])
		assert build.exit_code == 0, 'target headers ${target_headers}: ${build.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, 'target headers ${target_headers}: ${run.output}'
		outputs << run.output
	}
	assert outputs[0] == outputs[1]
	return outputs[0], sources
}

fn test_fastc_free_function_and_method_with_one_c_spelling_stay_distinct() {
	$if !macos && !linux {
		return
	}
	// `Pair.sum` and the free `Pair_sum` of module `mathgeom` both spell
	// `mathgeom__Pair_sum` in C, as `string.index_pair` and `string_index_pair`
	// of `builtin` once did. Like `builtin`, a `math` module keeps conventional
	// names in a compact self-host build, so both naming modes collide there;
	// `Point.total` and `Point_total` of `main` collide with target libc headers.
	output, sources := fastc_self_build_regression_program('name_collision', 'module main
import mathgeom
struct Point {
	x int
	y int
}
fn Point_total(x int, y int) int { return x * 100 + y }
fn (p Point) total() int { return Point_total(p.x, p.y) + 1 }
fn main() {
	p := mathgeom.Pair{ a: 4, b: 2 }
	println(p.sum())
	println(mathgeom.Pair_sum(1, 2))
	println(mathgeom.Pair{ a: 3, b: 4 }.via_value())
	println(Point{ x: 5, y: 6 }.total())
	println(Point_total(7, 8))
}
', {
		'mathgeom': 'module mathgeom
pub struct Pair {
pub:
	a int
	b int
}
pub fn Pair_sum(a int, b int) int { return a * 10 + b }
pub fn (p Pair) sum() int { return Pair_sum(p.a, p.b) + 1 }
pub fn (p Pair) via_value() int {
	f := Pair_sum
	return f(p.a, p.b)
}
'
	})
	assert output.trim_space().split_into_lines() == ['43', '12', '34', '507', '708']
	// The free function moves, never the method: generated code calls builtin
	// methods by their conventional names. Both modes must rename `mathgeom`.
	for mode, source in sources {
		assert source.contains(' __vf_function_mathgeom__Pair_sum(i64 '), 'mode ${mode}'
		assert !source.contains(' mathgeom__Pair_sum(i64 '), 'mode ${mode}'
	}
	assert sources[1].contains(' __vf_function_main__Point_total(i64 ')
	assert !sources[1].contains(' main__Point_total(i64 ')
}

fn test_fastc_reference_call_result_dereferences_into_value_parameter() {
	$if !macos && !linux {
		return
	}
	// A call returning `&Shape` passed where `Shape` is expected is
	// dereferenced, also for a sum type, when the callee returns multiple values
	// and when the reference call has an indexed argument (`parse_type(ids[1])`).
	output, _ := fastc_self_build_regression_program('reference_result', 'module main
struct Box {
	depth int
	name  string
}
struct Leaf {
	depth int
}
type Shape = Box | Leaf
struct Holder {
	shape Shape
}
fn (h &Holder) lookup(id u32) &Shape { return &h.shape }
fn split(s Shape) (int, Shape) {
	return match s {
		Box { s.depth }
		Leaf { s.depth }
	}, s
}
fn depth_of(s Shape) int { d, _ := split(s) return d }
fn main() {
	h := &Holder{ shape: Shape(Box{ depth: 3, name: "x" }) }
	println(depth_of(h.lookup(1)))
	depth, shape := split(h.lookup(u32(2)))
	println(depth)
	println(shape is Box)
	ids := [u32(5), 6]
	_, other := split(h.lookup(ids[1]))
	println(depth_of(other))
}
', map[string]string{})
	assert output.trim_space().split_into_lines() == ['3', '3', 'true', '3']
}
