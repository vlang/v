module fastc

import os
import v.cmdexec
import v.pref

// fastc_self_build_regression_program builds `main_source`, with `files` as its
// other source files by relative path, as a selfhost program with FastC, with and without
// target libc headers. It compiles each C file with `cc` and returns the output,
// which must be the same for both, and the C of each mode (compact names first).
fn fastc_self_build_regression_program(name string, main_source string, files map[string]string) (string, []string) {
	root := os.join_path(os.vtmp_dir(), 'fastc_self_build_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), main_source) or { panic(err) }
	for relative_path, file_source in files {
		path := os.join_path(root, relative_path)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, file_source) or { panic(err) }
	}
	mut prefs := fastc_self_build_regression_prefs()
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

fn fastc_self_build_regression_prefs() &pref.Preferences {
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.enable_globals = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	return prefs
}

const fastc_name_collision_module = 'module mathgeom
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

// fastc_assert_collision_renamed checks that `source`, the latest generation, moved the
// `mathgeom` free function, whose parameters are `int`, and left the method's name alone.
fn fastc_assert_collision_renamed(source string, label string) {
	int_type := fastc_output_c_type('int')
	assert source.contains(' __vf_function_mathgeom__Pair_sum(${int_type} '), label
	assert !source.contains(' mathgeom__Pair_sum(${int_type} '), label
	assert source.contains(' mathgeom__Pair_sum(mathgeom__Pair p)'), label
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
		'mathgeom/mathgeom.v': fastc_name_collision_module
	})
	assert output.trim_space().split_into_lines() == ['43', '12', '34', '507', '708']
	// The free function moves, never the method: generated code calls builtin
	// methods by their conventional names. Both modes must rename `mathgeom`.
	for mode, source in sources {
		fastc_assert_collision_renamed(source, 'mode ${mode}')
	}
	int_type := fastc_output_c_type('int')
	assert sources[1].contains(' __vf_function_main__Point_total(${int_type} ')
	assert !sources[1].contains(' main__Point_total(${int_type} ')
	assert sources[1].contains(' main__Point_total(Point p)')
}

fn test_fastc_renamed_free_function_follows_the_target_int_width() {
	$if !macos && !linux {
		return
	}
	// Generation only: a V `int` is 32 bits wide here, whatever the host's width.
	root := os.join_path(os.vtmp_dir(), 'fastc_self_build_int_width_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'mathgeom')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main
import mathgeom
fn main() {
	println(mathgeom.Pair{ a: 1, b: 2 }.sum())
	println(mathgeom.Pair_sum(1, 2))
	println(mathgeom.Pair{ a: 3, b: 4 }.via_value())
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'mathgeom', 'mathgeom.v'), fastc_name_collision_module) or {
		panic(err)
	}
	mut prefs := fastc_self_build_regression_prefs()
	prefs.target = pref.target_from('linux', 'x86') or { panic(err) }
	for target_headers in [false, true] {
		prefs.target_libc_headers = target_headers
		source := generate_files([root], prefs) or { panic(err) }
		assert source.contains('i32 __vf_function_mathgeom__Pair_sum(i32 a, i32 b)'), 'target headers ${target_headers}'
		fastc_assert_collision_renamed(source, 'target headers ${target_headers}')
	}
}

fn test_fastc_renamed_free_function_takes_an_unallocated_c_name() {
	$if !macos && !linux {
		return
	}
	// With target libc headers, the constant collision pass names both `main__...`
	// functions in `__vf_function_`, on the spellings a rename of `point_compute_score`
	// would try first. The method collides only there, so compact names still build.
	output, _ := fastc_self_build_regression_program('allocated_name', 'module main
const main__point_compute_score = 10
const main__point_compute_score_1 = 20
fn main__point_compute_score() int { return main__point_compute_score + 1 }
fn main__point_compute_score_1() int { return main__point_compute_score_1 + 2 }
fn point_compute_score(x int) int { return x + 100 }
fn main() {
	println(main__point_compute_score())
	println(main__point_compute_score_1())
	println(point_compute_score(5))
}
', {
		'types.v': '@[generated]
module main
struct point { n int }
fn (p point) compute_score() int { return p.n }
'
	})
	assert output.trim_space().split_into_lines() == ['11', '22', '105']
}

fn test_fastc_renamed_free_function_avoids_names_allocated_in_the_same_pass() {
	$if !macos && !linux {
		return
	}
	// With target libc headers, the first function moves to the `_1` spelling, which is
	// where the second function, whose own name ends in `_1`, would move first. Names are
	// allocated in key order, not in declaration order, so the symbols are reproducible.
	output, sources := fastc_self_build_regression_program('same_pass', 'module main
const main__point_compute_score = 10
fn main__point_compute_score() int { return main__point_compute_score + 1 }
fn point_compute_score_1(x int) int { return x + 200 }
fn point_compute_score(x int) int { return x + 100 }
fn main() {
	println(main__point_compute_score())
	println(point_compute_score(5))
	println(point_compute_score_1(5))
}
', {
		'types.v': '@[generated]
module main
struct point { n int }
fn (p point) compute_score() int { return p.n }
fn (p point) compute_score_1() int { return p.n }
'
	})
	assert output.trim_space().split_into_lines() == ['11', '105', '205']
	int_type := fastc_output_c_type('int')
	assert sources[1].contains(' __vf_function_main__point_compute_score_1(${int_type} x) {\n\treturn x+100')
	assert sources[1].contains(' __vf_function_main__point_compute_score_1_1(${int_type} ')
}

fn test_fastc_c_binding_is_not_taken_for_a_method_of_its_module() {
	$if !macos && !linux {
		return
	}
	// A `C.` key looks like a method: its prefix `C` is not its module. The binding names
	// the function it is declared over, which must keep that name for the linker.
	output, _ := fastc_self_build_regression_program('c_binding', 'module main
fn C.main__helper(a i64) i64
fn helper(a i64) i64 { return a + 1 }
fn main() {
	println(helper(1))
	println(C.main__helper(2))
}
', map[string]string{})
	assert output.trim_space().split_into_lines() == ['2', '3']
}

fn test_fastc_renamed_free_function_avoids_a_function_outside_the_name_table() {
	$if !macos && !linux {
		return
	}
	// In compact mode a main-module function has no entry in `function_c_names`, and a
	// `__vf_` name keeps its spelling: the one the rename would try first.
	output, _ := fastc_self_build_regression_program('untabled_function', 'module main
import mathgeom
fn __vf_function_mathgeom__Pair_sum() int { return 9 }
fn main() {
	println(mathgeom.Pair_sum(0, 7))
	println(mathgeom.Pair{ a: 1, b: 0 }.via_value())
	println(__vf_function_mathgeom__Pair_sum())
}
', {
		'mathgeom/mathgeom.v': fastc_name_collision_module
	})
	assert output.trim_space().split_into_lines() == ['7', '10', '9']
}

fn test_fastc_renamed_free_function_avoids_export_and_global_names() {
	$if !macos && !linux {
		return
	}
	// The spelling the rename would try first already belongs to a non-function symbol.
	collision_file := {
		'mathgeom/mathgeom.v': fastc_name_collision_module
	}
	export_output, _ := fastc_self_build_regression_program('export_name', 'module main
import mathgeom
@[export: "__vf_function_mathgeom__Pair_sum"]
fn other() int { return 9 }
fn main() {
	println(mathgeom.Pair_sum(0, 7))
	println(mathgeom.Pair{ a: 1, b: 0 }.via_value())
	println(other())
}
',
		collision_file)
	assert export_output.trim_space().split_into_lines() == ['7', '10', '9']
	global_output, _ := fastc_self_build_regression_program('global_name', 'module main
import mathgeom
__global __vf_function_mathgeom__Pair_sum = 5
fn main() {
	println(mathgeom.Pair_sum(0, 7))
	println(mathgeom.Pair{ a: 1, b: 0 }.via_value())
	println(__vf_function_mathgeom__Pair_sum)
}
',
		collision_file)
	assert global_output.trim_space().split_into_lines() == ['7', '10', '5']
}

fn test_fastc_main_function_keeps_its_name_when_only_its_key_resembles_a_method() {
	$if !macos && !linux {
		return
	}
	// The undotted key `Point_total` is the function `main__Point_total`, which no
	// method spells; the method's own name only looks like the key doubled.
	output, sources := fastc_self_build_regression_program('undotted_key', 'module main
struct Point {
	x int
}
fn Point_total(x int) int { return x + 1 }
fn (p Point) total_Point_total() int { return Point_total(p.x) + 10 }
fn main() {
	println(Point{ x: 5 }.total_Point_total())
}
', map[string]string{})
	assert output.trim_space().split_into_lines() == ['16']
	for mode, source in sources {
		assert source.contains(' main__Point_total(${fastc_output_c_type('int')} '), 'mode ${mode}'
		assert !source.contains('__vf_function_'), 'mode ${mode}'
	}
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
