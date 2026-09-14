import os

// A builtin that builds a new collection is not a window onto the one it was called
// on. The alias analysis could not read those declarations -- they live in `builtin`
// -- and an unreadable body is otherwise assumed to pass every argument through, so
// an array built by `map` out of an immutable struct's field was reported as aliasing
// that field and writing to it was rejected.

const fresh_builtin_vexe = @VEXE
const fresh_builtin_tests_dir = os.dir(@FILE)
const fresh_builtin_v3_dir = os.dir(fresh_builtin_tests_dir)
const fresh_builtin_vlib_dir = os.dir(fresh_builtin_v3_dir)
const fresh_builtin_v3_src = os.join_path(fresh_builtin_v3_dir, 'v.v')

fn fresh_builtin_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_fresh_builtin_compiler_${os.getpid()}')
	os.rm(v3_bin) or {}
	build := os.execute('${fresh_builtin_vexe} -gc none -path "${fresh_builtin_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${fresh_builtin_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn fresh_builtin_compile(v3_bin string, root string, source string) os.Result {
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	main_v := os.join_path(root, 'main.v')
	os.write_file(main_v, source) or { panic(err) }
	exe := os.join_path(root, 'prog')
	compile := os.execute('${v3_bin} -nocache ${main_v} -b c -o ${exe}')
	if compile.exit_code != 0 {
		return compile
	}
	return os.execute(exe)
}

fn test_an_array_built_by_a_builtin_is_not_an_alias_of_its_source() {
	v3_bin := fresh_builtin_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_fresh_builtin_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := fresh_builtin_compile(v3_bin, root, 'struct Table {
	widths []int
}

fn mapped(t Table) []int {
	return t.widths.map(it + 0)
}

fn filtered(t Table) []int {
	return t.widths.filter(it > 0)
}

fn cloned(t Table) []int {
	return t.widths.clone()
}

fn repeated(t Table) []int {
	return t.widths.repeat(1)
}

fn main() {
	t := Table{ widths: [1, 2, 3] }
	mut a := mapped(t)
	a[0] = 9
	mut b := filtered(t)
	b[0] = 9
	mut c := cloned(t)
	c[0] = 9
	mut d := repeated(t)
	d[0] = 9
	println(t.widths)
	println(a[0])
	println(b[0])
	println(c[0])
	println(d[0])
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ['[1, 2, 3]', '9', '9', '9',
		'9'], res.output
}

// The real thing is still caught: a function that hands back the field itself gives
// out a window onto it, and writing through that writes into the immutable value.
fn test_returning_the_field_itself_is_still_an_alias() {
	v3_bin := fresh_builtin_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_fresh_builtin_real_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := fresh_builtin_compile(v3_bin, root, 'struct Table {
	widths []int
}

fn borrowed(t Table) []int {
	if t.widths.len > 0 {
		return t.widths
	}
	return []int{}
}

fn main() {
	t := Table{ widths: [1, 2, 3] }
	mut a := borrowed(t)
	a[0] = 9
	println(t.widths)
}
')
	assert res.exit_code != 0, res.output
	assert res.output.contains('aliases mutable data from an immutable value'), res.output
}

// The exemption is for the builtins themselves, not for the names they go by. A
// method of one's own called `clone` is read like any other, so handing back the
// receiver's own array through it is still a window onto an immutable value.
fn test_a_method_of_ones_own_named_like_a_builtin_is_not_exempt() {
	v3_bin := fresh_builtin_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_fresh_builtin_shadow_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := fresh_builtin_compile(v3_bin, root, 'struct Table {
	widths []int
}

fn (t Table) clone() []int {
	return t.widths
}

fn (t Table) map_values() []int {
	return t.widths
}

fn borrowed(t Table) []int {
	return t.clone()
}

fn main() {
	t := Table{ widths: [1, 2, 3] }
	mut a := borrowed(t)
	a[0] = 9
	println(t.widths)
}
')
	assert res.exit_code != 0, res.output
	assert res.output.contains('aliases mutable data from an immutable value'), res.output
}

// `reverse` is not one of them. The builtin hands the receiver straight back when
// there are fewer than two elements to turn around, so what it returns can be a
// window onto an immutable value and has to stay in the analysis.
fn test_reverse_is_not_treated_as_fresh() {
	v3_bin := fresh_builtin_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_fresh_builtin_reverse_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := fresh_builtin_compile(v3_bin, root, 'struct Table {
	widths []int
}

fn reversed(t Table) []int {
	return t.widths.reverse()
}

fn main() {
	t := Table{ widths: [1] }
	mut a := reversed(t)
	a[0] = 9
	println(t.widths)
}
')
	assert res.exit_code != 0, res.output
	assert res.output.contains('aliases mutable data from an immutable value'), res.output
}

// Building a new collection copies the elements across, and a copied element is only
// a copy as deep as the element goes: a struct with an array field is copied with the
// array's header, and the two then share the data behind it. So the new outer array
// is fresh while what is reachable through its elements is not.
fn test_a_copied_collection_of_structs_is_not_fresh_underneath() {
	v3_bin := fresh_builtin_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_fresh_builtin_nested_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := fresh_builtin_compile(v3_bin, root, 'struct Row {
mut:
	cells []int
}

struct Table {
	rows []Row
}

fn picked(t Table) []Row {
	return t.rows.filter(true)
}

fn main() {
	t := Table{ rows: [Row{ cells: [1, 2, 3] }] }
	mut rows := picked(t)
	rows[0].cells[0] = 9
	println(t.rows[0].cells)
}
')
	assert res.exit_code != 0, res.output
	assert res.output.contains('aliases mutable data from an immutable value'), res.output
}

// The exemption still holds where the elements really do carry nothing shareable.
// A string is copied whole and shares nothing that can be written through it.
fn test_a_copied_collection_of_flat_elements_is_still_fresh() {
	v3_bin := fresh_builtin_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_fresh_builtin_flat_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := fresh_builtin_compile(v3_bin, root, 'enum Colour {
	red
	green
}

struct Names {
	names   []string
	colours []Colour
}

fn picked(n Names) []string {
	return n.names.filter(it.len > 0)
}

fn shades(n Names) []Colour {
	return n.colours.clone()
}

fn main() {
	n := Names{ names: ["a", "b"], colours: [Colour.red] }
	mut p := picked(n)
	p[0] = "z"
	mut c := shades(n)
	c[0] = .green
	println(n.names)
	println(n.colours)
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ["['a', 'b']", '[red]'], res.output
}

// `map` does not copy the receiver's elements, it makes new ones out of a callback,
// so what that callback makes is what decides. Counting the cells of every row hands
// back numbers, which carry nothing, however much the rows themselves carry.
fn test_map_is_judged_by_what_its_callback_makes() {
	v3_bin := fresh_builtin_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_fresh_builtin_map_result_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := fresh_builtin_compile(v3_bin, root, 'struct Row {
mut:
	cells []int
}

struct Table {
	rows []Row
}

fn counted(t Table) []int {
	return t.rows.map(it.cells.len)
}

fn main() {
	t := Table{ rows: [Row{ cells: [1, 2, 3] }] }
	mut counts := counted(t)
	counts[0] = 9
	println(t.rows[0].cells)
	println(counts)
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ['[1, 2, 3]', '[9]'], res.output
}

// The same callback handing the element straight back does carry what the element
// carries, and that is still a window onto the rows it was called on.
fn test_map_handing_the_element_back_still_shares() {
	v3_bin := fresh_builtin_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_fresh_builtin_map_identity_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := fresh_builtin_compile(v3_bin, root, 'struct Row {
mut:
	cells []int
}

struct Table {
	rows []Row
}

fn picked(t Table) []Row {
	return t.rows.map(it)
}

fn main() {
	t := Table{ rows: [Row{ cells: [1, 2, 3] }] }
	mut rows := picked(t)
	rows[0].cells[0] = 9
	println(t.rows[0].cells)
}
')
	assert res.exit_code != 0, res.output
	assert res.output.contains('aliases mutable data from an immutable value'), res.output
}

// `keys` hands back one side of a map, so only that side is in what comes out: the
// arrays on the other side never appear in it.
fn test_map_keys_is_judged_by_its_keys_alone() {
	v3_bin := fresh_builtin_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_fresh_builtin_keys_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := fresh_builtin_compile(v3_bin, root, 'struct Holder {
	m map[string][]int
}

fn names(h Holder) []string {
	return h.m.keys()
}

fn main() {
	mut mm := map[string][]int{}
	mm["a"] = [1, 2, 3]
	h := Holder{ m: mm }
	mut k := names(h)
	k[0] = "z"
	println(h.m.keys())
	println(k)
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ["['a']", "['z']"], res.output
}

// A callback can be written as an `it` expression, as a lambda, or as a function
// value, and what `map` makes is the callback's result in every one of them. Resolving
// the argument itself answers `fn (int) int` for the latter two, which carries no
// useful answer about the elements, so the callback has to be read for its result.
fn test_map_reads_every_form_of_callback_for_its_result() {
	v3_bin := fresh_builtin_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_fresh_builtin_callbacks_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := fresh_builtin_compile(v3_bin, root, 'struct Table {
	widths []int
}

fn doubled(n int) int {
	return n * 2
}

fn via_it(t Table) []int {
	return t.widths.map(it + 1)
}

fn via_lambda(t Table) []int {
	return t.widths.map(fn (n int) int {
		return n
	})
}

fn via_fn_value(t Table) []int {
	return t.widths.map(doubled)
}

fn main() {
	t := Table{ widths: [1, 2, 3] }
	mut a := via_it(t)
	a[0] = 9
	mut b := via_lambda(t)
	b[0] = 9
	mut c := via_fn_value(t)
	c[0] = 9
	println(t.widths)
	println(a[0])
	println(b[0])
	println(c[0])
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ['[1, 2, 3]', '9', '9', '9'], res.output
}
