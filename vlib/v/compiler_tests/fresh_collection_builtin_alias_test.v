import os
import v.tempname

// A builtin that builds a new collection is not a window onto the one it was called
// on. The alias analysis could not read those declarations -- they live in `builtin`
// -- and an unreadable body is otherwise assumed to pass every argument through, so
// an array built by `map` out of an immutable struct's field was reported as aliasing
// that field and writing to it was rejected.

fn fresh_builtin_fixture_dir() string {
	root := os.join_path(os.vtmp_dir(), 'v3_fresh_builtin_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	return root
}

fn fresh_builtin_write_case(root string, name string, source string) {
	dir := os.join_path(root, name)
	os.mkdir(dir) or { panic(err) }
	os.write_file(os.join_path(dir, '${name}.v'), 'module ${name}\n\n' + source) or { panic(err) }
}

fn fresh_builtin_compile(root string) os.Result {
	return os.exec([@VEXE, '-nocache', root, '-b', 'c', '-o', os.join_path(root, 'prog')])
}

struct FreshBuiltinCase {
	name     string
	source   string
	expected []string
}

struct FreshBuiltinRejection {
	name      string
	source    string
	statement string
}

fn test_collection_copies() {
	cases := [
		FreshBuiltinCase{'copied_widths', 'struct Table {
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
', ['[1, 2, 3]', '9', '9', '9', '9']},
		// The exemption still holds where the elements really do carry nothing shareable.
		// A string is copied whole and shares nothing that can be written through it.
		FreshBuiltinCase{'flat_elements', 'enum Colour {
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
', ["['a', 'b']", '[red]']},
		// `map` does not copy the receiver's elements, it makes new ones out of a callback,
		// so what that callback makes is what decides. Counting the cells of every row hands
		// back numbers, which carry nothing, however much the rows themselves carry.
		FreshBuiltinCase{'mapped_counts', 'struct Row {
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
', ['[1, 2, 3]', '[9]']},
		// `keys` hands back one side of a map, so only that side is in what comes out: the
		// arrays on the other side never appear in it.
		FreshBuiltinCase{'map_keys', 'struct Holder {
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
', ["['a']", "['z']"]},
		// A callback can be written as an `it` expression, as a lambda, or as a function
		// value, and what `map` makes is the callback's result in every one of them. Resolving
		// the argument itself answers `fn (int) int` for the latter two, which carries no
		// useful answer about the elements, so the callback has to be read for its result.
		FreshBuiltinCase{'callback_forms', 'struct Table {
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
', ['[1, 2, 3]', '9', '9', '9']},
	]
	root := fresh_builtin_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for case in cases {
		fresh_builtin_write_case(root, case.name, case.source.replace_once('\nfn main()',
			'\npub fn exercise()'))
		imports << 'import ${case.name}'
		calls << "\t\t'${case.name}' { ${case.name}.exercise() }"
	}
	os.write_file(os.join_path(root, 'main.v'), "module main\n\n${imports.join('\n')}\n\nfn main() {\n\tmatch arguments()[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n") or {
		panic(err)
	}
	compile := fresh_builtin_compile(root)
	assert compile.exit_code == 0, compile.output
	exe := os.join_path(root, 'prog')
	for case in cases {
		run := os.exec([exe, case.name])
		assert run.exit_code == 0, '${case.name}: ${run.output}'
		lines := run.output.trim_space().split('\n').map(it.trim_space())
		assert lines == case.expected, '${case.name}: ${run.output}'
	}
}

fn test_collection_aliases() {
	cases := [
		// The real thing is still caught: a function that hands back the field itself gives
		// out a window onto it, and writing through that writes into the immutable value.
		FreshBuiltinRejection{'returned_field', 'struct Table {
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
', '\ta[0] = 9'},
		// The exemption is for the builtins themselves, not for the names they go by. A
		// method of one's own called `clone` is read like any other, so handing back the
		// receiver's own array through it is still a window onto an immutable value.
		FreshBuiltinRejection{'shadowed_clone', 'struct Table {
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
', '\ta[0] = 9'},
		// `reverse` is not one of them. The builtin hands the receiver straight back when
		// there are fewer than two elements to turn around, so what it returns can be a
		// window onto an immutable value and has to stay in the analysis.
		FreshBuiltinRejection{'short_reverse', 'struct Table {
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
', '\ta[0] = 9'},
		// Building a new collection copies the elements across, and a copied element is only
		// a copy as deep as the element goes: a struct with an array field is copied with the
		// array's header, and the two then share the data behind it. So the new outer array
		// is fresh while what is reachable through its elements is not.
		FreshBuiltinRejection{'copied_nested_rows', 'struct Row {
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
', '\trows[0].cells[0] = 9'},
		// The same callback handing the element straight back does carry what the element
		// carries, and that is still a window onto the rows it was called on.
		FreshBuiltinRejection{'identity_map', 'struct Row {
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
', '\trows[0].cells[0] = 9'},
	]
	root := fresh_builtin_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	mut imports := []string{cap: cases.len}
	for case in cases {
		fresh_builtin_write_case(root, case.name, case.source)
		imports << 'import ${case.name} as _'
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main\n\n${imports.join('\n')}\n\nfn main() {}\n') or {
		panic(err)
	}
	compile := fresh_builtin_compile(root)
	assert compile.exit_code != 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	errors := compile.output.split_into_lines().filter(it.contains(': error: '))
	for case in cases {
		source_index := case.source.split_into_lines().index(case.statement)
		assert source_index >= 0, case.statement
		// The module declaration adds two lines to the original fixture.
		location := '${case.name}.v:${source_index + 3}:'
		assert errors.any(it.contains(location)
			&& it.contains('aliases mutable data from an immutable value')), '${case.name}: ${compile.output}'
	}
}
