import os

const default_clone_vexe = @VEXE
const default_clone_tests_dir = os.dir(@FILE)
const default_clone_v3_dir = os.dir(default_clone_tests_dir)
const default_clone_vlib_dir = os.dir(default_clone_v3_dir)
const default_clone_v3_src = os.join_path(default_clone_v3_dir, 'v3.v')

fn test_compiler_default_clone_uses_the_aggregate_type() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_default_clone_${pid}')
	src := os.join_path(os.temp_dir(), 'v3_default_clone_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_default_clone_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(src) or {}
		os.rm(out) or {}
		os.rm(out + '.c') or {}
	}
	build :=
		os.execute('${default_clone_vexe} -gc none -d ownership -path "${default_clone_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${default_clone_v3_src}')
	assert build.exit_code == 0, build.output
	os.write_file(src, 'module main

struct Item implements IClone {
	value int
}

fn copy_item(item Item) Item {
	return item.clone()
}

fn main() {
	item := Item{value: 7}
	copy := copy_item(item)
	assert copy.value == 7
}
') or {
		panic(err)
	}
	compile := os.execute('${v3_bin} ${src} -d ownership -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	c_source := os.read_file(out + '.c') or { panic(err) }
	assert !c_source.contains('string__clone(item)'), c_source
	run := os.execute(out)
	assert run.exit_code == 0, run.output
}

fn test_compiler_default_clone_deep_clones_recursive_aggregates() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_recursive_clone_${pid}')
	src := os.join_path(os.temp_dir(), 'v3_recursive_clone_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_recursive_clone_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(src) or {}
		os.rm(out) or {}
		os.rm(out + '.c') or {}
	}
	build :=
		os.execute('${default_clone_vexe} -gc none -d ownership -path "${default_clone_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${default_clone_v3_src}')
	assert build.exit_code == 0, build.output
	// A type that recurses through owned storage cannot be inlined into a bounded
	// clone. The compiler must emit a runtime-recursive helper so nested children are
	// duplicated; a shallow copy would alias the source, and mutating the clone would
	// corrupt the original (and double-free on cleanup).
	os.write_file(src, "module main

struct Node implements IClone {
mut:
	name     string
	children []Node
}

fn main() {
	original := Node{
		name:     'root'
		children: [
			Node{
				name:     'child'
				children: [
					Node{
						name:     'grandchild'
						children: []
					},
				]
			},
		]
	}
	mut copy := original.clone()
	copy.children[0].children[0].name = 'MUTATED'
	assert original.children[0].children[0].name == 'grandchild'
	assert copy.children[0].children[0].name == 'MUTATED'
}
") or {
		panic(err)
	}
	compile := os.execute('${v3_bin} ${src} -d ownership -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	c_source := os.read_file(out + '.c') or { panic(err) }
	// The repeated aggregate is cloned through the generated recursive helper.
	assert c_source.contains('__v3_default_clone_'), c_source
	run := os.execute(out)
	assert run.exit_code == 0, run.output
}

fn test_autofree_default_clone_requires_sum_field_clone() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_autofree_sum_clone_${pid}')
	defer {
		os.rm(v3_bin) or {}
	}
	build :=
		os.execute('${default_clone_vexe} -gc none -d ownership -path "${default_clone_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${default_clone_v3_src}')
	assert build.exit_code == 0, build.output

	// A compiler-default clone of a struct whose sum field owns storage has no sum-cloning
	// path, so it would share the payload and double-free; autofree must reject it.
	bad_src := os.join_path(os.temp_dir(), 'v3_autofree_sum_bad_${pid}.v')
	bad_out := os.join_path(os.temp_dir(), 'v3_autofree_sum_bad_${pid}')
	defer {
		os.rm(bad_src) or {}
		os.rm(bad_out) or {}
		os.rm(bad_out + '.c') or {}
	}
	os.write_file(bad_src, "module main

type Value = string | int

struct Holder implements IClone {
mut:
	value Value
}

fn main() {
	h := Holder{
		value: Value('x')
	}
	d := h.clone()
	if d.value is string {
		println(d.value)
	}
}
") or {
		panic(err)
	}
	bad := os.execute('${v3_bin} -autofree ${bad_src} -b c -o ${bad_out}')
	assert bad.exit_code != 0, bad.output
	assert bad.output.contains('has no `clone()` method'), bad.output

	// A standalone collection copy of the same sum keeps V1's shallow-copy compatibility.
	good_src := os.join_path(os.temp_dir(), 'v3_autofree_sum_good_${pid}.v')
	good_out := os.join_path(os.temp_dir(), 'v3_autofree_sum_good_${pid}')
	defer {
		os.rm(good_src) or {}
		os.rm(good_out) or {}
		os.rm(good_out + '.c') or {}
	}
	os.write_file(good_src, "module main

type Value = string | int

fn main() {
	arr := [Value('x'), Value(1)]
	dup := arr.clone()
	println(dup.len)
}
") or {
		panic(err)
	}
	good := os.execute('${v3_bin} -autofree ${good_src} -b c -o ${good_out}')
	assert good.exit_code == 0, good.output
}

fn test_autofree_default_clone_requires_custom_drop_clone() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_autofree_drop_clone_${pid}')
	defer {
		os.rm(v3_bin) or {}
	}
	build :=
		os.execute('${default_clone_vexe} -gc none -d ownership -path "${default_clone_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${default_clone_v3_src}')
	assert build.exit_code == 0, build.output

	// An IClone struct with a custom destructor has no explicit clone(). The
	// compiler-default clone only duplicates recognized owned storage, so the opaque
	// handle the destructor manages would be shared and then double-freed; autofree
	// must reject the default clone just like strict ownership does.
	bad_src := os.join_path(os.temp_dir(), 'v3_autofree_drop_bad_${pid}.v')
	bad_out := os.join_path(os.temp_dir(), 'v3_autofree_drop_bad_${pid}')
	defer {
		os.rm(bad_src) or {}
		os.rm(bad_out) or {}
		os.rm(bad_out + '.c') or {}
	}
	os.write_file(bad_src, 'module main

struct Handle implements IClone {
mut:
	fd int
}

fn (mut h Handle) free() {
	h.fd = -1
}

fn main() {
	h := Handle{
		fd: 3
	}
	d := h.clone()
	println(d.fd)
}
') or {
		panic(err)
	}
	bad := os.execute('${v3_bin} -autofree ${bad_src} -b c -o ${bad_out}')
	assert bad.exit_code != 0, bad.output
	assert bad.output.contains('has no `clone()` method'), bad.output

	// Providing the explicit clone() is the escape hatch and compiles again.
	good_src := os.join_path(os.temp_dir(), 'v3_autofree_drop_good_${pid}.v')
	good_out := os.join_path(os.temp_dir(), 'v3_autofree_drop_good_${pid}')
	defer {
		os.rm(good_src) or {}
		os.rm(good_out) or {}
		os.rm(good_out + '.c') or {}
	}
	os.write_file(good_src, 'module main

struct Handle implements IClone {
mut:
	fd int
}

fn (mut h Handle) free() {
	h.fd = -1
}

fn (h Handle) clone() Handle {
	return Handle{
		fd: h.fd
	}
}

fn main() {
	h := Handle{
		fd: 3
	}
	d := h.clone()
	println(d.fd)
}
') or {
		panic(err)
	}
	good := os.execute('${v3_bin} -autofree ${good_src} -b c -o ${good_out}')
	assert good.exit_code == 0, good.output

	// A non-IClone struct with a custom destructor is still copied like V1 through
	// collection operations; only IClone clone targets require the explicit clone.
	compat_src := os.join_path(os.temp_dir(), 'v3_autofree_drop_compat_${pid}.v')
	compat_out := os.join_path(os.temp_dir(), 'v3_autofree_drop_compat_${pid}')
	defer {
		os.rm(compat_src) or {}
		os.rm(compat_out) or {}
		os.rm(compat_out + '.c') or {}
	}
	os.write_file(compat_src, "module main

struct Pos {
	index int
}

fn (mut _ Pos) free() {}

struct Comment {
	text string
	pos  Pos
}

fn main() {
	comments := [Comment{
		text: 'kept'
	}]
	println(comments.filter(it.text == 'kept').len)
}
") or {
		panic(err)
	}
	compat := os.execute('${v3_bin} -autofree ${compat_src} -b c -o ${compat_out}')
	assert compat.exit_code == 0, compat.output
}

fn test_autofree_default_clone_requires_nested_custom_drop_field_clone() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_autofree_nested_drop_${pid}')
	defer {
		os.rm(v3_bin) or {}
	}
	build :=
		os.execute('${default_clone_vexe} -gc none -d ownership -path "${default_clone_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${default_clone_v3_src}')
	assert build.exit_code == 0, build.output

	// An IClone wrapper whose field is a non-IClone struct with a custom destructor
	// is deep-cloned strictly: the compiler-default clone shallow-copies that field,
	// so the destructor would run through both wrapper copies. The nested field must
	// be rejected even though its own type does not implement IClone.
	bad_src := os.join_path(os.temp_dir(), 'v3_autofree_nested_drop_bad_${pid}.v')
	bad_out := os.join_path(os.temp_dir(), 'v3_autofree_nested_drop_bad_${pid}')
	defer {
		os.rm(bad_src) or {}
		os.rm(bad_out) or {}
		os.rm(bad_out + '.c') or {}
	}
	os.write_file(bad_src, 'module main

struct Handle {
mut:
	fd int
}

fn (mut h Handle) free() {
	h.fd = -1
}

struct Wrapper implements IClone {
mut:
	handle Handle
}

fn main() {
	w := Wrapper{
		handle: Handle{
			fd: 3
		}
	}
	d := w.clone()
	println(d.handle.fd)
}
') or {
		panic(err)
	}
	bad := os.execute('${v3_bin} -autofree ${bad_src} -b c -o ${bad_out}')
	assert bad.exit_code != 0, bad.output
	assert bad.output.contains('has no `clone()` method'), bad.output

	// Giving the nested field an explicit clone() lets the wrapper deep-clone again.
	good_src := os.join_path(os.temp_dir(), 'v3_autofree_nested_drop_good_${pid}.v')
	good_out := os.join_path(os.temp_dir(), 'v3_autofree_nested_drop_good_${pid}')
	defer {
		os.rm(good_src) or {}
		os.rm(good_out) or {}
		os.rm(good_out + '.c') or {}
	}
	os.write_file(good_src, 'module main

struct Handle {
mut:
	fd int
}

fn (mut h Handle) free() {
	h.fd = -1
}

fn (h Handle) clone() Handle {
	return Handle{
		fd: h.fd
	}
}

struct Wrapper implements IClone {
mut:
	handle Handle
}

fn main() {
	w := Wrapper{
		handle: Handle{
			fd: 3
		}
	}
	d := w.clone()
	println(d.handle.fd)
}
') or {
		panic(err)
	}
	good := os.execute('${v3_bin} -autofree ${good_src} -b c -o ${good_out}')
	assert good.exit_code == 0, good.output

	// A non-IClone struct holding the same custom-drop field is still copied like V1
	// through collection operations; only strict deep clones require the explicit
	// clone, so the nested field stays a shallow copy here.
	compat_src := os.join_path(os.temp_dir(), 'v3_autofree_nested_drop_compat_${pid}.v')
	compat_out := os.join_path(os.temp_dir(), 'v3_autofree_nested_drop_compat_${pid}')
	defer {
		os.rm(compat_src) or {}
		os.rm(compat_out) or {}
		os.rm(compat_out + '.c') or {}
	}
	os.write_file(compat_src, "module main

struct Handle {
	fd int
}

fn (mut _ Handle) free() {}

struct Holder {
	name   string
	handle Handle
}

fn main() {
	holders := [Holder{
		name: 'kept'
	}]
	println(holders.filter(it.name == 'kept').len)
}
") or {
		panic(err)
	}
	compat := os.execute('${v3_bin} -autofree ${compat_src} -b c -o ${compat_out}')
	assert compat.exit_code == 0, compat.output
}
