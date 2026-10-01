import os

const borrow_storage_vexe = @VEXE
const borrow_storage_vroot = os.dir(os.dir(os.dir(os.dir(@FILE))))
const borrow_storage_tmp_dir = os.join_path(os.vtmp_dir(), 'borrow_storage_${os.getpid()}')
const borrow_storage_v3 = os.join_path(borrow_storage_tmp_dir, 'v3_ownership')

const borrow_storage_drop_decls = 'interface Drop {
mut:
	drop()
}

struct Res implements IClone, Drop {
	id int
}

fn (mut r Res) drop() {
	println("drop \${r.id}")
}

fn (r &Res) clone() Res {
	return Res{r.id + 100}
}
'

fn testsuite_begin() {
	os.mkdir_all(borrow_storage_tmp_dir) or { panic(err) }
	cmd_v := os.join_path(borrow_storage_vroot, 'cmd', 'v')
	vlib := os.join_path(borrow_storage_vroot, 'vlib')
	build := os.execute('${os.quoted_path(borrow_storage_vexe)} -gc none -d ownership -path "${vlib}|@vlib|@vmodules" -o ${os.quoted_path(borrow_storage_v3)} ${os.quoted_path(cmd_v)}')
	assert build.exit_code == 0, build.output
}

fn testsuite_end() {
	os.rmdir_all(borrow_storage_tmp_dir) or {}
}

fn borrow_storage_compile(name string, src string) os.Result {
	path := os.join_path(borrow_storage_tmp_dir, '${name}.v')
	os.write_file(path, src) or { panic(err) }
	out := os.join_path(borrow_storage_tmp_dir, name)
	return os.execute('${os.quoted_path(borrow_storage_v3)} -ownership -d ownership -no-parallel -o ${os.quoted_path(out)} ${os.quoted_path(path)}')
}

fn borrow_storage_run(name string, src string) string {
	build := borrow_storage_compile(name, src)
	assert build.exit_code == 0, build.output
	run := os.execute(os.quoted_path(os.join_path(borrow_storage_tmp_dir, name)))
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_owned_borrowed_array_views_clone_only_when_detaching() {
	output := borrow_storage_run('fixed_array_detach', '@[has_globals]
module main

__global next_id = 0
__global clone_count = 0
__global dropped = map[int]bool{}

interface Drop {
mut:
	drop()
}

struct Resource implements IClone, Drop {
	id int
mut:
	value int
}

fn fresh() Resource {
	next_id++
	return Resource{next_id, 0}
}

fn (r &Resource) clone() Resource {
	clone_count++
	mut result := fresh()
	result.value = r.value
	return result
}

fn (mut r Resource) drop() {
	if r.id == 0 { return }
	assert !dropped[r.id], "owner dropped twice"
	dropped[r.id] = true
}

fn observe(values []Resource) { assert values[0].value == 0 }
fn noops(mut values []Resource) {
	observe(values)
	values.ensure_cap(values.cap)
	values.grow_cap(0)
	unsafe { values.grow_len(0) }
	values << []Resource{}
	values.prepend([]Resource{})
	values.insert(0, []Resource{})
}

fn change(mut values []Resource, operation int) []Resource {
	values[0].value = 7
	if operation == 0 {
		values << fresh()
	} else if operation == 1 {
		values.prepend(fresh())
	} else if operation == 2 {
		values.insert(1, fresh())
	} else if operation == 3 {
		values.ensure_cap(values.cap + 1)
	} else if operation == 4 {
		values.delete(1)
	} else if operation == 5 {
		values.trim(1)
	} else if operation == 6 {
		popped := values.pop()
		assert !dropped[popped.id]
	} else if operation == 7 {
		values.grow_cap(1)
	} else {
		unsafe { values.grow_len(1) }
		values[2] = fresh()
	}
	return values
}

fn main() {
	for operation in 0 .. 9 {
		mut fixed := [fresh(), fresh()]
		original_id := fixed[0].id
		before := clone_count
		noops(mut fixed[..])
		assert clone_count == before
		changed := change(mut fixed[..], operation)
		assert clone_count == before + 2
		assert fixed[0].id == original_id
		assert fixed[0].value == 7
		assert !dropped[original_id]
		unsafe { changed.free() }
		assert !dropped[original_id]
		drop_owned(fixed)
	}
	assert dropped.len == next_id
	println("ok")
}
')
	assert output == 'ok'
}

fn test_owned_borrowed_array_borrowed_empty_uncloneable_range_can_grow() {
	output := borrow_storage_run('empty_uncloneable_fixed', '@[has_globals]
module main
__global dropped = map[int]bool{}
interface Drop {
mut:
	drop()
}
struct Handle implements Drop { id int }
fn (mut h Handle) drop() {
	assert !dropped[h.id]
	dropped[h.id] = true
}
fn grow(mut values []Handle, reserve bool) []Handle {
	if reserve { values.ensure_cap(2) }
	values << Handle{2}
	return values
}
fn main() {
	for reserve in [false, true] {
		dropped = map[int]bool{}
		mut fixed := [Handle{1}]
		result := grow(mut fixed[0..0], reserve)
		assert result.len == 1
		assert result[0].id == 2
		assert !dropped[1]
		drop_owned(result)
		drop_owned(fixed)
		assert dropped.len == 2
	}
	println("ok")
}
')
	assert output == 'ok'
}

fn test_owned_borrowed_array_nonempty_uncloneable_views_fail_when_owning() {
	for operation in ['values << Handle{2}', 'values.ensure_cap(values.cap + 1)', 'return values', 'reader := fn [values] () int { return values[0].id }; _ = reader'] {
		name := 'uncloneable_boundary_${operation.len}'
		tail := if operation.starts_with('return ') { '' } else { 'return []Handle{}' }
		source := 'interface Drop { mut: drop() }
struct Handle implements Drop { id int }
fn (mut h Handle) drop() {}
fn own(mut values []Handle) []Handle {
	${operation}
	${tail}
}

fn test_mutable_array_value_captures_acquire_owned_snapshots() {
	output := borrow_storage_run('array_capture_storage', '@[has_globals]
module main
__global next_id = 0
__global clones = 0
__global dropped = map[int]bool{}
interface Drop { mut: drop() }
struct Resource implements IClone, Drop { id int mut: value int }
fn fresh() Resource { next_id++; return Resource{next_id, 0} }
fn (r &Resource) clone() Resource { clones++; return Resource{fresh().id, r.value} }
fn (mut r Resource) drop() {
	assert !dropped[r.id], "double drop"
	dropped[r.id] = true
}
fn capture(mut values []Resource) fn () int {
	return fn [values] () int {
		assert !dropped[values[0].id]
		return values[0].value
	}
}
fn make_capture(borrowed bool) fn () int {
	mut owner := [fresh()]
	kept := if borrowed { capture(mut owner[..]) } else { capture(mut owner) }
	owner[0].value = 7
	return kept
}
fn check_capture(borrowed bool) {
	kept := make_capture(borrowed)
	assert kept() == 0
}
fn capture_pointer(values &[]Resource) fn () int {
	return fn [values] () int { return values[0].value }
}
fn check_pointer_capture() {
	mut owner := [fresh()]
	kept := capture_pointer(&owner)
	owner[0].value = 9
	assert kept() == 9
}
fn capture_ints(mut values []int) fn () int {
	return fn [values] () int { return values[0] }
}
fn check_primitive_capture() {
	mut owner := [401]
	kept := capture_ints(mut owner)
	owner[0] = 402
	assert kept() == 401
}
fn main() {
	for borrowed in [false, true] { check_capture(borrowed) }
	assert clones == 2
	check_pointer_capture()
	check_primitive_capture()
	assert clones == 2
	assert dropped.len == next_id
	println("ok")
}
')
	assert output == 'ok'
}
fn main() {
	mut fixed := [Handle{1}]
	result := own(mut fixed[..])
	assert result.len == 0
}
'
		build := borrow_storage_compile(name, source)
		assert build.exit_code == 0, build.output
		run := os.execute(os.quoted_path(os.join_path(borrow_storage_tmp_dir, name)))
		assert run.exit_code != 0, run.output
		assert run.output.contains('requires ownership destruction but has no compatible `clone()` method'), run.output
	}
}
