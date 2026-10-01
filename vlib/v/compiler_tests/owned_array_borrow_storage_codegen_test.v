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
	values.delete_many(0, 0)
	values.delete_many(values.len, 0)
}

fn change(mut values []Resource, operation int) {
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
	} else if operation == 8 {
		unsafe { values.grow_len(1) }
		values[2] = fresh()
	} else if operation == 9 {
		values << values
	} else if operation == 10 {
		values.prepend(values)
	} else {
		values.insert(1, values)
	}
	unsafe { values.free() }
}

fn main() {
	for operation in 0 .. 12 {
		mut fixed := [fresh(), fresh()]
		original_id := fixed[0].id
		before := clone_count
		noops(mut fixed[..])
		assert clone_count == before
		change(mut fixed[..], operation)
		assert clone_count == before + if operation >= 9 { 4 } else { 2 }
		assert fixed[0].id == original_id
		assert fixed[0].value == 7
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
fn duplicate_empty(mut values []Handle) {
	values << values
	values.prepend(values)
	values.insert(0, values)
}
fn delete_empty_range(mut values []Handle) {
	values.delete_many(0, 0)
	values.delete_many(values.len, 0)
}
fn keep_empty(mut values []Handle) []Handle { return values }
fn grow(mut values []Handle, reserve bool) {
	if reserve { values.ensure_cap(2) }
	values << Handle{2}
	assert values.len == 1 && values[0].id == 2
	unsafe { values.free() }
}
fn main() {
	mut empty := []Handle{}
	duplicate_empty(mut empty)
	assert empty.len == 0
	kept_empty := keep_empty(mut empty)
	assert kept_empty.len == 0
	drop_owned(kept_empty)
	for reserve in [false, true] {
		dropped = map[int]bool{}
		mut fixed := [Handle{1}]
		delete_empty_range(mut fixed[..])
		assert fixed.len == 1 && fixed[0].id == 1
		assert dropped.len == 0
		grow(mut fixed[0..0], reserve)
		assert dropped[2]
		assert !dropped[1]
		drop_owned(fixed)
		assert dropped.len == 2
	}
	println("ok")
}
')
	assert output == 'ok'
}

fn test_owned_borrowed_array_nonempty_uncloneable_views_fail_when_owning() {
	for operation in ['values << Handle{2}', 'values.ensure_cap(values.cap + 1)', 'return values',
		'reader := fn [values] () int { return values[0].id }; _ = reader'] {
		name := 'uncloneable_boundary_${operation.len}'
		tail := if operation.starts_with('return ') { '' } else { 'return []Handle{}' }
		source := 'interface Drop { mut: drop() }
struct Handle implements Drop { id int }
fn (mut h Handle) drop() {}
fn own(mut values []Handle) []Handle {
	${operation}
	${tail}
}

fn main() {
	mut fixed := [Handle{1}]
	result := own(mut fixed[..])
	assert result.len == 0
}
'
		for borrowed in [false, true] {
			input := if borrowed { source } else { source.replace('mut fixed[..]', 'mut fixed') }
			case_name := '${name}_${borrowed}'
			build := borrow_storage_compile(case_name, input)
			assert build.exit_code == 0, build.output
			run := os.execute(os.quoted_path(os.join_path(borrow_storage_tmp_dir, case_name)))
			assert run.exit_code != 0, run.output
			assert run.output.contains('requires ownership destruction but has no compatible `clone()` method'), run.output
		}
	}
}

fn test_borrowed_owned_array_storage_scope_and_retained_headers() {
	source := '@[has_globals]
module main
__global next_id = 0
__global clones = 0
__global dropped = map[int]bool{}
interface Drop {
mut:
	drop()
}
struct Resource implements IClone, Drop {
	id int
	payload string
mut:
	value int
}
fn fresh() Resource {
	next_id++
	return Resource{next_id, "payload".repeat(4), 0}
}
fn (r &Resource) clone() Resource {
	clones++
	mut result := fresh()
	result.value = r.value
	return result
}
fn (mut r Resource) drop() {
	assert !dropped[r.id], "double drop"
	dropped[r.id] = true
}
struct Holder {
mut:
	values []Resource
	label string
}
fn inspect(values &[]Resource) int { return values[0].id }
fn change(mut values []Resource) { values[0].value = 7 }
fn keep(mut values []Resource) []Resource { return values }
fn keep_reference(mut values []Resource) &[]Resource { return &values }
type ResourceArray = []Resource
struct Stored { mut: values &ResourceArray = unsafe { nil } }
type ResourceRef = &[]Resource
type ResourceRefAlias = ResourceRef
struct StoredRef { mut: values ResourceRefAlias = unsafe { nil } }
struct StoredOption { values ?&[]Resource }
fn store_and_change(mut values []Resource, mut stored Stored) {
	values[0].value = 9
	stored.values = &values
}
fn store_alias_and_change(mut values []Resource, mut stored StoredRef, initializer bool) {
	values[0].value = 11
	if initializer {
		stored = StoredRef{values: &values}
	} else {
		stored.values = &values
	}
	values[0].value = 12
}
fn ordinary_scope() {
	mut holder := Holder{[fresh()], "label".repeat(4)}
	assert inspect(holder.values) == holder.values[0].id
	change(mut holder.values[..])
	assert holder.values[0].value == 7
	assert clones == 0
}
fn returned() []Resource {
	mut holder := Holder{[fresh()], "label".repeat(4)}
	return keep(mut holder.values[..])
}
fn holder_value_reference(holder Holder) &[]Resource {
	unsafe { return keep_readonly_reference(&holder.values) }
}
fn keep_readonly_reference(values &[]Resource) &[]Resource { return values }
fn dispose_reference(values &[]Resource) {
	unsafe {
		values.free()
	}
}
fn check_stored_alias(values &[]Resource) {
	unsafe {
		assert (*values)[0].value == 11
		assert !dropped[(*values)[0].id]
		assert (*values)[0].payload == "payload".repeat(4)
		dispose_reference(values)
	}
}
fn returned_reference() &[]Resource {
	mut holder := Holder{[fresh()], "label".repeat(4)}
	return keep_reference(mut holder.values[..])
}
fn stored_reference() Stored {
	mut holder := Holder{[fresh()], "label".repeat(4)}
	mut stored := Stored{}
	store_and_change(mut holder.values[..], mut stored)
	assert holder.values[0].value == 9
	return stored
}
fn stored_reference_alias(initializer bool) StoredRef {
	mut holder := Holder{[fresh()], "label".repeat(4)}
	mut stored := StoredRef{}
	store_alias_and_change(mut holder.values[..], mut stored, initializer)
	assert holder.values[0].value == 12
	return stored
}
fn stored_option_reference(present bool) StoredOption {
	mut holder := Holder{[fresh()], "label".repeat(4)}
	if !present {
		return StoredOption{}
	}
	holder.values[0].value = 11
	stored := store_option_reference(mut holder.values[..])
	holder.values[0].value = 12
	return stored
}
fn store_option_reference(mut values []Resource) StoredOption {
	return StoredOption{values: &values}
}
fn map_literal_from_borrow(mut values []Resource) map[string][]Resource {
	return {"hit": values}
}
fn map_set_from_borrow(mut values []Resource, mut stored map[string][]Resource) {
	stored["hit"] = values
}
fn stored_map(initializer bool) map[string][]Resource {
	mut holder := Holder{[fresh()], "label".repeat(4)}
	if initializer {
		return map_literal_from_borrow(mut holder.values[..])
	}
	mut stored := map[string][]Resource{}
	map_set_from_borrow(mut holder.values[..], mut stored)
	return stored
}
type ResourceStorageSum = []Resource | int
type ResourceStorageLayer = ResourceStorageSum | bool
type ResourceGenericStorage[T] = []T | int
fn keep_sum(mut values []Resource) ResourceStorageSum { return values }
fn keep_layer(mut values []Resource) ResourceStorageLayer { return values }
fn keep_generic_sum(mut values []Resource) ResourceGenericStorage[Resource] { return values }
fn stored_sum() ResourceStorageSum {
	mut holder := Holder{[fresh()], "label".repeat(4)}
	return keep_sum(mut holder.values[..])
}
fn stored_layer() ResourceStorageLayer {
	mut holder := Holder{[fresh()], "label".repeat(4)}
	return keep_layer(mut holder.values[..])
}
fn stored_generic_sum() ResourceGenericStorage[Resource] {
	mut holder := Holder{[fresh()], "label".repeat(4)}
	return keep_generic_sum(mut holder.values[..])
}
fn capture_borrow(mut values []Resource) fn () int {
	return fn [values] () int {
		assert !dropped[values[0].id]
		assert values[0].payload == "payload".repeat(4)
		return values[0].value
	}
}
fn stored_borrow_capture() fn () int {
	mut holder := Holder{[fresh()], "label".repeat(4)}
	kept := capture_borrow(mut holder.values[..])
	holder.values[0].value = 31
	return kept
}
fn check_borrow_capture() {
	kept := stored_borrow_capture()
	assert kept() == 0
}
fn keep_nested_source(mut inner ResourceStorageSum) ResourceStorageLayer { return inner }
fn keep_optional_sum(mut values []Resource, mode int) ?ResourceStorageSum {
	if mode == 0 { return none }
	if mode == 2 { return ?ResourceStorageSum(ResourceStorageSum(values)) }
	return values
}
fn stored_optional_sum(mode int) ?ResourceStorageSum {
	mut holder := Holder{[fresh()], "label".repeat(4)}
	return keep_optional_sum(mut holder.values[..], mode)
}
fn keep_result_sum(mut values []Resource, present bool) !ResourceStorageSum {
	if !present { return error("absent sum") }
	return values
}
fn stored_result_sum(present bool) !ResourceStorageSum {
	mut holder := Holder{[fresh()], "label".repeat(4)}
	return keep_result_sum(mut holder.values[..], present)
}
fn main() {
	ordinary_scope()
	assert dropped.len == 1
	values := returned()
	assert clones == 1
	assert dropped.len == 2
	assert !dropped[values[0].id]
	assert values[0].payload == "payload".repeat(4)
	drop_owned(values)
	reference := returned_reference()
	assert clones == 2
	unsafe {
		assert !dropped[(*reference)[0].id]
		assert (*reference)[0].payload == "payload".repeat(4)
		dispose_reference(reference)
	}
	stored := stored_reference()
	assert clones == 3
	unsafe {
		assert (*stored.values)[0].value == 9
		assert !dropped[(*stored.values)[0].id]
		dispose_reference(stored.values)
	}
	for index, initializer in [false, true] {
		stored_alias := stored_reference_alias(initializer)
		assert clones == 4 + index
		check_stored_alias(stored_alias.values)
	}
	assert stored_option_reference(false).values == none
	assert clones == 5
	stored_option := stored_option_reference(true)
	assert clones == 6
	check_stored_alias(stored_option.values or { panic("missing retained reference") })
	for index, initializer in [false, true] {
		stored_values := stored_map(initializer)
		assert clones == 7 + index
		assert !dropped[stored_values["hit"][0].id]
		assert stored_values["hit"][0].payload == "payload".repeat(4)
		drop_owned(stored_values)
	}
	sum_value := stored_sum()
	assert clones == 9
	if sum_value is []Resource {
		assert !dropped[sum_value[0].id]
		assert sum_value[0].payload == "payload".repeat(4)
	} else { assert false }
	drop_owned(sum_value)
	layer := stored_layer()
	assert clones == 10
	if layer is ResourceStorageSum {
		if layer is []Resource {
			assert !dropped[layer[0].id]
			assert layer[0].payload == "payload".repeat(4)
		} else { assert false }
	} else { assert false }
	drop_owned(layer)
	generic_sum := stored_generic_sum()
	assert clones == 11
	if generic_sum is []Resource {
		assert !dropped[generic_sum[0].id]
		assert generic_sum[0].payload == "payload".repeat(4)
	} else { assert false }
	drop_owned(generic_sum)
	assert stored_optional_sum(0) == none
	assert clones == 11
	for mode in [1, 2] {
		optional_sum := stored_optional_sum(mode) or { panic("missing sum") }
		assert clones == 11 + mode
		if optional_sum is []Resource {
			assert !dropped[optional_sum[0].id]
			assert optional_sum[0].payload == "payload".repeat(4)
		} else { assert false }
		drop_owned(optional_sum)
	}
	if absent_sum := stored_result_sum(false) {
		drop_owned(absent_sum)
		assert false
	} else { assert err.msg() == "absent sum" }
	assert clones == 13
	result_sum := stored_result_sum(true) or { panic(err) }
	assert clones == 14
	if result_sum is []Resource {
		assert !dropped[result_sum[0].id]
		assert result_sum[0].payload == "payload".repeat(4)
	} else { assert false }
	drop_owned(result_sum)
	mut inner := ResourceStorageSum([fresh()])
	outer := keep_nested_source(mut inner)
	assert clones == 15
	if inner is []Resource {
		assert !dropped[inner[0].id]
		inner[0].value = 19
	} else { assert false }
	if outer is ResourceStorageSum {
		if outer is []Resource {
			assert !dropped[outer[0].id]
			assert outer[0].value == 0
		} else { assert false }
	} else { assert false }
	drop_owned(outer)
	drop_owned(inner)
	check_borrow_capture()
	assert clones == 16
	assert dropped.len == next_id
	println("ok")
}
'
	for borrowed in [false, true] {
		input := if borrowed {
			source
		} else {
			source.replace('mut holder.values[..]', 'mut holder.values')
		}
		output := borrow_storage_run('borrowed_array_scope_${borrowed}', input)
		assert output == 'ok'
	}
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

fn test_uncloneable_mut_array_bulk_copies_fail_before_duplicating_owners() {
	for index, operation in ['values << values', 'values.prepend(values)', 'values.insert(0, values)'] {
		for borrowed in [false, true] {
			name := 'uncloneable_bulk_${index}_${borrowed}'
			source := 'interface Drop { mut: drop() }
struct Handle implements Drop { id int }
fn (mut h Handle) drop() {}
fn duplicate(mut values []Handle) {
	${operation}
}
fn main() {
	mut values := [Handle{1}]
	if ${borrowed} { duplicate(mut values[..]) } else { duplicate(mut values) }
}
'
			build := borrow_storage_compile(name, source)
			assert build.exit_code == 0, build.output
			run := os.execute(os.quoted_path(os.join_path(borrow_storage_tmp_dir, name)))
			assert run.exit_code != 0, run.output
			assert run.output.contains('requires ownership destruction but has no compatible `clone()` method'), run.output
		}
	}
}

fn test_empty_owned_array_deletion_preserves_invalid_index_diagnostics() {
	for index in [-1, 2] {
		name := 'empty_delete_invalid_${index}'
		source := 'interface Drop { mut: drop() }
struct Handle implements Drop { id int }
fn (mut h Handle) drop() {}
fn delete_empty_range(mut values []Handle) { values.delete_many(${index}, 0) }
fn main() {
	mut values := [Handle{1}]
	delete_empty_range(mut values[..])
}
'
		build := borrow_storage_compile(name, source)
		assert build.exit_code == 0, build.output
		run := os.execute(os.quoted_path(os.join_path(borrow_storage_tmp_dir, name)))
		assert run.exit_code != 0, run.output
		assert run.output.contains('array.delete: index out of range'), run.output
	}
}
