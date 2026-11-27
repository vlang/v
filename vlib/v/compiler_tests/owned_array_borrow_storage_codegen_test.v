// vtest ownership
import os

const borrow_storage_tmp_dir = os.join_path(os.vtmp_dir(), 'borrow_storage_${os.getpid()}')
// `v -ownership` runs the launcher's cached ownership compiler, built from this tree.
const borrow_storage_v3 = @VEXE

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
}

fn testsuite_end() {
	os.rmdir_all(borrow_storage_tmp_dir) or {}
}

fn borrow_storage_compile(name string, src string) os.Result {
	return borrow_storage_compile_with_flags(name, src, '')
}

fn borrow_storage_compile_with_flags(name string, src string, flags string) os.Result {
	path := os.join_path(borrow_storage_tmp_dir, '${name}.v')
	os.write_file(path, src) or { panic(err) }
	out := os.join_path(borrow_storage_tmp_dir, name)
	return os.exec(['${borrow_storage_v3}', '-ownership', '-d', 'ownership', '-no-parallel',
		...(os.split_args(flags) or { panic(err) }), '-o', '${out}', path])
}

fn borrow_storage_run(name string, src string) string {
	build := borrow_storage_compile(name, src)
	assert build.exit_code == 0, '${name}: ${build.output}'
	run := os.exec([os.join_path(borrow_storage_tmp_dir, name)])
	assert run.exit_code == 0, '${name}: ${run.output}'
	return run.output.trim_space()
}

fn borrow_storage_compile_dispatch(name string, declarations string, branches []string) string {
	source := declarations + 'fn main() {
	match arguments()[1] {
${branches.join('\n')}
		else { panic("unknown array ownership case") }
	}
}
'
	build := borrow_storage_compile(name, source)
	assert build.exit_code == 0, '${name}: ${build.output}'
	return os.join_path(borrow_storage_tmp_dir, name)
}

fn test_owned_borrowed_array_views_preserve_owners() {
	source := '@[has_globals]
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

fn check_detach() {
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
	println("detach")
}

__global empty_dropped = map[int]bool{}
struct Handle implements Drop { id int }
fn (mut h Handle) drop() {
	assert !empty_dropped[h.id]
	empty_dropped[h.id] = true
}
fn duplicate_empty(mut values []Handle) {
	values << values
	values.prepend([]Handle{})
	values.insert(0, []Handle{})
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
fn check_empty_growth() {
	mut empty := []Handle{}
	duplicate_empty(mut empty)
	assert empty.len == 0
	kept_empty := keep_empty(mut empty)
	assert kept_empty.len == 0
	drop_owned(kept_empty)
	for reserve in [false, true] {
		empty_dropped = map[int]bool{}
		mut fixed := [Handle{1}]
		delete_empty_range(mut fixed[..])
		assert fixed.len == 1 && fixed[0].id == 1
		assert empty_dropped.len == 0
		grow(mut fixed[0..0], reserve)
		assert empty_dropped[2]
		assert !empty_dropped[1]
		drop_owned(fixed)
		assert empty_dropped.len == 2
	}
	println("empty_growth")
}
'
	binary := borrow_storage_compile_dispatch('borrowed_array_views', source, [
		'\t\t"detach" { check_detach() }',
		'\t\t"empty_growth" { check_empty_growth() }',
	])
	for name in ['detach', 'empty_growth'] {
		run := os.exec([binary, name])
		assert run.exit_code == 0, '${name}: ${run.output}'
		assert run.output.trim_space() == name, '${name}: ${run.output}'
	}
}

fn test_owned_borrowed_array_nonempty_uncloneable_views_fail_when_owning() {
	operations := {
		'append':  'values << Handle{2}'
		'reserve': 'values.ensure_cap(values.cap + 1)'
		'return':  'return values'
		'capture': 'reader := fn [values] () int { return values[0].id }; _ = reader'
	}
	mut source := 'interface Drop { mut: drop() }
struct Handle implements Drop { id int }
fn (mut h Handle) drop() {}
'
	mut branches := []string{cap: operations.len * 2}
	for action, operation in operations {
		tail := if action == 'return' { '' } else { 'return []Handle{}' }
		for borrowed in [false, true] {
			name := '${action}_${borrowed}'
			receiver := if borrowed { 'fixed[..]' } else { 'fixed' }
			source += '
fn own_${name}(mut values []Handle) []Handle {
	${operation}
	${tail}
}

'
			branches << '		"${name}" {
			mut fixed := [Handle{1}]
			result := own_${name}(mut ${receiver})
			assert result.len == 0
		}'
		}
	}
	binary := borrow_storage_compile_dispatch('uncloneable_boundary', source, branches)
	for action, _ in operations {
		for borrowed in [false, true] {
			name := '${action}_${borrowed}'
			run := os.exec([binary, name])
			if !borrowed && action in ['append', 'reserve'] {
				// Mutating an unsliced owner does not require independent element owners.
				assert run.exit_code == 0, '${name}: ${run.output}'
				continue
			}
			assert run.exit_code != 0, '${name}: ${run.output}'
			assert run.output.contains('requires ownership destruction but has no compatible `clone()` method'), '${name}: ${run.output}'
		}
	}
}

fn test_borrowed_owned_array_storage_scope_and_retained_headers() {
	source := '@[has_globals]
module main
import builtin.closure
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
// Retained references below request an owned snapshot, including managed inputs.
fn owned_reference(values &[]Resource) &[]Resource {
	return &[]Resource{len: values.len, init: values[index].clone()}
}
fn keep_reference(mut values []Resource) &[]Resource { return owned_reference(&values) }
type ResourceArray = []Resource
struct Stored { mut: values &ResourceArray = unsafe { nil } }
type ResourceRef = &[]Resource
type ResourceRefAlias = ResourceRef
struct StoredRef { mut: values ResourceRefAlias = unsafe { nil } }
struct StoredOption { values ?ResourceRefAlias }
fn store_and_change(mut values []Resource, mut stored Stored) {
	values[0].value = 9
	copy := owned_reference(&values)
	stored.values = copy
}
fn store_alias_and_change(mut values []Resource, mut stored StoredRef, initializer bool) {
	values[0].value = 11
	copy := owned_reference(&values)
	if initializer {
		stored = StoredRef{values: copy}
	} else {
		stored.values = copy
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
	copy := owned_reference(&values)
	return StoredOption{values: ?ResourceRefAlias(copy)}
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
	mut lifetime := closure.new_lifetime()
	lifetime.frame(fn () {
		kept := stored_borrow_capture()
		assert kept() == 0
	}) or { panic(err) }
	lifetime.dispose() or { panic(err) }
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
// check verifies how array storage is retained and dropped.
pub fn check() {
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
	println(@MOD)
}
'
	project := 'retained_array_headers'
	root := os.join_path(borrow_storage_tmp_dir, project)
	variants := ['owned_header', 'borrowed_header']
	mut imports := []string{cap: variants.len}
	mut branches := []string{cap: variants.len}
	for name in variants {
		input := if name == 'owned_header' {
			source.replace('mut holder.values[..]', 'mut holder.values')
		} else {
			source
		}
		dir := os.join_path(root, name)
		os.mkdir_all(dir) or { panic(err) }
		os.write_file(os.join_path(dir, 'check.v'), input.replace('module main', 'module ${name}')) or {
			panic(err)
		}
		imports << 'import ${name}'
		branches << '\t\t"${name}" { ${name}.check() }'
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${project}' }\n") or { panic(err) }
	binary := borrow_storage_compile_dispatch(os.join_path(project, 'main'), imports.join('\n') + '\n\n',
		branches)
	for name in variants {
		run := os.exec([binary, name])
		assert run.exit_code == 0, '${name}: ${run.output}'
		assert run.output.trim_space() == name, '${name}: ${run.output}'
	}
}

fn test_array_sum_storage_preserves_owned_payloads() {
	output := borrow_storage_run('array_sum_payloads', borrow_storage_drop_decls + '
type Bucket = []Res | int
fn keep(mut values []Res) ?Bucket { return ?Bucket(Bucket(values)) }
fn absent() ?Bucket { return none }
fn explicit_absent() ?Bucket { return ?Bucket(none) }
fn check_optional_sum() {
	assert absent() == none
	assert explicit_absent() == none
	mut empty := []Res{}
	empty_bucket := keep(mut empty) or { panic("missing empty payload") }
	if empty_bucket is []Res { assert empty_bucket.len == 0 } else { assert false }
	drop_owned(empty_bucket)
	mut values := [Res{1}]
	bucket := keep(mut values) or { panic("missing owned payload") }
	if bucket is []Res {
		assert bucket[0].id == 101
		assert values[0].id == 1
	} else { assert false }
	drop_owned(bucket)
	drop_owned(values)
}

type Storage = []Res | string
fn keep_string(mut source Storage) Storage { return source }
fn check_string_sum() {
	mut source := Storage("payload".repeat(4))
	retained := keep_string(mut source)
	drop_owned(source)
	if retained is string {
		assert retained == "payload".repeat(4)
	} else { assert false }
	drop_owned(retained)
	println("ok")
}

fn main() {
	println("optional_sum")
	check_optional_sum()
	println("string_sum")
	check_string_sum()
}
')
	assert output == 'optional_sum\ndrop 101\ndrop 1\nstring_sum\nok'
}

fn test_nested_optional_sum_cast_is_rejected() {
	nested := borrow_storage_compile('nested_optional_sum_cast', '
type Bucket = []int | int
fn absent() ?Bucket { return ?Bucket(?Bucket(none)) }
fn main() { _ = absent() }
')
	assert nested.exit_code != 0
	assert nested.output.contains('cannot cast `?Bucket` to `?Bucket`')
}

fn test_mutable_wrapped_array_storage_preserves_payloads() {
	wrappers := {
		'option':       '?[]Res'
		'sum':          'Storage'
		'optional_sum': '?Storage'
		'wrapped_sum':  'WrappedStorage'
	}
	mut source := borrow_storage_drop_decls + '
type Storage = []Res | int
type WrappedStorage = ?[]Res | int
'
	mut calls := []string{}
	mut expected := []string{}
	for name, wrapper in wrappers {
		keep := 'keep_${name}'
		payload := if wrapper == '?[]Res' {
			'[Res{1}]'
		} else if wrapper == 'WrappedStorage' {
			'WrappedStorage(?[]Res([Res{1}]))'
		} else {
			'Storage([Res{1}])'
		}
		initial := if wrapper.starts_with('?') { '${wrapper}(${payload})' } else { payload }
		unwrapped := if wrapper.starts_with('?') {
			'${keep}(mut source) or { panic("missing payload") }'
		} else {
			'${keep}(mut source)'
		}
		check := if wrapper == '?[]Res' {
			'assert retained[0].id == 101'
		} else if wrapper == 'WrappedStorage' {
			'assert retained is ?[]Res'
		} else {
			'if retained is []Res { assert retained[0].id == 101 } else { assert false }'
		}
		source += '
fn ${keep}(mut source ${wrapper}) ${wrapper} { return source }
fn check_${name}() {
	mut source := ${initial}
	retained := ${unwrapped}
	drop_owned(source)
	${check}
	drop_owned(retained)
}
'
		calls << '\tprintln("${name}")\n\tcheck_${name}()'
		expected << '${name}\ndrop 1\ndrop 101'
	}
	source += '
fn check_inactive() {
	mut absent := ?[]Res(none)
	assert keep_option(mut absent) == none
	mut scalar := Storage(7)
	retained := keep_sum(mut scalar)
	drop_owned(scalar)
	assert retained == Storage(7)
	drop_owned(retained)
	mut absent_sum := ?Storage(none)
	assert keep_optional_sum(mut absent_sum) == none
	mut scalar_sum := ?Storage(Storage(9))
	kept := keep_optional_sum(mut scalar_sum) or { panic("missing scalar") }
	assert kept == Storage(9)
	mut absent_variant := WrappedStorage(?[]Res(none))
	kept_variant := keep_wrapped_sum(mut absent_variant)
	assert kept_variant is ?[]Res
	drop_owned(absent_variant)
	drop_owned(kept_variant)
	println("ok")
}

fn main() {
${calls.join('\n')}
	println("inactive")
	check_inactive()
}
'
	expected << 'inactive\nok'
	output := borrow_storage_run('wrapped_mut_array', source)
	assert output == expected.join('\n')
}

fn test_generic_mutable_optional_array_storage_acquires_each_specialization() {
	output := borrow_storage_run('generic_optional_mut_warmed', borrow_storage_drop_decls + '
struct OtherRes implements IClone, Drop { id int }
fn (r &OtherRes) clone() OtherRes { return OtherRes{r.id + 1000} }
fn (mut r OtherRes) drop() { println("other \${r.id}") }
fn warm(mut source ?[]Res) ?[]Res { return source }
fn keep[T](mut source ?[]T) ?[]T { return source }
fn main() {
	mut initial := ?[]Res([Res{1}])
	warmed := warm(mut initial) or { panic("missing warm") }
	drop_owned(initial)
	assert warmed[0].id == 101
	drop_owned(warmed)
	mut source := ?[]Res([Res{2}])
	retained := keep[Res](mut source) or { panic("missing first") }
	drop_owned(source)
	assert retained[0].id == 102
	drop_owned(retained)
	mut other := ?[]OtherRes([OtherRes{3}])
	other_retained := keep[OtherRes](mut other) or { panic("missing second") }
	drop_owned(other)
	assert other_retained[0].id == 1003
	drop_owned(other_retained)
}
')
	assert output == 'drop 1\ndrop 101\ndrop 2\ndrop 102\nother 3\nother 1003'
}

fn test_mutable_array_value_captures_acquire_owned_snapshots() {
	output := borrow_storage_run('array_capture_storage', '@[has_globals]
module main
import builtin.closure
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
	mut lifetime := closure.new_lifetime()
	lifetime.frame(fn () {
		for borrowed in [false, true] { check_capture(borrowed) }
		check_pointer_capture()
		check_primitive_capture()
	}) or { panic(err) }
	lifetime.dispose() or { panic(err) }
	assert clones == 2
	assert dropped.len == next_id
	println("ok")
}
')
	assert output == 'ok'
}

fn test_uncloneable_mut_array_bulk_copies_fail_before_duplicating_owners() {
	operations := {
		'append':  'values << values'
		'prepend': 'values.prepend(values)'
		'insert':  'values.insert(0, values)'
	}
	declarations := 'interface Drop { mut: drop() }
struct Handle implements Drop { id int }
fn (mut h Handle) drop() {}
'
	mut append_source := declarations
	mut rejected_source := declarations
	mut append_branches := []string{}
	mut rejected_calls := []string{}
	mut rejected_lines := []int{}
	for action, operation in operations {
		for borrowed in [false, true] {
			name := '${action}_${borrowed}'
			source := '
fn duplicate_${name}(mut values []Handle) {
	${operation}
}
fn check_${name}() {
	mut values := [Handle{1}]
	if ${borrowed} { duplicate_${name}(mut values[..]) } else { duplicate_${name}(mut values) }
}

'
			if action == 'append' {
				append_source += source
				append_branches << '		"${name}" { check_${name}() }'
			} else {
				// The checker already rejects receiver-aliased prepend/insert copies.
				// The operation follows the blank line and function signature.
				rejected_lines << rejected_source.count('\n') + 3
				rejected_source += source
				rejected_calls << '	check_${name}()'
			}
		}
	}
	clone_error := 'requires ownership destruction but has no compatible `clone()` method'
	rejected_source += 'fn main() {
${rejected_calls.join('\n')}
}
'
	build := borrow_storage_compile_with_flags('uncloneable_bulk_rejected', rejected_source, '-nocolor')
	assert build.exit_code == 1, build.output
	errors := build.output.split_into_lines().filter(it.contains(': error: ')).map(it.all_after('.v:'))
	assert errors.map(it.all_before(':').int()) == rejected_lines, build.output
	for diagnostic in errors {
		assert diagnostic.contains('cannot copy receiver-aliased `[]Handle` value'), build.output
		assert diagnostic.contains(clone_error), build.output
	}

	binary := borrow_storage_compile_dispatch('uncloneable_bulk_append', append_source, append_branches)
	for borrowed in [false, true] {
		name := 'append_${borrowed}'
		run := os.exec([binary, name])
		assert run.exit_code != 0, '${name}: ${run.output}'
		assert run.output.contains(clone_error), '${name}: ${run.output}'
	}
}

fn test_empty_owned_array_deletion_preserves_invalid_index_diagnostics() {
	indices := {
		'negative': -1
		'past_end': 2
	}
	mut source := 'interface Drop { mut: drop() }
struct Handle implements Drop { id int }
fn (mut h Handle) drop() {}
'
	mut branches := []string{cap: indices.len}
	for name, index in indices {
		source += 'fn delete_empty_range_${name}(mut values []Handle) { values.delete_many(${index}, 0) }
'
		branches << '\t\t"${name}" {
			mut values := [Handle{1}]
			delete_empty_range_${name}(mut values[..])
		}'
	}
	binary := borrow_storage_compile_dispatch('empty_delete_invalid', source, branches)
	for name, index in indices {
		run := os.exec([binary, name])
		assert run.exit_code != 0, '${index}: ${run.output}'
		assert run.output.contains('array.delete: index out of range (i,a.len):${index}, 1'), '${index}: ${run.output}'
	}
}

fn test_owned_sum_box_and_array_trim_cleanup_match_gc_allocators() {
	source := '@[has_globals]
module main
' + borrow_storage_drop_decls + '
__global calls = 0
__global drops = 0
struct Handle implements Drop { id int }
fn (mut h Handle) drop() { drops++ }
fn trim_index(index int) int { calls++; return index }
fn check_trim() {
	for index in [-1, 2, 3, 1, 0] {
		calls = 0
		drops = 0
		mut values := [Handle{1}, Handle{2}]
		previous_len := values.len
		if calls < 0 { values[0].drop() }
		values.trim(trim_index(index))
		assert calls == 1
		assert drops == if index >= 0 && index < previous_len { previous_len - index } else { 0 }
		assert values.len == if index < previous_len { index } else { previous_len }
		if index < 0 {
			// Preserve the raw trim header behavior, then restore it for complete cleanup.
			unsafe { values.len = previous_len }
		}
		drop_owned(values)
		assert drops == previous_len
	}
	println("ok")
}

type Bucket = Res | int

fn main() {
	case_name := arguments()[1]
	if case_name == "trim" {
		check_trim()
		return
	}
	assert case_name == "sum_box"
	bucket := Bucket(Res{1})
	drop_owned(bucket)
	mut direct := Res{2}
	if direct.id == 0 { direct.drop() }
}
'
	outputs := {
		'sum_box': 'drop 1\ndrop 2'
		'trim':    'ok'
	}
	for i, flags in ['', '-gc none'] {
		name := 'owned_cleanup_allocator_${i}'
		build := borrow_storage_compile_with_flags(name, source, flags)
		assert build.exit_code == 0, '${name}: ${build.output}'
		for case_name, expected in outputs {
			run := os.exec([os.join_path(borrow_storage_tmp_dir, name), case_name])
			assert run.exit_code == 0, '${name} ${case_name}: ${run.output}'
			assert run.output.trim_space() == expected, '${name} ${case_name}: ${run.output}'
		}
	}
}

fn test_owned_array_trim_drops_only_a_valid_range() {
	source := borrow_storage_drop_decls + '
fn trim_at(mut values []Res, index int) {
	values.trim(index)
	println("ok")
	exit(0)
}
'
	indices := [-1, 0, 1, 2]
	mut branches := []string{cap: indices.len}
	for index in indices {
		branches << '\t\t"${index}" {
			mut values := [Res{1}]
			trim_at(mut values, ${index})
		}'
	}
	binary := borrow_storage_compile_dispatch('trim_drop_range', source, branches)
	for index in indices {
		run := os.exec([binary, index.str()])
		expected := if index == 0 { 'drop 1\nok' } else { 'ok' }
		assert run.exit_code == 0, '${index}: ${run.output}'
		assert run.output.trim_space() == expected, '${index}: ${run.output}'
	}
}

fn test_result_storage_aliases_require_immediate_unwrapping() {
	mut source := borrow_storage_drop_decls + '
struct Fault implements IClone, Drop { id int }
fn (r Fault) msg() string { return "failed" }
fn (r Fault) code() int { return r.id }
fn (r &Fault) clone() Fault { return Fault{r.id + 100} }
fn (mut r Fault) drop() { println("error \${r.id}") }
'
	mut alias_lines := {
		source.count('\n') + 1: 'ArrayResult'
	}
	source += 'type ArrayResult = ![]Res
type ArrayResultAlias = ArrayResult
type ResultStorage = ArrayResultAlias | int
fn fail(id int) ![]Res { return Fault{id} }
fn keep_alias(mut source ArrayResultAlias) ArrayResultAlias { return source }
fn keep_sum(mut source ResultStorage) ResultStorage { return source }
fn check_array_result() {
	mut source := ArrayResultAlias(fail(1))
	retained := ArrayResultAlias(keep_alias(mut source))
	drop_owned(source)
	drop_owned(retained)
	mut sum_source := ResultStorage(ArrayResultAlias(fail(2)))
	sum_retained := keep_sum(mut sum_source)
	drop_owned(sum_source)
	drop_owned(sum_retained)
}
'
	mut fail_lines := []int{}
	for i, line in source.split_into_lines() {
		if line.contains('ArrayResultAlias(fail(') {
			fail_lines << i + 1
		}
	}
	assert fail_lines.len == 2

	mut calls := []string{}
	for clone_kind in ['none', 'incompatible', 'value', 'pointer', 'pointer_alias'] {
		for failed in [false, true] {
			case_id := calls.len
			fault := 'Fault${case_id}'
			clone_method := match clone_kind {
				'incompatible' { 'fn (r &${fault}) clone() int { return r.id }' }
				'value' { 'fn (r &${fault}) clone() ${fault} { return ${fault}{r.id + 100} }' }
				'pointer' { 'fn (r &${fault}) clone() &${fault} { return &${fault}{r.id + 100} }' }
				'pointer_alias' {
					'type FaultRef${case_id} = &${fault}
type FaultRefAlias${case_id} = FaultRef${case_id}
fn (r &${fault}) clone() FaultRefAlias${case_id} { return &${fault}{r.id + 100} }'
				}
				else { '' }
			}
			source += '
struct ${fault} implements Drop { id int }
fn (r ${fault}) msg() string { return "failed" }
fn (r ${fault}) code() int { return r.id }
fn (mut r ${fault}) drop() { println("error \${r.id}") }
${clone_method}
'
			alias_lines[source.count('\n') + 1] = '${clone_kind}_${failed}'
			source += 'type StringResult${case_id} = !string
type StringResultAlias${case_id} = StringResult${case_id}
type Storage${case_id} = []Res | StringResultAlias${case_id}
fn make_result_${case_id}(failed bool) !string {
	if failed { return ${fault}{1} }
	return "payload".repeat(4)
}
fn keep_${case_id}(mut source Storage${case_id}) Storage${case_id} { return source }
fn check_${case_id}() {
	mut source := Storage${case_id}(StringResultAlias${case_id}(make_result_${case_id}(${failed})))
	retained := keep_${case_id}(mut source)
	drop_owned(source)
	drop_owned(retained)
	println("ok")
}
'
			calls << '\tcheck_${case_id}()'
		}
	}
	source += '
fn main() {
	check_array_result()
${calls.join('\n')}
}
'
	name := 'result_storage_aliases'
	build := borrow_storage_compile_with_flags(name, source, '-nocolor -message-limit 100')
	assert build.exit_code != 0, build.output
	mut rejected_lines := []int{}
	mut rejected_fail_lines := []int{}
	for diagnostic in build.output.split('${name}.v:') {
		line := diagnostic.all_before(':').int()
		if diagnostic.contains(': error: cannot make an alias of Result type')
			&& diagnostic.contains('Details: Result types cannot be stored and have to be unwrapped immediately') {
			rejected_lines << line
		}
		if diagnostic.contains(': error: fail() returns `![]Res`') {
			rejected_fail_lines << line
		}
	}
	for line, case_name in alias_lines {
		assert line in rejected_lines, '${case_name}: ${build.output}'
	}
	assert rejected_fail_lines.sorted() == fail_lines, build.output
}

fn test_invalid_owned_array_growth_preserves_diagnostics_without_cloning() {
	mut source := 'interface Drop { mut: drop() }
struct Handle implements Drop { id int }
fn (mut h Handle) drop() {}

struct CloneableHandle implements Drop { id int }
fn (mut h CloneableHandle) drop() {}
fn (h &CloneableHandle) clone() CloneableHandle { println("unexpected clone"); return CloneableHandle{h.id + 100} }
'
	mut branches := []string{}
	mut diagnostics := map[string]string{}
	for method in ['ensure_cap', 'grow_cap', 'grow_len'] {
		for overflow in [false, true] {
			if overflow && method == 'ensure_cap' {
				continue
			}
			amount := if overflow {
				'max_int'
			} else if method == 'ensure_cap' {
				'2'
			} else {
				'1'
			}
			diagnostic := if overflow {
				'array.${method}: max_int will be exceeded'
			} else {
				'flag `.nogrow` cannot grow in size'
			}
			for cloneable in [false, true] {
				name := '${method}_${overflow}_${cloneable}'
				element := if cloneable { 'CloneableHandle' } else { 'Handle' }
				source += '
fn grow_${name}(mut values []${element}, amount int) {
	if !${overflow} { unsafe { values.flags.set(.nogrow) } }
	unsafe { values.${method}(amount) }
}

'
				branches << '		"${name}" {
			mut values := [${element}{1}]
			grow_${name}(mut values[..], ${amount})
		}'
				diagnostics[name] = diagnostic
			}
		}
	}
	binary := borrow_storage_compile_dispatch('invalid_growth', source, branches)
	for name, diagnostic in diagnostics {
		run := os.exec([binary, name])
		assert run.exit_code != 0, '${name}: ${run.output}'
		assert !run.output.contains('unexpected clone'), '${name}: ${run.output}'
		assert run.output.contains(diagnostic), '${name}: ${run.output}'
	}
}
