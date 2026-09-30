import os

const copy_ownership_vexe = @VEXE
const copy_ownership_vroot = os.dir(os.dir(os.dir(os.dir(@FILE))))
const copy_ownership_tmp_dir = os.join_path(os.vtmp_dir(), 'copy_ownership_${os.getpid()}')
const copy_ownership_v3 = os.join_path(copy_ownership_tmp_dir, 'v3_ownership')

const copy_ownership_drop_decls = 'interface Drop {
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
	os.mkdir_all(copy_ownership_tmp_dir) or { panic(err) }
	cmd_v := os.join_path(copy_ownership_vroot, 'cmd', 'v')
	vlib := os.join_path(copy_ownership_vroot, 'vlib')
	build := os.execute('${os.quoted_path(copy_ownership_vexe)} -gc none -d ownership -path "${vlib}|@vlib|@vmodules" -o ${os.quoted_path(copy_ownership_v3)} ${os.quoted_path(cmd_v)}')
	assert build.exit_code == 0, build.output
}

fn testsuite_end() {
	os.rmdir_all(copy_ownership_tmp_dir) or {}
}

fn copy_ownership_compile(name string, src string) os.Result {
	path := os.join_path(copy_ownership_tmp_dir, '${name}.v')
	os.write_file(path, src) or { panic(err) }
	out := os.join_path(copy_ownership_tmp_dir, name)
	return os.execute('${os.quoted_path(copy_ownership_v3)} -ownership -d ownership -no-parallel -o ${os.quoted_path(out)} ${os.quoted_path(path)}')
}

fn copy_ownership_run(name string, src string) string {
	build := copy_ownership_compile(name, src)
	assert build.exit_code == 0, build.output
	run := os.execute(os.quoted_path(os.join_path(copy_ownership_tmp_dir, name)))
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_copy_clones_owned_elements_and_drops_replaced_ones() {
	output := copy_ownership_run('drops', copy_ownership_drop_decls + '
fn make() []Res {
	return [Res{7}, Res{8}]
}

fn main() {
	{
		mut dst := [Res{1}, Res{2}]
		src := [Res{10}, Res{20}, Res{30}]
		println("n=\${copy(mut dst, src)}")
		println("src \${src[2].id}, dst \${dst[0].id} \${dst[1].id}")
	}
	println("--- temporary")
	{
		mut dst := [Res{3}]
		println("n=\${copy(mut dst, make())}")
		println("dst \${dst[0].id}")
	}
	println("end")
}
')
	assert output.split_into_lines() == [
		'drop 1',
		'drop 2',
		'n=2',
		'src 30, dst 110 120',
		'drop 10',
		'drop 20',
		'drop 30',
		'drop 110',
		'drop 120',
		'--- temporary',
		'drop 3',
		'drop 7',
		'drop 8',
		'n=1',
		'dst 107',
		'drop 107',
		'end',
	]
}

fn test_copy_clones_strings_and_nested_arrays() {
	output := copy_ownership_run('strings', '
fn words() []string {
	return ["t".repeat(2), "u".repeat(2)]
}

fn main() {
	mut dst := ["a".repeat(3), "b".repeat(3)]
	src := ["x".repeat(3), "y".repeat(3), "z".repeat(3)]
	assert copy(mut dst, src) == 2
	assert dst == ["xxx", "yyy"]
	assert src == ["xxx", "yyy", "zzz"]
	assert copy(mut dst, words()) == 2
	assert dst == ["tt", "uu"]
	assert copy(mut dst[1..], src[2..]) == 1
	assert dst == ["tt", "zzz"]
	mut fixed := ["p".repeat(2), "q".repeat(2), "r".repeat(2)]!
	assert copy(mut fixed[1..], src) == 2
	assert fixed == ["pp", "xxx", "yyy"]!
	mut nested := [[1, 2], [3]]
	inner := [[7, 8, 9]]
	assert copy(mut nested, inner) == 1
	assert nested == [[7, 8, 9], [3]]
	assert inner == [[7, 8, 9]]
	println("ok")
}
')
	assert output == 'ok'
}

fn test_copy_rejects_owned_elements_without_clone() {
	build := copy_ownership_compile('uncloneable', 'interface Drop {
mut:
	drop()
}

struct Handle implements Drop {
	fd int
}

fn (mut h Handle) drop() {}

fn main() {
	mut dst := [Handle{1}]
	src := [Handle{2}]
	copy(mut dst, src)
}
')
	assert build.exit_code != 0
	assert build.output.contains('cannot copy `Handle` elements: `Handle` requires ownership destruction but has no compatible `clone()` method'), build.output
}

fn test_owned_fixed_array_returned_views_acquire_independent_owners() {
	output := copy_ownership_run('fixed_array_owners', '@[has_globals]
module main

__global next_owned_id = 0
__global dropped_ids = map[int]bool{}

interface Drop {
mut:
	drop()
}

struct Tracked implements IClone, Drop {
	id int
}

fn fresh() Tracked {
	next_owned_id++
	return Tracked{next_owned_id}
}

fn (r &Tracked) clone() Tracked {
	return fresh()
}

fn (mut r Tracked) drop() {
	assert !dropped_ids[r.id], "owner dropped twice"
	dropped_ids[r.id] = true
}

fn keep(mut values []Tracked) []Tracked {
	return values
}

fn keep_strings(mut values []string) []string {
	return values
}

struct Wrapper {
	items []Tracked
}

fn keep_wrappers(mut values []Wrapper) []Wrapper {
	return values
}

fn main() {
	mut fixed := [fresh()]!
	kept := keep(mut fixed)
	assert kept[0].id != fixed[0].id
	drop_owned(kept)
	assert dropped_ids.len == 1
	assert !dropped_ids[fixed[0].id]
	drop_owned(fixed)
	mut wrappers := [Wrapper{[fresh()]}]!
	kept_wrappers := keep_wrappers(mut wrappers)
	drop_owned(kept_wrappers)
	assert dropped_ids.len == 3
	assert !dropped_ids[wrappers[0].items[0].id]
	drop_owned(wrappers)
	assert dropped_ids.len == next_owned_id
	mut words := ["first".repeat(3), "second".repeat(3)]!
	kept_words := keep_strings(mut words)
	assert kept_words[0] == "firstfirstfirst"
	assert kept_words[1] == "secondsecondsecond"
	drop_owned(kept_words)
	assert words[0] == "firstfirstfirst"
	drop_owned(words)
	println("ok")
}
')
	assert output == 'ok'
}

fn test_mut_fixed_array_views_borrow_elements_without_clone() {
	for argument in ['values', 'values[0..1]'] {
		output := copy_ownership_run('uncloneable_fixed_${argument.len}', '@[has_globals]
module main

__global dropped = false

interface Drop {
mut:
	drop()
}

struct Handle implements Drop {
mut:
	fd int
}

fn (mut h Handle) drop() {
	assert !dropped
	dropped = true
}

fn change(mut values []Handle) {
	values[0].fd = 2
}

fn inspect(values &[]Handle) int {
	return values[0].fd
}

fn main() {
	mut values := [Handle{1}]!
	assert inspect(values) == 1
	change(mut ${argument})
	assert values[0].fd == 2
	assert !dropped
	drop_owned(values)
	assert dropped
	println("ok")
}
')
		assert output == 'ok'
	}
}

fn test_nonownership_fixed_array_views_do_not_root_unused_destructors() {
	path := os.join_path(copy_ownership_tmp_dir, 'unused_fixed_drop.c.v')
	os.write_file(path, 'fn C.unreachable_fixed_array_drop()

struct Storage {
	values []int
}

fn (mut s Storage) drop() {
	C.unreachable_fixed_array_drop()
}

fn keep(mut values []Storage) []Storage {
	return values
}

fn main() {
	mut values := [Storage{[1]}]!
	kept := keep(mut values)
	assert kept[0].values == [1]
	println("ok")
}
') or { panic(err) }
	output := os.execute('${os.quoted_path(copy_ownership_vexe)} run ${os.quoted_path(path)}')
	assert output.exit_code == 0, output.output
	assert output.output.trim_space() == 'ok'
}

fn test_immutable_fixed_array_reference_keeps_original_owners() {
	output := copy_ownership_run('immutable_fixed_reference', '@[has_globals]
module main

__global next_id = 0
__global cloned = 0
__global dropped = map[int]bool{}

interface Drop {
mut:
	drop()
}

struct Resource implements IClone, Drop {
	id int
}

fn fresh() Resource {
	next_id++
	return Resource{next_id}
}

fn (r &Resource) clone() Resource {
	cloned++
	return fresh()
}

fn (mut r Resource) drop() {
	assert !dropped[r.id]
	dropped[r.id] = true
}

fn inspect(values &[]Resource) int {
	return values[0].id
}

fn main() {
	values := [fresh()]!
	original_id := values[0].id
	assert inspect(values) == original_id
	assert cloned == 0
	assert values[0].id == original_id
	assert !dropped[original_id]
	drop_owned(values)
	assert dropped[original_id]
	println("ok")
}
')
	assert output == 'ok'
}

fn test_fixed_array_borrows_preserve_callee_evaluation_order() {
	output := copy_ownership_run('fixed_callee_order', '@[has_globals]
module main

__global order = []string{}

struct Item implements IClone {
	id int
}

fn (r &Item) clone() Item {
	order << "clone"
	return Item{r.id}
}

struct Runner {
mut:
	calls int
}

fn make_runner() Runner {
	order << "receiver"
	return Runner{}
}

fn (r Runner) consume(mut values []Item) int {
	order << "call"
	return values[0].id
}

fn (mut r Runner) consume_mut(mut values []Item) int {
	r.calls++
	order << "call"
	return values[0].id
}

fn next_runner() int {
	order << "index"
	return 0
}

fn consume_values(mut values []Item) int {
	order << "call"
	return values[0].id
}

fn consume_reference(values &[]Item) int {
	order << "call"
	return values[0].id
}

fn make_consumer() fn (&[]Item) int {
	order << "factory"
	return consume_reference
}

struct Holder {
	callback fn (mut []Item) int @[required]
}

fn get_holder() Holder {
	order << "getter"
	return Holder{consume_values}
}

fn main() {
	mut values := [Item{1}]!
	assert make_runner().consume(mut values) == 1
	assert order[0] == "receiver", order.str()
	assert order.filter(it == "receiver").len == 1
	assert order.filter(it == "clone").len == 0
	order = []string{}
	assert make_consumer()(values) == 1
	assert order[0] == "factory", order.str()
	assert order.filter(it == "factory").len == 1
	order = []string{}
	assert get_holder().callback(mut values) == 1
	assert order[0] == "getter", order.str()
	assert order.filter(it == "getter").len == 1
	order = []string{}
	mut runners := [Runner{}, Runner{}]!
	assert runners[next_runner()].consume_mut(mut values) == 1
	assert runners[0].calls == 1
	assert order[0] == "index", order.str()
	assert order.filter(it == "index").len == 1
	println("ok")
}
')
	assert output == 'ok'
}

fn test_owned_fixed_array_views_clone_only_when_detaching() {
	output := copy_ownership_run('fixed_array_detach', '@[has_globals]
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
		mut fixed := [fresh(), fresh()]!
		original_id := fixed[0].id
		before := clone_count
		noops(mut fixed)
		assert clone_count == before
		changed := change(mut fixed, operation)
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

fn test_owned_fixed_array_borrowed_empty_uncloneable_range_can_grow() {
	output := copy_ownership_run('empty_uncloneable_fixed', '@[has_globals]
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
		mut fixed := [Handle{1}]!
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

fn test_owned_fixed_array_borrow_scope_and_retained_headers() {
	output := copy_ownership_run('fixed_array_scope', '@[has_globals]
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
	values [1]Resource
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
	mut holder := Holder{[fresh()]!, "label".repeat(4)}
	assert inspect(holder.values) == holder.values[0].id
	change(mut holder.values)
	assert holder.values[0].value == 7
	assert clones == 0
}
fn returned() []Resource {
	mut holder := Holder{[fresh()]!, "label".repeat(4)}
	return keep(mut holder.values)
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
struct ScalarHolder {
mut:
	values [2]int
	label string
}
fn keep_scalars(mut values []int) []int { return values }
fn scalar_scope() []int {
	mut holder := ScalarHolder{[3, 4]!, "label".repeat(4)}
	return keep_scalars(mut holder.values)
}
fn returned_reference() &[]Resource {
	mut holder := Holder{[fresh()]!, "label".repeat(4)}
	return keep_reference(mut holder.values)
}
fn stored_reference() Stored {
	mut holder := Holder{[fresh()]!, "label".repeat(4)}
	mut stored := Stored{}
	store_and_change(mut holder.values, mut stored)
	assert holder.values[0].value == 9
	return stored
}
fn stored_reference_alias(initializer bool) StoredRef {
	mut holder := Holder{[fresh()]!, "label".repeat(4)}
	mut stored := StoredRef{}
	store_alias_and_change(mut holder.values, mut stored, initializer)
	assert holder.values[0].value == 12
	return stored
}
fn main() {
	ordinary_scope()
	assert dropped.len == 1
	assert scalar_scope() == [3, 4]
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
	assert dropped.len == next_id
	println("ok")
}
')
	assert output == 'ok'
}

fn test_owned_fixed_array_nonempty_uncloneable_views_fail_when_owning() {
	for operation in ['values << Handle{2}', 'values.ensure_cap(values.cap + 1)', 'return values'] {
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
	mut fixed := [Handle{1}]!
	result := own(mut fixed)
	assert result.len == 0
}
'
		build := copy_ownership_compile(name, source)
		assert build.exit_code == 0, build.output
		run := os.execute(os.quoted_path(os.join_path(copy_ownership_tmp_dir, name)))
		assert run.exit_code != 0, run.output
		assert run.output.contains('requires ownership destruction but has no compatible `clone()` method'), run.output
	}
}

fn test_promoted_fixed_array_roots_use_matching_windows_allocators() {
	path := os.join_path(copy_ownership_tmp_dir, 'fixed_root_windows.v')
	cpath := os.join_path(copy_ownership_tmp_dir, 'fixed_root_windows.c')
	os.write_file(path, 'interface Drop { mut: drop() }
struct PlainCell implements Drop { value int }
fn (mut cell PlainCell) drop() {}
struct PlainHolder { values [2]PlainCell label string }
@[aligned: 64]
struct AlignedCell implements Drop { value int }
fn (mut cell AlignedCell) drop() {}
struct AlignedHolder { values [2]AlignedCell label string }
struct NestedHolder { inner AlignedHolder }
fn inspect(values &[]PlainCell) int { return values[0].value }
fn inspect_aligned(values &[]AlignedCell) int { return values[0].value }
fn grow(mut values []PlainCell) {
	values.ensure_cap(1)
	values.grow_cap(1)
	unsafe { values.grow_len(1) }
}
fn make_plain() PlainHolder {
	return PlainHolder{[PlainCell{1}, PlainCell{2}]!, "label".repeat(4)}
}
fn make_aligned() AlignedHolder {
	return AlignedHolder{[AlignedCell{1}, AlignedCell{2}]!, "label".repeat(4)}
}
fn plain_scope() {
	holder := make_plain()
	assert inspect(holder.values) == 1
}
fn aligned_call_scope() {
	holder := make_aligned()
	assert inspect_aligned(holder.values) == 1
}
fn aligned_literal_scope() {
	holder := AlignedHolder{[AlignedCell{1}, AlignedCell{2}]!, "label".repeat(4)}
	assert inspect_aligned(holder.values) == 1
}
fn nested_literal_scope() {
	holder := NestedHolder{make_aligned()}
	assert inspect_aligned(holder.inner.values) == 1
}
fn main() {
	mut empty := []PlainCell{}
	grow(mut empty)
	plain_scope()
	aligned_call_scope()
	aligned_literal_scope()
	nested_literal_scope()
}
')!
	build := os.execute('${os.quoted_path(copy_ownership_v3)} -ownership -d ownership -no-parallel -os windows -o ${os.quoted_path(cpath)} ${os.quoted_path(path)}')
	assert build.exit_code == 0, build.output
	generated := os.read_file(cpath)!
	growth := generated.all_after('void grow(Array* values) {').all_before('\n}')
	for method in ['ensure_cap', 'grow_cap', 'grow_len'] {
		assert growth.contains('array__${method}('), growth
		assert !growth.contains('strings__Builder__${method}('), growth
	}
	plain := generated.all_after('void plain_scope(void) {').all_before('\n}')
	assert plain.contains('memdup('), plain
	assert !plain.contains('v3_aligned_memdup('), plain
	assert plain.contains('v_free(holder);'), plain
	for name in ['aligned_call_scope', 'aligned_literal_scope', 'nested_literal_scope'] {
		body := generated.all_after('void ${name}(void) {').all_before('\n}')
		assert body.contains('v3_aligned_memdup('), body
		assert body.contains('v3_aligned_free(holder);'), body
		assert !body.contains('v_free(holder);'), body
	}
}
