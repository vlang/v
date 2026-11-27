import os
import v.cmdexec
import v.tempname

fn occf_compile_and_run(tag string, source string) {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_ownership_clone_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_${tag}.v')
	out := os.join_path(root, 'v3_${tag}_prog')
	os.write_file(src, source) or { panic(err) }
	compile := cmdexec.run_in_merged_with_env(compiler, [src, '-d', 'ownership', '-b', 'c', '-o',
		out], '', {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	})
	assert compile.exit_code == 0, compile.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
}

fn test_ownership_clones_arc_arrays_and_owned_values() {
	occf_compile_and_run('clone_arc_arrays_and_owned_values', 'module main

import sync.arc

// Arc.get() already returns &[]T, so cloning must not take its address again.
fn f() []string {
	a := arc.new(["x", "y"])
	return a.get().clone()
}

fn length(values []string) int {
	return values.len
}

fn check_arc_array_clone() {
	assert f().len == 2
	a := arc.new(["x", "y"])
	assert length(a.get()) == 2
}

// A Drop type that implements IClone can use structural cloning for cloneable fields.
struct Owned implements IClone, Drop {
mut:
	value string
}

fn (mut x Owned) drop() {
	x.value = ""
}

fn check_drop_type_clone() {
	first := Owned{
		value: "hello".to_owned()
	}
	second := first.clone()
	assert first.value == "hello"
	assert second.value == "hello"
}

struct Record implements IClone {
	name string
	arg  ?string
}

// The loop clones each element before binding it so the array keeps its storage.
fn check_array_iteration_clone() {
	records := [Record{
		name: "after-context".to_owned()
		arg:  "5".to_owned()
	}]
	for record in records {
		assert record.name == "after-context"
		assert record.arg or { "" } == "5"
	}
	assert records[0].name == "after-context"
	assert records[0].arg or { "" } == "5"
}

fn main() {
	check_arc_array_clone()
	check_drop_type_clone()
	check_array_iteration_clone()
}
')
}

// Captured function literals retain their declared parameter types during checking, so
// generic builtin methods can validate them against the specialized element
// type instead of falling back to `fn (voidptr, voidptr)`.
fn test_captured_sort_compare_keeps_parameter_types() {
	occf_compile_and_run('captured_sort_compare', 'module main

struct Item {
	value int
}

fn main() {
	reverse := true
	mut items := [Item{value: 1}, Item{value: 2}]
	items.sort_with_compare(fn [reverse] (a &Item, b &Item) int {
		if reverse {
			return b.value - a.value
		}
		return a.value - b.value
	})
	assert items[0].value == 2
}
')
}

fn test_generic_calls_preserve_interface_arguments_and_mutable_receivers() {
	occf_compile_and_run('generic_interface_and_receiver_calls', 'module main

// A concrete generic interface uses the base runtime box with specialized parameters.
interface CaptureFinder[T] {
	captures_at(mut caps T) bool
}

struct Captures {
mut:
	count int
}

struct Matcher {}

fn (m Matcher) captures_at(mut caps Captures) bool {
	_ = m
	caps.count++
	return true
}

fn captures[T](matcher &CaptureFinder[T], mut caps T) bool {
	return matcher.captures_at(mut caps)
}

fn check_generic_interface_box() {
	matcher := Matcher{}
	mut caps := Captures{}
	assert captures(matcher, mut caps)
	assert caps.count == 1
}

interface Reader {
	value() int
}

struct Borrowed[^a] {
	value &^a int
}

fn Borrowed.new[^a](value &^a int) Borrowed[^a] {
	return Borrowed[^a]{
		value: value
	}
}

fn (borrowed Borrowed[^a]) value[^a]() int {
	return *borrowed.value
}

struct Searcher {}

fn (searcher Searcher) search[^a](reader &^a Reader) int {
	_ = searcher
	return reader.value()
}

struct Worker[T] {
	value    T
	searcher Searcher
}

fn (worker Worker[T]) run(value &int) int {
	borrowed := Borrowed.new(value)
	return worker.searcher.search(borrowed)
}

// Specialization keeps the implicit reference conversion for borrowed interfaces.
fn check_borrowed_interface_parameter() {
	n := 42
	worker := Worker[int]{
		value: 1
		searcher: Searcher{}
	}
	assert worker.run(&n) == 42
}

struct Inner[T] {
mut:
	value T
}

fn (mut inner Inner[T]) get_mut() &T {
	return &inner.value
}

struct Outer[T] {
mut:
	inner Inner[T]
}

fn (mut outer Outer[T]) inner_mut() &Inner[T] {
	return &outer.inner
}

// Rebuilding local scopes after specialization must preserve mutability.
fn check_chained_mut_receiver() {
	mut outer := Outer[int]{
		inner: Inner[int]{
			value: 42
		}
	}
	value := outer.inner_mut().get_mut()
	assert *value == 42
}

fn main() {
	check_generic_interface_box()
	check_borrowed_interface_parameter()
	check_chained_mut_receiver()
}
')
}

fn test_owned_assignments_and_aggregate_moves_preserve_lifetimes() {
	occf_compile_and_run('owned_assignments_and_aggregate_moves', 'module main

// The drop-before-assign temporary keeps ?string storage after the smartcast.
fn f(input ?string) ?string {
	mut preprocessor := ?string(none)
	if value := input {
		preprocessor = value.clone()
	}
	return preprocessor
}

fn check_optional_assignment() {
	assert (f("hello") or { "none" }) == "hello"
}

struct FieldOwned implements Drop {
	drops &int
}

fn (mut value FieldOwned) drop() {
	unsafe {
		*value.drops += 1
	}
}

struct Holder {
mut:
	value FieldOwned
}

// A second transform of the synthetic store must still drop the replaced field once.
fn check_owned_field_assignment() {
	mut drops := 0
	mut holder := Holder{
		value: FieldOwned{
			drops: &drops
		}
	}
	holder.value = FieldOwned{
		drops: &drops
	}
	assert drops == 1
}

struct CallOwned implements Drop {
mut:
	value string
}

fn (mut value CallOwned) drop() {
	value.value = ""
}

struct Wrapper[T] {
	value T
}

fn wrap[T](value T) Wrapper[T] {
	return Wrapper[T]{
		value: value
	}
}

fn consume[T](first CallOwned, second Wrapper[T]) int {
	return first.value.len + second.value.value.len
}

// A consuming call transfers the aggregate and its owned descendants together.
fn check_consuming_call() {
	first := CallOwned{
		value: "a".to_owned()
	}
	second := CallOwned{
		value: "b".to_owned()
	}
	assert consume(first, wrap(second)) == 2
}

struct Owned implements Drop {
mut:
	value string
	drops &int
}

struct BorrowedAggregate {
	tag   rune
	value string
}

struct WrappedOwned {
	value Owned
}

fn (mut value Owned) drop() {
	unsafe {
		*value.drops += 1
	}
	value.value = ""
}

// Reassigning a moved-from aggregate initializes the source without a stale drop.
fn direct_move(drops &int) {
	mut current := Owned{
		value: "old".to_owned()
		drops: drops
	}
	saved := current
	current = Owned{
		value: "new".to_owned()
		drops: drops
	}
	unsafe {
		assert *drops == 0
	}
	assert saved.value == "old"
	assert current.value == "new"
}

fn optional_move(drops &int) {
	mut current := Owned{
		value: "old".to_owned()
		drops: drops
	}
	mut previous := ?Owned(none)
	previous = ?Owned(current)
	current = Owned{
		value: "new".to_owned()
		drops: drops
	}
	unsafe {
		assert *drops == 0
	}
	if saved := previous {
		assert saved.value == "old"
	}
	assert current.value == "new"
}

fn nested_array_append_move(drops &int) {
	mut outer := [][]Owned{}
	mut inner := [Owned{
		value: "nested".to_owned()
		drops: drops
	}]
	outer << inner
	inner = []Owned{}
	unsafe {
		assert *drops == 0
	}
	assert outer[0][0].value == "nested"
}

fn parse_number(text string) !u64 {
	if text == "" {
		return error("empty")
	}
	return text.u64()
}

fn scalar_result_is_copy(text string) {
	n := parse_number(text) or { 0 }
	copy := n
	assert n == copy
}

fn borrowed_scalar_field_is_copy(value &BorrowedAggregate) string {
	return unsafe { u8(value.tag).ascii_str() }
}

fn wrap_owned(value Owned) WrappedOwned {
	return WrappedOwned{
		value: value
	}
}

fn consume_wrapped(value WrappedOwned) bool {
	return value.value.value == "nested call"
}

fn nested_call_moves_source_once(drops &int) {
	value := Owned{
		value: "nested call".to_owned()
		drops: drops
	}
	assert consume_wrapped(wrap_owned(value))
}

fn check_moved_aggregate_reassignment() {
	mut direct_drops := 0
	direct_move(&direct_drops)
	assert direct_drops == 2
	mut optional_drops := 0
	optional_move(&optional_drops)
	assert optional_drops == 2
	mut nested_drops := 0
	nested_array_append_move(&nested_drops)
	assert nested_drops == 1
	scalar_result_is_copy("17")
	borrowed := BorrowedAggregate{
		tag: `A`
		value: "owned".to_owned()
	}
	assert borrowed_scalar_field_is_copy(&borrowed) == "A"
	mut nested_call_drops := 0
	nested_call_moves_source_once(&nested_call_drops)
	assert nested_call_drops == 1
}

fn main() {
	check_optional_assignment()
	check_owned_field_assignment()
	check_consuming_call()
	check_moved_aggregate_reassignment()
}
')
}
