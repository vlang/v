import os

const occf_vexe = @VEXE
const occf_tests_dir = os.dir(@FILE)
const occf_v3_dir = os.dir(occf_tests_dir)
const occf_vlib_dir = os.dir(occf_v3_dir)
const occf_v3_src = os.join_path(occf_v3_dir, 'v3.v')

fn occf_build_v3(tag string) string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_${tag}_${pid}')
	build :=
		os.execute('${occf_vexe} -gc none -d ownership -path "${occf_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${occf_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn occf_compile_and_run(v3_bin string, tag string, source string) {
	pid := os.getpid()
	src := os.join_path(os.temp_dir(), 'v3_${tag}_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_${tag}_prog_${pid}')
	defer {
		os.rm(src) or {}
		os.rm(out) or {}
		os.rm(out + '.c') or {}
	}
	os.write_file(src, source) or { panic(err) }
	compile := os.execute('${v3_bin} ${src} -d ownership -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
}

// Reassigning an `?string` field via the drop-before-assign lowering must keep
// the optional storage type; the temp used to hold the (auto-wrapped) value must
// be `Optional_string`, not the unwrapped `string` smartcast.
fn test_optional_drop_before_assign_keeps_optional_temp() {
	v3_bin := occf_build_v3('opt_drop_assign')
	defer {
		os.rm(v3_bin) or {}
	}
	occf_compile_and_run(v3_bin, 'opt_drop_assign', 'module main

fn f(input ?string) ?string {
	mut preprocessor := ?string(none)
	if value := input {
		preprocessor = value.clone()
	}
	return preprocessor
}

fn main() {
	assert (f("hello") or { "none" }) == "hello"
}
')
}

// The final synthetic store in drop-before-assign lowering may be visited by
// transform again. It must not destroy the same old field value a second time.
fn test_owned_field_assignment_drops_replaced_value_once() {
	v3_bin := occf_build_v3('field_assign_one_drop')
	defer {
		os.rm(v3_bin) or {}
	}
	occf_compile_and_run(v3_bin, 'field_assign_one_drop', 'module main

struct Owned implements Drop {
	drops &int
}

fn (mut value Owned) drop() {
	unsafe {
		*value.drops += 1
	}
}

struct Holder {
mut:
	value Owned
}

fn main() {
	mut drops := 0
	mut holder := Holder{
		value: Owned{
			drops: &drops
		}
	}
	holder.value = Owned{
		drops: &drops
	}
	assert drops == 1
}
')
}

// `.clone()` on an array reached through a method that already returns a pointer
// (`arc.Arc[[]T].get()` returns `&[]T`) must not take the address again.
fn test_array_clone_on_pointer_returning_receiver() {
	v3_bin := occf_build_v3('arc_get_clone')
	defer {
		os.rm(v3_bin) or {}
	}
	occf_compile_and_run(v3_bin, 'arc_get_clone', 'module main

import sync.arc

fn f() []string {
	a := arc.new(["x", "y"])
	return a.get().clone()
}

fn length(values []string) int {
	return values.len
}

fn main() {
	assert f().len == 2
	a := arc.new(["x", "y"])
	assert length(a.get()) == 2
}
')
}

// A concrete generic-interface application uses the base interface's runtime
// box while retaining its specialized method parameter types.
fn test_generic_interface_parameter_uses_base_runtime_box() {
	v3_bin := occf_build_v3('generic_interface_parameter')
	defer {
		os.rm(v3_bin) or {}
	}
	occf_compile_and_run(v3_bin, 'generic_interface_parameter', 'module main

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

fn main() {
	matcher := Matcher{}
	mut caps := Captures{}
	assert captures(matcher, mut caps)
	assert caps.count == 1
}
')
}

// `Drop` does not prevent Rust from deriving `Clone`. An ownership type that
// explicitly implements both markers can use the compiler's structural clone
// as long as each ownership-bearing field is itself cloneable.
fn test_default_clone_is_available_for_drop_type() {
	v3_bin := occf_build_v3('clone_drop_type')
	defer {
		os.rm(v3_bin) or {}
	}
	occf_compile_and_run(v3_bin, 'clone_drop_type', 'module main

struct Owned implements IClone, Drop {
mut:
	value string
}

fn (mut x Owned) drop() {
	x.value = ""
}

fn main() {
	first := Owned{
		value: "hello".to_owned()
	}
	second := first.clone()
	assert first.value == "hello"
	assert second.value == "hello"
}
')
}

// Array iteration binds elements by value. Ownership-bearing elements must be
// structurally cloned before the loop body so dropping a binding cannot free
// storage still owned by the source array.
fn test_array_for_in_clones_ownership_bearing_elements() {
	v3_bin := occf_build_v3('array_for_in_clone')
	defer {
		os.rm(v3_bin) or {}
	}
	occf_compile_and_run(v3_bin, 'array_for_in_clone', 'module main

struct Record implements IClone {
	name string
	arg  ?string
}

fn main() {
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
')
}

// Captured function literals retain their declared parameter types during checking, so
// generic builtin methods can validate them against the specialized element
// type instead of falling back to `fn (voidptr, voidptr)`.
fn test_captured_sort_compare_keeps_parameter_types() {
	v3_bin := occf_build_v3('captured_sort_compare')
	defer {
		os.rm(v3_bin) or {}
	}
	occf_compile_and_run(v3_bin, 'captured_sort_compare', 'module main

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

// Generic specialization keeps the checker's implicit reference conversion
// when a concrete value is passed to a borrowed interface parameter.
fn test_generic_method_accepts_value_for_borrowed_interface_parameter() {
	v3_bin := occf_build_v3('generic_borrowed_interface_arg')
	defer {
		os.rm(v3_bin) or {}
	}
	occf_compile_and_run(v3_bin, 'generic_borrowed_interface_arg', 'module main

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

fn main() {
	n := 42
	worker := Worker[int]{
		value: 1
		searcher: Searcher{}
	}
	assert worker.run(&n) == 42
}
')
}

// Consuming an aggregate call argument moves its base and owned descendants as
// one value instead of treating the descendant metadata as a second use.
fn test_call_consumes_owned_aggregate_and_descendants_once() {
	v3_bin := occf_build_v3('call_owned_aggregate')
	defer {
		os.rm(v3_bin) or {}
	}
	occf_compile_and_run(v3_bin, 'call_owned_aggregate', 'module main

struct Owned implements Drop {
mut:
	value string
}

fn (mut value Owned) drop() {
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

fn consume[T](first Owned, second Wrapper[T]) int {
	return first.value.len + second.value.value.len
}

fn main() {
	first := Owned{
		value: "a".to_owned()
	}
	second := Owned{
		value: "b".to_owned()
	}
	assert consume(first, wrap(second)) == 2
}
')
}

// Reannotation rebuilds local scopes after generic specialization. Mutable
// declarations must keep their mutability there so chained generic receiver
// calls are not diagnosed as calls on immutable values.
fn test_chained_generic_mut_receiver_keeps_mutable_local() {
	v3_bin := occf_build_v3('generic_mut_receiver')
	defer {
		os.rm(v3_bin) or {}
	}
	occf_compile_and_run(v3_bin, 'generic_mut_receiver', 'module main

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

fn main() {
	mut outer := Outer[int]{
		inner: Inner[int]{
			value: 42
		}
	}
	value := outer.inner_mut().get_mut()
	assert *value == 42
}
')
}

// Moving a destructible aggregate leaves its source storage uninitialized.
// A later assignment must initialize that storage directly instead of dropping
// the old bits, which now belong to the move destination. The same rule applies
// when the move destination is an optional.
fn test_moved_aggregate_reassignment_skips_stale_drop() {
	v3_bin := occf_build_v3('moved_reassign')
	defer {
		os.rm(v3_bin) or {}
	}
	occf_compile_and_run(v3_bin, 'moved_reassign', 'module main

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

fn main() {
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
')
}
