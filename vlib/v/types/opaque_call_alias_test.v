module types

import os
import v.parser
import v.pref

fn check_alias_source(name string, source string) TypeChecker {
	path := os.join_path(os.vtmp_dir(), 'v3_opaque_call_alias_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	return tc
}

fn test_voidptr_lookup_with_an_unsafe_boundary_does_not_borrow_its_arguments() {
	tc := check_alias_source('voidptr', 'struct Queryable { mut: on_query fn (&Queryable, usize) voidptr = unsafe { nil } }
fn (q &Queryable) query(idx usize) voidptr { return q.on_query(q, idx) }
struct View { mut: render_inc int }
fn View.from_context(ctx &Queryable) &View { return unsafe { &View(ctx.query(usize(typeof(View{}).idx))) } }
fn dev(ctx &Queryable) {
 mut view := View.from_context(ctx)
 view.render_inc++
 view.render_inc = 5
}
fn main() {}
')
	assert tc.notices.len == 0, tc.notices.str()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_scalar_argument_to_a_callback_is_not_borrowed() {
	tc := check_alias_source('scalar', 'struct Item { mut: x int }
fn by_index(n int, name string, get fn (int, string) &Item) {
 mut item := get(n, name)
 item.x = 1
}
fn main() {}
')
	assert tc.notices.len == 0, tc.notices.str()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_typed_callback_result_still_borrows_its_argument() {
	tc := check_alias_source('typed', 'struct Item { mut: x int }
fn passthrough(item &Item, get fn (&Item) &Item) {
 mut alias := get(item)
 alias.x = 1
}
fn main() {}
')
	assert tc.notices.any(it.msg == '`item` is immutable, cannot have a mutable reference to an immutable object'), tc.notices.str()
	assert tc.errors.any(it.msg == '`alias.x` aliases mutable data from an immutable value'), tc.errors.str()
}

fn test_voidptr_from_a_visible_body_still_borrows_its_argument() {
	tc := check_alias_source('visible', 'struct Item { mut: x int }
fn raw(item &Item) voidptr { return item }
fn typed(item &Item) &Item { return raw(item) }
fn main() {
 item := &Item{}
 mut alias := typed(item)
 alias.x = 1
}
')
	assert tc.errors.any(it.msg == '`alias.x` aliases mutable data from an immutable value'), tc.errors.str()
}

fn test_type_punned_voidptr_from_a_visible_body_keeps_its_source() {
	for i, expression in ['erase(source)', '&PunnedView(erase(source))'] {
		tc := check_alias_source('punned_visible_${i}', 'struct PunnedSource { mut: x int }
struct PunnedView { mut: x int }
fn erase(source &PunnedSource) voidptr { return source }
fn view(source &PunnedSource) &PunnedView { return ${expression} }
fn dev(source &PunnedSource) {
 mut alias := view(source)
 alias.x = 1
}
fn main() {}
')
		assert tc.notices.any(it.msg == '`source` is immutable, cannot have a mutable reference to an immutable object'), tc.notices.str()
		assert tc.errors.any(it.msg == '`alias.x` aliases mutable data from an immutable value'), tc.errors.str()
	}
}

fn test_type_punned_voidptr_from_a_callback_keeps_its_source() {
	tc := check_alias_source('punned_callback', 'struct PunnedSource { mut: x int }
struct PunnedView { mut: x int }
fn view(source &PunnedSource, get fn (&PunnedSource) voidptr) &PunnedView { return get(source) }
fn dev(source &PunnedSource, get fn (&PunnedSource) voidptr) {
 mut alias := view(source, get)
 alias.x = 1
}
fn main() {}
')
	assert tc.notices.any(it.msg == '`source` is immutable, cannot have a mutable reference to an immutable object'), tc.notices.str()
	assert tc.errors.any(it.msg == '`alias.x` aliases mutable data from an immutable value'), tc.errors.str()
}

fn test_visible_voidptr_lookup_of_stored_pointer_has_separate_storage() {
	tc := check_alias_source('separate_stored_pointer', 'struct View { mut: x int }
struct Container { view &View }
fn erase(view &View) voidptr { return view }
fn lookup(container &Container) &View { return erase(container.view) }
fn dev(container &Container) {
 mut view := lookup(container)
 view.x = 1
}
fn main() {}
')
	assert tc.notices.len == 0, tc.notices.str()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_visible_voidptr_lookup_of_fresh_storage_does_not_borrow_arguments() {
	tc := check_alias_source('separate_fresh_storage', 'struct View { mut: x int }
struct Container { key usize }
fn fresh(container &Container, key usize) voidptr { _ = container; _ = key; return &View{} }
fn lookup(container &Container) &View { return fresh(container, container.key) }
fn dev(container &Container) {
 mut view := lookup(container)
 view.x = 1
}
fn main() {}
')
	assert tc.notices.len == 0, tc.notices.str()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_voidptr_from_a_callback_still_borrows_its_argument() {
	tc := check_alias_source('indirect', 'struct Item { mut: x int }
fn raw(item &Item) voidptr { return item }
fn typed(item &Item, get fn (&Item) voidptr) &Item { return get(item) }
fn main() {
 item := &Item{}
 mut alias := typed(item, raw)
 alias.x = 1
}
')
	assert tc.notices.any(it.msg == '`item` is immutable, cannot have a mutable reference to an immutable object'), tc.notices.str()
	assert tc.errors.any(it.msg == '`alias.x` aliases mutable data from an immutable value'), tc.errors.str()
}

fn test_voidptr_from_a_stored_callback_still_borrows_its_argument() {
	tc := check_alias_source('stored', 'struct Item { mut: x int }
struct Getter { get fn (&Item) voidptr = unsafe { nil } }
fn typed(item &Item, getter Getter) &Item { return getter.get(item) }
fn main() {
 item := &Item{}
 getter := Getter{get: fn (item &Item) voidptr { return item }}
 mut alias := typed(item, getter)
 alias.x = 1
}
')
	assert tc.notices.any(it.msg == '`item` is immutable, cannot have a mutable reference to an immutable object'), tc.notices.str()
	assert tc.errors.any(it.msg == '`alias.x` aliases mutable data from an immutable value'), tc.errors.str()
}

fn container_lookup_source(fields string) string {
	return 'struct View { mut: render_inc int }
struct Queryable {
${fields}
 on_query fn (&Queryable, usize) voidptr = unsafe { nil }
}
fn (q &Queryable) query(idx usize) voidptr { return q.on_query(q, idx) }
fn View.from_context(ctx &Queryable) &View { return ctx.query(usize(typeof(View{}).idx)) }
fn dev(ctx &Queryable) {
 mut view := View.from_context(ctx)
 view.render_inc++
 view.render_inc = 5
}
fn main() {}
'
}

fn test_opaque_voidptr_lookup_without_unsafe_keeps_the_container_as_a_source() {
	for i, fields in ['', 'views []&View', 'views map[int]voidptr', 'view &View'] {
		tc := check_alias_source('container_elsewhere_${i}', container_lookup_source(fields))
		assert tc.notices.any(it.msg == '`ctx` is immutable, cannot have a mutable reference to an immutable object'), '${fields}: ${tc.notices}'
		assert tc.errors.any(it.msg == '`view.render_inc` aliases mutable data from an immutable value'), '${fields}: ${tc.errors}'
	}
}

fn test_voidptr_lookup_of_a_type_the_container_holds_borrows_it() {
	for i, fields in ['view View', 'View', 'views []View', 'views map[int]View', 'views [2]View',
		'inner struct { view View }', 'view ?View'] {
		tc := check_alias_source('container_holds_${i}', container_lookup_source(fields))
		assert tc.notices.any(it.msg == '`ctx` is immutable, cannot have a mutable reference to an immutable object'), '${fields}: ${tc.notices}'
		assert tc.errors.any(it.msg == '`view.render_inc` aliases mutable data from an immutable value'), '${fields}: ${tc.errors}'
	}
}

fn test_voidptr_lookup_through_a_copied_scalar_argument_does_not_borrow_it() {
	tc := check_alias_source('scalar_lookup', 'struct View { mut: render_inc int }
fn lookup(idx usize, get fn (usize) voidptr) &View { return get(idx) }
fn dev(idx usize, get fn (usize) voidptr) {
 mut view := lookup(idx, get)
 view.render_inc = 5
}
fn main() {}
')
	assert tc.notices.len == 0, tc.notices.str()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_voidptr_lookup_through_a_referenced_scalar_keeps_its_source() {
	tc := check_alias_source('scalar_lookup_ref', 'struct View { mut: render_inc int }
fn lookup(idx usize, get fn (&usize) voidptr) &View { return get(idx) }
fn dev(idx usize, get fn (&usize) voidptr) {
 mut view := lookup(idx, get)
 view.render_inc = 5
}
fn main() {}
')
	assert tc.notices.any(it.msg == '`idx` is immutable, cannot have a mutable reference to an immutable object'), tc.notices.str()
	assert tc.errors.any(it.msg == '`view.render_inc` aliases mutable data from an immutable value'), tc.errors.str()
}

fn test_voidptr_callback_conversion_inside_unsafe_does_not_borrow_its_argument() {
	tc := check_alias_source('unsafe_conversion', 'struct Item { mut: x int }
fn by_callback(item &Item, get fn (&Item) voidptr) {
 mut alias := unsafe { &Item(get(item)) }
 alias.x = 1
}
fn main() {}
')
	assert tc.notices.len == 0, tc.notices.str()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_scalar_passed_to_a_callback_reference_parameter_is_borrowed() {
	tc := check_alias_source('scalar_ref', 'struct Item { mut: x int }
fn by_reference(n int, get fn (&int) &Item) {
 mut alias := get(n)
 alias.x = 1
}
fn main() {}
')
	assert tc.notices.any(it.msg == '`n` is immutable, cannot have a mutable reference to an immutable object'), tc.notices.str()
	assert tc.errors.any(it.msg == '`alias.x` aliases mutable data from an immutable value'), tc.errors.str()
}

fn test_scalar_passed_to_a_stored_callback_reference_parameter_is_borrowed() {
	tc := check_alias_source('stored_scalar_ref', 'struct Item { mut: x int }
struct Getter { get fn (&int) &Item = unsafe { nil } }
fn by_reference(n int, getter Getter) {
 mut alias := getter.get(n)
 alias.x = 1
}
fn main() {}
')
	assert tc.notices.any(it.msg == '`n` is immutable, cannot have a mutable reference to an immutable object'), tc.notices.str()
	assert tc.errors.any(it.msg == '`alias.x` aliases mutable data from an immutable value'), tc.errors.str()
}

fn test_scalar_passed_by_value_to_a_stored_callback_is_not_borrowed() {
	tc := check_alias_source('stored_scalar', 'struct Item { mut: x int }
struct Getter { get fn (int) &Item = unsafe { nil } }
fn by_value(n int, getter Getter) {
 mut alias := getter.get(n)
 alias.x = 1
}
fn main() {}
')
	assert tc.notices.len == 0, tc.notices.str()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_scalar_reference_parameter_after_expanded_tuple_is_borrowed() {
	tc := check_alias_source('tuple_scalar_ref', 'struct Item { mut: x int }
fn pair() (int, int) { return 1, 2 }
fn by_reference(n int, get fn (int, int, &int) &Item) {
 mut alias := get(pair(), n)
 alias.x = 1
}
fn main() {}
')
	assert tc.notices.any(it.msg == '`n` is immutable, cannot have a mutable reference to an immutable object'), tc.notices.str()
	assert tc.errors.any(it.msg == '`alias.x` aliases mutable data from an immutable value'), tc.errors.str()
}

fn test_collapsed_scalar_fields_are_copied_before_a_callback_reference_argument() {
	tc := check_alias_source('collapsed_scalars', 'struct Item { mut: x int }
@[params]
struct Cfg { n int name string }
fn by_fields(n int, name string, get fn (&Cfg) &Item) {
 mut alias := get(n: n, name: name)
 alias.x = 1
}
fn main() {}
')
	assert tc.notices.len == 0, tc.notices.str()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_collapsed_nested_scalar_struct_is_copied_before_a_callback_reference_argument() {
	tc := check_alias_source('collapsed_nested', 'struct Item { mut: x int }
struct Value { n int }
@[params]
struct Cfg { value Value }
fn by_fields(value Value, get fn (&Cfg) &Item) {
 mut alias := get(value: value)
 alias.x = 1
}
fn main() {}
')
	assert tc.notices.len == 0, tc.notices.str()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_collapsed_scalar_fields_after_a_positional_argument_are_copied() {
	tc := check_alias_source('collapsed_after_positional', 'struct Item { mut: x int }
@[params]
struct Cfg { n int }
fn by_fields(n int, get fn (int, &Cfg) &Item) {
 mut alias := get(n, n: n)
 alias.x = 1
}
fn main() {}
')
	assert tc.notices.len == 0, tc.notices.str()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_collapsed_pointer_field_still_borrows_its_value() {
	tc := check_alias_source('collapsed_pointer', 'struct Item { mut: x int }
@[params]
struct Cfg { n int item &Item }
fn by_fields(n int, item &Item, get fn (&Cfg) &Item) {
 mut alias := get(n: n, item: item)
 alias.x = 1
}
fn main() {}
')
	assert !tc.notices.any(it.msg == '`n` is immutable, cannot have a mutable reference to an immutable object'), tc.notices.str()
	assert tc.notices.any(it.msg == '`item` is immutable, cannot have a mutable reference to an immutable object'), tc.notices.str()
	assert tc.errors.len == 1, tc.errors.str()
	assert tc.errors[0].msg == '`alias.x` aliases mutable data from an immutable value', tc.errors.str()
}

fn test_decomposed_scalar_elements_are_copied_to_callback_value_parameters() {
	tc := check_alias_source('spread_scalars', 'struct Item { mut: x int }
fn by_values(values []int, get fn (int, int) &Item) {
 mut alias := get(...values)
 alias.x = 1
}
fn main() {}
')
	assert tc.notices.len == 0, tc.notices.str()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_decomposed_fixed_array_scalar_elements_are_copied() {
	tc := check_alias_source('spread_fixed_scalars', 'struct Item { mut: x int }
fn by_values(values [2]int, get fn (int, int) &Item) {
 mut alias := get(...values)
 alias.x = 1
}
fn main() {}
')
	assert tc.notices.len == 0, tc.notices.str()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_decomposed_scalar_elements_after_a_positional_argument_are_copied() {
	tc := check_alias_source('spread_after_positional', 'struct Item { mut: x int }
fn by_values(n int, values []int, get fn (int, int, int) &Item) {
 mut alias := get(n, ...values)
 alias.x = 1
}
fn main() {}
')
	assert tc.notices.len == 0, tc.notices.str()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_decomposed_pointer_elements_still_borrow_the_array() {
	tc := check_alias_source('spread_pointers', 'struct Item { mut: x int }
fn by_values(values []&Item, get fn (&Item, &Item) &Item) {
 mut alias := get(...values)
 alias.x = 1
}
fn main() {}
')
	assert tc.notices.any(it.msg == '`values` is immutable, cannot have a mutable reference to an immutable object'), tc.notices.str()
	assert tc.errors.len == 1, tc.errors.str()
	assert tc.errors[0].msg == '`alias.x` aliases mutable data from an immutable value', tc.errors.str()
}

fn test_decomposed_scalar_elements_passed_to_reference_parameters_are_borrowed() {
	tc := check_alias_source('spread_scalar_refs', 'struct Item { mut: x int }
fn by_values(values []int, get fn (int, &int) &Item) {
 mut alias := get(...values)
 alias.x = 1
}
fn main() {}
')
	assert tc.notices.any(it.msg == '`values` is immutable, cannot have a mutable reference to an immutable object'), tc.notices.str()
	assert tc.errors.len == 1, tc.errors.str()
	assert tc.errors[0].msg == '`alias.x` aliases mutable data from an immutable value', tc.errors.str()
}
