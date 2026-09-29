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

fn test_voidptr_lookup_without_an_unsafe_boundary_borrows_its_arguments() {
	tc := check_alias_source('container_borrow', 'struct Queryable { on_query fn (&Queryable, usize) voidptr = unsafe { nil } }
fn (q &Queryable) query(idx usize) voidptr { return q.on_query(q, idx) }
struct View { mut: render_inc int }
fn View.from_context(ctx &Queryable) &View { return ctx.query(usize(typeof(View{}).idx)) }
fn dev(ctx &Queryable) {
 mut view := View.from_context(ctx)
 view.render_inc = 5
}
fn main() {}
')
	assert tc.notices.any(it.msg == '`ctx` is immutable, cannot have a mutable reference to an immutable object'), tc.notices.str()
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
