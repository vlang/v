// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: !windows
module types

import os
import v2.parser
import v2.pref
import v2.token

// Helper to parse code and run the checker, returning the environment
fn check_code(code string) &Environment {
	// Write code to a temp file
	tmp_file := '/tmp/checker_test_${os.getpid()}.v'
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}

	p := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(p)
	files := par.parse_files([tmp_file], mut file_set)

	mut env := Environment.new()
	mut checker := Checker.new(p, file_set, env)
	checker.check_files(files)
	return env
}

// Helper to check if a specific type exists in the environment
fn has_type(env &Environment, type_name string) bool {
	for typ in env.expr_type_values {
		if typ is Void {
			continue
		}
		if typ.name() == type_name {
			return true
		}
	}
	return false
}

// Helper to check if a type matches a predicate
fn has_type_matching(env &Environment, predicate fn (Type) bool) bool {
	for typ in env.expr_type_values {
		if typ is Void {
			continue
		}
		if predicate(typ) {
			return true
		}
	}
	return false
}

// === Basic Literal Tests ===

fn test_basic_literal_int() {
	env := check_code('fn main() { x := 42 }')
	assert env.expr_type_values.len > 0, 'checker should populate expr_types'
	// int literals get int_literal type
	assert has_type_matching(env, fn (t Type) bool {
		return t is Primitive && t.props.has(.integer)
	}), 'should have integer primitive type'
}

fn test_basic_literal_float() {
	env := check_code('fn main() { x := 3.14 }')
	assert env.expr_type_values.len > 0, 'checker should populate expr_types for float'
	assert has_type_matching(env, fn (t Type) bool {
		return t is Primitive && t.props.has(.float)
	}), 'should have float primitive type'
}

fn test_basic_literal_bool_true() {
	env := check_code('fn main() { x := true }')
	assert has_type(env, 'bool'), 'true should have bool type'
}

fn test_basic_literal_bool_false() {
	env := check_code('fn main() { x := false }')
	assert has_type(env, 'bool'), 'false should have bool type'
}

fn test_basic_literal_char() {
	env := check_code('fn main() { x := `a` }')
	assert has_type(env, 'rune'), 'char literal should have rune type'
}

fn test_basic_literal_string() {
	env := check_code('fn main() { x := "hello" }')
	assert has_type(env, 'string'), 'string literal should have string type'
}

fn test_or_expr_accepts_int_literal_fallback() {
	env := check_code('fn may_fail() !int { return 1 }
fn main() { x := may_fail() or { 0 } }')
	assert has_type(env, 'int'), 'or { 0 } should be accepted for !int'
}

fn test_or_expr_accepts_float_literal_fallback() {
	env := check_code('fn may_fail() !f64 { return 1.0 }
fn main() { x := may_fail() or { 0 } }')
	assert has_type(env, 'f64'), 'or { 0 } should be accepted for !f64'
}

fn test_comptime_embed_file_type_and_methods() {
	code := 'fn main() { x := ' + '$' + 'embed_file("asset.txt"); y := x.to_string(); z := x.len }'
	env := check_code(code)
	assert has_type(env, embed_file_helper_type_name), 'embed_file helper type should be recorded'
	assert has_type(env, 'string'), 'embed_file.to_string() should type as string'
	assert has_type(env, 'int'), 'embed_file.len should type as int'
}

fn test_comptime_embed_file_chained_method_type() {
	code := 'fn main() { x := ' + '$' + 'embed_file("asset.txt").to_bytes() }'
	env := check_code(code)
	assert has_type_matching(env, fn (t Type) bool {
		return t is Array && t.elem_type.name() == 'u8'
	}), 'chained embed_file.to_bytes() should type as []u8'
}

fn test_union_embedded_alias_fields_resolve() {
	code := '
struct Rect {
	x int
}

type GgRect = Rect

union Box {
	GgRect
}

fn main() {
	b := Box{}
	x := b.x
}
'
	env := check_code(code)
	assert has_type(env, 'int'), 'embedded union alias field access should resolve to int'
}

fn test_embedded_interface_fields_resolve() {
	code := '
interface ClippingWidget {
	x int
	y int
}

interface ScrollableWidget {
	ClippingWidget
}

fn pos(w ScrollableWidget) int {
	return w.x + w.y
}
'
	env := check_code(code)
	assert has_type(env, 'int'), 'embedded interface fields should resolve through the parent interface'
}

fn test_generic_body_field_lookup_uses_active_concrete_type() {
	code := '
struct ScrollView {
	children_to_update bool
}

struct Canvas {
	scrollview &ScrollView = unsafe { nil }
}

fn has_children_to_update[T](mut w T) bool {
	mut sv := w.scrollview
	return sv.children_to_update
}

fn main() {
	mut c := Canvas{
		scrollview: &ScrollView{
			children_to_update: true
		}
	}
	_ = has_children_to_update(mut c)
}
'
	env := check_code(code)
	assert has_type(env, 'bool'), 'generic body field lookup should use the active concrete type'
}

fn test_interface_field_alias_to_function_type_resolves() {
	code := '
type Handler = fn ()

interface ScrollableWidget {
	on_scroll_change Handler
}

fn call_handler(sw ScrollableWidget) {
	sw.on_scroll_change()
}
'
	env := check_code(code)
	assert has_type_matching(env, fn (t Type) bool {
		if t is FnType {
			return true
		}
		if t is Alias && t.base_type is FnType {
			return true
		}
		return false
	}), 'interface fields aliased to fn types should resolve as callable'
}

fn test_top_level_comptime_c_fn_decl_is_preregistered() {
	code := [
		'type Handler = fn (voidptr)',
		'',
		'$' + 'if !windows {',
		'\tfn C.foo(title &char, user_data voidptr, on_open Handler, on_refresh Handler, on_quit Handler)',
		'}',
		'',
		'fn main() {',
		'\ts := "hi"',
		'\tcb := unsafe { Handler(0) }',
		'\t' + '$' + 'if !windows {',
		'\t\tC.foo(&char(s.str), voidptr(0), cb, cb, cb)',
		'\t}',
		'}',
	].join('\n')
	env := check_code(code)
	assert has_type_matching(env, fn (t Type) bool {
		return t is FnType && t.params.len == 5
	}), 'top-level comptime C fn decl should be preregistered as a callable fn type'
}

fn test_if_expr_tuple_destructuring_types_follow_trailing_empty_stmt() {
	code := '
struct SpinLock {}

fn (s &SpinLock) lock() {}

struct Subscription {}

struct Channel {
	write_sub_mtx    &SpinLock     = unsafe { nil }
	read_sub_mtx     &SpinLock     = unsafe { nil }
	write_subscriber &Subscription = unsafe { nil }
	read_subscriber  &Subscription = unsafe { nil }
}

enum Direction {
	push
	pop
}

fn main() {
	ch := Channel{}
	dir := Direction.push
	sub_mtx, subscriber := if dir == .push {
		ch.write_sub_mtx, &ch.write_subscriber
	} else {
		ch.read_sub_mtx, &ch.read_subscriber
	}
	sub_mtx.lock()
	_ = subscriber
}
'
	env := check_code(code)
	assert has_type_matching(env, fn (t Type) bool {
		return t is Pointer && t.base_type.name().ends_with('SpinLock')
	}), 'tuple destructuring from if-expr should keep the pointer type for sub_mtx'
}

// === Infix Expression Tests ===

fn test_infix_expr_arithmetic() {
	env := check_code('fn main() { x := 1 + 2 }')
	assert env.expr_type_values.len > 0, 'checker should populate expr_types for infix'
}

fn test_infix_expr_comparison_lt() {
	env := check_code('fn main() { x := 1 < 2 }')
	assert has_type(env, 'bool'), 'comparison < should produce bool type'
}

fn test_infix_expr_comparison_eq() {
	env := check_code('fn main() { x := 1 == 2 }')
	assert has_type(env, 'bool'), 'comparison == should produce bool type'
}

fn test_infix_expr_comparison_ne() {
	env := check_code('fn main() { x := 1 != 2 }')
	assert has_type(env, 'bool'), 'comparison != should produce bool type'
}

fn test_infix_expr_logical_and() {
	env := check_code('fn main() { x := true && false }')
	assert has_type(env, 'bool'), 'logical && should produce bool type'
}

fn test_infix_expr_logical_or() {
	env := check_code('fn main() { x := true || false }')
	assert has_type(env, 'bool'), 'logical || should produce bool type'
}

// === Array Tests ===

fn test_array_init() {
	env := check_code('fn main() { x := [1, 2, 3] }')
	assert has_type_matching(env, fn (t Type) bool {
		return t is Array
	}), 'array init should produce Array type'
}

fn test_fixed_array_init() {
	env := check_code('fn main() { x := [1, 2, 3]! }')
	assert has_type_matching(env, fn (t Type) bool {
		return t is ArrayFixed
	}), 'fixed array init should produce ArrayFixed type'
}

fn test_fixed_array_slice_returns_array() {
	env := check_code('fn main() { x := [1, 2, 3]!; y := x[0..2] }')
	assert has_type_matching(env, fn (t Type) bool {
		return t is Array
	}), 'fixed array slicing should produce Array type'
}

fn test_array_element_type() {
	env := check_code('fn main() { x := [1, 2, 3] }')
	mut found := false
	for typ in env.expr_type_values {
		if typ is Array {
			if typ.elem_type is Primitive {
				found = typ.elem_type.props.has(.integer)
			}
			break
		}
	}
	assert found, 'array should have integer element type'
}

// === Map Tests ===

fn test_map_init() {
	env := check_code("fn main() { x := {'a': 1, 'b': 2} }")
	assert has_type_matching(env, fn (t Type) bool {
		return t is Map
	}), 'map init should produce Map type'
}

fn test_map_types() {
	env := check_code("fn main() { x := {'a': 1} }")
	mut found_correct_types := false
	for typ in env.expr_type_values {
		if typ is Map {
			key_is_string := typ.key_type.name() == 'string'
			value_is_int := typ.value_type is Primitive && typ.value_type.props.has(.integer)
			if key_is_string && value_is_int {
				found_correct_types = true
				break
			}
		}
	}
	assert found_correct_types, 'map should have string key and int value types'
}

// === Pointer Tests ===

fn test_prefix_address_of() {
	env := check_code('fn main() { x := 42; y := &x }')
	assert has_type_matching(env, fn (t Type) bool {
		return t is Pointer
	}), 'address-of should produce Pointer type'
}

fn test_pointer_base_type() {
	env := check_code('fn main() { x := 42; y := &x }')
	mut found_int_ptr := false
	for typ in env.expr_type_values {
		if typ is Pointer {
			if typ.base_type is Primitive && typ.base_type.props.has(.integer) {
				found_int_ptr = true
				break
			}
		}
	}
	assert found_int_ptr, 'pointer should have integer base type'
}

// === Function Tests ===

fn test_call_expr_return_type() {
	code := '
fn foo() int { return 42 }
fn main() { x := foo() }
'
	env := check_code(code)
	assert env.expr_type_values.len > 0, 'call expr should populate types'
}

fn test_fn_literal() {
	code := '
fn main() { f := fn() int { return 42 } }
'
	env := check_code(code)
	assert has_type_matching(env, fn (t Type) bool {
		return t is FnType
	}), 'fn literal should produce FnType'
}

// === Struct Tests ===

fn test_selector_expr() {
	code := '
struct Point { x int; y int }
fn main() { p := Point{x: 1, y: 2}; z := p.x }
'
	env := check_code(code)
	assert env.expr_type_values.len > 0, 'selector expr should populate types'
}

fn test_init_expr() {
	code := '
struct Point { x int; y int }
fn main() { p := Point{x: 1, y: 2} }
'
	env := check_code(code)
	assert has_type_matching(env, fn (t Type) bool {
		if t is Struct {
			return t.name == 'Point'
		}
		return false
	}), 'init expr should produce struct type'
}

fn test_struct_auto_str_method_returns_string() {
	code := '
struct Point { x int; y int }
fn main() {
	p := Point{x: 1, y: 2}
	s := p.str()
}
'
	env := check_code(code)
	assert has_type(env, 'string'), 'struct .str() should resolve to string'
}

fn test_generic_struct_alias_fields_are_instantiated() {
	code := '
struct Vec4[T] {
	x T
	y T
	z T
	w T
}

type SimdFloat4 = Vec4[f32]

fn main() {
	v := SimdFloat4{ x: 1.5, y: 2.5, z: 3.5, w: 4.5 }
	_ = v.x
}
'
	env := check_code(code)
	scope := env.get_scope('main') or { panic('missing main scope') }
	obj := scope.lookup_parent('SimdFloat4', 0) or { panic('missing SimdFloat4') }
	assert obj is Type
	typ_obj := obj as Type
	assert typ_obj is Alias
	base := (typ_obj as Alias).base_type
	assert base is Struct
	fields := (base as Struct).fields
	assert fields.len == 4
	assert fields[0].name == 'x'
	assert fields[0].typ.name() == 'f32'
}

fn test_generic_method_receiver_instantiation_handles_self_referential_nodes() {
	code := '
struct Node[T] {
mut:
	value T
	next  &Node[T] = unsafe { nil }
}

struct LinkedList[T] {
mut:
	head &Node[T] = unsafe { nil }
}

fn (mut list LinkedList[T]) push(value T) {}
'
	env := check_code(code)
	method := env.lookup_method('LinkedList', 'push') or { panic('missing LinkedList.push') }
	assert method.params.len >= 1
	last_param := method.params[method.params.len - 1]
	assert last_param.typ.name() == 'T'
}

fn test_nested_scope_updates_use_scope_identity_for_recursive_interfaces() {
	code := '
interface Node {
	children() []Node
}

fn main() {
	nodes := []Node{}
	for node in nodes {
		copy := node
		_ = copy
	}
}
'
	env := check_code(code)
	assert has_type(env, 'Node'), 'recursive interface in nested scope should type check'
}

// === Index Expression Tests ===

fn test_index_expr_array() {
	env := check_code('fn main() { arr := [1, 2, 3]; x := arr[0] }')
	assert env.expr_type_values.len > 0, 'index expr should populate types'
}

fn test_index_expr_returns_element_type() {
	env := check_code('fn main() { arr := [1, 2, 3]; x := arr[0] }')
	// The index expr should return the element type (int)
	assert has_type_matching(env, fn (t Type) bool {
		return t is Primitive && t.props.has(.integer)
	}), 'array index should return integer element type'
}

// === Control Flow Tests ===

fn test_if_expr() {
	env := check_code('fn main() { x := if true { 1 } else { 2 } }')
	assert env.expr_type_values.len > 0, 'if expr should populate types'
}

fn test_match_expr() {
	code := '
fn main() {
	x := 1
	y := match x {
		1 { "one" }
		else { "other" }
	}
}
'
	env := check_code(code)
	assert env.expr_type_values.len > 0, 'match expr should populate types'
	assert has_type(env, 'string'), 'match expr should produce string type'
}

// === Cast Expression Tests ===

fn test_cast_expr() {
	env := check_code('fn main() { x := int(3.14) }')
	assert env.expr_type_values.len > 0, 'cast expr should populate types'
}

// === Parenthesized Expression Tests ===

fn test_paren_expr() {
	env := check_code('fn main() { x := (1 + 2) * 3 }')
	assert env.expr_type_values.len > 0, 'paren expr should populate types'
}

// === Unsafe Expression Tests ===

fn test_unsafe_expr() {
	code := '
fn main() {
	x := unsafe { 42 }
}
'
	env := check_code(code)
	assert env.expr_type_values.len > 0, 'unsafe expr should populate types'
}

// === Tuple Tests ===

fn test_tuple_type() {
	code := '
fn foo() (int, string) { return 1, "hello" }
fn main() { a, b := foo() }
'
	env := check_code(code)
	assert has_type_matching(env, fn (t Type) bool {
		return t is Tuple
	}), 'multiple return should produce Tuple type'
}

// === Option/Result Type Tests ===

fn test_option_type() {
	code := '
fn foo() ?int { return 42 }
fn main() { x := foo() }
'
	env := check_code(code)
	assert has_type_matching(env, fn (t Type) bool {
		return t is OptionType
	}), 'option return should produce OptionType'
}

fn test_result_type() {
	code := '
fn foo() !int { return 42 }
fn main() { x := foo() }
'
	env := check_code(code)
	assert has_type_matching(env, fn (t Type) bool {
		return t is ResultType
	}), 'result return should produce ResultType'
}

// === Channel Tests ===

fn test_channel_type() {
	code := 'fn main() { ch := chan int{} }'
	env := check_code(code)
	assert has_type_matching(env, fn (t Type) bool {
		return t is Channel
	}), 'channel init should produce Channel type'
}

// === Thread Tests ===

fn test_spawn_returns_thread() {
	code := '
fn worker() {}
fn main() { t := spawn worker() }
'
	env := check_code(code)
	assert has_type_matching(env, fn (t Type) bool {
		return t is Thread
	}), 'spawn should produce Thread type'
}

// === Method Return Type Tests ===

fn test_method_returns_option() {
	code := '
struct Scope {
	name string
}

fn (s &Scope) lookup(name string) ?int {
	return 42
}

fn main() {
	s := Scope{}
	x := s.lookup("test")
}
'
	env := check_code(code)
	assert has_type_matching(env, fn (t Type) bool {
		return t is OptionType
	}), 'method returning ?int should produce OptionType'
}

fn test_method_returns_result() {
	code := '
struct Parser {
	data string
}

fn (p &Parser) parse() !string {
	return "parsed"
}

fn main() {
	p := Parser{}
	x := p.parse()
}
'
	env := check_code(code)
	assert has_type_matching(env, fn (t Type) bool {
		return t is ResultType
	}), 'method returning !string should produce ResultType'
}

fn test_method_option_base_type() {
	code := '
struct Container {
	value int
}

fn (c &Container) get() ?int {
	if c.value > 0 {
		return c.value
	}
	return none
}

fn main() {
	c := Container{}
	x := c.get()
}
'
	env := check_code(code)
	mut found_option_int := false
	for typ in env.expr_type_values {
		if typ is OptionType {
			if typ.base_type is Primitive && typ.base_type.props.has(.integer) {
				found_option_int = true
				break
			}
		}
	}
	assert found_option_int, 'method should return OptionType with int base type'
}

fn test_method_lookup_returns_correct_type() {
	// This tests the pattern used in lookup_type_from_env
	code := '
struct Object {
	name string
	value int
}

struct Scope {
	obj Object
}

fn (s &Scope) lookup_parent(name string, depth int) ?Object {
	if name == s.obj.name {
		return s.obj
	}
	return none
}

fn main() {
	s := Scope{}
	result := s.lookup_parent("test", 0)
}
'
	env := check_code(code)
	assert has_type_matching(env, fn (t Type) bool {
		if t is OptionType {
			if t.base_type is Struct {
				return t.base_type.name == 'Object'
			}
		}
		return false
	}), 'lookup_parent should return OptionType with Object base type'
}

fn test_scope_insert_replaces_module_placeholder_with_function() {
	mut scope := new_scope(unsafe { nil })
	scope.insert('optimize', Module{
		name:  'optimize'
		scope: new_scope(unsafe { nil })
	})
	scope.insert('optimize', Fn{
		name: 'optimize'
		typ:  Type(FnType{})
	})
	obj := scope.lookup_parent('optimize', 0) or { panic('missing optimize') }
	assert obj is Fn
}
