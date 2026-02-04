// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
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
	for _, typ in env.expr_types {
		if typ.name() == type_name {
			return true
		}
	}
	return false
}

// Helper to check if a type matches a predicate
fn has_type_matching(env &Environment, predicate fn (Type) bool) bool {
	for _, typ in env.expr_types {
		if predicate(typ) {
			return true
		}
	}
	return false
}

// === Basic Literal Tests ===

fn test_basic_literal_int() {
	env := check_code('fn main() { x := 42 }')
	assert env.expr_types.len > 0, 'checker should populate expr_types'
	// int literals get int_literal type
	assert has_type_matching(env, fn (t Type) bool {
		return t is Primitive && t.props.has(.integer)
	}), 'should have integer primitive type'
}

fn test_basic_literal_float() {
	env := check_code('fn main() { x := 3.14 }')
	assert env.expr_types.len > 0, 'checker should populate expr_types for float'
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
	assert has_type(env, 'char'), 'char literal should have char type'
}

fn test_basic_literal_string() {
	env := check_code('fn main() { x := "hello" }')
	assert has_type(env, 'string'), 'string literal should have string type'
}

// === Infix Expression Tests ===

fn test_infix_expr_arithmetic() {
	env := check_code('fn main() { x := 1 + 2 }')
	assert env.expr_types.len > 0, 'checker should populate expr_types for infix'
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

fn test_array_element_type() {
	env := check_code('fn main() { x := [1, 2, 3] }')
	mut found := false
	for _, typ in env.expr_types {
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
	for _, typ in env.expr_types {
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
	for _, typ in env.expr_types {
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
	assert env.expr_types.len > 0, 'call expr should populate types'
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
	assert env.expr_types.len > 0, 'selector expr should populate types'
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

// === Index Expression Tests ===

fn test_index_expr_array() {
	env := check_code('fn main() { arr := [1, 2, 3]; x := arr[0] }')
	assert env.expr_types.len > 0, 'index expr should populate types'
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
	assert env.expr_types.len > 0, 'if expr should populate types'
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
	assert env.expr_types.len > 0, 'match expr should populate types'
	assert has_type(env, 'string'), 'match expr should produce string type'
}

// === Cast Expression Tests ===

fn test_cast_expr() {
	env := check_code('fn main() { x := int(3.14) }')
	assert env.expr_types.len > 0, 'cast expr should populate types'
}

// === Parenthesized Expression Tests ===

fn test_paren_expr() {
	env := check_code('fn main() { x := (1 + 2) * 3 }')
	assert env.expr_types.len > 0, 'paren expr should populate types'
}

// === Unsafe Expression Tests ===

fn test_unsafe_expr() {
	code := '
fn main() {
	x := unsafe { 42 }
}
'
	env := check_code(code)
	assert env.expr_types.len > 0, 'unsafe expr should populate types'
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
	for _, typ in env.expr_types {
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
