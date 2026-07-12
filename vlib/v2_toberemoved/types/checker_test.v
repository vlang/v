// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
module types

import os
import v2.ast
import v2.parser
import v2.pref
import v2.token

// Helper to parse code and run the checker, returning the environment
fn check_code(code string) &Environment {
	env, _ := check_code_and_files(code)
	return env
}

fn check_code_and_files(code string) (&Environment, []ast.File) {
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
	return env, files
}

// Helper to check if a specific type exists in the environment
fn has_type(env &Environment, type_name string) bool {
	for typ in env.all_expr_types() {
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
	for typ in env.all_expr_types() {
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
	assert env.expr_type_count() > 0, 'checker should populate expr_types'
	// int literals get int_literal type
	assert has_type_matching(env, fn (t Type) bool {
		return t is Primitive && t.props.has(.integer)
	}), 'should have integer primitive type'
}

fn test_basic_literal_float() {
	env := check_code('fn main() { x := 3.14 }')
	assert env.expr_type_count() > 0, 'checker should populate expr_types for float'
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

fn test_comptime_env_expr_has_string_type() {
	env := check_code("const compile_time_backend = \$env('VIPER_BACKEND')

fn main() {
	_ = compile_time_backend
}")
	assert has_type(env, 'string'), '\$env should have string type'
}

fn test_alias_receiver_method_specialization_preserves_alias_return() {
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	env := Environment.new()
	checker := Checker.new(prefs, file_set, env)
	base_type := Type(Struct{
		name: 'math.vec__Vec4[f32]'
	})
	alias_type := Type(Alias{
		name:      'viper__SimdFloat4'
		base_type: base_type
	})
	method_type := Type(fn_with_return_type(empty_fn_type(), base_type))
	specialized := checker.specialize_method_type_for_receiver(method_type, alias_type, '+')
	assert specialized is FnType
	ret_type := (specialized as FnType).return_type or { panic('missing return type') }
	assert ret_type.name() == 'viper__SimdFloat4'
}

fn test_pointer_to_c_struct_field_re_resolves_stale_base_type() {
	check_code('struct C.Buffer {
	data voidptr
}

struct C.View {
	buffer &C.Buffer
}

fn view_data(view &C.View) voidptr {
	buffer := view.buffer
	return buffer.data
}')
}

fn test_inline_type_variants_are_valid_payloads() {
	string_type := Type(string_)
	assert !type_has_null_data(string_type)
	assert !type_data_ptr_is_nil(string_type)
	assert string_type.value_type().name() == 'u8'
	assert !type_has_null_data(Type(char_))
	assert !type_has_null_data(Type(void_))
}

fn test_non_ident_rune_method_uses_registered_return_type() {
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	env := Environment.new()
	mut checker := Checker.new(prefs, file_set, env)
	checker.scope.insert('rune', Type(rune_))
	checker.register_method_type('rune', 'length_in_bytes', fn_with_return_type(empty_fn_type(),
		Type(int_)))
	checker.expecting_method = true
	typ := checker.selector_expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.CallExpr{
			lhs:  ast.Expr(ast.Ident{
				name: 'rune'
			})
			args: [ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '65'
			})]
		})
		rhs: ast.Ident{
			name: 'length_in_bytes'
		}
	})
	assert typ is FnType
	fn_type := typ as FnType
	ret := fn_type.return_type or { panic('missing length_in_bytes return type') }
	assert ret.name() == 'int'
}

fn test_method_lookup_uses_short_owner_for_dot_qualified_sum_type() {
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	env := Environment.new()
	mut checker := Checker.new(prefs, file_set, env)
	checker.register_method_type('Object', 'typ', fn_with_return_type(empty_fn_type(), Type(int_)))
	method := checker.find_method(Type(SumType{
		name: 'types.Object'
	}), 'typ') or { panic('missing Object.typ method') }
	assert method is FnType
	fn_type := method as FnType
	ret := fn_type.return_type or { panic('missing Object.typ return type') }
	assert ret.name() == 'int'
}

fn test_find_method_specializes_array_first_return_type() {
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	env := Environment.new()
	mut checker := Checker.new(prefs, file_set, env)
	checker.register_method_type('array', 'first', fn_with_return_type(empty_fn_type(),
		Type(voidptr_)))
	method := checker.find_method(Type(Array{
		elem_type: Type(string_)
	}), 'first') or { panic('missing array.first method') }
	assert method is FnType
	fn_type := method as FnType
	ret := fn_type.return_type or { panic('missing array.first return type') }
	assert ret.name() == 'string'
}

fn test_non_array_sorted_method_uses_receiver_method_return_type() {
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	env := Environment.new()
	mut checker := Checker.new(prefs, file_set, env)
	foo_type := Type(Struct{
		name: 'Foo'
	})
	checker.scope.insert('foo', object_from_type(foo_type))
	checker.register_method_type('Foo', 'sorted', FnType{
		params:      [
			Parameter{
				name: 'value'
				typ:  Type(int_)
			},
		]
		return_type: Type(string_)
	})
	typ := checker.call_expr(&ast.CallExpr{
		lhs:  ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'foo'
			})
			rhs: ast.Ident{
				name: 'sorted'
			}
		})
		args: [
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '1'
			}),
		]
	})
	assert typ.name() == 'string'
}

fn intrinsic_string_method_param_len(name string) int {
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	env := Environment.new()
	mut checker := Checker.new(prefs, file_set, env)
	method := checker.find_field_or_method(Type(string_), name) or {
		panic('missing intrinsic string method `${name}`')
	}
	assert method is FnType
	return (method as FnType).params.len
}

fn test_intrinsic_string_method_arities_match_builtin_methods() {
	for name in ['clone', 'trim_space', 'to_lower', 'to_upper', 'split_into_lines', 'is_blank'] {
		assert intrinsic_string_method_param_len(name) == 0, '${name} should have no explicit parameters'
	}
	for name in ['trim', 'all_after', 'all_before', 'all_after_last', 'all_before_last', 'contains',
		'starts_with', 'ends_with', 'split', 'index', 'last_index'] {
		assert intrinsic_string_method_param_len(name) == 1, '${name} should have one explicit parameter'
	}
	for name in ['replace', 'index_after'] {
		assert intrinsic_string_method_param_len(name) == 2, '${name} should have two explicit parameters'
	}
}

fn test_slice_clone_preserves_array_type_for_method_chain() {
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	env := Environment.new()
	mut checker := Checker.new(prefs, file_set, env)
	checker.scope.insert('full_parts', object_from_type(Type(Array{
		elem_type: Type(string_)
	})))
	slice_expr := ast.IndexExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'full_parts'
		})
		expr: ast.Expr(ast.RangeExpr{})
	}
	slice_type := checker.expr_type_without_field_smartcast(ast.Expr(slice_expr)) or {
		panic('slice expression type missing')
	}
	assert slice_type is Array, 'range index should keep the array container type'
	assert (slice_type as Array).elem_type.name() == 'string'
}

fn test_smartcasted_sumtype_keeps_receiver_method_lookup_for_call() {
	env := check_code('
type Expr = Ident

struct Ident {
	pos int
}

fn (e Expr) pos() int {
	return 0
}

fn f(left Expr) int {
	if left !is Ident {
		return 0
	}
	return left.pos()
}
')
	assert has_type(env, 'int'), 'smartcasted receiver should still find sumtype method'
}

fn test_option_guard_sumtype_method_call_uses_sumtype_method_not_variant_field() {
	env := check_code('
type Object = Const | Module

struct Const {
	typ int
}

struct Module {
	name string
}

fn (obj &Object) typ() int {
	return match obj {
		Const { obj.typ }
		Module { 0 }
	}
}

fn lookup() ?Object {
	return Const{typ: 7}
}

fn use() int {
	if obj := lookup() {
		if obj !is Module {
			return obj.typ()
		}
	}
	return 0
}
')
	assert has_type(env, 'int'), 'obj.typ() should resolve to the Object.typ method return type'
}

fn test_smartcasted_variant_can_call_original_sumtype_method() {
	env := check_code('
type Expr = Ident | Call

struct Ident {
	name string
}

struct Call {
	lhs Expr
}

fn (expr &Expr) name() string {
	return match expr {
		Ident { expr.name }
		Call { expr.lhs.name() }
	}
}

fn use(expr Expr) string {
	unwrapped := expr
	if unwrapped !is Ident {
		return ""
	}
	return unwrapped.name()
}
')
	assert has_type(env, 'string'), 'smartcasted Expr variant should still resolve Expr.name()'
}

fn test_array_init_records_contextual_sumtype_element_type() {
	env := check_code('
struct Ident {}
struct CallExpr {}
type Expr = Ident | CallExpr
struct Holder {
	rhs []Expr
}
fn main() {
	h := Holder{
		rhs: [CallExpr{}]
	}
	_ = h
}
')
	assert has_type_matching(env, fn (t Type) bool {
		if t is Array {
			if t.elem_type is SumType {
				return t.elem_type.name == 'Expr'
			}
		}
		return false
	}), 'array literal should keep contextual []Expr type'
}

fn test_if_expr_empty_array_else_uses_then_branch_type() {
	env := check_code('
struct Comment {}
struct Pref {
	is_vls bool
}
struct Parser {
	pref         Pref
	cur_comments []Comment
}
struct Decl {}
fn parse(p Parser) Decl {
	comments := if p.pref.is_vls {
		[Comment{}]
	} else {
		[]
	}
	_ = comments
	return Decl{}
}
')
	scope := env.get_fn_scope('main', 'parse') or { panic('missing parse scope') }
	obj := scope.lookup_parent('comments', 0) or { panic('missing comments local') }
	typ := obj.typ()
	assert typ is Array
	arr := typ as Array
	assert arr.elem_type.name() == 'Comment'
}

fn test_if_expr_nested_array_branches_use_element_context() {
	env := check_code('
fn main() {
	is_max := false
	type_const_pairs := if is_max {
		[["f32", "math__max_f32"], ["f64", "math__max_f64"]]
	} else {
		[["i8", "min_i8"], ["i16", "min_i16"]]
	}
	_ = type_const_pairs
}
')
	scope := env.get_fn_scope('main', 'main') or { panic('missing main scope') }
	obj := scope.lookup_parent('type_const_pairs', 0) or { panic('missing type_const_pairs local') }
	typ := obj.typ()
	assert typ is Array
	outer := typ as Array
	assert outer.elem_type is Array
	inner := outer.elem_type as Array
	assert inner.elem_type.name() == 'string'
}

fn collect_column_name_array_literal_types_expr(expr ast.Expr, env &Environment, mut out []v2.types.Type) {
	if expr is ast.ArrayInitExpr {
		if expr.exprs.len == 1 && expr.exprs[0] is ast.Ident
			&& (expr.exprs[0] as ast.Ident).name == 'column_name' {
			if typ := env.get_expr_type(expr.pos.id) {
				out << typ
			}
		}
		for item in expr.exprs {
			collect_column_name_array_literal_types_expr(item, env, mut out)
		}
		return
	}
	match expr {
		ast.CallExpr {
			for arg in expr.args {
				collect_column_name_array_literal_types_expr(arg, env, mut out)
			}
		}
		ast.IfExpr {
			collect_column_name_array_literal_types_expr(expr.cond, env, mut out)
			for stmt in expr.stmts {
				collect_column_name_array_literal_types_stmt(stmt, env, mut out)
			}
			collect_column_name_array_literal_types_expr(expr.else_expr, env, mut out)
		}
		ast.InfixExpr {
			collect_column_name_array_literal_types_expr(expr.lhs, env, mut out)
			collect_column_name_array_literal_types_expr(expr.rhs, env, mut out)
		}
		ast.MatchExpr {
			collect_column_name_array_literal_types_expr(expr.expr, env, mut out)
			for branch in expr.branches {
				for stmt in branch.stmts {
					collect_column_name_array_literal_types_stmt(stmt, env, mut out)
				}
			}
		}
		ast.SelectorExpr {
			collect_column_name_array_literal_types_expr(expr.lhs, env, mut out)
		}
		else {}
	}
}

fn collect_column_name_array_literal_types_stmt(stmt ast.Stmt, env &Environment, mut out []v2.types.Type) {
	match stmt {
		ast.AssignStmt {
			for expr in stmt.rhs {
				collect_column_name_array_literal_types_expr(expr, env, mut out)
			}
		}
		ast.BlockStmt {
			for child in stmt.stmts {
				collect_column_name_array_literal_types_stmt(child, env, mut out)
			}
		}
		ast.ExprStmt {
			collect_column_name_array_literal_types_expr(stmt.expr, env, mut out)
		}
		ast.ForStmt {
			for child in stmt.stmts {
				collect_column_name_array_literal_types_stmt(child, env, mut out)
			}
		}
		else {}
	}
}

fn test_nested_array_append_rhs_literal_uses_element_context() {
	env, files := check_code_and_files('
enum AttrKind { string }

struct Attr {
	name string
	arg  string
	kind AttrKind
}

struct StructField {
	attrs []Attr
}

struct Gen {}

fn (g &Gen) get_orm_column_name_from_struct_field(field StructField) string {
	return field.attrs[0].name
}

fn (g &Gen) get_orm_upsert_conflict_groups(fields []StructField) [][]string {
	mut groups := [][]string{}
	mut named_unique_groups := map[string][]string{}
	mut seen := map[string]bool{}
	for field in fields {
		column_name := g.get_orm_column_name_from_struct_field(field)
		for attr in field.attrs {
			match attr.name {
				"primary" {
					key := column_name
					if key !in seen {
						groups << [column_name]
						seen[key] = true
					}
				}
				"unique" {
					if attr.arg != "" && attr.kind == .string {
						named_unique_groups[attr.arg] << column_name
					} else {
						key := column_name
						if key !in seen {
							groups << [column_name]
							seen[key] = true
						}
					}
				}
				else {}
			}
		}
	}
	return groups
}
')
	mut literal_types := []Type{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'get_orm_upsert_conflict_groups' {
				for fn_stmt in stmt.stmts {
					collect_column_name_array_literal_types_stmt(fn_stmt, env, mut literal_types)
				}
			}
		}
	}
	assert literal_types.len == 2
	for typ in literal_types {
		assert typ is Array
		arr := typ as Array
		assert arr.elem_type.name() == 'string'
	}
}

fn test_iclone_struct_clone_returns_self_type() {
	env := check_code('
interface IClone {}

struct Foo implements IClone {
	x int
}

fn main() {
	f := Foo{x: 1}
	y := f.clone().x
}
')
	assert has_type(env, 'int'), 'f.clone().x should type-check as int'
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

fn test_postfix_question_unwraps_option_payload_type() {
	env := check_code('
fn maybe_string() ?string {
	return "ok"
}

fn use_maybe() ?string {
	value := maybe_string()?
	return value
}
')
	scope := env.get_fn_scope('main', 'use_maybe') or { panic('missing use_maybe scope') }
	obj := scope.lookup_parent('value', 0) or { panic('missing value local') }
	assert obj.typ().name() == 'string'
}

fn test_generic_receiver_init_mapping_resolves_result_return_type() {
	env := check_code('
struct Queue[T] {
	value T
}

fn (mut queue Queue[T]) pop() !T {
	return queue.value
}

fn abort(err string) {
	_ = err
}

fn main() {
	mut queue := Queue[[]string]{value: ["alpha", "omega"]}
	v := queue.pop() or { abort(err) }
	last := v.last()
	_ = last
}
')
	assert has_type(env, 'string'), 'Queue[[]string].pop() or {} should type v as []string'
}

fn test_generic_receiver_init_mapping_keeps_method_generics_separate() {
	env := check_code('
struct Queue[T] {
	value T
}

fn (mut queue Queue[T]) pop_or[U](fallback U) !T {
	_ = fallback
	return queue.value
}

fn abort(err string) {
	_ = err
}

fn main() {
	mut queue := Queue[[]string]{value: ["alpha", "omega"]}
	v := queue.pop_or(123) or { abort(err) }
	last := v.last()
	_ = last
}
')
	assert has_type(env, 'string'), 'method generic U should not replace receiver generic T'
}

fn test_generic_receiver_init_mapping_respects_shadow_scope() {
	env := check_code('
struct Box[T] {
	value T
}

fn (mut box Box[T]) take() !T {
	return box.value
}

fn abort(err string) {
	_ = err
}

fn main() {
	mut box := Box[int]{value: 7}
	if true {
		mut box := Box[[]string]{value: ["alpha", "omega"]}
		v := box.take() or { abort(err) }
		inner_last := v.last()
		_ = inner_last
	}
	n := box.take() or { abort(err) }
	_ = n + 1
}
')
	scope := env.get_fn_scope('main', 'main') or { panic('missing main scope') }
	inner_last := scope.lookup_parent('inner_last', 0) or { panic('missing inner_last local') }
	n := scope.lookup_parent('n', 0) or { panic('missing n local') }
	assert inner_last.typ().name() == 'string'
	assert n.typ().name() == 'int'
}

fn test_temporary_generic_receiver_init_maps_method_generic() {
	env := check_code('
struct Box[T] {
	value T
}

fn (b Box[T]) get[T]() T {
	return b.value
}

fn main() {
	x := Box[int]{value: 7}.get()
	_ = x + 1
}
')
	scope := env.get_fn_scope('main', 'main') or { panic('missing main scope') }
	x := scope.lookup_parent('x', 0) or { panic('missing x local') }
	assert x.typ().name() == 'int'
}

fn test_temporary_generic_call_receiver_maps_method_generic() {
	env := check_code('
struct Box[T] {
	value T
}

fn (b Box[T]) get[T]() T {
	return b.value
}

fn make_box() Box[int] {
	return Box[int]{value: 7}
}

fn main() {
	x := make_box().get()
	_ = x + 1
}
')
	scope := env.get_fn_scope('main', 'main') or { panic('missing main scope') }
	x := scope.lookup_parent('x', 0) or { panic('missing x local') }
	assert x.typ().name() == 'int'
}

fn test_identifier_generic_receiver_from_call_maps_method_generic() {
	env := check_code('
struct Box[T] {
	value T
}

fn (b Box[T]) get[T]() T {
	return b.value
}

fn make_box[T](value T) Box[T] {
	return Box[T]{value: value}
}

fn main() {
	b := make_box[int](7)
	y := b.value
	x := b.get()
	_ = y + 1
	_ = x + 1
}
')
	scope := env.get_fn_scope('main', 'main') or { panic('missing main scope') }
	y := scope.lookup_parent('y', 0) or { panic('missing y local') }
	x := scope.lookup_parent('x', 0) or { panic('missing x local') }
	assert y.typ().name() == 'int'
	assert x.typ().name() == 'int'
}

fn test_phantom_generic_receiver_from_call_maps_method_generic() {
	env := check_code('
struct Box[T] {}

fn make_box() Box[int] {
	return Box[int]{}
}

fn (b Box[T]) touch[T]() {
	_ = b
}

fn main() {
	b := make_box()
	b.touch()
}
')
	scope := env.get_fn_scope('main', 'main') or { panic('missing main scope') }
	b := scope.lookup_parent('b', 0) or { panic('missing b local') }
	assert b.typ().name() == 'Box'
}

fn test_temporary_generic_selector_receiver_maps_method_generic() {
	env := check_code('
struct Box[T] {
	value T
}

struct Holder {
	box Box[int]
}

fn (b Box[T]) get[T]() T {
	return b.value
}

fn main() {
	holder := Holder{box: Box[int]{value: 7}}
	x := holder.box.get()
	_ = x + 1
}
')
	scope := env.get_fn_scope('main', 'main') or { panic('missing main scope') }
	x := scope.lookup_parent('x', 0) or { panic('missing x local') }
	assert x.typ().name() == 'int'
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
	assert env.expr_type_count() > 0, 'checker should populate expr_types for infix'
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
	for typ in env.all_expr_types() {
		if typ is Array {
			if typ.elem_type is Primitive {
				found = typ.elem_type.props.has(.integer)
			}
			break
		}
	}
	assert found, 'array should have integer element type'
}

fn test_array_init_mixed_float_and_int_literals() {
	env := check_code('fn main() { x := [50.0, 15, 1] }')
	mut found := false
	for typ in env.all_expr_types() {
		if typ is Array && typ.elem_type.name() == 'float_literal' {
			found = true
			break
		}
	}
	assert found, 'mixed float/int literal array should infer float literal element type'
}

fn is_nested_array_type(t Type) bool {
	if t is Array {
		elem := t.elem_type
		if elem is Array {
			return elem.elem_type is Array
		}
	}
	return false
}

fn test_array_init_nested_int_literal_matches_concrete_int_array() {
	env := check_code('fn main() { x := [[][]int{}, [[2, 22], [2]]] }')
	assert has_type_matching(env, fn (t Type) bool {
		return is_nested_array_type(t)
	}), 'nested int literal arrays should match concrete nested int array element type'
}

fn test_array_init_nested_concrete_int_array_matches_int_literal() {
	env := check_code('fn main() { x := [[[2, 22], [2]], [][]int{}] }')
	assert has_type_matching(env, fn (t Type) bool {
		return is_nested_array_type(t)
	}), 'nested concrete int array should match nested int literal array element type'
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
	for typ in env.all_expr_types() {
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
	for typ in env.all_expr_types() {
		if typ is Pointer {
			if typ.base_type is Primitive && typ.base_type.props.has(.integer) {
				found_int_ptr = true
				break
			}
		}
	}
	assert found_int_ptr, 'pointer should have integer base type'
}

fn test_sum_type_equality_is_name_based() {
	left := SumType{
		name:     'Value'
		variants: [Type(int_)]
	}
	right := SumType{
		name:     'Value'
		variants: [Type(string_)]
	}
	other := SumType{
		name: 'Other'
	}

	assert left == right
	assert left != other
}

// === Function Tests ===

fn test_call_expr_return_type() {
	code := '
fn foo() int { return 42 }
fn main() { x := foo() }
'
	env := check_code(code)
	assert env.expr_type_count() > 0, 'call expr should populate types'
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
	assert env.expr_type_count() > 0, 'selector expr should populate types'
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
	assert obj is TypeObject
	typ_obj := obj.typ()
	assert typ_obj is Alias
	base := (typ_obj as Alias).base_type
	assert base is Struct
	fields := (base as Struct).fields
	assert fields.len == 4
	assert fields[0].name == 'x'
	assert fields[0].typ.name() == 'f32'
}

fn test_struct_string_field_index_returns_u8_for_builtin_methods() {
	code := '
struct Scanner {
	src string
}

fn (c u8) is_space() bool {
	return true
}

fn starts_with_space(s Scanner) bool {
	ch := s.src[0]
	return ch.is_space() && s.src[0].is_space()
}
'
	env := check_code(code)
	fn_type := env.lookup_fn('main', 'starts_with_space') or { panic('missing starts_with_space') }
	ret := fn_type.return_type or { panic('missing return type') }
	assert ret.name() == 'bool'
	assert has_type(env, 'u8'), 'string index should produce u8'
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
	assert env.expr_type_count() > 0, 'index expr should populate types'
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
	assert env.expr_type_count() > 0, 'if expr should populate types'
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
	assert env.expr_type_count() > 0, 'match expr should populate types'
	assert has_type(env, 'string'), 'match expr should produce string type'
}

fn test_match_type_branch_smartcasts_array_variant() {
	code := '
type Value = string | []int

fn value_len(value Value) int {
	return match value {
		[]int {
			value.len
		}
		else {
			0
		}
	}
}
'
	env := check_code(code)

	assert has_type(env, '[]int'), 'match type branch should smartcast array variants'
}

fn test_logical_not_does_not_extract_positive_smartcast() {
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	env := Environment.new()
	mut checker := Checker.new(prefs, file_set, env)
	cond := ast.Expr(ast.PrefixExpr{
		op:   .not
		expr: ast.Expr(ast.ParenExpr{
			expr: ast.Expr(ast.InfixExpr{
				op:  .key_is
				lhs: ast.Expr(ast.SelectorExpr{
					lhs: ast.Expr(ast.IndexExpr{
						lhs:  ast.Expr(ast.Ident{
							name: 'args'
						})
						expr: ast.Expr(ast.BasicLiteral{
							kind:  .number
							value: '0'
						})
					})
					rhs: ast.Ident{
						name: 'expr'
					}
				})
				rhs: ast.Expr(ast.Ident{
					name: 'CallExpr'
				})
			})
		})
	})
	names, types_ := checker.extract_smartcasts(cond)
	assert names.len == 0
	assert types_.len == 0
}

fn test_not_is_return_smartcasts_following_statements() {
	code := '
type Stmt = EmptyStmt | ForStmt

struct EmptyStmt {}

struct ForStmt {
	init Stmt
}

fn has_init(stmt Stmt) bool {
	if stmt !is ForStmt {
		return false
	}
	_ = stmt.init
	return true
}
'
	env := check_code(code)

	assert has_type(env, 'ForStmt'), '`!is` early return should smartcast the following statements'
}

fn test_array_count_it_predicate() {
	code := '
fn count_first(values [][]int) int {
	return values.count(it[0] == 1)
}
'
	env := check_code(code)

	assert has_type(env, 'int'), 'array.count should typecheck implicit it predicates'
}

fn test_struct_field_forward_flag_enum_type() {
	code := '
struct Holder {
	flags Flags
}

@[flag]
enum Flags {
	enabled
}

fn uses_flag(h Holder) bool {
	return h.flags.has(.enabled)
}
'
	env := check_code(code)

	assert has_type(env, 'Flags'), 'struct fields should resolve enum types declared later'
	assert has_type(env, 'bool'), 'flag enum methods should typecheck shorthand arguments'
}

fn test_or_block_propagated_fn_parameter_call() {
	code := '
fn fallback(cb fn (int) !string) string {
	return cb(1) or {
		cb(2)!
	}
}
'
	env := check_code(code)

	assert has_type(env, 'string'), 'or block should unwrap propagated calls through fn parameters'
}

fn test_if_guard_optional_tuple_destructure() {
	code := '
fn pair() ?(string, string) {
	return none
}

fn use_pair() {
	if first, second := pair() {
		_ := first.len
		_ := second.len
	}
}
'
	env := check_code(code)

	assert has_type(env, 'string'), 'if guard should destructure unwrapped optional tuples'
}

fn test_multiline_struct_init_method_call() {
	code := '
struct Tx {
	inner int
}

fn (tx Tx) is_active() bool {
	return true
}

fn active() bool {
	return Tx{
		inner: 1
	}.is_active()
}
'
	env := check_code(code)

	assert has_type(env, 'bool'), 'multiline struct init method calls should use the struct type'
}

fn test_prefix_multiline_struct_init_method_call() {
	code := '
struct TxInner {}

struct Tx {
	inner &TxInner
}

fn (tx Tx) is_active() bool {
	return true
}

fn active(owner &TxInner) bool {
	return !Tx{
		inner: owner
	}.is_active()
}
'
	env := check_code(code)

	assert has_type(env, 'bool'), 'prefix multiline struct init method calls should use the method result type'
}

fn test_or_condition_prefix_multiline_struct_init_method_call() {
	code := '
struct TxInner {
	active bool
}

struct SavepointInner {
	active bool
	owner  &TxInner
}

struct Savepoint {
	inner &SavepointInner
}

struct Tx {
	inner &TxInner
}

fn (tx Tx) is_active() bool {
	return true
}

fn isnil(p &TxInner) bool {
	return false
}

fn ensure_active(sp Savepoint) bool {
	if !sp.inner.active || isnil(sp.inner.owner) || !Tx{
		inner: sp.inner.owner
	}.is_active() {
		return false
	}
	return true
}
'
	env := check_code(code)

	assert has_type(env, 'bool'), 'or conditions should keep multiline init method receiver type'
}

fn test_map_enum_shorthand_keys() {
	code := '
enum Kind {
	first
	second
}

const labels = {
	Kind.first: "first"
	.second:   "second"
}
'
	env := check_code(code)

	assert has_type(env, 'map[Kind]string'), 'map keys should support enum shorthand after typed first key'
}

// === Cast Expression Tests ===

fn test_cast_expr() {
	env := check_code('fn main() { x := int(3.14) }')
	assert env.expr_type_count() > 0, 'cast expr should populate types'
}

// === Parenthesized Expression Tests ===

fn test_paren_expr() {
	env := check_code('fn main() { x := (1 + 2) * 3 }')
	assert env.expr_type_count() > 0, 'paren expr should populate types'
}

// === Unsafe Expression Tests ===

fn test_unsafe_expr() {
	code := '
fn main() {
	x := unsafe { 42 }
}
'
	env := check_code(code)
	assert env.expr_type_count() > 0, 'unsafe expr should populate types'
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

fn test_result_return_accepts_option_or_error_fallback() {
	code := '
interface IError {
	msg() string
}

struct MessageError {}

fn (err MessageError) msg() string {
	return "missing"
}

fn error(msg string) IError {
	return MessageError{}
}

fn maybe() ?int { return none }
fn foo() !int { return maybe() or { error("missing") } }
fn main() {}
'
	env := check_code(code)
	scope := env.get_scope('main') or { panic('missing main scope') }
	foo_obj := scope.lookup_parent('foo', 0) or { panic('missing foo function') }
	foo_type := foo_obj.typ()
	assert foo_type is FnType
	foo_return := (foo_type as FnType).return_type or { panic('missing foo return type') }
	assert foo_return is ResultType
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
	for typ in env.all_expr_types() {
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

fn test_explicit_lifetime_syntax_in_fn_types_and_generic_args() {
	code := '
struct Ignore {}

struct IgnoreMatch[^a] {}

fn matched_dir_entry[^a](self &^a Ignore) IgnoreMatch[^a] {
	return IgnoreMatch[^a]{}
}
'
	env := check_code(code)
	fn_type := env.lookup_fn('main', 'matched_dir_entry') or { panic('missing matched_dir_entry') }
	assert '^a' in fn_type.generic_params
	assert fn_type.params.len == 1
	assert fn_type.params[0].typ is Pointer
	ptr := fn_type.params[0].typ as Pointer
	assert ptr.lifetime == 'a'
	assert ptr.base_type.name() == 'Ignore'
	assert has_type_matching(env, fn (t Type) bool {
		return t is Struct && t.name == 'IgnoreMatch' && t.generic_params == ['^a']
	})
}

fn test_explicit_lifetime_method_receiver_and_nested_generic_return_type() {
	code := '
struct Ignore {}

struct DirEntry {}

struct Match[T] {
	value T
}

struct IgnoreMatch[^a] {
	ig &^a Ignore
}

fn (ig &^a Ignore) matched_dir_entry[^a](dent &DirEntry) Match[IgnoreMatch[^a]] {
	return Match[IgnoreMatch[^a]]{
		value: IgnoreMatch[^a]{
			ig: ig
		}
	}
}

fn main() {
	ig := Ignore{}
	dent := DirEntry{}
	ig.matched_dir_entry(&dent)
}
'
	env := check_code(code)
	method := env.lookup_method('Ignore', 'matched_dir_entry') or {
		panic('missing Ignore.matched_dir_entry')
	}
	assert '^a' in method.generic_params
	assert method.params.len == 1
	assert method.params[0].typ is Pointer
	dent_param := method.params[0].typ as Pointer
	assert dent_param.lifetime == ''
	assert dent_param.base_type.name() == 'DirEntry'
	return_type := method.get_return_type() or { panic('missing matched_dir_entry return type') }
	assert return_type.name() == 'Match'
	assert has_type_matching(env, fn (t Type) bool {
		return t is Struct && t.name == 'IgnoreMatch' && t.generic_params == ['^a']
	})
	assert has_type_matching(env, fn (t Type) bool {
		return t is Struct && t.name == 'Match'
	})
}
