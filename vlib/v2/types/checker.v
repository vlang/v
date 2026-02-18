// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module types

import time
import v2.ast
import v2.errors
import v2.pref
import v2.token

pub struct Environment {
pub mut:
	// errors with no default value
	scopes shared map[string]&Scope = map[string]&Scope{}
	// Function scopes - stores the scope for each function by qualified name (module__fn_name)
	// This allows later passes (transformer, codegen) to look up local variable types
	fn_scopes shared map[string]&Scope = map[string]&Scope{}
	// types map[int]Type
	// methods - shared for parallel type checking
	methods           shared map[string][]&Fn = map[string][]&Fn{}
	generic_types     map[string][]map[string]Type
	cur_generic_types []map[string]Type
	// Expression types - indexed directly by pos.id.
	// Positive IDs (1-based) come from the parser via token.Pos.id.
	// Negative IDs come from transformer's synthesized nodes.
	expr_type_values     []Type // indexed by pos.id for positive IDs
	expr_type_neg_values []Type // indexed by -pos.id for negative IDs (synth nodes)
}

pub fn Environment.new() &Environment {
	return &Environment{
		expr_type_values: []Type{cap: 100_000}
	}
}

// set_expr_type stores the computed type for an expression by its unique ID.
pub fn (mut e Environment) set_expr_type(id int, typ Type) {
	if id >= 0 {
		if id >= e.expr_type_values.len {
			// Grow with 2x strategy to amortize reallocation cost
			mut new_len := if e.expr_type_values.len < 100_000 {
				100_000
			} else {
				e.expr_type_values.len * 2
			}
			if new_len <= id {
				new_len = id + 1
			}
			for e.expr_type_values.len < new_len {
				// Void(1) is the sentinel for "unset"; Void(0) is valid void type.
				e.expr_type_values << Type(Void(1))
			}
		}
		e.expr_type_values[id] = typ
	} else {
		idx := -id
		if idx >= e.expr_type_neg_values.len {
			mut new_len := if e.expr_type_neg_values.len == 0 {
				64
			} else {
				e.expr_type_neg_values.len * 2
			}
			if new_len <= idx {
				new_len = idx + 1
			}
			for e.expr_type_neg_values.len < new_len {
				e.expr_type_neg_values << Type(Void(1))
			}
		}
		e.expr_type_neg_values[idx] = typ
	}
}

// get_expr_type retrieves the computed type for an expression by its unique ID.
pub fn (e &Environment) get_expr_type(id int) ?Type {
	if id > 0 && id < e.expr_type_values.len {
		typ := e.expr_type_values[id]
		if typ is Void {
			return none
		}
		return typ
	} else if id < 0 {
		idx := -id
		if idx < e.expr_type_neg_values.len {
			typ := e.expr_type_neg_values[idx]
			if typ is Void {
				return none
			}
			return typ
		}
	}
	return none
}

// lookup_method looks up a method by receiver type name and method name
// Returns the method's FnType if found
pub fn (e &Environment) lookup_method(type_name string, method_name string) ?FnType {
	mut methods := []&Fn{}
	rlock e.methods {
		if type_name in e.methods {
			methods = unsafe { e.methods[type_name] }
		}
	}
	for method in methods {
		if method.get_name() == method_name {
			typ := method.get_typ()
			if typ is FnType {
				return typ
			}
		}
	}
	return none
}

// lookup_fn looks up a function by module and name in the environment's scopes
// Returns the function's FnType if found
pub fn (e &Environment) lookup_fn(module_name string, fn_name string) ?FnType {
	mut scope := &Scope(unsafe { nil })
	mut found_scope := false
	lock e.scopes {
		if module_name in e.scopes {
			scope = unsafe { e.scopes[module_name] }
			found_scope = true
		}
	}
	if !found_scope {
		return none
	}
	if obj := scope.lookup_parent(fn_name, 0) {
		if obj is Fn {
			typ := obj.get_typ()
			if typ is FnType {
				return typ
			}
		}
	}
	return none
}

// lookup_local_var looks up a local variable by name in the given scope.
// Walks up the scope chain to find the variable and returns its type.
pub fn (e &Environment) lookup_local_var(scope &Scope, name string) ?Type {
	mut s := unsafe { scope }
	if obj := s.lookup_parent(name, 0) {
		return obj.typ()
	}
	return none
}

// set_fn_scope stores the scope for a function by its qualified name
pub fn (mut e Environment) set_fn_scope(module_name string, fn_name string, scope &Scope) {
	key := if module_name == '' { fn_name } else { '${module_name}__${fn_name}' }
	lock e.fn_scopes {
		e.fn_scopes[key] = scope
	}
}

// get_fn_scope retrieves the scope for a function by its qualified name
pub fn (e &Environment) get_fn_scope(module_name string, fn_name string) ?&Scope {
	key := if module_name == '' { fn_name } else { '${module_name}__${fn_name}' }
	mut scope := &Scope(unsafe { nil })
	mut found_scope := false
	lock e.fn_scopes {
		if key in e.fn_scopes {
			scope = unsafe { e.fn_scopes[key] }
			found_scope = true
		}
	}
	if !found_scope {
		return none
	}
	return scope
}

// get_scope retrieves a module scope by exact module name.
pub fn (e &Environment) get_scope(module_name string) ?&Scope {
	mut scope := &Scope(unsafe { nil })
	mut found_scope := false
	lock e.scopes {
		if module_name in e.scopes {
			scope = unsafe { e.scopes[module_name] }
			found_scope = true
		}
	}
	if !found_scope {
		return none
	}
	return scope
}

// get_fn_scope_by_key retrieves a function scope by its fully-qualified key.
pub fn (e &Environment) get_fn_scope_by_key(key string) ?&Scope {
	mut scope := &Scope(unsafe { nil })
	mut found_scope := false
	lock e.fn_scopes {
		if key in e.fn_scopes {
			scope = unsafe { e.fn_scopes[key] }
			found_scope = true
		}
	}
	if !found_scope {
		return none
	}
	return scope
}

pub enum DeferredKind {
	fn_decl
	fn_decl_generic
	struct_decl
	const_decl
}

pub struct Deferred {
pub:
	kind  DeferredKind
	func  fn () = unsafe { nil }
	scope &Scope
}

struct PendingConstField {
	scope &Scope
	field ast.FieldInit
}

struct PendingInterfaceDecl {
	scope &Scope
	decl  ast.InterfaceDecl
}

struct PendingStructDecl {
	scope &Scope
	decl  ast.StructDecl
}

struct PendingTypeDecl {
	scope &Scope
	decl  ast.TypeDecl
}

struct PendingFnBody {
	scope         &Scope
	decl          ast.FnDecl
	typ           FnType
	scope_fn_name string
	module_name   string
}

struct Checker {
	pref &pref.Preferences
	// info Info
	// TODO: mod
	mod &Module = new_module('main', '')
mut:
	env           &Environment = &Environment{}
	file_set      &token.FileSet
	scope         &Scope = new_scope(unsafe { nil })
	c_scope       &Scope = new_scope(unsafe { nil })
	expected_type ?Type
	// Current file's module name (for saving function scopes)
	cur_file_module string
	// Function root scope - used to flatten local variable types for transformer lookup
	fn_root_scope &Scope = unsafe { nil }
	// when true, function declarations only register signatures and queue body checking
	collect_fn_signatures_only bool
	pending_const_fields       []PendingConstField
	pending_interface_decls    []PendingInterfaceDecl
	pending_struct_decls       []PendingStructDecl
	pending_type_decls         []PendingTypeDecl
	pending_fn_bodies          []PendingFnBody

	generic_params []string
	// TODO: remove once fields/methods with same name
	// are no longer allowed & removed.
	expecting_method bool
}

pub fn Checker.new(prefs &pref.Preferences, file_set &token.FileSet, env &Environment) &Checker {
	return &Checker{
		pref:     unsafe { prefs }
		file_set: unsafe { file_set }
		env:      unsafe { env }
	}
}

// qualify_type_name returns the fully qualified type name with module prefix.
// e.g., "File" in module "ast" becomes "ast__File".
// Builtin types and main module types are not prefixed.
fn (c &Checker) qualify_type_name(name string) string {
	// Don't qualify builtin or main module types
	if c.cur_file_module == 'builtin' || c.cur_file_module == '' || c.cur_file_module == 'main' {
		return name
	}
	return '${c.cur_file_module}__${name}'
}

fn to_optional_type(typ Type) ?Type {
	return typ
}

fn object_from_type(typ Type) Object {
	return typ
}

fn is_empty_expr(e ast.Expr) bool {
	return e is ast.EmptyExpr
}

fn with_generic_params(fn_type FnType, params []string) FnType {
	return FnType{
		generic_params: params
		params:         fn_type.params
		return_type:    fn_type.return_type
		is_variadic:    fn_type.is_variadic
		attributes:     fn_type.attributes
		generic_types:  fn_type.generic_types
	}
}

fn fn_with_return_type(fn_type FnType, return_type Type) FnType {
	return FnType{
		generic_params: fn_type.generic_params
		params:         fn_type.params
		return_type:    to_optional_type(return_type)
		is_variadic:    fn_type.is_variadic
		attributes:     fn_type.attributes
		generic_types:  fn_type.generic_types
	}
}

pub fn (mut c Checker) get_module_scope(module_name string, parent &Scope) &Scope {
	mut scope := &Scope(unsafe { nil })
	lock c.env.scopes {
		if module_name in c.env.scopes {
			scope = unsafe { c.env.scopes[module_name] }
		} else {
			scope = new_scope(parent)
			c.env.scopes[module_name] = scope
		}
	}
	return scope
}

pub fn (mut c Checker) check_files(files []ast.File) {
	// c.file_set = unsafe { file_set }
	c.preregister_all_scopes(files)
	c.preregister_all_types(files)
	c.collect_fn_signatures_only = true
	c.preregister_all_fn_signatures(files)
	c.collect_fn_signatures_only = false

	for file in files {
		c.check_file(file)
	}
	c.process_pending_const_fields()
	c.process_pending_fn_bodies()
	c.check_struct_field_defaults(files)
	c.check_enum_field_values(files)
}

pub fn (mut c Checker) check_file(file ast.File) {
	if !c.pref.verbose {
		unsafe {
			goto start_no_time
		}
	}
	mut sw := time.new_stopwatch()
	start_no_time:
	// Track current file's module for function scope saving
	c.cur_file_module = file.mod
	// file_scope := new_scope(c.mod.scope)
	// mut mod_scope := new_scope(c.mod.scope)
	// c.env.scopes[file.mod] = mod_scope
	mut mod_scope := &Scope(unsafe { nil })
	lock c.env.scopes {
		if file.mod in c.env.scopes {
			mod_scope = unsafe { c.env.scopes[file.mod] }
		} else {
			panic('not found for mod: ${file.mod}')
		}
	}
	c.scope = mod_scope
	// mut mod_scope := c.env.scopes[file.mod] or {
	// 	panic('scope should exist')
	// }
	// c.scope = mod_scope
	for stmt in file.stmts {
		// if stmt is ast.Decl {
		match stmt {
			// Types and constants are pre-registered in preregister_all_types
			ast.ConstDecl, ast.EnumDecl, ast.InterfaceDecl, ast.StructDecl, ast.TypeDecl {
				continue
			}
			// Functions are pre-registered in preregister_all_fn_signatures
			ast.FnDecl {
				continue
			}
			else {
				c.decl(stmt)
			}
		}
		// }
	}
	for stmt in file.stmts {
		c.stmt(stmt)
	}
	if c.pref.verbose {
		check_time := sw.elapsed()
		println('type check ${file.name}: ${check_time.milliseconds()}ms (${check_time.microseconds()}µs)')
	}
}

pub fn (mut c Checker) preregister_scopes(file ast.File) {
	builtin_scope := c.get_module_scope('builtin', universe)

	mod_scope := c.get_module_scope(file.mod, builtin_scope)
	c.scope = mod_scope
	// add self (own module) for constants. can use own module prefix inside module
	c.scope.insert(file.mod, Module{
		name:  file.mod
		scope: c.get_module_scope(file.mod, builtin_scope)
	})
	// add imports
	for imp in file.imports {
		mod := if imp.is_aliased { imp.name.all_after_last('.') } else { imp.alias }
		c.scope.insert(imp.alias, Module{ name: mod, scope: c.get_module_scope(mod, builtin_scope) })
	}
	// add C
	c.scope.insert('C', Module{ name: 'C', scope: c.c_scope })
}

fn (mut c Checker) preregister_all_scopes(files []ast.File) {
	// builtin_scope := c.get_module_scope('builtin', universe)
	// preregister scopes & imports
	for file in files {
		c.preregister_scopes(file)
		// mod_scope := c.get_module_scope(file.mod, builtin_scope)
		// c.scope = mod_scope
		// // add self (own module) for constants
		// c.scope.insert(file.mod, Module{scope: c.get_module_scope(file.mod, builtin_scope)})
		// // add imports
		// for imp in file.imports {
		// 	mod :=  if imp.is_aliased { imp.name.all_after_last('.') } else { imp.alias }
		// 	c.scope.insert(imp.alias, Module{scope: c.get_module_scope(mod, builtin_scope)})
		// }
		// // add C
		// c.scope.insert('C', Module{scope: c.c_scope})
	}
}

pub fn (mut c Checker) preregister_types(file ast.File) {
	c.cur_file_module = file.mod
	mut mod_scope := &Scope(unsafe { nil })
	lock c.env.scopes {
		if file.mod in c.env.scopes {
			mod_scope = unsafe { c.env.scopes[file.mod] }
		} else {
			panic('scope should exist for mod: ${file.mod}')
		}
	}
	c.scope = mod_scope
	for stmt in file.stmts {
		// if stmt is ast.Decl {
		match stmt {
			ast.ConstDecl, ast.EnumDecl, ast.InterfaceDecl, ast.StructDecl, ast.TypeDecl {}
			else {
				continue
			}
		}
		c.decl(stmt)
		// }
	}
}

fn (mut c Checker) preregister_all_types(files []ast.File) {
	for file in files {
		c.preregister_types(file)
	}
	c.process_pending_interface_decls()
	c.process_pending_struct_decls()
	c.process_pending_type_decls()
}

// preregister_all_fn_signatures registers all function/method signatures
// before processing any function bodies. This ensures methods are available
// when checking code that calls them, regardless of file order.
fn (mut c Checker) preregister_all_fn_signatures(files []ast.File) {
	for file in files {
		c.preregister_fn_signatures(file)
	}
}

pub fn (mut c Checker) preregister_fn_signatures(file ast.File) {
	c.cur_file_module = file.mod
	mut mod_scope := &Scope(unsafe { nil })
	lock c.env.scopes {
		if file.mod in c.env.scopes {
			mod_scope = unsafe { c.env.scopes[file.mod] }
		} else {
			panic('scope should exist for mod: ${file.mod}')
		}
	}
	c.scope = mod_scope
	prev_collect := c.collect_fn_signatures_only
	c.collect_fn_signatures_only = true
	for stmt in file.stmts {
		// Only process function declarations
		if stmt is ast.FnDecl {
			c.decl(stmt)
		}
	}
	c.collect_fn_signatures_only = prev_collect
}

fn (mut c Checker) decl(decl ast.Stmt) {
	match decl {
		ast.ConstDecl {
			for field in decl.fields {
				// c.log('const decl: ${field.name}')
				mut int_val := 0
				if field.value is ast.BasicLiteral {
					if field.value.kind == .number {
						int_val = field.value.value.int()
					}
				}
				obj := Const{
					mod:     c.mod
					name:    field.name
					int_val: int_val
					// typ: c.expr(field.value)
				}
				c.scope.insert(obj.name, obj)
				c.pending_const_fields << PendingConstField{
					scope: c.scope
					field: field
				}
			}
		}
		ast.EnumDecl {
			// TODO: if non builtin types can be used as part of
			// the expr then these will need to get delayed also.
			mut fields := []Field{}
			for field in decl.fields {
				// Type-check enum field value expressions
				if field.value !is ast.EmptyExpr {
					c.expr(field.value)
				}
				fields << Field{
					name: field.name
				}
			}
			// as_type := decl.as_type !is ast.EmptyExpr { c.expr(decl.as_type) } else { Type(int_) }
			mut is_flag := decl.attributes.has('flag')
			obj := Enum{
				is_flag: is_flag
				name:    c.qualify_type_name(decl.name)
				fields:  fields
			}
			c.scope.insert(decl.name, object_from_type(Type(obj)))
		}
		ast.FnDecl {
			// if decl.typ.generic_params.len > 0 {
			// 	fn_decl := decl
			// 	c.later(fn[mut c, fn_decl]() {
			// 		c.fn_decl(fn_decl)
			// 	}, .fn_decl_generic)
			// } else {
			// 	c.fn_decl(decl)
			// }
			c.fn_decl(decl)
		}
		ast.GlobalDecl {
			for field in decl.fields {
				mut field_type := Type(int_)
				if field.typ !is ast.EmptyExpr {
					field_type = c.expr(field.typ)
				} else if field.value !is ast.EmptyExpr {
					field_type = c.expr(field.value)
				}
				obj := Global{
					name: field.name
					typ:  field_type
				}
				c.scope.insert(field.name, obj)
			}
		}
		ast.InterfaceDecl {
			// TODO:
			obj := Interface{
				name: c.qualify_type_name(decl.name)
			}
			c.scope.insert(decl.name, object_from_type(Type(obj)))
			c.pending_interface_decls << PendingInterfaceDecl{
				scope: c.scope
				decl:  decl
			}
		}
		ast.StructDecl {
			// c.log(' # StructDecl: ${decl.name}')
			// TODO: clean this up
			c.pending_struct_decls << PendingStructDecl{
				scope: c.scope
				decl:  decl
			}
			// c.log('struct decl: ${decl.name}')
			// Don't qualify C types
			qualified_name := if decl.language == .c {
				decl.name
			} else {
				c.qualify_type_name(decl.name)
			}
			obj := Struct{
				name: qualified_name
				// fields: [Field{name: 'len', typ: ast.Ident{name: 'int'}}]
				// fields: [Field{name: 'len'}]
			}
			mut typ := Type(obj)
			// TODO: proper
			if decl.language == .c {
				c.c_scope.insert(decl.name, object_from_type(typ))
			} else {
				c.scope.insert(decl.name, object_from_type(typ))
			}
		}
		ast.TypeDecl {
			// alias
			if decl.variants.len == 0 {
				alias_type := Alias{
					name: c.qualify_type_name(decl.name)
					// TODO: defer
					// parent: c.expr(decl.base_type)
				}
				mut typ := Type(alias_type)
				c.scope.insert(decl.name, object_from_type(typ))
				c.pending_type_decls << PendingTypeDecl{
					scope: c.scope
					decl:  decl
				}
			}
			// sum type
			else {
				sum_type := SumType{
					name: c.qualify_type_name(decl.name)
					// variants: decl.variants
				}
				mut typ := Type(sum_type)
				c.scope.insert(decl.name, object_from_type(typ))
				c.pending_type_decls << PendingTypeDecl{
					scope: c.scope
					decl:  decl
				}
			}
		}
		else {}
	}
}

fn (mut c Checker) check_types(exp_type Type, got_type Type) bool {
	// TODO: will this work with Primitive? might need to add comparison methods
	if got_type == exp_type {
		return true
	}
	// unwrap aliases for compatibility checks
	if exp_type is Alias {
		return c.check_types(exp_type.base_type, got_type)
	}
	if got_type is Alias {
		return c.check_types(exp_type, got_type.base_type)
	}
	// Treat all `string` spellings as equivalent:
	// builtin string type, legacy `struct string`, and aliases named `string`.
	if c.is_string_like(exp_type) && c.is_string_like(got_type) {
		return true
	}
	// Some paths rebuild semantically identical types as distinct objects.
	// If the fully rendered type names match exactly, treat them as compatible.
	if exp_type.name() == got_type.name() {
		return true
	}
	// allow nil in expression contexts that expect pointer-like values
	if got_type is Nil {
		match exp_type {
			FnType, Interface, Pointer {
				return true
			}
			else {}
		}
	}
	// number literals
	if exp_type.is_number() && got_type.is_number_literal() {
		return true
	}
	// primitives
	if exp_type is Primitive && got_type is Primitive {
		// mut got_props := got_type.props
		// got_props.clear(.untyped)
		// // checks if both match flot or int
		// if got_props == exp_type.props {
		// 	return true
		// }
	}
	// sum type: accept any variant type
	if exp_type is SumType {
		if got_type in exp_type.variants {
			return true
		}
	}

	return false
}

fn (c &Checker) is_string_struct(typ Type) bool {
	if typ is Struct {
		return typ.name == 'string' || typ.name.ends_with('__string')
	}
	return false
}

fn (c &Checker) is_string_like(typ Type) bool {
	return typ.name() == 'string' || c.is_string_struct(typ)
}

fn (mut c Checker) expr(expr ast.Expr) Type {
	typ := c.expr_impl(expr)
	// Store the computed type in the environment for expressions with positions
	pos := expr.pos()
	if pos.is_valid() {
		c.env.set_expr_type(pos.id, typ)
	}
	return typ
}

fn (mut c Checker) expr_impl(expr ast.Expr) Type {
	c.log('expr: ${expr.name()}')
	match expr {
		ast.ArrayInitExpr {
			// c.log('ArrayInit:')
			// Process len/cap/init field expressions
			if !is_empty_expr(expr.len) {
				c.expr(expr.len)
			}
			if !is_empty_expr(expr.cap) {
				c.expr(expr.cap)
			}
			// NOTE: expr.init is not processed here because it may contain
			// enum shorthands (.value) that require array element type context.
			// `[1,2,3,4]`
			if expr.exprs.len > 0 {
				is_fixed := !is_empty_expr(expr.len)
				// TODO: check all exprs
				first_elem_type := c.expr(expr.exprs.first())
				// NOTE: why did I have this shortcut here?
				// if expr.exprs.len == 1 {
				// 	if first_elem_type.is_number_literal() {
				// 		return Array{
				// 			elem_type: int_
				// 		}
				// 	}
				// }
				// TODO: promote [0] - proper
				expected_type_prev := c.expected_type
				mut expected_type := first_elem_type
				if exp_type := c.expected_type {
					expected_type = exp_type
				} else {
					// `[Type.value_a, .value_b]`
					// set expected type for checking `.value_b`
					if first_elem_type is Enum {
						c.expected_type = to_optional_type(first_elem_type)
					}
				}

				for i, elem_expr in expr.exprs {
					if i == 0 {
						continue
					}
					mut elem_type := c.expr(elem_expr)
					// TODO: best way to handle this?
					if elem_type.is_number_literal() && first_elem_type.is_number() {
						elem_type = first_elem_type
					}
					// sum type, check variants
					if (expected_type is SumType && elem_type !in expected_type.variants)
						&& !c.check_types(first_elem_type, elem_type) { // everything else
						// TODO: add general method for promotion/coercion
						c.error_with_pos('expecting element of type: ${first_elem_type.name()}, got ${elem_type.name()}',
							expr.pos)
					}
				}
				c.expected_type = expected_type_prev
				if is_fixed {
					arr_len := expr.exprs.len
					return ArrayFixed{
						len:       arr_len
						elem_type: first_elem_type
					}
				}
				return Array{
					elem_type: first_elem_type
				}
			}
			// `[]int{}`
			return c.expr(expr.typ)
		}
		ast.AsCastExpr {
			// TODO:
			c.expr(expr.expr)
			return c.expr(expr.typ)
		}
		ast.BasicLiteral {
			// c.log('ast.BasicLiteral: ${expr.kind.str()}: ${expr.value}')
			match expr.kind {
				.char {
					return Type(rune_)
				}
				.key_false, .key_true {
					return bool_
				}
				// TODO:
				.number {
					// TODO: had to be a better way to do this
					// should this be handled earlier? scanner?
					if expr.value.contains('.') {
						return float_literal_
					}
					return int_literal_
				}
				else {
					panic('invalid ast.BasicLiteral kind: ${expr.kind}')
				}
			}
		}
		ast.AssocExpr {
			typ := c.expr(expr.typ)

			expected_type_prev := c.expected_type
			c.expected_type = to_optional_type(typ)
			base_type := c.expr(expr.expr)
			c.expected_type = expected_type_prev

			// Base expression should match the assoc target type (allow pointers and sum types).
			mut base_check := base_type
			for base_check is Alias {
				base_check = (base_check as Alias).base_type
			}
			if base_check is Pointer {
				base_check = (base_check as Pointer).base_type
			}
			for base_check is Alias {
				base_check = (base_check as Alias).base_type
			}
			// Smart-casting does not apply to mutable variables. Use an immutable copy.
			final_base := base_check
			if final_base is SumType {
				sum_t := final_base as SumType
				if typ !in sum_t.variants {
					c.error_with_pos('expected base of type: ${typ.name()}, got ${base_type.name()}',
						expr.pos)
				}
			} else if !c.check_types(typ, final_base) {
				c.error_with_pos('expected base of type: ${typ.name()}, got ${base_type.name()}',
					expr.pos)
			}

			if typ is Struct {
				for field in expr.fields {
					expected_type_prev2 := c.expected_type
					mut field_type := Type(void_)
					mut found := false
					for sf in typ.fields {
						if sf.name == field.name {
							field_type = sf.typ
							found = true
							break
						}
					}
					if !found {
						c.error_with_pos('unknown field `${field.name}` for type `${typ.name()}`',
							expr.pos)
					}
					c.expected_type = to_optional_type(field_type)
					got_type := c.expr(field.value)
					c.expected_type = expected_type_prev2
					if !c.check_types(field_type, got_type) {
						c.error_with_pos('expected field `${field.name}` of type: ${field_type.name()}, got ${got_type.name()}',
							expr.pos)
					}
				}
			} else {
				// Still visit values to populate env types for later passes.
				for field in expr.fields {
					c.expr(field.value)
				}
			}
			return typ
		}
		ast.CallOrCastExpr {
			// c.log('CallOrCastExpr: ${lhs_type.name()}')
			// lhs_type := c.expr(expr.lhs)
			// // call
			// if lhs_type is FnType {
			// 	return c.call_expr(ast.CallExpr{lhs: expr.lhs, args: [expr.expr]})
			// }
			// // cast
			// // expr_type := c.expr(expr.expr)
			// // TODO: check if expr_type can be cast to lhs_type
			// return lhs_type
			return c.expr(c.resolve_call_or_cast_expr(expr))
		}
		ast.CallExpr {
			// TODO/FIXME:
			// we need a way to handle C.stat|sigaction() / C.stat|sigaction{}
			// multiple items with same name inside scope lookup.
			if expr.lhs is ast.SelectorExpr {
				if expr.lhs.rhs.name == 'stat' {
					return int_
				}
			}
			return c.call_expr(expr)
		}
		ast.CastExpr {
			typ := c.expr(expr.typ)
			c.expr(expr.expr)
			c.log('CastExpr: ${typ.name()}')
			return typ
		}
		ast.ComptimeExpr {
			cexpr := c.resolve_expr(expr.expr)
			// TODO: move to checker, where `ast.*Or*` nodes will be resolved.
			if cexpr !is ast.CallExpr && cexpr !is ast.IfExpr {
				c.error_with_pos('unsupported comptime: ${cexpr.name()}', expr.pos)
			}
			// Handle compile-time $if/$else - evaluate condition and check only the matching branch
			if cexpr is ast.IfExpr {
				c.comptime_if_else(cexpr)
				return Type(void_)
			}
			c.log('ComptimeExpr: ' + cexpr.name())
			// return c.expr(cexpr)
		}
		ast.EmptyExpr {
			// TODO:
			return Type(void_)
		}
		ast.FnLiteral {
			return c.fn_type(expr.typ, FnTypeAttribute.empty)
		}
		ast.GenericArgs {
			// NOTE: first generic args handled in CallExpr
			// this is generic struct nested in generic args
			mut name := ''
			match expr.lhs {
				ast.Ident {
					name = expr.lhs.name
				}
				else {}
			}
			mut args := []string{}
			for arg in expr.args {
				if arg is ast.Ident {
					args << arg.name
				}
			}
			lhs_type := c.expr(expr.lhs)
			if lhs_type is FnType {
				fn_type := lhs_type as FnType
				return Type(with_generic_params(fn_type, args))
			}
			// If the LHS is indexable (array, map, string), re-interpret as IndexExpr.
			// The parser sometimes produces nested GenericArgs for nested indexing
			// like `a[b[c[d]]]` when it should produce nested IndexExprs.
			if expr.args.len == 1 && c.is_indexable_type(lhs_type) {
				return c.expr(ast.Expr(ast.IndexExpr{
					lhs:  expr.lhs
					expr: expr.args[0]
					pos:  expr.pos
				}))
			}

			return Struct{
				name:           name
				generic_params: args
			}
		}
		ast.GenericArgOrIndexExpr {
			return c.expr(c.resolve_generic_arg_or_index_expr(expr))
		}
		ast.Ident {
			// c.log('ident: ${expr.name}')
			obj := c.ident(expr)
			typ := obj.typ()
			// TODO:
			if expr.name == 'string' {
				if typ is Struct {
					return Type(string_)
				}
			}
			return typ
		}
		ast.IfExpr {
			// if guard
			if expr.cond is ast.IfGuardExpr {
				c.open_scope()
				c.assign_stmt(expr.cond.stmt, true)
				c.stmt_list(expr.stmts)
				c.close_scope()
				if expr.else_expr !is ast.EmptyExpr {
					c.expr(expr.else_expr)
				}
				// TODO: never used, add a special type?
				return Type(void_)
			}

			// normal if
			return c.if_expr(expr)
		}
		ast.IfGuardExpr {
			c.assign_stmt(expr.stmt, true)
			// TODO:
			return bool_
		}
		ast.IndexExpr {
			lhs_type := c.expr(expr.lhs)
			c.expr(expr.expr)
			// TODO: make sure lhs_type is indexable
			// if !lhs_type.is_indexable() { c.error('cannot index ${lhs_type.name()}') }
			// For slicing (RangeExpr), return the same type as the container
			// For single index, return the element type
			is_range := expr.expr is ast.RangeExpr
			mut container_type := lhs_type
			// Const/global strings can be represented as pointer-to-string in some paths.
			// For direct indexing/slicing expressions, treat them as plain strings.
			mut lhs_check := lhs_type
			for lhs_check is Alias {
				lhs_check = (lhs_check as Alias).base_type
			}
			if lhs_check is Pointer {
				mut ptr_base := (lhs_check as Pointer).base_type
				for ptr_base is Alias {
					ptr_base = (ptr_base as Alias).base_type
				}
				if ptr_base is String {
					is_explicit_deref := expr.lhs is ast.PrefixExpr
						&& (expr.lhs as ast.PrefixExpr).op == .mul
					if !is_explicit_deref {
						container_type = Type(string_)
					}
				}
			}
			value_type := if is_range {
				container_type
			} else {
				container_type.value_type()
			}
			// c.log('IndexExpr: ${value_type.name()} / ${lhs_type.name()}')
			return value_type
		}
		ast.InfixExpr {
			lhs_type := c.expr(expr.lhs)
			// Preserve enum context for shorthand RHS values like `.void_t`.
			expected_type := c.expected_type
			if lhs_type is Enum {
				c.expected_type = to_optional_type(Type(lhs_type))
			}
			if expr.op == .and {
				// In `a is T && a.field ...`, RHS is evaluated only when the smart-cast is true.
				// Type-check RHS in a nested scope with casts from LHS applied.
				c.open_scope()
				sc_names, sc_types := c.extract_smartcasts(expr.lhs)
				c.apply_smartcasts(sc_names, sc_types)
				c.expr(expr.rhs)
				c.close_scope()
			} else {
				c.expr(expr.rhs)
			}
			c.expected_type = expected_type
			if expr.op.is_comparison() {
				return bool_
			}
			return lhs_type
		}
		ast.InitExpr {
			// TODO: try handle this from expr
			// mut typ_expr := expr.typ
			// if expr.typ is ast.GenericArgs {
			// 	typ_expr = expr.typ.lhs
			// }
			typ := c.expr(expr.typ)
			// Visit field value expressions to store their types in the environment
			if typ is Struct {
				for field in expr.fields {
					if field.value !is ast.EmptyExpr {
						expected_type_prev := c.expected_type
						for sf in typ.fields {
							if sf.name == field.name {
								c.expected_type = to_optional_type(sf.typ)
								break
							}
						}
						c.expr(field.value)
						c.expected_type = expected_type_prev
					}
				}
			} else {
				for field in expr.fields {
					if field.value !is ast.EmptyExpr {
						c.expr(field.value)
					}
				}
			}
			return typ
		}
		ast.KeywordOperator {
			// TODO:
			typ := c.expr(expr.exprs[0])
			match expr.op {
				.key_go, .key_spawn {
					return Thread{}
				}
				.key_typeof {
					// typeof(expr) returns a comptime TypeInfo with .name and .idx
					return Struct{
						name:   'TypeInfo'
						fields: [
							Field{
								name: 'name'
								typ:  string_
							},
							Field{
								name: 'idx'
								typ:  int_
							},
						]
					}
				}
				.key_sizeof, .key_isreftype {
					return int_
				}
				else {
					return typ
				}
			}
		}
		ast.MapInitExpr {
			// TODO: type check keys/vals
			// `map[type]type{}`
			if expr.typ !is ast.EmptyExpr {
				typ := c.expr(expr.typ)
				return typ
			}
			// `{}`
			if expr.keys.len == 0 {
				if exp_type := c.expected_type {
					return exp_type
				}
				c.error_with_pos('empty map {} used in unsupported context', expr.pos)
				return Type(void_)
			}
			// `{key: value}`
			key0_type := c.expr(expr.keys[0])
			value0_type := c.expr(expr.vals[0])
			return Map{
				key_type:   key0_type
				value_type: value0_type
			}
		}
		ast.MatchExpr {
			return c.match_expr(expr, true)
		}
		ast.ModifierExpr {
			// if expr.expr !is ast.Ident && expr.expr !is ast.Type {
			// 	panic('not ident: ${expr.expr.type_name()}')
			// }
			return c.expr(expr.expr)
		}
		ast.OrExpr {
			cond := c.resolve_expr(expr.expr)
			cond_type := c.expr(cond).unwrap()
			// c.log('OrExpr: ${cond_type.name()}')
			if expr.stmts.len > 0 {
				mut err_type := Type(string_)
				if obj := c.scope.lookup_parent('IError', 0) {
					err_type = obj.typ()
				}
				c.open_scope()
				c.scope.insert('err', object_from_type(err_type))
				c.scope.insert('errcode', object_from_type(Type(int_)))
				last_stmt := expr.stmts.last()
				if last_stmt is ast.ExprStmt {
					expr_stmt_type := c.expr(last_stmt.expr).unwrap()
					c.close_scope()
					// c.log('OrExpr: last_stmt_type: ${cond_type.name()}')
					// TODO: non returning call (currently just checking void)
					// should probably lookup function/method and check for noreturn attribute?
					// if cond is ast.CallExpr {}
					// do we need to do promotion here

					// last stmt expr does does not return a type
					// None is always valid in or-blocks (propagates the error)
					if expr_stmt_type !is Void && expr_stmt_type !is None
						&& !c.check_types(cond_type, expr_stmt_type) {
						c.error_with_pos('or expr expecting ${cond_type.name()}, got ${expr_stmt_type.name()}',
							expr.pos)
					}
					return cond_type
				}
				c.stmt_list(expr.stmts)
				c.close_scope()
			}
			return cond_type
		}
		ast.ParenExpr {
			return c.expr(expr.expr)
		}
		ast.PostfixExpr {
			typ := c.expr(expr.expr)
			// The `!` operator (error propagation) unwraps result/option types
			if expr.op == .not {
				return typ.unwrap()
			}
			return typ
		}
		ast.PrefixExpr {
			expr_type := c.expr(expr.expr)
			if expr.op == .amp {
				return Pointer{
					base_type: expr_type
				}
			} else if expr.op == .mul {
				if expr_type is Pointer {
					// c.log('DEREF')
					return expr_type.base_type
				} else if expr_type is Interface {
					// Interface types are internally pointers, so dereference is valid
					// This handles match narrowing where the concrete type is still behind a pointer
					return expr_type
				} else if expr_type is Struct {
					// Allow deref on structs narrowed from interfaces in match expressions
					// TODO: properly track interface narrowing instead of this workaround
					return expr_type
				} else {
					c.error_with_pos('deref on non pointer type `${expr_type.name()}`',
						expr.pos)
				}
			} else if expr.op == .arrow {
				// Channel receive: <-ch returns the channel's element type
				if expr_type is Channel {
					if elem_type := expr_type.elem_type {
						return elem_type
					}
				}
				return Type(void_)
			}
			return c.expr(expr.expr)
		}
		ast.RangeExpr {
			start_type := c.expr(expr.start)
			c.expr(expr.end)
			return Type(Array{
				elem_type: start_type
			})
		}
		ast.SelectExpr {
			c.stmt_list(expr.stmts)
		}
		ast.SelectorExpr {
			// enum value: `.green`
			if expr.lhs is ast.EmptyExpr {
				// c.log('got enum value')
				// // dump(expr)
				// return c.expected_type
				if exp_type := c.expected_type {
					return exp_type
				}
				c.error_with_pos('c.expected_type is not set', expr.pos)
				return Type(void_)

				// return int_
			}
			// normal selector
			return c.selector_expr(expr)
		}
		ast.StringInterLiteral {
			for inter in expr.inters {
				c.expr(inter.expr)
			}
			return Type(string_)
		}
		ast.StringLiteral {
			// C strings (c'...' or c"...") return charptr
			if expr.kind == .c {
				return charptr_
			}
			return Type(string_)
		}
		ast.Tuple {
			mut types := []Type{}
			for x in expr.exprs {
				types << c.expr(x)
			}
			return Tuple{
				types: types
			}
		}
		ast.Type {
			return c.type_node_expr(expr)
		}
		ast.UnsafeExpr {
			// TODO: proper
			c.stmt_list(expr.stmts)
			last_stmt := expr.stmts.last()
			if last_stmt is ast.ExprStmt {
				return c.expr(last_stmt.expr)
			}
			// TODO: impl: avoid returning types everywhere / using void
			// perhaps use a struct and set the type and other info in it when needed
			return Type(void_)
		}
		ast.LockExpr {
			// Lock expression - check statements and return last expression type
			c.stmt_list(expr.stmts)
			if expr.stmts.len > 0 {
				last_stmt := expr.stmts.last()
				if last_stmt is ast.ExprStmt {
					return c.expr(last_stmt.expr)
				}
			}
			return Type(void_)
		}
		else {}
	}
	// TODO: remove (add all variants)
	c.log('expr: unhandled ${expr.name()}')
	return int_
}

fn (mut c Checker) type_node_expr(type_expr ast.Type) Type {
	if type_expr is ast.ArrayType {
		arr_type := type_expr as ast.ArrayType
		return Array{
			elem_type: c.expr(arr_type.elem_type)
		}
	}
	if type_expr is ast.ArrayFixedType {
		arr_fixed_type := type_expr as ast.ArrayFixedType
		mut len := 0
		if arr_fixed_type.len is ast.BasicLiteral {
			if arr_fixed_type.len.kind == .number {
				len = arr_fixed_type.len.value.int()
			}
		} else if arr_fixed_type.len is ast.Ident {
			if obj := c.scope.lookup_parent(arr_fixed_type.len.name, 0) {
				if obj is Const {
					len = obj.int_val
				}
			}
		}
		return ArrayFixed{
			len:       len
			elem_type: c.expr(arr_fixed_type.elem_type)
		}
	}
	if type_expr is ast.ChannelType {
		ch_type := type_expr as ast.ChannelType
		if ch_type.elem_type !is ast.EmptyExpr {
			return Channel{
				elem_type: to_optional_type(c.expr(ch_type.elem_type))
			}
		}
		return Channel{}
	}
	if type_expr is ast.FnType {
		fn_type := type_expr as ast.FnType
		return c.fn_type(fn_type, FnTypeAttribute.empty)
	}
	if type_expr is ast.GenericType {
		gen_type := type_expr as ast.GenericType
		return c.expr(gen_type.name)
	}
	if type_expr is ast.MapType {
		map_type := type_expr as ast.MapType
		return Map{
			key_type:   c.expr(map_type.key_type)
			value_type: c.expr(map_type.value_type)
		}
	}
	if type_expr is ast.NilType {
		return Type(nil_)
	}
	if type_expr is ast.NoneType {
		return Type(none_)
	}
	if type_expr is ast.OptionType {
		opt_type := type_expr as ast.OptionType
		return OptionType{
			base_type: c.expr(opt_type.base_type)
		}
	}
	if type_expr is ast.ResultType {
		res_type := type_expr as ast.ResultType
		return ResultType{
			base_type: c.expr(res_type.base_type)
		}
	}
	if type_expr is ast.ThreadType {
		thread_type := type_expr as ast.ThreadType
		if thread_type.elem_type !is ast.EmptyExpr {
			return Thread{
				elem_type: to_optional_type(c.expr(thread_type.elem_type))
			}
		}
		return Thread{}
	}
	if type_expr is ast.TupleType {
		tuple_type := type_expr as ast.TupleType
		mut types := []Type{}
		for tx in tuple_type.types {
			types << c.expr(tx)
		}
		return Tuple{
			types: types
		}
	}
	c.log('expr.Type: unhandled ${type_expr.type_name()}')
	return int_
}

fn (mut c Checker) stmt(stmt ast.Stmt) {
	match stmt {
		ast.AssertStmt {
			c.expr(stmt.expr)
			if stmt.extra !is ast.EmptyExpr {
				c.expr(stmt.extra)
			}
		}
		ast.AssignStmt {
			c.assign_stmt(stmt, false)
		}
		ast.BlockStmt {
			c.stmt_list(stmt.stmts)
		}
		// ast.Decl {
		// 	// Handled earlier
		// 	// match stmt {
		// 	// 	ast.FnDecl{}
		// 	// 	ast.TypeDecl {}
		// 	// 	else {}
		// 	// }
		// }
		ast.GlobalDecl {
			for field in stmt.fields {
				// c.log('GlobalDecl: ${field.name} - ${obj.typ.type_name()}')
				field_type := if field.typ !is ast.EmptyExpr {
					c.expr(field.typ)
				} else {
					c.expr(field.value)
				}
				obj := Global{
					name: field.name
					typ:  field_type
				}
				c.scope.insert(field.name, obj)
			}
		}
		ast.DeferStmt {
			c.stmt_list(stmt.stmts)
		}
		ast.ExprStmt {
			if stmt.expr is ast.MatchExpr {
				c.match_expr(stmt.expr, false)
			} else {
				c.expr(stmt.expr)
			}
		}
		ast.ForStmt {
			c.open_scope()
			// TODO: vars for other for loops
			if stmt.init is ast.ForInStmt {
				expr_type := c.expr(stmt.init.expr)
				if stmt.init.key is ast.Ident {
					// TODO: remove
					if expr_type is Void {
						c.scope.print(false)
						// dump(stmt)
						panic('## expr_type is Void!')
					}
					key_type := expr_type.key_type()
					c.scope.insert(stmt.init.key.name, object_from_type(key_type))
					if c.fn_root_scope != unsafe { nil } && c.fn_root_scope != c.scope {
						root_key_obj := object_from_type(key_type)
						c.fn_root_scope.objects[stmt.init.key.name] = root_key_obj
					}
					// Store key type in expr_types
					if stmt.init.key.pos.is_valid() {
						c.env.set_expr_type(stmt.init.key.pos.id, key_type)
					}
				}
				mut value_type := expr_type.value_type()
				// For iterator structs (e.g. RunesIterator), value_type() returns
				// the struct itself. The transformer lowers these to direct iteration,
				// so don't flatten the stale iterator type to fn_root_scope.
				is_iterator_struct := value_type is Struct
				if stmt.init.value is ast.ModifierExpr {
					// Store the non-ref type for fn_root_scope because the transformer
					// lowers mutable for-in loops to indexed access with value copies.
					non_ref_value_type := value_type
					if stmt.init.value.kind == .key_mut {
						value_type = Type(value_type.ref())
					}
					if stmt.init.value.expr is ast.Ident {
						c.scope.insert(stmt.init.value.expr.name, object_from_type(value_type))
						if !is_iterator_struct && c.fn_root_scope != unsafe { nil }
							&& c.fn_root_scope != c.scope {
							root_value_obj := object_from_type(non_ref_value_type)
							c.fn_root_scope.objects[stmt.init.value.expr.name] = root_value_obj
						}
						// Store value type in expr_types (Ident inside ModifierExpr)
						if stmt.init.value.expr.pos.is_valid() {
							c.env.set_expr_type(stmt.init.value.expr.pos.id, value_type)
						}
					}
					// Store ModifierExpr type too
					if stmt.init.value.pos.is_valid() {
						c.env.set_expr_type(stmt.init.value.pos.id, value_type)
					}
				} else if stmt.init.value is ast.Ident {
					c.scope.insert(stmt.init.value.name, object_from_type(value_type))
					if !is_iterator_struct && c.fn_root_scope != unsafe { nil }
						&& c.fn_root_scope != c.scope {
						root_value_obj := object_from_type(value_type)
						c.fn_root_scope.objects[stmt.init.value.name] = root_value_obj
					}
					// Store value type in expr_types
					if stmt.init.value.pos.is_valid() {
						c.env.set_expr_type(stmt.init.value.pos.id, value_type)
					}
				}
			} else {
				if stmt.cond !is ast.EmptyExpr {
					sc_names, sc_types := c.extract_smartcasts(stmt.cond)
					c.apply_smartcasts(sc_names, sc_types)
				}
				// c.stmt(stmt.init)
			}
			// sc_names, sc_types := c.extract_smartcasts(stmt.cond)
			// c.apply_smartcasts(sc_names, sc_types)
			c.stmt(stmt.init)
			c.expr(stmt.cond)
			c.stmt(stmt.post)
			c.stmt_list(stmt.stmts)
			c.close_scope()
		}
		ast.ImportStmt {
			// c.log('import: ${stmt.name} as ${stmt.alias}')
		}
		// ast.FnDecl - handled by preregister_all_fn_signatures / process_pending_fn_bodies
		ast.ReturnStmt {
			c.log('ReturnStmt:')
			for expr in stmt.exprs {
				c.expr(expr)
			}
		}
		ast.ComptimeStmt {
			c.stmt(stmt.stmt)
		}
		ast.LabelStmt {
			c.stmt(stmt.stmt)
		}
		else {}
	}
}

fn (mut c Checker) stmt_list(stmts []ast.Stmt) {
	for stmt in stmts {
		c.stmt(stmt)
	}
}

fn (mut c Checker) process_pending_const_fields() {
	for pending in c.pending_const_fields {
		c.scope = pending.scope
		const_type := c.expr(pending.field.value)
		mut scope := pending.scope
		if mut cd := scope.lookup(pending.field.name) {
			if mut cd is Const {
				cd.typ = const_type.typed_default()
			}
		}
	}
	c.pending_const_fields.clear()
}

fn (mut c Checker) process_pending_interface_decls() {
	for pending in c.pending_interface_decls {
		c.scope = pending.scope
		mut fields := []Field{}
		mut has_type_name := false
		for field in pending.decl.fields {
			if field.name == 'type_name' {
				has_type_name = true
			}
			fields << Field{
				name:         field.name
				typ:          c.expr(field.typ)
				default_expr: field.value
			}
		}
		if !has_type_name {
			fields << Field{
				name: 'type_name'
				typ:  Type(fn_with_return_type(FnType{}, String(0)))
			}
		}
		mut scope := pending.scope
		if mut id := scope.lookup(pending.decl.name) {
			if mut id is Type {
				if mut id is Interface {
					id.fields = fields
				}
			}
		}
	}
	c.pending_interface_decls.clear()
}

fn (mut c Checker) process_pending_struct_decls() {
	for pending in c.pending_struct_decls {
		c.scope = pending.scope
		// Insert generic type parameters into scope so field types can reference them
		for gp in pending.decl.generic_params {
			gp_name := if gp is ast.Ident { gp.name } else { '' }
			if gp_name != '' {
				c.scope.insert(gp_name, Type(NamedType(gp_name)))
			}
		}
		mut fields := []Field{}
		for field in pending.decl.fields {
			field_typ := c.expr(field.typ)
			fields << Field{
				name:         field.name
				typ:          field_typ
				default_expr: field.value
			}
		}
		mut embedded := []Struct{}
		for embedded_expr in pending.decl.embedded {
			embedded_type := c.expr(embedded_expr)
			if embedded_type is Struct {
				embedded << embedded_type
			} else {
				c.error_with_pos('can only structs, `${embedded_type.name()}` is not a struct.',
					pending.decl.pos)
			}
		}
		mut update_scope := if pending.decl.language == .c { c.c_scope } else { pending.scope }
		if mut sd := update_scope.lookup(pending.decl.name) {
			if mut sd is Type {
				if mut sd is Struct {
					sd.fields = fields
					sd.embedded = embedded
				}
			}
		}
	}
	c.pending_struct_decls.clear()
}

fn (mut c Checker) process_pending_type_decls() {
	for pending in c.pending_type_decls {
		c.scope = pending.scope
		mut scope := pending.scope
		if pending.decl.variants.len == 0 {
			if mut obj := scope.lookup(pending.decl.name) {
				if mut obj is Type {
					if mut obj is Alias {
						obj.base_type = c.expr(pending.decl.base_type)
					}
				}
			}
			continue
		}
		if mut obj := scope.lookup(pending.decl.name) {
			if mut obj is Type {
				if mut obj is SumType {
					for variant in pending.decl.variants {
						// Keep the inferred variant type in a local first so generated C
						// does not take the address of a temporary expression result.
						variant_type := c.expr(variant)
						obj.variants << variant_type
					}
				}
			}
		}
	}
	c.pending_type_decls.clear()
}

fn (mut c Checker) process_pending_fn_bodies() {
	// Use index-based loop to handle nested FnDecls that get added during processing.
	mut i := 0
	for i < c.pending_fn_bodies.len {
		c.check_pending_fn_body(c.pending_fn_bodies[i])
		i++
	}
	c.pending_fn_bodies.clear()
}

fn (mut c Checker) check_pending_fn_body(pending PendingFnBody) {
	if pending.decl.typ.generic_params.len > 0 {
		mut generic_types := []map[string]Type{}
		mut has_generic_types := false
		for name, inferred in c.env.generic_types {
			if name == pending.decl.name {
				generic_types = inferred.clone()
				has_generic_types = true
				break
			}
		}
		if !has_generic_types {
			// Skip checking generic functions that are never instantiated.
			return
		}
		c.env.cur_generic_types << generic_types
	}
	if pending.typ.generic_params.len == 0
		|| (pending.typ.generic_params.len > 0 && c.env.cur_generic_types.len > 0) {
		prev_scope := c.scope
		prev_fn_root_scope := c.fn_root_scope
		c.scope = pending.scope
		c.fn_root_scope = pending.scope
		expected_type := c.expected_type
		c.expected_type = pending.typ.return_type
		c.stmt_list(pending.decl.stmts)
		c.expected_type = expected_type
		c.fn_root_scope = prev_fn_root_scope
		c.env.set_fn_scope(pending.module_name, pending.scope_fn_name, pending.scope)
		c.scope = prev_scope
	}
	c.env.cur_generic_types = []
}

// check_struct_field_defaults visits struct field default expressions after all
// function signatures and bodies are registered, so function calls in defaults resolve.
fn (mut c Checker) check_struct_field_defaults(files []ast.File) {
	for file in files {
		mut mod_scope := &Scope(unsafe { nil })
		lock c.env.scopes {
			if file.mod in c.env.scopes {
				mod_scope = unsafe { c.env.scopes[file.mod] }
			} else {
				continue
			}
		}
		c.scope = mod_scope
		c.cur_file_module = file.mod
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				for field in stmt.fields {
					if field.value !is ast.EmptyExpr {
						field_typ := c.expr(field.typ)
						prev_expected := c.expected_type
						c.expected_type = to_optional_type(field_typ)
						c.expr(field.value)
						c.expected_type = prev_expected
					}
				}
			}
		}
	}
}

// check_enum_field_values visits enum field value expressions.
fn (mut c Checker) check_enum_field_values(files []ast.File) {
	for file in files {
		mut mod_scope := &Scope(unsafe { nil })
		lock c.env.scopes {
			if file.mod in c.env.scopes {
				mod_scope = unsafe { c.env.scopes[file.mod] }
			} else {
				continue
			}
		}
		c.scope = mod_scope
		c.cur_file_module = file.mod
		for stmt in file.stmts {
			if stmt is ast.EnumDecl {
				for field in stmt.fields {
					if field.value !is ast.EmptyExpr {
						c.expr(field.value)
					}
				}
			}
		}
	}
}

// take_deferred is kept for compatibility with parallel type-checking plumbing.
pub fn (mut c Checker) take_deferred() []Deferred {
	return []Deferred{}
}

// add_deferred is kept for compatibility with parallel type-checking plumbing.
pub fn (mut c Checker) add_deferred(items []Deferred) {
	_ = items
}

// process_struct_deferred is kept for compatibility with parallel type-checking plumbing.
pub fn (mut c Checker) process_struct_deferred() {
	c.process_pending_interface_decls()
	c.process_pending_struct_decls()
	c.process_pending_type_decls()
}

// process_all_deferred is kept for compatibility with parallel type-checking plumbing.
pub fn (mut c Checker) process_all_deferred() {
	c.process_struct_deferred()
	c.process_pending_const_fields()
	c.process_pending_fn_bodies()
}

fn (mut c Checker) assign_stmt(stmt ast.AssignStmt, unwrap_optional bool) {
	for i, lx in stmt.lhs {
		// TODO: proper / tuple (handle multi return)
		mut rx := stmt.rhs[0]
		if i < stmt.rhs.len {
			rx = stmt.rhs[i]
		}
		// lhs_type := c.scope.lookup_parent
		// TODO: ident field for blank ident?
		mut is_blank_ident := false
		if lx is ast.Ident {
			is_blank_ident = lx.name == '_'
		}
		mut lhs_type := Type(void_)
		if stmt.op != .decl_assign && !is_blank_ident {
			lhs_type = c.expr(lx)
		}
		expected_type := c.expected_type
		if lhs_type is Enum {
			c.expected_type = to_optional_type(Type(lhs_type))
		}
		rhs_type := c.expr(rx)
		// if t := expected_type {
		// 	c.log('AssignStmt: setting expected_type to: ${t.name()}')
		// } else {
		// 	c.log('AssignStmt: setting expected_type to: none')
		// }
		c.expected_type = expected_type
		// c.expected_type = none
		mut expr_type := rhs_type
		if unwrap_optional {
			expr_type = expr_type.unwrap()
		}
		if rhs_type is Tuple {
			expr_type = rhs_type.types[i]
		}
		// promote untyped literals
		expr_type = expr_type.typed_default()
		// TODO: assignment check
		// if stmt.op != .decl_assign {
		// 	c.assignment(expr_type, lhs_type) or {
		// 		c.log('error!!')
		// 		c.error_with_pos(err.msg(), stmt.pos)
		// 	}
		// }
		// TODO: proper
		// TODO: modifiers, lx_unwrapped := c.unwrap...
		// or some method to use modifiers
		lx_unwrapped := c.unwrap_expr(lx)
		if lx_unwrapped is ast.Ident {
			c.scope.insert(lx_unwrapped.name, object_from_type(expr_type))
			// Also insert into function root scope for transformer type lookups
			// This flattens nested scope variables into the function scope
			if c.fn_root_scope != unsafe { nil } && c.fn_root_scope != c.scope {
				c.fn_root_scope.insert(lx_unwrapped.name, object_from_type(expr_type))
			}
			// Store the type in expr_types for the lhs ident position
			// so the transformer can look it up directly
			lx_pos := lx_unwrapped.pos
			if lx_pos.is_valid() {
				c.env.set_expr_type(lx_pos.id, expr_type)
			}
		}
		// Also store the type for ModifierExpr-wrapped idents (e.g., `mut x := ...`)
		if lx is ast.ModifierExpr {
			lx_mod_pos := lx.pos
			if lx_mod_pos.is_valid() {
				c.env.set_expr_type(lx_mod_pos.id, expr_type)
			}
		}
	}
}

// TODO:
// fn (mut c Checker) assignment(lx ast.Expr, typ Type) {
fn (mut c Checker) assignment(from_type Type, to_type Type) ! {
	// same type
	if from_type == to_type {
		return
	}
	// numbers literals
	if from_type.is_number_literal() && to_type.is_number() {
		return
	}
	// aliases
	if from_type is Alias {
		return c.assignment(from_type.base_type, to_type)
	}
	if to_type is Alias {
		return c.assignment(from_type, to_type.base_type)
	}
	// TODO: provide context about if inside unsafe
	if to_type is FnType {
		// allow nil to fn pointer
		if from_type is Nil {
			return
		}
	}
	// pointers
	if to_type is Pointer {
		if from_type is Nil {
			return
		}
		if from_type.is_number() {
			return
		}
		// same
		if from_type is Pointer {
			// allow assigning &void to any pointer
			if from_type.base_type is Void {
				return
			}
			//
			if from_type.base_type.is_number() {
				return
			}
			// return c.assignment(from_type.base_type, to_type.base_type)!
			if from_type.base_type == to_type.base_type {
				return
			}
		}
	}
	if to_type.is_number() {
		// for now all all numvers to be compatible
		if from_type.is_number() {
			return
		}
		// TODO: since char is currently its own type, should this be changed?
		if from_type is Char {
			return
		}
	}
	// // dump(from_type)
	// // dump(to_type)
	return error('cannot assign `${from_type.name()}` to `${to_type.name()}`')
}

// fn (mut c Checker) implicit_type(from_type Type, to_type Type) !Type {
// 	if from_type.is_number_literal() && to_type.is_numver() {
// 		return to_type
// 	}
// }

fn (mut c Checker) block(stmts []ast.Stmt) {
}

fn (mut c Checker) apply_smartcast(sc_name_ ast.Expr, sc_type Type) {
	sc_name := c.unwrap_ident(sc_name_)
	if sc_name is ast.Ident {
		// println('added smartcast for ${sc_name.name} to ${sc_type.name()}')
		c.scope.insert(sc_name.name, object_from_type(sc_type))
	} else if sc_name is ast.SelectorExpr {
		// field := c.selector_expr(sc_name)
		// if sc_name.lhs is ast.Ident {
		// 	field := c.find_field_or_method()
		// 	c.scope.insert(sc_name.lhs.name, SmartCastSelector{origin: field, field: sc_name.rhs.name, cast_type: sc_type})
		// }
		// c.log('@@ selector smartcast: ${sc_name.name()} - ${field.type_name()}')
		// println('added smartcast for ${sc_name.name()} to ${sc_type.name()}')
		smartcast_name := sc_name.name()
		c.scope.field_smartcasts[smartcast_name] = sc_type
	}
}

fn (mut c Checker) apply_smartcasts(sc_names []ast.Expr, sc_types []Type) {
	for i, sc_name in sc_names {
		c.apply_smartcast(sc_name, sc_types[i])
	}
}

fn (mut c Checker) extract_smartcasts(expr ast.Expr) ([]ast.Expr, []Type) {
	mut names := []ast.Expr{}
	mut types := []Type{}
	expr_u := c.unwrap_expr(expr)
	if expr_u is ast.InfixExpr {
		if expr_u.op == .key_is {
			// eprintln('adding smartcast')
			names << expr_u.lhs
			types << c.expr(c.unwrap_expr(expr_u.rhs))
			// typ := c.expr(expr_u.rhs)
			// types << typ
		} else if expr_u.op == .and {
			lhs_names, lhs_types := c.extract_smartcasts(c.unwrap_expr(expr_u.lhs))
			rhs_names, rhs_types := c.extract_smartcasts(c.unwrap_expr(expr_u.rhs))
			names << lhs_names
			types << lhs_types
			names << rhs_names
			types << rhs_types
		}
	}
	// else if cond is ast.ParenExpr {
	// 	names2, types2 := c.extract_smartcasts(cond.expr)
	// 	names << names2
	// 	types << types2
	// }
	return names, types
}

fn (mut c Checker) fn_decl(decl ast.FnDecl) {
	// c.log('ast.FnDecl: ${decl.name}: ${c.file.name}')
	// c.log('return type:')
	// c.expr(decl.typ.return_type)
	mut prev_scope := c.scope
	c.open_scope()
	// mut fn_scope := c.scope
	// if decl.typ.generic_params.len > 0 {
	// 	eprintln('## GENERIC FN DECL: ${decl.name}')
	// }
	fn_typ := c.fn_type(decl.typ, FnTypeAttribute.from_ast_attributes(decl.attributes))
	mut typ := Type(fn_typ)
	// Heap-allocate Fn so pointer stored in env.methods remains valid
	obj := &Fn{
		name: decl.name
		typ:  typ
	}
	// TODO:
	if decl.is_method {
		// mut is_mut := false
		// receiver_type := if decl.receiver.typ is ast.ModifierExpr {
		// 	c.log('MUT RECEIVER')
		// 	is_mut = decl.receiver.typ.kind == .key_mut
		// 	if is_mut { Type(Pointer{base_type: c.expr(decl.receiver.typ.expr)}) }
		// 	else { c.expr(decl.receiver.typ) }
		// } else {
		// 	c.expr(decl.receiver.typ)
		// }
		mut receiver_type := c.expr(decl.receiver.typ)
		if decl.receiver.is_mut {
			if mut receiver_type is Pointer {
				c.error_with_pos('use `mut Type` not `mut &Type`. TODO: proper error message',
					decl.receiver.pos)
			}
			receiver_type = Type(Pointer{
				base_type: receiver_type
			})
		}

		c.scope.insert(decl.receiver.name, object_from_type(receiver_type))
		// TODO: interface methods
		receiver_base_type := receiver_type.base_type()
		method_owner_type := if receiver_type is Pointer {
			receiver_base_type
		} else {
			receiver_type
		}
		// type_name := if base_type is Interface { receiver_type.name() } else { base_type.name() }
		// Register method in shared methods map (safe inside lock)
		method_type_name := method_owner_type.name()
		lock c.env.methods {
			mut methods_for_type := []&Fn{}
			if method_type_name in c.env.methods {
				methods_for_type = unsafe { c.env.methods[method_type_name] }
			}
			methods_for_type << obj
			c.env.methods[method_type_name] = methods_for_type
		}
		c.log('registering method: ${decl.name} for ${receiver_type.name()} - ${method_owner_type.name()} - ${receiver_base_type.name()}')
		// Note: receiver was already inserted at line 1515 with correct type (including Pointer for mut)
	} else {
		if decl.language == .c {
			c.c_scope.insert(decl.name, *obj)
		} else {
			prev_scope.insert(decl.name, *obj)
		}
	}
	fn_scope := c.scope
	scope_fn_name := if decl.is_method {
		// Get receiver type name for method scope key.
		// Keep alias receiver names (e.g. `Builder`) instead of unwrapping to base
		// array/map types, otherwise transformer cannot retrieve method scopes.
		// Strip module prefix since set_fn_scope already prepends the module.
		mut receiver_type := c.expr(decl.receiver.typ)
		mut recv_scope_type := receiver_type
		if receiver_type is Pointer {
			recv_scope_type = (receiver_type as Pointer).base_type
		}
		mut recv_name := if recv_scope_type is Alias {
			recv_scope_type.name()
		} else {
			recv_scope_type.base_type().name()
		}
		if c.cur_file_module != '' {
			prefix := '${c.cur_file_module}__'
			if recv_name.starts_with(prefix) {
				recv_name = recv_name[prefix.len..]
			}
		}
		'${recv_name}__${decl.name}'
	} else {
		decl.name
	}
	module_name := c.cur_file_module
	pending := PendingFnBody{
		scope:         fn_scope
		decl:          decl
		typ:           fn_typ
		scope_fn_name: scope_fn_name
		module_name:   module_name
	}
	if c.collect_fn_signatures_only {
		c.pending_fn_bodies << pending
	} else {
		c.check_pending_fn_body(pending)
	}
	c.close_scope()
}

// eval_comptime_cond evaluates a compile-time condition expression
fn (c &Checker) eval_comptime_cond(cond ast.Expr) bool {
	match cond {
		ast.Ident {
			return c.eval_comptime_flag(cond.name)
		}
		ast.PrefixExpr {
			if cond.op == .not {
				return !c.eval_comptime_cond(cond.expr)
			}
		}
		ast.InfixExpr {
			if cond.op == .and {
				return c.eval_comptime_cond(cond.lhs) && c.eval_comptime_cond(cond.rhs)
			}
			if cond.op == .logical_or {
				return c.eval_comptime_cond(cond.lhs) || c.eval_comptime_cond(cond.rhs)
			}
		}
		ast.PostfixExpr {
			if cond.op == .question {
				if cond.expr is ast.Ident {
					return c.eval_comptime_flag(cond.expr.name)
				}
			}
		}
		ast.ParenExpr {
			return c.eval_comptime_cond(cond.expr)
		}
		else {}
	}
	return false
}

// eval_comptime_flag evaluates a single comptime flag/identifier
fn (c &Checker) eval_comptime_flag(name string) bool {
	match name {
		'macos', 'darwin' {
			$if macos {
				return true
			}
			return false
		}
		'linux' {
			$if linux {
				return true
			}
			return false
		}
		'windows' {
			$if windows {
				return true
			}
			return false
		}
		'freebsd' {
			$if freebsd {
				return true
			}
			return false
		}
		'x64', 'amd64' {
			$if amd64 {
				return true
			}
			return false
		}
		'arm64', 'aarch64' {
			$if arm64 {
				return true
			}
			return false
		}
		'little_endian' {
			$if little_endian {
				return true
			}
			return false
		}
		'big_endian' {
			$if big_endian {
				return true
			}
			return false
		}
		'debug' {
			$if debug {
				return true
			}
			return false
		}
		'native' {
			return c.pref != unsafe { nil } && (c.pref.backend == .arm64 || c.pref.backend == .x64)
		}
		'builtin_write_buf_to_fd_should_use_c_write' {
			return c.pref != unsafe { nil } && (c.pref.backend == .arm64 || c.pref.backend == .x64)
		}
		'new_int', 'gcboehm', 'prealloc', 'autofree' {
			return false
		}
		else {
			// Check user-defined comptime flags from -d <name>
			if c.pref != unsafe { nil } && name in c.pref.user_defines {
				return true
			}
			return false
		}
	}
}

// comptime_if_else checks a compile-time $if/$else expression,
// only processing the branch that matches the current platform
fn (mut c Checker) comptime_if_else(node ast.IfExpr) {
	if c.eval_comptime_cond(node.cond) {
		// Condition is true - check the then branch
		c.stmt_list(node.stmts)
	} else {
		// Condition is false - check the else branch
		match node.else_expr {
			ast.IfExpr {
				// Check if this is a plain $else block (empty condition) or chained $else $if
				if node.else_expr.cond is ast.EmptyExpr {
					// Plain $else { ... } - statements are in the IfExpr.stmts
					c.stmt_list(node.else_expr.stmts)
				} else {
					// Chained $else $if - check recursively
					c.comptime_if_else(node.else_expr)
				}
			}
			ast.EmptyExpr {
				// No else branch
			}
			else {
				// Other expression types - just evaluate
				_ = c.expr(node.else_expr)
			}
		}
	}
}

fn (mut c Checker) if_expr(expr ast.IfExpr) Type {
	c.open_scope()
	// BIG MESS peanut head! Fix this :)
	// TODO: this probably is not the best way to do this
	// need to think about this some more. also should this only
	// work for the top level? how complicated can this get? eiip
	// NOTE: in the current compiler there are smartcasts and then there
	// are explicit auto casts. I have inplemeted this just using smartcasts
	// for now, however I will come back to this, and work out what is most appropriate
	mut cond_type := Type(void_)
	mut is_inline_smartcast := false
	cond := c.unwrap_expr(expr.cond)
	if cond is ast.InfixExpr {
		cond_type = Type(bool_)
		mut left_node := c.unwrap_expr(cond.lhs)
		for mut left_node is ast.InfixExpr {
			left_node_rhs := c.unwrap_expr(left_node.rhs)
			if left_node.op == .and && left_node_rhs is ast.InfixExpr {
				if left_node_rhs.op == .key_is {
					is_inline_smartcast = true
					// c.expr(left_node.rhs)
					sc_names, sc_types := c.extract_smartcasts(left_node.rhs)
					c.apply_smartcasts(sc_names, sc_types)
					// c.expr(left_node.lhs)
				}
			}
			if left_node.op == .key_is {
				is_inline_smartcast = true
				// c.expr(left_node.lhs)
				sc_names, sc_types := c.extract_smartcasts(left_node)
				c.apply_smartcasts(sc_names, sc_types)
				// c.expr(left_node.rhs)
				break
			} else if left_node.op == .and {
				left_node = c.unwrap_expr(left_node.lhs)
			} else {
				break
			}
		}
		right_node := c.unwrap_expr(cond.rhs)
		if right_node is ast.InfixExpr && right_node.op == .key_is {
			is_inline_smartcast = true
			// c.expr(right_node.lhs)
			sc_names, sc_types := c.extract_smartcasts(right_node)
			c.apply_smartcasts(sc_names, sc_types)
			// c.expr(right_node.rhs)
		}
	}
	if !is_inline_smartcast {
		// println('## not is_inline_smartcast')
		cond_type = c.expr(expr.cond)
		sc_names, sc_types := c.extract_smartcasts(expr.cond)
		c.apply_smartcasts(sc_names, sc_types)
	}
	_ = cond_type
	c.stmt_list(expr.stmts)
	mut typ := Type(void_)
	if expr.stmts.len > 0 {
		last_stmt := expr.stmts.last()
		if last_stmt is ast.ExprStmt {
			typ = c.expr(last_stmt.expr)
		}
	}
	c.close_scope()
	// return typ
	else_type := if expr.else_expr !is ast.EmptyExpr { c.expr(expr.else_expr) } else { typ }
	return else_type
	// TODO: check all branches types match
}

fn (mut c Checker) match_expr(expr ast.MatchExpr, used_as_expr bool) Type {
	expr_type := c.expr(expr.expr)
	expected_type := c.expected_type
	if expr_type is Enum {
		c.expected_type = to_optional_type(Type(expr_type))
	}
	mut last_stmt_type := Type(void_)
	for _, branch in expr.branches {
		c.open_scope()
		for cond in branch.cond {
			expr_unwrapped := c.unwrap_ident(expr.expr)
			cond_type := c.expr(cond)
			if cond is ast.Ident || cond is ast.SelectorExpr {
				// `.value` enum value (without type)
				if cond is ast.SelectorExpr && cond.lhs is ast.EmptyExpr {
					continue
				}
				c.apply_smartcast(expr_unwrapped, cond_type)
			}
		}
		c.stmt_list(branch.stmts)
		// mut is_noreturn := false
		if used_as_expr && branch.stmts.len > 0 {
			last_stmt := branch.stmts.last()
			if last_stmt is ast.ExprStmt {
				t := c.expr(last_stmt.expr)
				// TODO: non else branch can only return void if its a noreturn
				// if last_stmt is ast.ExprStmt {
				// 	if last_stmt.expr is ast.CallExpr {
				// 		lhs := c.expr(last_stmt.expr.lhs)
				// 		if lhs is FnType {
				// 			is_noreturn = lhs.attributes.has(.noreturn)
				// 		}
				// 	}
				// }
				// TODO: fix last branch / void expr
				// actually make sure its an else branch
				if _ := expected_type {
					// TODO: re-enable type checking for match branches when v2 handles sum types better
					// Currently disabled because v2 checker doesn't understand sum type compatibility
					last_stmt_type = t
				}
			}
		}
		c.close_scope()
	}
	c.expected_type = expected_type
	return last_stmt_type
}

// TODO: unwrap ModifierExpr etc
// fn (mut c Checker) argument() ast.Expr {}

fn (mut c Checker) resolve_generic_arg_or_index_expr(expr ast.GenericArgOrIndexExpr) ast.Expr {
	lhs_type := c.expr(expr.lhs)
	// expr_type := c.expr(expr.expr)
	if c.is_callable_type(lhs_type) {
		return ast.GenericArgs{
			lhs:  expr.lhs
			args: [expr.expr]
		}
	} else {
		// c.unwrap_lhs_expr(ast.IndexExpr{lhs: expr.lhs, expr: expr.expr})
		return ast.IndexExpr{
			lhs:  expr.lhs
			expr: expr.expr
		}
	}
}

fn (c &Checker) is_callable_type(t Type) bool {
	match t {
		FnType {
			return true
		}
		else {
			// In self-host mode we can observe malformed transitional alias/pointer
			// payloads during ambiguous call-or-cast resolution. Be conservative
			// here and only treat direct function types as callable.
			return false
		}
	}
}

fn (c &Checker) is_indexable_type(t Type) bool {
	match t {
		Array, Map, String, Pointer {
			return true
		}
		Alias {
			return c.is_indexable_type(t.base_type)
		}
		else {
			return false
		}
	}
}

fn (mut c Checker) resolve_call_or_cast_expr(expr ast.CallOrCastExpr) ast.Expr {
	lhs_type := c.expr(expr.lhs)
	// expr_type := c.expr(expr.expr)
	if c.is_callable_type(lhs_type) {
		return ast.CallExpr{
			lhs:  expr.lhs
			args: [expr.expr]
			pos:  expr.pos
		}
	} else {
		// c.log(expr)
		return ast.CastExpr{
			typ:  expr.lhs
			expr: expr.expr
			pos:  expr.pos
		}
	}
}

// TODO:
fn (mut c Checker) resolve_expr(expr ast.Expr) ast.Expr {
	match expr {
		ast.CallOrCastExpr {
			return c.resolve_call_or_cast_expr(expr)
		}
		ast.GenericArgOrIndexExpr {
			return c.resolve_generic_arg_or_index_expr(expr)
		}
		else {
			return expr
		}
	}
}

// TODO:
fn (mut c Checker) unwrap_ident(expr ast.Expr) ast.Expr {
	match expr {
		ast.ModifierExpr {
			return expr.expr
		}
		else {
			return expr
		}
	}
}

// TODO:
fn (mut c Checker) unwrap_expr(expr ast.Expr) ast.Expr {
	match expr {
		// ast.Ident, ast.IndexExpr, ast.SelectorExpr {
		// 	return expr
		// }
		ast.CallOrCastExpr {
			// return c.unwrap_expr(c.resolve_expr(expr.expr))
			return c.unwrap_expr(c.resolve_call_or_cast_expr(expr))
		}
		ast.ModifierExpr {
			// TODO: actually do something with modifiers :)
			return c.unwrap_expr(expr.expr)
		}
		ast.ParenExpr {
			return c.unwrap_expr(expr.expr)
		}
		ast.PrefixExpr {
			return c.unwrap_expr(expr.expr)
		}
		else {
			// TODO: expr.pos()
			// c.error_with_pos('unwrap_expr: missing impl for expr ${expr.type_name()}', 2)
			// no unwrap needed
			return expr
		}
	}
}

// TODO:
fn (mut c Checker) unwrap_lhs_expr(expr ast.Expr) ast.Expr {
	match expr {
		ast.ModifierExpr {
			// return expr.expr
			return c.unwrap_lhs_expr(expr.expr)
		}
		ast.GenericArgs {
			// lhs_type := c.expr(expr.lhs)
			mut generic_arg_types := []Type{}
			for arg in expr.args {
				generic_arg_types << c.expr(arg)
			}
			return expr.lhs
		}
		ast.GenericArgOrIndexExpr {
			return c.unwrap_lhs_expr(c.resolve_generic_arg_or_index_expr(expr))
		}
		else {
			return expr
		}
	}
}

fn (mut c Checker) add_inferred_generic_type(mut type_map map[string]Type, name string, typ Type) ! {
	// NOTE: lookup on a mut map parameter currently miscompiles in the cleanc
	// self-host path, so keep this as a direct set for stability.
	type_map[name] = typ
}

// TODO: clean up & add any missing cases & missing recursion
fn (mut c Checker) infer_generic_type(param_type Type, arg_type Type, mut type_map map[string]Type) ! {
	match param_type {
		Alias {
			c.infer_generic_type(param_type.base_type, arg_type, mut type_map)!
		}
		Array {
			if arg_type is Array {
				c.infer_generic_type(param_type.elem_type, arg_type.elem_type, mut type_map)!
			}
		}
		ArrayFixed {
			if arg_type is ArrayFixed {
				c.infer_generic_type(param_type.elem_type, arg_type.elem_type, mut type_map)!
			} else if arg_type is Array {
				// `[]T` call-sites can infer generic params from `[N]T` params and vice versa.
				c.infer_generic_type(param_type.elem_type, arg_type.elem_type, mut type_map)!
			}
		}
		Channel {
			if pt_et := param_type.elem_type {
				if arg_type is Channel {
					if at_et := arg_type.elem_type {
						c.infer_generic_type(pt_et, at_et, mut type_map)!
					}
				}
			}
		}
		FnType {
			if arg_type is FnType {
				max_params := if param_type.params.len < arg_type.params.len {
					param_type.params.len
				} else {
					arg_type.params.len
				}
				for i in 0 .. max_params {
					param_param := param_type.params[i]
					arg_param := arg_type.params[i]
					c.infer_generic_type(param_param.typ, arg_param.typ, mut type_map)!
				}
				if param_rt := param_type.return_type {
					if arg_rt := arg_type.return_type {
						c.infer_generic_type(param_rt, arg_rt, mut type_map)!
					}
				}
			}
		}
		Map {
			if arg_type is Map {
				c.infer_generic_type(param_type.key_type, arg_type.key_type, mut type_map)!
				c.infer_generic_type(param_type.value_type, arg_type.value_type, mut type_map)!
			}
		}
		NamedType {
			c.add_inferred_generic_type(mut type_map, param_type, arg_type)!
		}
		Struct {}
		OptionType {
			if arg_type is OptionType {
				c.infer_generic_type(param_type.base_type, arg_type.base_type, mut type_map)!
			} else {
				c.infer_generic_type(param_type.base_type, arg_type, mut type_map)!
			}
		}
		Pointer {
			if arg_type is Pointer {
				c.infer_generic_type(param_type.base_type, arg_type.base_type, mut type_map)!
			}
		}
		ResultType {
			if arg_type is ResultType {
				c.infer_generic_type(param_type.base_type, arg_type.base_type, mut type_map)!
			} else {
				c.infer_generic_type(param_type.base_type, arg_type, mut type_map)!
			}
		}
		Thread {
			if pt_et := param_type.elem_type {
				if arg_type is Thread {
					if at_et := arg_type.elem_type {
						c.infer_generic_type(pt_et, at_et, mut type_map)!
					}
				}
			}
		}
		Tuple {
			if arg_type is Tuple {
				max_types := if param_type.types.len < arg_type.types.len {
					param_type.types.len
				} else {
					arg_type.types.len
				}
				for i in 0 .. max_types {
					c.infer_generic_type(param_type.types[i], arg_type.types[i], mut type_map)!
				}
			}
		}
		// Other concrete types do not participate in generic inference.
		Primitive, Char, Enum, ISize, Interface, Nil, None, Rune, String, SumType, USize, Void {}
	}
}

fn (mut c Checker) call_expr(expr ast.CallExpr) Type {
	lhs_expr := c.resolve_expr(expr.lhs)
	// TODO: remove, see comment at field decl
	expecting_method := c.expecting_method
	if lhs_expr is ast.SelectorExpr {
		c.expecting_method = true
	}
	mut fn_ := c.expr(lhs_expr)
	// fn_ := c.expr(expr.lhs)
	c.expecting_method = expecting_method
	// c.log('call expr: ${fn_.type_name()} - ${fn_.name()} - ${lhs_expr.type_name()}')

	// if expr.lhs is PrefixExpr {
	// 	xpr.op
	// }
	// if expr.lhs is Postfix
	// TODO: time.StopWatch has a fields and methods with the same name
	// and field was being returned instead of the methods. we could set a precedence
	// however what if the field is a fn type? (i guess one or the other could still habve priority)
	// TODO: talk to alex and spy about this
	if expr.lhs is ast.SelectorExpr {
		// POO POO
		// if expr.lhs.lhs is ast.Ident {
		if fn_ is Primitive && expr.lhs.rhs.name == 'start' {
			// c.log('#### ${expr.lhs.rhs.name}')
			// what is going on here?
			fn_ = Type(FnType{})
		} else if fn_ is Primitive && expr.lhs.rhs.name == 'elapsed' {
			// c.log('#### ${expr.lhs.rhs.name}')
			elapsed_ret := Type(Alias{
				name:      'Duration'
				base_type: i64_
			})
			fn_ = Type(fn_with_return_type(FnType{}, elapsed_ret))
		}
		// }
	}
	// 	else if lhs_expr is ast.GenericArgs {
	// 	c.log('GENERIC CALL: ')

	// }
	// In C, a name can be both a struct type and a function (e.g., `statvfs`).
	// If we resolved to a Struct but we're in a call expression with C. prefix,
	// treat it as a C function call returning int.
	if fn_ is Struct {
		if lhs_expr is ast.SelectorExpr {
			if lhs_expr.lhs is ast.Ident {
				if lhs_expr.lhs.name == 'C' {
					for arg in expr.args {
						c.expr(arg)
					}
					return int_
				}
			}
		}
	}
	if mut fn_ is Alias {
		fn_ = fn_.base_type
	}
	// When a selector resolved to a non-FnType (e.g., a field that shadows
	// a sum type method due to smartcasting), try finding the method on the
	// resolved type. For example, `cur.base_type()` where `cur` is smartcast
	// to `Alias` resolves `base_type` as a field of type `Type` (SumType),
	// but `base_type()` is a method on `Type`.
	if fn_ !is FnType && lhs_expr is ast.SelectorExpr {
		if method := c.find_method(fn_, lhs_expr.rhs.name) {
			fn_ = method
		}
	}
	if mut fn_ is FnType {
		if fn_.generic_params.len > 0 {
			// file := c.file_set.file(expr.pos)
			// pos := file.position(expr.pos)
			// eprintln('GENERIC CALL: ${expr.lhs.name()} - ${expr.pos} - ${file.name}:${pos.line}')
			mut generic_type_map := map[string]Type{}
			// generic types provided `[int, string]`
			if lhs_expr is ast.GenericArgs {
				// panic('GOT GENERIC CALL')
				mut generic_types := []Type{}
				for i, ga in lhs_expr.args {
					if i >= fn_.generic_params.len {
						break
					}
					generic_param := fn_.generic_params[i]
					generic_type := c.expr(ga)
					generic_types << generic_type
					generic_type_map[generic_param] = generic_type
				}
				// eprintln('GENERIC TYPES: ${expr.lhs.name()}')
				// dump(generic_types)
			}
			// infer generic types from args
			else {
				// TODO: move above (to be done globally)
				// once error is fixed
				mut arg_types := []Type{}
				// eprintln('INFERRED GENERIC TYPES: ${expr.lhs.name()}')
				for i, arg in expr.args {
					if i >= fn_.params.len {
						break
					}
					param := fn_.params[i]
					arg_type := c.expr(arg).typed_default()
					arg_types << arg_type
					// eprintln('call arg: ${param.typ.type_name()} - ${arg_type.type_name()}')

					// println('infcerring call: ')
					c.infer_generic_type(param.typ, arg_type, mut generic_type_map) or {
						c.error_with_pos(err.msg(), expr.pos)
					}
					// if param.typ !is NamedType {
					// 	eprintln('Generic argument: ${param.typ.name()} - ${arg_type.name()}')
					// 	eprintln('should be NamedType but got ${param.typ.name()}')
					// }
				}
			}
			// eprintln('## GENERIC TYPE MAP: ${expr.lhs.name()}')
			// eprintln('========================')
			// for k,v in generic_type_map {
			// 	eprintln(' * ${k} -> ${v}')
			// }
			// eprintln('========================')
			// if expr.lhs is ast.Ident {
			// 	if expr.lhs.name == 'generic_fn_d' {
			// 		panic('.')
			// 	}
			// }
			// dump(generic_type_map)
			if generic_type_map.len > 0 {
				fn_.generic_types << generic_type_map
				lhs_name := lhs_expr.name()
				if lhs_name !in c.env.generic_types {
					empty_generic_types := []map[string]Type{}
					c.env.generic_types[lhs_name] = empty_generic_types
				}
				mut inferred_generic_types := c.env.generic_types[lhs_name]
				inferred_generic_types << generic_type_map
				c.env.generic_types[lhs_name] = inferred_generic_types
			}
		} else if lhs_expr is ast.GenericArgs {
			c.error_with_pos('cannot call non generic function with generic argument list',
				expr.pos)
		}

		// TODO: is this best place for this?
		// why is it not being used any more? check if we are somehow skipping its uses
		// need to find a better way to do this, this only happens because there are
		// compiler magic, call_expr & selector expr both end up needing information which
		// the other one has
		if lhs_expr is ast.SelectorExpr {
			if lhs_expr.rhs.name in ['filter', 'map', 'any', 'all'] {
				if rt := fn_.return_type {
					// c.log('#### it variable inserted')
					// fn_.scope.insert('it', rt)
					it_type := rt.value_type()
					it_obj := object_from_type(it_type)
					c.scope.objects['it'] = it_obj
				}
				if lhs_expr.rhs.name == 'map' && expr.args.len > 0 {
					rt := c.expr(expr.args[0])
					c.log('map: ${rt.name()}')
					fn_type := fn_ as FnType
					fn_ = Type(fn_with_return_type(fn_type, Type(Array{
						elem_type: rt
					})))
				}
			}
		}
		// Type-check call arguments with parameter context so shorthand enum
		// selectors like `.assign` resolve against the callee parameter type.
		// Skip sort/sorted comparator sugar because that lambda form is lowered later.
		is_sort_cmp_call := lhs_expr is ast.SelectorExpr && lhs_expr.rhs.name in ['sort', 'sorted']
			&& expr.args.len == 1
		if fn_.generic_params.len == 0 && !is_sort_cmp_call {
			prev_expected := c.expected_type
			for i, arg in expr.args {
				if i < fn_.params.len {
					c.expected_type = to_optional_type(fn_.params[i].typ)
				} else if fn_.is_variadic && fn_.params.len > 0 {
					variadic_type := fn_.params[fn_.params.len - 1].typ
					if variadic_type is Array {
						c.expected_type = to_optional_type(variadic_type.elem_type)
					} else {
						c.expected_type = to_optional_type(variadic_type)
					}
				} else {
					c.expected_type = prev_expected
				}
				c.expr(arg)
			}
			c.expected_type = prev_expected
		}
		// TODO/FIXME: is FnType.return_type goin to stay optional
		// or not and just use void in case of returning nothing
		if return_type := fn_.return_type {
			// if expr.lhs is ast.SelectorExpr {
			// 	if expr.lhs.lhs is ast.Ident {
			// 		if expr.lhs.lhs.name == 'g_timers' && expr.lhs.rhs.name == 'show' {
			// 			c.log('## 2 ${expr.lhs.lhs.name}: fn_.type()')
			// 		}
			// 	}
			// }
			c.log('returning: ${return_type.name()}')
			return return_type
		}
		return Type(void_)
	}

	for arg in expr.args {
		c.expr(arg)
	}

	c.error_with_pos('call on non fn: ${fn_.name()}', expr.pos)
}

// TODO:
// fn (mut c Checker) fn_arguments() {}

// fn (mut c Checker) declare(scope &Scope, name string) {

// }

fn (mut c Checker) fn_type(fn_type ast.FnType, attributes FnTypeAttribute) FnType {
	// c.open_scope()
	// array of T types
	mut generic_params := []string{}
	for generic_param in fn_type.generic_params {
		if generic_param is ast.Ident {
			// generic_params << NamedType(generic_param.name)
			generic_params << generic_param.name
		} else {
			// TODO: correct position
			c.error_with_pos('expecting identifier', token.Pos{})
		}
	}
	mut params := []Parameter{}
	mut is_variadic := false
	// TODO: proper
	if generic_params.len > 0 {
		c.generic_params = generic_params
	}
	for i, param in fn_type.params {
		// of param type is generic, replace with NamedType
		// mut param_type := if param.typ is ast.Ident {
		// 	if param.typ.name in generic_params {
		// 		Type(NamedType(param.typ.name))
		// 	} else {
		// 		c.expr(param.typ)
		// 	}
		// } else { c.expr(param.typ) }
		mut param_type := c.expr(param.typ)
		if param.typ is ast.PrefixExpr {
			if param.typ.op == .ellipsis {
				if i < fn_type.params.len - 1 {
					c.error_with_pos('variadic must be last parameter.', param.pos)
				}
				param_type = Type(Array{
					elem_type: param_type
				})
				is_variadic = true
			}
		}
		// if param.is_mut && param_type !is Pointer {
		if param.is_mut {
			param_type = Type(Pointer{
				base_type: param_type
			})
		}
		params << Parameter{
			name: param.name
			// typ: c.expr(param.typ)
			typ:    param_type
			is_mut: param.is_mut
		}
		c.scope.insert(param.name, object_from_type(param_type))
	}
	// if return type is generic replace with NamedType
	// mut return_type := if fn_type.return_type is ast.Ident {
	// 	if fn_type.return_type.name in generic_params {
	// 		Type(NamedType(fn_type.return_type.name))
	// 	} else {
	// 		c.expr(fn_type.return_type)
	// 	}
	// } else { c.expr(fn_type.return_type) }

	mut typ := FnType{
		generic_params: generic_params
		params:         params
		// return_type: return_type
		return_type: to_optional_type(c.expr(fn_type.return_type))
		is_variadic: is_variadic
		attributes:  attributes
		// scope: c.scope
	}
	if generic_params.len > 0 {
		c.generic_params = []
	}
	// c.close_scope()
	return typ
}

fn (mut c Checker) ident(ident ast.Ident) Object {
	obj := c.scope.lookup_parent(ident.name, 0) or {
		if ident.name == '_' {
			c.error_with_pos('cannot use _ as value or type', ident.pos)
			// TODO: compiler error
			return Type(void_)
		} else {
			// TODO/FIXME: generic param - hack
			// if ident.name.len == 1 && ident.name[0].is_capital() {
			// 	return Type(string_)
			// }
		}

		if ident.name in c.generic_params {
			return Type(NamedType(ident.name))
		}

		for generic_types in c.env.cur_generic_types {
			if generic_type := generic_types[ident.name] {
				// eprintln('## replaced generic type ${ident.name} with ${generic_type.name()}')
				return object_from_type(generic_type)
			}
		}

		// TODO: proper
		if ident.name in [
			'@VROOT',
			'@VMODROOT',
			'@VEXEROOT',
			'@VCURRENTHASH',
			'@FN',
			'@METHOD',
			'@MOD',
			'@STRUCT',
			'@VEXE',
			'@FILE',
			'@LINE',
			'@COLUMN',
			'@VHASH',
			'@VMOD_FILE',
			'@FILE_LINE',
		] {
			return Type(string_)
		}

		// TODO: proper
		if ident.name in ['compile_error', 'compile_warn'] {
			return Type(FnType{
				return_type: to_optional_type(Type(void_))
			})
		}

		// c.scope.print(false)
		// c.scope.print(true)
		c.error_with_pos('unknown ident `${ident.name} - ${ident.pos}`', ident.pos)
		return Type(void_)
	}
	c.log('ident: ${ident.name} - ${obj.typ().name()}')
	// Store the ident type in env so the transformer can look it up by pos.id.
	// c.ident() is called from places like selector_expr() that bypass c.expr(),
	// so without this the lhs Ident wouldn't get stored.
	if ident.pos.is_valid() {
		c.env.set_expr_type(ident.pos.id, obj.typ())
	}
	return obj
}

fn (mut c Checker) selector_expr(expr ast.SelectorExpr) Type {
	c.log('## selector_expr')
	// file := c.file_set.file(expr.pos)
	// pos := file.position(expr.pos)
	// c.log('${expr.name()} - ${expr.rhs.name}')
	// println('selector_expr: ${expr.rhs.name} - ${file.name}:${pos.line} - ${pos.column}')

	// TODO: check todo in scope.v
	if expr.lhs is ast.Ident || expr.lhs is ast.SelectorExpr {
		// println('looking for smartcast ${expr.name()}')
		if cast_type := c.scope.lookup_field_smartcast(expr.name()) {
			// println('## 1 found smartcast for ${expr.name()} - ${cast_type.type_name()} - ${cast_type.name()} - ${expr.rhs.name}')
			return cast_type
		}
		if cast_type := c.scope.lookup_field_smartcast(expr.lhs.name()) {
			// println('## 2 found smartcast for ${expr.lhs.name()} - ${cast_type.type_name()} - ${cast_type.name()} - ${expr.rhs.name}')
			return c.find_field_or_method(cast_type, expr.rhs.name) or {
				c.error_with_pos('## smartcast field lookup error: ${err.msg()}', expr.pos)
				return Type(void_)
			}
		}
	}

	if expr.lhs is ast.Ident {
		lhs_obj := c.ident(expr.lhs)
		// c.log('LHS.RHS: ${expr.lhs.name} . ${expr.rhs.name} - ${lhs_obj.typ().name()}')
		match lhs_obj {
			Const {
				return c.find_field_or_method(lhs_obj.typ, expr.rhs.name) or {
					c.error_with_pos(err.msg(), expr.pos)
					return Type(void_)
				}
			}
			Global {
				return c.find_field_or_method(lhs_obj.typ, expr.rhs.name) or {
					c.error_with_pos(err.msg(), expr.pos)
					return Type(void_)
				}
			}
			Module {
				// return lhs_type.scope.lookup_parent(expr.rhs.name)
				mut mod_scope := lhs_obj.scope
				rhs_obj := mod_scope.lookup_parent(expr.rhs.name, 0) or {
					// TODO: proper
					if expr.lhs.name == 'C' {
						// c.log('assuming C constant: ${expr.lhs.name} . ${expr.rhs.name}')
						return int_
					}
					mod_scope.print(true)
					c.error_with_pos('missing ${expr.lhs.name}.${expr.rhs.name}', expr.pos)
					return Type(void_)
				}
				return rhs_obj.typ()
			}
			Type {
				return c.find_field_or_method(lhs_obj, expr.rhs.name) or {
					c.error_with_pos(err.msg(), expr.pos)
					return Type(void_)
				}
			}
			// SmartCastSelector {
			// 	c.log('@@ found smart cast selector: ${expr.rhs.name}')
			// 	if expr.rhs.name == lhs_obj.field {
			// 		return lhs_obj.cast_type
			// 		// return c.find_field_or_method(lhs_obj.cast_type, expr.rhs.name) or { c.error_with_pos(err.msg(), expr.pos) }
			// 	}
			// 	return c.find_field_or_method(lhs_obj.origin, expr.rhs.name) or { c.error_with_pos(err.msg(), expr.pos) }
			// }
			else {
				c.error_with_pos('unsupported ${lhs_obj.typ().name()} - ${expr.rhs.name}',
					token.Pos{})
				return Type(void_)
			}
		}
	}
	// else {
	// 	panic('unexpected expr.lhs: ${expr.lhs.type_name()}')
	// }
	// TODO: this will be removed
	// unset when checking lhs, only needed for rightmost selector
	expecting_method := c.expecting_method
	c.expecting_method = false
	lhs_type := c.expr(expr.lhs)
	c.expecting_method = expecting_method
	if lhs_type is Void || lhs_type.name() == '' {
		return Type(void_)
	}
	return c.find_field_or_method(lhs_type, expr.rhs.name) or {
		c.error_with_pos(err.msg(), expr.pos)
		return Type(void_)
	}
}

fn (mut c Checker) find_field_or_method(t Type, name string) !Type {
	// TODO: is this the best way to do this?
	// this whole field/method search needs to be cleaned up
	// fields and method with same name should not be allowed to begin with!
	if name in ['vbytes', 'vstring', 'vstring_with_len', 'vstring_literal',
		'vstring_literal_with_len'] {
		if method := c.find_builtin_ptr_helper_method(name) {
			return method
		}
	}
	if c.expecting_method {
		if method := c.find_method(t, name) {
			return method
		}
	}
	match t {
		Alias {
			// if field_or_method_type := c.find_field_or_method(t, name) {
			// 	return field_or_method_type
			// }
			if field_or_method_type := c.find_field_or_method(t.base_type, name) {
				return field_or_method_type
			}
		}
		Array {
			arr_type := t as Array
			if name == 'str' || name == 'hex' {
				return fn_with_return_type(FnType{}, Type(string_))
			}
			// Compiler-magic: []thread.wait() waits for all threads to complete
			if name == 'wait' && arr_type.elem_type is Thread {
				return fn_with_return_type(FnType{}, Type(void_))
			}
			// Compiler-magic array methods (any, all, contains, index)
			// Return FnType with array as return_type so 'it' gets the element type
			if name in ['any', 'all', 'contains'] {
				return fn_with_return_type(FnType{}, Type(arr_type))
			}
			if name == 'index' {
				return fn_with_return_type(FnType{}, Type(arr_type))
			}
			// TODO: has to be a better way
			// there is probably no reason to look these up, and just do what we do above
			mut builtin_scope := c.get_module_scope('builtin', universe)
			at := builtin_scope.lookup_parent('array', 0) or { panic('missing builtin array type') }
			at_type := at.typ()
			// c.log('ARRAY: looking for field or method ${name} on ${t.name()}')
			// // dump(t)
			if field_or_method_type := c.find_field_or_method(at_type, name) {
				c.log('found ${name}')
				if field_or_method_type is FnType {
					fn_type := field_or_method_type as FnType
					if name in ['clone', 'map', 'filter', 'repeat', 'reverse', 'sorted'] {
						// c.log('SNEAKY: ${t.name()}')
						// return t
						return fn_with_return_type(fn_type, Type(arr_type))
					}
					// TODO: proper PLEASE! :D
					// Array methods that return the element type
					else if name in ['first', 'last', 'pop'] {
						return fn_with_return_type(fn_type, arr_type.elem_type)
					}
				}
				return field_or_method_type
			}
		}
		ArrayFixed {
			arr_fixed_type := t as ArrayFixed
			if name in ['clone', 'map', 'filter', 'reverse', 'sorted', 'sorted_with_compare'] {
				return fn_with_return_type(FnType{}, Type(arr_fixed_type))
			} else if name in ['first', 'last'] {
				return fn_with_return_type(FnType{}, arr_fixed_type.elem_type)
			} else if name == 'len' {
				return int_
			} else if name in ['any', 'all', 'contains'] {
				return fn_with_return_type(FnType{}, bool_)
			} else if name == 'index' {
				return fn_with_return_type(FnType{}, int_)
			} else if name in ['sort', 'sort_with_compare', 'reverse_in_place'] {
				return fn_with_return_type(FnType{}, Type(void_))
			}
		}
		Channel {
			// Handle common channel methods directly
			if name == 'close' {
				return fn_with_return_type(FnType{}, Type(void_))
			} else if name == 'len' || name == 'cap' {
				return int_
			} else if name == 'try_push' || name == 'try_pop' {
				return fn_with_return_type(FnType{}, int_)
			}
			// Fallback to sync.Channel scope lookup
			mut sync_scope := c.get_module_scope('sync', universe)
			at := sync_scope.lookup_parent('Channel', 0) or {
				// sync module not imported; return void for unknown channel methods
				return fn_with_return_type(FnType{}, Type(void_))
			}
			at_type := at.typ()
			if field_or_method_type := c.find_field_or_method(at_type, name) {
				return field_or_method_type
			}
		}
		Enum {
			enum_type := t as Enum
			for field in enum_type.fields {
				if field.name == name {
					return Type(enum_type)
					// return int_
				}
			}
			if name == 'str' {
				return fn_with_return_type(FnType{}, Type(string_))
			}
			// flags - compiler magic (currently)
			if name == 'has' {
				return bool_
			} else if name == 'all' {
				return bool_
			} else if name in ['clear', 'set'] {
				return Type(void_)
			}
		}
		Interface {
			// c.log('looking for fields on interface ${t.name()}')
			for field in t.fields {
				if field.name == name {
					// c.log('found field ${name} for interface ${t.name()}')
					if c.expecting_method && field.typ !is FnType {
						continue
					}
					return field.typ
				}
			}
		}
		Map {
			map_type := t as Map
			if name == 'str' {
				return fn_with_return_type(FnType{}, Type(string_))
			}
			// clone/move return the same map type
			if name in ['clone', 'move'] {
				return fn_with_return_type(FnType{}, Type(map_type))
			}
			// values returns an array of the value type
			if name == 'values' {
				return fn_with_return_type(FnType{}, Type(Array{
					elem_type: map_type.value_type
				}))
			}
			mut builtin_scope := c.get_module_scope('builtin', universe)
			at := builtin_scope.lookup_parent('map', 0) or { panic('missing builtin map type') }
			at_type := at.typ()
			// c.log('MAP: looking for field or method ${name} on ${t.name()}')
			if field_or_method_type := c.find_field_or_method(at_type, name) {
				if name == 'keys' {
					if field_or_method_type is FnType {
						fn_type := field_or_method_type as FnType
						return fn_with_return_type(fn_type, Type(Array{
							elem_type: map_type.key_type
						}))
					}
				}
				return field_or_method_type
			}
		}
		Pointer {
			return c.find_field_or_method(t.base_type, name)!
		}
		OptionType {
			if name == 'state' {
				return u8_
			}
			if name == 'err' {
				mut builtin_scope := c.get_module_scope('builtin', universe)
				if obj := builtin_scope.lookup_parent('IError', 0) {
					return obj.typ()
				}
				return Type(Interface{
					name: 'IError'
				})
			}
			if name == 'data' {
				return t.base_type
			}
			return c.find_field_or_method(t.base_type, name)!
		}
		ResultType {
			if name == 'is_error' {
				return bool_
			}
			if name == 'err' {
				mut builtin_scope := c.get_module_scope('builtin', universe)
				if obj := builtin_scope.lookup_parent('IError', 0) {
					return obj.typ()
				}
				return Type(Interface{
					name: 'IError'
				})
			}
			if name == 'data' {
				return t.base_type
			}
			return c.find_field_or_method(t.base_type, name)!
		}
		// TODO:
		// currently should be handled at bottom by find_method call
		// Primitive {
		// 	c.log('#### method on ptimitive: ${name} on ${t.name()}')
		// 	// if name in ['free'] { return FnType{return_type: void_} }
		// }
		String {
			if name == 'len' {
				return int_
			}
			if o := c.scope.lookup_parent('string', 0) {
				// c.log(o)
				return c.find_field_or_method(o.typ(), name)
			}
		}
		Struct {
			for field in t.fields {
				// c.log('comparing field ${field.name} with ${name} for ${t.name}')
				if field.name == name {
					c.log('found field ${name} for ${t.name}: ${field.typ.name()}')
					return field.typ
				}
			}
			for embedded_type in t.embedded {
				if embedded_field_or_method_type := c.find_field_or_method(embedded_type,
					name)
				{
					return embedded_field_or_method_type
				}
			}
		}
		SumType {
			// if !c.expecting_method {
			mut prev_type := Type(nil_)
			for i, variant in t.variants {
				if variant is Struct {
					mut has_field := false
					for field in variant.fields {
						if field.name == name {
							has_field = true
							if i > 0 && field.typ != prev_type {
								return error('field ${name} must have the same type for all variants of ${t.name()}')
							}
							prev_type = field.typ
							break
						}
					}
					if !has_field {
						return error('not all variants of ${t.name()} have the field ${name}')
					}
				}
			}
			return prev_type
			// }
		}
		Thread {
			thread_type := t as Thread
			// TODO:
			if name == 'wait' {
				c.open_scope()
				mut wait_return_type := Type(thread_type)
				if elem_type := thread_type.elem_type {
					wait_return_type = elem_type
				}
				fn_type := fn_with_return_type(FnType{}, wait_return_type)
				c.close_scope()
				return fn_type
			}
		}
		else {
			c.log('find_field_or_method: unhandled ${t.name()}')
		}
	}
	// else if t is FnType {
	// 			c.log('FnType: ${t.name}')
	// }

	// // dump(t)
	// c.log('returning none for ${t.type_name()} - ${name}')
	if method := c.find_method(t, name) {
		return method
	}
	// Char and Rune share methods: look up 'rune' methods, then 'u8' methods
	if t is Char || t is Rune {
		for alias_name in ['rune', 'char', 'u8'] {
			mut methods := []&Fn{}
			rlock c.env.methods {
				if alias_name in c.env.methods {
					methods = unsafe { c.env.methods[alias_name] }
				}
			}
			for method in methods {
				if method.name == name {
					return method.typ
				}
			}
		}
	}
	if t is Struct {
		// dump(t.fields)
	}
	// Unresolved generic type parameters (e.g., T, Y) cannot have their
	// fields or methods resolved until the generic is instantiated.
	// Return void to allow type checking to continue.
	if t is NamedType {
		return Type(void_)
	}
	base_type := t.base_type()
	return error('cannot find field or method: `${name}` for type ${t.name()} - (base: ${base_type.name()})')
}

fn (mut c Checker) find_builtin_ptr_helper_method(name string) ?Type {
	for ptr_typ in [Type(voidptr_), Type(byteptr_), Type(charptr_)] {
		if method := c.find_method(ptr_typ, name) {
			return method
		}
	}
	return none
}

fn (mut c Checker) find_method(t Type, name string) !Type {
	c.log('looking for method `${name}` on type `${t.name()}`')
	// TODO: do we need to look for methods on the non base type first?
	// I think we will for aliases, probably not other types. we might
	// need to differentiate then for base_type / base_type in this case
	mut base_type := t.base_type()
	// Builtin sum methods are compiler magic and are not declared in env.methods.
	// Keep them in checker lookup so selector resolution does not fall through
	// to sum-field checks (e.g. `expr.type_name()` on `ast.Expr`).
	if base_type is SumType {
		if name == 'type_name' {
			return fn_with_return_type(FnType{}, Type(string_))
		}
	}
	// TODO: interface methods
	// if base_type is Interface { base_type = t }
	base_type_name := if t is Pointer { base_type.name() } else { t.name() }
	// c.log('base_type_name: ${base_type_name} - ${t.type_name()} - ${base_type.type_name()}')
	// Lookup method in shared methods map
	mut methods := []&Fn{}
	rlock c.env.methods {
		if base_type_name in c.env.methods {
			methods = unsafe { c.env.methods[base_type_name] }
		}
	}
	for method in methods {
		// c.log('# ${method.name} - ${name}')
		if method.name == name {
			c.log('found method ${name} for ${t.name()}')
			return method.typ
		}
	}
	return error('cannot find method `${name}` for `${t.name()}`')
}

// fn (mut c &Checker) open_scope(node ast.Node, comment string) {
// 	scope := new_scope(c.scope, node.Pos(), node.End(), comment)
// 	c.record_scope(node, scope)
// 	c.scope = scope
// }
fn (mut c Checker) open_scope() {
	c.scope = new_scope(c.scope)
}

fn (mut c Checker) close_scope() {
	c.scope = c.scope.parent
}

// so we can customize the error message used by warn & error
fn (mut c Checker) error_message(msg string, kind errors.Kind, pos token.Position, file &token.File) {
	errors.error(msg, errors.details(file, pos, 2), kind, pos)
}

// fn (mut c Checker) warn(msg string) {
// 	c.error_message(msg, .warning, p.current_position())
// }

// [noreturn]
// fn (mut c Checker) error(msg string) {
// 	c.error_with_position(msg, p.current_position())
// }

@[noreturn]
fn (mut c Checker) error_with_pos(msg string, pos token.Pos) {
	if !pos.is_valid() {
		// Handle expressions without position info - use current file if available
		eprintln('error: ${msg} (no position info)')
		exit(1)
	}
	file := c.file_set.file(pos)
	c.error_with_position(msg, file.position(pos), file)
}

@[noreturn]
fn (mut c Checker) error_with_position(msg string, pos token.Position, file &token.File) {
	c.error_message(msg, .error, pos, file)
	exit(1)
}

@[if verbose ?]
fn (c Checker) log[T](s T) {
	println(s)
}

@[if verbose ?]
fn (c Checker) elog[T](s T) {
	eprintln(s)
}
