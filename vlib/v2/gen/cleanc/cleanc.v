// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.types
import strings

pub struct Gen {
	files []ast.File
	// Type checker environment with populated scopes
	env &types.Environment = unsafe { nil }
mut:
	sb              strings.Builder
	indent          int
	tmp_counter     int // Counter for unique temp variable names
	fn_types        map[string]string
	fn_ret_counts   map[string]int // Track number of return values for multi-return functions
	var_types       map[string]string
	mut_receivers   map[string]bool              // Track which methods have mutable receivers
	ref_receivers   map[string]bool              // Track which methods have reference receivers (&Type)
	defer_stmts     [][]ast.Stmt                 // Deferred statements for current function
	enum_names      map[string]bool              // Track enum type names
	interface_names map[string]bool              // Track interface type names
	interface_meths map[string][]string          // Interface name -> method names
	type_methods    map[string][]string          // Type name -> method names (for vtable generation)
	struct_fields   map[string]map[string]string // Struct name -> (field name -> field type)
	known_types     map[string]bool              // All known mangled type names (structs, type aliases, interfaces)
	cur_match_type  string                       // Current match expression type for enum shorthand
	cur_module      string                       // Current module name for namespacing
	file_modules    map[string]string            // File path -> module name mapping
	module_names    map[string]bool              // All module names (for detecting module.ident access)
}

pub fn Gen.new(files []ast.File) &Gen {
	return Gen.new_with_env(files, unsafe { nil })
}

pub fn Gen.new_with_env(files []ast.File, env &types.Environment) &Gen {
	mut g := &Gen{
		files:           files
		env:             unsafe { env }
		sb:              strings.new_builder(4096)
		fn_types:        map[string]string{}
		fn_ret_counts:   map[string]int{}
		var_types:       map[string]string{}
		mut_receivers:   map[string]bool{}
		ref_receivers:   map[string]bool{}
		enum_names:      map[string]bool{}
		interface_names: map[string]bool{}
		interface_meths: map[string][]string{}
		type_methods:    map[string][]string{}
		struct_fields:   map[string]map[string]string{}
		file_modules:    map[string]string{}
		module_names:    map[string]bool{}
	}
	// Build file -> module mapping and collect module names
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.ModuleStmt {
				mod_name := stmt.name.replace('.', '_')
				g.file_modules[file.name] = mod_name
				g.module_names[mod_name] = true
				// Also track the original name (e.g., 'strconv' from 'strconv')
				g.module_names[stmt.name] = true
				break
			}
		}
	}
	// Pass 0: Register function return types, mutable receivers, enum names, and interfaces
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.EnumDecl {
				g.enum_names[stmt.name] = true
			}
			if stmt is ast.InterfaceDecl {
				g.interface_names[stmt.name] = true
				// Collect method names for this interface
				mut methods := []string{}
				for field in stmt.fields {
					methods << field.name
				}
				g.interface_meths[stmt.name] = methods
			}
			if stmt is ast.FnDecl {
				mut ret := 'void'
				mut ret_count := 1
				ret_expr := stmt.typ.return_type
				if ret_expr !is ast.EmptyExpr {
					// Check for multi-return (TupleType)
					if ret_expr is ast.Type {
						if ret_expr is ast.TupleType {
							ret_count = ret_expr.types.len
							ret = 'Tuple_${ret_count}_int'
						} else {
							ret = g.expr_type_to_c(ret_expr)
						}
					} else {
						ret = g.expr_type_to_c(ret_expr)
					}
				}

				// For C interop declarations (fn C.xxx), just register return type
				if stmt.language == .c {
					g.fn_types[stmt.name] = ret
					g.fn_ret_counts[stmt.name] = ret_count
					continue
				}

				if stmt.is_method {
					// For methods, use mangled name
					receiver_type := g.get_type_name_for_mangling(stmt.receiver.typ)
					mangled := '${receiver_type}__${stmt.name}'
					g.fn_types[mangled] = ret
					g.fn_ret_counts[mangled] = ret_count
					// Track if receiver is mutable or reference (&Type)
					g.mut_receivers[mangled] = stmt.receiver.is_mut
					// Check if receiver is a reference type (&Type)
					is_ref := stmt.receiver.typ is ast.PrefixExpr && stmt.receiver.typ.op == .amp
					g.ref_receivers[mangled] = is_ref
					// Track methods per type for vtable generation
					if receiver_type !in g.type_methods {
						g.type_methods[receiver_type] = []string{}
					}
					g.type_methods[receiver_type] << stmt.name
				} else {
					g.fn_types[stmt.name] = ret
					g.fn_ret_counts[stmt.name] = ret_count
				}
			}
		}
	}
	return g
}

// lookup_type_in_scope looks up a type by name in the environment's scopes.
// First tries current module scope, then builtin scope.
fn (g &Gen) lookup_type_in_scope(name string, module_name string) ?types.Type {
	if g.env == unsafe { nil } {
		return none
	}
	// Try module scope first
	mut scope := lock g.env.scopes {
		g.env.scopes[module_name] or {
			// Try builtin scope as fallback
			g.env.scopes['builtin'] or { return none }
		}
	}
	if obj := scope.lookup_parent(name, 0) {
		if obj is types.Type {
			return obj
		}
	}
	return none
}

// lookup_struct_type looks up a struct type by name from the environment.
// Tries current module scope first, then builtin scope.
fn (g &Gen) lookup_struct_type(name string) ?types.Struct {
	// Try environment - check current module, then builtin
	if typ := g.lookup_type_in_scope(name, g.cur_module) {
		if typ is types.Struct {
			return typ
		}
	}
	if typ := g.lookup_type_in_scope(name, 'builtin') {
		if typ is types.Struct {
			return typ
		}
	}
	return none
}

// get_struct_field_type gets the type of a struct field using the environment.
// Returns the C type name for the field if found.
fn (g &Gen) get_struct_field_type_from_env(struct_name string, field_name string) ?string {
	if struct_type := g.lookup_struct_type(struct_name) {
		for field in struct_type.fields {
			if field.name == field_name {
				return g.types_type_to_c(field.typ)
			}
		}
	}
	return none
}

// types_type_to_c converts a types.Type to a C type string
fn (g &Gen) types_type_to_c(t types.Type) string {
	match t {
		types.Primitive {
			if t.props.has(.integer) {
				size := if t.size == 0 { 64 } else { int(t.size) }
				signed := !t.props.has(.unsigned)
				return if signed {
					match size {
						8 { 'i8' }
						16 { 'i16' }
						32 { 'i32' }
						else { 'i64' }
					}
				} else {
					match size {
						8 { 'u8' }
						16 { 'u16' }
						32 { 'u32' }
						else { 'u64' }
					}
				}
			} else if t.props.has(.float) {
				return if t.size == 32 { 'f32' } else { 'f64' }
			} else if t.props.has(.boolean) {
				return 'bool'
			}
			return 'i64'
		}
		types.Pointer {
			base := g.types_type_to_c(t.base_type)
			return '${base}*'
		}
		types.Array {
			return 'Array' // Arrays use generic Array struct
		}
		types.Struct {
			return t.name
		}
		types.String {
			return 'string'
		}
		types.Alias {
			return t.name
		}
		types.Char {
			return 'char'
		}
		types.Void {
			return 'void'
		}
		else {
			return 'int' // fallback
		}
	}
}

pub fn (mut g Gen) gen() string {
	g.sb.writeln('// Generated by V Clean C Backend')
	g.sb.writeln('#include <stdio.h>')
	g.sb.writeln('#include <stdlib.h>')
	g.sb.writeln('#include <stdbool.h>')
	g.sb.writeln('#include <stdint.h>')
	g.sb.writeln('#include <stddef.h>')
	g.sb.writeln('#include <string.h>')
	g.sb.writeln('#include <float.h>')
	g.sb.writeln('')

	// V primitive type aliases (these are built-in to V, not defined in V code)
	g.sb.writeln('// V primitive types')
	g.sb.writeln('typedef int8_t i8;')
	g.sb.writeln('typedef int16_t i16;')
	g.sb.writeln('typedef int32_t i32;')
	g.sb.writeln('typedef int64_t i64;')
	g.sb.writeln('typedef uint8_t u8;')
	g.sb.writeln('typedef uint16_t u16;')
	g.sb.writeln('typedef uint32_t u32;')
	g.sb.writeln('typedef uint64_t u64;')
	g.sb.writeln('typedef float f32;')
	g.sb.writeln('typedef double f64;')
	g.sb.writeln('typedef u8 byte;')
	g.sb.writeln('typedef size_t usize;')
	g.sb.writeln('typedef ptrdiff_t isize;')
	g.sb.writeln('typedef u32 rune;')
	g.sb.writeln('typedef char* byteptr;')
	g.sb.writeln('typedef char* charptr;')
	g.sb.writeln('typedef void* voidptr;')
	g.sb.writeln('typedef void* chan;')
	g.sb.writeln('typedef double float_literal;')
	g.sb.writeln('typedef int64_t int_literal;')
	g.sb.writeln('')
	// Tuple types for multi-return functions
	g.sb.writeln('typedef struct { int f0; int f1; } Tuple_2_int;')
	g.sb.writeln('typedef struct { int f0; int f1; int f2; } Tuple_3_int;')
	g.sb.writeln('typedef struct { int f0; int f1; int f2; int f3; } Tuple_4_int;')
	g.sb.writeln('')
	// Wyhash stubs (C interop functions from hash module)
	g.sb.writeln('static const u64 _wyp[4] = {0xa0761d6478bd642full, 0xe7037ed1a0b428dbull, 0x8ebc6af09c88c6e3ull, 0x589965cc75374cc3ull};')
	g.sb.writeln('static inline u64 wyhash(void* key, u64 len, u64 seed, const u64* secret) { (void)key; (void)len; (void)seed; (void)secret; return 0; }')
	g.sb.writeln('static inline u64 wyhash64(u64 a, u64 b) { (void)a; (void)b; return 0; }')
	g.sb.writeln('')

	// 1. Struct/Union Declarations (Typedefs)
	mut seen_structs := map[string]bool{}
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				// Skip C interop, builtins, and generic structs
				if stmt.language == .c || stmt.name == 'Array' || stmt.generic_params.len > 0 {
					continue
				}
				// Generate mangled name with module prefix
				mangled_name := if file_module != '' && file_module != 'main'
					&& file_module != 'builtin' {
					'${file_module}__${stmt.name}'
				} else {
					stmt.name
				}
				// Skip duplicates (same struct may appear in multiple files)
				if mangled_name in seen_structs {
					continue
				}
				seen_structs[mangled_name] = true
				g.known_types[mangled_name] = true
				keyword := if stmt.is_union { 'union' } else { 'struct' }
				g.sb.writeln('typedef ${keyword} ${mangled_name} ${mangled_name};')
			}
		}
	}
	// Also forward-declare interfaces
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		for stmt in file.stmts {
			if stmt is ast.InterfaceDecl {
				mangled_name := if file_module != '' && file_module != 'main'
					&& file_module != 'builtin' {
					'${file_module}__${stmt.name}'
				} else {
					stmt.name
				}
				g.known_types[mangled_name] = true
				g.sb.writeln('typedef struct ${mangled_name} ${mangled_name};')
			}
		}
	}
	// Forward-declare sum types
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		for stmt in file.stmts {
			if stmt is ast.TypeDecl {
				if stmt.variants.len > 0 && stmt.language != .c {
					mangled_name := if file_module != '' && file_module != 'main'
						&& file_module != 'builtin' {
						'${file_module}__${stmt.name}'
					} else {
						stmt.name
					}
					g.known_types[mangled_name] = true
					g.sb.writeln('typedef struct ${mangled_name} ${mangled_name};')
				}
			}
		}
	}
	g.sb.writeln('')

	// 2. Enum Declarations (must come before structs that use enum fields)
	mut seen_enums := map[string]bool{}
	for file in g.files {
		for stmt in file.stmts {
			if stmt is ast.EnumDecl {
				if stmt.name in seen_enums {
					continue
				}
				seen_enums[stmt.name] = true
				g.gen_enum_decl(stmt)
				g.sb.writeln('')
			}
		}
	}

	// 2.5. Type Declarations (type aliases, must come before structs that use them)
	// Skip types that conflict with cleanc runtime or with struct definitions
	cleanc_builtin_types := ['byte', 'string', 'Array', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16',
		'u32', 'u64', 'f32', 'f64', 'rune', 'usize', 'isize']
	mut seen_types := map[string]bool{}
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		for stmt in file.stmts {
			if stmt is ast.TypeDecl {
				// Generate mangled name with module prefix
				mangled_name := if file_module != '' && file_module != 'main'
					&& file_module != 'builtin' {
					'${file_module}__${stmt.name}'
				} else {
					stmt.name
				}
				// Skip C interop, duplicates, and types that conflict with runtime/structs
				if stmt.language == .c || mangled_name in seen_types
					|| stmt.name in cleanc_builtin_types || mangled_name in seen_structs {
					continue
				}
				seen_types[mangled_name] = true
				g.known_types[mangled_name] = true
				g.gen_type_decl_with_name(stmt, mangled_name)
				g.sb.writeln('')
			}
		}
	}

	// 2.6. Interface Declarations (struct definitions)
	mut seen_ifaces := map[string]bool{}
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		for stmt in file.stmts {
			if stmt is ast.InterfaceDecl {
				mangled_name := if file_module != '' && file_module != 'main'
					&& file_module != 'builtin' {
					'${file_module}__${stmt.name}'
				} else {
					stmt.name
				}
				if mangled_name in seen_ifaces {
					continue
				}
				seen_ifaces[mangled_name] = true
				g.gen_interface_decl_with_name(stmt, mangled_name)
				g.sb.writeln('')
			}
		}
	}

	// 3. Struct Definitions - collect and sort by dependencies
	mut all_structs := map[string]ast.StructDecl{}
	mut struct_modules := map[string]string{} // mangled_name -> module
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				// Skip C interop, builtins, and generic structs
				if stmt.language == .c || stmt.name == 'Array' || stmt.generic_params.len > 0 {
					continue
				}
				// Generate mangled name with module prefix
				mangled_name := if file_module != '' && file_module != 'main'
					&& file_module != 'builtin' {
					'${file_module}__${stmt.name}'
				} else {
					stmt.name
				}
				if mangled_name !in all_structs {
					all_structs[mangled_name] = stmt
					struct_modules[mangled_name] = file_module
					// Track field types for type inference
					mut fields := map[string]string{}
					for field in stmt.fields {
						fields[field.name] = g.get_type_name_for_mangling(field.typ)
					}
					g.struct_fields[mangled_name] = fields.clone()
				}
			}
		}
	}
	// Topological sort: emit structs with resolved dependencies first
	mut emitted := map[string]bool{}
	for _ in 0 .. all_structs.len {
		for mangled_name, decl in all_structs {
			if mangled_name in emitted {
				continue
			}
			// Check if all non-pointer field types are emitted
			mut ready := true
			for field in decl.fields {
				dep := g.get_embedded_struct_type(field.typ)
				if dep != '' && dep in all_structs && dep !in emitted {
					ready = false
					break
				}
			}
			if ready {
				emitted[mangled_name] = true
				g.gen_struct_decl_with_name(decl, mangled_name)
				g.sb.writeln('')
			}
		}
	}
	// Emit remaining (circular deps)
	for mangled_name, decl in all_structs {
		if mangled_name !in emitted {
			g.gen_struct_decl_with_name(decl, mangled_name)
			g.sb.writeln('')
		}
	}

	// 3. Globals
	for file in g.files {
		for stmt in file.stmts {
			if stmt is ast.GlobalDecl {
				g.gen_global_decl(stmt)
				g.sb.writeln('')
			}
		}
	}

	// 3.5. Constants
	mut seen_consts := map[string]bool{}
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		for stmt in file.stmts {
			if stmt is ast.ConstDecl {
				g.gen_const_decl(stmt, file_module, mut seen_consts)
				g.sb.writeln('')
			}
		}
	}

	// 4. Function Prototypes
	mut seen_fns := map[string]bool{}
	for file in g.files {
		g.cur_module = g.file_modules[file.name] or { '' }
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				// Skip C interop, generic functions, and methods with unknown receiver types
				if stmt.language == .c || stmt.typ.generic_params.len > 0 {
					continue
				}
				if stmt.is_method && g.get_type_name_for_mangling(stmt.receiver.typ) == '' {
					continue
				}
				// Skip functions with empty names
				if stmt.name == '' {
					continue
				}
				// Generate module-prefixed function name to avoid conflicts
				fn_name := g.get_fn_name(stmt)
				if fn_name == '' || fn_name in seen_fns {
					continue
				}
				seen_fns[fn_name] = true
				g.gen_fn_head(stmt)
				g.sb.writeln(';')
			}
		}
	}
	g.sb.writeln('')

	// 4.5. Interface vtable wrapper functions
	g.gen_interface_wrappers()

	// 5. Functions
	seen_fns.clear()
	for file in g.files {
		g.cur_module = g.file_modules[file.name] or { '' }
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				// Skip C interop, generic functions, and methods with unknown receiver types
				if stmt.language == .c || stmt.typ.generic_params.len > 0 {
					continue
				}
				if stmt.is_method && g.get_type_name_for_mangling(stmt.receiver.typ) == '' {
					continue
				}
				// Skip functions with empty names
				if stmt.name == '' {
					continue
				}
				// Skip duplicate functions or stdlib conflicts
				fn_name := g.get_fn_name(stmt)
				if fn_name == '' || fn_name in seen_fns {
					continue
				}
				seen_fns[fn_name] = true
				g.gen_fn_decl(stmt)
				g.sb.writeln('')
			}
		}
	}

	return g.sb.str()
}

fn (mut g Gen) type_to_c(t ast.Type) string {
	s := t.str()
	if s == 'string' {
		return 'string'
	}
	return s
}

fn (mut g Gen) expr_type_to_c(e ast.Expr) string {
	match e {
		ast.Ident {
			name := e.name
			if name == 'int' || name == 'i64' || name == 'i32' || name == 'i16' || name == 'i8' {
				return 'int'
			}
			if name == 'u64' || name == 'u32' || name == 'u16' || name == 'u8' || name == 'byte' {
				return 'unsigned int'
			}
			if name == 'bool' {
				return 'bool'
			}
			if name == 'string' {
				return 'string'
			}
			if name == 'voidptr' {
				return 'void*'
			}
			if name == 'charptr' {
				return 'char*'
			}
			// Check if this type exists with current module prefix in known_types
			if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
				mangled_name := '${g.cur_module}__${name}'
				if mangled_name in g.known_types {
					return mangled_name
				}
			}
			// Assume it's a struct type (builtin or already mangled)
			return name
		}
		ast.PrefixExpr {
			// Handle pointer types like &Point
			if e.op == .amp {
				return g.expr_type_to_c(e.expr) + '*'
			}
			return 'void*'
		}
		ast.ModifierExpr {
			// Handle mut, shared, etc.
			return g.expr_type_to_c(e.expr)
		}
		ast.SelectorExpr {
			// Handle qualified type names like strconv.Float64u -> strconv__Float64u
			if e.lhs is ast.Ident {
				return '${e.lhs.name}__${e.rhs.name}'
			}
			// Nested selectors - recursively process
			return g.expr_type_to_c(e.lhs) + '__${e.rhs.name}'
		}
		ast.EmptyExpr {
			return 'void'
		}
		ast.Type {
			// Handle Type variants
			if e is ast.MapType {
				return 'Map_int_int'
			}
			if e is ast.TupleType {
				// Multi-return tuple type
				return 'Tuple_${e.types.len}_int'
			}
			// Handle Option and Result types
			if e is ast.OptionType {
				// For ?T, we use the base type (simplified - no proper optional handling)
				return g.expr_type_to_c(e.base_type)
			}
			if e is ast.ResultType {
				// For !T, we use the base type (simplified - no proper result handling)
				return g.expr_type_to_c(e.base_type)
			}
			return 'int'
		}
		else {
			return 'int'
		}
	}
}

fn (mut g Gen) infer_type(node ast.Expr) string {
	match node {
		ast.BasicLiteral {
			if node.kind == .number {
				// Check if it's a float literal (contains decimal point or exponent)
				if node.value.contains('.') || node.value.contains('e') || node.value.contains('E') {
					return 'double'
				}
				return 'int'
			}
			if node.kind in [.key_true, .key_false] {
				return 'bool'
			}
		}
		ast.StringLiteral {
			if node.value.starts_with("c'") {
				return 'char*'
			}
			return 'string'
		}
		ast.StringInterLiteral {
			return 'string'
		}
		ast.ArrayInitExpr {
			// For array literals, infer element type from first expr
			if node.exprs.len > 0 {
				elem_type := g.infer_type(node.exprs[0])
				return 'Array_${elem_type}'
			}
			return 'Array_int'
		}
		ast.InitExpr {
			// Check if it's a map type
			if node.typ is ast.Type {
				if node.typ is ast.MapType {
					return 'Map_int_int'
				}
			}
			return g.expr_type_to_c(node.typ)
		}
		ast.PrefixExpr {
			if node.op == .amp {
				// Address-of: &x -> pointer to type of x
				inner := g.infer_type(node.expr)
				return inner + '*'
			}
			if node.op == .mul {
				// Dereference: *x -> base type of pointer
				inner := g.infer_type(node.expr)
				if inner.ends_with('*') {
					return inner[..inner.len - 1]
				}
			}
			return g.infer_type(node.expr)
		}
		ast.CallExpr {
			mut name := ''
			if node.lhs is ast.Ident {
				name = node.lhs.name
			} else if node.lhs is ast.SelectorExpr {
				// Method call - look up method return type
				name = node.lhs.rhs.name
				// Get receiver type
				receiver_type := g.infer_type(node.lhs.lhs)
				clean_type := if receiver_type.ends_with('*') {
					receiver_type[..receiver_type.len - 1]
				} else {
					receiver_type
				}
				mangled := '${clean_type}__${name}'
				if t := g.fn_types[mangled] {
					return t
				}
			}

			if t := g.fn_types[name] {
				return t
			}
			return 'int'
		}
		ast.CallOrCastExpr {
			mut name := ''
			if node.lhs is ast.Ident {
				name = node.lhs.name
				// Check if this is interface boxing - return the interface type
				if name in g.interface_names {
					return name
				}
				// Check if this is a type cast - return the type name
				if name in ['u8', 'i8', 'u16', 'i16', 'u32', 'i32', 'u64', 'i64', 'f32', 'f64',
					'int', 'bool', 'byte', 'rune', 'voidptr', 'charptr', 'byteptr', 'usize', 'isize',
					'string'] {
					return name
				}
			} else if node.lhs is ast.SelectorExpr {
				// Handle C function calls (C.putchar, etc.)
				if node.lhs.lhs is ast.Ident {
					if node.lhs.lhs.name == 'C' {
						name = node.lhs.rhs.name
					}
				}
				// Handle method calls - look up method return type
				if name == '' {
					name = node.lhs.rhs.name
					receiver_type := g.infer_type(node.lhs.lhs)
					clean_type := if receiver_type.ends_with('*') {
						receiver_type[..receiver_type.len - 1]
					} else {
						receiver_type
					}
					mangled := '${clean_type}__${name}'
					if t := g.fn_types[mangled] {
						return t
					}
				}
			}
			if t := g.fn_types[name] {
				return t
			}
			return 'int'
		}
		ast.Ident {
			if t := g.var_types[node.name] {
				return t
			}
			return 'int'
		}
		ast.ParenExpr {
			return g.infer_type(node.expr)
		}
		ast.InfixExpr {
			return g.infer_type(node.lhs)
		}
		ast.SelectorExpr {
			// Check if this is an enum value access (EnumName.value)
			if node.lhs is ast.Ident {
				if node.lhs.name in g.enum_names {
					return node.lhs.name
				}
			}
			// Try to look up field type from struct definitions
			base_type := g.infer_type(node.lhs)
			// Strip pointer suffix for struct lookup
			clean_base := if base_type.ends_with('*') {
				base_type[..base_type.len - 1]
			} else {
				base_type
			}
			if fields := g.struct_fields[clean_base] {
				if field_type := fields[node.rhs.name] {
					return field_type
				}
			}
			return 'int'
		}
		ast.IndexExpr {
			// Check if this is a slice (range) operation
			if node.expr is ast.RangeExpr {
				// Slice returns same type as the base array
				return g.infer_type(node.lhs)
			}
			// For regular array/map indexing, get the element type
			base_type := g.infer_type(node.lhs)
			if base_type.starts_with('Map_') {
				// Map_int_int -> return int (the value type)
				return 'int'
			}
			if base_type.starts_with('Array_') {
				return base_type['Array_'.len..]
			}
			return 'int'
		}
		ast.ModifierExpr {
			return g.infer_type(node.expr)
		}
		ast.RangeExpr {
			return 'Range_'
		}
		ast.MapInitExpr {
			return 'Map_int_int'
		}
		ast.UnsafeExpr {
			// Unsafe block - infer type from last expression statement
			if node.stmts.len > 0 {
				last_stmt := node.stmts[node.stmts.len - 1]
				if last_stmt is ast.ExprStmt {
					return g.infer_type(last_stmt.expr)
				}
			}
			return 'int'
		}
		else {
			return 'int'
		}
	}
	return ''
}

// Convert operator symbols to valid C identifier names
fn operator_to_name(op string) string {
	return match op {
		'+' { '__plus' }
		'-' { '__minus' }
		'*' { '__mul' }
		'/' { '__div' }
		'%' { '__mod' }
		'==' { '__eq' }
		'!=' { '__ne' }
		'<' { '__lt' }
		'>' { '__gt' }
		'<=' { '__le' }
		'>=' { '__ge' }
		'|' { '__pipe' }
		'^' { '__xor' }
		else { op }
	}
}

// Get the full function name with module prefix
fn (g Gen) get_fn_name(node ast.FnDecl) string {
	// Skip C stdlib functions that would conflict
	if node.name in c_stdlib_fns {
		return ''
	}
	// Convert operator names to valid C identifiers
	name := operator_to_name(node.name)
	mut fn_name := name
	if node.is_method {
		base_type := g.get_type_name_for_mangling(node.receiver.typ)
		// Operator names already have __ prefix (e.g., __plus), regular methods need __
		if name.starts_with('__') {
			fn_name = '${base_type}${name}'
		} else {
			fn_name = '${base_type}__${name}'
		}
	}
	// Add module prefix for non-main modules
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		return '${g.cur_module}__${fn_name}'
	}
	return fn_name
}

// C keywords that need escaping when used as identifiers
const c_keywords = ['auto', 'break', 'case', 'char', 'const', 'continue', 'default', 'do', 'double',
	'else', 'enum', 'extern', 'float', 'for', 'goto', 'if', 'inline', 'int', 'long', 'register',
	'restrict', 'return', 'short', 'signed', 'sizeof', 'static', 'struct', 'switch', 'typedef',
	'union', 'unsigned', 'void', 'volatile', 'while', '_Bool', '_Complex', '_Imaginary']

// C stdlib functions that should never be generated (they conflict with libc)
const c_stdlib_fns = ['malloc', 'calloc', 'realloc', 'free', 'atoi', 'atof', 'atol', 'memcpy',
	'memset', 'memmove', 'strlen', 'strcpy', 'strcat', 'strcmp', 'memcmp']

fn escape_c_keyword(name string) string {
	if name in c_keywords {
		return '_${name}'
	}
	return name
}

// Get embedded (non-pointer) struct type name from a field type, or empty string
fn (g Gen) get_embedded_struct_type(typ ast.Expr) string {
	match typ {
		ast.Ident {
			// Simple identifier - could be a struct type
			// Exclude primitives
			if typ.name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32',
				'f64', 'bool', 'rune', 'byte', 'voidptr', 'charptr', 'usize', 'isize'] {
				return ''
			}
			return typ.name
		}
		ast.PrefixExpr {
			// Pointer type (&Foo) - not an embedded struct
			return ''
		}
		ast.ModifierExpr {
			return g.get_embedded_struct_type(typ.expr)
		}
		ast.Type {
			// Array, Map, Option, etc. - not simple embedded structs
			return ''
		}
		else {
			return ''
		}
	}
}

// Get a safe identifier for type mangling (no spaces or special chars)
fn (g Gen) get_type_name_for_mangling(typ ast.Expr) string {
	match typ {
		ast.Ident {
			return typ.name
		}
		ast.PrefixExpr {
			// Pointer type (&Foo) - use base type
			if typ.op == .amp || typ.op == .mul {
				return g.get_type_name_for_mangling(typ.expr)
			}
			return ''
		}
		ast.ModifierExpr {
			return g.get_type_name_for_mangling(typ.expr)
		}
		else {
			return ''
		}
	}
}

fn (mut g Gen) gen_struct_decl(node ast.StructDecl) {
	g.gen_struct_decl_with_name(node, node.name)
}

fn (mut g Gen) gen_struct_decl_with_name(node ast.StructDecl, mangled_name string) {
	keyword := if node.is_union { 'union' } else { 'struct' }
	g.sb.writeln('${keyword} ${mangled_name} {')
	for field in node.fields {
		g.write_indent()
		g.sb.write_string('\t')
		t := g.expr_type_to_c(field.typ)
		field_name := escape_c_keyword(field.name)
		g.sb.writeln('${t} ${field_name};')
	}
	g.sb.writeln('};')
}

fn (mut g Gen) gen_global_decl(node ast.GlobalDecl) {
	for field in node.fields {
		t := g.expr_type_to_c(field.typ)
		// Skip globals with void type (type inference failed)
		if t == 'void' {
			continue
		}
		g.sb.writeln('${t} ${field.name};')
	}
}

fn (mut g Gen) gen_const_decl(node ast.ConstDecl, file_module string, mut seen_consts map[string]bool) {
	for field in node.fields {
		// Generate mangled name with module prefix
		mangled_name := if file_module != '' && file_module != 'main' && file_module != 'builtin' {
			'${file_module}__${field.name}'
		} else {
			field.name
		}
		// Skip duplicate constants
		if mangled_name in seen_consts {
			continue
		}
		// Only generate simple literal constants
		if !g.is_simple_literal(field.value) {
			continue
		}
		t := g.infer_type(field.value)
		// Skip consts with problematic types
		if t == 'void' || t == '' || t.starts_with('Array_') {
			continue
		}
		seen_consts[mangled_name] = true
		g.sb.write_string('const ${t} ${mangled_name} = ')
		g.gen_expr(field.value)
		g.sb.writeln(';')
	}
}

// Check if expression is a simple compile-time literal
fn (g Gen) is_simple_literal(e ast.Expr) bool {
	match e {
		ast.BasicLiteral {
			return true
		}
		ast.StringLiteral {
			return true
		}
		ast.PrefixExpr {
			// Allow negation of literals: -5
			if e.op == .minus {
				return g.is_simple_literal(e.expr)
			}
			return false
		}
		ast.ParenExpr {
			return g.is_simple_literal(e.expr)
		}
		ast.CastExpr {
			// Allow type casts of literals: u64(0x123)
			return g.is_simple_literal(e.expr)
		}
		ast.CallOrCastExpr {
			// Allow type casts like u64(0x8000...) if they're primitive types
			if e.lhs is ast.Ident {
				if e.lhs.name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64',
					'f32', 'f64', 'bool', 'byte', 'rune', 'usize', 'isize'] {
					return g.is_simple_literal(e.expr)
				}
			}
			return false
		}
		else {
			return false
		}
	}
}

// Check if an expression references any identifier (which could be undefined)
fn (g Gen) expr_references_ident(e ast.Expr) bool {
	match e {
		ast.Ident {
			// Allow primitive type casts
			if e.name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64',
				'bool', 'byte', 'rune'] {
				return false
			}
			return true
		}
		ast.SelectorExpr {
			return g.expr_references_ident(e.lhs) || g.expr_references_ident(e.rhs)
		}
		ast.InfixExpr {
			return g.expr_references_ident(e.lhs) || g.expr_references_ident(e.rhs)
		}
		ast.PrefixExpr {
			return g.expr_references_ident(e.expr)
		}
		ast.ParenExpr {
			return g.expr_references_ident(e.expr)
		}
		ast.CastExpr {
			return g.expr_references_ident(e.expr)
		}
		ast.CallOrCastExpr {
			return g.expr_references_ident(e.expr)
		}
		ast.InitExpr {
			// Check field values
			for field in e.fields {
				if g.expr_references_ident(field.value) {
					return true
				}
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) gen_enum_decl(node ast.EnumDecl) {
	// Skip enums that reference C constants (they use macros from system headers)
	for field in node.fields {
		if g.expr_references_c(field.value) {
			return
		}
	}
	// Generate C enum declaration
	g.sb.writeln('typedef enum {')
	for i, field in node.fields {
		g.sb.write_string('\t${node.name}__${field.name}')
		if field.value !is ast.EmptyExpr {
			g.sb.write_string(' = ')
			g.gen_expr(field.value)
		}
		if i < node.fields.len - 1 {
			g.sb.writeln(',')
		} else {
			g.sb.writeln('')
		}
	}
	g.sb.writeln('} ${node.name};')
}

// Check if an expression references C interop (C.xxx)
fn (g Gen) expr_references_c(e ast.Expr) bool {
	match e {
		ast.SelectorExpr {
			if e.lhs is ast.Ident {
				if e.lhs.name == 'C' {
					return true
				}
			}
			return g.expr_references_c(e.lhs)
		}
		ast.InfixExpr {
			return g.expr_references_c(e.lhs) || g.expr_references_c(e.rhs)
		}
		ast.PrefixExpr {
			return g.expr_references_c(e.expr)
		}
		ast.ParenExpr {
			return g.expr_references_c(e.expr)
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) gen_interface_decl(node ast.InterfaceDecl) {
	g.gen_interface_decl_with_name(node, node.name)
}

fn (mut g Gen) gen_interface_decl_with_name(node ast.InterfaceDecl, mangled_name string) {
	// Generate C struct for interface (vtable-style)
	// Note: typedef forward declaration is already generated above
	g.sb.writeln('struct ${mangled_name} {')
	g.sb.writeln('\tvoid* _object;  // Pointer to concrete object')
	g.sb.writeln('\tint _type_id;   // Type identifier')
	// Generate function pointers for each method
	for field in node.fields {
		g.write_indent()
		g.sb.write_string('\t')
		// Interface fields can be method signatures or regular fields
		// FnType is wrapped: Expr -> Type -> FnType
		if field.typ is ast.Type {
			if field.typ is ast.FnType {
				// Method signature - generate function pointer
				fn_type := field.typ as ast.FnType
				mut ret := 'void'
				if fn_type.return_type !is ast.EmptyExpr {
					ret = g.expr_type_to_c(fn_type.return_type)
				}
				g.sb.write_string('${ret} (*${field.name})(void*')
				for param in fn_type.params {
					g.sb.write_string(', ')
					t := g.expr_type_to_c(param.typ)
					g.sb.write_string(t)
				}
				g.sb.writeln(');')
			} else {
				// Other type expression
				t := g.expr_type_to_c(field.typ)
				g.sb.writeln('${t} ${field.name};')
			}
		} else {
			// Regular field
			t := g.expr_type_to_c(field.typ)
			g.sb.writeln('${t} ${field.name};')
		}
	}
	g.sb.writeln('};')
}

// gen_interface_wrappers generates wrapper functions for interface vtables
// Each wrapper casts void* to the concrete type and calls the actual method
fn (mut g Gen) gen_interface_wrappers() {
	// For each interface
	for iface_name, methods in g.interface_meths {
		// For each concrete type that has these methods
		for type_name, type_meths in g.type_methods {
			// Check if this type implements all interface methods
			if !g.type_implements_interface(methods, type_meths) {
				continue
			}

			// Generate wrapper functions for this type implementing this interface
			for meth in methods {
				// Get return type from the registered function
				mangled := '${type_name}__${meth}'
				ret_type := g.fn_types[mangled] or { 'int' }

				// Generate: RetType InterfaceName_TypeName_method_wrapper(void* _obj) {
				//     return TypeName__method(*(TypeName*)_obj);
				// }
				g.sb.writeln('static ${ret_type} ${iface_name}_${type_name}_${meth}_wrapper(void* _obj) {')
				g.sb.writeln('\treturn ${type_name}__${meth}(*(${type_name}*)_obj);')
				g.sb.writeln('}')
				g.sb.writeln('')
			}
		}
	}
}

// type_implements_interface checks if a type has all the interface methods
fn (g Gen) type_implements_interface(iface_methods []string, type_methods []string) bool {
	for meth in iface_methods {
		if meth !in type_methods {
			return false
		}
	}
	return true
}

fn (mut g Gen) gen_type_decl(node ast.TypeDecl) {
	g.gen_type_decl_with_name(node, node.name)
}

fn (mut g Gen) gen_type_decl_with_name(node ast.TypeDecl, mangled_name string) {
	if node.variants.len > 0 {
		// Sum type: generate a tagged union
		// Note: typedef forward declaration is already generated above
		g.sb.writeln('struct ${mangled_name} {')
		g.sb.writeln('\tint _tag;')
		g.sb.writeln('\tunion {')
		for i, variant in node.variants {
			// Use a safe name for union fields (prefix with underscore to avoid reserved words)
			variant_name := if variant is ast.Ident {
				'_${variant.name}'
			} else {
				'_v${i}'
			}
			g.sb.writeln('\t\tvoid* ${variant_name};')
		}
		g.sb.writeln('\t} _data;')
		g.sb.writeln('};')
	} else if node.base_type !is ast.EmptyExpr {
		// Type alias: generate typedef
		base_type := g.expr_type_to_c(node.base_type)
		g.sb.writeln('typedef ${base_type} ${mangled_name};')
	}
}

fn (mut g Gen) gen_fn_head(node ast.FnDecl) {
	mut ret := 'void'
	ret_expr := node.typ.return_type
	if ret_expr !is ast.EmptyExpr {
		ret = g.expr_type_to_c(ret_expr)
	}
	if node.name == 'main' {
		ret = 'int'
	}

	// Get module-prefixed function name
	fn_name := g.get_fn_name(node)

	mut has_receiver := false
	mut receiver_type := ''
	mut receiver_name := ''
	mut receiver_is_ptr := false
	if node.is_method {
		receiver_type = g.expr_type_to_c(node.receiver.typ)
		receiver_name = node.receiver.name
		receiver_is_ptr = node.receiver.is_mut
		has_receiver = true
	}

	g.sb.write_string('${ret} ${fn_name}(')

	mut first := true
	// Add receiver as first parameter for methods
	if has_receiver {
		if receiver_is_ptr {
			g.sb.write_string('${receiver_type}* ${receiver_name}')
		} else {
			g.sb.write_string('${receiver_type} ${receiver_name}')
		}
		first = false
	}

	for param in node.typ.params {
		if !first {
			g.sb.write_string(', ')
		}
		first = false
		t := g.expr_type_to_c(param.typ)
		if param.is_mut {
			g.sb.write_string('${t}* ${param.name}')
		} else {
			g.sb.write_string('${t} ${param.name}')
		}
	}
	g.sb.write_string(')')
}

fn (mut g Gen) gen_fn_decl(node ast.FnDecl) {
	g.var_types = map[string]string{}
	g.defer_stmts.clear()

	// Register receiver for methods
	if node.is_method {
		receiver_type := g.expr_type_to_c(node.receiver.typ)
		if node.receiver.is_mut {
			g.var_types[node.receiver.name] = receiver_type + '*'
		} else {
			g.var_types[node.receiver.name] = receiver_type
		}
	}

	// Register params
	for param in node.typ.params {
		t := g.expr_type_to_c(param.typ)
		if param.is_mut {
			g.var_types[param.name] = t + '*'
		} else {
			g.var_types[param.name] = t
		}
	}

	g.gen_fn_head(node)
	g.sb.writeln(' {')
	g.indent++
	g.gen_stmts(node.stmts)

	// Emit deferred statements before implicit return (for all functions)
	g.emit_deferred_stmts()

	if node.name == 'main' {
		g.write_indent()
		g.sb.writeln('return 0;')
	}
	g.indent--
	g.sb.writeln('}')
}

fn (mut g Gen) gen_stmts(stmts []ast.Stmt) {
	for s in stmts {
		g.gen_stmt(s)
	}
}

fn (mut g Gen) gen_stmt(node ast.Stmt) {
	match node {
		ast.AssignStmt {
			// Handle multi-return assignments: a, b := func()
			if node.lhs.len > 1 && node.rhs.len == 1 && node.op == .decl_assign {
				g.gen_multi_return_assign(node)
				return
			}
			lhs := node.lhs[0]
			rhs := node.rhs[0]
			// Check if LHS is blank identifier '_'
			mut is_blank := false
			if lhs is ast.Ident {
				if lhs.name == '_' {
					is_blank = true
				}
			}
			if is_blank {
				// Skip blank identifier assignments, but evaluate RHS for side effects
				g.write_indent()
				g.sb.write_string('(void)(')
				g.gen_expr(rhs)
				g.sb.writeln(');')
				return
			}
			g.write_indent()
			if node.op == .decl_assign {
				// var decl
				mut name := ''
				if lhs is ast.Ident {
					name = lhs.name
				} else if lhs is ast.ModifierExpr {
					// Handle mut x := ...
					if lhs.expr is ast.Ident {
						name = lhs.expr.name
					}
				}
				typ := g.infer_type(rhs)
				g.var_types[name] = typ
				g.sb.write_string('${typ} ${name} = ')
				g.gen_expr(rhs)
				g.sb.writeln(';')
			} else {
				// assignment
				// Check if LHS is a map index expression
				if lhs is ast.IndexExpr {
					lhs_type := g.infer_type(lhs.lhs)
					if lhs_type.starts_with('Map_') {
						// Map assignment: __map_int_int_set(&m, key, val)
						g.sb.write_string('__map_int_int_set(&')
						g.gen_expr(lhs.lhs)
						g.sb.write_string(', ')
						g.gen_expr(lhs.expr)
						g.sb.write_string(', ')
						g.gen_expr(rhs)
						g.sb.writeln(');')
						return
					}
				}
				g.gen_expr(lhs)
				op_str := match node.op {
					.assign { '=' }
					.plus_assign { '+=' }
					.minus_assign { '-=' }
					.mul_assign { '*=' }
					.div_assign { '/=' }
					.mod_assign { '%=' }
					.and_assign { '&=' }
					.or_assign { '|=' }
					.xor_assign { '^=' }
					.left_shift_assign { '<<=' }
					.right_shift_assign { '>>=' }
					else { '=' }
				}
				g.sb.write_string(' ${op_str} ')
				g.gen_expr(rhs)
				g.sb.writeln(';')
			}
		}
		ast.ExprStmt {
			g.write_indent()
			g.gen_expr(node.expr)
			g.sb.writeln(';')
		}
		ast.ReturnStmt {
			// Handle multi-return: return a, b -> return (Tuple_N_int){a, b}
			if node.exprs.len > 1 {
				// Emit deferred statements first (if any)
				g.emit_deferred_stmts()
				g.write_indent()
				g.sb.write_string('return (Tuple_${node.exprs.len}_int){')
				for i, expr in node.exprs {
					if i > 0 {
						g.sb.write_string(', ')
					}
					g.gen_expr(expr)
				}
				g.sb.writeln('};')
			} else if node.exprs.len > 0 && g.defer_stmts.len > 0 {
				// In V/Go semantics: evaluate return value FIRST, then run defer
				// Store return value in temp variable before running defers
				ret_type := g.infer_type(node.exprs[0])
				g.write_indent()
				g.sb.write_string('${ret_type} __ret_val = ')
				g.gen_expr(node.exprs[0])
				g.sb.writeln(';')
				// Emit deferred statements
				g.emit_deferred_stmts()
				// Return the stored value
				g.write_indent()
				g.sb.writeln('return __ret_val;')
			} else {
				// Emit deferred statements (if any)
				g.emit_deferred_stmts()
				g.write_indent()
				g.sb.write_string('return')
				if node.exprs.len > 0 {
					g.sb.write_string(' ')
					g.gen_expr(node.exprs[0])
				}
				g.sb.writeln(';')
			}
		}
		ast.DeferStmt {
			// Collect deferred statements to be executed before return
			g.defer_stmts << node.stmts
		}
		ast.BlockStmt {
			g.write_indent()
			g.sb.writeln('{')
			g.indent++
			g.gen_stmts(node.stmts)
			g.indent--
			g.write_indent()
			g.sb.writeln('}')
		}
		ast.ForStmt {
			// Check for for-in: `for i in 1..10` or `for elem in array`
			if node.init is ast.ForInStmt {
				g.gen_for_in(node, node.init)
				return
			}

			g.write_indent()
			if node.init is ast.EmptyStmt && node.cond is ast.EmptyExpr
				&& node.post is ast.EmptyStmt {
				g.sb.writeln('while (1) {')
			} else if node.init is ast.EmptyStmt && node.post is ast.EmptyStmt {
				g.sb.write_string('while (')
				g.gen_expr(node.cond)
				g.sb.writeln(') {')
			} else {
				g.sb.write_string('for (')
				if node.init !is ast.EmptyStmt {
					g.gen_stmt_inline(node.init)
				}
				g.sb.write_string('; ')
				if node.cond !is ast.EmptyExpr {
					g.gen_expr(node.cond)
				}
				g.sb.write_string('; ')
				if node.post !is ast.EmptyStmt {
					g.gen_stmt_inline(node.post)
				}
				g.sb.writeln(') {')
			}
			g.indent++
			g.gen_stmts(node.stmts)
			g.indent--
			g.write_indent()
			g.sb.writeln('}')
		}
		ast.FlowControlStmt {
			g.write_indent()
			if node.op == .key_break {
				g.sb.writeln('break;')
			} else {
				g.sb.writeln('continue;')
			}
		}
		ast.AssertStmt {
			g.write_indent()
			g.sb.write_string('if (!(')
			g.gen_expr(node.expr)
			g.sb.writeln(')) { fprintf(stderr, "Assertion failed\\n"); exit(1); }')
		}
		ast.LabelStmt {
			// Labels for labeled loops (break/continue targets)
			// Generate: label_name: stmt
			g.write_indent()
			g.sb.write_string('${node.name}:')
			if node.stmt !is ast.EmptyStmt {
				g.sb.writeln('')
				g.gen_stmt(node.stmt)
			} else {
				g.sb.writeln(';')
			}
		}
		else {
			g.sb.writeln('// Unhandled stmt: ${node.type_name()}')
		}
	}
}

fn (mut g Gen) gen_stmt_inline(node ast.Stmt) {
	match node {
		ast.AssignStmt {
			lhs := node.lhs[0]
			rhs := node.rhs[0]
			if node.op == .decl_assign {
				mut name := ''
				if lhs is ast.Ident {
					name = lhs.name
				}
				t := g.infer_type(rhs)
				g.var_types[name] = t
				g.sb.write_string('${t} ${name} = ')
				g.gen_expr(rhs)
			} else {
				g.gen_expr(lhs)
				op := match node.op {
					.assign { '=' }
					.plus_assign { '+=' }
					.minus_assign { '-=' }
					else { '=' }
				}
				g.sb.write_string(' ${op} ')
				g.gen_expr(rhs)
			}
		}
		ast.ExprStmt {
			g.gen_expr(node.expr)
		}
		else {}
	}
}

fn (mut g Gen) gen_expr(node ast.Expr) {
	match node {
		ast.BasicLiteral {
			if node.kind == .key_true {
				g.sb.write_string('true')
			} else if node.kind == .key_false {
				g.sb.write_string('false')
			} else if node.kind == .char {
				// Character literal: wrap in single quotes for C
				g.sb.write_string("'${node.value}'")
			} else {
				mut val := node.value.replace('_', '') // Strip underscores
				// Convert V octal (0o777) to C octal (0777)
				if val.starts_with('0o') || val.starts_with('0O') {
					val = '0' + val[2..]
				}
				g.sb.write_string(val)
			}
		}
		ast.StringLiteral {
			if node.value.starts_with("c'") {
				val := node.value.trim("c'").trim("'")
				g.sb.write_string('"${val}"')
			} else {
				val := node.value.trim("'").trim('"')
				// Escape double quotes for C string
				escaped := val.replace('"', '\\"')
				g.sb.write_string('(string){"${escaped}", ${val.len}}')
			}
		}
		ast.StringInterLiteral {
			// String interpolation: 'prefix${a}middle${b}suffix'
			// Generate: ({ char buf[256]; sprintf(buf, "prefix%lldmiddle%lldsuffix", a, b); (string){buf, strlen(buf)}; })
			g.sb.write_string('({ char _buf[256]; sprintf(_buf, "')
			// Build format string
			for i, val in node.values {
				// Strip quotes from the first and last value parts
				mut clean_val := val
				if i == 0 {
					// First part: strip leading quote
					clean_val = clean_val.trim_left("'").trim_left('"')
				}
				if i == node.values.len - 1 {
					// Last part: strip trailing quote
					clean_val = clean_val.trim_right("'").trim_right('"')
				}
				g.sb.write_string(clean_val)
				if i < node.inters.len {
					inter := node.inters[i]
					// Check if expression is a string type - use %s format
					expr_type := g.infer_type(inter.expr)
					if expr_type == 'string' && inter.format == .unformatted {
						g.sb.write_string('%s')
					} else {
						g.sb.write_string(g.get_printf_format(inter))
					}
				}
			}
			g.sb.write_string('"')
			// Add arguments
			for inter in node.inters {
				g.sb.write_string(', ')
				// For string types, we need to pass the .str field (C string pointer)
				expr_type := g.infer_type(inter.expr)
				if expr_type == 'string' {
					g.gen_expr(inter.expr)
					g.sb.write_string('.str')
				} else {
					g.gen_expr(inter.expr)
				}
			}
			g.sb.write_string('); (string){_buf, strlen(_buf)}; })')
		}
		ast.Ident {
			g.sb.write_string(node.name)
		}
		ast.ParenExpr {
			g.sb.write_string('(')
			g.gen_expr(node.expr)
			g.sb.write_string(')')
		}
		ast.PrefixExpr {
			// Check for heap allocation: &StructType{...}
			if node.op == .amp && node.expr is ast.InitExpr {
				// Heap allocation using a helper macro or inline malloc
				init_expr := node.expr as ast.InitExpr
				typ_name := g.expr_type_to_c(init_expr.typ)
				// Use a compound literal approach - create on heap
				g.sb.write_string('({ ${typ_name}* _tmp = (${typ_name}*)malloc(sizeof(${typ_name})); *_tmp = (${typ_name}){')
				for i, field in init_expr.fields {
					if i > 0 {
						g.sb.write_string(', ')
					}
					g.sb.write_string('.${field.name} = ')
					g.gen_expr(field.value)
				}
				g.sb.write_string('}; _tmp; })')
				return
			}
			// Check for pointer cast: &type(expr) -> (type*)(expr)
			// This is V syntax for casting to a pointer type
			if node.op == .amp && node.expr is ast.CallOrCastExpr {
				cast_expr := node.expr as ast.CallOrCastExpr
				if cast_expr.lhs is ast.Ident {
					type_name := cast_expr.lhs.name
					if type_name in ['u8', 'i8', 'u16', 'i16', 'u32', 'i32', 'u64', 'i64', 'f32',
						'f64', 'int', 'char', 'byte', 'rune', 'voidptr', 'charptr', 'byteptr',
						'string'] {
						// Generate pointer cast: (type*)(expr)
						g.sb.write_string('((${type_name}*)(')
						g.gen_expr(cast_expr.expr)
						g.sb.write_string('))')
						return
					}
				}
			}
			op := match node.op {
				.minus { '-' }
				.not { '!' }
				.amp { '&' }
				.mul { '*' }
				.bit_not { '~' }
				else { '' }
			}
			g.sb.write_string(op)
			g.gen_expr(node.expr)
		}
		ast.IfExpr {
			// Check if this if-expression can be converted to a ternary operator
			// (i.e., used as a value rather than a statement)
			if g.can_be_ternary(node) {
				// Generate C ternary: (cond) ? true_val : false_val
				g.sb.write_string('(')
				g.gen_expr(node.cond)
				g.sb.write_string(') ? (')
				g.gen_if_value(node.stmts)
				g.sb.write_string(') : (')
				g.gen_else_value(node.else_expr)
				g.sb.write_string(')')
				return
			}

			// Statement IF
			// First check if this is just an else block (no condition)
			if node.cond is ast.EmptyExpr {
				// This is a pure else block, just output the statements
				g.sb.writeln('{')
				g.indent++
				g.gen_stmts(node.stmts)
				g.indent--
				g.write_indent()
				g.sb.writeln('}')
				return
			}

			// Check if condition is an if-guard expression
			if node.cond is ast.IfGuardExpr {
				g.gen_if_guard(node, node.cond)
				return
			}

			g.write_indent()
			g.sb.write_string('if (')
			g.gen_expr(node.cond)
			g.sb.writeln(') {')
			g.indent++
			g.gen_stmts(node.stmts)
			g.indent--
			g.write_indent()
			g.sb.write_string('}')
			if node.else_expr !is ast.EmptyExpr {
				if node.else_expr is ast.IfExpr {
					// Check if this is else-if or pure else
					if node.else_expr.cond is ast.EmptyExpr {
						// Pure else block
						g.sb.writeln(' else {')
						g.indent++
						g.gen_stmts(node.else_expr.stmts)
						g.indent--
						g.write_indent()
						g.sb.writeln('}')
					} else {
						// Else-if chain
						g.sb.write_string(' else ')
						g.gen_expr(node.else_expr)
					}
				} else {
					// Some other expression in else
					g.sb.write_string(' else ')
					g.gen_expr(node.else_expr)
				}
			} else {
				g.sb.writeln('')
			}
		}
		ast.MatchExpr {
			g.write_indent()
			g.sb.write_string('switch (')
			g.gen_expr(node.expr)
			g.sb.writeln(') {')
			// Set match type context for enum shorthand (.value syntax)
			match_type := g.infer_type(node.expr)
			old_match_type := g.cur_match_type
			g.cur_match_type = match_type
			for branch in node.branches {
				if branch.cond.len == 0 {
					g.write_indent()
					g.sb.writeln('default:')
				} else {
					for c in branch.cond {
						g.write_indent()
						g.sb.write_string('case ')
						g.gen_expr(c)
						g.sb.writeln(':')
					}
				}
				g.indent++
				g.gen_stmts(branch.stmts)
				g.write_indent()
				g.sb.writeln('break;')
				g.indent--
			}
			g.cur_match_type = old_match_type
			g.write_indent()
			g.sb.writeln('}')
		}
		ast.InfixExpr {
			// Handle 'in' and '!in' operators with array literals specially
			if node.op == .key_in || node.op == .not_in {
				if node.rhs is ast.ArrayInitExpr {
					// x in [a, b, c] => (x == a || x == b || x == c)
					// x !in [a, b, c] => (x != a && x != b && x != c)
					cmp_op := if node.op == .key_in { '==' } else { '!=' }
					join_op := if node.op == .key_in { ' || ' } else { ' && ' }
					g.sb.write_string('(')
					for i, elem in node.rhs.exprs {
						if i > 0 {
							g.sb.write_string(join_op)
						}
						g.sb.write_string('(')
						g.gen_expr(node.lhs)
						g.sb.write_string(' ${cmp_op} ')
						g.gen_expr(elem)
						g.sb.write_string(')')
					}
					g.sb.write_string(')')
				} else {
					// For non-array RHS, generate a TODO marker
					g.sb.write_string('/* TODO: in/!in with non-array */')
				}
			} else if node.op == .plus && g.infer_type(node.lhs) == 'string' {
				// String concatenation: use string__plus instead of +
				g.sb.write_string('string__plus(')
				g.gen_expr(node.lhs)
				g.sb.write_string(', ')
				g.gen_expr(node.rhs)
				g.sb.write_string(')')
			} else {
				g.sb.write_string('(')
				g.gen_expr(node.lhs)
				op := match node.op {
					.plus { '+' }
					.minus { '-' }
					.mul { '*' }
					.div { '/' }
					.mod { '%' }
					.gt { '>' }
					.lt { '<' }
					.eq { '==' }
					.ne { '!=' }
					.ge { '>=' }
					.le { '<=' }
					.and { '&&' }
					.logical_or { '||' }
					.amp { '&' }
					.pipe { '|' }
					.xor { '^' }
					.left_shift { '<<' }
					.right_shift { '>>' }
					else { '?' }
				}
				g.sb.write_string(' ${op} ')
				g.gen_expr(node.rhs)
				g.sb.write_string(')')
			}
		}
		ast.CallExpr {
			mut name := ''
			mut is_method := false
			mut receiver_expr := ast.empty_expr
			if node.lhs is ast.Ident {
				name = node.lhs.name
			} else if node.lhs is ast.SelectorExpr {
				// Check if this is a C library call (C.putchar, C.puts, etc.)
				if node.lhs.lhs is ast.Ident {
					if node.lhs.lhs.name == 'C' {
						// C library function call
						name = node.lhs.rhs.name
						is_method = false
					} else if node.lhs.lhs.name in g.module_names {
						// Module-qualified function call: strconv.f64_to_str_l(x)
						name = '${node.lhs.lhs.name}__${node.lhs.rhs.name}'
						is_method = false
					} else {
						// Regular method call: obj.method(args)
						name = node.lhs.rhs.name
						receiver_expr = node.lhs.lhs
						is_method = true
					}
				} else {
					// Chained selector, treat as method call
					name = node.lhs.rhs.name
					receiver_expr = node.lhs.lhs
					is_method = true
				}
			}

			// Handle C function calls (C.puts, C.putchar, etc.) - fallback
			if name.starts_with('C.') {
				name = name[2..] // Strip the 'C.' prefix
			}

			if is_method {
				// Determine the receiver type to mangle the method name
				receiver_type := g.infer_type(receiver_expr)
				receiver_is_ptr := receiver_type.ends_with('*')
				// Clean up pointer prefix for type lookup
				clean_type := if receiver_is_ptr {
					receiver_type[..receiver_type.len - 1]
				} else {
					receiver_type
				}

				// Check if receiver is an interface type - call through vtable
				if clean_type in g.interface_names {
					// Interface method call: iface.method(args)
					// Generate: iface.method(iface._object, args)
					g.gen_expr(receiver_expr)
					g.sb.write_string('.${name}(')
					g.gen_expr(receiver_expr)
					g.sb.write_string('._object')
					if node.args.len > 0 {
						g.sb.write_string(', ')
					}
					for i, arg in node.args {
						if i > 0 {
							g.sb.write_string(', ')
						}
						if arg is ast.ModifierExpr {
							if arg.kind == .key_mut {
								g.sb.write_string('&')
								g.gen_expr(arg.expr)
							} else {
								g.gen_expr(arg)
							}
						} else {
							g.gen_expr(arg)
						}
					}
					g.sb.write_string(')')
					return
				}

				mangled := '${clean_type}__${name}'
				// Method expects pointer if receiver is mut OR reference (&Type)
				method_wants_ptr := g.mut_receivers[mangled] or { false } || g.ref_receivers[mangled] or {
					false
				}

				g.sb.write_string('${mangled}(')

				// Handle receiver passing:
				// - Method wants pointer (mut or &Type): pass &receiver (or receiver if already ptr)
				// - Method wants value: pass receiver (or *receiver if ptr)
				if method_wants_ptr {
					if receiver_is_ptr {
						// Already a pointer, pass as-is
						g.gen_expr(receiver_expr)
					} else {
						// Need to pass address
						g.sb.write_string('&')
						g.gen_expr(receiver_expr)
					}
				} else {
					if receiver_is_ptr {
						// Need to dereference
						g.sb.write_string('*')
						g.gen_expr(receiver_expr)
					} else {
						// Pass by value
						g.gen_expr(receiver_expr)
					}
				}

				if node.args.len > 0 {
					g.sb.write_string(', ')
				}
			} else {
				g.sb.write_string('${name}(')
			}
			for i, arg in node.args {
				if i > 0 {
					g.sb.write_string(', ')
				}
				// Check if the argument is a mut parameter (ModifierExpr with mut)
				if arg is ast.ModifierExpr {
					if arg.kind == .key_mut {
						g.sb.write_string('&')
						g.gen_expr(arg.expr)
					} else {
						g.gen_expr(arg)
					}
				} else {
					g.gen_expr(arg)
				}
			}
			g.sb.write_string(')')
		}
		ast.CallOrCastExpr {
			// This is a call that looks like a cast, e.g., fib(n-1) or int(color)
			mut name := ''
			if node.lhs is ast.Ident {
				name = node.lhs.name
				// Check if this is a primitive type cast (int, i64, etc.)
				// For enums, int(enum_val) just returns the value since C enums are ints
				if name in ['int', 'i64', 'i32', 'i16', 'i8', 'u64', 'u32', 'u16', 'u8', 'f32',
					'f64', 'byte', 'rune', 'char', 'bool', 'isize', 'usize'] {
					g.sb.write_string('((${name})(')
					g.gen_expr(node.expr)
					g.sb.write_string('))')
					return
				}
				// Pointer type casts: voidptr, charptr, byteptr
				if name in ['voidptr', 'charptr', 'byteptr'] {
					ptr_type := match name {
						'voidptr' { 'void*' }
						'charptr' { 'char*' }
						'byteptr' { 'u8*' }
						else { 'void*' }
					}
					g.sb.write_string('((${ptr_type})(')
					g.gen_expr(node.expr)
					g.sb.write_string('))')
					return
				}
				// Check if this is interface boxing: Drawable(point)
				if name in g.interface_names {
					// Get the concrete type being boxed
					concrete_type := g.infer_type(node.expr)
					// Generate interface struct initialization with vtable
					g.sb.write_string('({ ')
					// Allocate heap memory for the object
					g.sb.write_string('${concrete_type}* _iface_obj = (${concrete_type}*)malloc(sizeof(${concrete_type})); ')
					g.sb.write_string('*_iface_obj = ')
					g.gen_expr(node.expr)
					g.sb.write_string('; ')
					// Create interface struct with function pointers
					g.sb.write_string('(${name}){._object = _iface_obj, ._type_id = 0')
					// Add function pointers for each interface method
					if methods := g.interface_meths[name] {
						for meth in methods {
							g.sb.write_string(', .${meth} = ${name}_${concrete_type}_${meth}_wrapper')
						}
					}
					g.sb.write_string('}; })')
					return
				}
			} else if node.lhs is ast.SelectorExpr {
				// Check for C library call (C.putchar, etc.)
				if node.lhs.lhs is ast.Ident && node.lhs.lhs.name == 'C' {
					name = node.lhs.rhs.name
				} else if node.lhs.lhs is ast.Ident && node.lhs.lhs.name in g.module_names {
					// Module-qualified function call: strconv.f64_to_str_l(x)
					name = '${node.lhs.lhs.name}__${node.lhs.rhs.name}'
					g.sb.write_string('${name}(')
					g.gen_expr(node.expr)
					g.sb.write_string(')')
					return
				} else {
					// Method call: obj.method(arg)
					name = node.lhs.rhs.name
					receiver_expr := node.lhs.lhs
					receiver_type := g.infer_type(receiver_expr)
					receiver_is_ptr := receiver_type.ends_with('*')
					clean_type := if receiver_is_ptr {
						receiver_type[..receiver_type.len - 1]
					} else {
						receiver_type
					}

					mangled := '${clean_type}__${name}'
					// Method expects pointer if receiver is mut OR reference (&Type)
					method_wants_ptr := g.mut_receivers[mangled] or { false } || g.ref_receivers[mangled] or {
						false
					}

					g.sb.write_string('${mangled}(')

					// Handle receiver passing
					if method_wants_ptr {
						if receiver_is_ptr {
							g.gen_expr(receiver_expr)
						} else {
							g.sb.write_string('&')
							g.gen_expr(receiver_expr)
						}
					} else {
						if receiver_is_ptr {
							g.sb.write_string('*')
							g.gen_expr(receiver_expr)
						} else {
							g.gen_expr(receiver_expr)
						}
					}

					g.sb.write_string(', ')
					// Generate the single argument
					if node.expr is ast.ModifierExpr {
						if node.expr.kind == .key_mut {
							g.sb.write_string('&')
							g.gen_expr(node.expr.expr)
						} else {
							g.gen_expr(node.expr)
						}
					} else {
						g.gen_expr(node.expr)
					}
					g.sb.write_string(')')
					return
				}
			}
			g.sb.write_string('${name}(')
			// Check if the argument is a mut parameter (ModifierExpr with mut)
			if node.expr is ast.ModifierExpr {
				if node.expr.kind == .key_mut {
					g.sb.write_string('&')
					g.gen_expr(node.expr.expr)
				} else {
					g.gen_expr(node.expr)
				}
			} else {
				g.gen_expr(node.expr)
			}
			g.sb.write_string(')')
		}
		ast.InitExpr {
			// Check if it's a map type
			if node.typ is ast.Type {
				if node.typ is ast.MapType {
					// Generate empty map initialization
					g.sb.write_string('__new_map_int_int()')
					return
				}
			}
			// Get the type name properly
			typ_name := g.expr_type_to_c(node.typ)
			g.sb.write_string('(${typ_name}){')
			for i, field in node.fields {
				if i > 0 {
					g.sb.write_string(', ')
				}
				g.sb.write_string('.${field.name} = ')
				g.gen_expr(field.value)
			}
			g.sb.write_string('}')
		}
		ast.SelectorExpr {
			// Check if this is a C interop constant access (C.xxx)
			if node.lhs is ast.Ident {
				if node.lhs.name == 'C' {
					// C constant access: emit just the constant name
					g.sb.write_string(node.rhs.name)
					return
				}
				if node.lhs.name in g.enum_names {
					// Enum value access: generate EnumName__field
					g.sb.write_string('${node.lhs.name}__${node.rhs.name}')
					return
				}
				// Check if this is a module-qualified name (e.g., strconv.double_minus_zero)
				if node.lhs.name in g.module_names {
					// Module constant/function access: generate module__name
					g.sb.write_string('${node.lhs.name}__${node.rhs.name}')
					return
				}
			}
			// Check for enum shorthand (.value) - LHS is EmptyExpr
			if node.lhs is ast.EmptyExpr {
				// Use the match expression type context
				if g.cur_match_type in g.enum_names {
					g.sb.write_string('${g.cur_match_type}__${node.rhs.name}')
					return
				}
				// Fallback: just output the field name (might not be valid C)
				g.sb.write_string('${node.rhs.name}')
				return
			}
			// Check if we need to use -> for pointers
			lhs_type := g.infer_type(node.lhs)
			g.gen_expr(node.lhs)
			if lhs_type.ends_with('*') {
				g.sb.write_string('->')
			} else {
				g.sb.write_string('.')
			}
			g.sb.write_string(node.rhs.name)
		}
		ast.IndexExpr {
			// Check if this is a slice operation (arr[start..end])
			if node.expr is ast.RangeExpr {
				g.gen_slice_expr(node.lhs, node.expr)
				return
			}
			// Check if this is an array access (Array struct type)
			lhs_type := g.infer_type(node.lhs)
			if lhs_type.starts_with('Map_') {
				// Map access: __map_int_int_get(&m, key)
				g.sb.write_string('__map_int_int_get(&')
				g.gen_expr(node.lhs)
				g.sb.write_string(', ')
				g.gen_expr(node.expr)
				g.sb.write_string(')')
			} else if lhs_type.starts_with('Array_') {
				// For Array types, access via ((elem_type*)arr.data)[index]
				elem_type := lhs_type['Array_'.len..]
				g.sb.write_string('((${elem_type}*)')
				g.gen_expr(node.lhs)
				g.sb.write_string('.data)[')
				g.gen_expr(node.expr)
				g.sb.write_string(']')
			} else {
				// Regular C array access
				g.gen_expr(node.lhs)
				g.sb.write_string('[')
				g.gen_expr(node.expr)
				g.sb.write_string(']')
			}
		}
		ast.PostfixExpr {
			g.gen_expr(node.expr)
			if node.op == .inc {
				g.sb.write_string('++')
			} else {
				g.sb.write_string('--')
			}
		}
		ast.ModifierExpr {
			// Handle mut, shared, etc.
			g.gen_expr(node.expr)
		}
		ast.ArrayInitExpr {
			// Generate array using new_array_from_c_array builtin
			len := node.exprs.len
			elem_type := if len > 0 { g.infer_type(node.exprs[0]) } else { 'int' }
			// new_array_from_c_array(len, len, sizeof(elem), (elem_type[len]){values})
			g.sb.write_string('new_array_from_c_array(${len}, ${len}, sizeof(${elem_type}), (${elem_type}[${len}]){')
			for i, expr in node.exprs {
				if i > 0 {
					g.sb.write_string(', ')
				}
				g.gen_expr(expr)
			}
			g.sb.write_string('})')
		}
		ast.MapInitExpr {
			// Generate empty map initialization
			g.sb.write_string('__new_map_int_int()')
		}
		ast.IfGuardExpr {
			// If-guard expression: `if x := opt() { ... }`
			// For cleanc, we generate the RHS expression and use it as condition
			// The variable binding is handled in the if-statement context
			if node.stmt.rhs.len > 0 {
				g.gen_expr(node.stmt.rhs[0])
			} else {
				g.sb.write_string('0')
			}
		}
		ast.Keyword {
			// Handle keywords like 'none'
			if node.tok == .key_none {
				g.sb.write_string('0') // none is represented as 0/NULL
			} else if node.tok == .key_true {
				g.sb.write_string('true')
			} else if node.tok == .key_false {
				g.sb.write_string('false')
			} else {
				g.sb.write_string('/* keyword: ${node.tok} */')
			}
		}
		ast.Type {
			// Handle type expressions (e.g., in return statements or comparisons)
			// In V, comparing to a type like `ptr != voidptr` means null check
			if node is ast.NoneType {
				g.sb.write_string('0') // none
			} else if node is ast.NilType {
				g.sb.write_string('NULL') // nil
			} else {
				g.sb.write_string('NULL') // Default to NULL for type comparisons
			}
		}
		ast.RangeExpr {
			// RangeExpr: start..end or start...end
			// Generate a compound literal for the range struct
			g.sb.write_string('(Range_){')
			g.gen_expr(node.start)
			g.sb.write_string(', ')
			g.gen_expr(node.end)
			g.sb.write_string('}')
		}
		ast.ComptimeExpr {
			// Handle comptime expressions like $if macos { ... } $else { ... }
			g.gen_comptime_expr(node)
		}
		ast.UnsafeExpr {
			// Unsafe block - generate as GCC compound expression
			if node.stmts.len == 0 {
				g.sb.write_string('0')
			} else {
				g.sb.write_string('({ ')
				// Generate all but last statement
				for i, stmt in node.stmts {
					if i < node.stmts.len - 1 {
						g.gen_stmt(stmt)
					}
				}
				// Generate last statement - if it's an ExprStmt, we need its value
				last := node.stmts[node.stmts.len - 1]
				if last is ast.ExprStmt {
					g.gen_expr(last.expr)
					g.sb.write_string('; ')
				} else {
					g.gen_stmt(last)
					g.sb.write_string('0; ')
				}
				g.sb.write_string('})')
			}
		}
		ast.EmptyExpr {
			// Empty expression - output nothing or 0
			g.sb.write_string('0')
		}
		ast.OrExpr {
			// Or expression: expr or { fallback }
			// For now, just generate the main expression
			// TODO: Add proper optional/error handling
			g.gen_expr(node.expr)
		}
		else {
			g.sb.write_string('/* expr: ${node.type_name()} */')
		}
	}
}

// gen_comptime_expr handles compile-time conditionals like $if macos { ... } $else { ... }
fn (mut g Gen) gen_comptime_expr(node ast.ComptimeExpr) {
	// The inner expression should be an IfExpr
	if node.expr is ast.IfExpr {
		g.gen_comptime_if(node.expr)
	} else {
		// For other comptime expressions, just emit them
		g.gen_expr(node.expr)
	}
}

// gen_comptime_if handles $if/$else compile-time conditionals
fn (mut g Gen) gen_comptime_if(node ast.IfExpr) {
	// Evaluate the comptime condition
	cond_result := g.eval_comptime_cond(node.cond)

	if cond_result {
		// Condition is true - emit then branch
		g.gen_stmts(node.stmts)
	} else {
		// Condition is false - emit else branch if present
		if node.else_expr !is ast.EmptyExpr {
			if node.else_expr is ast.IfExpr {
				// Could be $else if or plain $else
				if node.else_expr.cond is ast.EmptyExpr {
					// Plain $else block
					g.gen_stmts(node.else_expr.stmts)
				} else {
					// $else $if - recursive comptime evaluation
					g.gen_comptime_if(node.else_expr)
				}
			}
		}
	}
}

// eval_comptime_cond evaluates a compile-time condition expression
fn (g Gen) eval_comptime_cond(cond ast.Expr) bool {
	match cond {
		ast.Ident {
			// Platform and feature flags
			return g.eval_comptime_flag(cond.name)
		}
		ast.PrefixExpr {
			// Handle negation: !macos
			if cond.op == .not {
				return !g.eval_comptime_cond(cond.expr)
			}
		}
		ast.InfixExpr {
			// Handle && and ||
			if cond.op == .and {
				return g.eval_comptime_cond(cond.lhs) && g.eval_comptime_cond(cond.rhs)
			}
			if cond.op == .logical_or {
				return g.eval_comptime_cond(cond.lhs) || g.eval_comptime_cond(cond.rhs)
			}
		}
		ast.PostfixExpr {
			// Handle optional feature check: feature?
			if cond.op == .question {
				if cond.expr is ast.Ident {
					return g.eval_comptime_flag(cond.expr.name)
				}
			}
		}
		ast.ParenExpr {
			return g.eval_comptime_cond(cond.expr)
		}
		else {}
	}
	return false
}

// eval_comptime_flag evaluates a single comptime flag/identifier
fn (g Gen) eval_comptime_flag(name string) bool {
	// OS checks - use comptime conditionals with direct returns
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
		'posix', 'unix' {
			$if macos {
				return true
			} $else $if linux {
				return true
			} $else $if freebsd {
				return true
			}
			return false
		}
		// Architecture checks
		'amd64', 'x86_64' {
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
		'x86' {
			// x86 (32-bit) is rarely used in modern systems
			return false
		}
		// Common feature flags (typically false in simple compilers)
		'freestanding', 'ios', 'android', 'termux', 'debug', 'test' {
			return false
		}
		else {
			// Unknown flag - default to false
			return false
		}
	}
}

fn (mut g Gen) write_indent() {
	for _ in 0 .. g.indent {
		g.sb.write_string('\t')
	}
}

// Check if an IfExpr can be converted to a C ternary operator
// This is true when the branches contain simple value expressions (single ExprStmt)
fn (g Gen) can_be_ternary(node ast.IfExpr) bool {
	// If-guard expressions cannot be ternary (they need variable declarations)
	if node.cond is ast.IfGuardExpr {
		return false
	}
	// Must have both branches
	if node.else_expr is ast.EmptyExpr {
		return false
	}
	// Check if true branch has exactly one ExprStmt with a simple expression
	if node.stmts.len != 1 {
		return false
	}
	stmt := node.stmts[0]
	if stmt !is ast.ExprStmt {
		return false
	}
	// Exclude complex expressions that can't be used in ternary (MatchExpr, IfExpr as statement)
	expr_stmt := stmt as ast.ExprStmt
	if expr_stmt.expr is ast.MatchExpr {
		return false
	}
	if expr_stmt.expr is ast.IfExpr {
		// If nested, check if it's also a value expression
		nested_if := expr_stmt.expr as ast.IfExpr
		if !g.can_be_ternary(nested_if) {
			return false
		}
	}
	// Check else branch
	if node.else_expr is ast.IfExpr {
		// Could be else-if chain or pure else
		else_if := node.else_expr
		if else_if.cond is ast.EmptyExpr {
			// Pure else: check its statements
			if else_if.stmts.len != 1 {
				return false
			}
			else_stmt := else_if.stmts[0]
			if else_stmt !is ast.ExprStmt {
				return false
			}
			else_expr_stmt := else_stmt as ast.ExprStmt
			if else_expr_stmt.expr is ast.MatchExpr {
				return false
			}
		} else {
			// Nested if-expression (else if) - can be ternary if nested can
			return g.can_be_ternary(else_if)
		}
	}
	return true
}

// Generate the value from the if-branch statements
fn (mut g Gen) gen_if_value(stmts []ast.Stmt) {
	if stmts.len == 1 {
		stmt := stmts[0]
		if stmt is ast.ExprStmt {
			g.gen_expr(stmt.expr)
			return
		}
	}
	g.sb.write_string('0')
}

// Generate the value from the else-branch expression
fn (mut g Gen) gen_else_value(else_expr ast.Expr) {
	if else_expr is ast.IfExpr {
		if else_expr.cond is ast.EmptyExpr {
			// Pure else: extract value from its statements
			g.gen_if_value(else_expr.stmts)
		} else {
			// Nested if-expression (else if) - recurse with ternary
			g.sb.write_string('(')
			g.gen_expr(else_expr.cond)
			g.sb.write_string(') ? (')
			g.gen_if_value(else_expr.stmts)
			g.sb.write_string(') : (')
			g.gen_else_value(else_expr.else_expr)
			g.sb.write_string(')')
		}
	} else if else_expr is ast.EmptyExpr {
		g.sb.write_string('0')
	} else {
		g.gen_expr(else_expr)
	}
}

// Generate if-guard statement: `if x := opt() { ... } else { ... }`
fn (mut g Gen) gen_if_guard(node ast.IfExpr, guard ast.IfGuardExpr) {
	// For if-guard, we:
	// 1. Declare the guard variable(s)
	// 2. Assign the RHS to the variable(s)
	// 3. Use the value as condition (non-zero = success)

	// Get the variable name(s) from LHS
	mut var_names := []string{}
	for lhs_expr in guard.stmt.lhs {
		if lhs_expr is ast.Ident {
			var_names << lhs_expr.name
		} else if lhs_expr is ast.ModifierExpr {
			// Handle 'mut x'
			if lhs_expr.expr is ast.Ident {
				var_names << lhs_expr.expr.name
			}
		}
	}

	// Infer type from RHS
	mut rhs_type := 'int'
	if guard.stmt.rhs.len > 0 {
		rhs_type = g.infer_type(guard.stmt.rhs[0])
	}

	// Generate: { type var = rhs; if (var) { ... } else { ... } }
	g.sb.writeln('{')
	g.indent++

	// Declare and assign guard variable(s)
	for i, var_name in var_names {
		g.write_indent()
		g.var_types[var_name] = rhs_type
		g.sb.write_string('${rhs_type} ${var_name} = ')
		if i < guard.stmt.rhs.len {
			g.gen_expr(guard.stmt.rhs[i])
		} else if guard.stmt.rhs.len > 0 {
			g.gen_expr(guard.stmt.rhs[0])
		} else {
			g.sb.write_string('0')
		}
		g.sb.writeln(';')
	}

	// Generate the if statement using the first variable as condition
	g.write_indent()
	if var_names.len > 0 {
		g.sb.write_string('if (${var_names[0]})')
	} else {
		g.sb.write_string('if (0)')
	}
	g.sb.writeln(' {')
	g.indent++
	g.gen_stmts(node.stmts)
	g.indent--
	g.write_indent()
	g.sb.write_string('}')

	// Handle else branch
	if node.else_expr !is ast.EmptyExpr {
		if node.else_expr is ast.IfExpr {
			if node.else_expr.cond is ast.EmptyExpr {
				// Pure else block
				g.sb.writeln(' else {')
				g.indent++
				g.gen_stmts(node.else_expr.stmts)
				g.indent--
				g.write_indent()
				g.sb.writeln('}')
			} else {
				// Else-if chain
				g.sb.write_string(' else ')
				g.gen_expr(node.else_expr)
			}
		} else {
			g.sb.write_string(' else ')
			g.gen_expr(node.else_expr)
		}
	} else {
		g.sb.writeln('')
	}

	g.indent--
	g.write_indent()
	g.sb.writeln('}')
}

// Generate for-in loop: dispatches to range or array handling
fn (mut g Gen) gen_for_in(node ast.ForStmt, for_in ast.ForInStmt) {
	if for_in.expr is ast.RangeExpr {
		g.gen_for_in_range(node, for_in)
	} else {
		g.gen_for_in_array(node, for_in)
	}
}

// Generate for-in loop with range: `for i in start..end { ... }`
fn (mut g Gen) gen_for_in_range(node ast.ForStmt, for_in ast.ForInStmt) {
	// Get loop variable name
	mut var_name := ''
	if for_in.value is ast.Ident {
		var_name = for_in.value.name
	} else if for_in.value is ast.ModifierExpr {
		if for_in.value.expr is ast.Ident {
			var_name = for_in.value.expr.name
		}
	}

	range_expr := for_in.expr as ast.RangeExpr

	// Register loop variable type
	g.var_types[var_name] = 'int'

	// Generate: for (int i = start; i < end; i++) { ... }
	g.write_indent()
	g.sb.write_string('for (int ${var_name} = ')
	g.gen_expr(range_expr.start)
	g.sb.write_string('; ${var_name} < ')
	g.gen_expr(range_expr.end)
	g.sb.write_string('; ${var_name}++')
	g.sb.writeln(') {')
	g.indent++
	g.gen_stmts(node.stmts)
	g.indent--
	g.write_indent()
	g.sb.writeln('}')
}

// Generate for-in loop over array: `for elem in array { ... }` or `for i, elem in array { ... }`
fn (mut g Gen) gen_for_in_array(node ast.ForStmt, for_in ast.ForInStmt) {
	// Get key variable name (index)
	mut key_name := ''
	if for_in.key !is ast.EmptyExpr {
		if for_in.key is ast.Ident {
			key_name = for_in.key.name
		} else if for_in.key is ast.ModifierExpr {
			if for_in.key.expr is ast.Ident {
				key_name = for_in.key.expr.name
			}
		}
	}

	// Get value variable name
	mut value_name := ''
	if for_in.value is ast.Ident {
		value_name = for_in.value.name
	} else if for_in.value is ast.ModifierExpr {
		if for_in.value.expr is ast.Ident {
			value_name = for_in.value.expr.name
		}
	}

	// Infer array type
	arr_type := g.infer_type(for_in.expr)

	// Determine element type
	mut elem_type := 'int'
	if arr_type.starts_with('Array_') {
		elem_type = arr_type['Array_'.len..]
	}

	// Register variable types
	g.var_types[value_name] = elem_type
	if key_name != '' {
		g.var_types[key_name] = 'int'
	}

	// Use a hidden index if no key specified
	idx_var := if key_name != '' { key_name } else { '_idx_${value_name}' }

	g.write_indent()
	g.sb.write_string('for (int ${idx_var} = 0; ${idx_var} < ')
	g.gen_expr(for_in.expr)
	g.sb.writeln('.len; ${idx_var}++) {')
	g.indent++

	// Declare and assign value variable
	g.write_indent()
	g.sb.write_string('${elem_type} ${value_name} = ((${elem_type}*)')
	g.gen_expr(for_in.expr)
	g.sb.writeln('.data)[${idx_var}];')

	g.gen_stmts(node.stmts)
	g.indent--
	g.write_indent()
	g.sb.writeln('}')
}

// Generate array slice expression: arr[start..end]
fn (mut g Gen) gen_slice_expr(base ast.Expr, range_expr ast.RangeExpr) {
	// Get the base array type
	lhs_type := g.infer_type(base)

	if lhs_type.starts_with('Array_') {
		// For Array struct types
		elem_type := lhs_type['Array_'.len..]
		// Generate: __slice_array(arr, start, end, sizeof(elem_type))
		// For simplicity, generate inline slice creation
		g.sb.write_string('({ ')
		g.sb.write_string('int _start = ')
		g.gen_expr(range_expr.start)
		g.sb.write_string('; int _end = ')
		g.gen_expr(range_expr.end)
		g.sb.write_string('; int _len = _end - _start; ')
		g.sb.write_string('Array _slice = new_array_from_c_array(_len, _len, sizeof(${elem_type}), ')
		g.sb.write_string('((${elem_type}*)')
		g.gen_expr(base)
		g.sb.write_string('.data) + _start); _slice; })')
	} else {
		// For C-style arrays, return pointer to start
		g.sb.write_string('(&(')
		g.gen_expr(base)
		g.sb.write_string(')[')
		g.gen_expr(range_expr.start)
		g.sb.write_string('])')
	}
}

// Convert V string interpolation format to C printf format specifier
fn (g Gen) get_printf_format(inter ast.StringInter) string {
	base_fmt := match inter.format {
		.unformatted { '%lld' } // Default: assume integer
		.decimal { '%lld' }
		.hex { '%llx' }
		.octal { '%llo' }
		.binary { '%lld' } // C doesn't have binary, use decimal
		.float { '%f' }
		.exponent { '%e' }
		.exponent_short { '%g' }
		.character { '%c' }
		.string { '%s' }
		.pointer_address { '%p' }
	}

	// Handle width and precision if specified
	if inter.width > 0 && inter.precision > 0 {
		return '%${inter.width}.${inter.precision}' + base_fmt[1..]
	} else if inter.width > 0 {
		return '%${inter.width}' + base_fmt[1..]
	} else if inter.precision > 0 {
		return '%.${inter.precision}' + base_fmt[1..]
	}
	return base_fmt
}

// emit_deferred_stmts emits all deferred statements in reverse order
fn (mut g Gen) emit_deferred_stmts() {
	// Execute deferred statements in LIFO order (last defer first)
	for i := g.defer_stmts.len - 1; i >= 0; i-- {
		g.gen_stmts(g.defer_stmts[i])
	}
}

// gen_multi_return_assign handles multi-return assignments like: a, b := func()
fn (mut g Gen) gen_multi_return_assign(node ast.AssignStmt) {
	rhs := node.rhs[0]

	// Generate tuple type name based on number of return values
	tuple_type := 'Tuple_${node.lhs.len}_int'

	// Get a unique temp variable name
	tmp_name := '__tmp_${g.tmp_counter}'
	g.tmp_counter++

	// Generate: TupleN_int __tmp = func();
	g.write_indent()
	g.sb.write_string('${tuple_type} ${tmp_name} = ')
	g.gen_expr(rhs)
	g.sb.writeln(';')

	// Extract each field: int a = __tmp.f0; int b = __tmp.f1;
	for i, lhs_expr in node.lhs {
		mut name := ''
		mut is_blank := false
		if lhs_expr is ast.Ident {
			name = lhs_expr.name
			is_blank = name == '_'
		} else if lhs_expr is ast.ModifierExpr {
			if lhs_expr.expr is ast.Ident {
				name = lhs_expr.expr.name
				is_blank = name == '_'
			}
		}

		if is_blank {
			continue // Skip blank identifiers
		}

		g.var_types[name] = 'int' // Simplified - assume int return type
		g.write_indent()
		g.sb.writeln('int ${name} = ${tmp_name}.f${i};')
	}
}
