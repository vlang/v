// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.token
import v2.types
import strings

pub struct Gen {
	files []ast.File
	// Type checker environment with populated scopes
	env &types.Environment = unsafe { nil }
mut:
	sb                   strings.Builder
	indent               int
	tmp_counter          int // Counter for unique temp variable names
	fn_types             map[string]string
	fn_ret_counts        map[string]int // Track number of return values for multi-return functions
	var_types            map[string]string
	mut_params           map[string]bool              // Track mut parameters (need dereference on assignment)
	mut_receivers        map[string]bool              // Track which methods have mutable receivers
	ref_receivers        map[string]bool              // Track which methods have reference receivers (&Type)
	defer_stmts          [][]ast.Stmt                 // Deferred statements for current function
	enum_names           map[string]bool              // Track enum type names
	flag_enum_names      map[string]bool              // Track flag enum type names
	interface_names      map[string]bool              // Track interface type names
	interface_meths      map[string][]string          // Interface name -> method names
	type_methods         map[string][]string          // Type name -> method names (for vtable generation)
	struct_fields        map[string]map[string]string // Struct name -> (field name -> field type)
	known_types          map[string]bool              // All known mangled type names (structs, type aliases, interfaces)
	module_types         map[string]map[string]bool   // Module name -> set of type names defined in that module
	cur_match_type       string                       // Current match expression type for enum shorthand
	cur_module           string                       // Current module name for namespacing
	file_modules         map[string]string            // File path -> module name mapping
	module_names         map[string]bool              // All module names (for detecting module.ident access)
	params_structs       map[string]bool              // Struct types with @[params] attribute
	fn_params            map[string][]string          // Function name -> list of parameter types
	seen_consts          map[string]bool              // Track generated constants for reference in other consts
	const_types          map[string]string            // Track constant types for type inference
	const_mangled        map[string]string            // Map unmangled const name -> mangled name for cross-module refs
	fn_mangled           map[string]string            // Map unmangled fn name -> mangled name for same-module calls
	tuple_types          map[string][]string          // Tuple type name -> list of element types
	cur_fn_ret_type      string                       // Current function's return type (for proper return statement generation)
	global_var_types     map[string]string            // Global variable name -> type (persists across functions)
	fn_ptr_typedefs      map[string]ast.FnType        // Synthetic fn pointer typedef name -> FnType (for inline fn types)
	type_aliases         map[string]string            // Type alias name -> base type name (for method resolution)
	type_ids             map[string]int               // Concrete type name -> unique type ID for interface type matching
	next_type_id         int    // Counter for assigning unique type IDs
	cur_iface_match_var  string // Interface variable being matched (e.g., "err" when matching IError)
	cur_iface_match_type string // Concrete type in current match branch (e.g., "MessageError")
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
		flag_enum_names: map[string]bool{}
		interface_names: map[string]bool{}
		interface_meths: map[string][]string{}
		type_methods:    map[string][]string{}
		struct_fields:   map[string]map[string]string{}
		file_modules:    map[string]string{}
		module_names:    map[string]bool{}
		module_types:    map[string]map[string]bool{}
		params_structs:  map[string]bool{}
		fn_params:       map[string][]string{}
		fn_ptr_typedefs: map[string]ast.FnType{}
		type_aliases:    map[string]string{}
		type_ids:        map[string]int{}
		next_type_id:    1 // Start from 1, 0 means "no type" or error
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
	// Pass -1: Collect type names per module (structs, type aliases, interfaces)
	for file in files {
		file_module := g.file_modules[file.name] or { '' }
		g.cur_module = file_module // Set cur_module for expr_type_to_c
		if file_module !in g.module_types {
			g.module_types[file_module] = map[string]bool{}
		}
		for stmt in file.stmts {
			match stmt {
				ast.StructDecl {
					g.module_types[file_module][stmt.name] = true
				}
				ast.InterfaceDecl {
					g.module_types[file_module][stmt.name] = true
				}
				ast.TypeDecl {
					g.module_types[file_module][stmt.name] = true
					// Track type aliases for method resolution
					if stmt.variants.len == 0 && stmt.base_type !is ast.EmptyExpr {
						mangled_name := if file_module != '' && file_module != 'main'
							&& file_module != 'builtin' {
							'${file_module}__${stmt.name}'
						} else {
							stmt.name
						}
						base_type := g.expr_type_to_c(stmt.base_type)
						g.type_aliases[mangled_name] = base_type
					}
				}
				else {}
			}
		}
	}
	// Pass 0: Register function return types, mutable receivers, enum names, and interfaces
	for file in files {
		file_module := g.file_modules[file.name] or { '' }
		g.cur_module = file_module // Set cur_module for expr_type_to_c
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				// Check for @[params] attribute
				if stmt.attributes.has('params') {
					mangled_name := if file_module != '' && file_module != 'main'
						&& file_module != 'builtin' {
						'${file_module}__${stmt.name}'
					} else {
						stmt.name
					}
					g.params_structs[mangled_name] = true
					g.params_structs[stmt.name] = true // Also store unmangled name
				}
			}
			if stmt is ast.EnumDecl {
				g.enum_names[stmt.name] = true
				if stmt.attributes.has('flag') {
					g.flag_enum_names[stmt.name] = true
				}
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
							ret = g.get_tuple_type(ret_expr.types)
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

				// Collect parameter types for tracking (with module prefixes)
				// Also collect inline function pointer types for typedef generation
				mut param_types := []string{}
				for param in stmt.typ.params {
					// Use expr_type_to_c which handles all type variants including FnType
					type_name := g.expr_type_to_c(param.typ)
					// Add module prefix if this is a type defined in the current module
					mangled_type := if file_module != '' && file_module != 'main'
						&& file_module != 'builtin' && !type_name.contains('__') && type_name != ''
						&& type_name in g.module_types[file_module] {
						'${file_module}__${type_name}'
					} else {
						type_name
					}
					param_types << mangled_type
				}

				if stmt.is_method {
					// For methods, use mangled name with module prefix
					receiver_type := g.get_type_name_for_mangling(stmt.receiver.typ)
					// Add module prefix to receiver type if not already present
					full_receiver_type := if file_module != '' && file_module != 'main'
						&& file_module != 'builtin' && !receiver_type.contains('__') {
						'${file_module}__${receiver_type}'
					} else {
						receiver_type
					}
					mangled := '${full_receiver_type}__${stmt.name}'
					g.fn_types[mangled] = ret
					g.fn_ret_counts[mangled] = ret_count
					g.fn_params[mangled] = param_types
					// Track if receiver is mutable or reference (&Type)
					g.mut_receivers[mangled] = stmt.receiver.is_mut
					// Check if receiver is a reference type (&Type)
					is_ref := stmt.receiver.typ is ast.PrefixExpr && stmt.receiver.typ.op == .amp
					g.ref_receivers[mangled] = is_ref
					// Track methods per type for vtable generation
					if full_receiver_type !in g.type_methods {
						g.type_methods[full_receiver_type] = []string{}
					}
					g.type_methods[full_receiver_type] << stmt.name
				} else {
					// For regular functions, use module-qualified name
					fn_name := if file_module != '' && file_module != 'main'
						&& file_module != 'builtin' {
						'${file_module}__${stmt.name}'
					} else {
						stmt.name
					}
					g.fn_types[fn_name] = ret
					g.fn_ret_counts[fn_name] = ret_count
					g.fn_params[fn_name] = param_types
					// Also track unmangled name for same-module lookups and code generation
					if fn_name != stmt.name {
						g.fn_types[stmt.name] = ret
						g.fn_mangled[stmt.name] = fn_name
					}
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
	g.sb.writeln('#ifdef __APPLE__')
	g.sb.writeln('#include <execinfo.h>')
	g.sb.writeln('#endif')
	g.sb.writeln('#ifndef _WIN32')
	g.sb.writeln('#include <unistd.h>')
	g.sb.writeln('#endif')
	g.sb.writeln('')
	// _MOV macro for compound literal support
	g.sb.writeln('#ifdef __cplusplus')
	g.sb.writeln('#include <utility>')
	g.sb.writeln('#define _MOV std::move')
	g.sb.writeln('#else')
	g.sb.writeln('#define _MOV')
	g.sb.writeln('#endif')
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
	// Global variables for command-line arguments
	g.sb.writeln('static int g_main_argc = 0;')
	g.sb.writeln('static char** g_main_argv = 0;')
	g.sb.writeln('')
	// Alignment enum values (used in string formatting)
	g.sb.writeln('typedef enum { left = 0, right = 1, center = 2 } Align;')
	g.sb.writeln('')
	// Wyhash stubs (C interop functions from hash module)
	g.sb.writeln('static const u64 _wyp[4] = {0xa0761d6478bd642full, 0xe7037ed1a0b428dbull, 0x8ebc6af09c88c6e3ull, 0x589965cc75374cc3ull};')
	g.sb.writeln('static inline u64 wyhash(void* key, u64 len, u64 seed, const u64* secret) { (void)key; (void)len; (void)seed; (void)secret; return 0; }')
	g.sb.writeln('static inline u64 wyhash64(u64 a, u64 b) { (void)a; (void)b; return 0; }')
	g.sb.writeln('')
	// String free stub
	g.sb.writeln('static inline void string__free(void* s) { (void)s; }')
	g.sb.writeln('')
	// Map function stubs as macros (to handle varying call signatures)
	g.sb.writeln('#define map__hash_fn(...) ((u64)0)')
	g.sb.writeln('#define map__key_eq_fn(...) (false)')
	g.sb.writeln('#define map__clone_fn(...) ((void)0)')
	g.sb.writeln('#define map__free_fn(...) ((void)0)')
	g.sb.writeln('')
	// Clone stubs for primitive types
	g.sb.writeln('static inline int int__clone(int* x) { return *x; }')
	g.sb.writeln('static inline i64 i64__clone(i64* x) { return *x; }')
	g.sb.writeln('static inline u64 u64__clone(u64* x) { return *x; }')
	g.sb.writeln('')
	// Variadic argument struct for V-style variadics
	g.sb.writeln('// Variadic argument types')
	g.sb.writeln('typedef struct { int len; voidptr* data; } VArg_voidptr;')
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

	// Array type aliases (arrays are all the same struct, but V names them by element type)
	g.sb.writeln('// Array type aliases')
	g.sb.writeln('typedef array Array_int;')
	g.sb.writeln('typedef array Array_u8;')
	g.sb.writeln('typedef array Array_i8;')
	g.sb.writeln('typedef array Array_u16;')
	g.sb.writeln('typedef array Array_i16;')
	g.sb.writeln('typedef array Array_u32;')
	g.sb.writeln('typedef array Array_i32;')
	g.sb.writeln('typedef array Array_u64;')
	g.sb.writeln('typedef array Array_i64;')
	g.sb.writeln('typedef array Array_f32;')
	g.sb.writeln('typedef array Array_f64;')
	g.sb.writeln('typedef array Array_string;')
	g.sb.writeln('typedef array Array_rune;')
	g.sb.writeln('typedef array Array_bool;')
	g.sb.writeln('typedef array Array_voidptr;')
	g.sb.writeln('typedef array Array_byte;')
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

	// 2.5b. Function Pointer Typedefs (for inline fn types in function parameters)
	for typedef_name, fn_type in g.fn_ptr_typedefs {
		// Generate: typedef ret_type (*name)(param_types);
		mut ret_type := 'void'
		if fn_type.return_type !is ast.EmptyExpr {
			ret_type = g.expr_type_to_c(fn_type.return_type)
		}
		g.sb.write_string('typedef ${ret_type} (*${typedef_name})(')
		for i, param in fn_type.params {
			if i > 0 {
				g.sb.write_string(', ')
			}
			param_type := g.expr_type_to_c(param.typ)
			if param.is_mut {
				g.sb.write_string('${param_type}*')
			} else {
				g.sb.write_string(param_type)
			}
		}
		g.sb.writeln(');')
	}
	if g.fn_ptr_typedefs.len > 0 {
		g.sb.writeln('')
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
						fields[field.name] = g.get_field_type_for_inference(field.typ)
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

	// 3.4. Tuple types for multi-return functions (after struct definitions)
	for tuple_name, elem_types in g.tuple_types {
		g.sb.write_string('typedef struct { ')
		for i, elem_type in elem_types {
			g.sb.write_string('${elem_type} f${i}; ')
		}
		g.sb.writeln('} ${tuple_name};')
	}
	// Keep some common fallback tuple types in case they weren't registered
	if 'Tuple_2_int' !in g.tuple_types {
		g.sb.writeln('typedef struct { int f0; int f1; } Tuple_2_int;')
	}
	g.sb.writeln('')

	// 3.5. Constants
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		for stmt in file.stmts {
			if stmt is ast.ConstDecl {
				g.gen_const_decl(stmt, file_module)
				g.sb.writeln('')
			}
		}
	}

	// 3.6. Builtin helpers (array__free, etc.)
	// array__free - inline implementation since method may not be generated due to $if blocks
	g.sb.writeln('static inline void array__free(array* a) { if (a->data) free(a->data); a->data = 0; a->len = 0; a->cap = 0; }')
	// strings__Builder__free - Builder is a type alias for []u8, delegate to array__free
	g.sb.writeln('static inline void strings__Builder__free(strings__Builder* b) { array__free((array*)b); }')
	// array__contains_u8 - check if array contains element (for 'in' operator)
	g.sb.writeln('static inline bool array__contains_u8(array* a, u8 v) { for (int i = 0; i < a->len; i++) { if (((u8*)a->data)[i] == v) return true; } return false; }')
	// strings__Builder__trim - trim builder to specified length
	g.sb.writeln('static inline void strings__Builder__trim(strings__Builder b, int n) { if (n < b.len) b.len = n; }')
	// ArrayFlags__has - check if flag is set
	g.sb.writeln('static inline bool ArrayFlags__has(ArrayFlags* f, ArrayFlags v) { return (((int)(*f)) & ((int)(v))) != 0; }')
	// array__eq - compare two arrays for equality
	g.sb.writeln('static inline bool array__eq(array a, array b) { if (a.len != b.len) return false; return memcmp(a.data, b.data, a.len * a.element_size) == 0; }')
	// Map types and functions stub
	g.sb.writeln('typedef map Map_int_int;')
	g.sb.writeln('static inline Map_int_int __new_Map_int_int() { return (Map_int_int){0}; }')
	g.sb.writeln('static inline int __Map_int_int_get(Map_int_int* m, int key) { return 0; }')
	g.sb.writeln('static inline void __Map_int_int_set(Map_int_int* m, int key, int val) { }')
	g.sb.writeln('typedef map Map_string_int;')
	g.sb.writeln('static inline Map_string_int __new_Map_string_int() { return (Map_string_int){0}; }')
	g.sb.writeln('static inline int __Map_string_int_get(Map_string_int* m, string key) { return 0; }')
	g.sb.writeln('static inline void __Map_string_int_set(Map_string_int* m, string key, int val) { }')
	// bits module tables - as V array structs for .data access
	g.sb.writeln('static u8 _bits__ntz_8_tab_data[] = {0,1,2,0,3,0,0,0,4,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8};')
	g.sb.writeln('static array bits__ntz_8_tab = {.data = _bits__ntz_8_tab_data, .len = 129, .cap = 129, .element_size = sizeof(u8)};')
	g.sb.writeln('static u8 _bits__de_bruijn32tab_data[] = {0,1,28,2,29,14,24,3,30,22,20,15,25,17,4,8,31,27,13,23,21,19,16,7,26,12,18,6,11,5,10,9};')
	g.sb.writeln('static array bits__de_bruijn32tab = {.data = _bits__de_bruijn32tab_data, .len = 32, .cap = 32, .element_size = sizeof(u8)};')
	g.sb.writeln('static u8 _bits__de_bruijn64tab_data[] = {0,1,56,2,57,49,28,3,61,58,42,50,38,29,17,4,62,47,59,36,45,43,51,22,53,39,33,30,24,18,12,5,63,55,48,27,60,41,37,16,46,35,44,21,52,32,23,11,54,26,40,15,34,20,31,10,25,14,19,9,13,8,7,6};')
	g.sb.writeln('static array bits__de_bruijn64tab = {.data = _bits__de_bruijn64tab_data, .len = 64, .cap = 64, .element_size = sizeof(u8)};')
	g.sb.writeln('static u8 _bits__pop_8_tab_data[] = {0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8};')
	g.sb.writeln('static array bits__pop_8_tab = {.data = _bits__pop_8_tab_data, .len = 256, .cap = 256, .element_size = sizeof(u8)};')
	g.sb.writeln('static u8 _bits__rev_8_tab_data[] = {0,128,64,192,32,160,96,224,16,144,80,208,48,176,112,240,8,136,72,200,40,168,104,232,24,152,88,216,56,184,120,248,4,132,68,196,36,164,100,228,20,148,84,212,52,180,116,244,12,140,76,204,44,172,108,236,28,156,92,220,60,188,124,252,2,130,66,194,34,162,98,226,18,146,82,210,50,178,114,242,10,138,74,202,42,170,106,234,26,154,90,218,58,186,122,250,6,134,70,198,38,166,102,230,22,150,86,214,54,182,118,246,14,142,78,206,46,174,110,238,30,158,94,222,62,190,126,254,1,129,65,193,33,161,97,225,17,145,81,209,49,177,113,241,9,137,73,201,41,169,105,233,25,153,89,217,57,185,121,249,5,133,69,197,37,165,101,229,21,149,85,213,53,181,117,245,13,141,77,205,45,173,109,237,29,157,93,221,61,189,125,253,3,131,67,195,35,163,99,227,19,147,83,211,51,179,115,243,11,139,75,203,43,171,107,235,27,155,91,219,59,187,123,251,7,135,71,199,39,167,103,231,23,151,87,215,55,183,119,247,15,143,79,207,47,175,111,239,31,159,95,223,63,191,127,255};')
	g.sb.writeln('static array bits__rev_8_tab = {.data = _bits__rev_8_tab_data, .len = 256, .cap = 256, .element_size = sizeof(u8)};')
	g.sb.writeln('static u8 _bits__len_8_tab_data[] = {0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8};')
	g.sb.writeln('static array bits__len_8_tab = {.data = _bits__len_8_tab_data, .len = 256, .cap = 256, .element_size = sizeof(u8)};')
	// Rune map stub (placeholder - actual rune mapping is complex)
	// Uses array struct so indexing generates ((i32*)rune_maps.data)[index]
	g.sb.writeln('static i32 _rune_maps_data[] = {0};')
	g.sb.writeln('static array rune_maps = {.data = _rune_maps_data, .len = 0, .cap = 0, .element_size = sizeof(i32)};')
	// Power of 10 tables for string conversion (strconv module)
	g.sb.writeln('static u64 _ten_pow_64_data[] = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 10000000000ULL, 100000000000ULL, 1000000000000ULL, 10000000000000ULL, 100000000000000ULL, 1000000000000000ULL, 10000000000000000ULL, 100000000000000000ULL, 1000000000000000000ULL};')
	g.sb.writeln('static array strconv__ten_pow_table_64 = {.data = _ten_pow_64_data, .len = 19, .cap = 19, .element_size = sizeof(u64)};')
	// Power of 10 table for f32 (32-bit)
	g.sb.writeln('static f32 _ten_pow_32_data[] = {1.0f, 10.0f, 100.0f, 1000.0f, 10000.0f, 100000.0f, 1000000.0f, 10000000.0f, 100000000.0f, 1000000000.0f};')
	g.sb.writeln('static array strconv__ten_pow_table_32 = {.data = _ten_pow_32_data, .len = 10, .cap = 10, .element_size = sizeof(f32)};')
	// Decimal rounding table
	g.sb.writeln('static f64 _dec_round_data[] = {0.5, 0.05, 0.005, 0.0005, 0.00005, 0.000005, 0.0000005, 0.00000005, 0.000000005, 0.0000000005, 0.00000000005, 0.000000000005, 0.0000000000005, 0.00000000000005, 0.000000000000005, 0.0000000000000005, 0.00000000000000005, 0.000000000000000005};')
	g.sb.writeln('static array strconv__dec_round = {.data = _dec_round_data, .len = 18, .cap = 18, .element_size = sizeof(f64)};')
	// Power of 5 split tables for float-to-string conversion (stubs - actual tables are large)
	g.sb.writeln('static u64 _pow5_inv_split_64_x_data[] = {0, 0};')
	g.sb.writeln('static array strconv__pow5_inv_split_64_x = {.data = _pow5_inv_split_64_x_data, .len = 2, .cap = 2, .element_size = sizeof(u64)};')
	g.sb.writeln('static u64 _pow5_split_64_x_data[] = {0, 0};')
	g.sb.writeln('static array strconv__pow5_split_64_x = {.data = _pow5_split_64_x_data, .len = 2, .cap = 2, .element_size = sizeof(u64)};')
	// 32-bit power of 5 tables
	g.sb.writeln('static u64 _pow5_inv_split_32_data[] = {0, 0};')
	g.sb.writeln('static array strconv__pow5_inv_split_32 = {.data = _pow5_inv_split_32_data, .len = 2, .cap = 2, .element_size = sizeof(u64)};')
	g.sb.writeln('static u64 _pow5_split_32_data[] = {0, 0};')
	g.sb.writeln('static array strconv__pow5_split_32 = {.data = _pow5_split_32_data, .len = 2, .cap = 2, .element_size = sizeof(u64)};')
	// Exponent tables for float parsing
	g.sb.writeln('static u64 _pos_exp_data[] = {0};')
	g.sb.writeln('static array strconv__pos_exp = {.data = _pos_exp_data, .len = 1, .cap = 1, .element_size = sizeof(u64)};')
	g.sb.writeln('static u64 _neg_exp_data[] = {0};')
	g.sb.writeln('static array strconv__neg_exp = {.data = _neg_exp_data, .len = 1, .cap = 1, .element_size = sizeof(u64)};')
	// Memory panic variable
	g.sb.writeln('static bool v_memory_panic = false;')
	g.sb.writeln('')

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

// Generate tuple type name from element types and register it for struct generation
fn (mut g Gen) get_tuple_type(tuple_types []ast.Expr) string {
	mut elem_types := []string{cap: tuple_types.len}
	for t in tuple_types {
		elem_types << g.expr_type_to_c(t)
	}
	// Create type name like Tuple_2_string or Tuple_2_int_string
	type_name := 'Tuple_${tuple_types.len}_${elem_types.join('_')}'
	// Register this tuple type for struct generation
	if type_name !in g.tuple_types {
		g.tuple_types[type_name] = elem_types
	}
	return type_name
}

// Generate a synthetic typedef name for inline function pointer types
// and register it for typedef generation
fn (mut g Gen) get_fn_ptr_typedef_name(fn_type ast.FnType) string {
	// Build a mangled name from param types and return type
	mut parts := []string{}
	for param in fn_type.params {
		param_type := sanitize_type_for_mangling(g.expr_type_to_c(param.typ))
		if param.is_mut {
			parts << '${param_type}ptr'
		} else {
			parts << param_type
		}
	}
	mut ret_type := 'void'
	if fn_type.return_type !is ast.EmptyExpr {
		ret_type = sanitize_type_for_mangling(g.expr_type_to_c(fn_type.return_type))
	}
	parts << ret_type
	type_name := 'FnPtr_${parts.join('_')}'
	// Register for typedef generation
	if type_name !in g.fn_ptr_typedefs {
		g.fn_ptr_typedefs[type_name] = fn_type
	}
	return type_name
}

fn (mut g Gen) expr_type_to_c(e ast.Expr) string {
	match e {
		ast.Ident {
			name := e.name
			// Return the actual V type aliases (they are defined in the C preamble)
			if name in ['int', 'i64', 'i32', 'i16', 'i8', 'u64', 'u32', 'u16', 'u8', 'byte', 'rune',
				'f32', 'f64', 'usize', 'isize'] {
				return name
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
			// Add module prefix for types defined in the current module
			if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
				// Only add prefix if the type is defined in the current module
				if types_in_module := g.module_types[g.cur_module] {
					if name in types_in_module {
						return '${g.cur_module}__${name}'
					}
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
			// Handle variadic types like ...voidptr
			if e.op == .ellipsis {
				inner_type := g.expr_type_to_c(e.expr)
				// Map common types to VArg_<type>
				return 'VArg_${sanitize_type_for_mangling(inner_type)}'
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
			if e is ast.ArrayType {
				// Return Array_<elem_type> to preserve type information for element type inference
				elem_type := sanitize_type_for_mangling(g.expr_type_to_c(e.elem_type))
				return 'Array_${elem_type}'
			}
			if e is ast.ArrayFixedType {
				// Fixed-size arrays - return FixedArray_<elem_type> for type inference
				elem_type := sanitize_type_for_mangling(g.expr_type_to_c(e.elem_type))
				return 'FixedArray_${elem_type}'
			}
			if e is ast.MapType {
				key_type := sanitize_type_for_mangling(g.expr_type_to_c(e.key_type))
				value_type := sanitize_type_for_mangling(g.expr_type_to_c(e.value_type))
				return 'Map_${key_type}_${value_type}'
			}
			if e is ast.TupleType {
				// Multi-return tuple type
				return g.get_tuple_type(e.types)
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
			if e is ast.FnType {
				// Function pointer type - generate a synthetic typedef name
				fn_type := e as ast.FnType
				return g.get_fn_ptr_typedef_name(fn_type)
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
			if node.kind == .char {
				return 'rune'
			}
		}
		ast.StringLiteral {
			if node.kind == .c || node.value.starts_with("c'") {
				return 'char*'
			}
			return 'string'
		}
		ast.StringInterLiteral {
			return 'string'
		}
		ast.ArrayInitExpr {
			// For array literals, infer element type from explicit type annotation or first expr
			if node.typ !is ast.EmptyExpr {
				// node.typ is ArrayType{elem_type: T} for []T{} syntax
				// Use get_field_type_for_inference which handles ArrayType properly
				return g.get_field_type_for_inference(node.typ)
			}
			if node.exprs.len > 0 {
				elem_type := sanitize_type_for_mangling(g.infer_type(node.exprs[0]))
				return 'Array_${elem_type}'
			}
			return 'Array_int'
		}
		ast.InitExpr {
			// Check if it's a map type
			if node.typ is ast.Type {
				if node.typ is ast.MapType {
					mt := node.typ as ast.MapType
					key_type := sanitize_type_for_mangling(g.expr_type_to_c(mt.key_type))
					value_type := sanitize_type_for_mangling(g.expr_type_to_c(mt.value_type))
					return 'Map_${key_type}_${value_type}'
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
				// Check if we're dereferencing the interface match variable - return concrete type
				if g.cur_iface_match_var != '' && g.cur_iface_match_type != '' {
					if node.expr is ast.Ident
						&& (node.expr as ast.Ident).name == g.cur_iface_match_var {
						return g.cur_iface_match_type
					}
				}
			}
			return g.infer_type(node.expr)
		}
		ast.CallExpr {
			mut name := ''
			if node.lhs is ast.Ident {
				name = node.lhs.name
			} else if node.lhs is ast.SelectorExpr {
				// Check if this is a module-qualified function call (e.g., strings.new_builder)
				if node.lhs.lhs is ast.Ident && node.lhs.lhs.name in g.module_names {
					// Module-qualified function call
					name = '${node.lhs.lhs.name}__${node.lhs.rhs.name}'
				} else {
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
					// Array methods that return the element type
					if clean_type.starts_with('Array_') && name in ['first', 'last', 'pop'] {
						return clean_type['Array_'.len..]
					}
				}
			}

			if t := g.fn_types[name] {
				return t
			}
			// Try with current module prefix
			if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
				mangled_name := '${g.cur_module}__${name}'
				if t := g.fn_types[mangled_name] {
					return t
				}
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
				// Check if this is a struct type cast - return the struct type name
				if name !in g.fn_types {
					// Mangle type name if it's from the current module
					if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
						if types_in_module := g.module_types[g.cur_module] {
							if name in types_in_module {
								return '${g.cur_module}__${name}'
							}
						}
					}
					return name
				}
			} else if node.lhs is ast.SelectorExpr {
				// Handle C function calls (C.putchar, etc.)
				if node.lhs.lhs is ast.Ident {
					if node.lhs.lhs.name == 'C' {
						name = node.lhs.rhs.name
					} else if node.lhs.lhs.name in g.module_names {
						// Module-qualified function call
						name = '${node.lhs.lhs.name}__${node.lhs.rhs.name}'
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
					// Array methods that return the element type
					if clean_type.starts_with('Array_') && name in ['first', 'last', 'pop'] {
						return clean_type['Array_'.len..]
					}
				}
			} else if node.lhs is ast.IndexExpr || node.lhs is ast.Type {
				// Handle array type casts like []u8(b) - return the array type
				return g.expr_type_to_c(node.lhs)
			}
			if t := g.fn_types[name] {
				return t
			}
			// Try with current module prefix
			if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
				mangled_name := '${g.cur_module}__${name}'
				if t := g.fn_types[mangled_name] {
					return t
				}
			}
			return 'int'
		}
		ast.Ident {
			if t := g.var_types[node.name] {
				return t
			}
			// Check global variable types
			if t := g.global_var_types[node.name] {
				return t
			}
			// Check constant types
			if t := g.const_types[node.name] {
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
			// Hard-coded builtin type fields for common cases
			if clean_base == 'string' {
				match node.rhs.name {
					'str' { return 'u8*' }
					'len' { return 'int' }
					'is_lit' { return 'int' }
					else {}
				}
			}
			if clean_base == 'array' {
				match node.rhs.name {
					'data' { return 'voidptr' }
					'len' { return 'int' }
					'cap' { return 'int' }
					'element_size' { return 'int' }
					else {}
				}
			}
			// For Array_* types or type aliases to arrays, look up in the base 'array' struct
			// Handle type aliases like strings__Builder -> Array_u8 -> array
			lookup_type := if clean_base.starts_with('Array_')
				|| clean_base.starts_with('FixedArray_') {
				'array'
			} else if clean_base == 'strings__Builder' {
				// strings.Builder is an alias for []u8, so use array struct
				'array'
			} else {
				clean_base
			}
			if fields := g.struct_fields[lookup_type] {
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
			if base_type.starts_with('FixedArray_') {
				// FixedArray_string -> return string
				return base_type['FixedArray_'.len..]
			}
			if base_type.starts_with('Array_') {
				return base_type['Array_'.len..]
			}
			// Pointer indexing (e.g., s.str[i] where str is &u8 or u8*)
			if base_type.ends_with('*') {
				return base_type[..base_type.len - 1]
			}
			// String indexing returns u8 (byte)
			if base_type == 'string' {
				return 'u8'
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
			// Try to infer map type from explicit type annotation
			if node.typ !is ast.EmptyExpr {
				return g.expr_type_to_c(node.typ)
			}
			// Fall back to inferring from keys/values
			if node.keys.len > 0 {
				key_type := sanitize_type_for_mangling(g.infer_type(node.keys[0]))
				val_type := sanitize_type_for_mangling(g.infer_type(node.vals[0]))
				return 'Map_${key_type}_${val_type}'
			}
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
		ast.MatchExpr {
			// Infer type from first branch's result expression
			for branch in node.branches {
				if branch.stmts.len > 0 {
					last_stmt := branch.stmts[branch.stmts.len - 1]
					if last_stmt is ast.ExprStmt {
						return g.infer_type(last_stmt.expr)
					}
				}
			}
			return 'int'
		}
		ast.IfExpr {
			// Infer type from then branch's result expression
			if node.stmts.len > 0 {
				last_stmt := node.stmts[node.stmts.len - 1]
				if last_stmt is ast.ExprStmt {
					return g.infer_type(last_stmt.expr)
				}
			}
			return 'int'
		}
		ast.PostfixExpr {
			// For error propagation (!) and optional (?), return the inner expression type
			return g.infer_type(node.expr)
		}
		ast.CastExpr {
			// Type cast: return the type being cast to
			return g.expr_type_to_c(node.typ)
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
fn (mut g Gen) get_fn_name(node ast.FnDecl) string {
	// Skip C stdlib functions that would conflict
	if node.name in c_stdlib_fns {
		return ''
	}
	// Convert operator names to valid C identifiers
	name := operator_to_name(node.name)
	mut fn_name := name
	if node.is_method {
		mut base_type := g.get_type_name_for_mangling(node.receiver.typ)
		// Resolve type aliases to their base type for method calls
		// e.g., strings.Builder -> Array_u8
		if resolved := g.type_aliases[base_type] {
			base_type = resolved
		}
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
			// Check for fixed-size array - element type is an embedded dependency
			if typ is ast.ArrayFixedType {
				return g.get_embedded_struct_type(typ.elem_type)
			}
			// Dynamic array, Map, Option, etc. - not simple embedded structs
			return ''
		}
		else {
			return ''
		}
	}
}

// Get a safe identifier for type mangling (no spaces or special chars)
fn (mut g Gen) get_type_name_for_mangling(typ ast.Expr) string {
	match typ {
		ast.Ident {
			return typ.name
		}
		ast.SelectorExpr {
			// Qualified type name (e.g., strings.Builder -> strings__Builder)
			return typ.name().replace('.', '__')
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
		ast.Type {
			// Handle type expressions - need to check specific variants
			if typ is ast.ArrayType {
				arr := typ as ast.ArrayType
				elem_type := sanitize_type_for_mangling(g.get_type_name_for_mangling(arr.elem_type))
				return 'Array_${elem_type}'
			}
			if typ is ast.ArrayFixedType {
				arr := typ as ast.ArrayFixedType
				elem_type := sanitize_type_for_mangling(g.get_type_name_for_mangling(arr.elem_type))
				return 'FixedArray_${elem_type}'
			}
			if typ is ast.FnType {
				fn_type := typ as ast.FnType
				return g.get_fn_ptr_typedef_name(fn_type)
			}
			return ''
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
		field_name := escape_c_keyword(field.name)
		// Check for fixed-size array type - use the helper function
		if arr_info := g.get_fixed_array_info(field.typ) {
			g.sb.writeln('${arr_info.elem_type} ${field_name}[${arr_info.size}];')
		} else {
			t := type_for_c_decl(g.expr_type_to_c(field.typ))
			g.sb.writeln('${t} ${field_name};')
		}
	}
	g.sb.writeln('};')
}

struct FixedArrayInfo {
	elem_type string
	size      string
}

// get_fixed_array_info extracts element type and size from a fixed-size array type
fn (mut g Gen) get_fixed_array_info(typ ast.Expr) ?FixedArrayInfo {
	// ast.ArrayFixedType is a variant of ast.Type, which is a variant of ast.Expr
	// First match on ast.Type, then check for ArrayFixedType
	match typ {
		ast.Type {
			if typ is ast.ArrayFixedType {
				elem_type := g.expr_type_to_c(typ.elem_type)
				size_str := if typ.len is ast.BasicLiteral {
					typ.len.value
				} else if typ.len is ast.Ident {
					typ.len.name
				} else {
					'0'
				}
				return FixedArrayInfo{elem_type, size_str}
			}
		}
		else {}
	}
	return none
}

// sanitize_type_for_mangling converts C type names to safe identifiers for type mangling
fn sanitize_type_for_mangling(t string) string {
	return t.replace('*', 'ptr').replace(' ', '_')
}

// unsanitize_type_for_c converts a sanitized type name back to a valid C type
fn unsanitize_type_for_c(t string) string {
	return t.replace('ptr', '*').replace('_', ' ')
}

// Known array typedefs - types that have been typedef'd
const known_array_types = ['Array_int', 'Array_u8', 'Array_i8', 'Array_u16', 'Array_i16', 'Array_u32',
	'Array_i32', 'Array_u64', 'Array_i64', 'Array_f32', 'Array_f64', 'Array_string', 'Array_rune',
	'Array_bool', 'Array_voidptr', 'Array_byte']

// type_for_c_decl converts a V type name to a valid C type for variable declaration
// For unknown Array_ types, it falls back to 'array'
fn type_for_c_decl(t string) string {
	if t.starts_with('Array_') && t !in known_array_types {
		return 'array'
	}
	return t
}

// mangle_type_if_needed adds module prefix to type name if it's from the current module
fn (g Gen) mangle_type_if_needed(type_name string) string {
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		if types_in_module := g.module_types[g.cur_module] {
			if type_name in types_in_module {
				return '${g.cur_module}__${type_name}'
			}
		}
	}
	return type_name
}

// get_field_type_for_inference returns a type string suitable for type inference
// For fixed-size arrays, returns FixedArray_<elem_type> so indexing can return elem_type
// For dynamic arrays, returns Array_<elem_type>
fn (mut g Gen) get_field_type_for_inference(typ ast.Expr) string {
	match typ {
		ast.Type {
			if typ is ast.ArrayFixedType {
				elem_type := sanitize_type_for_mangling(g.expr_type_to_c(typ.elem_type))
				return 'FixedArray_${elem_type}'
			}
			if typ is ast.ArrayType {
				elem_type := sanitize_type_for_mangling(g.expr_type_to_c(typ.elem_type))
				return 'Array_${elem_type}'
			}
		}
		else {}
	}
	// Fall back to expr_type_to_c for other types
	return g.expr_type_to_c(typ)
}

fn (mut g Gen) gen_global_decl(node ast.GlobalDecl) {
	for field in node.fields {
		full_type := g.expr_type_to_c(field.typ)
		t := type_for_c_decl(full_type)
		// Skip globals with void type (type inference failed)
		if t == 'void' {
			continue
		}
		// Track global variable type for proper access operator (. vs ->)
		// Store full type (not simplified) for element type inference
		g.global_var_types[field.name] = full_type
		g.sb.writeln('${t} ${field.name};')
	}
}

fn (mut g Gen) gen_const_decl(node ast.ConstDecl, file_module string) {
	for field in node.fields {
		// Generate mangled name with module prefix
		mangled_name := if file_module != '' && file_module != 'main' && file_module != 'builtin' {
			'${file_module}__${field.name}'
		} else {
			field.name
		}
		// Skip duplicate constants
		if mangled_name in g.seen_consts {
			continue
		}
		// Infer type first (needed for type tracking even if we don't generate)
		t := g.infer_type(field.value)
		// Skip consts with void/empty types
		if t == 'void' || t == '' {
			continue
		}
		// Track both mangled and unmangled names for reference checking
		g.seen_consts[mangled_name] = true
		g.seen_consts[field.name] = true
		// Track constant types for type inference (even for complex constants like arrays)
		g.const_types[mangled_name] = t
		g.const_types[field.name] = t
		// Map unmangled name to mangled name for cross-module references
		// (do this for all constants, even arrays that are generated as stubs)
		g.const_mangled[field.name] = mangled_name
		// Skip generating array constants (handled separately as stubs)
		if t.starts_with('Array_') {
			continue
		}
		// Only generate simple literal constants
		if !g.is_simple_literal(field.value) {
			continue
		}
		// Use enum for integer types to support TCC's stricter const requirements
		// enum values are compile-time constants without the text substitution issues of #define
		if t in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'byte', 'rune', 'usize',
			'isize'] {
			g.sb.write_string('enum { ${mangled_name} = ')
			g.gen_expr(field.value)
			g.sb.writeln(' };')
		} else {
			g.sb.write_string('const ${t} ${mangled_name} = ')
			g.gen_expr(field.value)
			g.sb.writeln(';')
		}
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
		ast.InfixExpr {
			// Allow arithmetic on literals/consts: degree - 1, 2 * degree - 1, 1 << n
			if e.op in [.plus, .minus, .mul, .div, .mod, .left_shift, .right_shift, .amp, .pipe,
				.xor] {
				return g.is_simple_literal(e.lhs) && g.is_simple_literal(e.rhs)
			}
			return false
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
					return g.is_simple_literal_or_const_ref(e.expr)
				}
			}
			return false
		}
		ast.Ident {
			// Allow references to already-generated constants
			return e.name in g.seen_consts || g.is_known_const(e.name)
		}
		ast.KeywordOperator {
			// sizeof(type) is a compile-time constant
			if e.op == .key_sizeof {
				return true
			}
			return false
		}
		ast.ComptimeExpr {
			// Handle $if compile-time conditionals - evaluate and check the resulting branch
			if e.expr is ast.IfExpr {
				return g.is_comptime_if_simple_literal(e.expr)
			}
			return false
		}
		else {
			return false
		}
	}
}

// is_comptime_if_simple_literal checks if a compile-time $if expression results in a simple literal
fn (g Gen) is_comptime_if_simple_literal(node ast.IfExpr) bool {
	cond_result := g.eval_comptime_cond(node.cond)

	if cond_result {
		// Condition is true - check the then branch
		if node.stmts.len == 1 {
			stmt := node.stmts[0]
			if stmt is ast.ExprStmt {
				return g.is_simple_literal(stmt.expr)
			}
		}
	} else {
		// Condition is false - check the else branch
		if node.else_expr !is ast.EmptyExpr {
			else_e := node.else_expr
			if else_e is ast.IfExpr {
				if else_e.cond is ast.EmptyExpr {
					// Plain $else block
					if else_e.stmts.len == 1 {
						stmt := else_e.stmts[0]
						if stmt is ast.ExprStmt {
							return g.is_simple_literal(stmt.expr)
						}
					}
				} else {
					// $else $if - recursive check
					return g.is_comptime_if_simple_literal(else_e)
				}
			}
		}
	}
	return false
}

// is_simple_literal_or_const_ref checks if expression is a simple literal or a reference to a known constant
fn (g Gen) is_simple_literal_or_const_ref(e ast.Expr) bool {
	if g.is_simple_literal(e) {
		return true
	}
	// Also allow identifiers that refer to known constants
	if e is ast.Ident {
		return g.is_known_const(e.name)
	}
	return false
}

// is_known_const checks if a name refers to a constant in the type environment
fn (g Gen) is_known_const(name string) bool {
	if g.env == unsafe { nil } {
		return false
	}
	// Check builtin scope first, then current module scope
	lock g.env.scopes {
		if mut scope := g.env.scopes['builtin'] {
			if obj := scope.lookup_parent(name, 0) {
				if obj is types.Const {
					return true
				}
			}
		}
		if mut scope := g.env.scopes[g.cur_module] {
			if obj := scope.lookup_parent(name, 0) {
				if obj is types.Const {
					return true
				}
			}
		}
	}
	return false
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
	is_flag := node.attributes.has('flag')
	// Generate C enum declaration
	g.sb.writeln('typedef enum {')
	for i, field in node.fields {
		g.sb.write_string('\t${node.name}__${field.name}')
		if field.value !is ast.EmptyExpr {
			g.sb.write_string(' = ')
			g.gen_expr(field.value)
		} else if is_flag {
			// Flag enums use bit values: 1, 2, 4, 8, ...
			g.sb.write_string(' = ${u64(1) << i}U')
		}
		if i < node.fields.len - 1 {
			g.sb.writeln(',')
		} else {
			g.sb.writeln('')
		}
	}
	g.sb.writeln('} ${node.name};')
	// Note: has() and all() methods are fully desugared by the transformer
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
	// Add built-in type_name method (returns runtime type name string)
	g.sb.writeln('\tstring (*type_name)(void*);')
	// Generate function pointers for each method
	for field in node.fields {
		g.write_indent()
		g.sb.write_string('\t')
		// Interface fields can be method signatures or regular fields
		// Check if field.typ is a FnType (method signature)
		// Use get_fn_type helper to safely extract FnType from field.typ
		if fn_type := g.get_fn_type_from_expr(field.typ) {
			// Method signature - generate function pointer
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
			// Regular field or other type expression
			t := g.expr_type_to_c(field.typ)
			g.sb.writeln('${t} ${field.name};')
		}
	}
	g.sb.writeln('};')
}

// Helper to extract FnType from an Expr (handles ast.Type wrapping)
fn (g Gen) get_fn_type_from_expr(e ast.Expr) ?ast.FnType {
	if e is ast.Type {
		// After the is check, e is smart-cast to ast.Type
		// Now check if the Type variant is FnType
		if e is ast.FnType {
			return e
		}
	}
	return none
}

// gen_interface_wrappers generates wrapper functions for interface vtables
// Each wrapper casts void* to the concrete type and calls the actual method
fn (mut g Gen) gen_interface_wrappers() {
	// Track which type_name wrappers we've generated to avoid duplicates
	mut generated_type_name_wrappers := map[string]bool{}

	// For each interface
	for iface_name, methods in g.interface_meths {
		// For each concrete type that has these methods
		for type_name, type_meths in g.type_methods {
			// Check if this type implements all interface methods
			if !g.type_implements_interface(methods, type_meths) {
				continue
			}

			// Generate type_name wrapper for this type (once per interface+type combo)
			type_name_wrapper := '${iface_name}_${type_name}_type_name_wrapper'
			if type_name_wrapper !in generated_type_name_wrappers {
				generated_type_name_wrappers[type_name_wrapper] = true
				g.sb.writeln('static string ${type_name_wrapper}(void* _obj) {')
				g.sb.writeln('\t(void)_obj; // Unused')
				g.sb.writeln('\treturn (string){"${type_name}", ${type_name.len}};')
				g.sb.writeln('}')
				g.sb.writeln('')
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

// get_or_assign_type_id returns a unique type ID for the given type name.
// This is used for interface type matching.
fn (mut g Gen) get_or_assign_type_id(type_name string) int {
	if id := g.type_ids[type_name] {
		return id
	}
	id := g.next_type_id
	g.type_ids[type_name] = id
	g.next_type_id++
	return id
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
		// Check if base type is a function type - needs special syntax
		if node.base_type is ast.Type {
			if node.base_type is ast.FnType {
				fn_type := node.base_type as ast.FnType
				// Generate: typedef ret_type (*name)(param_types);
				mut ret_type := 'void'
				if fn_type.return_type !is ast.EmptyExpr {
					ret_type = g.expr_type_to_c(fn_type.return_type)
				}
				g.sb.write_string('typedef ${ret_type} (*${mangled_name})(')
				for i, param in fn_type.params {
					if i > 0 {
						g.sb.write_string(', ')
					}
					param_type := g.expr_type_to_c(param.typ)
					if param.is_mut {
						g.sb.write_string('${param_type}*')
					} else {
						g.sb.write_string(param_type)
					}
				}
				g.sb.writeln(');')
				return
			}
		}
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
	g.mut_params = map[string]bool{}
	g.defer_stmts.clear()

	// Set the current function's return type for proper return statement generation
	ret_expr := node.typ.return_type
	if ret_expr !is ast.EmptyExpr {
		g.cur_fn_ret_type = g.expr_type_to_c(ret_expr)
	} else {
		g.cur_fn_ret_type = 'void'
	}

	// Register receiver for methods
	if node.is_method {
		receiver_type := g.expr_type_to_c(node.receiver.typ)
		if node.receiver.is_mut {
			g.var_types[node.receiver.name] = receiver_type + '*'
			g.mut_params[node.receiver.name] = true
		} else {
			g.var_types[node.receiver.name] = receiver_type
		}
	}

	// Register params with element types for proper inference
	for param in node.typ.params {
		t := g.get_field_type_for_inference(param.typ)
		if param.is_mut {
			g.var_types[param.name] = t + '*'
			g.mut_params[param.name] = true
		} else {
			g.var_types[param.name] = t
		}
	}

	g.gen_fn_head(node)
	g.sb.writeln(' {')
	g.indent++
	g.gen_stmts(node.stmts)

	// Emit deferred statements before implicit return (only if no explicit return)
	// If the function has explicit returns, defers are emitted at each return site
	if !g.stmts_has_return(node.stmts) {
		g.emit_deferred_stmts()
	}

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
			// Handle multi-return assignments: a, b := func() or a, b = func()
			if node.lhs.len > 1 && node.rhs.len == 1 {
				g.gen_multi_return_assign(node)
				return
			}
			// Handle parallel assignments: a, b := x, y or a, b = x, y
			if node.lhs.len > 1 && node.rhs.len == node.lhs.len {
				for i in 0 .. node.lhs.len {
					lhs_i := node.lhs[i]
					rhs_i := node.rhs[i]
					mut name := ''
					if lhs_i is ast.Ident {
						name = lhs_i.name
					} else if lhs_i is ast.ModifierExpr {
						if lhs_i.expr is ast.Ident {
							name = lhs_i.expr.name
						}
					}
					if name == '_' {
						continue // Skip blank identifier
					}
					typ := g.infer_type(rhs_i)
					g.var_types[name] = typ
					c_typ := type_for_c_decl(typ)
					g.write_indent()
					g.sb.write_string('${c_typ} ${name} = ')
					g.gen_expr(rhs_i)
					g.sb.writeln(';')
				}
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
				// Check if RHS is a MatchExpr - need to hoist into branches
				if rhs is ast.MatchExpr {
					g.gen_assign_match_expr(lhs, rhs, node.op, true)
					return
				}
				typ := g.infer_type(rhs)
				g.var_types[name] = typ
				c_typ := type_for_c_decl(typ)
				g.sb.write_string('${c_typ} ${name} = ')
				g.gen_expr(rhs)
				g.sb.writeln(';')
			} else {
				// assignment
				// Check if LHS is a map index expression
				if lhs is ast.IndexExpr {
					lhs_type := g.infer_type(lhs.lhs)
					if lhs_type.starts_with('Map_') {
						// Map assignment: __map_<type>_set(&m, key, val)
						g.sb.write_string('__${lhs_type}_set(&')
						g.gen_expr(lhs.lhs)
						g.sb.write_string(', ')
						g.gen_expr(lhs.expr)
						g.sb.write_string(', ')
						g.gen_expr(rhs)
						g.sb.writeln(');')
						return
					}
				}
				// Check if RHS is an IfExpr that can't be ternary - hoist assignment into branches
				if rhs is ast.IfExpr && !g.can_be_ternary(rhs) && rhs.else_expr !is ast.EmptyExpr {
					g.gen_assign_if_expr(lhs, rhs, node.op)
					return
				}
				// Check if RHS is a MatchExpr - hoist assignment into branches
				if rhs is ast.MatchExpr {
					g.gen_assign_match_expr(lhs, rhs, node.op, false)
					return
				}
				// Check if LHS is a mut parameter - need to dereference for assignment
				if lhs is ast.Ident && lhs.name in g.mut_params {
					g.sb.write_string('*')
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
				// Set enum context for RHS based on LHS type
				lhs_type := g.infer_type(lhs)
				old_match_type := g.cur_match_type
				if lhs_type in g.enum_names {
					g.cur_match_type = lhs_type
				}
				g.gen_expr(rhs)
				g.cur_match_type = old_match_type
				g.sb.writeln(';')
			}
		}
		ast.ExprStmt {
			g.write_indent()
			g.gen_expr(node.expr)
			g.sb.writeln(';')
		}
		ast.ReturnStmt {
			// Handle multi-return: return a, b -> return (Tuple_N_type){a, b}
			if node.exprs.len > 1 {
				// Emit deferred statements first (if any)
				g.emit_deferred_stmts()
				g.write_indent()
				// Use the function's declared return type for proper tuple typing
				tuple_type := g.cur_fn_ret_type
				g.sb.write_string('return (${tuple_type}){')
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
				if node.exprs.len > 0 {
					expr := node.exprs[0]
					// Check if returning none for an optional tuple type
					mut is_none := false
					if expr is ast.Keyword {
						is_none = expr.tok == .key_none
					} else if expr is ast.Type {
						is_none = expr is ast.NoneType
					}
					if is_none && g.cur_fn_ret_type.starts_with('Tuple_') {
						// Return zero-initialized tuple
						g.write_indent()
						g.sb.writeln('return (${g.cur_fn_ret_type}){{0}};')
					} else if expr is ast.MatchExpr {
						// Handle match expression in return - generate if-else chain
						g.gen_return_match_expr(expr)
					} else if g.is_error_return(expr) {
						// Handle return error(...) in non-result functions - convert to panic
						// This happens when !T is simplified to T but the code has return error(...)
						g.write_indent()
						g.sb.write_string('panic(')
						if expr is ast.CallExpr {
							if expr.args.len > 0 {
								g.gen_expr(expr.args[0])
							} else {
								g.sb.write_string('(string){"error", 5}')
							}
						} else if expr is ast.CallOrCastExpr {
							// CallOrCastExpr has a single expr, not args array
							g.gen_expr(expr.expr)
						} else {
							g.sb.write_string('(string){"error", 5}')
						}
						g.sb.writeln(');')
					} else if g.cur_fn_ret_type in g.interface_names {
						// Returning to an interface type - may need boxing
						// Check if the expression is &StructType{...} (heap allocation returning pointer)
						if expr is ast.PrefixExpr && expr.op == .amp && expr.expr is ast.InitExpr {
							// Boxing a heap-allocated struct into an interface
							init_expr := expr.expr as ast.InitExpr
							concrete_type := g.expr_type_to_c(init_expr.typ)
							type_id := g.get_or_assign_type_id(concrete_type)
							g.write_indent()
							g.sb.write_string('return ({ ')
							g.sb.write_string('${concrete_type}* _iface_obj = (${concrete_type}*)malloc(sizeof(${concrete_type})); ')
							g.sb.write_string('*_iface_obj = (${concrete_type}){')
							for i, field in init_expr.fields {
								if i > 0 {
									g.sb.write_string(', ')
								}
								g.sb.write_string('.${field.name} = ')
								g.gen_expr(field.value)
							}
							g.sb.write_string('}; ')
							// Create interface struct with vtable
							g.sb.write_string('(${g.cur_fn_ret_type}){._object = _iface_obj, ._type_id = ${type_id}')
							// Add type_name wrapper
							g.sb.write_string(', .type_name = ${g.cur_fn_ret_type}_${concrete_type}_type_name_wrapper')
							// Add function pointers for interface methods
							if methods := g.interface_meths[g.cur_fn_ret_type] {
								for meth in methods {
									g.sb.write_string(', .${meth} = ${g.cur_fn_ret_type}_${concrete_type}_${meth}_wrapper')
								}
							}
							g.sb.writeln('}; });')
						} else {
							g.write_indent()
							g.sb.write_string('return ')
							g.gen_expr(expr)
							g.sb.writeln(';')
						}
					} else {
						g.write_indent()
						g.sb.write_string('return ')
						// Set enum context for shorthand resolution in return expression
						old_match_type := g.cur_match_type
						if g.cur_fn_ret_type in g.enum_names {
							g.cur_match_type = g.cur_fn_ret_type
						}
						g.gen_expr(expr)
						g.cur_match_type = old_match_type
						g.sb.writeln(';')
					}
				} else {
					g.write_indent()
					g.sb.writeln('return;')
				}
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
				// Handle special characters that need escaping
				char_val := if node.value == "'" {
					"\\'" // Single quote needs escaping in C char literals
				} else if node.value == '\\' {
					'\\\\' // Backslash needs escaping
				} else {
					node.value
				}
				g.sb.write_string("'${char_val}'")
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
			// Check for C string literal via kind field (preferred) or value prefix
			if node.kind == .c || node.value.starts_with("c'") {
				mut val := node.value.trim("c'").trim("'")
				// Also handle double-quoted C strings
				val = val.trim('"')
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
				// Convert V string escapes to C string escapes:
				// - V's \' (escaped single quote) becomes ' (no escape needed in C double-quoted string)
				// - Any " in the value needs to become \" in C
				clean_val = clean_val.replace("\\'", "'") // V escaped single quote -> C single quote
				clean_val = clean_val.replace('"', '\\"') // Escape double quotes for C
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
			// Handle comptime identifiers that start with @
			if node.name.starts_with('@') {
				// Handle known comptime values
				if node.name == '@VCURRENTHASH' {
					// Return a placeholder string for the V compiler hash
					g.sb.write_string('(string){"VCURRENTHASH", 12}')
				} else if node.name == '@FN' {
					// Return the current function name as a string
					// For now, just return a placeholder
					g.sb.write_string('(string){"<fn>", 4}')
				} else if node.name == '@LINE' {
					g.sb.write_string('__LINE__')
				} else if node.name == '@FILE' {
					g.sb.write_string('__FILE__')
				} else {
					// Unknown comptime value - return placeholder
					g.sb.write_string('0 /* unknown comptime: ${node.name} */')
				}
			} else if node.name in g.var_types {
				// Local variable - use as-is
				g.sb.write_string(node.name)
			} else if mangled := g.const_mangled[node.name] {
				// Use mangled name for constants from other modules
				g.sb.write_string(mangled)
			} else {
				g.sb.write_string(node.name)
			}
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
			// Also handle &&type(expr) -> (type**)(expr)
			// This is V syntax for casting to a pointer type
			if node.op == .amp && node.expr is ast.CastExpr {
				// Handle &[]u8(b) -> (Array_u8*)(b) - type cast to pointer
				cast_expr := node.expr as ast.CastExpr
				type_name := g.expr_type_to_c(cast_expr.typ)
				g.sb.write_string('((${type_name}*)(')
				g.gen_expr(cast_expr.expr)
				g.sb.write_string('))')
				return
			}
			if node.op == .amp && node.expr is ast.CallOrCastExpr {
				cast_expr := node.expr as ast.CallOrCastExpr
				if cast_expr.lhs is ast.Ident {
					type_name := cast_expr.lhs.name
					// Any identifier that's not a known function is treated as a type cast
					// This handles primitive types, structs, and other user-defined types
					if type_name !in g.fn_types {
						// Mangle type name if it's from the current module
						mangled_type := g.mangle_type_if_needed(type_name)
						// Generate pointer cast: (type*)(expr)
						g.sb.write_string('((${mangled_type}*)(')
						g.gen_expr(cast_expr.expr)
						g.sb.write_string('))')
						return
					}
				} else if cast_expr.lhs is ast.IndexExpr || cast_expr.lhs is ast.Type {
					// Handle &[]u8(b) -> (Array_u8*)(b) - array type cast to pointer
					type_name := g.expr_type_to_c(cast_expr.lhs)
					g.sb.write_string('((${type_name}*)(')
					g.gen_expr(cast_expr.expr)
					g.sb.write_string('))')
					return
				} else if cast_expr.lhs is ast.PrefixExpr && cast_expr.lhs.op == .amp {
					// Handle &&type(expr) -> (type**)(expr)
					// cast_expr.lhs is &type, so cast_expr.lhs.expr is the type name
					if cast_expr.lhs.expr is ast.Ident {
						type_name := cast_expr.lhs.expr.name
						if type_name !in g.fn_types {
							mangled_type := g.mangle_type_if_needed(type_name)
							g.sb.write_string('((${mangled_type}**)(')
							g.gen_expr(cast_expr.expr)
							g.sb.write_string('))')
							return
						}
					}
				}
			}
			// Check for nested & on pointer cast: &(&type(expr)) or similar patterns
			if node.op == .amp && node.expr is ast.PrefixExpr {
				inner := node.expr as ast.PrefixExpr
				if inner.op == .amp && inner.expr is ast.CallOrCastExpr {
					cast_expr := inner.expr as ast.CallOrCastExpr
					if cast_expr.lhs is ast.Ident {
						type_name := cast_expr.lhs.name
						if type_name !in g.fn_types {
							// &&type(expr) -> (type**)(expr)
							mangled_type := g.mangle_type_if_needed(type_name)
							g.sb.write_string('((${mangled_type}**)(')
							g.gen_expr(cast_expr.expr)
							g.sb.write_string('))')
							return
						}
					}
				}
			}
			// Check for interface match variable dereference: *matched_var -> *((ConcreteType*)matched_var._object)
			if node.op == .mul && g.cur_iface_match_var != '' && g.cur_iface_match_type != '' {
				if node.expr is ast.Ident && node.expr.name == g.cur_iface_match_var {
					// Generate: *((ConcreteType*)matched_var._object)
					g.sb.write_string('(*((${g.cur_iface_match_type}*)${g.cur_iface_match_var}._object))')
					return
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

			// Statement IF
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
			// Set match type context for enum shorthand (.value syntax)
			match_type := g.infer_type(node.expr)
			old_match_type := g.cur_match_type
			g.cur_match_type = match_type

			// Check if this is a type match on an interface (matching against type names)
			is_interface_match := match_type in g.interface_names

			if is_interface_match {
				// Interface type matching - generate if-else chain checking _type_id
				// Save expression to temp var to avoid multiple evaluation
				g.write_indent()
				g.sb.write_string('{ ')
				// Get the interface value
				g.sb.write_string('int __type_id = (')
				g.gen_expr(node.expr)
				g.sb.writeln(')._type_id;')

				// Track the matched variable name for deref/method call handling
				old_iface_match_var := g.cur_iface_match_var
				old_iface_match_type := g.cur_iface_match_type
				g.cur_iface_match_var = if node.expr is ast.Ident { node.expr.name } else { '' }

				mut first := true
				for branch in node.branches {
					if branch.cond.len == 0 {
						// else branch - no concrete type
						g.cur_iface_match_type = ''
						g.write_indent()
						if first {
							g.sb.writeln('{')
						} else {
							g.sb.writeln(' else {')
						}
						g.indent++
						g.gen_stmts(branch.stmts)
						g.indent--
						g.write_indent()
						g.sb.write_string('}')
					} else {
						// Type conditions - generate if/else if chain
						g.write_indent()
						if first {
							g.sb.write_string('if (')
							first = false
						} else {
							g.sb.write_string(' else if (')
						}
						// Generate condition: type_id == TYPE_ID_TypeName || type_id == TYPE_ID_TypeName2 ...
						// Also track the first concrete type for this branch
						mut concrete_type := ''
						for i, c in branch.cond {
							if i > 0 {
								g.sb.write_string(' || ')
							}
							// Get type name from condition
							type_name := if c is ast.Ident {
								c.name
							} else {
								'unknown'
							}
							if i == 0 {
								concrete_type = type_name
							}
							type_id := g.get_or_assign_type_id(type_name)
							g.sb.write_string('__type_id == ${type_id}')
						}
						g.sb.writeln(') {')
						g.indent++
						// Set the concrete type for this branch
						g.cur_iface_match_type = concrete_type
						g.gen_stmts(branch.stmts)
						g.indent--
						g.write_indent()
						g.sb.write_string('}')
					}
				}
				// Restore interface match tracking
				g.cur_iface_match_var = old_iface_match_var
				g.cur_iface_match_type = old_iface_match_type
				g.sb.writeln('')
				g.write_indent()
				g.sb.writeln('}')
			} else if g.has_range_condition(node) {
				// Match with range conditions - use if-else chain
				g.write_indent()
				g.sb.write_string('{ ')
				c_match_type := type_for_c_decl(match_type)
				g.sb.write_string('${c_match_type} __match_val = ')
				g.gen_expr(node.expr)
				g.sb.writeln(';')

				mut first := true
				for branch in node.branches {
					if branch.cond.len == 0 {
						// else branch
						g.write_indent()
						if first {
							g.sb.writeln('{')
						} else {
							g.sb.writeln(' else {')
						}
					} else {
						g.write_indent()
						if first {
							g.sb.write_string('if (')
							first = false
						} else {
							g.sb.write_string(' else if (')
						}
						// Generate conditions with proper handling for ranges
						for ci, c in branch.cond {
							if ci > 0 {
								g.sb.write_string(' || ')
							}
							if c is ast.RangeExpr {
								// Range condition: __match_val >= start && __match_val <= end
								g.sb.write_string('(__match_val >= ')
								g.gen_expr(c.start)
								g.sb.write_string(' && __match_val <= ')
								g.gen_expr(c.end)
								g.sb.write_string(')')
							} else {
								g.sb.write_string('(__match_val == ')
								g.gen_expr(c)
								g.sb.write_string(')')
							}
						}
						g.sb.writeln(') {')
					}
					g.indent++
					g.gen_stmts(branch.stmts)
					g.indent--
					g.write_indent()
					g.sb.write_string('}')
				}
				g.sb.writeln('')
				g.write_indent()
				g.sb.writeln('}')
			} else {
				// Regular value match - use switch statement
				g.write_indent()
				g.sb.write_string('switch (')
				g.gen_expr(node.expr)
				g.sb.writeln(') {')
				for branch in node.branches {
					if branch.cond.len == 0 {
						g.write_indent()
						g.sb.writeln('default: {')
					} else {
						for i, c in branch.cond {
							g.write_indent()
							g.sb.write_string('case ')
							g.gen_expr(c)
							g.sb.write_string(':')
							// Only open brace on the last condition
							if i == branch.cond.len - 1 {
								g.sb.writeln(' {')
							} else {
								g.sb.writeln('')
							}
						}
					}
					g.indent++
					g.gen_stmts(branch.stmts)
					g.write_indent()
					g.sb.writeln('break;')
					g.indent--
					g.write_indent()
					g.sb.writeln('}')
				}
				g.write_indent()
				g.sb.writeln('}')
			}
			g.cur_match_type = old_match_type
		}
		ast.InfixExpr {
			// Handle 'in' and '!in' operators with array literals specially
			if node.op == .key_in || node.op == .not_in {
				if node.rhs is ast.ArrayInitExpr {
					// x in [a, b, c] => (x == a || x == b || x == c)
					// x !in [a, b, c] => (x != a && x != b && x != c)
					cmp_op := if node.op == .key_in { '==' } else { '!=' }
					join_op := if node.op == .key_in { ' || ' } else { ' && ' }
					// Set enum context for shorthand resolution in array elements
					lhs_type_in := g.infer_type(node.lhs)
					old_match_type_in := g.cur_match_type
					if lhs_type_in in g.enum_names {
						g.cur_match_type = lhs_type_in
					}
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
					g.cur_match_type = old_match_type_in
				} else {
					// Check if RHS is a map type
					rhs_type := g.infer_type(node.rhs)
					if rhs_type.starts_with('Map_') || rhs_type == 'map' {
						// x in map => map__exists(&map, &key)
						// x !in map => !map__exists(&map, &key)
						if node.op == .not_in {
							g.sb.write_string('!')
						}
						g.sb.write_string('map__exists(&')
						g.gen_expr(node.rhs)
						g.sb.write_string(', &')
						g.gen_expr(node.lhs)
						g.sb.write_string(')')
					} else if rhs_type.starts_with('Array_') {
						// x in arr => array__contains_u8(&arr, x) (for u8 arrays)
						// x !in arr => !array__contains_u8(&arr, x)
						elem_type := rhs_type['Array_'.len..]
						if node.op == .not_in {
							g.sb.write_string('!')
						}
						g.sb.write_string('array__contains_${elem_type}(&')
						g.gen_expr(node.rhs)
						g.sb.write_string(', ')
						g.gen_expr(node.lhs)
						g.sb.write_string(')')
					} else {
						// For other non-array RHS, generate a TODO marker
						g.sb.write_string('/* TODO: in/!in with non-array type: ${rhs_type} */')
					}
				}
			} else if node.op == .left_shift {
				// Check if this is array append (arr << elem) vs bitshift
				lhs_type := g.infer_type(node.lhs)
				// Handle array types and type aliases (e.g., strings__Builder is an alias for []u8)
				lhs_is_array := lhs_type.starts_with('Array_') || lhs_type == 'array'
					|| lhs_type.starts_with('strings__Builder')
					|| (lhs_type.ends_with('*') && (lhs_type.starts_with('Array_')
					|| lhs_type[..lhs_type.len - 1] == 'strings__Builder'))
				if lhs_is_array {
					// Array append: arr << elem => array__push(&arr, elem)
					// Determine element type
					clean_lhs_type := if lhs_type.ends_with('*') {
						lhs_type[..lhs_type.len - 1]
					} else {
						lhs_type
					}
					elem_type := if clean_lhs_type.starts_with('Array_') {
						clean_lhs_type['Array_'.len..]
					} else if clean_lhs_type.starts_with('strings__Builder') {
						'u8' // strings.Builder is []u8
					} else {
						'int'
					}
					rhs_type := g.infer_type(node.rhs)
					// Check if RHS is also an array (need push_many instead of push)
					rhs_clean := if rhs_type.ends_with('*') {
						rhs_type[..rhs_type.len - 1]
					} else {
						rhs_type
					}
					rhs_is_array := rhs_clean.starts_with('Array_') || rhs_clean == 'array'
						|| rhs_clean.starts_with('strings__Builder')
					if rhs_is_array {
						// Pushing entire array: use array__push_many
						if lhs_type.ends_with('*') {
							g.sb.write_string('array__push_many(')
							g.gen_expr(node.lhs)
						} else {
							g.sb.write_string('array__push_many(&')
							g.gen_expr(node.lhs)
						}
						g.sb.write_string(', ')
						// Need to pass data pointer and length
						if rhs_type.ends_with('*') {
							g.gen_expr(node.rhs)
							g.sb.write_string('->data, ')
							g.gen_expr(node.rhs)
							g.sb.write_string('->len)')
						} else {
							g.sb.write_string('(')
							g.gen_expr(node.rhs)
							g.sb.write_string(').data, (')
							g.gen_expr(node.rhs)
							g.sb.write_string(').len)')
						}
					} else {
						// If LHS is already a pointer, pass it directly; otherwise take address
						if lhs_type.ends_with('*') {
							g.sb.write_string('array__push(')
							g.gen_expr(node.lhs)
						} else {
							g.sb.write_string('array__push(&')
							g.gen_expr(node.lhs)
						}
						// If RHS is already a pointer or void*, wrap it differently
						if rhs_type.ends_with('*') || rhs_type == 'voidptr'
							|| elem_type == 'voidptr' {
							g.sb.write_string(', _MOV((void*[]){')
							g.gen_expr(node.rhs)
							g.sb.write_string('}))')
						} else {
							g.sb.write_string(', _MOV((${elem_type}[]){')
							g.gen_expr(node.rhs)
							g.sb.write_string('}))')
						}
					}
				} else {
					// Regular bitshift
					g.sb.write_string('(')
					g.gen_expr(node.lhs)
					g.sb.write_string(' << ')
					g.gen_expr(node.rhs)
					g.sb.write_string(')')
				}
			} else if node.op in [.eq, .ne, .lt, .gt, .le, .ge] {
				// Check for string comparisons - need function calls instead of operators
				lhs_type := g.infer_type(node.lhs)
				rhs_type := g.infer_type(node.rhs)
				if lhs_type == 'string' && rhs_type == 'string' {
					// Generate string comparison function calls
					match node.op {
						.eq {
							// s1 == s2 -> string__eq(s1, s2)
							g.sb.write_string('string__eq(')
							g.gen_expr(node.lhs)
							g.sb.write_string(', ')
							g.gen_expr(node.rhs)
							g.sb.write_string(')')
						}
						.ne {
							// s1 != s2 -> !string__eq(s1, s2)
							g.sb.write_string('!string__eq(')
							g.gen_expr(node.lhs)
							g.sb.write_string(', ')
							g.gen_expr(node.rhs)
							g.sb.write_string(')')
						}
						.lt {
							// s1 < s2 -> string__lt(s1, s2)
							g.sb.write_string('string__lt(')
							g.gen_expr(node.lhs)
							g.sb.write_string(', ')
							g.gen_expr(node.rhs)
							g.sb.write_string(')')
						}
						.gt {
							// s1 > s2 -> string__lt(s2, s1)
							g.sb.write_string('string__lt(')
							g.gen_expr(node.rhs)
							g.sb.write_string(', ')
							g.gen_expr(node.lhs)
							g.sb.write_string(')')
						}
						.le {
							// s1 <= s2 -> !string__lt(s2, s1)
							g.sb.write_string('!string__lt(')
							g.gen_expr(node.rhs)
							g.sb.write_string(', ')
							g.gen_expr(node.lhs)
							g.sb.write_string(')')
						}
						.ge {
							// s1 >= s2 -> !string__lt(s1, s2)
							g.sb.write_string('!string__lt(')
							g.gen_expr(node.lhs)
							g.sb.write_string(', ')
							g.gen_expr(node.rhs)
							g.sb.write_string(')')
						}
						else {}
					}
				} else if lhs_type.starts_with('Array_') && rhs_type.starts_with('Array_') {
					// Array comparison - need to use array_eq function
					if node.op == .ne {
						g.sb.write_string('!')
					}
					g.sb.write_string('array__eq(')
					g.gen_expr(node.lhs)
					g.sb.write_string(', ')
					g.gen_expr(node.rhs)
					g.sb.write_string(')')
				} else {
					// Non-string comparison - use regular operator
					// Set enum context if LHS is an enum type (for shorthand resolution on RHS)
					lhs_type_cmp := g.infer_type(node.lhs)
					old_match_type_cmp := g.cur_match_type
					if lhs_type_cmp in g.enum_names {
						g.cur_match_type = lhs_type_cmp
					}
					g.sb.write_string('(')
					g.gen_expr(node.lhs)
					op := match node.op {
						.eq { '==' }
						.ne { '!=' }
						.lt { '<' }
						.gt { '>' }
						.le { '<=' }
						.ge { '>=' }
						else { '?' }
					}
					g.sb.write_string(' ${op} ')
					g.gen_expr(node.rhs)
					g.sb.write_string(')')
					g.cur_match_type = old_match_type_cmp
				}
			} else if node.op == .plus {
				// Check for string concatenation
				lhs_type := g.infer_type(node.lhs)
				rhs_type := g.infer_type(node.rhs)
				if lhs_type == 'string' || rhs_type == 'string' {
					// string + string -> string__plus(a, b)
					g.sb.write_string('string__plus(')
					g.gen_expr(node.lhs)
					g.sb.write_string(', ')
					g.gen_expr(node.rhs)
					g.sb.write_string(')')
				} else {
					// Regular addition
					g.sb.write_string('(')
					g.gen_expr(node.lhs)
					g.sb.write_string(' + ')
					g.gen_expr(node.rhs)
					g.sb.write_string(')')
				}
			} else {
				// Other binary operators
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
			mut method_receiver_type := ''
			mut method_type := '' // Resolved type used for fn_params lookup
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
				method_receiver_type = clean_type

				// Handle builtin array methods
				if clean_type.starts_with('Array_') || clean_type == 'array' {
					if name == 'free' {
						// arr.free() -> array__free(&arr)
						g.sb.write_string('array__free(&')
						g.gen_expr(receiver_expr)
						g.sb.write_string(')')
						return
					}
					if name == 'sort' {
						// arr.sort(a.x < b.x) -> qsort call
						// For now, skip sort - it needs comparison function generation
						g.sb.write_string('(void)0 /* sort */')
						return
					}
					// Handle first/last on typed arrays - need to cast from void*
					if name in ['first', 'last'] && clean_type.starts_with('Array_') {
						// Extract element type from Array_T
						elem_type := clean_type[6..] // Remove 'Array_' prefix
						g.sb.write_string('(*(${elem_type}*)array__${name}(')
						g.gen_expr(receiver_expr)
						g.sb.write_string('))')
						return
					}
				}

				// Check if receiver is an interface type - call through vtable
				if clean_type in g.interface_names {
					// Check if this is the matched interface variable in a type match branch
					// If so, call the concrete type's method directly
					// Check both direct use (err.method()) and deref (*err).method()
					mut is_iface_match_var := false
					if g.cur_iface_match_var != '' && g.cur_iface_match_type != '' {
						if receiver_expr is ast.Ident
							&& (receiver_expr as ast.Ident).name == g.cur_iface_match_var {
							is_iface_match_var = true
						} else if receiver_expr is ast.PrefixExpr
							&& (receiver_expr as ast.PrefixExpr).op == .mul {
							inner := (receiver_expr as ast.PrefixExpr).expr
							if inner is ast.Ident
								&& (inner as ast.Ident).name == g.cur_iface_match_var {
								is_iface_match_var = true
							}
						}
					}
					if is_iface_match_var {
						// Call concrete method: ConcreteType__method(*((ConcreteType*)matched_var._object), args)
						g.sb.write_string('${g.cur_iface_match_type}__${name}(')
						// Check if method expects pointer receiver
						mangled := '${g.cur_iface_match_type}__${name}'
						method_wants_ptr := g.mut_receivers[mangled] or { false } || g.ref_receivers[mangled] or {
							false
						}
						if method_wants_ptr {
							g.sb.write_string('((${g.cur_iface_match_type}*)${g.cur_iface_match_var}._object)')
						} else {
							g.sb.write_string('(*((${g.cur_iface_match_type}*)${g.cur_iface_match_var}._object))')
						}
						if node.args.len > 0 {
							g.sb.write_string(', ')
						}
						for i, arg in node.args {
							if i > 0 {
								g.sb.write_string(', ')
							}
							g.gen_expr(arg)
						}
						g.sb.write_string(')')
						return
					}
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
								// Only add & if the inner expression is not already a pointer (&expr)
								if arg.expr is ast.PrefixExpr && arg.expr.op == .amp {
									g.gen_expr(arg.expr)
								} else {
									g.sb.write_string('&')
									g.gen_expr(arg.expr)
								}
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

				// For method calls, first try the direct type, then try type alias resolution
				method_type = clean_type
				mut mangled := '${method_type}__${name}'
				// Only resolve type alias if method doesn't exist on the original type
				if mangled !in g.fn_types {
					if resolved := g.type_aliases[clean_type] {
						method_type = resolved
						mangled = '${method_type}__${name}'
					}
				}
				if (method_type.starts_with('Array_') || method_type.starts_with('FixedArray_'))
					&& mangled !in g.fn_types {
					// Specific method not found, try generic array method
					method_type = 'array'
					mangled = '${method_type}__${name}'
				}
				// If type is 'int' but method doesn't exist, try u8 or rune for common byte/char methods
				if method_type == 'int' && mangled !in g.fn_types {
					// Methods typically defined on u8 (byte)
					if name in ['ascii_str', 'hex', 'repeat', 'is_space', 'is_digit', 'is_letter',
						'is_alnum'] {
						mangled = 'u8__${name}'
					}
					// Methods typically defined on rune
					else if name in ['to_lower', 'to_upper', 'bytes', 'str', 'utf8_len'] {
						mangled = 'rune__${name}'
					}
				}
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
				// Use mangled name if available (for same-module function calls)
				call_name := g.fn_mangled[name] or { name }
				g.sb.write_string('${call_name}(')
				method_type = call_name // For non-method calls, use the function name
			}
			// For flag enum methods (has/all), set context for enum shorthand resolution
			old_match_type := g.cur_match_type
			if method_receiver_type in g.flag_enum_names && name in ['has', 'all'] {
				g.cur_match_type = method_receiver_type
			}
			// Determine the function key for param lookup
			// Use method_type (which may be resolved to 'array') instead of original receiver type
			// For non-method calls, also try the mangled name with module prefix
			fn_key := if is_method {
				'${method_type}__${name}'
			} else {
				name
			}
			// Try unmangled name first, then mangled name for same-module lookups
			expected_params := g.fn_params[fn_key] or {
				// Try with mangled name (for same-module function calls)
				mangled_key := g.fn_mangled[name] or { '' }
				if mangled_key != '' {
					g.fn_params[mangled_key] or { []string{} }
				} else {
					[]string{}
				}
			}
			// Check if there are FieldInit arguments that should be packed into a struct
			// Find the first FieldInit and check if the corresponding param is a struct type
			mut first_field_init_idx := -1
			mut params_struct_type := ''
			for i, arg in node.args {
				if arg is ast.FieldInit {
					first_field_init_idx = i
					// Get param type for this position - FieldInit args map to last struct param
					param_idx := if expected_params.len > 0 { expected_params.len - 1 } else { i }
					if param_idx < expected_params.len {
						pt := expected_params[param_idx]
						// Check if param type is a known struct (has fields) or is marked as params struct
						if pt in g.params_structs || pt in g.struct_fields {
							params_struct_type = pt
						}
					}
					break
				}
			}
			for i, arg in node.args {
				// If we've reached FieldInit args that should be packed into a struct
				if first_field_init_idx >= 0 && i == first_field_init_idx
					&& params_struct_type != '' {
					if i > 0 {
						g.sb.write_string(', ')
					}
					// Start the params struct
					g.sb.write_string('(${params_struct_type}){')
					mut first_field := true
					// Emit all remaining FieldInit arguments
					for j := i; j < node.args.len; j++ {
						if node.args[j] is ast.FieldInit {
							field_init := node.args[j] as ast.FieldInit
							if !first_field {
								g.sb.write_string(', ')
							}
							first_field = false
							g.sb.write_string('.${field_init.name} = ')
							g.gen_expr(field_init.value)
						}
					}
					g.sb.write_string('}')
					break // We've handled all remaining args
				}
				if i > 0 {
					g.sb.write_string(', ')
				}
				// Set enum context if parameter type is an enum (for shorthand resolution)
				param_type := if i < expected_params.len { expected_params[i] } else { '' }
				old_match_type_arg := g.cur_match_type
				if param_type in g.enum_names {
					g.cur_match_type = param_type
				}
				// Check if the argument is a mut parameter (ModifierExpr with mut)
				if arg is ast.ModifierExpr {
					if arg.kind == .key_mut {
						// Only add & if the inner expression is not already a pointer (&expr)
						if arg.expr is ast.PrefixExpr && arg.expr.op == .amp {
							// Already has &, don't add another
							g.gen_expr(arg.expr)
						} else {
							g.sb.write_string('&')
							g.gen_expr(arg.expr)
						}
					} else {
						g.gen_expr(arg)
					}
				} else if arg is ast.FieldInit {
					// Named argument - check if this should be wrapped in a params struct
					if param_type in g.params_structs {
						g.sb.write_string('(${param_type}){.${arg.name} = ')
						g.gen_expr(arg.value)
						g.sb.write_string('}')
					} else {
						g.gen_expr(arg)
					}
				} else {
					// Check if this is a function pointer argument that needs casting
					// e.g., passing typed compare_strings to generic sort expecting void* comparator
					if param_type.starts_with('FnPtr_') {
						// Cast the function pointer to the expected type
						g.sb.write_string('(${param_type})')
						g.gen_expr(arg)
					} else {
						g.gen_expr(arg)
					}
				}
				g.cur_match_type = old_match_type_arg
			}
			// Fill in missing @[params] arguments with default values
			if expected_params.len > node.args.len {
				for i in node.args.len .. expected_params.len {
					param_type := expected_params[i]
					if param_type in g.params_structs {
						// Need comma if: there were explicit args, or this is not the first missing arg,
						// or this is a method call (receiver already written)
						if node.args.len > 0 || i > node.args.len || is_method {
							g.sb.write_string(', ')
						}
						g.sb.write_string('(${param_type}){}')
					}
				}
			}
			g.cur_match_type = old_match_type
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
					// Get unique type ID for this concrete type
					type_id := g.get_or_assign_type_id(concrete_type)
					// Generate interface struct initialization with vtable
					g.sb.write_string('({ ')
					// Allocate heap memory for the object
					g.sb.write_string('${concrete_type}* _iface_obj = (${concrete_type}*)malloc(sizeof(${concrete_type})); ')
					g.sb.write_string('*_iface_obj = ')
					g.gen_expr(node.expr)
					g.sb.write_string('; ')
					// Create interface struct with function pointers
					g.sb.write_string('(${name}){._object = _iface_obj, ._type_id = ${type_id}')
					// Add type_name wrapper
					g.sb.write_string(', .type_name = ${name}_${concrete_type}_type_name_wrapper')
					// Add function pointers for each interface method
					if methods := g.interface_meths[name] {
						for meth in methods {
							g.sb.write_string(', .${meth} = ${name}_${concrete_type}_${meth}_wrapper')
						}
					}
					g.sb.write_string('}; })')
					return
				}
				// Check if this is a struct type cast: StructName(expr)
				// If name is not a known function, treat it as a type cast
				if name !in g.fn_types {
					// Mangle type name if it's from the current module
					mangled_type := if g.cur_module != '' && g.cur_module != 'main'
						&& g.cur_module != 'builtin' {
						if types_in_module := g.module_types[g.cur_module] {
							if name in types_in_module {
								'${g.cur_module}__${name}'
							} else {
								name
							}
						} else {
							name
						}
					} else {
						name
					}
					g.sb.write_string('((${mangled_type})(')
					g.gen_expr(node.expr)
					g.sb.write_string('))')
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

					// Handle builtin array methods
					if clean_type.starts_with('Array_') || clean_type == 'array' {
						if name == 'free' {
							// arr.free() -> array__free(&arr)
							g.sb.write_string('array__free(&')
							g.gen_expr(receiver_expr)
							g.sb.write_string(')')
							return
						}
						if name == 'sort' {
							// arr.sort(a.x < b.x) -> qsort call
							// For now, skip sort - it needs comparison function generation
							g.sb.write_string('(void)0 /* sort */')
							return
						}
					}

					// Handle flag enum methods (set, clear, has, toggle)
					// These methods operate on integer flags using bitwise operations
					if name in ['set', 'clear', 'toggle'] {
						// Set the enum type context for shorthand resolution
						// The receiver type (e.g., ArrayFlags) is used for .shorthand expansion
						old_match_type := g.cur_match_type
						if clean_type in g.enum_names || clean_type in g.flag_enum_names {
							g.cur_match_type = clean_type
						}
						if name == 'set' {
							// x.set(.flag) -> x |= flag
							g.gen_expr(receiver_expr)
							g.sb.write_string(' |= ')
							g.gen_expr(node.expr)
						} else if name == 'clear' {
							// x.clear(.flag) -> x &= ~flag
							g.gen_expr(receiver_expr)
							g.sb.write_string(' &= ~(')
							g.gen_expr(node.expr)
							g.sb.write_string(')')
						} else { // toggle
							// x.toggle(.flag) -> x ^= flag
							g.gen_expr(receiver_expr)
							g.sb.write_string(' ^= ')
							g.gen_expr(node.expr)
						}
						g.cur_match_type = old_match_type
						return
					}

					mut method_type := clean_type
					mut mangled := '${method_type}__${name}'
					// For Array_ types, first try specific type method, then fall back to generic 'array'
					if (clean_type.starts_with('Array_') || clean_type.starts_with('FixedArray_'))
						&& mangled !in g.fn_types {
						method_type = 'array'
						mangled = '${method_type}__${name}'
					}
					// Method expects pointer if receiver is mut OR reference (&Type)
					// Flag enum methods (has/all) also expect pointer receivers
					is_flag_enum_method := clean_type in g.flag_enum_names && name in ['has', 'all']
					method_wants_ptr := g.mut_receivers[mangled] or { false } || g.ref_receivers[mangled] or {
						false
					} || is_flag_enum_method

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
					// Check if the argument should be cast to a function pointer type
					// Look up the expected parameter type for this method
					expected_params := g.fn_params[mangled] or { []string{} }
					param_type := if expected_params.len > 0 { expected_params[0] } else { '' }
					// Set enum context for shorthand resolution
					old_match_type := g.cur_match_type
					if param_type in g.enum_names {
						g.cur_match_type = param_type
					} else if clean_type in g.flag_enum_names && name in ['has', 'all'] {
						g.cur_match_type = clean_type
					}
					// Generate the single argument
					if node.expr is ast.ModifierExpr {
						if node.expr.kind == .key_mut {
							// Only add & if the inner expression is not already a pointer (&expr)
							if node.expr.expr is ast.PrefixExpr && node.expr.expr.op == .amp {
								g.gen_expr(node.expr.expr)
							} else {
								g.sb.write_string('&')
								g.gen_expr(node.expr.expr)
							}
						} else {
							g.gen_expr(node.expr)
						}
					} else if param_type.starts_with('FnPtr_') {
						// Cast function pointer to expected type
						g.sb.write_string('(${param_type})')
						g.gen_expr(node.expr)
					} else {
						g.gen_expr(node.expr)
					}
					g.cur_match_type = old_match_type
					g.sb.write_string(')')
					return
				}
			}
			// Use mangled name if available (for cross-module function calls)
			call_name := g.fn_mangled[name] or { name }
			g.sb.write_string('${call_name}(')
			// Check if the argument is a mut parameter (ModifierExpr with mut)
			if node.expr is ast.ModifierExpr {
				if node.expr.kind == .key_mut {
					// Only add & if the inner expression is not already a pointer (&expr)
					if node.expr.expr is ast.PrefixExpr && node.expr.expr.op == .amp {
						g.gen_expr(node.expr.expr)
					} else {
						g.sb.write_string('&')
						g.gen_expr(node.expr.expr)
					}
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
					mt := node.typ as ast.MapType
					key_type := sanitize_type_for_mangling(g.expr_type_to_c(mt.key_type))
					value_type := sanitize_type_for_mangling(g.expr_type_to_c(mt.value_type))
					map_type := 'Map_${key_type}_${value_type}'
					g.sb.write_string('__new_${map_type}()')
					return
				}
			}
			// Get the type name properly
			typ_name := g.expr_type_to_c(node.typ)
			// Get struct field types for enum context resolution
			mut field_types := map[string]string{}
			if typ_name in g.struct_fields {
				field_types = g.struct_fields[typ_name].clone()
			}
			g.sb.write_string('(${typ_name}){')
			for i, field in node.fields {
				if i > 0 {
					g.sb.write_string(', ')
				}
				g.sb.write_string('.${field.name} = ')
				// Set enum context if field type is an enum or flag enum
				field_type := field_types[field.name] or { '' }
				old_match_type := g.cur_match_type
				if field_type in g.enum_names || field_type in g.flag_enum_names {
					g.cur_match_type = field_type
				}
				g.gen_expr(field.value)
				g.cur_match_type = old_match_type
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
				if g.cur_match_type in g.enum_names || g.cur_match_type in g.flag_enum_names {
					g.sb.write_string('${g.cur_match_type}__${node.rhs.name}')
					return
				}
				// Fallback: just output the field name (might not be valid C)
				g.sb.write_string('${node.rhs.name}')
				return
			}
			// Check if we need to use -> for pointers
			lhs_type := g.infer_type(node.lhs)
			// Handle variadic argument access (VArg_* types)
			if lhs_type.starts_with('VArg_') && node.rhs.name == 'len' {
				// pt.len -> pt.len (direct field access on VArg struct)
				g.gen_expr(node.lhs)
				g.sb.write_string('.len')
				return
			}
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
				// Map access: __<map_type>_get(&m, key)
				g.sb.write_string('__${lhs_type}_get(&')
				g.gen_expr(node.lhs)
				g.sb.write_string(', ')
				g.gen_expr(node.expr)
				g.sb.write_string(')')
			} else if lhs_type == 'string' {
				// String character access: s.str[index]
				g.gen_expr(node.lhs)
				g.sb.write_string('.str[')
				g.gen_expr(node.expr)
				g.sb.write_string(']')
			} else if lhs_type.starts_with('Array_') || lhs_type == 'array' {
				// For Array types, access via ((elem_type*)arr.data)[index]
				// Handle pointer types (mutable parameters): arr->data vs arr.data
				is_ptr := lhs_type.ends_with('*')
				clean_type := if is_ptr { lhs_type[..lhs_type.len - 1] } else { lhs_type }
				// Convert sanitized type back to valid C type for the cast
				// For generic 'array' type, default to string (common case for split functions)
				elem_type := if clean_type == 'array' {
					'string'
				} else {
					unsanitize_type_for_c(clean_type['Array_'.len..])
				}
				accessor := if is_ptr { '->' } else { '.' }
				g.sb.write_string('((${elem_type}*)')
				g.gen_expr(node.lhs)
				g.sb.write_string('${accessor}data)[')
				g.gen_expr(node.expr)
				g.sb.write_string(']')
			} else if lhs_type.starts_with('VArg_') {
				// Variadic argument access: pt[i] -> pt.data[i]
				g.gen_expr(node.lhs)
				g.sb.write_string('.data[')
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
			if node.op == .not {
				// Error propagation: expr! - unwrap the result value
				// For now, just generate the expression (proper error handling TODO)
				g.gen_expr(node.expr)
			} else if node.op == .question {
				// Optional check: expr? - returns none on error
				// For now, just generate the expression
				g.gen_expr(node.expr)
			} else {
				g.gen_expr(node.expr)
				if node.op == .inc {
					g.sb.write_string('++')
				} else if node.op == .dec {
					g.sb.write_string('--')
				}
			}
		}
		ast.ModifierExpr {
			// Handle mut, shared, etc.
			g.gen_expr(node.expr)
		}
		ast.ArrayInitExpr {
			// Generate array using new_array_from_c_array builtin
			arr_len := node.exprs.len
			mut c_elem_type := 'int' // Actual C type for sizeof and array literals
			if node.typ !is ast.EmptyExpr {
				// node.typ is ArrayType{elem_type: T} for []T{} syntax
				// Get the actual C type for sizeof and array literals
				match node.typ {
					ast.Type {
						if node.typ is ast.ArrayType {
							c_elem_type = g.expr_type_to_c(node.typ.elem_type)
						} else {
							c_elem_type = g.expr_type_to_c(node.typ)
						}
					}
					else {
						c_elem_type = g.expr_type_to_c(node.typ)
					}
				}
			} else if arr_len > 0 {
				c_elem_type = g.infer_type(node.exprs[0])
			}
			// new_array_from_c_array(len, len, sizeof(elem), (elem_type[len]){values})
			g.sb.write_string('new_array_from_c_array(${arr_len}, ${arr_len}, sizeof(${c_elem_type}), (${c_elem_type}[${arr_len}]){')
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
			map_type := g.infer_type(node)
			g.sb.write_string('__new_${map_type}()')
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
			// Unsafe block - check if it contains return statements
			if node.stmts.len == 0 {
				g.sb.write_string('0')
			} else {
				// If unsafe block contains return, generate as regular statements
				// because compound expressions don't work with return
				has_return := g.stmts_has_return(node.stmts)
				if has_return {
					// Generate as regular statements (not compound expression)
					for stmt in node.stmts {
						g.gen_stmt(stmt)
					}
				} else {
					// Generate as GCC compound expression
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
		}
		ast.EmptyExpr {
			// Empty expression - output nothing or 0
			g.sb.write_string('0')
		}
		ast.CastExpr {
			// Explicit type cast: int(expr), u64(expr), etc.
			mut type_name := ''
			if node.typ is ast.Ident {
				type_name = node.typ.name
			} else if node.typ is ast.Type {
				type_name = g.expr_type_to_c(node.typ)
			} else {
				type_name = g.expr_type_to_c(node.typ)
			}
			g.sb.write_string('((${type_name})(')
			g.gen_expr(node.expr)
			g.sb.write_string('))')
		}
		ast.FieldInit {
			// Named argument or struct field init when used as expression
			// Just emit the value
			g.gen_expr(node.value)
		}
		ast.KeywordOperator {
			// Handle keyword operators: sizeof, typeof, isreftype, offsetof, dump, likely, unlikely
			match node.op {
				.key_sizeof {
					g.sb.write_string('sizeof(')
					if node.exprs.len > 0 {
						g.sb.write_string(g.expr_type_to_c(node.exprs[0]))
					}
					g.sb.write_string(')')
				}
				.key_typeof {
					// In C, use the type name as a string
					g.sb.write_string('"')
					if node.exprs.len > 0 {
						g.sb.write_string(g.expr_type_to_c(node.exprs[0]))
					}
					g.sb.write_string('"')
				}
				.key_offsetof {
					// offsetof(type, field)
					g.sb.write_string('offsetof(')
					if node.exprs.len > 0 {
						g.sb.write_string(g.expr_type_to_c(node.exprs[0]))
					}
					if node.exprs.len > 1 {
						g.sb.write_string(', ')
						field_expr := node.exprs[1]
						if field_expr is ast.Ident {
							g.sb.write_string(field_expr.name)
						} else {
							g.gen_expr(field_expr)
						}
					}
					g.sb.write_string(')')
				}
				.key_isreftype {
					// isreftype - check if a type is a reference type
					// For now, generate 0 (false) as default
					g.sb.write_string('0')
				}
				.key_likely {
					g.sb.write_string('__builtin_expect((')
					if node.exprs.len > 0 {
						g.gen_expr(node.exprs[0])
					}
					g.sb.write_string('), 1)')
				}
				.key_unlikely {
					g.sb.write_string('__builtin_expect((')
					if node.exprs.len > 0 {
						g.gen_expr(node.exprs[0])
					}
					g.sb.write_string('), 0)')
				}
				.key_dump {
					// dump(expr) - for debugging, just generate the expression
					if node.exprs.len > 0 {
						g.gen_expr(node.exprs[0])
					}
				}
				else {
					g.sb.write_string('/* KeywordOp: ${node.op} */')
				}
			}
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
		// Check if this is an expression-context $if (single ExprStmt in branch)
		// vs a statement-context $if (multiple statements)
		if g.is_comptime_if_expr_context(node.expr) {
			g.gen_comptime_if_expr(node.expr)
		} else {
			g.gen_comptime_if(node.expr)
		}
	} else {
		// For other comptime expressions, just emit them
		g.gen_expr(node.expr)
	}
}

// is_comptime_if_expr_context checks if a $if should be treated as an expression (single ExprStmt in branch)
// Both then and else branches must have single ExprStmt for this to return true
fn (g Gen) is_comptime_if_expr_context(node ast.IfExpr) bool {
	// Check then branch - must have exactly 1 ExprStmt
	then_is_expr := node.stmts.len == 1 && node.stmts[0] is ast.ExprStmt

	// Check else branch if present
	else_e := node.else_expr
	if else_e is ast.IfExpr {
		if else_e.cond is ast.EmptyExpr {
			// Plain $else - must also have exactly 1 ExprStmt
			else_is_expr := else_e.stmts.len == 1 && else_e.stmts[0] is ast.ExprStmt
			// Both branches must be expressions
			return then_is_expr && else_is_expr
		} else {
			// $else $if chain - recurse to check that branch
			return then_is_expr && g.is_comptime_if_expr_context(else_e)
		}
	}
	// No else branch - then branch alone determines
	return then_is_expr
}

// gen_comptime_if_expr handles $if as an expression (returns a value, not statements)
fn (mut g Gen) gen_comptime_if_expr(node ast.IfExpr) {
	cond_result := g.eval_comptime_cond(node.cond)

	if cond_result {
		// Condition is true - emit the expression from then branch
		if node.stmts.len == 1 {
			stmt := node.stmts[0]
			if stmt is ast.ExprStmt {
				g.gen_expr(stmt.expr)
			}
		}
	} else {
		// Condition is false - emit expression from else branch
		if node.else_expr !is ast.EmptyExpr {
			else_e := node.else_expr
			if else_e is ast.IfExpr {
				if else_e.cond is ast.EmptyExpr {
					// Plain $else block
					if else_e.stmts.len == 1 {
						stmt := else_e.stmts[0]
						if stmt is ast.ExprStmt {
							g.gen_expr(stmt.expr)
						}
					}
				} else {
					// $else $if - recursive
					g.gen_comptime_if_expr(else_e)
				}
			}
		}
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

// gen_assign_if_expr hoists assignment into if-expression branches
// For: lhs = if (cond) { stmts; val } else { val }
// Generates: if (cond) { stmts; lhs = val; } else { lhs = val; }
fn (mut g Gen) gen_assign_if_expr(lhs ast.Expr, if_expr ast.IfExpr, op token.Token) {
	op_str := match op {
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

	g.sb.write_string('if (')
	g.gen_expr(if_expr.cond)
	g.sb.writeln(') {')
	g.indent++
	// Generate statements, last one is the assignment value
	for i, stmt in if_expr.stmts {
		if i == if_expr.stmts.len - 1 {
			if stmt is ast.ExprStmt {
				g.write_indent()
				g.gen_expr(lhs)
				g.sb.write_string(' ${op_str} ')
				g.gen_expr(stmt.expr)
				g.sb.writeln(';')
			} else {
				g.gen_stmt(stmt)
			}
		} else {
			g.gen_stmt(stmt)
		}
	}
	g.indent--
	g.write_indent()
	g.sb.write_string('}')

	// Handle else branch
	if if_expr.else_expr is ast.IfExpr {
		if if_expr.else_expr.cond is ast.EmptyExpr {
			// Pure else block
			g.sb.writeln(' else {')
			g.indent++
			for i, stmt in if_expr.else_expr.stmts {
				if i == if_expr.else_expr.stmts.len - 1 {
					if stmt is ast.ExprStmt {
						g.write_indent()
						g.gen_expr(lhs)
						g.sb.write_string(' ${op_str} ')
						g.gen_expr(stmt.expr)
						g.sb.writeln(';')
					} else {
						g.gen_stmt(stmt)
					}
				} else {
					g.gen_stmt(stmt)
				}
			}
			g.indent--
			g.write_indent()
			g.sb.writeln('}')
		} else {
			// Else-if chain - recurse
			g.sb.write_string(' else ')
			g.gen_assign_if_expr(lhs, if_expr.else_expr, op)
		}
	}
}

fn (mut g Gen) write_indent() {
	for _ in 0 .. g.indent {
		g.sb.write_string('\t')
	}
}

// has_range_condition checks if any branch condition in a match expression is a RangeExpr
// or contains non-constant expressions that can't be used in C switch case labels
fn (g Gen) has_range_condition(match_expr ast.MatchExpr) bool {
	for branch in match_expr.branches {
		for c in branch.cond {
			if c is ast.RangeExpr {
				return true
			}
			// Field access like param.block_start is not a compile-time constant in C
			if c is ast.SelectorExpr {
				return true
			}
			// Variable references are not compile-time constants
			if c is ast.Ident && c.name !in g.enum_names {
				// Check if it's a local variable (not an enum value)
				if c.name in g.var_types {
					return true
				}
			}
		}
	}
	return false
}

// gen_assign_match_expr hoists assignment into match expression branches
// For: lhs = match expr { val1 { res1 } val2 { res2 } else { res3 } }
// Generates: switch (expr) { case val1: lhs = res1; break; ... }
fn (mut g Gen) gen_assign_match_expr(lhs ast.Expr, match_expr ast.MatchExpr, op token.Token, is_decl bool) {
	op_str := match op {
		.assign, .decl_assign { '=' }
		.plus_assign { '+=' }
		.minus_assign { '-=' }
		.mul_assign { '*=' }
		.div_assign { '/=' }
		.mod_assign { '%=' }
		else { '=' }
	}

	// Get variable name and type for declaration
	mut var_name := ''
	if lhs is ast.Ident {
		var_name = lhs.name
	} else if lhs is ast.ModifierExpr && lhs.expr is ast.Ident {
		var_name = lhs.expr.name
	}

	// For declarations, emit the variable declaration first
	if is_decl && var_name != '' {
		typ := g.infer_type(match_expr)
		g.var_types[var_name] = typ
		c_typ := type_for_c_decl(typ)
		g.write_indent()
		g.sb.writeln('${c_typ} ${var_name};')
	}

	// Check if we have range conditions - need to use if-else instead of switch
	if g.has_range_condition(match_expr) {
		// Generate if-else chain for match with ranges
		// Save match expression to temp var to avoid multiple evaluation
		g.write_indent()
		g.sb.write_string('{ ')
		match_type := g.infer_type(match_expr.expr)
		c_match_type := type_for_c_decl(match_type)
		g.sb.write_string('${c_match_type} __match_val = ')
		g.gen_expr(match_expr.expr)
		g.sb.writeln(';')

		mut first := true
		for branch in match_expr.branches {
			if branch.cond.len == 0 {
				// else branch
				g.write_indent()
				if first {
					g.sb.writeln('{')
				} else {
					g.sb.writeln(' else {')
				}
			} else {
				g.write_indent()
				if first {
					g.sb.write_string('if (')
					first = false
				} else {
					g.sb.write_string(' else if (')
				}
				// Generate conditions with proper handling for ranges
				for ci, c in branch.cond {
					if ci > 0 {
						g.sb.write_string(' || ')
					}
					if c is ast.RangeExpr {
						// Range condition: __match_val >= start && __match_val <= end
						g.sb.write_string('(__match_val >= ')
						g.gen_expr(c.start)
						g.sb.write_string(' && __match_val <= ')
						g.gen_expr(c.end)
						g.sb.write_string(')')
					} else {
						g.sb.write_string('(__match_val == ')
						g.gen_expr(c)
						g.sb.write_string(')')
					}
				}
				g.sb.writeln(') {')
			}

			g.indent++
			// Generate statements, hoisting the last value as an assignment
			for i, stmt in branch.stmts {
				if i == branch.stmts.len - 1 {
					if stmt is ast.ExprStmt {
						g.write_indent()
						g.gen_expr(lhs)
						g.sb.write_string(' ${op_str} ')
						g.gen_expr(stmt.expr)
						g.sb.writeln(';')
					} else {
						g.gen_stmt(stmt)
					}
				} else {
					g.gen_stmt(stmt)
				}
			}
			g.indent--
			g.write_indent()
			g.sb.write_string('}')
		}
		g.sb.writeln('')
		g.write_indent()
		g.sb.writeln('}')
		return
	}

	// Generate switch statement (no range conditions)
	g.write_indent()
	g.sb.write_string('switch (')
	g.gen_expr(match_expr.expr)
	g.sb.writeln(') {')

	for branch in match_expr.branches {
		if branch.cond.len == 0 {
			g.write_indent()
			g.sb.writeln('default: {')
		} else {
			for i, c in branch.cond {
				g.write_indent()
				g.sb.write_string('case ')
				g.gen_expr(c)
				g.sb.write_string(':')
				if i == branch.cond.len - 1 {
					g.sb.writeln(' {')
				} else {
					g.sb.writeln('')
				}
			}
		}
		g.indent++

		// Generate statements, hoisting the last value as an assignment
		for i, stmt in branch.stmts {
			if i == branch.stmts.len - 1 {
				// Last statement - try to extract value and assign
				if stmt is ast.ExprStmt {
					g.write_indent()
					g.gen_expr(lhs)
					g.sb.write_string(' ${op_str} ')
					g.gen_expr(stmt.expr)
					g.sb.writeln(';')
				} else {
					g.gen_stmt(stmt)
				}
			} else {
				g.gen_stmt(stmt)
			}
		}

		g.write_indent()
		g.sb.writeln('break;')
		g.indent--
		g.write_indent()
		g.sb.writeln('}')
	}

	g.write_indent()
	g.sb.writeln('}')
}

// Check if an expression is a call to error() or error_with_code()
// Used to detect error returns in functions where result type was simplified
fn (g Gen) is_error_return(expr ast.Expr) bool {
	if expr is ast.CallExpr {
		if expr.lhs is ast.Ident {
			return expr.lhs.name in ['error', 'error_with_code']
		}
	}
	if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.Ident {
			return expr.lhs.name in ['error', 'error_with_code']
		}
	}
	return false
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

	// Handle iterator types (e.g. RunesIterator) - use iterator protocol
	if arr_type.ends_with('Iterator') {
		g.gen_for_in_iterator(node, for_in, arr_type, value_name)
		return
	}

	// Determine element type and data accessor
	mut elem_type := 'int'
	mut data_accessor := '.data'
	if arr_type == 'string' {
		// For strings, iterate over bytes using .str
		elem_type = 'u8'
		data_accessor = '.str'
	} else if arr_type.starts_with('Array_') {
		elem_type = unsanitize_type_for_c(arr_type['Array_'.len..])
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
	g.sb.writeln('${data_accessor})[${idx_var}];')

	g.gen_stmts(node.stmts)
	g.indent--
	g.write_indent()
	g.sb.writeln('}')
}

// Generate for-in loop over iterator: `for elem in iter { ... }`
// Uses iterator protocol: call next() until it returns 0
fn (mut g Gen) gen_for_in_iterator(node ast.ForStmt, for_in ast.ForInStmt, iter_type string, value_name string) {
	// Determine the element type (e.g. RunesIterator -> rune)
	elem_type := if iter_type == 'RunesIterator' {
		'rune'
	} else {
		'int'
	}

	// Register variable type
	g.var_types[value_name] = elem_type

	// Generate: { IterType _iter = expr; ElemType value; while ((value = IterType__next(&_iter)) != 0) { ... } }
	g.write_indent()
	g.sb.writeln('{')
	g.indent++

	// Declare iterator variable
	g.write_indent()
	g.sb.write_string('${iter_type} _iter_${value_name} = ')
	g.gen_expr(for_in.expr)
	g.sb.writeln(';')

	// Declare value variable
	g.write_indent()
	g.sb.writeln('${elem_type} ${value_name};')

	// Generate while loop using iterator next()
	g.write_indent()
	g.sb.writeln('while ((${value_name} = ${iter_type}__next(&_iter_${value_name})) != 0) {')
	g.indent++

	g.gen_stmts(node.stmts)

	g.indent--
	g.write_indent()
	g.sb.writeln('}')

	g.indent--
	g.write_indent()
	g.sb.writeln('}')
}

// Generate array slice expression: arr[start..end]
fn (mut g Gen) gen_slice_expr(base ast.Expr, range_expr ast.RangeExpr) {
	// Get the base array type
	lhs_type := g.infer_type(base)

	if lhs_type == 'string' {
		// For string slicing: string__substr(s, start, end)
		g.sb.write_string('string__substr(')
		g.gen_expr(base)
		g.sb.write_string(', ')
		g.gen_expr(range_expr.start)
		g.sb.write_string(', ')
		if range_expr.end is ast.EmptyExpr {
			// s[start..] - slice to end
			g.gen_expr(base)
			g.sb.write_string('.len')
		} else {
			g.gen_expr(range_expr.end)
		}
		g.sb.write_string(')')
	} else if lhs_type.starts_with('Array_') {
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
		g.sb.write_string('array _slice = new_array_from_c_array(_len, _len, sizeof(${elem_type}), ')
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

// gen_return_match_expr handles match expressions in return statements by generating if-else chains
fn (mut g Gen) gen_return_match_expr(node ast.MatchExpr) {
	// Set match type context for enum shorthand (.value syntax)
	match_type := g.infer_type(node.expr)
	old_match_type := g.cur_match_type
	g.cur_match_type = match_type
	defer {
		g.cur_match_type = old_match_type
	}

	// Check if this is an interface type match
	is_interface_match := match_type in g.interface_names

	// Track interface match variable
	old_iface_match_var := g.cur_iface_match_var
	old_iface_match_type := g.cur_iface_match_type
	if is_interface_match {
		g.cur_iface_match_var = if node.expr is ast.Ident { node.expr.name } else { '' }
	}
	defer {
		g.cur_iface_match_var = old_iface_match_var
		g.cur_iface_match_type = old_iface_match_type
	}

	// For interface matches, save the type_id to a local variable
	if is_interface_match {
		g.write_indent()
		g.sb.write_string('{ int __type_id = (')
		g.gen_expr(node.expr)
		g.sb.writeln(')._type_id;')
		g.indent++
	}

	// Generate if-else chain for each branch
	for i, branch in node.branches {
		if branch.cond.len == 0 {
			// else branch - close previous if and start else
			g.cur_iface_match_type = '' // No concrete type in else branch
			g.write_indent()
			if i > 0 {
				g.sb.writeln('} else {')
			} else {
				g.sb.writeln('{')
			}
			g.indent++
			// Generate statements, last one should be the return value
			if branch.stmts.len > 0 {
				for j, stmt in branch.stmts {
					if j == branch.stmts.len - 1 {
						// Last statement is the return value
						if stmt is ast.ExprStmt {
							g.write_indent()
							g.sb.write_string('return ')
							g.gen_expr(stmt.expr)
							g.sb.writeln(';')
						} else {
							g.gen_stmt(stmt)
						}
					} else {
						g.gen_stmt(stmt)
					}
				}
			}
			g.indent--
			g.write_indent()
			g.sb.writeln('}')
		} else {
			// Conditional branch
			g.write_indent()
			if i > 0 {
				g.sb.write_string('} else ')
			}
			g.sb.write_string('if (')
			// Generate condition: match expr == cond, or for match true, just use the condition
			if node.expr is ast.BasicLiteral && node.expr.kind in [.key_true, .key_false] {
				// match true { cond1 { ... } } -> if (cond1)
				// Multiple conditions use OR
				for ci, c in branch.cond {
					if ci > 0 {
						g.sb.write_string(' || ')
					}
					g.gen_expr(c)
				}
			} else if is_interface_match {
				// Interface type match: compare type_id
				// Track first concrete type for this branch
				mut concrete_type := ''
				for ci, c in branch.cond {
					if ci > 0 {
						g.sb.write_string(' || ')
					}
					// Get type name from condition
					type_name := if c is ast.Ident {
						c.name
					} else {
						'unknown'
					}
					if ci == 0 {
						concrete_type = type_name
					}
					type_id := g.get_or_assign_type_id(type_name)
					g.sb.write_string('__type_id == ${type_id}')
				}
				g.cur_iface_match_type = concrete_type
			} else {
				// match expr { val1 { ... } } -> if (expr == val1)
				for ci, c in branch.cond {
					if ci > 0 {
						g.sb.write_string(' || ')
					}
					g.sb.write_string('(')
					g.gen_expr(node.expr)
					g.sb.write_string(' == ')
					g.gen_expr(c)
					g.sb.write_string(')')
				}
			}
			g.sb.writeln(') {')
			g.indent++
			// Generate statements, last one should be the return value
			if branch.stmts.len > 0 {
				for j, stmt in branch.stmts {
					if j == branch.stmts.len - 1 {
						// Last statement is the return value
						if stmt is ast.ExprStmt {
							g.write_indent()
							g.sb.write_string('return ')
							g.gen_expr(stmt.expr)
							g.sb.writeln(';')
						} else {
							g.gen_stmt(stmt)
						}
					} else {
						g.gen_stmt(stmt)
					}
				}
			}
			g.indent--
		}
	}
	// Close the last if block if no else branch
	if node.branches.len > 0 && node.branches[node.branches.len - 1].cond.len > 0 {
		g.write_indent()
		g.sb.writeln('}')
	}

	// Close the interface match block
	if is_interface_match {
		g.indent--
		g.write_indent()
		g.sb.writeln('}')
	}
}

// emit_deferred_stmts emits all deferred statements in reverse order
fn (mut g Gen) emit_deferred_stmts() {
	// Execute deferred statements in LIFO order (last defer first)
	for i := g.defer_stmts.len - 1; i >= 0; i-- {
		g.gen_stmts(g.defer_stmts[i])
	}
}

// stmts_has_return checks if any statement in the list is a return statement
fn (g &Gen) stmts_has_return(stmts []ast.Stmt) bool {
	for stmt in stmts {
		if stmt is ast.ReturnStmt {
			return true
		}
		// Check nested blocks
		if stmt is ast.BlockStmt {
			if g.stmts_has_return(stmt.stmts) {
				return true
			}
		}
		// Check for statements
		if stmt is ast.ForStmt {
			if g.stmts_has_return(stmt.stmts) {
				return true
			}
		}
		// Check expressions that might contain statements (if/match/etc)
		if stmt is ast.ExprStmt {
			if g.expr_has_return(stmt.expr) {
				return true
			}
		}
	}
	return false
}

// expr_has_return checks if an expression contains return statements
fn (g &Gen) expr_has_return(expr ast.Expr) bool {
	if expr is ast.IfExpr {
		// Check if/else branches
		if g.stmts_has_return(expr.stmts) {
			return true
		}
		if expr.else_expr !is ast.EmptyExpr && g.expr_has_return(expr.else_expr) {
			return true
		}
	} else if expr is ast.MatchExpr {
		for branch in expr.branches {
			if g.stmts_has_return(branch.stmts) {
				return true
			}
		}
	} else if expr is ast.UnsafeExpr {
		if g.stmts_has_return(expr.stmts) {
			return true
		}
	}
	return false
}

// gen_multi_return_assign handles multi-return assignments like: a, b := func() or a, b = func()
fn (mut g Gen) gen_multi_return_assign(node ast.AssignStmt) {
	rhs := node.rhs[0]
	is_decl := node.op == .decl_assign

	// Infer tuple type from RHS (typically a function call)
	tuple_type := g.infer_type(rhs)
	// Get element types from tuple_types map if available
	elem_types := g.tuple_types[tuple_type] or { []string{len: node.lhs.len, init: 'int'} }

	// Get a unique temp variable name
	tmp_name := '__tmp_${g.tmp_counter}'
	g.tmp_counter++

	// Generate: TupleN_type __tmp = func();
	g.write_indent()
	g.sb.write_string('${tuple_type} ${tmp_name} = ')
	g.gen_expr(rhs)
	g.sb.writeln(';')

	// Extract each field
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

		elem_type := if i < elem_types.len { elem_types[i] } else { 'int' }
		g.write_indent()
		if is_decl {
			// Declaration: Type a = __tmp.f0;
			g.var_types[name] = elem_type
			g.sb.writeln('${elem_type} ${name} = ${tmp_name}.f${i};')
		} else {
			// Assignment: a = __tmp.f0;
			g.sb.writeln('${name} = ${tmp_name}.f${i};')
		}
	}
}
