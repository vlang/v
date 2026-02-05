// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.pref
import v2.token
import v2.types
import strings

// MapTypeInfo holds information needed to generate map typedefs and stubs
struct MapTypeInfo {
	key_c_type   string // C type for key (e.g., "string", "int")
	value_c_type string // C type for value (e.g., "int", "array")
}

pub struct Gen {
	files []ast.File
	// Type checker environment with populated scopes
	env  &types.Environment = unsafe { nil }
	pref &pref.Preferences  = unsafe { nil }
mut:
	sb                             strings.Builder
	indent                         int
	tmp_counter                    int // Counter for unique temp variable names
	fn_types                       map[string]string
	fn_ret_counts                  map[string]int // Track number of return values for multi-return functions
	var_types                      map[string]string
	mut_params                     map[string]bool                // Track mut parameters (need dereference on assignment)
	mut_receivers                  map[string]bool                // Track which methods have mutable receivers
	ref_receivers                  map[string]bool                // Track which methods have reference receivers (&Type)
	defer_stmts                    [][]ast.Stmt                   // Deferred statements for current function
	enum_names                     map[string]bool                // Track enum type names
	flag_enum_names                map[string]bool                // Track flag enum type names
	interface_names                map[string]bool                // Track interface type names
	interface_meths                map[string][]string            // Interface name -> method names
	interface_meth_params          map[string]map[string][]string // Interface name -> (method name -> param types)
	type_methods                   map[string][]string            // Type name -> method names (for vtable generation)
	struct_fields                  map[string]map[string]string   // Struct name -> (field name -> field type)
	known_types                    map[string]bool                // All known mangled type names (structs, type aliases, interfaces)
	module_types                   map[string]map[string]bool     // Module name -> set of type names defined in that module
	cur_match_type                 string                         // Current match expression type for enum shorthand
	cur_module                     string                         // Current module name for namespacing
	file_modules                   map[string]string              // File path -> module name mapping
	module_names                   map[string]bool                // All module names (for detecting module.ident access)
	import_aliases                 map[string]string              // Import alias -> actual module name (e.g., gen_v -> v)
	params_structs                 map[string]bool                // Struct types with @[params] attribute
	fn_params                      map[string][]string            // Function name -> list of parameter types
	seen_consts                    map[string]bool                // Track generated constants for reference in other consts
	const_types                    map[string]string              // Track constant types for type inference
	const_values                   map[string]string              // Track integer constant values for fixed-size arrays
	const_mangled                  map[string]string              // Map unmangled const name -> mangled name for cross-module refs
	emitted_consts                 map[string]bool                // Track which constants have been emitted (for ordering check)
	fn_mangled                     map[string]string              // Map unmangled fn name -> mangled name for same-module calls
	tuple_types                    map[string][]string            // Tuple type name -> list of element types
	result_types                   map[string]string              // Result type name -> base type (e.g., "_result_int" -> "int")
	option_types                   map[string]string              // Option type name -> base type (e.g., "_option_int" -> "int")
	emitted_result_types           map[string]bool                // Track which Result types have been emitted
	emitted_option_types           map[string]bool                // Track which Option types have been emitted
	cur_fn_ret_type                string                         // Current function's return type (for proper return statement generation)
	cur_fn_returns_result          bool                  // Whether current function returns a Result type (!T)
	cur_fn_result_base_type        string                // Base type of Result (e.g., "int" for !int)
	cur_fn_returns_option          bool                  // Whether current function returns an Option type (?T)
	cur_fn_option_base_type        string                // Base type of Option (e.g., "int" for ?int)
	cur_fn_name                    string                // Current function name (for @FN and @METHOD comptime values)
	global_var_types               map[string]string     // Global variable name -> type (persists across functions)
	fn_ptr_typedefs                map[string]ast.FnType // Synthetic fn pointer typedef name -> FnType (for inline fn types)
	fn_ptr_modules                 map[string]string     // Synthetic fn pointer typedef name -> module name
	type_aliases                   map[string]string     // Type alias name -> base type name (for method resolution)
	type_ids                       map[string]int        // Concrete type name -> unique type ID for interface type matching
	sum_type_names                 map[string]bool       // Sum type names (for generating _tag checks in match expressions)
	sum_type_variants              map[string][]string   // Sum type name -> list of variant names (for tag values)
	next_type_id                   int                   // Counter for assigning unique type IDs
	cur_iface_match_var            string                // Interface variable being matched (e.g., "err" when matching IError)
	cur_iface_match_type           string                // Concrete type in current match branch (e.g., "MessageError")
	cur_sumtype_match_var          string                // Variable being matched in sum type match (e.g., "__match_val")
	cur_sumtype_match_orig         string                // Original expression name if Ident (e.g., "stmt" when matching stmt)
	cur_sumtype_match_variant      string                // Current variant type name (e.g., "SelectorExpr")
	cur_sumtype_match_type         string                // Sum type name (e.g., "ast__Expr")
	cur_sumtype_match_selector_lhs string                // For SelectorExpr: LHS ident (e.g., "se" for se.lhs)
	cur_sumtype_match_selector_rhs string                // For SelectorExpr: RHS field (e.g., "lhs" for se.lhs)
	cur_lambda_elem_type           string                // Element type for lambda `it` variable in filter/map/any
	// For -printfn support
	last_fn_c_name              string                    // Current function's C name for -printfn
	fn_start_pos                int                       // Start position of current function in output buffer
	collected_map_types         map[string]MapTypeInfo    // Collected map types from AST traversal
	collected_array_types       map[string]bool           // Collected array types from AST traversal (e.g., "Array_int")
	collected_fixed_array_types map[string]FixedArrayInfo // Fixed array types with element type and size
}

pub fn Gen.new(files []ast.File) &Gen {
	return Gen.new_with_env_and_pref(files, unsafe { nil }, unsafe { nil })
}

pub fn Gen.new_with_env(files []ast.File, env &types.Environment) &Gen {
	return Gen.new_with_env_and_pref(files, env, unsafe { nil })
}

pub fn Gen.new_with_env_and_pref(files []ast.File, env &types.Environment, p &pref.Preferences) &Gen {
	mut g := &Gen{
		files:                       files
		env:                         unsafe { env }
		pref:                        unsafe { p }
		sb:                          strings.new_builder(4096)
		fn_types:                    map[string]string{}
		fn_ret_counts:               map[string]int{}
		var_types:                   map[string]string{}
		mut_receivers:               map[string]bool{}
		ref_receivers:               map[string]bool{}
		enum_names:                  map[string]bool{}
		flag_enum_names:             map[string]bool{}
		interface_names:             map[string]bool{}
		interface_meths:             map[string][]string{}
		interface_meth_params:       map[string]map[string][]string{}
		type_methods:                map[string][]string{}
		struct_fields:               map[string]map[string]string{}
		file_modules:                map[string]string{}
		module_names:                map[string]bool{}
		import_aliases:              map[string]string{}
		module_types:                map[string]map[string]bool{}
		params_structs:              map[string]bool{}
		fn_params:                   map[string][]string{}
		fn_ptr_typedefs:             map[string]ast.FnType{}
		fn_ptr_modules:              map[string]string{}
		type_aliases:                map[string]string{}
		type_ids:                    map[string]int{}
		next_type_id:                1 // Start from 1, 0 means "no type" or error
		sum_type_names:              map[string]bool{}
		sum_type_variants:           map[string][]string{}
		collected_map_types:         map[string]MapTypeInfo{}
		collected_array_types:       map[string]bool{}
		collected_fixed_array_types: map[string]FixedArrayInfo{}
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
	// Collect import aliases (e.g., import v2.gen.v as gen_v -> gen_v maps to v)
	for file in files {
		for imp in file.imports {
			// Get the last component of the import path (actual module name)
			parts := imp.name.split('.')
			actual_module := if parts.len > 0 { parts[parts.len - 1] } else { imp.name }
			actual_module_mangled := actual_module.replace('.', '_')
			// Register the alias as a valid module name
			g.module_names[imp.alias] = true
			// If aliased, map alias -> actual module name
			if imp.is_aliased && imp.alias != actual_module {
				g.import_aliases[imp.alias] = actual_module_mangled
			}
			// Also register the actual module name
			g.module_names[actual_module] = true
			g.module_names[actual_module_mangled] = true
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
					mangled_name := if file_module != '' && file_module != 'main'
						&& file_module != 'builtin' {
						'${file_module}__${stmt.name}'
					} else {
						stmt.name
					}
					if stmt.variants.len > 0 {
						// Sum type - track name and variant names
						g.sum_type_names[stmt.name] = true
						g.sum_type_names[mangled_name] = true
						mut variants := []string{}
						for variant in stmt.variants {
							if variant is ast.Ident {
								variants << variant.name
							}
						}
						g.sum_type_variants[stmt.name] = variants
						g.sum_type_variants[mangled_name] = variants
					} else if stmt.base_type !is ast.EmptyExpr {
						// Track type aliases for method resolution
						base_type := g.expr_type_to_c(stmt.base_type)
						g.type_aliases[mangled_name] = base_type
					}
				}
				ast.EnumDecl {
					g.module_types[file_module][stmt.name] = true
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
				// Also store mangled name for type lookups
				mangled_enum := if file_module != '' && file_module != 'main'
					&& file_module != 'builtin' {
					'${file_module}__${stmt.name}'
				} else {
					stmt.name
				}
				g.enum_names[mangled_enum] = true
				if stmt.attributes.has('flag') {
					g.flag_enum_names[stmt.name] = true
					g.flag_enum_names[mangled_enum] = true
				}
			}
			if stmt is ast.InterfaceDecl {
				g.interface_names[stmt.name] = true
				// Collect method names and their param types for this interface
				mut methods := []string{}
				mut meth_params := map[string][]string{}
				for field in stmt.fields {
					methods << field.name
					// Extract parameter types from the method signature
					if field.typ is ast.Type {
						if field.typ is ast.FnType {
							mut params := []string{}
							for param in field.typ.params {
								params << g.expr_type_to_c(param.typ)
							}
							meth_params[field.name] = params
						}
					}
				}
				g.interface_meths[stmt.name] = methods
				g.interface_meth_params[stmt.name] = meth_params.clone()
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

				// For C interop declarations (fn C.xxx), register with C__ prefix
				if stmt.language == .c {
					g.fn_types['C__${stmt.name}'] = ret
					g.fn_ret_counts['C__${stmt.name}'] = ret_count
					continue
				}

				// Collect parameter types for tracking (with module prefixes)
				// Also collect inline function pointer types for typedef generation
				mut param_types := []string{}
				for param in stmt.typ.params {
					// Use expr_type_to_c which handles all type variants including FnType
					type_name := g.expr_type_to_c(param.typ)
					// Add module prefix if this is a type defined in the current module
					mut mangled_type := if file_module != '' && file_module != 'main'
						&& file_module != 'builtin' && !type_name.contains('__') && type_name != ''
						&& type_name in g.module_types[file_module] {
						'${file_module}__${type_name}'
					} else {
						type_name
					}
					// Add * suffix for mut params (they become pointers in C)
					if param.is_mut && !mangled_type.ends_with('*') {
						mangled_type += '*'
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
					is_ref_recv := if stmt.receiver.typ is ast.PrefixExpr {
						stmt.receiver.typ.op == .amp
					} else {
						false
					}
					g.ref_receivers[mangled] = is_ref_recv
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

// resolve_module_name resolves a module name (potentially an alias) to the actual module name
fn (g &Gen) resolve_module_name(name string) string {
	// Check if this is an import alias
	if actual := g.import_aliases[name] {
		return actual
	}
	// Return as-is if not an alias
	return name
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

// lookup_method_return_type_from_env looks up a method's return type from the environment
fn (g &Gen) lookup_method_return_type_from_env(type_name string, method_name string) ?string {
	if g.env == unsafe { nil } {
		return none
	}
	if fn_type := g.env.lookup_method(type_name, method_name) {
		if ret := fn_type.get_return_type() {
			return g.types_type_to_c(ret)
		}
		return 'void'
	}
	return none
}

// lookup_fn_return_type_from_env looks up a function's return type from the environment
fn (g &Gen) lookup_fn_return_type_from_env(module_name string, fn_name string) ?string {
	if g.env == unsafe { nil } {
		return none
	}
	if fn_type := g.env.lookup_fn(module_name, fn_name) {
		if ret := fn_type.get_return_type() {
			return g.types_type_to_c(ret)
		}
		return 'void'
	}
	return none
}

// get_expr_type_from_env retrieves the C type string for an expression from the Environment
// This is the primary way to get types - it uses the checker's computed types
fn (g &Gen) get_expr_type_from_env(e ast.Expr) ?string {
	if g.env == unsafe { nil } {
		return none
	}
	pos := e.pos()
	if pos != 0 {
		if typ := g.env.get_expr_type(pos) {
			return g.types_type_to_c(typ)
		}
	}
	return none
}

// types_type_to_c converts a types.Type to a C type string
fn (g &Gen) types_type_to_c(t types.Type) string {
	match t {
		types.Primitive {
			if t.props.has(.integer) {
				// Untyped integer literals (int_literal) default to int
				if t.props.has(.untyped) {
					return 'int'
				}
				size := if t.size == 0 { 32 } else { int(t.size) }
				signed := !t.props.has(.unsigned)
				return if signed {
					match size {
						8 { 'i8' }
						16 { 'i16' }
						32 { 'int' }
						64 { 'i64' }
						else { 'int' }
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
				// Untyped float literals default to f64
				if t.props.has(.untyped) {
					return 'f64'
				}
				return if t.size == 32 { 'f32' } else { 'f64' }
			} else if t.props.has(.boolean) {
				return 'bool'
			}
			return 'int'
		}
		types.Pointer {
			base := g.types_type_to_c(t.base_type)
			return '${base}*'
		}
		types.Array {
			elem := g.types_type_to_c(t.elem_type)
			return 'Array_${elem}'
		}
		types.ArrayFixed {
			// Return the same as dynamic arrays for compatibility
			elem := g.types_type_to_c(t.elem_type)
			return 'Array_${elem}'
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
		types.Rune {
			return 'rune'
		}
		types.Void {
			return 'void'
		}
		types.Enum {
			return t.name
		}
		types.Interface {
			return t.name
		}
		types.SumType {
			return t.get_name()
		}
		types.Map {
			key := g.types_type_to_c(t.key_type)
			val := g.types_type_to_c(t.value_type)
			return 'Map_${key}_${val}'
		}
		types.OptionType {
			base := g.types_type_to_c(t.base_type)
			return '_option_${base}'
		}
		types.ResultType {
			base := g.types_type_to_c(t.base_type)
			return '_result_${base}'
		}
		types.FnType {
			// Function pointer type - just return generic fn pointer
			return 'void*'
		}
		types.ISize {
			return 'isize'
		}
		types.USize {
			return 'usize'
		}
		types.Nil {
			return 'void*'
		}
		types.None {
			return 'void'
		}
		else {
			return 'int' // fallback
		}
	}
}

pub fn (mut g Gen) gen() string {
	// First pass: collect all map types from the AST
	g.collect_all_map_types()

	g.sb.writeln('// Generated by V Clean C Backend')
	g.sb.writeln('#include <stdio.h>')
	g.sb.writeln('#include <stdlib.h>')
	g.sb.writeln('#include <stdbool.h>')
	g.sb.writeln('#include <stdint.h>')
	g.sb.writeln('#include <stddef.h>')
	g.sb.writeln('#include <string.h>')
	g.sb.writeln('#include <float.h>')
	g.sb.writeln('#include <errno.h>')
	g.sb.writeln('#include <pthread.h>')
	g.sb.writeln('#ifdef __APPLE__')
	g.sb.writeln('#include <execinfo.h>')
	g.sb.writeln('#endif')
	g.sb.writeln('#ifndef _WIN32')
	g.sb.writeln('#include <unistd.h>')
	g.sb.writeln('#include <fcntl.h>')
	g.sb.writeln('#include <sys/stat.h>')
	g.sb.writeln('#include <dirent.h>')
	g.sb.writeln('#include <signal.h>')
	g.sb.writeln('#include <sys/wait.h>')
	g.sb.writeln('#include <termios.h>')
	g.sb.writeln('#include <sys/ioctl.h>')
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
	// ADDR macro for taking address of rvalues (compound literal trick)
	g.sb.writeln('#define ADDR(type, expr) (&((type[]){expr}[0]))')
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
	// C interop types
	g.sb.writeln('#ifndef _WIN32')
	g.sb.writeln('typedef struct termios C__termios;')
	g.sb.writeln('#endif')
	g.sb.writeln('')
	// Global variables for command-line arguments
	g.sb.writeln('static int g_main_argc = 0;')
	g.sb.writeln('static char** g_main_argv = 0;')
	g.sb.writeln('')
	// Alignment enum values (used in string formatting)
	g.sb.writeln('typedef enum { left = 0, right = 1, center = 2 } Align;')
	g.sb.writeln('')
	// Wyhash implementation (from wyhash v4.2)
	g.sb.writeln('#ifndef wyhash_final_version_4_2')
	g.sb.writeln('#define wyhash_final_version_4_2')
	g.sb.writeln('#ifndef WYHASH_CONDOM')
	g.sb.writeln('#define WYHASH_CONDOM 1')
	g.sb.writeln('#endif')
	g.sb.writeln('#ifndef WYHASH_32BIT_MUM')
	g.sb.writeln('#define WYHASH_32BIT_MUM 0')
	g.sb.writeln('#endif')
	g.sb.writeln('static inline u64 _wyrot(u64 x) { return (x>>32)|(x<<32); }')
	g.sb.writeln('static inline void _wymum(u64 *A, u64 *B){')
	g.sb.writeln('#if(WYHASH_32BIT_MUM)')
	g.sb.writeln('  u64 hh=(*A>>32)*(*B>>32), hl=(*A>>32)*(u32)*B, lh=(u32)*A*(*B>>32), ll=(u64)(u32)*A*(u32)*B;')
	g.sb.writeln('#if(WYHASH_CONDOM>1)')
	g.sb.writeln('  *A^=_wyrot(hl)^hh; *B^=_wyrot(lh)^ll;')
	g.sb.writeln('#else')
	g.sb.writeln('  *A=_wyrot(hl)^hh; *B=_wyrot(lh)^ll;')
	g.sb.writeln('#endif')
	g.sb.writeln('#elif defined(__SIZEOF_INT128__) && !defined(VWASM)')
	g.sb.writeln('  __uint128_t r=*A; r*=*B;')
	g.sb.writeln('#if(WYHASH_CONDOM>1)')
	g.sb.writeln('  *A^=(u64)r; *B^=(u64)(r>>64);')
	g.sb.writeln('#else')
	g.sb.writeln('  *A=(u64)r; *B=(u64)(r>>64);')
	g.sb.writeln('#endif')
	g.sb.writeln('#elif defined(_MSC_VER) && defined(_M_X64)')
	g.sb.writeln('#if(WYHASH_CONDOM>1)')
	g.sb.writeln('  u64 a,b; a=_umul128(*A,*B,&b); *A^=a; *B^=b;')
	g.sb.writeln('#else')
	g.sb.writeln('  *A=_umul128(*A,*B,B);')
	g.sb.writeln('#endif')
	g.sb.writeln('#else')
	g.sb.writeln('  u64 ha=*A>>32, hb=*B>>32, la=(u32)*A, lb=(u32)*B, hi, lo;')
	g.sb.writeln('  u64 rh=ha*hb, rm0=ha*lb, rm1=hb*la, rl=la*lb, t=rl+(rm0<<32), c=t<rl;')
	g.sb.writeln('  lo=t+(rm1<<32); c+=lo<t; hi=rh+(rm0>>32)+(rm1>>32)+c;')
	g.sb.writeln('#if(WYHASH_CONDOM>1)')
	g.sb.writeln('  *A^=lo; *B^=hi;')
	g.sb.writeln('#else')
	g.sb.writeln('  *A=lo; *B=hi;')
	g.sb.writeln('#endif')
	g.sb.writeln('#endif')
	g.sb.writeln('}')
	g.sb.writeln('static inline u64 _wymix(u64 A, u64 B){ _wymum(&A,&B); return A^B; }')
	g.sb.writeln('#ifndef WYHASH_LITTLE_ENDIAN')
	g.sb.writeln('#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__')
	g.sb.writeln('#define WYHASH_LITTLE_ENDIAN 1')
	g.sb.writeln('#else')
	g.sb.writeln('#define WYHASH_LITTLE_ENDIAN 0')
	g.sb.writeln('#endif')
	g.sb.writeln('#endif')
	g.sb.writeln('#if (WYHASH_LITTLE_ENDIAN)')
	g.sb.writeln('static inline u64 _wyr8(const u8 *p) { u64 v; memcpy(&v, p, 8); return v;}')
	g.sb.writeln('static inline u64 _wyr4(const u8 *p) { u32 v; memcpy(&v, p, 4); return v;}')
	g.sb.writeln('#elif defined(__GNUC__) || defined(__INTEL_COMPILER) || defined(__clang__)')
	g.sb.writeln('static inline u64 _wyr8(const u8 *p) { u64 v; memcpy(&v, p, 8); return __builtin_bswap64(v);}')
	g.sb.writeln('static inline u64 _wyr4(const u8 *p) { u32 v; memcpy(&v, p, 4); return __builtin_bswap32(v);}')
	g.sb.writeln('#else')
	g.sb.writeln('static inline u64 _wyr8(const u8 *p) { u64 v; memcpy(&v, p, 8); return (((v >> 56) & 0xff)| ((v >> 40) & 0xff00)| ((v >> 24) & 0xff0000)| ((v >> 8) & 0xff000000)| ((v << 8) & 0xff00000000ull)| ((v << 24) & 0xff0000000000ull)| ((v << 40) & 0xff000000000000ull)| ((v << 56) & 0xff00000000000000ull)); }')
	g.sb.writeln('static inline u64 _wyr4(const u8 *p) { u32 v; memcpy(&v, p, 4); return (((v >> 24) & 0xff)| ((v >> 8) & 0xff00)| ((v << 8) & 0xff0000)| ((v << 24) & 0xff000000)); }')
	g.sb.writeln('#endif')
	g.sb.writeln('static inline u64 _wyr3(const u8 *p, usize k) { return (((u64)p[0])<<16)|(((u64)p[k>>1])<<8)|p[k-1];}')
	g.sb.writeln('#ifndef _likely_')
	g.sb.writeln('#if defined(__GNUC__) || defined(__clang__)')
	g.sb.writeln('#define _likely_(x) __builtin_expect(!!(x), 1)')
	g.sb.writeln('#define _unlikely_(x) __builtin_expect(!!(x), 0)')
	g.sb.writeln('#else')
	g.sb.writeln('#define _likely_(x) (x)')
	g.sb.writeln('#define _unlikely_(x) (x)')
	g.sb.writeln('#endif')
	g.sb.writeln('#endif')
	g.sb.writeln('static inline u64 wyhash(const void *key, usize len, u64 seed, const u64 *secret){')
	g.sb.writeln('  const u8 *p=(const u8 *)key; seed^=_wymix(seed^secret[0],secret[1]); u64 a, b;')
	g.sb.writeln('  if (_likely_(len<=16)) {')
	g.sb.writeln('    if (_likely_(len>=4)) { a=(_wyr4(p)<<32)|_wyr4(p+((len>>3)<<2)); b=(_wyr4(p+len-4)<<32)|_wyr4(p+len-4-((len>>3)<<2)); }')
	g.sb.writeln('    else if (_likely_(len>0)) { a=_wyr3(p,len); b=0; }')
	g.sb.writeln('    else a=b=0;')
	g.sb.writeln('  } else {')
	g.sb.writeln('    usize i=len;')
	g.sb.writeln('    if (_unlikely_(i>=48)) {')
	g.sb.writeln('      u64 see1=seed, see2=seed;')
	g.sb.writeln('      do {')
	g.sb.writeln('        seed=_wymix(_wyr8(p)^secret[1],_wyr8(p+8)^seed);')
	g.sb.writeln('        see1=_wymix(_wyr8(p+16)^secret[2],_wyr8(p+24)^see1);')
	g.sb.writeln('        see2=_wymix(_wyr8(p+32)^secret[3],_wyr8(p+40)^see2);')
	g.sb.writeln('        p+=48; i-=48;')
	g.sb.writeln('      } while(_likely_(i>=48));')
	g.sb.writeln('      seed^=see1^see2;')
	g.sb.writeln('    }')
	g.sb.writeln('    while(_unlikely_(i>16)) { seed=_wymix(_wyr8(p)^secret[1],_wyr8(p+8)^seed); i-=16; p+=16; }')
	g.sb.writeln('    a=_wyr8(p+i-16); b=_wyr8(p+i-8);')
	g.sb.writeln('  }')
	g.sb.writeln('  a^=secret[1]; b^=seed; _wymum(&a,&b);')
	g.sb.writeln('  return _wymix(a^secret[0]^len,b^secret[1]);')
	g.sb.writeln('}')
	g.sb.writeln('static const u64 _wyp[4] = {0x2d358dccaa6c78a5ull, 0x8bb84b93962eacc9ull, 0x4b33a62ed433d4a3ull, 0x4d5a2da51de1aa47ull};')
	g.sb.writeln('static inline u64 wyhash64(u64 A, u64 B){ A^=0x2d358dccaa6c78a5ull; B^=0x8bb84b93962eacc9ull; _wymum(&A,&B); return _wymix(A^0x2d358dccaa6c78a5ull,B^0x8bb84b93962eacc9ull);}')
	g.sb.writeln('#endif // wyhash_final_version_4_2')
	g.sb.writeln('')
	// String free stub
	g.sb.writeln('static inline void string__free(void* s) { (void)s; }')
	g.sb.writeln('')
	// Profiler allocator stub (when --profile-alloc is enabled)
	if g.pref != unsafe { nil } && g.pref.use_context_allocator {
		g.sb.writeln('// Profiler allocator support')
		g.sb.writeln('// v2_profiler_alloc is a stub that can be replaced with actual profiler')
		g.sb.writeln('// To enable full profiling, link with the v2 profiler library')
		g.sb.writeln('static inline void* v2_profiler_alloc(size_t size) {')
		g.sb.writeln('    return malloc(size);')
		g.sb.writeln('}')
		g.sb.writeln('static inline void v2_profiler_free(void* ptr) {')
		g.sb.writeln('    free(ptr);')
		g.sb.writeln('}')
		g.sb.writeln('')
	}
	// Map function access macros (to call function pointers stored in map struct)
	g.sb.writeln('#define map__hash_fn(m, key) ((m).hash_fn((key)))')
	g.sb.writeln('#define map__key_eq_fn(m, key1, key2) ((m).key_eq_fn((key1), (key2)))')
	g.sb.writeln('#define map__clone_fn(m, dest, src) ((m).clone_fn((dest), (src)))')
	g.sb.writeln('#define map__free_fn(m, key) ((m).free_fn((key)))')
	g.sb.writeln('')
	// Clone stubs for primitive types
	g.sb.writeln('static inline int int__clone(int* x) { return *x; }')
	g.sb.writeln('static inline i64 i64__clone(i64* x) { return *x; }')
	g.sb.writeln('static inline u64 u64__clone(u64* x) { return *x; }')
	g.sb.writeln('')
	// Variadic argument struct for V-style variadics
	g.sb.writeln('// Variadic argument types')
	g.sb.writeln('typedef struct { int len; voidptr* data; } VArg_voidptr;')
	g.sb.writeln('typedef struct { int len; int* data; } VArg_int;')
	g.sb.writeln('typedef struct { int len; int* data; } VArg_os__Signal;')
	g.sb.writeln('')
	// Stubs for Android-specific C types (not available on other platforms)
	g.sb.writeln('typedef struct { voidptr dummy; } C__ANativeActivity;')
	g.sb.writeln('typedef struct { voidptr dummy; } C__AAssetManager;')
	g.sb.writeln('typedef struct { voidptr dummy; } C__AAsset;')
	g.sb.writeln('typedef int os__AssetMode;')
	g.sb.writeln('')
	// C interop types
	g.sb.writeln('typedef FILE* C__FILE;')
	g.sb.writeln('typedef int os__FileBufferMode;')
	g.sb.writeln('#ifdef __APPLE__')
	g.sb.writeln('#include <mach/mach_time.h>')
	g.sb.writeln('#include <libproc.h>')
	g.sb.writeln('typedef mach_timebase_info_data_t C__mach_timebase_info_data_t;')
	g.sb.writeln('#else')
	g.sb.writeln('typedef struct { int numer; int denom; } C__mach_timebase_info_data_t;')
	g.sb.writeln('#endif')
	// utime and utsname for POSIX
	g.sb.writeln('#include <utime.h>')
	g.sb.writeln('#include <sys/utsname.h>')
	g.sb.writeln('typedef struct utimbuf C__utimbuf;')
	g.sb.writeln('typedef struct utsname C__utsname;')
	g.sb.writeln('typedef DIR C__DIR;')
	g.sb.writeln('typedef struct dirent C__dirent;')
	g.sb.writeln('typedef struct tm C__tm;')
	g.sb.writeln('typedef struct timespec C__timespec;')
	g.sb.writeln('')
	// termios constants (platform-specific)
	g.sb.writeln('#ifndef NCCS')
	g.sb.writeln('#define NCCS 32')
	g.sb.writeln('#endif')
	g.sb.writeln('#define cclen NCCS')
	// fd_set and select() types
	g.sb.writeln('#include <sys/select.h>')
	g.sb.writeln('#include <sys/stat.h>')
	g.sb.writeln('typedef fd_set C__fd_set;')
	g.sb.writeln('typedef struct timeval C__timeval;')
	g.sb.writeln('typedef struct stat C__stat;')
	// statvfs for disk usage
	g.sb.writeln('#include <sys/statvfs.h>')
	g.sb.writeln('typedef struct statvfs C__statvfs;')
	// ptrace (for debug detection)
	g.sb.writeln('#include <sys/ptrace.h>')
	g.sb.writeln('')
	// POSIX extern variables
	g.sb.writeln('extern char** environ;')
	g.sb.writeln('')

	// 1. Struct/Union Declarations (Typedefs)
	mut seen_structs := map[string]bool{}
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				// Skip C interop and generic structs
				if stmt.language == .c || stmt.generic_params.len > 0 {
					continue
				}
				// Generate mangled name with module prefix
				mangled_name := if file_module != '' && file_module != 'main'
					&& file_module != 'builtin' {
					'${file_module}__${stmt.name}'
				} else {
					stmt.name
				}
				// Skip builtin Array (handled separately as generic)
				if mangled_name == 'Array' {
					continue
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

	// Array type aliases (dynamically generated from types collected in AST traversal)
	g.sb.writeln('// Array type aliases')
	for array_type_name, _ in g.collected_array_types {
		g.sb.writeln('typedef array ${array_type_name};')
	}
	g.sb.writeln('')

	// Fixed array type aliases - primitive types only (e.g., typedef u8 Array_fixed_u8_2 [2];)
	// Non-primitive types (string, custom types) are deferred until after struct/type definitions
	primitive_c_types := ['u8', 'i8', 'u16', 'i16', 'u32', 'i32', 'u64', 'i64', 'int', 'f32', 'f64',
		'bool', 'char', 'voidptr', 'byte']
	g.sb.writeln('// Fixed array type aliases (primitive types)')
	for fixed_array_name, info in g.collected_fixed_array_types {
		// Map V types to C types
		c_elem_type := match info.elem_type {
			'u8', 'byte' { 'u8' }
			'i8' { 'i8' }
			'u16' { 'u16' }
			'i16' { 'i16' }
			'u32' { 'u32' }
			'i32' { 'i32' }
			'u64' { 'u64' }
			'i64' { 'i64' }
			'int' { 'int' }
			'f32' { 'f32' }
			'f64' { 'f64' }
			'bool' { 'bool' }
			'char' { 'char' }
			'voidptr' { 'voidptr' }
			else { info.elem_type }
		}
		// Only emit primitive types here - non-primitive types need full definitions first
		if c_elem_type in primitive_c_types {
			g.sb.writeln('typedef ${c_elem_type} ${fixed_array_name} [${info.size}];')
		}
	}
	g.sb.writeln('')

	// Variadic string type (needs to come after string is defined)
	g.sb.writeln('typedef struct { int len; string* data; } VArg_string;')
	g.sb.writeln('')

	// Map type aliases (maps are all the same struct, but V names them by key/value types)
	// Dynamically generated from types collected in AST traversal
	g.sb.writeln('// Map type aliases')
	for map_type_name, _ in g.collected_map_types {
		g.sb.writeln('typedef map ${map_type_name};')
	}
	g.sb.writeln('')

	// 2. Enum Declarations (must come before structs that use enum fields)
	mut seen_enums := map[string]bool{}
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		for stmt in file.stmts {
			if stmt is ast.EnumDecl {
				// Generate mangled name with module prefix
				mangled_name := if file_module != '' && file_module != 'main'
					&& file_module != 'builtin' {
					'${file_module}__${stmt.name}'
				} else {
					stmt.name
				}
				if mangled_name in seen_enums {
					continue
				}
				seen_enums[mangled_name] = true
				g.gen_enum_decl_with_name(stmt, mangled_name)
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
		g.cur_module = file_module // Set for expr_type_to_c in function types
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
		// Restore the module context from when the typedef was registered
		saved_module := g.cur_module
		if stored_module := g.fn_ptr_modules[typedef_name] {
			g.cur_module = stored_module
		}
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
		g.cur_module = saved_module
	}
	if g.fn_ptr_typedefs.len > 0 {
		g.sb.writeln('')
	}

	// 2.5.1. Pre-scan interfaces to collect Result/Option types for method return types
	// This must happen BEFORE interface struct definitions so the types are defined
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		g.cur_module = file_module
		for stmt in file.stmts {
			if stmt is ast.InterfaceDecl {
				for field in stmt.fields {
					if fn_type := g.get_fn_type_from_expr(field.typ) {
						if fn_type.return_type !is ast.EmptyExpr {
							// This call will register any Result/Option types
							_ = g.expr_type_to_c(fn_type.return_type)
						}
					}
				}
			}
			// Also pre-scan struct fields for Option/Result types
			if stmt is ast.StructDecl {
				for field in stmt.fields {
					// This call will register any Option/Result types in field types
					_ = g.expr_type_to_c(field.typ)
				}
			}
		}
	}

	// 2.5.2. Forward-declare Result/Option types (needed before interface structs use them as function pointer return types)
	// Full definitions come later in section 3.4.1/3.4.2 after IError is defined
	for result_name, _ in g.result_types {
		g.sb.writeln('typedef struct ${result_name} ${result_name};')
	}
	for option_name, _ in g.option_types {
		g.sb.writeln('typedef struct ${option_name} ${option_name};')
	}
	if g.result_types.len > 0 || g.option_types.len > 0 {
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

	// 2.7. Result/Option struct definitions (must come after IError, before structs that use them)
	// These were forward-declared in section 2.5.2
	// Use fixed-size buffer to avoid circular dependency with base type definitions
	for result_name, _ in g.result_types {
		g.sb.writeln('struct ${result_name} { bool is_error; IError err; u8 data[256]; };')
		g.emitted_result_types[result_name] = true
	}
	for option_name, _ in g.option_types {
		g.sb.writeln('struct ${option_name} { u8 state; IError err; u8 data[256]; };')
		g.emitted_option_types[option_name] = true
	}
	if g.result_types.len > 0 || g.option_types.len > 0 {
		g.sb.writeln('')
	}

	// 3. Struct Definitions - collect and sort by dependencies
	mut all_structs := map[string]ast.StructDecl{}
	mut struct_modules := map[string]string{} // mangled_name -> module
	// Emit core builtin types first (string, etc.) - they're used by many other structs
	core_builtins := ['string', 'array', 'Error', 'VAssertMetaInfo']
	mut emitted_core := map[string]bool{}
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		if file_module != 'builtin' && file_module != '' {
			continue
		}
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				if stmt.name in core_builtins && stmt.language != .c {
					g.gen_struct_decl_with_name(stmt, stmt.name)
					g.sb.writeln('')
					emitted_core[stmt.name] = true
					// Track field types for type inference (core builtins)
					mut fields := map[string]string{}
					// Include embedded struct fields first
					for emb in stmt.embedded {
						embedded_type := g.expr_type_to_c(emb)
						if emb_fields := g.struct_fields[embedded_type] {
							for k, v in emb_fields {
								fields[k] = v
							}
						}
					}
					for field in stmt.fields {
						fields[field.name] = g.get_field_type_for_inference(field.typ)
					}
					g.struct_fields[stmt.name] = fields.clone()
				}
			}
		}
	}
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		g.cur_module = file_module // Set for expr_type_to_c in field types
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				// Skip C interop and generic structs
				if stmt.language == .c || stmt.generic_params.len > 0 {
					continue
				}
				// Skip core builtins already emitted
				if stmt.name in emitted_core {
					continue
				}
				// Generate mangled name with module prefix
				mangled_name := if file_module != '' && file_module != 'main'
					&& file_module != 'builtin' {
					'${file_module}__${stmt.name}'
				} else {
					stmt.name
				}
				// Skip builtin Array (handled separately as generic)
				if mangled_name == 'Array' {
					continue
				}
				if mangled_name !in all_structs {
					all_structs[mangled_name] = stmt
					struct_modules[mangled_name] = file_module
					// Track field types for type inference
					mut fields := map[string]string{}
					// Include embedded struct fields first
					for emb in stmt.embedded {
						embedded_type := g.expr_type_to_c(emb)
						if emb_fields := g.struct_fields[embedded_type] {
							for k, v in emb_fields {
								fields[k] = v
							}
						}
					}
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
			// Set cur_module for proper dependency name resolution
			saved_module := g.cur_module
			g.cur_module = struct_modules[mangled_name]
			// Check if all non-pointer field types are emitted
			mut ready := true
			// Check embedded struct dependencies
			for emb in decl.embedded {
				dep := g.expr_type_to_c(emb)
				if dep != '' && dep in all_structs && dep !in emitted {
					ready = false
					break
				}
			}
			// Check field type dependencies
			if ready {
				for field in decl.fields {
					dep := g.get_embedded_struct_type(field.typ)
					if dep != '' && dep in all_structs && dep !in emitted {
						ready = false
						break
					}
				}
			}
			if ready {
				emitted[mangled_name] = true
				// cur_module already set for dependency check, use it for codegen
				g.gen_struct_decl_with_name(decl, mangled_name)
				g.sb.writeln('')
			}
			g.cur_module = saved_module
		}
	}
	// Emit remaining (circular deps)
	for mangled_name, decl in all_structs {
		if mangled_name !in emitted {
			// Set cur_module so field types get correct prefixes
			saved_module := g.cur_module
			g.cur_module = struct_modules[mangled_name]
			g.gen_struct_decl_with_name(decl, mangled_name)
			g.cur_module = saved_module
			g.sb.writeln('')
		}
	}

	// Fixed array type aliases for non-primitive types (now that structs and type aliases are defined)
	g.sb.writeln('// Fixed array type aliases (non-primitive types)')
	for fixed_array_name, info in g.collected_fixed_array_types {
		c_elem_type := match info.elem_type {
			'u8', 'byte' { 'u8' }
			'i8' { 'i8' }
			'u16' { 'u16' }
			'i16' { 'i16' }
			'u32' { 'u32' }
			'i32' { 'i32' }
			'u64' { 'u64' }
			'i64' { 'i64' }
			'int' { 'int' }
			'f32' { 'f32' }
			'f64' { 'f64' }
			'bool' { 'bool' }
			'char' { 'char' }
			'voidptr' { 'voidptr' }
			else { info.elem_type }
		}
		// Only emit non-primitive types here (primitive ones were emitted earlier)
		if c_elem_type !in primitive_c_types {
			g.sb.writeln('typedef ${c_elem_type} ${fixed_array_name} [${info.size}];')
		}
	}
	g.sb.writeln('')

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

	// 3.4.1. Result types for !T - generate any NEW types discovered during function generation
	for result_name, _ in g.result_types {
		if result_name !in g.emitted_result_types {
			g.sb.writeln('struct ${result_name} { bool is_error; IError err; u8 data[256]; };')
			g.emitted_result_types[result_name] = true
		}
	}

	// 3.4.2. Option types for ?T - generate any NEW types discovered during function generation
	for option_name, _ in g.option_types {
		if option_name !in g.emitted_option_types {
			g.sb.writeln('struct ${option_name} { u8 state; IError err; u8 data[256]; };')
			g.emitted_option_types[option_name] = true
		}
	}
	g.sb.writeln('')

	// 3.5. Constants
	// Pre-register all constants so cross-references work
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		g.cur_module = file_module
		for stmt in file.stmts {
			if stmt is ast.ConstDecl {
				for field in stmt.fields {
					mangled_name := if file_module != '' && file_module != 'main'
						&& file_module != 'builtin' {
						'${file_module}__${field.name}'
					} else {
						field.name
					}
					g.const_mangled[field.name] = mangled_name
				}
			}
		}
	}
	// Pre-collect constant values (first pass: simple literals)
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		g.cur_module = file_module
		for stmt in file.stmts {
			if stmt is ast.ConstDecl {
				for field in stmt.fields {
					mangled_name := g.const_mangled[field.name] or { field.name }
					// Only collect simple integer literals in first pass
					if field.value is ast.BasicLiteral {
						if field.value.kind == .number {
							g.const_values[field.name] = field.value.value
							g.const_values[mangled_name] = field.value.value
						}
					}
				}
			}
		}
	}
	// Pre-collect constant values (second pass: resolve references)
	for _ in 0 .. 3 {
		// Multiple passes to handle transitive references
		for file in g.files {
			file_module := g.file_modules[file.name] or { '' }
			g.cur_module = file_module
			for stmt in file.stmts {
				if stmt is ast.ConstDecl {
					for field in stmt.fields {
						if field.name in g.const_values {
							continue // Already resolved
						}
						mangled_name := g.const_mangled[field.name] or { field.name }
						if val := g.try_eval_int_const(field.value) {
							g.const_values[field.name] = val
							g.const_values[mangled_name] = val
						}
					}
				}
			}
		}
	}
	// Now generate the constants
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		g.cur_module = file_module // Set for expr_type_to_c in constant types
		for stmt in file.stmts {
			if stmt is ast.ConstDecl {
				g.gen_const_decl(stmt, file_module)
				g.sb.writeln('')
			}
		}
	}

	// 3.6. Builtin helpers (array__free, etc.)
	// IError__free - C interop in source doesn't generate correctly
	g.sb.writeln('static inline void IError__free(IError* ie) { if (ie && ie->_object) free(ie->_object); }')
	// IError_str - convert IError to string by calling msg() method
	g.sb.writeln('static inline string IError_str(IError e) { return e.msg ? e.msg(e._object) : (string){"", 0}; }')
	// array__free - inline implementation since method may not be generated due to $if blocks
	g.sb.writeln('static inline void array__free(array* a) { if (a->data) free(a->data); a->data = 0; a->len = 0; a->cap = 0; }')
	// Array_string__free - free each string then free the array
	g.sb.writeln('static inline void Array_string__free(Array_string* a) { for (int i = 0; i < a->len; i++) string__free(&((string*)a->data)[i]); array__free((array*)a); }')
	// strings__Builder__free - Builder is a type alias for []u8, delegate to array__free
	g.sb.writeln('static inline void strings__Builder__free(strings__Builder* b) { array__free((array*)b); }')
	// array__contains_u8 - check if array contains element (for 'in' operator)
	g.sb.writeln('static inline bool array__contains_u8(array a, u8 v) { for (int i = 0; i < a.len; i++) { if (((u8*)a.data)[i] == v) return true; } return false; }')
	// Forward declaration for string__eq (used by array__contains_string)
	g.sb.writeln('bool string__eq(string s, string a);')
	// array__contains_string - check if array contains string element
	g.sb.writeln('static inline bool array__contains_string(array a, string v) { for (int i = 0; i < a.len; i++) { if (string__eq(((string*)a.data)[i], v)) return true; } return false; }')
	// builtin__new_array_from_c_array_noscan - create array from C array literal
	g.sb.writeln('static inline array builtin__new_array_from_c_array_noscan(int len, int cap, int elem_size, void* c_array) {')
	g.sb.writeln('    array a = {0}; a.len = len; a.cap = cap; a.element_size = elem_size;')
	g.sb.writeln('    if (len > 0) { a.data = malloc(cap * elem_size); if (a.data) memcpy(a.data, c_array, len * elem_size); }')
	g.sb.writeln('    return a;')
	g.sb.writeln('}')
	// builtin____new_array_noscan - create empty array with given capacity
	g.sb.writeln('static inline array builtin____new_array_noscan(int len, int cap, int elem_size) {')
	g.sb.writeln('    array a = {0}; a.len = len; a.cap = cap > len ? cap : len; a.element_size = elem_size;')
	g.sb.writeln('    if (a.cap > 0) { a.data = calloc(a.cap, elem_size); }')
	g.sb.writeln('    return a;')
	g.sb.writeln('}')
	// builtin__array_push_noscan - push element to array
	g.sb.writeln('static inline void builtin__array_push_noscan(array* a, void* elem) {')
	g.sb.writeln('    if (a->len >= a->cap) { a->cap = a->cap == 0 ? 8 : a->cap * 2; a->data = realloc(a->data, a->cap * a->element_size); }')
	g.sb.writeln('    memcpy((char*)a->data + a->len * a->element_size, elem, a->element_size);')
	g.sb.writeln('    a->len++;')
	g.sb.writeln('}')
	// array__contains_* functions for common types (takes array by value)
	g.sb.writeln('static inline bool array__contains_int(array a, int v) { for (int i = 0; i < a.len; i++) { if (((int*)a.data)[i] == v) return true; } return false; }')
	g.sb.writeln('static inline bool array__contains_i64(array a, i64 v) { for (int i = 0; i < a.len; i++) { if (((i64*)a.data)[i] == v) return true; } return false; }')
	g.sb.writeln('static inline bool array__contains_u64(array a, u64 v) { for (int i = 0; i < a.len; i++) { if (((u64*)a.data)[i] == v) return true; } return false; }')
	g.sb.writeln('static inline bool array__contains_rune(array a, rune v) { for (int i = 0; i < a.len; i++) { if (((rune*)a.data)[i] == v) return true; } return false; }')
	// strings__Builder__trim - trim builder to specified length
	g.sb.writeln('static inline void strings__Builder__trim(strings__Builder b, int n) { if (n < b.len) b.len = n; }')
	// ArrayFlags__has - check if flag is set
	g.sb.writeln('static inline bool ArrayFlags__has(ArrayFlags* f, ArrayFlags v) { return (((int)(*f)) & ((int)(v))) != 0; }')
	// array__eq - compare two arrays for equality
	g.sb.writeln('static inline bool array__eq(array a, array b) { if (a.len != b.len) return false; return memcmp(a.data, b.data, a.len * a.element_size) == 0; }')
	// array__clone - clone an array (returns a copy with same elements)
	g.sb.writeln('static inline array array__clone(array* a) { array c = {0}; if (!a || a->len == 0) return c; c.len = a->len; c.cap = a->len; c.element_size = a->element_size; c.data = malloc(a->len * a->element_size); if (c.data) memcpy(c.data, a->data, a->len * a->element_size); return c; }')
	// Register transformer-synthesized function return types (not in type system)
	g.fn_types['string__plus'] = 'string'
	g.fn_types['string__plus_two'] = 'string'
	g.fn_types['string__eq'] = 'bool'
	g.fn_types['string__lt'] = 'bool'
	// Forward declarations for map runtime functions
	g.sb.writeln('// Map function forward declarations')
	g.sb.writeln('typedef u64 (*MapHashFn)(void*);')
	g.sb.writeln('typedef bool (*MapEqFn)(void*, void*);')
	g.sb.writeln('typedef void (*MapCloneFn)(void*, void*);')
	g.sb.writeln('typedef void (*MapFreeFn)(void*);')
	g.sb.writeln('map new_map(int key_bytes, int value_bytes, MapHashFn hash_fn, MapEqFn key_eq_fn, MapCloneFn clone_fn, MapFreeFn free_fn);')
	g.sb.writeln('void* map__get(map* m, void* key, void* zero);')
	g.sb.writeln('void map__set(map* m, void* key, void* value);')
	g.sb.writeln('void* map__get_and_set(map* m, void* key, void* zero);')
	g.sb.writeln('void* map__get_check(map* m, void* key);')
	g.sb.writeln('void map__delete(map* m, void* key);')
	g.sb.writeln('void map__clear(map* m);')
	g.sb.writeln('map map__clone(map* m);')
	// builtin__map_get_and_set - get value from map, setting default if not present
	g.sb.writeln('static inline void* builtin__map_get_and_set(map* m, void* key, void* zero) { return map__get_and_set(m, key, zero); }')
	g.sb.writeln('static inline void* builtin__map_get_check(map* m, void* key) { return map__get_check(m, key); }')
	g.sb.writeln('u64 map_hash_string(void* pkey);')
	g.sb.writeln('bool map_eq_string(void* a, void* b);')
	g.sb.writeln('void map_clone_string(void* dest, void* pkey);')
	g.sb.writeln('void map_free_string(void* pkey);')
	g.sb.writeln('u64 map_hash_int_1(void* pkey);')
	g.sb.writeln('u64 map_hash_int_2(void* pkey);')
	g.sb.writeln('u64 map_hash_int_4(void* pkey);')
	g.sb.writeln('u64 map_hash_int_8(void* pkey);')
	g.sb.writeln('bool map_eq_int_1(void* a, void* b);')
	g.sb.writeln('bool map_eq_int_2(void* a, void* b);')
	g.sb.writeln('bool map_eq_int_4(void* a, void* b);')
	g.sb.writeln('bool map_eq_int_8(void* a, void* b);')
	g.sb.writeln('void map_clone_int_1(void* dest, void* pkey);')
	g.sb.writeln('void map_clone_int_2(void* dest, void* pkey);')
	g.sb.writeln('void map_clone_int_4(void* dest, void* pkey);')
	g.sb.writeln('void map_clone_int_8(void* dest, void* pkey);')
	g.sb.writeln('void map_free_nop(void* _);')
	g.sb.writeln('')
	// Dynamically generate map function wrappers from collected map types
	for map_type_name, info in g.collected_map_types {
		// Register getter return type
		g.fn_types['__${map_type_name}_get'] = info.value_c_type
		// Register get_check return type (pointer to value type)
		g.fn_types['__${map_type_name}_get_check'] = '${info.value_c_type}*'
		// Determine hash/eq/clone functions based on key type
		key_size := match info.key_c_type {
			'i8', 'u8', 'bool' { '1' }
			'i16', 'u16' { '2' }
			'int', 'i32', 'u32', 'rune', 'f32' { '4' }
			'i64', 'u64', 'f64', 'isize', 'usize' { '8' }
			else { '4' } // default to 4-byte
		}
		is_string_key := info.key_c_type == 'string'
		hash_fn := if is_string_key { 'map_hash_string' } else { 'map_hash_int_${key_size}' }
		eq_fn := if is_string_key { 'map_eq_string' } else { 'map_eq_int_${key_size}' }
		clone_fn := if is_string_key { 'map_clone_string' } else { 'map_clone_int_${key_size}' }
		free_fn := if is_string_key { 'map_free_string' } else { 'map_free_nop' }
		// Generate actual wrappers that call V runtime
		default_val := g.get_c_default_value(info.value_c_type)
		g.sb.writeln('static inline ${map_type_name} __new_${map_type_name}() { return new_map(sizeof(${info.key_c_type}), sizeof(${info.value_c_type}), ${hash_fn}, ${eq_fn}, ${clone_fn}, ${free_fn}); }')
		// Skip by-value get function for fixed arrays - C functions can't return arrays by value
		// Use get_check (returns pointer) instead for fixed array value types
		if !info.value_c_type.starts_with('Array_fixed_') {
			g.sb.writeln('static inline ${info.value_c_type} __${map_type_name}_get(${map_type_name}* m, ${info.key_c_type} key) { ${info.value_c_type} _zero = ${default_val}; return *(${info.value_c_type}*)map__get((map*)m, &key, &_zero); }')
		}
		g.sb.writeln('static inline ${info.value_c_type}* __${map_type_name}_get_check(${map_type_name}* m, ${info.key_c_type} key) { return (${info.value_c_type}*)map__get_check((map*)m, &key); }')
		// For fixed arrays, use a pointer parameter since C can't pass arrays by value
		if info.value_c_type.starts_with('Array_fixed_') {
			// Parse fixed array size from type name: Array_fixed_int_2 -> 2
			parts := info.value_c_type.split('_')
			arr_size := if parts.len >= 4 { parts[parts.len - 1] } else { '1' }
			elem_type := if parts.len >= 4 { parts[2] } else { 'int' }
			// Generate: void __Map_K_V_set(Map* m, K key, const V val) { V* ptr = map__get_or_insert(m, &key); memcpy(ptr, val, sizeof(V)); }
			g.sb.writeln('static inline void __${map_type_name}_set(${map_type_name}* m, ${info.key_c_type} key, const ${elem_type} val[${arr_size}]) { ${elem_type}* ptr = (${elem_type}*)map__get_and_set((map*)m, &key, &(${elem_type}[${arr_size}]){0}); memcpy(ptr, val, sizeof(${elem_type}) * ${arr_size}); }')
		} else {
			g.sb.writeln('static inline void __${map_type_name}_set(${map_type_name}* m, ${info.key_c_type} key, ${info.value_c_type} val) { map__set((map*)m, &key, &val); }')
		}
	}
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
	// Thread-related globals
	g.sb.writeln('static u64 g_main_thread_id = 0;')
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

	// Helper function to find V executable path (for @VEXE)
	// This finds the vroot by walking up from cwd looking for vlib directory
	g.sb.writeln('static string __vexe_path() {')
	g.sb.writeln('    static char __cached_vexe[1024] = {0};')
	g.sb.writeln('    if (__cached_vexe[0]) return tos_clone((u8*)__cached_vexe);')
	g.sb.writeln('    char cwd[1024]; getcwd(cwd, sizeof(cwd));')
	g.sb.writeln('    char path[1024]; strncpy(path, cwd, sizeof(path));')
	g.sb.writeln('    // Walk up looking for vlib directory')
	g.sb.writeln('    for (int i = 0; i < 10; i++) {')
	g.sb.writeln('        char vlib[1024]; snprintf(vlib, sizeof(vlib), "%s/vlib", path);')
	g.sb.writeln('        struct stat st; if (stat(vlib, &st) == 0 && S_ISDIR(st.st_mode)) {')
	g.sb.writeln('            // Found vlib, return a fake exe path so os.dir() gives this path')
	g.sb.writeln('            snprintf(__cached_vexe, sizeof(__cached_vexe), "%s/v", path);')
	g.sb.writeln('            return tos_clone((u8*)__cached_vexe);')
	g.sb.writeln('        }')
	g.sb.writeln("        char* last_slash = strrchr(path, '/');")
	g.sb.writeln('        if (last_slash == NULL || last_slash == path) break;')
	g.sb.writeln("        *last_slash = '\\0';")
	g.sb.writeln('    }')
	g.sb.writeln('    // Fallback to cwd')
	g.sb.writeln('    snprintf(__cached_vexe, sizeof(__cached_vexe), "%s/v", cwd);')
	g.sb.writeln('    return tos_clone((u8*)__cached_vexe);')
	g.sb.writeln('}')
	g.sb.writeln('')

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
	s := t.name()
	if s == 'string' {
		return 'string'
	}
	return s
}

// Generate tuple type name from element types and register it for struct generation
fn (mut g Gen) get_tuple_type(tuple_types []ast.Expr) string {
	mut elem_types := []string{cap: tuple_types.len}
	mut elem_types_mangled := []string{cap: tuple_types.len}
	for t in tuple_types {
		typ := g.expr_type_to_c(t)
		elem_types << typ
		elem_types_mangled << sanitize_type_for_mangling(typ)
	}
	// Create type name like Tuple_2_string or Tuple_2_int_string
	type_name := 'Tuple_${tuple_types.len}_${elem_types_mangled.join('_')}'
	// Register this tuple type for struct generation
	if type_name !in g.tuple_types {
		g.tuple_types[type_name] = elem_types
	}
	return type_name
}

// Generate Result type name for !T types and register it for struct generation
// Following V's pattern: _result_T with is_error, err, and data[sizeof(T)]
fn (mut g Gen) get_result_type(base_type string) string {
	// Handle void result type - just use the generic _result
	if base_type == 'void' || base_type == '' {
		return '_result'
	}
	// Skip generic type parameters (single uppercase letters like T, U, V)
	if base_type.len == 1 && base_type[0] >= `A` && base_type[0] <= `Z` {
		return '_result'
	}
	// Create type name like _result_int, _result_string
	mangled_base := sanitize_type_for_mangling(base_type)
	type_name := '_result_${mangled_base}'
	// Register this result type for struct generation
	if type_name !in g.result_types {
		g.result_types[type_name] = base_type
	}
	return type_name
}

// Generate Option type name for ?T types and register it for struct generation
fn (mut g Gen) get_option_type(base_type string) string {
	if base_type == 'void' || base_type == '' {
		return '_option'
	}
	// Skip generic type parameters (single uppercase letters like T, U, V)
	if base_type.len == 1 && base_type[0] >= `A` && base_type[0] <= `Z` {
		return '_option'
	}
	mangled_base := sanitize_type_for_mangling(base_type)
	type_name := '_option_${mangled_base}'
	if type_name !in g.option_types {
		g.option_types[type_name] = base_type
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
		g.fn_ptr_modules[type_name] = g.cur_module
	}
	return type_name
}

fn (mut g Gen) expr_type_to_c(e ast.Expr) string {
	match e {
		ast.Ident {
			name := e.name
			// Handle V-style array type names that weren't properly converted to AST nodes
			if name.starts_with('[]') {
				elem_type := name[2..]
				// Recursively convert the element type
				elem_c := g.expr_type_to_c(ast.Ident{ name: elem_type })
				return 'Array_${sanitize_type_for_mangling(elem_c)}'
			}
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
				// Fixed-size arrays - return Array_fixed_<elem_type>_<len>
				elem_type := sanitize_type_for_mangling(g.expr_type_to_c(e.elem_type))
				// Extract length from the len expression
				mut len_str := '0'
				if e.len is ast.BasicLiteral {
					len_str = e.len.value
				}
				return 'Array_fixed_${elem_type}_${len_str}'
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
			// Handle Option and Result types - generate proper wrapper types
			if e is ast.OptionType {
				base_type := g.expr_type_to_c(e.base_type)
				return g.get_option_type(base_type)
			}
			if e is ast.ResultType {
				base_type := g.expr_type_to_c(e.base_type)
				return g.get_result_type(base_type)
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
	// For BasicLiteral, use environment type (set by checker)
	if node is ast.BasicLiteral {
		if t := g.get_expr_type_from_env(node) {
			return t
		}
		return 'int'
	}
	// For StringInterLiteral, use environment type
	if node is ast.StringInterLiteral {
		if t := g.get_expr_type_from_env(node) {
			return t
		}
		return 'string'
	}
	// For CastExpr, use environment type
	if node is ast.CastExpr {
		if t := g.get_expr_type_from_env(node) {
			return t
		}
		return g.expr_type_to_c(node.typ)
	}
	// For AsCastExpr, the type is the target type
	if node is ast.AsCastExpr {
		if t := g.get_expr_type_from_env(node) {
			return t
		}
		return g.expr_type_to_c(node.typ)
	}
	// For MatchExpr, use environment type
	if node is ast.MatchExpr {
		if t := g.get_expr_type_from_env(node) {
			return t
		}
		return 'int'
	}
	// For IfExpr, use environment type or infer from branches
	if node is ast.IfExpr {
		if t := g.get_expr_type_from_env(node) {
			return t
		}
		// Fallback: infer type from the last statement in branches
		// This handles if-guard expressions like: x := if val := map[key] { val } else { default }
		if node.stmts.len > 0 {
			last_stmt := node.stmts[node.stmts.len - 1]
			if last_stmt is ast.ExprStmt {
				return g.infer_type(last_stmt.expr)
			}
		}
		return 'int'
	}
	// For UnsafeExpr, use environment type
	if node is ast.UnsafeExpr {
		if t := g.get_expr_type_from_env(node) {
			return t
		}
		// Fallback: infer type from last statement
		if node.stmts.len > 0 {
			last_stmt := node.stmts[node.stmts.len - 1]
			if last_stmt is ast.ExprStmt {
				return g.infer_type(last_stmt.expr)
			}
		}
		return 'int'
	}
	// For ParenExpr, always delegate to inner expression
	// ParenExpr type is always the same as its inner expression - this is delegation, not inference
	// Note: Environment lookup can return wrong types due to position collisions,
	// so we always delegate to inner expression for correctness
	if node is ast.ParenExpr {
		return g.infer_type(node.expr)
	}
	// For ModifierExpr, use environment type or delegate to inner expression
	if node is ast.ModifierExpr {
		if t := g.get_expr_type_from_env(node) {
			return t
		}
		// Type not found - delegate to inner expression
		return g.infer_type(node.expr)
	}
	// Other expression types - manual type inference
	match node {
		ast.StringLiteral {
			if node.kind == .c || node.value.starts_with("c'") {
				return 'char*'
			}
			return 'string'
		}
		ast.ArrayInitExpr {
			// For array literals, infer element type from explicit type annotation or first expr
			if node.typ !is ast.EmptyExpr {
				// node.typ is ArrayType{elem_type: T} for []T{} syntax
				// Use get_field_type_for_inference which handles ArrayType properly
				return g.get_field_type_for_inference(node.typ)
			}
			// Check for [a, b, c]! fixed array syntax
			if node.len is ast.PostfixExpr {
				postfix := node.len as ast.PostfixExpr
				if postfix.op == .not && postfix.expr is ast.EmptyExpr {
					// Fixed array - infer element type from first expression
					// Include size in type for .len access: FixedArray_u8_29
					if node.exprs.len > 0 {
						elem_type := sanitize_type_for_mangling(g.infer_type(node.exprs[0]))
						return 'FixedArray_${elem_type}_${node.exprs.len}'
					}
					return 'FixedArray_u8_0'
				}
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
			mut is_c_fn := false
			if node.lhs is ast.Ident {
				name = node.lhs.name
			} else if node.lhs is ast.SelectorExpr {
				// Check if this is a C function call (e.g., C.stat)
				if node.lhs.lhs is ast.Ident && node.lhs.lhs.name == 'C' {
					name = node.lhs.rhs.name
					is_c_fn = true
				} else if node.lhs.lhs is ast.Ident && node.lhs.lhs.name in g.module_names {
					// Module-qualified function call - resolve import alias
					resolved_mod := g.resolve_module_name(node.lhs.lhs.name)
					name = '${resolved_mod}__${node.lhs.rhs.name}'
				} else if node.lhs.lhs is ast.SelectorExpr {
					// Chained selector - could be module.Type.method() static call
					inner_sel := node.lhs.lhs as ast.SelectorExpr
					if inner_sel.lhs is ast.Ident {
						mod_name := (inner_sel.lhs as ast.Ident).name
						if mod_name in g.module_names {
							// module.Type.method() -> look up module__Type__method
							// Resolve import alias
							resolved_mod := g.resolve_module_name(mod_name)
							static_name := '${resolved_mod}__${inner_sel.rhs.name}__${node.lhs.rhs.name}'
							if t := g.fn_types[static_name] {
								return t
							}
							// Static method not in fn_types yet - return the type based on convention
							// For Type.new() methods, return pointer to that type
							if node.lhs.rhs.name == 'new' || node.lhs.rhs.name.starts_with('new_') {
								return '${resolved_mod}__${inner_sel.rhs.name}*'
							}
						}
					}
					// Fall through to method call handling
					name = node.lhs.rhs.name
					receiver_type := g.infer_type(node.lhs.lhs)
					// Unsanitize first (types__Scopeptr -> types__Scope*)
					unsanitized := unsanitize_type_for_c(receiver_type)
					// Then strip pointer suffix
					clean_type := if unsanitized.ends_with('*') {
						unsanitized[..unsanitized.len - 1]
					} else {
						unsanitized
					}
					mangled := '${clean_type}__${name}'
					if t := g.fn_types[mangled] {
						return t
					}
					// Look up method return type from environment
					if ret := g.lookup_method_return_type_from_env(clean_type, name) {
						return ret
					}
					// Array methods that return the element type
					if clean_type.starts_with('Array_') && name in ['first', 'last', 'pop'] {
						return clean_type['Array_'.len..]
					}
					// Array methods that return the same array type
					if clean_type.starts_with('Array_')
						&& name in ['clone', 'filter', 'map', 'sorted', 'reverse', 'slice'] {
						return clean_type
					}
					// Base array type methods
					if clean_type == 'array' && name in ['clone', 'reverse', 'sorted'] {
						return 'array'
					}
					// Map methods that return the same map type
					if clean_type.starts_with('Map_') && name == 'clone' {
						return clean_type
					}
				} else {
					// Check if this is a static method call on a local type: Type.method()
					if node.lhs.lhs is ast.Ident {
						type_name := (node.lhs.lhs as ast.Ident).name
						method_name := node.lhs.rhs.name
						// Check if type_name is a type in the current module
						if types_in_module := g.module_types[g.cur_module] {
							if type_name in types_in_module {
								// Local type static method: Type.method() -> module__Type__method
								mangled_type := '${g.cur_module}__${type_name}'
								static_name := '${mangled_type}__${method_name}'
								if t := g.fn_types[static_name] {
									return t
								}
								// For Type.new() methods, return pointer to that type
								if method_name == 'new' || method_name.starts_with('new_') {
									return '${mangled_type}*'
								}
							}
						}
					}
					// Method call - look up method return type
					name = node.lhs.rhs.name
					// Get receiver type
					receiver_type := g.infer_type(node.lhs.lhs)
					// Unsanitize first (types__Scopeptr -> types__Scope*)
					unsanitized := unsanitize_type_for_c(receiver_type)
					// Then strip pointer suffix
					clean_type := if unsanitized.ends_with('*') {
						unsanitized[..unsanitized.len - 1]
					} else {
						unsanitized
					}
					mangled := '${clean_type}__${name}'
					if t := g.fn_types[mangled] {
						return t
					}
					// Look up method return type from environment
					if ret := g.lookup_method_return_type_from_env(clean_type, name) {
						return ret
					}
					// Map function pointer fields - return type of the function pointer
					if clean_type == 'map' {
						if name == 'hash_fn' {
							return 'u64' // MapHashFn returns u64
						}
						if name == 'key_eq_fn' {
							return 'bool' // MapEqFn returns bool
						}
					}
					// Check if this is a function pointer field being called
					if fields := g.struct_fields[clean_type] {
						if field_type := fields[name] {
							// Function pointer types like MapHashFn, MapEqFn have known return types
							if field_type == 'MapHashFn' {
								return 'u64'
							}
							if field_type == 'MapEqFn' {
								return 'bool'
							}
						}
					}
					// Array methods that return the element type
					if clean_type.starts_with('Array_') && name in ['first', 'last', 'pop'] {
						return clean_type['Array_'.len..]
					}
					// Array methods that return the same array type
					if clean_type.starts_with('Array_')
						&& name in ['clone', 'filter', 'map', 'sorted', 'reverse', 'slice'] {
						return clean_type
					}
					// Base array type methods
					if clean_type == 'array' && name in ['clone', 'reverse', 'sorted'] {
						return 'array'
					}
					// Map methods that return the same map type
					if clean_type.starts_with('Map_') && name == 'clone' {
						return clean_type
					}
				}
			}

			// For C functions, look up with C__ prefix
			if is_c_fn {
				if t := g.fn_types['C__${name}'] {
					return t
				}
				return 'int' // Default return type for C functions
			}

			// Handle __Map_*_get_check - returns pointer to value type
			if name.starts_with('__Map_') && name.ends_with('_get_check') {
				// Extract value type from __Map_K_V_get_check -> V*
				// e.g., __Map_string_types__Scopeptr_get_check -> types__Scope**
				map_type_and_suffix := name[2..] // Remove __ prefix: Map_K_V_get_check
				map_type := map_type_and_suffix[..map_type_and_suffix.len - 10] // Remove _get_check: Map_K_V
				if map_type.starts_with('Map_') {
					rest := map_type['Map_'.len..] // K_V
					// Use known key types to find the split point
					key_types := ['int_', 'string_', 'i64_', 'u64_', 'bool_', 'u8_', 'i8_', 'i16_',
						'u16_', 'i32_', 'u32_', 'rune_']
					for key_prefix in key_types {
						if rest.starts_with(key_prefix) {
							value_type := rest[key_prefix.len..] // e.g., types__Scopeptr
							// Unsanitize pointer types (e.g., types__Scopeptr -> types__Scope*)
							unsanitized := unsanitize_type_for_c(value_type)
							// get_check returns pointer to value type
							return '${unsanitized}*'
						}
					}
				}
			}
			// Handle __Map_*_get - returns value type directly
			if name.starts_with('__Map_') && name.ends_with('_get') && !name.ends_with('_get_check') {
				// Extract value type from __Map_K_V_get -> V (unsanitized)
				// Map type name is between __ and _get: Map_K_V
				map_type := name[2..name.len - 4] // Remove __ prefix and _get suffix
				if map_type.starts_with('Map_') {
					suffix := map_type['Map_'.len..]
					// Find first underscore that separates key from value type
					first_underscore := suffix.index('_') or { -1 }
					if first_underscore > 0 {
						value_type := suffix[first_underscore + 1..]
						// Unsanitize pointer types (e.g., arm64__Intervalptr -> arm64__Interval*)
						return unsanitize_type_for_c(value_type)
					}
				}
			}
			// Try with current module prefix FIRST to avoid conflicts with same-named functions in other modules
			if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
				mangled_name := '${g.cur_module}__${name}'
				if t := g.fn_types[mangled_name] {
					return t
				}
			}
			// Fall back to unmangled name
			if t := g.fn_types[name] {
				return t
			}
			// Look up function return type from environment
			if ret := g.lookup_fn_return_type_from_env(g.cur_module, name) {
				return ret
			}
			if ret := g.lookup_fn_return_type_from_env('builtin', name) {
				return ret
			}
			// Handle builtin__new_map_init_noscan_value - infer map type from keys/vals args
			if name == 'builtin__new_map_init_noscan_value' {
				if node.args.len >= 9 {
					// args: hash_fn, eq_fn, clone_fn, free_fn, n, key_size, val_size, keys_arr, vals_arr
					keys_arg := node.args[7]
					vals_arg := node.args[8]
					// Infer key/value types from the array arguments
					mut key_type := 'string'
					mut val_type := 'int'
					if keys_arg is ast.ArrayInitExpr && keys_arg.exprs.len > 0 {
						key_type = sanitize_type_for_mangling(g.infer_type(keys_arg.exprs[0]))
					}
					if vals_arg is ast.ArrayInitExpr && vals_arg.exprs.len > 0 {
						val_type = sanitize_type_for_mangling(g.infer_type(vals_arg.exprs[0]))
					}
					return 'Map_${key_type}_${val_type}'
				}
				// Fall back for map init with less than 9 args (shouldn't happen)
				return 'Map_string_int'
			}
			// For calls with ArrayInitExpr argument, infer array type from that argument
			for arg in node.args {
				if arg is ast.ArrayInitExpr {
					arr_type := g.infer_type(arg)
					if arr_type.starts_with('Array_') {
						return arr_type
					}
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
				mut is_c_fn := false
				if node.lhs.lhs is ast.Ident {
					if node.lhs.lhs.name == 'C' {
						name = node.lhs.rhs.name
						is_c_fn = true
					} else if node.lhs.lhs.name in g.module_names {
						// Module-qualified function call - resolve import alias
						resolved_mod := g.resolve_module_name(node.lhs.lhs.name)
						name = '${resolved_mod}__${node.lhs.rhs.name}'
					} else {
						// Check if this is a local type static method call: TypeName.method(arg)
						type_name := (node.lhs.lhs as ast.Ident).name
						method_name := node.lhs.rhs.name
						if types_in_module := g.module_types[g.cur_module] {
							if type_name in types_in_module {
								// Local type static method: TypeName.method(arg) -> module__TypeName__method
								mangled_type := '${g.cur_module}__${type_name}'
								static_name := '${mangled_type}__${method_name}'
								if t := g.fn_types[static_name] {
									return t
								}
								// For Type.new() methods, return pointer to that type
								if method_name == 'new' || method_name.starts_with('new_') {
									return '${mangled_type}*'
								}
							}
						}
					}
				} else if node.lhs.lhs is ast.SelectorExpr {
					// Chained selector - could be module.Type.method(arg) static call
					inner_sel := node.lhs.lhs as ast.SelectorExpr
					if inner_sel.lhs is ast.Ident {
						mod_name := (inner_sel.lhs as ast.Ident).name
						if mod_name in g.module_names {
							// module.Type.method(arg) -> look up module__Type__method return type
							resolved_mod := g.resolve_module_name(mod_name)
							static_name := '${resolved_mod}__${inner_sel.rhs.name}__${node.lhs.rhs.name}'
							if t := g.fn_types[static_name] {
								return t
							}
							// Static method not in fn_types yet - infer from convention
							// For Type.new() methods, return pointer to that type
							if node.lhs.rhs.name == 'new' || node.lhs.rhs.name.starts_with('new_') {
								return '${resolved_mod}__${inner_sel.rhs.name}*'
							}
						}
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
					// Map function pointer fields - return type of the function pointer
					if clean_type == 'map' {
						if name == 'hash_fn' {
							return 'u64' // MapHashFn returns u64
						}
						if name == 'key_eq_fn' {
							return 'bool' // MapEqFn returns bool
						}
					}
					// Check if this is a function pointer field being called
					if fields := g.struct_fields[clean_type] {
						if field_type := fields[name] {
							// Function pointer types like MapHashFn, MapEqFn have known return types
							if field_type == 'MapHashFn' {
								return 'u64'
							}
							if field_type == 'MapEqFn' {
								return 'bool'
							}
						}
					}
					// Array methods that return the element type
					if clean_type.starts_with('Array_') && name in ['first', 'last', 'pop'] {
						return clean_type['Array_'.len..]
					}
					// Array methods that return the same array type
					if clean_type.starts_with('Array_')
						&& name in ['clone', 'filter', 'map', 'sorted', 'reverse', 'slice'] {
						return clean_type
					}
					// Base array type methods
					if clean_type == 'array' && name in ['clone', 'reverse', 'sorted'] {
						return 'array'
					}
					// Map methods that return the same map type
					if clean_type.starts_with('Map_') && name == 'clone' {
						return clean_type
					}
				}
				// For C functions/types, look up with C__ prefix
				if is_c_fn {
					if t := g.fn_types['C__${name}'] {
						return t
					}
					// For C type casts like C.utsname(...), return the C type
					return 'C__${name}'
				}
			} else if node.lhs is ast.IndexExpr || node.lhs is ast.Type {
				// Handle array type casts like []u8(b) - return the array type
				return g.expr_type_to_c(node.lhs)
			}
			// Try with current module prefix FIRST to avoid conflicts with same-named functions in other modules
			if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
				mangled_name := '${g.cur_module}__${name}'
				if t := g.fn_types[mangled_name] {
					return t
				}
			}
			// Fall back to unmangled name (for builtin functions or if not found in current module)
			if t := g.fn_types[name] {
				return t
			}
			return 'int'
		}
		ast.Ident {
			// Check if this is the lambda `it` variable
			if node.name == 'it' && g.cur_lambda_elem_type != '' {
				return g.cur_lambda_elem_type
			}
			// Check if this is the sumtype match variable - return variant type
			if g.cur_sumtype_match_var != '' && g.cur_sumtype_match_variant != '' {
				if node.name == g.cur_sumtype_match_var
					|| (g.cur_sumtype_match_orig != '' && node.name == g.cur_sumtype_match_orig) {
					// Return the mangled variant type
					// If cur_sumtype_match_variant already has __ (e.g., 'ast__TypeDecl'), use it directly
					variant_mangled := if g.cur_sumtype_match_variant.contains('__') {
						g.cur_sumtype_match_variant
					} else if g.cur_sumtype_match_type.contains('__') {
						parts := g.cur_sumtype_match_type.split('__')
						if parts.len >= 2 {
							'${parts[0]}__${g.cur_sumtype_match_variant}'
						} else {
							g.cur_sumtype_match_variant
						}
					} else {
						g.mangle_type_if_needed(g.cur_sumtype_match_variant)
					}
					return variant_mangled
				}
			}
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
			// Special case: 'err' in or-blocks is always IError
			if node.name == 'err' {
				return 'IError'
			}
			return 'int'
		}
		ast.InfixExpr {
			// Comparison, containment, and type-check operators return bool
			if node.op in [.eq, .ne, .lt, .gt, .le, .ge, .key_in, .not_in, .and, .logical_or, .key_is,
				.not_is] {
				return 'bool'
			}
			return g.infer_type(node.lhs)
		}
		ast.SelectorExpr {
			// Check if this is an enum value access (EnumName.value)
			if node.lhs is ast.Ident {
				if node.lhs.name in g.enum_names {
					// Try to return the mangled name if available
					if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
						mangled := '${g.cur_module}__${node.lhs.name}'
						if mangled in g.enum_names {
							return mangled
						}
					}
					return node.lhs.name
				}
				// Check if this is module-qualified global variable access (os.args)
				if node.lhs.name in g.module_names {
					// Resolve import alias if present
					resolved_mod := g.resolve_module_name(node.lhs.name)
					mangled := '${resolved_mod}__${node.rhs.name}'
					if t := g.global_var_types[mangled] {
						return t
					}
					if t := g.const_types[mangled] {
						return t
					}
				}
				// Check if this is a sum type match variable - use variant type for field lookup
				if g.cur_sumtype_match_var != '' && g.cur_sumtype_match_variant != '' {
					mut is_match := false
					// Simple identifier match (e.g., expr in `if expr is Type`)
					// Check both the temp __match_val and the original variable name (e.g., 'stmt')
					if g.cur_sumtype_match_var != '__selector__'
						&& (node.lhs.name == g.cur_sumtype_match_var
						|| (g.cur_sumtype_match_orig != ''
						&& node.lhs.name == g.cur_sumtype_match_orig)) {
						is_match = true
					}
					// Selector expression match (e.g., se.lhs in `if se.lhs is Type`)
					// We need to check if node matches the tracked selector
					if g.cur_sumtype_match_var == '__selector__'
						&& g.cur_sumtype_match_selector_lhs != ''
						&& g.cur_sumtype_match_selector_rhs != ''
						&& node.lhs.name == g.cur_sumtype_match_selector_lhs {
						// This is accessing a field on the LHS of the tracked selector
						// But we're looking for the full selector itself, not a field on its LHS
					}
					if is_match {
						// Look up field from variant struct
						// The variant name needs to be mangled using the sum type's module prefix
						// e.g., for sum type 'ast__Expr' and variant 'AsCastExpr', look up 'ast__AsCastExpr'
						// BUT if cur_sumtype_match_variant already has __ (e.g., 'ast__TypeDecl'), use it directly
						variant_mangled := if g.cur_sumtype_match_variant.contains('__') {
							// Already mangled (e.g., 'ast__TypeDecl')
							g.cur_sumtype_match_variant
						} else if g.cur_sumtype_match_type.contains('__') {
							// Extract module prefix from sum type name
							parts := g.cur_sumtype_match_type.split('__')
							if parts.len >= 2 {
								'${parts[0]}__${g.cur_sumtype_match_variant}'
							} else {
								g.cur_sumtype_match_variant
							}
						} else {
							g.mangle_type_if_needed(g.cur_sumtype_match_variant)
						}
						if fields := g.struct_fields[variant_mangled] {
							if field_type := fields[node.rhs.name] {
								return field_type
							}
						}
					}
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
			// Hard-coded Result/Option type fields
			if clean_base.starts_with('_result_') {
				match node.rhs.name {
					'is_error' { return 'bool' }
					'err' { return 'IError' }
					'data' { return g.result_types[clean_base] or { 'int' } }
					else {}
				}
			}
			if clean_base.starts_with('_option_') {
				match node.rhs.name {
					'state' { return 'u8' }
					'err' { return 'IError' }
					'data' { return g.option_types[clean_base] or { 'int' } }
					else {}
				}
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
				// Parse Map_<key>_<value> -> return value type
				// Map types are: Map_int_int, Map_string_bool, Map_int_Array_int, etc.
				rest := base_type['Map_'.len..]
				// Find the first underscore after the key type
				// Common key types: int, string, i64, u64
				key_types := ['int_', 'string_', 'i64_', 'u64_', 'bool_']
				for key_prefix in key_types {
					if rest.starts_with(key_prefix) {
						val_type := rest[key_prefix.len..]
						// Value type might be complex like Array_int or Map_string_bool
						// Unsanitize pointer types (e.g., Intervalptr -> Interval*)
						return unsanitize_type_for_c(val_type)
					}
				}
				// Default to int for unknown map types
				return 'int'
			}
			if base_type.starts_with('Array_fixed_') {
				// Array_fixed_int_2 -> return int
				// Parse: strip 'Array_fixed_' prefix and last '_<size>' suffix
				rest := base_type['Array_fixed_'.len..]
				// Find the last underscore followed by a number
				mut last_underscore := -1
				for i := rest.len - 1; i >= 0; i-- {
					if rest[i] == `_` {
						// Check if what follows is all digits
						suffix := rest[i + 1..]
						mut all_digits := suffix.len > 0
						for c in suffix {
							if c < `0` || c > `9` {
								all_digits = false
								break
							}
						}
						if all_digits {
							last_underscore = i
							break
						}
					}
				}
				elem_type := if last_underscore > 0 { rest[..last_underscore] } else { rest }
				return unsanitize_type_for_c(elem_type)
			}
			if base_type.starts_with('FixedArray_') {
				// Format: FixedArray_<elem_type>_<size> -> return elem_type
				// e.g., FixedArray_string_11 -> string, FixedArray_u8_29 -> u8
				rest := base_type['FixedArray_'.len..]
				// Find the last underscore followed by a number (the size)
				mut last_underscore := -1
				for i := rest.len - 1; i >= 0; i-- {
					if rest[i] == `_` {
						suffix := rest[i + 1..]
						mut all_digits := suffix.len > 0
						for c in suffix {
							if c < `0` || c > `9` {
								all_digits = false
								break
							}
						}
						if all_digits {
							last_underscore = i
							break
						}
					}
				}
				elem_type := if last_underscore > 0 { rest[..last_underscore] } else { rest }
				return unsanitize_type_for_c(elem_type)
			}
			if base_type.starts_with('Array_') {
				// Unsanitize pointer types (e.g., Intervalptr -> Interval*)
				return unsanitize_type_for_c(base_type['Array_'.len..])
			}
			if base_type.starts_with('VArg_') {
				// VArg_voidptr -> return voidptr
				return base_type['VArg_'.len..]
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
		ast.PostfixExpr {
			// For error propagation (!) and optional (?), return the unwrapped base type
			if node.op == .not || node.op == .question {
				inner_type := g.infer_type(node.expr)
				// Unwrap Result type
				if inner_type.starts_with('_result_') {
					return g.result_types[inner_type] or { inner_type }
				}
				// Unwrap Option type
				if inner_type.starts_with('_option_') {
					return g.option_types[inner_type] or { inner_type }
				}
				return inner_type
			}
			return g.infer_type(node.expr)
		}
		ast.Type {
			// Handle type expressions
			if node is ast.NilType {
				return 'voidptr' // nil is a pointer
			}
			if node is ast.NoneType {
				return 'int' // none is typically 0
			}
			return 'int'
		}
		ast.OrExpr {
			// Or expression: expr or { fallback }
			// Return the unwrapped type (base type for Result/Option types)
			typ := g.infer_type(node.expr)
			// Result type - return base type
			if typ.starts_with('_result_') {
				return g.result_types[typ] or { typ }
			}
			// Option type - return base type
			if typ.starts_with('_option_') {
				return g.option_types[typ] or { typ }
			}
			return typ
		}
		ast.LockExpr {
			// Lock expression - return the type of the last expression in the block
			if node.stmts.len > 0 {
				last_stmt := node.stmts.last()
				if last_stmt is ast.ExprStmt {
					// Check for expanded or-block pattern: the last expr is *_temp_var
					// and we need to find the type from the initial assignment
					if last_stmt.expr is ast.PrefixExpr && last_stmt.expr.op == .mul {
						if last_stmt.expr.expr is ast.Ident {
							temp_name := last_stmt.expr.expr.name
							// Look for initial assignment of this temp variable
							for stmt in node.stmts {
								if stmt is ast.AssignStmt && stmt.op == .decl_assign {
									if stmt.lhs.len == 1 && stmt.lhs[0] is ast.Ident {
										lhs_ident := stmt.lhs[0] as ast.Ident
										if lhs_ident.name == temp_name {
											// Found it - infer type from RHS which should be a map get_check call
											ptr_type := g.infer_type(stmt.rhs[0])
											// ptr_type is something like types__Scope** (pointer to pointer)
											// We're dereferencing it, so return types__Scope*
											if ptr_type.ends_with('*') {
												return ptr_type[..ptr_type.len - 1]
											}
											return ptr_type
										}
									}
								}
							}
						}
					}
					return g.infer_type(last_stmt.expr)
				}
			}
			return 'void'
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
	// Skip C stdlib functions that would conflict (but only for standalone functions in builtin, not methods)
	// Note: strconv.atoi is fine - it becomes strconv__atoi which doesn't conflict with C's atoi
	if !node.is_method && node.name in c_stdlib_fns
		&& (g.cur_module == '' || g.cur_module == 'main' || g.cur_module == 'builtin') {
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
	mut result := fn_name
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		result = '${g.cur_module}__${fn_name}'
	}
	// Skip functions that have inline stubs
	if result in stubbed_fns {
		return ''
	}
	return result
}

// C keywords that need escaping when used as identifiers
const c_keywords = ['auto', 'break', 'case', 'char', 'const', 'continue', 'default', 'do', 'double',
	'else', 'enum', 'extern', 'float', 'for', 'goto', 'if', 'inline', 'int', 'long', 'register',
	'restrict', 'return', 'short', 'signed', 'sizeof', 'static', 'struct', 'switch', 'typedef',
	'union', 'unsigned', 'void', 'volatile', 'while', '_Bool', '_Complex', '_Imaginary']

// C stdlib functions that should never be generated (they conflict with libc)
const c_stdlib_fns = ['malloc', 'calloc', 'realloc', 'free', 'atoi', 'atof', 'atol', 'memcpy',
	'memset', 'memmove', 'strlen', 'strcpy', 'strcat', 'strcmp', 'memcmp']

// Methods that have inline stubs and should not be generated from source
// These are mangled names like 'string__free' or 'array__free'
// Only include functions that genuinely need stubs due to $if blocks or complex C interop
const stubbed_fns = ['string__free', 'array__free', 'strings__Builder__free', 'IError__free',
	'Array_string__free', 'array__clone']

fn escape_c_keyword(name string) string {
	if name in c_keywords {
		return '_${name}'
	}
	return name
}

// Get embedded (non-pointer) struct type name from a field type, or empty string
// Returns mangled name (e.g., os__FilePermission) for proper dependency tracking
fn (g Gen) get_embedded_struct_type(typ ast.Expr) string {
	match typ {
		ast.Ident {
			// Simple identifier - could be a struct type
			// Exclude primitives
			if typ.name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32',
				'f64', 'bool', 'rune', 'byte', 'voidptr', 'charptr', 'usize', 'isize', 'string'] {
				return ''
			}
			// Return mangled name using current module context
			if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
				if types_in_module := g.module_types[g.cur_module] {
					if typ.name in types_in_module {
						return '${g.cur_module}__${typ.name}'
					}
				}
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
		ast.SelectorExpr {
			// Qualified name like module.Type - return mangled name
			if typ.lhs is ast.Ident {
				return '${typ.lhs.name}__${typ.rhs.name}'
			}
			return ''
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
	// Include fields from embedded structs first
	for emb in node.embedded {
		embedded_type := g.expr_type_to_c(emb)
		// Look up the embedded struct's fields and include them
		if emb_fields := g.struct_fields[embedded_type] {
			for field_name, field_type in emb_fields {
				g.write_indent()
				g.sb.write_string('\t')
				safe_name := escape_c_keyword(field_name)
				t := g.type_for_c_decl(field_type)
				g.sb.writeln('${t} ${safe_name};')
			}
		}
	}
	for field in node.fields {
		g.write_indent()
		g.sb.write_string('\t')
		field_name := escape_c_keyword(field.name)
		// Check for fixed-size array type - use the helper function
		if arr_info := g.get_fixed_array_info(field.typ) {
			g.sb.writeln('${arr_info.elem_type} ${field_name}[${arr_info.size}];')
		} else {
			t := g.type_for_c_decl(g.expr_type_to_c(field.typ))
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
				size_str := g.get_array_size_str(typ.len)
				return FixedArrayInfo{elem_type, size_str}
			}
		}
		else {}
	}
	return none
}

// get_fixed_array_info_from_init extracts fixed array info from an ArrayInitExpr
// This handles both explicit [n]T{} syntax and [a,b,c]! literal syntax
fn (mut g Gen) get_fixed_array_info_from_init(arr ast.ArrayInitExpr) ?FixedArrayInfo {
	// First check if typ is explicitly set as ArrayFixedType
	if info := g.get_fixed_array_info(arr.typ) {
		return info
	}
	// Check for [a, b, c]! syntax - parser marks this with len: PostfixExpr{op: .not}
	if arr.len is ast.PostfixExpr {
		postfix := arr.len as ast.PostfixExpr
		if postfix.op == .not && postfix.expr is ast.EmptyExpr {
			// Infer element type from first expression
			mut elem_type := 'u8' // default
			if arr.exprs.len > 0 {
				elem_type = g.infer_type(arr.exprs[0])
			}
			return FixedArrayInfo{elem_type, '${arr.exprs.len}'}
		}
	}
	return none
}

// Helper to get the size string for a fixed-size array
fn (mut g Gen) get_array_size_str(len ast.Expr) string {
	if len is ast.BasicLiteral {
		return len.value
	}
	if len is ast.Ident {
		// First try to get the resolved constant value (needed for fixed-size arrays)
		if val := g.const_values[len.name] {
			return val
		}
		// Fall back to mangled name (only works for compile-time constants)
		if mangled_name := g.const_mangled[len.name] {
			return mangled_name
		}
		return len.name
	}
	return '0'
}

// get_c_default_value returns the default value for a C type
fn (g &Gen) get_c_default_value(c_type string) string {
	return match c_type {
		'string' {
			'(string){"", 0}'
		}
		'bool' {
			'false'
		}
		'array' {
			'(array){0}'
		}
		'map' {
			'(map){0}'
		}
		else {
			// For pointers, return NULL
			if c_type.ends_with('*') || c_type.ends_with('ptr') {
				'0'
			} else if c_type.starts_with('Map_') || c_type.starts_with('Array_')
				|| c_type.starts_with('FixedArray_') {
				// Container types
				'(${c_type}){0}'
			} else if c_type in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32',
				'f64', 'isize', 'usize', 'rune', 'byte'] {
				// Numeric primitives
				'0'
			} else {
				// Struct types - use compound literal
				'(${c_type}){0}'
			}
		}
	}
}

// collect_all_map_types traverses all AST files and collects all map types used
fn (mut g Gen) collect_all_map_types() {
	for file in g.files {
		file_module := g.file_modules[file.name] or { '' }
		g.cur_module = file_module
		for stmt in file.stmts {
			g.collect_map_types_from_stmt(stmt)
		}
	}
}

// collect_map_types_from_stmt recursively collects map types from a statement
fn (mut g Gen) collect_map_types_from_stmt(stmt ast.Stmt) {
	match stmt {
		ast.AssignStmt {
			for lhs in stmt.lhs {
				g.collect_map_types_from_expr(lhs)
			}
			for rhs in stmt.rhs {
				g.collect_map_types_from_expr(rhs)
			}
		}
		ast.FnDecl {
			// Collect from function type (params and return type)
			for param in stmt.typ.params {
				g.collect_map_types_from_expr(param.typ)
			}
			g.collect_map_types_from_expr(stmt.typ.return_type)
			for s in stmt.stmts {
				g.collect_map_types_from_stmt(s)
			}
		}
		ast.StructDecl {
			for field in stmt.fields {
				g.collect_map_types_from_expr(field.typ)
			}
		}
		ast.TypeDecl {
			g.collect_map_types_from_expr(stmt.base_type)
			for variant in stmt.variants {
				g.collect_map_types_from_expr(variant)
			}
		}
		ast.ExprStmt {
			g.collect_map_types_from_expr(stmt.expr)
		}
		ast.ReturnStmt {
			for e in stmt.exprs {
				g.collect_map_types_from_expr(e)
			}
		}
		ast.ForStmt {
			g.collect_map_types_from_stmt(stmt.init)
			g.collect_map_types_from_expr(stmt.cond)
			g.collect_map_types_from_stmt(stmt.post)
			for s in stmt.stmts {
				g.collect_map_types_from_stmt(s)
			}
		}
		ast.ForInStmt {
			g.collect_map_types_from_expr(stmt.key)
			g.collect_map_types_from_expr(stmt.value)
			g.collect_map_types_from_expr(stmt.expr)
		}
		ast.BlockStmt {
			for s in stmt.stmts {
				g.collect_map_types_from_stmt(s)
			}
		}
		ast.DeferStmt {
			for s in stmt.stmts {
				g.collect_map_types_from_stmt(s)
			}
		}
		ast.GlobalDecl {
			for field in stmt.fields {
				g.collect_map_types_from_expr(field.typ)
				g.collect_map_types_from_expr(field.value)
			}
		}
		ast.ConstDecl {
			for field in stmt.fields {
				g.collect_map_types_from_expr(field.value)
			}
		}
		else {}
	}
}

// collect_map_types_from_expr recursively collects map types from an expression
fn (mut g Gen) collect_map_types_from_expr(expr ast.Expr) {
	match expr {
		ast.Type {
			// Type expressions contain MapType, ArrayType, etc.
			g.collect_map_types_from_type(expr)
		}
		ast.MapInitExpr {
			g.collect_map_types_from_expr(expr.typ)
			for key in expr.keys {
				g.collect_map_types_from_expr(key)
			}
			for val in expr.vals {
				g.collect_map_types_from_expr(val)
			}
		}
		ast.CallExpr {
			g.collect_map_types_from_expr(expr.lhs)
			for arg in expr.args {
				g.collect_map_types_from_expr(arg)
			}
		}
		ast.SelectorExpr {
			g.collect_map_types_from_expr(expr.lhs)
		}
		ast.IndexExpr {
			g.collect_map_types_from_expr(expr.lhs)
			g.collect_map_types_from_expr(expr.expr)
		}
		ast.CastExpr {
			g.collect_map_types_from_expr(expr.typ)
			g.collect_map_types_from_expr(expr.expr)
		}
		ast.IfExpr {
			g.collect_map_types_from_expr(expr.cond)
			for s in expr.stmts {
				g.collect_map_types_from_stmt(s)
			}
			g.collect_map_types_from_expr(expr.else_expr)
		}
		ast.MatchExpr {
			g.collect_map_types_from_expr(expr.expr)
			for branch in expr.branches {
				for cond in branch.cond {
					g.collect_map_types_from_expr(cond)
				}
				for s in branch.stmts {
					g.collect_map_types_from_stmt(s)
				}
			}
		}
		ast.InfixExpr {
			g.collect_map_types_from_expr(expr.lhs)
			g.collect_map_types_from_expr(expr.rhs)
		}
		ast.PrefixExpr {
			g.collect_map_types_from_expr(expr.expr)
		}
		ast.PostfixExpr {
			g.collect_map_types_from_expr(expr.expr)
		}
		ast.ArrayInitExpr {
			g.collect_map_types_from_expr(expr.typ)
			for elem in expr.exprs {
				g.collect_map_types_from_expr(elem)
			}
		}
		ast.InitExpr {
			g.collect_map_types_from_expr(expr.typ)
			for field in expr.fields {
				g.collect_map_types_from_expr(field.value)
			}
		}
		ast.ParenExpr {
			g.collect_map_types_from_expr(expr.expr)
		}
		ast.LambdaExpr {
			g.collect_map_types_from_expr(expr.expr)
		}
		ast.OrExpr {
			g.collect_map_types_from_expr(expr.expr)
			for s in expr.stmts {
				g.collect_map_types_from_stmt(s)
			}
		}
		ast.RangeExpr {
			g.collect_map_types_from_expr(expr.start)
			g.collect_map_types_from_expr(expr.end)
		}
		ast.ComptimeExpr {
			g.collect_map_types_from_expr(expr.expr)
		}
		ast.ModifierExpr {
			// Handle pointer/reference types like &Scope
			g.collect_map_types_from_expr(expr.expr)
		}
		else {}
	}
}

// collect_map_types_from_type recursively collects map types from a type expression
fn (mut g Gen) collect_map_types_from_type(typ ast.Type) {
	match typ {
		ast.MapType {
			key_type := sanitize_type_for_mangling(g.expr_type_to_c(typ.key_type))
			value_type := sanitize_type_for_mangling(g.expr_type_to_c(typ.value_type))
			map_type_name := 'Map_${key_type}_${value_type}'
			// Recursively collect nested map types first
			g.collect_map_types_from_expr(typ.key_type)
			g.collect_map_types_from_expr(typ.value_type)
			// Skip generic type parameters (single uppercase letters)
			if is_generic_type_param(key_type) || is_generic_type_param(value_type) {
				return
			}
			// Now register this map type
			if map_type_name !in g.collected_map_types {
				g.collected_map_types[map_type_name] = MapTypeInfo{
					key_c_type:   get_c_runtime_type(key_type)
					value_c_type: get_c_runtime_type(value_type)
				}
			}
		}
		ast.ArrayType {
			elem_type := sanitize_type_for_mangling(g.expr_type_to_c(typ.elem_type))
			// Recursively collect nested types first
			g.collect_map_types_from_expr(typ.elem_type)
			// Skip generic type parameters
			if !is_generic_type_param(elem_type) {
				array_type_name := 'Array_${elem_type}'
				g.collected_array_types[array_type_name] = true
			}
		}
		ast.ArrayFixedType {
			elem_type := sanitize_type_for_mangling(g.expr_type_to_c(typ.elem_type))
			// Recursively collect nested types first
			g.collect_map_types_from_expr(typ.elem_type)
			// Skip generic type parameters
			if !is_generic_type_param(elem_type) {
				// Extract size from length expression
				mut size_str := '0'
				if typ.len is ast.BasicLiteral {
					size_str = typ.len.value
				}
				array_type_name := 'Array_fixed_${elem_type}_${size_str}'
				g.collected_fixed_array_types[array_type_name] = FixedArrayInfo{
					elem_type: elem_type
					size:      size_str
				}
			}
		}
		ast.OptionType {
			g.collect_map_types_from_expr(typ.base_type)
		}
		ast.ResultType {
			g.collect_map_types_from_expr(typ.base_type)
		}
		ast.FnType {
			for param in typ.params {
				g.collect_map_types_from_expr(param.typ)
			}
			g.collect_map_types_from_expr(typ.return_type)
		}
		ast.ChannelType {
			g.collect_map_types_from_expr(typ.elem_type)
		}
		ast.TupleType {
			for t in typ.types {
				g.collect_map_types_from_expr(t)
			}
		}
		else {}
	}
}

// is_generic_type_param checks if a type name is a generic type parameter (single uppercase letter)
fn is_generic_type_param(t string) bool {
	if t.len == 1 {
		c := t[0]
		return c >= `A` && c <= `Z`
	}
	return false
}

// get_c_runtime_type returns the actual C runtime type for a mangled type name
// Arrays become 'array', Maps become 'map', pointers get '*' suffix, etc.
fn get_c_runtime_type(mangled_type string) string {
	// Fixed arrays stay as their typedef name (they're actual C arrays, not 'array' struct)
	if mangled_type.starts_with('Array_fixed_') {
		return mangled_type
	}
	if mangled_type.starts_with('Array_') || mangled_type.starts_with('FixedArray_') {
		return 'array'
	}
	if mangled_type.starts_with('Map_') {
		return 'map'
	}
	// Handle pointer types - convert 'ptr' suffix back to '*'
	if mangled_type.ends_with('ptr') && !mangled_type.ends_with('voidptr')
		&& !mangled_type.ends_with('charptr') && !mangled_type.ends_with('byteptr') {
		return mangled_type[..mangled_type.len - 3] + '*'
	}
	return mangled_type
}

// sanitize_type_for_mangling converts C type names to safe identifiers for type mangling
fn sanitize_type_for_mangling(t string) string {
	return t.replace('*', 'ptr').replace(' ', '_')
}

// unsanitize_type_for_c converts a sanitized type name back to a valid C type
fn unsanitize_type_for_c(t string) string {
	// Only handle pointer suffixes - don't replace all underscores
	// as that would break module-qualified types like ssa__Type
	if t.ends_with('ptr') {
		return t[..t.len - 3] + '*'
	}
	return t
}

// is_pointer_type checks if a type name represents a pointer type
fn is_pointer_type(t string) bool {
	return t.ends_with('*') || t in ['voidptr', 'charptr', 'byteptr']
}

// type_for_c_decl converts a V type name to a valid C type for variable declaration
// For unknown Array_ types, it falls back to 'array'
// Also unsanitizes pointer types (e.g., types__Scopeptr -> types__Scope*)
// but NOT container types (Array_X, Map_X_Y) which keep mangled element types
fn (g &Gen) type_for_c_decl(t string) string {
	// Handle pointer types first - preserve the pointer suffix
	if t.ends_with('*') {
		base_type := t[..t.len - 1]
		return g.type_for_c_decl(base_type) + '*'
	}
	// Fixed arrays use their typedef directly
	if t.starts_with('Array_fixed_') {
		if t in g.collected_fixed_array_types {
			return t
		}
		// Fall back to array struct if not collected (shouldn't happen)
		return 'array'
	}
	if (t.starts_with('Array_') || t.starts_with('FixedArray_')) && t !in g.collected_array_types {
		return 'array'
	}
	// Don't unsanitize container types - they need mangled element types
	if t.starts_with('Array_') || t.starts_with('FixedArray_') || t.starts_with('Map_') {
		return t
	}
	// Unsanitize direct pointer types for C declarations
	return unsanitize_type_for_c(t)
}

// mangle_type_if_needed adds module prefix to type name if it's from the current module
fn (g Gen) mangle_type_if_needed(type_name string) string {
	// If already mangled (contains __), don't double-mangle
	if type_name.contains('__') {
		return type_name
	}
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
// For fixed-size arrays, returns FixedArray_<elem_type>_<size> so indexing can return elem_type
// and .len can return the compile-time size
// For dynamic arrays, returns Array_<elem_type>
fn (mut g Gen) get_field_type_for_inference(typ ast.Expr) string {
	match typ {
		ast.Type {
			if typ is ast.ArrayFixedType {
				elem_type := sanitize_type_for_mangling(g.expr_type_to_c(typ.elem_type))
				size_str := g.get_array_size_str(typ.len)
				return 'FixedArray_${elem_type}_${size_str}'
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
		t := g.type_for_c_decl(full_type)
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
		// Array constants need to be declared as global variables for runtime init
		// Skip hardcoded arrays (already defined with static initializers)
		hardcoded_arrays := ['bits__ntz_8_tab', 'bits__de_bruijn32tab', 'bits__de_bruijn64tab',
			'bits__pop_8_tab', 'bits__rev_8_tab', 'bits__len_8_tab', 'rune_maps',
			'strconv__ten_pow_table_64', 'strconv__ten_pow_table_32', 'strconv__dec_round',
			'strconv__pow5_inv_split_64_x', 'strconv__pow5_split_64_x', 'strconv__pow5_inv_split_32',
			'strconv__pow5_split_32', 'strconv__pos_exp', 'strconv__neg_exp']
		if t.starts_with('Array_') {
			if mangled_name in hardcoded_arrays {
				g.global_var_types[mangled_name] = t
				continue
			}
			g.sb.writeln('${t} ${mangled_name}; // runtime-initialized array constant')
			g.global_var_types[mangled_name] = t
			continue
		}
		// FixedArray_<elem>_<size> constants need proper C array declaration
		if t.starts_with('FixedArray_') {
			// Parse: FixedArray_<elem_type>_<size> -> elem_type name[size]
			rest := t['FixedArray_'.len..]
			parts := rest.split('_')
			if parts.len >= 2 {
				size := parts[parts.len - 1]
				elem_type := parts[..parts.len - 1].join('_')
				if mangled_name in hardcoded_arrays {
					// Hardcoded arrays are declared as dynamic array structs in cleanc
					// so we need to use Array_* type for proper indexing via .data
					array_type := 'Array_${elem_type}'
					g.global_var_types[mangled_name] = array_type
					// Also update const_types to use the dynamic array type
					g.const_types[mangled_name] = array_type
					g.const_types[field.name] = array_type
					continue
				}
				g.sb.writeln('${elem_type} ${mangled_name}[${size}]; // runtime-initialized constant')
				g.global_var_types[mangled_name] = t
				continue
			}
		}
		// Track integer constant values for fixed-size arrays
		if val := g.try_eval_int_const(field.value) {
			g.const_values[field.name] = val
			g.const_values[mangled_name] = val
		}
		// Only generate simple literal constants
		if !g.is_simple_literal(field.value) {
			// Generate as global variable (initialized at runtime) for complex constants
			g.sb.writeln('${t} ${mangled_name}; // runtime-initialized constant')
			g.global_var_types[mangled_name] = t
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
		// Mark constant as emitted (for forward reference checking in other constants)
		g.emitted_consts[mangled_name] = true
		g.emitted_consts[field.name] = true
	}
}

// try_eval_int_const tries to evaluate an expression to a constant integer value
fn (g Gen) try_eval_int_const(e ast.Expr) ?string {
	match e {
		ast.BasicLiteral {
			if e.kind == .number {
				return e.value
			}
		}
		ast.Ident {
			// Try to resolve reference to another constant
			if val := g.const_values[e.name] {
				return val
			}
		}
		ast.PrefixExpr {
			if e.op == .minus {
				if val := g.try_eval_int_const(e.expr) {
					return '-${val}'
				}
			}
		}
		ast.ParenExpr {
			return g.try_eval_int_const(e.expr)
		}
		ast.InfixExpr {
			// Try to evaluate simple arithmetic
			left := g.try_eval_int_const(e.lhs) or { return none }
			right := g.try_eval_int_const(e.rhs) or { return none }
			left_val := left.int()
			right_val := right.int()
			result := match e.op {
				.plus {
					left_val + right_val
				}
				.minus {
					left_val - right_val
				}
				.mul {
					left_val * right_val
				}
				.div {
					if right_val != 0 { left_val / right_val } else { 0 }
				}
				.left_shift {
					left_val << right_val
				}
				.right_shift {
					left_val >> right_val
				}
				.amp {
					left_val & right_val
				}
				.pipe {
					left_val | right_val
				}
				.xor {
					left_val ^ right_val
				}
				.mod {
					if right_val != 0 { left_val % right_val } else { 0 }
				}
				else {
					return none
				}
			}
			return result.str()
		}
		else {}
	}
	return none
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
				return g.is_simple_literal_or_const_ref(e.lhs)
					&& g.is_simple_literal_or_const_ref(e.rhs)
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
			// Only allow identifiers that are C builtin constants or keywords
			// Module constants cause ordering issues, so generate them at runtime
			return e.name in ['true', 'false', 'nil', 'NULL']
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

// is_simple_literal_or_const_ref checks if expression is a simple literal or a reference to an already-emitted constant
fn (g Gen) is_simple_literal_or_const_ref(e ast.Expr) bool {
	if g.is_simple_literal(e) {
		return true
	}
	// Also allow identifiers that refer to already-emitted constants
	// This prevents forward references in enum declarations (C requires declaration before use)
	if e is ast.Ident {
		// Check if this constant has already been emitted (exists in emitted_consts)
		if e.name in g.emitted_consts {
			return true
		}
		// Also check the mangled version via const_mangled map
		if mangled := g.const_mangled[e.name] {
			return mangled in g.emitted_consts
		}
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
	g.gen_enum_decl_with_name(node, node.name)
}

fn (mut g Gen) gen_enum_decl_with_name(node ast.EnumDecl, mangled_name string) {
	// Check if any fields reference C constants
	mut has_c_refs := false
	for field in node.fields {
		if g.expr_references_c(field.value) {
			has_c_refs = true
			break
		}
	}
	// For enums with C constant values, generate #define macros
	if has_c_refs {
		for field in node.fields {
			g.sb.write_string('#define ${mangled_name}__${field.name} ')
			if field.value !is ast.EmptyExpr {
				g.gen_expr(field.value)
			} else {
				g.sb.write_string('0')
			}
			g.sb.writeln('')
		}
		return
	}
	is_flag := node.attributes.has('flag')
	// Generate C enum declaration
	g.sb.writeln('typedef enum {')
	for i, field in node.fields {
		g.sb.write_string('\t${mangled_name}__${field.name}')
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
	g.sb.writeln('} ${mangled_name};')
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

				// Get the parameter types for this interface method
				meth_params := g.interface_meth_params[iface_name][meth] or { []string{} }

				// Build the parameter list string for the wrapper signature and call
				mut wrapper_params := 'void* _obj'
				mut call_params := []string{}
				for i, param_type in meth_params {
					wrapper_params += ', ${param_type} _p${i}'
					call_params << '_p${i}'
				}
				call_params_str := call_params.join(', ')

				// Check if method actually exists for this type
				if mangled !in g.fn_types {
					// Method doesn't exist - generate a stub
					if ret_type == 'void' || ret_type == '' {
						g.sb.writeln('static void ${iface_name}_${type_name}_${meth}_wrapper(${wrapper_params}) {')
						g.sb.writeln('\t(void)_obj; // No-op - method not implemented')
						for i, _ in meth_params {
							g.sb.writeln('\t(void)_p${i};')
						}
						g.sb.writeln('}')
					} else {
						g.sb.writeln('static ${ret_type} ${iface_name}_${type_name}_${meth}_wrapper(${wrapper_params}) {')
						g.sb.writeln('\t(void)_obj; // Stub - method not implemented')
						for i, _ in meth_params {
							g.sb.writeln('\t(void)_p${i};')
						}
						g.sb.writeln('\treturn (${ret_type}){0};')
						g.sb.writeln('}')
					}
					g.sb.writeln('')
					continue
				}

				// Generate: RetType InterfaceName_TypeName_method_wrapper(void* _obj, params...) {
				//     return TypeName__method((TypeName*)_obj, params...); // for mut/ref receiver
				//     return TypeName__method(*(TypeName*)_obj, params...); // for value receiver
				// }
				// Check if receiver is mutable or reference (takes pointer), or value (needs deref)
				is_ptr_receiver := g.mut_receivers[mangled] || g.ref_receivers[mangled]
				receiver_arg := if is_ptr_receiver {
					'(${type_name}*)_obj'
				} else {
					'*(${type_name}*)_obj'
				}
				// Build the full call argument list
				full_call_args := if call_params_str != '' {
					'${receiver_arg}, ${call_params_str}'
				} else {
					receiver_arg
				}
				if ret_type == 'void' || ret_type == '' {
					g.sb.writeln('static void ${iface_name}_${type_name}_${meth}_wrapper(${wrapper_params}) {')
					g.sb.writeln('\t${type_name}__${meth}(${full_call_args});')
					g.sb.writeln('}')
				} else {
					g.sb.writeln('static ${ret_type} ${iface_name}_${type_name}_${meth}_wrapper(${wrapper_params}) {')
					g.sb.writeln('\treturn ${type_name}__${meth}(${full_call_args});')
					g.sb.writeln('}')
				}
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

// gen_malloc generates either a direct malloc call or a context allocator call
// depending on the use_context_allocator preference. This enables allocation profiling.
fn (g &Gen) gen_malloc(type_name string) string {
	if g.pref != unsafe { nil } && g.pref.use_context_allocator {
		// Use context allocator for profiling support
		return '(${type_name}*)v2_profiler_alloc(sizeof(${type_name}))'
	}
	// Default: direct malloc call
	return '(${type_name}*)malloc(sizeof(${type_name}))'
}

// is_smartcast_to_sum_type_variant checks if an expression is a smartcast pattern
// where the variant type is a known variant of some sum type.
fn (g &Gen) is_smartcast_to_sum_type_variant(expr ast.Expr, arg_type string) bool {
	// First check if this is a smartcast pattern
	orig_var := g.extract_smartcast_original_var(expr)
	if orig_var == '' {
		return false
	}
	// Get the simple name of arg_type (e.g., ast__TypeDecl -> TypeDecl)
	arg_type_simple := arg_type.all_after_last('__')
	// Check if arg_type is a variant of any known sum type
	for _, variants in g.sum_type_variants {
		for v in variants {
			// Compare both simple and full names
			if v == arg_type || v == arg_type_simple {
				return true
			}
		}
	}
	return false
}

// extract_smartcast_original_var extracts the original variable from a smartcast pattern.
// Smartcast pattern: (*((VariantType*)(var._data._Variant))) -> returns "var"
// Returns empty string if not a smartcast pattern.
fn (g &Gen) extract_smartcast_original_var(expr ast.Expr) string {
	// Pattern: ParenExpr(PrefixExpr(*)(CastExpr(SelectorExpr(SelectorExpr(Ident(var), _data), _Variant))))
	if expr is ast.ParenExpr {
		if expr.expr is ast.PrefixExpr {
			prefix := expr.expr as ast.PrefixExpr
			if prefix.op == token.Token.mul {
				if prefix.expr is ast.CastExpr {
					cast := prefix.expr as ast.CastExpr
					if cast.expr is ast.SelectorExpr {
						sel := cast.expr as ast.SelectorExpr
						// Check if accessing ._data._Variant pattern
						if sel.lhs is ast.SelectorExpr {
							inner_sel := sel.lhs as ast.SelectorExpr
							if inner_sel.rhs.name == '_data' && inner_sel.lhs is ast.Ident {
								return (inner_sel.lhs as ast.Ident).name
							}
						}
					}
				}
			}
		}
	}
	return ''
}

// gen_sumtype_wrap generates sum type wrapping code for a variant value.
// Returns true if wrapping was generated, false if variant wasn't found.
// The generated code is: (SumType){._tag = N, ._data._variant = boxed_value}
fn (mut g Gen) gen_sumtype_wrap(sumtype_name string, variant_type string, expr ast.Expr) bool {
	variants := g.sum_type_variants[sumtype_name] or { return false }
	mut tag_value := -1
	for vi, v in variants {
		if v == variant_type || '${g.cur_module}__${v}' == variant_type
			|| v == variant_type.all_after('__') || variant_type == v.all_after_last('__') {
			tag_value = vi
			break
		}
	}
	if tag_value < 0 {
		return false
	}
	// Use short variant name for union field (e.g., _Ident not _ast__Ident)
	short_variant := variant_type.all_after_last('__')
	variant_field := '_${short_variant}'
	g.sb.write_string('(${sumtype_name}){._tag = ${tag_value}, ._data.${variant_field} = ')
	if variant_type in ['int', 'i64', 'i32', 'i16', 'i8', 'u64', 'u32', 'u16', 'u8', 'byte', 'rune',
		'bool', 'f32', 'f64', 'usize', 'isize'] {
		// Primitives: store value in pointer space via intptr_t cast
		g.sb.write_string('(void*)(intptr_t)(')
		g.gen_expr(expr)
		g.sb.write_string(')')
	} else {
		// Structs/strings: heap allocate to ensure lifetime
		g.sb.write_string('({ ${variant_type}* _tmp = ${g.gen_malloc(variant_type)}; *_tmp = (')
		g.gen_expr(expr)
		g.sb.write_string('); (void*)_tmp; })')
	}
	g.sb.write_string('}')
	return true
}

// gen_sumtype_return generates a return statement with sum type wrapping if needed.
// Handles the common pattern of returning a variant value where sum type is expected.
fn (mut g Gen) gen_sumtype_return(sumtype_name string, variant_type string, expr ast.Expr) {
	g.write_indent()
	// If variant_type is already the sum type, no wrapping needed
	if variant_type == sumtype_name {
		g.sb.write_string('return ')
		g.gen_expr(expr)
		g.sb.writeln(';')
		return
	}
	// Try to wrap as a variant
	g.sb.write_string('return ')
	if !g.gen_sumtype_wrap(sumtype_name, variant_type, expr) {
		// Unknown variant, just return as-is
		g.gen_expr(expr)
	}
	g.sb.writeln(';')
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
	if node.is_method && node.receiver.name != '' {
		// Static methods like `fn Type.new()` have is_method=true but no receiver name
		receiver_type = g.expr_type_to_c(node.receiver.typ)
		receiver_name = node.receiver.name
		receiver_is_ptr = node.receiver.is_mut
		has_receiver = true
	}

	// Special case: main function takes argc/argv
	if node.name == 'main' {
		g.sb.write_string('${ret} ${fn_name}(int ___argc, char** ___argv)')
		return
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

	// Record start position and function name for -printfn support
	g.fn_start_pos = g.sb.len
	g.last_fn_c_name = g.get_fn_name(node)

	// Set the current function's return type for proper return statement generation
	ret_expr := node.typ.return_type
	g.cur_fn_returns_result = false
	g.cur_fn_result_base_type = ''
	g.cur_fn_returns_option = false
	g.cur_fn_option_base_type = ''
	if ret_expr !is ast.EmptyExpr {
		// Check if this is a Result or Option type
		if ret_expr is ast.Type {
			if ret_expr is ast.ResultType {
				g.cur_fn_returns_result = true
				g.cur_fn_result_base_type = g.expr_type_to_c(ret_expr.base_type)
			} else if ret_expr is ast.OptionType {
				g.cur_fn_returns_option = true
				g.cur_fn_option_base_type = g.expr_type_to_c(ret_expr.base_type)
			}
		}
		g.cur_fn_ret_type = g.expr_type_to_c(ret_expr)
	} else {
		g.cur_fn_ret_type = 'void'
	}
	// Special case: main returns int in C
	if node.name == 'main' {
		g.cur_fn_ret_type = 'int'
	}

	// Set current function name for @FN and @METHOD comptime values
	g.cur_fn_name = node.name

	// Skip modules with complex smartcast patterns that require nested match/if support
	// These modules use patterns like: match stmt { FnDecl { if stmt.x is Type { stmt.x.field } } }
	// which require both match smartcast and nested if smartcast to work together.
	// The transformer currently only handles one level of smartcast context.
	// Exception: new, new_with_env can be generated correctly (new_with_env is just a wrapper)
	if g.cur_module in ['v', 'cleanc', 'x64', 'transformer', 'types', 'parser', 'binary', 'sha256', 'term']
		&& node.name !in ['new', 'new_with_env'] {
		g.gen_fn_head(node)
		g.sb.writeln(' {')
		g.indent++
		g.write_indent()
		// Return appropriate default based on return type
		mut is_ptr_return := false
		if node.typ.return_type is ast.PrefixExpr {
			if (node.typ.return_type as ast.PrefixExpr).op == .amp {
				is_ptr_return = true
			}
		}
		if is_ptr_return {
			g.sb.writeln('return NULL; // TODO: Stub - nested smartcast')
			g.indent--
			g.sb.writeln('}')
			return
		}
		// Check for array return type
		mut is_array_return := false
		mut array_elem_type := ast.Expr(ast.empty_expr)
		match node.typ.return_type {
			ast.Type {
				match node.typ.return_type {
					ast.ArrayType {
						is_array_return = true
						array_elem_type = node.typ.return_type.elem_type
					}
					else {}
				}
			}
			else {}
		}
		if is_array_return {
			elem_c_type := g.expr_type_to_c(array_elem_type)
			g.sb.writeln('return new_array_from_c_array(0, 0, sizeof(${elem_c_type}), (${elem_c_type}[0]){}); // TODO: Stub')
		} else {
			ret_type_raw := if node.typ.return_type is ast.Ident {
				(node.typ.return_type as ast.Ident).name
			} else {
				''
			}
			ret_type := if ret_type_raw != ''
				&& ret_type_raw !in ['string', 'bool', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'void', 'voidptr', 'IError'] {
				'${g.cur_module}__${ret_type_raw}'
			} else {
				ret_type_raw
			}
			if ret_type == 'string' {
				g.sb.writeln('return (string){"", 0}; // TODO: Stub')
			} else if ret_type == 'bool' {
				g.sb.writeln('return false; // TODO: Stub')
			} else if ret_type == '' || ret_type == 'void' {
				g.sb.writeln('// TODO: Stub')
			} else if ret_type in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64'] {
				g.sb.writeln('return 0; // TODO: Stub')
			} else if ret_type in ['f32', 'f64'] {
				g.sb.writeln('return 0.0; // TODO: Stub')
			} else {
				g.sb.writeln('return (${ret_type}){0}; // TODO: Stub')
			}
		}
		g.indent--
		g.sb.writeln('}')
		return
	}

	// Register receiver for methods (skip static methods with no receiver)
	if node.is_method && node.receiver.name != '' {
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

	// Initialize os.args from argc/argv in main
	if node.name == 'main' {
		g.write_indent()
		g.sb.writeln('g_main_argc = ___argc;')
		g.write_indent()
		g.sb.writeln('g_main_argv = ___argv;')
		// Only initialize os__args if os module is imported
		if 'os' in g.module_names {
			g.write_indent()
			g.sb.writeln('os__args = arguments();')
		}
	}

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
	// Print function source if in -printfn list and exit
	if g.pref != unsafe { nil } && g.pref.printfn_list.len > 0
		&& g.last_fn_c_name in g.pref.printfn_list {
		println(g.sb.after(g.fn_start_pos))
		exit(0)
	}
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
					c_typ := g.type_for_c_decl(typ)
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
				// Check if RHS is an IfExpr that can't be ternary - need to hoist
				if rhs is ast.IfExpr && !g.can_be_ternary(rhs) && rhs.else_expr !is ast.EmptyExpr {
					g.gen_decl_if_expr(name, rhs)
					return
				}
				// Check if RHS is error() - declare as IError
				if rhs is ast.CallOrCastExpr {
					if rhs.lhs is ast.Ident {
						if rhs.lhs.name == 'error' || rhs.lhs.name == 'error_with_code' {
							// error() creates an IError value
							g.var_types[name] = 'IError'
							g.write_indent()
							g.sb.write_string('IError ${name} = ')
							g.gen_expr(rhs)
							g.sb.writeln(';')
							return
						}
					}
				}
				typ := g.infer_type(rhs)
				g.var_types[name] = typ
				// Handle fixed-size array declarations specially
				if rhs is ast.ArrayInitExpr {
					if arr_info := g.get_fixed_array_info_from_init(rhs) {
						// Generate C array declaration: int name[6] = {0}
						g.sb.write_string('${arr_info.elem_type} ${name}[${arr_info.size}] = ')
						g.gen_expr(rhs)
						g.sb.writeln(';')
						return
					}
				}
				c_typ := g.type_for_c_decl(typ)
				g.sb.write_string('${c_typ} ${name} = ')
				g.gen_expr(rhs)
				g.sb.writeln(';')
			} else {
				// assignment
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
				old_match_type := g.cur_match_type
				lhs_type := g.infer_type(lhs)
				if lhs_type in g.enum_names || lhs_type in g.flag_enum_names {
					g.cur_match_type = lhs_type
				}
				// Check if assigning a variant to a sum type variable
				if lhs_type in g.sum_type_names {
					rhs_type := g.infer_type(rhs)
					// Only wrap if RHS is not already the sum type
					if rhs_type != lhs_type && rhs_type !in g.sum_type_names {
						if g.gen_sumtype_wrap(lhs_type, rhs_type, rhs) {
							g.sb.writeln(';')
							g.cur_match_type = old_match_type
							return
						}
					}
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
				if g.cur_fn_returns_option {
					// Wrap tuple in Option: ({ _option_Tuple _o; *(Tuple*)_o.data = (Tuple){...}; _o.state = 0; _o; })
					g.sb.write_string('return ({ ${g.cur_fn_ret_type} _o; *(${g.cur_fn_option_base_type}*)_o.data = (${g.cur_fn_option_base_type}){')
					for i, expr in node.exprs {
						if i > 0 {
							g.sb.write_string(', ')
						}
						g.gen_expr(expr)
					}
					g.sb.writeln('}; _o.state = 0; _o; });')
				} else if g.cur_fn_returns_result {
					// Wrap tuple in Result: ({ _result_Tuple _r; *(Tuple*)_r.data = (Tuple){...}; _r.is_error = false; _r; })
					g.sb.write_string('return ({ ${g.cur_fn_ret_type} _r; *(${g.cur_fn_result_base_type}*)_r.data = (${g.cur_fn_result_base_type}){')
					for i, expr in node.exprs {
						if i > 0 {
							g.sb.write_string(', ')
						}
						g.gen_expr(expr)
					}
					g.sb.writeln('}; _r.is_error = false; _r; });')
				} else {
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
				}
			} else if node.exprs.len > 0 && g.defer_stmts.len > 0 {
				// In V/Go semantics: evaluate return value FIRST, then run defer
				// Store return value in temp variable before running defers
				expr_type := g.infer_type(node.exprs[0])
				if expr_type == 'void' {
					// For void expressions (like panic), just call - panic doesn't return
					g.write_indent()
					g.gen_expr(node.exprs[0])
					g.sb.writeln(';')
					// Note: deferred stmts won't run after panic, but emit them for completeness
					g.emit_deferred_stmts()
				} else if g.cur_fn_ret_type == 'void' {
					// Void function but expression has value - just call and ignore result
					g.write_indent()
					g.sb.write_string('(void)(')
					g.gen_expr(node.exprs[0])
					g.sb.writeln(');')
					g.emit_deferred_stmts()
					g.write_indent()
					g.sb.writeln('return;')
				} else {
					// Check if this is an error return - handle before value wrapping
					if g.cur_fn_returns_result && g.is_error_return(node.exprs[0]) {
						// Error return with defer - store error in Result, then run defers
						g.write_indent()
						if g.cur_fn_ret_type == '_result' {
							// Bare Result - no data field
							g.sb.write_string('${g.cur_fn_ret_type} __ret_val = (${g.cur_fn_ret_type}){ .is_error=true, .err=')
						} else {
							g.sb.write_string('${g.cur_fn_ret_type} __ret_val = (${g.cur_fn_ret_type}){ .is_error=true, .err=')
						}
						g.gen_error_expr(node.exprs[0])
						if g.cur_fn_ret_type == '_result' {
							g.sb.writeln(' };')
						} else {
							g.sb.writeln(', .data={0} };')
						}
						g.emit_deferred_stmts()
						g.write_indent()
						g.sb.writeln('return __ret_val;')
					} else {
						// For Result/Option returning functions, check if we need to wrap
						ret_expr_type := g.infer_type(node.exprs[0])
						if g.cur_fn_returns_result && ret_expr_type != g.cur_fn_ret_type {
							// Wrap value in Result before storing
							g.write_indent()
							g.sb.write_string('${g.cur_fn_ret_type} __ret_val = ({ ${g.cur_fn_ret_type} _r; *(${g.cur_fn_result_base_type}*)_r.data = ')
							g.gen_expr(node.exprs[0])
							g.sb.writeln('; _r.is_error = false; _r; });')
						} else if g.cur_fn_returns_option && ret_expr_type != g.cur_fn_ret_type {
							// Wrap value in Option before storing
							g.write_indent()
							g.sb.write_string('${g.cur_fn_ret_type} __ret_val = ({ ${g.cur_fn_ret_type} _o; *(${g.cur_fn_option_base_type}*)_o.data = ')
							g.gen_expr(node.exprs[0])
							g.sb.writeln('; _o.state = 0; _o; });')
						} else {
							g.write_indent()
							g.sb.write_string('${g.cur_fn_ret_type} __ret_val = ')
							g.gen_expr(node.exprs[0])
							g.sb.writeln(';')
						}
						// Emit deferred statements
						g.emit_deferred_stmts()
						// Return the stored value
						g.write_indent()
						g.sb.writeln('return __ret_val;')
					}
				}
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
					if is_none {
						g.write_indent()
						if g.cur_fn_returns_option {
							// Return proper Option wrapper with state=2 (none)
							g.sb.writeln('return (${g.cur_fn_ret_type}){ .state=2, .data={0} };')
						} else {
							// Fallback: return appropriate zero/default value for the return type
							if g.cur_fn_ret_type.starts_with('Tuple_') {
								// Return zero-initialized tuple
								g.sb.writeln('return (${g.cur_fn_ret_type}){{0}};')
							} else if g.cur_fn_ret_type == 'string' {
								// Return empty string
								g.sb.writeln('return (string){"", 0};')
							} else if g.cur_fn_ret_type.starts_with('Array_')
								|| g.cur_fn_ret_type == 'array' {
								// Return empty array
								g.sb.writeln('return (${g.cur_fn_ret_type}){0};')
							} else if g.cur_fn_ret_type.starts_with('Map_') {
								// Return empty map
								g.sb.writeln('return (${g.cur_fn_ret_type}){0};')
							} else if g.cur_fn_ret_type.ends_with('*')
								|| g.cur_fn_ret_type == 'voidptr' {
								// Return NULL for pointers
								g.sb.writeln('return 0;')
							} else if g.cur_fn_ret_type in ['int', 'i8', 'i16', 'i32', 'i64', 'u8',
								'u16', 'u32', 'u64', 'f32', 'f64', 'bool', 'rune'] {
								// Return 0 for primitives
								g.sb.writeln('return 0;')
							} else if g.cur_fn_ret_type == 'void' {
								// Just return for void
								g.sb.writeln('return;')
							} else {
								// Return zero-initialized struct
								g.sb.writeln('return (${g.cur_fn_ret_type}){0};')
							}
						}
					} else if expr is ast.MatchExpr {
						// Handle match expression in return - generate if-else chain
						g.gen_return_match_expr(expr)
					} else if g.is_error_return(expr) {
						// Handle return error(...) - wrap in Result type
						if g.cur_fn_returns_result {
							// Generate: return (_result_T){ .is_error=true, .err=error_expr, .data={0} }
							// But for bare _result (no value type), omit .data
							g.write_indent()
							g.sb.write_string('return (${g.cur_fn_ret_type}){ .is_error=true, .err=')
							g.gen_error_expr(expr)
							if g.cur_fn_ret_type == '_result' {
								g.sb.writeln(' };')
							} else {
								g.sb.writeln(', .data={0} };')
							}
						} else {
							// Fallback to panic for non-Result functions
							g.write_indent()
							g.sb.write_string('panic(')
							if expr is ast.CallExpr {
								if expr.args.len > 0 {
									g.gen_expr(expr.args[0])
								} else {
									g.sb.write_string('(string){"error", 5}')
								}
							} else if expr is ast.CallOrCastExpr {
								g.gen_expr(expr.expr)
							} else {
								g.sb.write_string('(string){"error", 5}')
							}
							g.sb.writeln(');')
						}
					} else if expr is ast.Ident {
						// Check if returning an IError variable (from err := error(msg))
						id := expr as ast.Ident
						if var_type := g.var_types[id.name] {
							if var_type == 'IError' {
								if g.cur_fn_returns_result {
									// Wrap error in Result type
									g.write_indent()
									g.sb.write_string('return (${g.cur_fn_ret_type}){ .is_error=true, .err=')
									g.gen_expr(expr)
									if g.cur_fn_ret_type == '_result' {
										g.sb.writeln(' };')
									} else {
										g.sb.writeln(', .data={0} };')
									}
								} else {
									// Fallback to panic
									g.write_indent()
									g.sb.writeln('panic((string){"error", 5});')
								}
								return
							}
						}
						// Regular value return
						if g.cur_fn_returns_result {
							// Wrap value in Result type: ({ _result_T _r; *(T*)_r.data = value; _r.is_error = false; _r; })
							g.write_indent()
							g.sb.write_string('return ({ ${g.cur_fn_ret_type} _r; *(${g.cur_fn_result_base_type}*)_r.data = ')
							g.gen_expr(expr)
							g.sb.writeln('; _r.is_error = false; _r; });')
						} else if g.cur_fn_returns_option {
							// Wrap value in Option type: ({ _option_T _o; *(T*)_o.data = value; _o.state = 0; _o; })
							g.write_indent()
							g.sb.write_string('return ({ ${g.cur_fn_ret_type} _o; *(${g.cur_fn_option_base_type}*)_o.data = ')
							g.gen_expr(expr)
							g.sb.writeln('; _o.state = 0; _o; });')
						} else if g.cur_fn_ret_type in g.sum_type_names {
							// Sum type return - wrap variant in sum type struct
							variant_type := g.infer_type(expr)
							g.gen_sumtype_return(g.cur_fn_ret_type, variant_type, expr)
						} else {
							g.write_indent()
							g.sb.write_string('return ')
							g.gen_expr(expr)
							g.sb.writeln(';')
						}
					} else if g.cur_fn_ret_type in g.interface_names {
						// Returning to an interface type - may need boxing
						// Check if the expression is already the same interface type (no boxing needed)
						if expr is ast.InitExpr {
							expr_type := g.expr_type_to_c(expr.typ)
							if expr_type == g.cur_fn_ret_type {
								// Already the correct interface type - just return it
								g.write_indent()
								g.sb.write_string('return ')
								g.gen_expr(expr)
								g.sb.writeln(';')
							} else {
								// Different struct type, needs boxing
								g.write_indent()
								g.sb.write_string('return ')
								g.gen_expr(expr)
								g.sb.writeln(';')
							}
						} else if expr is ast.PrefixExpr && expr.op == .amp
							&& expr.expr is ast.InitExpr {
							// Boxing a heap-allocated struct into an interface
							init_expr := expr.expr as ast.InitExpr
							concrete_type := g.expr_type_to_c(init_expr.typ)
							// Skip if already the interface type
							if concrete_type == g.cur_fn_ret_type {
								g.write_indent()
								g.sb.write_string('return ')
								g.gen_expr(expr)
								g.sb.writeln(';')
							} else {
								type_id := g.get_or_assign_type_id(concrete_type)
								g.write_indent()
								g.sb.write_string('return ({ ')
								g.sb.write_string('${concrete_type}* _iface_obj = ${g.gen_malloc(concrete_type)}; ')
								g.sb.write_string('*_iface_obj = (${concrete_type}){')
								for i, field in init_expr.fields {
									if i > 0 {
										g.sb.write_string(', ')
									}
									g.sb.write_string('.${escape_c_keyword(field.name)} = ')
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
							}
						} else {
							g.write_indent()
							g.sb.write_string('return ')
							g.gen_expr(expr)
							g.sb.writeln(';')
						}
					} else {
						// Check if this is a panic/exit call (returns void, never returns)
						if g.is_error_propagation(expr) {
							// Generate panic call without return, then return default value
							g.write_indent()
							g.gen_expr(expr)
							g.sb.writeln(';')
							g.write_indent()
							if g.cur_fn_ret_type == 'void' {
								g.sb.writeln('return;')
							} else {
								g.sb.writeln('return (${g.cur_fn_ret_type}){0};')
							}
						} else if g.cur_fn_returns_result {
							// Check if the expression already returns the same Result type
							expr_type := g.infer_type(expr)
							if expr_type == g.cur_fn_ret_type {
								// Already a Result of the same type - return directly
								g.write_indent()
								g.sb.write_string('return ')
								g.gen_expr(expr)
								g.sb.writeln(';')
							} else {
								// Wrap value in Result type for successful return
								g.write_indent()
								g.sb.write_string('return ({ ${g.cur_fn_ret_type} _r; *(${g.cur_fn_result_base_type}*)_r.data = ')
								// Set enum context for shorthand resolution in return expression
								old_match_type := g.cur_match_type
								if g.cur_fn_result_base_type in g.enum_names {
									g.cur_match_type = g.cur_fn_result_base_type
								}
								g.gen_expr(expr)
								g.cur_match_type = old_match_type
								g.sb.writeln('; _r.is_error = false; _r; });')
							}
						} else if g.cur_fn_returns_option {
							// Check if the expression already returns the same Option type
							expr_type := g.infer_type(expr)
							if expr_type == g.cur_fn_ret_type {
								// Already an Option of the same type - return directly
								g.write_indent()
								g.sb.write_string('return ')
								g.gen_expr(expr)
								g.sb.writeln(';')
							} else {
								// Wrap value in Option type for successful return
								g.write_indent()
								g.sb.write_string('return ({ ${g.cur_fn_ret_type} _o; *(${g.cur_fn_option_base_type}*)_o.data = ')
								// Set enum context for shorthand resolution in return expression
								old_match_type := g.cur_match_type
								if g.cur_fn_option_base_type in g.enum_names {
									g.cur_match_type = g.cur_fn_option_base_type
								}
								g.gen_expr(expr)
								g.cur_match_type = old_match_type
								g.sb.writeln('; _o.state = 0; _o; });')
							}
						} else if g.cur_fn_ret_type in g.sum_type_names {
							// Sum type return - wrap variant in sum type struct
							variant_type := g.infer_type(expr)
							g.gen_sumtype_return(g.cur_fn_ret_type, variant_type, expr)
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
					}
				} else {
					// No return expression
					g.write_indent()
					if g.cur_fn_returns_result {
						// For Result-returning functions, return success with no data
						if g.cur_fn_ret_type == '_result' {
							g.sb.writeln('return (_result){ .is_error=false };')
						} else {
							g.sb.writeln('return (${g.cur_fn_ret_type}){ .is_error=false, .data={0} };')
						}
					} else if g.cur_fn_returns_option {
						// For Option-returning functions, return success with no data
						if g.cur_fn_ret_type == '_option' {
							g.sb.writeln('return (_option){ .state=0 };')
						} else {
							g.sb.writeln('return (${g.cur_fn_ret_type}){ .state=0, .data={0} };')
						}
					} else if g.cur_fn_ret_type == 'int' {
						// For int-returning functions (like main), return 0
						g.sb.writeln('return 0;')
					} else {
						g.sb.writeln('return;')
					}
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
			} else if node.op == .key_goto {
				g.sb.writeln('goto ${node.label};')
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
			// Handle nil literal - convert to C NULL
			if node.name == 'nil' {
				g.sb.write_string('NULL')
				return
			}
			// Handle comptime identifiers that start with @
			if node.name.starts_with('@') {
				// Handle known comptime values
				if node.name == '@VCURRENTHASH' {
					// Return a placeholder string for the V compiler hash
					g.sb.write_string('(string){"VCURRENTHASH", 12}')
				} else if node.name == '@FN' || node.name == '@METHOD' {
					// Return the current function name as a string
					fn_name := g.cur_fn_name
					g.sb.write_string('(string){"${fn_name}", ${fn_name.len}}')
				} else if node.name == '@LINE' {
					g.sb.write_string('__LINE__')
				} else if node.name == '@FILE' {
					g.sb.write_string('__FILE__')
				} else if node.name == '@VEXE' {
					// Return a path that when os.dir() is applied, gives the V root
					// Find vroot by walking up directories from cwd looking for vlib
					// For now, use a simple heuristic: go up from cwd/argv[0] until we find vlib
					g.sb.write_string('__vexe_path()') // Helper function defined in runtime
				} else {
					// Unknown comptime value - return placeholder
					g.sb.write_string('(string){"<unknown>", 9} /* unknown comptime: ${node.name} */')
				}
			} else if node.name.starts_with('__type_id_') {
				// Type ID placeholder from transformer - resolve to actual type ID
				type_name := node.name['__type_id_'.len..]
				type_id := g.get_or_assign_type_id(type_name)
				g.sb.write_string('${type_id}')
			} else if node.name in g.var_types {
				// Local variable - use as-is
				g.sb.write_string(node.name)
			} else if mangled := g.const_mangled[node.name] {
				// Use mangled name for constants from other modules
				g.sb.write_string(mangled)
			} else if mangled := g.fn_mangled[node.name] {
				// Use mangled name for functions from current module (e.g., used as function pointers)
				g.sb.write_string(mangled)
			} else if g.cur_match_type != '' {
				// Enum shorthand value - resolve using match type context
				// This is the fallback when the identifier isn't a known variable, constant, or function
				g.sb.write_string('${g.cur_match_type}__${node.name}')
			} else {
				// Try module-qualified function name for current module
				if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
					cur_module_fn := '${g.cur_module}__${node.name}'
					if cur_module_fn in g.fn_types {
						g.sb.write_string(cur_module_fn)
					} else {
						g.sb.write_string(node.name)
					}
				} else {
					g.sb.write_string(node.name)
				}
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
				// Get struct field types for FixedArray detection and enum context resolution
				// Try both the type name directly and with module prefix
				mut field_types := map[string]string{}
				if typ_name in g.struct_fields {
					field_types = g.struct_fields[typ_name].clone()
				} else if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
					mangled_typ := '${g.cur_module}__${typ_name}'
					if mangled_typ in g.struct_fields {
						field_types = g.struct_fields[mangled_typ].clone()
					}
				}
				// Use a compound literal approach - create on heap
				g.sb.write_string('({ ${typ_name}* _tmp = ${g.gen_malloc(typ_name)}; *_tmp = (${typ_name}){')
				for i, field in init_expr.fields {
					if i > 0 {
						g.sb.write_string(', ')
					}
					g.sb.write_string('.${escape_c_keyword(field.name)} = ')
					// Handle fixed-size array fields specially
					field_type := field_types[field.name] or { '' }
					// Set enum context if field type is an enum (for shorthand resolution)
					old_match_type := g.cur_match_type
					if field_type in g.enum_names || field_type in g.flag_enum_names {
						g.cur_match_type = field_type
					}
					if field_type.starts_with('FixedArray_') && field.value is ast.ArrayInitExpr {
						arr_init := field.value as ast.ArrayInitExpr
						g.sb.write_string('{')
						for j, elem in arr_init.exprs {
							if j > 0 {
								g.sb.write_string(', ')
							}
							g.gen_expr(elem)
						}
						g.sb.write_string('}')
					} else if field_type in g.sum_type_names {
						// Sum type field - check if value needs wrapping
						value_type := g.infer_type(field.value)
						if value_type == field_type || value_type in g.sum_type_names {
							g.gen_expr(field.value)
						} else if !g.gen_sumtype_wrap(field_type, value_type, field.value) {
							g.gen_expr(field.value)
						}
					} else {
						g.gen_expr(field.value)
					}
					// Restore enum context
					g.cur_match_type = old_match_type
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
				} else if cast_expr.lhs is ast.SelectorExpr {
					// Handle &C.TYPE(expr) -> (C__TYPE*)(expr)
					sel := cast_expr.lhs as ast.SelectorExpr
					if sel.lhs is ast.Ident && sel.lhs.name == 'C' {
						c_type := 'C__${sel.rhs.name}'
						g.sb.write_string('((${c_type}*)(')
						g.gen_expr(cast_expr.expr)
						g.sb.write_string('))')
						return
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
			// Check for address-of map index: &m[key] where m is a map
			// This is used for nested map assignment: outer[k1][k2] = val
			// transforms to: __Map_inner_set(&outer[k1], k2, val)
			// We need: (Map_inner*)builtin__map_get_and_set((map*)&outer, &k1, &default)
			if node.op == .amp && node.expr is ast.IndexExpr {
				index_expr := node.expr as ast.IndexExpr
				lhs_type := g.infer_type(index_expr.lhs)
				if lhs_type.starts_with('Map_') {
					// Parse Map_<key>_<value> to extract key and value types
					rest := lhs_type['Map_'.len..]
					// Find the key type (common patterns: string_, int_, i64_, u64_, bool_)
					mut key_type := ''
					mut val_type := ''
					key_patterns := ['string_', 'int_', 'i64_', 'u64_', 'bool_', 'u8_', 'i8_',
						'i16_', 'u16_', 'i32_', 'u32_', 'rune_']
					for kp in key_patterns {
						if rest.starts_with(kp) {
							key_type = kp[..kp.len - 1] // Remove trailing _
							val_type = rest[kp.len..]
							break
						}
					}
					if key_type != '' && val_type != '' {
						// Generate: (val_type*)builtin__map_get_and_set((map*)&m, &(key_type[]){key}, &(val_type[]){default})
						// Get default value for value type
						default_val := if val_type.starts_with('Map_') {
							'__new_${val_type}()'
						} else if val_type.starts_with('Array_') {
							'(${val_type}){0}'
						} else if val_type == 'string' {
							'(string){"", 0}'
						} else if val_type in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32',
							'u64', 'bool', 'rune'] {
							'0'
						} else {
							'(${val_type}){0}'
						}
						g.sb.write_string('((${val_type}*)builtin__map_get_and_set((map*)&')
						g.gen_expr(index_expr.lhs)
						g.sb.write_string(', &(${key_type}[]){')
						g.gen_expr(index_expr.expr)
						g.sb.write_string('}, &(${val_type}[]){${default_val}}))')
						return
					}
				}
			}
			// Handle &ident where ident is a mut parameter (already a pointer)
			// In this case, skip the & since the parameter is already a pointer
			if node.op == .amp && node.expr is ast.Ident {
				if node.expr.name in g.mut_params {
					// mut parameter is already a pointer, don't take its address
					g.gen_expr(node.expr)
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

			// Statement IF
			// Check if this is `if x is Type { ... }` for smart-casting
			old_sumtype_match_var := g.cur_sumtype_match_var
			old_sumtype_match_variant := g.cur_sumtype_match_variant
			old_sumtype_match_type := g.cur_sumtype_match_type
			mut has_is_condition := false

			if node.cond is ast.InfixExpr {
				cond := node.cond as ast.InfixExpr
				if cond.op == .key_is {
					// Get the variable being checked
					if cond.lhs is ast.Ident {
						lhs_type := g.infer_type(cond.lhs)
						if lhs_type in g.sum_type_names {
							has_is_condition = true
							g.cur_sumtype_match_var = cond.lhs.name
							g.cur_sumtype_match_type = lhs_type

							// Get variant name - handle both Ident and SelectorExpr (module.Type)
							variant_name, variant_module := if cond.rhs is ast.Ident {
								cond.rhs.name, ''
							} else if cond.rhs is ast.SelectorExpr {
								sel := cond.rhs as ast.SelectorExpr
								module_name := if sel.lhs is ast.Ident {
									g.resolve_module_name((sel.lhs as ast.Ident).name)
								} else {
									''
								}
								sel.rhs.name, module_name
							} else {
								'', ''
							}

							// Store the fully qualified variant name for proper type resolution
							// If it has a module prefix, use that; otherwise, derive from sum type
							if variant_module != '' {
								g.cur_sumtype_match_variant = '${variant_module}__${variant_name}'
							} else if lhs_type.contains('__') {
								// Derive module from sum type: types__Object -> types
								parts := lhs_type.split('__')
								if parts.len >= 2 {
									g.cur_sumtype_match_variant = '${parts[0]}__${variant_name}'
								} else {
									g.cur_sumtype_match_variant = variant_name
								}
							} else {
								g.cur_sumtype_match_variant = g.mangle_type_if_needed(variant_name)
							}
						}
					}
				}
			}

			g.write_indent()
			g.sb.write_string('if (')
			g.gen_expr(node.cond)
			g.sb.writeln(') {')
			g.indent++

			g.gen_stmts(node.stmts)
			g.indent--

			// Restore smart-cast context
			if has_is_condition {
				g.cur_sumtype_match_var = old_sumtype_match_var
				g.cur_sumtype_match_variant = old_sumtype_match_variant
				g.cur_sumtype_match_type = old_sumtype_match_type
			}
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
			} else if match_type in g.sum_type_names {
				// Sum type matching - generate if-else chain checking _tag
				g.write_indent()
				g.sb.write_string('{ ')
				g.sb.write_string('${match_type} __match_val = ')
				g.gen_expr(node.expr)
				g.sb.writeln(';')

				// Get variant list for this sum type
				variants := g.sum_type_variants[match_type] or { []string{} }

				// Save sum type match context
				old_sumtype_match_var := g.cur_sumtype_match_var
				old_sumtype_match_orig := g.cur_sumtype_match_orig
				old_sumtype_match_variant := g.cur_sumtype_match_variant
				old_sumtype_match_type := g.cur_sumtype_match_type
				g.cur_sumtype_match_var = '__match_val'
				// Track original variable name for smart casting (e.g., "stmt" when matching stmt)
				g.cur_sumtype_match_orig = if node.expr is ast.Ident { node.expr.name } else { '' }
				g.cur_sumtype_match_type = match_type

				mut first := true
				for branch in node.branches {
					// Get variant name from first condition (for field access casting)
					mut branch_variant := ''
					if branch.cond.len > 0 {
						cond0 := branch.cond[0]
						if cond0 is ast.Ident {
							branch_variant = cond0.name
						} else if cond0 is ast.SelectorExpr {
							// ast.StructDecl -> ast__StructDecl (preserve module prefix)
							sel := cond0 as ast.SelectorExpr
							if sel.lhs is ast.Ident {
								mod_name := (sel.lhs as ast.Ident).name
								resolved_mod := g.resolve_module_name(mod_name)
								branch_variant = '${resolved_mod}__${sel.rhs.name}'
							} else {
								branch_variant = sel.rhs.name
							}
						}
					}

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
						// Generate tag comparisons
						for ci, c in branch.cond {
							if ci > 0 {
								g.sb.write_string(' || ')
							}
							// The condition should be a type name (Ident or SelectorExpr like ast.StructDecl)
							// Find the tag value (index in variants list)
							variant_name := if c is ast.Ident {
								c.name
							} else if c is ast.SelectorExpr {
								// ast.StructDecl -> StructDecl
								sel := c as ast.SelectorExpr
								sel.rhs.name
							} else {
								'unknown'
							}
							mut tag_value := -1
							for vi, v in variants {
								if v == variant_name {
									tag_value = vi
									break
								}
							}
							if tag_value >= 0 {
								g.sb.write_string('__match_val._tag == ${tag_value}')
							} else {
								g.sb.write_string('/* unknown variant: ${variant_name} */ 0')
							}
						}
						g.sb.writeln(') {')
					}
					g.indent++
					// Set variant context for this branch (for field access casting)
					g.cur_sumtype_match_variant = branch_variant
					g.gen_stmts(branch.stmts)
					g.indent--
					g.write_indent()
					g.sb.write_string('}')
				}
				// Restore sum type match context
				g.cur_sumtype_match_var = old_sumtype_match_var
				g.cur_sumtype_match_orig = old_sumtype_match_orig
				g.cur_sumtype_match_variant = old_sumtype_match_variant
				g.cur_sumtype_match_type = old_sumtype_match_type
				g.cur_match_type = old_match_type
				g.sb.writeln('')
				g.write_indent()
				g.sb.writeln('}')
			} else if g.has_range_condition(node) {
				// Match with range conditions - use if-else chain
				g.write_indent()
				g.sb.write_string('{ ')
				c_match_type := g.type_for_c_decl(match_type)
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
							} else if match_type == 'string' {
								// String comparison: string__eq(__match_val, c)
								g.sb.write_string('string__eq(__match_val, ')
								g.gen_expr(c)
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
				g.cur_match_type = old_match_type
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
							// DEBUG: check condition type
							if c is ast.SelectorExpr {
								if c.lhs is ast.EmptyExpr {
									g.sb.write_string('/* SelectorExpr.empty:${g.cur_match_type} */ ')
								} else {
									g.sb.write_string('/* SelectorExpr.nonempty */ ')
								}
							} else if c is ast.Ident {
								g.sb.write_string('/* Ident:${g.cur_match_type} */ ')
							} else {
								g.sb.write_string('/* OTHER */ ')
							}
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
			// Handle 'is' and '!is' operators for sum type checking
			if node.op == .key_is || node.op == .not_is {
				// x is Type => (x)._tag == TAG_VALUE
				// x !is Type => (x)._tag != TAG_VALUE
				lhs_type := g.infer_type(node.lhs)
				if lhs_type in g.sum_type_names {
					// Get variant name from RHS (can be an Ident or SelectorExpr like ast.ModuleStmt)
					variant_name := if node.rhs is ast.Ident {
						node.rhs.name
					} else if node.rhs is ast.SelectorExpr {
						// ast.ModuleStmt -> ModuleStmt
						sel := node.rhs as ast.SelectorExpr
						sel.rhs.name
					} else {
						''
					}
					// Find tag value for this variant
					variants := g.sum_type_variants[lhs_type] or { []string{} }
					mut tag_value := -1
					for vi, v in variants {
						if v == variant_name {
							tag_value = vi
							break
						}
					}
					if tag_value >= 0 {
						cmp_op := if node.op == .key_is { '==' } else { '!=' }
						g.sb.write_string('((')
						g.gen_expr(node.lhs)
						g.sb.write_string(')._tag ${cmp_op} ${tag_value})')
					} else {
						// Unknown variant - generate error marker
						g.sb.write_string('/* unknown variant: ${variant_name} */ 0')
					}
				} else {
					// Fallback for non-sum types
					g.sb.write_string('/* is on non-sum-type */ 0')
				}
			} else if node.op == .key_in || node.op == .not_in {
				if node.rhs is ast.ArrayInitExpr {
					// x in [a, b, c] => (x == a || x == b || x == c)
					// x !in [a, b, c] => (x != a && x != b && x != c)
					join_op := if node.op == .key_in { ' || ' } else { ' && ' }
					// Set enum context for shorthand resolution in array elements
					lhs_type_in := g.infer_type(node.lhs)
					old_match_type_in := g.cur_match_type
					if lhs_type_in in g.enum_names {
						g.cur_match_type = lhs_type_in
					}
					is_string := lhs_type_in == 'string'
					// Check if this is a sum type variant check: x in [TypeA, TypeB]
					is_sumtype_in := lhs_type_in in g.sum_type_names
					g.sb.write_string('(')
					for i, elem in node.rhs.exprs {
						if i > 0 {
							g.sb.write_string(join_op)
						}
						if is_sumtype_in && elem is ast.Ident {
							// Sum type variant check: x in [TypeA, TypeB] => (x._tag == TAG_A || x._tag == TAG_B)
							elem_ident := elem as ast.Ident
							variants := g.sum_type_variants[lhs_type_in] or { []string{} }
							mut tag_value := -1
							for vi, v in variants {
								if v == elem_ident.name {
									tag_value = vi
									break
								}
							}
							if tag_value >= 0 {
								cmp_op := if node.op == .key_in { '==' } else { '!=' }
								g.sb.write_string('((')
								g.gen_expr(node.lhs)
								g.sb.write_string(')._tag ${cmp_op} ${tag_value})')
							} else {
								// Unknown variant - treat as regular comparison
								cmp_op := if node.op == .key_in { '==' } else { '!=' }
								g.sb.write_string('(')
								g.gen_expr(node.lhs)
								g.sb.write_string(' ${cmp_op} ')
								g.gen_expr(elem)
								g.sb.write_string(')')
							}
						} else if is_string {
							// Use string__eq for string comparisons
							if node.op == .not_in {
								g.sb.write_string('!')
							}
							g.sb.write_string('string__eq(')
							g.gen_expr(node.lhs)
							g.sb.write_string(', ')
							g.gen_expr(elem)
							g.sb.write_string(')')
						} else {
							cmp_op := if node.op == .key_in { '==' } else { '!=' }
							g.sb.write_string('(')
							g.gen_expr(node.lhs)
							g.sb.write_string(' ${cmp_op} ')
							g.gen_expr(elem)
							g.sb.write_string(')')
						}
					}
					g.sb.write_string(')')
					g.cur_match_type = old_match_type_in
				} else {
					// Check if RHS is a map type
					rhs_type := g.infer_type(node.rhs)
					if rhs_type.starts_with('Map_') || rhs_type == 'map' {
						// x in map => map__exists(&map, ADDR(key_type, key))
						// x !in map => !map__exists(&map, ADDR(key_type, key))
						if node.op == .not_in {
							g.sb.write_string('!')
						}
						g.sb.write_string('map__exists(&')
						g.gen_expr(node.rhs)
						g.sb.write_string(', ')
						// Use ADDR macro for key to handle literals (can't take address of rvalue)
						// Extra parens around expr needed for compound literals with commas
						key_type := g.infer_type(node.lhs)
						g.sb.write_string('ADDR(${key_type}, (')
						g.gen_expr(node.lhs)
						g.sb.write_string(')))')
					} else if rhs_type.starts_with('Array_') {
						// x in arr => array__contains_T(arr, x) (for arrays)
						// x !in arr => !array__contains_T(arr, x)
						elem_type := rhs_type['Array_'.len..]
						if node.op == .not_in {
							g.sb.write_string('!')
						}
						g.sb.write_string('array__contains_${elem_type}(')
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
				// Check for map[key] << value pattern (map index returning array)
				if node.lhs is ast.IndexExpr {
					index_expr := node.lhs as ast.IndexExpr
					base_type := g.infer_type(index_expr.lhs)
					if base_type.starts_with('Map_') {
						// Parse Map_<key>_<value> to check if value is an Array type
						rest := base_type['Map_'.len..]
						// Find the key type (int_, string_, i64_, u64_, bool_)
						key_types := ['int_', 'string_', 'i64_', 'u64_', 'bool_']
						for key_prefix in key_types {
							if rest.starts_with(key_prefix) {
								val_type := rest[key_prefix.len..]
								if val_type.starts_with('Array_') {
									// Map index returning array: g.pending_labels[blk] << off
									// Generate: builtin__array_push_noscan(
									//   (array*)&(*(Array_T*)builtin__map_get_and_set(
									//     (map*)&map, &(K[]){key}, &(Array_T[]){ builtin____new_array_noscan(0, 0, sizeof(T)) })),
									//   _MOV((T[]){ value }))
									elem_type := val_type['Array_'.len..]
									key_type := key_prefix[..key_prefix.len - 1] // Remove trailing _
									g.sb.write_string('builtin__array_push_noscan((array*)&(*((${val_type}*)builtin__map_get_and_set((map*)&')
									g.gen_expr(index_expr.lhs)
									g.sb.write_string(', &(${key_type}[]){')
									g.gen_expr(index_expr.expr)
									g.sb.write_string('}, &(${val_type}[]){ builtin____new_array_noscan(0, 0, sizeof(${elem_type})) }))), _MOV((${elem_type}[]){')
									g.gen_expr(node.rhs)
									g.sb.write_string('}))')
									return
								}
								break
							}
						}
					}
				}
				// Regular bitshift (array append is handled by transformer)
				g.sb.write_string('(')
				g.gen_expr(node.lhs)
				g.sb.write_string(' << ')
				g.gen_expr(node.rhs)
				g.sb.write_string(')')
			} else if node.op in [.eq, .ne, .lt, .gt, .le, .ge] {
				// Comparison operators - string/array comparisons handled by transformer
				lhs_type := g.infer_type(node.lhs)
				old_match_type := g.cur_match_type
				if lhs_type in g.enum_names {
					g.cur_match_type = lhs_type
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
				g.cur_match_type = old_match_type
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
			mut is_c_call := false // Track if this is a C library function call
			mut receiver_expr := ast.empty_expr
			mut method_receiver_type := ''
			mut method_type := '' // Resolved type used for fn_params lookup
			if node.lhs is ast.Ident {
				name = node.lhs.name
				// Special handling for builtin error() function
				// Don't confuse with errors__error from v2/errors module
				if name == 'error' && node.args.len == 1 {
					if g.cur_fn_returns_result {
						// Generate proper error() call that returns IError
						g.sb.write_string('error(')
						g.gen_expr(node.args[0])
						g.sb.write_string(')')
					} else {
						// Simplified: just panic
						g.sb.write_string('panic(')
						g.gen_expr(node.args[0])
						g.sb.write_string(')')
					}
					return
				}
				if name == 'error_with_code' && node.args.len == 2 {
					if g.cur_fn_returns_result {
						// Generate proper error_with_code() call
						g.sb.write_string('error_with_code(')
						g.gen_expr(node.args[0])
						g.sb.write_string(', ')
						g.gen_expr(node.args[1])
						g.sb.write_string(')')
					} else {
						// Simplified: just panic
						g.sb.write_string('panic(')
						g.gen_expr(node.args[0])
						g.sb.write_string(')')
					}
					return
				}
				// Handle builtin__array_push_noscan - transformer generates this for array append
				// The second argument should be _MOV((elem_type[]){value}), not a full array
				if name == 'builtin__array_push_noscan' && node.args.len == 2 {
					g.sb.write_string('builtin__array_push_noscan(')
					g.gen_expr(node.args[0])
					g.sb.write_string(', ')
					// Generate the second argument as _MOV((type[]){value})
					// If it's a CallExpr to new_array_from_c_array_noscan, extract the _MOV part
					second_arg := node.args[1]
					if second_arg is ast.CallExpr {
						if second_arg.lhs is ast.Ident
							&& second_arg.lhs.name == 'builtin__new_array_from_c_array_noscan' {
							// Extract the last argument which is _MOV((type[]){val})
							if second_arg.args.len >= 4 {
								g.gen_expr(second_arg.args[3])
							} else {
								g.gen_expr(second_arg)
							}
						} else {
							g.gen_expr(second_arg)
						}
					} else if second_arg is ast.ArrayInitExpr {
						// Generate _MOV((type[]){value}) directly
						arr_len := second_arg.exprs.len
						if arr_len == 0 {
							g.sb.write_string('NULL')
						} else {
							mut c_elem_type := 'int'
							if second_arg.typ is ast.Type {
								if second_arg.typ is ast.ArrayType {
									c_elem_type = g.expr_type_to_c(second_arg.typ.elem_type)
								}
							}
							if c_elem_type == 'int' && second_arg.exprs.len > 0 {
								c_elem_type = g.infer_type(second_arg.exprs[0])
							}
							// Use unsanitize_type_for_c to convert mangled type names back to C types
							c_elem_type_for_c := unsanitize_type_for_c(c_elem_type)
							g.sb.write_string('_MOV((${c_elem_type_for_c}[${arr_len}]){')
							for i, expr in second_arg.exprs {
								if i > 0 {
									g.sb.write_string(', ')
								}
								g.gen_expr(expr)
							}
							g.sb.write_string('})')
						}
					} else {
						g.gen_expr(second_arg)
					}
					g.sb.write_string(')')
					return
				}
				// Handle builtin__new_map_init_noscan_value - transformer generates this for non-empty map init
				// Convert to: ({ MapType _m = __new_MapType(); __MapType_set(&_m, k1, v1); ...; _m; })
				if name == 'builtin__new_map_init_noscan_value' && node.args.len >= 9 {
					// args: hash_fn, eq_fn, clone_fn, free_fn, n, key_size, val_size, keys_arr, vals_arr
					// The keys and vals arrays contain the actual key/value pairs
					keys_arg := node.args[7]
					vals_arg := node.args[8]
					// Infer map type from keys/values or use a default
					// Extract key and value type from the arguments
					mut key_type := 'string'
					mut val_type := 'int'
					// Try to infer types from the arguments (they're array literals wrapped in _MOV)
					// For now, use string->int as default since that's what arr.v uses
					map_type := 'Map_${key_type}_${val_type}'
					g.sb.write_string('({ ${map_type} _m = __new_${map_type}(); ')
					// Generate set calls for each key-value pair
					// The keys and vals are ArrayInitExpr wrapped in CastExpr from transformer
					// Extract the arrays and iterate
					if keys_arg is ast.ArrayInitExpr && vals_arg is ast.ArrayInitExpr {
						for i, key in keys_arg.exprs {
							val := vals_arg.exprs[i]
							g.sb.write_string('__${map_type}_set(&_m, ')
							g.gen_expr(key)
							g.sb.write_string(', ')
							g.gen_expr(val)
							g.sb.write_string('); ')
						}
					}
					g.sb.write_string('_m; })')
					return
				}
			} else if node.lhs is ast.SelectorExpr {
				// Check if this is a C library call (C.putchar, C.puts, etc.)
				if node.lhs.lhs is ast.Ident {
					if node.lhs.lhs.name == 'C' {
						// C library function call
						name = node.lhs.rhs.name
						// Use _Exit for C.exit to avoid name collision with V's exit function
						if name == 'exit' {
							name = '_Exit'
						}
						is_method = false
						is_c_call = true
					} else if node.lhs.lhs.name in g.module_names {
						// Module-qualified function call: strconv.f64_to_str_l(x)
						// Resolve import alias if present
						resolved_mod := g.resolve_module_name(node.lhs.lhs.name)
						name = '${resolved_mod}__${node.lhs.rhs.name}'
						is_method = false
					} else {
						// Check if this is a static method call on a local type: TypeName.method()
						type_name := node.lhs.lhs.name
						mut is_local_type := false
						if types_in_module := g.module_types[g.cur_module] {
							if type_name in types_in_module {
								is_local_type = true
							}
						}
						if is_local_type {
							// Local type static method call: TypeName.method() -> module__TypeName__method()
							if g.cur_module != '' && g.cur_module != 'main'
								&& g.cur_module != 'builtin' {
								name = '${g.cur_module}__${type_name}__${node.lhs.rhs.name}'
							} else {
								name = '${type_name}__${node.lhs.rhs.name}'
							}
							is_method = false
						} else {
							// Regular method call: obj.method(args)
							name = node.lhs.rhs.name
							receiver_expr = node.lhs.lhs
							is_method = true
						}
					}
				} else if node.lhs.lhs is ast.SelectorExpr {
					// Chained selector - could be module.Type.method() or obj.field.method()
					inner_sel := node.lhs.lhs as ast.SelectorExpr
					// Explicitly check if this is a static method call: module.Type.method()
					mut is_static_method := false
					if inner_sel.lhs is ast.Ident {
						mod_name := (inner_sel.lhs as ast.Ident).name
						if mod_name in g.module_names {
							// module.Type.method() -> module__Type__method()
							// This is a static method call
							// Resolve import alias if present
							resolved_mod := g.resolve_module_name(mod_name)
							name = '${resolved_mod}__${inner_sel.rhs.name}__${node.lhs.rhs.name}'
							is_method = false
							is_static_method = true
						}
					}
					if !is_static_method {
						// Regular chained method call: obj.field.method()
						name = node.lhs.rhs.name
						receiver_expr = node.lhs.lhs
						is_method = true
					}
				} else {
					// Other chained selector
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
				mut receiver_type := g.infer_type(receiver_expr)
				mut sumtype_receiver_cast := false // Track if we need to cast sum type receiver

				// Check if receiver is the tracked sum type match variable (simple Ident case)
				// e.g., for `match expr { SelectorExpr { expr.name() } }` - expr is smart-cast to SelectorExpr
				if g.cur_sumtype_match_var != '' && g.cur_sumtype_match_var != '__selector__'
					&& g.cur_sumtype_match_variant != '' && receiver_type in g.sum_type_names {
					if receiver_expr is ast.Ident {
						ident := receiver_expr as ast.Ident
						// Check if this ident matches the original match variable or the temp __match_val
						if ident.name == g.cur_sumtype_match_var
							|| ident.name == g.cur_sumtype_match_orig {
							// Override type to variant type
							variant_type := if g.cur_sumtype_match_type.contains('__') {
								parts := g.cur_sumtype_match_type.split('__')
								if parts.len >= 2 {
									'${parts[0]}__${g.cur_sumtype_match_variant}'
								} else {
									g.cur_sumtype_match_variant
								}
							} else {
								g.mangle_type_if_needed(g.cur_sumtype_match_variant)
							}
							receiver_type = variant_type
							sumtype_receiver_cast = true // Mark that we need to cast
						}
					}
				}

				// Check if receiver matches tracked selector in sum type context
				// e.g., for `if se.lhs is SelectorExpr { se.lhs.leftmost() }`
				if g.cur_sumtype_match_var == '__selector__' && g.cur_sumtype_match_variant != ''
					&& receiver_type in g.sum_type_names {
					// Check if receiver_expr is the tracked selector (e.g., se.lhs)
					if receiver_expr is ast.SelectorExpr {
						sel := receiver_expr as ast.SelectorExpr
						if sel.lhs is ast.Ident {
							lhs_name := (sel.lhs as ast.Ident).name
							if lhs_name == g.cur_sumtype_match_selector_lhs
								&& sel.rhs.name == g.cur_sumtype_match_selector_rhs {
								// Override type to variant type
								variant_type := if g.cur_sumtype_match_type.contains('__') {
									parts := g.cur_sumtype_match_type.split('__')
									if parts.len >= 2 {
										'${parts[0]}__${g.cur_sumtype_match_variant}'
									} else {
										g.cur_sumtype_match_variant
									}
								} else {
									g.mangle_type_if_needed(g.cur_sumtype_match_variant)
								}
								receiver_type = variant_type
								sumtype_receiver_cast = true // Mark that we need to cast
							}
						}
					}
				}

				receiver_is_ptr := receiver_type.ends_with('*')
				// Clean up pointer prefix for type lookup
				// Also unsanitize pointer types (e.g., types__Scopeptr -> types__Scope)
				clean_type := if receiver_is_ptr {
					receiver_type[..receiver_type.len - 1]
				} else {
					// Unsanitize types with 'ptr' suffix (from sanitize_type_for_mangling)
					unsanitize_type_for_c(receiver_type).trim_right('*')
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
					// first/last take array by value, not pointer
					if name in ['first', 'last'] && clean_type.starts_with('Array_') {
						elem_type := clean_type[6..] // Remove 'Array_' prefix
						g.sb.write_string('(*(${elem_type}*)array__${name}(')
						if receiver_is_ptr {
							g.sb.write_string('*')
						}
						g.gen_expr(receiver_expr)
						g.sb.write_string('))')
						return
					}
					// Handle pop on typed arrays - need to cast from void*
					// pop takes array* (pointer)
					if name == 'pop' && clean_type.starts_with('Array_') {
						elem_type := clean_type[6..] // Remove 'Array_' prefix
						g.sb.write_string('(*(${elem_type}*)array__pop(')
						if receiver_is_ptr {
							g.gen_expr(receiver_expr)
						} else {
							// Check if receiver is a map index (rvalue - can't take address directly)
							mut is_map_index := false
							if receiver_expr is ast.IndexExpr {
								idx_expr := receiver_expr as ast.IndexExpr
								recv_lhs_type := g.infer_type(idx_expr.lhs)
								if recv_lhs_type.starts_with('Map_') {
									is_map_index = true
									// Use ADDR macro for rvalue
									g.sb.write_string('ADDR(${clean_type}, ')
									g.gen_expr(receiver_expr)
									g.sb.write_string(')')
								}
							}
							if !is_map_index {
								g.sb.write_string('&')
								g.gen_expr(receiver_expr)
							}
						}
						g.sb.write_string('))')
						return
					}
					// Handle prepend/insert - need to wrap value arg in _MOV for void* param
					if name in ['prepend', 'insert'] && clean_type.starts_with('Array_') {
						elem_type := clean_type[6..] // Remove 'Array_' prefix
						if receiver_is_ptr {
							g.sb.write_string('array__${name}(')
							g.gen_expr(receiver_expr)
						} else {
							g.sb.write_string('array__${name}(&')
							g.gen_expr(receiver_expr)
						}
						g.sb.write_string(', ')
						if name == 'insert' && node.args.len >= 2 {
							// insert(index, val) - first arg is index
							g.gen_expr(node.args[0])
							g.sb.write_string(', ')
							// Wrap the value argument
							g.sb.write_string('_MOV((${elem_type}[]){')
							g.gen_expr(node.args[1])
							g.sb.write_string('}))')
						} else if node.args.len >= 1 {
							// prepend(val) - only arg is value
							g.sb.write_string('_MOV((${elem_type}[]){')
							g.gen_expr(node.args[0])
							g.sb.write_string('}))')
						} else {
							g.sb.write_string(')')
						}
						return
					}
					// Handle index on typed arrays - need type-specific comparison
					// For string arrays, use string__eq; for other types, use ==
					if name == 'index' && clean_type.starts_with('Array_') && node.args.len >= 1 {
						elem_type := clean_type[6..] // Remove 'Array_' prefix
						// Generate inline search with type-specific comparison
						g.sb.write_string('({ int _idx = -1; ${elem_type}* _p = ')
						if receiver_is_ptr {
							g.sb.write_string('(*')
							g.gen_expr(receiver_expr)
							g.sb.write_string(')')
						} else {
							g.gen_expr(receiver_expr)
						}
						g.sb.write_string('.data; for (int _i = 0; _i < ')
						if receiver_is_ptr {
							g.sb.write_string('(*')
							g.gen_expr(receiver_expr)
							g.sb.write_string(')')
						} else {
							g.gen_expr(receiver_expr)
						}
						g.sb.write_string('.len; _i++, _p++) { if (')
						// Use string__eq for string arrays, == for others
						if elem_type == 'string' {
							g.sb.write_string('string__eq(*_p, ')
							g.gen_expr(node.args[0])
							g.sb.write_string(')')
						} else {
							g.sb.write_string('*_p == ')
							g.gen_expr(node.args[0])
						}
						g.sb.write_string(') { _idx = _i; break; } } _idx; })')
						return
					}
				}

				// Handle builtin map methods (delete, clear)
				// Use map__delete/map__clear like v2 generates
				if clean_type.starts_with('Map_') {
					if name == 'delete' && node.args.len == 1 {
						// map.delete(key) -> map__delete(&m, &(KeyType[]){key})
						// Infer key type from the argument
						key_type := g.infer_type(node.args[0])
						g.sb.write_string('map__delete(')
						if receiver_is_ptr {
							g.gen_expr(receiver_expr)
						} else {
							g.sb.write_string('&')
							g.gen_expr(receiver_expr)
						}
						g.sb.write_string(', &(${key_type}[]){')
						g.gen_expr(node.args[0])
						g.sb.write_string('})')
						return
					}
					if name == 'clear' {
						// map.clear() -> map__clear(&m)
						g.sb.write_string('map__clear(')
						if receiver_is_ptr {
							g.gen_expr(receiver_expr)
						} else {
							g.sb.write_string('&')
							g.gen_expr(receiver_expr)
						}
						g.sb.write_string(')')
						return
					}
					if name == 'clone' {
						// map.clone() -> map__clone(&m)
						g.sb.write_string('map__clone(')
						if receiver_is_ptr {
							g.gen_expr(receiver_expr)
						} else {
							g.sb.write_string('&')
							g.gen_expr(receiver_expr)
						}
						g.sb.write_string(')')
						return
					}
				}

				// Handle .type_name() method on sum types
				if clean_type in g.sum_type_names && name == 'type_name' {
					// For now, return a placeholder string since we don't have runtime variant info
					// TODO: Implement proper type_name that uses a switch on _tag
					g.sb.write_string('(string){"<sumtype>", 9}')
					return
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
						// Skip first arg if it's the _object selector added by transformer
						// (since we already pass the dereferenced concrete object above)
						mut start_idx := 0
						if node.args.len > 0 {
							if node.args[0] is ast.SelectorExpr {
								sel := node.args[0] as ast.SelectorExpr
								if sel.rhs.name == '_object' {
									start_idx = 1
								}
							}
						}
						if node.args.len > start_idx {
							g.sb.write_string(', ')
						}
						for i := start_idx; i < node.args.len; i++ {
							if i > start_idx {
								g.sb.write_string(', ')
							}
							g.gen_expr(node.args[i])
						}
						g.sb.write_string(')')
						return
					}
					// Interface method call: transformer already added _object as first argument
					// Generate vtable call: receiver.method(args) - args already include _object
					g.gen_expr(receiver_expr)
					g.sb.write_string('.${name}(')
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
					// Try module-prefixed form: Array_module__ElemType -> module__Array_ElemType
					// e.g., Array_ast__Expr -> ast__Array_Expr
					elem_type := if method_type.starts_with('Array_') {
						method_type[6..]
					} else {
						method_type[11..] // FixedArray_
					}
					if elem_type.contains('__') {
						parts := elem_type.split('__')
						if parts.len >= 2 {
							mod_name := parts[0]
							base_elem := parts[1..].join('__')
							alt_mangled := '${mod_name}__Array_${base_elem}__${name}'
							if alt_mangled in g.fn_types {
								method_type = '${mod_name}__Array_${base_elem}'
								mangled = alt_mangled
							}
						}
					}
					// Fall back to generic array method if still not found
					if mangled !in g.fn_types {
						method_type = 'array'
						mangled = '${method_type}__${name}'
					}
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
				// - If sum type cast needed: cast and dereference the receiver
				// - Method wants pointer (mut or &Type): pass &receiver (or receiver if already ptr)
				// - Method wants value: pass receiver (or *receiver if ptr)
				if sumtype_receiver_cast {
					// Sum type receiver inside is-check: cast to variant type
					// e.g., se.lhs -> *((SelectorExpr*)&se.lhs)
					if method_wants_ptr {
						g.sb.write_string('((${method_receiver_type}*)&')
						g.gen_expr(receiver_expr)
						g.sb.write_string(')')
					} else {
						g.sb.write_string('(*((${method_receiver_type}*)&')
						g.gen_expr(receiver_expr)
						g.sb.write_string('))')
					}
				} else if method_wants_ptr {
					if receiver_is_ptr {
						// Already a pointer, pass as-is
						g.gen_expr(receiver_expr)
					} else {
						// Need to pass address
						// Check if receiver is a map index (rvalue - can't take address directly)
						mut is_map_idx := false
						if receiver_expr is ast.IndexExpr {
							idx_expr2 := receiver_expr as ast.IndexExpr
							recv_lhs_type := g.infer_type(idx_expr2.lhs)
							if recv_lhs_type.starts_with('Map_') {
								is_map_idx = true
								// Use ADDR macro for rvalue
								g.sb.write_string('ADDR(${clean_type}, ')
								g.gen_expr(receiver_expr)
								g.sb.write_string(')')
							}
						}
						if !is_map_idx {
							g.sb.write_string('&')
							g.gen_expr(receiver_expr)
						}
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
				// For same-module function calls, first try current module prefix
				mut call_name := name
				// Don't transform C library function calls - they should use the raw C function name
				if !is_c_call {
					// Also try method receiver's module if we're in a method
					cur_mod := g.cur_module
					// If function name is in cur_module, use that
					if cur_mod != '' && cur_mod != 'main' && cur_mod != 'builtin' {
						cur_module_fn := '${cur_mod}__${name}'
						if cur_module_fn in g.fn_types {
							call_name = cur_module_fn
						} else if mangled := g.fn_mangled[name] {
							call_name = mangled
						}
					} else if mangled := g.fn_mangled[name] {
						call_name = mangled
					}
				}
				// Special handling for builtin__array_push_noscan when it reaches this fallback path
				// The transformer generates this, but it may not have been caught by the earlier handler
				if call_name == 'builtin__array_push_noscan' && node.args.len == 2 {
					g.sb.write_string('builtin__array_push_noscan(')
					g.gen_expr(node.args[0])
					g.sb.write_string(', ')
					// Extract _MOV from builtin__new_array_from_c_array_noscan if present
					second_arg := node.args[1]
					if second_arg is ast.CallExpr {
						if second_arg.lhs is ast.Ident
							&& second_arg.lhs.name == 'builtin__new_array_from_c_array_noscan' {
							// Extract the last argument which is _MOV((type[]){val})
							if second_arg.args.len >= 4 {
								g.gen_expr(second_arg.args[3])
							} else {
								g.gen_expr(second_arg)
							}
						} else {
							g.gen_expr(second_arg)
						}
					} else if second_arg is ast.ArrayInitExpr {
						// Generate _MOV((type[]){value}) directly
						arr_len := second_arg.exprs.len
						if arr_len == 0 {
							g.sb.write_string('NULL')
						} else {
							mut c_elem_type := 'int'
							if second_arg.typ is ast.Type {
								if second_arg.typ is ast.ArrayType {
									c_elem_type = g.expr_type_to_c(second_arg.typ.elem_type)
								}
							}
							if c_elem_type == 'int' && second_arg.exprs.len > 0 {
								c_elem_type = g.infer_type(second_arg.exprs[0])
							}
							c_elem_type_for_c := unsanitize_type_for_c(c_elem_type)
							g.sb.write_string('_MOV((${c_elem_type_for_c}[${arr_len}]){')
							for i, expr in second_arg.exprs {
								if i > 0 {
									g.sb.write_string(', ')
								}
								g.gen_expr(expr)
							}
							g.sb.write_string('})')
						}
					} else {
						g.gen_expr(second_arg)
					}
					g.sb.write_string(')')
					return
				}
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
			mut expected_params := g.fn_params[fn_key] or { []string{} }
			if expected_params.len == 0 {
				// Try with mangled name (for same-module function calls)
				mk := g.fn_mangled[name] or { '' }
				if mk != '' {
					expected_params = g.fn_params[mk] or { []string{} }
				}
			}
			// Fallback: also try method_type if set (for non-method module-qualified calls)
			if expected_params.len == 0 && !is_method && method_type != '' {
				expected_params = g.fn_params[method_type] or { []string{} }
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
			// Check if last param is variadic (VArg_type) and we need to wrap args
			mut variadic_start_idx := -1
			mut variadic_elem_type := ''
			mut variadic_wrapper_type := ''
			if expected_params.len > 0 {
				last_param := expected_params[expected_params.len - 1]
				if last_param.starts_with('VArg_') {
					variadic_start_idx = expected_params.len - 1
					variadic_elem_type = last_param['VArg_'.len..]
					variadic_wrapper_type = last_param
				}
			}
			for i, arg in node.args {
				// If we've reached variadic arguments, wrap them in VArg struct
				if variadic_start_idx >= 0 && i == variadic_start_idx {
					if i > 0 {
						g.sb.write_string(', ')
					}
					// Check if this is a vararg splat (e.g., ...pt) - pass through directly
					if arg is ast.PrefixExpr && arg.op == .ellipsis {
						// Vararg splat: pass the VArg directly without re-wrapping
						g.gen_expr(arg.expr)
						break
					}
					// Collect all remaining args into VArg struct
					variadic_count := node.args.len - i
					// Generate: (VArg_type){.data = (elem_type[]){arg1, arg2, ...}, .len = N}
					g.sb.write_string('(${variadic_wrapper_type}){.data = (${variadic_elem_type}[]){')
					for j := i; j < node.args.len; j++ {
						if j > i {
							g.sb.write_string(', ')
						}
						g.gen_expr(node.args[j])
					}
					g.sb.write_string('}, .len = ${variadic_count}}')
					break // We've handled all remaining args as variadic
				}
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
							g.sb.write_string('.${escape_c_keyword(field_init.name)} = ')
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
						} else if arg.expr is ast.Ident && arg.expr.name in g.mut_params {
							// Inner expression is a mut parameter (already a pointer), don't add &
							g.gen_expr(arg.expr)
						} else if arg.expr is ast.IndexExpr
							&& (arg.expr as ast.IndexExpr).expr is ast.RangeExpr {
							// Mutable slice argument - need to wrap in compound literal
							// Generate: &(array[]){array__slice(...)}[0]
							g.sb.write_string('&(array[]){')
							index_expr := arg.expr as ast.IndexExpr
							g.gen_slice_expr(index_expr.lhs, index_expr.expr as ast.RangeExpr)
							g.sb.write_string('}[0]')
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
						g.sb.write_string('(${param_type}){.${escape_c_keyword(arg.name)} = ')
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
					} else if param_type.ends_with('*') {
						// Parameter expects pointer - check if arg is already a pointer
						arg_type := g.infer_type(arg)
						if !is_pointer_type(arg_type) && !(arg is ast.PrefixExpr
							&& (arg as ast.PrefixExpr).op == .amp) {
							// Need to take address
							g.sb.write_string('&')
						}
						g.gen_expr(arg)
					} else if is_c_call {
						// For C function calls, check if we need to cast function pointer arguments
						// SignalHandler is void(*)(Signal) but C signal() expects void(*)(int)
						arg_type := g.infer_type(arg)
						if arg_type.ends_with('SignalHandler') {
							g.sb.write_string('((void (*)(int))(')
							g.gen_expr(arg)
							g.sb.write_string('))')
						} else {
							g.gen_expr(arg)
						}
					} else {
						// Check if IError needs to be converted to string
						arg_type := g.infer_type(arg)
						// Convert IError to string for print functions or when param expects string
						is_print_fn := name in ['println', 'eprintln', 'print', 'eprint']
						if arg_type == 'IError' && (param_type == 'string' || is_print_fn) {
							// Convert IError to string: IError_str(err)
							g.sb.write_string('IError_str(')
							g.gen_expr(arg)
							g.sb.write_string(')')
						} else if g.is_smartcast_to_sum_type_variant(arg, arg_type) {
							// Arg is a smartcast pattern - check if param expects sum type or variant
							// If param expects the sum type, pass the original variable (un-smartcast)
							// If param expects the variant type, pass the smartcast pattern as-is
							orig_var := g.extract_smartcast_original_var(arg)
							if param_type in g.sum_type_names {
								// Param expects sum type, but arg is smartcast to variant
								// Pass the original variable (the sum type)
								g.sb.write_string(orig_var)
							} else {
								// Param expects the variant type, pass the smartcast
								g.gen_expr(arg)
							}
						} else if param_type in g.sum_type_names {
							// Wrap value in sum type struct
							if !g.gen_sumtype_wrap(param_type, arg_type, arg) {
								g.gen_expr(arg)
							}
						} else {
							// Check if arg is a mut parameter being passed to by-value param
							// In V, `mut m ssa.Module` is a pointer (Module*) internally
							// When passed to `fn(m ssa.Module)` (by value), need to dereference
							mut needs_deref := false
							if arg is ast.Ident {
								// Check if this identifier is a mut parameter in current function
								if arg.name in g.mut_params {
									// Check if target param expects non-pointer type
									// (param_type doesn't end with * and is not empty)
									if param_type != '' && !param_type.ends_with('*')
										&& !param_type.ends_with('ptr') {
										needs_deref = true
									}
								}
							}
							if needs_deref {
								g.sb.write_string('*')
							}
							g.gen_expr(arg)
						}
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
			// Check for Option type cast: ?string(x) wraps x in an Option
			// Use expr_type_to_c to check if it's an option type
			lhs_type := g.expr_type_to_c(node.lhs)
			if lhs_type.starts_with('_option_') {
				base_type := lhs_type['_option_'.len..]
				// Generate: ({ _option_T _o; *(T*)_o.data = expr; _o.state = 0; _o; })
				g.sb.write_string('({ ${lhs_type} _o; *(${base_type}*)_o.data = ')
				g.gen_expr(node.expr)
				g.sb.write_string('; _o.state = 0; _o; })')
				return
			}
			mut name := ''
			if node.lhs is ast.Ident {
				name = node.lhs.name
				// Special handling for builtin error() function
				// Don't confuse with errors__error from v2/errors module
				if name == 'error' {
					if g.cur_fn_returns_result {
						// Generate proper error() call that returns IError
						g.sb.write_string('error(')
						g.gen_expr(node.expr)
						g.sb.write_string(')')
					} else {
						// Simplified: just panic
						g.sb.write_string('panic(')
						g.gen_expr(node.expr)
						g.sb.write_string(')')
					}
					return
				}
				if name == 'error_with_code' {
					if g.cur_fn_returns_result {
						// Generate proper error_with_code() call
						g.sb.write_string('error_with_code(')
						g.gen_expr(node.expr)
						g.sb.write_string(')')
					} else {
						// Simplified: just panic
						g.sb.write_string('panic(')
						g.gen_expr(node.expr)
						g.sb.write_string(')')
					}
					return
				}
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
					// Skip boxing if concrete type is same as interface (no-op cast)
					if concrete_type == name {
						g.gen_expr(node.expr)
						return
					}
					// Get unique type ID for this concrete type
					type_id := g.get_or_assign_type_id(concrete_type)
					// Generate interface struct initialization with vtable
					g.sb.write_string('({ ')
					// Allocate heap memory for the object
					g.sb.write_string('${concrete_type}* _iface_obj = ${g.gen_malloc(concrete_type)}; ')
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
					// Check if this is a sum type construction: SumType(variant_value)
					// Sum types need special initialization with tag and union field
					if mangled_type in g.sum_type_names {
						variant_type := g.infer_type(node.expr)
						if g.gen_sumtype_wrap(mangled_type, variant_type, node.expr) {
							return
						}
					}
					g.sb.write_string('((${mangled_type})(')
					g.gen_expr(node.expr)
					g.sb.write_string('))')
					return
				}
			} else if node.lhs is ast.SelectorExpr {
				// Check for C library call or type cast (C.putchar, C.FILE, etc.)
				if node.lhs.lhs is ast.Ident && node.lhs.lhs.name == 'C' {
					c_name := node.lhs.rhs.name
					// Check if this is a C type (like FILE, DIR, stat, etc.)
					// C types typically start with uppercase or are all lowercase for struct types
					// For C type casts, generate (TypeName*)(expr) for pointer conversion
					if c_name in ['FILE', 'DIR', 'stat', 'dirent', 'IError'] {
						// C type cast: &C.FILE(ptr) should become (FILE*)(ptr)
						// C.FILE(ptr) alone should become *((FILE*)(ptr))
						g.sb.write_string('((${c_name}*)(')
						g.gen_expr(node.expr)
						g.sb.write_string('))')
						return
					}
					// C function call: C.getenv(key) -> getenv(key)
					// Use _Exit for C.exit to avoid name collision with V's exit function
					actual_name := if c_name == 'exit' { '_Exit' } else { c_name }
					g.sb.write_string('${actual_name}(')
					g.gen_expr(node.expr)
					g.sb.write_string(')')
					return
				} else if node.lhs.lhs is ast.Ident {
					lhs_name := node.lhs.lhs.name
					// Check if this is a local type static method call: TypeName.method(arg)
					mut is_local_type := false
					if types_in_module := g.module_types[g.cur_module] {
						if lhs_name in types_in_module {
							is_local_type = true
						}
					}
					if is_local_type {
						// Local type static method call: TypeName.method(arg) -> module__TypeName__method(arg)
						fn_name := if g.cur_module != '' && g.cur_module != 'main'
							&& g.cur_module != 'builtin' {
							'${g.cur_module}__${lhs_name}__${node.lhs.rhs.name}'
						} else {
							'${lhs_name}__${node.lhs.rhs.name}'
						}
						// Check if param needs pointer conversion
						expected_params := g.fn_params[fn_name] or { []string{} }
						g.sb.write_string('${fn_name}(')
						if expected_params.len > 0 && expected_params[0].ends_with('*') {
							arg_type := g.infer_type(node.expr)
							if !is_pointer_type(arg_type) && !(node.expr is ast.PrefixExpr
								&& (node.expr as ast.PrefixExpr).op == .amp) {
								g.sb.write_string('&')
							}
						}
						g.gen_expr(node.expr)
						g.sb.write_string(')')
						return
					} else if lhs_name in g.module_names {
						// Resolve import alias if present
						resolved_mod := g.resolve_module_name(lhs_name)
						type_or_fn_name := node.lhs.rhs.name
						mangled_name := '${resolved_mod}__${type_or_fn_name}'
						// Check if this is a module-qualified type cast: time.Duration(x)
						// (not a function call)
						// Check both original module name and resolved name for types
						mut found_type := false
						if types_in_module := g.module_types[resolved_mod] {
							if type_or_fn_name in types_in_module {
								found_type = true
							}
						}
						if !found_type {
							if types_in_module := g.module_types[lhs_name] {
								if type_or_fn_name in types_in_module {
									found_type = true
								}
							}
						}
						if found_type {
							// Check if this is a sum type construction
							if mangled_name in g.sum_type_names {
								variant_type := g.infer_type(node.expr)
								if g.gen_sumtype_wrap(mangled_name, variant_type, node.expr) {
									return
								}
							}
							// It's a type cast, generate C-style cast
							g.sb.write_string('((${mangled_name})(')
							g.gen_expr(node.expr)
							g.sb.write_string('))')
							return
						}
						// Module-qualified function call: strconv.f64_to_str_l(x)
						name = mangled_name
						// Check if param needs pointer conversion
						expected_params := g.fn_params[name] or { []string{} }
						g.sb.write_string('${name}(')
						// Handle pointer param conversion for single-arg calls
						if expected_params.len > 0 && expected_params[0].ends_with('*') {
							arg_type := g.infer_type(node.expr)
							if !is_pointer_type(arg_type) && !(node.expr is ast.PrefixExpr
								&& (node.expr as ast.PrefixExpr).op == .amp) {
								g.sb.write_string('&')
							}
						}
						g.gen_expr(node.expr)
						g.sb.write_string(')')
						return
					}
					// Method call on a variable: receiver.method(arg) where receiver is an Ident
					// (not a type, not a module name)
					method_name := node.lhs.rhs.name
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
						if method_name == 'free' {
							g.sb.write_string('array__free(&')
							g.gen_expr(receiver_expr)
							g.sb.write_string(')')
							return
						}
						if method_name == 'sort' {
							g.sb.write_string('(void)0 /* sort */')
							return
						}
						if method_name == 'prepend' && clean_type.starts_with('Array_') {
							elem_type := clean_type[6..]
							if receiver_is_ptr {
								g.sb.write_string('array__prepend(')
								g.gen_expr(receiver_expr)
							} else {
								g.sb.write_string('array__prepend(&')
								g.gen_expr(receiver_expr)
							}
							g.sb.write_string(', _MOV((${elem_type}[]){')
							g.gen_expr(node.expr)
							g.sb.write_string('}))')
							return
						}
						// Handle index on typed arrays - need type-specific comparison
						// For string arrays, use string__eq; for other types, use ==
						if method_name == 'index' && clean_type.starts_with('Array_') {
							elem_type := clean_type[6..] // Remove 'Array_' prefix
							// Generate inline search with type-specific comparison
							g.sb.write_string('({ int _idx = -1; ${elem_type}* _p = ')
							if receiver_is_ptr {
								g.sb.write_string('(*')
								g.gen_expr(receiver_expr)
								g.sb.write_string(')')
							} else {
								g.gen_expr(receiver_expr)
							}
							g.sb.write_string('.data; for (int _i = 0; _i < ')
							if receiver_is_ptr {
								g.sb.write_string('(*')
								g.gen_expr(receiver_expr)
								g.sb.write_string(')')
							} else {
								g.gen_expr(receiver_expr)
							}
							g.sb.write_string('.len; _i++, _p++) { if (')
							// Use string__eq for string arrays, == for others
							if elem_type == 'string' {
								g.sb.write_string('string__eq(*_p, ')
								g.gen_expr(node.expr)
								g.sb.write_string(')')
							} else {
								g.sb.write_string('*_p == ')
								g.gen_expr(node.expr)
							}
							g.sb.write_string(') { _idx = _i; break; } } _idx; })')
							return
						}
					}

					// Handle builtin map methods (delete) - single-arg method calls
					// Use map__delete like v2 generates
					if clean_type.starts_with('Map_') && method_name == 'delete' {
						// map.delete(key) -> map__delete(&m, &(KeyType[]){key})
						key_type := g.infer_type(node.expr)
						g.sb.write_string('map__delete(')
						if receiver_is_ptr {
							g.gen_expr(receiver_expr)
						} else {
							g.sb.write_string('&')
							g.gen_expr(receiver_expr)
						}
						g.sb.write_string(', &(${key_type}[]){')
						g.gen_expr(node.expr)
						g.sb.write_string('})')
						return
					}

					// Handle flag enum methods (set, clear, toggle)
					if method_name in ['set', 'clear', 'toggle'] {
						old_match_type := g.cur_match_type
						if clean_type in g.enum_names || clean_type in g.flag_enum_names {
							g.cur_match_type = clean_type
						}
						if method_name == 'set' {
							g.gen_expr(receiver_expr)
							g.sb.write_string(' |= ')
							g.gen_expr(node.expr)
						} else if method_name == 'clear' {
							g.gen_expr(receiver_expr)
							g.sb.write_string(' &= ~(')
							g.gen_expr(node.expr)
							g.sb.write_string(')')
						} else { // toggle
							g.gen_expr(receiver_expr)
							g.sb.write_string(' ^= ')
							g.gen_expr(node.expr)
						}
						g.cur_match_type = old_match_type
						return
					}

					mut method_type := clean_type
					mut mangled := '${method_type}__${method_name}'
					// For Array_ types, try different naming conventions
					if (clean_type.starts_with('Array_') || clean_type.starts_with('FixedArray_'))
						&& mangled !in g.fn_types {
						// Try module-prefixed form: Array_module__ElemType -> module__Array_ElemType
						// e.g., Array_ast__Expr -> ast__Array_Expr
						elem_type := if clean_type.starts_with('Array_') {
							clean_type[6..]
						} else {
							clean_type[11..] // FixedArray_
						}
						if elem_type.contains('__') {
							parts := elem_type.split('__')
							if parts.len >= 2 {
								mod_name := parts[0]
								base_elem := parts[1..].join('__')
								alt_mangled := '${mod_name}__Array_${base_elem}__${method_name}'
								if alt_mangled in g.fn_types {
									method_type = '${mod_name}__Array_${base_elem}'
									mangled = alt_mangled
								}
							}
						}
						// Fall back to generic 'array' if still not found
						if mangled !in g.fn_types {
							method_type = 'array'
							mangled = '${method_type}__${method_name}'
						}
					}
					// Method expects pointer if receiver is mut OR reference
					is_flag_enum_method := clean_type in g.flag_enum_names
						&& method_name in ['has', 'all']
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
					// Check param type for enum shorthand or function pointer cast
					expected_params := g.fn_params[mangled] or { []string{} }
					param_type := if expected_params.len > 0 { expected_params[0] } else { '' }
					old_match_type := g.cur_match_type
					if param_type in g.enum_names {
						g.cur_match_type = param_type
					} else if clean_type in g.flag_enum_names && method_name in ['has', 'all'] {
						g.cur_match_type = clean_type
					}
					// Set lambda element type for filter/map/any/all methods
					old_lambda_elem_type := g.cur_lambda_elem_type
					if method_name in ['filter', 'map', 'any', 'all']
						&& clean_type.starts_with('Array_') {
						g.cur_lambda_elem_type = clean_type[6..] // Remove 'Array_' prefix
					}
					// Generate the single argument
					if node.expr is ast.ModifierExpr {
						if node.expr.kind == .key_mut {
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
						g.sb.write_string('(${param_type})')
						g.gen_expr(node.expr)
					} else {
						// Check for smartcast pattern being passed to sum type parameter
						arg_type := g.infer_type(node.expr)
						if g.is_smartcast_to_sum_type_variant(node.expr, arg_type) {
							orig_var := g.extract_smartcast_original_var(node.expr)
							if param_type in g.sum_type_names {
								// Param expects sum type, pass the original variable
								g.sb.write_string(orig_var)
							} else {
								// Param expects variant, pass the smartcast
								g.gen_expr(node.expr)
							}
						} else {
							g.gen_expr(node.expr)
						}
					}
					g.cur_match_type = old_match_type
					g.cur_lambda_elem_type = old_lambda_elem_type
					g.sb.write_string(')')
					return
				} else if node.lhs.lhs is ast.SelectorExpr {
					// Chained selector - could be module.Type.method(arg) static method call
					inner_sel := node.lhs.lhs as ast.SelectorExpr
					if inner_sel.lhs is ast.Ident {
						mod_name := (inner_sel.lhs as ast.Ident).name
						if mod_name in g.module_names {
							// module.Type.method(arg) -> module__Type__method(arg)
							// This is a static method call with single argument
							resolved_mod := g.resolve_module_name(mod_name)
							fn_name := '${resolved_mod}__${inner_sel.rhs.name}__${node.lhs.rhs.name}'
							// Check if param needs pointer conversion
							expected_params := g.fn_params[fn_name] or { []string{} }
							g.sb.write_string('${fn_name}(')
							if expected_params.len > 0 && expected_params[0].ends_with('*') {
								arg_type := g.infer_type(node.expr)
								if !is_pointer_type(arg_type) && !(node.expr is ast.PrefixExpr
									&& (node.expr as ast.PrefixExpr).op == .amp) {
									g.sb.write_string('&')
								}
							}
							g.gen_expr(node.expr)
							g.sb.write_string(')')
							return
						}
					}
					// Method call: selector.method(arg) where selector is also a SelectorExpr
					// e.g., m.key_values.key(kv_index) -> DenseArray__key(&m->key_values, kv_index)
					method_name := node.lhs.rhs.name
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
						if method_name == 'free' {
							g.sb.write_string('array__free(&')
							g.gen_expr(receiver_expr)
							g.sb.write_string(')')
							return
						}
						if method_name == 'sort' {
							g.sb.write_string('(void)0 /* sort */')
							return
						}
						if method_name == 'prepend' && clean_type.starts_with('Array_') {
							elem_type := clean_type[6..]
							if receiver_is_ptr {
								g.sb.write_string('array__prepend(')
								g.gen_expr(receiver_expr)
							} else {
								g.sb.write_string('array__prepend(&')
								g.gen_expr(receiver_expr)
							}
							g.sb.write_string(', _MOV((${elem_type}[]){')
							g.gen_expr(node.expr)
							g.sb.write_string('}))')
							return
						}
					}

					// Handle flag enum methods (set, clear, toggle)
					if method_name in ['set', 'clear', 'toggle'] {
						old_match_type := g.cur_match_type
						if clean_type in g.enum_names || clean_type in g.flag_enum_names {
							g.cur_match_type = clean_type
						}
						if method_name == 'set' {
							g.gen_expr(receiver_expr)
							g.sb.write_string(' |= ')
							g.gen_expr(node.expr)
						} else if method_name == 'clear' {
							g.gen_expr(receiver_expr)
							g.sb.write_string(' &= ~(')
							g.gen_expr(node.expr)
							g.sb.write_string(')')
						} else { // toggle
							g.gen_expr(receiver_expr)
							g.sb.write_string(' ^= ')
							g.gen_expr(node.expr)
						}
						g.cur_match_type = old_match_type
						return
					}

					mut method_type := clean_type
					mut mangled := '${method_type}__${method_name}'
					// For Array_ types, try different naming conventions
					if (clean_type.starts_with('Array_') || clean_type.starts_with('FixedArray_'))
						&& mangled !in g.fn_types {
						// Try module-prefixed form: Array_module__ElemType -> module__Array_ElemType
						// e.g., Array_ast__Expr -> ast__Array_Expr
						elem_type := if clean_type.starts_with('Array_') {
							clean_type[6..]
						} else {
							clean_type[11..] // FixedArray_
						}
						if elem_type.contains('__') {
							parts := elem_type.split('__')
							if parts.len >= 2 {
								mod_name := parts[0]
								base_elem := parts[1..].join('__')
								alt_mangled := '${mod_name}__Array_${base_elem}__${method_name}'
								if alt_mangled in g.fn_types {
									method_type = '${mod_name}__Array_${base_elem}'
									mangled = alt_mangled
								}
							}
						}
						// Fall back to generic 'array' if still not found
						if mangled !in g.fn_types {
							method_type = 'array'
							mangled = '${method_type}__${method_name}'
						}
					}
					// Method expects pointer if receiver is mut OR reference
					is_flag_enum_method := clean_type in g.flag_enum_names
						&& method_name in ['has', 'all']
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
					// Check param type for enum shorthand or function pointer cast
					expected_params := g.fn_params[mangled] or { []string{} }
					param_type := if expected_params.len > 0 { expected_params[0] } else { '' }
					old_match_type := g.cur_match_type
					if param_type in g.enum_names {
						g.cur_match_type = param_type
					} else if clean_type in g.flag_enum_names && method_name in ['has', 'all'] {
						g.cur_match_type = clean_type
					}
					// Generate the single argument
					if node.expr is ast.ModifierExpr {
						if node.expr.kind == .key_mut {
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
						g.sb.write_string('(${param_type})')
						g.gen_expr(node.expr)
					} else {
						g.gen_expr(node.expr)
					}
					g.cur_match_type = old_match_type
					g.sb.write_string(')')
					return
				} else {
					// Method call: obj.method(arg) where obj is not Ident or SelectorExpr
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
						// Handle prepend - need to wrap value arg in _MOV for void* param
						if name == 'prepend' && clean_type.starts_with('Array_') {
							elem_type := clean_type[6..] // Remove 'Array_' prefix
							if receiver_is_ptr {
								g.sb.write_string('array__prepend(')
								g.gen_expr(receiver_expr)
							} else {
								g.sb.write_string('array__prepend(&')
								g.gen_expr(receiver_expr)
							}
							g.sb.write_string(', _MOV((${elem_type}[]){')
							g.gen_expr(node.expr)
							g.sb.write_string('}))')
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
					// For Array_ types, try different naming conventions
					if (clean_type.starts_with('Array_') || clean_type.starts_with('FixedArray_'))
						&& mangled !in g.fn_types {
						// Try module-prefixed form: Array_module__ElemType -> module__Array_ElemType
						elem_type := if clean_type.starts_with('Array_') {
							clean_type[6..]
						} else {
							clean_type[11..] // FixedArray_
						}
						if elem_type.contains('__') {
							parts := elem_type.split('__')
							if parts.len >= 2 {
								mod_name := parts[0]
								base_elem := parts[1..].join('__')
								alt_mangled := '${mod_name}__Array_${base_elem}__${name}'
								if alt_mangled in g.fn_types {
									method_type = '${mod_name}__Array_${base_elem}'
									mangled = alt_mangled
								}
							}
						}
						// Fall back to generic 'array' if still not found
						if mangled !in g.fn_types {
							method_type = 'array'
							mangled = '${method_type}__${name}'
						}
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
					// Set lambda element type for filter/map/any/all methods
					old_lambda_elem_type := g.cur_lambda_elem_type
					if name in ['filter', 'map', 'any', 'all'] && clean_type.starts_with('Array_') {
						g.cur_lambda_elem_type = clean_type[6..] // Remove 'Array_' prefix
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
					g.cur_lambda_elem_type = old_lambda_elem_type
					g.sb.write_string(')')
					return
				}
			}
			// Use mangled name if available (for cross-module function calls)
			// First try current module prefix
			mut call_name := name
			if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
				cur_module_fn := '${g.cur_module}__${name}'
				if cur_module_fn in g.fn_types {
					call_name = cur_module_fn
				} else if mangled := g.fn_mangled[name] {
					call_name = mangled
				}
			} else if mangled := g.fn_mangled[name] {
				call_name = mangled
			}
			g.sb.write_string('${call_name}(')
			// Look up the expected parameter type for this function
			expected_params := g.fn_params[call_name] or { []string{} }
			param_type := if expected_params.len > 0 { expected_params[0] } else { '' }
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
			} else if node.expr is ast.FieldInit {
				// Named argument - wrap in @[params] struct if param type is a params struct
				if param_type in g.params_structs {
					field_init := node.expr as ast.FieldInit
					g.sb.write_string('(${param_type}){.${escape_c_keyword(field_init.name)} = ')
					g.gen_expr(field_init.value)
					g.sb.write_string('}')
				} else {
					g.gen_expr(node.expr)
				}
			} else if param_type.ends_with('*') && !param_type.starts_with('FnPtr_') {
				// Parameter expects pointer - check if arg is already a pointer
				arg_type := g.infer_type(node.expr)
				if !is_pointer_type(arg_type) && !(node.expr is ast.PrefixExpr
					&& (node.expr as ast.PrefixExpr).op == .amp) {
					// Need to take address
					g.sb.write_string('&')
				}
				g.gen_expr(node.expr)
			} else {
				// Check if IError needs to be converted to string
				arg_type := g.infer_type(node.expr)
				// Convert IError to string for print functions or when param expects string
				is_print_fn := name in ['println', 'eprintln', 'print', 'eprint']
				if arg_type == 'IError' && (param_type == 'string' || is_print_fn) {
					// Convert IError to string: IError_str(err)
					g.sb.write_string('IError_str(')
					g.gen_expr(node.expr)
					g.sb.write_string(')')
				} else if param_type in g.sum_type_names {
					// Wrap value in sum type struct
					if !g.gen_sumtype_wrap(param_type, arg_type, node.expr) {
						g.gen_expr(node.expr)
					}
				} else {
					g.gen_expr(node.expr)
				}
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
			// Try both the type name directly and with module prefix
			mut field_types := map[string]string{}
			if typ_name in g.struct_fields {
				field_types = g.struct_fields[typ_name].clone()
			} else if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
				mangled_typ := '${g.cur_module}__${typ_name}'
				if mangled_typ in g.struct_fields {
					field_types = g.struct_fields[mangled_typ].clone()
				}
			}
			g.sb.write_string('(${typ_name}){')
			// Track which fields are explicitly initialized
			mut explicit_fields := map[string]bool{}
			for i, field in node.fields {
				if i > 0 {
					g.sb.write_string(', ')
				}
				// Only emit field designator if field name is specified (not positional init)
				if field.name != '' {
					g.sb.write_string('.${escape_c_keyword(field.name)} = ')
					explicit_fields[field.name] = true
				}
				// Set enum context if field type is an enum or flag enum
				field_type := field_types[field.name] or { '' }
				old_match_type := g.cur_match_type
				if field_type in g.enum_names || field_type in g.flag_enum_names {
					g.cur_match_type = field_type
				}
				// Handle fixed-size array fields specially - generate C array initializer
				if field_type.starts_with('FixedArray_') && field.value is ast.ArrayInitExpr {
					arr_init := field.value as ast.ArrayInitExpr
					g.sb.write_string('{')
					for j, elem in arr_init.exprs {
						if j > 0 {
							g.sb.write_string(', ')
						}
						g.gen_expr(elem)
					}
					g.sb.write_string('}')
				} else if field_type in g.sum_type_names {
					// Sum type field - check if value needs wrapping
					value_type := g.infer_type(field.value)
					if value_type == field_type || value_type in g.sum_type_names {
						// Already the sum type or another sum type - no wrapping needed
						g.gen_expr(field.value)
					} else if !g.gen_sumtype_wrap(field_type, value_type, field.value) {
						g.gen_expr(field.value)
					}
				} else {
					g.gen_expr(field.value)
				}
				g.cur_match_type = old_match_type
			}
			// Initialize map fields that weren't explicitly set
			mut first_implicit := node.fields.len == 0
			for fname, ftype in field_types {
				if fname !in explicit_fields && ftype.starts_with('Map_') {
					// This is a map field that needs initialization
					if !first_implicit {
						g.sb.write_string(', ')
					}
					first_implicit = false
					g.sb.write_string('.${escape_c_keyword(fname)} = __new_${ftype}()')
				}
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
					// Try to use mangled name if available
					enum_name := node.lhs.name
					if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
						mangled := '${g.cur_module}__${enum_name}'
						if mangled in g.enum_names {
							g.sb.write_string('${mangled}__${node.rhs.name}')
							return
						}
					}
					g.sb.write_string('${enum_name}__${node.rhs.name}')
					return
				}
				// Check if this is a module-qualified name (e.g., strconv.double_minus_zero)
				// But only if it's not a local variable (which takes precedence)
				if node.lhs.name in g.module_names && node.lhs.name !in g.var_types
					&& node.lhs.name !in g.global_var_types {
					// Module constant/function access: generate module__name
					// Resolve import alias if present
					resolved_mod := g.resolve_module_name(node.lhs.name)
					g.sb.write_string('${resolved_mod}__${node.rhs.name}')
					return
				}
			}
			// Check for module-qualified enum access: module.EnumType.field
			// e.g., token.Token.key_is -> token__Token__key_is
			if node.lhs is ast.SelectorExpr {
				sel_lhs := node.lhs as ast.SelectorExpr
				if sel_lhs.lhs is ast.Ident {
					ident_lhs := sel_lhs.lhs as ast.Ident
					if ident_lhs.name in g.module_names {
						// This is module.Type.field - check if Type is an enum
						resolved_mod := g.resolve_module_name(ident_lhs.name)
						mangled_type := '${resolved_mod}__${sel_lhs.rhs.name}'
						if mangled_type in g.enum_names {
							// Module-qualified enum value: module__EnumType__field
							g.sb.write_string('${mangled_type}__${node.rhs.name}')
							return
						}
					}
				}
			}
			// Check for enum shorthand (.value) - LHS is EmptyExpr
			if node.lhs is ast.EmptyExpr {
				// Use the match expression type context
				if g.cur_match_type in g.enum_names || g.cur_match_type in g.flag_enum_names {
					g.sb.write_string('${g.cur_match_type}__${node.rhs.name}')
					return
				}
				// Fallback: search all flag enums for a matching field
				// This handles cases where cur_match_type isn't set but we have a known flag enum field
				for enum_name, _ in g.flag_enum_names {
					// Check if this could be a field of this enum (heuristic based on common naming)
					if node.rhs.name in ['noslices', 'noshrink', 'nogrow', 'nofree']
						&& enum_name == 'ArrayFlags' {
						g.sb.write_string('${enum_name}__${node.rhs.name}')
						return
					}
				}
				// Check for known enum values from stdlib
				// ProcessState enum from os module
				if node.rhs.name in ['not_started', 'running', 'stopped', 'exited', 'closed'] {
					g.sb.write_string('os__ProcessState__${node.rhs.name}')
					return
				}
				// TrimMode enum from strings module
				if node.rhs.name in ['trim_left', 'trim_right', 'trim_both'] {
					g.sb.write_string('TrimMode__${node.rhs.name}')
					return
				}
				// CaseMode enum from unicode module
				if node.rhs.name in ['to_upper', 'to_lower', 'to_title'] {
					g.sb.write_string('CaseMode__${node.rhs.name}')
					return
				}
				// Last fallback: just output the field name (might not be valid C)
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
			// Handle fixed array .len access - return compile-time size constant
			// Type format: FixedArray_<elem_type>_<size>
			if lhs_type.starts_with('FixedArray_') && node.rhs.name == 'len' {
				// Extract size from type name (e.g., FixedArray_u8_29 -> 29)
				parts := lhs_type.split('_')
				if parts.len >= 3 {
					size := parts[parts.len - 1]
					g.sb.write_string(size)
					return
				}
			}
			// Note: Sum type field access smartcasting is handled by the transformer.
			// The transformer produces AST like: ((Type*)(t._data._Variant))->field
			// We skip cleanc's smartcast handling to avoid double-smartcasting.
			// Handle Result/Option .data field access - needs proper pointer casting
			if node.rhs.name == 'data' {
				if lhs_type.starts_with('_result_') {
					// Extract base type: _result_int -> int
					base_type := lhs_type[8..] // Skip "_result_"
					g.sb.write_string('(*(${base_type}*)')
					g.gen_expr(node.lhs)
					g.sb.write_string('.data)')
					return
				} else if lhs_type.starts_with('_option_') {
					// Extract base type: _option_int -> int
					base_type := lhs_type[8..] // Skip "_option_"
					g.sb.write_string('(*(${base_type}*)')
					g.gen_expr(node.lhs)
					g.sb.write_string('.data)')
					return
				}
			}
			g.gen_expr(node.lhs)
			if lhs_type.ends_with('*') {
				g.sb.write_string('->')
			} else {
				g.sb.write_string('.')
			}
			g.sb.write_string(escape_c_keyword(node.rhs.name))
		}
		ast.IndexExpr {
			// Check if this is a slice operation (arr[start..end])
			if node.expr is ast.RangeExpr {
				g.gen_slice_expr(node.lhs, node.expr)
				return
			}
			// Check if this is an array access (Array struct type)
			lhs_type := g.infer_type(node.lhs)
			// Handle pointer types (e.g., Map_int_bool* from mut parameters)
			map_is_ptr := lhs_type.ends_with('*')
			map_base_type := if map_is_ptr { lhs_type[..lhs_type.len - 1] } else { lhs_type }
			if map_base_type.starts_with('Map_') {
				// Map access: __<map_type>_get(&m, key)
				// For nested map access (lhs is itself a map index), we need to use
				// ADDR(map, inner_get) pattern because we can't take address of rvalue
				is_nested_map := if node.lhs is ast.IndexExpr {
					inner_lhs_type := g.infer_type(node.lhs.lhs)

					inner_lhs_type.starts_with('Map_')
						|| inner_lhs_type.trim_right('*').starts_with('Map_')
				} else {
					false
				}
				if is_nested_map {
					// Generate: __Map_X_get(&((map[]){__Map_Y_get(&m, key)}[0]), inner_key)
					g.sb.write_string('__${map_base_type}_get(&((map[]){')
					g.gen_expr(node.lhs) // This generates __Map_Y_get(...)
					g.sb.write_string('}[0]), ')
					g.gen_expr(node.expr)
					g.sb.write_string(')')
				} else {
					g.sb.write_string('__${map_base_type}_get(')
					if !map_is_ptr {
						g.sb.write_string('&')
					}
					g.gen_expr(node.lhs)
					g.sb.write_string(', ')
					g.gen_expr(node.expr)
					g.sb.write_string(')')
				}
			} else if lhs_type == 'string' {
				// String character access: s.str[index]
				g.gen_expr(node.lhs)
				g.sb.write_string('.str[')
				g.gen_expr(node.expr)
				g.sb.write_string(']')
			} else if lhs_type.starts_with('Array_') || lhs_type == 'array'
				|| lhs_type == 'strings__Builder' || lhs_type == 'strings__Builder*' {
				// For Array types, access via ((elem_type*)arr.data)[index]
				// Handle pointer types (mutable parameters): arr->data vs arr.data
				is_ptr := lhs_type.ends_with('*')
				clean_type := if is_ptr { lhs_type[..lhs_type.len - 1] } else { lhs_type }
				// Convert sanitized type back to valid C type for the cast
				// For generic 'array' type, default to string (common case for split functions)
				// strings__Builder is Array_u8 so element is u8
				elem_type := if clean_type == 'array' {
					'string'
				} else if clean_type == 'strings__Builder' {
					'u8'
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
				// Error propagation: expr! - unwrap the result, propagating errors
				// Generate: ({ _result_T _t = expr; if (_t.is_error) return (_result_U){.is_error=true,.err=_t.err}; *(T*)_t.data; })
				expr_type := g.infer_type(node.expr)
				if expr_type.starts_with('_result_') {
					base_type := g.result_types[expr_type] or { 'int' }
					tmp_name := '_prop_${g.tmp_counter}'
					g.tmp_counter++
					g.sb.write_string('({ ${expr_type} ${tmp_name} = ')
					g.gen_expr(node.expr)
					g.sb.write_string('; if (${tmp_name}.is_error) { ')
					if g.cur_fn_returns_result {
						g.sb.write_string('return (${g.cur_fn_ret_type}){.is_error=true,.err=${tmp_name}.err}; ')
					} else {
						g.sb.write_string('panic(${tmp_name}.err.msg); ')
					}
					g.sb.write_string('} *(${base_type}*)${tmp_name}.data; })')
				} else if expr_type.starts_with('_option_') {
					base_type := g.option_types[expr_type] or { 'int' }
					tmp_name := '_prop_${g.tmp_counter}'
					g.tmp_counter++
					g.sb.write_string('({ ${expr_type} ${tmp_name} = ')
					g.gen_expr(node.expr)
					g.sb.write_string('; if (${tmp_name}.state != 0) { ')
					if g.cur_fn_returns_option {
						g.sb.write_string('return (${g.cur_fn_ret_type}){.state=${tmp_name}.state,.err=${tmp_name}.err}; ')
					} else if g.cur_fn_returns_result {
						g.sb.write_string('return (${g.cur_fn_ret_type}){.is_error=true,.err=${tmp_name}.err}; ')
					} else {
						g.sb.write_string('panic(${tmp_name}.err.msg); ')
					}
					g.sb.write_string('} *(${base_type}*)${tmp_name}.data; })')
				} else {
					// Not a Result/Option - just generate the expression
					g.gen_expr(node.expr)
				}
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
			// After transformer, ArrayInitExpr is either:
			// 1. Fixed-size array: generate {val1, val2, ...}
			// 2. Dynamic array as arg to builtin call: generate _MOV((elem_type[len]){values})
			mut is_fixed_array := false
			match node.typ {
				ast.Type {
					if node.typ is ast.ArrayFixedType {
						is_fixed_array = true
					}
				}
				else {}
			}
			if is_fixed_array {
				// Fixed-size array: generate C array initializer
				g.sb.write_string('{')
				if node.exprs.len == 0 {
					g.sb.write_string('0')
				} else {
					for i, expr in node.exprs {
						if i > 0 {
							g.sb.write_string(', ')
						}
						g.gen_expr(expr)
					}
				}
				g.sb.write_string('}')
			} else {
				// Dynamic array values: generate _MOV((elem_type[len]){values})
				arr_len := node.exprs.len
				mut c_elem_type := 'int'
				match node.typ {
					ast.Type {
						if node.typ is ast.ArrayType {
							c_elem_type = g.expr_type_to_c(node.typ.elem_type)
						}
					}
					else {}
				}
				// If no type annotation, infer from first element
				if c_elem_type == 'int' && node.exprs.len > 0 {
					inferred := g.infer_type(node.exprs[0])
					if inferred != '' {
						c_elem_type = inferred
					}
				}
				// Check if cap or len is specified
				has_cap := node.cap !is ast.EmptyExpr
				has_len := node.len !is ast.EmptyExpr
				// Handle empty arrays with cap - need to allocate memory
				if arr_len == 0 && has_cap {
					// Generate: ({array a = {0}; a.cap = cap; a.element_size = sizeof(elem); a.data = malloc(cap * sizeof(elem)); a;})
					g.sb.write_string('({ array _arr = {0}; _arr.element_size = sizeof(${c_elem_type}); _arr.cap = ')
					g.gen_expr(node.cap)
					g.sb.write_string('; _arr.data = malloc(_arr.cap * _arr.element_size); _arr; })')
				} else if arr_len == 0 && has_len {
					// Generate: ({array a = {0}; a.len = len; a.cap = len; a.element_size = sizeof(elem); a.data = calloc(len, sizeof(elem)); a;})
					g.sb.write_string('({ array _arr = {0}; _arr.element_size = sizeof(${c_elem_type}); _arr.len = ')
					g.gen_expr(node.len)
					g.sb.write_string('; _arr.cap = _arr.len; _arr.data = calloc(_arr.len, _arr.element_size); _arr; })')
				} else if arr_len == 0 {
					// Empty array - still need to set element_size for push operations
					g.sb.write_string('({ array _arr = {0}; _arr.element_size = sizeof(${c_elem_type}); _arr; })')
				} else {
					// Use unsanitize_type_for_c to convert mangled type names (e.g., arm64__Intervalptr) back to C types (arm64__Interval*)
					c_elem_type_for_c := unsanitize_type_for_c(c_elem_type)
					g.sb.write_string('_MOV((${c_elem_type_for_c}[${arr_len}]){')
					for i, expr in node.exprs {
						if i > 0 {
							g.sb.write_string(', ')
						}
						g.gen_expr(expr)
					}
					g.sb.write_string('})')
				}
			}
		}
		ast.MapInitExpr {
			// Generate empty map initialization
			map_type := g.infer_type(node)
			g.sb.write_string('__new_${map_type}()')
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
			// Handle Option type casts: ?string(x) should wrap in Option
			if type_name.starts_with('_option_') {
				base_type := type_name['_option_'.len..]
				// Generate: ({ _option_T _o; *(T*)_o.data = expr; _o.state = 0; _o; })
				g.sb.write_string('({ ${type_name} _o; *(${base_type}*)_o.data = ')
				g.gen_expr(node.expr)
				g.sb.write_string('; _o.state = 0; _o; })')
			} else {
				g.sb.write_string('((${type_name})(')
				g.gen_expr(node.expr)
				g.sb.write_string('))')
			}
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
			// Most OrExpr should be expanded by transformer, but handle fallback
			// for edge cases (e.g., builtin functions not tracked by transformer)
			// Note: Map index or blocks are now handled in the transformer

			typ := g.infer_type(node.expr)

			if typ.starts_with('_result_') {
				base_type := g.result_types[typ] or { 'int' }
				g.sb.write_string('({ ${typ} _or_res = ')
				g.gen_expr(node.expr)
				g.sb.write_string('; if (_or_res.is_error) { ')
				if node.stmts.len == 1 {
					stmt := node.stmts[0]
					if stmt is ast.ExprStmt {
						g.gen_expr(stmt.expr)
						g.sb.write_string('; ')
					}
				}
				g.sb.write_string('} *(${base_type}*)_or_res.data; })')
			} else if typ.starts_with('_option_') {
				base_type := g.option_types[typ] or { 'int' }
				g.sb.write_string('({ ${typ} _or_opt = ')
				g.gen_expr(node.expr)
				g.sb.write_string('; if (_or_opt.state != 0) { ')
				if node.stmts.len == 1 {
					stmt := node.stmts[0]
					if stmt is ast.ExprStmt {
						g.gen_expr(stmt.expr)
						g.sb.write_string('; ')
					}
				}
				g.sb.write_string('} *(${base_type}*)_or_opt.data; })')
			} else {
				// Unknown type - just generate the expression
				g.gen_expr(node.expr)
			}
		}
		ast.AssocExpr {
			// Struct update expression: {...base, field1: val1}
			// Generate: ({ TypeName _tmp = base; _tmp.field1 = val1; _tmp; })
			typ_name := g.expr_type_to_c(node.typ)
			// Get struct field types for sum type wrapping
			mut field_types := map[string]string{}
			if typ_name in g.struct_fields {
				field_types = g.struct_fields[typ_name].clone()
			} else if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
				mangled_typ := '${g.cur_module}__${typ_name}'
				if mangled_typ in g.struct_fields {
					field_types = g.struct_fields[mangled_typ].clone()
				}
			}
			g.sb.write_string('({ ${typ_name} _assoc_tmp = ')
			g.gen_expr(node.expr)
			g.sb.write_string('; ')
			for field in node.fields {
				g.sb.write_string('_assoc_tmp.${escape_c_keyword(field.name)} = ')
				// Check if field type is a sum type and needs wrapping
				field_type := field_types[field.name] or { '' }
				if field_type in g.sum_type_names {
					value_type := g.infer_type(field.value)
					if value_type == field_type || value_type in g.sum_type_names {
						// Already the sum type - no wrapping needed
						g.gen_expr(field.value)
					} else if !g.gen_sumtype_wrap(field_type, value_type, field.value) {
						g.gen_expr(field.value)
					}
				} else {
					g.gen_expr(field.value)
				}
				g.sb.write_string('; ')
			}
			g.sb.write_string('_assoc_tmp; })')
		}
		ast.GenericArgs {
			// GenericArgs can be either generic type arguments or array/map indexing
			// Check if the LHS is an array/map type - if so, generate as indexing
			lhs_type := g.infer_type(node.lhs)
			if lhs_type.starts_with('Map_') && node.args.len > 0 {
				// This is map indexing that was parsed as generic args
				g.sb.write_string('__${lhs_type}_get(&')
				g.gen_expr(node.lhs)
				g.sb.write_string(', ')
				g.gen_expr(node.args[0])
				g.sb.write_string(')')
			} else if lhs_type.starts_with('Array_') && node.args.len > 0 {
				// This is array indexing that was parsed as generic args
				is_ptr := lhs_type.ends_with('*')
				clean_type := if is_ptr { lhs_type[..lhs_type.len - 1] } else { lhs_type }
				elem_type := unsanitize_type_for_c(clean_type['Array_'.len..])
				accessor := if is_ptr { '->' } else { '.' }
				g.sb.write_string('((${elem_type}*)')
				g.gen_expr(node.lhs)
				g.sb.write_string('${accessor}data)[')
				g.gen_expr(node.args[0])
				g.sb.write_string(']')
			} else {
				// Generic type arguments - in C, just generate the base expression
				// The type arguments are typically used at compile time, not runtime
				g.gen_expr(node.lhs)
			}
		}
		ast.GenericArgOrIndexExpr {
			// Ambiguous expression - could be generic args or index
			// Check the LHS type to determine how to generate
			lhs_type := g.infer_type(node.lhs)
			if lhs_type.starts_with('Map_') {
				// Map access: __<map_type>_get(&m, key)
				g.sb.write_string('__${lhs_type}_get(&')
				g.gen_expr(node.lhs)
				g.sb.write_string(', ')
				g.gen_expr(node.expr)
				g.sb.write_string(')')
			} else if lhs_type.starts_with('Array_') || lhs_type == 'array' {
				// Array access: ((elem_type*)arr.data)[index]
				is_ptr := lhs_type.ends_with('*')
				clean_type := if is_ptr { lhs_type[..lhs_type.len - 1] } else { lhs_type }
				elem_type := if clean_type == 'array' {
					'int'
				} else {
					unsanitize_type_for_c(clean_type['Array_'.len..])
				}
				accessor := if is_ptr { '->' } else { '.' }
				g.sb.write_string('((${elem_type}*)')
				g.gen_expr(node.lhs)
				g.sb.write_string('${accessor}data)[')
				g.gen_expr(node.expr)
				g.sb.write_string(']')
			} else {
				// Regular C array access (or generic type args)
				g.gen_expr(node.lhs)
				g.sb.write_string('[')
				g.gen_expr(node.expr)
				g.sb.write_string(']')
			}
		}
		ast.LockExpr {
			// Lock expression - for now, just generate the statements without actual locking
			// (proper lock handling would require runtime support)
			g.sb.write_string('({ ')
			g.gen_stmts(node.stmts)
			g.sb.write_string(' })')
		}
		ast.AsCastExpr {
			// As cast expression: expr as Type
			// For sum types, cast to the variant pointer
			typ_name := g.expr_type_to_c(node.typ)
			// Check if this is a sum type cast by seeing if the target is a sum type variant
			if typ_name in g.sum_type_names {
				// Casting to sum type - just use the expression
				g.gen_expr(node.expr)
			} else {
				// Check if source is a sum type (need pointer cast to variant)
				expr_type := g.infer_type(node.expr)
				if expr_type in g.sum_type_names {
					// Cast from sum type to variant: ((VariantType*)&expr)->_
					// or just access the variant directly
					g.sb.write_string('(*((${typ_name}*)&')
					g.gen_expr(node.expr)
					g.sb.write_string('))')
				} else {
					// Regular C-style cast
					g.sb.write_string('((${typ_name})(')
					g.gen_expr(node.expr)
					g.sb.write_string('))')
				}
			}
		}
		ast.FnLiteral {
			// Anonymous function / closure
			// In C, we can't have inline lambdas, so we generate a function pointer
			// For now, generate a placeholder that shows it's a lambda
			// Full closure support would require generating a separate function
			g.sb.write_string('((void*)0) /* lambda */')
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

// gen_decl_if_expr handles variable declaration with if-expression initializer that can't be ternary
// For: name := if (cond) { stmts; val } else { stmts; val }
// Generates: Type name; if (cond) { stmts; name = val; } else { stmts; name = val; }
fn (mut g Gen) gen_decl_if_expr(name string, if_expr ast.IfExpr) {
	// Pre-register variable declarations from if-branch for type inference
	g.preregister_branch_vars(if_expr.stmts)

	// Infer type and declare variable
	typ := g.infer_type(if_expr)
	g.var_types[name] = typ
	c_typ := g.type_for_c_decl(typ)
	g.sb.writeln('${c_typ} ${name};')

	// Generate if-statement with assignment in each branch
	g.write_indent()
	g.sb.write_string('if (')
	g.gen_expr(if_expr.cond)
	g.sb.writeln(') {')
	g.indent++
	// Generate statements, last one is the assignment value
	for i, stmt in if_expr.stmts {
		if i == if_expr.stmts.len - 1 {
			if stmt is ast.ExprStmt {
				g.write_indent()
				g.sb.write_string('${name} = ')
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
						g.sb.write_string('${name} = ')
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
			// Else-if chain
			g.sb.write_string(' else ')
			g.gen_decl_if_expr_branch(name, if_expr.else_expr)
		}
	} else {
		g.sb.writeln('')
	}
}

// gen_decl_if_expr_branch generates assignment in nested if-else branches
fn (mut g Gen) gen_decl_if_expr_branch(name string, if_expr ast.IfExpr) {
	g.sb.write_string('if (')
	g.gen_expr(if_expr.cond)
	g.sb.writeln(') {')
	g.indent++
	for i, stmt in if_expr.stmts {
		if i == if_expr.stmts.len - 1 {
			if stmt is ast.ExprStmt {
				g.write_indent()
				g.sb.write_string('${name} = ')
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

	// Handle else
	if if_expr.else_expr is ast.IfExpr {
		if if_expr.else_expr.cond is ast.EmptyExpr {
			g.sb.writeln(' else {')
			g.indent++
			for i, stmt in if_expr.else_expr.stmts {
				if i == if_expr.else_expr.stmts.len - 1 {
					if stmt is ast.ExprStmt {
						g.write_indent()
						g.sb.write_string('${name} = ')
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
			g.sb.write_string(' else ')
			g.gen_decl_if_expr_branch(name, if_expr.else_expr)
		}
	} else {
		g.sb.writeln('')
	}
}

// preregister_branch_vars pre-registers variable declarations from if-branch statements
// so that type inference can work correctly for the final expression
fn (mut g Gen) preregister_branch_vars(stmts []ast.Stmt) {
	for stmt in stmts {
		if stmt is ast.AssignStmt {
			if stmt.op == .decl_assign {
				for i, lhs in stmt.lhs {
					mut var_name := ''
					if lhs is ast.Ident {
						var_name = lhs.name
					} else if lhs is ast.ModifierExpr {
						if lhs.expr is ast.Ident {
							var_name = lhs.expr.name
						}
					}
					if var_name != '' && i < stmt.rhs.len {
						var_type := g.infer_type(stmt.rhs[i])
						g.var_types[var_name] = var_type
					}
				}
			}
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
fn (mut g Gen) has_range_condition(match_expr ast.MatchExpr) bool {
	// Check if match expression type is an enum - if so, shorthand values are allowed
	match_type := g.infer_type(match_expr.expr)

	// Strings cannot be used in C switch statements - use if-else chain
	if match_type == 'string' {
		return true
	}

	is_enum_match := match_type in g.enum_names || match_type in g.flag_enum_names

	for branch in match_expr.branches {
		for c in branch.cond {
			if c is ast.RangeExpr {
				return true
			}
			// Field access like param.block_start is not a compile-time constant in C
			// But enum shorthand like .value is fine
			if c is ast.SelectorExpr {
				// Check if it's enum shorthand (empty LHS)
				if c.lhs is ast.EmptyExpr {
					continue // Enum shorthand is OK for switch
				}
				return true
			}
			// Variable references are not compile-time constants
			// But enum shorthand values (like 'any' when matching enum) are OK
			if c is ast.Ident && c.name !in g.enum_names {
				// If we're matching an enum, bare identifiers are likely enum values
				if is_enum_match {
					continue // Assume it's an enum shorthand value
				}
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
		c_typ := g.type_for_c_decl(typ)
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
		c_match_type := g.type_for_c_decl(match_type)
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
					} else if match_type == 'string' {
						// String comparison: string__eq(__match_val, c)
						g.sb.write_string('string__eq(__match_val, ')
						g.gen_expr(c)
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
	// Set cur_match_type for enum shorthand resolution in case conditions
	match_type := g.infer_type(match_expr.expr)
	old_match_type_sw := g.cur_match_type
	g.cur_match_type = match_type

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

	g.cur_match_type = old_match_type_sw
}

// gen_error_expr generates an IError expression.
// The transformer has already converted error struct literals to IError init expressions,
// so this just calls gen_expr.
fn (mut g Gen) gen_error_expr(expr ast.Expr) {
	g.gen_expr(expr)
}

// Check if an expression is a call to error() or error_with_code() or a function returning IError
// Also checks for IError init expressions (from transformer) and direct construction of error types.
// Used to detect error returns in functions where result type was simplified
fn (mut g Gen) is_error_return(expr ast.Expr) bool {
	// Check for IError init expression (from transformer boxing error struct literals)
	if expr is ast.InitExpr {
		if expr.typ is ast.Ident && expr.typ.name == 'IError' {
			return true
		}
		// Also check for error struct literals (legacy/untransformed code)
		type_name := g.expr_type_to_c(expr.typ)
		base_name := if type_name.contains('__') {
			type_name.all_after_last('__')
		} else {
			type_name
		}
		if base_name in ['Eof', 'NotExpected', 'MessageError', 'Error'] {
			return true
		}
	}
	if expr is ast.CallExpr {
		if expr.lhs is ast.Ident {
			name := expr.lhs.name
			// Direct error functions
			if name in ['error', 'error_with_code'] {
				return true
			}
			// Check if the function returns IError
			if ret_type := g.fn_types[name] {
				if ret_type == 'IError' {
					return true
				}
			}
			// Also check with current module prefix
			if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
				mangled_name := '${g.cur_module}__${name}'
				if ret_type := g.fn_types[mangled_name] {
					if ret_type == 'IError' {
						return true
					}
				}
			}
		}
	}
	if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.Ident {
			name := expr.lhs.name
			if name in ['error', 'error_with_code'] {
				return true
			}
			// Check if the function returns IError
			if ret_type := g.fn_types[name] {
				if ret_type == 'IError' {
					return true
				}
			}
			// Check if this is a cast to an error type like (Eof){}
			if name in ['Eof', 'NotExpected', 'MessageError', 'Error'] {
				return true
			}
		} else if expr.lhs is ast.SelectorExpr {
			// Check for module.ErrorType like os.Eof
			sel := expr.lhs as ast.SelectorExpr
			if sel.rhs.name in ['Eof', 'NotExpected', 'Error'] {
				return true
			}
		}
	}
	return false
}

// Check if an expression is an error propagation (panic, return, or similar)
// Used to detect or-block expressions that should propagate errors
fn (g Gen) is_error_propagation(expr ast.Expr) bool {
	if expr is ast.CallExpr {
		if expr.lhs is ast.Ident {
			return expr.lhs.name in ['panic', 'exit', 'error', 'error_with_code']
		}
	}
	if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.Ident {
			return expr.lhs.name in ['panic', 'exit', 'error', 'error_with_code']
		}
	}
	return false
}

// Check if an IfExpr can be converted to a C ternary operator
// This is true when the branches contain simple value expressions (single ExprStmt)
fn (g Gen) can_be_ternary(node ast.IfExpr) bool {
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
	// Exclude complex expressions that can't be used in ternary (MatchExpr, IfExpr as statement, ComptimeExpr)
	expr_stmt := stmt as ast.ExprStmt
	if expr_stmt.expr is ast.MatchExpr {
		return false
	}
	// ComptimeExpr ($if) can expand to statements like for loops - can't be ternary
	if expr_stmt.expr is ast.ComptimeExpr {
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

// Generate for-in loop: dispatches to range or array handling
fn (mut g Gen) gen_for_in(node ast.ForStmt, for_in ast.ForInStmt) {
	if for_in.expr is ast.RangeExpr {
		g.gen_for_in_range(node, for_in)
	} else {
		// Map iteration is handled by the transformer, so only arrays reach here
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

	// Check if array expression is a pointer type (e.g., mut receiver)
	is_ptr := arr_type.ends_with('*')
	clean_arr_type := if is_ptr { arr_type[..arr_type.len - 1] } else { arr_type }
	member_op := if is_ptr { '->' } else { '.' }

	// Determine element type and data accessor
	mut elem_type := 'int'
	mut data_accessor := '${member_op}data'
	if clean_arr_type == 'string' {
		// For strings, iterate over bytes using .str
		elem_type = 'u8'
		data_accessor = '${member_op}str'
	} else if clean_arr_type.starts_with('Array_') {
		raw_elem := clean_arr_type['Array_'.len..]
		// Always unsanitize - converts Typeptr to Type* for proper C type
		elem_type = unsanitize_type_for_c(raw_elem)
	} else if clean_arr_type.starts_with('VArg_') {
		// Variadic args (VArg_string, VArg_int, etc.) have same structure as arrays
		raw_elem := clean_arr_type['VArg_'.len..]
		// Always unsanitize - converts Typeptr to Type* for proper C type
		elem_type = unsanitize_type_for_c(raw_elem)
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
	g.sb.writeln('${member_op}len; ${idx_var}++) {')
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
// Uses iterator protocol: call next() until it returns none (state != 0)
fn (mut g Gen) gen_for_in_iterator(node ast.ForStmt, for_in ast.ForInStmt, iter_type string, value_name string) {
	// Determine the element type (e.g. RunesIterator -> rune)
	elem_type := if iter_type == 'RunesIterator' {
		'rune'
	} else {
		'int'
	}

	// Register variable type
	g.var_types[value_name] = elem_type

	// Get the option type for the element
	option_type := g.get_option_type(elem_type)

	// Generate:
	// { IterType _iter = expr;
	//   ElemType value;
	//   _option_ElemType _opt;
	//   while ((_opt = IterType__next(&_iter), _opt.state == 0)) {
	//       value = *(ElemType*)_opt.data;
	//       ...
	//   }
	// }
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

	// Declare option temporary variable
	g.write_indent()
	g.sb.writeln('${option_type} _opt_${value_name};')

	// Generate while loop using iterator next() with option unwrapping
	g.write_indent()
	g.sb.writeln('while ((_opt_${value_name} = ${iter_type}__next(&_iter_${value_name}), _opt_${value_name}.state == 0)) {')
	g.indent++

	// Extract value from option
	g.write_indent()
	g.sb.writeln('${value_name} = *(${elem_type}*)_opt_${value_name}.data;')

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
	} else if lhs_type.starts_with('Array_') || lhs_type == 'array' {
		// For Array struct types - use array__slice (builtin method without module prefix)
		g.sb.write_string('array__slice(')
		g.gen_expr(base)
		g.sb.write_string(', ')
		g.gen_expr(range_expr.start)
		g.sb.write_string(', ')
		if range_expr.end is ast.EmptyExpr {
			// arr[start..] - slice to end
			g.gen_expr(base)
			g.sb.write_string('.len')
		} else {
			g.gen_expr(range_expr.end)
		}
		g.sb.write_string(')')
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
	// For return statements, if the function returns an enum, use that for shorthand resolution
	// This handles cases like: return match c { `b` { .binary } ... }
	// Also check for Result/Option types that wrap an enum (e.g., _result_ast__StringInterFormat)
	if g.cur_fn_ret_type in g.enum_names {
		g.cur_match_type = g.cur_fn_ret_type
	} else if g.cur_fn_returns_result && g.cur_fn_result_base_type in g.enum_names {
		g.cur_match_type = g.cur_fn_result_base_type
	} else if g.cur_fn_returns_option && g.cur_fn_option_base_type in g.enum_names {
		g.cur_match_type = g.cur_fn_option_base_type
	} else {
		g.cur_match_type = match_type
	}
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

	// Track sum type match variable (for field access casting in branches)
	old_sumtype_match_var := g.cur_sumtype_match_var
	old_sumtype_match_variant := g.cur_sumtype_match_variant
	old_sumtype_match_type := g.cur_sumtype_match_type
	if match_type in g.sum_type_names {
		g.cur_sumtype_match_var = if node.expr is ast.Ident { node.expr.name } else { '' }
		g.cur_sumtype_match_type = match_type
	}
	defer {
		g.cur_sumtype_match_var = old_sumtype_match_var
		g.cur_sumtype_match_variant = old_sumtype_match_variant
		g.cur_sumtype_match_type = old_sumtype_match_type
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
			g.cur_sumtype_match_variant = '' // No variant in else branch
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
							// Check if this is a panic/exit call (returns void, never returns)
							is_panic := g.is_error_propagation(stmt.expr)
							if is_panic {
								// Generate panic call without return, then return default value
								g.write_indent()
								g.gen_expr(stmt.expr)
								g.sb.writeln(';')
								g.write_indent()
								g.sb.writeln('return (${g.cur_fn_ret_type}){0};')
							} else {
								g.write_indent()
								// Wrap in Result/Option if needed
								if g.cur_fn_returns_result {
									g.sb.write_string('return ({ ${g.cur_fn_ret_type} _r; *(${g.cur_fn_result_base_type}*)_r.data = ')
									g.gen_expr(stmt.expr)
									g.sb.writeln('; _r.is_error = false; _r; });')
								} else if g.cur_fn_returns_option {
									g.sb.write_string('return ({ ${g.cur_fn_ret_type} _o; *(${g.cur_fn_option_base_type}*)_o.data = ')
									g.gen_expr(stmt.expr)
									g.sb.writeln('; _o.state = 0; _o; });')
								} else {
									g.sb.write_string('return ')
									g.gen_expr(stmt.expr)
									g.sb.writeln(';')
								}
							}
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
			} else if match_type in g.sum_type_names {
				// Sum type match: compare _tag field
				variants := g.sum_type_variants[match_type] or { []string{} }
				// Get variant name from first condition for field access casting
				mut branch_variant := ''
				// Temporarily clear variant context to prevent smart-casting during condition generation
				g.cur_sumtype_match_variant = ''
				for ci, c in branch.cond {
					if ci > 0 {
						g.sb.write_string(' || ')
					}
					// Get variant name from condition
					variant_name := if c is ast.Ident {
						c.name
					} else {
						'unknown'
					}
					if ci == 0 {
						branch_variant = variant_name
					}
					// Find tag value (index in variants list)
					mut tag_value := -1
					for vi, v in variants {
						if v == variant_name {
							tag_value = vi
							break
						}
					}
					if tag_value >= 0 {
						g.sb.write_string('(')
						g.gen_expr(node.expr)
						g.sb.write_string(')._tag == ${tag_value}')
					} else {
						g.sb.write_string('/* unknown variant: ${variant_name} */ 0')
					}
				}
				// Restore and set variant context for field access casting
				g.cur_sumtype_match_variant = branch_variant
			} else {
				// match expr { val1 { ... } } -> if (expr == val1)
				// Temporarily use match_type for condition generation (the match expression type)
				// This is critical when the return type differs from the match expression type
				// e.g., fn (t Token) get_power() Power { return match t { .plus { .low } } }
				// The condition .plus should resolve to Token, not Power
				saved_match_type := g.cur_match_type
				g.cur_match_type = match_type
				for ci, c in branch.cond {
					if ci > 0 {
						g.sb.write_string(' || ')
					}
					if match_type == 'string' {
						// String comparison: use string__eq function
						g.sb.write_string('string__eq(')
						g.gen_expr(node.expr)
						g.sb.write_string(', ')
						g.gen_expr(c)
						g.sb.write_string(')')
					} else {
						g.sb.write_string('(')
						g.gen_expr(node.expr)
						g.sb.write_string(' == ')
						g.gen_expr(c)
						g.sb.write_string(')')
					}
				}
				g.cur_match_type = saved_match_type
			}
			g.sb.writeln(') {')
			g.indent++
			// Generate statements, last one should be the return value
			if branch.stmts.len > 0 {
				for j, stmt in branch.stmts {
					if j == branch.stmts.len - 1 {
						// Last statement is the return value
						if stmt is ast.ExprStmt {
							// Check if this is a panic/exit call (returns void, never returns)
							is_panic := g.is_error_propagation(stmt.expr)
							if is_panic {
								// Generate panic call without return, then return default value
								g.write_indent()
								g.gen_expr(stmt.expr)
								g.sb.writeln(';')
								g.write_indent()
								g.sb.writeln('return (${g.cur_fn_ret_type}){0};')
							} else {
								g.write_indent()
								// Wrap in Result/Option if needed
								if g.cur_fn_returns_result {
									g.sb.write_string('return ({ ${g.cur_fn_ret_type} _r; *(${g.cur_fn_result_base_type}*)_r.data = ')
									g.gen_expr(stmt.expr)
									g.sb.writeln('; _r.is_error = false; _r; });')
								} else if g.cur_fn_returns_option {
									g.sb.write_string('return ({ ${g.cur_fn_ret_type} _o; *(${g.cur_fn_option_base_type}*)_o.data = ')
									g.gen_expr(stmt.expr)
									g.sb.writeln('; _o.state = 0; _o; });')
								} else {
									g.sb.write_string('return ')
									g.gen_expr(stmt.expr)
									g.sb.writeln(';')
								}
							}
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

	// Check if this is a Result type wrapping a tuple
	is_result_tuple := tuple_type.starts_with('_result_Tuple_')
	is_option_tuple := tuple_type.starts_with('_option_Tuple_')
	base_tuple_type := if is_result_tuple {
		tuple_type['_result_'.len..]
	} else if is_option_tuple {
		tuple_type['_option_'.len..]
	} else {
		tuple_type
	}

	// Get element types from tuple_types map if available
	elem_types := g.tuple_types[base_tuple_type] or { []string{len: node.lhs.len, init: 'int'} }

	// Get a unique temp variable name
	tmp_name := '__tmp_${g.tmp_counter}'
	g.tmp_counter++

	// Generate: TupleN_type __tmp = func();
	g.write_indent()
	g.sb.write_string('${tuple_type} ${tmp_name} = ')
	g.gen_expr(rhs)
	g.sb.writeln(';')

	// For Result/Option types, we need to unwrap to access the tuple fields
	accessor := if is_result_tuple || is_option_tuple {
		'(*(${base_tuple_type}*)${tmp_name}.data)'
	} else {
		tmp_name
	}

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
			// Declaration: Type a = __tmp.f0; (or unwrapped for Result/Option)
			g.var_types[name] = elem_type
			g.sb.writeln('${elem_type} ${name} = ${accessor}.f${i};')
		} else {
			// Assignment: a = __tmp.f0;
			g.sb.writeln('${name} = ${accessor}.f${i};')
		}
	}
}
