// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module ssa

import v2.ast
import v2.types
import v2.token

pub struct Builder {
mut:
	mod       &Module
	cur_func  int     = -1
	cur_block BlockID = -1

	// Type checker environment with populated scopes
	env &types.Environment = unsafe { nil }

	// Current module being processed (for type lookups)
	cur_module string = 'main'

	// Maps AST variable name to SSA ValueID (pointer to stack slot)
	vars map[string]ValueID

	// Maps variable name to struct type name (for method resolution)
	var_struct_types map[string]string

	// Stack for break/continue targets
	loop_stack []LoopInfo

	// Maps struct name to TypeID
	struct_types map[string]TypeID

	// Current match expression type for enum shorthand
	cur_match_type string

	// Deferred statements for current function (executed in reverse order at return)
	defer_stmts [][]ast.Stmt

	// Maps enum value (EnumName__field) to integer value
	enum_values map[string]int

	// Maps variable name to array length (for fixed-size arrays)
	var_array_sizes map[string]int

	// Maps variable name to map type info (key_bytes, value_bytes)
	var_map_types map[string][2]int // Variable name -> [key_bytes, value_bytes]

	// Interface support
	interface_names      map[string]bool     // Track interface type names
	interface_meths      map[string][]string // Interface name -> method names
	type_methods         map[string][]string // Type name -> method names (for vtable)
	iface_concrete_types map[string]string   // Variable name -> concrete type (for interface vars)

	// Expression type tracking (v2.types integration)
	var_types      map[string]types.Type // Variable name -> v2.types.Type
	func_ret_types map[string]TypeID     // Function name -> return TypeID

	// Function pointer field support
	fn_type_aliases map[string]bool // Type names that are function types (e.g., MapEqFn)
	fn_ptr_fields   map[string]bool // Struct.field combinations that are function pointers

	// Type alias resolution for method calls (e.g., Builder -> array)
	type_alias_bases map[string]string // Type alias name -> base type name for methods

	// Enum type names for cast detection
	enum_names map[string]bool

	// Flag enum type names for shorthand resolution
	flag_enum_names map[string]bool
}

struct LoopInfo {
	head BlockID
	exit BlockID
}

pub fn Builder.new(mod &Module) &Builder {
	return Builder.new_with_env(mod, unsafe { nil })
}

pub fn Builder.new_with_env(mod &Module, env &types.Environment) &Builder {
	mut b := &Builder{
		mod:                  mod
		vars:                 map[string]ValueID{}
		var_struct_types:     map[string]string{}
		loop_stack:           []LoopInfo{}
		struct_types:         map[string]TypeID{}
		enum_values:          map[string]int{}
		var_array_sizes:      map[string]int{}
		var_map_types:        map[string][2]int{}
		interface_names:      map[string]bool{}
		interface_meths:      map[string][]string{}
		type_methods:         map[string][]string{}
		iface_concrete_types: map[string]string{}
		var_types:            map[string]types.Type{}
		func_ret_types:       map[string]TypeID{}
		fn_type_aliases:      map[string]bool{}
		fn_ptr_fields:        map[string]bool{}
		type_alias_bases:     map[string]string{}
		enum_names:           map[string]bool{}
		flag_enum_names:      map[string]bool{}
	}
	unsafe {
		b.env = env
		// Also store environment on the module for backends to use
		mod.env = env
	}
	return b
}

// lookup_type_in_scope looks up a type by name in the environment's scopes.
// First tries module scope, then builtin scope.
// Returns none if not found or if env is nil.
fn (b &Builder) lookup_type_in_scope(name string, module_name string) ?types.Type {
	if b.env == unsafe { nil } {
		return none
	}
	// Try module scope first
	mut scope := lock b.env.scopes {
		b.env.scopes[module_name] or {
			// Try builtin scope as fallback
			b.env.scopes['builtin'] or { return none }
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
// Falls back to the local struct_types map if not found in environment.
fn (mut b Builder) lookup_struct_type(name string) ?types.Struct {
	// Try environment first - check current module, then builtin
	if typ := b.lookup_type_in_scope(name, b.cur_module) {
		if typ is types.Struct {
			return typ
		}
	}
	if typ := b.lookup_type_in_scope(name, 'builtin') {
		if typ is types.Struct {
			return typ
		}
	}
	// Fallback: check if we have it in struct_types but can't get full info
	// This path is used when type checker is skipped
	return none
}

// lookup_var_type_from_env looks up a variable's type from the environment's scopes.
// Returns the type if found, none otherwise.
fn (b &Builder) lookup_var_type_from_env(name string) ?types.Type {
	if b.env == unsafe { nil } {
		return none
	}
	// Try current module scope first
	mut scope := lock b.env.scopes {
		b.env.scopes[b.cur_module] or {
			// Try builtin scope as fallback
			b.env.scopes['builtin'] or { return none }
		}
	}
	if obj := scope.lookup_parent(name, 0) {
		return obj.typ()
	}
	return none
}

// extract_type_name extracts the name from a types.Type
// Only handles public types; for private types (Enum, Interface, SumType),
// the local var_struct_types map should be used instead.
fn (b &Builder) extract_type_name(t types.Type) string {
	match t {
		types.Struct {
			return t.name
		}
		types.Alias {
			return t.name
		}
		types.String {
			return 'string'
		}
		types.Array {
			return 'Array'
		}
		types.Pointer {
			// For pointer types, get the base type name
			return b.extract_type_name(t.base_type)
		}
		else {
			return ''
		}
	}
}

// get_type_name_from_env extracts the type name for a variable from the environment.
// Returns the type name string if found, empty string otherwise.
fn (b &Builder) get_type_name_from_env(var_name string) string {
	if typ := b.lookup_var_type_from_env(var_name) {
		return b.extract_type_name(typ)
	}
	return ''
}

// get_var_struct_type looks up the struct type name for a variable.
// First checks the local var_struct_types map, then falls back to environment lookup.
fn (b &Builder) get_var_struct_type(var_name string) ?string {
	// First try local tracking map (set during SSA building)
	if struct_type := b.var_struct_types[var_name] {
		return struct_type
	}
	// Fall back to environment lookup
	type_name := b.get_type_name_from_env(var_name)
	if type_name != '' {
		return type_name
	}
	return none
}

// Convert v2.types.Type to SSA TypeID
fn (mut b Builder) type_to_ssa(t types.Type) TypeID {
	match t {
		types.Primitive {
			if t.props.has(.integer) {
				size := if t.size == 0 { 64 } else { int(t.size) } // int defaults to 64-bit
				return b.mod.type_store.get_int(size)
			} else if t.props.has(.float) {
				return b.mod.type_store.get_float(int(t.size))
			} else if t.props.has(.boolean) {
				return b.mod.type_store.get_int(8) // bool as i8
			}
			return b.mod.type_store.get_int(64) // fallback
		}
		types.Pointer {
			elem_type := b.type_to_ssa(t.base_type)
			return b.mod.type_store.get_ptr(elem_type)
		}
		types.Array {
			elem_type := b.type_to_ssa(t.elem_type)
			return b.mod.type_store.get_ptr(elem_type) // arrays are pointers in SSA
		}
		types.Struct {
			// Check if already registered
			if struct_id := b.struct_types[t.name] {
				return struct_id
			}
			// Convert and register
			mut ssa_fields := []TypeID{}
			mut ssa_field_names := []string{}
			for field in t.fields {
				ssa_fields << b.type_to_ssa(field.typ)
				ssa_field_names << field.name
			}
			struct_id := b.mod.type_store.register(Type{
				kind:        .struct_t
				fields:      ssa_fields
				field_names: ssa_field_names
			})
			b.struct_types[t.name] = struct_id
			return struct_id
		}
		types.String {
			// String is a special type - return the string struct
			if struct_id := b.struct_types['string'] {
				return struct_id
			}
			// Should not happen if register_string_type() was called
			return 0
		}
		types.Alias {
			return b.type_to_ssa(t.base_type)
		}
		types.Char {
			return b.mod.type_store.get_int(8)
		}
		types.Void {
			return 0 // void type
		}
		else {
			// Fallback for unhandled types
			return b.mod.type_store.get_int(64)
		}
	}
}

// ast_type_to_ssa converts an AST type expression to an SSA TypeID.
// This is used when we have type annotations in declarations.
fn (mut b Builder) ast_type_to_ssa(typ ast.Expr) TypeID {
	match typ {
		ast.Ident {
			// Check for primitive types
			match typ.name {
				'int' {
					return b.mod.type_store.get_int(64) // V's int is platform-dependent, default 64-bit
				}
				'i8' {
					return b.mod.type_store.get_int(8)
				}
				'i16' {
					return b.mod.type_store.get_int(16)
				}
				'i32' {
					return b.mod.type_store.get_int(32)
				}
				'i64' {
					return b.mod.type_store.get_int(64)
				}
				'u8', 'byte' {
					return b.mod.type_store.get_int(8)
				}
				'u16' {
					return b.mod.type_store.get_int(16)
				}
				'u32' {
					return b.mod.type_store.get_int(32)
				}
				'u64' {
					return b.mod.type_store.get_int(64)
				}
				'f32' {
					return b.mod.type_store.get_float(32)
				}
				'f64' {
					return b.mod.type_store.get_float(64)
				}
				'bool' {
					return b.mod.type_store.get_int(8) // bool as i8
				}
				'isize', 'usize' {
					return b.mod.type_store.get_int(64) // Platform-dependent, default 64-bit
				}
				'rune' {
					return b.mod.type_store.get_int(32) // Unicode code point
				}
				'char' {
					return b.mod.type_store.get_int(8)
				}
				'string' {
					if struct_id := b.struct_types['string'] {
						return struct_id
					}
					return 0 // Should not happen since string is pre-registered
				}
				'voidptr', 'charptr', 'byteptr' {
					i8_t := b.mod.type_store.get_int(8)
					return b.mod.type_store.get_ptr(i8_t)
				}
				else {
					// Check if it's a struct type
					if struct_t := b.struct_types[typ.name] {
						return struct_t
					}
					// Default to i64 for unknown types
					return b.mod.type_store.get_int(64)
				}
			}
		}
		ast.PrefixExpr {
			// Pointer type like &T
			if typ.op == .amp {
				elem_type := b.ast_type_to_ssa(typ.expr)
				return b.mod.type_store.get_ptr(elem_type)
			}
		}
		ast.Type {
			// Handle ast.Type variants
			match typ {
				ast.ArrayType {
					elem_type := b.ast_type_to_ssa(typ.elem_type)
					return b.mod.type_store.get_ptr(elem_type) // arrays as pointers
				}
				ast.ArrayFixedType {
					elem_type := b.ast_type_to_ssa(typ.elem_type)
					mut length := 0
					if typ.len is ast.BasicLiteral {
						if typ.len.kind == .number {
							length = typ.len.value.int()
						}
					}
					return b.mod.type_store.get_array(elem_type, length)
				}
				ast.OptionType {
					// Option types - for now, same as base type
					return b.ast_type_to_ssa(typ.base_type)
				}
				ast.ResultType {
					// Result types - for now, same as base type
					return b.ast_type_to_ssa(typ.base_type)
				}
				ast.TupleType {
					// Tuple type for multi-return functions
					if typ.types.len == 0 {
						return 0 // void
					}
					if typ.types.len == 1 {
						return b.ast_type_to_ssa(typ.types[0])
					}
					// Multi-value tuple - create a struct type
					mut elem_types := []TypeID{}
					for e in typ.types {
						elem_types << b.ast_type_to_ssa(e)
					}
					return b.mod.type_store.get_tuple(elem_types)
				}
				else {}
			}
		}
		ast.EmptyExpr {
			// EmptyExpr means void return type or unspecified type
			return 0 // void
		}
		ast.Tuple {
			// Tuple type for multi-return functions
			if typ.exprs.len == 0 {
				return 0 // void
			}
			if typ.exprs.len == 1 {
				return b.ast_type_to_ssa(typ.exprs[0])
			}
			// Multi-value tuple - create a struct type
			mut elem_types := []TypeID{}
			for e in typ.exprs {
				elem_types << b.ast_type_to_ssa(e)
			}
			return b.mod.type_store.get_tuple(elem_types)
		}
		else {}
	}
	// Default fallback for unknown types
	return b.mod.type_store.get_int(64)
}

// infer_literal_type determines the type of a literal value.
// Returns an SSA TypeID based on the literal's content.
fn (mut b Builder) infer_literal_type(value string) TypeID {
	// Check for float literal
	if value.contains('.') || value.contains('e') || value.contains('E') {
		return b.mod.type_store.get_float(64) // default to f64
	}
	// Integer literal - default to i64
	return b.mod.type_store.get_int(64)
}

// get_receiver_type_name extracts the base type name from a receiver type expression.
// For both `T` and `&T` receivers, returns just `T` since they share method namespaces in V.
fn (b Builder) get_receiver_type_name(typ ast.Expr) string {
	if typ is ast.Ident {
		return typ.name
	} else if typ is ast.PrefixExpr {
		// Pointer type like &DenseArray - extract base type name
		if typ.expr is ast.Ident {
			return typ.expr.name
		}
	}
	return ''
}

// register_builtin_types registers built-in types like string that aren't
// declared as structs in source but need SSA struct type representations.
fn (mut b Builder) register_builtin_types() {
	// Register string struct type: { str &u8, len int, is_lit int }
	// This is 24 bytes on 64-bit: 8 (ptr) + 8 (int) + 8 (int)
	if _ := b.struct_types['string'] {
		return
	}
	i64_t := b.mod.type_store.get_int(64)
	i8_t := b.mod.type_store.get_int(8)
	ptr_t := b.mod.type_store.get_ptr(i8_t)
	string_id := b.mod.type_store.register(Type{
		kind:        .struct_t
		fields:      [ptr_t, i64_t, i64_t]
		field_names: ['str', 'len', 'is_lit']
	})
	b.struct_types['string'] = string_id
}

// build_all processes multiple files with proper multi-file ordering:
// 1. Register all types from all files first
// 2. Register all function signatures from all files
// 3. Generate all function bodies
pub fn (mut b Builder) build_all(files []ast.File) {
	// Phase 0: Register built-in types (string, etc.)
	b.register_builtin_types()

	// Phase 1: Register all types from all files
	for file in files {
		b.cur_module = file.mod
		b.build_types(file)
	}
	// Phase 2: Register all function signatures from all files
	for file in files {
		b.cur_module = file.mod
		b.build_fn_signatures(file)
	}
	// Phase 3: Generate all function bodies
	for file in files {
		b.cur_module = file.mod
		b.build_fn_bodies(file)
	}
}

// build_types registers struct types, globals, enums from a single file
pub fn (mut b Builder) build_types(file ast.File) {
	// First pass: register type aliases (needed for function pointer detection)
	for stmt in file.stmts {
		if stmt is ast.TypeDecl {
			b.stmt(stmt)
		}
	}
	// Second pass: register structs, globals, consts, enums, interfaces
	for stmt in file.stmts {
		match stmt {
			ast.StructDecl { b.stmt(stmt) }
			ast.GlobalDecl { b.stmt(stmt) }
			ast.ConstDecl { b.stmt(stmt) }
			ast.EnumDecl { b.stmt(stmt) }
			ast.InterfaceDecl { b.stmt(stmt) }
			else {}
		}
	}
}

// build_fn_signatures registers function signatures from a single file
pub fn (mut b Builder) build_fn_signatures(file ast.File) {
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			// Skip operator overloads (parsed with empty name)
			if stmt.name == '' {
				continue
			}
			i64_t := b.mod.type_store.get_int(64)

			// Determine return type from the function declaration
			ret_type := b.ast_type_to_ssa(stmt.typ.return_type)

			// For C functions (fn C.xxx), only track the return type for calls
			// Don't create an SSA function since they're externally defined
			if stmt.language == .c {
				b.func_ret_types[stmt.name] = ret_type
				// Generate stub implementations for critical C functions that
				// aren't available when doing native codegen
				if stmt.name == 'wyhash' || stmt.name == 'wyhash64' {
					b.generate_wyhash_stub(stmt.name, ret_type)
				}
				continue
			}

			// Map params with proper types
			mut param_types := []TypeID{}

			// For methods, add receiver as first parameter (always as pointer)
			if stmt.is_method {
				if stmt.receiver.typ is ast.Ident {
					if struct_t := b.struct_types[stmt.receiver.typ.name] {
						param_types << b.mod.type_store.get_ptr(struct_t)
					} else {
						param_types << i64_t
					}
				} else {
					param_types << b.ast_type_to_ssa(stmt.receiver.typ)
				}
			}

			// Map parameter types from declarations
			// For struct types (like string), pass as pointers to match ARM64 ABI
			// where large structs (>16 bytes) are passed by reference
			for param in stmt.typ.params {
				param_type := b.ast_type_to_ssa(param.typ)
				// Check if this is a struct type that should be passed by reference
				type_info := b.mod.type_store.types[param_type]
				if type_info.kind == .struct_t {
					// Struct types are passed as pointers
					param_types << b.mod.type_store.get_ptr(param_type)
				} else {
					param_types << param_type
				}
			}

			// Create Function Skeleton
			// For methods, use mangled name: TypeName__methodName
			// Operator overloads need special mangling: + -> __plus, - -> __minus, etc.
			mut fn_name := stmt.name
			if stmt.is_method {
				mut receiver_type_name := b.get_receiver_type_name(stmt.receiver.typ)
				// Resolve type aliases for method name mangling
				if base_type := b.type_alias_bases[receiver_type_name] {
					receiver_type_name = base_type
				}
				if receiver_type_name != '' {
					// Mangle operator names to valid symbol names
					// Note: use single word names since we already add '__' separator below
					method_name := match stmt.name {
						'+' { 'plus' }
						'-' { 'minus' }
						'*' { 'mul' }
						'/' { 'div' }
						'%' { 'mod' }
						'==' { 'eq' }
						'!=' { 'ne' }
						'<' { 'lt' }
						'>' { 'gt' }
						'<=' { 'le' }
						'>=' { 'ge' }
						else { stmt.name }
					}
					fn_name = '${receiver_type_name}__${method_name}'
					// Track methods per type for vtable generation
					if receiver_type_name !in b.type_methods {
						b.type_methods[receiver_type_name] = []string{}
					}
					if stmt.name !in b.type_methods[receiver_type_name] {
						b.type_methods[receiver_type_name] << stmt.name
					}
				}
			}
			// Create the function with proper return type
			b.mod.new_function(fn_name, ret_type, param_types)
			// Track return type for call expressions
			b.func_ret_types[fn_name] = ret_type
		}
	}
}

// build_fn_bodies generates function bodies from a single file
pub fn (mut b Builder) build_fn_bodies(file ast.File) {
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			// Skip operator overloads (parsed with empty name)
			if stmt.name == '' {
				continue
			}
			// Skip C functions - they're externally defined
			if stmt.language == .c {
				continue
			}
			// Get mangled function name (same logic as first pass)
			mut fn_name := stmt.name
			if stmt.is_method {
				mut receiver_type_name := b.get_receiver_type_name(stmt.receiver.typ)
				// Resolve type aliases for method name mangling
				if base_type := b.type_alias_bases[receiver_type_name] {
					receiver_type_name = base_type
				}
				if receiver_type_name != '' {
					// Mangle operator names to valid symbol names
					// Note: use single word names since we already add '__' separator below
					method_name := match stmt.name {
						'+' { 'plus' }
						'-' { 'minus' }
						'*' { 'mul' }
						'/' { 'div' }
						'%' { 'mod' }
						'==' { 'eq' }
						'!=' { 'ne' }
						'<' { 'lt' }
						'>' { 'gt' }
						'<=' { 'le' }
						'>=' { 'ge' }
						else { stmt.name }
					}
					fn_name = '${receiver_type_name}__${method_name}'
				}
			}
			// Find function by name
			fn_idx := b.find_function(fn_name)
			if fn_idx < 0 {
				continue // Should not happen
			}
			// Skip if already built (has blocks)
			if b.mod.funcs[fn_idx].blocks.len > 0 {
				continue
			}
			b.build_fn(stmt, fn_idx)
		}
	}
}

// build processes a single file (legacy method for backward compatibility)
pub fn (mut b Builder) build(file ast.File) {
	b.build_types(file)
	b.build_fn_signatures(file)
	b.build_fn_bodies(file)
}

// find_function looks up a function by name, returns -1 if not found
fn (b &Builder) find_function(name string) int {
	for i, f in b.mod.funcs {
		if f.name == name {
			return i
		}
	}
	return -1
}

// infer_receiver_type attempts to find the receiver type for a method call
// by looking at registered functions to see which type defines this method
fn (b &Builder) infer_receiver_type(method_name string) string {
	// Search registered functions for a matching method
	for f in b.mod.funcs {
		// Check if function name matches pattern: TypeName__methodName (double underscore)
		if f.name.ends_with('__${method_name}') {
			// Extract type name (everything before __methodName)
			type_name := f.name.all_before_last('__${method_name}')
			if type_name != '' {
				return type_name
			}
		}
	}
	return ''
}

// generate_flag_enum_has generates the has() method for flag enums
// The method checks if a flag is set: (self & flag) != 0
fn (mut b Builder) generate_flag_enum_has(enum_name string, field_type TypeID) {
	fn_name := '${enum_name}__has'

	// Create function with bool return type
	bool_t := b.mod.type_store.get_int(1)
	fn_id := b.mod.new_function(fn_name, bool_t, [field_type, field_type])

	b.cur_func = fn_id
	b.vars.clear()
	b.var_struct_types.clear()
	b.var_types.clear()

	// Create entry block
	entry := b.mod.add_block(fn_id, 'entry')
	b.cur_block = entry

	// Parameters: self (receiver), flag (argument)
	self_param := b.mod.add_value_node(.argument, field_type, 'self', 0)
	b.mod.funcs[fn_id].params << self_param
	b.vars['self'] = self_param

	flag_param := b.mod.add_value_node(.argument, field_type, 'flag', 1)
	b.mod.funcs[fn_id].params << flag_param
	b.vars['flag'] = flag_param

	eprintln('  after adding params: ${b.mod.funcs[fn_id].params.len}')

	// Compute: self & flag
	and_result := b.mod.add_instr(.and_, b.cur_block, field_type, [self_param, flag_param])

	// Compare: (self & flag) != 0
	zero := b.mod.add_value_node(.constant, field_type, '0', 0)
	cmp_result := b.mod.add_instr(.ne, b.cur_block, bool_t, [and_result, zero])

	// Return
	b.mod.add_instr(.ret, b.cur_block, 0, [cmp_result])

	// Track return type
	b.func_ret_types[fn_name] = bool_t
}

// generate_flag_enum_all generates the all() method for flag enums
// The method checks if all flags are set: (self & flags) == flags
fn (mut b Builder) generate_flag_enum_all(enum_name string, field_type TypeID) {
	fn_name := '${enum_name}__all'

	// Create function with bool return type
	bool_t := b.mod.type_store.get_int(1)
	fn_id := b.mod.new_function(fn_name, bool_t, [field_type, field_type])
	b.cur_func = fn_id
	b.vars.clear()
	b.var_struct_types.clear()
	b.var_types.clear()

	// Create entry block
	entry := b.mod.add_block(fn_id, 'entry')
	b.cur_block = entry

	// Parameters: self (receiver), flags (argument)
	self_param := b.mod.add_value_node(.argument, field_type, 'self', 0)
	b.mod.funcs[fn_id].params << self_param
	b.vars['self'] = self_param

	flags_param := b.mod.add_value_node(.argument, field_type, 'flags', 1)
	b.mod.funcs[fn_id].params << flags_param
	b.vars['flags'] = flags_param

	// Compute: self & flags
	and_result := b.mod.add_instr(.and_, b.cur_block, field_type, [self_param, flags_param])

	// Compare: (self & flags) == flags
	cmp_result := b.mod.add_instr(.eq, b.cur_block, bool_t, [and_result, flags_param])

	// Return
	b.mod.add_instr(.ret, b.cur_block, 0, [cmp_result])

	// Track return type
	b.func_ret_types[fn_name] = bool_t
}

// generate_wyhash_stub generates a stub implementation for wyhash/wyhash64
// These are C library functions needed by map hashing that aren't available
// in native codegen. The stub returns a simple hash based on XOR.
fn (mut b Builder) generate_wyhash_stub(name string, ret_type TypeID) {
	i64_t := b.mod.type_store.get_int(64)

	// Create function
	fn_id := b.mod.new_function(name, ret_type, [i64_t, i64_t])
	b.cur_func = fn_id
	b.vars.clear()
	b.var_struct_types.clear()
	b.var_types.clear()

	// Create entry block
	entry := b.mod.add_block(fn_id, 'entry')
	b.cur_block = entry

	// Parameters
	param0 := b.mod.add_value_node(.argument, i64_t, 'a', 0)
	param1 := b.mod.add_value_node(.argument, i64_t, 'b', 1)

	// Simple hash: return a ^ b (XOR the inputs)
	// This is a placeholder - real wyhash is more complex
	result := b.mod.add_instr(.xor, b.cur_block, ret_type, [param0, param1])

	// Return
	b.mod.add_instr(.ret, b.cur_block, 0, [result])
}

fn (mut b Builder) build_fn(decl ast.FnDecl, fn_id int) {
	b.cur_func = fn_id
	b.vars.clear()
	b.var_struct_types.clear()
	b.var_types.clear()
	b.defer_stmts.clear()

	// Create Entry Block
	entry := b.mod.add_block(fn_id, 'entry')
	b.cur_block = entry

	// Define Arguments
	i32_t := b.mod.type_store.get_int(64)

	// Handle method receiver as first parameter
	// Note: receivers are always passed as pointers internally
	if decl.is_method {
		receiver := decl.receiver
		mut receiver_type := i32_t
		mut struct_type_name := ''

		if receiver.typ is ast.Ident {
			struct_type_name = receiver.typ.name
			if struct_t := b.struct_types[receiver.typ.name] {
				// Always use pointer for receiver (structs are passed by reference)
				receiver_type = b.mod.type_store.get_ptr(struct_t)
			}
		} else if receiver.typ is ast.PrefixExpr {
			// Handle pointer receiver types like &map
			if receiver.typ.op == .amp && receiver.typ.expr is ast.Ident {
				struct_type_name = receiver.typ.expr.name
				if struct_t := b.struct_types[struct_type_name] {
					receiver_type = b.mod.type_store.get_ptr(struct_t)
				}
			}
		}

		// 1. Create Argument Value for receiver
		arg_val := b.mod.add_value_node(.argument, receiver_type, receiver.name, 0)
		b.mod.funcs[fn_id].params << arg_val

		// 2. Allocate Stack Slot
		stack_ptr := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(receiver_type),
			[])

		// 3. Store Argument to Stack
		b.mod.add_instr(.store, entry, 0, [arg_val, stack_ptr])

		// 4. Register variable
		b.vars[receiver.name] = stack_ptr

		// 5. Track struct type for method resolution
		if struct_type_name != '' {
			b.var_struct_types[receiver.name] = struct_type_name
		}
	}

	// FIX: Access params via decl.typ.params
	for _, param in decl.typ.params {
		// Determine actual parameter type
		mut param_type := i32_t
		mut struct_type_name := ''

		// Check if parameter type is a struct (look up by name)
		if param.typ is ast.Ident {
			if struct_t := b.struct_types[param.typ.name] {
				struct_type_name = param.typ.name
				// V passes structs by pointer (reference) for efficiency
				param_type = b.mod.type_store.get_ptr(struct_t)
			}
		} else if param.typ is ast.SelectorExpr {
			// Module-qualified type like strings.Builder
			type_name := param.typ.rhs.name
			if struct_t := b.struct_types[type_name] {
				struct_type_name = type_name
				param_type = b.mod.type_store.get_ptr(struct_t)
			}
		}

		// 1. Create Argument Value
		arg_val := b.mod.add_value_node(.argument, param_type, param.name, 0)
		b.mod.funcs[fn_id].params << arg_val

		// 2. Allocate Stack Slot (so we can modify it if needed)
		stack_ptr := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(param_type),
			[])

		// 3. Store Argument to Stack
		b.mod.add_instr(.store, entry, 0, [arg_val, stack_ptr])

		// 4. Register variable
		b.vars[param.name] = stack_ptr

		// 5. Track struct type for method resolution
		if struct_type_name != '' {
			b.var_struct_types[param.name] = struct_type_name
		}
	}

	// Process Statements
	b.stmts(decl.stmts)
	// FIX: Ensure the function ends with a return to prevent fallthrough
	if !b.is_block_terminated(b.cur_block) {
		// Emit deferred statements before implicit return
		b.emit_deferred_stmts()

		ret_type := b.mod.funcs[fn_id].typ
		if ret_type != 0 {
			// Return 0 for non-void functions (satisfy C signature)
			zero := b.mod.add_value_node(.constant, ret_type, '0', 0)
			b.mod.add_instr(.ret, b.cur_block, 0, [zero])
		} else {
			// void return
			b.mod.add_instr(.ret, b.cur_block, 0, [])
		}
	}
}

fn (mut b Builder) stmts(stmts []ast.Stmt) {
	for s in stmts {
		// Stop processing if block is already terminated (e.g., after return in $if)
		if b.is_block_terminated(b.cur_block) {
			break
		}
		b.stmt(s)
	}
}

// stmt_for_in handles `for i in start..end { ... }` and `for elem in array { ... }`
fn (mut b Builder) stmt_for_in(node ast.ForStmt, for_in ast.ForInStmt) {
	// Get loop variable name(s)
	mut key_name := ''
	mut value_name := ''
	if for_in.key !is ast.EmptyExpr {
		if for_in.key is ast.Ident {
			key_name = for_in.key.name
		} else if for_in.key is ast.ModifierExpr {
			if for_in.key.expr is ast.Ident {
				key_name = for_in.key.expr.name
			}
		}
	}
	if for_in.value is ast.Ident {
		value_name = for_in.value.name
	} else if for_in.value is ast.ModifierExpr {
		if for_in.value.expr is ast.Ident {
			value_name = for_in.value.expr.name
		}
	}

	// Check if this is a range expression or array iteration
	if for_in.expr is ast.RangeExpr {
		// Range iteration: for i in start..end
		b.stmt_for_in_range(node, for_in, value_name)
	} else {
		// Array iteration: for elem in array or for i, elem in array
		b.stmt_for_in_array(node, for_in, key_name, value_name)
	}
}

// stmt_for_in_range handles `for i in start..end { ... }`
fn (mut b Builder) stmt_for_in_range(node ast.ForStmt, for_in ast.ForInStmt, var_name string) {
	i64_t := b.mod.type_store.get_int(64)

	range_expr := for_in.expr as ast.RangeExpr
	start_val := b.expr(range_expr.start)
	end_val := b.expr(range_expr.end)

	// Allocate loop variable on stack
	ptr_t := b.mod.type_store.get_ptr(i64_t)
	var_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
	b.vars[var_name] = var_ptr

	// Initialize loop variable to start
	b.mod.add_instr(.store, b.cur_block, 0, [start_val, var_ptr])

	// Create control flow blocks
	// We use a post block so that continue jumps to the increment, not the condition
	head_blk := b.mod.add_block(b.cur_func, 'for_in.head')
	body_blk := b.mod.add_block(b.cur_func, 'for_in.body')
	post_blk := b.mod.add_block(b.cur_func, 'for_in.post')
	exit_blk := b.mod.add_block(b.cur_func, 'for_in.exit')

	// For continue, we want to jump to post (which increments then goes to head)
	// For break, we want to jump to exit
	b.loop_stack << LoopInfo{
		head: post_blk // continue jumps here (to increment)
		exit: exit_blk // break jumps here
	}

	// Jump to head
	head_val := b.mod.blocks[head_blk].val_id
	b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])

	// Head: check i < end
	b.cur_block = head_blk
	cur_val := b.mod.add_instr(.load, b.cur_block, i64_t, [var_ptr])
	cond := b.mod.add_instr(.lt, b.cur_block, i64_t, [cur_val, end_val])
	body_val := b.mod.blocks[body_blk].val_id
	exit_val := b.mod.blocks[exit_blk].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [cond, body_val, exit_val])

	// Body
	b.cur_block = body_blk
	b.stmts(node.stmts)

	// Jump to post at end of body (if not already terminated by break/continue/return)
	if !b.is_block_terminated(b.cur_block) {
		post_val := b.mod.blocks[post_blk].val_id
		b.mod.add_instr(.jmp, b.cur_block, 0, [post_val])
	}

	// Post: increment i and jump back to head
	b.cur_block = post_blk
	cur_val2 := b.mod.add_instr(.load, b.cur_block, i64_t, [var_ptr])
	one := b.mod.add_value_node(.constant, i64_t, '1', 0)
	new_val := b.mod.add_instr(.add, b.cur_block, i64_t, [cur_val2, one])
	b.mod.add_instr(.store, b.cur_block, 0, [new_val, var_ptr])
	b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])

	// Exit
	b.cur_block = exit_blk
	b.loop_stack.pop()
}

// stmt_for_in_array handles `for elem in array { ... }` and `for i, elem in array { ... }`
fn (mut b Builder) stmt_for_in_array(node ast.ForStmt, for_in ast.ForInStmt, key_name string, value_name string) {
	i64_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i64_t)
	elem_ptr_t := b.mod.type_store.get_ptr(i64_t)

	// Get the array pointer - addr returns pointer to where array ptr is stored
	array_ptr_ptr := b.addr(for_in.expr)
	// Load the actual array pointer
	array_ptr := b.mod.add_instr(.load, b.cur_block, ptr_t, [array_ptr_ptr])

	// For simplicity, we assume arrays have a fixed known length or we get it from context
	// In V's Array struct, length is typically in a .len field
	// For now, we'll need the array length - this is a simplified implementation
	// that assumes arrays are pointers to contiguous memory with a known length

	// Allocate index variable on stack (always need index for array access)
	idx_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
	zero := b.mod.add_value_node(.constant, i64_t, '0', 0)
	b.mod.add_instr(.store, b.cur_block, 0, [zero, idx_ptr])

	// If key variable is specified, register it
	if key_name != '' {
		b.vars[key_name] = idx_ptr
	}

	// Allocate value variable on stack
	value_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
	b.vars[value_name] = value_ptr

	// Create control flow blocks
	head_blk := b.mod.add_block(b.cur_func, 'for_in_arr.head')
	body_blk := b.mod.add_block(b.cur_func, 'for_in_arr.body')
	post_blk := b.mod.add_block(b.cur_func, 'for_in_arr.post')
	exit_blk := b.mod.add_block(b.cur_func, 'for_in_arr.exit')

	b.loop_stack << LoopInfo{
		head: post_blk // continue jumps here (to increment)
		exit: exit_blk // break jumps here
	}

	// Jump to head
	head_val := b.mod.blocks[head_blk].val_id
	b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])

	// Head: check idx < array.len
	b.cur_block = head_blk
	cur_idx := b.mod.add_instr(.load, b.cur_block, i64_t, [idx_ptr])

	// Get array length from tracked sizes or the expression
	mut arr_len_val := 0
	if for_in.expr is ast.Ident {
		if size := b.var_array_sizes[for_in.expr.name] {
			arr_len_val = size
		}
	} else if for_in.expr is ast.ArrayInitExpr {
		arr_len_val = for_in.expr.exprs.len
	}
	if arr_len_val == 0 {
		arr_len_val = 100 // Fallback
	}
	arr_len := b.mod.add_value_node(.constant, i64_t, '${arr_len_val}', 0)

	cond := b.mod.add_instr(.lt, b.cur_block, i64_t, [cur_idx, arr_len])
	body_val := b.mod.blocks[body_blk].val_id
	exit_val := b.mod.blocks[exit_blk].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [cond, body_val, exit_val])

	// Body: load array element into value variable
	b.cur_block = body_blk
	cur_idx2 := b.mod.add_instr(.load, b.cur_block, i64_t, [idx_ptr])
	elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_t, [
		array_ptr,
		cur_idx2,
	])
	elem_val := b.mod.add_instr(.load, b.cur_block, i64_t, [elem_addr])
	b.mod.add_instr(.store, b.cur_block, 0, [elem_val, value_ptr])

	b.stmts(node.stmts)

	if !b.is_block_terminated(b.cur_block) {
		post_val := b.mod.blocks[post_blk].val_id
		b.mod.add_instr(.jmp, b.cur_block, 0, [post_val])
	}

	// Post: increment index and jump back to head
	b.cur_block = post_blk
	cur_idx3 := b.mod.add_instr(.load, b.cur_block, i64_t, [idx_ptr])
	one := b.mod.add_value_node(.constant, i64_t, '1', 0)
	new_idx := b.mod.add_instr(.add, b.cur_block, i64_t, [cur_idx3, one])
	b.mod.add_instr(.store, b.cur_block, 0, [new_idx, idx_ptr])
	b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])

	// Exit
	b.cur_block = exit_blk
	b.loop_stack.pop()
}

fn (mut b Builder) stmt(node ast.Stmt) {
	// println('stmt ${node}')
	match node {
		ast.AssignStmt {
			// x := 10 or x = 10
			// 1. Calc RHS
			if node.rhs.len == 0 {
				println('AssignStmt node.rhs.len == 0')
				println(node)
				return
			}
			if node.lhs.len == 0 {
				println('AssignStmt node.lhs.len == 0')
				println(node)
				return
			}

			// Check for multi-return assignment: a, b := foo()
			if node.lhs.len > 1 && node.rhs.len == 1 && node.op == .decl_assign {
				// Multi-return: evaluate RHS (which returns a tuple)
				tuple_val := b.expr(node.rhs[0])
				tuple_typ := b.mod.values[tuple_val].typ
				tuple_info := b.mod.type_store.types[tuple_typ]

				// Extract each value from the tuple and assign to LHS variables
				for i, lhs_expr in node.lhs {
					mut ident := ast.Ident{}
					if lhs_expr is ast.ModifierExpr {
						ident = lhs_expr.expr as ast.Ident
					} else {
						ident = lhs_expr as ast.Ident
					}
					name := ident.name

					// Skip underscore (discard)
					if name == '_' {
						continue
					}

					// Get element type from tuple
					elem_typ := if i < tuple_info.fields.len {
						tuple_info.fields[i]
					} else {
						b.mod.type_store.get_int(64)
					}

					// Extract value from tuple
					idx := b.mod.add_value_node(.constant, b.mod.type_store.get_int(32),
						'${i}', 0)
					elem_val := b.mod.add_instr(.extractvalue, b.cur_block, elem_typ,
						[
						tuple_val,
						idx,
					])

					// Allocate and store
					ptr_t := b.mod.type_store.get_ptr(elem_typ)
					stack_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
					b.mod.add_instr(.store, b.cur_block, 0, [elem_val, stack_ptr])
					b.vars[name] = stack_ptr
				}
				return
			}

			rhs_val := b.expr(node.rhs[0])

			// 2. Get LHS Address
			// If declaration, allocate new stack slot

			if node.op == .decl_assign {
				mut ident_node := node.lhs[0]
				mut ident := ast.Ident{}
				// Unwrap 'mut x'
				if ident_node is ast.ModifierExpr {
					mod := ident_node as ast.ModifierExpr
					ident = mod.expr as ast.Ident
				} else {
					ident = ident_node as ast.Ident
				}
				// ident := ident_node as ast.Ident
				name := ident.name

				// Track struct/enum type for method resolution and match shorthand
				rhs_expr := node.rhs[0]

				// Special handling for map initialization - can be MapInitExpr or InitExpr with MapType
				mut is_map := false
				if rhs_expr is ast.MapInitExpr {
					is_map = true
				} else if rhs_expr is ast.InitExpr {
					// Check if it's a map init: map[K]V{}
					if rhs_expr.typ is ast.Type {
						if rhs_expr.typ is ast.MapType {
							is_map = true
						}
					}
				}

				if is_map {
					// Track map type - use default int-int for now
					b.var_map_types[name] = [8, 8]!
					// Map init already returns a pointer, use directly
					b.vars[name] = rhs_val
				} else {
					// Alloca for non-map types

					// Get type from RHS or default to i32
					rhs_type := b.mod.values[rhs_val].typ
					ptr_t := b.mod.type_store.get_ptr(rhs_type)

					stack_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])

					// Store
					b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, stack_ptr])
					b.vars[name] = stack_ptr

					if rhs_expr is ast.InitExpr {
						if rhs_expr.typ is ast.Ident {
							b.var_struct_types[name] = rhs_expr.typ.name
						}
					} else if rhs_expr is ast.PrefixExpr {
						// Handle &Point{} heap allocation
						if rhs_expr.expr is ast.InitExpr {
							init_expr := rhs_expr.expr as ast.InitExpr
							if init_expr.typ is ast.Ident {
								b.var_struct_types[name] = init_expr.typ.name
							}
						}
					} else if rhs_expr is ast.SelectorExpr {
						// Track enum type for variables assigned enum values (e.g., color1 := Color.red)
						if rhs_expr.lhs is ast.Ident {
							enum_name := '${rhs_expr.lhs.name}__${rhs_expr.rhs.name}'
							if enum_name in b.enum_values {
								b.var_struct_types[name] = rhs_expr.lhs.name
							}
						}
					} else if rhs_expr is ast.InfixExpr {
						// Track enum type for flag enum combinations (e.g., perms := Permissions.read | Permissions.write)
						// Check if LHS of the binary op is an enum selector
						if rhs_expr.lhs is ast.SelectorExpr {
							sel := rhs_expr.lhs
							if sel.lhs is ast.Ident {
								enum_name := '${sel.lhs.name}__${sel.rhs.name}'
								if enum_name in b.enum_values {
									b.var_struct_types[name] = sel.lhs.name
								}
							}
						}
					} else if rhs_expr is ast.ArrayInitExpr {
						// Track array size for for-in loops
						mut arr_size := rhs_expr.exprs.len
						if rhs_expr.len !is ast.EmptyExpr {
							if rhs_expr.len is ast.BasicLiteral {
								if rhs_expr.len.kind == .number {
									arr_size = rhs_expr.len.value.int()
								}
							}
						}
						if arr_size > 0 {
							b.var_array_sizes[name] = arr_size
						}
					} else if rhs_expr is ast.CallOrCastExpr {
						// Track interface boxing: d := Drawable(point)
						if rhs_expr.lhs is ast.Ident {
							iface_name := rhs_expr.lhs.name
							if iface_name in b.interface_names {
								b.var_struct_types[name] = iface_name
								// Also track the concrete type for direct method calls
								concrete := b.infer_concrete_type(rhs_expr.expr)
								if concrete != 'unknown' {
									b.iface_concrete_types[name] = concrete
								}
							}
						}
					} else if rhs_expr is ast.StringLiteral || rhs_expr is ast.StringInterLiteral {
						// Track string type for variables assigned from string literals
						b.var_struct_types[name] = 'string'
					}
				}
			} else if node.op in [.plus_assign, .minus_assign, .mul_assign, .div_assign] {
				// Compound assignment: x += 1, x -= 1, x *= 2, x /= 2
				ptr := b.addr(node.lhs[0])
				val_typ := b.mod.type_store.types[b.mod.values[ptr].typ].elem_type

				lhs_val := b.mod.add_instr(.load, b.cur_block, val_typ, [ptr])
				op := match node.op {
					.plus_assign { OpCode.add }
					.minus_assign { OpCode.sub }
					.mul_assign { OpCode.mul }
					.div_assign { OpCode.sdiv }
					else { OpCode.add }
				}
				res := b.mod.add_instr(op, b.cur_block, val_typ, [lhs_val, rhs_val])
				b.mod.add_instr(.store, b.cur_block, 0, [res, ptr])
			} else {
				// Check if this is a map assignment: m[key] = value
				lhs_expr := node.lhs[0]
				if lhs_expr is ast.IndexExpr {
					if lhs_expr.lhs is ast.Ident {
						var_name := lhs_expr.lhs.name
						if var_name in b.var_map_types {
							// Map set: use simple inline map set
							map_ptr := b.vars[var_name]
							key_val := b.expr(lhs_expr.expr)
							b.emit_simple_map_set(map_ptr, key_val, rhs_val)
							return
						}
					}
				}
				// Assignment to existing variable, field, or array index
				ptr := b.addr(node.lhs[0])
				b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, ptr])
			}
		}
		ast.ReturnStmt {
			// In V/Go semantics: evaluate return value FIRST, then run defer
			// 1. Evaluate return expression (if any)
			mut ret_val := ValueID(0)
			if node.exprs.len == 1 {
				ret_val = b.expr(node.exprs[0])
			} else if node.exprs.len > 1 {
				// Multi-return: build a tuple from the expressions
				mut elem_vals := []ValueID{}
				mut elem_types := []TypeID{}
				for e in node.exprs {
					val := b.expr(e)
					elem_vals << val
					elem_types << b.mod.values[val].typ
				}
				tuple_t := b.mod.type_store.get_tuple(elem_types)
				// Build tuple using insertvalue instructions
				mut tuple_val := b.mod.add_value_node(.constant, tuple_t, 'undef', 0)
				for i, val in elem_vals {
					idx := b.mod.add_value_node(.constant, b.mod.type_store.get_int(32),
						'${i}', 0)
					tuple_val = b.mod.add_instr(.insertvalue, b.cur_block, tuple_t, [
						tuple_val,
						val,
						idx,
					])
				}
				ret_val = tuple_val
			}
			// 2. Execute deferred statements in reverse order
			b.emit_deferred_stmts()
			// 3. Return the pre-computed value
			if node.exprs.len > 0 {
				b.mod.add_instr(.ret, b.cur_block, 0, [ret_val])
			} else {
				b.mod.add_instr(.ret, b.cur_block, 0, [])
			}
		}
		ast.DeferStmt {
			// Collect deferred statements to be executed before return
			b.defer_stmts << node.stmts
		}
		ast.ExprStmt {
			b.expr(node.expr)
		}
		ast.BlockStmt {
			b.stmts(node.stmts)
		}
		ast.ForStmt {
			// Check if this is a for-in loop: `for i in 1..10` or `for elem in array`
			if node.init is ast.ForInStmt {
				b.stmt_for_in(node, node.init)
				return
			}

			// 1. Init
			if node.init !is ast.EmptyStmt {
				b.stmt(node.init)
			}

			// 2. Control Flow Blocks
			head_blk := b.mod.add_block(b.cur_func, 'for.head')
			body_blk := b.mod.add_block(b.cur_func, 'for.body')
			exit_blk := b.mod.add_block(b.cur_func, 'for.exit')

			b.loop_stack << LoopInfo{
				head: head_blk
				exit: exit_blk
			}

			// Jump to Head
			head_val := b.mod.blocks[head_blk].val_id
			b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])

			// 3. Head (Condition)
			b.cur_block = head_blk
			body_val := b.mod.blocks[body_blk].val_id
			exit_val := b.mod.blocks[exit_blk].val_id

			if node.cond !is ast.EmptyExpr {
				cond_val := b.expr(node.cond)
				b.mod.add_instr(.br, b.cur_block, 0, [cond_val, body_val, exit_val])
			} else {
				// Infinite loop
				b.mod.add_instr(.jmp, b.cur_block, 0, [body_val])
			}

			// 4. Body
			b.cur_block = body_blk
			b.stmts(node.stmts)

			// 5. Post
			if node.post !is ast.EmptyStmt {
				b.stmt(node.post)
			}

			// Loop back
			if !b.is_block_terminated(b.cur_block) {
				b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])
			}

			// 6. Exit
			b.cur_block = exit_blk
			b.loop_stack.pop()
		}
		ast.FlowControlStmt {
			if b.loop_stack.len == 0 {
				return
			}
			info := b.loop_stack.last()
			target := if node.op == .key_break { info.exit } else { info.head }

			target_val := b.mod.blocks[target].val_id
			b.mod.add_instr(.jmp, b.cur_block, 0, [target_val])
		}
		ast.StructDecl {
			// Register Struct Type
			mut field_types := []TypeID{}
			mut field_names := []string{}
			for field in node.fields {
				// Convert field type to SSA type (handles primitives, pointers, structs, etc.)
				field_type := b.ast_type_to_ssa(field.typ)
				field_types << field_type
				field_names << field.name

				// Check if field type is a function pointer type alias or FnType
				if field.typ is ast.Ident {
					if field.typ.name in b.fn_type_aliases {
						b.fn_ptr_fields['${node.name}.${field.name}'] = true
						// eprintln('DEBUG: Registered fn_ptr_field: ${node.name}.${field.name} (type: ${field.typ.name})')
					}
				} else if field.typ is ast.Type {
					// Check if it's a direct function type
					match field.typ {
						ast.FnType {
							b.fn_ptr_fields['${node.name}.${field.name}'] = true
						}
						else {}
					}
				}
			}

			// Register the struct type
			t := Type{
				kind:        .struct_t
				fields:      field_types
				field_names: field_names
				width:       0
			}
			type_id := b.mod.type_store.register(t)
			// Register struct name for type lookup
			b.struct_types[node.name] = type_id
		}
		ast.GlobalDecl {
			for field in node.fields {
				// Check if field type is a known struct type
				mut field_type := b.mod.type_store.get_int(64) // default to int64
				if field.typ is ast.Ident {
					type_name := field.typ.name
					if st := b.struct_types[type_name] {
						// Field is a struct type
						field_type = st
					}
				}
				// Register global
				b.mod.add_global(field.name, field_type, false)
			}
		}
		ast.ConstDecl {
			for field in node.fields {
				// Constants infer type from their value - default to int64
				field_type := b.mod.type_store.get_int(64)
				// Evaluate initial value
				initial_value := b.eval_const_expr(field.value)
				// Register constant with initial value
				b.mod.add_global_with_value(field.name, field_type, true, initial_value)
			}
		}
		ast.EnumDecl {
			// Enums are lowered to integer constants
			// Each field gets an incrementing value starting from 0 (or explicit value)
			// Track enum type name for cast detection
			b.enum_names[node.name] = true
			field_type := b.mod.type_store.get_int(64)

			// Check if this is a @[flag] enum
			// Note: attribute name is stored in value as Ident for simple attributes
			is_flag_enum := node.attributes.any(fn (attr ast.Attribute) bool {
				if attr.name == 'flag' {
					return true
				}
				if attr.value is ast.Ident {
					return attr.value.name == 'flag'
				}
				return false
			})

			// For flag enums, values are powers of 2
			mut next_value := 0
			for i, field in node.fields {
				// If field has explicit value, use it
				if field.value is ast.BasicLiteral {
					if field.value.kind == .number {
						next_value = field.value.value.int()
					}
				} else if is_flag_enum {
					// For flag enums, values are powers of 2 (1, 2, 4, 8, ...)
					next_value = 1 << i
				}
				// Register enum value with its integer value
				const_name := '${node.name}__${field.name}'
				b.enum_values[const_name] = next_value
				// Also register as global for native backends
				b.mod.add_global_with_value(const_name, field_type, true, i64(next_value))
				if !is_flag_enum {
					next_value++
				}
			}

			// Track flag enum names (has/all calls are fully desugared by transformer)
			if is_flag_enum {
				b.flag_enum_names[node.name] = true
			}
		}
		ast.LabelStmt {
			// Labels are used for labeled loops (break/continue targets)
			// For now, we just process the associated statement if any
			// TODO: Track labels for labeled break/continue support
			if node.stmt !is ast.EmptyStmt {
				b.stmt(node.stmt)
			}
		}
		ast.InterfaceDecl {
			// Track interface name and methods for vtable support
			b.interface_names[node.name] = true
			mut method_names := []string{}
			for field in node.fields {
				method_names << field.name
			}
			b.interface_meths[node.name] = method_names

			// For SSA, interface struct has: _object (ptr), _type_id (int), func ptrs
			i64_t := b.mod.type_store.get_int(64)
			ptr_t := b.mod.type_store.get_ptr(i64_t)
			mut field_types := []TypeID{}
			mut field_names := []string{}
			field_types << ptr_t // _object pointer
			field_names << '_object'
			field_types << i64_t // _type_id
			field_names << '_type_id'
			for field in node.fields {
				// Each method is a function pointer
				field_types << ptr_t // function pointer
				field_names << field.name
			}
			// Register interface as a struct type
			t := Type{
				kind:        .struct_t
				fields:      field_types
				field_names: field_names
				width:       0
			}
			type_id := b.mod.type_store.register(t)
			b.struct_types[node.name] = type_id
		}
		ast.TypeDecl {
			// Type declarations: type aliases or sum types
			if node.variants.len > 0 {
				// Sum type: register as a tagged union
				// For now, treat as an integer (the tag)
				field_type := b.mod.type_store.get_int(64)
				t := Type{
					kind:        .struct_t
					fields:      [field_type, field_type] // tag + data
					field_names: ['_tag', '_data']
					width:       0
				}
				type_id := b.mod.type_store.register(t)
				b.struct_types[node.name] = type_id
			} else if node.base_type !is ast.EmptyExpr {
				// Type alias: look up the base type
				if node.base_type is ast.Ident {
					base_name := node.base_type.name
					if st := b.struct_types[base_name] {
						// Alias to existing struct type
						b.struct_types[node.name] = st
					} else {
						// Alias to primitive type - register as int64
						t := Type{
							kind:  .int_t
							width: 64
						}
						type_id := b.mod.type_store.register(t)
						b.struct_types[node.name] = type_id
					}
				} else if node.base_type is ast.Type {
					// Check if it's a function type alias or array type alias
					match node.base_type {
						ast.FnType {
							// Function type alias (e.g., type MapEqFn = fn(voidptr, voidptr) bool)
							b.fn_type_aliases[node.name] = true
							// Register as pointer type for SSA
							i64_t := b.mod.type_store.get_int(64)
							ptr_t := b.mod.type_store.get_ptr(i64_t)
							b.struct_types[node.name] = ptr_t
						}
						ast.ArrayType {
							// Array type alias (e.g., type Builder = []u8)
							// Map to 'array' for method resolution
							b.type_alias_bases[node.name] = 'array'
							// Register the type itself
							if array_t := b.struct_types['array'] {
								b.struct_types[node.name] = array_t
							}
						}
						else {}
					}
				}
			}
		}
		ast.AssertStmt {
			// Assert: if condition is false, abort/exit
			// Generate: if (!cond) { unreachable/abort }
			i64_t := b.mod.type_store.get_int(64)

			// Evaluate the assertion condition
			cond_val := b.expr(node.expr)

			// Create blocks for pass and fail paths
			pass_blk := b.mod.add_block(b.cur_func, 'assert.pass')
			fail_blk := b.mod.add_block(b.cur_func, 'assert.fail')

			// Branch: if cond is true, go to pass; else go to fail
			pass_val := b.mod.blocks[pass_blk].val_id
			fail_val := b.mod.blocks[fail_blk].val_id
			b.mod.add_instr(.br, b.cur_block, 0, [cond_val, pass_val, fail_val])

			// Fail block: call abort() or exit(1) then unreachable
			b.cur_block = fail_blk
			// Call C.exit(1) to terminate
			exit_fn := b.mod.add_value_node(.unknown, 0, 'exit', 0)
			one := b.mod.add_value_node(.constant, i64_t, '1', 0)
			b.mod.add_instr(.call, b.cur_block, i64_t, [exit_fn, one])
			b.mod.add_instr(.unreachable, b.cur_block, 0, [])

			// Continue in pass block
			b.cur_block = pass_blk
		}
		ast.ComptimeStmt {
			// Comptime statement wrapper - just process the inner statement
			// The comptime evaluation happens when parsing the $if/$else structure
			b.stmt(node.stmt)
		}
		else {
			// println('Builder: Unhandled stmt ${node.type_name()}')
		}
	}
}

fn (mut b Builder) expr(node ast.Expr) ValueID {
	match node {
		ast.BasicLiteral {
			return b.expr_basic_literal(node)
		}
		ast.Ident {
			return b.expr_ident(node)
		}
		ast.InitExpr {
			return b.expr_init(node)
		}
		ast.SelectorExpr {
			// Check if this is an enum value access (EnumName.value)
			if node.lhs is ast.Ident {
				enum_name := '${node.lhs.name}__${node.rhs.name}'
				if enum_val := b.enum_values[enum_name] {
					i64_t := b.mod.type_store.get_int(64)
					return b.mod.add_value_node(.constant, i64_t, '${enum_val}', 0)
				}
			}
			// Check for enum shorthand (.value) - LHS is EmptyExpr
			if node.lhs is ast.EmptyExpr {
				if b.cur_match_type != '' {
					enum_name := '${b.cur_match_type}__${node.rhs.name}'
					if enum_val := b.enum_values[enum_name] {
						i64_t := b.mod.type_store.get_int(64)
						return b.mod.add_value_node(.constant, i64_t, '${enum_val}', 0)
					}
				}
			}
			return b.expr_selector(node)
		}
		ast.IndexExpr {
			return b.expr_index(node)
		}
		ast.CastExpr {
			// Get the value being cast
			val := b.expr(node.expr)
			val_typ := b.mod.values[val].typ
			val_info := b.mod.type_store.types[val_typ]

			// Determine the target type
			target_typ := b.ast_type_to_ssa(node.typ)
			target_info := b.mod.type_store.types[target_typ]

			// Check if we need a float-to-int conversion
			if val_info.kind == .float_t && target_info.kind == .int_t {
				// Float to signed int conversion
				return b.mod.add_instr(.fptosi, b.cur_block, target_typ, [val])
			}

			// Check if we need an int-to-float conversion
			if val_info.kind == .int_t && target_info.kind == .float_t {
				// Signed int to float conversion
				return b.mod.add_instr(.sitofp, b.cur_block, target_typ, [val])
			}

			// No conversion needed or just truncation/extension
			return val
		}
		ast.ParenExpr {
			return b.expr(node.expr)
		}
		ast.InfixExpr {
			return b.expr_infix(node)
		}
		ast.IfExpr {
			return b.expr_if(node)
		}
		ast.MatchExpr {
			return b.expr_match(node)
		}
		ast.CallExpr {
			return b.expr_call(node)
		}
		ast.StringLiteral {
			return b.expr_string_literal(node)
		}
		ast.StringInterLiteral {
			return b.expr_string_inter_literal(node)
		}
		ast.CallOrCastExpr {
			return b.expr_call_or_cast(node)
		}
		ast.PrefixExpr {
			return b.expr_prefix(node)
		}
		ast.PostfixExpr {
			return b.expr_postfix(node)
		}
		ast.ModifierExpr {
			// Handle 'mut x' - just unwrap and process the inner expression
			return b.expr(node.expr)
		}
		ast.ArrayInitExpr {
			return b.expr_array_init(node)
		}
		ast.MapInitExpr {
			return b.expr_map_init(node)
		}
		ast.IfGuardExpr {
			return b.expr_if_guard(node)
		}
		ast.RangeExpr {
			return b.expr_range(node)
		}
		ast.Type {
			// Handle type expressions (none, nil, etc.)
			// For none/nil, return 0 (null/false value)
			i64_t := b.mod.type_store.get_int(64)
			return b.mod.add_value_node(.constant, i64_t, '0', 0)
		}
		ast.Keyword {
			// Handle keywords like none, true, false
			i64_t := b.mod.type_store.get_int(64)
			match node.tok {
				.key_none, .key_nil {
					return b.mod.add_value_node(.constant, i64_t, '0', 0)
				}
				.key_true {
					return b.mod.add_value_node(.constant, i64_t, '1', 0)
				}
				.key_false {
					return b.mod.add_value_node(.constant, i64_t, '0', 0)
				}
				else {
					return b.mod.add_value_node(.constant, i64_t, '0', 0)
				}
			}
		}
		ast.ComptimeExpr {
			// Handle comptime expressions like $if macos { ... } $else { ... }
			return b.expr_comptime(node)
		}
		ast.UnsafeExpr {
			// Process statements in unsafe block and return value of last expr
			return b.stmts_with_value(node.stmts)
		}
		ast.EmptyExpr {
			// Empty expression - return 0 as sentinel value
			i64_t := b.mod.type_store.get_int(64)
			return b.mod.add_value_node(.constant, i64_t, '0', 0)
		}
		ast.OrExpr {
			// Or expression: expr or { fallback }
			// For now, evaluate the main expression
			// TODO: Add proper optional/error handling with branches
			main_val := b.expr(node.expr)
			// For now, just return main value - full implementation would check for error
			// and branch to fallback block if needed
			return main_val
		}
		ast.FieldInit {
			// Named argument in function call: name: value
			// Just evaluate the value expression
			return b.expr(node.value)
		}
		ast.KeywordOperator {
			// Handle keyword operators: sizeof, typeof, isreftype, go, spawn, __offsetof
			i64_t := b.mod.type_store.get_int(64)
			match node.op {
				.key_sizeof {
					// sizeof(type) - return 8 for 64-bit systems as default
					// TODO: compute actual size based on type
					return b.mod.add_value_node(.constant, i64_t, '8', 0)
				}
				.key_typeof {
					// typeof(expr) - return 0 for now
					// TODO: return actual type info
					return b.mod.add_value_node(.constant, i64_t, '0', 0)
				}
				.key_isreftype {
					// isreftype(type) - return 0 for now
					// TODO: return actual isreftype value
					return b.mod.add_value_node(.constant, i64_t, '0', 0)
				}
				.key_go, .key_spawn {
					// go/spawn - evaluate the expression (function call)
					if node.exprs.len > 0 {
						return b.expr(node.exprs[0])
					}
					return b.mod.add_value_node(.constant, i64_t, '0', 0)
				}
				else {
					// __offsetof or other - return 0 as default
					return b.mod.add_value_node(.constant, i64_t, '0', 0)
				}
			}
		}
		ast.LockExpr {
			// lock/rlock expression - process the body statements
			// TODO: add proper mutex lock/unlock calls
			return b.stmts_with_value(node.stmts)
		}
		ast.GenericArgs {
			// Generic instantiation like Foo!int - evaluate the base expression
			return b.expr(node.lhs)
		}
		ast.AsCastExpr {
			// 'as' cast expression (e.g., x as SomeType)
			// Similar to CastExpr but used for sum type narrowing
			val := b.expr(node.expr)
			// For now, just return the value - proper implementation would
			// add runtime type checking for sum types
			return val
		}
		ast.AssocExpr {
			// Copy-with-modification expression: { expr | field: value }
			// Evaluate the base expression - the field modifications would be applied in codegen
			return b.expr(node.expr)
		}
		ast.FnLiteral {
			// Lambda/anonymous function
			// TODO: generate anonymous function and return function pointer
			i64_t := b.mod.type_store.get_int(64)
			return b.mod.add_value_node(.constant, i64_t, '0', 0)
		}
		ast.Tuple {
			// Tuple expression (e.g., (a, b) for multi-return)
			if node.exprs.len == 0 {
				i64_t := b.mod.type_store.get_int(64)
				return b.mod.add_value_node(.constant, i64_t, '0', 0)
			}
			if node.exprs.len == 1 {
				return b.expr(node.exprs[0])
			}
			// Multi-value tuple - build a tuple type and insertvalue instructions
			mut elem_vals := []ValueID{}
			mut elem_types := []TypeID{}
			for e in node.exprs {
				val := b.expr(e)
				elem_vals << val
				elem_types << b.mod.values[val].typ
			}
			tuple_t := b.mod.type_store.get_tuple(elem_types)
			// Build tuple using insertvalue instructions
			mut tuple_val := b.mod.add_value_node(.constant, tuple_t, 'undef', 0)
			for i, val in elem_vals {
				idx := b.mod.add_value_node(.constant, b.mod.type_store.get_int(32), '${i}',
					0)
				tuple_val = b.mod.add_instr(.insertvalue, b.cur_block, tuple_t, [
					tuple_val,
					val,
					idx,
				])
			}
			return tuple_val
		}
		else {
			println('Builder: Unhandled expr ${node.type_name()}')
			// Return constant 0 (i32) to prevent cascading void errors
			i32_t := b.mod.type_store.get_int(64)
			return b.mod.add_value_node(.constant, i32_t, '0', 0)
		}
	}
}

fn (mut b Builder) expr_basic_literal(node ast.BasicLiteral) ValueID {
	if node.kind == .number {
		// Determine if this is a float or integer literal
		typ := b.infer_literal_type(node.value)
		val := b.mod.add_value_node(.constant, typ, node.value, 0)
		return val
	} else if node.kind == .char {
		// Character literal - u8/i8
		i8_t := b.mod.type_store.get_int(8)
		// Extract char value (strip quotes if present)
		char_val := node.value.trim("'`")
		mut val_int := 0
		if char_val.len > 0 {
			// Handle escape sequences
			if char_val.len >= 2 && char_val[0] == `\\` {
				match char_val[1] {
					`n` { val_int = 10 } // newline
					`t` { val_int = 9 } // tab
					`r` { val_int = 13 } // carriage return
					`\\` { val_int = 92 } // backslash
					`'` { val_int = 39 } // single quote
					`"` { val_int = 34 } // double quote
					`0` { val_int = 0 } // null
					else { val_int = int(char_val[1]) }
				}
			} else {
				val_int = int(char_val[0])
			}
		}
		return b.mod.add_value_node(.constant, i8_t, '${val_int}', 0)
	} else if node.kind in [.key_true, .key_false] {
		// Boolean - use i8 for bool type
		bool_t := b.mod.type_store.get_int(8)
		val_str := if node.kind == .key_true { '1' } else { '0' }
		val := b.mod.add_value_node(.constant, bool_t, val_str, 0)
		return val
	}
	return 0
}

fn (mut b Builder) expr_ident(node ast.Ident) ValueID {
	ptr := b.addr(node)
	// Get type pointed to
	ptr_typ := b.mod.values[ptr].typ
	val_typ := b.mod.type_store.types[ptr_typ].elem_type

	return b.mod.add_instr(.load, b.cur_block, val_typ, [ptr])
}

fn (mut b Builder) expr_init(node ast.InitExpr) ValueID {
	// Check if this is a map init: map[K]V{}
	if node.typ is ast.Type {
		if node.typ is ast.MapType {
			return b.expr_map_init_from_type(node.typ)
		}
	}

	// Struct Init: MyStruct{ a: 1, b: 2 }
	// 1. Allocate Struct
	// Need to find the TypeID for the struct.
	mut struct_t := 0

	// Try to get struct type from the type name in the init expression
	if node.typ is ast.Ident {
		if st := b.struct_types[node.typ.name] {
			struct_t = st
		}
	}

	// Fallback: search for first struct type (for backwards compatibility)
	if struct_t == 0 {
		for i, t in b.mod.type_store.types {
			if t.kind == .struct_t {
				struct_t = i
				break
			}
		}
	}

	ptr_t := b.mod.type_store.get_ptr(struct_t)
	struct_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])

	// 2. Initialize Fields
	// Build a map of explicitly initialized fields by name
	mut init_fields := map[string]ast.Expr{}
	for field in node.fields {
		init_fields[field.name] = field.value
	}

	// Get struct type info to iterate all fields
	struct_type := b.mod.type_store.types[struct_t]

	// Initialize all fields (explicit value or zero)
	for i, field_name in struct_type.field_names {
		idx_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(64), i.str(),
			0)

		// GEP to field - use the actual field type
		field_type := struct_type.fields[i]
		field_ptr_type := b.mod.type_store.get_ptr(field_type)
		field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_type, [
			struct_ptr,
			idx_val,
		])

		if expr := init_fields[field_name] {
			// Explicitly initialized
			val := b.expr(expr)
			b.mod.add_instr(.store, b.cur_block, 0, [val, field_ptr])
		} else {
			// Zero initialize
			zero_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(64),
				'0', 0)
			b.mod.add_instr(.store, b.cur_block, 0, [zero_val, field_ptr])
		}
	}

	// 3. Return Pointer (Structs are value types in V, but usually passed by ref in SSA construction phase or loaded)
	return struct_ptr
}

fn (mut b Builder) expr_selector(node ast.SelectorExpr) ValueID {
	// Check for map.len access
	if node.lhs is ast.Ident && node.rhs.name == 'len' {
		var_name := node.lhs.name
		if var_name in b.var_map_types {
			// Map len access - load the len field from our simple map struct
			// Simple map structure: [len, keys[0..31], values[0..31]]
			// len is at index 0
			i64_t := b.mod.type_store.get_int(64)
			ptr_t := b.mod.type_store.get_ptr(i64_t)
			map_ptr := b.vars[var_name]

			// len is at index 0
			len_offset := b.mod.add_value_node(.constant, i64_t, '0', 0)
			len_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [
				map_ptr,
				len_offset,
			])
			return b.mod.add_instr(.load, b.cur_block, i64_t, [len_ptr])
		}
	}

	// Load value from field
	ptr := b.addr(node)
	// Get the actual field type from the pointer type
	ptr_val := b.mod.values[ptr]
	ptr_typ := b.mod.type_store.types[ptr_val.typ]
	field_typ := ptr_typ.elem_type // Dereference the pointer type to get field type
	return b.mod.add_instr(.load, b.cur_block, field_typ, [ptr])
}

fn (mut b Builder) expr_index(node ast.IndexExpr) ValueID {
	// Check if this is a range-based slice (arr[start..end])
	if node.expr is ast.RangeExpr {
		return b.expr_slice(node.lhs, node.expr)
	}

	// Check if this is a map access (m[key])
	if node.lhs is ast.Ident {
		var_name := node.lhs.name
		if var_name in b.var_map_types {
			// Map access: use simple inline map get
			map_ptr := b.vars[var_name]
			key_val := b.expr(node.expr)
			return b.emit_simple_map_get(map_ptr, key_val)
		}
	}

	// Load value from index
	ptr := b.addr(node)
	i32_t := b.mod.type_store.get_int(64) // Assume i32
	return b.mod.add_instr(.load, b.cur_block, i32_t, [ptr])
}

fn (mut b Builder) expr_slice(base ast.Expr, range_expr ast.RangeExpr) ValueID {
	// Array slicing: arr[start..end]
	// Returns a new heap-allocated array containing elements from start to end-1
	i64_t := b.mod.type_store.get_int(64)
	elem_ptr_t := b.mod.type_store.get_ptr(i64_t)

	// Get base array pointer
	base_ptr := b.addr(base)

	// Auto-dereference if it's a pointer-to-pointer
	base_val := b.mod.values[base_ptr]
	ptr_typ := b.mod.type_store.types[base_val.typ]
	elem_typ_id := ptr_typ.elem_type
	elem_typ := b.mod.type_store.types[elem_typ_id]

	mut actual_base := base_ptr
	if elem_typ.kind == .ptr_t {
		actual_base = b.mod.add_instr(.load, b.cur_block, elem_typ_id, [base_ptr])
	}

	// Evaluate start and end indices
	start_val := b.expr(range_expr.start)
	end_val := b.expr(range_expr.end)

	// Calculate slice length: end - start
	slice_len := b.mod.add_instr(.sub, b.cur_block, i64_t, [end_val, start_val])

	// Calculate allocation size: len * 8 (sizeof int64)
	elem_size := b.mod.add_value_node(.constant, i64_t, '8', 0)
	alloc_size := b.mod.add_instr(.mul, b.cur_block, i64_t, [slice_len, elem_size])

	// Call malloc to allocate heap memory
	malloc_fn := b.mod.add_value_node(.unknown, 0, 'malloc', 0)
	slice_ptr := b.mod.add_instr(.call, b.cur_block, elem_ptr_t, [malloc_fn, alloc_size])

	// Copy elements in a loop: for i = 0; i < len; i++ { slice[i] = base[start + i] }
	head_blk := b.mod.add_block(b.cur_func, 'slice.head')
	body_blk := b.mod.add_block(b.cur_func, 'slice.body')
	exit_blk := b.mod.add_block(b.cur_func, 'slice.exit')

	// Allocate loop counter on stack
	counter_ptr := b.mod.add_instr(.alloca, b.cur_block, b.mod.type_store.get_ptr(i64_t),
		[])
	zero := b.mod.add_value_node(.constant, i64_t, '0', 0)
	b.mod.add_instr(.store, b.cur_block, 0, [zero, counter_ptr])

	// Jump to loop head
	head_val := b.mod.blocks[head_blk].val_id
	b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])

	// Loop head: check counter < slice_len
	b.cur_block = head_blk
	counter := b.mod.add_instr(.load, b.cur_block, i64_t, [counter_ptr])
	cond := b.mod.add_instr(.lt, b.cur_block, i64_t, [counter, slice_len])
	body_val := b.mod.blocks[body_blk].val_id
	exit_val := b.mod.blocks[exit_blk].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [cond, body_val, exit_val])

	// Loop body: copy one element
	b.cur_block = body_blk
	counter2 := b.mod.add_instr(.load, b.cur_block, i64_t, [counter_ptr])

	// Source: base[start + counter]
	src_idx := b.mod.add_instr(.add, b.cur_block, i64_t, [start_val, counter2])
	src_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_t, [
		actual_base,
		src_idx,
	])
	elem_val := b.mod.add_instr(.load, b.cur_block, i64_t, [src_ptr])

	// Dest: slice[counter]
	dst_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_t, [slice_ptr, counter2])
	b.mod.add_instr(.store, b.cur_block, 0, [elem_val, dst_ptr])

	// Increment counter
	one := b.mod.add_value_node(.constant, i64_t, '1', 0)
	counter3 := b.mod.add_instr(.load, b.cur_block, i64_t, [counter_ptr])
	new_counter := b.mod.add_instr(.add, b.cur_block, i64_t, [counter3, one])
	b.mod.add_instr(.store, b.cur_block, 0, [new_counter, counter_ptr])

	// Jump back to loop head
	b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])

	// Continue at exit block
	b.cur_block = exit_blk

	return slice_ptr
}

fn (mut b Builder) expr_infix(node ast.InfixExpr) ValueID {
	bool_t := b.mod.type_store.get_int(8) // Boolean results use i8

	// Handle logical operators: && and ||
	// For simplicity, we use bitwise AND/OR on boolean (0/1) values.
	// This gives correct results when operands are already 0 or 1.
	// True short-circuit evaluation would require control flow.
	if node.op == .and {
		left := b.expr(node.lhs)
		right := b.expr(node.rhs)
		return b.mod.add_instr(.and_, b.cur_block, bool_t, [left, right])
	}

	if node.op == .logical_or {
		left := b.expr(node.lhs)
		right := b.expr(node.rhs)
		return b.mod.add_instr(.or_, b.cur_block, bool_t, [left, right])
	}

	// Note: String concatenation (str1 + str2 -> string_plus, s1 + s2 + s3 -> string_plus_two)
	// is fully desugared by the transformer

	mut left := b.expr(node.lhs)
	mut right := b.expr(node.rhs)

	// Get operand types for type propagation
	left_typ := b.mod.values[left].typ
	right_typ := b.mod.values[right].typ

	// Check if operands are floats
	left_type_info := b.mod.type_store.types[left_typ]
	right_type_info := b.mod.type_store.types[right_typ]
	left_is_float := left_type_info.kind == .float_t
	right_is_float := right_type_info.kind == .float_t

	// For mixed float/int operations, convert int to float
	is_float := left_is_float || right_is_float
	if is_float && node.op in [.plus, .minus, .mul, .div, .mod] {
		if left_is_float && !right_is_float {
			// Convert right operand from int to float
			float_t := left_typ
			right = b.mod.add_instr(.sitofp, b.cur_block, float_t, [right])
		} else if right_is_float && !left_is_float {
			// Convert left operand from int to float
			float_t := right_typ
			left = b.mod.add_instr(.sitofp, b.cur_block, float_t, [left])
		}
	}

	// Determine result type based on operation and operands
	result_typ := b.infer_binop_result_type(node.op, left_typ, right_typ)

	// Map Token Op to SSA OpCode
	op := match node.op {
		.plus {
			if is_float { OpCode.fadd } else { OpCode.add }
		}
		.minus {
			if is_float { OpCode.fsub } else { OpCode.sub }
		}
		.mul {
			if is_float { OpCode.fmul } else { OpCode.mul }
		}
		.div {
			if is_float { OpCode.fdiv } else { OpCode.sdiv }
		}
		.mod {
			if is_float { OpCode.frem } else { OpCode.srem }
		}
		.amp {
			OpCode.and_
		}
		.pipe {
			OpCode.or_
		}
		.xor {
			OpCode.xor
		}
		.left_shift {
			OpCode.shl
		}
		.right_shift {
			OpCode.ashr
		}
		.gt {
			OpCode.gt
		}
		.lt {
			OpCode.lt
		}
		.eq {
			OpCode.eq
		}
		.ne {
			OpCode.ne
		}
		.ge {
			OpCode.ge
		}
		.le {
			OpCode.le
		}
		else {
			OpCode.add
		}
	}

	return b.mod.add_instr(op, b.cur_block, result_typ, [left, right])
}

// infer_binop_result_type determines the result type of a binary operation.
// Comparison operators return bool (i8), arithmetic operators propagate operand type.
fn (mut b Builder) infer_binop_result_type(op token.Token, left_typ TypeID, right_typ TypeID) TypeID {
	// Comparison operators always return bool (i8)
	if op in [.gt, .lt, .eq, .ne, .ge, .le] {
		return b.mod.type_store.get_int(8)
	}

	// Logical operators return bool
	if op in [.and, .logical_or] {
		return b.mod.type_store.get_int(8)
	}

	// Arithmetic operations: use the "wider" type, prefer floats
	left_info := b.mod.type_store.types[left_typ]
	right_info := b.mod.type_store.types[right_typ]

	// If either operand is float, result is float
	if left_info.kind == .float_t || right_info.kind == .float_t {
		// Use the wider float type
		if left_info.kind == .float_t && right_info.kind == .float_t {
			if left_info.width >= right_info.width {
				return left_typ
			}
			return right_typ
		}
		// Return the float type
		return if left_info.kind == .float_t { left_typ } else { right_typ }
	}

	// Both integers: use the wider type
	if left_info.kind == .int_t && right_info.kind == .int_t {
		if left_info.width >= right_info.width {
			return left_typ
		}
		return right_typ
	}

	// Default: use left operand type
	return left_typ
}

// expr_comptime handles compile-time conditionals like $if macos { ... } $else { ... }
fn (mut b Builder) expr_comptime(node ast.ComptimeExpr) ValueID {
	// The inner expression should be an IfExpr
	if node.expr is ast.IfExpr {
		return b.expr_comptime_if(node.expr)
	}
	// For other comptime expressions, just evaluate them (e.g., $embed_file)
	return b.expr(node.expr)
}

// expr_comptime_if handles $if/$else compile-time conditionals
fn (mut b Builder) expr_comptime_if(node ast.IfExpr) ValueID {
	// Evaluate the comptime condition
	cond_result := b.eval_comptime_cond(node.cond)

	if cond_result {
		// Condition is true - emit then branch
		return b.stmts_with_value(node.stmts)
	} else {
		// Condition is false - emit else branch if present
		if node.else_expr !is ast.EmptyExpr {
			if node.else_expr is ast.IfExpr {
				// Could be $else if or plain $else
				if node.else_expr.cond is ast.EmptyExpr {
					// Plain $else block
					return b.stmts_with_value(node.else_expr.stmts)
				} else {
					// $else $if - recursive comptime evaluation
					return b.expr_comptime_if(node.else_expr)
				}
			}
		}
	}
	// No branch taken - return 0
	i64_t := b.mod.type_store.get_int(64)
	return b.mod.add_value_node(.constant, i64_t, '0', 0)
}

// eval_comptime_cond evaluates a compile-time condition expression
fn (b Builder) eval_comptime_cond(cond ast.Expr) bool {
	match cond {
		ast.Ident {
			// Platform and feature flags
			return b.eval_comptime_flag(cond.name)
		}
		ast.PrefixExpr {
			// Handle negation: !macos
			if cond.op == .not {
				return !b.eval_comptime_cond(cond.expr)
			}
		}
		ast.InfixExpr {
			// Handle && and ||
			if cond.op == .and {
				return b.eval_comptime_cond(cond.lhs) && b.eval_comptime_cond(cond.rhs)
			}
			if cond.op == .logical_or {
				return b.eval_comptime_cond(cond.lhs) || b.eval_comptime_cond(cond.rhs)
			}
		}
		ast.PostfixExpr {
			// Handle optional feature check: feature?
			if cond.op == .question {
				if cond.expr is ast.Ident {
					return b.eval_comptime_flag(cond.expr.name)
				}
			}
		}
		ast.ParenExpr {
			return b.eval_comptime_cond(cond.expr)
		}
		else {}
	}
	return false
}

// eval_comptime_flag evaluates a single comptime flag/identifier
fn (b Builder) eval_comptime_flag(name string) bool {
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
		'freestanding', 'ios', 'android', 'termux', 'debug', 'test', 'prealloc', 'gcboehm' {
			return false
		}
		// v2 generates native code
		'native' {
			return true
		}
		// Use buffered I/O (fwrite to C.stdout) for printing
		// This provides consistent output ordering with the reference compiler
		'builtin_write_buf_to_fd_should_use_c_write' {
			return false
		}
		else {
			// Unknown flag - default to false
			return false
		}
	}
}

fn (mut b Builder) expr_if(node ast.IfExpr) ValueID {
	// If cond is empty, it's a plain 'else' block from a parent IfExpr
	if node.cond is ast.EmptyExpr {
		return b.stmts_with_value(node.stmts)
	}

	// Check if this is an if-guard expression
	if node.cond is ast.IfGuardExpr {
		return b.expr_if_with_guard(node, node.cond)
	}

	// Check if this is an if-expression (has else and branches have values)
	has_else := node.else_expr !is ast.EmptyExpr
	is_expr := has_else && b.branch_has_value(node.stmts)

	// 1. Evaluate Condition
	cond_val := b.expr(node.cond)

	// 2. Create Blocks
	then_blk := b.mod.add_block(b.cur_func, 'if.then')
	merge_blk := b.mod.add_block(b.cur_func, 'if.end')
	mut else_blk := merge_blk

	if has_else {
		else_blk = b.mod.add_block(b.cur_func, 'if.else')
	}

	// 3. For if-expressions, allocate result storage
	i32_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i32_t)
	mut result_ptr := ValueID(0)
	if is_expr {
		result_ptr = b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
	}

	// 4. Emit Branch
	then_val := b.mod.blocks[then_blk].val_id
	else_val := b.mod.blocks[else_blk].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [cond_val, then_val, else_val])

	// 5. Build Then Block
	b.cur_block = then_blk
	then_result := b.stmts_with_value(node.stmts)
	if is_expr && then_result != 0 && !b.is_block_terminated(b.cur_block) {
		b.mod.add_instr(.store, b.cur_block, 0, [then_result, result_ptr])
	}
	if !b.is_block_terminated(b.cur_block) {
		merge_val := b.mod.blocks[merge_blk].val_id
		b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val])
	}

	// 6. Build Else Block (if any)
	if has_else {
		b.cur_block = else_blk
		else_result := b.expr_if_else(node.else_expr)
		if is_expr && else_result != 0 && !b.is_block_terminated(b.cur_block) {
			b.mod.add_instr(.store, b.cur_block, 0, [else_result, result_ptr])
		}
		if !b.is_block_terminated(b.cur_block) {
			merge_val := b.mod.blocks[merge_blk].val_id
			b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val])
		}
	}

	// 7. Continue generation at Merge Block
	b.cur_block = merge_blk

	// 8. Load result for if-expressions
	if is_expr && result_ptr != 0 {
		return b.mod.add_instr(.load, b.cur_block, i32_t, [result_ptr])
	}
	return 0
}

// expr_if_with_guard handles if statements with guard expressions
// `if x := opt() { ... } else { ... }`
fn (mut b Builder) expr_if_with_guard(node ast.IfExpr, guard ast.IfGuardExpr) ValueID {
	has_else := node.else_expr !is ast.EmptyExpr
	is_expr := has_else && b.branch_has_value(node.stmts)

	i32_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i32_t)

	// 1. Evaluate the RHS of the guard (the optional expression)
	stmt := guard.stmt
	mut rhs_val := ValueID(0)
	if stmt.rhs.len > 0 {
		rhs_val = b.expr(stmt.rhs[0])
	}

	// 2. For now, use the value directly as condition
	// In a full optional implementation, we'd check the error flag
	// For simplicity, we treat non-zero as success
	cond_val := rhs_val

	// 3. Create Blocks
	then_blk := b.mod.add_block(b.cur_func, 'if.then')
	merge_blk := b.mod.add_block(b.cur_func, 'if.end')
	mut else_blk := merge_blk

	if has_else {
		else_blk = b.mod.add_block(b.cur_func, 'if.else')
	}

	// 4. For if-expressions, allocate result storage
	mut result_ptr := ValueID(0)
	if is_expr {
		result_ptr = b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
	}

	// 5. Emit Branch
	then_val := b.mod.blocks[then_blk].val_id
	else_val := b.mod.blocks[else_blk].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [cond_val, then_val, else_val])

	// 6. Build Then Block - bind guard variables here
	b.cur_block = then_blk

	// Bind the guard variables (x := ... binds x)
	b.expr_if_guard_bind(guard, rhs_val)

	then_result := b.stmts_with_value(node.stmts)
	if is_expr && then_result != 0 && !b.is_block_terminated(b.cur_block) {
		b.mod.add_instr(.store, b.cur_block, 0, [then_result, result_ptr])
	}
	if !b.is_block_terminated(b.cur_block) {
		merge_val := b.mod.blocks[merge_blk].val_id
		b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val])
	}

	// 7. Build Else Block (if any)
	if has_else {
		b.cur_block = else_blk
		else_result := b.expr_if_else(node.else_expr)
		if is_expr && else_result != 0 && !b.is_block_terminated(b.cur_block) {
			b.mod.add_instr(.store, b.cur_block, 0, [else_result, result_ptr])
		}
		if !b.is_block_terminated(b.cur_block) {
			merge_val := b.mod.blocks[merge_blk].val_id
			b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val])
		}
	}

	// 8. Continue generation at Merge Block
	b.cur_block = merge_blk

	// 9. Load result for if-expressions
	if is_expr && result_ptr != 0 {
		return b.mod.add_instr(.load, b.cur_block, i32_t, [result_ptr])
	}
	return 0
}

// expr_if_guard handles if-guard expressions: `if x := opt() {`
// When used as a condition, it evaluates the RHS and checks for success
// Returns a boolean value (0 = failure/none, 1 = success)
fn (mut b Builder) expr_if_guard(node ast.IfGuardExpr) ValueID {
	i32_t := b.mod.type_store.get_int(64)

	// Get the assignment statement
	stmt := node.stmt

	// For if-guard, we evaluate the RHS expression
	// In V, the RHS returns an optional type that we need to unwrap
	// For now, we assume any non-zero/non-none value is success
	if stmt.rhs.len == 0 {
		// No RHS - always false
		return b.mod.add_value_node(.constant, i32_t, '0', 0)
	}

	rhs_val := b.expr(stmt.rhs[0])

	// For the condition check, we need to determine if the optional succeeded
	// In a full implementation, this would check the error flag of the optional
	// For now, we'll assume the value itself indicates success (non-zero = success)
	// TODO: Proper optional type handling with error flags

	// Return the condition value (will be used in branch)
	// For now, return 1 (true) - actual unwrapping happens in expr_if when
	// the condition is an IfGuardExpr
	return rhs_val
}

// expr_if_guard_bind binds the variables from an if-guard expression
// This is called after the condition check succeeds
fn (mut b Builder) expr_if_guard_bind(node ast.IfGuardExpr, rhs_val ValueID) {
	stmt := node.stmt

	// Bind each LHS variable to the unwrapped value
	for i, lhs_expr in stmt.lhs {
		mut ident := ast.Ident{}

		// Unwrap 'mut x' if present
		if lhs_expr is ast.ModifierExpr {
			mod := lhs_expr as ast.ModifierExpr
			ident = mod.expr as ast.Ident
		} else if lhs_expr is ast.Ident {
			ident = lhs_expr
		} else {
			continue
		}

		name := ident.name

		// Get type from RHS value
		rhs_type := b.mod.values[rhs_val].typ
		ptr_t := b.mod.type_store.get_ptr(rhs_type)

		// Allocate stack slot for the variable
		stack_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])

		// For single-value assignment, use the rhs_val directly
		// For multi-value (like tuple unpacking), we'd need to extract individual values
		if i == 0 {
			b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, stack_ptr])
		}

		// Register the variable
		b.vars[name] = stack_ptr
	}
}

// Helper to check if a branch has a value (last stmt is an expression)
fn (b Builder) branch_has_value(stmts []ast.Stmt) bool {
	if stmts.len == 0 {
		return false
	}
	last := stmts[stmts.len - 1]
	return last is ast.ExprStmt
}

// Process else expression which can be another IfExpr or statements
fn (mut b Builder) expr_if_else(else_expr ast.Expr) ValueID {
	if else_expr is ast.IfExpr {
		return b.expr_if(else_expr)
	}
	return 0
}

// Process statements and return the value of the last expression if any
fn (mut b Builder) stmts_with_value(stmts []ast.Stmt) ValueID {
	if stmts.len == 0 {
		return 0
	}
	// Process all but the last statement
	for i := 0; i < stmts.len - 1; i++ {
		b.stmt(stmts[i])
	}
	// Process the last statement and return its value if it's an expression
	last := stmts[stmts.len - 1]
	if last is ast.ExprStmt {
		return b.expr(last.expr)
	}
	b.stmt(last)
	return 0
}

fn (mut b Builder) expr_match(node ast.MatchExpr) ValueID {
	// 1. Eval Cond
	cond_val := b.expr(node.expr)

	// Try to infer the match expression type for enum shorthand
	old_match_type := b.cur_match_type
	if node.expr is ast.Ident {
		// Check if this variable has an enum type
		if var_type := b.get_var_struct_type(node.expr.name) {
			// If it's an enum type, set the context
			b.cur_match_type = var_type
		}
	} else if node.expr is ast.SelectorExpr {
		if node.expr.lhs is ast.Ident {
			// Check if LHS is an enum name
			for key, _ in b.enum_values {
				if key.starts_with('${node.expr.lhs.name}__') {
					b.cur_match_type = node.expr.lhs.name
					break
				}
			}
		}
	}

	// 2. Setup Blocks
	merge_blk := b.mod.add_block(b.cur_func, 'match.merge')
	mut default_blk := merge_blk

	// We need to collect all cases and branch blocks
	// Format: val -> block
	// Ops: [cond, default_blk, val1, blk1, val2, blk2...]

	mut cases := []ValueID{} // alternating val, blk_id

	// Pre-create blocks for branches to get their IDs
	mut branch_blks := []BlockID{}
	for i, branch in node.branches {
		name := if branch.cond.len == 0 { 'match.else' } else { 'match.case_${i}' }
		blk := b.mod.add_block(b.cur_func, name)
		branch_blks << blk

		if branch.cond.len == 0 {
			default_blk = blk
		} else {
			for expr in branch.cond {
				val := b.expr(expr)
				cases << val
				cases << b.mod.blocks[blk].val_id
			}
		}
	}

	// 3. Emit switch
	mut ops := []ValueID{}
	ops << cond_val
	ops << b.mod.blocks[default_blk].val_id
	ops << cases

	b.mod.add_instr(.switch_, b.cur_block, 0, ops)

	// 4. Build Branches
	for i, branch in node.branches {
		blk := branch_blks[i]
		b.cur_block = blk
		b.stmts(branch.stmts)

		if !b.is_block_terminated(b.cur_block) {
			merge_val := b.mod.blocks[merge_blk].val_id
			b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val])
		}
	}

	b.cur_block = merge_blk
	b.cur_match_type = old_match_type
	return 0
}

fn (mut b Builder) expr_call(node ast.CallExpr) ValueID {
	// Resolve Function Name first to detect flag enum methods
	// Arguments are resolved after we know if we need to set cur_match_type
	mut args := []ValueID{}
	mut flag_enum_receiver_type := ''
	mut name := ''
	mut is_method_call := false
	mut receiver_val := ValueID(0)
	i32_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i32_t)

	lhs := node.lhs
	if lhs is ast.Ident {
		name = lhs.name
	} else if lhs is ast.SelectorExpr {
		method_name := lhs.rhs.name
		// Check if this is a method call (receiver.method()) or C.func() or module.func()
		if lhs.lhs is ast.Ident {
			receiver_name := lhs.lhs.name
			// Check if receiver is 'C' (C interop) or a module name
			// Known module names that might be used in qualified calls
			known_modules := ['C', 'strconv', 'math', 'os', 'strings', 'hash', 'time', 'rand',
				'sync', 'runtime', 'builtin', 'bits', 'encoding', 'mem']
			if receiver_name in known_modules {
				// Module-qualified function call - just use the function name
				name = method_name
			} else if struct_type_name := b.get_var_struct_type(receiver_name) {
				// Check if this is a function pointer field call (e.g., m.key_eq_fn(args))
				fn_ptr_key := '${struct_type_name}.${method_name}'
				if fn_ptr_key in b.fn_ptr_fields {
					// Function pointer field call - load the function pointer and do indirect call
					// Get the struct pointer
					var_ptr := b.addr(lhs.lhs)
					ptr_typ := b.mod.values[var_ptr].typ
					elem_typ := b.mod.type_store.types[ptr_typ].elem_type
					struct_ptr := b.mod.add_instr(.load, b.cur_block, elem_typ, [
						var_ptr,
					])

					// Get field index
					struct_type := b.mod.type_store.types[elem_typ]
					mut field_idx := -1
					for i, fname in struct_type.field_names {
						if fname == method_name {
							field_idx = i
							break
						}
					}

					if field_idx >= 0 {
						// Get field pointer and load function pointer
						idx_val := b.mod.add_value_node(.constant, i32_t, '${field_idx}',
							field_idx)
						field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t,
							[
							struct_ptr,
							idx_val,
						])
						fn_ptr := b.mod.add_instr(.load, b.cur_block, ptr_t, [
							field_ptr,
						])

						// Prepend function pointer as first operand for indirect call
						args.prepend(fn_ptr)
						return b.mod.add_instr(.call_indirect, b.cur_block, i32_t, args)
					}
				} else if struct_type_name in b.interface_names {
					// Check if this is an interface type - use direct call with concrete type
					// Look up the concrete type for this interface variable
					if concrete_type := b.iface_concrete_types[receiver_name] {
						// Use direct call with the concrete type
						name = '${concrete_type}__${method_name}'
						is_method_call = true

						// The interface value is just a pointer to the boxed object
						// Load the object pointer from the variable
						var_ptr := b.addr(lhs.lhs)
						// Load the pointer value (interface is a ptr)
						receiver_val = b.mod.add_instr(.load, b.cur_block, ptr_t, [
							var_ptr,
						])
					}
				} else {
					// Regular method call - mangle the name
					// Resolve type alias to base type for method name
					mangled_type := if base_type := b.type_alias_bases[struct_type_name] {
						base_type
					} else {
						struct_type_name
					}
					// Mangle operator names to valid symbol names
					mangled_method := match method_name {
						'+' { 'plus' }
						'-' { 'minus' }
						'*' { 'mul' }
						'/' { 'div' }
						'%' { 'mod' }
						'==' { 'eq' }
						'!=' { 'ne' }
						'<' { 'lt' }
						'>' { 'gt' }
						'<=' { 'le' }
						'>=' { 'ge' }
						else { method_name }
					}
					name = '${mangled_type}__${mangled_method}'
					is_method_call = true
					// Check if this is a flag enum method
					if mangled_type in b.flag_enum_names && mangled_method in ['has', 'all'] {
						flag_enum_receiver_type = mangled_type
					}
					// Get the receiver - need to load the struct pointer from the variable
					// b.vars stores Ptr(Ptr(struct)), we need Ptr(struct)
					var_ptr := b.addr(lhs.lhs)
					ptr_typ := b.mod.values[var_ptr].typ
					elem_typ := b.mod.type_store.types[ptr_typ].elem_type
					receiver_val = b.mod.add_instr(.load, b.cur_block, elem_typ, [
						var_ptr,
					])
				}
			} else {
				// Unknown receiver type - try to infer from registered functions
				// Mangle operator names to valid symbol names first
				mangled_method_name := match method_name {
					'+' { 'plus' }
					'-' { 'minus' }
					'*' { 'mul' }
					'/' { 'div' }
					'%' { 'mod' }
					'==' { 'eq' }
					'!=' { 'ne' }
					'<' { 'lt' }
					'>' { 'gt' }
					'<=' { 'le' }
					'>=' { 'ge' }
					else { method_name }
				}
				inferred_type := b.infer_receiver_type(mangled_method_name)
				if inferred_type != '' {
					name = '${inferred_type}__${mangled_method_name}'
					// Check if this is a flag enum method
					if inferred_type in b.flag_enum_names && mangled_method_name in ['has', 'all'] {
						flag_enum_receiver_type = inferred_type
					}
				} else {
					eprintln('WARN: Method ${method_name} on ${receiver_name} - type not found')
					name = mangled_method_name
				}
			}
		} else {
			// Complex expression as receiver - try to infer type from method name
			mname := lhs.rhs.name
			// Mangle operator names to valid symbol names
			mangled_mname := match mname {
				'+' { 'plus' }
				'-' { 'minus' }
				'*' { 'mul' }
				'/' { 'div' }
				'%' { 'mod' }
				'==' { 'eq' }
				'!=' { 'ne' }
				'<' { 'lt' }
				'>' { 'gt' }
				'<=' { 'le' }
				'>=' { 'ge' }
				else { mname }
			}
			inferred_type := b.infer_receiver_type(mangled_mname)
			if inferred_type != '' {
				name = '${inferred_type}__${mangled_mname}'
				// Check if this is a flag enum method
				if inferred_type in b.flag_enum_names && mangled_mname in ['has', 'all'] {
					flag_enum_receiver_type = inferred_type
				}
			} else {
				eprintln('WARN: Method ${mname} on complex expression - inference failed')
				name = mangled_mname
			}
			// Generate receiver value for complex expression
			receiver_val = b.expr(lhs.lhs)
			is_method_call = true
		}
	}

	// Now resolve arguments with proper enum context for flag enum methods
	old_match_type := b.cur_match_type
	if flag_enum_receiver_type != '' {
		b.cur_match_type = flag_enum_receiver_type
	}
	for arg in node.args {
		args << b.expr(arg)
	}
	b.cur_match_type = old_match_type

	// For method calls, prepend receiver as first argument
	if is_method_call && receiver_val != 0 {
		args.prepend(receiver_val)
	}

	// Create a Value representing the function symbol (operand 0)
	fn_val := b.mod.add_value_node(.unknown, 0, name, 0)
	args.prepend(fn_val)

	// Look up the return type from tracked functions
	ret_type := if rt := b.func_ret_types[name] { rt } else { i32_t }

	return b.mod.add_instr(.call, b.cur_block, ret_type, args)
}

fn (mut b Builder) expr_string_literal(node ast.StringLiteral) ValueID {
	// Strip quotes from value (the parser includes them)
	val := node.value.trim("'").trim('"')

	// Check for C-string literal using the kind field
	if node.kind == .c {
		// C string: just return char* pointer
		i8_t := b.mod.type_store.get_int(8)
		ptr_t := b.mod.type_store.get_ptr(i8_t)
		return b.mod.add_value_node(.constant, ptr_t, '"${val}"', 0)
	}

	// V string: create a string struct literal
	// The ARM64 backend materializes string_literal as a pointer to a stack-resident struct
	// So the SSA type should be Ptr -> string_struct to match the actual semantics
	string_type_id := b.struct_types['string']
	string_ptr_type := b.mod.type_store.get_ptr(string_type_id)
	return b.mod.add_value_node(.string_literal, string_ptr_type, val, val.len)
}

fn (mut b Builder) expr_string_inter_literal(node ast.StringInterLiteral) ValueID {
	// String interpolation: 'prefix${a}middle${b}suffix'
	// Lower to: sprintf(buf, "prefix%lldmiddle%lldsuffix", a, b)
	// Then wrap in a string struct using statement expression
	//
	// For now, use libc sprintf. Later this can use strconv functions.

	i8_t := b.mod.type_store.get_int(8)
	i64_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i8_t)

	// 1. Build the format string and collect argument values
	mut format_str := ''
	mut args := []ValueID{}

	for i, val in node.values {
		// Add the literal string part (strip quotes from first/last parts)
		mut clean_val := val
		if i == 0 {
			clean_val = clean_val.trim_left("'").trim_left('"')
		}
		if i == node.values.len - 1 {
			clean_val = clean_val.trim_right("'").trim_right('"')
		}
		format_str += clean_val

		// Add format specifier and argument for interpolation
		if i < node.inters.len {
			inter := node.inters[i]

			// Check if this is a string type expression
			mut is_string_type := false
			if inter.expr is ast.Ident {
				if t := b.get_var_struct_type(inter.expr.name) {
					if t == 'string' {
						is_string_type = true
					}
				}
			}

			if is_string_type {
				// For strings, extract the .str field (index 0) and use %s format
				// String struct: { str *char, len int, is_lit int }
				// We need to get the char pointer (field 0)
				//
				// Variable layout: b.addr() -> ptr to slot -> ptr to string struct
				// So we need to: load(addr) -> ptr to struct, then GEP to field 0
				var_slot := b.addr(inter.expr)
				// Load the string pointer from the variable slot
				string_struct_ptr := b.mod.add_instr(.load, b.cur_block, ptr_t, [
					var_slot,
				])
				// GEP to get address of .str field (index 0)
				zero_idx := b.mod.add_value_node(.constant, i64_t, '0', 0)
				str_field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t,
					[
					string_struct_ptr,
					zero_idx,
				])
				// Load the char* from the .str field
				str_val := b.mod.add_instr(.load, b.cur_block, ptr_t, [str_field_ptr])
				args << str_val
				format_str += '%s'
			} else {
				// Evaluate the interpolated expression
				arg_val := b.expr(inter.expr)
				args << arg_val

				// Determine format specifier based on format type
				format_str += b.get_printf_format(inter)
			}
		}
	}

	// 2. Allocate buffer on stack (256 bytes should be enough for most strings)
	buf_size := 256
	array_type := b.mod.type_store.get_array(i8_t, buf_size)
	array_ptr_t := b.mod.type_store.get_ptr(array_type)
	buf_ptr := b.mod.add_instr(.alloca, b.cur_block, array_ptr_t, [])

	// 3. Create format string constant
	format_val := b.mod.add_value_node(.constant, ptr_t, '"${format_str}"', 0)

	// 4. Call sprintf(buf, format, args...)
	sprintf_fn := b.mod.add_value_node(.unknown, 0, 'sprintf', 0)
	mut call_args := []ValueID{}
	call_args << sprintf_fn
	call_args << buf_ptr
	call_args << format_val
	for arg in args {
		call_args << arg
	}
	b.mod.add_instr(.call, b.cur_block, i64_t, call_args)

	// 5. Call strlen to get the length
	strlen_fn := b.mod.add_value_node(.unknown, 0, 'strlen', 0)
	strlen_result := b.mod.add_instr(.call, b.cur_block, i64_t, [strlen_fn, buf_ptr])

	// 6. Create string struct by value using inline_string_init instruction
	// This will generate: (string){buf, strlen_result, 0}
	// ARM64 backend materializes this as pointer to stack-resident struct
	string_type_id := b.struct_types['string']
	string_ptr_type := b.mod.type_store.get_ptr(string_type_id)
	zero := b.mod.add_value_node(.constant, i64_t, '0', 0)
	return b.mod.add_instr(.inline_string_init, b.cur_block, string_ptr_type, [
		buf_ptr,
		strlen_result,
		zero,
	])
}

fn (b Builder) get_printf_format(inter ast.StringInter) string {
	// Convert V format specifier to printf format specifier
	// For now, default to %lld for integers (64-bit)
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
		// Both width and precision specified
		return '%${inter.width}.${inter.precision}' + base_fmt[1..]
	} else if inter.width > 0 {
		return '%${inter.width}' + base_fmt[1..]
	} else if inter.precision > 0 {
		return '%.${inter.precision}' + base_fmt[1..]
	}
	return base_fmt
}

fn (mut b Builder) expr_call_or_cast(node ast.CallOrCastExpr) ValueID {
	// Check if this is a primitive type cast (int, i64, voidptr, etc.)
	// These are not function calls - just return the expression value
	if node.lhs is ast.Ident {
		cast_name := node.lhs.name
		if cast_name in ['int', 'i64', 'i32', 'i16', 'i8', 'u64', 'u32', 'u16', 'u8', 'f32', 'f64',
			'voidptr', 'charptr', 'byteptr', 'usize', 'isize', 'rune', 'bool'] {
			// Type cast: just evaluate the expression (enums are already ints in SSA)
			return b.expr(node.expr)
		}
		// Check if this is interface boxing: Drawable(point)
		if cast_name in b.interface_names {
			return b.expr_interface_box(cast_name, node.expr)
		}
		// Check if this is a type alias cast: Builder([]u8{...})
		if cast_name in b.type_alias_bases || cast_name in b.struct_types {
			// Type cast to a struct/alias type - just evaluate the expression
			return b.expr(node.expr)
		}
		// Check if this is an enum cast: StrIntpType(1)
		if cast_name in b.enum_names {
			// Enum cast - just evaluate the expression (enums are ints)
			return b.expr(node.expr)
		}
	}

	// Handle ambiguous calls like print_int(1111)
	// Note: We defer argument evaluation until we know if this is a flag enum method
	// so we can set cur_match_type for enum shorthand resolution
	mut args := []ValueID{}
	mut flag_enum_receiver_type := ''

	mut name := ''
	mut is_method_call := false
	mut receiver_val := ValueID(0)

	if node.lhs is ast.Ident {
		name = node.lhs.name
	} else if node.lhs is ast.SelectorExpr {
		method_name := node.lhs.rhs.name
		// Check if this is a method call (receiver.method()) or C.func()
		if node.lhs.lhs is ast.Ident {
			receiver_name := node.lhs.lhs.name
			if receiver_name == 'C' {
				name = method_name
			} else if struct_type_name := b.get_var_struct_type(receiver_name) {
				i32_t := b.mod.type_store.get_int(64)
				ptr_t := b.mod.type_store.get_ptr(i32_t)
				// Check if this is a function pointer field call (e.g., m.hash_fn(args))
				fn_ptr_key := '${struct_type_name}.${method_name}'
				if fn_ptr_key in b.fn_ptr_fields {
					// Function pointer field call - load the function pointer and do indirect call
					var_ptr := b.addr(node.lhs.lhs)
					ptr_typ := b.mod.values[var_ptr].typ
					elem_typ := b.mod.type_store.types[ptr_typ].elem_type
					struct_ptr := b.mod.add_instr(.load, b.cur_block, elem_typ, [
						var_ptr,
					])

					// Get field index - look up the struct type by name
					struct_typ_id := b.struct_types[struct_type_name] or { 0 }
					struct_type := b.mod.type_store.types[struct_typ_id]
					mut field_idx := -1
					for i, fname in struct_type.field_names {
						if fname == method_name {
							field_idx = i
							break
						}
					}

					if field_idx >= 0 {
						// Get field pointer and load function pointer
						idx_val := b.mod.add_value_node(.constant, i32_t, '${field_idx}',
							field_idx)
						field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t,
							[
							struct_ptr,
							idx_val,
						])
						fn_ptr := b.mod.add_instr(.load, b.cur_block, ptr_t, [
							field_ptr,
						])

						// Evaluate argument and prepend function pointer as first operand for indirect call
						mut fn_args := []ValueID{}
						fn_args << b.expr(node.expr)
						fn_args.prepend(fn_ptr)
						return b.mod.add_instr(.call_indirect, b.cur_block, i32_t, fn_args)
					}
				}
				// Regular method call - mangle the name
				// Resolve type alias to base type for method name
				mangled_type := if base_type := b.type_alias_bases[struct_type_name] {
					base_type
				} else {
					struct_type_name
				}
				name = '${mangled_type}__${method_name}'
				is_method_call = true
				// Check if this is a flag enum method
				if mangled_type in b.flag_enum_names && method_name in ['has', 'all'] {
					flag_enum_receiver_type = mangled_type
				}
				// Get the receiver - need to load the struct pointer from the variable
				var_ptr := b.addr(node.lhs.lhs)
				ptr_typ := b.mod.values[var_ptr].typ
				elem_typ := b.mod.type_store.types[ptr_typ].elem_type
				receiver_val = b.mod.add_instr(.load, b.cur_block, elem_typ, [var_ptr])
			} else {
				// Unknown receiver type - try to infer from registered functions
				// Mangle operator names to valid symbol names first
				mangled_method := match method_name {
					'+' { 'plus' }
					'-' { 'minus' }
					'*' { 'mul' }
					'/' { 'div' }
					'%' { 'mod' }
					'==' { 'eq' }
					'!=' { 'ne' }
					'<' { 'lt' }
					'>' { 'gt' }
					'<=' { 'le' }
					'>=' { 'ge' }
					else { method_name }
				}
				inferred_type := b.infer_receiver_type(mangled_method)
				if inferred_type != '' {
					name = '${inferred_type}__${mangled_method}'
					// Check if this is a flag enum method - SET cur_match_type
					if inferred_type in b.flag_enum_names && mangled_method in ['has', 'all'] {
						flag_enum_receiver_type = inferred_type
					}
				} else {
					name = mangled_method
				}
			}
		} else {
			// Complex expression as receiver - try to infer type from method name
			mname := node.lhs.rhs.name
			// Mangle operator names to valid symbol names
			mangled_mname := match mname {
				'+' { 'plus' }
				'-' { 'minus' }
				'*' { 'mul' }
				'/' { 'div' }
				'%' { 'mod' }
				'==' { 'eq' }
				'!=' { 'ne' }
				'<' { 'lt' }
				'>' { 'gt' }
				'<=' { 'le' }
				'>=' { 'ge' }
				else { mname }
			}
			inferred_type := b.infer_receiver_type(mangled_mname)
			if inferred_type != '' {
				name = '${inferred_type}__${mangled_mname}'
				// Check if this is a flag enum method
				if inferred_type in b.flag_enum_names && mangled_mname in ['has', 'all'] {
					flag_enum_receiver_type = inferred_type
				}
			} else {
				name = mangled_mname
			}
			// Generate receiver and set it for method call
			receiver_val = b.expr(node.lhs.lhs)
			is_method_call = true
		}
	}

	// For flag enum methods, set cur_match_type for enum shorthand resolution
	old_match_type := b.cur_match_type
	if flag_enum_receiver_type != '' {
		b.cur_match_type = flag_enum_receiver_type
	}
	// Now evaluate the argument with proper enum context
	args << b.expr(node.expr)
	b.cur_match_type = old_match_type

	// For method calls, prepend receiver as first argument
	if is_method_call && receiver_val != 0 {
		args.prepend(receiver_val)
	}

	fn_val := b.mod.add_value_node(.unknown, 0, name, 0)
	args.prepend(fn_val)

	// Look up the return type from tracked functions
	i64_t := b.mod.type_store.get_int(64)
	ret_type := if rt := b.func_ret_types[name] { rt } else { i64_t }

	return b.mod.add_instr(.call, b.cur_block, ret_type, args)
}

fn (mut b Builder) expr_prefix(node ast.PrefixExpr) ValueID {
	// Handle address-of operator with struct init: &Point{} -> heap allocation
	if node.op == .amp {
		if node.expr is ast.InitExpr {
			return b.expr_heap_alloc(node.expr)
		}
		// For other &expr cases, just return the address
		return b.addr(node.expr)
	}

	right := b.expr(node.expr)
	right_typ := b.mod.values[right].typ

	match node.op {
		.minus {
			// Unary minus: propagate the operand type
			right_info := b.mod.type_store.types[right_typ]
			zero_str := if right_info.kind == .float_t { '0.0' } else { '0' }
			zero := b.mod.add_value_node(.constant, right_typ, zero_str, 0)
			op := if right_info.kind == .float_t { OpCode.fsub } else { OpCode.sub }
			return b.mod.add_instr(op, b.cur_block, right_typ, [zero, right])
		}
		.not {
			// Logical not: result is bool (i8)
			bool_t := b.mod.type_store.get_int(8)
			zero := b.mod.add_value_node(.constant, right_typ, '0', 0)
			return b.mod.add_instr(.eq, b.cur_block, bool_t, [right, zero])
		}
		else {
			return 0
		}
	}
}

fn (mut b Builder) expr_range(node ast.RangeExpr) ValueID {
	// RangeExpr represents a range like 0..10 or 0...10
	// We create a small struct with (start, end) values
	i64_t := b.mod.type_store.get_int(64)

	// Evaluate start and end expressions
	start_val := b.expr(node.start)
	end_val := b.expr(node.end)

	// Create a range type (struct with 2 int64 fields)
	// Check if we already have a range type registered
	mut range_t := TypeID(0)
	for i, t in b.mod.type_store.types {
		if t.kind == .struct_t && t.field_names.len == 2 && t.field_names[0] == '_range_start' {
			range_t = i
			break
		}
	}
	if range_t == 0 {
		// Register new range type
		t := Type{
			kind:        .struct_t
			fields:      [i64_t, i64_t]
			field_names: ['_range_start', '_range_end']
			width:       0
		}
		range_t = b.mod.type_store.register(t)
	}

	ptr_t := b.mod.type_store.get_ptr(range_t)
	range_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])

	// Store start value at field 0
	idx0 := b.mod.add_value_node(.constant, i64_t, '0', 0)
	start_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, b.mod.type_store.get_ptr(i64_t),
		[range_ptr, idx0])
	b.mod.add_instr(.store, b.cur_block, 0, [start_val, start_ptr])

	// Store end value at field 1
	idx1 := b.mod.add_value_node(.constant, i64_t, '1', 0)
	end_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, b.mod.type_store.get_ptr(i64_t),
		[range_ptr, idx1])
	b.mod.add_instr(.store, b.cur_block, 0, [end_val, end_ptr])

	return range_ptr
}

fn (mut b Builder) expr_array_init(node ast.ArrayInitExpr) ValueID {
	// Array Init: [1, 2, 3] or []int{len: 10, cap: 20, init: 0}
	i64_t := b.mod.type_store.get_int(64)

	// Determine element count
	mut elem_count := node.exprs.len

	// Check if this is a sized array initialization (len: expr)
	if node.len !is ast.EmptyExpr {
		// For []T{len: n}, we need to evaluate the length expression
		len_val := b.expr(node.len)
		// For now, only support constant lengths
		if b.mod.values[len_val].kind == .constant {
			elem_count = b.mod.values[len_val].name.int()
		}
	}

	if elem_count == 0 {
		// Empty array - return null pointer for now
		return b.mod.add_value_node(.constant, b.mod.type_store.get_ptr(i64_t), '0', 0)
	}

	// Create array type
	array_type := b.mod.type_store.get_array(i64_t, elem_count)
	array_ptr_t := b.mod.type_store.get_ptr(array_type)
	elem_ptr_t := b.mod.type_store.get_ptr(i64_t)

	// Allocate array on stack
	array_ptr := b.mod.add_instr(.alloca, b.cur_block, array_ptr_t, [])

	// Initialize elements
	if node.exprs.len > 0 {
		// Literal array: [1, 2, 3]
		for i, elem_expr in node.exprs {
			// Evaluate element expression
			elem_val := b.expr(elem_expr)

			// Compute element address using GEP
			idx_val := b.mod.add_value_node(.constant, i64_t, i.str(), 0)
			elem_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_t, [
				array_ptr,
				idx_val,
			])

			// Store element value
			b.mod.add_instr(.store, b.cur_block, 0, [elem_val, elem_ptr])
		}
	} else if node.init !is ast.EmptyExpr {
		// Sized array with init value: []int{len: 10, init: 0}
		init_val := b.expr(node.init)

		// Initialize all elements with the init value
		for i in 0 .. elem_count {
			idx_val := b.mod.add_value_node(.constant, i64_t, i.str(), 0)
			elem_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_t, [
				array_ptr,
				idx_val,
			])
			b.mod.add_instr(.store, b.cur_block, 0, [init_val, elem_ptr])
		}
	} else {
		// Sized array without init: zero-initialize
		zero_val := b.mod.add_value_node(.constant, i64_t, '0', 0)

		for i in 0 .. elem_count {
			idx_val := b.mod.add_value_node(.constant, i64_t, i.str(), 0)
			elem_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_t, [
				array_ptr,
				idx_val,
			])
			b.mod.add_instr(.store, b.cur_block, 0, [zero_val, elem_ptr])
		}
	}

	// Return the array pointer
	return array_ptr
}

// expr_map_init_from_type handles empty map init from InitExpr with MapType: map[K]V{}
fn (mut b Builder) expr_map_init_from_type(map_type ast.MapType) ValueID {
	// Simple inline map implementation
	// Structure: [len, keys[0..31], values[0..31]]
	i64_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i64_t)

	// Layout: index 0 = len, index 1-32 = keys, index 33-64 = values
	total_slots := 1 + 32 + 32

	// Allocate map structure on stack
	map_struct := b.mod.type_store.get_array(i64_t, total_slots)
	map_ptr_t := b.mod.type_store.get_ptr(map_struct)
	map_ptr := b.mod.add_instr(.alloca, b.cur_block, map_ptr_t, [])

	// Initialize len = 0
	zero := b.mod.add_value_node(.constant, i64_t, '0', 0)
	offset_0 := b.mod.add_value_node(.constant, i64_t, '0', 0)
	len_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [map_ptr, offset_0])
	b.mod.add_instr(.store, b.cur_block, 0, [zero, len_ptr])

	return map_ptr
}

fn (mut b Builder) expr_map_init(node ast.MapInitExpr) ValueID {
	// Simple inline map implementation for int->int maps
	// Structure (simplified): [len, keys[0..31], values[0..31]]
	// Total: 1 + 32 + 32 = 65 i64s = 520 bytes
	i64_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i64_t)

	// Layout (all in i64 units):
	// index 0: len
	// index 1-32: keys[0..31]
	// index 33-64: values[0..31]
	total_slots := 1 + 32 + 32

	// Allocate map structure on stack
	map_type := b.mod.type_store.get_array(i64_t, total_slots)
	map_ptr_t := b.mod.type_store.get_ptr(map_type)
	map_ptr := b.mod.add_instr(.alloca, b.cur_block, map_ptr_t, [])

	// Initialize len = 0
	zero := b.mod.add_value_node(.constant, i64_t, '0', 0)
	offset_0 := b.mod.add_value_node(.constant, i64_t, '0', 0)
	len_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [map_ptr, offset_0])
	b.mod.add_instr(.store, b.cur_block, 0, [zero, len_ptr])

	// If there are initial key-value pairs, add them
	if node.keys.len > 0 {
		for i, key_expr in node.keys {
			key_val := b.expr(key_expr)
			val_expr := node.vals[i]
			val_val := b.expr(val_expr)
			b.emit_simple_map_set(map_ptr, key_val, val_val)
		}
	}

	return map_ptr
}

// type_size_from_ast returns the size in bytes for an AST type expression.
fn (b &Builder) type_size_from_ast(typ ast.Expr) int {
	if typ is ast.Ident {
		match typ.name {
			'i8', 'u8', 'byte', 'bool' { return 1 }
			'i16', 'u16' { return 2 }
			'i32', 'u32', 'rune' { return 4 }
			'i64', 'u64', 'f64', 'int' { return 8 }
			'f32' { return 4 }
			'string' { return 24 } // V string struct: ptr + len + is_lit
			else { return 8 } // Default to pointer size
		}
	}
	return 8
}

// emit_simple_map_set - inline map set operation (linear search + append)
// Map layout: index 0 = len, index 1-32 = keys, index 33-64 = values
fn (mut b Builder) emit_simple_map_set(map_ptr ValueID, key_val ValueID, val_val ValueID) {
	i64_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i64_t)

	// Load map len from index 0
	offset_0 := b.mod.add_value_node(.constant, i64_t, '0', 0)
	len_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [map_ptr, offset_0])
	map_len := b.mod.add_instr(.load, b.cur_block, i64_t, [len_ptr])

	// Keys start at index 1, values at index 33
	keys_base := b.mod.add_value_node(.constant, i64_t, '1', 0)
	values_base := b.mod.add_value_node(.constant, i64_t, '33', 0)

	// Linear search loop to find existing key or end of list
	search_head := b.mod.add_block(b.cur_func, 'map_set.search_head')
	search_body := b.mod.add_block(b.cur_func, 'map_set.search_body')
	search_cont := b.mod.add_block(b.cur_func, 'map_set.search_cont')
	found_blk := b.mod.add_block(b.cur_func, 'map_set.found')
	not_found_blk := b.mod.add_block(b.cur_func, 'map_set.not_found')
	exit_blk := b.mod.add_block(b.cur_func, 'map_set.exit')

	// Allocate loop index on stack
	idx_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
	zero := b.mod.add_value_node(.constant, i64_t, '0', 0)
	b.mod.add_instr(.store, b.cur_block, 0, [zero, idx_ptr])

	// Jump to search head
	search_head_val := b.mod.blocks[search_head].val_id
	b.mod.add_instr(.jmp, b.cur_block, 0, [search_head_val])

	// Search head: check if idx < len
	b.cur_block = search_head
	idx := b.mod.add_instr(.load, b.cur_block, i64_t, [idx_ptr])
	cmp := b.mod.add_instr(.lt, b.cur_block, i64_t, [idx, map_len])
	search_body_val := b.mod.blocks[search_body].val_id
	not_found_val := b.mod.blocks[not_found_blk].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [cmp, search_body_val, not_found_val])

	// Search body: compare keys[idx] with key
	b.cur_block = search_body
	idx2 := b.mod.add_instr(.load, b.cur_block, i64_t, [idx_ptr])
	// key index = keys_base + idx = 1 + idx
	key_idx := b.mod.add_instr(.add, b.cur_block, i64_t, [keys_base, idx2])
	key_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [map_ptr, key_idx])
	existing_key := b.mod.add_instr(.load, b.cur_block, i64_t, [key_ptr])
	key_eq := b.mod.add_instr(.eq, b.cur_block, i64_t, [existing_key, key_val])
	found_val := b.mod.blocks[found_blk].val_id
	search_cont_val := b.mod.blocks[search_cont].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [key_eq, found_val, search_cont_val])

	// Search continue: increment index and jump back to head
	b.cur_block = search_cont
	one := b.mod.add_value_node(.constant, i64_t, '1', 0)
	idx3 := b.mod.add_instr(.load, b.cur_block, i64_t, [idx_ptr])
	next_idx := b.mod.add_instr(.add, b.cur_block, i64_t, [idx3, one])
	b.mod.add_instr(.store, b.cur_block, 0, [next_idx, idx_ptr])
	b.mod.add_instr(.jmp, b.cur_block, 0, [search_head_val])

	// Found: update existing value
	b.cur_block = found_blk
	idx4 := b.mod.add_instr(.load, b.cur_block, i64_t, [idx_ptr])
	// value index = values_base + idx = 33 + idx
	val_idx := b.mod.add_instr(.add, b.cur_block, i64_t, [values_base, idx4])
	val_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [map_ptr, val_idx])
	b.mod.add_instr(.store, b.cur_block, 0, [val_val, val_ptr])
	exit_val := b.mod.blocks[exit_blk].val_id
	b.mod.add_instr(.jmp, b.cur_block, 0, [exit_val])

	// Not found: append new key-value pair
	b.cur_block = not_found_blk
	// Store key at keys[len] = map_ptr[1 + len]
	new_key_idx := b.mod.add_instr(.add, b.cur_block, i64_t, [keys_base, map_len])
	new_key_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [map_ptr, new_key_idx])
	b.mod.add_instr(.store, b.cur_block, 0, [key_val, new_key_ptr])
	// Store value at values[len] = map_ptr[33 + len]
	new_val_idx := b.mod.add_instr(.add, b.cur_block, i64_t, [values_base, map_len])
	new_val_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [map_ptr, new_val_idx])
	b.mod.add_instr(.store, b.cur_block, 0, [val_val, new_val_ptr])
	// Increment len
	new_len := b.mod.add_instr(.add, b.cur_block, i64_t, [map_len, one])
	b.mod.add_instr(.store, b.cur_block, 0, [new_len, len_ptr])
	b.mod.add_instr(.jmp, b.cur_block, 0, [exit_val])

	// Exit block
	b.cur_block = exit_blk
}

// emit_simple_map_get - inline map get operation (linear search)
// Map layout: index 0 = len, index 1-32 = keys, index 33-64 = values
fn (mut b Builder) emit_simple_map_get(map_ptr ValueID, key_val ValueID) ValueID {
	i64_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i64_t)

	// Load map len from index 0
	offset_0 := b.mod.add_value_node(.constant, i64_t, '0', 0)
	len_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [map_ptr, offset_0])
	map_len := b.mod.add_instr(.load, b.cur_block, i64_t, [len_ptr])

	// Keys start at index 1, values at index 33
	keys_base := b.mod.add_value_node(.constant, i64_t, '1', 0)
	values_base := b.mod.add_value_node(.constant, i64_t, '33', 0)

	// Allocate result on stack (default to 0)
	result_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
	zero := b.mod.add_value_node(.constant, i64_t, '0', 0)
	b.mod.add_instr(.store, b.cur_block, 0, [zero, result_ptr])

	// Linear search loop
	search_head := b.mod.add_block(b.cur_func, 'map_get.search_head')
	search_body := b.mod.add_block(b.cur_func, 'map_get.search_body')
	search_cont := b.mod.add_block(b.cur_func, 'map_get.search_cont')
	found_blk := b.mod.add_block(b.cur_func, 'map_get.found')
	exit_blk := b.mod.add_block(b.cur_func, 'map_get.exit')

	// Allocate loop index on stack
	idx_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
	b.mod.add_instr(.store, b.cur_block, 0, [zero, idx_ptr])

	// Jump to search head
	search_head_val := b.mod.blocks[search_head].val_id
	b.mod.add_instr(.jmp, b.cur_block, 0, [search_head_val])

	// Search head: check if idx < len
	b.cur_block = search_head
	idx := b.mod.add_instr(.load, b.cur_block, i64_t, [idx_ptr])
	cmp := b.mod.add_instr(.lt, b.cur_block, i64_t, [idx, map_len])
	search_body_val := b.mod.blocks[search_body].val_id
	exit_val := b.mod.blocks[exit_blk].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [cmp, search_body_val, exit_val])

	// Search body: compare keys[idx] with key
	b.cur_block = search_body
	idx2 := b.mod.add_instr(.load, b.cur_block, i64_t, [idx_ptr])
	// key index = keys_base + idx = 1 + idx
	key_idx := b.mod.add_instr(.add, b.cur_block, i64_t, [keys_base, idx2])
	key_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [map_ptr, key_idx])
	existing_key := b.mod.add_instr(.load, b.cur_block, i64_t, [key_ptr])
	key_eq := b.mod.add_instr(.eq, b.cur_block, i64_t, [existing_key, key_val])
	found_val := b.mod.blocks[found_blk].val_id
	search_cont_val := b.mod.blocks[search_cont].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [key_eq, found_val, search_cont_val])

	// Search continue: increment index and jump back to head
	b.cur_block = search_cont
	one := b.mod.add_value_node(.constant, i64_t, '1', 0)
	idx3 := b.mod.add_instr(.load, b.cur_block, i64_t, [idx_ptr])
	next_idx := b.mod.add_instr(.add, b.cur_block, i64_t, [idx3, one])
	b.mod.add_instr(.store, b.cur_block, 0, [next_idx, idx_ptr])
	b.mod.add_instr(.jmp, b.cur_block, 0, [search_head_val])

	// Found: store value to result
	b.cur_block = found_blk
	idx4 := b.mod.add_instr(.load, b.cur_block, i64_t, [idx_ptr])
	// value index = values_base + idx = 33 + idx
	val_idx := b.mod.add_instr(.add, b.cur_block, i64_t, [values_base, idx4])
	val_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [map_ptr, val_idx])
	found_value := b.mod.add_instr(.load, b.cur_block, i64_t, [val_ptr])
	b.mod.add_instr(.store, b.cur_block, 0, [found_value, result_ptr])
	b.mod.add_instr(.jmp, b.cur_block, 0, [exit_val])

	// Exit block: return result
	b.cur_block = exit_blk
	return b.mod.add_instr(.load, b.cur_block, i64_t, [result_ptr])
}

fn (mut b Builder) expr_heap_alloc(node ast.InitExpr) ValueID {
	// Heap allocation: &StructInit{}
	// 1. Find struct type
	mut struct_t := 0

	if node.typ is ast.Ident {
		if st := b.struct_types[node.typ.name] {
			struct_t = st
		}
	}

	if struct_t == 0 {
		for i, t in b.mod.type_store.types {
			if t.kind == .struct_t {
				struct_t = i
				break
			}
		}
	}

	// 2. Calculate size (fields.len * 8 for 64-bit)
	struct_type := b.mod.type_store.types[struct_t]
	mut size := struct_type.fields.len * 8
	if size == 0 {
		size = 16 // Default size for empty struct
	}

	i64_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(struct_t)

	// 3. Call malloc
	malloc_fn := b.mod.add_value_node(.unknown, 0, 'malloc', 0)
	size_val := b.mod.add_value_node(.constant, i64_t, size.str(), 0)
	heap_ptr := b.mod.add_instr(.call, b.cur_block, ptr_t, [malloc_fn, size_val])

	// 4. Initialize fields
	for i, field in node.fields {
		val := b.expr(field.value)
		idx_val := b.mod.add_value_node(.constant, i64_t, i.str(), 0)

		// GEP to field
		field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, b.mod.type_store.get_ptr(i64_t),
			[heap_ptr, idx_val])
		b.mod.add_instr(.store, b.cur_block, 0, [val, field_ptr])
	}

	return heap_ptr
}

fn (mut b Builder) expr_postfix(node ast.PostfixExpr) ValueID {
	// Handle i++ / i--
	if node.expr is ast.Ident {
		name := (node.expr as ast.Ident).name
		if ptr := b.vars[name] {
			if ptr != 0 {
				i32_t := b.mod.type_store.get_int(64)

				// 1. Load current value
				old_val := b.mod.add_instr(.load, b.cur_block, i32_t, [ptr])

				// 2. Add/Sub 1
				one := b.mod.add_value_node(.constant, i32_t, '1', 0)
				op := if node.op == .inc { OpCode.add } else { OpCode.sub }
				new_val := b.mod.add_instr(op, b.cur_block, i32_t, [old_val, one])

				// 3. Store new value
				b.mod.add_instr(.store, b.cur_block, 0, [new_val, ptr])

				// Postfix returns the old value
				return old_val
			}
		}
	}
	return 0
}

fn (b Builder) is_block_terminated(blk_id int) bool {
	if blk_id >= b.mod.blocks.len {
		return false
	}
	blk := b.mod.blocks[blk_id]
	if blk.instrs.len == 0 {
		return false
	}

	last_val_id := blk.instrs.last()
	val := b.mod.values[last_val_id]
	if val.kind != .instruction {
		return false
	}

	instr := b.mod.instrs[val.index]
	return instr.op in [.ret, .br, .jmp, .unreachable]
}

// addr returns the ValueID (pointer) representing the L-Value of an expression
fn (mut b Builder) addr(node ast.Expr) ValueID {
	match node {
		ast.Ident {
			// Check locals
			if ptr := b.vars[node.name] {
				// FIX: Ensure it is a valid ID (0 is invalid now)
				if ptr != 0 {
					return ptr
				}
			}
			// Check globals
			for g in b.mod.globals {
				if g.name == node.name {
					// Globals are values in the values arena but effectively pointers
					// We need to find the ValueID that corresponds to this global
					// For this demo, we iterate values to find it (slow, but works)
					for v in b.mod.values {
						if v.kind == .global && v.name == node.name {
							return v.id
						}
					}
				}
			}
			return 0
		}
		ast.SelectorExpr {
			// Check for C module globals (C.stdout, C.stderr)
			if node.lhs is ast.Ident && node.lhs.name == 'C' {
				// Map C.stdout -> __stdoutp, C.stderr -> __stderrp
				c_global_name := match node.rhs.name {
					'stdout' { '__stdoutp' }
					'stderr' { '__stderrp' }
					else { '' }
				}
				if c_global_name != '' {
					// Create external global for C stdio
					voidptr_t := b.mod.type_store.get_ptr(b.mod.type_store.get_int(8))
					return b.mod.add_external_global(c_global_name, voidptr_t)
				}
			}
			// struct.field
			base_ptr := b.addr(node.lhs)

			// Resolve the type of the base pointer
			base_val := b.mod.values[base_ptr]
			mut ptr_typ := b.mod.type_store.types[base_val.typ]

			// We expect ptr_typ to be Ptr -> (Struct) OR Ptr -> (Ptr -> Struct)
			// If it's Ptr -> Ptr -> ..., we must Load to get the actual struct pointer.

			// Unpack one level of pointer (the variable address)
			mut val_typ_id := ptr_typ.elem_type
			mut val_typ := b.mod.type_store.types[val_typ_id]

			mut actual_base := base_ptr

			// Check if the value stored is a pointer (Reference semantics for variable)
			if val_typ.kind == .ptr_t {
				// Load the pointer value
				actual_base = b.mod.add_instr(.load, b.cur_block, val_typ_id, [
					base_ptr,
				])

				// Update types for the loaded value
				// actual_base is now Ptr -> Struct
				ptr_typ = val_typ
				val_typ_id = ptr_typ.elem_type
				val_typ = b.mod.type_store.types[val_typ_id]
			}

			// Now val_typ should be the Struct
			if val_typ.kind != .struct_t {
				// Fallback or error. For now, try to proceed, but it might panic if we access fields.
				// In a real compiler, this checks if it's a struct.
			}

			// Find field index by name
			mut idx := -1
			for i, name in val_typ.field_names {
				if name == node.rhs.name {
					idx = i
					break
				}
			}
			if idx == -1 {
				// Fallback to old behavior for backwards compatibility
				// Field indices for common structs:
				// - Point/Coord: x=0, y=1
				// - Pair: a=0, b=1
				// - string: str=0, len=1, is_lit=2
				idx = 0
				if node.rhs.name == 'y' || node.rhs.name == 'b' || node.rhs.name == 'len' {
					idx = 1
				} else if node.rhs.name == 'is_lit' {
					idx = 2
				}
			}

			// Safety check for index
			mut field_type := if idx < val_typ.fields.len {
				val_typ.fields[idx]
			} else {
				// Type resolution failed (val_typ.fields is empty)
				// For known struct fields, use i64 as default field type
				// - struct Point: x=0, y=1 (both int)
				// - struct Pair: a=0, b=1 (both int)
				// - struct string: str=0 (ptr), len=1 (int), is_lit=2 (int)
				if idx == 0 && val_typ.kind != .struct_t {
					// Field 0 is at base address, return it directly
					return actual_base
				}
				// Use i64 as default for unresolved fields
				b.mod.type_store.get_int(64)
			}

			idx_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(64), idx.str(),
				0)

			// GEP
			field_ptr_t := b.mod.type_store.get_ptr(field_type)
			return b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_t, [
				actual_base,
				idx_val,
			])
		}
		ast.IndexExpr {
			// array[index]
			base_ptr := b.addr(node.lhs)
			index_val := b.expr(node.expr)

			// Auto-dereference if it's a pointer-to-pointer (variable holding array ptr)
			base_val := b.mod.values[base_ptr]
			ptr_typ := b.mod.type_store.types[base_val.typ]
			elem_typ_id := ptr_typ.elem_type
			elem_typ := b.mod.type_store.types[elem_typ_id]

			mut actual_base := base_ptr

			if elem_typ.kind == .ptr_t {
				actual_base = b.mod.add_instr(.load, b.cur_block, elem_typ_id, [
					base_ptr,
				])
			}

			return b.mod.add_instr(.get_element_ptr, b.cur_block, b.mod.values[actual_base].typ,
				[
				actual_base,
				index_val,
			])
		}
		else {
			return 0
		}
	}
}

// emit_deferred_stmts emits all deferred statements in reverse order
fn (mut b Builder) emit_deferred_stmts() {
	// Execute deferred statements in LIFO order (last defer first)
	for i := b.defer_stmts.len - 1; i >= 0; i-- {
		b.stmts(b.defer_stmts[i])
	}
}

// eval_const_expr evaluates a constant expression at compile time
fn (mut b Builder) eval_const_expr(expr ast.Expr) i64 {
	match expr {
		ast.BasicLiteral {
			if expr.kind == .number {
				return expr.value.i64()
			}
		}
		ast.PrefixExpr {
			if expr.op == .minus {
				return -b.eval_const_expr(expr.expr)
			}
		}
		ast.InfixExpr {
			lhs := b.eval_const_expr(expr.lhs)
			rhs := b.eval_const_expr(expr.rhs)
			return match expr.op {
				.plus { lhs + rhs }
				.minus { lhs - rhs }
				.mul { lhs * rhs }
				.div { lhs / rhs }
				.mod { lhs % rhs }
				else { 0 }
			}
		}
		ast.ParenExpr {
			return b.eval_const_expr(expr.expr)
		}
		else {}
	}
	return 0
}

// expr_interface_box creates an interface value from a concrete type
// For the native backend, we simply store a pointer to the original value
// The concrete type is tracked at compile time for direct method dispatch
fn (mut b Builder) expr_interface_box(iface_name string, expr ast.Expr) ValueID {
	// Get the address of the expression (pointer to the struct)
	addr := b.addr(expr)

	// b.addr() returns the stack slot address (Ptr(Ptr(struct)))
	// We need to load to get the actual pointer to the struct (Ptr(struct))
	ptr_typ := b.mod.values[addr].typ
	elem_typ := b.mod.type_store.types[ptr_typ].elem_type
	return b.mod.add_instr(.load, b.cur_block, elem_typ, [addr])
}

// infer_concrete_type attempts to determine the type name of an expression
fn (b &Builder) infer_concrete_type(expr ast.Expr) string {
	match expr {
		ast.Ident {
			// Look up variable type from local map or environment
			if t := b.get_var_struct_type(expr.name) {
				return t
			}
		}
		ast.InitExpr {
			// Struct init: Point{...}
			if expr.typ is ast.Ident {
				return expr.typ.name
			}
		}
		else {}
	}
	return 'unknown'
}
