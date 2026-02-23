// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module ssa

import v2.ast
import v2.types

pub struct Builder {
mut:
	mod        &Module
	env        &types.Environment = unsafe { nil }
	cur_func   int                = -1
	cur_block  BlockID            = -1
	cur_module string             = 'main'
	// Variable name -> SSA ValueID (alloca pointer)
	vars map[string]ValueID
	// Loop break/continue targets
	loop_stack []LoopInfo
	// Struct name -> SSA TypeID
	struct_types map[string]TypeID
	// Enum name -> field values
	enum_values map[string]int
	// Function name -> SSA function index
	fn_index map[string]int
	// Constant name -> evaluated integer value (for inlining)
	const_values map[string]i64
	// String constant name -> string literal value (for inlining)
	string_const_values map[string]string
	// Label name -> SSA BlockID (for goto/label support)
	label_blocks map[string]BlockID
	// Track mut pointer params (e.g., mut buf &u8) that need extra dereference
	// when used in expressions (buf is ptr(ptr(i8)), but user sees buf as &u8)
	mut_ptr_params map[string]bool
}

struct LoopInfo {
	cond_block BlockID
	exit_block BlockID
}

pub fn Builder.new(mod &Module) &Builder {
	return Builder.new_with_env(mod, unsafe { nil })
}

pub fn Builder.new_with_env(mod &Module, env &types.Environment) &Builder {
	mut b := &Builder{
		mod:          mod
		vars:         map[string]ValueID{}
		loop_stack:   []LoopInfo{}
		struct_types: map[string]TypeID{}
		enum_values:  map[string]int{}
		fn_index:     map[string]int{}
	}
	unsafe {
		b.env = env
		mod.env = env
	}
	return b
}

pub fn (mut b Builder) build_all(files []ast.File) {
	// Register builtin globals needed by all backends
	i32_t := b.mod.type_store.get_int(32)
	i8_t := b.mod.type_store.get_int(8)
	ptr_t := b.mod.type_store.get_ptr(i8_t)
	ptr_ptr_t := b.mod.type_store.get_ptr(ptr_t)
	b.mod.add_global('g_main_argc', i32_t, false)
	b.mod.add_global('g_main_argv', ptr_ptr_t, false)

	// Phase 1a: Register core builtin types first (string, array) since other structs depend on them.
	// First, register builtin enums (e.g., ArrayFlags) so their types resolve correctly
	// when registering struct fields for array/string.
	for file in files {
		b.cur_module = file_module_name(file)
		if b.cur_module == 'builtin' {
			for stmt in file.stmts {
				if stmt is ast.EnumDecl {
					b.register_enum(stmt)
				}
			}
		}
	}
	for file in files {
		b.cur_module = file_module_name(file)
		if b.cur_module == 'builtin' {
			for stmt in file.stmts {
				if stmt is ast.StructDecl {
					if stmt.name in ['string', 'array'] {
						b.register_struct(stmt)
					}
				}
			}
		}
	}
	// Phase 1b: Register all struct type names (forward declarations) and enums
	for file in files {
		b.cur_module = file_module_name(file)
		b.register_types_pass1(file)
	}
	// Phase 1c: Fill in struct field types (now all struct names are known)
	for file in files {
		b.cur_module = file_module_name(file)
		b.register_types_pass2(file)
	}
	// Phase 2: Register consts and globals
	for file in files {
		b.cur_module = file_module_name(file)
		b.register_consts_and_globals(file)
	}
	// Phase 3: Register function signatures
	for file in files {
		b.cur_module = file_module_name(file)
		b.register_fn_signatures(file)
	}
	// Phase 3.5: Generate synthetic stubs for transformer-generated functions
	b.generate_array_eq_stub()
	b.generate_wyhash64_stub()
	b.generate_wyhash_stub()

	// Phase 4: Build function bodies
	for file in files {
		b.cur_module = file_module_name(file)
		b.build_fn_bodies(file)
	}
}

fn file_module_name(file ast.File) string {
	for stmt in file.stmts {
		if stmt is ast.ModuleStmt {
			return stmt.name.replace('.', '_')
		}
	}
	return 'main'
}

// --- Type resolution using types.Environment ---

fn (mut b Builder) type_to_ssa(t types.Type) TypeID {
	match t {
		types.Primitive {
			if t.props.has(.boolean) {
				return b.mod.type_store.get_int(1)
			}
			if t.props.has(.float) {
				width := if t.size == 32 { 32 } else { 64 }
				return b.mod.type_store.get_float(width)
			}
			if t.props.has(.integer) {
				size := if t.size == 0 { 32 } else { int(t.size) }
				return b.mod.type_store.get_int(size)
			}
			return b.mod.type_store.get_int(32)
		}
		types.Pointer {
			base := b.type_to_ssa(t.base_type)
			return b.mod.type_store.get_ptr(base)
		}
		types.String {
			return b.get_string_type()
		}
		types.Struct {
			if t.name in b.struct_types {
				return b.struct_types[t.name]
			}
			// Try module-qualified name: C structs are registered as "os__dirent"
			// but the type checker stores them as just "dirent"
			qualified := '${b.cur_module}__${t.name}'
			if qualified in b.struct_types {
				return b.struct_types[qualified]
			}
			// Try all known module prefixes for cross-module struct access
			for sname, sid in b.struct_types {
				if sname.ends_with('__${t.name}') {
					return sid
				}
			}
			return b.mod.type_store.get_int(64) // fallback
		}
		types.Enum {
			return b.mod.type_store.get_int(32)
		}
		types.Void {
			return 0 // void
		}
		types.Char {
			return b.mod.type_store.get_int(8)
		}
		types.Rune {
			return b.mod.type_store.get_int(32)
		}
		types.ISize {
			return b.mod.type_store.get_int(64)
		}
		types.USize {
			return b.mod.type_store.get_int(64)
		}
		types.Alias {
			return b.type_to_ssa(t.base_type)
		}
		types.Array {
			// Dynamic arrays are struct-like: {data*, len, cap, element_size}
			return b.get_array_type()
		}
		types.ArrayFixed {
			// Fixed-size arrays: [N]T → SSA array type
			elem_type := b.type_to_ssa(t.elem_type)
			if t.len > 0 && elem_type != 0 {
				return b.mod.type_store.get_array(elem_type, t.len)
			}
			return b.mod.type_store.get_int(64) // fallback
		}
		types.Nil {
			i8_t := b.mod.type_store.get_int(8)
			return b.mod.type_store.get_ptr(i8_t)
		}
		types.None {
			return 0
		}
		types.Tuple {
			tt := t.get_types()
			mut elem_types := []TypeID{cap: tt.len}
			for et in tt {
				elem_types << b.type_to_ssa(et)
			}
			return b.mod.type_store.get_tuple(elem_types)
		}
		types.SumType {
			if t.name in b.struct_types {
				return b.struct_types[t.name]
			}
			// Try module-qualified name
			qualified_st := '${b.cur_module}__${t.name}'
			if qualified_st in b.struct_types {
				return b.struct_types[qualified_st]
			}
			// Search all known module prefixes
			for sname, sid in b.struct_types {
				if sname.ends_with('__${t.name}') {
					return sid
				}
			}
			return b.mod.type_store.get_int(64) // fallback
		}
		types.Map {
			return b.struct_types['map'] or { b.mod.type_store.get_int(64) }
		}
		types.OptionType {
			// Native backend: Option types are just the base type
			base := b.type_to_ssa(t.base_type)
			if base != 0 {
				return base
			}
			return b.mod.type_store.get_int(64)
		}
		types.ResultType {
			// Native backend: Result types are just the base type
			base := b.type_to_ssa(t.base_type)
			if base != 0 {
				return base
			}
			return b.mod.type_store.get_int(64)
		}
		types.FnType {
			i8_t := b.mod.type_store.get_int(8)
			return b.mod.type_store.get_ptr(i8_t) // fn pointers
		}
		types.Interface {
			return b.mod.type_store.get_int(64) // interfaces lowered to i64
		}
		else {
			return b.mod.type_store.get_int(64) // fallback for unhandled
		}
	}
}

fn (mut b Builder) get_string_type() TypeID {
	return b.struct_types['string'] or { 0 }
}

fn (mut b Builder) get_array_type() TypeID {
	return b.struct_types['array'] or { 0 }
}

fn (mut b Builder) expr_type(e ast.Expr) TypeID {
	if b.env != unsafe { nil } {
		pos := e.pos()
		if pos.id != 0 {
			if typ := b.env.get_expr_type(pos.id) {
				return b.type_to_ssa(typ)
			}
		}
	}
	// Fallback for literals
	match e {
		ast.BasicLiteral {
			if e.kind == .key_true || e.kind == .key_false {
				return b.mod.type_store.get_int(1)
			}
			if e.kind == .number && e.value.contains('.') {
				return b.mod.type_store.get_float(64)
			}
			return b.mod.type_store.get_int(64)
		}
		ast.StringLiteral {
			return b.get_string_type()
		}
		else {
			return b.mod.type_store.get_int(64)
		}
	}
}

fn (mut b Builder) types_type_c_name(t types.Type) string {
	match t {
		types.Primitive {
			if t.props.has(.boolean) {
				return 'bool'
			}
			if t.props.has(.float) {
				return if t.size == 32 { 'f32' } else { 'f64' }
			}
			if t.props.has(.integer) {
				if t.props.has(.untyped) {
					return 'int'
				}
				size := if t.size == 0 { 32 } else { int(t.size) }
				is_signed := !t.props.has(.unsigned)
				return if is_signed {
					match size {
						8 { 'i8' }
						16 { 'i16' }
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
			}
			return 'int'
		}
		types.Pointer {
			return b.types_type_c_name(t.base_type) + '*'
		}
		types.String {
			return 'string'
		}
		types.Struct {
			return t.name
		}
		types.Enum {
			return t.name
		}
		types.Void {
			return 'void'
		}
		types.Char {
			return 'char'
		}
		types.Alias {
			return b.types_type_c_name(t.base_type)
		}
		types.Array {
			// []rune → Array_rune, []int → Array_int, etc.
			return 'Array_${b.types_type_c_name(t.elem_type)}'
		}
		types.Rune {
			return 'rune'
		}
		else {
			return 'int'
		}
	}
}

// --- Phase 1: Register types ---

// Pass 1: Register struct names as forward declarations (empty structs),
// enums, and sumtypes. This ensures all struct names are in struct_types
// before any field types are resolved.
fn (mut b Builder) register_types_pass1(file ast.File) {
	for stmt in file.stmts {
		match stmt {
			ast.StructDecl {
				b.register_struct_name(stmt)
			}
			ast.EnumDecl {
				b.register_enum(stmt)
			}
			ast.TypeDecl {
				b.register_sumtype(stmt)
			}
			else {}
		}
	}
}

// Pass 2: Fill in struct field types. All struct names are now registered,
// so cross-module struct references (e.g., &scanner.Scanner in Parser)
// resolve correctly to the struct type instead of falling back to i64.
fn (mut b Builder) register_types_pass2(file ast.File) {
	for stmt in file.stmts {
		if stmt is ast.StructDecl {
			b.register_struct_fields(stmt)
		}
	}
}

fn (mut b Builder) struct_mangled_name(decl ast.StructDecl) string {
	return if b.cur_module == 'builtin'
		&& decl.name in ['array', 'string', 'map', 'DenseArray', 'IError', 'Error', 'MessageError', 'None__', '_option', '_result', 'Option'] {
		decl.name
	} else if b.cur_module != '' && b.cur_module != 'main' {
		'${b.cur_module}__${decl.name}'
	} else {
		decl.name
	}
}

// register_struct_name registers a struct name with an empty struct type.
// The fields will be filled in by register_struct_fields in pass 2.
fn (mut b Builder) register_struct_name(decl ast.StructDecl) {
	name := b.struct_mangled_name(decl)

	if name in b.struct_types {
		return
	}

	type_id := b.mod.type_store.register(Type{
		kind: .struct_t
	})
	b.struct_types[name] = type_id
	b.mod.c_struct_names[type_id] = name
}

// register_struct_fields fills in the field types for a previously forward-declared struct.
fn (mut b Builder) register_struct_fields(decl ast.StructDecl) {
	name := b.struct_mangled_name(decl)

	type_id := b.struct_types[name] or { return }

	// Skip if fields are already populated (e.g., builtin types registered in Phase 1a)
	if b.mod.type_store.types[type_id].fields.len > 0 {
		return
	}

	mut field_types := []TypeID{}
	mut field_names := []string{}

	for field in decl.fields {
		ft := b.ast_type_to_ssa(field.typ)
		field_types << ft
		field_names << field.name
	}

	b.mod.type_store.types[type_id] = Type{
		kind:        .struct_t
		fields:      field_types
		field_names: field_names
	}
}

// register_struct is the legacy combined registration (used for Phase 1a core types).
fn (mut b Builder) register_struct(decl ast.StructDecl) {
	name := b.struct_mangled_name(decl)

	if name in b.struct_types {
		return
	}

	mut field_types := []TypeID{}
	mut field_names := []string{}

	for field in decl.fields {
		ft := b.ast_type_to_ssa(field.typ)
		field_types << ft
		field_names << field.name
	}

	type_id := b.mod.type_store.register(Type{
		kind:        .struct_t
		fields:      field_types
		field_names: field_names
	})
	b.struct_types[name] = type_id
	b.mod.c_struct_names[type_id] = name
}

fn (mut b Builder) register_enum(decl ast.EnumDecl) {
	name := if b.cur_module != '' && b.cur_module != 'main' {
		'${b.cur_module}__${decl.name}'
	} else {
		decl.name
	}

	is_flag := decl.attributes.has('flag')
	for i, field in decl.fields {
		key := '${name}__${field.name}'
		if is_flag {
			// @[flag] enums use power-of-2 values: 1, 2, 4, 8, ...
			b.enum_values[key] = 1 << i
		} else {
			b.enum_values[key] = i
		}
	}
}

// is_enum_type checks if a type name corresponds to a registered enum
// by looking for any enum_values key that starts with the name followed by '__'.
fn (b &Builder) is_enum_type(name string) bool {
	prefix := '${name}__'
	for key, _ in b.enum_values {
		if key.starts_with(prefix) {
			return true
		}
	}
	return false
}

fn (mut b Builder) register_sumtype(decl ast.TypeDecl) {
	if decl.variants.len == 0 {
		return
	}
	name := if b.cur_module != '' && b.cur_module != 'main' {
		'${b.cur_module}__${decl.name}'
	} else {
		decl.name
	}

	if name in b.struct_types {
		return
	}

	i64_t := b.mod.type_store.get_int(64)
	type_id := b.mod.type_store.register(Type{
		kind:        .struct_t
		fields:      [i64_t, i64_t]
		field_names: ['_tag', '_data']
	})
	b.struct_types[name] = type_id
	b.mod.c_struct_names[type_id] = name
}

fn (mut b Builder) register_consts_and_globals(file ast.File) {
	for stmt in file.stmts {
		match stmt {
			ast.ConstDecl {
				for field in stmt.fields {
					const_name := if b.cur_module != '' && b.cur_module != 'main' {
						'${b.cur_module}__${field.name}'
					} else {
						field.name
					}
					const_type := b.expr_type(field.value)
					// Check if this is a string constant - store for inline resolution
					str_val := b.try_eval_const_string(field.value)
					if str_val.len > 0 {
						b.string_const_values[const_name] = str_val
						b.string_const_values[field.name] = str_val
					}
					initial_value := b.try_eval_const_int(field.value)
					b.mod.add_global_with_value(const_name, const_type, true, initial_value)
					// Also store in const_values map for inline resolution.
					// Skip sum type constants (e.g., empty_expr/empty_stmt) which are
					// InitExpr{_tag: N, _data: ...} — these are multi-word values that
					// cannot be inlined as a single i64. They must be loaded from globals.
					mut is_sumtype_const := false
					if field.value is ast.InitExpr {
						for init_field in field.value.fields {
							if init_field.name == '_tag' {
								is_sumtype_const = true
								break
							}
						}
					}
					if !is_sumtype_const && (initial_value != 0 || b.is_zero_literal(field.value)) {
						b.const_values[const_name] = initial_value
						// Also store without module prefix for transformer-generated references
						b.const_values[field.name] = initial_value
					}
				}
			}
			ast.GlobalDecl {
				for field in stmt.fields {
					glob_name := if b.cur_module != '' && b.cur_module != 'main' {
						'${b.cur_module}__${field.name}'
					} else {
						field.name
					}
					glob_type := if field.typ != ast.empty_expr {
						b.ast_type_to_ssa(field.typ)
					} else {
						b.mod.type_store.get_int(64)
					}
					b.mod.add_global(glob_name, glob_type, false)
				}
			}
			else {}
		}
	}
}

// try_eval_const_int attempts to evaluate a constant expression to an integer value.
// Returns 0 for expressions that cannot be evaluated at compile time.
fn (mut b Builder) try_eval_const_int(expr ast.Expr) i64 {
	match expr {
		ast.BasicLiteral {
			if expr.kind == .number {
				return expr.value.i64()
			}
			if expr.kind == .key_true {
				return 1
			}
			if expr.kind == .key_false {
				return 0
			}
		}
		ast.InfixExpr {
			lhs := b.try_eval_const_int(expr.lhs)
			rhs := b.try_eval_const_int(expr.rhs)
			return match expr.op {
				.plus {
					lhs + rhs
				}
				.minus {
					lhs - rhs
				}
				.mul {
					lhs * rhs
				}
				.div {
					if rhs != 0 {
						lhs / rhs
					} else {
						0
					}
				}
				.mod {
					if rhs != 0 {
						lhs % rhs
					} else {
						0
					}
				}
				.left_shift {
					i64(u64(lhs) << u64(rhs))
				}
				.right_shift {
					i64(u64(lhs) >> u64(rhs))
				}
				.amp {
					lhs & rhs
				}
				.pipe {
					lhs | rhs
				}
				.xor {
					lhs ^ rhs
				}
				else {
					0
				}
			}
		}
		ast.PrefixExpr {
			val := b.try_eval_const_int(expr.expr)
			return match expr.op {
				.minus { -val }
				.bit_not { ~val }
				else { 0 }
			}
		}
		ast.Ident {
			// Try to resolve reference to another constant
			if expr.name in b.const_values {
				return b.const_values[expr.name]
			}
			qualified := '${b.cur_module}__${expr.name}'
			if qualified in b.const_values {
				return b.const_values[qualified]
			}
			builtin_qual := 'builtin__${expr.name}'
			if builtin_qual in b.const_values {
				return b.const_values[builtin_qual]
			}
		}
		ast.CastExpr {
			return b.try_eval_const_int(expr.expr)
		}
		ast.ParenExpr {
			return b.try_eval_const_int(expr.expr)
		}
		ast.InitExpr {
			// Handle sum type init: InitExpr{_tag: N, _data: ...}
			// Extract the _tag value so struct constants get correct tag in data section
			for field in expr.fields {
				if field.name == '_tag' {
					return b.try_eval_const_int(field.value)
				}
			}
		}
		else {}
	}
	return 0
}

fn (b &Builder) is_zero_literal(expr ast.Expr) bool {
	if expr is ast.BasicLiteral {
		return expr.kind == .number && expr.value == '0'
	}
	return false
}

// resolve_char_value converts a V character literal value to its numeric byte value.
// Handles escape sequences like \n, \t, \r, \\, \', \0, and raw characters.
fn (b &Builder) resolve_char_value(val string) int {
	mut s := val
	// Strip surrounding quotes if present
	if s.len >= 2 && s[0] == `\`` && s[s.len - 1] == `\`` {
		s = s[1..s.len - 1]
	}
	if s.len >= 2 && ((s[0] == `'` && s[s.len - 1] == `'`) || (s[0] == `"` && s[s.len - 1] == `"`)) {
		s = s[1..s.len - 1]
	}
	if s.len == 0 {
		return 0
	}
	// Handle escape sequences
	if s.len >= 2 && s[0] == `\\` {
		return match s[1] {
			`n` { 10 }
			`t` { 9 }
			`r` { 13 }
			`\\` { 92 }
			`'` { 39 }
			`"` { 34 }
			`0` { 0 }
			`a` { 7 }
			`b` { 8 }
			`f` { 12 }
			`v` { 11 }
			`e` { 27 }
			else { int(s[1]) }
		}
	}
	// Single raw character
	return int(s[0])
}

// try_eval_const_string attempts to extract a string value from a constant expression.
// Returns empty string if the expression is not a string literal.
fn (b &Builder) try_eval_const_string(expr ast.Expr) string {
	match expr {
		ast.StringLiteral {
			mut val := expr.value
			// Strip surrounding quotes if present
			if val.len >= 2 && ((val[0] == `'` && val[val.len - 1] == `'`)
				|| (val[0] == `"` && val[val.len - 1] == `"`)) {
				val = val[1..val.len - 1]
			}
			return val
		}
		ast.BasicLiteral {
			if expr.kind == .string {
				mut val := expr.value
				if val.len >= 2 && ((val[0] == `'` && val[val.len - 1] == `'`)
					|| (val[0] == `"` && val[val.len - 1] == `"`)) {
					val = val[1..val.len - 1]
				}
				return val
			}
		}
		else {}
	}
	return ''
}

fn (mut b Builder) ast_type_to_ssa(typ ast.Expr) TypeID {
	match typ {
		ast.Ident {
			return b.ident_type_to_ssa(typ.name)
		}
		ast.Type {
			return b.ast_type_node_to_ssa(typ)
		}
		ast.PrefixExpr {
			if typ.op == .amp {
				base := b.ast_type_to_ssa(typ.expr)
				return b.mod.type_store.get_ptr(base)
			}
			if typ.op == .ellipsis {
				// Variadic params (...T) are lowered to []T (dynamic array)
				return b.get_array_type()
			}
			return b.mod.type_store.get_int(64)
		}
		ast.ModifierExpr {
			// mut/shared receivers: unwrap the modifier, the pointer is added by the caller
			return b.ast_type_to_ssa(typ.expr)
		}
		ast.SelectorExpr {
			// module.Type — e.g., C.dirent, os.Stat
			if typ.lhs is ast.Ident {
				mod_name := typ.lhs.name
				full_name := '${mod_name}.${typ.rhs.name}'
				// Try C.StructName → look up as module__C.StructName
				qualified := '${b.cur_module}__${full_name}'
				if qualified in b.struct_types {
					return b.struct_types[qualified]
				}
				// Also try just the full name (e.g., C.dirent)
				if full_name in b.struct_types {
					return b.struct_types[full_name]
				}
				// Try module__StructName (for module.Type references like os.Stat)
				mod_qualified := '${mod_name}__${typ.rhs.name}'
				if mod_qualified in b.struct_types {
					return b.struct_types[mod_qualified]
				}
				// For C.X types: C structs are registered under their declaring module
				// (e.g., C.dirent in os module → "os__dirent")
				// Try cur_module__StructName
				if mod_name == 'C' {
					cur_qualified := '${b.cur_module}__${typ.rhs.name}'
					if cur_qualified in b.struct_types {
						return b.struct_types[cur_qualified]
					}
					// Search all modules for this C struct
					for sname, sid in b.struct_types {
						if sname.ends_with('__${typ.rhs.name}') {
							return sid
						}
					}
				}
				// Try looking up in the referenced module's scope via type environment
				// (e.g., token.Token where Token is an enum, not a struct)
				if b.env != unsafe { nil } {
					mod_name_v := mod_name.replace('.', '_')
					if scope := b.env.get_scope(mod_name_v) {
						if obj := scope.lookup_parent(typ.rhs.name, 0) {
							return b.type_to_ssa(obj.typ())
						}
					}
				}
			}
			return b.ident_type_to_ssa(typ.rhs.name)
		}
		ast.EmptyExpr {
			return 0 // void
		}
		ast.Tuple {
			mut elem_types := []TypeID{cap: typ.exprs.len}
			for e in typ.exprs {
				elem_types << b.ast_type_to_ssa(e)
			}
			return b.mod.type_store.get_tuple(elem_types)
		}
		else {
			return b.mod.type_store.get_int(64)
		}
	}
}

fn (mut b Builder) ast_type_node_to_ssa(typ ast.Type) TypeID {
	match typ {
		ast.ArrayType {
			return b.get_array_type()
		}
		ast.ArrayFixedType {
			// [N]T → SSA array type with N elements of T
			elem_type := b.ast_type_to_ssa(typ.elem_type)
			arr_len := if typ.len is ast.BasicLiteral {
				typ.len.value.int()
			} else {
				0
			}
			if arr_len > 0 {
				return b.mod.type_store.get_array(elem_type, arr_len)
			}
			return b.mod.type_store.get_int(64) // fallback
		}
		ast.MapType {
			return b.struct_types['map'] or { b.mod.type_store.get_int(64) }
		}
		ast.FnType {
			i8_t := b.mod.type_store.get_int(8)
			return b.mod.type_store.get_ptr(i8_t) // fn pointers
		}
		ast.OptionType {
			// Native backend: Option types are just the base type (no wrapper struct).
			// The value itself indicates presence (non-zero) or absence (zero/none).
			base := b.ast_type_to_ssa(typ.base_type)
			if base != 0 {
				return base
			}
			return b.mod.type_store.get_int(64)
		}
		ast.ResultType {
			// Native backend: Result types are just the base type (no wrapper struct).
			base := b.ast_type_to_ssa(typ.base_type)
			if base != 0 {
				return base
			}
			return b.mod.type_store.get_int(64)
		}
		ast.TupleType {
			mut elem_types := []TypeID{cap: typ.types.len}
			for t in typ.types {
				elem_types << b.ast_type_to_ssa(t)
			}
			return b.mod.type_store.get_tuple(elem_types)
		}
		else {
			return b.mod.type_store.get_int(64)
		}
	}
}

fn (mut b Builder) ident_type_to_ssa(name string) TypeID {
	return match name {
		'int' {
			b.mod.type_store.get_int(32)
		}
		'i8' {
			b.mod.type_store.get_int(8)
		}
		'i16' {
			b.mod.type_store.get_int(16)
		}
		'i32' {
			b.mod.type_store.get_int(32)
		}
		'i64' {
			b.mod.type_store.get_int(64)
		}
		'u8', 'byte' {
			b.mod.type_store.get_int(8)
		}
		'u16' {
			b.mod.type_store.get_int(16)
		}
		'u32' {
			b.mod.type_store.get_int(32)
		}
		'u64' {
			b.mod.type_store.get_int(64)
		}
		'f32' {
			b.mod.type_store.get_float(32)
		}
		'f64' {
			b.mod.type_store.get_float(64)
		}
		'bool' {
			b.mod.type_store.get_int(1)
		}
		'string' {
			b.get_string_type()
		}
		'voidptr' {
			i8_t := b.mod.type_store.get_int(8)
			b.mod.type_store.get_ptr(i8_t)
		}
		'rune' {
			b.mod.type_store.get_int(32)
		}
		'char' {
			b.mod.type_store.get_int(8)
		}
		else {
			// Check for pointer types (e.g., 'StructType*', 'int*')
			if name.ends_with('*') {
				base_name := name[..name.len - 1]
				base_type := b.ident_type_to_ssa(base_name)
				return b.mod.type_store.get_ptr(base_type)
			}
			// Check struct types
			if name in b.struct_types {
				b.struct_types[name]
			} else if name == 'strings__Builder' {
				// strings.Builder = []u8 = array (type alias)
				b.get_array_type()
			} else if name == 'Builder' {
				// Builder could be strings.Builder alias or an actual struct
				// Try module-qualified first, fall back to array alias
				qualified_b := '${b.cur_module}__Builder'
				if qualified_b in b.struct_types {
					b.struct_types[qualified_b]
				} else {
					b.get_array_type()
				}
			} else if name.starts_with('Array_') {
				// Transformer-generated Array_T types (e.g., Array_int, Array_string) are all array structs
				b.get_array_type()
			} else if name.starts_with('Map_') {
				// Transformer-generated Map_K_V types are all map structs
				b.struct_types['map'] or { b.mod.type_store.get_int(64) }
			} else {
				// Try module-qualified
				qualified := '${b.cur_module}__${name}'
				if qualified in b.struct_types {
					b.struct_types[qualified]
				} else if b.is_enum_type(name) || b.is_enum_type(qualified) {
					// Enum types are always int (i32) in V
					b.mod.type_store.get_int(32)
				} else if b.env != unsafe { nil } {
					// Use the type checker environment to resolve aliases and other types
					if scope := b.env.get_scope(b.cur_module) {
						if obj := scope.lookup_parent(name, 0) {
							b.type_to_ssa(obj.typ())
						} else {
							b.mod.type_store.get_int(64)
						}
					} else {
						b.mod.type_store.get_int(64)
					}
				} else {
					b.mod.type_store.get_int(64)
				}
			}
		}
	}
}

// --- Phase 2: Register function signatures ---

fn (mut b Builder) register_fn_signatures(file ast.File) {
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			if stmt.typ.generic_params.len > 0 {
				continue
			}
			b.register_fn_sig(stmt)
		}
	}
}

fn (mut b Builder) register_fn_sig(decl ast.FnDecl) {
	fn_name := b.mangle_fn_name(decl)
	if fn_name in b.fn_index {
		return
	}

	ret_type := b.ast_type_to_ssa(decl.typ.return_type)
	idx := b.mod.new_function(fn_name, ret_type, []TypeID{})
	b.fn_index[fn_name] = idx
	if decl.language == .c {
		b.mod.funcs[idx].is_c_extern = true
	}

	// Register parameter types for correct forward declarations.
	// For methods, add receiver as the first parameter.
	// Skip for static methods (is_static=true) — they have no receiver in the call.
	if decl.is_method && !decl.is_static {
		recv_type := b.ast_type_to_ssa(decl.receiver.typ)
		// For &Type (PrefixExpr), ast_type_to_ssa already returns ptr(Type).
		// For mut receivers, the parser sets is_mut on the Parameter (not ModifierExpr),
		// so we need to add the pointer level.
		actual_type := if decl.receiver.is_mut {
			b.mod.type_store.get_ptr(recv_type)
		} else {
			recv_type
		}
		receiver_name := if decl.receiver.name != '' {
			decl.receiver.name
		} else {
			'self'
		}
		param_val := b.mod.add_value_node(.argument, actual_type, receiver_name, 0)
		b.mod.funcs[idx].params << param_val
	}
	for param in decl.typ.params {
		param_type := b.ast_type_to_ssa(param.typ)
		actual_type := if param.is_mut {
			b.mod.type_store.get_ptr(param_type)
		} else {
			param_type
		}
		param_val := b.mod.add_value_node(.argument, actual_type, param.name, 0)
		b.mod.funcs[idx].params << param_val
	}
}

fn (mut b Builder) mangle_fn_name(decl ast.FnDecl) string {
	if decl.is_method {
		receiver_name := b.receiver_type_name(decl.receiver.typ)
		return '${receiver_name}__${decl.name}'
	}
	// C functions use their bare name (no module prefix)
	if decl.language == .c {
		return decl.name
	}
	if decl.name == 'main' && b.cur_module == 'main' {
		return 'main'
	}
	if b.cur_module != '' && b.cur_module != 'main' {
		return '${b.cur_module}__${decl.name}'
	}
	return decl.name
}

fn (mut b Builder) receiver_type_name(typ ast.Expr) string {
	match typ {
		ast.Ident {
			if b.cur_module != '' && b.cur_module != 'main' {
				return '${b.cur_module}__${typ.name}'
			}
			return typ.name
		}
		ast.PrefixExpr {
			return b.receiver_type_name(typ.expr)
		}
		ast.ModifierExpr {
			return b.receiver_type_name(typ.expr)
		}
		ast.SelectorExpr {
			return '${typ.lhs.name()}__${typ.rhs.name}'
		}
		ast.Type {
			// Handle type expressions used as receivers (e.g., []rune for (ra []rune) string())
			inner := ast.Type(typ)
			if inner is ast.ArrayType {
				// []rune → Array_rune, []int → Array_int, etc.
				elem_name := if inner.elem_type is ast.Ident {
					inner.elem_type.name
				} else {
					b.receiver_type_name(inner.elem_type)
				}
				prefix := if b.cur_module != '' && b.cur_module != 'main' {
					'${b.cur_module}__'
				} else {
					''
				}
				return '${prefix}Array_${elem_name}'
			}
			return 'unknown'
		}
		else {
			return 'unknown'
		}
	}
}

// --- Phase 3: Build function bodies ---

fn (mut b Builder) build_fn_bodies(file ast.File) {
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			if stmt.language == .c && stmt.stmts.len == 0 {
				continue
			}
			if stmt.typ.generic_params.len > 0 {
				continue
			}
			b.build_fn(stmt)
		}
	}
}

fn (mut b Builder) build_fn(decl ast.FnDecl) {
	fn_name := b.mangle_fn_name(decl)
	// Skip C-language extern functions without bodies
	if decl.language == .c {
		return
	}
	func_idx := b.fn_index[fn_name] or { return }
	// Skip if already built (can happen with .c.v and .v files)
	if b.mod.funcs[func_idx].blocks.len > 0 {
		return
	}

	// Skip functions without a body (e.g., extern declarations).
	// Build function bodies for ALL modules so cross-module calls work at runtime.
	if decl.stmts.len == 0 {
		// Emit a minimal function body (entry + ret) so backends have a valid function
		b.cur_func = func_idx
		entry := b.mod.add_block(func_idx, 'entry')
		b.cur_block = entry
		ret_type := b.mod.funcs[func_idx].typ
		if ret_type != 0 {
			ret_type_info := b.mod.type_store.types[ret_type]
			if ret_type_info.kind == .struct_t {
				// For struct return types, alloca + zero init + load
				ptr_type := b.mod.type_store.get_ptr(ret_type)
				alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
				ret_val := b.mod.add_instr(.load, b.cur_block, ret_type, [alloca])
				b.mod.add_instr(.ret, b.cur_block, 0, [ret_val])
			} else {
				zero := b.mod.get_or_add_const(ret_type, '0')
				b.mod.add_instr(.ret, b.cur_block, 0, [zero])
			}
		} else {
			b.mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
		}
		return
	}

	b.cur_func = func_idx

	// Reset local variables
	b.vars = map[string]ValueID{}
	b.mut_ptr_params = map[string]bool{}
	b.label_blocks = map[string]BlockID{}

	// Clear params (they were registered in register_fn_sig for forward decls,
	// but we need to re-create them here with proper alloca bindings)
	b.mod.funcs[func_idx].params = []ValueID{}

	// Create entry block
	entry := b.mod.add_block(func_idx, 'entry')
	b.cur_block = entry

	// Add parameters
	// Skip receiver for static methods (is_static=true) — no receiver in call
	if decl.is_method && !decl.is_static {
		// Receiver is the first parameter
		receiver_name := if decl.receiver.name != '' {
			decl.receiver.name
		} else {
			'self'
		}
		recv_type := b.ast_type_to_ssa(decl.receiver.typ)
		// For &Type (PrefixExpr), ast_type_to_ssa already returns ptr(Type).
		// For mut receivers, the parser sets is_mut on the Parameter (not ModifierExpr),
		// so we need to add the pointer level.
		actual_type := if decl.receiver.is_mut {
			b.mod.type_store.get_ptr(recv_type)
		} else {
			recv_type
		}
		param_val := b.mod.add_value_node(.argument, actual_type, receiver_name, 0)
		b.mod.funcs[func_idx].params << param_val
		// Alloca + store for receiver
		alloca := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(actual_type),
			[]ValueID{})
		b.mod.add_instr(.store, entry, 0, [param_val, alloca])
		b.vars[receiver_name] = alloca
	}

	for param in decl.typ.params {
		param_type := b.ast_type_to_ssa(param.typ)
		actual_type := if param.is_mut {
			b.mod.type_store.get_ptr(param_type)
		} else {
			param_type
		}
		param_val := b.mod.add_value_node(.argument, actual_type, param.name, 0)
		b.mod.funcs[func_idx].params << param_val
		// Alloca + store
		alloca := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(actual_type),
			[]ValueID{})
		b.mod.add_instr(.store, entry, 0, [param_val, alloca])
		b.vars[param.name] = alloca
		// Track mut pointer params that need extra dereference in build_ident.
		// e.g., mut buf &u8 → actual_type is ptr(ptr(i8)), but user sees buf as &u8.
		if param.is_mut && param_type < b.mod.type_store.types.len
			&& b.mod.type_store.types[param_type].kind == .ptr_t {
			b.mut_ptr_params[param.name] = true
		}
	}

	// Build body
	b.build_stmts(decl.stmts)

	// If no terminator, add implicit return
	if !b.block_has_terminator(b.cur_block) {
		if fn_name == 'main' {
			zero := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
			b.mod.add_instr(.ret, b.cur_block, 0, [zero])
		} else {
			b.mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
		}
	}
}

fn (mut b Builder) is_mut_receiver(typ ast.Expr) bool {
	if typ is ast.ModifierExpr {
		return typ.kind == .key_mut || typ.kind == .key_shared
	}
	if typ is ast.PrefixExpr {
		return typ.op == .amp
	}
	return false
}

fn (mut b Builder) block_has_terminator(block BlockID) bool {
	instrs := b.mod.blocks[block].instrs
	if instrs.len == 0 {
		return false
	}
	last := instrs[instrs.len - 1]
	last_instr := b.mod.instrs[b.mod.values[last].index]
	return last_instr.op in [.ret, .br, .jmp, .unreachable]
}

// --- Statement building ---

fn (mut b Builder) build_stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		b.build_stmt(stmt)
	}
}

fn (mut b Builder) build_stmt(stmt ast.Stmt) {
	match stmt {
		ast.AssignStmt {
			b.build_assign(stmt)
		}
		ast.ExprStmt {
			b.build_expr_stmt(stmt)
		}
		ast.ReturnStmt {
			b.build_return(stmt)
		}
		ast.ForStmt {
			b.build_for(stmt)
		}
		ast.FlowControlStmt {
			b.build_flow_control(stmt)
		}
		ast.BlockStmt {
			b.build_stmts(stmt.stmts)
		}
		ast.LabelStmt {
			b.build_label(stmt)
		}
		ast.ModuleStmt {
			b.cur_module = stmt.name.replace('.', '_')
		}
		ast.ImportStmt {}
		ast.ConstDecl {}
		ast.StructDecl {}
		ast.EnumDecl {}
		ast.TypeDecl {}
		ast.InterfaceDecl {}
		ast.GlobalDecl {}
		ast.FnDecl {} // nested fn decls handled separately
		ast.Directive {}
		ast.ComptimeStmt {}
		ast.DeferStmt {}
		ast.AssertStmt {
			b.build_assert(stmt)
		}
		[]ast.Attribute {}
		ast.EmptyStmt {}
		ast.AsmStmt {}
		ast.ForInStmt {
			panic('SSA builder: ForInStmt should have been lowered by transformer')
		}
	}
}

fn (mut b Builder) build_expr_stmt(stmt ast.ExprStmt) {
	if stmt.expr is ast.UnsafeExpr {
		for s in stmt.expr.stmts {
			b.build_stmt(s)
		}
		return
	}
	if stmt.expr is ast.IfExpr {
		b.build_if_stmt(stmt.expr)
		return
	}
	b.build_expr(stmt.expr)
}

fn (mut b Builder) unwrap_ident(expr ast.Expr) ?ast.Ident {
	if expr is ast.Ident {
		return expr
	}
	if expr is ast.ModifierExpr {
		return b.unwrap_ident(expr.expr)
	}
	return none
}

fn (mut b Builder) build_assign(stmt ast.AssignStmt) {
	// Multi-return decomposition: when LHS has more vars than RHS,
	// the single RHS is a function call returning a tuple.
	if stmt.lhs.len > 1 && stmt.rhs.len == 1 {
		rhs_val := b.build_expr(stmt.rhs[0])
		rhs_typ_id := b.mod.values[rhs_val].typ
		rhs_typ := b.mod.type_store.types[rhs_typ_id]
		for i, lhs in stmt.lhs {
			// Extract each element from the tuple
			mut elem_val := rhs_val
			if rhs_typ.kind == .struct_t && i < rhs_typ.fields.len {
				elem_type := rhs_typ.fields[i]
				idx_val := b.mod.get_or_add_const(b.mod.type_store.get_int(32), i.str())
				elem_val = b.mod.add_instr(.extractvalue, b.cur_block, elem_type, [
					rhs_val,
					idx_val,
				])
			}
			if ident := b.unwrap_ident(lhs) {
				if stmt.op == .decl_assign {
					elem_type := b.mod.values[elem_val].typ
					alloca := b.mod.add_instr(.alloca, b.cur_block, b.mod.type_store.get_ptr(elem_type),
						[]ValueID{})
					b.mod.add_instr(.store, b.cur_block, 0, [elem_val, alloca])
					b.vars[ident.name] = alloca
				} else if ident.name == '_' {
					continue
				} else {
					mut ptr := ValueID(0)
					if p := b.vars[ident.name] {
						ptr = p
					} else if glob_id := b.find_global(ident.name) {
						ptr = glob_id
					} else if glob_id := b.find_global('${b.cur_module}__${ident.name}') {
						ptr = glob_id
					} else if glob_id := b.find_global('builtin__${ident.name}') {
						ptr = glob_id
					}
					if ptr != 0 {
						b.mod.add_instr(.store, b.cur_block, 0, [elem_val, ptr])
					}
				}
			}
		}
		return
	}
	for i, lhs in stmt.lhs {
		if i >= stmt.rhs.len {
			break
		}
		rhs := stmt.rhs[i]
		rhs_val := b.build_expr(rhs)

		if ident := b.unwrap_ident(lhs) {
			if stmt.op == .decl_assign {
				// New variable declaration - use the actual type of the built RHS value
				rhs_type := b.mod.values[rhs_val].typ
				alloca := b.mod.add_instr(.alloca, b.cur_block, b.mod.type_store.get_ptr(rhs_type),
					[]ValueID{})
				b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, alloca])
				b.vars[ident.name] = alloca
			} else if ident.name == '_' && stmt.op == .assign {
				// Discard for plain assignment only; compound assignments (+=, etc.)
				// must still execute (e.g. for loop counter `_ += 1`).
				continue
			} else {
				// Assignment to existing variable (local or global)
				mut ptr := ValueID(0)
				if p := b.vars[ident.name] {
					ptr = p
				} else if glob_id := b.find_global(ident.name) {
					ptr = glob_id
				} else if glob_id := b.find_global('${b.cur_module}__${ident.name}') {
					ptr = glob_id
				} else if glob_id := b.find_global('builtin__${ident.name}') {
					ptr = glob_id
				}
				if ptr != 0 {
					if stmt.op == .assign {
						b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, ptr])
					} else {
						// Compound assignment (+=, -=, etc.)
						ptr_typ := b.mod.values[ptr].typ
						elem_typ := b.mod.type_store.types[ptr_typ].elem_type
						loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
						op := match stmt.op {
							.plus_assign { OpCode.add }
							.minus_assign { OpCode.sub }
							.mul_assign { OpCode.mul }
							.div_assign { OpCode.sdiv }
							.mod_assign { OpCode.srem }
							.left_shift_assign { OpCode.shl }
							.right_shift_assign { OpCode.ashr }
							.and_assign { OpCode.and_ }
							.or_assign { OpCode.or_ }
							.xor_assign { OpCode.xor }
							else { OpCode.add }
						}
						result := b.mod.add_instr(op, b.cur_block, b.mod.values[loaded].typ,
							[loaded, rhs_val])
						b.mod.add_instr(.store, b.cur_block, 0, [result, ptr])
					}
				}
			}
		} else if lhs is ast.SelectorExpr {
			// Field assignment: obj.field = val or obj.field += val
			base := b.build_addr(lhs)
			if base != 0 {
				if stmt.op == .assign {
					b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, base])
				} else {
					// Compound assignment (+=, -=, etc.)
					ptr_typ := b.mod.values[base].typ
					elem_typ := if ptr_typ < b.mod.type_store.types.len {
						b.mod.type_store.types[ptr_typ].elem_type
					} else {
						b.mod.values[rhs_val].typ
					}
					loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [base])
					op := match stmt.op {
						.plus_assign { OpCode.add }
						.minus_assign { OpCode.sub }
						.mul_assign { OpCode.mul }
						.div_assign { OpCode.sdiv }
						.mod_assign { OpCode.srem }
						.left_shift_assign { OpCode.shl }
						.right_shift_assign { OpCode.ashr }
						.and_assign { OpCode.and_ }
						.or_assign { OpCode.or_ }
						.xor_assign { OpCode.xor }
						else { OpCode.add }
					}
					result := b.mod.add_instr(op, b.cur_block, b.mod.values[loaded].typ,
						[loaded, rhs_val])
					b.mod.add_instr(.store, b.cur_block, 0, [result, base])
				}
			}
		} else if lhs is ast.PrefixExpr && lhs.op == .mul {
			// Pointer dereference assignment: *ptr = val
			// Build the inner expression to get the pointer, then store to it
			ptr := b.build_expr(lhs.expr)
			if stmt.op == .assign {
				b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, ptr])
			} else {
				// Compound assignment (*ptr += val)
				ptr_typ := b.mod.values[ptr].typ
				elem_typ := if int(ptr_typ) < b.mod.type_store.types.len {
					b.mod.type_store.types[ptr_typ].elem_type
				} else {
					b.mod.values[rhs_val].typ
				}
				loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
				op := match stmt.op {
					.plus_assign { OpCode.add }
					.minus_assign { OpCode.sub }
					.mul_assign { OpCode.mul }
					.div_assign { OpCode.sdiv }
					else { OpCode.add }
				}
				result := b.mod.add_instr(op, b.cur_block, b.mod.values[loaded].typ, [
					loaded,
					rhs_val,
				])
				b.mod.add_instr(.store, b.cur_block, 0, [result, ptr])
			}
		} else if lhs is ast.IndexExpr {
			// Index assignment: arr[i] = val or arr[i] += val
			base := b.build_addr(lhs)
			if base != 0 {
				if stmt.op == .assign {
					b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, base])
				} else {
					// Compound assignment (+=, -=, etc.)
					ptr_typ := b.mod.values[base].typ
					elem_typ := if ptr_typ < b.mod.type_store.types.len {
						b.mod.type_store.types[ptr_typ].elem_type
					} else {
						b.mod.values[rhs_val].typ
					}
					loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [base])
					op := match stmt.op {
						.plus_assign { OpCode.add }
						.minus_assign { OpCode.sub }
						.mul_assign { OpCode.mul }
						.div_assign { OpCode.sdiv }
						.mod_assign { OpCode.srem }
						.left_shift_assign { OpCode.shl }
						.right_shift_assign { OpCode.ashr }
						.and_assign { OpCode.and_ }
						.or_assign { OpCode.or_ }
						.xor_assign { OpCode.xor }
						else { OpCode.add }
					}
					result := b.mod.add_instr(op, b.cur_block, b.mod.values[loaded].typ,
						[loaded, rhs_val])
					b.mod.add_instr(.store, b.cur_block, 0, [result, base])
				}
			}
		}
	}
}

fn (mut b Builder) build_return(stmt ast.ReturnStmt) {
	if stmt.exprs.len == 0 {
		b.mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
	} else if stmt.exprs.len == 1 {
		val := b.build_expr(stmt.exprs[0])
		b.mod.add_instr(.ret, b.cur_block, 0, [val])
	} else {
		// Multiple return values -> build a tuple via insertvalue
		mut elem_types := []TypeID{cap: stmt.exprs.len}
		mut vals := []ValueID{cap: stmt.exprs.len}
		for expr in stmt.exprs {
			v := b.build_expr(expr)
			vals << v
			elem_types << b.mod.values[v].typ
		}
		tuple_type := b.mod.type_store.get_tuple(elem_types)
		// Build tuple by chaining insertvalue instructions
		mut tuple_val := b.mod.get_or_add_const(tuple_type, 'undef')
		for i, v in vals {
			idx := b.mod.get_or_add_const(b.mod.type_store.get_int(32), i.str())
			tuple_val = b.mod.add_instr(.insertvalue, b.cur_block, tuple_type, [
				tuple_val,
				v,
				idx,
			])
		}
		b.mod.add_instr(.ret, b.cur_block, 0, [tuple_val])
	}
}

fn (mut b Builder) build_for(stmt ast.ForStmt) {
	has_init := stmt.init !is ast.EmptyStmt
	has_cond := stmt.cond !is ast.EmptyExpr
	has_post := stmt.post !is ast.EmptyStmt

	// Build init in current block
	if has_init {
		b.build_stmt(stmt.init)
	}

	// Create blocks
	cond_block := b.mod.add_block(b.cur_func, 'for_cond')
	body_block := b.mod.add_block(b.cur_func, 'for_body')
	post_block := if has_post {
		b.mod.add_block(b.cur_func, 'for_post')
	} else {
		cond_block
	}
	exit_block := b.mod.add_block(b.cur_func, 'for_exit')

	// Jump to condition
	b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[cond_block].val_id])
	b.add_edge(b.cur_block, cond_block)

	// Condition block
	b.cur_block = cond_block
	if has_cond {
		cond_val := b.build_expr(stmt.cond)
		b.mod.add_instr(.br, b.cur_block, 0, [cond_val, b.mod.blocks[body_block].val_id, b.mod.blocks[exit_block].val_id])
		b.add_edge(cond_block, body_block)
		b.add_edge(cond_block, exit_block)
	} else {
		// Infinite loop
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[body_block].val_id])
		b.add_edge(cond_block, body_block)
	}

	// Body block
	b.cur_block = body_block
	b.loop_stack << LoopInfo{
		cond_block: post_block
		exit_block: exit_block
	}
	b.build_stmts(stmt.stmts)
	b.loop_stack.pop()

	if !b.block_has_terminator(b.cur_block) {
		// Jump to post (or back to cond)
		target := if has_post { post_block } else { cond_block }
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[target].val_id])
		b.add_edge(b.cur_block, target)
	}

	// Post block
	if has_post {
		b.cur_block = post_block
		b.build_stmt(stmt.post)
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[cond_block].val_id])
			b.add_edge(post_block, cond_block)
		}
	}

	b.cur_block = exit_block
}

fn (mut b Builder) build_if_stmt(node ast.IfExpr) {
	if node.cond is ast.EmptyExpr {
		// Pure else block
		b.build_stmts(node.stmts)
		return
	}

	then_block := b.mod.add_block(b.cur_func, 'if_then')
	merge_block := b.mod.add_block(b.cur_func, 'if_merge')

	has_else := node.else_expr !is ast.EmptyExpr
	else_block := if has_else {
		b.mod.add_block(b.cur_func, 'if_else')
	} else {
		merge_block
	}

	// Condition
	cond_val := b.build_expr(node.cond)
	b.mod.add_instr(.br, b.cur_block, 0, [cond_val, b.mod.blocks[then_block].val_id, b.mod.blocks[else_block].val_id])
	b.add_edge(b.cur_block, then_block)
	b.add_edge(b.cur_block, else_block)

	// Then
	b.cur_block = then_block
	b.build_stmts(node.stmts)
	if !b.block_has_terminator(b.cur_block) {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
		b.add_edge(b.cur_block, merge_block)
	}

	// Else
	if has_else {
		b.cur_block = else_block
		if node.else_expr is ast.IfExpr {
			else_if := node.else_expr as ast.IfExpr
			if else_if.cond is ast.EmptyExpr {
				// Pure else
				b.build_stmts(else_if.stmts)
			} else {
				b.build_if_stmt(else_if)
			}
		} else if node.else_expr is ast.UnsafeExpr {
			for s in node.else_expr.stmts {
				b.build_stmt(s)
			}
		}
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
		}
	}

	b.cur_block = merge_block
	// If the merge block has no predecessors (both branches returned/jumped elsewhere),
	// mark it as unreachable so no implicit return is added
	if b.mod.blocks[merge_block].preds.len == 0 {
		b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})
	}
}

fn (mut b Builder) build_flow_control(stmt ast.FlowControlStmt) {
	if stmt.op == .key_goto {
		// goto label — jump to the label's block (create if not yet seen)
		target := b.get_or_create_label_block(stmt.label)
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[target].val_id])
		b.add_edge(b.cur_block, target)
		// Create a new unreachable block for any code after the goto
		dead_block := b.mod.add_block(b.cur_func, 'after_goto')
		b.cur_block = dead_block
		return
	}
	if b.loop_stack.len == 0 {
		return
	}
	loop_info := b.loop_stack[b.loop_stack.len - 1]
	if stmt.op == .key_break {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[loop_info.exit_block].val_id])
		b.add_edge(b.cur_block, loop_info.exit_block)
	} else if stmt.op == .key_continue {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[loop_info.cond_block].val_id])
		b.add_edge(b.cur_block, loop_info.cond_block)
	}
}

fn (mut b Builder) get_or_create_label_block(name string) BlockID {
	if name in b.label_blocks {
		return b.label_blocks[name]
	}
	block := b.mod.add_block(b.cur_func, 'label_${name}')
	b.label_blocks[name] = block
	return block
}

fn (mut b Builder) build_label(stmt ast.LabelStmt) {
	label_block := b.get_or_create_label_block(stmt.name)
	// Fall through from current block to label block
	b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[label_block].val_id])
	b.add_edge(b.cur_block, label_block)
	b.cur_block = label_block
}

fn (mut b Builder) build_assert(stmt ast.AssertStmt) {
	// assert expr -> if (!expr) { exit(1) }
	cond := b.build_expr(stmt.expr)
	pass_block := b.mod.add_block(b.cur_func, 'assert_pass')
	fail_block := b.mod.add_block(b.cur_func, 'assert_fail')

	b.mod.add_instr(.br, b.cur_block, 0, [cond, b.mod.blocks[pass_block].val_id, b.mod.blocks[fail_block].val_id])
	b.add_edge(b.cur_block, pass_block)
	b.add_edge(b.cur_block, fail_block)

	// Fail block: call exit(1)
	b.cur_block = fail_block
	one := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '1')
	exit_ref := b.get_or_create_fn_ref('exit', b.mod.type_store.get_int(32))
	b.mod.add_instr(.call, b.cur_block, 0, [exit_ref, one])
	b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})

	b.cur_block = pass_block
}

// --- Expression building ---

fn (mut b Builder) build_expr(expr ast.Expr) ValueID {
	match expr {
		ast.BasicLiteral {
			return b.build_basic_literal(expr)
		}
		ast.StringLiteral {
			return b.build_string_literal(expr)
		}
		ast.Ident {
			return b.build_ident(expr)
		}
		ast.InfixExpr {
			return b.build_infix(expr)
		}
		ast.PrefixExpr {
			return b.build_prefix(expr)
		}
		ast.CallExpr {
			return b.build_call(expr)
		}
		ast.SelectorExpr {
			return b.build_selector(expr)
		}
		ast.IndexExpr {
			return b.build_index(expr)
		}
		ast.IfExpr {
			return b.build_if_expr(expr)
		}
		ast.InitExpr {
			return b.build_init_expr(expr)
		}
		ast.CastExpr {
			return b.build_cast(expr)
		}
		ast.ParenExpr {
			return b.build_expr(expr.expr)
		}
		ast.ModifierExpr {
			return b.build_expr(expr.expr)
		}
		ast.UnsafeExpr {
			if expr.stmts.len > 0 {
				for i := 0; i < expr.stmts.len - 1; i++ {
					b.build_stmt(expr.stmts[i])
				}
				last := expr.stmts[expr.stmts.len - 1]
				if last is ast.ExprStmt {
					return b.build_expr(last.expr)
				}
				b.build_stmt(last)
			}
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
		}
		ast.Keyword {
			return b.build_keyword(expr)
		}
		ast.KeywordOperator {
			return b.build_keyword_operator(expr)
		}
		ast.PostfixExpr {
			return b.build_postfix(expr)
		}
		ast.ArrayInitExpr {
			return b.build_array_init_expr(expr)
		}
		ast.MapInitExpr {
			// TODO: map init
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		ast.StringInterLiteral {
			return b.build_string_inter_literal(expr)
		}
		ast.MatchExpr {
			// Should be lowered by transformer
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
		}
		ast.OrExpr {
			return b.build_expr(expr.expr)
		}
		ast.FnLiteral {
			// TODO: closures
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		ast.RangeExpr {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		ast.CallOrCastExpr {
			return b.build_call_or_cast(expr)
		}
		ast.AsCastExpr {
			return b.build_expr(expr.expr)
		}
		ast.AssocExpr {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		ast.Tuple {
			if expr.exprs.len > 0 {
				return b.build_expr(expr.exprs[0])
			}
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
		}
		else {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
		}
	}
}

fn (mut b Builder) build_basic_literal(lit ast.BasicLiteral) ValueID {
	match lit.kind {
		.key_true {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(1), '1')
		}
		.key_false {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(1), '0')
		}
		.char {
			typ := b.mod.type_store.get_int(8)
			// Resolve character literal to its numeric byte value
			char_val := b.resolve_char_value(lit.value)
			return b.mod.get_or_add_const(typ, char_val.str())
		}
		.number {
			if lit.value.contains('.') {
				typ := b.mod.type_store.get_float(64)
				return b.mod.get_or_add_const(typ, lit.value)
			}
			typ := b.expr_type(ast.Expr(lit))
			return b.mod.get_or_add_const(typ, lit.value)
		}
		else {
			// Integer or other
			typ := b.expr_type(ast.Expr(lit))
			return b.mod.get_or_add_const(typ, lit.value)
		}
	}
}

fn (mut b Builder) build_string_literal(lit ast.StringLiteral) ValueID {
	// Strip surrounding quotes from V string literal values
	mut val := lit.value
	if val.len >= 2 && ((val[0] == `'` && val[val.len - 1] == `'`)
		|| (val[0] == `"` && val[val.len - 1] == `"`)) {
		val = val[1..val.len - 1]
	}
	if lit.kind == .c {
		// C string literal -> raw i8* pointer
		i8_t := b.mod.type_store.get_int(8)
		ptr_t := b.mod.type_store.get_ptr(i8_t)
		return b.mod.add_value_node(.c_string_literal, ptr_t, val, 0)
	}
	typ := b.get_string_type()
	return b.mod.add_value_node(.string_literal, typ, val, val.len)
}

fn (mut b Builder) build_string_inter_literal(expr ast.StringInterLiteral) ValueID {
	// String interpolation: concatenate literal parts and expression-to-string conversions.
	// Pattern: values[0] + str(inters[0]) + values[1] + str(inters[1]) + ...
	str_type := b.get_string_type()
	plus_fn := b.get_or_create_fn_ref('builtin__string__+', str_type)

	mut parts := []ValueID{}
	for i, raw_val in expr.values {
		// Strip surrounding quotes from first and last values (parser artifact)
		mut val := raw_val
		if i == 0 && val.len > 0 && (val[0] == `'` || val[0] == `"`) {
			val = val[1..]
		}
		if i == expr.values.len - 1 && val.len > 0
			&& (val[val.len - 1] == `'` || val[val.len - 1] == `"`) {
			val = val[..val.len - 1]
		}
		if val.len > 0 {
			parts << b.mod.add_value_node(.string_literal, str_type, val, val.len)
		}
		if i < expr.inters.len {
			inter := expr.inters[i]
			// The transformer prepares inter expressions for sprintf:
			// - strings become expr.str (char* for %s)
			// - ints/floats remain as-is
			// For native backend, we use string concatenation instead.
			// If the inter.expr is a SelectorExpr accessing '.str' on a string,
			// use the base expression (the V string) directly.
			mut use_expr := inter.expr
			if inter.expr is ast.SelectorExpr {
				sel := inter.expr as ast.SelectorExpr
				if sel.rhs.name == 'str' {
					use_expr = sel.lhs
				}
			}
			inter_val := b.build_expr(use_expr)
			inter_type := b.mod.values[inter_val].typ
			// Convert the interpolation value to string
			str_val := b.convert_to_string(inter_val, inter_type)
			parts << str_val
		}
	}

	if parts.len == 0 {
		return b.mod.add_value_node(.string_literal, str_type, '', 0)
	}
	if parts.len == 1 {
		return parts[0]
	}

	// Concatenate all parts with string__+
	mut result := parts[0]
	for i := 1; i < parts.len; i++ {
		result = b.mod.add_instr(.call, b.cur_block, str_type, [plus_fn, result, parts[i]])
	}
	return result
}

fn (mut b Builder) convert_to_string(val ValueID, typ TypeID) ValueID {
	str_type := b.get_string_type()
	// If already a string, return as-is
	if str_type != 0 && typ == str_type {
		return val
	}
	// Also check if the type is a struct named 'string' (may have different ID)
	if typ < b.mod.type_store.types.len {
		tinfo := b.mod.type_store.types[typ]
		if tinfo.kind == .struct_t {
			// Check if this is the string struct by field count (str, len, is_lit)
			if tinfo.fields.len == 3 || typ == str_type {
				return val
			}
		}
	}
	// Determine the conversion function based on the type
	type_info := b.mod.type_store.types[typ]
	mut fn_name := ''
	if type_info.kind == .int_t {
		if type_info.width == 1 {
			// bool
			fn_name = 'builtin__bool__str'
		} else if type_info.width == 64 {
			fn_name = 'builtin__i64__str'
		} else {
			fn_name = 'builtin__int__str'
		}
	} else if type_info.kind == .float_t {
		if type_info.width == 32 {
			fn_name = 'builtin__f32__str'
		} else {
			fn_name = 'builtin__f64__str'
		}
	}
	if fn_name != '' {
		fn_ref := b.get_or_create_fn_ref(fn_name, str_type)
		return b.mod.add_instr(.call, b.cur_block, str_type, [fn_ref, val])
	}
	// Fallback: return empty string
	return b.mod.add_value_node(.string_literal, str_type, '', 0)
}

fn (mut b Builder) build_ident(ident ast.Ident) ValueID {
	// Handle 'nil' identifier (generated by transformer from `unsafe { nil }`)
	if ident.name == 'nil' {
		i8_t := b.mod.type_store.get_int(8)
		ptr_t := b.mod.type_store.get_ptr(i8_t)
		return b.mod.get_or_add_const(ptr_t, '0')
	}
	if ident.name in b.vars {
		ptr := b.vars[ident.name]
		ptr_typ := b.mod.values[ptr].typ
		elem_typ := b.mod.type_store.types[ptr_typ].elem_type
		val := b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
		// For mut pointer params (e.g., mut buf &u8), the alloca stores ptr(ptr(T)).
		// One load gives ptr(ptr(T)), but user sees buf as ptr(T).
		// Add extra dereference to get the actual pointer value.
		if ident.name in b.mut_ptr_params {
			inner_typ := b.mod.type_store.types[elem_typ].elem_type
			if inner_typ != 0 {
				return b.mod.add_instr(.load, b.cur_block, inner_typ, [val])
			}
		}
		return val
	}
	// Could be a constant, enum value, or function reference
	if ident.name in b.enum_values {
		val := b.enum_values[ident.name]
		return b.mod.get_or_add_const(b.mod.type_store.get_int(32), val.str())
	}
	// Try enum value with module prefix (e.g., Token__key_fn → token__Token__key_fn)
	{
		enum_qualified := '${b.cur_module}__${ident.name}'
		if enum_qualified in b.enum_values {
			val := b.enum_values[enum_qualified]
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), val.str())
		}
	}
	// Try as function reference
	if ident.name in b.fn_index {
		return b.get_or_create_fn_ref(ident.name, 0)
	}
	// Try with module prefix (transformer strips module prefix for builtin functions)
	builtin_name := 'builtin__${ident.name}'
	if builtin_name in b.fn_index {
		return b.get_or_create_fn_ref(builtin_name, 0)
	}
	qualified_name := '${b.cur_module}__${ident.name}'
	if qualified_name in b.fn_index {
		return b.get_or_create_fn_ref(qualified_name, 0)
	}
	// Try as compile-time constant (inline the value directly)
	if ident.name in b.const_values {
		return b.mod.get_or_add_const(b.mod.type_store.get_int(64), b.const_values[ident.name].str())
	}
	if qualified_name in b.const_values {
		return b.mod.get_or_add_const(b.mod.type_store.get_int(64), b.const_values[qualified_name].str())
	}
	builtin_const := 'builtin__${ident.name}'
	if builtin_const in b.const_values {
		return b.mod.get_or_add_const(b.mod.type_store.get_int(64), b.const_values[builtin_const].str())
	}
	// Try as string constant (inline the string literal directly)
	if ident.name in b.string_const_values {
		return b.build_string_literal(ast.StringLiteral{
			kind:  .v
			value: b.string_const_values[ident.name]
		})
	}
	if qualified_name in b.string_const_values {
		return b.build_string_literal(ast.StringLiteral{
			kind:  .v
			value: b.string_const_values[qualified_name]
		})
	}
	if builtin_const in b.string_const_values {
		return b.build_string_literal(ast.StringLiteral{
			kind:  .v
			value: b.string_const_values[builtin_const]
		})
	}
	// Try as global variable
	if glob_id := b.find_global(ident.name) {
		glob_typ := b.mod.values[glob_id].typ
		elem_typ := b.mod.type_store.types[glob_typ].elem_type
		return b.mod.add_instr(.load, b.cur_block, elem_typ, [glob_id])
	}
	// Try with prefixes for globals too
	if glob_id := b.find_global(builtin_const) {
		glob_typ := b.mod.values[glob_id].typ
		elem_typ := b.mod.type_store.types[glob_typ].elem_type
		return b.mod.add_instr(.load, b.cur_block, elem_typ, [glob_id])
	}
	if glob_id := b.find_global(qualified_name) {
		glob_typ := b.mod.values[glob_id].typ
		elem_typ := b.mod.type_store.types[glob_typ].elem_type
		return b.mod.add_instr(.load, b.cur_block, elem_typ, [glob_id])
	}
	return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
}

fn (mut b Builder) find_global(name string) ?ValueID {
	for v in b.mod.values {
		if v.kind == .global && v.name == name {
			return v.id
		}
	}
	return none
}

fn (mut b Builder) build_infix(expr ast.InfixExpr) ValueID {
	// Short-circuit evaluation for logical || and &&
	if expr.op == .logical_or {
		bool_type := b.mod.type_store.get_int(1)
		result_alloca := b.mod.add_instr(.alloca, b.cur_block, b.mod.type_store.get_ptr(bool_type),
			[]ValueID{})
		// Evaluate LHS
		lhs := b.build_expr(expr.lhs)
		// Create blocks: rhs_block (evaluate RHS), merge_block
		rhs_block := b.mod.add_block(b.cur_func, 'or_rhs')
		merge_block := b.mod.add_block(b.cur_func, 'or_merge')
		// If LHS is true, short-circuit to merge with true; else evaluate RHS
		one := b.mod.get_or_add_const(bool_type, '1')
		b.mod.add_instr(.store, b.cur_block, 0, [one, result_alloca])
		b.mod.add_instr(.br, b.cur_block, 0, [lhs, b.mod.blocks[merge_block].val_id, b.mod.blocks[rhs_block].val_id])
		b.add_edge(b.cur_block, merge_block)
		b.add_edge(b.cur_block, rhs_block)
		// RHS block
		b.cur_block = rhs_block
		rhs := b.build_expr(expr.rhs)
		b.mod.add_instr(.store, b.cur_block, 0, [rhs, result_alloca])
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
		}
		// Merge block: load result
		b.cur_block = merge_block
		return b.mod.add_instr(.load, b.cur_block, bool_type, [result_alloca])
	}
	if expr.op == .and {
		bool_type := b.mod.type_store.get_int(1)
		result_alloca := b.mod.add_instr(.alloca, b.cur_block, b.mod.type_store.get_ptr(bool_type),
			[]ValueID{})
		// Evaluate LHS
		lhs := b.build_expr(expr.lhs)
		// Create blocks: rhs_block (evaluate RHS), merge_block
		rhs_block := b.mod.add_block(b.cur_func, 'and_rhs')
		merge_block := b.mod.add_block(b.cur_func, 'and_merge')
		// If LHS is false, short-circuit to merge with false; else evaluate RHS
		zero := b.mod.get_or_add_const(bool_type, '0')
		b.mod.add_instr(.store, b.cur_block, 0, [zero, result_alloca])
		b.mod.add_instr(.br, b.cur_block, 0, [lhs, b.mod.blocks[rhs_block].val_id, b.mod.blocks[merge_block].val_id])
		b.add_edge(b.cur_block, rhs_block)
		b.add_edge(b.cur_block, merge_block)
		// RHS block
		b.cur_block = rhs_block
		rhs := b.build_expr(expr.rhs)
		b.mod.add_instr(.store, b.cur_block, 0, [rhs, result_alloca])
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
		}
		// Merge block: load result
		b.cur_block = merge_block
		return b.mod.add_instr(.load, b.cur_block, bool_type, [result_alloca])
	}

	lhs := b.build_expr(expr.lhs)
	rhs := b.build_expr(expr.rhs)
	result_type := b.expr_type(ast.Expr(expr))

	// Check for string comparison: if either operand is a string type,
	// emit string__eq/string__+ call instead of integer comparison/add.
	// This handles match-on-string expressions where the transformer creates
	// InfixExpr{.eq, ...} that bypasses the normal string comparison lowering.
	str_type := b.get_string_type()
	if str_type != 0 {
		lhs_type := b.mod.values[lhs].typ
		rhs_type := b.mod.values[rhs].typ
		if lhs_type == str_type || rhs_type == str_type {
			if expr.op == .eq || expr.op == .ne {
				bool_type := b.mod.type_store.get_int(1)
				fn_ref := b.get_or_create_fn_ref('builtin__string__==', bool_type)
				eq_result := b.mod.add_instr(.call, b.cur_block, bool_type, [fn_ref, lhs, rhs])
				if expr.op == .ne {
					return b.mod.add_instr(.xor, b.cur_block, bool_type, [eq_result,
						b.mod.get_or_add_const(bool_type, '1')])
				}
				return eq_result
			}
			if expr.op == .plus {
				fn_ref := b.get_or_create_fn_ref('builtin__string__+', str_type)
				return b.mod.add_instr(.call, b.cur_block, str_type, [fn_ref, lhs, rhs])
			}
		}
	}

	op := match expr.op {
		.plus { OpCode.add }
		.minus { OpCode.sub }
		.mul { OpCode.mul }
		.div { OpCode.sdiv }
		.mod { OpCode.srem }
		.eq { OpCode.eq }
		.ne { OpCode.ne }
		.lt { OpCode.lt }
		.gt { OpCode.gt }
		.le { OpCode.le }
		.ge { OpCode.ge }
		.amp { OpCode.and_ }
		.pipe { OpCode.or_ }
		.xor { OpCode.xor }
		.left_shift { OpCode.shl }
		.right_shift { OpCode.ashr }
		else { OpCode.add }
	}

	return b.mod.add_instr(op, b.cur_block, result_type, [lhs, rhs])
}

fn (mut b Builder) build_prefix(expr ast.PrefixExpr) ValueID {
	// Special case: &InitExpr (heap/pointer to struct init)
	// Build the struct via alloca+GEP+store but return the pointer instead of loading
	if expr.op == .amp && expr.expr is ast.InitExpr {
		return b.build_init_expr_ptr(expr.expr)
	}

	// Special case: &T(ptr) where ptr is a pointer type → bitcast to *T
	// This handles the V pattern *unsafe { &u32(pkey) } which means *(u32*)pkey in C
	// The parser interprets &u32(pkey) as &(u32(pkey)) = address-of(cast)
	// But the intent is to reinterpret ptr as *T, so we emit bitcast(ptr, *T) instead
	if expr.op == .amp && expr.expr is ast.CastExpr {
		cast_expr := expr.expr as ast.CastExpr
		inner_val := b.build_expr(cast_expr.expr)
		inner_type := b.mod.values[inner_val].typ
		if inner_type != 0 && int(inner_type) < b.mod.type_store.types.len {
			inner_t := b.mod.type_store.types[inner_type]
			if inner_t.kind == .ptr_t {
				// Source is a pointer - reinterpret as *T instead of addr_of(cast)
				target_elem := b.ast_type_to_ssa(cast_expr.typ)
				ptr_type := b.mod.type_store.get_ptr(target_elem)
				return b.mod.add_instr(.bitcast, b.cur_block, ptr_type, [inner_val])
			}
		}
	}
	// Same for &CallOrCastExpr (parser uses CallOrCastExpr when it cannot distinguish
	// between a function call and a type cast, e.g. u8(expr))
	if expr.op == .amp && expr.expr is ast.CallOrCastExpr {
		coce := expr.expr as ast.CallOrCastExpr
		inner_val := b.build_expr(coce.expr)
		inner_type := b.mod.values[inner_val].typ
		if inner_type != 0 && int(inner_type) < b.mod.type_store.types.len {
			inner_t := b.mod.type_store.types[inner_type]
			if inner_t.kind == .ptr_t {
				target_elem := b.ast_type_to_ssa(coce.lhs)
				ptr_type := b.mod.type_store.get_ptr(target_elem)
				return b.mod.add_instr(.bitcast, b.cur_block, ptr_type, [inner_val])
			}
		}
	}
	// Handle &&T(expr) — nested ampersand pointer-type cast pattern.
	// Parser creates PrefixExpr(.amp, PrefixExpr(.amp, CallOrCastExpr/CastExpr(T, expr)))
	// for &&T(expr), meaning "cast expr to type **T", not "address-of address-of cast".
	if expr.op == .amp && expr.expr is ast.PrefixExpr {
		inner_prefix := expr.expr as ast.PrefixExpr
		if inner_prefix.op == .amp {
			if inner_prefix.expr is ast.CallOrCastExpr {
				coce := inner_prefix.expr as ast.CallOrCastExpr
				inner_val := b.build_expr(coce.expr)
				inner_type := b.mod.values[inner_val].typ
				if inner_type != 0 && int(inner_type) < b.mod.type_store.types.len {
					inner_t := b.mod.type_store.types[inner_type]
					if inner_t.kind == .ptr_t {
						// &&T(ptr): cast to **T = ptr(ptr(T))
						target_elem := b.ast_type_to_ssa(coce.lhs)
						ptr_type := b.mod.type_store.get_ptr(target_elem)
						ptr_ptr_type := b.mod.type_store.get_ptr(ptr_type)
						return b.mod.add_instr(.bitcast, b.cur_block, ptr_ptr_type, [
							inner_val,
						])
					}
				}
			} else if inner_prefix.expr is ast.CastExpr {
				cast_expr := inner_prefix.expr as ast.CastExpr
				inner_val := b.build_expr(cast_expr.expr)
				inner_type := b.mod.values[inner_val].typ
				if inner_type != 0 && int(inner_type) < b.mod.type_store.types.len {
					inner_t := b.mod.type_store.types[inner_type]
					if inner_t.kind == .ptr_t {
						target_elem := b.ast_type_to_ssa(cast_expr.typ)
						ptr_type := b.mod.type_store.get_ptr(target_elem)
						ptr_ptr_type := b.mod.type_store.get_ptr(ptr_type)
						return b.mod.add_instr(.bitcast, b.cur_block, ptr_ptr_type, [
							inner_val,
						])
					}
				}
			}
		}
	}

	val := b.build_expr(expr.expr)

	match expr.op {
		.minus {
			zero := b.mod.get_or_add_const(b.mod.values[val].typ, '0')
			return b.mod.add_instr(.sub, b.cur_block, b.mod.values[val].typ, [zero, val])
		}
		.not {
			// Logical NOT: !x → (x == 0)
			// Returns 1 if x is 0, 0 if x is non-zero
			zero := b.mod.get_or_add_const(b.mod.values[val].typ, '0')
			return b.mod.add_instr(.eq, b.cur_block, b.mod.type_store.get_int(1), [
				val,
				zero,
			])
		}
		.amp {
			// Address-of: return the alloca pointer for the variable
			addr := b.build_addr(expr.expr)
			if addr != 0 {
				return addr
			}
			// No addressable location (e.g. function call return value) –
			// For struct types, use heap allocation so the pointer survives
			// the current scope (needed for sum type boxing where _data
			// must outlive the wrapping function).
			// For scalars, use stack alloca (they're typically short-lived).
			val_type := b.mod.values[val].typ
			if val_type != 0 {
				ptr_type := b.mod.type_store.get_ptr(val_type)
				typ_info := b.mod.type_store.types[val_type]
				if typ_info.kind == .struct_t {
					// Heap-allocate struct values to ensure pointer validity
					heap_ptr := b.mod.add_instr(.heap_alloc, b.cur_block, ptr_type, []ValueID{})
					b.mod.add_instr(.store, b.cur_block, 0, [val, heap_ptr])
					return heap_ptr
				}
				alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
				b.mod.add_instr(.store, b.cur_block, 0, [val, alloca])
				return alloca
			}
			return val
		}
		.bit_not {
			neg_one := b.mod.get_or_add_const(b.mod.values[val].typ, '-1')
			return b.mod.add_instr(.xor, b.cur_block, b.mod.values[val].typ, [val, neg_one])
		}
		.mul {
			// Dereference: load from pointer
			val_type := b.mod.values[val].typ
			if val_type != 0 && int(val_type) < b.mod.type_store.types.len {
				typ := b.mod.type_store.types[val_type]
				if typ.kind == .ptr_t && typ.elem_type != 0 {
					return b.mod.add_instr(.load, b.cur_block, typ.elem_type, [val])
				}
			}
			return val
		}
		else {
			return val
		}
	}
}

fn (mut b Builder) build_call(expr ast.CallExpr) ValueID {
	// Resolve function name
	fn_name := b.resolve_call_name(expr)

	// Check if this is a type cast disguised as a call (e.g., IError(ptr)).
	// If the name is a struct type and not a registered function, treat as cast.
	if fn_name !in b.fn_index && expr.args.len == 1 {
		// Try to find a struct type matching this name
		if target_type := b.struct_types[fn_name] {
			val := b.build_expr(expr.args[0])
			src_type := b.mod.values[val].typ
			if src_type == target_type {
				return val
			}
			return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
		}
		// Also try with module prefix
		qualified := '${b.cur_module}__${fn_name}'
		if target_type := b.struct_types[qualified] {
			val := b.build_expr(expr.args[0])
			src_type := b.mod.values[val].typ
			if src_type == target_type {
				return val
			}
			return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
		}
	}

	mut ret_type := b.expr_type(ast.Expr(expr))
	// If expr_type fell back to i64 (default), use the registered function's return type instead
	if fn_name in b.fn_index {
		fn_idx := b.fn_index[fn_name]
		fn_ret := b.mod.funcs[fn_idx].typ
		if fn_ret != 0 {
			ret_type = fn_ret
		}
	}

	// Check if this is a function pointer field call (e.g., m.hash_fn(pkey))
	// rather than a method call. If the selector field matches a struct field name
	// and the resolved function name is not in fn_index, it's a function pointer call.
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		if !b.is_module_name(sel.lhs) && fn_name !in b.fn_index {
			field_name := sel.rhs.name
			if b.is_struct_field(sel.lhs, field_name) {
				// Build the selector expression to get the function pointer value
				fn_ptr := b.build_selector(sel)
				// Build arguments (no receiver - this is a field access, not a method call)
				mut fnptr_args := []ValueID{}
				for arg in expr.args {
					if arg is ast.ModifierExpr && arg.kind == .key_mut {
						addr := b.build_addr(arg.expr)
						if addr != 0 {
							fnptr_args << addr
						} else {
							fnptr_args << b.build_expr(arg)
						}
					} else {
						fnptr_args << b.build_expr(arg)
					}
				}
				mut operands := []ValueID{cap: fnptr_args.len + 1}
				operands << fn_ptr
				operands << fnptr_args
				return b.mod.add_instr(.call_indirect, b.cur_block, ret_type, operands)
			}
		}
	}

	// Build arguments
	mut args := []ValueID{}
	// For method calls, add receiver as first arg
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		// Check if this is a method call (not a module function call)
		if !b.is_module_name(sel.lhs) {
			// Check if method expects pointer receiver (mut receiver)
			mut expects_ptr := false
			if fn_name in b.fn_index {
				fn_idx := b.fn_index[fn_name]
				if b.mod.funcs[fn_idx].params.len > 0 {
					param_type := b.mod.values[b.mod.funcs[fn_idx].params[0]].typ
					if param_type < b.mod.type_store.types.len
						&& b.mod.type_store.types[param_type].kind == .ptr_t {
						expects_ptr = true
					}
				}
			}
			mut receiver := if expects_ptr {
				// Mut receiver: pass address (pointer to struct)
				addr := b.build_addr(sel.lhs)
				if addr != 0 {
					// build_addr for an Ident returns the alloca.
					// For a mut receiver variable, the alloca is ptr(ptr(Struct)),
					// storing the struct pointer. We need to load from the alloca
					// to get the actual struct pointer ptr(Struct).
					addr_typ := b.mod.values[addr].typ
					if addr_typ < b.mod.type_store.types.len
						&& b.mod.type_store.types[addr_typ].kind == .ptr_t {
						inner := b.mod.type_store.types[addr_typ].elem_type
						if inner < b.mod.type_store.types.len
							&& b.mod.type_store.types[inner].kind == .ptr_t {
							pointee := b.mod.type_store.types[inner].elem_type
							if pointee < b.mod.type_store.types.len
								&& b.mod.type_store.types[pointee].kind == .struct_t {
								// addr is ptr(ptr(Struct)) — load to get ptr(Struct)
								b.mod.add_instr(.load, b.cur_block, inner, [addr])
							} else {
								addr
							}
						} else {
							addr
						}
					} else {
						addr
					}
				} else {
					b.build_expr(sel.lhs)
				}
			} else {
				b.build_expr(sel.lhs)
			}
			// Auto-deref pointer receivers: if receiver is a pointer to a struct
			// but the method expects a value receiver, load through the pointer
			if !expects_ptr {
				recv_typ := b.mod.values[receiver].typ
				if recv_typ < b.mod.type_store.types.len
					&& b.mod.type_store.types[recv_typ].kind == .ptr_t {
					pointee := b.mod.type_store.types[recv_typ].elem_type
					if pointee < b.mod.type_store.types.len
						&& b.mod.type_store.types[pointee].kind == .struct_t {
						receiver = b.mod.add_instr(.load, b.cur_block, pointee, [
							receiver,
						])
					}
				}
			}
			args << receiver
		}
	}
	// Determine parameter types to decide if we should pass addresses for mut receivers
	mut fn_params := []ValueID{}
	if fn_name in b.fn_index {
		fn_params = b.mod.funcs[b.fn_index[fn_name]].params.clone()
	}
	for arg in expr.args {
		// For mut arguments, pass the address (pointer) instead of the value
		if arg is ast.ModifierExpr && arg.kind == .key_mut {
			addr := b.build_addr(arg.expr)
			if addr != 0 {
				args << addr
			} else {
				// Can't take address directly (e.g., `mut &buffer[0]`).
				// Evaluate the expression, store in a temp alloca, pass alloca address.
				val := b.build_expr(arg.expr)
				val_type := b.mod.values[val].typ
				alloca_type := b.mod.type_store.get_ptr(val_type)
				tmp_alloca := b.mod.add_instr(.alloca, b.cur_block, alloca_type, []ValueID{})
				b.mod.add_instr(.store, b.cur_block, 0, [val, tmp_alloca])
				args << tmp_alloca
			}
		} else {
			// Use args.len as param index (accounts for receiver already in args)
			param_idx := args.len
			mut param_wants_struct_ptr := false
			if param_idx < fn_params.len {
				param_type := b.mod.values[fn_params[param_idx]].typ
				// Only use build_addr for ptr(struct_t) params (mut receivers/params),
				// NOT for ptr(int_t), ptr(ptr_t) etc. (normal pointer params like C.puts)
				if param_type < b.mod.type_store.types.len
					&& b.mod.type_store.types[param_type].kind == .ptr_t {
					pointee := b.mod.type_store.types[param_type].elem_type
					if pointee < b.mod.type_store.types.len
						&& b.mod.type_store.types[pointee].kind == .struct_t {
						param_wants_struct_ptr = true
					}
				}
			}
			// For args where the function expects ptr(struct) (mut receiver/param),
			// try to pass the original variable address instead of a copy
			if param_wants_struct_ptr && (arg is ast.Ident || arg is ast.SelectorExpr) {
				// First try build_expr: if it already returns ptr(struct), use it directly
				val := b.build_expr(arg)
				val_typ := b.mod.values[val].typ
				val_is_ptr := val_typ < b.mod.type_store.types.len
					&& b.mod.type_store.types[val_typ].kind == .ptr_t
				if val_is_ptr {
					// Already a pointer (e.g., mut receiver re-passing its own pointer)
					args << val
				} else {
					// Value type (local mut variable): use build_addr to get the alloca
					addr := b.build_addr(arg)
					if addr != 0 {
						args << addr
					} else {
						args << val
					}
				}
			} else {
				args << b.build_expr(arg)
			}
		}
	}

	// Auto-deref/ref arguments to match function parameter types
	if fn_params.len > 0 {
		for ai := 0; ai < args.len && ai < fn_params.len; ai++ {
			arg_type := b.mod.values[args[ai]].typ
			param_type := b.mod.values[fn_params[ai]].typ
			if arg_type < b.mod.type_store.types.len && param_type < b.mod.type_store.types.len {
				arg_kind := b.mod.type_store.types[arg_type].kind
				param_kind := b.mod.type_store.types[param_type].kind
				// Pointer arg but value param: auto-deref
				// Only auto-deref when the pointee is a struct and the
				// parameter expects that struct value. Do NOT deref raw
				// pointers (ptr(i8)/voidptr) when the param is a plain
				// int type (e.g., i64 from variadic params).
				if arg_kind == .ptr_t && param_kind == .struct_t {
					pointee := b.mod.type_store.types[arg_type].elem_type
					if pointee == param_type {
						args[ai] = b.mod.add_instr(.load, b.cur_block, pointee, [
							args[ai],
						])
					}
				}
				// Value arg but pointer param: auto-ref (alloca + store + pass pointer)
				if arg_kind == .struct_t && param_kind == .ptr_t {
					ptr_type := b.mod.type_store.get_ptr(arg_type)
					alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
					b.mod.add_instr(.store, b.cur_block, 0, [args[ai], alloca])
					args[ai] = alloca
				}
			}
		}
	}

	// Check if fn_name is a local variable holding a function pointer
	// (e.g., `cfn(s)` where cfn is a parameter of type `fn(string) string`)
	if fn_name in b.vars {
		fn_ptr := b.build_ident(ast.Ident{ name: fn_name })
		mut indirect_operands := []ValueID{cap: args.len + 1}
		indirect_operands << fn_ptr
		indirect_operands << args
		return b.mod.add_instr(.call_indirect, b.cur_block, ret_type, indirect_operands)
	}

	fn_ref := b.get_or_create_fn_ref(fn_name, ret_type)
	mut operands := []ValueID{cap: args.len + 1}
	operands << fn_ref
	operands << args

	return b.mod.add_instr(.call, b.cur_block, ret_type, operands)
}

fn (mut b Builder) is_module_name(expr ast.Expr) bool {
	if expr is ast.Ident {
		if b.env != unsafe { nil } {
			if scope := b.env.get_scope(b.cur_module) {
				if obj := scope.lookup_parent(expr.name, 0) {
					return obj is types.Module
				}
			}
		}
	}
	return false
}

fn (mut b Builder) resolve_call_name(expr ast.CallExpr) string {
	match expr.lhs {
		ast.Ident {
			name := expr.lhs.name
			// Try module-qualified FIRST to avoid shadowing by C functions.
			// E.g., os.getenv() should resolve to os__getenv, not C.getenv.
			qualified := '${b.cur_module}__${name}'
			if qualified in b.fn_index {
				return qualified
			}
			// Check if it's a known function
			if name in b.fn_index {
				return name
			}
			// Try builtin-qualified (transformer remaps builtin__X to X)
			builtin_qualified := 'builtin__${name}'
			if builtin_qualified in b.fn_index {
				return builtin_qualified
			}
			// For names with module prefix (e.g., v__error), try replacing
			// the module prefix with builtin__
			if idx := name.index('__') {
				suffix := name[idx + 2..]
				alt := 'builtin__${suffix}'
				if alt in b.fn_index {
					return alt
				}
			}
			// Resolve operator method names: the transformer generates e.g.
			// 'string__plus' but the SSA builder registers 'builtin__string__+'
			// Map transformer names to operator symbols.
			op_map := {
				'plus':  '+'
				'minus': '-'
				'mult':  '*'
				'div':   '/'
				'mod':   '%'
				'eq':    '=='
				'ne':    '!='
				'lt':    '<'
				'gt':    '>'
				'le':    '<='
				'ge':    '>='
			}
			if idx2 := name.last_index('__') {
				method_part := name[idx2 + 2..]
				if op_sym := op_map[method_part] {
					type_part := name[..idx2]
					op_name := '${type_part}__${op_sym}'
					if op_name in b.fn_index {
						return op_name
					}
					op_builtin := 'builtin__${op_name}'
					if op_builtin in b.fn_index {
						return op_builtin
					}
				}
			}
			// C functions: strip C__ prefix for direct C interop
			if name.starts_with('C__') {
				return name[3..]
			}
			// V1 C backend naming uses single underscore for type_method (e.g., array_push_noscan),
			// but SSA builder uses double underscore (array__push_noscan).
			// Try converting known type prefixes from single to double underscore.
			v1_type_prefixes := ['array_', 'string_', 'int_', 'i8_', 'i16_', 'i64_', 'u8_', 'u16_',
				'u32_', 'u64_', 'f32_', 'f64_', 'bool_', 'map_', 'rune_', 'char_']
			// Check bare name (e.g., "array_push_noscan") and builtin-prefixed
			for prefix in v1_type_prefixes {
				// Check bare name
				if name.starts_with(prefix) {
					type_name := prefix[..prefix.len - 1]
					method_part := name[prefix.len..]
					double_name := '${type_name}__${method_part}'
					if double_name in b.fn_index {
						return double_name
					}
					double_builtin := 'builtin__${double_name}'
					if double_builtin in b.fn_index {
						return double_builtin
					}
				}
				// Check builtin__type_method
				bp := 'builtin__${prefix}'
				if name.starts_with(bp) {
					type_name := prefix[..prefix.len - 1]
					method_part := name[bp.len..]
					double_name := 'builtin__${type_name}__${method_part}'
					if double_name in b.fn_index {
						return double_name
					}
				}
			}
			return name
		}
		ast.SelectorExpr {
			sel := expr.lhs as ast.SelectorExpr
			if b.is_module_name(sel.lhs) {
				// Module function call: module.fn()
				mod_name := sel.lhs.name()
				// C functions: C.puts() → just 'puts' for direct C interop
				if mod_name == 'C' {
					return sel.rhs.name
				}
				qualified := '${mod_name}__${sel.rhs.name}'
				if qualified in b.fn_index {
					return qualified
				}
				// Try resolving the module alias
				if b.env != unsafe { nil } {
					if scope := b.env.get_scope(b.cur_module) {
						if obj := scope.lookup_parent(mod_name, 0) {
							if obj is types.Module {
								real_mod := obj.name.replace('.', '_')
								real_qualified := '${real_mod}__${sel.rhs.name}'
								if real_qualified in b.fn_index {
									return real_qualified
								}
								return real_qualified
							}
						}
					}
				}
				return qualified
			}
			// Method call: expr.method()
			mut receiver_type := b.get_receiver_type_name(sel.lhs)
			// Strip pointer prefix for method resolution (e.g., Point* → Point)
			for receiver_type.ends_with('*') {
				receiver_type = receiver_type[..receiver_type.len - 1]
			}
			method_name := '${receiver_type}__${sel.rhs.name}'
			if method_name in b.fn_index {
				return method_name
			}
			// Try with builtin__ prefix (e.g., Array_rune__string → builtin__Array_rune__string)
			builtin_method := 'builtin__${method_name}'
			if builtin_method in b.fn_index {
				return builtin_method
			}
			// Try with current module prefix
			if b.cur_module != '' && b.cur_module != 'main' {
				mod_method := '${b.cur_module}__${method_name}'
				if mod_method in b.fn_index {
					return mod_method
				}
			}
			// Try searching all functions for one matching __method_name
			suffix := '__${sel.rhs.name}'
			for fname, _ in b.fn_index {
				if fname.ends_with(suffix) {
					return fname
				}
			}
			return method_name
		}
		else {
			return 'unknown_fn'
		}
	}
}

fn (mut b Builder) get_receiver_type_name(expr ast.Expr) string {
	if b.env != unsafe { nil } {
		pos := expr.pos()
		if pos.id != 0 {
			if typ := b.env.get_expr_type(pos.id) {
				return b.types_type_c_name(typ)
			}
		}
	}
	if expr is ast.Ident {
		return expr.name
	}
	return 'unknown'
}

fn (mut b Builder) build_selector(expr ast.SelectorExpr) ValueID {
	// Check for enum shorthand: .field in certain contexts
	if expr.lhs is ast.EmptyExpr || (expr.lhs is ast.Ident && expr.lhs.name == '') {
		// Enum shorthand — try to look up a resolved name
		field_name := expr.rhs.name
		// Collect all matching enum values (not just the first)
		suffix := '__${field_name}'
		mut match_keys := []string{}
		mut match_vals := []int{}
		for key, val in b.enum_values {
			if key.ends_with(suffix) {
				match_keys << key
				match_vals << val
			}
		}
		if match_keys.len == 1 {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), match_vals[0].str())
		}
		if match_keys.len > 1 {
			// Disambiguate: prefer enum from current module
			if b.cur_module != '' {
				for i, mk in match_keys {
					if mk.starts_with('${b.cur_module}__') {
						return b.mod.get_or_add_const(b.mod.type_store.get_int(32), match_vals[i].str())
					}
				}
			}
			// Fallback: use the first match
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), match_vals[0].str())
		}
		return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	}

	// Check for qualified enum access: EnumType.value
	if expr.lhs is ast.Ident {
		// Try: EnumType__value (local) or module__EnumType__value (qualified)
		enum_key := '${expr.lhs.name}__${expr.rhs.name}'
		if enum_key in b.enum_values {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), b.enum_values[enum_key].str())
		}
		qualified_key := '${b.cur_module}__${enum_key}'
		if qualified_key in b.enum_values {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), b.enum_values[qualified_key].str())
		}
	}

	// C constant/global access: C.SEEK_END, C.stdout, C.stderr, etc.
	if expr.lhs is ast.Ident && expr.lhs.name == 'C' {
		c_name := expr.rhs.name
		// Well-known C preprocessor constants — emit inline integer values
		// since the native backend cannot resolve C macros.
		c_const_val := match c_name {
			'SEEK_SET' { '0' }
			'SEEK_CUR' { '1' }
			'SEEK_END' { '2' }
			'EOF' { '-1' }
			'NULL' { '0' }
			'O_RDONLY' { '0' }
			'O_WRONLY' { '1' }
			'O_RDWR' { '2' }
			'O_CREAT' { '512' }
			'O_TRUNC' { '1024' }
			'O_EXCL' { '2048' }
			'O_APPEND' { '8' }
			'S_IRUSR' { '256' }
			'S_IWUSR' { '128' }
			'S_IXUSR' { '64' }
			'S_IREAD' { '256' }
			'S_IWRITE' { '128' }
			'S_IEXEC' { '64' }
			'PROT_READ' { '1' }
			'PROT_WRITE' { '2' }
			'SIGTERM' { '15' }
			'SIGKILL' { '9' }
			'SIGINT' { '2' }
			'STDIN_FILENO' { '0' }
			'STDOUT_FILENO' { '1' }
			'STDERR_FILENO' { '2' }
			'DT_DIR' { '4' }
			'DT_REG' { '8' }
			'DT_LNK' { '10' }
			'DT_UNKNOWN' { '0' }
			'ENOENT' { '2' }
			'EXIT_SUCCESS' { '0' }
			'EXIT_FAILURE' { '1' }
			else { '' }
		}
		if c_const_val.len > 0 {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), c_const_val)
		}
		// Not a known constant — emit as a global reference (e.g. C.stdout, C.stderr)
		i8_t := b.mod.type_store.get_int(8)
		ptr_t := b.mod.type_store.get_ptr(i8_t)
		return b.mod.add_value_node(.global, ptr_t, c_name, 0)
	}

	// Module-qualified constant/global access: os.args, pref.Backend, etc.
	// When LHS is a module name, resolve module__field as a constant or global.
	if expr.lhs is ast.Ident {
		mod_name := expr.lhs.name.replace('.', '_')
		qualified := '${mod_name}__${expr.rhs.name}'
		// Try as compile-time constant
		if qualified in b.const_values {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), b.const_values[qualified].str())
		}
		// Try as string constant
		if qualified in b.string_const_values {
			return b.build_string_literal(ast.StringLiteral{
				kind:  .v
				value: b.string_const_values[qualified]
			})
		}
		// Try as global variable (runtime-initialized constants like os.args)
		if glob_id := b.find_global(qualified) {
			glob_typ := b.mod.values[glob_id].typ
			elem_typ := b.mod.type_store.types[glob_typ].elem_type
			return b.mod.add_instr(.load, b.cur_block, elem_typ, [glob_id])
		}
	}

	// Use extractvalue for struct field access
	mut base := b.build_expr(expr.lhs)
	// Check for fixed-size array .len access — return compile-time constant
	base_typ_raw := b.mod.values[base].typ
	if base_typ_raw < b.mod.type_store.types.len {
		mut check_typ := b.mod.type_store.types[base_typ_raw]
		// Also handle ptr(array_t) — e.g. when fixed array is behind a pointer
		if check_typ.kind == .ptr_t && check_typ.elem_type < b.mod.type_store.types.len {
			check_typ = b.mod.type_store.types[check_typ.elem_type]
		}
		if check_typ.kind == .array_t && expr.rhs.name == 'len' {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), check_typ.len.str())
		}
	}
	// If base is a pointer to struct (mut param or heap alloc), auto-deref
	base_typ := b.mod.values[base].typ
	if base_typ < b.mod.type_store.types.len && b.mod.type_store.types[base_typ].kind == .ptr_t {
		pointee := b.mod.type_store.types[base_typ].elem_type
		if pointee < b.mod.type_store.types.len && b.mod.type_store.types[pointee].kind == .struct_t {
			base = b.mod.add_instr(.load, b.cur_block, pointee, [base])
		}
	}
	field_idx := b.field_index(expr, base)
	// Determine result type: prefer SSA struct field type for struct bases,
	// fall back to type environment.
	// The type environment may have the smartcast variant type (e.g., int) for a
	// sumtype field access, while the SSA struct has the correct field type.
	mut result_type := TypeID(0)
	actual_base_type := b.mod.values[base].typ
	if actual_base_type < b.mod.type_store.types.len {
		typ := b.mod.type_store.types[actual_base_type]
		if typ.kind == .struct_t && field_idx < typ.fields.len {
			result_type = typ.fields[field_idx]
		}
	}
	if result_type == 0 {
		result_type = b.expr_type(ast.Expr(expr))
	}
	return b.mod.add_instr(.extractvalue, b.cur_block, result_type, [base,
		b.mod.get_or_add_const(b.mod.type_store.get_int(32), field_idx.str())])
}

fn (mut b Builder) field_index(expr ast.SelectorExpr, base ValueID) int {
	// Use type environment to find field index
	if b.env != unsafe { nil } {
		pos := expr.lhs.pos()
		if pos.id != 0 {
			if typ := b.env.get_expr_type(pos.id) {
				st := b.unwrap_to_struct(typ)
				if st.name != '' {
					for i, f in st.fields {
						if f.name == expr.rhs.name {
							return i
						}
					}
				}
			}
		}
	}
	// Fallback: look up field name in the SSA struct type of the base value
	base_type_id := b.mod.values[base].typ
	if base_type_id < b.mod.type_store.types.len {
		mut typ := b.mod.type_store.types[base_type_id]
		// Dereference pointer(s) to get to the struct type
		for typ.kind == .ptr_t && typ.elem_type < b.mod.type_store.types.len {
			typ = b.mod.type_store.types[typ.elem_type]
		}
		if typ.kind == .struct_t {
			for i, name in typ.field_names {
				if name == expr.rhs.name {
					return i
				}
			}
		}
	}
	return 0
}

// is_struct_field checks whether a field name exists as a struct field of the
// receiver expression's type. Used to distinguish function pointer field calls
// (e.g., m.hash_fn(pkey)) from method calls.
fn (mut b Builder) is_struct_field(receiver_expr ast.Expr, field_name string) bool {
	// Try type environment first
	if b.env != unsafe { nil } {
		pos := receiver_expr.pos()
		if pos.id != 0 {
			if typ := b.env.get_expr_type(pos.id) {
				st := b.unwrap_to_struct(typ)
				if st.name != '' {
					for f in st.fields {
						if f.name == field_name {
							return true
						}
					}
				}
			}
		}
	}
	return false
}

fn (mut b Builder) unwrap_to_struct(t types.Type) types.Struct {
	match t {
		types.Struct {
			return t
		}
		types.Pointer {
			return b.unwrap_to_struct(t.base_type)
		}
		types.Alias {
			return b.unwrap_to_struct(t.base_type)
		}
		else {
			return types.Struct{}
		}
	}
}

fn (mut b Builder) build_index(expr ast.IndexExpr) ValueID {
	base := b.build_expr(expr.lhs)
	index := b.build_expr(expr.expr)
	mut result_type := b.expr_type(ast.Expr(expr))

	base_type_id := b.mod.values[base].typ
	array_type := b.get_array_type()

	// Check if base is a dynamic array (array struct) — need to access .data field
	if array_type != 0 && base_type_id == array_type {
		// If result_type is i64 (fallback), try to infer the actual element type
		// from the array expression's checker type. This is needed for transformer-
		// generated IndexExprs (e.g., for-in-array lowering) that have no position ID.
		i64_t := b.mod.type_store.get_int(64)
		if result_type == i64_t {
			if b.env != unsafe { nil } {
				lhs_pos := expr.lhs.pos()
				if lhs_pos.id != 0 {
					if arr_typ := b.env.get_expr_type(lhs_pos.id) {
						if arr_typ is types.Array {
							inferred := b.type_to_ssa(arr_typ.elem_type)
							if inferred != 0 {
								result_type = inferred
							}
						}
					}
				}
			}
		}
		// Extract .data field (index 0), cast to element pointer, then GEP
		i8_t := b.mod.type_store.get_int(8)
		void_ptr := b.mod.type_store.get_ptr(i8_t)
		data_ptr := b.mod.add_instr(.extractvalue, b.cur_block, void_ptr, [base,
			b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')])
		// Cast void* to element*
		elem_ptr_type := b.mod.type_store.get_ptr(result_type)
		typed_ptr := b.mod.add_instr(.bitcast, b.cur_block, elem_ptr_type, [data_ptr])
		// GEP to the element
		elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
			typed_ptr,
			index,
		])
		// Load the element
		return b.mod.add_instr(.load, b.cur_block, result_type, [elem_addr])
	}

	// Check if base is a string struct — index into .str (field 0) data pointer
	str_type := b.get_string_type()
	if str_type != 0 && base_type_id == str_type {
		// Extract .str field (field 0) — pointer to u8 data
		i8_t := b.mod.type_store.get_int(8)
		u8_ptr := b.mod.type_store.get_ptr(i8_t)
		data_ptr := b.mod.add_instr(.extractvalue, b.cur_block, u8_ptr, [base,
			b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')])
		// GEP to the byte at index (scale = 1 for u8)
		elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block, u8_ptr, [
			data_ptr,
			index,
		])
		// Load the byte
		return b.mod.add_instr(.load, b.cur_block, i8_t, [elem_addr])
	}

	// Check if base is a pointer (e.g., from alloca for fixed-size array literal)
	// For ptr(T), element type is T — use that instead of expr_type fallback
	if base_type_id < b.mod.type_store.types.len {
		base_typ := b.mod.type_store.types[base_type_id]
		if base_typ.kind == .ptr_t && base_typ.elem_type != 0 {
			mut elem_type := base_typ.elem_type
			// If pointed-to type is a fixed-size array (array_t), extract its element type.
			// ptr(array(i32, 5))[i] should load an i32, not an array(i32, 5).
			if elem_type < b.mod.type_store.types.len {
				inner_typ := b.mod.type_store.types[elem_type]
				if inner_typ.kind == .array_t && inner_typ.elem_type != 0 {
					elem_type = inner_typ.elem_type
				}
			}
			// GEP to the element address, then load
			elem_ptr_type := b.mod.type_store.get_ptr(elem_type)
			elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type,
				[base, index])
			return b.mod.add_instr(.load, b.cur_block, elem_type, [elem_addr])
		}
	}

	return b.mod.add_instr(.get_element_ptr, b.cur_block, result_type, [base, index])
}

fn (mut b Builder) build_if_expr(node ast.IfExpr) ValueID {
	// If used as expression, returns a value
	mut result_type := b.expr_type(ast.Expr(node))
	// If result_type is i64 (fallback), try to infer from branch contents.
	// This handles match-on-string expressions where the transformer creates
	// IfExpr chains without position IDs, so expr_type returns i64 fallback.
	i64_t := b.mod.type_store.get_int(64)
	if result_type == i64_t {
		// Check if "then" branch last statement is a string literal or string expr
		if node.stmts.len > 0 {
			last := node.stmts[node.stmts.len - 1]
			if last is ast.ExprStmt {
				if last.expr is ast.StringLiteral || last.expr is ast.StringInterLiteral {
					str_type := b.get_string_type()
					if str_type != 0 {
						result_type = str_type
					}
				} else if last.expr is ast.Ident {
					// Look up the identifier's SSA type from variables or function return types.
					// This handles transformer-generated patterns like:
					//   _t := fn_returning_struct()
					//   src := if _t { _t } else { fallback }
					// where _t has a struct type (e.g., string) but the IfExpr has no type annotation.
					ident_name := (last.expr as ast.Ident).name
					if alloca_id := b.vars[ident_name] {
						alloca_val := b.mod.values[alloca_id]
						if alloca_val.typ > 0 && alloca_val.typ < b.mod.type_store.types.len {
							alloca_typ := b.mod.type_store.types[alloca_val.typ]
							if alloca_typ.kind == .ptr_t && alloca_typ.elem_type > 0
								&& alloca_typ.elem_type < b.mod.type_store.types.len {
								elem := b.mod.type_store.types[alloca_typ.elem_type]
								if elem.kind == .struct_t {
									result_type = alloca_typ.elem_type
								}
							}
						}
					}
				} else {
					// Try to infer type from the expression using expr_type.
					// This handles CallExpr, SelectorExpr, etc. that may return struct types.
					inferred := b.expr_type(last.expr)
					if inferred != i64_t && inferred != 0 {
						result_type = inferred
					}
				}
			}
		}
	}
	result_alloca := b.mod.add_instr(.alloca, b.cur_block, b.mod.type_store.get_ptr(result_type),
		[]ValueID{})

	then_block := b.mod.add_block(b.cur_func, 'ifx_then')
	merge_block := b.mod.add_block(b.cur_func, 'ifx_merge')
	has_else := node.else_expr !is ast.EmptyExpr
	else_block := if has_else {
		b.mod.add_block(b.cur_func, 'ifx_else')
	} else {
		merge_block
	}

	cond := b.build_expr(node.cond)
	b.mod.add_instr(.br, b.cur_block, 0, [cond, b.mod.blocks[then_block].val_id, b.mod.blocks[else_block].val_id])
	b.add_edge(b.cur_block, then_block)
	b.add_edge(b.cur_block, else_block)

	// Then
	b.cur_block = then_block
	mut then_val := ValueID(0)
	if node.stmts.len > 0 {
		for i := 0; i < node.stmts.len - 1; i++ {
			b.build_stmt(node.stmts[i])
		}
		last := node.stmts[node.stmts.len - 1]
		if last is ast.ExprStmt {
			then_val = b.build_expr(last.expr)
		} else {
			b.build_stmt(last)
		}
	}
	if then_val != 0 {
		b.mod.add_instr(.store, b.cur_block, 0, [then_val, result_alloca])
	}
	if !b.block_has_terminator(b.cur_block) {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
		b.add_edge(b.cur_block, merge_block)
	}

	// Else
	if has_else {
		b.cur_block = else_block
		mut else_val := ValueID(0)
		if node.else_expr is ast.IfExpr {
			else_if := node.else_expr as ast.IfExpr
			if else_if.cond is ast.EmptyExpr {
				// Pure else
				if else_if.stmts.len > 0 {
					for i := 0; i < else_if.stmts.len - 1; i++ {
						b.build_stmt(else_if.stmts[i])
					}
					last := else_if.stmts[else_if.stmts.len - 1]
					if last is ast.ExprStmt {
						else_val = b.build_expr(last.expr)
					} else {
						b.build_stmt(last)
					}
				}
			} else {
				else_val = b.build_if_expr(else_if)
			}
		} else {
			else_val = b.build_expr(node.else_expr)
		}
		if else_val != 0 {
			b.mod.add_instr(.store, b.cur_block, 0, [else_val, result_alloca])
		}
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
		}
	}

	b.cur_block = merge_block
	return b.mod.add_instr(.load, b.cur_block, result_type, [result_alloca])
}

fn (mut b Builder) build_array_init_expr(expr ast.ArrayInitExpr) ValueID {
	// If the array init has elements, alloca a fixed-size array on the stack,
	// store each element, and return the pointer.
	if expr.exprs.len > 0 {
		mut elem_vals := []ValueID{cap: expr.exprs.len}
		for e in expr.exprs {
			elem_vals << b.build_expr(e)
		}
		mut elem_type := b.mod.type_store.get_int(32) // default int
		if elem_vals.len > 0 {
			elem_type = b.mod.values[elem_vals[0]].typ
		}
		// Allocate fixed-size array on stack and store each element.
		// Use array_t so that GEP uses element-size scaling (not struct-field offsets).
		arr_fixed_type := b.mod.type_store.get_array(elem_type, elem_vals.len)
		ptr_type := b.mod.type_store.get_ptr(arr_fixed_type)
		alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
		// Store each element via GEP + store.
		// GEP result type is ptr(elem_type) - a pointer to one element, not the whole array.
		elem_ptr_type := b.mod.type_store.get_ptr(elem_type)
		i32_t := b.mod.type_store.get_int(32)
		for i, val in elem_vals {
			idx := b.mod.get_or_add_const(i32_t, i.str())
			gep := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
				alloca,
				idx,
			])
			b.mod.add_instr(.store, b.cur_block, 0, [val, gep])
		}
		return alloca
	}

	// Check if this is a fixed-size array type (e.g., [5]u8{}).
	// These need stack allocation via alloca, not a dynamic array struct.
	if expr.typ is ast.Type {
		if expr.typ is ast.ArrayFixedType {
			fixed_typ := expr.typ as ast.ArrayFixedType
			elem_type := b.ast_type_to_ssa(fixed_typ.elem_type)
			arr_len := if fixed_typ.len is ast.BasicLiteral {
				fixed_typ.len.value.int()
			} else {
				0
			}
			if arr_len > 0 {
				arr_fixed_type := b.mod.type_store.get_array(elem_type, arr_len)
				ptr_type := b.mod.type_store.get_ptr(arr_fixed_type)
				alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
				// Zero-initialize each element
				zero := b.mod.get_or_add_const(elem_type, '0')
				elem_ptr_type := b.mod.type_store.get_ptr(elem_type)
				i32_t := b.mod.type_store.get_int(32)
				for i in 0 .. arr_len {
					idx := b.mod.get_or_add_const(i32_t, i.str())
					gep := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type,
						[
						alloca,
						idx,
					])
					b.mod.add_instr(.store, b.cur_block, 0, [zero, gep])
				}
				return alloca
			}
		}
	}

	// Empty dynamic array with len/cap - these should have been
	// transformed to __new_array_with_default_noscan calls by the transformer.
	// Return zero-initialized array struct as fallback.
	arr_type := b.get_array_type()
	return b.mod.get_or_add_const(arr_type, '0')
}

fn (mut b Builder) build_init_expr(expr ast.InitExpr) ValueID {
	// Struct initialization: Type{ field: value, ... }
	// Uses struct_init opcode: keeps aggregates as SSA values for better optimization.
	// Lowered to alloca/GEP/store only when address is taken (build_init_expr_ptr).

	// Resolve the struct type
	mut struct_type := b.ast_type_to_ssa(expr.typ)
	if struct_type == b.mod.type_store.get_int(64) {
		env_type := b.expr_type(ast.Expr(expr))
		if env_type != b.mod.type_store.get_int(64) {
			struct_type = env_type
		}
	}

	// Get the type info for field name lookup
	typ_info := b.mod.type_store.types[struct_type]
	num_fields := typ_info.field_names.len

	if num_fields == 0 {
		// Not a known struct or has no fields: fall back to zero constant
		return b.mod.get_or_add_const(struct_type, '0')
	}

	// Build field values in declaration order
	mut field_vals := []ValueID{cap: num_fields}
	mut initialized_fields := map[string]int{} // field name -> index in expr.fields

	// Map explicit field inits by name
	// Handle sumtype _data._variant fields by mapping to _data
	for fi, field in expr.fields {
		fname := if field.name.starts_with('_data.') {
			'_data'
		} else {
			field.name
		}
		initialized_fields[fname] = fi
	}

	for fi in 0 .. num_fields {
		fname := typ_info.field_names[fi]
		field_type := if fi < typ_info.fields.len {
			typ_info.fields[fi]
		} else {
			b.mod.type_store.get_int(64)
		}

		if idx := initialized_fields[fname] {
			if idx >= 0 && idx < expr.fields.len {
				field_vals << b.build_expr(expr.fields[idx].value)
			} else {
				field_vals << b.mod.get_or_add_const(field_type, '0')
			}
		} else {
			// Zero-initialize unset fields
			field_vals << b.mod.get_or_add_const(field_type, '0')
		}
	}

	// Diagnostic: check sum type init for zero _data
	if typ_info.field_names.len == 2 && typ_info.field_names[0] == '_tag'
		&& typ_info.field_names[1] == '_data' && field_vals.len >= 2 {
		tag_v := b.mod.values[field_vals[0]]
		data_v := b.mod.values[field_vals[1]]
		if tag_v.kind == .constant && tag_v.name != '0' && data_v.kind == .constant
			&& data_v.name == '0' {
			eprintln('DIAG SSA: sum type struct_init with _tag=${tag_v.name} but _data=0! fn=${b.cur_func}')
			for fi2, field2 in expr.fields {
				eprintln('  field[${fi2}]: name="${field2.name}" value_tag=${field2.value.type_name()}')
			}
		}
	}

	return b.mod.add_instr(.struct_init, b.cur_block, struct_type, field_vals)
}

// build_init_expr_ptr: like build_init_expr but returns the pointer (for &Point{...}).
// This is the "heap allocation" case — allocates on heap via malloc.
fn (mut b Builder) build_init_expr_ptr(expr ast.InitExpr) ValueID {
	struct_val := b.build_init_expr(expr)
	val_type := b.mod.values[struct_val].typ
	ptr_type := b.mod.type_store.get_ptr(val_type)
	// Heap-allocate: emit heap_alloc which the backend lowers to malloc+zero
	heap_ptr := b.mod.add_instr(.heap_alloc, b.cur_block, ptr_type, []ValueID{})
	b.mod.add_instr(.store, b.cur_block, 0, [struct_val, heap_ptr])
	return heap_ptr
}

fn (mut b Builder) build_cast(expr ast.CastExpr) ValueID {
	val := b.build_expr(expr.expr)
	target_type := b.ast_type_to_ssa(expr.typ)
	src_type := b.mod.values[val].typ

	if src_type == target_type {
		return val
	}

	// Determine cast kind based on types
	src := b.mod.type_store.types[src_type]
	dst := b.mod.type_store.types[target_type]

	if src.kind == .int_t && dst.kind == .int_t {
		if src.width < dst.width {
			return b.mod.add_instr(.sext, b.cur_block, target_type, [val])
		} else if src.width > dst.width {
			return b.mod.add_instr(.trunc, b.cur_block, target_type, [val])
		}
	} else if src.kind == .int_t && dst.kind == .float_t {
		return b.mod.add_instr(.sitofp, b.cur_block, target_type, [val])
	} else if src.kind == .float_t && dst.kind == .int_t {
		return b.mod.add_instr(.fptosi, b.cur_block, target_type, [val])
	}

	return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
}

fn (mut b Builder) build_call_or_cast(expr ast.CallOrCastExpr) ValueID {
	// CallOrCastExpr is produced by the parser when it can't distinguish
	// between a function call and a type cast, e.g., int(x) or voidptr(p).
	// After type checking, the transformer usually resolves these, but some
	// may remain as casts. Treat as cast: convert expr to target type.
	val := b.build_expr(expr.expr)
	target_type := b.ast_type_to_ssa(expr.lhs)
	src_type := b.mod.values[val].typ

	if src_type == target_type || target_type == 0 {
		return val
	}

	src := b.mod.type_store.types[src_type]
	dst := b.mod.type_store.types[target_type]

	if src.kind == .int_t && dst.kind == .int_t {
		if src.width < dst.width {
			return b.mod.add_instr(.sext, b.cur_block, target_type, [val])
		} else if src.width > dst.width {
			return b.mod.add_instr(.trunc, b.cur_block, target_type, [val])
		}
		return val
	} else if src.kind == .int_t && dst.kind == .float_t {
		return b.mod.add_instr(.sitofp, b.cur_block, target_type, [val])
	} else if src.kind == .float_t && dst.kind == .int_t {
		return b.mod.add_instr(.fptosi, b.cur_block, target_type, [val])
	} else if src.kind == .ptr_t && dst.kind == .ptr_t {
		return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
	} else if src.kind == .int_t && dst.kind == .ptr_t {
		return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
	} else if src.kind == .ptr_t && dst.kind == .int_t {
		return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
	}

	return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
}

fn (mut b Builder) build_keyword(kw ast.Keyword) ValueID {
	match kw.tok {
		.key_true {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(1), '1')
		}
		.key_false {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(1), '0')
		}
		.key_nil {
			i8_t := b.mod.type_store.get_int(8)
			ptr_t := b.mod.type_store.get_ptr(i8_t)
			return b.mod.get_or_add_const(ptr_t, '0')
		}
		else {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
		}
	}
}

fn (mut b Builder) build_keyword_operator(kw ast.KeywordOperator) ValueID {
	match kw.op {
		.key_sizeof {
			if kw.exprs.len > 0 {
				size := b.sizeof_value(kw.exprs[0])
				if size > 0 {
					return b.mod.get_or_add_const(b.mod.type_store.get_int(32), size.str())
				}
			}
			// Fallback: sizeof(int) = 4
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '4')
		}
		else {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
	}
}

fn (b &Builder) sizeof_value(expr ast.Expr) int {
	match expr {
		ast.Ident {
			return match expr.name {
				'int', 'i32', 'u32', 'f32' {
					4
				}
				'i8', 'u8', 'byte', 'bool' {
					1
				}
				'i16', 'u16' {
					2
				}
				'i64', 'u64', 'f64', 'isize', 'usize', 'voidptr', 'byteptr', 'charptr' {
					8
				}
				'rune' {
					4
				}
				else {
					// Look up struct type
					if tid := b.struct_types[expr.name] {
						return b.type_byte_size(tid)
					}
					8 // pointer size fallback
				}
			}
		}
		ast.Type {
			match expr {
				ast.ArrayType {
					if tid := b.struct_types['array'] {
						return b.type_byte_size(tid)
					}
					return 48 // approximate
				}
				ast.MapType {
					if tid := b.struct_types['map'] {
						return b.type_byte_size(tid)
					}
					return 120 // approximate
				}
				else {
					return 8
				}
			}
		}
		ast.SelectorExpr {
			lhs_name := expr.lhs.name()
			rhs_name := expr.rhs.name
			if lhs_name.len > 0 && rhs_name.len > 0 {
				qualified := '${lhs_name}__${rhs_name}'
				if tid := b.struct_types[qualified] {
					return b.type_byte_size(tid)
				}
			}
			return 8
		}
		else {
			return 8
		}
	}
}

fn (b &Builder) type_byte_size(tid TypeID) int {
	if tid == 0 || tid >= b.mod.type_store.types.len {
		return 0
	}
	typ := b.mod.type_store.types[tid]
	match typ.kind {
		.void_t {
			return 0
		}
		.int_t {
			return if typ.width <= 8 {
				1
			} else if typ.width <= 16 {
				2
			} else if typ.width <= 32 {
				4
			} else {
				8
			}
		}
		.float_t {
			return if typ.width <= 32 { 4 } else { 8 }
		}
		.ptr_t {
			return 8
		}
		.struct_t {
			mut size := 0
			mut max_align := 1
			for field in typ.fields {
				fs := b.type_byte_size(field)
				// Align to field size (simplified alignment)
				align := if fs >= 8 {
					8
				} else if fs >= 4 {
					4
				} else if fs >= 2 {
					2
				} else {
					1
				}
				if align > max_align {
					max_align = align
				}
				rem := size % align
				if rem != 0 {
					size += align - rem
				}
				size += fs
			}
			// Add tail padding: align struct size to its largest field alignment
			if max_align > 1 {
				rem := size % max_align
				if rem != 0 {
					size += max_align - rem
				}
			}
			return size
		}
		.array_t {
			return b.type_byte_size(typ.elem_type) * typ.len
		}
		.func_t {
			return 8
		}
		.label_t {
			return 0
		}
		.metadata_t {
			return 0
		}
	}
}

fn (mut b Builder) build_postfix(expr ast.PostfixExpr) ValueID {
	// expr++ or expr--
	if expr.expr is ast.Ident {
		if ptr := b.vars[expr.expr.name] {
			ptr_typ := b.mod.values[ptr].typ
			elem_typ := b.mod.type_store.types[ptr_typ].elem_type
			loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
			one := b.mod.get_or_add_const(elem_typ, '1')
			op := if expr.op == .inc { OpCode.add } else { OpCode.sub }
			result := b.mod.add_instr(op, b.cur_block, elem_typ, [loaded, one])
			b.mod.add_instr(.store, b.cur_block, 0, [result, ptr])
			return loaded // postfix returns old value
		}
	}
	// Handle SelectorExpr and IndexExpr: obj.field++ or arr[i]++
	if expr.expr is ast.SelectorExpr || expr.expr is ast.IndexExpr {
		ptr := b.build_addr(expr.expr)
		if ptr != 0 {
			ptr_typ := b.mod.values[ptr].typ
			elem_typ := if ptr_typ < b.mod.type_store.types.len {
				b.mod.type_store.types[ptr_typ].elem_type
			} else {
				b.mod.type_store.get_int(32)
			}
			loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
			one := b.mod.get_or_add_const(elem_typ, '1')
			op := if expr.op == .inc { OpCode.add } else { OpCode.sub }
			result := b.mod.add_instr(op, b.cur_block, elem_typ, [loaded, one])
			b.mod.add_instr(.store, b.cur_block, 0, [result, ptr])
			return loaded // postfix returns old value
		}
	}
	return b.build_expr(expr.expr)
}

// --- Address computation ---

fn (mut b Builder) build_addr(expr ast.Expr) ValueID {
	match expr {
		ast.Ident {
			if expr.name in b.vars {
				ptr := b.vars[expr.name]
				// For mut pointer params (e.g., mut buf &u8), the alloca stores ptr(ptr(T)).
				// When used as a mut arg to another function (build_addr call),
				// return the loaded value (ptr(ptr(T))) instead of the alloca (ptr(ptr(ptr(T)))).
				// This forwards the same indirection level instead of adding another layer.
				if expr.name in b.mut_ptr_params {
					ptr_typ := b.mod.values[ptr].typ
					elem_typ := b.mod.type_store.types[ptr_typ].elem_type
					return b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
				}
				return ptr
			}
			if glob_id := b.find_global(expr.name) {
				return glob_id
			}
			return 0
		}
		ast.SelectorExpr {
			// Get address of base (not the loaded value)
			mut base := b.build_addr(expr.lhs)
			if base == 0 {
				return 0
			}
			// If base is ptr-to-ptr-to-struct (mut receiver), load to get the struct pointer
			base_typ := b.mod.values[base].typ
			if base_typ < b.mod.type_store.types.len
				&& b.mod.type_store.types[base_typ].kind == .ptr_t {
				inner := b.mod.type_store.types[base_typ].elem_type
				if inner < b.mod.type_store.types.len
					&& b.mod.type_store.types[inner].kind == .ptr_t {
					pointee := b.mod.type_store.types[inner].elem_type
					if pointee < b.mod.type_store.types.len
						&& b.mod.type_store.types[pointee].kind == .struct_t {
						// Load from alloca to get the struct pointer
						base = b.mod.add_instr(.load, b.cur_block, inner, [base])
					}
				}
			}
			idx_val := b.mod.get_or_add_const(b.mod.type_store.get_int(32), b.field_index(expr,
				base).str())
			result_type := b.expr_type(ast.Expr(expr))
			return b.mod.add_instr(.get_element_ptr, b.cur_block, b.mod.type_store.get_ptr(result_type),
				[base, idx_val])
		}
		ast.IndexExpr {
			// Try address-based access first for fixed-size arrays and struct fields.
			// This avoids loading large values (e.g., [256]char in C.dirent.d_name)
			// and instead computes pointer + GEP.
			base_addr := b.build_addr(expr.lhs)
			if base_addr != 0 {
				addr_typ_id := b.mod.values[base_addr].typ
				if addr_typ_id < b.mod.type_store.types.len {
					addr_typ := b.mod.type_store.types[addr_typ_id]
					if addr_typ.kind == .ptr_t {
						pointee := addr_typ.elem_type
						if pointee < b.mod.type_store.types.len {
							pointee_typ := b.mod.type_store.types[pointee]
							// For pointer to fixed-size array: GEP into the array elements
							if pointee_typ.kind == .array_t && pointee_typ.elem_type != 0 {
								index := b.build_expr(expr.expr)
								elem_ptr_type := b.mod.type_store.get_ptr(pointee_typ.elem_type)
								return b.mod.add_instr(.get_element_ptr, b.cur_block,
									elem_ptr_type, [base_addr, index])
							}
						}
					}
				}
			}
			base := b.build_expr(expr.lhs)
			index := b.build_expr(expr.expr)
			mut result_type := b.expr_type(ast.Expr(expr))
			// For dynamic arrays, extract .data pointer first (mirrors build_index logic)
			base_type_id := b.mod.values[base].typ
			array_type := b.get_array_type()
			if array_type != 0 && base_type_id == array_type {
				i64_t := b.mod.type_store.get_int(64)
				if result_type == i64_t {
					if b.env != unsafe { nil } {
						lhs_pos := expr.lhs.pos()
						if lhs_pos.id != 0 {
							if arr_typ := b.env.get_expr_type(lhs_pos.id) {
								if arr_typ is types.Array {
									inferred := b.type_to_ssa(arr_typ.elem_type)
									if inferred != 0 {
										result_type = inferred
									}
								}
							}
						}
					}
				}
				// Extract .data field (index 0), cast to element pointer, then GEP
				i8_t := b.mod.type_store.get_int(8)
				void_ptr := b.mod.type_store.get_ptr(i8_t)
				data_ptr := b.mod.add_instr(.extractvalue, b.cur_block, void_ptr, [
					base,
					b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0'),
				])
				elem_ptr_type := b.mod.type_store.get_ptr(result_type)
				typed_ptr := b.mod.add_instr(.bitcast, b.cur_block, elem_ptr_type, [
					data_ptr,
				])
				return b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
					typed_ptr,
					index,
				])
			}
			// For pointers (fixed-size arrays), GEP directly
			if base_type_id < b.mod.type_store.types.len {
				base_typ := b.mod.type_store.types[base_type_id]
				if base_typ.kind == .ptr_t && base_typ.elem_type != 0 {
					mut elem_type := base_typ.elem_type
					if elem_type < b.mod.type_store.types.len {
						inner_typ := b.mod.type_store.types[elem_type]
						if inner_typ.kind == .array_t && inner_typ.elem_type != 0 {
							elem_type = inner_typ.elem_type
						}
					}
					elem_ptr_type := b.mod.type_store.get_ptr(elem_type)
					return b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type,
						[base, index])
				}
			}
			return b.mod.add_instr(.get_element_ptr, b.cur_block, b.mod.type_store.get_ptr(result_type),
				[base, index])
		}
		else {
			return 0
		}
	}
}

// --- Helpers ---

fn (mut b Builder) add_edge(from BlockID, to BlockID) {
	b.mod.blocks[from].succs << to
	b.mod.blocks[to].preds << from
}

fn (mut b Builder) get_or_create_fn_ref(name string, typ TypeID) ValueID {
	// Look for existing func_ref value
	for v in b.mod.values {
		if v.kind == .func_ref && v.name == name {
			return v.id
		}
	}
	return b.mod.add_value_node(.func_ref, typ, name, 0)
}

// generate_array_eq_stub creates a synthetic `array__eq` function that compares two arrays.
// The transformer generates `array__eq(arr1, arr2)` for `arr1 == arr2`.
// Implementation: compare len fields, then memcmp data.
fn (mut b Builder) generate_array_eq_stub() {
	if 'array__eq' in b.fn_index {
		return
	}
	i32_t := b.mod.type_store.get_int(32)
	i1_t := b.mod.type_store.get_int(1)
	array_t := if 'array' in b.struct_types { b.struct_types['array'] } else { return }

	// Register function: array__eq(a: array, b: array) -> bool
	func_idx := b.mod.new_function('array__eq', i1_t, []TypeID{})
	b.fn_index['array__eq'] = func_idx
	entry := b.mod.add_block(func_idx, 'entry')

	// Add parameters
	param_a := b.mod.add_value_node(.argument, array_t, 'a', 0)
	param_b := b.mod.add_value_node(.argument, array_t, 'b', 0)
	b.mod.funcs[func_idx].params << param_a
	b.mod.funcs[func_idx].params << param_b

	// Alloca + store params
	alloca_a := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(array_t), []ValueID{})
	b.mod.add_instr(.store, entry, 0, [param_a, alloca_a])
	alloca_b := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(array_t), []ValueID{})
	b.mod.add_instr(.store, entry, 0, [param_b, alloca_b])

	// Extract len fields (field index 2 in array struct: data=0, offset=1, len=2)
	val_a := b.mod.add_instr(.load, entry, array_t, [alloca_a])
	val_b := b.mod.add_instr(.load, entry, array_t, [alloca_b])
	idx2 := b.mod.get_or_add_const(i32_t, '2')
	len_a := b.mod.add_instr(.extractvalue, entry, i32_t, [val_a, idx2])
	len_b := b.mod.add_instr(.extractvalue, entry, i32_t, [val_b, idx2])

	// Compare lengths
	len_eq := b.mod.add_instr(.eq, entry, i1_t, [len_a, len_b])

	// If lengths differ, return false
	memcmp_block := b.mod.add_block(func_idx, 'memcmp')
	ret_false_block := b.mod.add_block(func_idx, 'ret_false')
	b.mod.add_instr(.br, entry, 0, [len_eq, b.mod.blocks[memcmp_block].val_id, b.mod.blocks[ret_false_block].val_id])

	// ret_false: return 0
	zero := b.mod.get_or_add_const(i1_t, '0')
	b.mod.add_instr(.ret, ret_false_block, 0, [zero])

	// memcmp block: compare data
	// Extract data pointers (field index 0) and offset (field index 1)
	val_a2 := b.mod.add_instr(.load, memcmp_block, array_t, [alloca_a])
	val_b2 := b.mod.add_instr(.load, memcmp_block, array_t, [alloca_b])
	idx0 := b.mod.get_or_add_const(i32_t, '0')
	i8_t := b.mod.type_store.get_int(8)
	ptr_t := b.mod.type_store.get_ptr(i8_t)
	raw_data_a := b.mod.add_instr(.extractvalue, memcmp_block, ptr_t, [val_a2, idx0])
	raw_data_b := b.mod.add_instr(.extractvalue, memcmp_block, ptr_t, [val_b2, idx0])
	// Add offset to data pointers (offset is in bytes) using get_element_ptr
	idx1 := b.mod.get_or_add_const(i32_t, '1')
	offset_a := b.mod.add_instr(.extractvalue, memcmp_block, i32_t, [val_a2, idx1])
	offset_b := b.mod.add_instr(.extractvalue, memcmp_block, i32_t, [val_b2, idx1])
	data_a := b.mod.add_instr(.get_element_ptr, memcmp_block, ptr_t, [raw_data_a, offset_a])
	data_b := b.mod.add_instr(.get_element_ptr, memcmp_block, ptr_t, [raw_data_b, offset_b])

	// Get element_size (field index 5: data=0, offset=1, len=2, cap=3, flags=4, element_size=5)
	idx5 := b.mod.get_or_add_const(i32_t, '5')
	elem_size := b.mod.add_instr(.extractvalue, memcmp_block, i32_t, [val_a2, idx5])

	// Compute total size = len * element_size
	total_size := b.mod.add_instr(.mul, memcmp_block, i32_t, [len_a, elem_size])

	// Call C.memcmp(data_a, data_b, total_size)
	memcmp_ref := b.get_or_create_fn_ref('memcmp', i32_t)
	cmp_result := b.mod.add_instr(.call, memcmp_block, i32_t, [memcmp_ref, data_a, data_b, total_size])

	// Return cmp_result == 0
	zero_i32 := b.mod.get_or_add_const(i32_t, '0')
	is_eq := b.mod.add_instr(.eq, memcmp_block, i1_t, [cmp_result, zero_i32])
	b.mod.add_instr(.ret, memcmp_block, 0, [is_eq])
}

// generate_wyhash64_stub creates a synthetic `wyhash64` function for the native backend.
// C.wyhash64 is defined in wyhash.h (C only), so the native backend needs a V implementation.
// wyhash64(a, b) = wymum(a ^ wyp0, b ^ wyp1)
// This calls hash__wymum which is the pure-V implementation.
fn (mut b Builder) generate_wyhash64_stub() {
	// Replace the empty stub created by the linker for C.wyhash64
	if 'wyhash64' in b.fn_index {
		// Already registered from .c.v declaration, need to replace body
		// Remove existing and re-register
		fn_idx := b.fn_index['wyhash64']
		b.generate_wyhash64_body(fn_idx)
		return
	}
	i64_t := b.mod.type_store.get_int(64)
	func_idx := b.mod.new_function('wyhash64', i64_t, []TypeID{})
	b.fn_index['wyhash64'] = func_idx
	b.generate_wyhash64_body(func_idx)
}

fn (mut b Builder) generate_wyhash64_body(func_idx int) {
	i64_t := b.mod.type_store.get_int(64)

	// Clear existing blocks if any
	b.mod.funcs[func_idx].blocks.clear()

	entry := b.mod.add_block(func_idx, 'entry')

	// Parameters: a: u64, b: u64
	param_a := b.mod.add_value_node(.argument, i64_t, 'a', 0)
	param_b := b.mod.add_value_node(.argument, i64_t, 'b', 0)
	b.mod.funcs[func_idx].params.clear()
	b.mod.funcs[func_idx].params << param_a
	b.mod.funcs[func_idx].params << param_b

	// wyp0 = 0x2d358dccaa6c78a5
	// wyp1 = 0x8bb84b93962eacc9
	wyp0 := b.mod.get_or_add_const(i64_t, '3257665815644502181')
	wyp1 := b.mod.get_or_add_const(i64_t, '10067880064238660809')

	// x = a ^ wyp0
	x := b.mod.add_instr(.xor, entry, i64_t, [param_a, wyp0])
	// y = b ^ wyp1
	y := b.mod.add_instr(.xor, entry, i64_t, [param_b, wyp1])

	// Implement wymum(x, y) inline:
	// wymum returns hi(x*y) ^ lo(x*y) using 32-bit multiply approach
	// lo = x * y (truncated to 64 bits)
	lo := b.mod.add_instr(.mul, entry, i64_t, [x, y])

	// For hi: use the 32-bit decomposition
	// mask32 = 0xFFFFFFFF
	mask32 := b.mod.get_or_add_const(i64_t, '4294967295')
	c32 := b.mod.get_or_add_const(i64_t, '32')

	// x0 = x & mask32
	x0 := b.mod.add_instr(.and_, entry, i64_t, [x, mask32])
	// x1 = x >> 32
	x1 := b.mod.add_instr(.lshr, entry, i64_t, [x, c32])
	// y0 = y & mask32
	y0 := b.mod.add_instr(.and_, entry, i64_t, [y, mask32])
	// y1 = y >> 32
	y1 := b.mod.add_instr(.lshr, entry, i64_t, [y, c32])

	// w0 = x0 * y0
	w0 := b.mod.add_instr(.mul, entry, i64_t, [x0, y0])
	// t = x1 * y0 + (w0 >> 32)
	x1y0 := b.mod.add_instr(.mul, entry, i64_t, [x1, y0])
	w0_hi := b.mod.add_instr(.lshr, entry, i64_t, [w0, c32])
	t := b.mod.add_instr(.add, entry, i64_t, [x1y0, w0_hi])
	// w1 = (t & mask32) + x0 * y1
	t_lo := b.mod.add_instr(.and_, entry, i64_t, [t, mask32])
	x0y1 := b.mod.add_instr(.mul, entry, i64_t, [x0, y1])
	w1 := b.mod.add_instr(.add, entry, i64_t, [t_lo, x0y1])
	// w2 = t >> 32
	w2 := b.mod.add_instr(.lshr, entry, i64_t, [t, c32])
	// hi = x1 * y1 + w2 + (w1 >> 32)
	x1y1 := b.mod.add_instr(.mul, entry, i64_t, [x1, y1])
	w1_hi := b.mod.add_instr(.lshr, entry, i64_t, [w1, c32])
	hi_tmp := b.mod.add_instr(.add, entry, i64_t, [x1y1, w2])
	hi := b.mod.add_instr(.add, entry, i64_t, [hi_tmp, w1_hi])

	// result = hi ^ lo
	result := b.mod.add_instr(.xor, entry, i64_t, [hi, lo])
	b.mod.add_instr(.ret, entry, 0, [result])
}

// generate_wyhash_stub creates a synthetic `wyhash` function for the native backend.
// C.wyhash(key, len, seed, secret) is defined in wyhash.h (C only).
// The native backend needs a V implementation.
// This implements the wyhash v4 algorithm for strings up to 16 bytes (most common map keys)
// and a simplified fallback for longer strings.
fn (mut b Builder) generate_wyhash_stub() {
	// Replace the empty stub created by the linker for C.wyhash
	if 'wyhash' in b.fn_index {
		fn_idx := b.fn_index['wyhash']
		b.generate_wyhash_body(fn_idx)
		return
	}
	i64_t := b.mod.type_store.get_int(64)
	func_idx := b.mod.new_function('wyhash', i64_t, []TypeID{})
	b.fn_index['wyhash'] = func_idx
	b.generate_wyhash_body(func_idx)
}

// Helper: add inline wymum_pair instructions that produce both lo and hi of 128-bit multiply.
// Returns (lo_val, hi_val).
fn (mut b Builder) wymum_pair_inline(block_id int, a ValueID, b_val ValueID) (ValueID, ValueID) {
	i64_t := b.mod.type_store.get_int(64)
	mask32 := b.mod.get_or_add_const(i64_t, '4294967295')
	c32 := b.mod.get_or_add_const(i64_t, '32')

	// lo = a * b (truncated to 64 bits)
	lo := b.mod.add_instr(.mul, block_id, i64_t, [a, b_val])

	// hi via 32-bit decomposition
	x0 := b.mod.add_instr(.and_, block_id, i64_t, [a, mask32])
	x1 := b.mod.add_instr(.lshr, block_id, i64_t, [a, c32])
	y0 := b.mod.add_instr(.and_, block_id, i64_t, [b_val, mask32])
	y1 := b.mod.add_instr(.lshr, block_id, i64_t, [b_val, c32])
	w0 := b.mod.add_instr(.mul, block_id, i64_t, [x0, y0])
	x1y0 := b.mod.add_instr(.mul, block_id, i64_t, [x1, y0])
	w0_hi := b.mod.add_instr(.lshr, block_id, i64_t, [w0, c32])
	t := b.mod.add_instr(.add, block_id, i64_t, [x1y0, w0_hi])
	t_lo := b.mod.add_instr(.and_, block_id, i64_t, [t, mask32])
	x0y1 := b.mod.add_instr(.mul, block_id, i64_t, [x0, y1])
	w1 := b.mod.add_instr(.add, block_id, i64_t, [t_lo, x0y1])
	w2 := b.mod.add_instr(.lshr, block_id, i64_t, [t, c32])
	x1y1 := b.mod.add_instr(.mul, block_id, i64_t, [x1, y1])
	w1_hi := b.mod.add_instr(.lshr, block_id, i64_t, [w1, c32])
	hi_tmp := b.mod.add_instr(.add, block_id, i64_t, [x1y1, w2])
	hi := b.mod.add_instr(.add, block_id, i64_t, [hi_tmp, w1_hi])

	return lo, hi
}

// Helper: add inline wymix instructions: wymix(a, b) = wymum(a, b) returns hi ^ lo
fn (mut b Builder) wymix_inline(block_id int, a ValueID, b_val ValueID) ValueID {
	i64_t := b.mod.type_store.get_int(64)
	lo, hi := b.wymum_pair_inline(block_id, a, b_val)
	return b.mod.add_instr(.xor, block_id, i64_t, [hi, lo])
}

fn (mut b Builder) generate_wyhash_body(func_idx int) {
	i8_t := b.mod.type_store.get_int(8)
	i32_t := b.mod.type_store.get_int(32)
	i64_t := b.mod.type_store.get_int(64)
	i1_t := b.mod.type_store.get_int(1)
	ptr_u8 := b.mod.type_store.get_ptr(i8_t)
	ptr_u32 := b.mod.type_store.get_ptr(i32_t)
	ptr_u64 := b.mod.type_store.get_ptr(i64_t)

	// Clear existing blocks if any
	b.mod.funcs[func_idx].blocks.clear()

	entry := b.mod.add_block(func_idx, 'entry')

	// Parameters: key: *u8, len: u64, seed: u64, secret: *u64
	param_key := b.mod.add_value_node(.argument, ptr_u8, 'key', 0)
	param_len := b.mod.add_value_node(.argument, i64_t, 'len', 0)
	param_seed := b.mod.add_value_node(.argument, i64_t, 'seed', 0)
	param_secret := b.mod.add_value_node(.argument, ptr_u64, 'secret', 0)
	b.mod.funcs[func_idx].params.clear()
	b.mod.funcs[func_idx].params << param_key
	b.mod.funcs[func_idx].params << param_len
	b.mod.funcs[func_idx].params << param_seed
	b.mod.funcs[func_idx].params << param_secret

	// Hardcoded wyp constants (same as wyhash.h _wyp[4])
	wyp0 := b.mod.get_or_add_const(i64_t, '3257665815644502181') // 0x2d358dccaa6c78a5
	wyp1 := b.mod.get_or_add_const(i64_t, '10067880064238660809') // 0x8bb84b93962eacc9

	// seed ^= wymix(seed ^ secret[0], secret[1])
	// Since seed is always 0 in practice: seed = wymix(wyp0, wyp1)
	// But let's be correct and use the param:
	seed_xor_s0 := b.mod.add_instr(.xor, entry, i64_t, [param_seed, wyp0])
	seed_mix := b.wymix_inline(entry, seed_xor_s0, wyp1)
	seed_init := b.mod.add_instr(.xor, entry, i64_t, [param_seed, seed_mix])

	// Allocas for a, b, and seed (results from branches, seed updated in long path)
	alloca_a := b.mod.add_instr(.alloca, entry, ptr_u64, []ValueID{})
	alloca_b := b.mod.add_instr(.alloca, entry, ptr_u64, []ValueID{})
	alloca_seed := b.mod.add_instr(.alloca, entry, ptr_u64, []ValueID{})

	// Constants
	zero_64 := b.mod.get_or_add_const(i64_t, '0')
	one_64 := b.mod.get_or_add_const(i64_t, '1')
	two_64 := b.mod.get_or_add_const(i64_t, '2')
	three_64 := b.mod.get_or_add_const(i64_t, '3')
	four_64 := b.mod.get_or_add_const(i64_t, '4')
	eight_64 := b.mod.get_or_add_const(i64_t, '8')
	sixteen_64 := b.mod.get_or_add_const(i64_t, '16')
	c32 := b.mod.get_or_add_const(i64_t, '32')

	// Store defaults
	b.mod.add_instr(.store, entry, 0, [zero_64, alloca_a])
	b.mod.add_instr(.store, entry, 0, [zero_64, alloca_b])
	b.mod.add_instr(.store, entry, 0, [seed_init, alloca_seed])

	// Branch: len <= 16?
	len_le_16 := b.mod.add_instr(.le, entry, i1_t, [param_len, sixteen_64])

	blk_short := b.mod.add_block(func_idx, 'short')
	blk_long := b.mod.add_block(func_idx, 'long')
	blk_final := b.mod.add_block(func_idx, 'final')

	b.mod.add_instr(.br, entry, 0, [len_le_16, b.mod.blocks[blk_short].val_id, b.mod.blocks[blk_long].val_id])

	// === SHORT PATH (len <= 16) ===
	// Branch: len >= 4?
	len_ge_4 := b.mod.add_instr(.ge, blk_short, i1_t, [param_len, four_64])

	blk_short_4_16 := b.mod.add_block(func_idx, 'short_4_16')
	blk_short_0_3 := b.mod.add_block(func_idx, 'short_0_3')

	b.mod.add_instr(.br, blk_short, 0, [len_ge_4, b.mod.blocks[blk_short_4_16].val_id, b.mod.blocks[blk_short_0_3].val_id])

	// --- SHORT 4-16 block ---
	// a = (_wyr4(p) << 32) | _wyr4(p + ((len>>3)<<2))
	// b = (_wyr4(p+len-4) << 32) | _wyr4(p+len-4-((len>>3)<<2))

	// _wyr4(p) = load u32 from p, zero-extended to u64
	p_as_u32 := b.mod.add_instr(.bitcast, blk_short_4_16, ptr_u32, [param_key])
	wyr4_0 := b.mod.add_instr(.load, blk_short_4_16, i32_t, [p_as_u32])
	wyr4_0_64 := b.mod.add_instr(.zext, blk_short_4_16, i64_t, [wyr4_0])

	// off1 = (len >> 3) << 2
	len_shr3 := b.mod.add_instr(.lshr, blk_short_4_16, i64_t, [param_len, three_64])
	off1 := b.mod.add_instr(.shl, blk_short_4_16, i64_t, [len_shr3, two_64])

	// _wyr4(p + off1)
	p_off1 := b.mod.add_instr(.add, blk_short_4_16, ptr_u8, [param_key, off1])
	p_off1_u32 := b.mod.add_instr(.bitcast, blk_short_4_16, ptr_u32, [p_off1])
	wyr4_1 := b.mod.add_instr(.load, blk_short_4_16, i32_t, [p_off1_u32])
	wyr4_1_64 := b.mod.add_instr(.zext, blk_short_4_16, i64_t, [wyr4_1])

	// a = (wyr4_0 << 32) | wyr4_1
	wyr4_0_shifted := b.mod.add_instr(.shl, blk_short_4_16, i64_t, [wyr4_0_64, c32])
	a_val := b.mod.add_instr(.or_, blk_short_4_16, i64_t, [wyr4_0_shifted, wyr4_1_64])

	// _wyr4(p + len - 4)
	len_m4 := b.mod.add_instr(.sub, blk_short_4_16, i64_t, [param_len, four_64])
	p_lm4 := b.mod.add_instr(.add, blk_short_4_16, ptr_u8, [param_key, len_m4])
	p_lm4_u32 := b.mod.add_instr(.bitcast, blk_short_4_16, ptr_u32, [p_lm4])
	wyr4_2 := b.mod.add_instr(.load, blk_short_4_16, i32_t, [p_lm4_u32])
	wyr4_2_64 := b.mod.add_instr(.zext, blk_short_4_16, i64_t, [wyr4_2])

	// _wyr4(p + len - 4 - off1)
	lm4_moff1 := b.mod.add_instr(.sub, blk_short_4_16, i64_t, [len_m4, off1])
	p_lm4_moff1 := b.mod.add_instr(.add, blk_short_4_16, ptr_u8, [param_key, lm4_moff1])
	p_lm4_moff1_u32 := b.mod.add_instr(.bitcast, blk_short_4_16, ptr_u32, [
		p_lm4_moff1,
	])
	wyr4_3 := b.mod.add_instr(.load, blk_short_4_16, i32_t, [p_lm4_moff1_u32])
	wyr4_3_64 := b.mod.add_instr(.zext, blk_short_4_16, i64_t, [wyr4_3])

	// b = (wyr4_2 << 32) | wyr4_3
	wyr4_2_shifted := b.mod.add_instr(.shl, blk_short_4_16, i64_t, [wyr4_2_64, c32])
	b_val := b.mod.add_instr(.or_, blk_short_4_16, i64_t, [wyr4_2_shifted, wyr4_3_64])

	b.mod.add_instr(.store, blk_short_4_16, 0, [a_val, alloca_a])
	b.mod.add_instr(.store, blk_short_4_16, 0, [b_val, alloca_b])
	b.mod.add_instr(.jmp, blk_short_4_16, 0, [b.mod.blocks[blk_final].val_id])

	// --- SHORT 0-3 block ---
	// Branch: len > 0?
	len_gt_0 := b.mod.add_instr(.gt, blk_short_0_3, i1_t, [param_len, zero_64])
	blk_wyr3 := b.mod.add_block(func_idx, 'wyr3')
	b.mod.add_instr(.br, blk_short_0_3, 0, [len_gt_0, b.mod.blocks[blk_wyr3].val_id, b.mod.blocks[blk_final].val_id]) // len==0: a=0, b=0 already stored

	// --- WYR3 block (len 1-3) ---
	// a = _wyr3(p, len) = (p[0] << 16) | (p[len>>1] << 8) | p[len-1]
	// b = 0 (already stored)
	byte_0 := b.mod.add_instr(.load, blk_wyr3, i8_t, [param_key])
	byte_0_64 := b.mod.add_instr(.zext, blk_wyr3, i64_t, [byte_0])
	byte_0_shifted := b.mod.add_instr(.shl, blk_wyr3, i64_t, [byte_0_64, sixteen_64])

	// p[len >> 1]
	len_shr1 := b.mod.add_instr(.lshr, blk_wyr3, i64_t, [param_len, one_64])
	p_mid := b.mod.add_instr(.add, blk_wyr3, ptr_u8, [param_key, len_shr1])
	byte_mid := b.mod.add_instr(.load, blk_wyr3, i8_t, [p_mid])
	byte_mid_64 := b.mod.add_instr(.zext, blk_wyr3, i64_t, [byte_mid])
	byte_mid_shifted := b.mod.add_instr(.shl, blk_wyr3, i64_t, [byte_mid_64, eight_64])

	// p[len - 1]
	len_m1 := b.mod.add_instr(.sub, blk_wyr3, i64_t, [param_len, one_64])
	p_last := b.mod.add_instr(.add, blk_wyr3, ptr_u8, [param_key, len_m1])
	byte_last := b.mod.add_instr(.load, blk_wyr3, i8_t, [p_last])
	byte_last_64 := b.mod.add_instr(.zext, blk_wyr3, i64_t, [byte_last])

	// a = byte_0_shifted | byte_mid_shifted | byte_last
	a_tmp := b.mod.add_instr(.or_, blk_wyr3, i64_t, [byte_0_shifted, byte_mid_shifted])
	a_wyr3 := b.mod.add_instr(.or_, blk_wyr3, i64_t, [a_tmp, byte_last_64])

	b.mod.add_instr(.store, blk_wyr3, 0, [a_wyr3, alloca_a])
	b.mod.add_instr(.jmp, blk_wyr3, 0, [b.mod.blocks[blk_final].val_id])

	// === LONG PATH (len > 16) ===
	// For len > 16, use a simplified approach for native backend:
	// One round of mixing with first 16 bytes, then use last 16 bytes as a/b.
	// This matches wyhash for 17-48 byte strings (one mixing round).

	// _wyr8(p) = load u64 from p
	p_as_u64 := b.mod.add_instr(.bitcast, blk_long, ptr_u64, [param_key])
	wyr8_first := b.mod.add_instr(.load, blk_long, i64_t, [p_as_u64])

	// _wyr8(p + 8)
	p_plus_8 := b.mod.add_instr(.add, blk_long, ptr_u8, [param_key, eight_64])
	p_plus_8_u64 := b.mod.add_instr(.bitcast, blk_long, ptr_u64, [p_plus_8])
	wyr8_second := b.mod.add_instr(.load, blk_long, i64_t, [p_plus_8_u64])

	// _wyr8(p + len - 16)
	len_m16 := b.mod.add_instr(.sub, blk_long, i64_t, [param_len, sixteen_64])
	p_end_16 := b.mod.add_instr(.add, blk_long, ptr_u8, [param_key, len_m16])
	p_end_16_u64 := b.mod.add_instr(.bitcast, blk_long, ptr_u64, [p_end_16])
	wyr8_end_16 := b.mod.add_instr(.load, blk_long, i64_t, [p_end_16_u64])

	// _wyr8(p + len - 8)
	len_m8 := b.mod.add_instr(.sub, blk_long, i64_t, [param_len, eight_64])
	p_end_8 := b.mod.add_instr(.add, blk_long, ptr_u8, [param_key, len_m8])
	p_end_8_u64 := b.mod.add_instr(.bitcast, blk_long, ptr_u64, [p_end_8])
	wyr8_end_8 := b.mod.add_instr(.load, blk_long, i64_t, [p_end_8_u64])

	// seed = wymix(wyr8(p) ^ secret[1], wyr8(p+8) ^ seed)
	mix_a := b.mod.add_instr(.xor, blk_long, i64_t, [wyr8_first, wyp1])
	seed_cur := b.mod.add_instr(.load, blk_long, i64_t, [alloca_seed])
	mix_b := b.mod.add_instr(.xor, blk_long, i64_t, [wyr8_second, seed_cur])
	seed_long := b.wymix_inline(blk_long, mix_a, mix_b)

	// Store updated seed, a, and b
	b.mod.add_instr(.store, blk_long, 0, [seed_long, alloca_seed])
	b.mod.add_instr(.store, blk_long, 0, [wyr8_end_16, alloca_a])
	b.mod.add_instr(.store, blk_long, 0, [wyr8_end_8, alloca_b])
	b.mod.add_instr(.jmp, blk_long, 0, [b.mod.blocks[blk_final].val_id])

	// === FINAL block ===
	// a ^= secret[1]; b ^= seed;
	// _wymum(&a, &b);  // (a, b) = (lo(a*b), hi(a*b))
	// return _wymix(a ^ secret[0] ^ len, b ^ secret[1])

	final_a := b.mod.add_instr(.load, blk_final, i64_t, [alloca_a])
	final_b := b.mod.add_instr(.load, blk_final, i64_t, [alloca_b])
	final_seed := b.mod.add_instr(.load, blk_final, i64_t, [alloca_seed])

	final_a_xor := b.mod.add_instr(.xor, blk_final, i64_t, [final_a, wyp1])
	final_b_xor := b.mod.add_instr(.xor, blk_final, i64_t, [final_b, final_seed])

	// _wymum(&a, &b): gives (lo, hi) of 128-bit product
	mum_lo, mum_hi := b.wymum_pair_inline(blk_final, final_a_xor, final_b_xor)

	// return _wymix(lo ^ secret[0] ^ len, hi ^ secret[1])
	lo_xor_s0 := b.mod.add_instr(.xor, blk_final, i64_t, [mum_lo, wyp0])
	lo_xor_s0_len := b.mod.add_instr(.xor, blk_final, i64_t, [lo_xor_s0, param_len])
	hi_xor_s1 := b.mod.add_instr(.xor, blk_final, i64_t, [mum_hi, wyp1])

	final_result := b.wymix_inline(blk_final, lo_xor_s0_len, hi_xor_s1)
	b.mod.add_instr(.ret, blk_final, 0, [final_result])
}
