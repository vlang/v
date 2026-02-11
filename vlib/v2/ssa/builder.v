// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module ssa

import v2.ast
import v2.markused
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
	// Function-local label target blocks for goto/label lowering.
	label_blocks map[string]BlockID

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
	// Maps variable/selector key to dynamic array element type (for indexing loads/stores)
	var_array_elem_types map[string]TypeID

	// Maps variable name to map type info (key_bytes, value_bytes)
	var_map_types map[string][2]int // Variable name -> [key_bytes, value_bytes]
	// Maps variable name to map value type (for map indexing loads)
	var_map_value_types map[string]TypeID

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

	// Module tracking for types (type_name -> module_name)
	type_modules map[string]string

	// Sum type variant mapping: SumTypeName -> ordered variant TypeIDs.
	sumtype_variants map[string][]TypeID
	// Sum type variant ASTs captured during registration and resolved once
	// all structs/enums are registered.
	sumtype_variant_exprs map[string][]ast.Expr

	// Used-function keys from markused stage.
	used_fn_keys map[string]bool

	// Ordered global initializers emitted into a synthetic init function.
	global_init_entries []GlobalInitEntry
	in_global_init      bool
}

struct LoopInfo {
	head BlockID
	exit BlockID
}

struct MapDeclInfo {
	key_bytes   int
	value_bytes int
	value_type  TypeID
}

struct GlobalInitEntry {
	target ValueID
	expr   ast.Expr
}

pub fn Builder.new(mod &Module) &Builder {
	return Builder.new_with_env(mod, unsafe { nil })
}

pub fn Builder.new_with_env(mod &Module, env &types.Environment) &Builder {
	mut b := &Builder{
		mod:                   mod
		vars:                  map[string]ValueID{}
		var_struct_types:      map[string]string{}
		loop_stack:            []LoopInfo{}
		label_blocks:          map[string]BlockID{}
		struct_types:          map[string]TypeID{}
		enum_values:           map[string]int{}
		var_array_sizes:       map[string]int{}
		var_array_elem_types:  map[string]TypeID{}
		var_map_types:         map[string][2]int{}
		var_map_value_types:   map[string]TypeID{}
		interface_names:       map[string]bool{}
		interface_meths:       map[string][]string{}
		type_methods:          map[string][]string{}
		iface_concrete_types:  map[string]string{}
		var_types:             map[string]types.Type{}
		func_ret_types:        map[string]TypeID{}
		fn_type_aliases:       map[string]bool{}
		fn_ptr_fields:         map[string]bool{}
		type_alias_bases:      map[string]string{}
		enum_names:            map[string]bool{}
		flag_enum_names:       map[string]bool{}
		type_modules:          map[string]string{}
		sumtype_variants:      map[string][]TypeID{}
		sumtype_variant_exprs: map[string][]ast.Expr{}
		used_fn_keys:          map[string]bool{}
		global_init_entries:   []GlobalInitEntry{}
		in_global_init:        false
	}
	unsafe {
		b.env = env
		// Also store environment on the module for backends to use
		mod.env = env
	}
	return b
}

// set_used_fn_keys sets the declaration keys that should be emitted.
pub fn (mut b Builder) set_used_fn_keys(used map[string]bool) {
	b.used_fn_keys = used.clone()
}

fn (b &Builder) should_emit_fn(file ast.File, decl ast.FnDecl) bool {
	if b.used_fn_keys.len == 0 {
		return true
	}
	// Keep all user/main functions to avoid missing local method bodies
	// when markused misses transformed call edges.
	if file.mod == 'main' {
		return true
	}
	if is_builtin_array_file(file.name) && should_keep_builtin_array_decl(decl) {
		return true
	}
	if is_builtin_int_file(file.name) && should_keep_builtin_int_decl(decl) {
		return true
	}
	if is_builtin_map_file(file.name) && should_keep_builtin_map_decl(decl) {
		return true
	}
	if should_always_emit_for_markused(file.name) {
		return true
	}
	key := markused.decl_key(file.mod, decl, b.env)
	return key in b.used_fn_keys
}

fn is_builtin_array_file(path string) bool {
	normalized := path.replace('\\', '/')
	return normalized.ends_with('vlib/builtin/array.v')
		|| normalized.ends_with('vlib/builtin/array_notd_gcboehm_opt.v')
		|| normalized.ends_with('vlib/builtin/array_d_gcboehm_opt.v')
}

fn should_keep_builtin_array_decl(decl ast.FnDecl) bool {
	return decl.name in ['sort', 'grow_len', 'delete', 'clear', 'prepend', 'insert', '__new_array',
		'new_array_from_c_array', 'new_array_from_c_array_noscan', 'new_array_from_c_array_no_alloc',
		'push_many', 'push_many_noscan', 'delete_many', 'contains', 'bytestr']
}

fn is_builtin_int_file(path string) bool {
	normalized := path.replace('\\', '/')
	return normalized.ends_with('vlib/builtin/int.v')
}

fn should_keep_builtin_int_decl(decl ast.FnDecl) bool {
	return decl.name == 'bytestr'
}

fn is_builtin_map_file(path string) bool {
	normalized := path.replace('\\', '/')
	return normalized.ends_with('vlib/builtin/map.v')
}

fn should_keep_builtin_map_decl(decl ast.FnDecl) bool {
	base_keep := decl.name in ['new_map', 'move', 'clear', 'key_to_index', 'meta_less',
		'meta_greater', 'ensure_extra_metas', 'set', 'expand', 'rehash', 'reserve', 'cached_rehash',
		'get_and_set', 'get', 'get_check', 'exists', 'delete', 'keys', 'values', 'clone', 'free',
		'key', 'value', 'has_index', 'zeros_to_end', 'str']
	return base_keep || decl.name.starts_with('map_eq_') || decl.name.starts_with('map_clone_')
		|| decl.name.starts_with('map_free_')
}

fn should_always_emit_for_markused(path string) bool {
	if path.ends_with('.vh') {
		return true
	}
	normalized := path.replace('\\', '/')
	if normalized.contains('/vlib/') && !normalized.contains('/vlib/v/') {
		return true
	}
	return is_builtin_runtime_keep_file(path)
}

fn is_builtin_runtime_keep_file(path string) bool {
	normalized := path.replace('\\', '/')
	return normalized.ends_with('vlib/builtin/map.v')
		|| normalized.ends_with('vlib/builtin/map.c.v')
		|| normalized.ends_with('vlib/builtin/builtin.c.v')
		|| normalized.ends_with('vlib/builtin/panicing.c.v')
		|| normalized.ends_with('vlib/builtin/chan_option_result.v')
		|| normalized.ends_with('vlib/os/os.c.v')
}

fn map_has_key_int(m map[string]int, key string) bool {
	for k, _ in m {
		if k == key {
			return true
		}
	}
	return false
}

fn map_has_key_bool(m map[string]bool, key string) bool {
	for k, _ in m {
		if k == key {
			return true
		}
	}
	return false
}

fn map_has_key_type_id(m map[string]TypeID, key string) bool {
	for k, _ in m {
		if k == key {
			return true
		}
	}
	return false
}

fn map_has_key_value_id(m map[string]ValueID, key string) bool {
	for k, _ in m {
		if k == key {
			return true
		}
	}
	return false
}

fn map_has_key_string(m map[string]string, key string) bool {
	for k, _ in m {
		if k == key {
			return true
		}
	}
	return false
}

fn map_has_key_map_bytes(m map[string][2]int, key string) bool {
	for k, _ in m {
		if k == key {
			return true
		}
	}
	return false
}

fn map_has_key_block_id(m map[string]BlockID, key string) bool {
	for k, _ in m {
		if k == key {
			return true
		}
	}
	return false
}

fn map_get_int(m map[string]int, key string) ?int {
	for k, v in m {
		if k == key {
			return v
		}
	}
	return none
}

fn map_get_type_id(m map[string]TypeID, key string) ?TypeID {
	for k, v in m {
		if k == key {
			return v
		}
	}
	return none
}

fn map_get_value_id(m map[string]ValueID, key string) ?ValueID {
	for k, v in m {
		if k == key {
			return v
		}
	}
	return none
}

fn map_get_string(m map[string]string, key string) ?string {
	for k, v in m {
		if k == key {
			return v
		}
	}
	return none
}

fn (b &Builder) map_type_from_type_expr(type_expr ast.Expr) ?ast.MapType {
	match type_expr {
		ast.Type {
			if type_expr is ast.MapType {
				return type_expr
			}
		}
		else {}
	}
	return none
}

fn (mut b Builder) map_decl_info_from_expr(expr ast.Expr) ?MapDeclInfo {
	mut map_type := ast.MapType{}
	match expr {
		ast.MapInitExpr {
			if mt := b.map_type_from_type_expr(expr.typ) {
				map_type = mt
			} else {
				return none
			}
		}
		ast.InitExpr {
			if mt := b.map_type_from_type_expr(expr.typ) {
				map_type = mt
			} else {
				return none
			}
		}
		else {
			return none
		}
	}
	return MapDeclInfo{
		key_bytes:   b.type_size_from_ast(map_type.key_type)
		value_bytes: b.type_size_from_ast(map_type.value_type)
		value_type:  b.ast_type_to_ssa(map_type.value_type)
	}
}

fn (mut b Builder) map_decl_info_from_type(typ types.Type) ?MapDeclInfo {
	match typ {
		types.Map {
			key_type := b.type_to_ssa(typ.key_type)
			value_type := b.type_to_ssa(typ.value_type)
			mut key_bytes := b.type_size_from_ssa_type(key_type)
			mut value_bytes := b.type_size_from_ssa_type(value_type)
			if key_bytes <= 0 {
				key_bytes = 8
			}
			if value_bytes <= 0 {
				value_bytes = 8
			}
			return MapDeclInfo{
				key_bytes:   key_bytes
				value_bytes: value_bytes
				value_type:  value_type
			}
		}
		else {}
	}
	return none
}

fn (mut b Builder) fn_ptr_ret_type_from_field_name(field_name string) TypeID {
	if field_name in ['msg', 'str', 'type_name'] {
		if st := b.get_struct_type_id('string') {
			return st
		}
	}
	if field_name == 'code' {
		return b.mod.type_store.get_int(64)
	}
	if field_name.ends_with('clone_fn') || field_name.ends_with('free_fn') {
		return 0
	}
	if field_name.ends_with('eq_fn') {
		return b.mod.type_store.get_int(8)
	}
	// Hash/result-like callbacks default to integer return.
	return b.mod.type_store.get_int(64)
}

fn (b &Builder) resolve_method_fn_name(concrete_type string, method_name string) string {
	if concrete_type == '' {
		return ''
	}
	candidate := '${concrete_type}__${method_name}'
	if map_has_key_type_id(b.func_ret_types, candidate) {
		return candidate
	}
	lower_candidate := '${concrete_type.to_lower()}__${method_name}'
	if map_has_key_type_id(b.func_ret_types, lower_candidate) {
		return lower_candidate
	}
	mod_name := b.module_for_type_name(concrete_type)
	if !concrete_type.contains('__') && mod_name != '' && mod_name != 'main' {
		mod_candidate := '${mod_name}__${concrete_type}__${method_name}'
		if map_has_key_type_id(b.func_ret_types, mod_candidate) {
			return mod_candidate
		}
		mod_lower_candidate := '${mod_name}__${concrete_type.to_lower()}__${method_name}'
		if map_has_key_type_id(b.func_ret_types, mod_lower_candidate) {
			return mod_lower_candidate
		}
	}
	return ''
}

fn (b &Builder) unwrap_paren_expr(expr ast.Expr) ast.Expr {
	return match expr {
		ast.ParenExpr { b.unwrap_paren_expr(expr.expr) }
		else { expr }
	}
}

fn (b &Builder) exprs_match(lhs ast.Expr, rhs ast.Expr) bool {
	left := b.unwrap_paren_expr(lhs)
	right := b.unwrap_paren_expr(rhs)
	match left {
		ast.Ident {
			if right is ast.Ident {
				return left.name == right.name
			}
		}
		ast.SelectorExpr {
			if right is ast.SelectorExpr {
				return left.rhs.name == right.rhs.name
					&& b.exprs_match(ast.Expr(left.lhs), ast.Expr(right.lhs))
			}
		}
		else {}
	}
	return false
}

fn (b &Builder) is_implicit_interface_receiver_arg(arg_expr ast.Expr, receiver_expr ast.Expr) bool {
	arg := b.unwrap_paren_expr(arg_expr)
	recv := b.unwrap_paren_expr(receiver_expr)
	if arg is ast.SelectorExpr {
		mut is_object_field := arg.rhs.name == '_object'
		if !is_object_field {
			is_object_field = arg.rhs.name == 'f_0__object' || arg.rhs.name.ends_with('__object')
				|| arg.rhs.name.ends_with('_object')
		}
		if is_object_field {
			lhs_expr := b.unwrap_paren_expr(ast.Expr(arg.lhs))
			if lhs_expr is ast.Ident && recv is ast.Ident {
				return lhs_expr.name == recv.name
			}
			return b.exprs_match(lhs_expr, recv)
		}
	}
	if arg is ast.Ident && recv is ast.Ident {
		return arg.name == recv.name
	}
	if b.exprs_match(arg, recv) {
		return true
	}
	return false
}

fn (mut b Builder) emit_interface_method_call(receiver_expr ast.Expr, iface_name string, method_name string, call_args []ast.Expr) ?ValueID {
	iface_type := b.get_struct_type_id(iface_name) or { return none }
	if iface_type <= 0 || iface_type >= b.mod.type_store.types.len {
		return none
	}
	method_idx := b.field_index_by_name(iface_type, method_name)
	object_idx := b.field_index_by_name(iface_type, '_object')
	if method_idx < 0 || object_idx < 0 {
		return none
	}
	iface_slot := b.addr(receiver_expr)
	if iface_slot <= 0 || iface_slot >= b.mod.values.len {
		return none
	}
	slot_typ_id := b.mod.values[iface_slot].typ
	if slot_typ_id <= 0 || slot_typ_id >= b.mod.type_store.types.len {
		return none
	}
	slot_typ := b.mod.type_store.types[slot_typ_id]
	if slot_typ.kind != .ptr_t || slot_typ.elem_type <= 0
		|| slot_typ.elem_type >= b.mod.type_store.types.len {
		return none
	}
	iface_val := b.mod.add_instr(.load, b.cur_block, slot_typ.elem_type, [iface_slot])
	i32_t := b.mod.type_store.get_int(32)
	method_idx_val := b.mod.add_value_node(.constant, i32_t, method_idx.str(), 0)
	object_idx_val := b.mod.add_value_node(.constant, i32_t, object_idx.str(), 0)
	iface_info := b.mod.type_store.types[iface_type]
	if method_idx >= iface_info.fields.len || object_idx >= iface_info.fields.len {
		return none
	}
	method_ptr_t := iface_info.fields[method_idx]
	object_ptr_t := iface_info.fields[object_idx]
	method_ptr := b.mod.add_instr(.extractvalue, b.cur_block, method_ptr_t, [
		iface_val,
		method_idx_val,
	])
	object_ptr := b.mod.add_instr(.extractvalue, b.cur_block, object_ptr_t, [
		iface_val,
		object_idx_val,
	])
	mut call_arg_start := 0
	if call_args.len > 0 && b.is_implicit_interface_receiver_arg(call_args[0], receiver_expr) {
		call_arg_start = 1
	}
	mut args := []ValueID{cap: call_args.len - call_arg_start + 2}
	args << method_ptr
	args << object_ptr
	for i := call_arg_start; i < call_args.len; i++ {
		args << b.expr(call_args[i])
	}
	ret_type := b.fn_ptr_ret_type_from_field_name(method_name)
	return b.mod.add_instr(.call_indirect, b.cur_block, ret_type, args)
}

fn (mut b Builder) box_call_arg_as_ptr(val ValueID) ValueID {
	mut val_type := b.mod.values[val].typ
	if val_type == 0 {
		val_type = b.mod.type_store.get_int(64)
	}
	slot_type := b.mod.type_store.get_ptr(val_type)
	slot := b.mod.add_instr(.alloca, b.cur_block, slot_type, [])
	b.mod.add_instr(.store, b.cur_block, 0, [val, slot])
	return slot
}

fn (b &Builder) const_i64_value(val_id ValueID) ?i64 {
	if val_id <= 0 || val_id >= b.mod.values.len {
		return none
	}
	val := b.mod.values[val_id]
	if val.kind != .constant {
		return none
	}
	return val.name.i64()
}

fn (b &Builder) is_zero_const(val_id ValueID) bool {
	if val_id == 0 {
		return true
	}
	if v := b.const_i64_value(val_id) {
		return v == 0
	}
	return false
}

fn (b &Builder) is_module_call_receiver(name string) bool {
	if name in ['C', 'builtin'] {
		return true
	}
	if b.env != unsafe { nil } {
		if scope_ref := b.env.get_scope(b.cur_module) {
			mut scope := scope_ref
			if obj := scope.lookup_parent(name, 0) {
				if obj is types.Module {
					return true
				}
			}
		}
		if scope_ref := b.env.get_scope('builtin') {
			mut scope := scope_ref
			if obj := scope.lookup_parent(name, 0) {
				if obj is types.Module {
					return true
				}
			}
		}
		if _ := b.env.get_scope(name) {
			return true
		}
	}
	// Fallback for frequently used stdlib module qualifiers in partially initialized envs.
	return name in ['strconv', 'math', 'os', 'strings', 'hash', 'time', 'rand', 'sync', 'runtime',
		'bits', 'encoding', 'mem', 'pref', 'cmdline', 'term', 'ssa', 'optimize', 'ast', 'abi',
		'token', 'types', 'parser', 'scanner', 'builder', 'c', 'cleanc', 'x64', 'arm64', 'mir',
		'insel', 'util']
}

fn (mut b Builder) adjust_map_runtime_call_args(name string, mut args []ValueID) {
	is_new_map_init_noscan := name == 'new_map_init_noscan_value'
		|| name.ends_with('__new_map_init_noscan_value')
	if is_new_map_init_noscan && args.len >= 9 {
		if b.is_zero_const(args[0]) && b.is_zero_const(args[1]) && b.is_zero_const(args[2])
			&& b.is_zero_const(args[3]) {
			mut key_bytes := 8
			if kb := b.const_i64_value(args[5]) {
				key_bytes = int(kb)
			}
			key_width := map_int_key_width(key_bytes)
			mut hash_fn_name := 'map_hash_int_${key_width}'
			mut eq_fn_name := 'map_eq_int_${key_width}'
			mut clone_fn_name := 'map_clone_int_${key_width}'
			mut free_fn_name := 'map_free_nop'
			if key_bytes >= 24 {
				hash_fn_name = 'map_hash_string'
				eq_fn_name = 'map_eq_string'
				clone_fn_name = 'map_clone_string'
				free_fn_name = 'map_free_string'
			}
			i64_t := b.mod.type_store.get_int(64)
			args[0] = b.mod.add_value_node(.func_ref, i64_t, hash_fn_name, 0)
			args[1] = b.mod.add_value_node(.func_ref, i64_t, eq_fn_name, 0)
			args[2] = b.mod.add_value_node(.func_ref, i64_t, clone_fn_name, 0)
			args[3] = b.mod.add_value_node(.func_ref, i64_t, free_fn_name, 0)
		}
		return
	}
	if name !in ['map__set', 'map__get', 'map__get_check', 'map__get_and_set', 'map__delete'] {
		return
	}
	if args.len >= 1 {
		args[0] = b.normalize_map_ptr(args[0])
		if map_t := b.get_struct_type_id('map') {
			map_ptr_t := b.mod.type_store.get_ptr(map_t)
			map_arg := args[0]
			if map_arg > 0 && map_arg < b.mod.values.len {
				arg_typ := b.mod.values[map_arg].typ
				if arg_typ > 0 && arg_typ < b.mod.type_store.types.len {
					arg_info := b.mod.type_store.types[arg_typ]
					if arg_info.kind == .ptr_t && arg_info.elem_type > 0
						&& arg_info.elem_type < b.mod.type_store.types.len {
						elem_info := b.mod.type_store.types[arg_info.elem_type]
						if elem_info.kind == .int_t {
							raw_map := b.mod.add_instr(.load, b.cur_block, arg_info.elem_type,
								[
								map_arg,
							])
							args[0] = b.coerce_value_to_type(raw_map, map_ptr_t)
						}
					} else if arg_info.kind == .int_t {
						args[0] = b.coerce_value_to_type(map_arg, map_ptr_t)
					}
				}
			}
		}
	}
	if args.len >= 2 {
		mut key_typ := b.mod.values[args[1]].typ
		if key_typ > 0 && key_typ < b.mod.type_store.types.len {
			key_info := b.mod.type_store.types[key_typ]
			if key_info.kind == .ptr_t && key_info.elem_type > 0
				&& key_info.elem_type < b.mod.type_store.types.len {
				inner_info := b.mod.type_store.types[key_info.elem_type]
				if inner_info.kind == .ptr_t {
					// Runtime map helpers expect key pointers, not addresses of key pointers.
					// Normalize T** -> T* where needed (common for string map keys).
					args[1] = b.mod.add_instr(.load, b.cur_block, key_info.elem_type,
						[
						args[1],
					])
					key_typ = b.mod.values[args[1]].typ
				}
			}
		}
		mut key_is_ptr := false
		if key_typ > 0 && key_typ < b.mod.type_store.types.len {
			key_kind := b.mod.type_store.types[key_typ].kind
			key_is_ptr = key_kind in [.ptr_t, .array_t]
		}
		if !key_is_ptr {
			args[1] = b.box_call_arg_as_ptr(args[1])
		}
	}
	if name in ['map__set', 'map__get_and_set'] && args.len >= 3 {
		// Runtime map helpers receive a pointer to value bytes.
		// For regular call sites we pass &value (so pointer-typed values are copied by value).
		// Internal builtin map routines already pass raw value-byte pointers.
		mut skip_box_value := false
		mut val_typ := b.mod.values[args[2]].typ
		if val_typ > 0 && val_typ < b.mod.type_store.types.len {
			val_info := b.mod.type_store.types[val_typ]
			if val_info.kind == .ptr_t && val_info.elem_type > 0
				&& val_info.elem_type < b.mod.type_store.types.len {
				inner_info := b.mod.type_store.types[val_info.elem_type]
				if inner_info.kind == .ptr_t && inner_info.elem_type > 0
					&& inner_info.elem_type < b.mod.type_store.types.len {
					if string_t := b.get_struct_type_id('string') {
						if inner_info.elem_type == string_t {
							args[2] = b.mod.add_instr(.load, b.cur_block, val_info.elem_type,
								[
								args[2],
							])
							val_typ = b.mod.values[args[2]].typ
						}
					}
				}
			}
		}
		if val_typ > 0 && val_typ < b.mod.type_store.types.len {
			val_info := b.mod.type_store.types[val_typ]
			if val_info.kind == .ptr_t && val_info.elem_type > 0
				&& val_info.elem_type < b.mod.type_store.types.len {
				elem_info := b.mod.type_store.types[val_info.elem_type]
				if elem_info.kind == .struct_t {
					if string_t := b.get_struct_type_id('string') {
						if val_info.elem_type == string_t {
							skip_box_value = true
						}
					}
				}
			}
			if val_info.kind == .ptr_t && b.is_map_type_id(val_info.elem_type) {
				// Nested map values are map structs; their expressions already
				// produce pointers to map-value bytes.
				skip_box_value = true
			}
		}
		if b.cur_func >= 0 && b.cur_func < b.mod.funcs.len {
			cur_fn_name := b.mod.funcs[b.cur_func].name
			skip_box_value = skip_box_value
				|| cur_fn_name in ['new_map_init', 'new_map_update_init', 'get_and_set', 'map__get_and_set']
				|| cur_fn_name.ends_with('__new_map_init')
				|| cur_fn_name.ends_with('__new_map_update_init')
				|| cur_fn_name.ends_with('__get_and_set')
		}
		if !skip_box_value {
			args[2] = b.box_call_arg_as_ptr(args[2])
		}
	}
}

fn (mut b Builder) remap_missing_runtime_fn(name string) string {
	if name == 'array__push_noscan' && !map_has_key_type_id(b.func_ret_types, name) {
		if map_has_key_type_id(b.func_ret_types, 'array__push') {
			return 'array__push'
		}
	}
	if name == 'array__push_many_noscan' && !map_has_key_type_id(b.func_ret_types, name) {
		if map_has_key_type_id(b.func_ret_types, 'array__push_many') {
			return 'array__push_many'
		}
	}
	if name == 'array__write_u8' && !map_has_key_type_id(b.func_ret_types, name) {
		if map_has_key_type_id(b.func_ret_types, 'strings__Builder__write_u8') {
			return 'strings__Builder__write_u8'
		}
	}
	if (name.starts_with('array__write') || name.starts_with('array__writeln'))
		&& !map_has_key_type_id(b.func_ret_types, name) {
		candidate := 'strings__Builder__' + name['array__'.len..]
		if map_has_key_type_id(b.func_ret_types, candidate) {
			return candidate
		}
	}
	if name == 'array__reuse_as_plain_u8_array' && !map_has_key_type_id(b.func_ret_types, name) {
		if map_has_key_type_id(b.func_ret_types, 'strings__Builder__reuse_as_plain_u8_array') {
			return 'strings__Builder__reuse_as_plain_u8_array'
		}
	}
	if name == 'array__disable_echo' && !map_has_key_type_id(b.func_ret_types, name) {
		if map_has_key_type_id(b.func_ret_types, 'termios__Termios__disable_echo') {
			return 'termios__Termios__disable_echo'
		}
	}
	if name == 'string__process_str_intp_data' && !map_has_key_type_id(b.func_ret_types, name) {
		if map_has_key_type_id(b.func_ret_types, 'StrIntpData__process_str_intp_data') {
			return 'StrIntpData__process_str_intp_data'
		}
	}
	if name == 'init_time_base' && !map_has_key_type_id(b.func_ret_types, name) {
		if map_has_key_type_id(b.func_ret_types, 'time__init_time_base') {
			return 'time__init_time_base'
		}
	}
	if name.starts_with('time__Time__') && !map_has_key_type_id(b.func_ret_types, name) {
		method := name['time__Time__'.len..]
		for candidate in ['textscanner__TextScanner__${method}', 'time__DateTimeParser__${method}'] {
			if map_has_key_type_id(b.func_ret_types, candidate) {
				return candidate
			}
		}
		// Last resort: keep native link happy for obviously wrong/missing symbol references.
		b.generate_noop_stub(name)
		return name
	}
	if name in ['f', 'fcb', 'cfn'] && !map_has_key_type_id(b.func_ret_types, name) {
		b.generate_noop_stub(name)
		return name
	}
	if name in ['WEXITSTATUS', 'WIFEXITED', 'WIFSIGNALED', 'WIFSTOPPED', 'WTERMSIG', 'WSTOPSIG'] {
		stub_name := '__v_' + name
		b.generate_unary_passthrough_stub(stub_name)
		return stub_name
	}
	if name in ['FD_ZERO', 'FD_CLR'] {
		stub_name := '__v_' + name
		b.generate_noop_stub(stub_name)
		return stub_name
	}
	if name in ['FD_SET', 'FD_ISSET'] {
		stub_name := '__v_' + name
		b.generate_fd_macro_stub(stub_name, name == 'FD_ISSET')
		return stub_name
	}
	return name
}

fn (b &Builder) resolve_receiver_method_name(recv_name string, mangled_method string) string {
	if recv_name == '' || mangled_method == '' {
		return ''
	}
	mut candidates := []string{}
	if recv_name.contains('__') {
		candidates << '${recv_name}__${mangled_method}'
		candidates << '${recv_name.to_lower()}__${mangled_method}'
	} else {
		recv_module := b.module_for_type_name(recv_name)
		if recv_module != '' && recv_module != 'main' {
			candidates << '${recv_module}__${recv_name}__${mangled_method}'
			candidates << '${recv_module}__${recv_name.to_lower()}__${mangled_method}'
		}
		candidates << '${recv_name}__${mangled_method}'
		candidates << '${recv_name.to_lower()}__${mangled_method}'
		if recv_module != 'builtin' {
			candidates << 'builtin__${recv_name}__${mangled_method}'
			candidates << 'builtin__${recv_name.to_lower()}__${mangled_method}'
		}
		if base_type := b.get_type_alias_base(recv_name) {
			candidates << '${base_type}__${mangled_method}'
			candidates << '${base_type.to_lower()}__${mangled_method}'
		}
	}
	mut seen := map[string]bool{}
	for candidate in candidates {
		if candidate == '' || map_has_key_bool(seen, candidate) {
			continue
		}
		seen[candidate] = true
		if map_has_key_type_id(b.func_ret_types, candidate) {
			return candidate
		}
	}
	return ''
}

fn (b &Builder) get_struct_type_id(name string) ?TypeID {
	if name == '' {
		return none
	}
	normalized := b.canonical_struct_type_name(name)
	if id := map_get_type_id(b.struct_types, normalized) {
		return id
	}
	if normalized != name {
		if id := map_get_type_id(b.struct_types, name) {
			return id
		}
	}
	if !normalized.contains('__') {
		suffix := '__${normalized}'
		mut match_found := false
		mut matched := TypeID(0)
		for key, id in b.struct_types {
			if key.ends_with(suffix) {
				if !match_found {
					match_found = true
					matched = id
				} else if matched != id {
					return none
				}
			}
		}
		if match_found {
			return matched
		}
	}
	return none
}

fn (b &Builder) struct_name_from_type_id(type_id TypeID) ?string {
	mut best := ''
	for name, id in b.struct_types {
		if id == type_id {
			if best == '' || name.len < best.len {
				best = name
			}
		}
	}
	if best != '' {
		return best
	}
	return none
}

fn (b &Builder) canonical_struct_type_name(name string) string {
	mut normalized := name.replace('.', '__')
	if !normalized.contains('__') {
		if b.cur_module != '' && b.cur_module != 'main' {
			cur_key := '${b.cur_module}__${normalized}'
			if map_has_key_type_id(b.struct_types, cur_key) {
				return cur_key
			}
		}
		builtin_key := 'builtin__${normalized}'
		if map_has_key_type_id(b.struct_types, builtin_key) {
			return builtin_key
		}
	}
	if map_has_key_type_id(b.struct_types, normalized) {
		return normalized
	}
	return normalized
}

fn (b &Builder) unqualified_type_name(name string) string {
	if name.starts_with('__') {
		return name
	}
	if name.contains('__') {
		return name.all_after_last('__')
	}
	return name
}

fn (b &Builder) module_for_type_name(name string) string {
	if name.starts_with('__') {
		return ''
	}
	normalized := name.replace('.', '__')
	if normalized.contains('__') {
		return normalized.all_before('__')
	}
	if mod_name := b.type_modules[normalized] {
		return mod_name
	}
	return ''
}

fn (mut b Builder) register_struct_type_name(name string, type_id TypeID) {
	mut should_replace_existing := false
	if existing := map_get_type_id(b.struct_types, name) {
		if existing > 0 && existing < b.mod.type_store.types.len && type_id > 0
			&& type_id < b.mod.type_store.types.len {
			existing_t := b.mod.type_store.types[existing]
			new_t := b.mod.type_store.types[type_id]
			// Prefer richer concrete type definitions over earlier placeholders.
			if existing_t.kind == .struct_t && new_t.kind == .struct_t
				&& existing_t.fields.len < new_t.fields.len {
				should_replace_existing = true
			}
		}
	}
	if !map_has_key_type_id(b.struct_types, name) || should_replace_existing {
		b.struct_types[name] = type_id
	}
	if name.starts_with('__') || name.contains('__') || b.cur_module == '' || b.cur_module == 'main' {
		if !map_has_key_string(b.type_modules, name) {
			b.type_modules[name] = b.cur_module
		}
		return
	}
	qualified := '${b.cur_module}__${name}'
	// Preserve the first qualified registration so C shims like `C.IError`
	// do not overwrite richer V interface layouts from builtin source files.
	if !map_has_key_type_id(b.struct_types, qualified) {
		b.struct_types[qualified] = type_id
	}
	if !map_has_key_string(b.type_modules, qualified) {
		b.type_modules[qualified] = b.cur_module
	}
	if !map_has_key_string(b.type_modules, name) {
		b.type_modules[name] = b.cur_module
	}
}

fn (b &Builder) is_map_type_id(type_id TypeID) bool {
	if type_id <= 0 || type_id >= b.mod.type_store.types.len {
		return false
	}
	t := b.mod.type_store.types[type_id]
	if t.kind != .struct_t {
		return false
	}
	if struct_name := b.struct_name_from_type_id(type_id) {
		return struct_name == 'map'
	}
	return false
}

fn (b &Builder) is_string_ptr_type_id(type_id TypeID) bool {
	if type_id <= 0 || type_id >= b.mod.type_store.types.len {
		return false
	}
	if string_t := b.get_struct_type_id('string') {
		typ := b.mod.type_store.types[type_id]
		return typ.kind == .ptr_t && typ.elem_type == string_t
	}
	return false
}

fn (b &Builder) is_map_slot(slot ValueID) bool {
	if slot <= 0 || slot >= b.mod.values.len {
		return false
	}
	slot_typ_id := b.mod.values[slot].typ
	if slot_typ_id <= 0 || slot_typ_id >= b.mod.type_store.types.len {
		return false
	}
	slot_typ := b.mod.type_store.types[slot_typ_id]
	if slot_typ.kind != .ptr_t || slot_typ.elem_type <= 0 {
		return false
	}
	return b.is_map_type_id(slot_typ.elem_type)
}

fn (mut b Builder) normalize_map_ptr(map_ptr ValueID) ValueID {
	if map_ptr <= 0 || map_ptr >= b.mod.values.len {
		return map_ptr
	}
	map_ptr_typ_id := b.mod.values[map_ptr].typ
	if map_ptr_typ_id <= 0 || map_ptr_typ_id >= b.mod.type_store.types.len {
		return map_ptr
	}
	map_ptr_typ := b.mod.type_store.types[map_ptr_typ_id]
	if map_ptr_typ.kind != .ptr_t || map_ptr_typ.elem_type <= 0
		|| map_ptr_typ.elem_type >= b.mod.type_store.types.len {
		return map_ptr
	}
	elem_typ_id := map_ptr_typ.elem_type
	elem_typ := b.mod.type_store.types[elem_typ_id]
	if elem_typ.kind == .ptr_t && elem_typ.elem_type > 0
		&& elem_typ.elem_type < b.mod.type_store.types.len && b.is_map_type_id(elem_typ.elem_type) {
		return b.mod.add_instr(.load, b.cur_block, elem_typ_id, [map_ptr])
	}
	return map_ptr
}

fn (b &Builder) is_sum_type_id(type_id TypeID) bool {
	if type_id <= 0 || type_id >= b.mod.type_store.types.len {
		return false
	}
	t := b.mod.type_store.types[type_id]
	if t.kind != .struct_t || t.field_names.len < 2 {
		return false
	}
	return t.field_names[0] == '_tag' && t.field_names[1] == '_data'
}

fn (b &Builder) is_string_like_type_id(type_id TypeID) bool {
	if type_id <= 0 || type_id >= b.mod.type_store.types.len {
		return false
	}
	mut cur_t := type_id
	cur_info := b.mod.type_store.types[cur_t]
	if cur_info.kind == .ptr_t {
		cur_t = cur_info.elem_type
		if cur_t <= 0 || cur_t >= b.mod.type_store.types.len {
			return false
		}
	}
	if cur_t > 0 && cur_t < b.mod.type_store.types.len {
		cur_info2 := b.mod.type_store.types[cur_t]
		if cur_info2.kind == .struct_t {
			if struct_name := b.struct_name_from_type_id(cur_t) {
				return struct_name == 'string'
			}
			return cur_info2.field_names.len >= 3
				&& ('str' in cur_info2.field_names || 'f_0_str' in cur_info2.field_names)
				&& ('len' in cur_info2.field_names || 'f_1_len' in cur_info2.field_names)
		}
	}
	return false
}

fn (b &Builder) value_is_string_like(val ValueID) bool {
	if val <= 0 || val >= b.mod.values.len {
		return false
	}
	return b.is_string_like_type_id(b.mod.values[val].typ)
}

fn (mut b Builder) option_like_data_value(val ValueID) ?ValueID {
	if val <= 0 || val >= b.mod.values.len {
		return none
	}
	mut wrapped_val := val
	mut wrapped_typ := b.mod.values[wrapped_val].typ
	if wrapped_typ <= 0 || wrapped_typ >= b.mod.type_store.types.len {
		return none
	}
	wrapped_info := b.mod.type_store.types[wrapped_typ]
	if wrapped_info.kind == .ptr_t && wrapped_info.elem_type > 0
		&& wrapped_info.elem_type < b.mod.type_store.types.len
		&& b.is_option_like_type_id(wrapped_info.elem_type) {
		wrapped_val = b.mod.add_instr(.load, b.cur_block, wrapped_info.elem_type, [
			wrapped_val,
		])
		wrapped_typ = wrapped_info.elem_type
	}
	if !b.is_option_like_type_id(wrapped_typ) {
		return none
	}
	data_idx := b.field_index_by_name(wrapped_typ, 'data')
	if data_idx < 0 {
		return none
	}
	wrapped_type := b.mod.type_store.types[wrapped_typ]
	if data_idx >= wrapped_type.fields.len {
		return none
	}
	data_typ := wrapped_type.fields[data_idx]
	idx_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(32), data_idx.str(),
		0)
	return b.mod.add_instr(.extractvalue, b.cur_block, data_typ, [wrapped_val, idx_val])
}

fn (mut b Builder) string_data_ptr_from_value(val ValueID) ?ValueID {
	if val <= 0 || val >= b.mod.values.len {
		return none
	}
	i8_t := b.mod.type_store.get_int(8)
	i8_ptr_t := b.mod.type_store.get_ptr(i8_t)
	mut string_val := val
	mut string_typ := b.mod.values[val].typ
	if string_typ <= 0 || string_typ >= b.mod.type_store.types.len {
		return none
	}
	string_info := b.mod.type_store.types[string_typ]
	if string_info.kind == .ptr_t && string_info.elem_type > 0
		&& string_info.elem_type < b.mod.type_store.types.len
		&& b.is_string_like_type_id(string_info.elem_type) {
		string_val = b.mod.add_instr(.load, b.cur_block, string_info.elem_type, [val])
		string_typ = string_info.elem_type
	}
	if !b.is_string_like_type_id(string_typ) {
		return none
	}
	mut data_idx := b.field_index_by_name(string_typ, 'str')
	if data_idx < 0 {
		if b.mod.type_store.types[string_typ].fields.len == 0 {
			return none
		}
		data_idx = 0
	}
	idx_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(32), data_idx.str(),
		0)
	return b.mod.add_instr(.extractvalue, b.cur_block, i8_ptr_t, [string_val, idx_val])
}

fn (b &Builder) is_ierror_layout(type_id TypeID) bool {
	if type_id <= 0 || type_id >= b.mod.type_store.types.len {
		return false
	}
	t := b.mod.type_store.types[type_id]
	if t.kind != .struct_t || t.field_names.len < 4 {
		return false
	}
	mut has_msg := false
	mut has_code := false
	for field_name in t.field_names {
		if field_name == 'msg' {
			has_msg = true
		} else if field_name == 'code' {
			has_code = true
		}
	}
	return t.field_names[0] == '_object' && t.field_names[1] == '_type_id' && has_msg && has_code
}

fn (b &Builder) get_ierror_type_id() ?TypeID {
	mut fallback := TypeID(0)
	for candidate in ['builtin__IError', 'builtin.IError', 'IError'] {
		if tid := b.get_struct_type_id(candidate) {
			if b.is_ierror_layout(tid) {
				return tid
			}
			if fallback == 0 {
				fallback = tid
			}
		}
	}
	for name, tid in b.struct_types {
		if !name.ends_with('IError') {
			continue
		}
		if b.is_ierror_layout(tid) {
			return tid
		}
		if fallback == 0 {
			fallback = tid
		}
	}
	for i, t in b.mod.type_store.types {
		if t.kind != .struct_t || t.field_names.len < 4 {
			continue
		}
		if b.is_ierror_layout(i) {
			return i
		}
	}
	if fallback != 0 {
		return fallback
	}
	return none
}

fn (mut b Builder) ensure_option_like_type(base_type TypeID, is_result bool) TypeID {
	i64_t := b.mod.type_store.get_int(64)
	mut err_t := i64_t
	if ierr_t := b.get_ierror_type_id() {
		err_t = ierr_t
	}
	kind := if is_result { 'result' } else { 'option' }
	key := '__${kind}_${base_type}'
	if existing := b.get_struct_type_id(key) {
		// Wrapper types can be created before IError is registered. Upgrade err/error
		// field types once IError becomes available so !T/or{ err } semantics stay correct.
		if err_t != i64_t && existing > 0 && existing < b.mod.type_store.types.len {
			existing_t := b.mod.type_store.types[existing]
			if existing_t.kind == .struct_t {
				mut updated_fields := existing_t.fields.clone()
				for i, field_name in existing_t.field_names {
					if (field_name == 'err' || field_name == 'error') && i >= 0
						&& i < updated_fields.len {
						updated_fields[i] = err_t
					}
				}
				b.mod.type_store.types[existing] = Type{
					kind:        existing_t.kind
					width:       existing_t.width
					elem_type:   existing_t.elem_type
					len:         existing_t.len
					fields:      updated_fields
					field_names: existing_t.field_names
					params:      existing_t.params
					ret_type:    existing_t.ret_type
				}
			}
		}
		return existing
	}
	// Keep the canonical transformed field names so selector lowering works.
	wrap_t := Type{
		kind:        .struct_t
		fields:      [i64_t, err_t, base_type, err_t, i64_t]
		field_names: ['state', 'err', 'data', 'error', 'is_error']
		width:       0
	}
	type_id := b.mod.type_store.register(wrap_t)
	b.struct_types[key] = type_id
	return type_id
}

fn (b &Builder) is_option_like_type_id(type_id TypeID) bool {
	if type_id <= 0 || type_id >= b.mod.type_store.types.len {
		return false
	}
	t := b.mod.type_store.types[type_id]
	if t.kind != .struct_t {
		return false
	}
	mut has_state := false
	mut has_data := false
	for field_name in t.field_names {
		if field_name == 'state' {
			has_state = true
		} else if field_name == 'data' {
			has_data = true
		}
	}
	return has_state && has_data
}

fn canonical_transformed_field_name(name string) string {
	if name.starts_with('f_') && name.len > 2 {
		rest := name[2..]
		mut i := 0
		for i < rest.len && rest[i] >= `0` && rest[i] <= `9` {
			i++
		}
		if i < rest.len && rest[i] == `_` && i + 1 < rest.len {
			return rest[i + 1..]
		}
	}
	return name
}

fn (b &Builder) field_index_by_name(type_id TypeID, field_name string) int {
	if type_id <= 0 || type_id >= b.mod.type_store.types.len {
		return -1
	}
	trimmed_field_name := if field_name.starts_with('_') && field_name.len > 1 {
		field_name[1..]
	} else {
		field_name
	}
	t := b.mod.type_store.types[type_id]
	for i, name in t.field_names {
		canonical_name := canonical_transformed_field_name(name)
		if name == field_name || canonical_name == field_name
			|| canonical_name == trimmed_field_name {
			return i
		}
		if name.ends_with(field_name)
			|| (trimmed_field_name != field_name && name.ends_with(trimmed_field_name)) {
			return i
		}
	}
	return -1
}

fn (mut b Builder) coerce_value_to_type(val ValueID, target_type TypeID) ValueID {
	if val <= 0 || val >= b.mod.values.len {
		return val
	}
	val_type := b.mod.values[val].typ
	if val_type == target_type {
		return val
	}
	if target_type <= 0 || target_type >= b.mod.type_store.types.len {
		return val
	}
	if val_type <= 0 || val_type >= b.mod.type_store.types.len {
		return val
	}
	target_info := b.mod.type_store.types[target_type]
	val_info := b.mod.type_store.types[val_type]
	if target_info.kind == .int_t && b.is_string_like_type_id(val_type) && val_info.kind != .ptr_t {
		i64_t := b.mod.type_store.get_int(64)
		i8_t := b.mod.type_store.get_int(8)
		voidptr_t := b.mod.type_store.get_ptr(i8_t)
		mut size_bytes := b.type_size_from_ssa_type(val_type)
		if size_bytes <= 0 {
			size_bytes = 24
		}
		size_val := b.mod.add_value_node(.constant, i64_t, size_bytes.str(), 0)
		malloc_sym := b.mod.add_value_node(.unknown, 0, 'malloc', 0)
		raw_mem := b.mod.add_instr(.call, b.cur_block, voidptr_t, [malloc_sym, size_val])
		typed_ptr := b.mod.add_instr(.bitcast, b.cur_block, b.mod.type_store.get_ptr(val_type),
			[raw_mem])
		b.mod.add_instr(.store, b.cur_block, 0, [val, typed_ptr])
		return b.mod.add_instr(.bitcast, b.cur_block, target_type, [typed_ptr])
	}
	if target_info.kind == .ptr_t && val_info.kind in [.ptr_t, .int_t] {
		return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
	}
	if target_info.kind == .int_t && val_info.kind in [.ptr_t, .int_t] {
		return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
	}
	if target_info.kind == .float_t && val_info.kind == .int_t {
		return b.mod.add_instr(.sitofp, b.cur_block, target_type, [val])
	}
	if target_info.kind == .int_t && val_info.kind == .float_t {
		return b.mod.add_instr(.fptosi, b.cur_block, target_type, [val])
	}
	return val
}

fn (mut b Builder) call_str_helper_if_exists(name string, val ValueID, string_ptr_t TypeID) ?ValueID {
	fn_idx := b.find_function(name)
	if fn_idx < 0 || fn_idx >= b.mod.funcs.len {
		return none
	}
	mut arg_val := val
	if b.mod.funcs[fn_idx].params.len > 0 {
		param_val := b.mod.funcs[fn_idx].params[0]
		if param_val > 0 && param_val < b.mod.values.len {
			param_typ := b.mod.values[param_val].typ
			arg_val = b.coerce_value_to_type(val, param_typ)
		}
	}
	ret_typ := b.mod.funcs[fn_idx].typ
	if ret_typ <= 0 || ret_typ >= b.mod.type_store.types.len {
		return none
	}
	fn_val := b.mod.add_value_node(.unknown, 0, name, 0)
	call_res := b.mod.add_instr(.call, b.cur_block, ret_typ, [fn_val, arg_val])
	return b.coerce_merge_value_to_type(call_res, string_ptr_t)
}

fn (mut b Builder) coerce_value_to_string(val ValueID) ValueID {
	if val <= 0 || val >= b.mod.values.len {
		return val
	}
	if string_t := b.get_struct_type_id('string') {
		string_ptr_t := b.mod.type_store.get_ptr(string_t)
		val_typ := b.mod.values[val].typ
		if val_typ == string_ptr_t {
			return val
		}
		if val_typ == string_t {
			return b.box_call_arg_as_ptr(val)
		}
		if val_typ <= 0 || val_typ >= b.mod.type_store.types.len {
			return val
		}
		val_info := b.mod.type_store.types[val_typ]
		if val_info.kind == .ptr_t && val_info.elem_type == string_t {
			return val
		}
		mut candidates := []string{}
		match val_info.kind {
			.int_t {
				match val_info.width {
					1 {
						candidates << 'bool__str'
					}
					8 {
						candidates << 'f64__str'
						candidates << 'i8__str'
						candidates << 'u8__str'
					}
					16 {
						candidates << 'f64__str'
						candidates << 'i16__str'
						candidates << 'u16__str'
					}
					32 {
						candidates << 'f64__str'
						candidates << 'i32__str'
						candidates << 'u32__str'
					}
					else {
						candidates << 'f64__str'
						// V `int` is 64-bit on modern targets.
						candidates << 'int__str'
						candidates << 'i64__str'
						candidates << 'u64__str'
					}
				}
			}
			.float_t {
				if val_info.width == 32 {
					candidates << 'f32__str'
				} else {
					candidates << 'f64__str'
				}
			}
			.ptr_t {
				candidates << 'voidptr__str'
				candidates << 'charptr__str'
				candidates << 'byteptr__str'
			}
			.struct_t {
				if val_typ != string_t {
					if struct_name := b.struct_name_from_type_id(val_typ) {
						candidates << '${struct_name}__str'
						lower_name := '${struct_name.to_lower()}__str'
						if lower_name !in candidates {
							candidates << lower_name
						}
					}
				}
			}
			else {}
		}
		if 'int__str' !in candidates {
			candidates << 'int__str'
		}
		for candidate in candidates {
			if converted := b.call_str_helper_if_exists(candidate, val, string_ptr_t) {
				return converted
			}
		}
	}
	return val
}

fn (mut b Builder) coerce_merge_value_to_type(val ValueID, target_type TypeID) ValueID {
	if val <= 0 || val >= b.mod.values.len {
		return val
	}
	if target_type <= 0 || target_type >= b.mod.type_store.types.len {
		return b.coerce_value_to_type(val, target_type)
	}
	val_type := b.mod.values[val].typ
	if val_type <= 0 || val_type >= b.mod.type_store.types.len {
		return b.coerce_value_to_type(val, target_type)
	}
	target_info := b.mod.type_store.types[target_type]
	val_info := b.mod.type_store.types[val_type]
	if target_info.kind == .struct_t && val_info.kind == .ptr_t && val_info.elem_type == target_type {
		return b.mod.add_instr(.load, b.cur_block, target_type, [val])
	}
	if target_info.kind == .ptr_t && val_info.kind == .struct_t && target_info.elem_type == val_type {
		stack_ptr := b.mod.add_instr(.alloca, b.cur_block, target_type, [])
		b.mod.add_instr(.store, b.cur_block, 0, [val, stack_ptr])
		return stack_ptr
	}
	return b.coerce_value_to_type(val, target_type)
}

fn (mut b Builder) wrap_option_like_value(wrapper_type TypeID, state int, data_val ValueID, err_val ValueID) ValueID {
	i64_t := b.mod.type_store.get_int(64)
	mut wrapped := b.mod.add_value_node(.constant, wrapper_type, 'undef', 0)
	state_idx := b.field_index_by_name(wrapper_type, 'state')
	if state_idx >= 0 {
		state_const := b.mod.add_value_node(.constant, i64_t, state.str(), 0)
		state_idx_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(32),
			state_idx.str(), 0)
		wrapped = b.mod.add_instr(.insertvalue, b.cur_block, wrapper_type, [wrapped, state_const,
			state_idx_val])
	}
	if state == 0 {
		data_idx := b.field_index_by_name(wrapper_type, 'data')
		if data_idx >= 0 && data_val != 0 {
			data_field_type := b.mod.type_store.types[wrapper_type].fields[data_idx]
			coerced_data := b.coerce_value_to_type(data_val, data_field_type)
			data_idx_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(32),
				data_idx.str(), 0)
			wrapped = b.mod.add_instr(.insertvalue, b.cur_block, wrapper_type, [
				wrapped,
				coerced_data,
				data_idx_val,
			])
		}
	} else if err_val != 0 {
		err_idx := b.field_index_by_name(wrapper_type, 'err')
		if err_idx >= 0 {
			err_field_type := b.mod.type_store.types[wrapper_type].fields[err_idx]
			coerced_err := b.coerce_value_to_type(err_val, err_field_type)
			err_idx_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(32),
				err_idx.str(), 0)
			wrapped = b.mod.add_instr(.insertvalue, b.cur_block, wrapper_type, [
				wrapped,
				coerced_err,
				err_idx_val,
			])
		}
		error_idx := b.field_index_by_name(wrapper_type, 'error')
		if error_idx >= 0 {
			error_field_type := b.mod.type_store.types[wrapper_type].fields[error_idx]
			coerced_error := b.coerce_value_to_type(err_val, error_field_type)
			error_idx_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(32),
				error_idx.str(), 0)
			wrapped = b.mod.add_instr(.insertvalue, b.cur_block, wrapper_type, [
				wrapped,
				coerced_error,
				error_idx_val,
			])
		}
	}
	is_error_idx := b.field_index_by_name(wrapper_type, 'is_error')
	if is_error_idx >= 0 {
		is_error_const := b.mod.add_value_node(.constant, i64_t, state.str(), 0)
		is_error_idx_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(32),
			is_error_idx.str(), 0)
		wrapped = b.mod.add_instr(.insertvalue, b.cur_block, wrapper_type, [wrapped, is_error_const,
			is_error_idx_val])
	}
	return wrapped
}

fn (b &Builder) sumtype_tag_for_value(sum_name string, value_type TypeID) int {
	variants := b.sumtype_variants[sum_name] or { []TypeID{} }
	if variants.len == 0 {
		return 0
	}
	for i, vt in variants {
		if vt == value_type {
			return i
		}
		if value_type > 0 && value_type < b.mod.type_store.types.len {
			value_info := b.mod.type_store.types[value_type]
			if value_info.kind == .ptr_t && value_info.elem_type == vt {
				return i
			}
		}
		if vt > 0 && vt < b.mod.type_store.types.len {
			vt_info := b.mod.type_store.types[vt]
			if vt_info.kind == .ptr_t && vt_info.elem_type == value_type {
				return i
			}
		}
	}
	return 0
}

fn (mut b Builder) wrap_sumtype_value(sum_name string, sum_type TypeID, value_val ValueID) ValueID {
	if value_val <= 0 || value_val >= b.mod.values.len {
		return b.mod.add_value_node(.constant, sum_type, 'undef', 0)
	}
	value_type := b.mod.values[value_val].typ
	if value_type == sum_type {
		return value_val
	}
	i64_t := b.mod.type_store.get_int(64)
	i8_t := b.mod.type_store.get_int(8)
	voidptr_t := b.mod.type_store.get_ptr(i8_t)
	mut wrapped := b.mod.add_value_node(.constant, sum_type, 'undef', 0)
	tag := b.sumtype_tag_for_value(sum_name, value_type)
	tag_idx := b.field_index_by_name(sum_type, '_tag')
	if tag_idx >= 0 {
		tag_val := b.mod.add_value_node(.constant, i64_t, tag.str(), 0)
		tag_idx_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(32), tag_idx.str(),
			0)
		wrapped = b.mod.add_instr(.insertvalue, b.cur_block, sum_type, [wrapped, tag_val, tag_idx_val])
	}
	data_idx := b.field_index_by_name(sum_type, '_data')
	if data_idx >= 0 {
		mut data_ptr := value_val
		if value_type > 0 && value_type < b.mod.type_store.types.len {
			value_info := b.mod.type_store.types[value_type]
			if value_info.kind !in [.ptr_t, .int_t, .float_t] {
				mut size_bytes := b.type_size_from_ssa_type(value_type)
				if size_bytes <= 0 {
					size_bytes = 8
				}
				size_val := b.mod.add_value_node(.constant, i64_t, size_bytes.str(), 0)
				malloc_sym := b.mod.add_value_node(.unknown, 0, 'malloc', 0)
				raw_mem := b.mod.add_instr(.call, b.cur_block, voidptr_t, [malloc_sym, size_val])
				typed_ptr := b.mod.add_instr(.bitcast, b.cur_block, b.mod.type_store.get_ptr(value_type),
					[raw_mem])
				b.mod.add_instr(.store, b.cur_block, 0, [value_val, typed_ptr])
				data_ptr = typed_ptr
			}
		}
		data_i64 := b.coerce_value_to_type(data_ptr, i64_t)
		data_idx_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(32),
			data_idx.str(), 0)
		wrapped = b.mod.add_instr(.insertvalue, b.cur_block, sum_type, [wrapped, data_i64,
			data_idx_val])
	}
	return wrapped
}

fn (mut b Builder) addr_sumtype_variant_field(sum_ptr ValueID, sum_type TypeID, field_name string) ?ValueID {
	if field_name.starts_with('_') {
		return none
	}
	sum_name := b.struct_name_from_type_id(sum_type) or { return none }
	variants := b.sumtype_variants[sum_name] or { []TypeID{} }
	if variants.len == 0 {
		return none
	}
	mut matched_variant := TypeID(0)
	mut matched_field_idx := -1
	for variant in variants {
		mut variant_type := variant
		if variant_type <= 0 || variant_type >= b.mod.type_store.types.len {
			continue
		}
		if b.mod.type_store.types[variant_type].kind == .ptr_t {
			variant_type = b.mod.type_store.types[variant_type].elem_type
		}
		if variant_type <= 0 || variant_type >= b.mod.type_store.types.len {
			continue
		}
		variant_info := b.mod.type_store.types[variant_type]
		if variant_info.kind != .struct_t {
			continue
		}
		mut idx := -1
		for i, variant_field in variant_info.field_names {
			if variant_field == field_name {
				idx = i
				break
			}
		}
		if idx >= 0 {
			if matched_variant != 0 {
				// Ambiguous field across variants; require explicit smartcast.
				return none
			}
			matched_variant = variant_type
			matched_field_idx = idx
		}
	}
	if matched_variant == 0 || matched_field_idx < 0 {
		return none
	}
	data_idx := b.field_index_by_name(sum_type, '_data')
	if data_idx < 0 {
		return none
	}
	data_field_type := b.mod.type_store.types[sum_type].fields[data_idx]
	data_slot_ptr_t := b.mod.type_store.get_ptr(data_field_type)
	data_idx_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(64), data_idx.str(),
		0)
	data_slot_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, data_slot_ptr_t, [
		sum_ptr,
		data_idx_val,
	])
	data_raw := b.mod.add_instr(.load, b.cur_block, data_field_type, [data_slot_ptr])
	variant_ptr_t := b.mod.type_store.get_ptr(matched_variant)
	variant_ptr := b.coerce_value_to_type(data_raw, variant_ptr_t)
	field_type := b.mod.type_store.types[matched_variant].fields[matched_field_idx]
	field_ptr_t := b.mod.type_store.get_ptr(field_type)
	field_idx_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(64), matched_field_idx.str(),
		0)
	return b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_t, [
		variant_ptr,
		field_idx_val,
	])
}

fn (b &Builder) get_var_ptr(name string) ?ValueID {
	return map_get_value_id(b.vars, name)
}

fn (b &Builder) get_type_alias_base(name string) ?string {
	return map_get_string(b.type_alias_bases, name)
}

fn (b &Builder) get_func_ret_type(name string) ?TypeID {
	return map_get_type_id(b.func_ret_types, name)
}

fn (b &Builder) get_func_first_param_type(name string) ?TypeID {
	fn_idx := b.find_function(name)
	if fn_idx < 0 {
		return none
	}
	if fn_idx >= b.mod.funcs.len {
		return none
	}
	if b.mod.funcs[fn_idx].params.len == 0 {
		return none
	}
	first_param := b.mod.funcs[fn_idx].params[0]
	if first_param <= 0 || first_param >= b.mod.values.len {
		return none
	}
	return b.mod.values[first_param].typ
}

fn (b &Builder) is_runtime_array_struct_type(type_id TypeID) bool {
	if type_id <= 0 || type_id >= b.mod.type_store.types.len {
		return false
	}
	typ := b.mod.type_store.types[type_id]
	if typ.kind != .struct_t {
		return false
	}
	if struct_name := b.struct_name_from_type_id(type_id) {
		return b.unqualified_type_name(struct_name) == 'array'
	}
	return typ.field_names.len == 6 && typ.field_names[0] == 'data'
		&& typ.field_names[1] == 'offset' && typ.field_names[2] == 'len'
		&& typ.field_names[3] == 'cap'
}

fn (b &Builder) variadic_param_array_info(param_typ TypeID) (bool, TypeID) {
	if param_typ <= 0 || param_typ >= b.mod.type_store.types.len {
		return false, 0
	}
	typ := b.mod.type_store.types[param_typ]
	if typ.kind == .ptr_t && b.is_runtime_array_struct_type(typ.elem_type) {
		return true, typ.elem_type
	}
	if b.is_runtime_array_struct_type(param_typ) {
		return false, param_typ
	}
	return false, 0
}

fn (mut b Builder) build_runtime_array_arg(values []ValueID, as_ptr bool, array_struct_t TypeID, preferred_elem_type TypeID) ValueID {
	i64_t := b.mod.type_store.get_int(64)
	i8_t := b.mod.type_store.get_int(8)
	i8_ptr_t := b.mod.type_store.get_ptr(i8_t)
	array_ptr_t := b.mod.type_store.get_ptr(array_struct_t)
	array_val_ptr := b.mod.add_instr(.alloca, b.cur_block, array_ptr_t, [])

	mut elem_type := if preferred_elem_type > 0 { preferred_elem_type } else { i64_t }
	if preferred_elem_type <= 0 {
		for v in values {
			if v > 0 && v < b.mod.values.len {
				v_typ := b.mod.values[v].typ
				if v_typ > 0 {
					elem_type = v_typ
					break
				}
			}
		}
	}
	mut elem_size := b.type_size_from_ssa_type(elem_type)
	if elem_size <= 0 {
		elem_size = 8
	}

	elem_count := values.len
	len_const := b.mod.add_value_node(.constant, i64_t, '${elem_count}', 0)
	elem_size_const := b.mod.add_value_node(.constant, i64_t, '${elem_size}', 0)
	zero_i64 := b.mod.add_value_node(.constant, i64_t, '0', 0)
	zero_i8_ptr := b.mod.add_value_node(.constant, i8_ptr_t, '0', 0)

	mut data_ptr := zero_i8_ptr
	if elem_count > 0 {
		total_bytes := elem_count * elem_size
		total_size_const := b.mod.add_value_node(.constant, i64_t, '${total_bytes}', 0)
		malloc_fn := b.mod.add_value_node(.unknown, 0, 'malloc', 0)
		data_ptr = b.mod.add_instr(.call, b.cur_block, i8_ptr_t, [malloc_fn, total_size_const])
		elem_ptr_t := b.mod.type_store.get_ptr(elem_type)
		typed_data := b.coerce_value_to_type(data_ptr, elem_ptr_t)
		for i, arg in values {
			idx := b.mod.add_value_node(.constant, i64_t, '${i}', 0)
			mut elem_ptr := ValueID(0)
			if elem_type > 0 && elem_type < b.mod.type_store.types.len
				&& b.mod.type_store.types[elem_type].kind == .struct_t {
				size_val := b.mod.add_value_node(.constant, i64_t, '${elem_size}', 0)
				byte_offset := b.mod.add_instr(.mul, b.cur_block, i64_t, [idx, size_val])
				elem_i8 := b.mod.add_instr(.add, b.cur_block, i8_ptr_t, [data_ptr, byte_offset])
				elem_ptr = b.mod.add_instr(.bitcast, b.cur_block, elem_ptr_t, [
					elem_i8,
				])
			} else {
				elem_ptr = b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_t,
					[typed_data, idx])
			}
			mut stored := arg
			if elem_type > 0 && elem_type < b.mod.type_store.types.len {
				elem_info := b.mod.type_store.types[elem_type]
				if elem_info.kind == .struct_t && arg > 0 && arg < b.mod.values.len {
					arg_typ := b.mod.values[arg].typ
					if arg_typ > 0 && arg_typ < b.mod.type_store.types.len {
						arg_info := b.mod.type_store.types[arg_typ]
						if arg_info.kind == .ptr_t && arg_info.elem_type == elem_type {
							stored = b.mod.add_instr(.load, b.cur_block, elem_type, [
								arg,
							])
						}
					}
				}
			}
			coerced := b.coerce_value_to_type(stored, elem_type)
			b.mod.add_instr(.store, b.cur_block, 0, [coerced, elem_ptr])
		}
	}

	data_ptr_ptr_t := b.mod.type_store.get_ptr(i8_ptr_t)
	idx0 := b.mod.add_value_node(.constant, i64_t, '0', 0)
	data_field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, data_ptr_ptr_t, [
		array_val_ptr,
		idx0,
	])
	b.mod.add_instr(.store, b.cur_block, 0, [data_ptr, data_field_ptr])

	i64_ptr_t := b.mod.type_store.get_ptr(i64_t)
	idx1 := b.mod.add_value_node(.constant, i64_t, '1', 0)
	offset_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, i64_ptr_t, [
		array_val_ptr,
		idx1,
	])
	b.mod.add_instr(.store, b.cur_block, 0, [zero_i64, offset_ptr])

	idx2 := b.mod.add_value_node(.constant, i64_t, '2', 0)
	len_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, i64_ptr_t, [
		array_val_ptr,
		idx2,
	])
	b.mod.add_instr(.store, b.cur_block, 0, [len_const, len_ptr])

	idx3 := b.mod.add_value_node(.constant, i64_t, '3', 0)
	cap_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, i64_ptr_t, [
		array_val_ptr,
		idx3,
	])
	b.mod.add_instr(.store, b.cur_block, 0, [len_const, cap_ptr])

	idx4 := b.mod.add_value_node(.constant, i64_t, '4', 0)
	flags_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, i64_ptr_t, [
		array_val_ptr,
		idx4,
	])
	b.mod.add_instr(.store, b.cur_block, 0, [zero_i64, flags_ptr])

	idx5 := b.mod.add_value_node(.constant, i64_t, '5', 0)
	elem_size_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, i64_ptr_t, [
		array_val_ptr,
		idx5,
	])
	b.mod.add_instr(.store, b.cur_block, 0, [elem_size_const, elem_size_ptr])

	if as_ptr {
		return array_val_ptr
	}
	return b.mod.add_instr(.load, b.cur_block, array_struct_t, [array_val_ptr])
}

fn (mut b Builder) variadic_elem_type_from_env(name string) ?TypeID {
	if b.env == unsafe { nil } || name == '' {
		return none
	}
	mut module_name := b.cur_module
	mut fn_name := name
	if name.starts_with('builtin__') {
		module_name = 'builtin'
		fn_name = name[9..]
	} else if sep := name.index('__') {
		module_name = name[..sep]
		fn_name = name[sep + 2..]
	}
	mut modules := []string{}
	if module_name != '' {
		modules << module_name
	}
	if module_name != 'builtin' {
		modules << 'builtin'
	}
	for mod_name in modules {
		if fn_typ := b.env.lookup_fn(mod_name, fn_name) {
			param_types := fn_typ.get_param_types()
			if param_types.len == 0 {
				continue
			}
			last_param := param_types[param_types.len - 1]
			if last_param is types.Array {
				return b.type_to_ssa(last_param.elem_type)
			}
		}
	}
	return none
}

fn (mut b Builder) pack_variadic_call_args(name string, mut args []ValueID) {
	fn_idx := b.find_function(name)
	if fn_idx < 0 || fn_idx >= b.mod.funcs.len {
		return
	}
	params := b.mod.funcs[fn_idx].params
	if params.len == 0 {
		return
	}
	last_param := params[params.len - 1]
	if last_param <= 0 || last_param >= b.mod.values.len {
		return
	}
	param_typ := b.mod.values[last_param].typ
	as_ptr, array_struct_t := b.variadic_param_array_info(param_typ)
	if array_struct_t == 0 {
		return
	}
	fixed_count := params.len - 1
	if args.len < fixed_count {
		return
	}
	// Already packed as a runtime array argument.
	if args.len == params.len {
		last_arg := args[fixed_count]
		if last_arg > 0 && last_arg < b.mod.values.len {
			last_arg_typ := b.mod.values[last_arg].typ
			if as_ptr {
				if last_arg_typ > 0 && last_arg_typ < b.mod.type_store.types.len {
					last_arg_info := b.mod.type_store.types[last_arg_typ]
					if last_arg_info.kind == .ptr_t
						&& b.is_runtime_array_struct_type(last_arg_info.elem_type) {
						return
					}
				}
			} else if b.is_runtime_array_struct_type(last_arg_typ) {
				return
			}
		}
	}
	if args.len < params.len {
		return
	}
	variadic_values := args[fixed_count..].clone()
	mut elem_type := TypeID(0)
	if t := b.variadic_elem_type_from_env(name) {
		elem_type = t
	}
	packed_arg := b.build_runtime_array_arg(variadic_values, as_ptr, array_struct_t, elem_type)
	mut packed_args := []ValueID{cap: fixed_count + 1}
	for i := 0; i < fixed_count; i++ {
		packed_args << args[i]
	}
	packed_args << packed_arg
	args.clear()
	args << packed_args
}

fn (mut b Builder) ensure_label_block(label string) BlockID {
	if map_has_key_block_id(b.label_blocks, label) {
		return b.label_blocks[label]
	}
	blk := b.mod.add_block(b.cur_func, 'label.${label}')
	b.label_blocks[label] = blk
	return blk
}

fn (mut b Builder) maybe_use_addressable_receiver(fn_name string, recv_expr ast.Expr, recv_val ValueID) ValueID {
	// For mut methods on selector receivers (e.g. m.key_values.expand()),
	// prefer passing the addressable receiver instead of a loaded value copy.
	if expected_typ := b.get_func_first_param_type(fn_name) {
		if expected_typ > 0 && expected_typ < b.mod.type_store.types.len {
			expected_info := b.mod.type_store.types[expected_typ]
			if expected_info.kind == .ptr_t {
				if recv_val > 0 && recv_val < b.mod.values.len {
					recv_typ := b.mod.values[recv_val].typ
					if recv_typ > 0 && recv_typ < b.mod.type_store.types.len
						&& b.mod.type_store.types[recv_typ].kind == .ptr_t {
						return recv_val
					}
				}
				mut recv_addr := ValueID(0)
				match recv_expr {
					ast.Ident, ast.SelectorExpr {
						recv_addr = b.addr(recv_expr)
					}
					else {}
				}
				if recv_addr != 0 && recv_addr < b.mod.values.len {
					recv_addr_typ := b.mod.values[recv_addr].typ
					if recv_addr_typ == expected_typ {
						return recv_addr
					}
					if recv_addr_typ > 0 && recv_addr_typ < b.mod.type_store.types.len {
						recv_addr_info := b.mod.type_store.types[recv_addr_typ]
						if recv_addr_info.kind == .ptr_t {
							if recv_addr_info.elem_type == expected_typ {
								return b.mod.add_instr(.load, b.cur_block, expected_typ,
									[
									recv_addr,
								])
							}
							if recv_addr_info.elem_type == expected_info.elem_type {
								return recv_addr
							}
						}
					}
				}
			}
		}
	}
	return recv_val
}

fn (mut b Builder) adjust_call_args_for_fn(name string, mut args []ValueID) {
	fn_idx := b.find_function(name)
	if fn_idx < 0 || fn_idx >= b.mod.funcs.len {
		return
	}
	params := b.mod.funcs[fn_idx].params
	if params.len == 0 || args.len == 0 {
		return
	}
	limit := if params.len < args.len { params.len } else { args.len }
	for i := 0; i < limit; i++ {
		arg_val := args[i]
		if arg_val <= 0 || arg_val >= b.mod.values.len {
			continue
		}
		param_val := params[i]
		if param_val <= 0 || param_val >= b.mod.values.len {
			continue
		}
		param_typ := b.mod.values[param_val].typ
		if param_typ <= 0 || param_typ >= b.mod.type_store.types.len {
			continue
		}
		arg_typ := b.mod.values[arg_val].typ
		param_info := b.mod.type_store.types[param_typ]
		// V print helpers accept string, but front-end calls can pass scalars.
		// Insert explicit `*__str` conversions so non-C backends get valid IR.
		if b.is_string_ptr_type_id(param_typ) && !b.is_string_ptr_type_id(arg_typ) {
			converted := b.coerce_value_to_string(arg_val)
			if converted != arg_val {
				args[i] = converted
				continue
			}
		}
		// For parameters lowered as `*SumType`, smartcasted variant arguments must
		// be wrapped back into `SumType` first, then passed by pointer.
		if param_info.kind == .ptr_t && param_info.elem_type > 0
			&& param_info.elem_type < b.mod.type_store.types.len
			&& b.is_sum_type_id(param_info.elem_type) {
			elem_sum_typ := param_info.elem_type
			// Already `*SumType` argument.
			if arg_typ > 0 && arg_typ < b.mod.type_store.types.len {
				arg_info := b.mod.type_store.types[arg_typ]
				if arg_info.kind == .ptr_t && arg_info.elem_type == elem_sum_typ {
					continue
				}
			}
			sum_name := b.struct_name_from_type_id(elem_sum_typ) or { continue }
			mut wrapped := arg_val
			if arg_typ != elem_sum_typ {
				wrapped = b.wrap_sumtype_value(sum_name, elem_sum_typ, arg_val)
			}
			if wrapped <= 0 || wrapped >= b.mod.values.len {
				continue
			}
			wrapped_typ := b.mod.values[wrapped].typ
			if wrapped_typ > 0 && wrapped_typ < b.mod.type_store.types.len {
				wrapped_info := b.mod.type_store.types[wrapped_typ]
				if wrapped_info.kind == .ptr_t && wrapped_info.elem_type == elem_sum_typ {
					args[i] = wrapped
					continue
				}
			}
			if wrapped_typ == elem_sum_typ {
				args[i] = b.box_call_arg_as_ptr(wrapped)
				continue
			}
		}
		// For by-value sum-type parameters, wrap smartcasted variant args.
		if b.is_sum_type_id(param_typ) && arg_typ != param_typ {
			if sum_name := b.struct_name_from_type_id(param_typ) {
				args[i] = b.wrap_sumtype_value(sum_name, param_typ, arg_val)
				continue
			}
		}
		// Keep scalar coercions for non-pointer params only.
		if param_info.kind != .ptr_t {
			coerced := b.coerce_value_to_type(arg_val, param_typ)
			if coerced != arg_val {
				args[i] = coerced
			}
		}
	}
	b.pack_variadic_call_args(name, mut args)
}

fn (mut b Builder) adjust_map_ptr_args_from_exprs(name string, arg_exprs []ast.Expr, mut args []ValueID) {
	fn_idx := b.find_function(name)
	if fn_idx < 0 || fn_idx >= b.mod.funcs.len {
		return
	}
	params := b.mod.funcs[fn_idx].params
	if params.len == 0 || args.len == 0 || arg_exprs.len == 0 {
		return
	}
	mut limit := params.len
	if args.len < limit {
		limit = args.len
	}
	if arg_exprs.len < limit {
		limit = arg_exprs.len
	}
	for i := 0; i < limit; i++ {
		param_val := params[i]
		if param_val <= 0 || param_val >= b.mod.values.len {
			continue
		}
		param_typ := b.mod.values[param_val].typ
		if param_typ <= 0 || param_typ >= b.mod.type_store.types.len {
			continue
		}
		param_info := b.mod.type_store.types[param_typ]
		if param_info.kind != .ptr_t || !b.is_map_type_id(param_info.elem_type) {
			continue
		}
		mut addr := ValueID(0)
		mut addr_expr := b.unwrap_paren_expr(arg_exprs[i])
		for {
			cur_expr := addr_expr
			if cur_expr is ast.ModifierExpr {
				if cur_expr.kind == .key_mut {
					addr_expr = b.unwrap_paren_expr(cur_expr.expr)
					continue
				}
			}
			if cur_expr is ast.PrefixExpr {
				if cur_expr.op == .key_mut {
					addr_expr = b.unwrap_paren_expr(cur_expr.expr)
					continue
				}
			}
			break
		}
		match addr_expr {
			ast.Ident, ast.SelectorExpr {
				addr = b.addr(addr_expr)
			}
			else {}
		}
		if addr <= 0 || addr >= b.mod.values.len {
			continue
		}
		addr_typ := b.mod.values[addr].typ
		if addr_typ == param_typ {
			args[i] = addr
			continue
		}
		if addr_typ <= 0 || addr_typ >= b.mod.type_store.types.len {
			continue
		}
		addr_info := b.mod.type_store.types[addr_typ]
		if addr_info.kind != .ptr_t {
			continue
		}
		if addr_info.elem_type == param_typ {
			args[i] = b.mod.add_instr(.load, b.cur_block, param_typ, [addr])
			continue
		}
		if addr_info.elem_type == param_info.elem_type {
			args[i] = addr
		}
	}
}

fn (b &Builder) get_enum_value(name string) ?int {
	return map_get_int(b.enum_values, name)
}

fn (b &Builder) const_global_value(name string) ?i64 {
	for g in b.mod.globals {
		if g.name == name {
			return g.initial_value
		}
	}
	return none
}

fn (b &Builder) get_iface_concrete_type(name string) ?string {
	return map_get_string(b.iface_concrete_types, name)
}

fn (b &Builder) get_array_size(name string) ?int {
	return map_get_int(b.var_array_sizes, name)
}

// lookup_type_in_scope looks up a type by name in the environment's scopes.
// First tries module scope, then builtin scope.
// Returns none if not found or if env is nil.
fn (b &Builder) lookup_type_in_scope(name string, module_name string) ?types.Type {
	if b.env == unsafe { nil } {
		return none
	}
	mut scope := &types.Scope(unsafe { nil })
	if s := b.env.get_scope(module_name) {
		scope = s
	} else if s := b.env.get_scope('builtin') {
		scope = s
	} else {
		return none
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
	// Prefer function-local scope first (includes transformer temp vars).
	if b.cur_func >= 0 && b.cur_func < b.mod.funcs.len {
		fn_name := b.mod.funcs[b.cur_func].name
		mut fn_scope := &types.Scope(unsafe { nil })
		mut found_fn_scope := false
		if s := b.env.get_fn_scope_by_key(fn_name) {
			fn_scope = s
			found_fn_scope = true
		} else if s := b.env.get_fn_scope(b.cur_module, fn_name) {
			fn_scope = s
			found_fn_scope = true
		} else if sep := fn_name.index('__') {
			module_name := fn_name[..sep]
			base_name := fn_name[sep + 2..]
			if s := b.env.get_fn_scope(module_name, base_name) {
				fn_scope = s
				found_fn_scope = true
			}
		}
		if found_fn_scope {
			if obj := fn_scope.lookup_parent(name, 0) {
				return obj.typ()
			}
		}
	}
	mut scope := &types.Scope(unsafe { nil })
	if s := b.env.get_scope(b.cur_module) {
		scope = s
	} else if s := b.env.get_scope('builtin') {
		scope = s
	} else {
		return none
	}
	if obj := scope.lookup_parent(name, 0) {
		return obj.typ()
	}
	return none
}

fn (mut b Builder) lookup_expr_type_from_env(pos token.Pos) ?TypeID {
	if b.env == unsafe { nil } {
		return none
	}
	if int(pos) <= 0 {
		return none
	}
	if typ := b.env.get_expr_type(int(pos)) {
		return b.type_to_ssa(typ)
	}
	return none
}

// extract_type_name extracts the name from a types.Type for receiver/type inference.
fn (b &Builder) extract_type_name(t types.Type) string {
	match t {
		types.Struct {
			return t.name
		}
		types.Interface {
			return t.name
		}
		types.SumType {
			return t.get_name()
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
		types.Map {
			return 'Map'
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
	if struct_type := map_get_string(b.var_struct_types, var_name) {
		return b.canonical_struct_type_name(struct_type)
	}
	// Fall back to environment lookup
	type_name := b.get_type_name_from_env(var_name)
	if type_name != '' {
		return b.canonical_struct_type_name(type_name)
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
			// Dynamic arrays in V use the runtime `array` struct layout.
			return b.ensure_runtime_array_type()
		}
		types.ArrayFixed {
			elem_type := b.type_to_ssa(t.elem_type)
			return b.mod.type_store.get_array(elem_type, t.len)
		}
		types.Map {
			// Dynamic maps in V use the runtime `map` struct layout.
			if map_t := b.get_struct_type_id('map') {
				return map_t
			}
			return b.mod.type_store.get_int(64)
		}
		types.Struct {
			// Check if already registered
			if struct_id := b.get_struct_type_id(t.name) {
				return struct_id
			}
			// Register a placeholder first so self-referential structs
			// (`next &Node`) do not recurse indefinitely.
			struct_id := b.mod.type_store.register(Type{
				kind:        .struct_t
				fields:      []TypeID{}
				field_names: []string{}
			})
			b.register_struct_type_name(t.name, struct_id)
			// Convert and populate fields.
			mut ssa_fields := []TypeID{}
			mut ssa_field_names := []string{}
			for field in t.fields {
				ssa_fields << b.type_to_ssa(field.typ)
				ssa_field_names << field.name
			}
			b.mod.type_store.types[struct_id] = Type{
				kind:        .struct_t
				fields:      ssa_fields
				field_names: ssa_field_names
			}
			return struct_id
		}
		types.SumType {
			sum_name := t.get_name()
			if sum_id := b.get_struct_type_id(sum_name) {
				return sum_id
			}
			if sum_name.contains('__') {
				short_name := sum_name.all_after_last('__')
				if sum_id := b.get_struct_type_id(short_name) {
					return sum_id
				}
			}
			return b.mod.type_store.get_int(64)
		}
		types.String {
			// String is a special type - return the string struct
			if struct_id := b.get_struct_type_id('string') {
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
			// Transformer emits pointer type casts as identifiers like `StructType*`.
			if typ.name.ends_with('*') {
				mut base_name := typ.name
				mut ptr_count := 0
				for base_name.ends_with('*') {
					base_name = base_name[..base_name.len - 1].trim_space()
					ptr_count++
				}
				base_type := b.ast_type_to_ssa(ast.Expr(ast.Ident{
					name: base_name
					pos:  typ.pos
				}))
				mut ptr_type := base_type
				for _ in 0 .. ptr_count {
					ptr_type = b.mod.type_store.get_ptr(ptr_type)
				}
				return ptr_type
			}
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
					if struct_id := b.get_struct_type_id('string') {
						return struct_id
					}
					return 0 // Should not happen since string is pre-registered
				}
				'voidptr', 'charptr', 'byteptr' {
					i8_t := b.mod.type_store.get_int(8)
					return b.mod.type_store.get_ptr(i8_t)
				}
				else {
					// Transformer models variadics as VArg_T aliases.
					if typ.name.starts_with('VArg_') {
						return b.ensure_runtime_array_type()
					}
					// Transformer-generated aliases like `Array_int`, `Array_fixed_*`,
					// `Map_string_int`, etc. should use the runtime container layouts.
					if typ.name.starts_with('Array_') {
						return b.ensure_runtime_array_type()
					}
					if typ.name.starts_with('Map_') || typ.name.starts_with('__Map_') {
						if map_t := b.get_struct_type_id('map') {
							return map_t
						}
					}
					// Check if it's a struct type
					if struct_t := b.get_struct_type_id(typ.name) {
						return struct_t
					}
					// Default to i64 for unknown types
					return b.mod.type_store.get_int(64)
				}
			}
		}
		ast.SelectorExpr {
			if typ.lhs is ast.Ident {
				qualified_name := '${typ.lhs.name}__${typ.rhs.name}'
				if struct_t := b.get_struct_type_id(qualified_name) {
					return struct_t
				}
				if map_has_key_bool(b.enum_names, qualified_name) {
					return b.mod.type_store.get_int(64)
				}
			}
			if struct_t := b.get_struct_type_id(typ.rhs.name) {
				return struct_t
			}
			if map_has_key_bool(b.enum_names, typ.rhs.name) {
				return b.mod.type_store.get_int(64)
			}
			return b.mod.type_store.get_int(64)
		}
		ast.PrefixExpr {
			// Pointer type like &T
			if typ.op == .amp {
				elem_type := b.ast_type_to_ssa(typ.expr)
				return b.mod.type_store.get_ptr(elem_type)
			}
			// Variadic type ...T lowers to runtime array.
			if typ.op == .ellipsis {
				return b.ensure_runtime_array_type()
			}
		}
		ast.ModifierExpr {
			// `shared T`, `atomic T`, `mut T` should preserve the underlying type shape.
			// Concurrency semantics are handled during statement lowering, not type layout.
			return b.ast_type_to_ssa(typ.expr)
		}
		ast.Type {
			// Handle ast.Type variants
			match typ {
				ast.ArrayType {
					// Dynamic arrays in V use the runtime `array` struct layout.
					return b.ensure_runtime_array_type()
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
				ast.MapType {
					if map_t := b.get_struct_type_id('map') {
						return map_t
					}
					return b.mod.type_store.get_int(64)
				}
				ast.OptionType {
					base_type := b.ast_type_to_ssa(typ.base_type)
					return b.ensure_option_like_type(base_type, false)
				}
				ast.ResultType {
					base_type := b.ast_type_to_ssa(typ.base_type)
					return b.ensure_option_like_type(base_type, true)
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
	} else if typ is ast.Type {
		match typ {
			ast.ArrayType {
				return 'array'
			}
			ast.MapType {
				return 'map'
			}
			else {}
		}
	}
	return ''
}

// ensure_runtime_array_type registers/returns the canonical runtime `array` type.
fn (mut b Builder) ensure_runtime_array_type() TypeID {
	if array_t := b.get_struct_type_id('array') {
		return array_t
	}
	i64_t := b.mod.type_store.get_int(64)
	i8_t := b.mod.type_store.get_int(8)
	data_ptr_t := b.mod.type_store.get_ptr(i8_t)
	array_id := b.mod.type_store.register(Type{
		kind:        .struct_t
		fields:      [data_ptr_t, i64_t, i64_t, i64_t, i64_t, i64_t]
		field_names: ['data', 'offset', 'len', 'cap', 'flags', 'element_size']
	})
	b.struct_types['array'] = array_id
	return array_id
}

// register_builtin_types registers built-in types like string that aren't
// declared as structs in source but need SSA struct type representations.
fn (mut b Builder) register_builtin_types() {
	b.ensure_runtime_array_type()

	// Register string struct type: { str &u8, len int, is_lit int }
	// This is 24 bytes on 64-bit: 8 (ptr) + 8 (int) + 8 (int)
	if map_has_key_type_id(b.struct_types, 'string') {
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
	// Sum-type variants may refer to structs registered later in the file list.
	// Resolve variant TypeIDs only after all types are known.
	b.resolve_pending_sumtype_variants()
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
	// Phase 4: Lower global/const initializers that need runtime setup.
	b.build_global_init_function()
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

fn (mut b Builder) resolve_pending_sumtype_variants() {
	if b.sumtype_variant_exprs.len == 0 {
		return
	}
	for sum_name, variants in b.sumtype_variant_exprs {
		mut variant_types := []TypeID{}
		for variant in variants {
			variant_types << b.ast_type_to_ssa(variant)
		}
		b.sumtype_variants[sum_name] = variant_types
	}
}

// build_fn_signatures registers function signatures from a single file
pub fn (mut b Builder) build_fn_signatures(file ast.File) {
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			if !b.should_emit_fn(file, stmt) {
				continue
			}
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
				if stmt.name == '_wymix' {
					b.generate_wyhash_stub(stmt.name, ret_type)
				}
				continue
			}

			// Map params with proper types
			mut param_types := []TypeID{}

			// For methods, add receiver as first parameter.
			if stmt.is_method {
				mut receiver_type := i64_t
				if stmt.receiver.typ is ast.Ident {
					if struct_t := b.get_struct_type_id(stmt.receiver.typ.name) {
						receiver_type = b.mod.type_store.get_ptr(struct_t)
					} else if stmt.receiver.typ.name.starts_with('Array_') {
						array_t := b.ensure_runtime_array_type()
						receiver_type = if stmt.receiver.is_mut {
							b.mod.type_store.get_ptr(array_t)
						} else {
							array_t
						}
					} else if stmt.receiver.typ.name.starts_with('Map_')
						|| stmt.receiver.typ.name.starts_with('__Map_') {
						if map_t := b.get_struct_type_id('map') {
							receiver_type = if stmt.receiver.is_mut {
								b.mod.type_store.get_ptr(map_t)
							} else {
								map_t
							}
						}
					}
				} else if stmt.receiver.typ is ast.PrefixExpr {
					if stmt.receiver.typ.op == .amp && stmt.receiver.typ.expr is ast.Ident {
						if struct_t := b.get_struct_type_id(stmt.receiver.typ.expr.name) {
							receiver_type = b.mod.type_store.get_ptr(struct_t)
						}
					}
				} else if stmt.receiver.typ is ast.Type {
					match stmt.receiver.typ {
						ast.ArrayType {
							array_t := b.ensure_runtime_array_type()
							receiver_type = if stmt.receiver.is_mut {
								b.mod.type_store.get_ptr(array_t)
							} else {
								array_t
							}
						}
						ast.MapType {
							if map_t := b.get_struct_type_id('map') {
								receiver_type = if stmt.receiver.is_mut {
									b.mod.type_store.get_ptr(map_t)
								} else {
									map_t
								}
							}
						}
						else {
							receiver_type = b.ast_type_to_ssa(stmt.receiver.typ)
						}
					}
				} else {
					receiver_type = b.ast_type_to_ssa(stmt.receiver.typ)
				}
				if receiver_type == 0 {
					receiver_type = i64_t
				}
				param_types << receiver_type
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
				mut receiver_type_name := b.canonical_struct_type_name(b.get_receiver_type_name(stmt.receiver.typ))
				// For types from non-main/non-builtin modules, use module-prefixed type name
				// to match transformer-generated calls (e.g., strings__Builder__write_string)
				if !receiver_type_name.contains('__') {
					receiver_module := b.module_for_type_name(receiver_type_name)
					if receiver_module != '' && receiver_module != 'main'
						&& receiver_module != 'builtin' {
						receiver_type_name = receiver_module + '__' + receiver_type_name
					} else {
						// Resolve type aliases for method name mangling (only for main/builtin types)
						if base_type := b.get_type_alias_base(receiver_type_name) {
							receiver_type_name = base_type
						}
					}
				} else {
					// Resolve type aliases for method name mangling (only for main/builtin types)
					if base_type := b.get_type_alias_base(receiver_type_name) {
						receiver_type_name = base_type
					}
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
			} else if b.cur_module != 'main' && b.cur_module != 'builtin' {
				// Add module prefix for non-method functions from non-main/non-builtin modules
				fn_name = b.cur_module + '__' + fn_name
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
			if !b.should_emit_fn(file, stmt) {
				continue
			}
			// Skip operator overloads (parsed with empty name)
			if stmt.name == '' {
				continue
			}
			// Skip extern C declarations, but keep V function bodies from `.c.v` files.
			if stmt.language == .c && stmt.stmts.len == 0 {
				continue
			}
			// Get mangled function name (same logic as first pass)
			mut fn_name := stmt.name
			if stmt.is_method {
				mut receiver_type_name := b.canonical_struct_type_name(b.get_receiver_type_name(stmt.receiver.typ))
				// For types from non-main/non-builtin modules, use module-prefixed type name
				if !receiver_type_name.contains('__') {
					receiver_module := b.module_for_type_name(receiver_type_name)
					if receiver_module != '' && receiver_module != 'main'
						&& receiver_module != 'builtin' {
						receiver_type_name = receiver_module + '__' + receiver_type_name
					} else {
						// Resolve type aliases for method name mangling (only for main/builtin types)
						if base_type := b.get_type_alias_base(receiver_type_name) {
							receiver_type_name = base_type
						}
					}
				} else {
					// Resolve type aliases for method name mangling (only for main/builtin types)
					if base_type := b.get_type_alias_base(receiver_type_name) {
						receiver_type_name = base_type
					}
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
			} else if b.cur_module != 'main' && b.cur_module != 'builtin' {
				// Add module prefix for non-method functions from non-main/non-builtin modules
				fn_name = b.cur_module + '__' + fn_name
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
	b.build_global_init_function()
}

fn (mut b Builder) build_global_init_function() {
	if b.global_init_entries.len == 0 {
		return
	}
	init_fn_name := '__v2_global_init'
	if b.find_function(init_fn_name) >= 0 {
		return
	}
	fn_id := b.mod.new_function(init_fn_name, 0, [])
	b.func_ret_types[init_fn_name] = 0
	b.cur_func = fn_id
	b.cur_block = b.mod.add_block(fn_id, 'entry')
	b.vars = map[string]ValueID{}
	b.var_struct_types = map[string]string{}
	b.var_array_sizes = map[string]int{}
	b.var_array_elem_types = map[string]TypeID{}
	b.var_map_types = map[string][2]int{}
	b.var_map_value_types = map[string]TypeID{}
	b.iface_concrete_types = map[string]string{}
	b.defer_stmts = [][]ast.Stmt{}
	b.in_global_init = true
	for entry in b.global_init_entries {
		if entry.target <= 0 || entry.target >= b.mod.values.len {
			continue
		}
		val := b.expr(entry.expr)
		b.mod.add_instr(.store, b.cur_block, 0, [val, entry.target])
	}
	b.in_global_init = false
	b.mod.add_instr(.ret, b.cur_block, 0, [])
}

fn (b &Builder) resolve_call_name_for_type_inference(lhs ast.Expr) string {
	match lhs {
		ast.Ident {
			mut name := lhs.name
			if name.starts_with('builtin__') {
				name = name[9..]
			}
			if map_has_key_type_id(b.func_ret_types, name) {
				return name
			}
			if !name.contains('__') && b.cur_module != 'main' && b.cur_module != 'builtin' {
				prefixed := b.cur_module + '__' + name
				if map_has_key_type_id(b.func_ret_types, prefixed) {
					return prefixed
				}
			}
			return name
		}
		ast.SelectorExpr {
			if lhs.lhs is ast.SelectorExpr {
				if resolved := b.resolve_module_type_method_call_name(lhs.lhs, lhs.rhs.name) {
					return resolved
				}
			}
			if lhs.lhs is ast.Ident {
				receiver_name := lhs.lhs.name
				method_name := lhs.rhs.name
				if b.is_module_call_receiver(receiver_name) {
					if receiver_name == 'C' || receiver_name == 'builtin' {
						return method_name
					}
					return receiver_name + '__' + method_name
				}
				return receiver_name + '__' + method_name
			}
		}
		else {}
	}
	return ''
}

fn (mut b Builder) infer_call_like_const_return_type(lhs ast.Expr) ?TypeID {
	mut module_name := ''
	mut fn_name := ''
	match lhs {
		ast.Ident {
			fn_name = lhs.name
			if fn_name.starts_with('builtin__') {
				fn_name = fn_name[9..]
				module_name = 'builtin'
			} else {
				module_name = b.cur_module
			}
		}
		ast.SelectorExpr {
			if lhs.lhs is ast.SelectorExpr {
				if resolved := b.resolve_module_type_method_call_name(lhs.lhs, lhs.rhs.name) {
					if ret_t := map_get_type_id(b.func_ret_types, resolved) {
						return ret_t
					}
					fn_idx := b.find_function(resolved)
					if fn_idx >= 0 && fn_idx < b.mod.funcs.len {
						return b.mod.funcs[fn_idx].typ
					}
				}
			}
			if lhs.lhs is ast.Ident {
				receiver_name := lhs.lhs.name
				if receiver_name == 'C' {
					return none
				}
				module_name = if receiver_name == 'builtin' { 'builtin' } else { receiver_name }
				fn_name = lhs.rhs.name
			}
		}
		else {}
	}
	if fn_name.len > 0 && b.env != unsafe { nil } {
		if module_name.len > 0 {
			if fn_typ := b.env.lookup_fn(module_name, fn_name) {
				if ret_t := fn_typ.get_return_type() {
					return b.type_to_ssa(ret_t)
				}
				return 0
			}
		}
		if module_name != 'builtin' {
			if fn_typ := b.env.lookup_fn('builtin', fn_name) {
				if ret_t := fn_typ.get_return_type() {
					return b.type_to_ssa(ret_t)
				}
				return 0
			}
		}
	}
	name := b.resolve_call_name_for_type_inference(lhs)
	if name == '' {
		return none
	}
	if ret_t := map_get_type_id(b.func_ret_types, name) {
		return ret_t
	}
	if !name.contains('__') && b.cur_module != 'main' && b.cur_module != 'builtin' {
		prefixed := b.cur_module + '__' + name
		if ret_t := map_get_type_id(b.func_ret_types, prefixed) {
			return ret_t
		}
	}
	fn_idx := b.find_function(name)
	if fn_idx >= 0 && fn_idx < b.mod.funcs.len {
		return b.mod.funcs[fn_idx].typ
	}
	if !name.contains('__') && b.cur_module != 'main' && b.cur_module != 'builtin' {
		prefixed := b.cur_module + '__' + name
		fn_idx2 := b.find_function(prefixed)
		if fn_idx2 >= 0 && fn_idx2 < b.mod.funcs.len {
			return b.mod.funcs[fn_idx2].typ
		}
	}
	return none
}

fn (mut b Builder) infer_const_decl_type(expr ast.Expr) TypeID {
	i64_t := b.mod.type_store.get_int(64)
	match expr {
		ast.StringLiteral, ast.StringInterLiteral {
			if st := b.get_struct_type_id('string') {
				return st
			}
			return i64_t
		}
		ast.BasicLiteral {
			if expr.kind in [.key_true, .key_false] {
				return b.mod.type_store.get_int(8)
			}
			return b.infer_literal_type(expr.value)
		}
		ast.ArrayInitExpr {
			if expr.typ !is ast.EmptyExpr {
				return b.ast_type_to_ssa(expr.typ)
			}
			if expr.exprs.len > 0 {
				elem_t := b.infer_const_decl_type(expr.exprs[0])
				return b.mod.type_store.get_array(elem_t, expr.exprs.len)
			}
			return b.mod.type_store.get_ptr(i64_t)
		}
		ast.MapInitExpr {
			if expr.typ !is ast.EmptyExpr {
				return b.ast_type_to_ssa(expr.typ)
			}
			if map_t := b.get_struct_type_id('map') {
				return map_t
			}
			return i64_t
		}
		ast.InitExpr {
			return b.ast_type_to_ssa(expr.typ)
		}
		ast.CastExpr {
			return b.ast_type_to_ssa(expr.typ)
		}
		ast.CallExpr {
			if ret_t := b.infer_call_like_const_return_type(expr.lhs) {
				return ret_t
			}
			return i64_t
		}
		ast.CallOrCastExpr {
			if ret_t := b.infer_call_like_const_return_type(expr.lhs) {
				return ret_t
			}
			if expr.lhs is ast.Ident {
				return b.ast_type_to_ssa(expr.lhs)
			}
			return b.infer_const_decl_type(expr.expr)
		}
		ast.PrefixExpr {
			if expr.op == .amp {
				base_t := b.infer_const_decl_type(expr.expr)
				return b.mod.type_store.get_ptr(base_t)
			}
			return b.infer_const_decl_type(expr.expr)
		}
		ast.ParenExpr {
			return b.infer_const_decl_type(expr.expr)
		}
		ast.InfixExpr {
			left_t := b.infer_const_decl_type(expr.lhs)
			right_t := b.infer_const_decl_type(expr.rhs)
			return b.infer_binop_result_type(expr.op, left_t, right_t)
		}
		ast.Ident {
			for i := b.mod.globals.len - 1; i >= 0; i-- {
				g := b.mod.globals[i]
				if g.name == expr.name {
					return g.typ
				}
			}
			if map_has_key_bool(b.enum_names, expr.name) {
				return i64_t
			}
			return i64_t
		}
		else {}
	}
	return i64_t
}

fn (mut b Builder) try_eval_const_expr_i64(expr ast.Expr) ?i64 {
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
			return none
		}
		ast.Ident {
			if v := b.const_global_value(expr.name) {
				return v
			}
			return none
		}
		ast.PrefixExpr {
			if expr.op == .minus {
				base := b.try_eval_const_expr_i64(expr.expr) or { return none }
				return -base
			}
			if expr.op == .plus {
				return b.try_eval_const_expr_i64(expr.expr)
			}
			return none
		}
		ast.InfixExpr {
			lhs := b.try_eval_const_expr_i64(expr.lhs) or { return none }
			rhs := b.try_eval_const_expr_i64(expr.rhs) or { return none }
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
						return none
					}
				}
				.mod {
					if rhs != 0 {
						lhs % rhs
					} else {
						return none
					}
				}
				.left_shift {
					lhs << rhs
				}
				.right_shift {
					lhs >> rhs
				}
				.and, .amp {
					lhs & rhs
				}
				.pipe {
					lhs | rhs
				}
				.xor {
					lhs ^ rhs
				}
				else {
					return none
				}
			}
		}
		ast.CallOrCastExpr {
			return b.try_eval_const_expr_i64(expr.expr)
		}
		ast.UnsafeExpr {
			if expr.stmts.len == 0 {
				return none
			}
			last_stmt := expr.stmts[expr.stmts.len - 1]
			if last_stmt is ast.ExprStmt {
				return b.try_eval_const_expr_i64(last_stmt.expr)
			}
			return none
		}
		ast.Type {
			// nil/none-like type nodes in const/global initializers lower to 0.
			return 0
		}
		ast.CastExpr {
			return b.try_eval_const_expr_i64(expr.expr)
		}
		ast.ParenExpr {
			return b.try_eval_const_expr_i64(expr.expr)
		}
		else {
			return none
		}
	}
}

fn (mut b Builder) resolve_const_ident_i64(name string) ?i64 {
	// Fast path: directly known constant value.
	if v := b.const_global_value(name) {
		if v != 0 {
			return v
		}
		// Keep resolving aliases for constants that are initialized through
		// global init expressions (e.g. const_a = const_b).
	}
	if gv := b.global_value_id(name) {
		for entry in b.global_init_entries {
			if entry.target != gv {
				continue
			}
			if direct := b.try_eval_const_expr_i64(entry.expr) {
				return direct
			}
			if entry.expr is ast.Ident {
				if aliased := b.const_global_value(entry.expr.name) {
					return aliased
				}
			}
		}
	}
	if v := b.const_global_value(name) {
		return v
	}
	return none
}

fn (mut b Builder) array_elem_type_from_size(elem_size i64) TypeID {
	return match elem_size {
		1 { b.mod.type_store.get_int(8) }
		2 { b.mod.type_store.get_int(16) }
		4 { b.mod.type_store.get_int(32) }
		8 { b.mod.type_store.get_int(64) }
		else { b.mod.type_store.get_int(64) }
	}
}

fn (mut b Builder) infer_array_elem_type(expr ast.Expr, rhs_val ValueID) ?TypeID {
	match expr {
		ast.ArrayInitExpr {
			if expr.typ !is ast.EmptyExpr {
				if expr.typ is ast.Type {
					match expr.typ {
						ast.ArrayType {
							return b.ast_type_to_ssa(expr.typ.elem_type)
						}
						ast.ArrayFixedType {
							return b.ast_type_to_ssa(expr.typ.elem_type)
						}
						else {}
					}
				}
			}
			if expr.exprs.len > 0 {
				return b.infer_const_decl_type(expr.exprs[0])
			}
		}
		ast.CallExpr {
			if expr.lhs is ast.Ident {
				base_name := expr.lhs.name.all_after_last('__')
				if base_name in ['new_array_from_c_array', 'new_array_from_c_array_noscan', 'new_array_from_c_array_no_alloc', 'new_array', '__new_array', '__new_array_noscan', '__new_array_with_default', '__new_array_with_default_noscan']
					&& expr.args.len >= 3 {
					if elem_size := b.try_eval_const_expr_i64(expr.args[2]) {
						return b.array_elem_type_from_size(elem_size)
					}
				}
			}
		}
		else {}
	}
	if rhs_val > 0 && rhs_val < b.mod.values.len {
		rhs_typ_id := b.mod.values[rhs_val].typ
		if rhs_typ_id > 0 && rhs_typ_id < b.mod.type_store.types.len {
			rhs_typ := b.mod.type_store.types[rhs_typ_id]
			if rhs_typ.kind == .array_t && rhs_typ.elem_type > 0 {
				return rhs_typ.elem_type
			}
			if rhs_typ.kind == .ptr_t && rhs_typ.elem_type > 0
				&& rhs_typ.elem_type < b.mod.type_store.types.len {
				elem_typ := b.mod.type_store.types[rhs_typ.elem_type]
				if elem_typ.kind == .array_t && elem_typ.elem_type > 0 {
					return elem_typ.elem_type
				}
			}
		}
	}
	return none
}

fn (mut b Builder) array_elem_type_from_types_type(typ types.Type) ?TypeID {
	match typ {
		types.Array {
			return b.type_to_ssa(typ.elem_type)
		}
		types.ArrayFixed {
			return b.type_to_ssa(typ.elem_type)
		}
		types.Pointer {
			return b.array_elem_type_from_types_type(typ.base_type)
		}
		types.Alias {
			return b.array_elem_type_from_types_type(typ.base_type)
		}
		else {}
	}
	return none
}

fn (mut b Builder) struct_field_type_from_types_type(typ types.Type, field_name string) ?types.Type {
	match typ {
		types.Struct {
			for field in typ.fields {
				if field.name == field_name {
					return field.typ
				}
			}
		}
		types.Pointer {
			return b.struct_field_type_from_types_type(typ.base_type, field_name)
		}
		types.Alias {
			return b.struct_field_type_from_types_type(typ.base_type, field_name)
		}
		else {}
	}
	return none
}

fn (mut b Builder) infer_array_elem_type_from_base_expr(expr ast.Expr) ?TypeID {
	base := b.unwrap_paren_expr(expr)
	match base {
		ast.Ident {
			if vt := map_get_type_id(b.var_array_elem_types, base.name) {
				return vt
			}
			if var_typ := b.lookup_var_type_from_env(base.name) {
				if elem_t := b.array_elem_type_from_types_type(var_typ) {
					return elem_t
				}
			}
		}
		ast.SelectorExpr {
			if base.lhs is ast.Ident {
				lhs_ident := base.lhs as ast.Ident
				field_key := '${lhs_ident.name}.${base.rhs.name}'
				if vt := map_get_type_id(b.var_array_elem_types, field_key) {
					return vt
				}
				if owner_typ := b.lookup_var_type_from_env(lhs_ident.name) {
					if field_typ := b.struct_field_type_from_types_type(owner_typ, base.rhs.name) {
						if elem_t := b.array_elem_type_from_types_type(field_typ) {
							return elem_t
						}
					}
				}
				// Module-qualified globals/constants (e.g. os.args).
				if b.env != unsafe { nil } {
					if scope_ref := b.env.get_scope(lhs_ident.name) {
						mut scope := scope_ref
						if obj := scope.lookup_parent(base.rhs.name, 0) {
							if elem_t := b.array_elem_type_from_types_type(obj.typ()) {
								return elem_t
							}
						}
					}
				}
			}
		}
		else {}
	}
	return none
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

fn (b &Builder) find_module_prefixed_receiver_type(short_type_name string, method_name string) ?string {
	if short_type_name == '' || method_name == '' {
		return none
	}
	suffix := '__${short_type_name}__${method_name}'
	mut found := ''
	for f in b.mod.funcs {
		if f.name.ends_with(suffix) {
			candidate := f.name.all_before_last('__${method_name}')
			if candidate == '' {
				continue
			}
			if found == '' {
				found = candidate
			} else if found != candidate {
				return none
			}
		}
	}
	if found != '' {
		return found
	}
	return none
}

fn (b &Builder) resolve_module_type_method_call_name(receiver ast.SelectorExpr, method_name string) ?string {
	if method_name == '' {
		return none
	}
	module_name := match receiver.lhs {
		ast.Ident { receiver.lhs.name }
		else { return none }
	}
	if module_name == '' {
		return none
	}
	// Local variables shadow module qualifiers in selectors.
	if _ := b.get_var_ptr(module_name) {
		return none
	}
	if !b.is_module_call_receiver(module_name) {
		return none
	}
	short_type_name := receiver.rhs.name
	if short_type_name == '' {
		return none
	}
	mut candidates := []string{}
	if resolved := b.find_module_prefixed_receiver_type(short_type_name, method_name) {
		candidates << '${resolved}__${method_name}'
	}
	if module_name != 'C' && module_name != 'builtin' {
		candidates << '${module_name}__${short_type_name}__${method_name}'
	}
	candidates << '${short_type_name}__${method_name}'
	candidates << '${short_type_name.to_lower()}__${method_name}'
	for candidate in candidates {
		if map_has_key_type_id(b.func_ret_types, candidate) || b.find_function(candidate) >= 0 {
			return candidate
		}
	}
	if module_name != 'C' && module_name != 'builtin' {
		return '${module_name}__${short_type_name}__${method_name}'
	}
	return none
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

// generate_array_contains_stub generates a stub implementation for array__contains_T
// functions. These are type-specialized contains functions generated by the transformer
// for 'elem in array' expressions. The implementation iterates through the array and
// compares each element using memcmp.
fn (mut b Builder) generate_array_contains_stub(name string) {
	i64_t := b.mod.type_store.get_int(64)
	bool_t := b.mod.type_store.get_int(1)
	ptr_t := b.mod.type_store.get_ptr(i64_t)

	// Save current function context
	save_func := b.cur_func
	save_block := b.cur_block

	// array__contains_T takes (array, elem) -> bool
	// The array is passed as a pointer to struct, elem is an i64
	mut array_t := i64_t
	if st := b.get_struct_type_id('array') {
		array_t = st
	}
	array_ptr_t := b.mod.type_store.get_ptr(array_t)

	fn_id := b.mod.new_function(name, bool_t, [array_ptr_t, i64_t])
	b.cur_func = fn_id
	b.func_ret_types[name] = bool_t

	// Create blocks
	entry := b.mod.add_block(fn_id, 'entry')
	loop_head := b.mod.add_block(fn_id, 'loop.head')
	loop_body := b.mod.add_block(fn_id, 'loop.body')
	found := b.mod.add_block(fn_id, 'found')
	not_found := b.mod.add_block(fn_id, 'not_found')

	// Entry block
	b.cur_block = entry
	arr_param := b.mod.add_value_node(.argument, array_ptr_t, 'arr', 0)
	elem_param := b.mod.add_value_node(.argument, i64_t, 'elem', 1)
	b.mod.funcs[fn_id].params << arr_param
	b.mod.funcs[fn_id].params << elem_param

	// Load array.data (field 0) and array.len (field 1)
	zero := b.mod.add_value_node(.constant, i64_t, '0', 0)
	one := b.mod.add_value_node(.constant, i64_t, '1', 1)
	field0_idx := b.mod.add_value_node(.constant, i64_t, '0', 0)
	field1_idx := b.mod.add_value_node(.constant, i64_t, '1', 1)

	// Get data pointer (field 0 of array struct)
	data_field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [
		arr_param,
		field0_idx,
	])
	data_ptr := b.mod.add_instr(.load, b.cur_block, ptr_t, [data_field_ptr])

	// Get len (field 1 of array struct)
	len_field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [
		arr_param,
		field1_idx,
	])
	arr_len := b.mod.add_instr(.load, b.cur_block, i64_t, [len_field_ptr])

	// Allocate loop counter
	counter_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
	b.mod.add_instr(.store, b.cur_block, 0, [zero, counter_ptr])

	// Jump to loop head
	loop_head_val := b.mod.blocks[loop_head].val_id
	b.mod.add_instr(.jmp, b.cur_block, 0, [loop_head_val])

	// Loop head: compare counter < len
	b.cur_block = loop_head
	counter := b.mod.add_instr(.load, b.cur_block, i64_t, [counter_ptr])
	cmp := b.mod.add_instr(.lt, b.cur_block, bool_t, [counter, arr_len])
	loop_body_val := b.mod.blocks[loop_body].val_id
	not_found_val := b.mod.blocks[not_found].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [cmp, loop_body_val, not_found_val])

	// Loop body: load element and compare
	b.cur_block = loop_body
	// Calculate element address: data + counter * 8 (assuming 8-byte elements)
	eight := b.mod.add_value_node(.constant, i64_t, '8', 8)
	offset := b.mod.add_instr(.mul, b.cur_block, i64_t, [counter, eight])
	elem_addr := b.mod.add_instr(.add, b.cur_block, ptr_t, [data_ptr, offset])
	loaded_elem := b.mod.add_instr(.load, b.cur_block, i64_t, [elem_addr])

	// Compare
	eq := b.mod.add_instr(.eq, b.cur_block, bool_t, [loaded_elem, elem_param])
	found_val := b.mod.blocks[found].val_id
	// If equal, jump to found; otherwise increment and loop
	next_counter := b.mod.add_instr(.add, b.cur_block, i64_t, [counter, one])
	b.mod.add_instr(.store, b.cur_block, 0, [next_counter, counter_ptr])
	b.mod.add_instr(.br, b.cur_block, 0, [eq, found_val, loop_head_val])

	// Found block: return true
	b.cur_block = found
	true_val := b.mod.add_value_node(.constant, bool_t, '1', 1)
	b.mod.add_instr(.ret, b.cur_block, 0, [true_val])

	// Not found block: return false
	b.cur_block = not_found
	false_val := b.mod.add_value_node(.constant, bool_t, '0', 0)
	b.mod.add_instr(.ret, b.cur_block, 0, [false_val])

	// Restore function context
	b.cur_func = save_func
	b.cur_block = save_block
}

// generate_str_stub generates a stub str() function that returns an empty string.
// Used for types that don't have a custom str() implementation (structs, maps, etc.)
fn (mut b Builder) generate_str_stub(name string) {
	i64_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i64_t)

	save_func := b.cur_func
	save_block := b.cur_block

	mut string_t := i64_t
	if st := b.get_struct_type_id('string') {
		string_t = st
	}
	string_ptr_t := b.mod.type_store.get_ptr(string_t)

	fn_id := b.mod.new_function(name, string_ptr_t, [ptr_t])
	b.cur_func = fn_id
	b.func_ret_types[name] = string_ptr_t

	entry := b.mod.add_block(fn_id, 'entry')
	b.cur_block = entry
	param := b.mod.add_value_node(.argument, ptr_t, 'self', 0)
	b.mod.funcs[fn_id].params << param

	// Return empty string: (string){.str = "", .len = 0, .is_lit = 1}
	empty_str := b.mod.add_value_node(.constant, ptr_t, '""', 0)
	zero := b.mod.add_value_node(.constant, i64_t, '0', 0)
	result := b.mod.add_instr(.inline_string_init, b.cur_block, string_ptr_t, [
		empty_str,
		zero,
		zero,
	])
	b.mod.add_instr(.ret, b.cur_block, 0, [result])

	b.cur_func = save_func
	b.cur_block = save_block
}

fn (mut b Builder) generate_ierror_type_name_stub(wrapper_name string, concrete_name string) {
	if map_has_key_type_id(b.func_ret_types, wrapper_name) || b.find_function(wrapper_name) >= 0 {
		return
	}

	i64_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i64_t)

	save_func := b.cur_func
	save_block := b.cur_block

	mut string_t := i64_t
	if st := b.get_struct_type_id('string') {
		string_t = st
	}
	string_ptr_t := b.mod.type_store.get_ptr(string_t)

	fn_id := b.mod.new_function(wrapper_name, string_ptr_t, [ptr_t])
	b.cur_func = fn_id
	b.func_ret_types[wrapper_name] = string_ptr_t

	entry := b.mod.add_block(fn_id, 'entry')
	b.cur_block = entry
	param := b.mod.add_value_node(.argument, ptr_t, '_obj', 0)
	b.mod.funcs[fn_id].params << param

	type_name := concrete_name.replace('__', '.')
	cstr := b.mod.add_value_node(.constant, ptr_t, '"${type_name}"', 0)
	name_len := b.mod.add_value_node(.constant, i64_t, type_name.len.str(), 0)
	is_lit := b.mod.add_value_node(.constant, i64_t, '1', 1)
	result := b.mod.add_instr(.inline_string_init, b.cur_block, string_ptr_t, [
		cstr,
		name_len,
		is_lit,
	])
	b.mod.add_instr(.ret, b.cur_block, 0, [result])

	b.cur_func = save_func
	b.cur_block = save_block
}

fn (mut b Builder) generate_noop_stub(name string) {
	i64_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i64_t)

	save_func := b.cur_func
	save_block := b.cur_block

	fn_id := b.mod.new_function(name, 0, [ptr_t])
	b.cur_func = fn_id
	b.func_ret_types[name] = 0

	entry := b.mod.add_block(fn_id, 'entry')
	b.cur_block = entry
	param := b.mod.add_value_node(.argument, ptr_t, 'self', 0)
	b.mod.funcs[fn_id].params << param
	b.mod.add_instr(.ret, b.cur_block, 0, [])

	b.cur_func = save_func
	b.cur_block = save_block
}

fn (mut b Builder) generate_fd_macro_stub(name string, returns_flag bool) {
	if map_has_key_type_id(b.func_ret_types, name) || b.find_function(name) >= 0 {
		return
	}
	i64_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i64_t)

	save_func := b.cur_func
	save_block := b.cur_block

	ret_type := if returns_flag { i64_t } else { TypeID(0) }

	fn_id := b.mod.new_function(name, ret_type, [i64_t, ptr_t])
	b.cur_func = fn_id
	b.func_ret_types[name] = ret_type

	entry := b.mod.add_block(fn_id, 'entry')
	b.cur_block = entry
	fd_arg := b.mod.add_value_node(.argument, i64_t, 'fd', 0)
	set_arg := b.mod.add_value_node(.argument, ptr_t, 'set', 0)
	b.mod.funcs[fn_id].params << fd_arg
	b.mod.funcs[fn_id].params << set_arg

	if ret_type == 0 {
		b.mod.add_instr(.ret, b.cur_block, 0, [])
	} else {
		zero := b.mod.add_value_node(.constant, ret_type, '0', 0)
		b.mod.add_instr(.ret, b.cur_block, 0, [zero])
	}

	b.cur_func = save_func
	b.cur_block = save_block
}

fn (mut b Builder) generate_unary_passthrough_stub(name string) {
	if map_has_key_type_id(b.func_ret_types, name) || b.find_function(name) >= 0 {
		return
	}
	i64_t := b.mod.type_store.get_int(64)

	save_func := b.cur_func
	save_block := b.cur_block

	fn_id := b.mod.new_function(name, i64_t, [i64_t])
	b.cur_func = fn_id
	b.func_ret_types[name] = i64_t

	entry := b.mod.add_block(fn_id, 'entry')
	b.cur_block = entry
	status_arg := b.mod.add_value_node(.argument, i64_t, 'status', 0)
	b.mod.funcs[fn_id].params << status_arg
	b.mod.add_instr(.ret, b.cur_block, 0, [status_arg])

	b.cur_func = save_func
	b.cur_block = save_block
}

fn (mut b Builder) generate_array_eq_stub() {
	i64_t := b.mod.type_store.get_int(64)
	bool_t := b.mod.type_store.get_int(1)
	ptr_t := b.mod.type_store.get_ptr(i64_t)

	save_func := b.cur_func
	save_block := b.cur_block

	mut array_t := i64_t
	if st := b.get_struct_type_id('array') {
		array_t = st
	}
	array_ptr_t := b.mod.type_store.get_ptr(array_t)

	// array__eq(a1 *array, a2 *array) -> bool
	fn_id := b.mod.new_function('array__eq', bool_t, [array_ptr_t, array_ptr_t])
	b.cur_func = fn_id
	b.func_ret_types['array__eq'] = bool_t

	entry := b.mod.add_block(fn_id, 'entry')
	len_eq_block := b.mod.add_block(fn_id, 'len_eq')
	ret_true := b.mod.add_block(fn_id, 'ret_true')
	ret_false := b.mod.add_block(fn_id, 'ret_false')

	// Entry: load lengths and compare
	b.cur_block = entry
	a1_param := b.mod.add_value_node(.argument, array_ptr_t, 'a1', 0)
	a2_param := b.mod.add_value_node(.argument, array_ptr_t, 'a2', 1)
	b.mod.funcs[fn_id].params << a1_param
	b.mod.funcs[fn_id].params << a2_param

	field2_idx := b.mod.add_value_node(.constant, i64_t, '2', 2)

	// Load a1.len (field 2)
	a1_len_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [a1_param, field2_idx])
	a1_len := b.mod.add_instr(.load, b.cur_block, i64_t, [a1_len_ptr])
	// Load a2.len (field 2)
	a2_len_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [a2_param, field2_idx])
	a2_len := b.mod.add_instr(.load, b.cur_block, i64_t, [a2_len_ptr])

	// Compare lengths
	len_cmp := b.mod.add_instr(.eq, b.cur_block, bool_t, [a1_len, a2_len])
	len_eq_val := b.mod.blocks[len_eq_block].val_id
	ret_false_val := b.mod.blocks[ret_false].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [len_cmp, len_eq_val, ret_false_val])

	// Lengths equal: compare data with memcmp
	b.cur_block = len_eq_block
	field0_idx := b.mod.add_value_node(.constant, i64_t, '0', 0)
	field5_idx := b.mod.add_value_node(.constant, i64_t, '5', 5)

	// Load a1.data (field 0)
	a1_data_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [a1_param, field0_idx])
	a1_data := b.mod.add_instr(.load, b.cur_block, ptr_t, [a1_data_ptr])
	// Load a2.data (field 0)
	a2_data_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [a2_param, field0_idx])
	a2_data := b.mod.add_instr(.load, b.cur_block, ptr_t, [a2_data_ptr])

	// Load a1.element_size (field 5)
	a1_esz_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_t, [a1_param, field5_idx])
	a1_esz := b.mod.add_instr(.load, b.cur_block, i64_t, [a1_esz_ptr])

	// Total bytes = len * element_size
	total_bytes := b.mod.add_instr(.mul, b.cur_block, i64_t, [a1_len, a1_esz])

	// Call memcmp(a1.data, a2.data, total_bytes)
	memcmp_sym := b.mod.add_value_node(.unknown, 0, 'memcmp', 0)
	memcmp_result := b.mod.add_instr(.call, b.cur_block, i64_t, [memcmp_sym, a1_data, a2_data,
		total_bytes])

	// Check if memcmp returned 0
	zero := b.mod.add_value_node(.constant, i64_t, '0', 0)
	is_eq := b.mod.add_instr(.eq, b.cur_block, bool_t, [memcmp_result, zero])
	ret_true_val := b.mod.blocks[ret_true].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [is_eq, ret_true_val, ret_false_val])

	// Return true
	b.cur_block = ret_true
	true_val := b.mod.add_value_node(.constant, bool_t, '1', 1)
	b.mod.add_instr(.ret, b.cur_block, 0, [true_val])

	// Return false
	b.cur_block = ret_false
	false_val := b.mod.add_value_node(.constant, bool_t, '0', 0)
	b.mod.add_instr(.ret, b.cur_block, 0, [false_val])

	b.cur_func = save_func
	b.cur_block = save_block
}

fn (mut b Builder) build_fn(decl ast.FnDecl, fn_id int) {
	b.cur_func = fn_id
	b.vars.clear()
	b.var_struct_types.clear()
	b.var_array_sizes.clear()
	b.var_array_elem_types.clear()
	b.var_map_types.clear()
	b.var_map_value_types.clear()
	b.var_types.clear()
	b.defer_stmts.clear()
	b.label_blocks.clear()

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
			if struct_t := b.get_struct_type_id(receiver.typ.name) {
				// Always use pointer for receiver (structs are passed by reference)
				receiver_type = b.mod.type_store.get_ptr(struct_t)
			} else if receiver.typ.name.starts_with('Array_') {
				// Transformer-generated specializations like `Array_int` use the
				// runtime `array` layout at codegen time.
				array_t := b.ensure_runtime_array_type()
				struct_type_name = 'array'
				receiver_type = if receiver.is_mut {
					b.mod.type_store.get_ptr(array_t)
				} else {
					array_t
				}
			} else if receiver.typ.name.starts_with('Map_')
				|| receiver.typ.name.starts_with('__Map_') {
				if map_t := b.get_struct_type_id('map') {
					struct_type_name = 'map'
					receiver_type = if receiver.is_mut {
						b.mod.type_store.get_ptr(map_t)
					} else {
						map_t
					}
				}
			}
		} else if receiver.typ is ast.PrefixExpr {
			// Handle pointer receiver types like &map
			if receiver.typ.op == .amp && receiver.typ.expr is ast.Ident {
				struct_type_name = receiver.typ.expr.name
				if struct_t := b.get_struct_type_id(struct_type_name) {
					receiver_type = b.mod.type_store.get_ptr(struct_t)
				}
			}
		} else if receiver.typ is ast.Type {
			match receiver.typ {
				ast.ArrayType {
					// Array methods (e.g. []int.str()) need the runtime array struct receiver.
					// Lowering them to i64 breaks field access (a.len/a[i]) in generated SSA.
					array_t := b.ensure_runtime_array_type()
					struct_type_name = 'array'
					receiver_type = if receiver.is_mut {
						b.mod.type_store.get_ptr(array_t)
					} else {
						array_t
					}
				}
				ast.MapType {
					if map_t := b.get_struct_type_id('map') {
						struct_type_name = 'map'
						receiver_type = if receiver.is_mut {
							b.mod.type_store.get_ptr(map_t)
						} else {
							map_t
						}
					}
				}
				else {}
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
		// Determine actual parameter type from declaration.
		// Struct parameters are passed as pointers to match signature lowering.
		mut declared_param_type := b.ast_type_to_ssa(param.typ)
		if declared_param_type == 0 {
			declared_param_type = i32_t
		}
		mut param_type := declared_param_type
		mut struct_type_name := ''
		if declared_param_type > 0 && declared_param_type < b.mod.type_store.types.len {
			if b.mod.type_store.types[declared_param_type].kind == .struct_t {
				param_type = b.mod.type_store.get_ptr(declared_param_type)
			}
		}
		if param.typ is ast.Ident {
			if b.mod.type_store.types[declared_param_type].kind == .struct_t {
				struct_type_name = param.typ.name
			}
		} else if param.typ is ast.SelectorExpr {
			// Module-qualified type like strings.Builder
			if b.mod.type_store.types[declared_param_type].kind == .struct_t {
				struct_type_name = param.typ.rhs.name
			}
		} else if param.typ is ast.Type {
			match param.typ {
				ast.MapType {
					struct_type_name = 'map'
				}
				ast.ArrayType {
					struct_type_name = 'array'
				}
				else {}
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
		if param.typ is ast.Type {
			match param.typ {
				ast.MapType {
					map_info := MapDeclInfo{
						key_bytes:   b.type_size_from_ast(param.typ.key_type)
						value_bytes: b.type_size_from_ast(param.typ.value_type)
						value_type:  b.ast_type_to_ssa(param.typ.value_type)
					}
					mut map_bytes := [2]int{}
					map_bytes[0] = map_info.key_bytes
					map_bytes[1] = map_info.value_bytes
					b.var_map_types[param.name] = map_bytes
					b.var_map_value_types[param.name] = map_info.value_type
				}
				else {}
			}
		}

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
		if size := b.get_array_size(for_in.expr.name) {
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
				// println(node) // TODO: struct printing not supported in cleanc
				return
			}
			if node.lhs.len == 0 {
				println('AssignStmt node.lhs.len == 0')
				// println(node) // TODO: struct printing not supported in cleanc
				return
			}

			// Check for multi-return assignment: a, b := foo() / a, b = foo()
			if node.lhs.len > 1 && node.rhs.len == 1 && node.op in [.decl_assign, .assign] {
				// Multi-return: evaluate RHS (which returns a tuple)
				tuple_val := b.expr(node.rhs[0])
				tuple_typ := b.mod.values[tuple_val].typ
				tuple_info := b.mod.type_store.types[tuple_typ]

				// Extract each value from the tuple and assign to LHS variables
				for i, lhs_expr in node.lhs {
					mut name := ''
					if lhs_expr is ast.ModifierExpr {
						if lhs_expr.expr is ast.Ident {
							name = lhs_expr.expr.name
						}
					} else if lhs_expr is ast.Ident {
						name = lhs_expr.name
					}

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

					if node.op == .decl_assign {
						// Declaration form: allocate and bind new slots.
						ptr_t := b.mod.type_store.get_ptr(elem_typ)
						stack_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
						b.mod.add_instr(.store, b.cur_block, 0, [elem_val, stack_ptr])
						b.vars[name] = stack_ptr
					} else {
						// Assignment form: store into existing l-values.
						ptr := b.addr(lhs_expr)
						if ptr != 0 {
							b.mod.add_instr(.store, b.cur_block, 0, [elem_val, ptr])
						}
					}
				}
				return
			}

			mut rhs_val := b.expr(node.rhs[0])

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

				// Prefer checker-validated declaration types (including transformer temp vars)
				// over raw RHS inference, so map get_check + deref patterns keep pointer depth.
				if name != '_' {
					if decl_t := b.lookup_var_type_from_env(name) {
						target_t := b.type_to_ssa(decl_t)
						if target_t > 0 {
							rhs_val = b.coerce_value_to_type(rhs_val, target_t)
						}
						struct_name := b.extract_type_name(decl_t)
						if struct_name != '' {
							b.var_struct_types[name] = struct_name
						}
						if elem_t := b.array_elem_type_from_types_type(decl_t) {
							b.var_array_elem_types[name] = elem_t
						}
					}
				}

				// Track struct/enum type for method resolution and match shorthand
				rhs_expr := node.rhs[0]
				if rhs_expr is ast.InitExpr {
					// Track map-typed struct fields for selector-based map indexing:
					// e.g. `pl.labels[5]` -> key `pl.labels`.
					for field in rhs_expr.fields {
						field_key := '${name}.${field.name}'
						if info := b.map_decl_info_from_expr(field.value) {
							mut map_bytes := [2]int{}
							map_bytes[0] = info.key_bytes
							map_bytes[1] = info.value_bytes
							b.var_map_types[field_key] = map_bytes
							b.var_map_value_types[field_key] = info.value_type
						}
						if elem_t := b.infer_array_elem_type(field.value, 0) {
							b.var_array_elem_types[field_key] = elem_t
						}
					}
				}

				// Special handling for map initialization - can be MapInitExpr or InitExpr with MapType
				mut is_map := false
				if rhs_expr is ast.MapInitExpr {
					is_map = true
				} else if rhs_expr is ast.InitExpr {
					// Check if it's a map init: map[K]V{}
					if _ := b.map_type_from_type_expr(rhs_expr.typ) {
						is_map = true
					}
				}

				rhs_type := b.mod.values[rhs_val].typ
				if !is_map {
					if map_t := b.get_struct_type_id('map') {
						if rhs_type == map_t {
							is_map = true
						} else if rhs_type > 0 && rhs_type < b.mod.type_store.types.len {
							rhs_info := b.mod.type_store.types[rhs_type]
							if rhs_info.kind == .ptr_t && rhs_info.elem_type == map_t {
								is_map = true
							}
						}
					}
				}

				if is_map {
					// Track map layout for map indexing helpers.
					mut map_info := MapDeclInfo{
						key_bytes:   8
						value_bytes: 8
						value_type:  b.mod.type_store.get_int(64)
					}
					if info := b.map_decl_info_from_expr(rhs_expr) {
						map_info = info
					} else if decl_t := b.lookup_var_type_from_env(name) {
						if info := b.map_decl_info_from_type(decl_t) {
							map_info = info
						}
					}
					mut map_bytes := [2]int{}
					map_bytes[0] = map_info.key_bytes
					map_bytes[1] = map_info.value_bytes
					b.var_map_types[name] = map_bytes
					b.var_map_value_types[name] = map_info.value_type
					b.var_struct_types[name] = 'map'

					// Keep map vars as pointers to a concrete map value.
					if rhs_type > 0 && rhs_type < b.mod.type_store.types.len
						&& b.mod.type_store.types[rhs_type].kind == .ptr_t {
						b.vars[name] = rhs_val
					} else {
						mut stored_typ := rhs_type
						if stored_typ == 0 {
							stored_typ = b.mod.type_store.get_int(64)
						}
						ptr_t := b.mod.type_store.get_ptr(stored_typ)
						stack_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])
						b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, stack_ptr])
						b.vars[name] = stack_ptr
					}
				} else {
					// Alloca for non-map types

					// Get type from RHS or default to i32
					mut stored_typ := rhs_type
					if stored_typ == 0 {
						stored_typ = b.mod.type_store.get_int(64)
					}
					ptr_t := b.mod.type_store.get_ptr(stored_typ)
					stack_ptr := b.mod.add_instr(.alloca, b.cur_block, ptr_t, [])

					// Store
					b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, stack_ptr])
					b.vars[name] = stack_ptr
					if elem_t := b.infer_array_elem_type(rhs_expr, rhs_val) {
						b.var_array_elem_types[name] = elem_t
					}

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
						} else if rhs_expr.op == .mul && rhs_expr.expr is ast.PrefixExpr {
							inner := rhs_expr.expr as ast.PrefixExpr
							if inner.op == .amp && inner.expr is ast.CallOrCastExpr {
								cast_expr := inner.expr as ast.CallOrCastExpr
								if cast_expr.lhs is ast.Ident {
									b.var_struct_types[name] = cast_expr.lhs.name
								}
							}
						}
					} else if rhs_expr is ast.SelectorExpr {
						// Track enum type for variables assigned enum values (e.g., color1 := Color.red)
						if rhs_expr.lhs is ast.Ident {
							enum_name := '${rhs_expr.lhs.name}__${rhs_expr.rhs.name}'
							if map_has_key_int(b.enum_values, enum_name) {
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
								if map_has_key_int(b.enum_values, enum_name) {
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
							if map_has_key_bool(b.interface_names, iface_name) {
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
					// Fallback: infer struct name from the resolved RHS type ID.
					if !map_has_key_string(b.var_struct_types, name) && rhs_type > 0
						&& rhs_type < b.mod.type_store.types.len {
						rhs_info := b.mod.type_store.types[rhs_type]
						mut struct_typ := TypeID(0)
						if rhs_info.kind == .struct_t {
							struct_typ = rhs_type
						} else if rhs_info.kind == .ptr_t && rhs_info.elem_type > 0
							&& rhs_info.elem_type < b.mod.type_store.types.len {
							elem_info := b.mod.type_store.types[rhs_info.elem_type]
							if elem_info.kind == .struct_t {
								struct_typ = rhs_info.elem_type
							}
						}
						if struct_typ > 0 {
							if struct_name := b.struct_name_from_type_id(struct_typ) {
								b.var_struct_types[name] = struct_name
							}
						}
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
						if map_has_key_map_bytes(b.var_map_types, var_name) {
							// Map set: use builtin runtime map implementation.
							map_ptr := b.vars[var_name]
							key_val := b.expr(lhs_expr.expr)
							mut value_type := b.mod.type_store.get_int(64)
							if vt := map_get_type_id(b.var_map_value_types, var_name) {
								value_type = vt
							}
							b.emit_runtime_map_set(map_ptr, key_val, rhs_val, value_type)
							return
						}
					} else if lhs_expr.lhs is ast.SelectorExpr {
						if lhs_expr.lhs.lhs is ast.Ident {
							field_key := '${lhs_expr.lhs.lhs.name}.${lhs_expr.lhs.rhs.name}'
							if map_has_key_map_bytes(b.var_map_types, field_key) {
								map_ptr := b.addr(lhs_expr.lhs)
								key_val := b.expr(lhs_expr.expr)
								mut value_type := b.mod.type_store.get_int(64)
								if vt := map_get_type_id(b.var_map_value_types, field_key) {
									value_type = vt
								}
								b.emit_runtime_map_set(map_ptr, key_val, rhs_val, value_type)
								return
							}
						}
					}
					map_ptr := b.addr(lhs_expr.lhs)
					if b.is_map_slot(map_ptr) {
						key_val := b.expr(lhs_expr.expr)
						b.emit_runtime_map_set(map_ptr, key_val, rhs_val, b.mod.type_store.get_int(64))
						return
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
			mut has_ret_expr := false
			if node.exprs.len == 1 {
				has_ret_expr = true
				ret_val = b.expr(node.exprs[0])
			} else if node.exprs.len > 1 {
				has_ret_expr = true
				// Multi-return: build a tuple from the expressions
				mut elem_vals := []ValueID{}
				mut elem_types := []TypeID{}
				for e in node.exprs {
					val := b.expr(e)
					elem_vals << val
					elem_types << b.mod.values[val].typ
				}
				// Prefer the declared function return tuple type to keep field layout
				// stable (e.g. u32/u32), even when expression inference widens values.
				mut tuple_t := TypeID(0)
				if b.cur_func >= 0 && b.cur_func < b.mod.funcs.len {
					declared_ret := b.mod.funcs[b.cur_func].typ
					if declared_ret > 0 && declared_ret < b.mod.type_store.types.len {
						declared_ret_t := b.mod.type_store.types[declared_ret]
						if declared_ret_t.kind == .struct_t
							&& declared_ret_t.fields.len == elem_vals.len {
							tuple_t = declared_ret
						}
					}
				}
				if tuple_t == 0 {
					tuple_t = b.mod.type_store.get_tuple(elem_types)
				}
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

			// 1.5. Coerce return values to transformed wrapper/sum types when needed.
			if has_ret_expr && b.cur_func >= 0 && b.cur_func < b.mod.funcs.len {
				declared_ret_type := b.mod.funcs[b.cur_func].typ
				if declared_ret_type > 0 && declared_ret_type < b.mod.type_store.types.len
					&& node.exprs.len == 1 {
					expr0 := node.exprs[0]
					if b.is_option_like_type_id(declared_ret_type) {
						mut is_none_ret := false
						match expr0 {
							ast.Keyword {
								is_none_ret = expr0.tok in [.key_none, .key_nil]
							}
							ast.Type {
								is_none_ret = true
							}
							else {}
						}
						if ret_val <= 0 || ret_val >= b.mod.values.len
							|| b.mod.values[ret_val].typ != declared_ret_type {
							ret_val_type := if ret_val > 0 && ret_val < b.mod.values.len {
								b.mod.values[ret_val].typ
							} else {
								TypeID(0)
							}
							err_idx := b.field_index_by_name(declared_ret_type, 'err')
							err_field_type := if err_idx >= 0
								&& err_idx < b.mod.type_store.types[declared_ret_type].fields.len {
								b.mod.type_store.types[declared_ret_type].fields[err_idx]
							} else {
								TypeID(0)
							}
							mut is_error_ret := false
							if ret_val_type > 0 && err_field_type > 0
								&& ret_val_type == err_field_type {
								is_error_ret = true
							}
							if is_none_ret {
								ret_val = b.wrap_option_like_value(declared_ret_type,
									1, 0, 0)
							} else if is_error_ret {
								ret_val = b.wrap_option_like_value(declared_ret_type,
									1, 0, ret_val)
							} else {
								ret_val = b.wrap_option_like_value(declared_ret_type,
									0, ret_val, 0)
							}
						}
					} else if b.is_sum_type_id(declared_ret_type) && ret_val > 0
						&& ret_val < b.mod.values.len
						&& b.mod.values[ret_val].typ != declared_ret_type {
						if sum_name := b.struct_name_from_type_id(declared_ret_type) {
							ret_val = b.wrap_sumtype_value(sum_name, declared_ret_type,
								ret_val)
						}
					}
				}
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
			has_post := node.post !is ast.EmptyStmt
			head_blk := b.mod.add_block(b.cur_func, 'for.head')
			body_blk := b.mod.add_block(b.cur_func, 'for.body')
			// Create a separate post block when there's a post statement,
			// so that `continue` jumps to the increment instead of the condition.
			mut post_blk := head_blk
			if has_post {
				post_blk = b.mod.add_block(b.cur_func, 'for.post')
			}
			exit_blk := b.mod.add_block(b.cur_func, 'for.exit')

			b.loop_stack << LoopInfo{
				head: post_blk // continue jumps to post (increment), or head if no post
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
			if has_post {
				// Jump from body to post block
				if !b.is_block_terminated(b.cur_block) {
					post_val := b.mod.blocks[post_blk].val_id
					b.mod.add_instr(.jmp, b.cur_block, 0, [post_val])
				}
				// Post block: execute post statement, then jump to head
				b.cur_block = post_blk
				b.stmt(node.post)
				if !b.is_block_terminated(b.cur_block) {
					b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])
				}
			} else {
				// No post: jump directly to head
				if !b.is_block_terminated(b.cur_block) {
					b.mod.add_instr(.jmp, b.cur_block, 0, [head_val])
				}
			}

			// 6. Exit
			b.cur_block = exit_blk
			b.loop_stack.pop()
		}
		ast.FlowControlStmt {
			match node.op {
				.key_goto {
					if node.label == '' {
						return
					}
					target := b.ensure_label_block(node.label)
					target_val := b.mod.blocks[target].val_id
					b.mod.add_instr(.jmp, b.cur_block, 0, [target_val])
					// Continue SSA construction in a detached block so following statements
					// remain syntactically processable without becoming reachable.
					b.cur_block = b.mod.add_block(b.cur_func, 'goto.after')
				}
				.key_break, .key_continue {
					if b.loop_stack.len == 0 {
						return
					}
					info := b.loop_stack.last()
					target := if node.op == .key_break { info.exit } else { info.head }
					target_val := b.mod.blocks[target].val_id
					b.mod.add_instr(.jmp, b.cur_block, 0, [target_val])
					b.cur_block = b.mod.add_block(b.cur_func, 'flow.after')
				}
				else {}
			}
		}
		ast.StructDecl {
			// Keep the first registered layout for runtime container names.
			if node.name in ['array', 'string'] {
				if _ := b.get_struct_type_id(node.name) {
					b.type_modules[node.name] = b.cur_module
					return
				}
			}

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
					if map_has_key_bool(b.fn_type_aliases, field.typ.name) {
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
			// Register struct name for type lookup.
			b.register_struct_type_name(node.name, type_id)
		}
		ast.GlobalDecl {
			for field in node.fields {
				mut field_type := b.mod.type_store.get_int(64)
				if field.typ !is ast.EmptyExpr {
					field_type = b.ast_type_to_ssa(field.typ)
				} else if field.value !is ast.EmptyExpr {
					field_type = b.infer_const_decl_type(field.value)
				}
				mut initial_value := i64(0)
				mut needs_runtime_init := field.value !is ast.EmptyExpr
				if field.value !is ast.EmptyExpr {
					if int_val := b.try_eval_const_expr_i64(field.value) {
						initial_value = int_val
						if field_type > 0 && field_type < b.mod.type_store.types.len {
							field_kind := b.mod.type_store.types[field_type].kind
							needs_runtime_init = field_kind !in [.int_t, .ptr_t]
						}
					}
				}
				target := if needs_runtime_init {
					b.mod.add_global(field.name, field_type, false)
				} else {
					b.mod.add_global_with_value(field.name, field_type, false, initial_value)
				}
				if field.value !is ast.EmptyExpr && needs_runtime_init {
					b.global_init_entries << GlobalInitEntry{
						target: target
						expr:   field.value
					}
				}
			}
		}
		ast.ConstDecl {
			for field in node.fields {
				field_type := b.infer_const_decl_type(field.value)
				mut initial_value := i64(0)
				mut needs_runtime_init := true
				if int_val := b.try_eval_const_expr_i64(field.value) {
					initial_value = int_val
					if field_type > 0 && field_type < b.mod.type_store.types.len {
						field_kind := b.mod.type_store.types[field_type].kind
						needs_runtime_init = field_kind !in [.int_t, .ptr_t]
					}
				}
				target := b.mod.add_global_with_value(field.name, field_type, true, initial_value)
				if needs_runtime_init {
					b.global_init_entries << GlobalInitEntry{
						target: target
						expr:   field.value
					}
				}
			}
		}
		ast.EnumDecl {
			// Enums are lowered to integer constants
			// Each field gets an incrementing value starting from 0 (or explicit value)
			// Track enum type name for cast detection
			b.enum_names[node.name] = true
			field_type := b.mod.type_store.get_int(64)

			// Check if this is a @[flag] enum.
			// Avoid function-literal helpers here: v2 cleanc does not lower them reliably yet.
			mut is_flag_enum := false
			for attr in node.attributes {
				if attr.name == 'flag' {
					is_flag_enum = true
					break
				}
				if attr.value is ast.Ident && attr.value.name == 'flag' {
					is_flag_enum = true
					break
				}
			}

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

			// Track flag enum names and generate has/all methods
			if is_flag_enum {
				b.flag_enum_names[node.name] = true
				b.generate_flag_enum_has(node.name, field_type)
				b.generate_flag_enum_all(node.name, field_type)
			}
		}
		ast.LabelStmt {
			if node.name != '' {
				label_blk := b.ensure_label_block(node.name)
				if b.cur_block != label_blk {
					if !b.is_block_terminated(b.cur_block) {
						label_val := b.mod.blocks[label_blk].val_id
						b.mod.add_instr(.jmp, b.cur_block, 0, [label_val])
					}
					b.cur_block = label_blk
				}
			}
			if node.stmt !is ast.EmptyStmt {
				b.stmt(node.stmt)
			}
		}
		ast.InterfaceDecl {
			// Track interface name and methods for vtable support
			b.interface_names[node.name] = true
			mut method_names := []string{}
			method_names << 'type_name'
			for field in node.fields {
				if field.name !in method_names {
					method_names << field.name
				}
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
			for method_name in method_names {
				// Each method is a function pointer
				field_types << ptr_t // function pointer
				field_names << method_name
			}
			// Register interface as a struct type
			t := Type{
				kind:        .struct_t
				fields:      field_types
				field_names: field_names
				width:       0
			}
			type_id := b.mod.type_store.register(t)
			b.register_struct_type_name(node.name, type_id)
			// Interface layouts must win over C shim structs (e.g. C.IError)
			// so interface method/value lowering sees the full vtable fields.
			b.struct_types[node.name] = type_id
			if !node.name.contains('__') && b.cur_module != '' && b.cur_module != 'main' {
				qualified := '${b.cur_module}__${node.name}'
				b.struct_types[qualified] = type_id
				b.type_modules[qualified] = b.cur_module
			}
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
				b.register_struct_type_name(node.name, type_id)
				b.sumtype_variant_exprs[node.name] = node.variants.clone()
			} else if node.base_type !is ast.EmptyExpr {
				// Type alias: look up the base type
				if node.base_type is ast.Ident {
					base_name := node.base_type.name
					if st := b.get_struct_type_id(base_name) {
						// Alias to existing struct type
						b.register_struct_type_name(node.name, st)
					} else {
						// Alias to primitive type - register as int64
						t := Type{
							kind:  .int_t
							width: 64
						}
						type_id := b.mod.type_store.register(t)
						b.register_struct_type_name(node.name, type_id)
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
							b.register_struct_type_name(node.name, ptr_t)
						}
						ast.ArrayType {
							// Array type alias (e.g., type Builder = []u8)
							// Map to 'array' for method resolution
							b.type_alias_bases[node.name] = 'array'
							// Register the type itself
							array_t := b.ensure_runtime_array_type()
							b.register_struct_type_name(node.name, array_t)
						}
						ast.MapType {
							// Map type alias (e.g., type Map = map[int]int)
							// Map to 'map' for method resolution
							b.type_alias_bases[node.name] = 'map'
							if map_t := b.get_struct_type_id('map') {
								b.register_struct_type_name(node.name, map_t)
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
				if enum_val := b.get_enum_value(enum_name) {
					i64_t := b.mod.type_store.get_int(64)
					return b.mod.add_value_node(.constant, i64_t, '${enum_val}', 0)
				}
			}
			// Check for enum shorthand (.value) - LHS is EmptyExpr
			if node.lhs is ast.EmptyExpr {
				if b.cur_match_type != '' {
					enum_name := '${b.cur_match_type}__${node.rhs.name}'
					if enum_val := b.get_enum_value(enum_name) {
						i64_t := b.mod.type_store.get_int(64)
						return b.mod.add_value_node(.constant, i64_t, '${enum_val}', 0)
					}
				}
				// Fallback: resolve `.field` by unique enum member name across known enums.
				mut matched_value := 0
				mut match_count := 0
				suffix := '__${node.rhs.name}'
				for key, value in b.enum_values {
					if key.ends_with(suffix) {
						matched_value = value
						match_count++
						if match_count > 1 {
							break
						}
					}
				}
				if match_count == 1 {
					i64_t := b.mod.type_store.get_int(64)
					return b.mod.add_value_node(.constant, i64_t, '${matched_value}',
						0)
				}
			}
			return b.expr_selector(node)
		}
		ast.IndexExpr {
			return b.expr_index(node)
		}
		ast.CastExpr {
			// Get the value being cast
			mut val := b.expr(node.expr)
			if val <= 0 || val >= b.mod.values.len {
				return val
			}

			// Determine the target type
			target_typ := b.ast_type_to_ssa(node.typ)
			if target_typ <= 0 || target_typ >= b.mod.type_store.types.len {
				return val
			}
			target_info := b.mod.type_store.types[target_typ]
			// Explicit casts from !T/?T wrappers to T should unwrap `.data`.
			if !b.is_option_like_type_id(target_typ) {
				if data_val := b.option_like_data_value(val) {
					val = data_val
				}
			}
			if val <= 0 || val >= b.mod.values.len {
				return val
			}
			val_typ := b.mod.values[val].typ
			if val_typ <= 0 || val_typ >= b.mod.type_store.types.len {
				return val
			}
			val_info := b.mod.type_store.types[val_typ]

			// `string -> byteptr/voidptr/uintptr` casts should use `.str` only
			// when the source is a string value, not an already-addressable `&string`.
			if target_info.kind in [.ptr_t, .int_t] && val_info.kind != .ptr_t {
				if data_ptr := b.string_data_ptr_from_value(val) {
					return b.coerce_value_to_type(data_ptr, target_typ)
				}
			}

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

			// Pointer/integer reinterpret casts used by transformed option/sum-type lowering.
			if target_info.kind == .ptr_t && val_info.kind in [.ptr_t, .int_t] {
				return b.mod.add_instr(.bitcast, b.cur_block, target_typ, [val])
			}
			if target_info.kind == .int_t && val_info.kind == .ptr_t {
				return b.mod.add_instr(.bitcast, b.cur_block, target_typ, [val])
			}
			if target_info.kind == .struct_t && b.is_sum_type_id(target_typ) {
				if node.typ is ast.Ident {
					return b.wrap_sumtype_value(node.typ.name, target_typ, val)
				}
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
					// sizeof(type) - lower to a concrete byte size for the queried type
					mut size := 8
					if node.exprs.len > 0 {
						size = b.type_size_from_ast(node.exprs[0])
					}
					return b.mod.add_value_node(.constant, i64_t, size.str(), 0)
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
			println('Builder: Unhandled expr ${node.name()}')
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
	if node.name.starts_with('__type_id_') {
		type_name := node.name['__type_id_'.len..]
		if st := b.get_struct_type_id(type_name) {
			i64_t := b.mod.type_store.get_int(64)
			return b.mod.add_value_node(.constant, i64_t, int(st).str(), 0)
		}
	}
	if node.name.starts_with('IError_') && node.name.ends_with('_wrapper') {
		payload := node.name['IError_'.len..node.name.len - '_wrapper'.len]
		method_name := payload.all_after_last('_')
		concrete_name := payload.all_before_last('_')
		if method_name in ['msg', 'code'] && concrete_name != '' {
			fn_name := b.resolve_method_fn_name(concrete_name, method_name)
			if fn_name != '' {
				i64_t := b.mod.type_store.get_int(64)
				return b.mod.add_value_node(.func_ref, i64_t, fn_name, 0)
			}
		} else if method_name == 'type_name' && concrete_name != '' {
			b.generate_ierror_type_name_stub(node.name, concrete_name)
			i64_t := b.mod.type_store.get_int(64)
			return b.mod.add_value_node(.func_ref, i64_t, node.name, 0)
		}
	}
	ptr := b.addr(ast.Expr(node))
	if ptr != 0 && ptr < b.mod.values.len {
		// Get type pointed to
		ptr_typ := b.mod.values[ptr].typ
		if ptr_typ > 0 && ptr_typ < b.mod.type_store.types.len {
			val_typ := b.mod.type_store.types[ptr_typ].elem_type
			return b.mod.add_instr(.load, b.cur_block, val_typ, [ptr])
		}
	}
	// Function value used as an expression (e.g. map hash/eq callback args).
	if map_has_key_type_id(b.func_ret_types, node.name) || b.find_function(node.name) >= 0 {
		i64_t := b.mod.type_store.get_int(64)
		return b.mod.add_value_node(.func_ref, i64_t, node.name, 0)
	}

	i64_t := b.mod.type_store.get_int(64)
	return b.mod.add_value_node(.constant, i64_t, '0', 0)
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
		if st := b.get_struct_type_id(node.typ.name) {
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
		// Transformed sum-type inits use nested field names like `_data._Variant`.
		// Map these to their parent field so the runtime `_data` slot is initialized.
		if field.name.contains('.') {
			parent_name := field.name.all_before('.')
			// Always prefer the concrete nested payload over synthetic parent placeholders.
			init_fields[parent_name] = field.value
		}
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
			mut val := b.expr(expr)
			if b.is_sum_type_id(struct_t) && field_name == '_data' {
				if val > 0 && val < b.mod.values.len {
					val_typ := b.mod.values[val].typ
					if val_typ > 0 && val_typ < b.mod.type_store.types.len {
						val_info := b.mod.type_store.types[val_typ]
						if val_info.kind !in [.ptr_t, .int_t, .float_t] {
							i8_t := b.mod.type_store.get_int(8)
							i64_t := b.mod.type_store.get_int(64)
							voidptr_t := b.mod.type_store.get_ptr(i8_t)
							mut size_bytes := b.type_size_from_ssa_type(val_typ)
							if size_bytes <= 0 {
								size_bytes = 8
							}
							size_val := b.mod.add_value_node(.constant, i64_t, size_bytes.str(),
								0)
							malloc_sym := b.mod.add_value_node(.unknown, 0, 'malloc',
								0)
							raw_mem := b.mod.add_instr(.call, b.cur_block, voidptr_t,
								[
								malloc_sym,
								size_val,
							])
							typed_ptr := b.mod.add_instr(.bitcast, b.cur_block, b.mod.type_store.get_ptr(val_typ),
								[raw_mem])
							b.mod.add_instr(.store, b.cur_block, 0, [val, typed_ptr])
							val = typed_ptr
						}
					}
				}
			}
			val = b.coerce_value_to_type(val, field_type)
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
	if node.rhs.name == 'len' && node.lhs is ast.Ident {
		if ptr := b.get_var_ptr(node.lhs.name) {
			if ptr > 0 && ptr < b.mod.values.len {
				slot_typ_id := b.mod.values[ptr].typ
				if slot_typ_id > 0 && slot_typ_id < b.mod.type_store.types.len {
					slot_typ := b.mod.type_store.types[slot_typ_id]
					if slot_typ.kind == .ptr_t && slot_typ.elem_type > 0
						&& slot_typ.elem_type < b.mod.type_store.types.len {
						stored_typ := b.mod.type_store.types[slot_typ.elem_type]
						if stored_typ.kind == .array_t {
							return b.mod.add_value_node(.constant, b.mod.type_store.get_int(64),
								stored_typ.len.str(), 0)
						}
						if stored_typ.kind == .ptr_t && stored_typ.elem_type > 0
							&& stored_typ.elem_type < b.mod.type_store.types.len {
							pointee_typ := b.mod.type_store.types[stored_typ.elem_type]
							if pointee_typ.kind == .array_t {
								return b.mod.add_value_node(.constant, b.mod.type_store.get_int(64),
									pointee_typ.len.str(), 0)
							}
						}
					}
				}
			}
		}
	}
	// Load value from field
	ptr := b.addr(ast.Expr(node))
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
		if map_has_key_map_bytes(b.var_map_types, var_name) {
			// Map access: use builtin runtime map implementation.
			map_ptr := b.vars[var_name]
			key_val := b.expr(node.expr)
			mut value_type := b.mod.type_store.get_int(64)
			if vt := map_get_type_id(b.var_map_value_types, var_name) {
				value_type = vt
			}
			return b.emit_runtime_map_get(map_ptr, key_val, value_type)
		}
	}
	if node.lhs is ast.SelectorExpr && node.lhs.lhs is ast.Ident {
		field_key := '${node.lhs.lhs.name}.${node.lhs.rhs.name}'
		if map_has_key_map_bytes(b.var_map_types, field_key) {
			map_ptr := b.addr(node.lhs)
			key_val := b.expr(node.expr)
			mut value_type := b.mod.type_store.get_int(64)
			if vt := map_get_type_id(b.var_map_value_types, field_key) {
				value_type = vt
			}
			return b.emit_runtime_map_get(map_ptr, key_val, value_type)
		}
	}
	// Nested map access: lhs resolves to a map value (e.g. m['k1']['k2']).
	map_ptr := b.addr(node.lhs)
	if b.is_map_slot(map_ptr) {
		key_val := b.expr(node.expr)
		return b.emit_runtime_map_get(map_ptr, key_val, b.mod.type_store.get_int(64))
	}

	// Load value from index
	ptr := b.addr(ast.Expr(node))
	if ptr > 0 && ptr < b.mod.values.len {
		ptr_typ_id := b.mod.values[ptr].typ
		if ptr_typ_id > 0 && ptr_typ_id < b.mod.type_store.types.len {
			ptr_typ := b.mod.type_store.types[ptr_typ_id]
			if ptr_typ.kind == .ptr_t && ptr_typ.elem_type > 0 {
				return b.mod.add_instr(.load, b.cur_block, ptr_typ.elem_type, [ptr])
			}
		}
	}
	return b.mod.add_value_node(.constant, b.mod.type_store.get_int(64), '0', 0)
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

	if node.op in [.key_is, .not_is] {
		return b.expr_infix_is_check(node, bool_t)
	}

	// Handle logical operators with proper short-circuit control flow.
	if node.op == .and {
		left := b.expr(node.lhs)
		rhs_blk := b.mod.add_block(b.cur_func, 'and.rhs')
		short_blk := b.mod.add_block(b.cur_func, 'and.short')
		merge_blk := b.mod.add_block(b.cur_func, 'and.end')
		result_ptr := b.mod.add_instr(.alloca, b.cur_block, b.mod.type_store.get_ptr(bool_t),
			[])

		rhs_val_id := b.mod.blocks[rhs_blk].val_id
		short_val_id := b.mod.blocks[short_blk].val_id
		b.mod.add_instr(.br, b.cur_block, 0, [left, rhs_val_id, short_val_id])

		b.cur_block = rhs_blk
		right := b.expr(node.rhs)
		b.mod.add_instr(.store, b.cur_block, 0, [right, result_ptr])
		merge_val_id := b.mod.blocks[merge_blk].val_id
		b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val_id])

		b.cur_block = short_blk
		false_val := b.mod.add_value_node(.constant, bool_t, '0', 0)
		b.mod.add_instr(.store, b.cur_block, 0, [false_val, result_ptr])
		b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val_id])

		b.cur_block = merge_blk
		return b.mod.add_instr(.load, b.cur_block, bool_t, [result_ptr])
	}

	if node.op == .logical_or {
		left := b.expr(node.lhs)
		short_blk := b.mod.add_block(b.cur_func, 'or.short')
		rhs_blk := b.mod.add_block(b.cur_func, 'or.rhs')
		merge_blk := b.mod.add_block(b.cur_func, 'or.end')
		result_ptr := b.mod.add_instr(.alloca, b.cur_block, b.mod.type_store.get_ptr(bool_t),
			[])

		short_val_id := b.mod.blocks[short_blk].val_id
		rhs_val_id := b.mod.blocks[rhs_blk].val_id
		b.mod.add_instr(.br, b.cur_block, 0, [left, short_val_id, rhs_val_id])

		b.cur_block = short_blk
		true_val := b.mod.add_value_node(.constant, bool_t, '1', 0)
		b.mod.add_instr(.store, b.cur_block, 0, [true_val, result_ptr])
		merge_val_id := b.mod.blocks[merge_blk].val_id
		b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val_id])

		b.cur_block = rhs_blk
		right := b.expr(node.rhs)
		b.mod.add_instr(.store, b.cur_block, 0, [right, result_ptr])
		b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val_id])

		b.cur_block = merge_blk
		return b.mod.add_instr(.load, b.cur_block, bool_t, [result_ptr])
	}

	// Note: String concatenation (str1 + str2 -> string_plus, s1 + s2 + s3 -> string_plus_two)
	// is fully desugared by the transformer

	mut left := b.expr(node.lhs)
	mut right := b.expr(node.rhs)

	// Get operand types for type propagation
	left_typ := b.mod.values[left].typ
	right_typ := b.mod.values[right].typ

	// String equality must compare contents, not pointer identity.
	if node.op in [.eq, .ne] && b.is_string_like_type_id(left_typ)
		&& b.is_string_like_type_id(right_typ) {
		mut lhs := left
		mut rhs := right
		if string_t := b.get_struct_type_id('string') {
			string_ptr_t := b.mod.type_store.get_ptr(string_t)
			lhs = b.coerce_value_to_type(lhs, string_ptr_t)
			rhs = b.coerce_value_to_type(rhs, string_ptr_t)
		}
		eq_fn := b.mod.add_value_node(.unknown, 0, 'string__eq', 0)
		eq_val := b.mod.add_instr(.call, b.cur_block, bool_t, [eq_fn, lhs, rhs])
		if node.op == .ne {
			one := b.mod.add_value_node(.constant, bool_t, '1', 0)
			return b.mod.add_instr(.xor, b.cur_block, bool_t, [eq_val, one])
		}
		return eq_val
	}

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

fn (mut b Builder) expr_infix_is_check(node ast.InfixExpr, bool_t TypeID) ValueID {
	true_val := b.mod.add_value_node(.constant, bool_t, '1', 0)
	false_val := b.mod.add_value_node(.constant, bool_t, '0', 0)
	default_val := if node.op == .not_is { true_val } else { false_val }

	lhs_val := b.expr(node.lhs)
	if lhs_val <= 0 || lhs_val >= b.mod.values.len {
		return default_val
	}
	lhs_typ := b.mod.values[lhs_val].typ
	if lhs_typ <= 0 || lhs_typ >= b.mod.type_store.types.len {
		return default_val
	}

	mut sum_type := TypeID(0)
	mut sum_ptr := ValueID(0)
	mut sum_val := ValueID(0)

	lhs_info := b.mod.type_store.types[lhs_typ]
	if lhs_info.kind == .ptr_t {
		elem_type := lhs_info.elem_type
		if elem_type > 0 && elem_type < b.mod.type_store.types.len {
			elem_info := b.mod.type_store.types[elem_type]
			if elem_info.kind == .struct_t && b.is_sum_type_id(elem_type) {
				sum_type = elem_type
				sum_ptr = lhs_val
			} else if elem_info.kind == .ptr_t {
				pointee_type := elem_info.elem_type
				if pointee_type > 0 && pointee_type < b.mod.type_store.types.len
					&& b.mod.type_store.types[pointee_type].kind == .struct_t
					&& b.is_sum_type_id(pointee_type) {
					sum_type = pointee_type
					sum_ptr = b.mod.add_instr(.load, b.cur_block, elem_type, [lhs_val])
				}
			}
		}
	} else if lhs_info.kind == .struct_t && b.is_sum_type_id(lhs_typ) {
		sum_type = lhs_typ
		sum_val = lhs_val
	}

	if sum_type == 0 {
		return default_val
	}
	sum_name := b.struct_name_from_type_id(sum_type) or { return default_val }
	target_type := b.ast_type_to_ssa(node.rhs)
	target_tag := b.sumtype_tag_for_value(sum_name, target_type)

	tag_idx := b.field_index_by_name(sum_type, '_tag')
	if tag_idx < 0 {
		return default_val
	}
	i64_t := b.mod.type_store.get_int(64)
	mut tag_val := ValueID(0)
	if sum_ptr != 0 {
		tag_ptr_t := b.mod.type_store.get_ptr(i64_t)
		tag_idx_val := b.mod.add_value_node(.constant, i64_t, tag_idx.str(), 0)
		tag_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, tag_ptr_t, [sum_ptr, tag_idx_val])
		tag_val = b.mod.add_instr(.load, b.cur_block, i64_t, [tag_ptr])
	} else if sum_val != 0 {
		tag_idx_val := b.mod.add_value_node(.constant, b.mod.type_store.get_int(32), tag_idx.str(),
			0)
		tag_val = b.mod.add_instr(.extractvalue, b.cur_block, i64_t, [sum_val, tag_idx_val])
	} else {
		return default_val
	}

	tag_const := b.mod.add_value_node(.constant, i64_t, target_tag.str(), 0)
	op := if node.op == .not_is { OpCode.ne } else { OpCode.eq }
	return b.mod.add_instr(op, b.cur_block, bool_t, [tag_val, tag_const])
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

fn (mut b Builder) known_c_ret_type(name string) ?TypeID {
	i64_t := b.mod.type_store.get_int(64)
	i8_t := b.mod.type_store.get_int(8)
	voidptr_t := b.mod.type_store.get_ptr(i8_t)
	match name {
		'malloc', 'calloc', 'realloc', 'malloc_noscan', 'vcalloc', 'vcalloc_noscan', 'v__malloc',
		'memcpy', 'memmove', 'memset', 'vmemcpy', 'vmemmove', 'vmemset' {
			return voidptr_t
		}
		'strlen', 'strnlen' {
			return i64_t
		}
		else {
			return none
		}
	}
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
	mut result_type := TypeID(0)
	mut result_ptr := ValueID(0)
	if is_expr {
		result_type = b.infer_if_expr_result_type(node)
		if result_type == 0 {
			result_type = b.mod.type_store.get_int(64)
		}
		result_ptr = b.mod.add_instr(.alloca, b.cur_block, b.mod.type_store.get_ptr(result_type),
			[])
	}

	// 4. Emit Branch
	then_val := b.mod.blocks[then_blk].val_id
	else_val := b.mod.blocks[else_blk].val_id
	b.mod.add_instr(.br, b.cur_block, 0, [cond_val, then_val, else_val])

	// 5. Build Then Block
	b.cur_block = then_blk
	then_result := b.stmts_with_value(node.stmts)
	if is_expr && then_result != 0 && !b.is_block_terminated(b.cur_block) {
		store_val := b.coerce_merge_value_to_type(then_result, result_type)
		b.mod.add_instr(.store, b.cur_block, 0, [store_val, result_ptr])
	}
	if !b.is_block_terminated(b.cur_block) {
		merge_val := b.mod.blocks[merge_blk].val_id
		b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val])
	}

	// 6. Build Else Block (if any)
	if has_else {
		b.cur_block = else_blk
		else_result := b.expr_if_else(node.else_expr, is_expr)
		if is_expr && else_result != 0 && !b.is_block_terminated(b.cur_block) {
			store_val := b.coerce_merge_value_to_type(else_result, result_type)
			b.mod.add_instr(.store, b.cur_block, 0, [store_val, result_ptr])
		}
		if !b.is_block_terminated(b.cur_block) {
			merge_val := b.mod.blocks[merge_blk].val_id
			b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val])
		}
	}

	// 7. Continue generation at Merge Block
	b.cur_block = merge_blk

	// 8. Load result for if-expressions
	if is_expr && result_ptr != 0 && result_type != 0 {
		return b.mod.add_instr(.load, b.cur_block, result_type, [result_ptr])
	}
	return 0
}

// expr_if_with_guard handles if statements with guard expressions
// `if x := opt() { ... } else { ... }`
fn (mut b Builder) expr_if_with_guard(node ast.IfExpr, guard ast.IfGuardExpr) ValueID {
	has_else := node.else_expr !is ast.EmptyExpr
	is_expr := has_else && b.branch_has_value(node.stmts)

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
	mut result_type := TypeID(0)
	mut result_ptr := ValueID(0)
	if is_expr {
		result_type = b.infer_if_expr_result_type(node)
		if result_type == 0 {
			result_type = b.mod.type_store.get_int(64)
		}
		result_ptr = b.mod.add_instr(.alloca, b.cur_block, b.mod.type_store.get_ptr(result_type),
			[])
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
		store_val := b.coerce_merge_value_to_type(then_result, result_type)
		b.mod.add_instr(.store, b.cur_block, 0, [store_val, result_ptr])
	}
	if !b.is_block_terminated(b.cur_block) {
		merge_val := b.mod.blocks[merge_blk].val_id
		b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val])
	}

	// 7. Build Else Block (if any)
	if has_else {
		b.cur_block = else_blk
		else_result := b.expr_if_else(node.else_expr, is_expr)
		if is_expr && else_result != 0 && !b.is_block_terminated(b.cur_block) {
			store_val := b.coerce_merge_value_to_type(else_result, result_type)
			b.mod.add_instr(.store, b.cur_block, 0, [store_val, result_ptr])
		}
		if !b.is_block_terminated(b.cur_block) {
			merge_val := b.mod.blocks[merge_blk].val_id
			b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val])
		}
	}

	// 8. Continue generation at Merge Block
	b.cur_block = merge_blk

	// 9. Load result for if-expressions
	if is_expr && result_ptr != 0 && result_type != 0 {
		return b.mod.add_instr(.load, b.cur_block, result_type, [result_ptr])
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

fn (b Builder) branch_last_expr(stmts []ast.Stmt) ?ast.Expr {
	if stmts.len == 0 {
		return none
	}
	last := stmts[stmts.len - 1]
	if last is ast.ExprStmt {
		return last.expr
	}
	return none
}

fn (mut b Builder) expr_type_hint(expr ast.Expr) ?TypeID {
	match expr {
		ast.ParenExpr {
			return b.expr_type_hint(expr.expr)
		}
		ast.Ident {
			if typ := b.lookup_var_type_from_env(expr.name) {
				return b.type_to_ssa(typ)
			}
			if struct_name := b.get_var_struct_type(expr.name) {
				if st := b.get_struct_type_id(struct_name) {
					return st
				}
			}
		}
		ast.SelectorExpr {
			if expr.rhs.name in ['len', 'cap', 'is_lit'] {
				return b.mod.type_store.get_int(64)
			}
			if expr.lhs is ast.Ident {
				if owner_t := b.lookup_var_type_from_env(expr.lhs.name) {
					if field_t := b.struct_field_type_from_types_type(owner_t, expr.rhs.name) {
						return b.type_to_ssa(field_t)
					}
				}
			}
		}
		ast.IfExpr {
			t := b.infer_if_expr_result_type(expr)
			if t != 0 {
				return t
			}
		}
		ast.MatchExpr {
			t := b.infer_match_expr_result_type(expr)
			if t != 0 {
				return t
			}
		}
		ast.StringLiteral, ast.StringInterLiteral {
			if st := b.get_struct_type_id('string') {
				return st
			}
		}
		ast.InitExpr {
			t := b.ast_type_to_ssa(expr.typ)
			if t != 0 {
				return t
			}
		}
		ast.CastExpr {
			t := b.ast_type_to_ssa(expr.typ)
			if t != 0 {
				return t
			}
		}
		else {}
	}
	pos := b.expr_pos(expr)
	if int(pos) > 0 {
		if t := b.lookup_expr_type_from_env(pos) {
			return t
		}
	}
	return none
}

fn (b Builder) expr_pos(expr ast.Expr) token.Pos {
	match expr {
		ast.Ident {
			return expr.pos
		}
		ast.ParenExpr {
			return expr.pos
		}
		ast.IfExpr {
			return expr.pos
		}
		ast.MatchExpr {
			return expr.pos
		}
		ast.InitExpr {
			return expr.pos
		}
		ast.CastExpr {
			return expr.pos
		}
		ast.StringLiteral {
			return expr.pos
		}
		ast.StringInterLiteral {
			return expr.pos
		}
		ast.SelectorExpr {
			return expr.pos
		}
		ast.IndexExpr {
			return expr.pos
		}
		ast.InfixExpr {
			return expr.pos
		}
		ast.PrefixExpr {
			return expr.pos
		}
		ast.PostfixExpr {
			return expr.pos
		}
		ast.CallExpr {
			return expr.pos
		}
		ast.CallOrCastExpr {
			return expr.pos
		}
		ast.BasicLiteral {
			return expr.pos
		}
		ast.ArrayInitExpr {
			return expr.pos
		}
		ast.MapInitExpr {
			return expr.pos
		}
		ast.IfGuardExpr {
			return expr.pos
		}
		ast.RangeExpr {
			return expr.pos
		}
		ast.AsCastExpr {
			return expr.pos
		}
		ast.AssocExpr {
			return expr.pos
		}
		ast.FnLiteral {
			return expr.pos
		}
		ast.Tuple {
			return expr.pos
		}
		ast.ComptimeExpr {
			return expr.pos
		}
		ast.UnsafeExpr {
			return expr.pos
		}
		ast.EmptyExpr {
			return token.Pos(0)
		}
		ast.Type {
			return token.Pos(0)
		}
		ast.Keyword {
			return token.Pos(0)
		}
		ast.LockExpr {
			return expr.pos
		}
		ast.GenericArgs {
			return expr.pos
		}
		ast.GenericArgOrIndexExpr {
			return expr.pos
		}
		ast.LambdaExpr {
			return expr.pos
		}
		ast.KeywordOperator {
			return expr.pos
		}
		ast.ModifierExpr {
			return expr.pos
		}
		ast.OrExpr {
			return expr.pos
		}
		else {
			return token.Pos(0)
		}
	}
}

fn (mut b Builder) infer_if_expr_result_type(node ast.IfExpr) TypeID {
	if then_expr := b.branch_last_expr(node.stmts) {
		if then_t := b.expr_type_hint(then_expr) {
			return then_t
		}
	}
	if node.else_expr !is ast.EmptyExpr {
		if else_t := b.expr_type_hint(node.else_expr) {
			return else_t
		}
		if node.else_expr is ast.IfExpr {
			if else_expr := b.branch_last_expr(node.else_expr.stmts) {
				if else_t := b.expr_type_hint(else_expr) {
					return else_t
				}
			}
		}
	}
	if expr_t := b.lookup_expr_type_from_env(node.pos) {
		return expr_t
	}
	return 0
}

fn (mut b Builder) infer_match_expr_result_type(node ast.MatchExpr) TypeID {
	if expr_t := b.lookup_expr_type_from_env(node.pos) {
		return expr_t
	}
	for branch in node.branches {
		if branch_expr := b.branch_last_expr(branch.stmts) {
			if branch_t := b.expr_type_hint(branch_expr) {
				return branch_t
			}
		}
	}
	return 0
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
fn (mut b Builder) expr_if_else(else_expr ast.Expr, want_value bool) ValueID {
	if else_expr is ast.IfExpr {
		if want_value && else_expr.else_expr is ast.EmptyExpr && b.branch_has_value(else_expr.stmts) {
			zero_expr := ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			})
			wrapped_else := ast.Expr(ast.IfExpr{
				cond:  ast.empty_expr
				stmts: [ast.Stmt(ast.ExprStmt{
					expr: zero_expr
				})]
			})
			return b.expr_if(ast.IfExpr{
				cond:      else_expr.cond
				else_expr: wrapped_else
				stmts:     else_expr.stmts
				pos:       else_expr.pos
			})
		}
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
			enum_keys := b.enum_values.keys()
			for key in enum_keys {
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
	mut is_expr := true
	for branch in node.branches {
		if !b.branch_has_value(branch.stmts) {
			is_expr = false
			break
		}
	}
	i64_t := b.mod.type_store.get_int(64)
	mut result_type := TypeID(0)
	mut result_ptr := ValueID(0)
	if is_expr {
		result_type = b.infer_match_expr_result_type(node)
		if result_type == 0 {
			result_type = i64_t
		}
		result_ptr = b.mod.add_instr(.alloca, b.cur_block, b.mod.type_store.get_ptr(result_type),
			[])
	}

	// We need to collect all cases and branch blocks
	// Format: val -> block
	// Ops: [cond, default_blk, val1, blk1, val2, blk2...]

	mut cases := []ValueID{} // alternating val, blk_id
	mut branch_case_vals := [][]ValueID{}

	// Pre-create blocks for branches to get their IDs
	mut branch_blks := []BlockID{}
	for i, branch in node.branches {
		name := if branch.cond.len == 0 { 'match.else' } else { 'match.case_${i}' }
		blk := b.mod.add_block(b.cur_func, name)
		branch_blks << blk

		mut case_vals := []ValueID{}
		if branch.cond.len == 0 {
			default_blk = blk
		} else {
			for expr in branch.cond {
				val := b.expr(expr)
				case_vals << val
				cases << val
				cases << b.mod.blocks[blk].val_id
			}
		}
		branch_case_vals << case_vals
	}

	// 3. Emit dispatch
	if b.value_is_string_like(cond_val) {
		bool_t := b.mod.type_store.get_int(8)
		mut cond_str := cond_val
		mut string_ptr_t := TypeID(0)
		if string_t := b.get_struct_type_id('string') {
			string_ptr_t = b.mod.type_store.get_ptr(string_t)
			cond_str = b.coerce_value_to_type(cond_str, string_ptr_t)
		}
		mut case_indices := []int{}
		for i, branch in node.branches {
			if branch.cond.len > 0 {
				case_indices << i
			}
		}
		if case_indices.len == 0 {
			default_val := b.mod.blocks[default_blk].val_id
			b.mod.add_instr(.jmp, b.cur_block, 0, [default_val])
		} else {
			for case_i, branch_i in case_indices {
				mut branch_cond := ValueID(0)
				for j, raw_case in branch_case_vals[branch_i] {
					mut case_val := raw_case
					if string_ptr_t != 0 {
						case_val = b.coerce_value_to_type(case_val, string_ptr_t)
					}
					eq_fn := b.mod.add_value_node(.unknown, 0, 'string__eq', 0)
					cmp := b.mod.add_instr(.call, b.cur_block, bool_t, [eq_fn, cond_str, case_val])
					if j == 0 {
						branch_cond = cmp
					} else {
						branch_cond = b.mod.add_instr(.or_, b.cur_block, bool_t, [
							branch_cond,
							cmp,
						])
					}
				}
				if branch_cond == 0 {
					branch_cond = b.mod.add_value_node(.constant, bool_t, '0', 0)
				}
				true_val := b.mod.blocks[branch_blks[branch_i]].val_id
				if case_i < case_indices.len - 1 {
					next_blk := b.mod.add_block(b.cur_func, 'match.check_${case_i + 1}')
					next_val := b.mod.blocks[next_blk].val_id
					b.mod.add_instr(.br, b.cur_block, 0, [branch_cond, true_val, next_val])
					b.cur_block = next_blk
				} else {
					default_val := b.mod.blocks[default_blk].val_id
					b.mod.add_instr(.br, b.cur_block, 0, [branch_cond, true_val, default_val])
				}
			}
		}
	} else {
		mut ops := []ValueID{}
		ops << cond_val
		ops << b.mod.blocks[default_blk].val_id
		ops << cases
		b.mod.add_instr(.switch_, b.cur_block, 0, ops)
	}

	// 4. Build Branches
	for i, branch in node.branches {
		blk := branch_blks[i]
		b.cur_block = blk
		if is_expr {
			branch_result := b.stmts_with_value(branch.stmts)
			if branch_result != 0 && !b.is_block_terminated(b.cur_block) {
				coerced_result := b.coerce_merge_value_to_type(branch_result, result_type)
				b.mod.add_instr(.store, b.cur_block, 0, [coerced_result, result_ptr])
			}
		} else {
			b.stmts(branch.stmts)
		}

		if !b.is_block_terminated(b.cur_block) {
			merge_val := b.mod.blocks[merge_blk].val_id
			b.mod.add_instr(.jmp, b.cur_block, 0, [merge_val])
		}
	}

	b.cur_block = merge_blk
	b.cur_match_type = old_match_type
	if is_expr && result_ptr != 0 && result_type != 0 {
		return b.mod.add_instr(.load, b.cur_block, result_type, [result_ptr])
	}
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

	lhs := node.lhs
	if lhs is ast.Ident {
		name = lhs.name
		// Strip 'builtin__' prefix from transformer-generated calls
		// since builtin functions are registered without module prefix
		if name.starts_with('builtin__') {
			name = name[9..]
			// Remap transformer's single-underscore method naming to SSA double-underscore convention
			// e.g., 'array_push_noscan' -> 'array__push_noscan'
			for type_prefix in ['array', 'string', 'map'] {
				if name.starts_with(type_prefix + '_') && !name.starts_with(type_prefix + '__') {
					name = type_prefix + '__' + name[type_prefix.len + 1..]
					break
				}
			}
		}
		if node.args.len == 1 {
			cast_name := name
			if cast_name in ['int', 'i64', 'i32', 'i16', 'i8', 'u64', 'u32', 'u16', 'u8', 'f32',
				'f64', 'voidptr', 'charptr', 'byteptr', 'usize', 'isize', 'rune', 'bool', 'char',
				'byte'] {
				return b.expr(node.args[0])
			}
			if map_has_key_bool(b.interface_names, cast_name) {
				return b.expr_interface_box(cast_name, node.args[0])
			}
			if map_has_key_string(b.type_alias_bases, cast_name)
				|| b.get_struct_type_id(cast_name) != none
				|| map_has_key_bool(b.enum_names, cast_name) {
				return b.expr(node.args[0])
			}
			if b.find_function(cast_name) < 0 && !map_has_key_type_id(b.func_ret_types, cast_name) {
				if node.args[0] is ast.UnsafeExpr || node.args[0] is ast.PrefixExpr
					|| node.args[0] is ast.ParenExpr {
					return b.expr(node.args[0])
				}
			}
		}
		// Resolve intra-module calls: if name has no module prefix
		// and we're in a non-main/non-builtin module, try with module prefix
		if !name.contains('__') && b.cur_module != 'main' && b.cur_module != 'builtin' {
			prefixed := b.cur_module + '__' + name
			if map_has_key_type_id(b.func_ret_types, prefixed) {
				name = prefixed
			}
		}
		// Remap transformer-generated specialized map functions to generic map methods
		// e.g., '__Map_string_int_set' -> 'map__set', '__Map_string_int_get_check' -> 'map__get_check'
		if name.starts_with('__Map_') || name.starts_with('Map_') {
			if name.ends_with('_get_and_set') {
				name = 'map__get_and_set'
			} else if name.ends_with('_set') {
				name = 'map__set'
			} else if name.ends_with('_get_check') {
				name = 'map__get_check'
			} else if name.ends_with('_str') {
				name = 'map__str'
			} else if name.ends_with('_get') {
				name = 'map__get'
			}
		}
		// Fix transformer's single-underscore Type_method naming to SSA double-underscore convention
		// e.g., 'int_str' -> 'int__str', 'sync__RwMutex_rlock' -> 'sync__RwMutex__rlock'
		if !map_has_key_type_id(b.func_ret_types, name) && name.contains('_') {
			// Try each _ as __ to find a matching function
			for i := name.len - 1; i >= 0; i-- {
				if name[i] == `_` && (i == 0 || name[i - 1] != `_`)
					&& (i == name.len - 1 || name[i + 1] != `_`) {
					candidate := '${name[..i]}__${name[i + 1..]}'
					if map_has_key_type_id(b.func_ret_types, candidate) {
						name = candidate
						break
					}
				}
			}
		}
		name = b.remap_missing_runtime_fn(name)
	} else if lhs is ast.SelectorExpr {
		method_name := lhs.rhs.name
		// Handle flag-style mutators on integer fields directly.
		// Example: `res.flags.set(.noslices)` should lower to bitwise ops,
		// not to unrelated method inference (which can mis-resolve to map__set).
		if method_name in ['set', 'clear', 'toggle'] && node.args.len == 1 {
			recv_ptr := b.addr(ast.Expr(lhs.lhs))
			if recv_ptr != 0 {
				recv_ptr_typ_id := b.mod.values[recv_ptr].typ
				if recv_ptr_typ_id > 0 && recv_ptr_typ_id < b.mod.type_store.types.len {
					recv_ptr_typ := b.mod.type_store.types[recv_ptr_typ_id]
					if recv_ptr_typ.kind == .ptr_t && recv_ptr_typ.elem_type > 0
						&& recv_ptr_typ.elem_type < b.mod.type_store.types.len {
						recv_typ_id := recv_ptr_typ.elem_type
						recv_typ := b.mod.type_store.types[recv_typ_id]
						if recv_typ.kind == .int_t {
							recv_cur := b.mod.add_instr(.load, b.cur_block, recv_typ_id,
								[recv_ptr])
							arg_val := b.expr(node.args[0])
							mut new_val := ValueID(0)
							match method_name {
								'set' {
									new_val = b.mod.add_instr(.or_, b.cur_block, recv_typ_id,
										[recv_cur, arg_val])
								}
								'toggle' {
									new_val = b.mod.add_instr(.xor, b.cur_block, recv_typ_id,
										[recv_cur, arg_val])
								}
								else { // clear
									minus_one := b.mod.add_value_node(.constant, recv_typ_id,
										'-1', 0)
									inv := b.mod.add_instr(.xor, b.cur_block, recv_typ_id,
										[arg_val, minus_one])
									new_val = b.mod.add_instr(.and_, b.cur_block, recv_typ_id,
										[recv_cur, inv])
								}
							}
							b.mod.add_instr(.store, b.cur_block, 0, [new_val, recv_ptr])
							return new_val
						}
					}
				}
			}
		}
		// Resolve static calls like `token.FileSet.new()` before generic method inference.
		if lhs.lhs is ast.SelectorExpr {
			if resolved := b.resolve_module_type_method_call_name(lhs.lhs, method_name) {
				name = resolved
			}
		}
		// Check if this is a method call (receiver.method()) or C.func() or module.func()
		if name == '' && lhs.lhs is ast.Ident {
			receiver_name := lhs.lhs.name
			// Check if receiver is C interop or a module qualifier.
			if b.is_module_call_receiver(receiver_name) {
				// Module-qualified function call
				if receiver_name == 'C' || receiver_name == 'builtin' {
					// C interop and builtin functions don't get module prefix
					name = method_name
				} else {
					name = receiver_name + '__' + method_name
				}
			} else if struct_type_name := b.get_var_struct_type(receiver_name) {
				short_type_name := b.unqualified_type_name(struct_type_name)
				// Check if this is a function pointer field call (e.g., m.key_eq_fn(args))
				fn_ptr_key := '${short_type_name}.${method_name}'
				struct_typ_id := b.get_struct_type_id(struct_type_name) or { TypeID(0) }
				mut field_idx := -1
				mut field_type := TypeID(0)
				if struct_typ_id > 0 && struct_typ_id < b.mod.type_store.types.len {
					struct_info := b.mod.type_store.types[struct_typ_id]
					for i, fname in struct_info.field_names {
						if fname == method_name {
							field_idx = i
							break
						}
					}
					if field_idx >= 0 && field_idx < struct_info.fields.len {
						field_type = struct_info.fields[field_idx]
					}
				}
				mut is_fn_ptr_field := map_has_key_bool(b.fn_ptr_fields, fn_ptr_key)
				if !is_fn_ptr_field && field_idx >= 0 && field_type > 0
					&& field_type < b.mod.type_store.types.len {
					field_info := b.mod.type_store.types[field_type]
					if field_info.kind == .ptr_t && method_name.ends_with('_fn') {
						is_fn_ptr_field = true
					}
				}
				if is_fn_ptr_field && field_idx >= 0 && field_type > 0 {
					// Function pointer field call - load the function pointer and do indirect call.
					var_ptr := b.addr(ast.Expr(lhs.lhs))
					ptr_typ := b.mod.values[var_ptr].typ
					elem_typ := b.mod.type_store.types[ptr_typ].elem_type
					struct_ptr := b.mod.add_instr(.load, b.cur_block, elem_typ, [
						var_ptr,
					])
					idx_val := b.mod.add_value_node(.constant, i32_t, '${field_idx}',
						field_idx)
					field_ptr_t := b.mod.type_store.get_ptr(field_type)
					field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_t,
						[
						struct_ptr,
						idx_val,
					])
					fn_ptr := b.mod.add_instr(.load, b.cur_block, field_type, [
						field_ptr,
					])
					mut fn_args := []ValueID{}
					for arg in node.args {
						fn_args << b.expr(arg)
					}
					fn_args.prepend(fn_ptr)
					ret_type := b.fn_ptr_ret_type_from_field_name(method_name)
					return b.mod.add_instr(.call_indirect, b.cur_block, ret_type, fn_args)
				} else if map_has_key_bool(b.interface_names, struct_type_name)
					|| map_has_key_bool(b.interface_names, short_type_name) {
					// Prefer vtable-based dispatch for interface calls.
					// This preserves the correct receiver object pointer (`_object`)
					// instead of passing the interface wrapper itself.
					iface_name := if map_has_key_bool(b.interface_names, struct_type_name) {
						struct_type_name
					} else {
						short_type_name
					}
					if call_id := b.emit_interface_method_call(ast.Expr(lhs.lhs), iface_name,
						method_name, node.args)
					{
						return call_id
					}
					// Fallback: if vtable info is unavailable, try concrete resolution.
					if concrete_type := b.get_iface_concrete_type(receiver_name) {
						concrete_name := b.resolve_method_fn_name(concrete_type, method_name)
						if concrete_name != '' {
							name = concrete_name
							is_method_call = true

							// The interface value is just a pointer to the boxed object.
							var_ptr := b.addr(ast.Expr(lhs.lhs))
							ptr_typ := b.mod.values[var_ptr].typ
							elem_typ := b.mod.type_store.types[ptr_typ].elem_type
							receiver_val = b.mod.add_instr(.load, b.cur_block, elem_typ,
								[
								var_ptr,
							])
						}
					}
					if name == '' {
						// Keep a best-effort symbol to avoid empty unresolved names.
						name = method_name
					}
				} else {
					// Regular method call - mangle the name
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
					// Try module-prefixed name first (e.g., strings__Builder__write_string)
					// then fall back to alias resolution (e.g., array__push_many)
					receiver_module := b.module_for_type_name(struct_type_name)
					mut mangled_type := struct_type_name
					if !mangled_type.contains('__') && receiver_module != ''
						&& receiver_module != 'main' && receiver_module != 'builtin' {
						candidate := receiver_module + '__' + struct_type_name
						candidate_fn := '${candidate}__${mangled_method}'
						if map_has_key_type_id(b.func_ret_types, candidate_fn) {
							mangled_type = candidate
						} else if resolved := b.find_module_prefixed_receiver_type(short_type_name,
							mangled_method)
						{
							mangled_type = resolved
						} else if base_type := b.get_type_alias_base(short_type_name) {
							// Method not found on module type, try alias base (e.g., array)
							mangled_type = base_type
						} else {
							mangled_type = candidate
						}
					} else if resolved := b.find_module_prefixed_receiver_type(short_type_name,
						mangled_method)
					{
						mangled_type = resolved
					} else if base_type := b.get_type_alias_base(short_type_name) {
						mangled_type = base_type
					}
					name = '${mangled_type}__${mangled_method}'
					// If method not found, try lowercase type name (e.g., Array -> array)
					if !map_has_key_type_id(b.func_ret_types, name) {
						lower_name := '${mangled_type.to_lower()}__${mangled_method}'
						if map_has_key_type_id(b.func_ret_types, lower_name) {
							name = lower_name
							mangled_type = mangled_type.to_lower()
						}
					}
					is_method_call = true
					// Check if this is a flag enum method
					if map_has_key_bool(b.flag_enum_names, mangled_type)
						&& mangled_method in ['has', 'all'] {
						flag_enum_receiver_type = mangled_type
					}
					// Get the receiver - need to load the struct pointer from the variable
					// b.vars stores Ptr(Ptr(struct)), we need Ptr(struct)
					var_ptr := b.addr(ast.Expr(lhs.lhs))
					ptr_typ := b.mod.values[var_ptr].typ
					elem_typ := b.mod.type_store.types[ptr_typ].elem_type
					receiver_val = b.mod.add_instr(.load, b.cur_block, elem_typ, [
						var_ptr,
					])
				}
			} else {
				// Unknown receiver type - try to infer from the current variable type first.
				// Mangle operator names to valid symbol names first.
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
				if var_ptr := b.get_var_ptr(receiver_name) {
					if var_ptr > 0 && var_ptr < b.mod.values.len {
						var_ptr_typ_id := b.mod.values[var_ptr].typ
						if var_ptr_typ_id > 0 && var_ptr_typ_id < b.mod.type_store.types.len {
							var_ptr_typ := b.mod.type_store.types[var_ptr_typ_id]
							if var_ptr_typ.kind == .ptr_t && var_ptr_typ.elem_type > 0
								&& var_ptr_typ.elem_type < b.mod.type_store.types.len {
								recv_typ_id := var_ptr_typ.elem_type
								recv_typ := b.mod.type_store.types[recv_typ_id]
								if recv_typ.kind == .struct_t {
									if recv_name := b.struct_name_from_type_id(recv_typ_id) {
										candidate := b.resolve_receiver_method_name(recv_name,
											mangled_method_name)
										if candidate != '' {
											name = candidate
											is_method_call = true
											receiver_val = b.mod.add_instr(.load, b.cur_block,
												recv_typ_id, [var_ptr])
											if map_has_key_bool(b.flag_enum_names, recv_name)
												&& mangled_method_name in ['has', 'all'] {
												flag_enum_receiver_type = recv_name
											}
										}
									}
								}
							}
						}
					}
				}
				if !is_method_call {
					inferred_type := b.infer_receiver_type(mangled_method_name)
					if inferred_type != '' {
						name = '${inferred_type}__${mangled_method_name}'
						is_method_call = true
						receiver_val = b.expr(ast.Expr(lhs.lhs))
						// Check if this is a flag enum method
						if map_has_key_bool(b.flag_enum_names, inferred_type)
							&& mangled_method_name in ['has', 'all'] {
							flag_enum_receiver_type = inferred_type
						}
					} else {
						name = mangled_method_name
					}
				}
			}
		} else if name == '' {
			// Complex expression as receiver - infer from receiver value type first.
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
			receiver_val = b.expr(lhs.lhs)
			mut resolved := false
			if receiver_val > 0 && receiver_val < b.mod.values.len {
				mut recv_typ_id := b.mod.values[receiver_val].typ
				if recv_typ_id > 0 && recv_typ_id < b.mod.type_store.types.len {
					recv_typ := b.mod.type_store.types[recv_typ_id]
					if recv_typ.kind == .ptr_t && recv_typ.elem_type > 0
						&& recv_typ.elem_type < b.mod.type_store.types.len
						&& b.mod.type_store.types[recv_typ.elem_type].kind == .struct_t {
						recv_typ_id = recv_typ.elem_type
					}
					if recv_typ_id > 0 && recv_typ_id < b.mod.type_store.types.len
						&& b.mod.type_store.types[recv_typ_id].kind == .struct_t {
						if recv_name := b.struct_name_from_type_id(recv_typ_id) {
							candidate := b.resolve_receiver_method_name(recv_name, mangled_mname)
							if candidate != '' {
								name = candidate
								resolved = true
								if map_has_key_bool(b.flag_enum_names, recv_name)
									&& mangled_mname in ['has', 'all'] {
									flag_enum_receiver_type = recv_name
								}
							}
						}
					}
				}
			}
			if !resolved {
				inferred_type := b.infer_receiver_type(mangled_mname)
				if inferred_type != '' {
					name = '${inferred_type}__${mangled_mname}'
					// Check if this is a flag enum method
					if map_has_key_bool(b.flag_enum_names, inferred_type)
						&& mangled_mname in ['has', 'all'] {
						flag_enum_receiver_type = inferred_type
					}
				} else {
					name = mangled_mname
				}
			}
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
	if !is_method_call {
		b.adjust_map_ptr_args_from_exprs(name, node.args, mut args)
	}

	// For method calls, prepend receiver as first argument
	if is_method_call && receiver_val != 0 {
		if lhs is ast.SelectorExpr {
			receiver_val = b.maybe_use_addressable_receiver(name, lhs.lhs, receiver_val)
		}
		args.prepend(receiver_val)
	}
	b.adjust_map_runtime_call_args(name, mut args)
	b.adjust_call_args_for_fn(name, mut args)

	// Remap common transformed helper names to canonical builtin symbols.
	if !map_has_key_type_id(b.func_ret_types, name) {
		if name == 'new_array_from_c_array_noscan'
			&& map_has_key_type_id(b.func_ret_types, 'new_array_from_c_array') {
			name = 'new_array_from_c_array'
		} else if name == 'bytestr' && map_has_key_type_id(b.func_ret_types, 'array__bytestr') {
			name = 'array__bytestr'
		} else if name.starts_with('Array_') && name.ends_with('_contains')
			&& map_has_key_type_id(b.func_ret_types, 'array__contains') {
			name = 'array__contains'
		}
	}

	// Generate stubs for type-specialized functions that don't have V implementations
	if name.starts_with('array__contains_') && !map_has_key_type_id(b.func_ret_types, name) {
		b.generate_array_contains_stub(name)
	}
	if name == 'array__eq' && !map_has_key_type_id(b.func_ret_types, name) {
		b.generate_array_eq_stub()
	}
	// Generate no-op stubs for sync RwMutex methods (single-threaded in native backend)
	if !map_has_key_type_id(b.func_ret_types, name) && name.contains('RwMutex') {
		b.generate_noop_stub(name)
	}
	// Remap double-underscore method names to single-underscore for transformer-generated functions
	// e.g., Array_int__str -> Array_int_str (transformer uses single underscore for these)
	if !map_has_key_type_id(b.func_ret_types, name) && name.ends_with('__str') {
		candidate := name[..name.len - 5] + '_str'
		if map_has_key_type_id(b.func_ret_types, candidate) {
			name = candidate
		}
	}
	// Generate str() stubs for types that don't have an implementation
	if !map_has_key_type_id(b.func_ret_types, name) && name.ends_with('__str') {
		b.generate_str_stub(name)
	}
	name = b.remap_missing_runtime_fn(name)

	// Create a Value representing the function symbol (operand 0)
	fn_val := b.mod.add_value_node(.unknown, 0, name, 0)
	args.prepend(fn_val)

	// Look up the return type from tracked functions
	mut ret_type := i32_t
	if rt := b.get_func_ret_type(name) {
		ret_type = rt
	} else if crt := b.known_c_ret_type(name) {
		ret_type = crt
	}

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
	if node.lhs is ast.SelectorExpr {
		if node.lhs.lhs is ast.Ident && node.lhs.lhs.name == 'C' {
			candidate := node.lhs.rhs.name
			if b.find_function(candidate) < 0 && !map_has_key_type_id(b.func_ret_types, candidate) {
				val := b.expr(node.expr)
				target_typ := b.ast_type_to_ssa(ast.Expr(node.lhs))
				return b.coerce_value_to_type(val, target_typ)
			}
		}
	}

	// Check if this is a primitive type cast (int, i64, voidptr, etc.)
	// These are not function calls - just return the expression value
	if node.lhs is ast.Ident {
		cast_name := node.lhs.name
		// Generic casts like `T(expr)` should lower as plain value coercions.
		if cast_name.len == 1 && cast_name[0] >= `A` && cast_name[0] <= `Z` {
			return b.expr(node.expr)
		}
		if cast_name in ['int', 'i64', 'i32', 'i16', 'i8', 'u64', 'u32', 'u16', 'u8', 'f32', 'f64',
			'voidptr', 'charptr', 'byteptr', 'usize', 'isize', 'rune', 'bool', 'char', 'byte'] {
			// Type cast: just evaluate the expression (enums are already ints in SSA)
			return b.expr(node.expr)
		}
		if cast_name.ends_with('*') {
			val := b.expr(node.expr)
			target_typ := b.ast_type_to_ssa(ast.Expr(ast.Ident{
				name: cast_name
				pos:  node.lhs.pos
			}))
			return b.coerce_value_to_type(val, target_typ)
		}
		// Check if this is interface boxing: Drawable(point)
		if map_has_key_bool(b.interface_names, cast_name) {
			return b.expr_interface_box(cast_name, node.expr)
		}
		// Sum-type cast: TypeVariant(value) packs tag + data pointer.
		if sum_t := b.get_struct_type_id(cast_name) {
			if b.is_sum_type_id(sum_t) {
				val := b.expr(node.expr)
				return b.wrap_sumtype_value(cast_name, sum_t, val)
			}
		}
		// Check if this is a type alias cast: Builder([]u8{...})
		if map_has_key_string(b.type_alias_bases, cast_name)
			|| b.get_struct_type_id(cast_name) != none {
			// Type cast to a struct/alias type - just evaluate the expression
			return b.expr(node.expr)
		}
		// Check if this is an enum cast: StrIntpType(1)
		if map_has_key_bool(b.enum_names, cast_name) {
			// Enum cast - just evaluate the expression (enums are ints)
			return b.expr(node.expr)
		}
		// Fallback for unresolved private/runtime type casts like mapnode(...)
		// that can survive transformation without explicit type registration.
		if b.find_function(cast_name) < 0 && !map_has_key_type_id(b.func_ret_types, cast_name) {
			if node.expr is ast.UnsafeExpr || node.expr is ast.PrefixExpr
				|| node.expr is ast.ParenExpr {
				return b.expr(node.expr)
			}
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
		// Strip 'builtin__' prefix from transformer-generated calls
		if name.starts_with('builtin__') {
			name = name[9..]
			// Remap transformer's single-underscore method naming to SSA double-underscore convention
			for type_prefix in ['array', 'string', 'map'] {
				if name.starts_with(type_prefix + '_') && !name.starts_with(type_prefix + '__') {
					name = type_prefix + '__' + name[type_prefix.len + 1..]
					break
				}
			}
		}
		// Resolve intra-module calls
		if !name.contains('__') && b.cur_module != 'main' && b.cur_module != 'builtin' {
			prefixed := b.cur_module + '__' + name
			if map_has_key_type_id(b.func_ret_types, prefixed) {
				name = prefixed
			}
		}
		// Remap transformer-generated specialized map functions to generic map methods
		if name.starts_with('__Map_') || name.starts_with('Map_') {
			if name.ends_with('_get_and_set') {
				name = 'map__get_and_set'
			} else if name.ends_with('_set') {
				name = 'map__set'
			} else if name.ends_with('_get_check') {
				name = 'map__get_check'
			} else if name.ends_with('_str') {
				name = 'map__str'
			} else if name.ends_with('_get') {
				name = 'map__get'
			}
		}
		// Fix transformer's single-underscore Type_method naming to SSA double-underscore convention
		if !map_has_key_type_id(b.func_ret_types, name) && name.contains('_') {
			for i := name.len - 1; i >= 0; i-- {
				if name[i] == `_` && (i == 0 || name[i - 1] != `_`)
					&& (i == name.len - 1 || name[i + 1] != `_`) {
					candidate := '${name[..i]}__${name[i + 1..]}'
					if map_has_key_type_id(b.func_ret_types, candidate) {
						name = candidate
						break
					}
				}
			}
		}
		name = b.remap_missing_runtime_fn(name)
	} else if node.lhs is ast.SelectorExpr {
		method_name := node.lhs.rhs.name
		// Handle flag-style mutators on integer fields directly.
		if method_name in ['set', 'clear', 'toggle'] {
			recv_ptr := b.addr(ast.Expr(node.lhs.lhs))
			if recv_ptr != 0 {
				recv_ptr_typ_id := b.mod.values[recv_ptr].typ
				if recv_ptr_typ_id > 0 && recv_ptr_typ_id < b.mod.type_store.types.len {
					recv_ptr_typ := b.mod.type_store.types[recv_ptr_typ_id]
					if recv_ptr_typ.kind == .ptr_t && recv_ptr_typ.elem_type > 0
						&& recv_ptr_typ.elem_type < b.mod.type_store.types.len {
						recv_typ_id := recv_ptr_typ.elem_type
						recv_typ := b.mod.type_store.types[recv_typ_id]
						if recv_typ.kind == .int_t {
							recv_cur := b.mod.add_instr(.load, b.cur_block, recv_typ_id,
								[recv_ptr])
							arg_val := b.expr(node.expr)
							mut new_val := ValueID(0)
							match method_name {
								'set' {
									new_val = b.mod.add_instr(.or_, b.cur_block, recv_typ_id,
										[recv_cur, arg_val])
								}
								'toggle' {
									new_val = b.mod.add_instr(.xor, b.cur_block, recv_typ_id,
										[recv_cur, arg_val])
								}
								else { // clear
									minus_one := b.mod.add_value_node(.constant, recv_typ_id,
										'-1', 0)
									inv := b.mod.add_instr(.xor, b.cur_block, recv_typ_id,
										[arg_val, minus_one])
									new_val = b.mod.add_instr(.and_, b.cur_block, recv_typ_id,
										[recv_cur, inv])
								}
							}
							b.mod.add_instr(.store, b.cur_block, 0, [new_val, recv_ptr])
							return new_val
						}
					}
				}
			}
		}
		// Resolve static calls like `token.FileSet.new()` before generic method inference.
		if node.lhs.lhs is ast.SelectorExpr {
			if resolved := b.resolve_module_type_method_call_name(node.lhs.lhs, method_name) {
				name = resolved
			}
		}
		// Check if this is a method call (receiver.method()) or C.func()
		if name == '' && node.lhs.lhs is ast.Ident {
			receiver_name := node.lhs.lhs.name
			if b.is_module_call_receiver(receiver_name) {
				if receiver_name == 'C' || receiver_name == 'builtin' {
					name = method_name
				} else {
					name = receiver_name + '__' + method_name
				}
			} else if struct_type_name := b.get_var_struct_type(receiver_name) {
				short_type_name := b.unqualified_type_name(struct_type_name)
				i32_t := b.mod.type_store.get_int(64)
				// Check if this is a function pointer field call (e.g., m.hash_fn(args))
				fn_ptr_key := '${short_type_name}.${method_name}'
				struct_typ_id := b.get_struct_type_id(struct_type_name) or { TypeID(0) }
				mut field_idx := -1
				mut field_type := TypeID(0)
				if struct_typ_id > 0 && struct_typ_id < b.mod.type_store.types.len {
					struct_info := b.mod.type_store.types[struct_typ_id]
					for i, fname in struct_info.field_names {
						if fname == method_name {
							field_idx = i
							break
						}
					}
					if field_idx >= 0 && field_idx < struct_info.fields.len {
						field_type = struct_info.fields[field_idx]
					}
				}
				mut is_fn_ptr_field := map_has_key_bool(b.fn_ptr_fields, fn_ptr_key)
				if !is_fn_ptr_field && field_idx >= 0 && field_type > 0
					&& field_type < b.mod.type_store.types.len {
					field_info := b.mod.type_store.types[field_type]
					if field_info.kind == .ptr_t && method_name.ends_with('_fn') {
						is_fn_ptr_field = true
					}
				}
				if is_fn_ptr_field && field_idx >= 0 && field_type > 0 {
					// Function pointer field call - load the function pointer and do indirect call.
					var_ptr := b.addr(ast.Expr(node.lhs.lhs))
					ptr_typ := b.mod.values[var_ptr].typ
					elem_typ := b.mod.type_store.types[ptr_typ].elem_type
					struct_ptr := b.mod.add_instr(.load, b.cur_block, elem_typ, [
						var_ptr,
					])
					idx_val := b.mod.add_value_node(.constant, i32_t, '${field_idx}',
						field_idx)
					field_ptr_t := b.mod.type_store.get_ptr(field_type)
					field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_t,
						[
						struct_ptr,
						idx_val,
					])
					fn_ptr := b.mod.add_instr(.load, b.cur_block, field_type, [
						field_ptr,
					])
					mut fn_args := []ValueID{}
					fn_args << b.expr(node.expr)
					fn_args.prepend(fn_ptr)
					ret_type := b.fn_ptr_ret_type_from_field_name(method_name)
					return b.mod.add_instr(.call_indirect, b.cur_block, ret_type, fn_args)
				}
				if map_has_key_bool(b.interface_names, struct_type_name)
					|| map_has_key_bool(b.interface_names, short_type_name) {
					// Prefer vtable-based dispatch for interface calls.
					iface_name := if map_has_key_bool(b.interface_names, struct_type_name) {
						struct_type_name
					} else {
						short_type_name
					}
					if call_id := b.emit_interface_method_call(ast.Expr(node.lhs.lhs),
						iface_name, method_name, [node.expr])
					{
						return call_id
					}
					// Fallback: if vtable info is unavailable, try concrete resolution.
					if concrete_type := b.get_iface_concrete_type(receiver_name) {
						concrete_name := b.resolve_method_fn_name(concrete_type, method_name)
						if concrete_name != '' {
							name = concrete_name
							is_method_call = true
							var_ptr := b.addr(ast.Expr(node.lhs.lhs))
							ptr_typ := b.mod.values[var_ptr].typ
							elem_typ := b.mod.type_store.types[ptr_typ].elem_type
							receiver_val = b.mod.add_instr(.load, b.cur_block, elem_typ,
								[var_ptr])
						}
					}
					if name == '' {
						name = method_name
					}
				} else {
					// Regular method call - mangle the name
					// Try module-prefixed name first, then fall back to alias resolution
					receiver_module2 := b.module_for_type_name(struct_type_name)
					mut mangled_type := struct_type_name
					if !mangled_type.contains('__') && receiver_module2 != ''
						&& receiver_module2 != 'main' && receiver_module2 != 'builtin' {
						candidate := receiver_module2 + '__' + struct_type_name
						candidate_fn := '${candidate}__${method_name}'
						if map_has_key_type_id(b.func_ret_types, candidate_fn) {
							mangled_type = candidate
						} else if resolved := b.find_module_prefixed_receiver_type(short_type_name,
							method_name)
						{
							mangled_type = resolved
						} else if base_type := b.get_type_alias_base(short_type_name) {
							mangled_type = base_type
						} else {
							mangled_type = candidate
						}
					} else if resolved := b.find_module_prefixed_receiver_type(short_type_name,
						method_name)
					{
						mangled_type = resolved
					} else if base_type := b.get_type_alias_base(short_type_name) {
						mangled_type = base_type
					}
					name = '${mangled_type}__${method_name}'
					is_method_call = true
					// Check if this is a flag enum method
					if map_has_key_bool(b.flag_enum_names, mangled_type)
						&& method_name in ['has', 'all'] {
						flag_enum_receiver_type = mangled_type
					}
					// Get the receiver - need to load the struct pointer from the variable
					var_ptr := b.addr(ast.Expr(node.lhs.lhs))
					ptr_typ := b.mod.values[var_ptr].typ
					elem_typ := b.mod.type_store.types[ptr_typ].elem_type
					receiver_val = b.mod.add_instr(.load, b.cur_block, elem_typ, [
						var_ptr,
					])
				}
			} else {
				// Unknown receiver type - try to infer from current variable type first.
				// Mangle operator names to valid symbol names first.
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
				if var_ptr := b.get_var_ptr(receiver_name) {
					if var_ptr > 0 && var_ptr < b.mod.values.len {
						var_ptr_typ_id := b.mod.values[var_ptr].typ
						if var_ptr_typ_id > 0 && var_ptr_typ_id < b.mod.type_store.types.len {
							var_ptr_typ := b.mod.type_store.types[var_ptr_typ_id]
							if var_ptr_typ.kind == .ptr_t && var_ptr_typ.elem_type > 0
								&& var_ptr_typ.elem_type < b.mod.type_store.types.len {
								recv_typ_id := var_ptr_typ.elem_type
								recv_typ := b.mod.type_store.types[recv_typ_id]
								if recv_typ.kind == .struct_t {
									if recv_name := b.struct_name_from_type_id(recv_typ_id) {
										candidate := b.resolve_receiver_method_name(recv_name,
											mangled_method)
										if candidate != '' {
											name = candidate
											is_method_call = true
											receiver_val = b.mod.add_instr(.load, b.cur_block,
												recv_typ_id, [var_ptr])
											if map_has_key_bool(b.flag_enum_names, recv_name)
												&& mangled_method in ['has', 'all'] {
												flag_enum_receiver_type = recv_name
											}
										}
									}
								}
							}
						}
					}
				}
				if !is_method_call {
					inferred_type := b.infer_receiver_type(mangled_method)
					if inferred_type != '' {
						name = '${inferred_type}__${mangled_method}'
						is_method_call = true
						receiver_val = b.expr(ast.Expr(node.lhs.lhs))
						// Check if this is a flag enum method - SET cur_match_type
						if map_has_key_bool(b.flag_enum_names, inferred_type)
							&& mangled_method in ['has', 'all'] {
							flag_enum_receiver_type = inferred_type
						}
					} else {
						name = mangled_method
					}
				}
			}
		} else if name == '' {
			// Complex expression as receiver - infer from receiver value type first.
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
			receiver_val = b.expr(node.lhs.lhs)
			mut resolved := false
			if receiver_val > 0 && receiver_val < b.mod.values.len {
				mut recv_typ_id := b.mod.values[receiver_val].typ
				if recv_typ_id > 0 && recv_typ_id < b.mod.type_store.types.len {
					recv_typ := b.mod.type_store.types[recv_typ_id]
					if recv_typ.kind == .ptr_t && recv_typ.elem_type > 0
						&& recv_typ.elem_type < b.mod.type_store.types.len
						&& b.mod.type_store.types[recv_typ.elem_type].kind == .struct_t {
						recv_typ_id = recv_typ.elem_type
					}
					if recv_typ_id > 0 && recv_typ_id < b.mod.type_store.types.len
						&& b.mod.type_store.types[recv_typ_id].kind == .struct_t {
						if recv_name := b.struct_name_from_type_id(recv_typ_id) {
							candidate := b.resolve_receiver_method_name(recv_name, mangled_mname)
							if candidate != '' {
								name = candidate
								resolved = true
								if map_has_key_bool(b.flag_enum_names, recv_name)
									&& mangled_mname in ['has', 'all'] {
									flag_enum_receiver_type = recv_name
								}
							}
						}
					}
				}
			}
			if !resolved {
				inferred_type := b.infer_receiver_type(mangled_mname)
				if inferred_type != '' {
					name = '${inferred_type}__${mangled_mname}'
					// Check if this is a flag enum method
					if map_has_key_bool(b.flag_enum_names, inferred_type)
						&& mangled_mname in ['has', 'all'] {
						flag_enum_receiver_type = inferred_type
					}
				} else {
					name = mangled_mname
				}
			}
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
	if !is_method_call {
		b.adjust_map_ptr_args_from_exprs(name, [node.expr], mut args)
	}

	// For method calls, prepend receiver as first argument
	if is_method_call && receiver_val != 0 {
		if node.lhs is ast.SelectorExpr {
			receiver_val = b.maybe_use_addressable_receiver(name, node.lhs.lhs, receiver_val)
		}
		args.prepend(receiver_val)
	}
	b.adjust_map_runtime_call_args(name, mut args)
	b.adjust_call_args_for_fn(name, mut args)

	// Remap common transformed helper names to canonical builtin symbols.
	if !map_has_key_type_id(b.func_ret_types, name) {
		if name == 'new_array_from_c_array_noscan'
			&& map_has_key_type_id(b.func_ret_types, 'new_array_from_c_array') {
			name = 'new_array_from_c_array'
		} else if name == 'bytestr' && map_has_key_type_id(b.func_ret_types, 'array__bytestr') {
			name = 'array__bytestr'
		} else if name.starts_with('Array_') && name.ends_with('_contains')
			&& map_has_key_type_id(b.func_ret_types, 'array__contains') {
			name = 'array__contains'
		}
	}

	// Remap double-underscore method names to single-underscore for transformer-generated functions
	if !map_has_key_type_id(b.func_ret_types, name) && name.ends_with('__str') {
		candidate := name[..name.len - 5] + '_str'
		if map_has_key_type_id(b.func_ret_types, candidate) {
			name = candidate
		}
	}
	// Generate str() stubs for types that don't have an implementation
	if !map_has_key_type_id(b.func_ret_types, name) && name.ends_with('__str') {
		b.generate_str_stub(name)
	}
	name = b.remap_missing_runtime_fn(name)

	fn_val := b.mod.add_value_node(.unknown, 0, name, 0)
	args.prepend(fn_val)

	// Look up the return type from tracked functions
	i64_t := b.mod.type_store.get_int(64)
	mut ret_type := i64_t
	if rt := b.get_func_ret_type(name) {
		ret_type = rt
	} else if crt := b.known_c_ret_type(name) {
		ret_type = crt
	}

	return b.mod.add_instr(.call, b.cur_block, ret_type, args)
}

fn (mut b Builder) expr_prefix(node ast.PrefixExpr) ValueID {
	// Handle address-of operator with struct init: &Point{} -> heap allocation
	if node.op == .amp {
		if node.expr is ast.InitExpr {
			return b.expr_heap_alloc(node.expr)
		}
		// Address of a call result (`&foo()`) needs storage for the temporary.
		// This appears in transformed sumtype payload construction.
		if node.expr is ast.CallExpr {
			call_val := b.expr(node.expr)
			if call_val > 0 && call_val < b.mod.values.len {
				call_typ := b.mod.values[call_val].typ
				if call_typ > 0 && call_typ < b.mod.type_store.types.len {
					call_info := b.mod.type_store.types[call_typ]
					if call_info.kind == .ptr_t {
						return call_val
					}
					i64_t := b.mod.type_store.get_int(64)
					i8_t := b.mod.type_store.get_int(8)
					voidptr_t := b.mod.type_store.get_ptr(i8_t)
					mut size_bytes := b.type_size_from_ssa_type(call_typ)
					if size_bytes <= 0 {
						size_bytes = 8
					}
					size_val := b.mod.add_value_node(.constant, i64_t, size_bytes.str(),
						0)
					malloc_sym := b.mod.add_value_node(.unknown, 0, 'malloc', 0)
					raw_mem := b.mod.add_instr(.call, b.cur_block, voidptr_t, [
						malloc_sym,
						size_val,
					])
					typed_ptr := b.mod.add_instr(.bitcast, b.cur_block, b.mod.type_store.get_ptr(call_typ),
						[raw_mem])
					b.mod.add_instr(.store, b.cur_block, 0, [call_val, typed_ptr])
					return typed_ptr
				}
			}
		}
		// Transformed sumtype payloads use `voidptr(&'str')`.
		// String literals already lower to pointers, so preserve that pointer
		// instead of trying to take the address of a non-lvalue literal.
		if node.expr is ast.StringLiteral || node.expr is ast.StringInterLiteral {
			return b.expr(node.expr)
		}
		// `&Type(expr)` is a pointer cast, not taking the address of a temporary.
		if node.expr is ast.CallOrCastExpr {
			if node.expr.lhs is ast.Ident {
				raw_val := b.expr(node.expr.expr)
				cast_type := b.ast_type_to_ssa(node.expr.lhs)
				if cast_type > 0 {
					ptr_type := b.mod.type_store.get_ptr(cast_type)
					mut raw_type := TypeID(0)
					if raw_val > 0 && raw_val < b.mod.values.len {
						raw_type = b.mod.values[raw_val].typ
						if raw_type > 0 && raw_type < b.mod.type_store.types.len
							&& b.mod.type_store.types[raw_type].kind != .ptr_t {
							if raw_string_data := b.string_data_ptr_from_value(raw_val) {
								return b.coerce_value_to_type(raw_string_data, ptr_type)
							}
						}
						if raw_type == ptr_type {
							return raw_val
						}
						if raw_type > 0 && raw_type < b.mod.type_store.types.len {
							raw_kind := b.mod.type_store.types[raw_type].kind
							if raw_kind in [.ptr_t, .int_t] {
								return b.mod.add_instr(.bitcast, b.cur_block, ptr_type,
									[raw_val])
							}
						}
					}
				}
				return raw_val
			}
			return b.expr(node.expr)
		}
		// In patterns like `&u8(a.data)` transformed to `&a.data`, if the selector
		// already evaluates to a pointer value, keep the value (cast semantics).
		if node.expr is ast.SelectorExpr {
			sel_val := b.expr(node.expr)
			if sel_val > 0 && sel_val < b.mod.values.len {
				sel_typ := b.mod.values[sel_val].typ
				if sel_typ > 0 && sel_typ < b.mod.type_store.types.len {
					if b.mod.type_store.types[sel_typ].kind == .ptr_t {
						return sel_val
					}
				}
			}
		}
		if node.expr is ast.Ident {
			ident_ptr := b.addr(node.expr)
			if ident_ptr > 0 && ident_ptr < b.mod.values.len {
				slot_typ_id := b.mod.values[ident_ptr].typ
				if slot_typ_id > 0 && slot_typ_id < b.mod.type_store.types.len {
					slot_typ := b.mod.type_store.types[slot_typ_id]
					if slot_typ.kind == .ptr_t {
						elem_typ_id := slot_typ.elem_type
						if elem_typ_id > 0 && elem_typ_id < b.mod.type_store.types.len {
							// Struct values are commonly represented as stack slots that hold
							// a pointer to the actual struct allocation. `&x` should yield that
							// struct pointer, not the address of the pointer slot.
							if b.mod.type_store.types[elem_typ_id].kind == .ptr_t
								&& b.get_var_struct_type(node.expr.name) != none {
								return b.mod.add_instr(.load, b.cur_block, elem_typ_id,
									[ident_ptr])
							}
						}
					}
				}
			}
			return ident_ptr
		}
		// Transformed pointer casts like `&&u8(x)` are represented as nested
		// prefixes (`&` of `&u8(x)`). Treat the outer `&` as pointer-depth cast,
		// not as taking the address of a temporary expression.
		if node.expr is ast.PrefixExpr && node.expr.op == .amp
			&& (node.expr.expr is ast.CallOrCastExpr || node.expr.expr is ast.CastExpr) {
			inner_val := b.expr(node.expr)
			if inner_val > 0 && inner_val < b.mod.values.len {
				inner_typ := b.mod.values[inner_val].typ
				if inner_typ > 0 && inner_typ < b.mod.type_store.types.len {
					inner_info := b.mod.type_store.types[inner_typ]
					if inner_info.kind in [.ptr_t, .int_t] {
						target_ptr := b.mod.type_store.get_ptr(inner_typ)
						return b.mod.add_instr(.bitcast, b.cur_block, target_ptr, [
							inner_val,
						])
					}
				}
			}
			return inner_val
		}
		// For other &expr cases, just return the address
		return b.addr(node.expr)
	}

	// Unary dereference: *ptr
	if node.op == .mul {
		ptr := b.expr(node.expr)
		if ptr <= 0 || ptr >= b.mod.values.len {
			return 0
		}
		ptr_typ := b.mod.values[ptr].typ
		if ptr_typ <= 0 || ptr_typ >= b.mod.type_store.types.len {
			return 0
		}
		ptr_info := b.mod.type_store.types[ptr_typ]
		if ptr_info.kind != .ptr_t || ptr_info.elem_type <= 0 {
			return 0
		}
		return b.mod.add_instr(.load, b.cur_block, ptr_info.elem_type, [ptr])
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
	mut declared_elem_type := TypeID(0)
	if node.typ is ast.Type {
		match node.typ {
			ast.ArrayType {
				declared_elem_type = b.ast_type_to_ssa(node.typ.elem_type)
			}
			ast.ArrayFixedType {
				declared_elem_type = b.ast_type_to_ssa(node.typ.elem_type)
				if elem_count == 0 {
					if len_i64 := b.try_eval_const_expr_i64(node.typ.len) {
						if len_i64 > 0 {
							elem_count = int(len_i64)
						}
					}
					if elem_count == 0 && node.typ.len is ast.Ident {
						if resolved_len := b.resolve_const_ident_i64(node.typ.len.name) {
							if resolved_len > 0 {
								elem_count = int(resolved_len)
							}
						}
					}
					if elem_count == 0 {
						len_val := b.expr(node.typ.len)
						if len_val > 0 && len_val < b.mod.values.len
							&& b.mod.values[len_val].kind == .constant {
							elem_count = b.mod.values[len_val].name.int()
						}
					}
				}
			}
			else {}
		}
	}

	// Check if this is a sized array initialization (len: expr)
	// Only use `len:` when there are no explicit literal elements.
	if node.exprs.len == 0 && node.len !is ast.EmptyExpr {
		// For []T{len: n}, prefer compile-time evaluation first so named
		// constants like `max_path_len` can materialize fixed buffers.
		if len_i64 := b.try_eval_const_expr_i64(node.len) {
			if len_i64 > 0 {
				elem_count = int(len_i64)
			}
		}
		if elem_count == 0 && node.len is ast.Ident {
			if resolved_len := b.resolve_const_ident_i64(node.len.name) {
				if resolved_len > 0 {
					elem_count = int(resolved_len)
				}
			}
		}
		if elem_count == 0 {
			len_val := b.expr(node.len)
			// Fallback to SSA constants if available.
			if len_val > 0 && len_val < b.mod.values.len && b.mod.values[len_val].kind == .constant {
				elem_count = b.mod.values[len_val].name.int()
			}
		}
	}

	if elem_count == 0 {
		// Empty array - return null pointer for now
		return b.mod.add_value_node(.constant, b.mod.type_store.get_ptr(i64_t), '0', 0)
	}

	// Infer element type from declared array type and literal values.
	mut elem_type := TypeID(0)
	mut literal_vals := []ValueID{}
	if node.exprs.len > 0 {
		for elem_expr in node.exprs {
			elem_val := b.expr(elem_expr)
			literal_vals << elem_val
			if elem_type == 0 && elem_val > 0 && elem_val < b.mod.values.len {
				elem_info := b.mod.values[elem_val]
				mut candidate := elem_info.typ
				// Struct literals (`Type{...}`) are lowered as pointers by expr_init.
				// Arrays of struct values must still use the struct element type.
				if elem_expr is ast.InitExpr && candidate > 0
					&& candidate < b.mod.type_store.types.len {
					candidate_info := b.mod.type_store.types[candidate]
					if candidate_info.kind == .ptr_t && candidate_info.elem_type > 0
						&& candidate_info.elem_type < b.mod.type_store.types.len {
						pointee_info := b.mod.type_store.types[candidate_info.elem_type]
						if pointee_info.kind == .struct_t {
							candidate = candidate_info.elem_type
						}
					}
				}
				if elem_info.kind == .string_literal && candidate > 0
					&& candidate < b.mod.type_store.types.len {
					ptr_info := b.mod.type_store.types[candidate]
					if ptr_info.kind == .ptr_t && ptr_info.elem_type > 0
						&& ptr_info.elem_type < b.mod.type_store.types.len
						&& b.mod.type_store.types[ptr_info.elem_type].kind == .struct_t {
						if struct_name := b.struct_name_from_type_id(ptr_info.elem_type) {
							if struct_name == 'string' {
								candidate = ptr_info.elem_type
							}
						}
					}
				}
				if candidate > 0 {
					elem_type = candidate
				}
			}
		}
	}
	if elem_type == 0 {
		if declared_elem_type > 0 {
			elem_type = declared_elem_type
		}
	}
	if elem_type == 0 {
		elem_type = i64_t
	}

	// Create array type
	array_type := b.mod.type_store.get_array(elem_type, elem_count)
	array_ptr_t := b.mod.type_store.get_ptr(array_type)
	elem_ptr_t := b.mod.type_store.get_ptr(elem_type)

	mut array_ptr := ValueID(0)
	if b.in_global_init {
		// Global initializers must not keep pointers to stack storage.
		mut elem_size := b.type_size_from_ssa_type(elem_type)
		if elem_size <= 0 {
			elem_size = 8
		}
		total_bytes := elem_count * elem_size
		size_val := b.mod.add_value_node(.constant, i64_t, total_bytes.str(), 0)
		i8_t := b.mod.type_store.get_int(8)
		raw_ptr_t := b.mod.type_store.get_ptr(i8_t)
		malloc_fn := b.mod.add_value_node(.unknown, 0, 'malloc', 0)
		raw_ptr := b.mod.add_instr(.call, b.cur_block, raw_ptr_t, [malloc_fn, size_val])
		array_ptr = b.mod.add_instr(.bitcast, b.cur_block, array_ptr_t, [raw_ptr])
	} else {
		// Function-local array literals can stay on the stack.
		array_ptr = b.mod.add_instr(.alloca, b.cur_block, array_ptr_t, [])
	}

	// Initialize elements
	if literal_vals.len > 0 {
		// Literal array: [1, 2, 3]
		for i, elem_val in literal_vals {
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
		// Sized array without init: zero-initialize with memset to avoid
		// generating thousands of per-element stores for large buffers.
		mut elem_size := b.type_size_from_ssa_type(elem_type)
		if elem_size <= 0 {
			elem_size = 8
		}
		total_bytes := elem_count * elem_size
		if total_bytes > 0 {
			i8_t := b.mod.type_store.get_int(8)
			i8_ptr_t := b.mod.type_store.get_ptr(i8_t)
			base_ptr := b.coerce_value_to_type(array_ptr, i8_ptr_t)
			zero := b.mod.add_value_node(.constant, i64_t, '0', 0)
			size_val := b.mod.add_value_node(.constant, i64_t, total_bytes.str(), 0)
			memset_fn := b.mod.add_value_node(.unknown, 0, 'memset', 0)
			b.mod.add_instr(.call, b.cur_block, i8_ptr_t, [memset_fn, base_ptr, zero, size_val])
		}
	}

	// Return the array pointer
	return array_ptr
}

// expr_map_init_from_type handles empty map init from InitExpr with MapType: map[K]V{}
fn (mut b Builder) expr_map_init_from_type(map_type ast.MapType) ValueID {
	map_val := b.emit_new_map_value(map_type)
	map_ptr_t := b.mod.type_store.get_ptr(b.mod.values[map_val].typ)
	map_ptr := b.mod.add_instr(.alloca, b.cur_block, map_ptr_t, [])
	b.mod.add_instr(.store, b.cur_block, 0, [map_val, map_ptr])
	return map_ptr
}

fn (mut b Builder) expr_map_init(node ast.MapInitExpr) ValueID {
	mut map_val := ValueID(0)
	if map_type := b.map_type_from_type_expr(node.typ) {
		map_val = b.emit_new_map_value(map_type)
	} else {
		// Fallback for unresolved map type metadata.
		map_val = b.emit_new_map_value_with_sizes(8, 8, false)
	}
	map_ptr_t := b.mod.type_store.get_ptr(b.mod.values[map_val].typ)
	map_ptr := b.mod.add_instr(.alloca, b.cur_block, map_ptr_t, [])
	b.mod.add_instr(.store, b.cur_block, 0, [map_val, map_ptr])

	// Add initial key-value pairs through the runtime map API.
	mut init_value_type := TypeID(0)
	if map_type := b.map_type_from_type_expr(node.typ) {
		init_value_type = b.ast_type_to_ssa(map_type.value_type)
	}
	for i, key_expr in node.keys {
		key_val := b.expr(key_expr)
		val_expr := node.vals[i]
		val_val := b.expr(val_expr)
		b.emit_runtime_map_set(map_ptr, key_val, val_val, init_value_type)
	}
	return map_ptr
}

fn map_int_key_width(key_size int) int {
	if key_size <= 1 {
		return 1
	}
	if key_size <= 2 {
		return 2
	}
	if key_size <= 4 {
		return 4
	}
	return 8
}

fn (mut b Builder) emit_new_map_value(map_type ast.MapType) ValueID {
	key_is_string := map_type.key_type is ast.Ident && map_type.key_type.name == 'string'
	key_bytes := b.type_size_from_ast(map_type.key_type)
	value_bytes := b.type_size_from_ast(map_type.value_type)
	return b.emit_new_map_value_with_sizes(key_bytes, value_bytes, key_is_string)
}

fn (mut b Builder) emit_new_map_value_with_sizes(key_bytes int, value_bytes int, key_is_string bool) ValueID {
	i64_t := b.mod.type_store.get_int(64)
	mut map_t := b.mod.type_store.get_int(64)
	if map_struct_t := b.get_struct_type_id('map') {
		map_t = map_struct_t
	}
	mut hash_fn_name := ''
	mut eq_fn_name := ''
	mut clone_fn_name := ''
	mut free_fn_name := ''
	if key_is_string {
		hash_fn_name = 'map_hash_string'
		eq_fn_name = 'map_eq_string'
		clone_fn_name = 'map_clone_string'
		free_fn_name = 'map_free_string'
	} else {
		key_width := map_int_key_width(key_bytes)
		hash_fn_name = 'map_hash_int_${key_width}'
		eq_fn_name = 'map_eq_int_${key_width}'
		clone_fn_name = 'map_clone_int_${key_width}'
		free_fn_name = 'map_free_nop'
	}
	new_map_fn := b.mod.add_value_node(.unknown, 0, 'new_map', 0)
	key_bytes_val := b.mod.add_value_node(.constant, i64_t, key_bytes.str(), 0)
	value_bytes_val := b.mod.add_value_node(.constant, i64_t, value_bytes.str(), 0)
	// Pass function symbols as function references so native backends materialize addresses.
	hash_fn := b.mod.add_value_node(.func_ref, i64_t, hash_fn_name, 0)
	eq_fn := b.mod.add_value_node(.func_ref, i64_t, eq_fn_name, 0)
	clone_fn := b.mod.add_value_node(.func_ref, i64_t, clone_fn_name, 0)
	free_fn := b.mod.add_value_node(.func_ref, i64_t, free_fn_name, 0)
	return b.mod.add_instr(.call, b.cur_block, map_t, [new_map_fn, key_bytes_val, value_bytes_val,
		hash_fn, eq_fn, clone_fn, free_fn])
}

// type_size_from_ssa_type returns the byte size for an SSA type.
fn (b &Builder) type_size_from_ssa_type(type_id TypeID) int {
	if type_id <= 0 || type_id >= b.mod.type_store.types.len {
		return 8
	}
	t := b.mod.type_store.types[type_id]
	return match t.kind {
		.int_t, .float_t {
			if t.width > 0 {
				(t.width + 7) / 8
			} else {
				8
			}
		}
		.ptr_t {
			8
		}
		.array_t {
			elem_size := b.type_size_from_ssa_type(t.elem_type)
			if t.len > 0 {
				elem_size * t.len
			} else {
				elem_size
			}
		}
		.struct_t {
			if t.fields.len == 0 {
				8
			} else {
				mut total := 0
				for field_t in t.fields {
					total += b.type_size_from_ssa_type(field_t)
				}
				if total > 0 {
					total
				} else {
					8
				}
			}
		}
		else {
			8
		}
	}
}

// type_size_from_ast returns the size in bytes for an AST type expression.
fn (mut b Builder) type_size_from_ast(typ ast.Expr) int {
	if typ is ast.Ident {
		match typ.name {
			'i8', 'u8', 'byte', 'bool' {
				return 1
			}
			'i16', 'u16' {
				return 2
			}
			'i32', 'u32', 'rune' {
				return 4
			}
			'i64', 'u64', 'f64', 'int' {
				return 8
			}
			'f32' {
				return 4
			}
			'string' {
				return 24
			} // V string struct: ptr + len + is_lit
			else {
				if st := b.get_struct_type_id(typ.name) {
					return b.type_size_from_ssa_type(st)
				}
				if typ.name.contains('.') {
					short_name := typ.name.all_after_last('.')
					if st := b.get_struct_type_id(short_name) {
						return b.type_size_from_ssa_type(st)
					}
				}
				type_id := b.ast_type_to_ssa(typ)
				if type_id > 0 {
					return b.type_size_from_ssa_type(type_id)
				}
				return 8 // Default to pointer size
			}
		}
	}
	if typ is ast.PrefixExpr && typ.op == .amp {
		return 8
	}
	if typ is ast.Type {
		match typ {
			ast.ArrayType {
				return b.type_size_from_ssa_type(b.ensure_runtime_array_type())
			}
			ast.MapType {
				if map_t := b.get_struct_type_id('map') {
					return b.type_size_from_ssa_type(map_t)
				}
				return 8
			}
			else {}
		}
	}
	type_id := b.ast_type_to_ssa(typ)
	return b.type_size_from_ssa_type(type_id)
}

fn (mut b Builder) emit_runtime_map_set(map_ptr ValueID, key_val ValueID, val_val ValueID, expected_value_type TypeID) {
	map_arg := b.normalize_map_ptr(map_ptr)
	key_type := b.mod.values[key_val].typ
	mut value_type := b.mod.values[val_val].typ
	mut key_arg := key_val
	if key_type > 0 && key_type < b.mod.type_store.types.len {
		key_kind := b.mod.type_store.types[key_type].kind
		if key_kind !in [.ptr_t, .array_t] {
			key_ptr_t := b.mod.type_store.get_ptr(key_type)
			key_ptr := b.mod.add_instr(.alloca, b.cur_block, key_ptr_t, [])
			b.mod.add_instr(.store, b.cur_block, 0, [key_val, key_ptr])
			key_arg = key_ptr
		}
	}
	mut val_base := val_val
	if expected_value_type > 0 && expected_value_type < b.mod.type_store.types.len && value_type > 0
		&& value_type < b.mod.type_store.types.len {
		expected_info := b.mod.type_store.types[expected_value_type]
		value_info := b.mod.type_store.types[value_type]
		if expected_info.kind != .ptr_t && value_info.kind == .ptr_t && value_info.elem_type > 0
			&& value_info.elem_type < b.mod.type_store.types.len {
			elem_info := b.mod.type_store.types[value_info.elem_type]
			if elem_info.kind == .ptr_t && elem_info.elem_type == expected_value_type {
				val_base = b.mod.add_instr(.load, b.cur_block, value_info.elem_type, [
					val_val,
				])
				value_type = b.mod.values[val_base].typ
			}
		}
	}
	if value_type > 0 && value_type < b.mod.type_store.types.len {
		value_info := b.mod.type_store.types[value_type]
		if value_info.kind == .ptr_t && value_info.elem_type > 0
			&& value_info.elem_type < b.mod.type_store.types.len {
			elem_info := b.mod.type_store.types[value_info.elem_type]
			if elem_info.kind == .ptr_t && elem_info.elem_type > 0
				&& elem_info.elem_type < b.mod.type_store.types.len {
				if string_t := b.get_struct_type_id('string') {
					if elem_info.elem_type == string_t {
						val_base = b.mod.add_instr(.load, b.cur_block, value_info.elem_type,
							[
							val_base,
						])
						value_type = b.mod.values[val_base].typ
					}
				}
			}
		}
	}
	mut val_arg := val_base
	if value_type > 0 && value_type < b.mod.type_store.types.len {
		value_kind := b.mod.type_store.types[value_type].kind
		if value_kind !in [.ptr_t, .array_t] {
			value_ptr_t := b.mod.type_store.get_ptr(value_type)
			value_ptr := b.mod.add_instr(.alloca, b.cur_block, value_ptr_t, [])
			b.mod.add_instr(.store, b.cur_block, 0, [val_base, value_ptr])
			val_arg = value_ptr
		}
	}
	map_set_fn := b.mod.add_value_node(.unknown, 0, 'map__set', 0)
	b.mod.add_instr(.call, b.cur_block, 0, [map_set_fn, map_arg, key_arg, val_arg])
}

fn (mut b Builder) emit_runtime_map_get(map_ptr ValueID, key_val ValueID, value_type TypeID) ValueID {
	map_arg := b.normalize_map_ptr(map_ptr)
	i8_t := b.mod.type_store.get_int(8)
	i64_t := b.mod.type_store.get_int(64)
	voidptr_t := b.mod.type_store.get_ptr(i8_t)
	key_type := b.mod.values[key_val].typ
	mut out_type := value_type
	if out_type == 0 {
		out_type = i64_t
	}
	out_ptr_t := b.mod.type_store.get_ptr(out_type)
	mut key_arg := key_val
	if key_type > 0 && key_type < b.mod.type_store.types.len {
		key_kind := b.mod.type_store.types[key_type].kind
		if key_kind !in [.ptr_t, .array_t] {
			key_ptr_t := b.mod.type_store.get_ptr(key_type)
			key_ptr := b.mod.add_instr(.alloca, b.cur_block, key_ptr_t, [])
			b.mod.add_instr(.store, b.cur_block, 0, [key_val, key_ptr])
			key_arg = key_ptr
		}
	}
	out_ptr := b.mod.add_instr(.alloca, b.cur_block, out_ptr_t, [])
	zero := b.mod.add_value_node(.constant, i64_t, '0', 0)
	b.mod.add_instr(.store, b.cur_block, 0, [zero, out_ptr])
	map_get_fn := b.mod.add_value_node(.unknown, 0, 'map__get', 0)
	raw_ptr := b.mod.add_instr(.call, b.cur_block, voidptr_t, [map_get_fn, map_arg, key_arg, out_ptr])
	typed_ptr := b.mod.add_instr(.bitcast, b.cur_block, out_ptr_t, [raw_ptr])
	return b.mod.add_instr(.load, b.cur_block, out_type, [typed_ptr])
}

fn (mut b Builder) emit_runtime_map_get_ptr(map_ptr ValueID, key_val ValueID, value_type TypeID) ValueID {
	map_arg := b.normalize_map_ptr(map_ptr)
	i8_t := b.mod.type_store.get_int(8)
	i64_t := b.mod.type_store.get_int(64)
	voidptr_t := b.mod.type_store.get_ptr(i8_t)
	key_type := b.mod.values[key_val].typ
	mut out_type := value_type
	if out_type == 0 {
		out_type = i64_t
	}
	out_ptr_t := b.mod.type_store.get_ptr(out_type)
	mut key_arg := key_val
	if key_type > 0 && key_type < b.mod.type_store.types.len {
		key_kind := b.mod.type_store.types[key_type].kind
		if key_kind !in [.ptr_t, .array_t] {
			key_ptr_t := b.mod.type_store.get_ptr(key_type)
			key_ptr := b.mod.add_instr(.alloca, b.cur_block, key_ptr_t, [])
			b.mod.add_instr(.store, b.cur_block, 0, [key_val, key_ptr])
			key_arg = key_ptr
		}
	}
	out_ptr := b.mod.add_instr(.alloca, b.cur_block, out_ptr_t, [])
	zero := b.mod.add_value_node(.constant, i64_t, '0', 0)
	b.mod.add_instr(.store, b.cur_block, 0, [zero, out_ptr])
	map_get_fn := b.mod.add_value_node(.unknown, 0, 'map__get', 0)
	raw_ptr := b.mod.add_instr(.call, b.cur_block, voidptr_t, [map_get_fn, map_arg, key_arg, out_ptr])
	return b.mod.add_instr(.bitcast, b.cur_block, out_ptr_t, [raw_ptr])
}

fn (mut b Builder) expr_heap_alloc(node ast.InitExpr) ValueID {
	// Heap allocation: &StructInit{}
	// 1. Find struct type
	mut struct_t := 0

	if node.typ is ast.Ident {
		if st := b.get_struct_type_id(node.typ.name) {
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

	// 2. Calculate allocation size from concrete type layout.
	struct_type := b.mod.type_store.types[struct_t]
	mut size := b.type_size_from_ssa_type(struct_t)
	if size <= 0 {
		size = 8
	}

	i64_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(struct_t)

	// 3. Call malloc
	malloc_fn := b.mod.add_value_node(.unknown, 0, 'malloc', 0)
	size_val := b.mod.add_value_node(.constant, i64_t, size.str(), 0)
	heap_ptr := b.mod.add_instr(.call, b.cur_block, ptr_t, [malloc_fn, size_val])

	// 4. Initialize fields (explicit values or zero defaults).
	mut init_fields := map[string]ast.Expr{}
	for i, field in node.fields {
		if field.name != '' {
			init_fields[field.name] = field.value
			if field.name.contains('.') {
				parent_name := field.name.all_before('.')
				if parent_name !in init_fields {
					init_fields[parent_name] = field.value
				}
			}
		} else if i < struct_type.field_names.len {
			init_fields[struct_type.field_names[i]] = field.value
		}
	}
	for i, field_name in struct_type.field_names {
		idx_val := b.mod.add_value_node(.constant, i64_t, i.str(), 0)
		field_type := if i < struct_type.fields.len { struct_type.fields[i] } else { i64_t }
		field_ptr_t := b.mod.type_store.get_ptr(field_type)
		field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_t, [
			heap_ptr,
			idx_val,
		])
		if expr := init_fields[field_name] {
			mut val := b.expr(expr)
			val = b.coerce_value_to_type(val, field_type)
			b.mod.add_instr(.store, b.cur_block, 0, [val, field_ptr])
		} else {
			zero_val := b.mod.add_value_node(.constant, i64_t, '0', 0)
			b.mod.add_instr(.store, b.cur_block, 0, [zero_val, field_ptr])
		}
	}

	return heap_ptr
}

fn (mut b Builder) expr_postfix(node ast.PostfixExpr) ValueID {
	if node.op in [.not, .question] {
		if data_val := b.option_like_data_value(b.expr(node.expr)) {
			return data_val
		}
		return b.expr(node.expr)
	}
	// Handle x++ / x-- for any addressable expression (ident, selector, index, ...).
	ptr := b.addr(node.expr)
	if ptr != 0 {
		i64_t := b.mod.type_store.get_int(64)
		mut value_type := i64_t
		ptr_type := b.mod.values[ptr].typ
		if ptr_type > 0 && ptr_type < b.mod.type_store.types.len {
			ptr_info := b.mod.type_store.types[ptr_type]
			if ptr_info.kind == .ptr_t && ptr_info.elem_type > 0 {
				value_type = ptr_info.elem_type
			}
		}

		// 1. Load current value
		old_val := b.mod.add_instr(.load, b.cur_block, value_type, [ptr])

		// 2. Add/Sub 1
		one := b.mod.add_value_node(.constant, value_type, '1', 0)
		op := if node.op == .inc { OpCode.add } else { OpCode.sub }
		new_val := b.mod.add_instr(op, b.cur_block, value_type, [old_val, one])

		// 3. Store new value
		b.mod.add_instr(.store, b.cur_block, 0, [new_val, ptr])

		// Postfix returns the old value
		return old_val
	}
	// For postfix forms like fixed-array literal `[...]!`, preserve the
	// underlying expression value instead of dropping it to zero.
	return b.expr(node.expr)
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

fn (b &Builder) global_value_id(name string) ?ValueID {
	for v in b.mod.values {
		if v.kind == .global && v.name == name {
			return v.id
		}
	}
	return none
}

// addr returns the ValueID (pointer) representing the L-Value of an expression
fn (mut b Builder) addr(node ast.Expr) ValueID {
	match node {
		ast.Ident {
			mut lookup_name := node.name
			if lookup_name.starts_with('builtin__') {
				lookup_name = lookup_name[9..]
			}
			// Check locals
			if ptr := b.get_var_ptr(lookup_name) {
				// FIX: Ensure it is a valid ID (0 is invalid now)
				if ptr != 0 {
					return ptr
				}
			}
			// Check globals
			for g in b.mod.globals {
				if g.name == lookup_name {
					if gv := b.global_value_id(lookup_name) {
						return gv
					}
					break
				}
			}
			return 0
		}
		ast.PrefixExpr {
			if node.op == .mul {
				ptr_val := b.expr(node.expr)
				if ptr_val > 0 && ptr_val < b.mod.values.len {
					ptr_typ_id := b.mod.values[ptr_val].typ
					if ptr_typ_id > 0 && ptr_typ_id < b.mod.type_store.types.len {
						if b.mod.type_store.types[ptr_typ_id].kind == .ptr_t {
							return ptr_val
						}
					}
				}
			}
			return 0
		}
		ast.CastExpr {
			cast_val := b.expr(node)
			if cast_val > 0 && cast_val < b.mod.values.len {
				cast_typ_id := b.mod.values[cast_val].typ
				if cast_typ_id > 0 && cast_typ_id < b.mod.type_store.types.len {
					if b.mod.type_store.types[cast_typ_id].kind == .ptr_t {
						return cast_val
					}
				}
			}
			return 0
		}
		ast.ParenExpr {
			return b.addr(node.expr)
		}
		ast.ModifierExpr {
			return b.addr(node.expr)
		}
		ast.SelectorExpr {
			if node.lhs is ast.Ident {
				lhs_name := node.lhs.name
				// Module-qualified globals/constants (e.g. os.args, os.wd_at_startup).
				// Prefer this when there is no local receiver named `lhs_name`.
				mut has_local_receiver := false
				if ptr := b.get_var_ptr(lhs_name) {
					has_local_receiver = ptr != 0
				}
				if !has_local_receiver && b.is_module_call_receiver(lhs_name) {
					if gv := b.global_value_id(node.rhs.name) {
						return gv
					}
					qualified_name := '${lhs_name}__${node.rhs.name}'
					if gv := b.global_value_id(qualified_name) {
						return gv
					}
				}
			}
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
			// Transformed sum-type payload access often appears as nested selectors:
			// `sum._data._Variant`. `_Variant` does not exist as a real field;
			// it aliases the raw `_data` storage slot.
			if node.lhs is ast.SelectorExpr {
				if node.lhs.rhs.name == '_data' && node.rhs.name.starts_with('_') {
					return b.addr(ast.Expr(node.lhs))
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
			mut rhs_name := node.rhs.name
			// Transformed sum-type payload selectors use names like `_data._Variant`.
			// Resolve these to the concrete `_data` storage field.
			if rhs_name.contains('.') {
				rhs_name = rhs_name.all_before('.')
			}
			// Accept both transformed and runtime field spellings.
			if rhs_name == 'tag' {
				rhs_name = '_tag'
			}
			if rhs_name == 'data' && '_data' in val_typ.field_names {
				rhs_name = '_data'
			}
			if rhs_name == 'error' && 'err' in val_typ.field_names {
				rhs_name = 'err'
			}
			mut idx := -1
			for i, name in val_typ.field_names {
				if name == rhs_name {
					idx = i
					break
				}
			}
			is_array_like := val_typ.fields.len >= 6
				&& ('data' in val_typ.field_names || 'f_0_data' in val_typ.field_names)
				&& ('len' in val_typ.field_names || 'f_2_len' in val_typ.field_names)
			is_string_like := val_typ.fields.len >= 3
				&& ('str' in val_typ.field_names || 'f_0_str' in val_typ.field_names)
				&& ('len' in val_typ.field_names || 'f_1_len' in val_typ.field_names)
			if idx == -1 {
				if b.is_sum_type_id(val_typ_id) {
					if variant_field_ptr := b.addr_sumtype_variant_field(actual_base,
						val_typ_id, node.rhs.name)
					{
						return variant_field_ptr
					}
				}
				// Fallback mappings for transformed/runtime field spellings.
				if struct_name := b.struct_name_from_type_id(val_typ_id) {
					if struct_name == 'array' || is_array_like {
						idx = match node.rhs.name {
							'data', 'f_0_data' { 0 }
							'offset', 'f_1_offset' { 1 }
							'len', 'f_2_len' { 2 }
							'cap', 'f_3_cap' { 3 }
							'flags', 'f_4_flags' { 4 }
							'element_size', 'f_5_element_size' { 5 }
							else { -1 }
						}
					} else if struct_name == 'string' || is_string_like {
						idx = match node.rhs.name {
							'str', 'f_0_str' { 0 }
							'len', 'f_1_len' { 1 }
							'is_lit', 'f_2_is_lit' { 2 }
							else { -1 }
						}
					}
				} else if is_array_like {
					idx = match node.rhs.name {
						'data', 'f_0_data' { 0 }
						'offset', 'f_1_offset' { 1 }
						'len', 'f_2_len' { 2 }
						'cap', 'f_3_cap' { 3 }
						'flags', 'f_4_flags' { 4 }
						'element_size', 'f_5_element_size' { 5 }
						else { -1 }
					}
				} else if is_string_like {
					idx = match node.rhs.name {
						'str', 'f_0_str' { 0 }
						'len', 'f_1_len' { 1 }
						'is_lit', 'f_2_is_lit' { 2 }
						else { -1 }
					}
				}
				if idx == -1 {
					// Legacy generic fallback.
					idx = 0
					if node.rhs.name == 'y' || node.rhs.name == 'b' || node.rhs.name == 'len' {
						idx = 1
					} else if node.rhs.name == 'is_lit' {
						idx = 2
					}
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
			if struct_name := b.struct_name_from_type_id(val_typ_id) {
				if struct_name == 'array' || is_array_like {
					i64_t := b.mod.type_store.get_int(64)
					if idx == 0 {
						i8_t := b.mod.type_store.get_int(8)
						field_type = b.mod.type_store.get_ptr(i8_t)
					} else if idx >= 1 && idx <= 5 {
						field_type = i64_t
					}
				} else if struct_name == 'string' || is_string_like {
					i64_t := b.mod.type_store.get_int(64)
					if idx == 0 {
						i8_t := b.mod.type_store.get_int(8)
						field_type = b.mod.type_store.get_ptr(i8_t)
					} else if idx == 1 || idx == 2 {
						field_type = i64_t
					}
				}
			} else if is_array_like {
				i64_t := b.mod.type_store.get_int(64)
				if idx == 0 {
					i8_t := b.mod.type_store.get_int(8)
					field_type = b.mod.type_store.get_ptr(i8_t)
				} else if idx >= 1 && idx <= 5 {
					field_type = i64_t
				}
			} else if is_string_like {
				i64_t := b.mod.type_store.get_int(64)
				if idx == 0 {
					i8_t := b.mod.type_store.get_int(8)
					field_type = b.mod.type_store.get_ptr(i8_t)
				} else if idx == 1 || idx == 2 {
					field_type = i64_t
				}
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
			// map[index] as an l-value/r-value base (e.g. map[idx].len, map[idx][0])
			if node.lhs is ast.Ident {
				var_name := node.lhs.name
				if map_has_key_map_bytes(b.var_map_types, var_name) {
					map_ptr := b.vars[var_name]
					key_val := b.expr(node.expr)
					mut value_type := b.mod.type_store.get_int(64)
					if vt := map_get_type_id(b.var_map_value_types, var_name) {
						value_type = vt
					}
					return b.emit_runtime_map_get_ptr(map_ptr, key_val, value_type)
				}
			}
			if node.lhs is ast.SelectorExpr && node.lhs.lhs is ast.Ident {
				field_key := '${node.lhs.lhs.name}.${node.lhs.rhs.name}'
				if map_has_key_map_bytes(b.var_map_types, field_key) {
					map_ptr := b.addr(node.lhs)
					key_val := b.expr(node.expr)
					mut value_type := b.mod.type_store.get_int(64)
					if vt := map_get_type_id(b.var_map_value_types, field_key) {
						value_type = vt
					}
					return b.emit_runtime_map_get_ptr(map_ptr, key_val, value_type)
				}
			}
			// Nested map access (e.g. m['k1']['k2']) where lhs itself is a map expression.
			map_ptr := b.addr(node.lhs)
			if b.is_map_slot(map_ptr) {
				key_val := b.expr(node.expr)
				value_type := b.mod.type_store.get_int(64)
				return b.emit_runtime_map_get_ptr(map_ptr, key_val, value_type)
			}

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
				// If we loaded a pointer to a runtime array/string struct,
				// index into its data field, not into the struct itself.
				if elem_typ.elem_type > 0 && elem_typ.elem_type < b.mod.type_store.types.len {
					loaded_elem_typ_id := elem_typ.elem_type
					loaded_elem_typ := b.mod.type_store.types[loaded_elem_typ_id]
					if loaded_elem_typ.kind == .struct_t {
						struct_val := b.mod.add_instr(.load, b.cur_block, loaded_elem_typ_id,
							[actual_base])
						mut data_ptr_t := TypeID(0)
						is_array_like_loaded := loaded_elem_typ.fields.len >= 6
							&& ('data' in loaded_elem_typ.field_names
							|| 'f_0_data' in loaded_elem_typ.field_names)
							&& ('len' in loaded_elem_typ.field_names
							|| 'f_2_len' in loaded_elem_typ.field_names)
						is_string_like_loaded := loaded_elem_typ.fields.len >= 3
							&& ('str' in loaded_elem_typ.field_names
							|| 'f_0_str' in loaded_elem_typ.field_names)
							&& ('len' in loaded_elem_typ.field_names
							|| 'f_1_len' in loaded_elem_typ.field_names)
						if struct_name := b.struct_name_from_type_id(loaded_elem_typ_id) {
							if struct_name == 'string' || is_string_like_loaded {
								i8_t := b.mod.type_store.get_int(8)
								data_ptr_t = b.mod.type_store.get_ptr(i8_t)
							} else if struct_name == 'array' || is_array_like_loaded {
								mut elem_load_t := b.mod.type_store.get_int(64)
								if vt := b.infer_array_elem_type_from_base_expr(node.lhs) {
									elem_load_t = vt
								}
								data_ptr_t = b.mod.type_store.get_ptr(elem_load_t)
							}
						} else if is_string_like_loaded {
							i8_t := b.mod.type_store.get_int(8)
							data_ptr_t = b.mod.type_store.get_ptr(i8_t)
						} else if is_array_like_loaded {
							mut elem_load_t := b.mod.type_store.get_int(64)
							if vt := b.infer_array_elem_type_from_base_expr(node.lhs) {
								elem_load_t = vt
							}
							data_ptr_t = b.mod.type_store.get_ptr(elem_load_t)
						}
						if loaded_elem_typ.fields.len > 0 {
							if data_ptr_t == 0 {
								data_ptr_t = loaded_elem_typ.fields[0]
							}
						}
						if data_ptr_t == 0 {
							i8_t := b.mod.type_store.get_int(8)
							data_ptr_t = b.mod.type_store.get_ptr(i8_t)
						}
						idx := b.mod.add_value_node(.constant, b.mod.type_store.get_int(32),
							'0', 0)
						actual_base = b.mod.add_instr(.extractvalue, b.cur_block, data_ptr_t,
							[
							struct_val,
							idx,
						])
					}
				}
			} else if elem_typ.kind == .struct_t {
				// Dynamic array or string: load the struct, then extract .data pointer (field 0).
				// Using load(struct_type) + extractvalue survives mem2reg correctly:
				// mem2reg replaces load with stored value (same struct type), then
				// extractvalue extracts .data from either the load result or promoted value.
				struct_val := b.mod.add_instr(.load, b.cur_block, elem_typ_id, [
					base_ptr,
				])
				mut data_ptr_t := TypeID(0)
				is_array_like := elem_typ.fields.len >= 6
					&& ('data' in elem_typ.field_names || 'f_0_data' in elem_typ.field_names)
					&& ('len' in elem_typ.field_names || 'f_2_len' in elem_typ.field_names)
				is_string_like := elem_typ.fields.len >= 3
					&& ('str' in elem_typ.field_names || 'f_0_str' in elem_typ.field_names)
					&& ('len' in elem_typ.field_names || 'f_1_len' in elem_typ.field_names)
				if struct_name := b.struct_name_from_type_id(elem_typ_id) {
					if struct_name == 'string' || is_string_like {
						i8_t := b.mod.type_store.get_int(8)
						data_ptr_t = b.mod.type_store.get_ptr(i8_t)
					} else if struct_name == 'array' || is_array_like {
						mut elem_load_t := b.mod.type_store.get_int(64)
						if vt := b.infer_array_elem_type_from_base_expr(node.lhs) {
							elem_load_t = vt
						}
						data_ptr_t = b.mod.type_store.get_ptr(elem_load_t)
					}
				} else if is_string_like {
					i8_t := b.mod.type_store.get_int(8)
					data_ptr_t = b.mod.type_store.get_ptr(i8_t)
				} else if is_array_like {
					mut elem_load_t := b.mod.type_store.get_int(64)
					if vt := b.infer_array_elem_type_from_base_expr(node.lhs) {
						elem_load_t = vt
					}
					data_ptr_t = b.mod.type_store.get_ptr(elem_load_t)
				}
				if elem_typ.fields.len > 0 {
					if data_ptr_t == 0 {
						data_ptr_t = elem_typ.fields[0]
					}
				}
				if data_ptr_t == 0 {
					i8_t := b.mod.type_store.get_int(8)
					data_ptr_t = b.mod.type_store.get_ptr(i8_t)
				}
				idx := b.mod.add_value_node(.constant, b.mod.type_store.get_int(32), '0',
					0)
				actual_base = b.mod.add_instr(.extractvalue, b.cur_block, data_ptr_t,
					[
					struct_val,
					idx,
				])
			}

			mut gep_type := b.mod.values[actual_base].typ
			if gep_type > 0 && gep_type < b.mod.type_store.types.len {
				base_ptr_info := b.mod.type_store.types[gep_type]
				if base_ptr_info.kind == .ptr_t && base_ptr_info.elem_type > 0
					&& base_ptr_info.elem_type < b.mod.type_store.types.len {
					elem_info := b.mod.type_store.types[base_ptr_info.elem_type]
					if elem_info.kind == .array_t && elem_info.elem_type > 0 {
						gep_type = b.mod.type_store.get_ptr(elem_info.elem_type)
					}
				} else if base_ptr_info.kind == .array_t && base_ptr_info.elem_type > 0 {
					gep_type = b.mod.type_store.get_ptr(base_ptr_info.elem_type)
				}
			}

			// Indexing an array whose element type is a struct needs pointer arithmetic,
			// not struct-field GEP. Otherwise idx=1 becomes `->field1` in C codegen.
			if gep_type > 0 && gep_type < b.mod.type_store.types.len {
				gep_info := b.mod.type_store.types[gep_type]
				if gep_info.kind == .ptr_t && gep_info.elem_type > 0
					&& gep_info.elem_type < b.mod.type_store.types.len {
					elem_info := b.mod.type_store.types[gep_info.elem_type]
					if elem_info.kind == .struct_t {
						i64_t := b.mod.type_store.get_int(64)
						i8_t := b.mod.type_store.get_int(8)
						i8_ptr_t := b.mod.type_store.get_ptr(i8_t)
						base_i8 := b.coerce_value_to_type(actual_base, i8_ptr_t)
						mut idx_i64 := index_val
						if idx_i64 > 0 && idx_i64 < b.mod.values.len {
							if b.mod.values[idx_i64].typ != i64_t {
								idx_i64 = b.coerce_value_to_type(idx_i64, i64_t)
							}
						}
						mut elem_size := b.type_size_from_ssa_type(gep_info.elem_type)
						if elem_size <= 0 {
							elem_size = 8
						}
						size_val := b.mod.add_value_node(.constant, i64_t, elem_size.str(),
							0)
						byte_offset := b.mod.add_instr(.mul, b.cur_block, i64_t, [
							idx_i64,
							size_val,
						])
						elem_i8 := b.mod.add_instr(.add, b.cur_block, i8_ptr_t, [
							base_i8,
							byte_offset,
						])
						return b.mod.add_instr(.bitcast, b.cur_block, gep_type, [
							elem_i8,
						])
					}
				}
			}

			return b.mod.add_instr(.get_element_ptr, b.cur_block, gep_type, [
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
		ast.Ident {
			if v := b.const_global_value(expr.name) {
				return v
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
					lhs << rhs
				}
				.right_shift {
					lhs >> rhs
				}
				.and, .amp {
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
		ast.CallOrCastExpr {
			// Constant casts like u32(0x00FFFFFF)
			return b.eval_const_expr(expr.expr)
		}
		ast.ParenExpr {
			return b.eval_const_expr(expr.expr)
		}
		else {}
	}
	return 0
}

// expr_interface_box creates an interface value with object pointer and vtable-like method pointers.
fn (mut b Builder) expr_interface_box(iface_name string, expr ast.Expr) ValueID {
	iface_type := b.get_struct_type_id(iface_name) or { return b.expr(expr) }
	if iface_type <= 0 || iface_type >= b.mod.type_store.types.len {
		return b.expr(expr)
	}
	i64_t := b.mod.type_store.get_int(64)
	i32_t := b.mod.type_store.get_int(32)
	iface_info := b.mod.type_store.types[iface_type]
	mut iface_val := b.mod.add_value_node(.constant, iface_type, 'undef', 0)

	// Resolve boxed object pointer.
	mut object_ptr := ValueID(0)
	expr_val := b.expr(expr)
	if expr_val > 0 && expr_val < b.mod.values.len {
		expr_typ_id := b.mod.values[expr_val].typ
		if expr_typ_id > 0 && expr_typ_id < b.mod.type_store.types.len {
			expr_typ := b.mod.type_store.types[expr_typ_id]
			if expr_typ.kind == .ptr_t {
				object_ptr = expr_val
			}
		}
	}
	if object_ptr == 0 {
		addr := b.addr(expr)
		if addr > 0 && addr < b.mod.values.len {
			addr_typ_id := b.mod.values[addr].typ
			if addr_typ_id > 0 && addr_typ_id < b.mod.type_store.types.len {
				addr_typ := b.mod.type_store.types[addr_typ_id]
				if addr_typ.kind == .ptr_t && addr_typ.elem_type > 0
					&& addr_typ.elem_type < b.mod.type_store.types.len {
					elem_typ := b.mod.type_store.types[addr_typ.elem_type]
					if elem_typ.kind == .ptr_t {
						object_ptr = b.mod.add_instr(.load, b.cur_block, addr_typ.elem_type,
							[addr])
					} else {
						object_ptr = addr
					}
				}
			}
		}
	}

	object_idx := b.field_index_by_name(iface_type, '_object')
	if object_idx >= 0 && object_idx < iface_info.fields.len && object_ptr != 0 {
		object_field_t := iface_info.fields[object_idx]
		coerced_object := b.coerce_value_to_type(object_ptr, object_field_t)
		object_idx_val := b.mod.add_value_node(.constant, i32_t, object_idx.str(), 0)
		iface_val = b.mod.add_instr(.insertvalue, b.cur_block, iface_type, [iface_val, coerced_object,
			object_idx_val])
	}

	// Store concrete type id when available.
	concrete_name := b.infer_concrete_type(expr)
	mut concrete_type_id := 0
	if concrete_name != 'unknown' {
		if st := b.get_struct_type_id(concrete_name) {
			concrete_type_id = st
		}
	}
	type_idx := b.field_index_by_name(iface_type, '_type_id')
	if type_idx >= 0 && type_idx < iface_info.fields.len {
		type_idx_val := b.mod.add_value_node(.constant, i32_t, type_idx.str(), 0)
		type_const := b.mod.add_value_node(.constant, i64_t, concrete_type_id.str(), 0)
		iface_val = b.mod.add_instr(.insertvalue, b.cur_block, iface_type, [iface_val, type_const,
			type_idx_val])
	}

	// Fill method pointers from concrete implementation when resolvable.
	methods := b.interface_meths[iface_name] or { []string{} }
	for method_name in methods {
		method_idx := b.field_index_by_name(iface_type, method_name)
		if method_idx < 0 || method_idx >= iface_info.fields.len {
			continue
		}
		fn_name := b.resolve_method_fn_name(concrete_name, method_name)
		if fn_name == '' {
			continue
		}
		method_field_t := iface_info.fields[method_idx]
		method_idx_val := b.mod.add_value_node(.constant, i32_t, method_idx.str(), 0)
		fn_val := b.mod.add_value_node(.func_ref, method_field_t, fn_name, 0)
		iface_val = b.mod.add_instr(.insertvalue, b.cur_block, iface_type, [iface_val, fn_val,
			method_idx_val])
	}

	return iface_val
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
		ast.PrefixExpr {
			return b.infer_concrete_type(expr.expr)
		}
		ast.CallOrCastExpr {
			if expr.lhs is ast.Ident {
				if map_has_key_type_id(b.struct_types, expr.lhs.name) {
					return expr.lhs.name
				}
			}
			return b.infer_concrete_type(expr.expr)
		}
		else {}
	}
	return 'unknown'
}
