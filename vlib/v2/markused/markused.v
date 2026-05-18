// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module markused

import v2.ast
import v2.token
import v2.types

// Guard functions for ARM64 backend where default-initialized sum types have NULL data pointers.
fn expr_ok(expr ast.Expr) bool {
	return unsafe { (&u64(&expr))[1] } != 0
}

fn stmt_ok(stmt ast.Stmt) bool {
	return unsafe { (&u64(&stmt))[1] } != 0
}

const builtin_cast_type_names = [
	'bool',
	'byte',
	'byteptr',
	'char',
	'charptr',
	'f32',
	'f64',
	'i8',
	'i16',
	'i32',
	'i64',
	'int',
	'isize',
	'rune',
	'u8',
	'u16',
	'u32',
	'u64',
	'usize',
	'voidptr',
]

struct FnInfo {
	key  string
	mod  string
	decl ast.FnDecl
}

struct Walker {
	files []ast.File
	env   &types.Environment = unsafe { nil }
mut:
	fns   []FnInfo
	queue []int

	used_keys map[string]bool

	module_names           map[string]bool
	module_alias_to_real   map[string]string
	type_names             map[string]bool
	interface_type_names   map[string]bool
	interface_method_names map[string][]string
	methods_by_receiver    map[string][]int
	struct_field_receivers map[string][]string

	lookup map[string][]int

	cur_fn_scope &types.Scope = unsafe { nil }
	cur_fn_decl  ast.FnDecl
}

// mark_used walks reachable function/method bodies and returns declaration keys
// for the functions that are used at least once.
pub fn mark_used(files []ast.File, env &types.Environment) map[string]bool {
	mut w := new_walker(files, env)
	return w.walk()
}

// decl_key computes the stable key used by mark_used for a function declaration.
pub fn decl_key(module_name string, decl ast.FnDecl, env &types.Environment) string {
	mod_name := normalize_module_name(module_name)
	if is_method_decl(decl) {
		receiver := receiver_primary_name(mod_name, decl, env)
		return '${mod_name}|m|${receiver}|${normalize_method_name(decl.name)}'
	}
	return '${mod_name}|f|${decl.name}'
}

fn new_walker(files []ast.File, env &types.Environment) Walker {
	return Walker{
		files:                  files
		env:                    unsafe { env }
		used_keys:              map[string]bool{}
		module_names:           map[string]bool{}
		type_names:             map[string]bool{}
		interface_type_names:   map[string]bool{}
		interface_method_names: map[string][]string{}
		methods_by_receiver:    map[string][]int{}
		struct_field_receivers: map[string][]string{}
		lookup:                 map[string][]int{}
	}
}

fn (mut w Walker) walk() map[string]bool {
	w.collect_defs()
	if w.fns.len == 0 {
		return map[string]bool{}
	}
	if !w.seed_roots() {
		mut all := map[string]bool{}
		for info in w.fns {
			all[info.key] = true
		}
		return all
	}
	mut qi := 0
	for qi < w.queue.len {
		idx := w.queue[qi]
		qi++
		info := w.fns[idx]
		w.cur_fn_scope = w.lookup_fn_scope(info)
		w.cur_fn_decl = info.decl
		w.walk_stmts(info.decl.stmts, info.mod)
		w.cur_fn_scope = unsafe { nil }
	}
	return w.used_keys
}

fn (mut w Walker) collect_defs() {
	for file in w.files {
		mod_name := normalize_module_name(file.mod)
		w.add_module_name(mod_name)
		for imp in file.imports {
			w.add_module_name(imp.name)
			if imp.alias != '' {
				w.add_module_name(imp.alias)
				// Map alias to real module name for correct symbol resolution.
				// e.g., 'import v2.ssa.optimize as ssa_optimize' → 'ssa_optimize' → 'optimize'
				real_name := if imp.name.contains('.') {
					imp.name.all_after_last('.')
				} else {
					imp.name
				}
				w.module_alias_to_real[imp.alias] = real_name
			}
		}
		for stmt in file.stmts {
			if !stmt_ok(stmt) {
				continue
			}
			match stmt {
				ast.StructDecl {
					w.add_type_name(mod_name, stmt.name)
					w.add_struct_field_receivers(mod_name, stmt)
				}
				ast.EnumDecl {
					w.add_type_name(mod_name, stmt.name)
				}
				ast.InterfaceDecl {
					w.add_type_name(mod_name, stmt.name)
					w.add_interface_decl(mod_name, stmt)
				}
				ast.TypeDecl {
					w.add_type_name(mod_name, stmt.name)
				}
				ast.FnDecl {
					if stmt.name == '' {
						continue
					}
					info := FnInfo{
						key:  decl_key(mod_name, stmt, w.env)
						mod:  mod_name
						decl: stmt
					}
					w.fns << info
					idx := w.fns.len - 1
					w.index_fn(idx, info)
				}
				else {}
			}
		}
	}
}

fn (mut w Walker) seed_roots() bool {
	mut has_root := false
	for i, info in w.fns {
		if is_main_root(info) {
			w.mark_fn(i)
			has_root = true
		}
	}
	if has_root {
		w.seed_generic_specialization_roots()
		// Also seed module init() functions (called from synthesized main)
		for i, info in w.fns {
			if is_module_init(info) {
				w.mark_fn(i)
			}
		}
		return true
	}
	for i, info in w.fns {
		if is_test_root(info) || is_module_init(info) {
			w.mark_fn(i)
			has_root = true
		}
	}
	if has_root {
		w.seed_generic_specialization_roots()
	}
	return has_root
}

fn generic_base_name_from_key(key string) string {
	mut name := key
	bracket_idx := name.index_u8(`[`)
	if bracket_idx > 0 {
		name = name[..bracket_idx]
	}
	dot_idx := name.last_index_u8(`.`)
	if dot_idx > 0 && dot_idx < name.len - 1 {
		name = name[dot_idx + 1..]
	}
	return strip_generic_specialization_suffix(name)
}

fn (mut w Walker) seed_generic_specialization_roots() {
	if w.env == unsafe { nil } {
		return
	}
	for key, _ in w.env.generic_types {
		base_name := generic_base_name_from_key(key)
		if base_name == '' {
			continue
		}
		w.mark_lookup('fn:${base_name}')
		w.mark_lookup('mname:${base_name}')
	}
}

fn is_main_root(info FnInfo) bool {
	return !is_method_decl(info.decl) && info.mod == 'main' && info.decl.name == 'main'
}

fn is_test_root(info FnInfo) bool {
	return !is_method_decl(info.decl) && info.decl.name.starts_with('test_')
		&& info.decl.typ.params.len == 0
}

fn is_method_decl(decl ast.FnDecl) bool {
	return decl.is_method || decl.is_static
}

fn is_module_init(info FnInfo) bool {
	return !is_method_decl(info.decl) && (info.decl.name == 'init'
		|| info.decl.name == 'deinit'
		|| info.decl.name.starts_with('__v_init_consts_'))
}

fn normalize_module_name(module_name string) string {
	if module_name == '' {
		return 'main'
	}
	return module_name
}

fn sanitize_receiver_name(name string) string {
	mut out := name.trim_space()
	for out.len > 0 && (out[0] == `&` || out[0] == `?` || out[0] == `!`) {
		out = out[1..]
	}
	return out
}

fn maybe_trim_module_prefix(mod_name string, name string) string {
	if mod_name == '' || mod_name == 'main' {
		return name
	}
	prefix := '${mod_name}__'
	if name.starts_with(prefix) {
		return name[prefix.len..]
	}
	return name
}

fn add_unique_string(mut out []string, value string) {
	if value == '' {
		return
	}
	for existing in out {
		if existing == value {
			return
		}
	}
	out << value
}

fn add_unique_int(mut out []int, value int) {
	for existing in out {
		if existing == value {
			return
		}
	}
	out << value
}

fn add_unique_fn_name(mut out []string, value string) {
	if value == '' {
		return
	}
	for existing in out {
		if existing == value {
			return
		}
	}
	out << value
}

fn type_name_candidates_from_type(mod_name string, typ types.Type) []string {
	mut out := []string{}
	names := [typ.name(), typ.base_type().name()]
	for raw_name in names {
		name := sanitize_receiver_name(raw_name)
		add_unique_string(mut out, name)
		add_unique_string(mut out, maybe_trim_module_prefix(mod_name, name))
		if name.contains('__') {
			add_unique_string(mut out, name.all_after_last('__'))
		} else if mod_name != '' && mod_name != 'main' {
			add_unique_string(mut out, '${mod_name}__${name}')
		}
		// For array types like []Attribute, also add Array_Attribute form.
		if name.starts_with('[]') {
			elem := name[2..]
			add_unique_string(mut out, 'Array_${elem}')
			add_unique_string(mut out, maybe_trim_module_prefix(mod_name, 'Array_${elem}'))
			if mod_name != '' && mod_name != 'main' {
				add_unique_string(mut out, 'Array_${mod_name}__${elem}')
			}
		}
	}
	return out
}

fn add_receiver_name_candidates(mut out []string, mod_name string, raw_name string) {
	name := sanitize_receiver_name(raw_name)
	if name == '' {
		return
	}
	add_unique_string(mut out, name)
	add_unique_string(mut out, maybe_trim_module_prefix(mod_name, name))
	if name.contains('__') {
		add_unique_string(mut out, name.all_after_last('__'))
	} else if mod_name != '' && mod_name != 'main' {
		add_unique_string(mut out, '${mod_name}__${name}')
	}
}

fn receiver_type_expr_name(expr ast.Expr) string {
	match expr {
		ast.Ident {
			return expr.name
		}
		ast.SelectorExpr {
			return expr.rhs.name
		}
		ast.PrefixExpr {
			if expr.op == .amp {
				return receiver_type_expr_name(expr.expr)
			}
		}
		ast.ModifierExpr {
			return receiver_type_expr_name(expr.expr)
		}
		ast.GenericArgs {
			return receiver_type_expr_name(expr.lhs)
		}
		ast.GenericArgOrIndexExpr {
			return receiver_type_expr_name(expr.lhs)
		}
		ast.Type {
			match expr {
				ast.PointerType {
					return receiver_type_expr_name(expr.base_type)
				}
				ast.GenericType {
					return receiver_type_expr_name(expr.name)
				}
				else {}
			}
		}
		else {}
	}

	return ''
}

fn type_expr_receiver_candidates(mod_name string, expr ast.Expr) []string {
	mut out := []string{}
	match expr {
		ast.SelectorExpr {
			if expr.lhs is ast.Ident {
				add_receiver_name_candidates(mut out, mod_name, expr.rhs.name)
				add_unique_string(mut out, '${expr.lhs.name}__${expr.rhs.name}')
			}
		}
		ast.PrefixExpr {
			if expr.op == .amp {
				return type_expr_receiver_candidates(mod_name, expr.expr)
			}
		}
		ast.ModifierExpr {
			return type_expr_receiver_candidates(mod_name, expr.expr)
		}
		ast.GenericArgs {
			return type_expr_receiver_candidates(mod_name, expr.lhs)
		}
		ast.GenericArgOrIndexExpr {
			return type_expr_receiver_candidates(mod_name, expr.lhs)
		}
		ast.Type {
			match expr {
				ast.PointerType {
					return type_expr_receiver_candidates(mod_name, expr.base_type)
				}
				ast.GenericType {
					return type_expr_receiver_candidates(mod_name, expr.name)
				}
				else {}
			}
		}
		else {}
	}

	name := receiver_type_expr_name(expr)
	if name != '' {
		add_receiver_name_candidates(mut out, mod_name, name)
	}
	return out
}

fn receiver_names_from_decl(mod_name string, decl ast.FnDecl, env &types.Environment) []string {
	mut out := []string{}
	add_receiver_name_candidates(mut out, mod_name, receiver_type_expr_name(decl.receiver.typ))
	match decl.receiver.typ {
		ast.Ident {
			name := sanitize_receiver_name(decl.receiver.typ.name)
			add_unique_string(mut out, name)
			add_unique_string(mut out, maybe_trim_module_prefix(mod_name, name))
			if mod_name != '' && mod_name != 'main' {
				add_unique_string(mut out, '${mod_name}__${name}')
			}
		}
		ast.PrefixExpr {
			if decl.receiver.typ.expr is ast.Ident {
				name := sanitize_receiver_name(decl.receiver.typ.expr.name)
				add_unique_string(mut out, name)
				add_unique_string(mut out, maybe_trim_module_prefix(mod_name, name))
				if mod_name != '' && mod_name != 'main' {
					add_unique_string(mut out, '${mod_name}__${name}')
				}
			}
		}
		else {}
	}

	// Method on []ElemType — receiver name is Array_ElemType.
	// Use the string name which includes [] prefix for array types.
	recv_name := decl.receiver.typ.name()
	if recv_name.starts_with('[]') {
		elem_name := recv_name[2..]
		if elem_name.len > 0 {
			arr_name := 'Array_${elem_name}'
			add_unique_string(mut out, arr_name)
			add_unique_string(mut out, maybe_trim_module_prefix(mod_name, arr_name))
			if mod_name != '' && mod_name != 'main' {
				add_unique_string(mut out, 'Array_${mod_name}__${elem_name}')
			}
		}
	}
	pos := decl.receiver.typ.pos()
	if env != unsafe { nil } && pos.is_valid() {
		if receiver_type := env.get_expr_type(pos.id) {
			for name in type_name_candidates_from_type(mod_name, receiver_type) {
				add_unique_string(mut out, name)
			}
		}
	}
	return out
}

fn receiver_primary_name(mod_name string, decl ast.FnDecl, env &types.Environment) string {
	names := receiver_names_from_decl(mod_name, decl, env)
	if names.len > 0 {
		return names[0]
	}
	return 'unknown'
}

fn normalize_method_name(name string) string {
	return match name {
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
		else { name }
	}
}

fn (w &Walker) lookup_fn_scope(info FnInfo) &types.Scope {
	if w.env == unsafe { nil } {
		return unsafe { nil }
	}
	decl := info.decl
	mod_name := info.mod
	if !is_method_decl(decl) {
		if scope := w.env.get_fn_scope(mod_name, decl.name) {
			return scope
		}
		return unsafe { nil }
	}
	receivers := receiver_names_from_decl(mod_name, decl, w.env)
	for recv in receivers {
		if scope := w.env.get_fn_scope(mod_name, '${recv}__${decl.name}') {
			return scope
		}
	}
	return unsafe { nil }
}

fn (mut w Walker) add_lookup(key string, idx int) {
	if key == '' {
		return
	}
	if key !in w.lookup {
		w.lookup[key] = [idx]
		return
	}
	mut values := w.lookup[key]
	add_unique_int(mut values, idx)
	w.lookup[key] = values
}

fn (mut w Walker) index_fn(idx int, info FnInfo) {
	decl := info.decl
	mod_name := info.mod
	if is_method_decl(decl) {
		mut method_names := []string{}
		add_unique_string(mut method_names, decl.name)
		add_unique_string(mut method_names, normalize_method_name(decl.name))
		receivers := receiver_names_from_decl(mod_name, decl, w.env)
		for method_name in method_names {
			w.add_lookup('mname:${method_name}', idx)
			for receiver_name in receivers {
				w.add_method_receiver(receiver_name, idx)
				if receiver_name.contains('__') {
					w.add_method_receiver(receiver_name.all_after_last('__'), idx)
				}
				w.add_lookup('meth:${receiver_name}:${method_name}', idx)
				w.add_lookup('fn:${receiver_name}__${method_name}', idx)
				w.add_lookup('mod:${mod_name}:${receiver_name}__${method_name}', idx)
			}
		}
		return
	}
	w.add_lookup('fn:${decl.name}', idx)
	w.add_lookup('mod:${mod_name}:${decl.name}', idx)
	if mod_name != '' && mod_name != 'main' && mod_name != 'builtin' {
		w.add_lookup('fn:${mod_name}__${decl.name}', idx)
	}
}

fn (mut w Walker) add_module_name(name string) {
	if name == '' {
		return
	}
	w.module_names[name] = true
	if name.contains('.') {
		w.module_names[name.all_after_last('.')] = true
	}
}

fn (mut w Walker) add_type_name(mod_name string, name string) {
	if name == '' {
		return
	}
	w.type_names[name] = true
	trimmed := maybe_trim_module_prefix(mod_name, name)
	w.type_names[trimmed] = true
	if mod_name != '' && mod_name != 'main' {
		w.type_names['${mod_name}__${trimmed}'] = true
	}
}

fn (mut w Walker) add_interface_name(mod_name string, name string) {
	if name == '' {
		return
	}
	w.interface_type_names[name] = true
	trimmed := maybe_trim_module_prefix(mod_name, name)
	w.interface_type_names[trimmed] = true
	if mod_name != '' && mod_name != 'main' {
		w.interface_type_names['${mod_name}__${trimmed}'] = true
	}
}

fn (mut w Walker) add_interface_decl(mod_name string, decl ast.InterfaceDecl) {
	w.add_interface_name(mod_name, decl.name)
	mut method_names := []string{}
	for field in decl.fields {
		if field.typ is ast.Type && field.typ is ast.FnType {
			add_unique_fn_name(mut method_names, field.name)
			add_unique_fn_name(mut method_names, normalize_method_name(field.name))
		}
	}
	if method_names.len == 0 {
		return
	}
	mut iface_names := []string{}
	add_unique_string(mut iface_names, decl.name)
	add_unique_string(mut iface_names, maybe_trim_module_prefix(mod_name, decl.name))
	if mod_name != '' && mod_name != 'main' {
		trimmed := maybe_trim_module_prefix(mod_name, decl.name)
		add_unique_string(mut iface_names, '${mod_name}__${trimmed}')
	}
	for iface_name in iface_names {
		if iface_name in w.interface_method_names {
			mut existing := w.interface_method_names[iface_name]
			for method_name in method_names {
				add_unique_fn_name(mut existing, method_name)
			}
			w.interface_method_names[iface_name] = existing
		} else {
			w.interface_method_names[iface_name] = method_names.clone()
		}
	}
}

fn (mut w Walker) add_struct_field_receivers(mod_name string, decl ast.StructDecl) {
	mut struct_names := []string{}
	add_receiver_name_candidates(mut struct_names, mod_name, decl.name)
	for field in decl.fields {
		field_receivers := type_expr_receiver_candidates(mod_name, field.typ)
		if field_receivers.len == 0 {
			continue
		}
		for struct_name in struct_names {
			key := '${struct_name}:${field.name}'
			mut existing := w.struct_field_receivers[key] or { []string{} }
			for receiver in field_receivers {
				add_unique_string(mut existing, receiver)
			}
			w.struct_field_receivers[key] = existing
		}
	}
}

fn (mut w Walker) add_method_receiver(receiver_name string, idx int) {
	if receiver_name == '' {
		return
	}
	if receiver_name !in w.methods_by_receiver {
		w.methods_by_receiver[receiver_name] = [idx]
		return
	}
	mut values := w.methods_by_receiver[receiver_name]
	add_unique_int(mut values, idx)
	w.methods_by_receiver[receiver_name] = values
}

fn (mut w Walker) mark_fn(idx int) {
	if idx < 0 || idx >= w.fns.len {
		return
	}
	info := w.fns[idx]
	key := info.key
	if key in w.used_keys {
		return
	}
	w.used_keys[key] = true
	w.queue << idx
	w.mark_generic_binding_receiver_methods(info)
}

fn (mut w Walker) mark_generic_binding_receiver_methods(info FnInfo) {
	if w.env == unsafe { nil } || info.decl.typ.generic_params.len == 0 {
		return
	}
	for key, bindings_list in w.env.generic_types {
		if generic_base_name_from_key(key) != info.decl.name {
			continue
		}
		for bindings in bindings_list {
			for _, concrete in bindings {
				receivers := type_name_candidates_from_type(info.mod, concrete)
				w.mark_all_methods_for_receivers(receivers)
			}
		}
	}
}

fn (w &Walker) lookup_count(key string) int {
	if key in w.lookup {
		return w.lookup[key].len
	}
	return 0
}

fn (mut w Walker) mark_lookup(key string) {
	if key in w.lookup {
		for idx in w.lookup[key] {
			w.mark_fn(idx)
		}
	}
}

fn called_fn_name_candidates(name string) []string {
	mut out := []string{}
	add_unique_string(mut out, name)
	generic_base := strip_generic_specialization_suffix(name)
	if generic_base != name {
		add_unique_string(mut out, generic_base)
	}
	if name == 'builtin__new_array_from_c_array_noscan' {
		add_unique_string(mut out, 'new_array_from_c_array')
		return out
	}
	if name == 'builtin__array_push_noscan' {
		add_unique_string(mut out, 'array__push')
		return out
	}
	if name.starts_with('builtin__') {
		base := name['builtin__'.len..]
		add_unique_string(mut out, base)
	}
	if name == 'gen_v__new_gen' {
		add_unique_string(mut out, 'v__new_gen')
	}
	if name.starts_with('strings__Builder__') {
		method_name := name.all_after_last('__')
		add_unique_string(mut out, 'array__${method_name}')
	}
	return out
}

fn strip_generic_specialization_suffix(name string) string {
	if idx := name.index('_T_') {
		if idx > 0 {
			return name[..idx]
		}
	}
	return name
}

fn should_mark_ident_as_fn(name string) bool {
	if name == '' {
		return false
	}
	return name.starts_with('map_') || name.starts_with('map__') || name.starts_with('new_map')
		|| name.starts_with('array__') || name.starts_with('IError_') || name.starts_with('Error__')
		|| name.starts_with('MessageError__') || name.starts_with('_option_')
		|| name.starts_with('_result_')
}

fn (mut w Walker) mark_fn_name(name string, mod_name string) {
	if name == '' {
		return
	}
	for candidate in called_fn_name_candidates(name) {
		w.mark_lookup('mod:${mod_name}:${candidate}')
		w.mark_lookup('fn:${candidate}')
		if !candidate.contains('__') && mod_name != '' && mod_name != 'main'
			&& mod_name != 'builtin' {
			w.mark_lookup('fn:${mod_name}__${candidate}')
		}
	}
}

fn (mut w Walker) mark_method_name(name string, receivers []string) {
	normalized := normalize_method_name(name)
	for receiver in receivers {
		for candidate in receiver_lookup_candidates(receiver) {
			w.mark_lookup('meth:${candidate}:${name}')
			if normalized != name {
				w.mark_lookup('meth:${candidate}:${normalized}')
			}
		}
	}
}

fn receiver_lookup_candidates(receiver string) []string {
	mut out := []string{}
	add_unique_string(mut out, receiver)
	if receiver.contains('__') {
		add_unique_string(mut out, receiver.all_after_last('__'))
	}
	if receiver.starts_with('Array_') || receiver.starts_with('Array_fixed_') {
		add_unique_string(mut out, 'array')
	}
	if receiver.starts_with('Map_') {
		add_unique_string(mut out, 'map')
	}
	if receiver.starts_with('_option_') {
		add_unique_string(mut out, '_option')
	}
	if receiver.starts_with('_result_') {
		add_unique_string(mut out, '_result')
	}
	return out
}

fn (mut w Walker) mark_method_name_fallback(name string) {
	normalized := normalize_method_name(name)
	// Check count first to avoid pulling in everything for common names.
	count := w.lookup_count('mname:${name}') +
		if normalized != name { w.lookup_count('mname:${normalized}') } else { 0 }
	if count == 0 || count > 64 {
		return
	}
	w.mark_lookup('mname:${name}')
	if normalized != name {
		w.mark_lookup('mname:${normalized}')
	}
}

fn infix_operator_may_use_method(op token.Token) bool {
	return op in [.plus, .minus, .mul, .div, .mod, .eq, .ne, .lt, .gt, .le, .ge, .pipe, .xor]
}

fn (mut w Walker) mark_infix_operator_method(expr ast.InfixExpr, mod_name string) {
	if !infix_operator_may_use_method(expr.op) {
		return
	}
	method_name := expr.op.str()
	receivers := w.receiver_candidates_for_expr(expr.lhs, mod_name)
	if receivers.len > 0 {
		w.mark_method_name(method_name, receivers)
		return
	}
	w.mark_method_name_fallback(method_name)
}

fn (mut w Walker) mark_all_methods_for_receivers(receivers []string) {
	for receiver in receivers {
		if receiver in w.methods_by_receiver {
			for idx in w.methods_by_receiver[receiver] {
				w.mark_fn(idx)
			}
		}
		if receiver.contains('__') {
			short_name := receiver.all_after_last('__')
			if short_name in w.methods_by_receiver {
				for idx in w.methods_by_receiver[short_name] {
					w.mark_fn(idx)
				}
			}
		}
	}
}

fn ierror_wrapper_base_from_ident(name string) string {
	if !name.starts_with('IError_') {
		return ''
	}
	prefix_len := 'IError_'.len
	if name.ends_with('_type_name_wrapper') {
		return name[prefix_len..name.len - '_type_name_wrapper'.len]
	}
	if name.ends_with('_msg_wrapper') {
		return name[prefix_len..name.len - '_msg_wrapper'.len]
	}
	if name.ends_with('_code_wrapper') {
		return name[prefix_len..name.len - '_code_wrapper'.len]
	}
	return ''
}

fn (mut w Walker) mark_ierror_wrapper_dependencies(name string, mod_name string) {
	base := ierror_wrapper_base_from_ident(name)
	if base == '' {
		return
	}
	w.mark_fn_name('${base}__msg', mod_name)
	w.mark_fn_name('${base}__code', mod_name)
	w.mark_fn_name('Error__code', mod_name)
}

fn (w &Walker) interface_name_from_expr(expr ast.Expr, mod_name string) string {
	match expr {
		ast.Ident {
			name := sanitize_receiver_name(expr.name)
			if name in w.interface_type_names {
				return name
			}
			if mod_name != '' && mod_name != 'main' {
				qualified := '${mod_name}__${name}'
				if qualified in w.interface_type_names {
					return qualified
				}
			}
		}
		ast.SelectorExpr {
			if expr.lhs is ast.Ident {
				mut candidates := []string{}
				add_unique_string(mut candidates, expr.rhs.name)
				add_unique_string(mut candidates, '${expr.lhs.name}__${expr.rhs.name}')
				if mod_name != '' && mod_name != 'main' {
					add_unique_string(mut candidates, '${mod_name}__${expr.rhs.name}')
				}
				for candidate in candidates {
					if candidate in w.interface_type_names {
						return candidate
					}
				}
			}
		}
		else {}
	}

	return ''
}

fn (mut w Walker) mark_interface_conversion_methods(target_expr ast.Expr, value_expr ast.Expr, mod_name string) {
	iface_name := w.interface_name_from_expr(target_expr, mod_name)
	if iface_name == '' {
		return
	}
	w.mark_interface_conversion_methods_for_name(iface_name, value_expr, mod_name)
}

fn (mut w Walker) mark_interface_conversion_methods_for_name(iface_name string, value_expr ast.Expr, mod_name string) {
	receivers := w.receiver_candidates_for_expr(value_expr, mod_name)
	if receivers.len == 0 {
		return
	}
	mut marked := false
	if iface_name in w.interface_method_names {
		prev_len := w.queue.len
		for method_name in w.interface_method_names[iface_name] {
			w.mark_method_name(method_name, receivers)
		}
		marked = w.queue.len > prev_len
	}
	// Interface wrappers generated by the C backend contain one wrapper for
	// every method in the concrete type's interface table. If the frontend
	// records only a subset of the interface field methods, the partial mark
	// above is not enough: the linker still needs the remaining concrete
	// methods referenced by wrappers. Marking all methods on the converted
	// receiver is conservative and keeps implicit interface conversions
	// link-complete.
	_ = marked
	w.mark_all_methods_for_receivers(receivers)
}

fn (mut w Walker) mark_call_arg_interface_conversions(expr ast.CallExpr, mod_name string) {
	mut marked := false
	if w.env == unsafe { nil } {
		w.mark_call_arg_interface_conversions_from_decls(expr.lhs, expr.args, mod_name)
		return
	}
	if lhs_type := w.call_lhs_fn_type(expr.lhs, mod_name) {
		param_types := lhs_type.get_param_types()
		param_offset := if expr.lhs is ast.SelectorExpr { 1 } else { 0 }
		for i, arg in expr.args {
			param_idx := i + param_offset
			if param_idx >= param_types.len {
				break
			}
			param_type := param_types[param_idx]
			if param_type is types.Interface {
				w.mark_interface_conversion_methods_for_name((param_type as types.Interface).name,
					arg, mod_name)
				marked = true
			}
		}
	}
	if !marked {
		w.mark_call_arg_interface_conversions_from_decls(expr.lhs, expr.args, mod_name)
	}
}

fn (mut w Walker) mark_call_or_cast_arg_interface_conversion(expr ast.CallOrCastExpr, mod_name string) {
	mut marked := false
	if w.env == unsafe { nil } {
		w.mark_call_arg_interface_conversions_from_decls(expr.lhs, [expr.expr], mod_name)
		return
	}
	if lhs_type := w.call_lhs_fn_type(expr.lhs, mod_name) {
		param_types := lhs_type.get_param_types()
		param_offset := if expr.lhs is ast.SelectorExpr { 1 } else { 0 }
		if param_offset < param_types.len {
			param_type := param_types[param_offset]
			if param_type is types.Interface {
				w.mark_interface_conversion_methods_for_name((param_type as types.Interface).name,
					expr.expr, mod_name)
				marked = true
			}
		}
	}
	if !marked {
		w.mark_call_arg_interface_conversions_from_decls(expr.lhs, [expr.expr], mod_name)
	}
}

fn (w &Walker) add_lookup_indices(key string, mut out []int) {
	if key in w.lookup {
		for idx in w.lookup[key] {
			add_unique_int(mut out, idx)
		}
	}
}

fn (w &Walker) add_fn_name_indices(name string, mod_name string, mut out []int) {
	if name == '' {
		return
	}
	for candidate in called_fn_name_candidates(name) {
		w.add_lookup_indices('mod:${mod_name}:${candidate}', mut out)
		w.add_lookup_indices('fn:${candidate}', mut out)
		if !candidate.contains('__') && mod_name != '' && mod_name != 'main'
			&& mod_name != 'builtin' {
			w.add_lookup_indices('fn:${mod_name}__${candidate}', mut out)
		}
	}
}

fn (w &Walker) add_method_name_indices(name string, receivers []string, mut out []int) {
	normalized := normalize_method_name(name)
	for receiver in receivers {
		for candidate in receiver_lookup_candidates(receiver) {
			w.add_lookup_indices('meth:${candidate}:${name}', mut out)
			if normalized != name {
				w.add_lookup_indices('meth:${candidate}:${normalized}', mut out)
			}
		}
	}
}

fn (w &Walker) call_lhs_decl_indices(lhs ast.Expr, mod_name string) []int {
	mut out := []int{}
	match lhs {
		ast.Ident {
			w.add_fn_name_indices(lhs.name, mod_name, mut out)
		}
		ast.GenericArgs {
			return w.call_lhs_decl_indices(lhs.lhs, mod_name)
		}
		ast.GenericArgOrIndexExpr {
			return w.call_lhs_decl_indices(lhs.lhs, mod_name)
		}
		ast.SelectorExpr {
			method_name := lhs.rhs.name
			if lhs.lhs is ast.Ident {
				left_name := lhs.lhs.name
				if left_name == 'C' {
					w.add_fn_name_indices(method_name, mod_name, mut out)
					return out
				}
				if left_name in w.module_names {
					real_mod := w.module_alias_to_real[left_name] or { left_name }
					w.add_fn_name_indices('${real_mod}__${method_name}', mod_name, mut out)
					return out
				}
				if left_name in w.type_names {
					w.add_method_name_indices(method_name,
						[left_name, '${mod_name}__${left_name}'], mut out)
					return out
				}
			}
			if lhs.lhs is ast.SelectorExpr {
				type_sel := lhs.lhs as ast.SelectorExpr
				if type_sel.lhs is ast.Ident {
					mod_ident := type_sel.lhs.name
					type_name := type_sel.rhs.name
					if mod_ident in w.module_names || mod_ident in w.type_names {
						candidates := [
							type_name,
							'${mod_name}__${type_name}',
							'${mod_ident}__${type_name}',
						]
						w.add_method_name_indices(method_name, candidates, mut out)
						return out
					}
				}
			}
			receivers := w.receiver_candidates_for_expr(lhs.lhs, mod_name)
			if receivers.len > 0 {
				w.add_method_name_indices(method_name, receivers, mut out)
				if out.len > 0 {
					return out
				}
			}
			normalized := normalize_method_name(method_name)
			w.add_lookup_indices('mname:${method_name}', mut out)
			if normalized != method_name {
				w.add_lookup_indices('mname:${normalized}', mut out)
			}
		}
		else {}
	}

	return out
}

fn (mut w Walker) mark_call_arg_interface_conversions_from_decls(lhs ast.Expr, args []ast.Expr, mod_name string) {
	decl_indices := w.call_lhs_decl_indices(lhs, mod_name)
	if decl_indices.len == 0 {
		return
	}
	for idx in decl_indices {
		if idx < 0 || idx >= w.fns.len {
			continue
		}
		info := w.fns[idx]
		params := info.decl.typ.params
		arg_offset := if lhs is ast.Ident && info.decl.is_method && !info.decl.is_static {
			1
		} else {
			0
		}
		for i, param in params {
			arg_idx := i + arg_offset
			if arg_idx >= args.len {
				break
			}
			iface_name := w.interface_name_from_expr(param.typ, info.mod)
			if iface_name == '' {
				continue
			}
			w.mark_interface_conversion_methods_for_name(iface_name, args[arg_idx], mod_name)
		}
	}
}

fn (mut w Walker) mark_string_interpolation_str_dependency(expr ast.Expr, mod_name string) {
	receivers := w.receiver_candidates_for_expr(expr, mod_name)
	if receivers.len == 0 {
		return
	}
	w.mark_method_name('str', receivers)
	for receiver in receivers {
		w.mark_fn_name('${receiver}__str', mod_name)
	}
}

fn (mut w Walker) mark_result_error_return_methods(return_type ast.Expr, expr ast.Expr, mod_name string) {
	if return_type is ast.Type && return_type is ast.ResultType {
		receivers := w.receiver_candidates_for_expr(expr, mod_name)
		if receivers.len == 0 {
			return
		}
		w.mark_method_name('msg', receivers)
		w.mark_method_name('code', receivers)
	}
}

fn (w &Walker) call_lhs_fn_type(lhs ast.Expr, mod_name string) ?types.FnType {
	if w.env != unsafe { nil } {
		if lhs_type := w.env.get_expr_type(lhs.pos().id) {
			if lhs_type is types.FnType {
				return lhs_type as types.FnType
			}
		}
		if lhs is ast.Ident {
			if fn_type := w.env.lookup_fn(mod_name, lhs.name) {
				return fn_type
			}
		}
		if lhs is ast.SelectorExpr {
			receivers := w.receiver_candidates_for_expr(lhs.lhs, mod_name)
			for receiver in receivers {
				if fn_type := w.env.lookup_method(receiver, lhs.rhs.name) {
					return fn_type
				}
			}
		}
	}
	if lhs is ast.Ident && w.cur_fn_scope != unsafe { nil } {
		if obj := w.cur_fn_scope.lookup_parent(lhs.name, 0) {
			typ := obj.typ()
			if typ is types.FnType {
				return typ as types.FnType
			}
		}
	}
	return none
}

fn (w &Walker) current_param_receiver_candidates(name string, mod_name string) []string {
	mut out := []string{}
	if name == '' || w.cur_fn_decl.name == '' {
		return out
	}
	for param in w.cur_fn_decl.typ.params {
		if param.name != name {
			continue
		}
		for receiver in type_expr_receiver_candidates(mod_name, param.typ) {
			add_unique_string(mut out, receiver)
		}
		generic_name := receiver_type_expr_name(param.typ)
		if generic_name == '' || w.env == unsafe { nil } {
			continue
		}
		for bindings in w.current_fn_generic_bindings() {
			if concrete := bindings[generic_name] {
				for receiver in type_name_candidates_from_type(mod_name, concrete) {
					add_unique_string(mut out, receiver)
				}
			}
		}
	}
	return out
}

fn (w &Walker) current_receiver_candidates(name string, mod_name string) []string {
	mut out := []string{}
	if name == '' || w.cur_fn_decl.name == '' || !is_method_decl(w.cur_fn_decl)
		|| w.cur_fn_decl.receiver.name != name {
		return out
	}
	for receiver in type_expr_receiver_candidates(mod_name, w.cur_fn_decl.receiver.typ) {
		add_unique_string(mut out, receiver)
	}
	generic_name := receiver_type_expr_name(w.cur_fn_decl.receiver.typ)
	if generic_name == '' || w.env == unsafe { nil } {
		return out
	}
	for bindings in w.current_fn_generic_bindings() {
		if concrete := bindings[generic_name] {
			for receiver in type_name_candidates_from_type(mod_name, concrete) {
				add_unique_string(mut out, receiver)
			}
		}
	}
	return out
}

fn (w &Walker) current_fn_generic_bindings() []map[string]types.Type {
	if w.env == unsafe { nil } || w.cur_fn_decl.name == '' {
		return []map[string]types.Type{}
	}
	mut out := []map[string]types.Type{}
	for key, bindings_list in w.env.generic_types {
		if generic_base_name_from_key(key) != w.cur_fn_decl.name {
			continue
		}
		for bindings in bindings_list {
			out << bindings
		}
	}
	return out
}

fn (w &Walker) receiver_candidates_for_expr(expr ast.Expr, mod_name string) []string {
	mut out := []string{}
	pos := expr.pos()
	if w.env != unsafe { nil } && pos.is_valid() {
		if receiver_type := w.env.get_expr_type(pos.id) {
			for name in type_name_candidates_from_type(mod_name, receiver_type) {
				add_unique_string(mut out, name)
			}
		}
	}
	match expr {
		ast.Ident {
			if w.env != unsafe { nil } && w.cur_fn_scope != unsafe { nil } {
				if local_type := w.env.lookup_local_var(w.cur_fn_scope, expr.name) {
					for type_name in type_name_candidates_from_type(mod_name, local_type) {
						add_unique_string(mut out, type_name)
					}
				}
			}
			name := sanitize_receiver_name(expr.name)
			for type_name in w.current_receiver_candidates(expr.name, mod_name) {
				add_unique_string(mut out, type_name)
			}
			for type_name in w.current_param_receiver_candidates(expr.name, mod_name) {
				add_unique_string(mut out, type_name)
			}
			add_unique_string(mut out, name)
			if mod_name != '' && mod_name != 'main' {
				add_unique_string(mut out, '${mod_name}__${name}')
			}
		}
		ast.PrefixExpr {
			for name in w.receiver_candidates_for_expr(expr.expr, mod_name) {
				add_unique_string(mut out, name)
			}
		}
		ast.ParenExpr {
			for name in w.receiver_candidates_for_expr(expr.expr, mod_name) {
				add_unique_string(mut out, name)
			}
		}
		ast.ModifierExpr {
			for name in w.receiver_candidates_for_expr(expr.expr, mod_name) {
				add_unique_string(mut out, name)
			}
		}
		ast.InitExpr {
			// InitExpr has a .typ that names the struct being constructed.
			for name in w.receiver_candidates_for_expr(expr.typ, mod_name) {
				add_unique_string(mut out, name)
			}
		}
		ast.CallExpr {
			if expr.lhs is ast.SelectorExpr {
				selector := expr.lhs as ast.SelectorExpr
				if selector.lhs is ast.Ident {
					type_name := selector.lhs.name
					if type_name in w.type_names {
						add_receiver_name_candidates(mut out, mod_name, type_name)
					}
				}
			}
		}
		ast.CallOrCastExpr {
			if expr.lhs is ast.SelectorExpr {
				selector := expr.lhs as ast.SelectorExpr
				if selector.lhs is ast.Ident {
					type_name := selector.lhs.name
					if type_name in w.type_names {
						add_receiver_name_candidates(mut out, mod_name, type_name)
					}
				}
			}
		}
		ast.SelectorExpr {
			lhs_receivers := w.receiver_candidates_for_expr(expr.lhs, mod_name)
			for lhs_receiver in lhs_receivers {
				key := '${lhs_receiver}:${expr.rhs.name}'
				if field_receivers := w.struct_field_receivers[key] {
					for field_receiver in field_receivers {
						add_unique_string(mut out, field_receiver)
					}
				}
			}
		}
		else {}
	}

	return out
}

fn (w &Walker) is_cast_type_name(name string) bool {
	if name in builtin_cast_type_names {
		return true
	}
	return name in w.type_names
}

fn (mut w Walker) mark_call_lhs(lhs ast.Expr, mod_name string) {
	match lhs {
		ast.Ident {
			w.mark_fn_name(lhs.name, mod_name)
		}
		ast.GenericArgs {
			w.mark_call_lhs(lhs.lhs, mod_name)
		}
		ast.GenericArgOrIndexExpr {
			w.mark_call_lhs(lhs.lhs, mod_name)
		}
		ast.SelectorExpr {
			method_name := lhs.rhs.name
			if lhs.lhs is ast.Ident {
				left_name := lhs.lhs.name
				if left_name == 'C' {
					w.mark_fn_name(method_name, mod_name)
					return
				}
				if left_name in w.module_names {
					// Resolve import aliases (e.g., 'ssa_optimize' → 'optimize')
					// so lookup finds 'optimize__func' not 'ssa_optimize__func'.
					real_mod := w.module_alias_to_real[left_name] or { left_name }
					w.mark_fn_name('${real_mod}__${method_name}', mod_name)
					return
				}
				if left_name in w.type_names {
					w.mark_method_name(method_name, [left_name, '${mod_name}__${left_name}'])
					return
				}
			}
			if lhs.lhs is ast.SelectorExpr {
				type_sel := lhs.lhs as ast.SelectorExpr
				if type_sel.lhs is ast.Ident {
					mod_ident := type_sel.lhs.name
					type_name := type_sel.rhs.name
					if mod_ident in w.module_names || mod_ident in w.type_names {
						candidates := [
							type_name,
							'${mod_name}__${type_name}',
							'${mod_ident}__${type_name}',
						]
						w.mark_method_name(method_name, candidates)
						return
					}
				}
			}
			receivers := w.receiver_candidates_for_expr(lhs.lhs, mod_name)
			if receivers.len > 0 {
				prev_queue_len := w.queue.len
				w.mark_method_name(method_name, receivers)
				if w.queue.len > prev_queue_len {
					return
				}
			}
			w.mark_method_name_fallback(method_name)
		}
		else {}
	}
}

fn (mut w Walker) walk_stmts(stmts []ast.Stmt, mod_name string) {
	for stmt in stmts {
		w.walk_stmt(stmt, mod_name)
	}
}

fn (mut w Walker) walk_stmt(stmt ast.Stmt, mod_name string) {
	if !stmt_ok(stmt) {
		return
	}
	match stmt {
		ast.AssertStmt {
			w.walk_expr(stmt.expr, mod_name)
			w.walk_expr(stmt.extra, mod_name)
		}
		ast.AssignStmt {
			for expr in stmt.lhs {
				w.walk_expr(expr, mod_name)
			}
			for expr in stmt.rhs {
				w.walk_expr(expr, mod_name)
			}
		}
		ast.BlockStmt {
			w.walk_stmts(stmt.stmts, mod_name)
		}
		ast.ComptimeStmt {
			w.walk_stmt(stmt.stmt, mod_name)
		}
		ast.ConstDecl {
			for field in stmt.fields {
				w.walk_expr(field.value, mod_name)
			}
		}
		ast.DeferStmt {
			w.walk_stmts(stmt.stmts, mod_name)
		}
		ast.EnumDecl {
			for field in stmt.fields {
				w.walk_expr(field.value, mod_name)
			}
		}
		ast.ExprStmt {
			w.walk_expr(stmt.expr, mod_name)
		}
		ast.ForInStmt {
			w.walk_expr(stmt.key, mod_name)
			w.walk_expr(stmt.value, mod_name)
			w.walk_expr(stmt.expr, mod_name)
		}
		ast.ForStmt {
			w.walk_stmt(stmt.init, mod_name)
			w.walk_expr(stmt.cond, mod_name)
			w.walk_stmt(stmt.post, mod_name)
			w.walk_stmts(stmt.stmts, mod_name)
		}
		ast.GlobalDecl {
			for field in stmt.fields {
				w.walk_expr(field.typ, mod_name)
				w.walk_expr(field.value, mod_name)
			}
		}
		ast.LabelStmt {
			w.walk_stmt(stmt.stmt, mod_name)
		}
		ast.ReturnStmt {
			for expr in stmt.exprs {
				w.walk_expr(expr, mod_name)
				// If the function returns an interface type and the return expr
				// is a concrete struct InitExpr, mark the concrete type's
				// interface methods so the vtable wrapper can call them.
				w.mark_interface_conversion_methods(w.cur_fn_decl.typ.return_type, expr, mod_name)
				w.mark_result_error_return_methods(w.cur_fn_decl.typ.return_type, expr, mod_name)
			}
		}
		ast.StructDecl {
			for field in stmt.fields {
				w.walk_expr(field.typ, mod_name)
				w.walk_expr(field.value, mod_name)
			}
		}
		ast.TypeDecl {
			w.walk_expr(stmt.base_type, mod_name)
			for variant in stmt.variants {
				w.walk_expr(variant, mod_name)
			}
		}
		[]ast.Attribute {
			attrs := stmt as []ast.Attribute
			for attr in attrs {
				w.walk_expr(attr.value, mod_name)
				w.walk_expr(attr.comptime_cond, mod_name)
			}
		}
		else {}
	}
}

fn (mut w Walker) walk_expr(expr ast.Expr, mod_name string) {
	if !expr_ok(expr) {
		return
	}
	match expr {
		ast.ArrayInitExpr {
			w.walk_expr(expr.typ, mod_name)
			for item in expr.exprs {
				w.walk_expr(item, mod_name)
			}
			w.walk_expr(expr.init, mod_name)
			w.walk_expr(expr.cap, mod_name)
			w.walk_expr(expr.len, mod_name)
		}
		ast.AsCastExpr {
			w.walk_expr(expr.expr, mod_name)
			w.walk_expr(expr.typ, mod_name)
		}
		ast.AssocExpr {
			w.walk_expr(expr.typ, mod_name)
			w.walk_expr(expr.expr, mod_name)
			for field in expr.fields {
				w.walk_expr(field.value, mod_name)
			}
		}
		ast.CallExpr {
			w.mark_call_lhs(expr.lhs, mod_name)
			w.mark_call_arg_interface_conversions(expr, mod_name)
			w.walk_expr(expr.lhs, mod_name)
			for arg in expr.args {
				w.walk_expr(arg, mod_name)
			}
		}
		ast.CallOrCastExpr {
			if expr.lhs is ast.Ident && w.is_cast_type_name(expr.lhs.name) {
				w.mark_interface_conversion_methods(expr.lhs, expr.expr, mod_name)
				w.walk_expr(expr.expr, mod_name)
				return
			}
			w.mark_call_lhs(expr.lhs, mod_name)
			w.mark_call_or_cast_arg_interface_conversion(expr, mod_name)
			w.walk_expr(expr.lhs, mod_name)
			w.walk_expr(expr.expr, mod_name)
		}
		ast.CastExpr {
			w.mark_interface_conversion_methods(expr.typ, expr.expr, mod_name)
			w.walk_expr(expr.typ, mod_name)
			w.walk_expr(expr.expr, mod_name)
		}
		ast.ComptimeExpr {
			w.walk_expr(expr.expr, mod_name)
		}
		ast.FnLiteral {
			w.walk_stmts(expr.stmts, mod_name)
		}
		ast.GenericArgs {
			w.walk_expr(expr.lhs, mod_name)
			for arg in expr.args {
				w.walk_expr(arg, mod_name)
			}
		}
		ast.GenericArgOrIndexExpr {
			w.walk_expr(expr.lhs, mod_name)
			w.walk_expr(expr.expr, mod_name)
		}
		ast.Ident {
			w.mark_ierror_wrapper_dependencies(expr.name, mod_name)
			if !w.is_cast_type_name(expr.name) {
				w.mark_fn_name(expr.name, mod_name)
			}
		}
		ast.IfExpr {
			w.walk_expr(expr.cond, mod_name)
			w.walk_stmts(expr.stmts, mod_name)
			w.walk_expr(expr.else_expr, mod_name)
		}
		ast.IfGuardExpr {
			w.walk_stmt(ast.Stmt(expr.stmt), mod_name)
		}
		ast.IndexExpr {
			w.walk_expr(expr.lhs, mod_name)
			w.walk_expr(expr.expr, mod_name)
		}
		ast.InfixExpr {
			w.mark_infix_operator_method(expr, mod_name)
			w.walk_expr(expr.lhs, mod_name)
			w.walk_expr(expr.rhs, mod_name)
		}
		ast.InitExpr {
			w.mark_interface_conversion_methods(expr.typ, expr, mod_name)
			w.walk_expr(expr.typ, mod_name)
			for field in expr.fields {
				w.walk_expr(field.value, mod_name)
			}
		}
		ast.KeywordOperator {
			for value in expr.exprs {
				w.walk_expr(value, mod_name)
			}
		}
		ast.LambdaExpr {
			w.walk_expr(expr.expr, mod_name)
		}
		ast.LockExpr {
			for lock_expr in expr.lock_exprs {
				w.walk_expr(lock_expr, mod_name)
			}
			for lock_expr in expr.rlock_exprs {
				w.walk_expr(lock_expr, mod_name)
			}
			w.walk_stmts(expr.stmts, mod_name)
		}
		ast.MapInitExpr {
			w.walk_expr(expr.typ, mod_name)
			for key in expr.keys {
				w.walk_expr(key, mod_name)
			}
			for value in expr.vals {
				w.walk_expr(value, mod_name)
			}
		}
		ast.MatchExpr {
			w.walk_expr(expr.expr, mod_name)
			for branch in expr.branches {
				for cond in branch.cond {
					w.walk_expr(cond, mod_name)
				}
				w.walk_stmts(branch.stmts, mod_name)
			}
		}
		ast.ModifierExpr {
			w.walk_expr(expr.expr, mod_name)
		}
		ast.OrExpr {
			w.walk_expr(expr.expr, mod_name)
			w.walk_stmts(expr.stmts, mod_name)
		}
		ast.ParenExpr {
			w.walk_expr(expr.expr, mod_name)
		}
		ast.PostfixExpr {
			w.walk_expr(expr.expr, mod_name)
		}
		ast.PrefixExpr {
			w.walk_expr(expr.expr, mod_name)
		}
		ast.RangeExpr {
			w.walk_expr(expr.start, mod_name)
			w.walk_expr(expr.end, mod_name)
		}
		ast.SelectExpr {
			w.walk_stmt(expr.stmt, mod_name)
			w.walk_stmts(expr.stmts, mod_name)
			w.walk_expr(expr.next, mod_name)
		}
		ast.SelectorExpr {
			w.walk_expr(expr.lhs, mod_name)
		}
		ast.SqlExpr {
			w.walk_expr(expr.expr, mod_name)
		}
		ast.StringInterLiteral {
			for inter in expr.inters {
				w.mark_string_interpolation_str_dependency(inter.expr, mod_name)
				w.walk_expr(inter.expr, mod_name)
				w.walk_expr(inter.format_expr, mod_name)
			}
		}
		ast.Tuple {
			for value in expr.exprs {
				w.walk_expr(value, mod_name)
			}
		}
		ast.UnsafeExpr {
			w.walk_stmts(expr.stmts, mod_name)
		}
		ast.Type {
			match expr {
				ast.ArrayFixedType {
					w.walk_expr(expr.len, mod_name)
					w.walk_expr(expr.elem_type, mod_name)
				}
				ast.ArrayType {
					w.walk_expr(expr.elem_type, mod_name)
				}
				ast.ChannelType {
					w.walk_expr(expr.cap, mod_name)
					w.walk_expr(expr.elem_type, mod_name)
				}
				ast.FnType {
					for param in expr.params {
						w.walk_expr(param.typ, mod_name)
					}
					w.walk_expr(expr.return_type, mod_name)
				}
				ast.GenericType {
					w.walk_expr(expr.name, mod_name)
					for param in expr.params {
						w.walk_expr(param, mod_name)
					}
				}
				ast.MapType {
					w.walk_expr(expr.key_type, mod_name)
					w.walk_expr(expr.value_type, mod_name)
				}
				ast.OptionType {
					w.walk_expr(expr.base_type, mod_name)
				}
				ast.PointerType {
					w.walk_expr(expr.base_type, mod_name)
				}
				ast.ResultType {
					w.walk_expr(expr.base_type, mod_name)
				}
				ast.ThreadType {
					w.walk_expr(expr.elem_type, mod_name)
				}
				ast.TupleType {
					for typ in expr.types {
						w.walk_expr(typ, mod_name)
					}
				}
				ast.AnonStructType {
					for embedded in expr.embedded {
						w.walk_expr(embedded, mod_name)
					}
					for field in expr.fields {
						w.walk_expr(field.typ, mod_name)
						w.walk_expr(field.value, mod_name)
					}
				}
				else {}
			}
		}
		else {}
	}
}
