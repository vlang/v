// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module markused

// Walk the entire program starting at fn main and marks used (called) functions.
// Unused functions can be safely skipped by the backends to save CPU time and space.
import v.ast
import v.pref

pub struct Walker {
pub mut:
	table           &ast.Table        = unsafe { nil }
	features        &ast.UsedFeatures = unsafe { nil }
	used_fns        map[string]bool // used_fns['println'] == true
	trace_enabled   bool
	used_consts     map[string]bool // used_consts['os.args'] == true
	used_globals    map[string]bool
	used_fields     map[string]bool
	used_structs    map[string]bool
	used_types      map[ast.Type]bool
	used_syms       map[int]bool
	used_arr_method map[string]bool
	used_map_method map[string]bool
	used_none       int // _option_none
	used_option     int // _option_ok
	used_result     int // _result_ok
	used_panic      int // option/result propagation
	used_closures   int // fn [x] (){}, and `instance.method` used in an expression
	pref            &pref.Preferences = unsafe { nil }
mut:
	all_fns       map[string]ast.FnDecl
	generic_fns   []&ast.FnDecl
	all_consts    map[string]ast.ConstField
	all_globals   map[string]ast.GlobalField
	all_fields    map[string]ast.StructField
	all_decltypes map[string]ast.TypeDecl
	all_structs   map[string]ast.StructDecl

	cur_fn                    string
	cur_fn_concrete_types     []ast.Type
	level                     int
	is_builtin_mod            bool
	is_direct_array_access    bool
	inside_in_op              bool
	inside_comptime           int
	inside_comptime_if        int
	used_fn_generic_types     map[string][][]ast.Type
	walked_fn_generic_types   map[string][][]ast.Type
	keep_all_fn_generic_types map[string]bool

	// dependencies finding flags
	uses_atomic                bool // has atomic
	uses_array                 bool // has array
	uses_array_sumtype         bool // has array on sumtype init
	uses_channel               bool // has chan dep
	uses_lock                  bool // has mutex dep
	uses_ct_fields             bool // $for .fields
	uses_ct_methods            bool // $for .methods
	uses_ct_params             bool // $for .params
	uses_ct_values             bool // $for .values
	uses_ct_variants           bool // $for .variants
	uses_ct_attribute          bool // $for .attributes
	uses_external_type         bool
	uses_err_block             bool // or { err var }
	uses_asserts               bool // assert
	uses_map_update            bool // has {...expr}
	uses_debugger              bool // has debugger;
	uses_mem_align             bool // @[aligned:N] for structs
	uses_eq                    bool // has == op
	uses_interp                bool // string interpolation
	uses_guard                 bool
	uses_orm                   bool
	uses_str                   map[ast.Type]bool // has .str() calls, and for which types
	uses_free                  map[ast.Type]bool // has .free() calls, and for which types
	uses_spawn                 bool
	uses_dump                  bool
	uses_memdup                bool // sumtype cast and &Struct{}
	uses_arr_void              bool // auto arr methods
	uses_index                 bool // var[k]
	uses_index_check           bool // var[k] or { }
	uses_arr_range_index       bool // arr[i..j]
	uses_str_range_index       bool // str[i..j]
	uses_range_index_check     bool // var[i..j] or { }
	uses_arr_range_index_gated bool
	uses_str_range_index_gated bool
	uses_str_index             bool // string[k]
	uses_str_index_check       bool // string[k] or { }
	uses_str_range             bool // string[a..b]
	uses_str_literal           bool
	uses_fixed_arr_int         bool // fixed_arr[k]
	uses_append                bool // var << item
	uses_map_setter            bool
	uses_map_getter            bool
	uses_arr_setter            bool
	uses_arr_getter            bool
	uses_arr_clone             bool
	uses_arr_sorted            bool
	uses_type_name             bool // sum_type.type_name()
}

pub fn Walker.new(params Walker) &Walker {
	mut new_walker := &Walker{
		...params
	}
	new_walker.features = params.table.used_features
	return new_walker
}

@[inline]
fn (mut w Walker) mark_fn_as_used(fkey string) {
	$if trace_skip_unused_marked ? {
		eprintln('    fn > |${fkey}|')
	}
	w.used_fns[fkey] = true
}

fn (w &Walker) fn_generic_names(node ast.FnDecl) []string {
	mut generic_names := []string{}
	if node.is_method {
		receiver_sym := w.table.sym(node.receiver.typ)
		match receiver_sym.info {
			ast.Struct {
				generic_names << w.table.get_generic_names(receiver_sym.info.generic_types)
			}
			ast.Interface {
				generic_names << w.table.get_generic_names(receiver_sym.info.generic_types)
			}
			ast.SumType {
				generic_names << w.table.get_generic_names(receiver_sym.info.generic_types)
			}
			else {}
		}
	}
	// Only add method-level generic names that aren't already from the receiver
	// (V puts inherited receiver generics into node.generic_names too).
	for gn in node.generic_names {
		if gn !in generic_names {
			generic_names << gn
		}
	}
	return generic_names
}

fn (mut w Walker) resolve_current_generic_type(typ ast.Type) ast.Type {
	if typ == 0 || !typ.has_flag(.generic) || w.cur_fn == '' || w.cur_fn_concrete_types.len == 0 {
		return typ
	}
	cur_fn := w.all_fns[w.cur_fn] or { return typ }
	generic_names := w.fn_generic_names(cur_fn)
	if generic_names.len == 0 || generic_names.len != w.cur_fn_concrete_types.len {
		return typ
	}
	return w.table.convert_generic_type(typ, generic_names, w.cur_fn_concrete_types) or { typ }
}

fn (mut w Walker) resolve_current_concrete_types(types []ast.Type) []ast.Type {
	if types.len == 0 {
		return []
	}
	mut concrete_types := []ast.Type{cap: types.len}
	for typ in types {
		concrete_types << w.resolve_current_generic_type(typ)
	}
	if concrete_types.any(it.has_flag(.generic)) {
		return []
	}
	return concrete_types
}

fn (w &Walker) current_generic_context() ([]string, []ast.Type) {
	if w.cur_fn == '' {
		return []string{}, []ast.Type{}
	}
	if w.cur_fn.contains('_T_') {
		return w.specialized_generic_context_for(w.cur_fn)
	}
	if w.cur_fn_concrete_types.len == 0 {
		return []string{}, []ast.Type{}
	}
	cur_fn := w.all_fns[w.cur_fn] or { return []string{}, []ast.Type{} }
	generic_names := w.fn_generic_names(cur_fn)
	if generic_names.len == 0 || generic_names.len != w.cur_fn_concrete_types.len {
		return []string{}, []ast.Type{}
	}
	return generic_names, w.cur_fn_concrete_types.clone()
}

fn (w &Walker) current_generic_type_by_name(name string) ast.Type {
	generic_names, concrete_types := w.current_generic_context()
	if generic_names.len == 0 || generic_names.len != concrete_types.len {
		return ast.no_type
	}
	idx := generic_names.index(name)
	if idx < 0 || idx >= concrete_types.len {
		return ast.no_type
	}
	return concrete_types[idx]
}

fn (w &Walker) has_sumtype_generic_context(types []ast.Type) bool {
	for typ in types {
		if typ == 0 {
			continue
		}
		if w.table.final_sym(w.table.unaliased_type(typ)).kind == .sum_type {
			return true
		}
	}
	return false
}

fn (w &Walker) sumtype_variant_concrete_types(types []ast.Type) [][]ast.Type {
	if types.len != 1 {
		return [][]ast.Type{}
	}
	sumtype := w.table.unaliased_type(types[0])
	sumtype_sym := w.table.final_sym(sumtype)
	if sumtype_sym.kind != .sum_type || sumtype_sym.info !is ast.SumType {
		return [][]ast.Type{}
	}
	mut concrete_types := [][]ast.Type{}
	for variant in (sumtype_sym.info as ast.SumType).variants {
		concrete_types << [variant]
	}
	return concrete_types
}

fn (mut w Walker) trusted_source_concrete_types(node ast.CallExpr, receiver_typ ast.Type) []ast.Type {
	if node.raw_concrete_types.len > 0 {
		return w.resolve_current_concrete_types(node.raw_concrete_types)
	}
	if node.concrete_types.len == 0 {
		return []ast.Type{}
	}
	resolved_source_concrete_types := w.resolve_current_concrete_types(node.concrete_types)
	if resolved_source_concrete_types.len == 0 {
		return []ast.Type{}
	}
	caller_generic_names, _ := w.current_generic_context()
	if caller_generic_names.len == 0 || w.inside_comptime_if > 0 {
		return resolved_source_concrete_types
	}
	mut receiver_concrete_types := w.receiver_concrete_types(receiver_typ)
	if receiver_concrete_types.len == 0 && node.receiver_concrete_type != 0
		&& node.receiver_concrete_type != receiver_typ {
		receiver_concrete_types = w.receiver_concrete_types(node.receiver_concrete_type)
	}
	if receiver_concrete_types.len > 0 && receiver_concrete_types == resolved_source_concrete_types {
		return resolved_source_concrete_types
	}
	return []ast.Type{}
}

fn (mut w Walker) mark_json2_optional_field_helpers(concrete_typ ast.Type) {
	concrete_sym := w.table.final_sym(w.table.unaliased_type(concrete_typ))
	if concrete_sym.kind != .struct || concrete_sym.info !is ast.Struct {
		return
	}
	struct_info := concrete_sym.info as ast.Struct
	if mut create_value_from_optional_fn := w.all_fns['x.json2.create_value_from_optional'] {
		for field in struct_info.fields {
			if field.typ.has_flag(.option) {
				w.fn_decl_with_concrete_types(mut create_value_from_optional_fn, [
					field.typ.clear_flag(.option),
				])
			}
		}
	}
}

fn (mut w Walker) mark_json2_encode_field_helpers(receiver_typ ast.Type, concrete_typ ast.Type) {
	concrete_sym := w.table.final_sym(w.table.unaliased_type(concrete_typ))
	if concrete_sym.kind != .struct || concrete_sym.info !is ast.Struct {
		return
	}
	struct_info := concrete_sym.info as ast.Struct
	encode_struct_field_value_fkey, _ := w.resolve_method_fkey_for_type(receiver_typ,
		'encode_struct_field_value')
	mut encode_struct_field_value_fn := if encode_struct_field_value_fkey != '' {
		w.all_fns[encode_struct_field_value_fkey] or { ast.FnDecl{} }
	} else {
		ast.FnDecl{}
	}
	mut struct_field_is_none_fn := w.all_fns['x.json2.struct_field_is_none'] or { ast.FnDecl{} }
	mut struct_field_is_nil_fn := w.all_fns['x.json2.struct_field_is_nil'] or { ast.FnDecl{} }
	mut check_not_empty_fn := w.all_fns['x.json2.check_not_empty'] or { ast.FnDecl{} }
	for field in struct_info.fields {
		if field.is_embed {
			continue
		}
		if encode_struct_field_value_fn.name != '' {
			w.fn_decl_with_concrete_types(mut encode_struct_field_value_fn, [field.typ])
		}
		if struct_field_is_none_fn.name != '' && field.typ.has_flag(.option) {
			w.fn_decl_with_concrete_types(mut struct_field_is_none_fn, [field.typ])
		}
		if struct_field_is_nil_fn.name != '' && field.typ.nr_muls() > 0 {
			w.fn_decl_with_concrete_types(mut struct_field_is_nil_fn, [field.typ])
		}
		if check_not_empty_fn.name != '' {
			w.fn_decl_with_concrete_types(mut check_not_empty_fn, [field.typ])
		}
	}
}

fn (w &Walker) resolve_comptime_condition_type(expr ast.Expr) ast.Type {
	match expr {
		ast.Ident {
			if expr.obj.typ != 0 {
				resolved_type := w.resolve_current_specialized_type(expr.obj.typ)
				if resolved_type != 0 {
					return resolved_type
				}
			}
			resolved_type := w.resolve_current_specialized_var_type(expr.name)
			if resolved_type != 0 {
				return resolved_type
			}
			generic_type := w.current_generic_type_by_name(expr.name)
			if generic_type != 0 {
				return generic_type
			}
		}
		ast.SelectorExpr {
			if expr.expr is ast.Ident {
				mut base_type := ast.no_type
				if expr.expr.obj.typ != 0 {
					base_type = w.resolve_current_specialized_type(expr.expr.obj.typ)
				}
				if base_type == 0 {
					base_type = w.resolve_current_specialized_var_type(expr.expr.name)
				}
				if base_type == 0 {
					base_type = w.current_generic_type_by_name(expr.expr.name)
				}
				if base_type == 0 {
					return ast.no_type
				}
				return if expr.field_name == 'unaliased_typ' {
					w.table.unaliased_type(base_type)
				} else if expr.field_name == 'typ' {
					base_type
				} else {
					ast.no_type
				}
			}
		}
		ast.TypeNode {
			if expr.typ.has_flag(.generic) {
				generic_name := w.table.sym(expr.typ).name
				if generic_name != '' {
					generic_type := w.current_generic_type_by_name(generic_name)
					if generic_type != 0 {
						return generic_type
					}
				}
			}
			return w.resolve_current_specialized_type(expr.typ)
		}
		else {}
	}

	return ast.no_type
}

fn (w &Walker) comptime_type_matches(left_type ast.Type, right ast.Expr) ?bool {
	if left_type == 0 {
		return none
	}
	match right {
		ast.ComptimeType {
			sym := w.table.sym(left_type)
			return match right.kind {
				.array { sym.info is ast.Array || sym.info is ast.ArrayFixed }
				.array_dynamic { sym.info is ast.Array }
				.array_fixed { sym.info is ast.ArrayFixed }
				.iface { sym.info is ast.Interface }
				.map { sym.info is ast.Map }
				.struct { sym.info is ast.Struct }
				.enum { sym.info is ast.Enum }
				.sum_type { sym.info is ast.SumType }
				.alias { sym.info is ast.Alias }
				.function { sym.kind == .function }
				.option { left_type.has_flag(.option) }
				.shared { left_type.has_flag(.shared_f) }
				.int { left_type.is_int() }
				.float { left_type.is_float() }
				.string { left_type.is_string() }
				.pointer { left_type.is_ptr() }
				.voidptr { w.table.unaliased_type(left_type) == ast.voidptr_type }
				else { none }
			}
		}
		ast.TypeNode {
			sym := w.table.sym(right.typ)
			if sym.info is ast.Interface {
				return w.table.does_type_implement_interface(left_type, right.typ)
			}
			return left_type == right.typ
		}
		else {
			return none
		}
	}
}

fn (mut w Walker) comptime_condition_is_true(expr ast.Expr) ?bool {
	match expr {
		ast.ParExpr {
			return w.comptime_condition_is_true(expr.expr)
		}
		ast.InfixExpr {
			match expr.op {
				.key_is {
					left_type := w.resolve_comptime_condition_type(expr.left)
					if left_type == 0 {
						return none
					}
					return w.comptime_type_matches(left_type, expr.right)
				}
				.and {
					left := w.comptime_condition_is_true(expr.left)?
					right := w.comptime_condition_is_true(expr.right)?
					return left && right
				}
				.logical_or {
					left := w.comptime_condition_is_true(expr.left)?
					right := w.comptime_condition_is_true(expr.right)?
					return left || right
				}
				else {
					return none
				}
			}
		}
		ast.NodeError {
			return true
		}
		else {
			return none
		}
	}
}

fn (mut w Walker) resolve_comptime_if_branch(node ast.IfExpr) ?ast.IfBranch {
	if !node.is_comptime {
		return none
	}
	for i, branch in node.branches {
		if node.has_else && i == node.branches.len - 1 {
			return branch
		}
		cond_result := w.comptime_condition_is_true(branch.cond) or { return none }
		if cond_result {
			return branch
		}
	}
	return none
}

fn (mut w Walker) record_used_fn_generic_types(fkey string, concrete_types []ast.Type) {
	if concrete_types.len == 0 || concrete_types.any(it.has_flag(.generic)) {
		return
	}
	if concrete_types !in w.used_fn_generic_types[fkey] {
		w.used_fn_generic_types[fkey] << concrete_types.clone()
	}
}

@[inline]
pub fn (mut w Walker) mark_builtin_array_method_as_used(method_name string) {
	w.mark_builtin_type_method_as_used('${ast.array_type_idx}.${method_name}',
		'${int(ast.array_type.ref())}.${method_name}')
}

@[inline]
pub fn (mut w Walker) mark_builtin_map_method_as_used(method_name string) {
	w.mark_builtin_type_method_as_used('${ast.map_type_idx}.${method_name}',
		'${int(ast.map_type.ref())}.${method_name}')
}

pub fn (mut w Walker) mark_builtin_type_method_as_used(k string, rk string) {
	if mut cfn := w.all_fns[k] {
		w.fn_decl(mut cfn)
	} else if mut cfn := w.all_fns[rk] {
		w.fn_decl(mut cfn)
	}
}

pub fn (mut w Walker) mark_const_as_used(ckey string) {
	$if trace_skip_unused_marked ? {
		eprintln('    const > |${ckey}|')
	}
	if w.used_consts[ckey] {
		return
	}
	w.used_consts[ckey] = true
	cfield := w.all_consts[ckey] or { return }
	w.expr(cfield.expr)
	w.mark_by_type(cfield.typ)
}

pub fn (mut w Walker) mark_global_as_used(ckey string) {
	$if trace_skip_unused_marked ? {
		eprintln('  global > |${ckey}|')
	}
	if w.used_globals[ckey] {
		return
	}
	w.used_globals[ckey] = true
	gfield := w.all_globals[ckey] or { return }
	w.table.used_features.used_attr_weak = w.table.used_features.used_attr_weak || gfield.is_weak
	w.table.used_features.used_attr_hidden = w.table.used_features.used_attr_hidden
		|| gfield.is_hidden || gfield.is_hidden
	w.expr(gfield.expr)
	if !gfield.has_expr {
		w.mark_by_type(gfield.typ)
	}
}

pub fn (mut w Walker) mark_struct_field_default_expr_as_used(sfkey string) {
	if w.used_fields[sfkey] {
		return
	}
	w.used_fields[sfkey] = true
	sfield := w.all_fields[sfkey] or { return }
	w.expr(sfield.default_expr)
}

pub fn (mut w Walker) mark_markused_fns() {
	for _, mut func in w.all_fns {
		// @[export]
		// @[markused]
		if func.is_exported || func.is_markused {
			$if trace_skip_unused_exported_fns ? {
				if func.is_exported {
					println('>>>> walking exported func: ${func.name} ...')
				}
				if func.is_markused {
					println('>>>> walking markused func: ${func.name} ...')
				}
			}
			w.fn_decl(mut func)
			continue
		}
		// veb actions
		if func.return_type == w.table.veb_res_idx_cache {
			$if trace_skip_veb_actions ? {
				println('>>>> walking veb action func: ${func.name} ...')
			}
			w.fn_decl(mut func)
		}
	}
}

pub fn (mut w Walker) mark_root_fns(all_fn_root_names []string) {
	if w.trace_enabled {
		eprintln('>>>>>>> ROOT ${all_fn_root_names}')
	}
	for fn_name in all_fn_root_names {
		if fn_name !in w.used_fns {
			$if trace_skip_unused_roots ? {
				println('>>>> walking root func: ${fn_name} ...')
			}
			unsafe { w.fn_decl(mut w.all_fns[fn_name]) }
		}
	}
}

pub fn (mut w Walker) mark_generic_fn_instances() {
	for generic_fn in w.generic_fns {
		if generic_fn.generic_names.len == 0 {
			continue
		}
		if w.table.final_sym(generic_fn.return_type).info is ast.FnType
			|| generic_fn.params.any(w.table.final_sym(it.typ).info is ast.FnType) {
			continue
		}
		base_fkey := generic_fn.fkey()
		for concrete_types in w.table.fn_generic_types[base_fkey] {
			if concrete_types.any(it.has_flag(.generic)) {
				continue
			}
			specialized_fkey := w.generic_concrete_name(base_fkey, concrete_types)
			mut fn_copy := ast.FnDecl{
				...*generic_fn
			}
			w.fn_decl_with_fkey(mut fn_copy, specialized_fkey)
		}
	}
}

pub fn (mut w Walker) mark_markused_consts() {
	for ckey, mut constfield in w.all_consts {
		if constfield.is_markused || constfield.is_exported {
			$if trace_skip_unused_markused_consts ? {
				println('>>>> walking markused const: ${ckey}')
			}
			w.mark_const_as_used(ckey)
		}
	}
}

pub fn (mut w Walker) mark_markused_globals() {
	for gkey, mut globalfield in w.all_globals {
		if globalfield.is_markused || globalfield.is_exported {
			$if trace_skip_unused_markused_globals ? {
				println('>>>> walking markused global: ${gkey}')
			}
			w.mark_global_as_used(gkey)
		}
	}
}

pub fn (mut w Walker) mark_markused_syms() {
	for sym in w.table.type_symbols {
		if sym.info is ast.Struct && sym.info.is_markused {
			w.mark_by_sym(sym)
		} else if sym.info is ast.Interface && sym.info.is_markused {
			w.mark_by_sym(sym)
		}
	}
}

pub fn (mut w Walker) mark_markused_decltypes() {
	for _, decl in w.all_decltypes {
		if decl.is_markused {
			w.mark_by_type(decl.typ)
		}
	}
}

pub fn (mut w Walker) stmt(node_ ast.Stmt) {
	mut node := unsafe { node_ }
	match mut node {
		ast.EmptyStmt {}
		ast.DebuggerStmt {
			w.uses_debugger = true
		}
		ast.AsmStmt {
			w.asm_io(node.output)
			w.asm_io(node.input)
		}
		ast.AssertStmt {
			if node.is_used {
				w.uses_asserts = true
				w.expr(node.expr)
				if node.extra !is ast.EmptyExpr {
					w.expr(node.extra)
				}
			}
		}
		ast.AssignStmt {
			w.exprs(node.left)
			w.exprs(node.right)
		}
		ast.Block {
			w.stmts(node.stmts)
		}
		ast.ComptimeFor {
			w.mark_by_type(node.typ)
			w.inside_comptime++
			defer {
				w.inside_comptime--
			}
			w.stmts(node.stmts)
			match node.kind {
				.attributes {
					w.uses_ct_attribute = true
				}
				.variants {
					w.uses_ct_variants = true
				}
				.params {
					w.uses_ct_params = true
				}
				.values {
					w.uses_ct_values = true
				}
				.fields {
					w.uses_ct_fields = true
				}
				.methods {
					w.uses_ct_methods = true
				}
			}
		}
		ast.ConstDecl {
			w.const_fields(node.fields)
		}
		ast.ExprStmt {
			w.expr(node.expr)
		}
		ast.FnDecl {
			w.fn_decl(mut node)
		}
		ast.ForCStmt {
			if node.has_init {
				w.stmt(node.init)
			}
			if node.has_cond {
				w.expr(node.cond)
			}
			if node.has_inc {
				w.stmt(node.inc)
			}
			w.stmts(node.stmts)
		}
		ast.ForInStmt {
			w.expr(node.cond)
			w.expr(node.high)
			w.stmts(node.stmts)
			w.mark_by_type(node.cond_type)
			if node.kind == .map {
				w.features.used_maps++
			} else if node.kind == .struct {
				if node.cond_type == 0 {
					return
				}
				// the .next() method of the struct will be used for iteration:
				// In generic functions, node.cond_type may have been overwritten by the checker
				// for the last specialization. Re-resolve from the variable's parameter type.
				mut resolved_cond_type := node.cond_type
				cond := node.cond
				if cond is ast.Ident {
					specialized_type := w.resolve_current_specialized_var_type(cond.name)
					if specialized_type != ast.no_type {
						resolved_cond_type = specialized_type
					}
				}
				cond_type_sym := w.table.sym(resolved_cond_type)
				if next_fn := cond_type_sym.find_method('next') {
					unsafe {
						w.fn_decl(mut &ast.FnDecl(next_fn.source_fn))
					}
				}
			}
		}
		ast.ForStmt {
			if !node.is_inf {
				w.expr(node.cond)
			}
			w.stmts(node.stmts)
		}
		ast.Return {
			w.exprs(node.exprs)
		}
		ast.SqlStmt {
			w.expr(node.db_expr)
			w.expr(node.or_expr)
			for line in node.lines {
				if line.table_expr.typ != 0 {
					w.mark_by_sym(w.table.sym(line.table_expr.typ))
				}
				w.expr(line.where_expr)
				w.exprs(line.update_exprs)
			}
			w.uses_orm = true
		}
		ast.StructDecl {
			for typ in node.implements_types {
				w.mark_by_type(typ.typ)
			}
			w.struct_fields(node.fields)
			w.uses_mem_align = w.uses_mem_align || node.is_aligned
		}
		ast.DeferStmt {
			w.stmts(node.stmts)
		}
		ast.GlobalDecl {
			for gf in node.fields {
				if gf.has_expr {
					w.expr(gf.expr)
				}
			}
		}
		ast.BranchStmt {}
		ast.EnumDecl {}
		ast.GotoLabel {}
		ast.GotoStmt {}
		ast.HashStmt {}
		ast.Import {}
		ast.InterfaceDecl {}
		ast.SemicolonStmt {}
		ast.Module {}
		ast.TypeDecl {}
		ast.NodeError {}
	}
}

fn (mut w Walker) asm_io(ios []ast.AsmIO) {
	for io in ios {
		w.expr(io.expr)
	}
}

fn (mut w Walker) defer_stmts(stmts []ast.DeferStmt) {
	for stmt in stmts {
		w.stmts(stmt.stmts)
	}
}

fn (mut w Walker) stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		w.stmt(stmt)
	}
}

fn (mut w Walker) exprs(exprs []ast.Expr) {
	for expr in exprs {
		w.expr(expr)
	}
}

fn (mut w Walker) expr(node_ ast.Expr) {
	mut node := unsafe { node_ }
	match mut node {
		ast.EmptyExpr {
			// TODO: make sure this doesn't happen
			// panic('Walker: EmptyExpr')
		}
		ast.ComptimeType {}
		ast.AnonFn {
			w.fn_decl(mut node.decl)
		}
		ast.ArrayInit {
			sym := w.table.sym(node.elem_type)
			w.mark_by_sym(sym)
			if sym.info is ast.Thread {
				w.mark_by_type(w.table.find_or_register_array(sym.info.return_type))
			}
			w.expr(node.len_expr)
			w.expr(node.cap_expr)
			w.expr(node.init_expr)
			w.exprs(node.exprs)
			if w.table.final_sym(node.typ).kind == .array {
				if !w.inside_in_op {
					w.uses_array = true
					w.mark_by_type(node.typ)
				}
			} else { // fixed arrays
				w.mark_by_type(node.typ)
			}
			if node.elem_type.has_flag(.option) {
				w.used_option++
			}
		}
		ast.Assoc {
			w.exprs(node.exprs)
		}
		ast.ArrayDecompose {
			w.expr(node.expr)
		}
		ast.CallExpr {
			w.call_expr(mut node)
			if node.is_fn_a_const {
				w.mark_const_as_used(node.name)
			} else if node.name == 'json.decode' {
				w.mark_by_type((node.args[0].expr as ast.TypeNode).typ)
			} else if node.name == 'json.encode' && node.args[0].typ != 0 {
				sym := w.table.final_sym(node.args[0].typ)
				if sym.info is ast.Map {
					w.mark_by_type(w.table.find_or_register_array(sym.info.key_type))
				}
			} else if !node.is_method && node.args.len == 1
				&& node.name in ['println', 'print', 'eprint', 'eprintln'] {
				if f := w.table.find_fn(node.name) {
					if f.mod == 'builtin' {
						if node.args[0].typ != ast.string_type {
							w.uses_str[node.args[0].typ] = true
						}
						if w.pref.gc_mode == .boehm_leak && (node.args[0].typ != ast.string_type
							|| node.args[0].expr !in [ast.Ident, ast.StringLiteral, ast.SelectorExpr, ast.ComptimeSelector]) {
							w.uses_free[ast.string_type] = true
						}
					}
				}
			} else if node.is_method && node.name == 'str' {
				w.uses_str[node.left_type] = true
			} else if node.is_method && node.name == 'free' {
				w.uses_free[node.left_type] = true
			} else if node.is_method && node.name == 'clone' && !w.uses_arr_clone
				&& node.left_type != 0 && w.table.final_sym(node.left_type).kind == .array {
				w.uses_arr_clone = true
			} else if node.is_method && node.name == 'sorted' && !w.uses_arr_sorted
				&& node.left_type != 0 && w.table.final_sym(node.left_type).kind == .array {
				w.uses_arr_sorted = true
			}
			if !w.is_builtin_mod && !w.uses_external_type {
				if node.is_method {
					w.uses_external_type = node.mod == 'builtin'
				} else if node.name.contains('.') && node.name.all_before_last('.').len > 1 {
					w.uses_external_type = true
				}
			}
			if node.is_method && node.left_type != 0
				&& w.table.final_sym(node.left_type).kind in [.array_fixed, .array] {
				w.mark_by_type(node.return_type)
			}
			if node.name.contains('new_array_from_c_array') {
				if !w.inside_in_op {
					w.uses_array = true
					w.mark_by_type(node.return_type) // the transformer fills this with the correct type
				}
			}
		}
		ast.CastExpr {
			w.expr(node.expr)
			w.expr(node.arg)
			if !w.uses_memdup {
				fsym := w.table.final_sym(node.typ)
				w.uses_memdup = fsym.kind in [.sum_type, .interface]
			}
			w.mark_by_type(node.typ)
			if node.typ.has_flag(.option) {
				w.used_option++
			} else if node.typ.has_flag(.result) {
				w.used_result++
			}
		}
		ast.ChanInit {
			w.expr(node.cap_expr)
			w.mark_by_type(node.typ)
			w.uses_channel = true
		}
		ast.ConcatExpr {
			w.exprs(node.vals)
			w.mark_by_sym(w.table.sym(node.return_type))
		}
		ast.ComptimeSelector {
			w.expr(node.left)
			w.expr(node.field_expr)
			w.or_block(node.or_block)
		}
		ast.ComptimeCall {
			w.inside_comptime++
			defer {
				w.inside_comptime--
			}
			w.expr(node.left)
			for args in node.args {
				w.expr(args.expr)
			}
			w.expr(node.or_block)
			if node.is_template {
				w.stmts(node.veb_tmpl.stmts)
			}
			if node.kind == .embed_file {
				w.features.used_maps++
			}
		}
		ast.DumpExpr {
			w.expr(node.expr)
			w.features.dump = true
			w.mark_by_type(node.expr_type)
		}
		ast.SpawnExpr {
			if node.is_expr {
				w.fn_by_name('free')
			}
			w.mark_by_type(w.table.find_or_register_thread(node.call_expr.return_type))
			w.expr(node.call_expr)
			w.uses_spawn = true

			if w.pref.os == .windows {
				w.fn_by_name('panic_lasterr')
				w.fn_by_name('winapi_lasterr_str')
			} else {
				w.fn_by_name('c_error_number_str')
				w.fn_by_name('panic_error_number')
			}
		}
		ast.GoExpr {
			if node.is_expr {
				w.fn_by_name('free')
			}
			w.mark_by_type(node.call_expr.return_type)
			w.expr(node.call_expr)
		}
		ast.IndexExpr {
			w.expr(node.left)
			w.expr(node.index)
			if node.or_expr.kind != .absent || node.is_option {
				w.uses_index_check = true
			}
			w.mark_by_type(node.typ)
			w.or_block(node.or_expr)
			left_type := if node.left_type != 0 {
				node.left_type
			} else {
				w.infer_expr_type(node.left)
			}
			if left_type == 0 {
				return
			}
			sym := w.table.final_sym(left_type)
			if sym.info is ast.Map {
				if node.is_setter && !w.uses_map_setter {
					w.mark_builtin_map_method_as_used('set')
				} else if !node.is_setter && !w.uses_map_getter {
					w.mark_builtin_map_method_as_used('get')
				}
				w.mark_by_type(sym.info.key_type)
				w.mark_by_type(sym.info.value_type)
				w.features.used_maps++
			} else if sym.kind in [.array, .array_fixed, .any] {
				if !w.is_direct_array_access || w.features.auto_str_arr {
					if node.is_setter && !w.uses_arr_setter {
						w.mark_builtin_array_method_as_used('set')
						w.uses_arr_setter = true
					} else if !node.is_setter && !w.uses_arr_getter {
						w.mark_builtin_array_method_as_used('get')
						w.uses_arr_getter = true
					}
				}
				if sym.info is ast.Array {
					w.mark_by_type(sym.info.elem_type)
				} else if sym.info is ast.ArrayFixed {
					w.mark_by_type(sym.info.elem_type)
				}
				if !node.is_gated && node.index is ast.RangeExpr && !w.uses_arr_range_index {
					w.uses_arr_range_index = true
				}
				if !w.uses_fixed_arr_int && sym.kind == .array_fixed {
					w.uses_fixed_arr_int = true
				}
				if !w.uses_index && !w.is_direct_array_access {
					w.uses_index = true
				}
				if node.is_gated && node.index is ast.RangeExpr && !w.uses_arr_range_index_gated {
					w.uses_arr_range_index_gated = node.is_gated
				}
			} else if sym.kind == .string {
				w.uses_str_index = true
				if node.index is ast.RangeExpr {
					if node.is_gated {
						w.fn_by_name('${ast.string_type_idx}.substr_ni')
					} else if node.or_expr.kind != .absent || node.is_option {
						w.fn_by_name('${ast.string_type_idx}.substr_with_check')
					} else {
						w.fn_by_name('${ast.string_type_idx}.substr')
					}
					if !w.uses_str_range_index {
						w.uses_str_range_index = true
					}
					if !w.uses_range_index_check {
						w.uses_range_index_check = node.or_expr.kind != .absent || node.is_option
					}
					if !w.uses_str_range_index_gated {
						w.uses_str_range_index_gated = node.is_gated
					}
				} else {
					if node.or_expr.kind != .absent || node.is_option {
						w.fn_by_name('${ast.string_type_idx}.at_with_check')
					} else {
						w.fn_by_name('${ast.string_type_idx}.at')
					}
					if !w.uses_str_index_check {
						w.uses_str_index_check = node.or_expr.kind != .absent || node.is_option
					}
					if !w.uses_str_range {
						w.uses_str_range = node.index is ast.RangeExpr
					}
				}
			} else if sym.info is ast.Struct {
				w.mark_by_sym(sym)
			} else if sym.info is ast.SumType {
				w.mark_by_sym(sym)
			} else if sym.kind == .any {
				if !w.is_direct_array_access {
					if node.is_setter && !w.uses_arr_setter {
						w.mark_builtin_array_method_as_used('set')
					} else if !node.is_setter && !w.uses_arr_getter {
						w.mark_builtin_array_method_as_used('get')
					}
				}
			}
		}
		ast.InfixExpr {
			w.expr(node.left)
			tmp_inside_in_op := w.inside_in_op
			w.inside_in_op = node.op in [.key_in, .not_in]
			w.expr(node.right)
			w.inside_in_op = tmp_inside_in_op
			w.or_block(node.or_block)
			if node.left_type != 0 {
				sym := w.table.sym(node.left_type)
				w.mark_by_sym(sym)
				if sym.kind == .struct {
					if opmethod := sym.find_method(node.op.str()) {
						unsafe {
							w.fn_decl(mut &ast.FnDecl(opmethod.source_fn))
						}
					}
				} else {
					if !w.uses_append && node.op == .left_shift && (sym.kind == .array
						|| (sym.kind == .alias && w.table.final_sym(node.left_type).kind == .array)) {
						w.uses_append = true
					}
				}
			}
			right_type := if node.right_type == 0 && mut node.right is ast.TypeNode {
				node.right.typ
			} else {
				node.right_type
			}
			if right_type != 0 {
				right_sym := w.table.sym(right_type)
				if !(w.is_direct_array_access && right_sym.kind == .array) {
					w.mark_by_type(right_type)
				}
				if node.op in [.not_in, .key_in] {
					if right_sym.kind == .map {
						w.features.used_maps++
					}
					if !w.uses_arr_void && !w.is_direct_array_access
						&& right_sym.kind in [.array, .array_fixed] {
						w.uses_arr_void = true
					}
				} else if node.op in [.key_is, .not_is] {
					w.mark_by_sym(right_sym)
				}
			}
			if node.op in [.eq, .ne, .gt, .ge, .lt, .le] {
				if !w.table.used_features.safe_int && node.left_type != 0 && node.right_type != 0 {
					left := w.table.unaliased_type(node.left_type)
					right := w.table.unaliased_type(node.right_type)
					w.table.used_features.safe_int = (
						(left.idx() in [ast.u32_type_idx, ast.u64_type_idx] && right.is_signed())
						|| (right.idx() in [ast.u32_type_idx, ast.u64_type_idx] && left.is_signed()))
				}
				w.uses_eq = w.uses_eq || node.op in [.eq, .ne]
			}
		}
		ast.IfGuardExpr {
			w.expr(node.expr)
			w.mark_by_type(node.expr_type)
			w.uses_guard = true
			if !w.uses_str_index_check && node.expr is ast.IndexExpr && node.expr_type != 0
				&& w.table.final_sym(node.expr_type).kind in [.u8, .string] {
				w.uses_str_index_check = true
			}
		}
		ast.IfExpr {
			w.expr(node.left)
			if branch := w.resolve_comptime_if_branch(node) {
				w.inside_comptime_if++
				defer {
					w.inside_comptime_if--
				}
				w.expr(branch.cond)
				w.stmts(branch.stmts)
				return
			}
			for b in node.branches {
				w.expr(b.cond)
				w.stmts(b.stmts)
			}
		}
		ast.Ident {
			match node.kind {
				.constant {
					w.mark_const_as_used(node.name)
				}
				.function {
					if mut stmt := w.all_fns[node.name] {
						ident_concrete_types :=
							w.resolve_current_concrete_types(node.concrete_types)
						if stmt.generic_names.len > 0
							&& (ident_concrete_types.len == 0 || w.inside_comptime > 0) {
							w.keep_all_fn_generic_types[node.name] = true
						}
						w.fn_decl_with_concrete_types(mut stmt, ident_concrete_types)
					} else {
						w.fn_by_name(node.name)
					}
					if node.info is ast.IdentFn {
						w.mark_by_type(node.info.typ)
					}
				}
				.global {
					w.mark_global_as_used(node.name)
				}
				.blank_ident {}
				else {
					// `.unresolved`, `.variable`
					// println('>>> else, ast.Ident ${node.name} kind: ${node.kind} ')
					if node.name in w.all_consts {
						w.mark_const_as_used(node.name)
					} else if node.name in w.all_globals {
						w.mark_global_as_used(node.name)
					} else {
						if (node.kind == .variable && node.obj is ast.Var && node.obj.is_used
							&& node.obj.typ != 0 && w.table.type_kind(node.obj.typ) == .function)
							|| (node.kind == .unresolved && (node.name.contains('.')
							|| node.name.contains('_T_')))
							|| (node.name.contains('_T_') && node.name in w.all_fns) {
							w.fn_by_name(node.name)
						}
					}
					if !w.uses_atomic && node.info is ast.IdentVar {
						w.uses_atomic = node.info.typ.has_flag(.atomic_f)
					}
				}
			}

			if node.obj is ast.Var && node.obj.is_unwrapped {
				w.used_option++
			}
			w.or_block(node.or_expr)
		}
		ast.LambdaExpr {
			w.expr(node.func)
		}
		ast.Likely {
			w.expr(node.expr)
		}
		ast.MapInit {
			if node.typ == 0 {
				return
			}
			w.exprs(node.keys)
			w.exprs(node.vals)
			if node.has_update_expr {
				w.expr(node.update_expr)
			}
			if node.vals.len > 0 && node.has_update_expr {
				w.uses_map_update = true
			}
			mapinfo := w.table.final_sym(node.typ).map_info()
			ksym := w.table.sym(mapinfo.key_type)
			vsym := w.table.sym(mapinfo.value_type)
			if node.typ.has_flag(.shared_f) {
				w.uses_lock = true
			}
			w.mark_by_sym(ksym)
			w.mark_by_sym(vsym)
			w.features.used_maps++
			w.mark_by_type(node.typ)
		}
		ast.MatchExpr {
			w.expr(node.cond)
			for b in node.branches {
				w.exprs(b.exprs)
				for expr in b.exprs {
					if expr is ast.TypeNode {
						w.mark_by_sym(w.table.sym(expr.typ))
					}
				}
				w.stmts(b.stmts)
			}
		}
		ast.None {
			w.used_none++
		}
		ast.Nil {}
		ast.ParExpr {
			w.expr(node.expr)
		}
		ast.PrefixExpr {
			if !w.uses_memdup && node.op == .amp {
				w.uses_memdup = true
			}
			w.expr(node.right)
		}
		ast.PostfixExpr {
			w.expr(node.expr)
			if node.op == .question && node.expr !is ast.Ident {
				w.used_option++
				w.used_panic++
			}
		}
		ast.RangeExpr {
			if node.has_low {
				w.expr(node.low)
			}
			if node.has_high {
				w.expr(node.high)
			}
		}
		ast.SizeOf, ast.IsRefType {
			w.expr(node.expr)
			w.mark_by_type(node.typ)
		}
		ast.StringInterLiteral {
			w.uses_interp = true
			w.exprs(node.exprs)
			for expr in node.fwidth_exprs {
				if expr !is ast.EmptyExpr {
					w.expr(expr)
				}
			}
			for expr in node.precision_exprs {
				if expr !is ast.EmptyExpr {
					w.expr(expr)
				}
			}
		}
		ast.SelectorExpr {
			w.expr(node.expr)
			if node.expr_type != 0 {
				w.mark_by_type(node.expr_type)
				if method := w.table.find_method(w.table.sym(node.expr_type), node.field_name) {
					w.fn_by_name(method.fkey())
				}
			}
			w.mark_by_type(node.typ)
			w.or_block(node.or_block)
			if node.has_hidden_receiver {
				w.used_closures++
			}
		}
		ast.SqlExpr {
			w.expr(node.db_expr)
			w.expr(node.or_expr)
			w.expr(node.offset_expr)
			w.expr(node.order_expr)
			w.expr(node.limit_expr)
			w.expr(node.where_expr)
			w.mark_by_type(node.typ)
			w.uses_orm = true
		}
		ast.SqlQueryDataExpr {
			for item in node.items {
				match item {
					ast.SqlQueryDataLeaf {
						w.expr(item.expr)
					}
					ast.SqlQueryDataIf {
						for branch in item.branches {
							w.expr(branch.cond)
							for branch_item in branch.items {
								match branch_item {
									ast.SqlQueryDataLeaf {
										w.expr(branch_item.expr)
									}
									ast.SqlQueryDataIf {
										for nested_branch in branch_item.branches {
											w.expr(nested_branch.cond)
											for nested_item in nested_branch.items {
												if nested_item is ast.SqlQueryDataLeaf {
													w.expr(nested_item.expr)
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
			w.mark_by_type(node.typ)
			w.uses_orm = true
		}
		ast.StructInit {
			if node.typ == 0 {
				return
			}
			sym := w.table.sym(node.typ)
			w.mark_by_sym(sym)
			if !w.uses_memdup
				&& (sym.kind == .sum_type || (sym.info is ast.Struct && sym.info.is_heap)) {
				w.uses_memdup = true
			}
			if node.has_update_expr {
				w.expr(node.update_expr)
			}
			for sif in node.init_fields {
				w.expr(sif.expr)
			}
		}
		ast.TypeOf {
			w.expr(node.expr)
			w.mark_by_type(node.typ)
		}
		///
		ast.AsCast {
			w.expr(node.expr)
			w.fn_by_name('__as_cast')
			w.fn_by_name('new_array_from_c_array')
			w.mark_by_sym_name('VCastTypeIndexName')
		}
		ast.AtExpr {}
		ast.BoolLiteral {}
		ast.FloatLiteral {}
		ast.CharLiteral {}
		ast.IntegerLiteral {}
		ast.StringLiteral {
			if !w.uses_str_literal && !node.is_raw {
				w.mark_by_sym_name('string')
				w.uses_str_literal = true
			}
		}
		ast.CTempVar {
			w.expr(node.orig)
		}
		ast.Comment {}
		ast.EnumVal {
			if e := w.table.enum_decls[node.enum_name] {
				filtered := e.fields.filter(it.name == node.val)
				if filtered.len != 0 && filtered[0].expr !is ast.EmptyExpr {
					w.expr(filtered[0].expr)
				}
			}
			w.mark_by_type(node.typ)
		}
		ast.LockExpr {
			w.uses_lock = true
			w.stmts(node.stmts)
		}
		ast.OffsetOf {}
		ast.OrExpr {
			w.or_block(node)
		}
		ast.SelectExpr {
			for branch in node.branches {
				w.stmt(branch.stmt)
				w.stmts(branch.stmts)
			}
			w.uses_channel = true
		}
		ast.TypeNode {
			w.mark_by_type(node.typ)
		}
		ast.UnsafeExpr {
			w.expr(node.expr)
		}
		ast.NodeError {}
	}
}

pub fn (mut w Walker) fn_decl(mut node ast.FnDecl) {
	w.fn_decl_with_concrete_types(mut node, [])
}

fn (mut w Walker) fn_decl_with_concrete_types(mut node ast.FnDecl, concrete_types []ast.Type) {
	if node == unsafe { nil } {
		return
	}
	if w.level == 0 {
		last_is_builtin_mod := w.is_builtin_mod
		w.is_builtin_mod = node.mod in ['builtin', 'os', 'strconv', 'builtin.closure']
		defer(fn) {
			w.is_builtin_mod = last_is_builtin_mod
		}
	}
	w.table.used_features.used_attr_weak = w.table.used_features.used_attr_weak || node.is_weak
	w.table.used_features.used_attr_noreturn = w.table.used_features.used_attr_noreturn
		|| node.is_noreturn
	if node.language == .c {
		w.mark_fn_as_used(node.fkey())
		w.mark_fn_ret_and_params(node.return_type, node.params)
		return
	}

	fkey := node.fkey()
	resolved_concrete_types := w.resolve_current_concrete_types(concrete_types)
	if resolved_concrete_types.len > 0 {
		w.record_used_fn_generic_types(fkey, resolved_concrete_types)
		if resolved_concrete_types in w.walked_fn_generic_types[fkey] {
			w.mark_fn_as_used(fkey)
			return
		}
		w.walked_fn_generic_types[fkey] << resolved_concrete_types.clone()
	} else if w.used_fns[fkey] {
		return
	}
	w.mark_fn_as_used(fkey)
	last_is_direct_array_access := w.is_direct_array_access
	w.is_direct_array_access = node.is_direct_arr || w.pref.no_bounds_checking
	defer { w.is_direct_array_access = last_is_direct_array_access }
	last_cur_fn := w.cur_fn
	last_cur_fn_concrete_types := w.cur_fn_concrete_types.clone()
	defer {
		w.cur_fn = last_cur_fn
		w.cur_fn_concrete_types = last_cur_fn_concrete_types
	}
	if w.trace_enabled {
		w.level++
		defer(fn) { w.level-- }
	}
	if node.is_closure {
		w.used_closures++
	}
	if node.no_body {
		w.mark_fn_as_used(fkey)
		return
	}
	if node.is_method {
		w.mark_by_type(node.receiver.typ)
	}
	w.mark_fn_ret_and_params(node.return_type, node.params)
	w.mark_fn_as_used(fkey)
	// For generic functions, mark the concrete param/return types for all instantiations.
	// This is needed because mark_fn_ret_and_params skips types with .generic flag,
	// but the cgen will emit concrete versions for all fn_generic_types entries.
	generic_names := w.fn_generic_names(node)
	if generic_names.len > 0 {
		mut concrete_type_lists := [][]ast.Type{}
		if resolved_concrete_types.len > 0 {
			concrete_type_lists << resolved_concrete_types.clone()
		}
		for concrete_type_list in w.table.fn_generic_types[fkey] {
			if concrete_type_list !in concrete_type_lists {
				concrete_type_lists << concrete_type_list
			}
		}
		max_param_len := if node.is_method { node.params.len - 1 } else { node.params.len }
		param_i := if node.is_method { 1 } else { 0 }
		for concrete_type_list in concrete_type_lists {
			if node.is_method {
				if resolved := w.table.convert_generic_type(node.receiver.typ, generic_names,
					concrete_type_list)
				{
					w.mark_by_type(resolved)
				}
			}
			// mark concrete return type
			if resolved := w.table.convert_generic_type(node.return_type, generic_names,
				concrete_type_list)
			{
				w.mark_by_type(resolved)
			}
			// mark concrete param types
			for k, concrete_type in concrete_type_list {
				if k >= max_param_len {
					break
				}
				param_typ := node.params[k + param_i].typ
				if resolved := w.table.convert_generic_type(param_typ, generic_names,
					concrete_type_list)
				{
					w.mark_by_type(resolved)
				} else if param_typ.has_flag(.generic) && w.table.type_kind(param_typ) == .array {
					w.mark_by_type(w.table.find_or_register_array(concrete_type))
				}
			}
		}
	}
	prev_cur_fn := w.cur_fn
	w.cur_fn = fkey
	w.cur_fn_concrete_types = resolved_concrete_types
	if node.mod == 'x.json2' && node.name == 'get_decoded_sumtype_workaround'
		&& w.has_sumtype_generic_context(resolved_concrete_types) {
		if mut copy_fn := w.all_fns['x.json2.copy_type'] {
			for concrete_type_list in w.sumtype_variant_concrete_types(resolved_concrete_types) {
				w.fn_decl_with_concrete_types(mut copy_fn, concrete_type_list)
			}
		}
	}
	if node.mod == 'x.json2' && node.name == 'get_struct_type_workaround'
		&& w.has_sumtype_generic_context(resolved_concrete_types) {
		check_struct_type_valid_fkey, _ := w.resolve_method_fkey_for_type(node.receiver.typ,
			'check_struct_type_valid')
		if check_struct_type_valid_fkey != '' {
			if mut check_struct_type_valid_fn := w.all_fns[check_struct_type_valid_fkey] {
				for concrete_type_list in w.sumtype_variant_concrete_types(resolved_concrete_types) {
					if concrete_type_list.len != 1 {
						continue
					}
					concrete_typ := w.table.unaliased_type(concrete_type_list[0])
					if w.table.final_sym(concrete_typ).kind == .struct {
						w.fn_decl_with_concrete_types(mut check_struct_type_valid_fn,
							concrete_type_list)
					}
				}
			}
		}
	}
	if node.mod == 'x.json2' && node.name == 'decode_value' && node.is_method
		&& resolved_concrete_types.len == 1 {
		concrete_typ := w.table.unaliased_type(resolved_concrete_types[0])
		concrete_sym := w.table.final_sym(concrete_typ)
		if concrete_typ.is_number() || concrete_sym.kind == .enum {
			decode_number_fkey, _ := w.resolve_method_fkey_for_type(node.receiver.typ,
				'decode_number')
			if decode_number_fkey != '' {
				if mut decode_number_fn := w.all_fns[decode_number_fkey] {
					w.fn_decl_with_concrete_types(mut decode_number_fn, resolved_concrete_types)
				}
			}
		}
		if concrete_sym.kind == .struct {
			if mut decode_struct_key_fn := w.all_fns['x.json2.decode_struct_key'] {
				w.fn_decl_with_concrete_types(mut decode_struct_key_fn, resolved_concrete_types)
			}
			if mut check_required_struct_fields_fn := w.all_fns['x.json2.check_required_struct_fields'] {
				w.fn_decl_with_concrete_types(mut check_required_struct_fields_fn,
					resolved_concrete_types)
			}
			w.mark_json2_optional_field_helpers(concrete_typ)
		}
	}
	if node.mod == 'x.json2' && node.name == 'decode_struct_key' && resolved_concrete_types.len == 1 {
		w.mark_json2_optional_field_helpers(resolved_concrete_types[0])
	}
	if node.mod == 'x.json2' && node.name == 'encode_value' && node.is_method
		&& resolved_concrete_types.len == 1 {
		concrete_typ := w.table.unaliased_type(resolved_concrete_types[0])
		concrete_sym := w.table.final_sym(concrete_typ)
		if concrete_sym.kind == .sum_type {
			for concrete_type_list in w.sumtype_variant_concrete_types([concrete_typ]) {
				if concrete_type_list != resolved_concrete_types {
					w.fn_decl_with_concrete_types(mut node, concrete_type_list)
				}
			}
		}
		if concrete_sym.kind in [.array, .array_fixed] {
			encode_array_fkey, _ := w.resolve_method_fkey_for_type(node.receiver.typ,
				'encode_array')
			if encode_array_fkey != '' {
				if mut encode_array_fn := w.all_fns[encode_array_fkey] {
					w.fn_decl_with_concrete_types(mut encode_array_fn, resolved_concrete_types)
				}
			}
		}
		if concrete_sym.kind == .map {
			encode_map_fkey, _ := w.resolve_method_fkey_for_type(node.receiver.typ, 'encode_map')
			if encode_map_fkey != '' {
				if mut encode_map_fn := w.all_fns[encode_map_fkey] {
					w.fn_decl_with_concrete_types(mut encode_map_fn, resolved_concrete_types)
				}
			}
		}
		if concrete_sym.kind == .struct {
			w.mark_json2_encode_field_helpers(node.receiver.typ, concrete_typ)
		}
		json_encoder_typ := w.table.find_type('x.json2.JsonEncoder')
		if json_encoder_typ != 0
			&& w.table.does_type_implement_interface(concrete_typ, json_encoder_typ) {
			to_json_fkey, _ := w.resolve_method_fkey_for_type(concrete_typ, 'to_json')
			if to_json_fkey != '' {
				w.fn_by_name(to_json_fkey)
			}
		}
		encodable_typ := w.table.find_type('x.json2.Encodable')
		if encodable_typ != 0 && w.table.does_type_implement_interface(concrete_typ, encodable_typ) {
			json_str_fkey, _ := w.resolve_method_fkey_for_type(concrete_typ, 'json_str')
			if json_str_fkey != '' {
				w.fn_by_name(json_str_fkey)
			}
		}
	}
	if node.mod == 'x.json2' && node.name == 'encode_struct_fields'
		&& resolved_concrete_types.len == 1 {
		w.mark_json2_encode_field_helpers(node.receiver.typ, resolved_concrete_types[0])
	}
	w.stmts(node.stmts)
	w.defer_stmts(node.defer_stmts)
	w.cur_fn = prev_cur_fn
}

fn (mut w Walker) fn_decl_with_fkey(mut node ast.FnDecl, walk_fkey string) {
	if node == unsafe { nil } {
		return
	}
	w.mark_fn_as_used(walk_fkey)
	w.fn_decl_with_concrete_types(mut node, [])
}

fn (w &Walker) receiver_concrete_types(typ ast.Type) []ast.Type {
	sym := w.table.final_sym(typ)
	return match sym.info {
		ast.Struct {
			mut concrete_types := sym.info.concrete_types.clone()
			if concrete_types.len == 0 && sym.generic_types.len == sym.info.generic_types.len
				&& sym.generic_types != sym.info.generic_types {
				concrete_types = sym.generic_types.clone()
			}
			concrete_types.map(it.clear_flag(.generic))
		}
		ast.Interface {
			mut concrete_types := sym.info.concrete_types.clone()
			if concrete_types.len == 0 && sym.generic_types.len == sym.info.generic_types.len
				&& sym.generic_types != sym.info.generic_types {
				concrete_types = sym.generic_types.clone()
			}
			concrete_types.map(it.clear_flag(.generic))
		}
		ast.SumType {
			mut concrete_types := sym.info.concrete_types.clone()
			if concrete_types.len == 0 && sym.generic_types.len == sym.info.generic_types.len
				&& sym.generic_types != sym.info.generic_types {
				concrete_types = sym.generic_types.clone()
			}
			concrete_types.map(it.clear_flag(.generic))
		}
		ast.GenericInst {
			sym.info.concrete_types.map(it.clear_flag(.generic))
		}
		else {
			[]ast.Type{}
		}
	}
}

fn (w &Walker) used_receiver_generic_instantiations(receiver_typ ast.Type) [][]ast.Type {
	if receiver_typ == 0 || !receiver_typ.has_flag(.generic) {
		return []
	}
	receiver_parent := receiver_typ.set_nr_muls(0).set_flag(.generic)
	mut concrete_type_lists := [][]ast.Type{}
	for sym_idx, _ in w.used_syms {
		sym := w.table.type_symbols[sym_idx]
		match sym.info {
			ast.Struct {
				if sym.info.parent_type == receiver_parent && sym.info.concrete_types.len > 0
					&& sym.info.concrete_types !in concrete_type_lists {
					concrete_type_lists << sym.info.concrete_types.clone()
				}
			}
			ast.Interface {
				if sym.info.parent_type == receiver_parent && sym.info.concrete_types.len > 0
					&& sym.info.concrete_types !in concrete_type_lists {
					concrete_type_lists << sym.info.concrete_types.clone()
				}
			}
			ast.SumType {
				if sym.info.parent_type == receiver_parent && sym.info.concrete_types.len > 0
					&& sym.info.concrete_types !in concrete_type_lists {
					concrete_type_lists << sym.info.concrete_types.clone()
				}
			}
			else {}
		}
	}
	return concrete_type_lists
}

pub fn (mut w Walker) call_expr(mut node ast.CallExpr) {
	if node == unsafe { nil } {
		return
	}
	for arg in node.args {
		w.expr(arg.expr)
	}
	if node.is_variadic && node.expected_arg_types.last().has_flag(.option) {
		w.used_option++
	}
	// Check for non-option args passed to option params (cgen wraps with option_ok)
	for i, exp_type in node.expected_arg_types {
		if exp_type.has_flag(.option) && i < node.args.len && !node.args[i].typ.has_flag(.option) {
			w.used_option++
			break
		}
	}
	// Mark function pointer types used in expected arg types, so their typedefs are emitted.
	// When cgen casts an argument to the expected function pointer type, the typedef must exist.
	for exp_type in node.expected_arg_types {
		exp_sym := w.table.sym(exp_type)
		if exp_sym.kind == .function && !exp_sym.name.starts_with('fn ') {
			w.mark_by_type(exp_type)
		}
	}
	source_concrete_types := if node.is_method
		&& node.concrete_types.len > node.raw_concrete_types.len {
		node.concrete_types
	} else if node.raw_concrete_types.len > 0 {
		node.raw_concrete_types
	} else {
		[]ast.Type{}
	}
	mut call_concrete_types := w.resolve_current_concrete_types(source_concrete_types)
	for concrete_type in call_concrete_types {
		w.mark_by_type(concrete_type)
	}
	if node.language == .c {
		if node.name in ['C.wyhash', 'C.wyhash64'] {
			w.features.used_maps++
		}
		w.mark_by_type(node.return_type)
		return
	}
	mut resolved_left_type := node.left_type
	if node.left is ast.Ident {
		left_ident := node.left as ast.Ident
		if left_ident.obj is ast.Var {
			current_specialized_left_type := w.resolve_current_specialized_var_type(left_ident.name)
			if current_specialized_left_type != 0 {
				resolved_left_type = current_specialized_left_type
			} else if left_ident.obj.typ.has_flag(.generic) {
				resolved_left_type = left_ident.obj.typ
			}
		}
	}
	// When inside a generic function, the checker may have resolved
	// the left_type to the last-processed concrete type. Re-resolve
	// through the current concrete types to get the correct type for
	// this specific instantiation.
	if node.is_method && w.cur_fn_concrete_types.len > 0 && w.cur_fn != '' {
		if cur_fn_decl := w.all_fns[w.cur_fn] {
			generic_names := w.fn_generic_names(cur_fn_decl)
			if generic_names.len > 0 && generic_names.len == w.cur_fn_concrete_types.len {
				// Check if resolved_left_type matches any concrete type from a
				// DIFFERENT instantiation of the same generic function. If so,
				// substitute it with the correct one for the current instantiation.
				for concrete_type_list in w.table.fn_generic_types[w.cur_fn] {
					if concrete_type_list.len != generic_names.len {
						continue
					}
					for i, ct in concrete_type_list {
						if ct == resolved_left_type && w.cur_fn_concrete_types[i] != ct {
							resolved_left_type = w.cur_fn_concrete_types[i]
							break
						}
					}
				}
			}
		}
	}
	if node.is_method && resolved_left_type != 0 {
		w.mark_by_type(resolved_left_type)
		left_sym := w.table.sym(resolved_left_type)
		w.uses_type_name = w.uses_type_name
			|| (left_sym.kind in [.sum_type, .interface] && node.name == 'type_name')
		if left_sym.info is ast.Aggregate {
			for receiver_type in left_sym.info.types {
				receiver_sym := w.table.sym(receiver_type)
				if m := receiver_sym.find_method(node.name) {
					fn_name := '${int(m.receiver_type)}.${node.name}'
					if !w.used_fns[fn_name] {
						w.fn_by_name(fn_name)
					}
				}
			}
		} else if left_sym.info is ast.Interface {
			for typ in left_sym.info.types {
				sym := w.table.sym(typ)
				method, embed_types := w.table.find_method_from_embeds(sym, node.name) or {
					ast.Fn{}, []ast.Type{}
				}
				if embed_types.len != 0 {
					w.fn_by_name(method.fkey())
				}
			}
		} else if node.from_embed_types.len != 0 && !resolved_left_type.has_flag(.generic) {
			method, embed_types := w.table.find_method_from_embeds(w.table.final_sym(resolved_left_type),
				node.name) or { ast.Fn{}, []ast.Type{} }
			if embed_types.len != 0 {
				w.fn_by_name(method.fkey())
			}
		} else if node.left_type.has_flag(.generic) || resolved_left_type != node.left_type {
			// Generic type parameter or re-resolved type from generic instantiation.
			// Use resolve_method_fkey_for_type which handles value/reference type lookup.
			concrete_type := if node.left_type.has_flag(.generic) {
				w.resolve_current_generic_type(node.left_type)
			} else {
				resolved_left_type
			}
			if concrete_type != 0 && !concrete_type.has_flag(.generic) {
				resolved_fkey, _ := w.resolve_method_fkey_for_type(concrete_type, node.name)
				method_name := if resolved_fkey != '' {
					resolved_fkey
				} else {
					'${int(concrete_type)}.${node.name}'
				}
				if !w.used_fns[method_name] {
					w.fn_by_name(method_name)
				}
			}
		} else {
			match left_sym.info {
				ast.Array, ast.ArrayFixed {
					if !w.uses_arr_void && node.name in ['contains', 'index', 'last_index'] {
						if w.table.final_sym(left_sym.info.elem_type).kind == .function {
							w.uses_arr_void = true
						}
					}
				}
				else {}
			}
		}
	}
	w.expr(node.left)
	w.or_block(node.or_block)

	mut fn_name := node.fkey()
	mut receiver_typ := node.receiver_type
	if !node.is_method && node.mod != '' {
		qualified_name := '${node.mod}.${node.name}'
		if qualified_name in w.all_fns {
			fn_name = qualified_name
		}
	}
	$if trace_skip_unused_walker ? {
		if node.name in ['decode', 'raw_decode', 'decode_value', 'copy_type', 'from_json_string', 'from_json_number']
			|| fn_name.contains('json2') {
			eprintln('>>> call_expr name=${node.name} mod=${node.mod} fkey=${node.fkey()} fn_name=${fn_name} method=${node.is_method} concrete=${node.concrete_types.map(w.table.type_to_str(it))}')
		}
	}
	if node.is_method {
		resolved_fkey, resolved_receiver := w.resolve_method_call_fkey(node)
		if resolved_fkey != '' {
			fn_name = resolved_fkey
			receiver_typ = resolved_receiver
		}
	}
	if node.is_method {
		if node.left_type != 0 {
			lsym := w.table.sym(node.left_type)
			// Note: maps and arrays are implemented in `builtin` as concrete types `map` and `array`.
			// They are not normal generics expanded, to separate structs, parametrized on the type of the element.
			// All []Type or map[Type]Another types are typedefs to those `map` and `array` types, and all map and array methods
			// are actually methods on the `builtin` concrete types.
			match lsym.kind {
				.array {
					if !w.used_arr_method[node.name] {
						w.mark_builtin_array_method_as_used(node.name)
						w.used_arr_method[node.name] = true
					}
				}
				.map {
					if !w.used_map_method[node.name] {
						w.mark_builtin_map_method_as_used(node.name)
						w.used_map_method[node.name] = true
					}
				}
				else {}
			}
		}
	} else if node.is_fn_a_const {
		const_fn_name := if fn_name.contains('.') {
			fn_name
		} else {
			'${node.mod}.${fn_name}'
		}
		if const_fn_name in w.all_consts {
			w.mark_const_as_used(const_fn_name)
		}
	} else if node.is_fn_var {
		w.mark_global_as_used(node.name)
	}
	if node.is_method && node.receiver_type.has_flag(.generic) {
		if call_concrete_types.len == 0 && node.left_type != 0 && !node.left_type.has_flag(.generic) {
			call_concrete_types = w.receiver_concrete_types(node.left_type)
			receiver_typ = node.left_type
		}
		if call_concrete_types.len == 0 && node.receiver_concrete_type != 0
			&& !node.receiver_concrete_type.has_flag(.generic) {
			call_concrete_types = w.receiver_concrete_types(node.receiver_concrete_type)
			receiver_typ = node.receiver_concrete_type
		}
		concrete_fn_name := '${int(receiver_typ)}.${node.name}'
		if concrete_fn_name in w.all_fns {
			fn_name = concrete_fn_name
		}
	}
	// Handle concrete instantiations of generic struct methods: when the
	// receiver type is a concrete instantiation (e.g. Vec3[f64]) but the
	// method is only registered under the parent generic type key (Vec3[T]),
	// remap fn_name to the generic key and preserve all registered generic
	// types so that other instantiations are not stripped.
	if node.is_method && fn_name !in w.all_fns && !node.receiver_type.has_flag(.generic)
		&& receiver_typ != 0 {
		rsym := w.table.sym(receiver_typ)
		parent_type := match rsym.info {
			ast.Struct { rsym.info.parent_type }
			ast.Interface { rsym.info.parent_type }
			ast.SumType { rsym.info.parent_type }
			ast.GenericInst { ast.new_type(rsym.info.parent_idx) }
			else { ast.Type(0) }
		}

		if parent_type != 0 && parent_type.has_flag(.generic) {
			generic_fn_name := '${int(parent_type.set_nr_muls(0))}.${node.name}'
			if generic_fn_name in w.all_fns {
				fn_name = generic_fn_name
				call_concrete_types = w.receiver_concrete_types(receiver_typ)
				receiver_typ = parent_type.set_nr_muls(0)
				w.keep_all_fn_generic_types[generic_fn_name] = true
			}
		}
	}
	w.mark_by_type(node.return_type)
	mut resolved_fn_name := fn_name
	if fn_name !in w.all_fns {
		if fn_name != node.fkey() && node.fkey() in w.all_fns {
			resolved_fn_name = node.fkey()
		} else {
			return
		}
	}
	if mut stmt := w.all_fns[resolved_fn_name] {
		if !stmt.should_be_skipped && stmt.name == node.name {
			caller_generic_names, caller_concrete_types := w.current_generic_context()
			if call_concrete_types.len == 0 {
				call_concrete_types = w.trusted_source_concrete_types(node, receiver_typ)
			}
			if call_concrete_types.len == 0 {
				callee_generic_names := w.fn_generic_names(stmt)
				if caller_generic_names.len > 0
					&& caller_generic_names.len == caller_concrete_types.len
					&& callee_generic_names.len > 0 {
					mut inherited_concrete_types := []ast.Type{cap: callee_generic_names.len}
					for generic_name in callee_generic_names {
						idx := caller_generic_names.index(generic_name)
						if idx < 0 || idx >= caller_concrete_types.len {
							inherited_concrete_types = []
							break
						}
						inherited_concrete_types << caller_concrete_types[idx]
					}
					if inherited_concrete_types.len == callee_generic_names.len
						&& inherited_concrete_types.all(!it.has_flag(.generic)) {
						call_concrete_types = inherited_concrete_types.clone()
					}
				}
			}
			if call_concrete_types.len == 0 && node.raw_concrete_types.len == 0 {
				call_concrete_types = w.resolve_current_concrete_types(node.concrete_types)
			}
			for concrete_type in call_concrete_types {
				w.mark_by_type(concrete_type)
			}
			generic_call_inside_generic_caller := w.fn_generic_names(stmt).len > 0
				&& node.raw_concrete_types.len == 0 && caller_generic_names.len > 0
			keep_all_generic_types := (stmt.generic_names.len > 0 && call_concrete_types.len == 0)
				|| generic_call_inside_generic_caller
			if keep_all_generic_types {
				w.keep_all_fn_generic_types[fn_name] = true
			}
			can_walk_method := !node.is_method || receiver_typ == stmt.receiver.typ
				|| (node.receiver_type.has_flag(.generic) && stmt.receiver.typ.has_flag(.generic)
				&& call_concrete_types.len > 0)
			if can_walk_method {
				if keep_all_generic_types {
					for concrete_type_list in w.table.fn_generic_types[fn_name] {
						w.fn_decl_with_concrete_types(mut stmt, concrete_type_list)
					}
				} else {
					w.fn_decl_with_concrete_types(mut stmt, call_concrete_types)
					if node.raw_concrete_types.len == 0 && w.inside_comptime > 0
						&& w.has_sumtype_generic_context(caller_concrete_types)
						&& stmt.mod == 'x.json2' && stmt.name in ['copy_type', 'decode_value'] {
						for concrete_type_list in w.sumtype_variant_concrete_types(caller_concrete_types) {
							if concrete_type_list != call_concrete_types {
								w.fn_decl_with_concrete_types(mut stmt, concrete_type_list)
							}
						}
					}
				}
			}
			if node.return_type.has_flag(.option) {
				w.used_option++
			} else if node.return_type.has_flag(.result) {
				w.used_result++
			}
			callee_generic_names := w.fn_generic_names(stmt)
			if ((node.is_method && stmt.params.len > 1) || !node.is_method)
				&& callee_generic_names.len > 0 {
				// mark concrete generic param types (e.g. []T, ...Node[T]) as used
				max_param_len := if node.is_method { stmt.params.len - 1 } else { stmt.params.len }
				param_i := if node.is_method { 1 } else { 0 }
				concrete_type_lists := if call_concrete_types.len > 0 {
					[call_concrete_types]
				} else {
					w.table.fn_generic_types[fn_name]
				}
				for concrete_type_list in concrete_type_lists {
					for k, concrete_type in concrete_type_list {
						if k >= max_param_len {
							break
						}
						param_typ := stmt.params[k + param_i].typ
						if param_typ.has_flag(.generic) {
							if resolved := w.table.convert_generic_type(param_typ,
								callee_generic_names, concrete_type_list)
							{
								w.mark_by_type(resolved)
							} else if w.table.type_kind(param_typ) == .array {
								w.mark_by_type(w.table.find_or_register_array(concrete_type))
							} else if param_typ.has_flag(.option) {
								w.used_option++
							}
						}
					}
				}
			}
		}
	}
}

fn (w &Walker) resolve_method_fkey_for_type(typ ast.Type, method_name string) (string, ast.Type) {
	mut candidate_types := []ast.Type{}
	for candidate in [typ] {
		if candidate == 0 {
			continue
		}
		if candidate !in candidate_types {
			candidate_types << candidate
		}
		unaliased := w.table.unaliased_type(candidate)
		if unaliased != 0 && unaliased != candidate && unaliased !in candidate_types {
			candidate_types << unaliased
		}
		if !candidate.is_ptr() {
			ref_typ := candidate.ref()
			if ref_typ != 0 && ref_typ !in candidate_types {
				candidate_types << ref_typ
			}
		}
		if candidate.is_ptr() {
			deref_typ := candidate.deref()
			if deref_typ != 0 && deref_typ !in candidate_types {
				candidate_types << deref_typ
			}
		}
	}
	for candidate in candidate_types {
		sym := w.table.sym(candidate)
		parent_fkey, parent_receiver := w.generic_parent_method_fkey(sym, method_name)
		if parent_fkey != '' {
			return parent_fkey, parent_receiver
		}
		if method := sym.find_method_with_generic_parent(method_name) {
			return w.method_decl_fkey(method)
		}
		if method := w.table.find_method(sym, method_name) {
			return w.method_decl_fkey(method)
		}
		method, embed_types := w.table.find_method_from_embeds(w.table.final_sym(candidate),
			method_name) or { ast.Fn{}, []ast.Type{} }
		if embed_types.len != 0 {
			return w.method_decl_fkey(method)
		}
	}
	return '', ast.no_type
}

fn (w &Walker) generic_parent_method_fkey(sym ast.TypeSymbol, method_name string) (string, ast.Type) {
	match sym.info {
		ast.Struct, ast.Interface, ast.SumType {
			if sym.info.parent_type.has_flag(.generic) {
				parent_sym := w.table.sym(sym.info.parent_type)
				if method := parent_sym.find_method(method_name) {
					return w.method_decl_fkey(method)
				}
			}
		}
		ast.GenericInst {
			if sym.info.parent_idx > 0 {
				parent_sym := w.table.sym(ast.idx_to_type(sym.info.parent_idx))
				if method := parent_sym.find_method(method_name) {
					return w.method_decl_fkey(method)
				}
			}
		}
		else {}
	}

	return '', ast.no_type
}

fn (w &Walker) method_decl_fkey(method ast.Fn) (string, ast.Type) {
	if method.source_fn != unsafe { nil } {
		fndecl := unsafe { &ast.FnDecl(method.source_fn) }
		return fndecl.fkey(), fndecl.receiver.typ
	}
	return method.fkey(), method.receiver_type
}

fn (w &Walker) resolve_method_call_fkey(node ast.CallExpr) (string, ast.Type) {
	mut candidate_types := []ast.Type{}
	if node.left is ast.Ident && node.left.obj is ast.Var {
		resolved_current_type := w.resolve_current_specialized_var_type(node.left.name)
		if resolved_current_type != 0 && resolved_current_type !in candidate_types {
			candidate_types << resolved_current_type
		} else if node.left.obj.typ.has_flag(.generic) {
			generic_names, concrete_types := w.specialized_generic_context_for(w.cur_fn)
			if generic_names.len > 0 && generic_names.len == concrete_types.len {
				mut muttable := unsafe { &ast.Table(w.table) }
				if resolved := muttable.convert_generic_type(node.left.obj.typ, generic_names,
					concrete_types)
				{
					resolved_type := resolved.clear_flag(.generic)
					if resolved_type != 0 && resolved_type !in candidate_types {
						candidate_types << resolved_type
					}
				}
			}
		}
	}
	for typ in [node.left_type, node.receiver_concrete_type, node.receiver_type] {
		if typ == 0 {
			continue
		}
		if typ !in candidate_types {
			candidate_types << typ
		}
		unaliased := w.table.unaliased_type(typ)
		if unaliased != 0 && unaliased != typ && unaliased !in candidate_types {
			candidate_types << unaliased
		}
		if !typ.is_ptr() {
			ref_typ := typ.ref()
			if ref_typ != 0 && ref_typ !in candidate_types {
				candidate_types << ref_typ
			}
		}
		if typ.is_ptr() {
			deref_typ := typ.deref()
			if deref_typ != 0 && deref_typ !in candidate_types {
				candidate_types << deref_typ
			}
		}
	}
	for typ in candidate_types {
		resolved_fkey, resolved_receiver := w.resolve_method_fkey_for_type(typ, node.name)
		if resolved_fkey != '' {
			return resolved_fkey, resolved_receiver
		}
	}
	return '', ast.no_type
}

fn (w &Walker) current_fn_concrete_types_list() [][]ast.Type {
	if w.cur_fn == '' {
		return [][]ast.Type{}
	}
	generic_names, concrete_types := w.specialized_generic_context_for(w.cur_fn)
	if generic_names.len > 0 && generic_names.len == concrete_types.len {
		return [concrete_types]
	}
	return w.table.fn_generic_types[w.cur_fn] or { [][]ast.Type{} }
}

fn (w &Walker) specialized_generic_context_for(fn_name string) ([]string, []ast.Type) {
	if fn_name == '' || !fn_name.contains('_T_') {
		return []string{}, []ast.Type{}
	}
	for generic_fn in w.generic_fns {
		if generic_fn.generic_names.len == 0 {
			continue
		}
		for base_name in [generic_fn.name, generic_fn.fkey()] {
			if !fn_name.starts_with(base_name + '_T_') {
				continue
			}
			for concrete_types in w.table.fn_generic_types[generic_fn.fkey()] {
				if concrete_types.any(it.has_flag(.generic)) {
					continue
				}
				if w.generic_concrete_name(base_name, concrete_types) == fn_name {
					return generic_fn.generic_names.clone(), concrete_types.clone()
				}
			}
		}
	}
	return []string{}, []ast.Type{}
}

fn (w &Walker) resolve_current_specialized_type(typ ast.Type) ast.Type {
	if typ == 0 {
		return ast.no_type
	}
	generic_names, concrete_types := w.specialized_generic_context_for(w.cur_fn)
	if generic_names.len == 0 || generic_names.len != concrete_types.len {
		return typ.clear_flag(.generic)
	}
	mut muttable := unsafe { &ast.Table(w.table) }
	if resolved := muttable.convert_generic_type(typ, generic_names, concrete_types) {
		return resolved.clear_flag(.generic)
	}
	return typ.clear_flag(.generic)
}

fn (w &Walker) generic_concrete_name(base_name string, concrete_types []ast.Type) string {
	mut name := base_name
	if concrete_types.len > 0 {
		name += '_T'
	}
	for typ in concrete_types {
		name += '_' + w.table.sym(typ.set_nr_muls(0)).scoped_cname()
	}
	return name
}

fn (w &Walker) resolve_current_specialized_var_type(var_name string) ast.Type {
	if var_name == '' || w.cur_fn == '' {
		return ast.no_type
	}
	mut base_name := w.cur_fn
	if w.cur_fn.contains('_T_') {
		base_name = w.cur_fn.all_before('_T_')
	}
	mut base_fn := w.all_fns[base_name] or { ast.FnDecl{} }
	if base_fn.name == '' {
		for generic_fn in w.generic_fns {
			if generic_fn.name == base_name || generic_fn.fkey() == base_name {
				base_fn = *generic_fn
				break
			}
		}
		if base_fn.name == '' {
			return ast.no_type
		}
	}
	mut generic_names := []string{}
	mut concrete_types := []ast.Type{}
	if w.cur_fn.contains('_T_') {
		generic_names, concrete_types = w.specialized_generic_context_for(w.cur_fn)
	} else if w.cur_fn_concrete_types.len > 0
		&& base_fn.generic_names.len == w.cur_fn_concrete_types.len {
		generic_names = base_fn.generic_names.clone()
		concrete_types = w.cur_fn_concrete_types.clone()
	}
	if generic_names.len == 0 || generic_names.len != concrete_types.len {
		return ast.no_type
	}
	for param in base_fn.params {
		if param.name != var_name {
			continue
		}
		mut muttable := unsafe { &ast.Table(w.table) }
		if resolved := muttable.convert_generic_type(param.typ, generic_names, concrete_types) {
			return resolved.clear_flag(.generic)
		}
		return param.typ.clear_flag(.generic)
	}
	return ast.no_type
}

fn (w &Walker) receiver_concrete_types_for_type(typ ast.Type) []ast.Type {
	if typ == 0 {
		return []ast.Type{}
	}
	sym := w.table.sym(typ)
	match sym.info {
		ast.Struct, ast.Interface, ast.SumType {
			mut concrete_types := sym.info.concrete_types.clone()
			if concrete_types.len == 0 && sym.generic_types.len == sym.info.generic_types.len
				&& sym.generic_types != sym.info.generic_types {
				concrete_types = sym.generic_types.clone()
			}
			return concrete_types.map(it.clear_flag(.generic))
		}
		ast.GenericInst {
			return sym.info.concrete_types.map(it.clear_flag(.generic))
		}
		else {
			return []ast.Type{}
		}
	}
}

fn (w &Walker) call_specialization_fkey(node ast.CallExpr, stmt ast.FnDecl, base_fkey string, receiver_typ ast.Type) string {
	if base_fkey == '' || stmt.generic_names.len == 0 {
		return base_fkey
	}
	source_concrete_types := if node.raw_concrete_types.len > 0 {
		node.raw_concrete_types
	} else {
		node.concrete_types
	}
	mut concrete_types := []ast.Type{}
	for concrete_type in source_concrete_types {
		resolved_type := w.resolve_current_specialized_type(concrete_type)
		if resolved_type == 0 || resolved_type.has_flag(.generic) {
			return base_fkey
		}
		concrete_types << resolved_type
	}
	if node.is_method && concrete_types.len < stmt.generic_names.len {
		receiver_generic_names := if stmt.params.len > 0 {
			mut muttable := unsafe { &ast.Table(w.table) }
			muttable.generic_type_names(stmt.params[0].typ)
		} else {
			[]string{}
		}
		if receiver_generic_names.len > 0 {
			mut receiver_concrete_types := w.receiver_concrete_types_for_type(receiver_typ)
			if receiver_concrete_types.len == 0 && node.receiver_concrete_type != 0 {
				receiver_concrete_types =
					w.receiver_concrete_types_for_type(node.receiver_concrete_type)
			}
			if receiver_concrete_types.len > 0 {
				if concrete_types.len == 0 {
					concrete_types = receiver_concrete_types.clone()
				} else if receiver_generic_names.len + concrete_types.len == stmt.generic_names.len {
					mut merged_types := receiver_concrete_types.clone()
					merged_types << concrete_types
					concrete_types = merged_types.clone()
				}
			}
		}
	}
	if concrete_types.len == 0 || concrete_types.len != stmt.generic_names.len {
		return base_fkey
	}
	return w.generic_concrete_name(base_fkey, concrete_types)
}

pub fn (mut w Walker) fn_by_name(fn_name string) {
	if w.used_fns[fn_name] {
		return
	}
	if mut stmt := w.all_fns[fn_name] {
		w.fn_decl(mut stmt)
	} else {
		if fn_name.contains('_T_') {
			base_name := fn_name.all_before('_T_')
			if mut base_fn := w.all_fns[base_name] {
				w.fn_decl_with_fkey(mut base_fn, fn_name)
			} else {
				for generic_fn in w.generic_fns {
					if generic_fn.name == base_name || generic_fn.fkey() == base_name {
						mut gfn := *generic_fn
						w.fn_decl_with_fkey(mut gfn, fn_name)
						break
					}
				}
			}
		}
	}
}

pub fn (mut w Walker) struct_fields(sfields []ast.StructField) {
	for sf in sfields {
		if sf.has_default_expr {
			w.expr(sf.default_expr)
		}
		if !w.uses_atomic && sf.typ.has_flag(.atomic_f) {
			w.uses_atomic = true
		}
	}
}

pub fn (mut w Walker) const_fields(cfields []ast.ConstField) {
	for cf in cfields {
		w.expr(cf.expr)
	}
}

fn (w &Walker) infer_expr_type(expr ast.Expr) ast.Type {
	match expr {
		ast.ArrayInit {
			return expr.typ
		}
		ast.AsCast {
			return expr.typ
		}
		ast.BoolLiteral {
			return ast.bool_type
		}
		ast.CallExpr {
			return expr.return_type
		}
		ast.CastExpr {
			return expr.typ
		}
		ast.ChanInit {
			return expr.typ
		}
		ast.CharLiteral {
			return ast.u8_type
		}
		ast.FloatLiteral {
			return ast.f64_type
		}
		ast.Ident {
			return match expr.obj {
				ast.AsmRegister {
					expr.obj.typ
				}
				ast.ConstField {
					expr.obj.typ
				}
				ast.GlobalField {
					expr.obj.typ
				}
				ast.Var {
					if expr.obj.smartcasts.len > 0 {
						expr.obj.smartcasts.last()
					} else {
						expr.obj.typ
					}
				}
				else {
					ast.void_type
				}
			}
		}
		ast.IfExpr {
			return expr.typ
		}
		ast.IndexExpr {
			return expr.typ
		}
		ast.IntegerLiteral {
			return ast.int_type
		}
		ast.MapInit {
			return expr.typ
		}
		ast.MatchExpr {
			return expr.return_type
		}
		ast.ParExpr {
			return w.infer_expr_type(expr.expr)
		}
		ast.PostfixExpr {
			return expr.typ
		}
		ast.PrefixExpr {
			return expr.right_type
		}
		ast.SelectorExpr {
			return expr.typ
		}
		ast.StringInterLiteral, ast.StringLiteral {
			return ast.string_type
		}
		ast.UnsafeExpr {
			return w.infer_expr_type(expr.expr)
		}
		else {
			return ast.void_type
		}
	}
}

@[inline]
pub fn (mut w Walker) or_block(node ast.OrExpr) {
	if node.kind == .block {
		w.uses_err_block = true
		w.stmts(node.stmts)
	} else if node.kind == .propagate_option {
		w.used_option++
		w.used_panic++
	} else if node.kind == .propagate_result {
		w.used_result++
		w.used_panic++
	}
}

pub fn (mut w Walker) mark_fn_ret_and_params(return_type ast.Type, params []ast.Param) {
	if return_type != 0 {
		if return_type.has_flag(.option) {
			w.used_option++
		} else if return_type.has_flag(.result) {
			w.used_result++
		}
		w.mark_by_type(return_type.clear_option_and_result())
	}
	for param in params {
		w.mark_by_type(param.typ)
	}
}

@[inline]
pub fn (mut w Walker) mark_by_sym_name(name string) {
	if sym := w.table.find_sym(name) {
		w.mark_by_sym(sym)
	}
}

@[inline]
pub fn (mut w Walker) mark_by_type(typ ast.Type) {
	if typ == 0 || typ.has_flag(.generic) {
		return
	}
	cleared_typ := typ.clear_option_and_result()
	if cleared_typ != typ {
		w.mark_by_type(cleared_typ)
	}
	if typ in w.used_types {
		return
	}
	w.mark_by_sym(w.table.sym(typ))
	w.used_types[typ] = true
}

pub fn (mut w Walker) mark_by_sym(isym ast.TypeSymbol) {
	if isym.idx in w.used_syms {
		return
	}
	w.used_syms[isym.idx] = true
	match isym.info {
		ast.Struct {
			for ifield in isym.info.fields {
				if ifield.has_default_expr {
					w.expr(ifield.default_expr)
				}
				if ifield.typ != 0 {
					fsym := w.table.sym(ifield.typ.idx())
					if ifield.typ.has_flag(.option) {
						w.used_option++
						if !ifield.has_default_expr {
							w.used_none++
						}
					}
					w.mark_by_sym(fsym)
				}
				if !w.features.auto_str_ptr && ifield.typ.is_ptr()
					&& isym.idx in w.features.print_types {
					w.features.auto_str_ptr = true
				}
			}
			for embed in isym.info.embeds {
				w.mark_by_type(embed)
			}
			if decl := w.all_structs[isym.name] {
				w.struct_fields(decl.fields)
				w.uses_mem_align = w.uses_mem_align || decl.is_aligned
				for iface_typ in decl.implements_types {
					w.mark_by_type(iface_typ.typ)
					iface_sym := w.table.sym(iface_typ.typ)
					for method in iface_sym.methods {
						if impl_method := isym.find_method_with_generic_parent(method.name) {
							w.fn_by_name(impl_method.fkey())
						} else {
							impl_method, _ := w.table.find_method_from_embeds(isym, method.name) or {
								ast.Fn{}, []ast.Type{}
							}
							if impl_method.name != '' {
								w.fn_by_name(impl_method.fkey())
							}
						}
					}
				}
			}
		}
		ast.ArrayFixed, ast.Array {
			if !w.uses_array && !w.is_direct_array_access {
				w.uses_array = true
			}
			if isym.info.elem_type.has_flag(.option) {
				w.used_option++
			}
			w.mark_by_type(isym.info.elem_type)
		}
		ast.SumType {
			for typ in isym.info.variants {
				if typ == ast.map_type {
					w.features.used_maps++
					continue
				}
				if typ.has_flag(.option) {
					w.used_option++
				}
				sym := w.table.sym(typ)
				w.mark_by_sym(sym)
				w.uses_array_sumtype = w.uses_array_sumtype || sym.kind == .array
			}
		}
		ast.Map {
			w.mark_by_type(isym.info.key_type)
			w.mark_by_type(isym.info.value_type)
			w.features.used_maps++
			if isym.info.value_type.has_flag(.option) {
				w.used_option++
			}
		}
		ast.Alias {
			w.mark_by_type(isym.info.parent_type)
		}
		ast.FnType {
			for param in isym.info.func.params {
				w.mark_by_type(param.typ)
			}
			if isym.info.func.return_type != 0 {
				w.mark_by_type(isym.info.func.return_type.clear_option_and_result())
			}
		}
		ast.MultiReturn {
			for typ in isym.info.types {
				w.mark_by_type(typ)
			}
		}
		ast.Chan {
			w.uses_channel = true
			w.mark_by_type(isym.info.elem_type)
		}
		ast.Aggregate {
			for typ in isym.info.types {
				w.mark_by_type(typ)
			}
		}
		ast.GenericInst {
			parent_typ := ast.new_type(isym.info.parent_idx)
			w.mark_by_type(parent_typ)
			for concrete_type in isym.info.concrete_types {
				w.mark_by_type(concrete_type)
			}
			parent_sym := w.table.sym(parent_typ)
			match parent_sym.info {
				ast.Struct {
					generic_names := parent_sym.info.generic_types.map(w.table.sym(it).name)
					for field in parent_sym.info.fields {
						if resolved := w.table.convert_generic_type(field.typ, generic_names,
							isym.info.concrete_types)
						{
							w.mark_by_type(resolved)
						} else {
							w.mark_by_type(field.typ)
						}
					}
					for embed in parent_sym.info.embeds {
						if resolved := w.table.convert_generic_type(embed, generic_names,
							isym.info.concrete_types)
						{
							w.mark_by_type(resolved)
						} else {
							w.mark_by_type(embed)
						}
					}
				}
				ast.SumType {
					generic_names := parent_sym.info.generic_types.map(w.table.sym(it).name)
					for variant in parent_sym.info.variants {
						if resolved := w.table.convert_generic_type(variant, generic_names,
							isym.info.concrete_types)
						{
							w.mark_by_type(resolved)
						} else {
							w.mark_by_type(variant)
						}
					}
				}
				else {}
			}
		}
		ast.Enum {
			w.mark_by_type(isym.info.typ)
			if enum_ := w.table.enum_decls[isym.name] {
				for field in enum_.fields {
					if field.has_expr {
						w.expr(field.expr)
					}
				}
			}
		}
		ast.Interface {
			for typ in isym.info.types {
				if typ == ast.map_type {
					w.features.used_maps++
				}
				w.mark_by_type(typ)
				for method in isym.info.methods {
					for ityp in [typ.set_nr_muls(1), typ.set_nr_muls(0)] {
						mname := int(ityp.clear_flags()).str() + '.' + method.name
						w.fn_by_name(mname)
					}
				}
				for embed_method in w.table.get_embed_methods(w.table.sym(typ)) {
					mname := int(embed_method.params[0].typ.clear_flags()).str() + '.' +
						embed_method.name
					w.fn_by_name(mname)
				}
			}
			for embed in isym.info.embeds {
				w.mark_by_type(embed)
			}
			for generic_type in isym.info.generic_types {
				w.mark_by_type(generic_type)
			}
			w.mark_by_type(isym.info.parent_type)
			for field in isym.info.fields {
				w.mark_by_type(field.typ)
			}
			for method in isym.methods {
				w.mark_by_type(method.receiver_type)
				w.mark_fn_ret_and_params(method.return_type, method.params)
			}
		}
		ast.Thread {
			w.mark_by_type(isym.info.return_type)
		}
		else {}
	}
}

fn (mut w Walker) remove_unused_fn_generic_types() {
	// Phase 1: Compute the union of concrete type lists per generic
	// receiver type, grouped by arity (number of type parameters).
	// Include types from ALL methods (walked and un-walked) in fn_generic_types,
	// so that every method on the same receiver type ends up with a consistent
	// set of concrete instantiations.
	mut receiver_used_types := map[string]map[int][][]ast.Type{}
	// First, collect from walked methods (used_fn_generic_types)
	for nkey, concrete_types in w.used_fn_generic_types {
		if fn_decl := w.all_fns[nkey] {
			if fn_decl.receiver.typ.has_flag(.generic) {
				rkey := int(fn_decl.receiver.typ).str()
				actual_types := if w.keep_all_fn_generic_types[nkey] {
					w.table.fn_generic_types[nkey]
				} else {
					concrete_types
				}
				for ctl in actual_types {
					arity := ctl.len
					if ctl !in receiver_used_types[rkey][arity] {
						receiver_used_types[rkey][arity] << ctl
					}
				}
			}
		}
	}
	// Also collect from un-walked methods in fn_generic_types whose receiver
	// type is already in used_syms (the struct instantiation is used).
	for fkey, fgt_types in w.table.fn_generic_types {
		if fkey in w.used_fn_generic_types || w.keep_all_fn_generic_types[fkey] {
			continue
		}
		if fn_decl := w.all_fns[fkey] {
			if fn_decl.receiver.typ.has_flag(.generic) {
				rkey := int(fn_decl.receiver.typ).str()
				for ctl in fgt_types {
					arity := ctl.len
					if ctl !in receiver_used_types[rkey][arity] {
						receiver_used_types[rkey][arity] << ctl
					}
				}
			}
		}
	}
	// Phase 2: Strip walked methods.  For methods on generic receiver types,
	// use the arity-matched union to ensure consistency across all methods on
	// the same receiver type.
	for nkey, concrete_types in w.used_fn_generic_types {
		if concrete_types.len == 0 || w.keep_all_fn_generic_types[nkey] {
			continue
		}
		if fn_decl := w.all_fns[nkey] {
			if fn_decl.receiver.typ.has_flag(.generic) {
				rkey := int(fn_decl.receiver.typ).str()
				if arity_map := receiver_used_types[rkey] {
					if concrete_types.len > 0 {
						arity := concrete_types[0].len
						if union_types := arity_map[arity] {
							w.table.fn_generic_types[nkey] = union_types.clone()
							continue
						}
					}
				}
			}
		}
		w.table.fn_generic_types[nkey] = concrete_types.clone()
	}
	// Phase 3: Strip un-walked methods on generic receiver types to the
	// arity-matched union.  This ensures un-walked methods don't keep a wider
	// set of concrete types than walked methods, preventing C errors when
	// un-walked method bodies call walked (stripped) methods.
	// Also collect these un-walked methods so we can walk them in Phase 4.
	mut unwalked_methods := map[string]bool{}
	for fkey, fgt_types in w.table.fn_generic_types {
		if fkey in w.used_fn_generic_types || w.keep_all_fn_generic_types[fkey] {
			continue
		}
		if fgt_types.len == 0 {
			continue
		}
		mut rkey := ''
		if fn_decl := w.all_fns[fkey] {
			if fn_decl.receiver.typ.has_flag(.generic) {
				rkey = int(fn_decl.receiver.typ).str()
			}
		}
		if rkey == '' {
			dot_idx := fkey.index_u8(`.`)
			if dot_idx > 0 {
				rkey = fkey[..dot_idx]
			}
		}
		if rkey == '' {
			continue
		}
		if arity_map := receiver_used_types[rkey] {
			if fgt_types.len > 0 {
				arity := fgt_types[0].len
				if union_types := arity_map[arity] {
					w.table.fn_generic_types[fkey] = union_types.clone()
					unwalked_methods[fkey] = true
				}
			}
		}
	}
	// Phase 4: Walk un-walked method bodies so that any internal method
	// calls (e.g. ec.generate_id() inside wait()) get properly marked.
	// Iterate until no new un-walked methods are discovered.
	// Also walk already-walked methods that got new concrete types from Phase 2.
	// Phase 2 may add new concrete type lists (from the receiver union) that
	// were not walked during the initial traversal.
	for nkey, used_concrete_types in w.used_fn_generic_types {
		if w.keep_all_fn_generic_types[nkey] {
			continue
		}
		for concrete_type_list in w.table.fn_generic_types[nkey] {
			if concrete_type_list !in used_concrete_types {
				unwalked_methods[nkey] = true
			}
		}
	}
	for unwalked_methods.len > 0 {
		mut next_unwalked := map[string]bool{}
		for fkey, _ in unwalked_methods {
			if mut fn_decl := w.all_fns[fkey] {
				for concrete_type_list in w.table.fn_generic_types[fkey] {
					w.fn_decl_with_concrete_types(mut fn_decl, concrete_type_list)
				}
			}
		}
		// Check if walking those methods discovered new un-walked methods
		for fkey2, fgt_types2 in w.table.fn_generic_types {
			if fkey2 in w.used_fn_generic_types || w.keep_all_fn_generic_types[fkey2]
				|| fkey2 in unwalked_methods {
				continue
			}
			if fgt_types2.len == 0 {
				continue
			}
			mut rkey2 := ''
			if fn_decl2 := w.all_fns[fkey2] {
				if fn_decl2.receiver.typ.has_flag(.generic) {
					rkey2 = int(fn_decl2.receiver.typ).str()
				}
			}
			if rkey2 == '' {
				dot_idx := fkey2.index_u8(`.`)
				if dot_idx > 0 {
					rkey2 = fkey2[..dot_idx]
				}
			}
			if rkey2 == '' {
				continue
			}
			if fkey2 !in w.used_fn_generic_types {
				if arity_map := receiver_used_types[rkey2] {
					if fgt_types2.len > 0 {
						arity := fgt_types2[0].len
						if union_types := arity_map[arity] {
							w.table.fn_generic_types[fkey2] = union_types.clone()
							next_unwalked[fkey2] = true
						}
					}
				}
			}
		}
		unwalked_methods = next_unwalked.move()
	}
	// Phase 5: Propagate newly-discovered concrete types from walking
	// (used_fn_generic_types / walked_fn_generic_types) back into
	// fn_generic_types so that the cgen emits all required instantiations.
	// Phase 4 may walk generic functions (including non-method ones) with
	// new concrete types that were not in fn_generic_types originally.
	for fkey, walked_types in w.walked_fn_generic_types {
		for ctl in walked_types {
			if ctl !in w.table.fn_generic_types[fkey] {
				w.table.fn_generic_types[fkey] << ctl
			}
		}
	}
}

fn (mut w Walker) mark_resource_dependencies() {
	string_idx_str := ast.string_type_idx.str()
	array_idx_str := ast.array_type_idx.str()

	if w.pref.gc_mode == .boehm_leak {
		// `-gc boehm_leak` emits scope-exit frees for used heap-backed values.
		w.uses_free[ast.string_type] = true
	}

	if w.trace_enabled {
		eprintln('>>>>>>>>>> DEPS USAGE')
	}
	if w.features.dump {
		w.fn_by_name('eprint')
		w.fn_by_name('eprintln')
		builderptr_idx := int(w.table.find_type('strings.Builder').ref()).str()
		w.fn_by_name(builderptr_idx + '.str')
		w.fn_by_name(builderptr_idx + '.free')
		w.fn_by_name(builderptr_idx + '.write_rune')
		w.fn_by_name(builderptr_idx + '.write_string')
		w.fn_by_name('strings.new_builder')
		w.uses_free[ast.string_type] = true

		if w.table.dumps.keys().any(ast.Type(u32(it)).has_flag(.option)) {
			w.fn_by_name('str_intp')
		}
	}
	if w.features.auto_str_ptr {
		w.fn_by_name('isnil')
		w.fn_by_name('tos4')
		w.fn_by_name('str_intp')
	}
	if w.uses_channel {
		w.fn_by_name('sync.new_channel_st')
		w.fn_by_name('sync.channel_select')
		w.fn_by_name('sync.channel_select_lang')
	}
	if w.uses_lock {
		w.mark_by_sym_name('sync.RwMutex')
	}
	if w.uses_orm {
		w.fn_by_name('__new_array_with_default_noscan')
		w.fn_by_name('new_array_from_c_array')
		w.fn_by_name('__new_array')
		w.fn_by_name('${ast.array_type_idx}.get')
		w.fn_by_name(int(ast.array_type.ref()).str() + '.push')
	}
	if w.uses_ct_fields {
		w.mark_by_sym_name('FieldData')
	}
	if w.uses_ct_methods {
		w.mark_by_sym_name('FunctionData')
	}
	if w.uses_ct_params {
		w.mark_by_sym_name('FunctionParam')
	}
	if w.uses_ct_values {
		w.mark_by_sym_name('EnumData')
	}
	if w.uses_ct_variants {
		w.mark_by_sym_name('VariantData')
	}
	if w.uses_ct_attribute {
		w.mark_by_sym_name('VAttribute')
	}
	if w.uses_map_update {
		w.fn_by_name('new_map_update_init')
	}
	if w.uses_mem_align {
		w.fn_by_name('memdup_align')
	}
	if w.uses_spawn {
		w.fn_by_name('malloc')
		w.fn_by_name('tos3')
	}
	if w.uses_memdup || w.used_none > 0 || w.used_option > 0 {
		// used_option => used_none => use memdup
		w.fn_by_name('memdup')
	}
	if w.uses_debugger {
		w.mark_by_type(w.table.find_or_register_map(ast.string_type, ast.string_type))
	}
	if w.uses_arr_void {
		w.mark_by_type(w.table.find_or_register_array(ast.voidptr_type))
	}
	if w.features.auto_str || w.uses_dump {
		w.fn_by_name(ast.string_type_idx.str() + '.repeat')
		w.fn_by_name('tos3')
		builderptr_idx := int(w.table.find_type('strings.Builder').ref()).str()
		w.fn_by_name(builderptr_idx + '.write_string')
		w.fn_by_name(builderptr_idx + '.writeln')
		w.fn_by_name(builderptr_idx + '.indent')
	}
	if w.uses_index || w.pref.is_shared {
		w.fn_by_name(array_idx_str + '.slice')
		w.fn_by_name(array_idx_str + '.get')
	}
	if w.uses_str_index {
		w.fn_by_name(string_idx_str + '.at')
		if w.uses_str_index_check {
			w.fn_by_name(string_idx_str + '.at_with_check')
		}
		if w.uses_str_range {
			w.fn_by_name(string_idx_str + '.substr')
		}
	}
	for typ, _ in w.table.used_features.print_types {
		w.mark_by_type(typ)
	}
	if w.trace_enabled {
		ptypes := w.table.used_features.print_types.keys().map(w.table.type_to_str(it))
		eprintln('>>>>>>>>>> PRINT TYPES ${ptypes}')
		stypes := w.uses_str.keys().filter(it != 0).map(w.table.type_to_str(it))
		eprintln('>>>>>>>>>> USES .str() CALLS ON TYPES ${stypes}')
		ftypes := w.uses_free.keys().map(w.table.type_to_str(it))
		eprintln('>>>>>>>>>> USES .free() CALLS ON TYPES ${ftypes}')
	}
	if w.trace_enabled {
		eprintln('>>>>>>>>>> ALL_FNS LOOP')
	}
	mut has_ptr_print := false
	mut map_fns := map[string]ast.FnDecl{}
	has_str_call := w.uses_interp || w.uses_asserts || w.uses_str.len > 0
		|| w.features.print_types.len > 0

	orm_impls := w.table.iface_types['orm.Connection'] or { []ast.Type{} }
	for k, mut func in w.all_fns {
		if has_str_call && k.ends_with('.str') {
			if func.receiver.typ.has_flag(.generic) {
				concrete_type_lists := w.used_receiver_generic_instantiations(func.receiver.typ)
				if concrete_type_lists.len > 0 {
					for concrete_type_list in concrete_type_lists {
						w.fn_decl_with_concrete_types(mut func, concrete_type_list)
					}
				} else if func.receiver.typ.idx() in w.used_syms {
					w.fn_by_name(k)
				}
				if !has_ptr_print && func.receiver.typ.is_ptr() {
					w.fn_by_name('ptr_str')
					has_ptr_print = true
				}
			} else if func.receiver.typ.idx() in w.used_syms {
				w.fn_by_name(k)
				if !has_ptr_print && func.receiver.typ.is_ptr() {
					w.fn_by_name('ptr_str')
					has_ptr_print = true
				}
			}
			continue
		}
		if (w.pref.autofree || (w.uses_free.len > 0 && func.receiver.typ.idx() in w.used_syms))
			&& k.ends_with('.free') {
			w.fn_by_name(k)
			continue
		}
		if w.uses_atomic && k.starts_with('_Atomic') {
			w.fn_by_name(k)
			continue
		}
		if func.name in ['+', '-', '*', '%', '/', '<', '=='] {
			if func.receiver.typ.idx() in w.used_syms {
				w.fn_by_name(k)
			}
			continue
		}
		if !func.is_static_type_method && func.receiver.typ != ast.void_type
			&& func.generic_names.len > 0 {
			if func.receiver.typ.set_nr_muls(0) in w.table.used_features.comptime_syms
				|| func.receiver.typ in w.table.used_features.comptime_syms {
				w.fn_by_name(k)
			}
			continue
		}
		if func.is_method && !func.receiver.typ.has_flag(.generic) && func.receiver.typ.is_ptr() {
			method_receiver_typename := w.table.type_to_str(func.receiver.typ)
			if method_receiver_typename in ['&map', '&mapnode', '&SortedMap', '&DenseArray'] {
				map_fns[k] = func
			}
			continue
		} else if k.starts_with('map_') {
			map_fns[k] = func
			continue
		}
		if orm_impls.len > 0 && k.starts_with('orm.') {
			w.fn_by_name(k)
			continue
		}
	}
	if w.uses_err_block {
		w.fn_by_name('error')
	}
	if w.uses_guard || w.uses_index_check {
		w.fn_by_name('error')
		w.fn_by_name(array_idx_str + '.get_with_check')
	}
	if w.uses_append {
		ref_array_idx_str := int(ast.array_type.ref()).str()
		w.fn_by_name(ref_array_idx_str + '.push')
		w.fn_by_name(ref_array_idx_str + '.push_many')
		w.fn_by_name(ref_array_idx_str + '.push_many_noscan')
		w.fn_by_name(ref_array_idx_str + '.push_noscan')
	}
	if w.uses_array {
		if w.pref.gc_mode in [.boehm_full_opt, .boehm_incr_opt, .vgc] {
			w.fn_by_name('__new_array_noscan')
			w.fn_by_name('new_array_from_c_array_noscan')
			w.fn_by_name('__new_array_with_multi_default_noscan')
			w.fn_by_name('__new_array_with_array_default_noscan')
			w.fn_by_name('__new_array_with_default_noscan')
		}
		w.fn_by_name('__new_array')
		w.fn_by_name('new_array_from_c_array')
		w.fn_by_name('__new_array_with_multi_default')
		w.fn_by_name('__new_array_with_array_default')
		w.fn_by_name('__new_array_with_default')
		w.fn_by_name('__new_array_with_default_noscan')
		w.fn_by_name(int(ast.array_type.ref()).str() + '.set')
		w.fn_by_name('clone_static_to_depth')
	}
	if w.uses_array_sumtype {
		w.fn_by_name('__new_array')
	}
	if w.uses_fixed_arr_int {
		w.fn_by_name('v_fixed_index')
	}
	if w.uses_str_range_index {
		w.fn_by_name(string_idx_str + '.substr')
	}
	if w.uses_arr_range_index {
		w.fn_by_name(array_idx_str + '.slice')
	}
	if w.uses_range_index_check {
		w.fn_by_name(string_idx_str + '.substr_with_check')
		w.fn_by_name(array_idx_str + '.get_with_check')
	}
	if w.uses_str_range_index_gated {
		w.fn_by_name(string_idx_str + '.substr_ni')
	}
	if w.uses_arr_range_index_gated {
		w.fn_by_name(array_idx_str + '.slice_ni')
	}
	if w.uses_array || w.uses_arr_clone || w.uses_arr_sorted {
		w.fn_by_name(array_idx_str + '.clone_static_to_depth')
	}
	// handle ORM drivers:
	if orm_impls.len > 0 {
		for orm_type in orm_impls {
			typ := int(orm_type).str()
			w.fn_by_name(typ + '.select')
			w.fn_by_name(typ + '.insert')
			w.fn_by_name(typ + '.update')
			w.fn_by_name(typ + '.delete')
			w.fn_by_name(typ + '.create')
			w.fn_by_name(typ + '.drop')
			w.fn_by_name(typ + '.last_id')
		}
	}
	if w.features.used_maps == 0 && w.pref.autofree {
		w.features.used_maps++
	}
	if w.features.used_maps > 0 {
		w.fn_by_name('new_map')
		w.fn_by_name('new_map_init')
		w.fn_by_name('map_hash_string')

		if w.pref.gc_mode in [.boehm_full_opt, .boehm_incr_opt, .vgc] {
			w.fn_by_name('new_map_noscan_key')
			w.fn_by_name('new_map_noscan_value')
			w.fn_by_name('new_map_noscan_key_value')
			w.fn_by_name('new_map_init_noscan_key')
			w.fn_by_name('new_map_init_noscan_value')
			w.fn_by_name('new_map_init_noscan_key_value')
		}
		for _, mut func in map_fns {
			if !func.is_method {
				w.fn_decl(mut func)
			} else {
				method_receiver_typename := w.table.type_to_str(func.receiver.typ)
				if method_receiver_typename in ['&map', '&DenseArray'] {
					w.fn_decl(mut func)
				}
			}
		}
	} else {
		for k, func in map_fns {
			if !func.is_method {
				continue
			}
			w.used_fns.delete(k)
		}
		w.used_fns.delete('new_map')
		w.used_fns.delete('new_map_init')
		w.used_fns.delete('map_hash_string')
		w.used_fns.delete('new_dense_array')
		w.used_fns.delete('new_dense_array_noscan')
	}
}

pub fn (mut w Walker) finalize(include_panic_deps bool) {
	w.mark_resource_dependencies()
	if w.trace_enabled {
		eprintln('>>>>>>>>>> FINALIZE')
	}
	if w.uses_asserts {
		w.fn_by_name('__print_assert_failure')
		w.fn_by_name('isnil')
		w.mark_by_sym_name('VAssertMetaInfo')
	}
	if w.used_panic > 0 {
		w.fn_by_name('panic_option_not_set')
		w.fn_by_name('panic_result_not_set')
	}
	if w.used_none > 0 || w.table.used_features.auto_str {
		w.fn_by_name('_option_none')
		w.mark_by_sym_name('_option')
	}
	if w.used_option > 0 {
		w.fn_by_name('_option_clone')
		w.fn_by_name('_option_ok')
		w.mark_by_sym_name('_option')
	}
	if w.used_result > 0 {
		w.fn_by_name('_result_ok')
		w.mark_by_sym_name('_result')
	}
	if (w.used_option + w.used_result + w.used_none) > 0 {
		w.mark_const_as_used('none__')
	}
	if include_panic_deps || w.uses_external_type || w.uses_asserts || w.uses_debugger
		|| w.uses_interp {
		if w.trace_enabled {
			eprintln('>>>>> PANIC DEPS ${include_panic_deps} | external_type=${w.uses_external_type} | asserts=${w.uses_asserts} | dbg=${w.uses_debugger} interp=${w.uses_interp}')
		}
		ref_array_idx_str := int(ast.array_type.ref()).str()
		string_idx_str := ast.string_type_idx.str()

		if w.pref.gc_mode in [.boehm_full_opt, .boehm_incr_opt] {
			w.fn_by_name('__new_array_with_default_noscan')
			w.fn_by_name(ref_array_idx_str + '.push_noscan')
		}
		w.fn_by_name('str_intp')
		w.fn_by_name('__new_array_with_default')
		w.fn_by_name(ref_array_idx_str + '.push')
		w.fn_by_name(string_idx_str + '.substr')
		w.fn_by_name('v_fixed_index')
		w.mark_by_sym_name('StrIntpData')
		w.mark_by_sym_name('StrIntpMem')
	}
	if w.uses_eq {
		w.fn_by_name('fast_string_eq')
	}
	if w.uses_type_name {
		charptr_idx_str := ast.charptr_type_idx.str()
		w.fn_by_name(charptr_idx_str + '.vstring_literal')
	}
	if w.used_arr_method['map'] || w.used_arr_method['filter'] {
		ref_array_idx_str := int(ast.array_type.ref()).str()
		w.fn_by_name(ref_array_idx_str + '.push')
		w.fn_by_name(ref_array_idx_str + '.push_noscan')
	}
	// remove unused symbols
	w.remove_unused_fn_generic_types()

	if w.trace_enabled {
		syms := w.used_syms.keys().map(w.table.type_to_str(it))
		eprintln('>>>>>>>>>> USED SYMS ${syms}')
	}
}

pub fn (mut w Walker) mark_generic_types() {
	if w.trace_enabled {
		eprintln('>>>>>>>>>> COMPTIME SYMS+CALLS')
	}
	for k, _ in w.table.used_features.comptime_calls {
		w.fn_by_name(k)
	}

	for k, _ in w.table.used_features.comptime_syms {
		sym := w.table.sym(k)
		w.mark_by_sym(sym)
		for method in sym.get_methods() {
			w.fn_by_name('${k}.${method.name}')
		}
	}
}
