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
	all_consts    map[string]ast.ConstField
	all_globals   map[string]ast.GlobalField
	all_fields    map[string]ast.StructField
	all_decltypes map[string]ast.TypeDecl
	all_structs   map[string]ast.StructDecl

	cur_fn                 string
	level                  int
	is_builtin_mod         bool
	is_direct_array_access bool

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
	uses_fixed_arr_int         bool // fixed_arr[k]
	uses_append                bool // var << item
	uses_map_setter            bool
	uses_map_getter            bool
	uses_arr_setter            bool
	uses_arr_getter            bool
	uses_arr_clone             bool
	uses_arr_sorted            bool
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

@[inline]
pub fn (mut w Walker) mark_builtin_array_method_as_used(method_name string) {
	w.mark_builtin_type_method_as_used('${ast.array_type_idx}.${method_name}', '${int(ast.array_type.ref())}.${method_name}')
}

@[inline]
pub fn (mut w Walker) mark_builtin_map_method_as_used(method_name string) {
	w.mark_builtin_type_method_as_used('${ast.map_type_idx}.${method_name}', '${int(ast.map_type.ref())}.${method_name}')
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
				cond_type_sym := w.table.sym(node.cond_type)
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
			w.mark_by_type(node.typ)
			w.expr(node.len_expr)
			w.expr(node.cap_expr)
			w.expr(node.init_expr)
			w.exprs(node.exprs)
			if !w.uses_array && !w.is_direct_array_access {
				w.uses_array = true
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
			if node.name == 'json.decode' {
				w.mark_by_type((node.args[0].expr as ast.TypeNode).typ)
			} else if node.name == 'json.encode' && node.args[0].typ != 0 {
				sym := w.table.final_sym(node.args[0].typ)
				if sym.info is ast.Map {
					w.mark_by_type(w.table.find_or_register_array(sym.info.key_type))
				}
			} else if !node.is_method && node.args.len == 1 && node.args[0].typ != ast.string_type
				&& node.name in ['println', 'print', 'eprint', 'eprintln'] {
				w.uses_str[node.args[0].typ] = true
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
		}
		ast.CastExpr {
			w.expr(node.expr)
			w.expr(node.arg)
			if !w.uses_memdup {
				fsym := w.table.final_sym(node.typ)
				w.uses_memdup = fsym.kind == .sum_type
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
		}
		ast.ComptimeCall {
			w.expr(node.left)
			if node.is_vweb {
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
			if node.or_expr.kind != .absent {
				w.uses_index_check = true
			}
			w.mark_by_type(node.typ)
			w.or_block(node.or_expr)
			if node.left_type == 0 {
				return
			}
			sym := w.table.final_sym(node.left_type)
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
				if !w.uses_arr_range_index {
					w.uses_arr_range_index = true
				}
				if !w.uses_fixed_arr_int && sym.kind == .array_fixed {
					w.uses_fixed_arr_int = true
				}
				if !w.uses_index && !w.is_direct_array_access {
					w.uses_index = true
				}
				if !w.uses_arr_range_index_gated {
					w.uses_arr_range_index_gated = node.is_gated
				}
			} else if sym.kind == .string {
				w.uses_str_index = true
				if node.index is ast.RangeExpr {
					if !w.uses_str_range_index {
						w.uses_str_range_index = true
					}
					if !w.uses_range_index_check {
						w.uses_range_index_check = node.or_expr.kind == .block
					}
					if !w.uses_str_range_index_gated {
						w.uses_str_range_index_gated = node.is_gated
					}
				} else {
					if !w.uses_str_index_check {
						w.uses_str_index_check = node.or_expr.kind == .block
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
			w.expr(node.right)
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
				w.mark_by_type(right_type)
				right_sym := w.table.sym(right_type)
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
					w.fn_by_name(node.name)
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
					// println('>>> else, ast.Ident ${node.name} kind: $node.kind ')
					if node.name in w.all_consts {
						w.mark_const_as_used(node.name)
					} else if node.name in w.all_globals {
						w.mark_global_as_used(node.name)
					} else {
						w.fn_by_name(node.name)
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
		ast.StructInit {
			if node.typ == 0 {
				return
			}
			sym := w.table.sym(node.typ)
			w.mark_by_sym(sym)
			if !w.uses_memdup && sym.kind == .sum_type {
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
		ast.StringLiteral {}
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
	if node == unsafe { nil } {
		return
	}
	if w.level == 0 {
		last_is_builtin_mod := w.is_builtin_mod
		w.is_builtin_mod = node.mod in ['builtin', 'os', 'strconv', 'builtin.closure']
		defer {
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
	if w.used_fns[fkey] {
		return
	}
	last_is_direct_array_access := w.is_direct_array_access
	w.is_direct_array_access = node.is_direct_arr || w.pref.no_bounds_checking
	defer { w.is_direct_array_access = last_is_direct_array_access }
	if w.trace_enabled {
		w.level++
		defer { w.level-- }
		receiver_name := if node.is_method && node.receiver.typ != 0 {
			w.table.type_to_str(node.receiver.typ) + '.'
		} else {
			''
		}
		eprintln('>>>${'  '.repeat(w.level)}${receiver_name}${node.name}')
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
	w.cur_fn = fkey
	w.stmts(node.stmts)
	w.defer_stmts(node.defer_stmts)
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
	for concrete_type in node.concrete_types {
		w.mark_by_type(concrete_type)
	}
	if node.language == .c {
		if node.name in ['C.wyhash', 'C.wyhash64'] {
			w.features.used_maps++
		}
		w.mark_by_type(node.return_type)
		return
	}
	if node.is_method && node.left_type != 0 {
		w.mark_by_type(node.left_type)
		left_sym := w.table.sym(node.left_type)
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
				_, embed_types := w.table.find_method_from_embeds(sym, node.name) or {
					ast.Fn{}, []ast.Type{}
				}
				if embed_types.len != 0 {
					fn_embed := '${int(embed_types.last())}.${node.name}'
					w.fn_by_name(fn_embed)
				}
			}
		} else if node.from_embed_types.len != 0 && !node.left_type.has_flag(.generic) {
			_, embed_types := w.table.find_method_from_embeds(w.table.final_sym(node.left_type),
				node.name) or { ast.Fn{}, []ast.Type{} }
			if embed_types.len != 0 {
				fn_embed := '${int(embed_types.last())}.${node.name}'
				w.fn_by_name(fn_embed)
			}
		} else if node.left_type.has_flag(.generic) {
			if w.cur_fn != '' {
				if concrete_types_list := w.table.fn_generic_types[w.cur_fn] {
					for concrete_types in concrete_types_list {
						for concrete_type in concrete_types {
							method_name := '${int(concrete_type)}.${node.name}'
							if !w.used_fns[method_name] {
								w.fn_by_name(method_name)
							}
						}
					}
				}
			}
		} else {
			match left_sym.info {
				ast.Array, ast.ArrayFixed {
					if !w.uses_arr_void && node.name in ['contains', 'index'] {
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
	if w.used_fns[fn_name] {
		return
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
		const_fn_name := '${node.mod}.${fn_name}'
		if const_fn_name in w.all_consts {
			w.mark_const_as_used(const_fn_name)
		}
	} else if node.is_fn_var {
		w.mark_global_as_used(node.name)
	}
	if node.is_method && node.receiver_type.has_flag(.generic) && node.receiver_concrete_type != 0
		&& !node.receiver_concrete_type.has_flag(.generic) {
		// if receiver is generic, then cgen requires `node.receiver_type` to be T.
		// We therefore need to get the concrete type from `node.receiver_concrete_type`.
		fn_name = '${int(node.receiver_concrete_type)}.${node.name}'
		receiver_typ = node.receiver_concrete_type
	}
	w.mark_by_type(node.return_type)
	mut stmt := w.all_fns[fn_name] or { return }
	if !stmt.should_be_skipped && stmt.name == node.name {
		if !node.is_method || receiver_typ == stmt.receiver.typ {
			w.fn_decl(mut stmt)
		}
		if node.return_type.has_flag(.option) {
			w.used_option++
		} else if node.return_type.has_flag(.result) {
			w.used_result++
		}
	}
}

pub fn (mut w Walker) fn_by_name(fn_name string) {
	if w.used_fns[fn_name] {
		return
	}
	mut stmt := w.all_fns[fn_name] or { return }
	w.fn_decl(mut stmt)
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
	if typ == 0 || typ.has_flag(.generic) || typ in w.used_types {
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
				}
			}
		}
		ast.ArrayFixed, ast.Array {
			if !w.uses_array && !w.is_direct_array_access {
				w.uses_array = true
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
	for _, node in w.all_fns {
		mut count := 0
		nkey := node.fkey()
		if all_concrete_types := w.table.fn_generic_types[nkey] {
			if all_concrete_types.len == 0 {
				continue
			}
			for k, concrete_types in all_concrete_types {
				if concrete_types.len != 1 {
					continue
				}
				if concrete_types[0].idx() !in w.used_syms {
					w.table.fn_generic_types[nkey].delete(k - count)
					count++
				}
			}
		}
	}
}

fn (mut w Walker) mark_resource_dependencies() {
	string_idx_str := ast.string_type_idx.str()
	array_idx_str := ast.array_type_idx.str()

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
	if w.uses_memdup {
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
			if func.receiver.typ.idx() in w.used_syms {
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
		if w.pref.gc_mode in [.boehm_full_opt, .boehm_incr_opt] {
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

		if w.pref.gc_mode in [.boehm_full_opt, .boehm_incr_opt] {
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
