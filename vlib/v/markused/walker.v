// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module markused

// Walk the entire program starting at fn main and marks used (called) functions.
// Unused functions can be safely skipped by the backends to save CPU time and space.
import v.ast
import v.pref

pub struct Walker {
pub mut:
	table        &ast.Table        = unsafe { nil }
	features     &ast.UsedFeatures = unsafe { nil }
	used_fns     map[string]bool // used_fns['println'] == true
	used_consts  map[string]bool // used_consts['os.args'] == true
	used_globals map[string]bool
	used_fields  map[string]bool
	used_syms    map[int]bool
	used_none    int // _option_none
	used_option  int // _option_ok
	used_result  int // _result_ok
	used_panic   int // option/result propagation
	used_interp  int // str interpolation
	n_asserts    int
	pref         &pref.Preferences = unsafe { nil }
mut:
	files         []&ast.File
	all_fns       map[string]ast.FnDecl
	all_consts    map[string]ast.ConstField
	all_globals   map[string]ast.GlobalField
	all_fields    map[string]ast.StructField
	all_decltypes map[string]ast.Type
	all_structs   map[string]ast.StructDecl
}

pub fn Walker.new(params Walker) &Walker {
	mut new_walker := &Walker{
		...params
	}
	new_walker.features = params.table.used_features
	return new_walker
}

@[inline]
pub fn (mut w Walker) mark_fn_as_used(fkey string) {
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
		w.mark_fn_as_used(k)
	} else if mut cfn := w.all_fns[rk] {
		w.fn_decl(mut cfn)
		w.mark_fn_as_used(rk)
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
	w.expr(gfield.expr)
	if !gfield.has_expr && gfield.typ != 0 {
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
		if constfield.is_markused {
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
	for _, typ in w.all_decltypes {
		w.mark_by_type(typ)
	}
}

pub fn (mut w Walker) mark_struct_field_default_expr() {
	for sfkey, mut structfield in w.all_fields {
		if structfield.has_default_expr {
			w.mark_struct_field_default_expr_as_used(sfkey)
		}
	}
}

pub fn (mut w Walker) stmt(node_ ast.Stmt) {
	mut node := unsafe { node_ }
	match mut node {
		ast.EmptyStmt {}
		ast.DebuggerStmt {}
		ast.AsmStmt {
			w.asm_io(node.output)
			w.asm_io(node.input)
		}
		ast.AssertStmt {
			if node.is_used {
				w.n_asserts++
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
					w.mark_by_sym_name('VAttribute')
				}
				.variants {
					w.mark_by_sym_name('VariantData')
				}
				.params {
					w.mark_by_sym_name('MethodParam')
				}
				.values {
					w.mark_by_sym_name('EnumData')
				}
				.fields {
					w.mark_by_sym_name('FieldData')
				}
				.methods {
					w.mark_by_sym_name('FunctionData')
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
			if node.kind == .map {
				w.features.used_maps++
			} else if node.kind == .array {
				w.features.used_arrays++
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
		}
		ast.StructDecl {
			for typ in node.implements_types {
				w.mark_by_type(typ.typ)
			}
			w.struct_fields(node.fields)
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
			w.mark_by_type(node.elem_type)
			w.expr(node.len_expr)
			w.expr(node.cap_expr)
			w.expr(node.init_expr)
			w.exprs(node.exprs)
			w.features.used_arrays++
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
			}
		}
		ast.CastExpr {
			w.expr(node.expr)
			w.expr(node.arg)
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
			if node.is_embed {
				w.features.used_maps++
			}
		}
		ast.DumpExpr {
			w.expr(node.expr)
			w.fn_by_name('eprint')
			w.fn_by_name('eprintln')
			if node.expr_type != 0 {
				w.mark_by_type(node.expr_type)
			}
		}
		ast.SpawnExpr {
			if node.is_expr {
				w.fn_by_name('free')
			}
			w.expr(node.call_expr)
			w.fn_by_name('tos3')
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
			w.expr(node.call_expr)
		}
		ast.IndexExpr {
			w.expr(node.left)
			w.expr(node.index)
			w.or_block(node.or_expr)
			if node.left_type == 0 {
				return
			}
			sym := w.table.final_sym(node.left_type)
			if sym.info is ast.Map {
				if node.is_setter {
					w.mark_builtin_map_method_as_used('set')
				} else {
					w.mark_builtin_map_method_as_used('get')
				}
				w.mark_by_sym(w.table.sym(sym.info.key_type))
				w.mark_by_sym(w.table.sym(sym.info.value_type))
				w.features.used_maps++
			} else if sym.info is ast.Array {
				if node.is_setter {
					w.mark_builtin_array_method_as_used('set')
				} else {
					w.mark_builtin_array_method_as_used('get')
				}
				w.mark_by_sym(w.table.sym(sym.info.elem_type))
				w.features.used_arrays++
			} else if sym.kind == .string {
				if node.index is ast.RangeExpr {
					w.mark_builtin_array_method_as_used('slice')
					w.features.range_index = true
				}
			} else if sym.info is ast.Struct {
				w.mark_by_sym(sym)
			} else if sym.info is ast.SumType {
				w.mark_by_sym(sym)
			}
		}
		ast.InfixExpr {
			w.expr(node.left)
			w.expr(node.right)
			w.or_block(node.or_block)
			if node.left_type != 0 {
				sym := w.table.sym(node.left_type)
				if sym.kind == .struct {
					if opmethod := sym.find_method(node.op.str()) {
						unsafe {
							w.fn_decl(mut &ast.FnDecl(opmethod.source_fn))
						}
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
				if node.op in [.not_in, .key_in] {
					if right_sym.kind == .map {
						w.features.used_maps++
					} else if right_sym.kind == .array {
						w.features.used_arrays++
					}
				} else if node.op in [.key_is, .not_is] {
					w.mark_by_sym(right_sym)
				}
			}
		}
		ast.IfGuardExpr {
			w.expr(node.expr)
			if node.expr_type != 0 {
				w.mark_by_type(node.expr_type)
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
				else {
					// `.unresolved`, `.blank_ident`, `.variable`, `.function`
					// println('>>> else, ast.Ident ${node.name} kind: $node.kind ')					
					if node.name in w.all_consts {
						w.mark_const_as_used(node.name)
					} else if node.name in w.all_globals {
						w.mark_global_as_used(node.name)
					} else {
						w.fn_by_name(node.name)
					}
				}
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
			mapinfo := w.table.final_sym(node.typ).map_info()
			ksym := w.table.sym(mapinfo.key_type)
			vsym := w.table.sym(mapinfo.value_type)
			if node.typ.has_flag(.shared_f) {
				if sym := w.table.find_sym('sync.RwMutex') {
					w.mark_by_sym(sym)
				}
			}
			w.mark_by_sym(ksym)
			w.mark_by_sym(vsym)
			w.features.used_maps++
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
			if node.typ != 0 {
				w.mark_by_type(node.typ)
			}
		}
		ast.StringInterLiteral {
			w.used_interp++
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
			if node.typ != 0 {
				w.mark_by_type(node.typ)
			}
			w.or_block(node.or_block)
		}
		ast.SqlExpr {
			w.expr(node.db_expr)
			w.expr(node.or_expr)
			w.expr(node.offset_expr)
			w.expr(node.order_expr)
			w.expr(node.limit_expr)
			w.expr(node.where_expr)
			w.mark_by_type(node.typ)
		}
		ast.StructInit {
			if node.typ == 0 {
				return
			}
			sym := w.table.sym(node.typ)
			w.mark_by_sym(sym)
			if node.has_update_expr {
				w.expr(node.update_expr)
			}
			for sif in node.init_fields {
				w.expr(sif.expr)
			}
		}
		ast.TypeOf {
			w.expr(node.expr)
			if node.typ != 0 {
				w.mark_by_type(node.typ)
			}
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
			w.mark_by_sym_name(node.enum_name)
		}
		ast.LockExpr {
			if sym := w.table.find_sym('sync.RwMutex') {
				w.mark_by_sym(sym)
			}
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
	if node.language == .c {
		w.mark_fn_as_used(node.fkey())
		w.mark_fn_ret_and_params(node.return_type, node.params)
		return
	}
	fkey := node.fkey()
	if w.used_fns[fkey] {
		return
	}
	if node.no_body {
		return
	}
	if node.is_method {
		w.mark_by_type(node.receiver.typ)
	}
	w.mark_fn_ret_and_params(node.return_type, node.params)
	w.mark_fn_as_used(fkey)
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
	for concrete_type in node.concrete_types {
		w.mark_by_type(concrete_type)
	}
	if node.language == .c {
		if node.name in ['C.wyhash', 'C.wyhash64'] {
			w.features.used_maps++
		}
		if node.return_type != 0 {
			w.mark_by_type(node.return_type)
		}
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
				.array { w.mark_builtin_array_method_as_used(node.name) }
				.map { w.mark_builtin_map_method_as_used(node.name) }
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
	w.mark_fn_as_used(fn_name)
	if node.is_method && node.receiver_type.has_flag(.generic) && node.receiver_concrete_type != 0
		&& !node.receiver_concrete_type.has_flag(.generic) {
		// if receiver is generic, then cgen requires `node.receiver_type` to be T.
		// We therefore need to get the concrete type from `node.receiver_concrete_type`.
		fn_name = '${int(node.receiver_concrete_type)}.${node.name}'
		receiver_typ = node.receiver_concrete_type
		w.mark_fn_as_used(fn_name)
	}
	stmt := w.all_fns[fn_name] or { return }
	if !stmt.should_be_skipped && stmt.name == node.name {
		if !node.is_method || receiver_typ == stmt.receiver.typ {
			w.mark_fn_ret_and_params(stmt.return_type, stmt.params)
			w.stmts(stmt.stmts)
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
	stmt := w.all_fns[fn_name] or { return }
	w.mark_fn_as_used(fn_name)
	w.mark_fn_ret_and_params(stmt.return_type, stmt.params)
	w.stmts(stmt.stmts)
}

pub fn (mut w Walker) struct_fields(sfields []ast.StructField) {
	for sf in sfields {
		if sf.has_default_expr {
			w.expr(sf.default_expr)
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
		w.stmts(node.stmts)
	} else if node.kind == .propagate_option {
		w.used_option++
		w.used_panic++
	} else if node.kind == .propagate_result {
		w.used_result++
		w.used_panic++
	}
}

pub fn (mut w Walker) mark_panic_deps() {
	ref_array_idx_str := int(ast.array_type.ref()).str()
	string_idx_str := ast.string_type_idx.str()
	array_idx_str := ast.array_type_idx.str()
	charptr_idx_str := ast.charptr_type_idx.str()

	w.fn_by_name('__new_array_with_default')
	w.fn_by_name('__new_array_with_default_noscan')
	w.fn_by_name('str_intp')
	w.fn_by_name(ref_array_idx_str + '.push')
	w.fn_by_name(ref_array_idx_str + '.push_noscan')
	w.fn_by_name(string_idx_str + '.substr')
	w.fn_by_name(array_idx_str + '.slice')
	w.fn_by_name(array_idx_str + '.get')
	w.fn_by_name('v_fixed_index')
	w.fn_by_name(charptr_idx_str + '.vstring_literal')

	w.mark_by_sym_name('StrIntpData')
	w.mark_by_sym_name('StrIntpMem')
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

pub fn (mut w Walker) mark_by_sym_name(name string) {
	if sym := w.table.find_sym(name) {
		w.mark_by_sym(sym)
	}
}

pub fn (mut w Walker) mark_by_type(typ ast.Type) {
	if typ.has_flag(.generic) {
		return
	}
	w.mark_by_sym(w.table.sym(typ))
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
					fsym := w.table.sym(ifield.typ)
					if ifield.typ.has_flag(.option) {
						w.used_option++
						if !ifield.has_default_expr {
							w.used_none++
						}
					}
					match fsym.info {
						ast.Array, ast.ArrayFixed {
							w.features.used_arrays++
							w.mark_by_sym(fsym)
						}
						ast.Map {
							w.features.used_maps++
							w.mark_by_sym(fsym)
						}
						else {
							w.mark_by_sym(fsym)
						}
					}
				}
			}
			for embed in isym.info.embeds {
				w.mark_by_type(embed)
			}
			if decl := w.all_structs[isym.name] {
				for iface_typ in decl.implements_types {
					w.mark_by_type(iface_typ.typ)
				}
			}
		}
		ast.ArrayFixed, ast.Array {
			w.mark_by_type(isym.info.elem_type)
		}
		ast.SumType {
			for typ in isym.info.variants {
				if typ == ast.map_type {
					w.features.used_maps++
					continue
				}
				w.mark_by_type(typ)
			}
		}
		ast.Map {
			w.mark_by_type(isym.info.key_type)
			w.mark_by_type(isym.info.value_type)
			w.features.used_maps++
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
			w.mark_by_type(isym.info.elem_type)
		}
		ast.Aggregate {
			for typ in isym.info.types {
				w.mark_by_type(typ)
			}
		}
		ast.Enum {
			w.mark_by_type(isym.info.typ)
		}
		ast.Interface {
			for typ in isym.info.types {
				if typ == ast.map_type {
					w.features.used_maps++
				}
				w.mark_by_type(typ)
			}
			for embed in isym.info.embeds {
				w.mark_by_type(embed)
			}
			for generic_type in isym.info.generic_types {
				w.mark_by_type(generic_type)
			}
			if isym.info.parent_type != 0 {
				w.mark_by_type(isym.info.parent_type)
			}
			for field in isym.info.fields {
				w.mark_by_type(field.typ)
			}
			for method in isym.methods {
				if method.receiver_type != 0 {
					w.mark_by_type(method.receiver_type)
				}
				w.mark_fn_ret_and_params(method.return_type, method.params)
			}
		}
		else {}
	}
}

pub fn (mut w Walker) remove_unused_fn_generic_types() {
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

pub fn (mut w Walker) remove_unused_dump_type() {
	for typ, _ in w.table.dumps {
		if ast.Type(u32(typ)).idx() !in w.used_syms {
			w.table.dumps.delete(typ)
		}
	}
}
