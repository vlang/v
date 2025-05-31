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
	used_structs map[string]bool
	used_fields  map[string]bool
	used_none    int
	n_asserts    int
	pref         &pref.Preferences = unsafe { nil }
mut:
	files       []&ast.File
	all_fns     map[string]ast.FnDecl
	all_consts  map[string]ast.ConstField
	all_globals map[string]ast.GlobalField
	all_fields  map[string]ast.StructField
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
		sym := w.table.sym(gfield.typ)
		if sym.info is ast.Struct {
			w.a_struct_info(sym.name, sym.info)
		}
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
			w.stmts(node.stmts)
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
				w.expr(line.where_expr)
				w.exprs(line.update_exprs)
			}
		}
		ast.StructDecl {
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
			sym := w.table.final_sym(node.elem_type)
			if sym.info is ast.Struct {
				w.a_struct_info(sym.name, sym.info)
			}
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
		}
		ast.CastExpr {
			w.expr(node.expr)
			w.expr(node.arg)
		}
		ast.ChanInit {
			w.expr(node.cap_expr)
		}
		ast.ConcatExpr {
			w.exprs(node.vals)
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
		}
		ast.DumpExpr {
			w.expr(node.expr)
			w.fn_by_name('eprint')
			w.fn_by_name('eprintln')
		}
		ast.SpawnExpr {
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
			if sym.kind == .map {
				if node.is_setter {
					w.mark_builtin_map_method_as_used('set')
				} else {
					w.mark_builtin_map_method_as_used('get')
				}

				w.features.used_maps++
			} else if sym.kind == .array {
				if node.is_setter {
					w.mark_builtin_array_method_as_used('set')
				} else {
					w.mark_builtin_array_method_as_used('get')
				}

				w.features.used_arrays++
			} else if sym.kind == .string {
				if node.index is ast.RangeExpr {
					w.mark_builtin_array_method_as_used('slice')
					w.features.range_index = true
				}
			}
		}
		ast.InfixExpr {
			w.expr(node.left)
			w.expr(node.right)
			w.or_block(node.or_block)
			if node.left_type == 0 {
				return
			}
			sym := w.table.sym(node.left_type)
			if sym.kind == .struct {
				if opmethod := sym.find_method(node.op.str()) {
					unsafe {
						w.fn_decl(mut &ast.FnDecl(opmethod.source_fn))
					}
				}
			}
			if node.right_type == 0 {
				return
			}
			right_sym := w.table.sym(node.right_type)
			if node.op in [.not_in, .key_in] {
				if right_sym.kind == .map {
					w.features.used_maps++
				} else if right_sym.kind == .array {
					w.features.used_arrays++
				}
			}
		}
		ast.IfGuardExpr {
			w.expr(node.expr)
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
				}
				.global {
					w.mark_global_as_used(node.name)
				}
				else {
					// `.unresolved`, `.blank_ident`, `.variable`, `.function`
					// println('>>> else, ast.Ident kind: $node.kind')
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
			w.features.used_maps++
		}
		ast.MatchExpr {
			w.expr(node.cond)
			for b in node.branches {
				w.exprs(b.exprs)
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
		}
		ast.StringInterLiteral {
			w.exprs(node.exprs)
		}
		ast.SelectorExpr {
			w.expr(node.expr)
			if node.expr_type != 0 {
				if method := w.table.find_method(w.table.sym(node.expr_type), node.field_name) {
					w.fn_by_name(method.fkey())
				}
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
		}
		ast.StructInit {
			if node.typ == 0 {
				return
			}
			sym := w.table.sym(node.typ)
			if sym.info is ast.Struct {
				w.a_struct_info(sym.name, sym.info)
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
		}
		///
		ast.AsCast {
			w.expr(node.expr)
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
		}
		ast.LockExpr {
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
		ast.TypeNode {}
		ast.UnsafeExpr {
			w.expr(node.expr)
		}
		ast.NodeError {}
	}
}

pub fn (mut w Walker) a_struct_info(sname string, info ast.Struct) {
	if sname in w.used_structs {
		return
	}
	w.used_structs[sname] = true
	for ifield in info.fields {
		if ifield.has_default_expr {
			w.expr(ifield.default_expr)
		}
		if ifield.typ != 0 {
			fsym := w.table.sym(ifield.typ)
			match fsym.info {
				ast.Struct {
					w.a_struct_info(fsym.name, fsym.info)
				}
				ast.Alias {
					value_sym := w.table.final_sym(ifield.typ)
					if value_sym.info is ast.Struct {
						w.a_struct_info(value_sym.name, value_sym.info)
					}
				}
				ast.Array, ast.ArrayFixed {
					w.features.used_arrays++
					value_sym := w.table.final_sym(w.table.value_type(ifield.typ))
					if value_sym.info is ast.Struct {
						w.a_struct_info(value_sym.name, value_sym.info)
					}
				}
				ast.Map {
					w.features.used_maps++
					value_sym := w.table.final_sym(w.table.value_type(ifield.typ))
					if value_sym.info is ast.Struct {
						w.a_struct_info(value_sym.name, value_sym.info)
					}
				}
				else {}
			}
		}
	}
	for embed in info.embeds {
		sym := w.table.final_sym(embed)
		if sym.info is ast.Struct {
			w.a_struct_info(sym.name, sym.info)
		}
	}
}

pub fn (mut w Walker) fn_decl(mut node ast.FnDecl) {
	if node == unsafe { nil } {
		return
	}
	if node.language == .c {
		w.mark_fn_as_used(node.fkey())
		return
	}
	fkey := node.fkey()
	if w.used_fns[fkey] {
		return
	}
	if node.no_body {
		return
	}
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
	if node.language == .c {
		if node.name in ['C.wyhash', 'C.wyhash64'] {
			w.features.used_maps++
		}
		return
	}
	if node.is_method && node.left_type != 0 {
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
	if stmt.name == node.name {
		if !node.is_method || receiver_typ == stmt.receiver.typ {
			w.stmts(stmt.stmts)
		}
	}
}

pub fn (mut w Walker) fn_by_name(fn_name string) {
	if w.used_fns[fn_name] {
		return
	}
	stmt := w.all_fns[fn_name] or { return }
	w.mark_fn_as_used(fn_name)
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
	}
}
