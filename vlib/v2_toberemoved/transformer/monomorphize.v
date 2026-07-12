// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

// Generic monomorphization belongs in the transformer so every backend receives
// concrete FnDecls and call names.
import v2.ast
import v2.token
import v2.types
import os
import time

struct CloneComptimeFieldCtx {
	var_name       string
	field          types.Field
	field_idx      int
	struct_type    types.Struct
	attrs          []string
	is_embed       bool
	continue_label string
	break_label    string
}

struct DeferredGenericCallSpec {
	base_name string
	bindings  map[string]types.Type
	file_idx  int
}

struct GenericStructSpec {
	base_c_name         string
	concrete_c_name     string
	concrete_short_name string
	module_name         string
	file_idx            int
	bindings            map[string]types.Type
}

struct GenericTypeArgCursorBindings {
	bindings      map[string]types.Type
	concrete_args []types.Type
}

struct StructDefaultDeclInfo {
	decl        ast.StructDecl
	module_name string
}

// needs_full_files_for_transform reports whether transform needs the whole
// legacy file set before per-file workers can run.
pub fn (t &Transformer) needs_full_files_for_transform() bool {
	return true
}

// prepare_files_for_transform performs whole-program preparation required
// before per-file transformation. Generic monomorphization lives here so
// sequential and parallel builders feed identical concrete ASTs to every
// backend.
pub fn (mut t Transformer) prepare_files_for_transform(files []ast.File) []ast.File {
	// The checker records generic bindings while checking generic bodies, which
	// can include speculative branches from unused library code. Build the
	// transformer worklist from concrete call sites instead, then scan concrete
	// clones to discover transitive generic calls.
	t.env.generic_types = map[string][]map[string]types.Type{}
	mut prepared := files.clone()
	t.collect_declared_method_fns(prepared)
	t.collect_struct_field_generic_decl_types(prepared)
	timing := os.getenv('V2_TTIME') != ''
	mut sw := time.new_stopwatch()
	// prepared_dirty tracks whether `prepared` changed since it was last scanned
	// by collect_generic_call_specs. collect is a pure (idempotent) function of
	// the program: scanning an unchanged file set rediscovers the exact same
	// specs. So the leading full-program scan is only needed when the program
	// actually changed since the previous scan — i.e. when the previous
	// iteration's inject step appended new struct specializations. The
	// post-mono_pass scan below already covers the clones mono_pass adds, so the
	// only unscanned source of change between iterations is inject. This removes
	// the fixpoint's redundant confirmation scan (~6s / ~700MB for v2 self-host).
	mut prepared_dirty := true
	for iter in 0 .. 64 {
		spec_count := t.monomorphized_specs.len
		generic_count := t.generic_types_spec_count()
		struct_count := t.generic_struct_specs.len
		ts_start := sw.elapsed().milliseconds()
		// collect1: full-program scan, only when the program changed since the
		// last scan (initial pass, or inject appended structs last iteration).
		if prepared_dirty {
			t.collect_generic_call_specs(prepared)
		}
		ts_collect1 := sw.elapsed().milliseconds()
		before_mono := t.monomorphized_specs.len
		prepared = t.monomorphize_pass(prepared)
		ts_mono := sw.elapsed().milliseconds()
		// collect2: only when monomorphize_pass materialized new clones. Those
		// clones are the only statements the previous scan has not seen — every
		// other statement is byte-for-byte unchanged and collect is idempotent —
		// so rescan just the new clones instead of re-walking all files. This
		// turns the dominant ~6s/~700MB full rescan into a walk of a few dozen
		// functions.
		if t.monomorphized_specs.len != before_mono {
			t.collect_generic_call_specs_in_new_clones(prepared)
		}
		ts_collect2 := sw.elapsed().milliseconds()
		prepared = t.inject_generic_struct_specializations(prepared)
		ts_inject := sw.elapsed().milliseconds()
		if t.inject_changed_files {
			t.collect_generic_call_specs_in_new_structs(prepared)
		}
		ts_collect3 := sw.elapsed().milliseconds()
		// collect2/collect3 rescan every statement appended by this iteration,
		// so the next iteration does not need another full-program scan.
		prepared_dirty = false
		if timing {
			eprintln('  [ttime] iter ${iter}: collect1=${ts_collect1 - ts_start}ms mono_pass=${ts_mono - ts_collect1}ms collect2=${ts_collect2 - ts_mono}ms inject=${ts_inject - ts_collect2}ms collect3=${ts_collect3 - ts_inject}ms files=${prepared.len}')
		}
		t_print_mem('monomorphize iter ${iter}')
		if t.monomorphized_specs.len == spec_count && t.generic_types_spec_count() == generic_count
			&& t.generic_struct_specs.len == struct_count {
			break
		}
	}
	if dump_path := os.getenv_opt('V2_TDUMP') {
		t.dump_monomorphize_specs(dump_path, prepared)
	}
	t.collect_struct_default_decl_infos(prepared)
	t.collect_concrete_embedded_owner_names(prepared)
	return prepared
}

// prepare_flat_for_transform performs the same whole-program generic
// preparation as prepare_files_for_transform, but keeps the original source in
// FlatAst form. Monomorphization and generic struct specialization are
// append-only, so the flat path stores just the per-file appended statements and
// lets the caller stream each original file through a one-file decode.
pub fn (mut t Transformer) prepare_flat_for_transform(flat &ast.FlatAst) map[int][]ast.Stmt {
	t.env.generic_types = map[string][]map[string]types.Type{}
	mut extra_stmts := map[int][]ast.Stmt{}
	t.collect_declared_method_fns_from_flat(flat)
	t.collect_struct_field_generic_decl_types_from_flat(flat)
	timing := os.getenv('V2_TTIME') != ''
	mut sw := time.new_stopwatch()
	mut prepared_dirty := true
	for iter in 0 .. 64 {
		spec_count := t.monomorphized_specs.len
		generic_count := t.generic_types_spec_count()
		struct_count := t.generic_struct_specs.len
		ts_start := sw.elapsed().milliseconds()
		if prepared_dirty {
			t.collect_generic_call_specs_from_flat(flat, extra_stmts)
		}
		ts_collect1 := sw.elapsed().milliseconds()
		before_mono := t.monomorphized_specs.len
		t.monomorphize_pass_from_flat(flat, mut extra_stmts)
		ts_mono := sw.elapsed().milliseconds()
		if t.monomorphized_specs.len != before_mono {
			t.collect_generic_call_specs_in_new_clones_from_flat(flat, extra_stmts)
		}
		ts_collect2 := sw.elapsed().milliseconds()
		t.inject_generic_struct_specializations_from_flat(flat, mut extra_stmts)
		ts_inject := sw.elapsed().milliseconds()
		if t.inject_changed_files {
			t.collect_generic_call_specs_in_new_structs_from_flat(flat, extra_stmts)
		}
		ts_collect3 := sw.elapsed().milliseconds()
		prepared_dirty = false
		if timing {
			eprintln('  [ttime] iter ${iter}: collect1=${ts_collect1 - ts_start}ms mono_pass=${ts_mono - ts_collect1}ms collect2=${ts_collect2 - ts_mono}ms inject=${ts_inject - ts_collect2}ms collect3=${ts_collect3 - ts_inject}ms files=${flat.files.len}')
		}
		t_print_mem('monomorphize iter ${iter}')
		if t.monomorphized_specs.len == spec_count && t.generic_types_spec_count() == generic_count
			&& t.generic_struct_specs.len == struct_count {
			break
		}
	}
	if dump_path := os.getenv_opt('V2_TDUMP') {
		t.dump_flat_monomorphize_specs(dump_path, flat, extra_stmts)
	}
	t.collect_struct_default_decl_infos_from_flat(flat, extra_stmts)
	t.collect_concrete_embedded_owner_names_from_flat(flat, extra_stmts)
	return extra_stmts
}

// dump_monomorphize_specs writes a deterministic snapshot of the fixpoint's
// result (all monomorphized fn spec keys, generic struct spec keys, generic
// binding signatures, and per-file appended-stmt counts) to `path`. Used only
// for correctness validation: the file must be byte-identical before and after
// any optimization to the fixpoint loop. Gated on V2_TDUMP.
fn (t &Transformer) dump_monomorphize_specs(path string, prepared []ast.File) {
	mut lines := []string{}
	mut mspecs := t.monomorphized_specs.keys()
	mspecs.sort()
	lines << '# monomorphized_specs (${mspecs.len})'
	for k in mspecs {
		lines << 'M ${k}'
	}
	mut sspecs := t.generic_struct_specs.keys()
	sspecs.sort()
	lines << '# generic_struct_specs (${sspecs.len})'
	for k in sspecs {
		lines << 'S ${k}'
	}
	mut gkeys := t.env.generic_types.keys()
	gkeys.sort()
	lines << '# generic_types'
	for k in gkeys {
		blist := t.env.generic_types[k] or { continue }
		mut sigs := []string{}
		for b in blist {
			sigs << generic_bindings_signature(b)
		}
		sigs.sort()
		for sig in sigs {
			lines << 'G ${k} :: ${sig}'
		}
	}
	// Per-file fingerprint: file name + stmt count, in input order.
	lines << '# files (${prepared.len})'
	for f in prepared {
		lines << 'F ${f.mod}/${f.name} stmts=${f.stmts.len}'
	}
	os.write_file(path, lines.join('\n')) or {}
}

fn (t &Transformer) dump_flat_monomorphize_specs(path string, flat &ast.FlatAst, extra_stmts map[int][]ast.Stmt) {
	mut lines := []string{}
	mut mspecs := t.monomorphized_specs.keys()
	mspecs.sort()
	lines << '# monomorphized_specs (${mspecs.len})'
	for k in mspecs {
		lines << 'M ${k}'
	}
	mut sspecs := t.generic_struct_specs.keys()
	sspecs.sort()
	lines << '# generic_struct_specs (${sspecs.len})'
	for k in sspecs {
		lines << 'S ${k}'
	}
	mut gkeys := t.env.generic_types.keys()
	gkeys.sort()
	lines << '# generic_types'
	for k in gkeys {
		blist := t.env.generic_types[k] or { continue }
		mut sigs := []string{}
		for b in blist {
			sigs << generic_bindings_signature(b)
		}
		sigs.sort()
		for sig in sigs {
			lines << 'G ${k} :: ${sig}'
		}
	}
	lines << '# files (${flat.files.len})'
	for i, ff in flat.files {
		extra := extra_stmts[i] or { []ast.Stmt{} }
		stmt_count := flat_file_stmt_count(flat, i) + extra.len
		lines << 'F ${flat.file_mod(ff)}/${flat.file_name(ff)} stmts=${stmt_count}'
	}
	os.write_file(path, lines.join('\n')) or {}
}

fn flat_file_stmt_count(flat &ast.FlatAst, fi int) int {
	if fi < 0 || fi >= flat.files.len {
		return 0
	}
	return flat.file_cursor(fi).stmts().len()
}

fn (t &Transformer) generic_types_spec_count() int {
	mut count := 0
	for _, bindings_list in t.env.generic_types {
		count += bindings_list.len
	}
	return count
}

fn (mut t Transformer) collect_declared_method_fns(files []ast.File) {
	t.declared_method_fns = map[string]bool{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.is_method {
				t.register_declared_method_fn(stmt, file.mod)
			}
		}
	}
}

fn (mut t Transformer) collect_declared_method_fns_from_flat(flat &ast.FlatAst) {
	t.declared_method_fns = map[string]bool{}
	for fi in 0 .. flat.files.len {
		module_name := flat.file_mod(flat.files[fi])
		stmts := flat.file_cursor(fi).stmts()
		for si in 0 .. stmts.len() {
			c := stmts.at(si)
			if c.kind() != .stmt_fn_decl || !c.flag(ast.flag_is_method) {
				continue
			}
			t.register_declared_method_fn_cursor(c, module_name)
		}
	}
}

fn (mut t Transformer) register_declared_method_fn(decl ast.FnDecl, module_name string) {
	recv_name := t.get_receiver_type_name(decl.receiver.typ)
	if recv_name == '' || decl.name == '' {
		return
	}
	t.declared_method_fns['${recv_name}__${decl.name}'] = true
	if module_name != '' && module_name != 'builtin' && !recv_name.contains('__') {
		qualified := '${module_name.replace('.', '__')}__${recv_name}__${decl.name}'
		t.declared_method_fns[qualified] = true
		call_prefix := module_call_c_prefix(module_name)
		if call_prefix != '' && call_prefix != module_name.replace('.', '__') {
			t.declared_method_fns['${call_prefix}__${recv_name}__${decl.name}'] = true
		}
	}
}

fn (mut t Transformer) register_declared_method_fn_cursor(decl ast.Cursor, module_name string) {
	recv := decl.edge(0)
	recv_name := t.get_receiver_type_name_cursor(recv.edge(0))
	if recv_name == '' || decl.name() == '' {
		return
	}
	t.declared_method_fns['${recv_name}__${decl.name()}'] = true
	if module_name != '' && module_name != 'builtin' && !recv_name.contains('__') {
		qualified := '${module_name.replace('.', '__')}__${recv_name}__${decl.name()}'
		t.declared_method_fns[qualified] = true
		call_prefix := module_call_c_prefix(module_name)
		if call_prefix != '' && call_prefix != module_name.replace('.', '__') {
			t.declared_method_fns['${call_prefix}__${recv_name}__${decl.name()}'] = true
		}
	}
}

fn (mut t Transformer) collect_struct_field_generic_decl_types(files []ast.File) {
	old_module := t.cur_module
	old_scope := t.scope
	t.struct_field_generic_decl_types = map[string]types.Type{}
	t.struct_field_generic_decl_bindings = map[string]map[string]types.Type{}
	for file in files {
		t.cur_module = file.mod
		if scope := t.get_module_scope(file.mod) {
			t.scope = scope
		} else {
			t.scope = unsafe { nil }
		}
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				t.collect_struct_decl_generic_field_types(stmt, file.mod)
			}
		}
	}
	t.cur_module = old_module
	t.scope = old_scope
}

fn (mut t Transformer) collect_struct_field_generic_decl_types_from_flat(flat &ast.FlatAst) {
	old_module := t.cur_module
	old_scope := t.scope
	t.struct_field_generic_decl_types = map[string]types.Type{}
	t.struct_field_generic_decl_bindings = map[string]map[string]types.Type{}
	for fi in 0 .. flat.files.len {
		module_name := flat.file_mod(flat.files[fi])
		t.cur_module = module_name
		if scope := t.get_module_scope(module_name) {
			t.scope = scope
		} else {
			t.scope = unsafe { nil }
		}
		stmts := flat.file_cursor(fi).stmts()
		for si in 0 .. stmts.len() {
			c := stmts.at(si)
			if c.kind() != .stmt_struct_decl {
				continue
			}
			t.collect_struct_decl_generic_field_types_cursor(c, module_name)
		}
	}
	t.cur_module = old_module
	t.scope = old_scope
}

fn (mut t Transformer) collect_struct_decl_generic_field_types(decl ast.StructDecl, module_name string) {
	parent_names := struct_decl_field_lookup_names(decl.name, module_name)
	if parent_names.len == 0 {
		return
	}
	for field in decl.fields {
		if field.name == '' || !field_type_expr_has_generic_args(field.typ) {
			continue
		}
		field_type := t.lookup_type_from_expr(field.typ) or { continue }
		if !types.type_has_valid_payload(field_type) {
			continue
		}
		for parent_name in parent_names {
			t.struct_field_generic_decl_types[struct_field_generic_decl_key(parent_name, field.name)] = field_type
			if bindings := t.generic_bindings_from_type_expr(field.typ) {
				t.struct_field_generic_decl_bindings[struct_field_generic_decl_key(parent_name,
					field.name)] = bindings.clone()
			}
		}
	}
	for embedded in decl.embedded {
		if !field_type_expr_has_generic_args(embedded) {
			continue
		}
		field_name := embedded_field_name_from_type_expr(embedded)
		if field_name == '' {
			continue
		}
		field_type := t.lookup_type_from_expr(embedded) or { continue }
		if !types.type_has_valid_payload(field_type) {
			continue
		}
		for parent_name in parent_names {
			t.struct_field_generic_decl_types[struct_field_generic_decl_key(parent_name, field_name)] = field_type
			if bindings := t.generic_bindings_from_type_expr(embedded) {
				t.struct_field_generic_decl_bindings[struct_field_generic_decl_key(parent_name,
					field_name)] = bindings.clone()
			}
		}
	}
}

fn (mut t Transformer) collect_struct_decl_generic_field_types_cursor(decl ast.Cursor, module_name string) {
	parent_names := struct_decl_field_lookup_names(decl.name(), module_name)
	if parent_names.len == 0 {
		return
	}
	fields := decl.list_at(4)
	for i in 0 .. fields.len() {
		field := fields.at(i)
		field_name := field.name()
		if field_name == '' {
			continue
		}
		field_type_expr := field.edge(0)
		if !field_type_cursor_has_generic_args(field_type_expr) {
			continue
		}
		field_type := t.lookup_type_from_expr_cursor(field_type_expr) or { continue }
		if !types.type_has_valid_payload(field_type) {
			continue
		}
		for parent_name in parent_names {
			t.struct_field_generic_decl_types[struct_field_generic_decl_key(parent_name, field_name)] = field_type
			if bindings := t.generic_bindings_from_type_expr_cursor(field_type_expr) {
				t.struct_field_generic_decl_bindings[struct_field_generic_decl_key(parent_name,
					field_name)] = bindings.clone()
			}
		}
	}
	embedded := decl.list_at(2)
	for i in 0 .. embedded.len() {
		embedded_expr := embedded.at(i)
		if !field_type_cursor_has_generic_args(embedded_expr) {
			continue
		}
		field_name := embedded_field_name_from_type_expr_cursor(embedded_expr)
		if field_name == '' {
			continue
		}
		field_type := t.lookup_type_from_expr_cursor(embedded_expr) or { continue }
		if !types.type_has_valid_payload(field_type) {
			continue
		}
		for parent_name in parent_names {
			t.struct_field_generic_decl_types[struct_field_generic_decl_key(parent_name, field_name)] = field_type
			if bindings := t.generic_bindings_from_type_expr_cursor(embedded_expr) {
				t.struct_field_generic_decl_bindings[struct_field_generic_decl_key(parent_name,
					field_name)] = bindings.clone()
			}
		}
	}
}

fn (mut t Transformer) collect_concrete_embedded_owner_names(files []ast.File) {
	old_module := t.cur_module
	old_file := t.cur_file_name
	old_scope := t.scope
	old_import_aliases := t.cur_import_aliases.clone()
	t.concrete_embedded_owner_names = map[string]map[string]string{}
	for file in files {
		t.cur_file_name = file.name
		t.cur_module = file.mod
		t.cur_import_aliases = import_aliases_for_generic_collect(file.imports)
		if scope := t.get_module_scope(file.mod) {
			t.scope = scope
		} else {
			t.scope = unsafe { nil }
		}
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				mut embedded := []ast.Expr{cap: stmt.embedded.len}
				for item in stmt.embedded {
					embedded << t.rewrite_concrete_generic_struct_type_expr(item)
				}
				t.register_concrete_embedded_owner_names(stmt, embedded)
			}
		}
	}
	t.cur_module = old_module
	t.cur_file_name = old_file
	t.scope = old_scope
	t.cur_import_aliases = old_import_aliases.clone()
}

fn (mut t Transformer) collect_concrete_embedded_owner_names_from_flat(flat &ast.FlatAst, extra_stmts map[int][]ast.Stmt) {
	old_module := t.cur_module
	old_file := t.cur_file_name
	old_scope := t.scope
	old_import_aliases := t.cur_import_aliases.clone()
	t.concrete_embedded_owner_names = map[string]map[string]string{}
	for fi in 0 .. flat.files.len {
		module_name := flat.file_mod(flat.files[fi])
		t.cur_file_name = flat.file_name(flat.files[fi])
		t.cur_module = module_name
		t.cur_import_aliases = flat_import_aliases_for_generic_collect(flat, fi)
		if scope := t.get_module_scope(module_name) {
			t.scope = scope
		} else {
			t.scope = unsafe { nil }
		}
		for stmt in flat_file_struct_decls_with_extra(flat, extra_stmts, fi) {
			mut embedded := []ast.Expr{cap: stmt.embedded.len}
			for item in stmt.embedded {
				embedded << t.rewrite_concrete_generic_struct_type_expr(item)
			}
			t.register_concrete_embedded_owner_names(stmt, embedded)
		}
	}
	t.cur_module = old_module
	t.cur_file_name = old_file
	t.scope = old_scope
	t.cur_import_aliases = old_import_aliases.clone()
}

fn (mut t Transformer) collect_struct_default_decl_infos(files []ast.File) {
	t.struct_default_decl_infos = map[string]StructDefaultDeclInfo{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				info := StructDefaultDeclInfo{
					decl:        stmt
					module_name: file.mod
				}
				c_name := generic_struct_decl_c_name(stmt, file.mod)
				t.struct_default_decl_infos[c_name] = info
				if c_name.contains('__') {
					t.struct_default_decl_infos[c_name.all_after_last('__')] = info
				}
				t.struct_default_decl_infos[stmt.name] = info
			}
		}
	}
}

fn (mut t Transformer) collect_struct_default_decl_infos_from_flat(flat &ast.FlatAst, extra_stmts map[int][]ast.Stmt) {
	t.struct_default_decl_infos = map[string]StructDefaultDeclInfo{}
	for fi in 0 .. flat.files.len {
		module_name := flat.file_mod(flat.files[fi])
		for stmt in flat_file_struct_decls_with_extra(flat, extra_stmts, fi) {
			info := StructDefaultDeclInfo{
				decl:        stmt
				module_name: module_name
			}
			c_name := generic_struct_decl_c_name(stmt, module_name)
			t.struct_default_decl_infos[c_name] = info
			if c_name.contains('__') {
				t.struct_default_decl_infos[c_name.all_after_last('__')] = info
			}
			t.struct_default_decl_infos[stmt.name] = info
		}
	}
}

fn generic_struct_runtime_args(args []ast.Expr) []ast.Expr {
	mut out := []ast.Expr{cap: args.len}
	for arg in args {
		if arg is ast.LifetimeExpr {
			continue
		}
		out << arg
	}
	return out
}

fn generic_struct_module_from_c_name(c_name string, fallback string) string {
	if c_name.contains('__') {
		return c_name.all_before_last('__')
	}
	return fallback
}

fn generic_struct_short_name_from_c_name(c_name string) string {
	if c_name.contains('__') {
		return c_name.all_after_last('__')
	}
	return c_name
}

fn generic_struct_decl_c_name(decl ast.StructDecl, module_name string) string {
	name := decl.name.replace('.', '__')
	if module_name != '' && module_name != 'main' && module_name != 'builtin'
		&& !name.contains('__') {
		return '${module_name.replace('.', '__')}__${name}'
	}
	return name
}

fn (mut t Transformer) collect_generic_struct_spec_from_type_expr(expr ast.Expr) {
	mut lhs := ast.empty_expr
	mut args := []ast.Expr{}
	match expr {
		ast.GenericArgs {
			lhs = expr.lhs
			args = expr.args.clone()
		}
		ast.GenericArgOrIndexExpr {
			lhs = expr.lhs
			args = [expr.expr]
		}
		ast.IndexExpr {
			lhs = expr.lhs
			args = [expr.expr]
		}
		ast.Type {
			if expr is ast.GenericType {
				lhs = expr.name
				args = expr.params.clone()
			} else {
				return
			}
		}
		ast.ModifierExpr {
			t.collect_generic_struct_spec_from_type_expr(expr.expr)
			return
		}
		ast.PrefixExpr {
			t.collect_generic_struct_spec_from_type_expr(expr.expr)
			return
		}
		else {
			return
		}
	}

	t.register_generic_struct_spec(lhs, args)
}

fn (mut t Transformer) collect_generic_struct_spec_from_type_expr_cursor(expr ast.Cursor) {
	if !expr.is_valid() {
		return
	}
	mut lhs := ast.Cursor{}
	mut args := []ast.Cursor{}
	match expr.kind() {
		.expr_generic_args {
			lhs = expr.edge(0)
			for i in 1 .. expr.edge_count() {
				args << expr.edge(i)
			}
		}
		.expr_generic_arg_or_index, .expr_index {
			lhs = expr.edge(0)
			args = [expr.edge(1)]
		}
		.typ_generic {
			lhs = expr.edge(0)
			for i in 1 .. expr.edge_count() {
				args << expr.edge(i)
			}
		}
		.expr_modifier, .expr_prefix {
			t.collect_generic_struct_spec_from_type_expr_cursor(expr.edge(0))
			return
		}
		else {
			return
		}
	}

	t.register_generic_struct_spec_cursor(lhs, args)
}

fn (mut t Transformer) register_generic_struct_spec(lhs ast.Expr, args []ast.Expr) {
	runtime_args := generic_struct_runtime_args(args)
	if runtime_args.len == 0 {
		return
	}
	base_type := t.lookup_type_from_expr(lhs) or { return }
	if base_type !is types.Struct {
		return
	}
	base_struct := base_type as types.Struct
	if base_struct.generic_params.len == 0 {
		return
	}
	bindings := t.generic_type_arg_bindings(base_struct.generic_params, runtime_args) or { return }
	if bindings.len != base_struct.generic_params.len {
		return
	}
	for _, concrete in bindings {
		if clone_type_contains_generic_placeholder(concrete) {
			return
		}
	}
	base_c_name := t.type_to_c_name(types.Type(base_struct))
	suffix := t.generic_specialization_suffix(runtime_args)
	if base_c_name == '' || suffix == '' || !suffix.starts_with('_T_') {
		return
	}
	concrete_c_name := base_c_name + suffix
	if concrete_c_name in t.generic_struct_specs {
		return
	}
	module_name := generic_struct_module_from_c_name(base_c_name, t.cur_module)
	file_idx := if t.generic_bindings_visible_in_module(module_name, bindings) {
		-1
	} else {
		t.cur_generic_call_file_idx
	}
	spec := GenericStructSpec{
		base_c_name:         base_c_name
		concrete_c_name:     concrete_c_name
		concrete_short_name: generic_struct_short_name_from_c_name(concrete_c_name)
		module_name:         module_name
		file_idx:            file_idx
		bindings:            bindings.clone()
	}
	t.generic_struct_specs[concrete_c_name] = spec
	t.register_generic_struct_env_type(base_struct, spec)
}

fn (mut t Transformer) register_generic_struct_spec_cursor(lhs ast.Cursor, args []ast.Cursor) {
	runtime_args := generic_struct_runtime_arg_cursors(args)
	if runtime_args.len == 0 {
		return
	}
	base_type := t.lookup_type_from_expr_cursor(lhs) or { return }
	if base_type !is types.Struct {
		return
	}
	base_struct := base_type as types.Struct
	if base_struct.generic_params.len == 0 {
		return
	}
	arg_bindings := t.generic_type_arg_bindings_cursor(base_struct.generic_params, runtime_args) or {
		return
	}
	bindings := arg_bindings.bindings.clone()
	if bindings.len != base_struct.generic_params.len {
		return
	}
	for _, concrete in bindings {
		if clone_type_contains_generic_placeholder(concrete) {
			return
		}
	}
	base_c_name := t.type_to_c_name(types.Type(base_struct))
	suffix := t.generic_specialization_suffix_from_types(arg_bindings.concrete_args)
	if base_c_name == '' || suffix == '' || !suffix.starts_with('_T_') {
		return
	}
	concrete_c_name := base_c_name + suffix
	if concrete_c_name in t.generic_struct_specs {
		return
	}
	module_name := generic_struct_module_from_c_name(base_c_name, t.cur_module)
	file_idx := if t.generic_bindings_visible_in_module(module_name, bindings) {
		-1
	} else {
		t.cur_generic_call_file_idx
	}
	spec := GenericStructSpec{
		base_c_name:         base_c_name
		concrete_c_name:     concrete_c_name
		concrete_short_name: generic_struct_short_name_from_c_name(concrete_c_name)
		module_name:         module_name
		file_idx:            file_idx
		bindings:            bindings.clone()
	}
	t.generic_struct_specs[concrete_c_name] = spec
	t.register_generic_struct_env_type(base_struct, spec)
}

fn generic_struct_runtime_arg_cursors(args []ast.Cursor) []ast.Cursor {
	mut out := []ast.Cursor{cap: args.len}
	for arg in args {
		if arg.kind() == .expr_lifetime {
			continue
		}
		out << arg
	}
	return out
}

fn (t &Transformer) generic_type_arg_bindings_cursor(generic_params []string, args []ast.Cursor) ?GenericTypeArgCursorBindings {
	if generic_params.len == 0 || args.len == 0 {
		return none
	}
	mut bindings := map[string]types.Type{}
	mut concrete_args := []types.Type{cap: args.len}
	for i, param_name in generic_params {
		if i >= args.len {
			break
		}
		arg := args[i]
		mut found := false
		mut concrete := types.Type(types.void_)
		if arg.kind() == .expr_ident {
			if bound_type := t.cur_monomorphized_fn_bindings[arg.name()] {
				concrete = t.qualify_generic_concrete_type_from_cursor(bound_type, arg)
				found = true
			}
		}
		if !found {
			if synth_type := t.get_synth_type(arg.pos()) {
				concrete = t.qualify_generic_concrete_type_from_cursor(synth_type, arg)
				found = true
			}
		}
		if !found {
			if typ := t.lookup_type_from_expr_cursor(arg) {
				concrete = t.qualify_generic_concrete_type_from_cursor(typ, arg)
				found = true
			}
		}
		if !found {
			if typ := t.get_expr_type_cursor(arg) {
				concrete = t.qualify_generic_concrete_type_from_cursor(typ, arg)
				found = true
			}
		}
		if found {
			bindings[param_name] = concrete
			concrete_args << concrete
		}
	}
	if bindings.len == 0 {
		return none
	}
	return GenericTypeArgCursorBindings{
		bindings:      bindings
		concrete_args: concrete_args
	}
}

fn (t &Transformer) generic_specialization_suffix_from_types(args []types.Type) string {
	if args.len == 0 {
		return ''
	}
	mut parts := []string{cap: args.len}
	for arg in args {
		parts << t.generic_specialization_token_from_type(arg)
	}
	return '_T_' + parts.join('_')
}

fn (t &Transformer) lookup_type_from_expr_cursor(expr ast.Cursor) ?types.Type {
	if !expr.is_valid() || expr.kind() == .expr_empty {
		return none
	}
	if typ := t.get_synth_type(expr.pos()) {
		return typ
	}
	match expr.kind() {
		.expr_ident {
			name := expr.name()
			if typ := t.lookup_type(name) {
				return typ
			}
			if typ := t.c_name_to_type(name) {
				return typ
			}
			c_name := t.v_type_name_to_c_name(name)
			if c_name != '' && c_name != name {
				if typ := t.c_name_to_type(c_name) {
					return typ
				}
			}
		}
		.expr_selector {
			lhs := expr.edge(0)
			rhs_name := selector_rhs_name_cursor(expr)
			if lhs.kind() == .expr_ident && rhs_name != '' {
				lhs_name := lhs.name()
				mut module_names := []string{}
				if full_name := t.cur_import_aliases[lhs_name] {
					module_names << module_call_c_prefix(full_name)
					module_names << full_name.replace('.', '__')
				}
				if prefix := t.resolve_module_call_prefix(lhs_name) {
					module_names << prefix
				}
				module_names << lhs_name.replace('.', '__')
				mut seen := map[string]bool{}
				for module_name in module_names {
					if module_name == '' || module_name in seen {
						continue
					}
					seen[module_name] = true
					qualified := '${module_name}__${rhs_name}'
					if typ := t.lookup_type(qualified) {
						return typ
					}
				}
				return t.lookup_type(rhs_name)
			}
		}
		.expr_prefix {
			op := unsafe { token.Token(int(expr.aux())) }
			if op == .amp {
				base := t.lookup_type_from_expr_cursor(expr.edge(0)) or { return none }
				return types.Type(types.Pointer{
					base_type: base
				})
			}
		}
		.expr_modifier {
			return t.lookup_type_from_expr_cursor(expr.edge(0))
		}
		.expr_generic_args {
			return t.lookup_generic_type_from_expr_cursor(expr.edge(0), cursor_edges(expr, 1))
		}
		.expr_generic_arg_or_index, .expr_index {
			return t.lookup_generic_type_from_expr_cursor(expr.edge(0), [
				expr.edge(1),
			])
		}
		.typ_pointer {
			base := t.lookup_type_from_expr_cursor(expr.edge(0)) or { return none }
			return types.Type(types.Pointer{
				base_type: base
			})
		}
		.typ_option {
			base := t.lookup_type_from_expr_cursor(expr.edge(0)) or { return none }
			return types.Type(types.OptionType{
				base_type: base
			})
		}
		.typ_result {
			base := t.lookup_type_from_expr_cursor(expr.edge(0)) or { return none }
			return types.Type(types.ResultType{
				base_type: base
			})
		}
		.typ_map {
			key := t.lookup_type_from_expr_cursor(expr.edge(0)) or { return none }
			value := t.lookup_type_from_expr_cursor(expr.edge(1)) or { return none }
			return types.Type(types.Map{
				key_type:   key
				value_type: value
			})
		}
		.typ_array {
			elem := t.lookup_type_from_expr_cursor(expr.edge(0)) or { return none }
			return types.Type(types.Array{
				elem_type: elem
			})
		}
		.typ_array_fixed {
			len_expr := expr.edge(0)
			mut len := 0
			if len_expr.kind() == .expr_basic_literal {
				len = len_expr.name().int()
			}
			elem := t.lookup_type_from_expr_cursor(expr.edge(1)) or { return none }
			return types.Type(types.ArrayFixed{
				len:       len
				elem_type: elem
			})
		}
		.typ_generic {
			return t.lookup_generic_type_from_expr_cursor(expr.edge(0), cursor_edges(expr, 1))
		}
		else {}
	}

	return none
}

fn cursor_edges(expr ast.Cursor, start int) []ast.Cursor {
	mut out := []ast.Cursor{cap: expr.edge_count() - start}
	for i in start .. expr.edge_count() {
		out << expr.edge(i)
	}
	return out
}

fn (t &Transformer) lookup_generic_type_from_expr_cursor(lhs ast.Cursor, args []ast.Cursor) ?types.Type {
	base := t.lookup_type_from_expr_cursor(lhs) or { return none }
	if base is types.Struct {
		arg_bindings := t.generic_type_arg_bindings_cursor(base.generic_params, args) or {
			return base
		}
		return substitute_type(base, arg_bindings.bindings)
	}
	return base
}

fn (t &Transformer) qualify_generic_concrete_type_from_cursor(concrete types.Type, arg ast.Cursor) types.Type {
	match concrete {
		types.Struct {
			if name := t.generic_concrete_type_arg_c_name_cursor(types.Type(concrete), arg) {
				return types.Type(types.Struct{
					name:           name
					generic_params: concrete.generic_params
					implements:     concrete.implements
					embedded:       concrete.embedded
					fields:         concrete.fields
					is_soa:         concrete.is_soa
				})
			}
		}
		types.Enum {
			if name := t.generic_concrete_type_arg_c_name_cursor(types.Type(concrete), arg) {
				return types.Type(types.Enum{
					is_flag: concrete.is_flag
					name:    name
					fields:  concrete.fields
				})
			}
		}
		types.Interface {
			if name := t.generic_concrete_type_arg_c_name_cursor(types.Type(concrete), arg) {
				return types.Type(types.Interface{
					name:   name
					fields: concrete.fields
				})
			}
		}
		types.SumType {
			if name := t.generic_concrete_type_arg_c_name_cursor(types.Type(concrete), arg) {
				return types.Type(types.SumType{
					name:           name
					generic_params: concrete.generic_params
					variants:       concrete.variants
				})
			}
		}
		types.Alias {
			if name := t.generic_concrete_type_arg_c_name_cursor(types.Type(concrete), arg) {
				return types.Type(types.Alias{
					name:      name
					base_type: concrete.base_type
				})
			}
		}
		types.NamedType {
			if name := t.generic_concrete_type_arg_c_name_cursor(types.Type(concrete), arg) {
				return types.Type(types.NamedType(name))
			}
		}
		types.Pointer {
			if base_arg := pointer_generic_type_arg_base_cursor(arg) {
				return types.Type(types.Pointer{
					lifetime:  concrete.lifetime
					base_type: t.qualify_generic_concrete_type_from_cursor(concrete.base_type,
						base_arg)
				})
			}
		}
		types.Array {
			if elem_arg := array_generic_type_arg_elem_cursor(arg) {
				return types.Type(types.Array{
					elem_type: t.qualify_generic_concrete_type_from_cursor(concrete.elem_type,
						elem_arg)
				})
			}
		}
		types.ArrayFixed {
			if elem_arg := array_fixed_generic_type_arg_elem_cursor(arg) {
				return types.Type(types.ArrayFixed{
					len:       concrete.len
					elem_type: t.qualify_generic_concrete_type_from_cursor(concrete.elem_type,
						elem_arg)
				})
			}
		}
		types.Map {
			if parts := map_generic_type_arg_parts_cursor(arg) {
				return types.Type(types.Map{
					key_type:   t.qualify_generic_concrete_type_from_cursor(concrete.key_type,
						parts.key)
					value_type: t.qualify_generic_concrete_type_from_cursor(concrete.value_type,
						parts.value)
				})
			}
		}
		types.OptionType {
			if base_arg := option_generic_type_arg_base_cursor(arg) {
				return types.Type(types.OptionType{
					base_type: t.qualify_generic_concrete_type_from_cursor(concrete.base_type,
						base_arg)
				})
			}
		}
		types.ResultType {
			if base_arg := result_generic_type_arg_base_cursor(arg) {
				return types.Type(types.ResultType{
					base_type: t.qualify_generic_concrete_type_from_cursor(concrete.base_type,
						base_arg)
				})
			}
		}
		else {}
	}

	return concrete
}

fn (t &Transformer) generic_concrete_type_arg_c_name_cursor(concrete types.Type, arg ast.Cursor) ?string {
	if arg.kind() == .expr_ident {
		name := arg.name()
		if bound_type := t.cur_monomorphized_fn_bindings[name] {
			c_name := t.type_to_c_name(bound_type)
			if c_name != '' {
				return c_name
			}
		}
		if name.contains('__') {
			return name
		}
		if concrete.name() == name {
			return t.generic_ident_type_arg_c_name(name)
		}
	}
	if arg.kind() == .typ_generic {
		base_name := t.expr_to_type_name_cursor_direct(arg.edge(0))
		suffix := t.generic_specialization_suffix_from_type_cursors(cursor_edges(arg, 1))
		if base_name != '' && suffix != '' {
			return base_name + suffix
		}
	}
	if name := t.generic_init_type_name_cursor_direct(arg) {
		return name
	}
	name := t.expr_to_type_name_cursor_direct(arg)
	if name == '' {
		return none
	}
	return name
}

fn (t &Transformer) generic_specialization_suffix_from_type_cursors(args []ast.Cursor) string {
	runtime_args := generic_struct_runtime_arg_cursors(args)
	if runtime_args.len == 0 {
		return ''
	}
	arg_bindings := t.generic_type_arg_bindings_cursor(runtime_arg_names(runtime_args),
		runtime_args) or { return '' }
	if arg_bindings.concrete_args.len != runtime_args.len {
		return ''
	}
	for concrete in arg_bindings.concrete_args {
		if clone_type_contains_generic_placeholder(concrete) {
			return ''
		}
	}
	return t.generic_specialization_suffix_from_types(arg_bindings.concrete_args)
}

fn runtime_arg_names(args []ast.Cursor) []string {
	mut names := []string{cap: args.len}
	for i in 0 .. args.len {
		names << 'T${i}'
	}
	return names
}

fn (t &Transformer) generic_init_type_name_cursor_direct(expr ast.Cursor) ?string {
	match expr.kind() {
		.expr_generic_args {
			base_name := t.expr_to_type_name_cursor_direct(expr.edge(0))
			suffix := t.generic_specialization_suffix_from_type_cursors(cursor_edges(expr, 1))
			if base_name != '' && suffix != '' {
				return base_name + suffix
			}
		}
		.expr_generic_arg_or_index, .expr_index {
			base_name := t.expr_to_type_name_cursor_direct(expr.edge(0))
			suffix := t.generic_specialization_suffix_from_type_cursors([
				expr.edge(1),
			])
			if base_name != '' && suffix != '' {
				return base_name + suffix
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) expr_to_type_name_cursor_direct(expr ast.Cursor) string {
	match expr.kind() {
		.expr_ident {
			if typ := t.get_synth_type(expr.pos()) {
				c_name := t.type_to_c_name(typ)
				if c_name != '' {
					return c_name
				}
			}
			return expr.name()
		}
		.expr_selector {
			return selector_type_name_cursor(expr, true)
		}
		.typ_array, .typ_array_fixed, .typ_map, .typ_option, .typ_pointer, .typ_result,
		.typ_generic {
			if typ := t.lookup_type_from_expr_cursor(expr) {
				c_name := t.type_to_c_name(typ)
				if c_name != '' {
					return c_name
				}
			}
		}
		else {}
	}

	return ''
}

fn pointer_generic_type_arg_base_cursor(arg ast.Cursor) ?ast.Cursor {
	match arg.kind() {
		.expr_prefix {
			op := unsafe { token.Token(int(arg.aux())) }
			if op in [.amp, .mul] {
				return arg.edge(0)
			}
		}
		.typ_pointer {
			return arg.edge(0)
		}
		else {}
	}

	return none
}

fn array_generic_type_arg_elem_cursor(arg ast.Cursor) ?ast.Cursor {
	if arg.kind() == .typ_array {
		return arg.edge(0)
	}
	return none
}

fn array_fixed_generic_type_arg_elem_cursor(arg ast.Cursor) ?ast.Cursor {
	if arg.kind() == .typ_array_fixed {
		return arg.edge(1)
	}
	return none
}

struct GenericMapTypeArgCursorParts {
	key   ast.Cursor
	value ast.Cursor
}

fn map_generic_type_arg_parts_cursor(arg ast.Cursor) ?GenericMapTypeArgCursorParts {
	if arg.kind() == .typ_map {
		return GenericMapTypeArgCursorParts{
			key:   arg.edge(0)
			value: arg.edge(1)
		}
	}
	return none
}

fn option_generic_type_arg_base_cursor(arg ast.Cursor) ?ast.Cursor {
	if arg.kind() == .typ_option {
		return arg.edge(0)
	}
	return none
}

fn result_generic_type_arg_base_cursor(arg ast.Cursor) ?ast.Cursor {
	if arg.kind() == .typ_result {
		return arg.edge(0)
	}
	return none
}

fn (t &Transformer) generic_bindings_visible_in_module(module_name string, bindings map[string]types.Type) bool {
	for _, typ in bindings {
		if !t.generic_type_visible_in_module(module_name, typ) {
			return false
		}
	}
	return true
}

fn (t &Transformer) generic_type_visible_in_module(module_name string, typ types.Type) bool {
	match typ {
		types.Primitive, types.String, types.Char, types.Rune, types.Void, types.Nil, types.None,
		types.ISize, types.USize {
			return true
		}
		types.Struct {
			return t.generic_type_name_visible_in_module(module_name, typ.name)
		}
		types.Enum {
			return t.generic_type_name_visible_in_module(module_name, typ.name)
		}
		types.Interface {
			return t.generic_type_name_visible_in_module(module_name, typ.name)
		}
		types.SumType {
			return t.generic_type_name_visible_in_module(module_name, types.sum_type_name(typ))
		}
		types.Alias {
			return t.generic_type_name_visible_in_module(module_name, typ.name)
		}
		types.NamedType {
			return t.generic_type_name_visible_in_module(module_name, string(typ))
		}
		types.Pointer {
			return t.generic_type_visible_in_module(module_name, typ.base_type)
		}
		types.Array {
			return t.generic_type_visible_in_module(module_name, typ.elem_type)
		}
		types.ArrayFixed {
			return t.generic_type_visible_in_module(module_name, typ.elem_type)
		}
		types.Map {
			return t.generic_type_visible_in_module(module_name, typ.key_type)
				&& t.generic_type_visible_in_module(module_name, typ.value_type)
		}
		types.OptionType {
			return t.generic_type_visible_in_module(module_name, typ.base_type)
		}
		types.ResultType {
			return t.generic_type_visible_in_module(module_name, typ.base_type)
		}
		types.FnType {
			for param_type in typ.get_param_types() {
				if !t.generic_type_visible_in_module(module_name, param_type) {
					return false
				}
			}
			if ret_type := typ.get_return_type() {
				return t.generic_type_visible_in_module(module_name, ret_type)
			}
			return true
		}
		else {
			return true
		}
	}
}

fn (t &Transformer) generic_type_name_visible_in_module(module_name string, type_name string) bool {
	if type_name == '' || t.is_builtin_type_name(type_name) {
		return true
	}
	name := type_name.replace('.', '__')
	if name.contains('__') {
		mod_prefix := module_name.replace('.', '__')
		return name.starts_with('${mod_prefix}__')
	}
	if module_name == '' || module_name == 'main' {
		return true
	}
	if scope := t.get_module_scope(module_name) {
		if obj := scope.objects[name] {
			if _ := transformer_object_type(obj) {
				return true
			}
		}
	}
	return false
}

fn (mut t Transformer) register_generic_struct_env_type(base_struct types.Struct, spec GenericStructSpec) {
	substituted := substitute_type(types.Type(base_struct), spec.bindings)
	mut struct_info := if substituted is types.Struct {
		substituted as types.Struct
	} else {
		base_struct
	}
	concrete_type := types.Type(types.Struct{
		name:           spec.concrete_c_name
		generic_params: []string{}
		implements:     struct_info.implements
		embedded:       struct_info.embedded
		fields:         struct_info.fields
		is_soa:         struct_info.is_soa
	})
	if mut scope := t.get_module_scope(spec.module_name) {
		scope.insert_type(spec.concrete_short_name, concrete_type)
		scope.objects[spec.concrete_short_name] = types.TypeObject{
			typ: concrete_type
		}
	}
}

fn (t &Transformer) generic_struct_spec_key_from_type_expr(expr ast.Expr) ?string {
	mut lhs := ast.empty_expr
	mut args := []ast.Expr{}
	match expr {
		ast.GenericArgs {
			lhs = expr.lhs
			args = expr.args.clone()
		}
		ast.GenericArgOrIndexExpr {
			lhs = expr.lhs
			args = [expr.expr]
		}
		ast.IndexExpr {
			lhs = expr.lhs
			args = [expr.expr]
		}
		ast.Type {
			if expr is ast.GenericType {
				lhs = expr.name
				args = expr.params.clone()
			} else {
				return none
			}
		}
		ast.ModifierExpr {
			return t.generic_struct_spec_key_from_type_expr(expr.expr)
		}
		ast.PrefixExpr {
			return t.generic_struct_spec_key_from_type_expr(expr.expr)
		}
		else {
			return none
		}
	}

	base_type := t.lookup_type_from_expr(lhs) or { return none }
	if base_type !is types.Struct {
		return none
	}
	base_struct := base_type as types.Struct
	if base_struct.generic_params.len == 0 {
		return none
	}
	runtime_args := generic_struct_runtime_args(args)
	if runtime_args.len == 0 {
		return none
	}
	base_c_name := t.type_to_c_name(types.Type(base_struct))
	suffix := t.generic_specialization_suffix(runtime_args)
	if base_c_name == '' || suffix == '' {
		return none
	}
	return base_c_name + suffix
}

fn (t &Transformer) generic_struct_spec_from_type_expr(expr ast.Expr) ?GenericStructSpec {
	key := t.generic_struct_spec_key_from_type_expr(expr) or { return none }
	return t.generic_struct_specs[key] or { return none }
}

fn (mut t Transformer) concrete_generic_struct_type_expr(expr ast.Expr) ?ast.Expr {
	spec := t.generic_struct_spec_from_type_expr(expr) or { return none }
	mut pos := expr.pos()
	if !pos.is_valid() {
		pos = t.next_synth_pos()
	}
	concrete_type := types.Type(types.Struct{
		name: spec.concrete_c_name
	})
	t.register_synth_type(pos, concrete_type)
	return ast.Expr(ast.Ident{
		name: spec.concrete_c_name
		pos:  pos
	})
}

fn (mut t Transformer) clone_generic_struct_decl(decl ast.StructDecl, spec GenericStructSpec, target_module string) ast.StructDecl {
	old_module := t.cur_module
	old_scope := t.scope
	t.cur_module = spec.module_name
	if scope := t.get_module_scope(spec.module_name) {
		t.scope = scope
	}
	should_qualify_source_types := target_module != spec.module_name && spec.module_name != ''
		&& spec.module_name != 'main' && spec.module_name != 'builtin'
	mut fields := []ast.FieldDecl{cap: decl.fields.len}
	for field in decl.fields {
		mut field_typ := t.substitute_type_in_expr(field.typ, spec.bindings)
		mut field_value := t.clone_expr_with_bindings(field.value, spec.bindings)
		if should_qualify_source_types {
			field_typ = t.qualify_moved_clone_type_expr(field_typ, spec.module_name)
			field_value = t.qualify_moved_clone_expr_type_positions(field_value, spec.module_name)
		}
		fields << ast.FieldDecl{
			name:                field.name
			typ:                 field_typ
			value:               field_value
			attributes:          field.attributes
			is_public:           field.is_public
			is_mut:              field.is_mut
			is_module_mut:       field.is_module_mut
			is_interface_method: field.is_interface_method
		}
	}
	mut embedded := []ast.Expr{cap: decl.embedded.len}
	for item in decl.embedded {
		mut embedded_type := t.substitute_type_in_expr(item, spec.bindings)
		if should_qualify_source_types {
			embedded_type = t.qualify_moved_clone_type_expr(embedded_type, spec.module_name)
		}
		embedded << embedded_type
	}
	mut implemented_types := []ast.Expr{cap: decl.implements.len}
	for item in decl.implements {
		mut implemented_type := t.substitute_type_in_expr(item, spec.bindings)
		if should_qualify_source_types {
			implemented_type = t.qualify_moved_clone_type_expr(implemented_type, spec.module_name)
		}
		implemented_types << implemented_type
	}
	cloned := ast.StructDecl{
		attributes:     decl.attributes
		is_public:      decl.is_public
		is_union:       decl.is_union
		implements:     implemented_types
		embedded:       embedded
		language:       decl.language
		name:           if target_module == spec.module_name {
			spec.concrete_short_name
		} else {
			spec.concrete_c_name
		}
		generic_params: []ast.Expr{}
		fields:         fields
		pos:            decl.pos
	}
	t.cur_module = old_module
	t.scope = old_scope
	return cloned
}

fn (mut t Transformer) inject_generic_struct_specializations(files []ast.File) []ast.File {
	t.inject_changed_files = false
	t.last_struct_clones = map[int][]ast.Stmt{}
	if t.generic_struct_specs.len == 0 {
		return files
	}
	mut existing := map[string]bool{}
	mut base_decls := map[string]ast.StructDecl{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				c_name := generic_struct_decl_c_name(stmt, file.mod)
				existing[c_name] = true
				if stmt.generic_params.len > 0 {
					base_decls[c_name] = stmt
				}
			}
		}
	}
	spec_keys := t.generic_struct_specs.keys()
	mut sorted_keys := spec_keys.clone()
	sorted_keys.sort()
	// Fast path: if every struct spec is already present in `files`, nothing
	// will be injected and the loop below would only rebuild an identical file
	// set. Return the input unchanged to avoid duplicating every file's stmt
	// list (~1.5GB under -gc none on the fixpoint loop's confirmation pass).
	// Mirrors monomorphize_pass's `per_file_clones.len == 0 { return files }`.
	mut any_pending := false
	for key in sorted_keys {
		spec := t.generic_struct_specs[key] or { continue }
		if spec.concrete_c_name !in existing {
			any_pending = true
			break
		}
	}
	if !any_pending {
		return files
	}
	mut out := []ast.File{cap: files.len}
	for file in files {
		mut stmts := []ast.Stmt{cap: file.stmts.len}
		for stmt in file.stmts {
			stmts << stmt
		}
		mut injected := []ast.Stmt{}
		for key in sorted_keys {
			spec := t.generic_struct_specs[key] or { continue }
			if spec.file_idx < 0 || spec.file_idx >= files.len || spec.file_idx != out.len
				|| spec.concrete_c_name in existing {
				continue
			}
			struct_decl := base_decls[spec.base_c_name] or { continue }
			cloned := ast.Stmt(t.clone_generic_struct_decl(struct_decl, spec, file.mod))
			stmts << cloned
			injected << cloned
			existing[spec.concrete_c_name] = true
		}
		for key in sorted_keys {
			spec := t.generic_struct_specs[key] or { continue }
			if (spec.file_idx >= 0 && spec.file_idx < files.len)
				|| spec.module_name != file.mod || spec.concrete_c_name in existing {
				continue
			}
			struct_decl := base_decls[spec.base_c_name] or { continue }
			cloned := ast.Stmt(t.clone_generic_struct_decl(struct_decl, spec, file.mod))
			stmts << cloned
			injected << cloned
			existing[spec.concrete_c_name] = true
		}
		if injected.len > 0 {
			t.last_struct_clones[out.len] = injected
		}
		out << ast.File{
			attributes:     file.attributes
			mod:            file.mod
			name:           file.name
			stmts:          stmts
			imports:        file.imports
			selector_names: file.selector_names
		}
	}
	t.inject_changed_files = true
	return out
}

fn (t &Transformer) generic_bindings_from_type_expr(expr ast.Expr) ?map[string]types.Type {
	match expr {
		ast.GenericArgs {
			return t.generic_bindings_from_generic_type_parts(expr.lhs, expr.args)
		}
		ast.GenericArgOrIndexExpr {
			return t.generic_bindings_from_generic_type_parts(expr.lhs, [expr.expr])
		}
		ast.ModifierExpr {
			return t.generic_bindings_from_type_expr(expr.expr)
		}
		ast.PrefixExpr {
			return t.generic_bindings_from_type_expr(expr.expr)
		}
		ast.Type {
			match expr {
				ast.GenericType {
					return t.generic_bindings_from_generic_type_parts(expr.name, expr.params)
				}
				ast.PointerType {
					return t.generic_bindings_from_type_expr(expr.base_type)
				}
				else {}
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) generic_bindings_from_generic_type_parts(lhs ast.Expr, args []ast.Expr) ?map[string]types.Type {
	generic_params := t.generic_template_type_param_names_from_type_lhs(lhs) or { return none }
	return t.generic_type_arg_bindings(generic_params, args)
}

fn (t &Transformer) generic_template_type_param_names_from_type_lhs(lhs ast.Expr) ?[]string {
	if base := t.generic_template_struct_from_type_lhs(lhs) {
		return base.generic_params.clone()
	}
	match lhs {
		ast.Ident {
			return t.generic_template_type_param_names_from_type_name(lhs.name)
		}
		ast.ModifierExpr {
			return t.generic_template_type_param_names_from_type_lhs(lhs.expr)
		}
		ast.PrefixExpr {
			return t.generic_template_type_param_names_from_type_lhs(lhs.expr)
		}
		else {}
	}

	return none
}

fn (t &Transformer) generic_template_type_param_names_from_type_name(name string) ?[]string {
	if name == '' {
		return none
	}
	if typ := t.lookup_type(name) {
		params := generic_template_type_param_names_from_type(typ)
		if params.len > 0 {
			return params
		}
	}
	if name.contains('_T_') {
		return t.generic_template_type_param_names_from_type_name(name.all_before('_T_'))
	}
	return none
}

fn generic_template_type_param_names_from_type(typ types.Type) []string {
	mut seen := map[string]bool{}
	mut structural_seen := map[string]bool{}
	mut names := []string{}
	collect_declared_generic_template_type_param_names(typ, mut seen, mut structural_seen, mut
		names)
	return names
}

fn generic_template_structural_seen_key(kind string, name string, generic_params []string) string {
	if name == '' {
		return ''
	}
	return '${kind}:${name}:${generic_params.join(',')}'
}

fn collect_declared_generic_template_type_param_names(typ types.Type, mut seen map[string]bool, mut structural_seen map[string]bool, mut names []string) {
	match typ {
		types.Alias {
			collect_declared_generic_template_type_param_names(typ.base_type, mut seen, mut
				structural_seen, mut names)
		}
		types.Array {
			collect_declared_generic_template_type_param_names(typ.elem_type, mut seen, mut
				structural_seen, mut names)
		}
		types.ArrayFixed {
			collect_declared_generic_template_type_param_names(typ.elem_type, mut seen, mut
				structural_seen, mut names)
		}
		types.Channel {
			if elem_type := typ.elem_type {
				collect_declared_generic_template_type_param_names(elem_type, mut seen, mut
					structural_seen, mut names)
			}
		}
		types.Map {
			collect_declared_generic_template_type_param_names(typ.key_type, mut seen, mut
				structural_seen, mut names)
			collect_declared_generic_template_type_param_names(typ.value_type, mut seen, mut
				structural_seen, mut names)
		}
		types.NamedType {
			name := string(typ)
			if is_generic_placeholder_ident(name) && name !in seen {
				seen[name] = true
				names << name
			}
		}
		types.OptionType {
			collect_declared_generic_template_type_param_names(typ.base_type, mut seen, mut
				structural_seen, mut names)
		}
		types.Pointer {
			collect_declared_generic_template_type_param_names(typ.base_type, mut seen, mut
				structural_seen, mut names)
		}
		types.ResultType {
			collect_declared_generic_template_type_param_names(typ.base_type, mut seen, mut
				structural_seen, mut names)
		}
		types.Struct {
			for param in typ.generic_params {
				if is_generic_placeholder_ident(param) && param !in seen {
					seen[param] = true
					names << param
				}
			}
			struct_key := generic_template_structural_seen_key('struct', typ.name,
				typ.generic_params)
			if struct_key != '' {
				if structural_seen[struct_key] {
					return
				}
				structural_seen[struct_key] = true
			}
			for field in typ.fields {
				collect_declared_generic_template_type_param_names(field.typ, mut seen, mut
					structural_seen, mut names)
			}
			for embedded in typ.embedded {
				collect_declared_generic_template_type_param_names(types.Type(embedded), mut seen, mut
					structural_seen, mut names)
			}
		}
		types.SumType {
			for param in typ.generic_params {
				if is_generic_placeholder_ident(param) && param !in seen {
					seen[param] = true
					names << param
				}
			}
			sumtype_key := generic_template_structural_seen_key('sumtype', typ.name,
				typ.generic_params)
			if sumtype_key != '' {
				if structural_seen[sumtype_key] {
					return
				}
				structural_seen[sumtype_key] = true
			}
			if typ.generic_params.len > 0 {
				return
			}
			for variant in typ.variants {
				collect_declared_generic_template_type_param_names(variant, mut seen, mut
					structural_seen, mut names)
			}
		}
		else {}
	}
}

fn (t &Transformer) generic_template_struct_from_type_lhs(lhs ast.Expr) ?types.Struct {
	match lhs {
		ast.Ident {
			return t.generic_template_struct_from_type_name(lhs.name)
		}
		ast.SelectorExpr {
			if lhs.lhs is ast.Ident {
				lhs_ident := lhs.lhs as ast.Ident
				mut module_names := []string{}
				if full_name := t.cur_import_aliases[lhs_ident.name] {
					module_names << module_call_c_prefix(full_name)
					module_names << full_name.replace('.', '__')
				}
				if prefix := t.resolve_module_call_prefix(lhs_ident.name) {
					module_names << prefix
				}
				module_names << lhs_ident.name.replace('.', '__')
				mut seen := map[string]bool{}
				for module_name in module_names {
					if module_name == '' || module_name in seen {
						continue
					}
					seen[module_name] = true
					if base := t.generic_template_struct_from_type_name('${module_name}__${lhs.rhs.name}') {
						return base
					}
				}
			}
			return t.generic_template_struct_from_type_name(lhs.rhs.name)
		}
		ast.ModifierExpr {
			return t.generic_template_struct_from_type_lhs(lhs.expr)
		}
		ast.PrefixExpr {
			return t.generic_template_struct_from_type_lhs(lhs.expr)
		}
		else {}
	}

	return none
}

fn (t &Transformer) generic_template_struct_from_type_name(name string) ?types.Struct {
	if name == '' {
		return none
	}
	if typ := t.lookup_type(name) {
		if typ is types.Struct && typ.generic_params.len > 0 {
			return typ
		}
	}
	if name.contains('_T_') {
		return t.generic_template_struct_from_type_name(name.all_before('_T_'))
	}
	return none
}

fn struct_decl_field_lookup_names(name string, module_name string) []string {
	if name == '' {
		return []string{}
	}
	mut names := [name]
	if module_name != '' && module_name != 'main' && module_name != 'builtin' {
		names << '${module_name.replace('.', '__')}__${name}'
	}
	return names
}

fn embedded_field_name_from_type_expr(expr ast.Expr) string {
	match expr {
		ast.Ident {
			return embedded_field_name_from_type_name(expr.name)
		}
		ast.SelectorExpr {
			return embedded_field_name_from_type_name(expr.rhs.name)
		}
		ast.GenericArgOrIndexExpr {
			return embedded_field_name_from_type_expr(expr.lhs)
		}
		ast.GenericArgs {
			return embedded_field_name_from_type_expr(expr.lhs)
		}
		ast.ModifierExpr {
			return embedded_field_name_from_type_expr(expr.expr)
		}
		ast.PrefixExpr {
			return embedded_field_name_from_type_expr(expr.expr)
		}
		ast.Type {
			match expr {
				ast.GenericType {
					return embedded_field_name_from_type_expr(expr.name)
				}
				ast.PointerType {
					return embedded_field_name_from_type_expr(expr.base_type)
				}
				else {
					return ''
				}
			}
		}
		else {
			return ''
		}
	}
}

fn embedded_field_name_from_type_expr_cursor(expr ast.Cursor) string {
	match expr.kind() {
		.expr_ident {
			return embedded_field_name_from_type_name(expr.name())
		}
		.expr_selector {
			return embedded_field_name_from_type_name(selector_rhs_name_cursor(expr))
		}
		.expr_generic_arg_or_index, .expr_generic_args, .expr_index {
			return embedded_field_name_from_type_expr_cursor(expr.edge(0))
		}
		.expr_modifier, .expr_prefix {
			return embedded_field_name_from_type_expr_cursor(expr.edge(0))
		}
		.typ_generic {
			return embedded_field_name_from_type_expr_cursor(expr.edge(0))
		}
		.typ_pointer {
			return embedded_field_name_from_type_expr_cursor(expr.edge(0))
		}
		else {
			return ''
		}
	}
}

fn embedded_field_name_from_type_name(name string) string {
	if name == '' {
		return ''
	}
	mut field_name := if name.contains('__') { name.all_after_last('__') } else { name }
	if field_name.contains('.') {
		field_name = field_name.all_after_last('.')
	}
	if field_name.contains('_T_') {
		field_name = field_name.all_before('_T_')
	}
	return field_name
}

fn field_type_expr_has_generic_args(expr ast.Expr) bool {
	match expr {
		ast.GenericArgOrIndexExpr, ast.GenericArgs {
			return true
		}
		ast.ModifierExpr {
			return field_type_expr_has_generic_args(expr.expr)
		}
		ast.PrefixExpr {
			return field_type_expr_has_generic_args(expr.expr)
		}
		ast.Type {
			match expr {
				ast.ArrayType {
					return field_type_expr_has_generic_args(expr.elem_type)
				}
				ast.ArrayFixedType {
					return field_type_expr_has_generic_args(expr.elem_type)
				}
				ast.ChannelType {
					return field_type_expr_has_generic_args(expr.elem_type)
				}
				ast.FnType {
					for param in expr.params {
						if field_type_expr_has_generic_args(param.typ) {
							return true
						}
					}
					return field_type_expr_has_generic_args(expr.return_type)
				}
				ast.GenericType {
					return true
				}
				ast.MapType {
					return field_type_expr_has_generic_args(expr.key_type)
						|| field_type_expr_has_generic_args(expr.value_type)
				}
				ast.OptionType {
					return field_type_expr_has_generic_args(expr.base_type)
				}
				ast.PointerType {
					return field_type_expr_has_generic_args(expr.base_type)
				}
				ast.ResultType {
					return field_type_expr_has_generic_args(expr.base_type)
				}
				ast.ThreadType {
					return field_type_expr_has_generic_args(expr.elem_type)
				}
				else {
					return false
				}
			}
		}
		else {
			return false
		}
	}
}

fn field_type_cursor_has_generic_args(expr ast.Cursor) bool {
	match expr.kind() {
		.expr_generic_arg_or_index, .expr_generic_args {
			return true
		}
		.expr_modifier, .expr_prefix {
			return field_type_cursor_has_generic_args(expr.edge(0))
		}
		.typ_array {
			return field_type_cursor_has_generic_args(expr.edge(0))
		}
		.typ_array_fixed {
			return field_type_cursor_has_generic_args(expr.edge(1))
		}
		.typ_channel {
			return field_type_cursor_has_generic_args(expr.edge(1))
		}
		.typ_fn {
			params := expr.list_at(1)
			for i in 0 .. params.len() {
				if field_type_cursor_has_generic_args(params.at(i).edge(0)) {
					return true
				}
			}
			return field_type_cursor_has_generic_args(expr.edge(2))
		}
		.typ_generic {
			return true
		}
		.typ_map {
			return field_type_cursor_has_generic_args(expr.edge(0))
				|| field_type_cursor_has_generic_args(expr.edge(1))
		}
		.typ_option, .typ_pointer, .typ_result, .typ_thread {
			return field_type_cursor_has_generic_args(expr.edge(0))
		}
		.typ_tuple {
			for i in 0 .. expr.edge_count() {
				if field_type_cursor_has_generic_args(expr.edge(i)) {
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

fn (t &Transformer) generic_bindings_from_type_expr_cursor(expr ast.Cursor) ?map[string]types.Type {
	match expr.kind() {
		.expr_generic_args {
			base := t.lookup_type_from_expr_cursor(expr.edge(0)) or { return none }
			return t.generic_bindings_from_template_type_cursor(base, cursor_edges(expr, 1))
		}
		.expr_generic_arg_or_index {
			base := t.lookup_type_from_expr_cursor(expr.edge(0)) or { return none }
			return t.generic_bindings_from_template_type_cursor(base, [
				expr.edge(1),
			])
		}
		.expr_modifier, .expr_prefix {
			return t.generic_bindings_from_type_expr_cursor(expr.edge(0))
		}
		.typ_generic {
			base := t.lookup_type_from_expr_cursor(expr.edge(0)) or { return none }
			return t.generic_bindings_from_template_type_cursor(base, cursor_edges(expr, 1))
		}
		.typ_pointer {
			return t.generic_bindings_from_type_expr_cursor(expr.edge(0))
		}
		else {}
	}

	return none
}

fn (t &Transformer) generic_bindings_from_template_type_cursor(base types.Type, args []ast.Cursor) ?map[string]types.Type {
	generic_params := generic_template_type_param_names_from_type(base)
	if generic_params.len == 0 {
		return none
	}
	arg_bindings := t.generic_type_arg_bindings_cursor(generic_params, args) or { return none }
	return arg_bindings.bindings.clone()
}

fn struct_field_generic_decl_key(struct_name string, field_name string) string {
	return '${struct_name}.${field_name}'
}

// monomorphize_pass walks env.generic_types, clones each generic FnDecl per
// binding map with concrete types substituted, and appends the clones to the
// owning file's stmts.
pub fn (mut t Transformer) monomorphize_pass(files []ast.File) []ast.File {
	// Index generic FnDecls by names that can appear in env.generic_types,
	// including module-qualified functions and method receiver keys.
	mut decl_owner := map[string]int{} // fn key -> file index
	mut decl_node := map[string]ast.FnDecl{}
	for fi, file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				if decl_generic_param_names(stmt).len == 0 {
					continue
				}
				t.index_generic_fn_decl_for_monomorphize(mut decl_owner, mut decl_node, stmt, fi,
					file.mod)
			}
		}
	}
	// Per-file accumulator for cloned stmts (keyed by file index).
	mut per_file_clones := map[int][]ast.Stmt{}
	old_deferred_specs := t.deferred_generic_call_specs.clone()
	t.deferred_generic_call_specs = []DeferredGenericCallSpec{}
	for fn_key, bindings_list in t.env.generic_types {
		lookup_key := t.resolve_monomorphize_decl_key(fn_key, decl_node) or { continue }
		decl := decl_node[lookup_key] or { continue }
		fi := decl_owner[lookup_key] or { continue }
		for bindings in bindings_list {
			spec_name := t.specialized_fn_name(decl, bindings).clone()
			if spec_name == decl.name {
				continue
			}
			clone_name := t.monomorphized_clone_name(lookup_key, decl, spec_name, bindings).clone()
			clone_ast_name := t.monomorphized_clone_ast_name(decl, clone_name, bindings).clone()
			spec_key := '${lookup_key}:${clone_name}'.clone()
			if spec_key in t.monomorphized_specs {
				continue
			}
			t.monomorphized_specs[spec_key] = true
			owner_key := generic_spec_owner_key(fn_key, bindings)
			owner_file := t.generic_spec_owner_file[owner_key] or { fi }
			nested_struct_file := if t.generic_bindings_visible_in_module(files[fi].mod, bindings) {
				fi
			} else {
				owner_file
			}
			if nested_struct_file < 0 || nested_struct_file >= files.len {
				continue
			}
			clone_file := nested_struct_file
			t.register_monomorphized_fn_bindings(files[fi].mod, clone_name, bindings)
			old_owner_file := t.cur_generic_call_file_idx
			old_import_aliases := t.cur_import_aliases.clone()
			t.cur_generic_call_file_idx = clone_file
			t.cur_import_aliases = import_aliases_for_generic_collect(files[clone_file].imports)
			mut cloned := t.clone_fn_decl_with_substitutions(decl, bindings, clone_ast_name,
				files[fi].mod, files[clone_file].mod)
			if files[clone_file].mod != files[fi].mod {
				cloned = t.qualify_moved_clone_source_module_types(cloned, files[fi].mod)
			}
			t.cur_generic_call_file_idx = old_owner_file
			t.cur_import_aliases = old_import_aliases.clone()
			mut bucket := []ast.Stmt{}
			if clone_file in per_file_clones {
				bucket = per_file_clones[clone_file]
			}
			bucket << ast.Stmt(cloned)
			per_file_clones[clone_file] = bucket
		}
	}
	deferred_specs := t.deferred_generic_call_specs.clone()
	t.deferred_generic_call_specs = old_deferred_specs.clone()
	t.flush_deferred_generic_call_specs(deferred_specs)
	// Record the freshly-materialized clones so the fixpoint can rescan only
	// them (collect_generic_call_specs_in_new_clones) instead of all files.
	t.last_mono_clones = per_file_clones.clone()
	if per_file_clones.len == 0 {
		return files
	}
	mut new_files := []ast.File{cap: files.len}
	for fi, file in files {
		if fi !in per_file_clones {
			new_files << file
			continue
		}
		extra := per_file_clones[fi]
		mut stmts := file.stmts.clone()
		stmts << extra
		new_files << ast.File{
			attributes:     file.attributes
			mod:            file.mod
			name:           file.name
			stmts:          stmts
			imports:        file.imports
			selector_names: file.selector_names
		}
	}
	return new_files
}

fn (mut t Transformer) monomorphize_pass_from_flat(flat &ast.FlatAst, mut extra_stmts map[int][]ast.Stmt) {
	mut decl_owner := map[string]int{}
	mut decl_node := map[string]ast.FnDecl{}
	mut decl_cursor_ids := map[string]ast.FlatNodeId{}
	mut decl_extra_nodes := map[string]ast.FnDecl{}
	for fi in 0 .. flat.files.len {
		module_name := flat.file_mod(flat.files[fi])
		stmts := flat.file_cursor(fi).stmts()
		for si in 0 .. stmts.len() {
			stmt_c := stmts.at(si)
			if stmt_c.kind() != .stmt_fn_decl {
				continue
			}
			decl := stmt_c.fn_decl_signature()
			if decl_generic_param_names(decl).len == 0 {
				continue
			}
			for key in t.generic_fn_decl_monomorphize_keys(decl, module_name) {
				if key == '' || key in decl_node {
					continue
				}
				decl_owner[key] = fi
				decl_node[key] = decl
				decl_cursor_ids[key] = stmt_c.id
			}
		}
		extra := extra_stmts[fi] or { []ast.Stmt{} }
		for stmt in extra {
			if stmt !is ast.FnDecl {
				continue
			}
			decl := stmt as ast.FnDecl
			if decl_generic_param_names(decl).len == 0 {
				continue
			}
			for key in t.generic_fn_decl_monomorphize_keys(decl, module_name) {
				if key == '' || key in decl_node {
					continue
				}
				decl_owner[key] = fi
				decl_node[key] = decl
				decl_extra_nodes[key] = decl
			}
		}
	}
	mut per_file_clones := map[int][]ast.Stmt{}
	mut decl_full_cache := map[string]ast.FnDecl{}
	old_deferred_specs := t.deferred_generic_call_specs.clone()
	t.deferred_generic_call_specs = []DeferredGenericCallSpec{}
	for fn_key, bindings_list in t.env.generic_types {
		lookup_key := t.resolve_monomorphize_decl_key(fn_key, decl_node) or { continue }
		decl_sig := decl_node[lookup_key] or { continue }
		fi := decl_owner[lookup_key] or { continue }
		decl_mod := flat.file_mod(flat.files[fi])
		for bindings in bindings_list {
			spec_name := t.specialized_fn_name(decl_sig, bindings).clone()
			if spec_name == decl_sig.name {
				continue
			}
			clone_name :=
				t.monomorphized_clone_name(lookup_key, decl_sig, spec_name, bindings).clone()
			clone_ast_name := t.monomorphized_clone_ast_name(decl_sig, clone_name, bindings).clone()
			spec_key := '${lookup_key}:${clone_name}'.clone()
			if spec_key in t.monomorphized_specs {
				continue
			}
			t.monomorphized_specs[spec_key] = true
			owner_key := generic_spec_owner_key(fn_key, bindings)
			owner_file := t.generic_spec_owner_file[owner_key] or { fi }
			nested_struct_file := if t.generic_bindings_visible_in_module(decl_mod, bindings) {
				fi
			} else {
				owner_file
			}
			if nested_struct_file < 0 || nested_struct_file >= flat.files.len {
				continue
			}
			clone_file := nested_struct_file
			clone_mod := flat.file_mod(flat.files[clone_file])
			t.register_monomorphized_fn_bindings(decl_mod, clone_name, bindings)
			old_owner_file := t.cur_generic_call_file_idx
			old_import_aliases := t.cur_import_aliases.clone()
			t.cur_generic_call_file_idx = clone_file
			t.cur_import_aliases = flat_import_aliases_for_generic_collect(flat, clone_file)
			decl := decl_full_cache[lookup_key] or {
				full_decl := decl_extra_nodes[lookup_key] or {
					cursor_id := decl_cursor_ids[lookup_key] or { ast.invalid_flat_node_id }
					if cursor_id < 0 {
						continue
					}
					decl_cursor := ast.Cursor{
						flat: unsafe { flat }
						id:   cursor_id
					}
					fn_decl_signature_with_body_cursor(decl_sig, decl_cursor)
				}
				decl_full_cache[lookup_key] = full_decl
				full_decl
			}
			mut cloned := t.clone_fn_decl_with_substitutions(decl, bindings, clone_ast_name,
				decl_mod, clone_mod)
			if clone_mod != decl_mod {
				cloned = t.qualify_moved_clone_source_module_types(cloned, decl_mod)
			}
			t.cur_generic_call_file_idx = old_owner_file
			t.cur_import_aliases = old_import_aliases.clone()
			mut bucket := per_file_clones[clone_file] or { []ast.Stmt{} }
			bucket << ast.Stmt(cloned)
			per_file_clones[clone_file] = bucket
		}
	}
	deferred_specs := t.deferred_generic_call_specs.clone()
	t.deferred_generic_call_specs = old_deferred_specs.clone()
	t.flush_deferred_generic_call_specs(deferred_specs)
	t.last_mono_clones = per_file_clones.clone()
	for fi, clones in per_file_clones {
		if clones.len == 0 {
			continue
		}
		append_flat_extra_stmts(mut extra_stmts, fi, clones)
	}
}

fn fn_decl_signature_with_body_cursor(signature ast.FnDecl, decl_cursor ast.Cursor) ast.FnDecl {
	return ast.FnDecl{
		attributes: signature.attributes
		is_public:  signature.is_public
		is_method:  signature.is_method
		is_static:  signature.is_static
		receiver:   signature.receiver
		language:   signature.language
		name:       signature.name
		typ:        signature.typ
		stmts:      decl_cursor.list_at(3).stmts()
		pos:        signature.pos
	}
}

fn (mut t Transformer) inject_generic_struct_specializations_from_flat(flat &ast.FlatAst, mut extra_stmts map[int][]ast.Stmt) {
	t.inject_changed_files = false
	t.last_struct_clones = map[int][]ast.Stmt{}
	if t.generic_struct_specs.len == 0 {
		return
	}
	mut existing := map[string]bool{}
	mut base_decls := map[string]ast.StructDecl{}
	for fi in 0 .. flat.files.len {
		module_name := flat.file_mod(flat.files[fi])
		for stmt in flat_file_struct_decls_with_extra(flat, extra_stmts, fi) {
			c_name := generic_struct_decl_c_name(stmt, module_name)
			existing[c_name] = true
			if stmt.generic_params.len > 0 {
				base_decls[c_name] = stmt
			}
		}
	}
	spec_keys := t.generic_struct_specs.keys()
	mut sorted_keys := spec_keys.clone()
	sorted_keys.sort()
	mut any_pending := false
	for key in sorted_keys {
		spec := t.generic_struct_specs[key] or { continue }
		if spec.concrete_c_name !in existing {
			any_pending = true
			break
		}
	}
	if !any_pending {
		return
	}
	for fi in 0 .. flat.files.len {
		module_name := flat.file_mod(flat.files[fi])
		mut injected := []ast.Stmt{}
		for key in sorted_keys {
			spec := t.generic_struct_specs[key] or { continue }
			if spec.file_idx < 0 || spec.file_idx >= flat.files.len || spec.file_idx != fi
				|| spec.concrete_c_name in existing {
				continue
			}
			struct_decl := base_decls[spec.base_c_name] or { continue }
			cloned := ast.Stmt(t.clone_generic_struct_decl(struct_decl, spec, module_name))
			injected << cloned
			existing[spec.concrete_c_name] = true
		}
		for key in sorted_keys {
			spec := t.generic_struct_specs[key] or { continue }
			if (spec.file_idx >= 0 && spec.file_idx < flat.files.len)
				|| spec.module_name != module_name
				|| spec.concrete_c_name in existing {
				continue
			}
			struct_decl := base_decls[spec.base_c_name] or { continue }
			cloned := ast.Stmt(t.clone_generic_struct_decl(struct_decl, spec, module_name))
			injected << cloned
			existing[spec.concrete_c_name] = true
		}
		if injected.len > 0 {
			t.last_struct_clones[fi] = injected.clone()
			append_flat_extra_stmts(mut extra_stmts, fi, injected)
		}
	}
	t.inject_changed_files = true
}

fn (mut t Transformer) flush_deferred_generic_call_specs(specs []DeferredGenericCallSpec) {
	old_file_idx := t.cur_generic_call_file_idx
	for spec in specs {
		if spec.file_idx >= 0 {
			t.cur_generic_call_file_idx = spec.file_idx
		}
		t.register_generic_bindings(spec.base_name, spec.bindings)
	}
	t.cur_generic_call_file_idx = old_file_idx
}

fn (t &Transformer) qualify_moved_clone_source_module_types(decl ast.FnDecl, source_mod string) ast.FnDecl {
	if source_mod == '' || source_mod == 'main' || source_mod == 'builtin' {
		return decl
	}
	mut receiver := decl.receiver
	receiver.typ = t.qualify_moved_clone_type_expr(receiver.typ, source_mod)
	mut params := []ast.Parameter{cap: decl.typ.params.len}
	for param in decl.typ.params {
		params << ast.Parameter{
			name:   param.name
			typ:    t.qualify_moved_clone_type_expr(param.typ, source_mod)
			is_mut: param.is_mut
			pos:    param.pos
		}
	}
	mut stmts := []ast.Stmt{cap: decl.stmts.len}
	for stmt in decl.stmts {
		stmts << t.qualify_moved_clone_stmt_type_positions(stmt, source_mod)
	}
	return ast.FnDecl{
		attributes: decl.attributes
		is_public:  decl.is_public
		is_method:  decl.is_method
		is_static:  decl.is_static
		receiver:   receiver
		language:   decl.language
		name:       decl.name
		typ:        ast.FnType{
			generic_params: decl.typ.generic_params
			params:         params
			return_type:    t.qualify_moved_clone_type_expr(decl.typ.return_type, source_mod)
		}
		stmts:      stmts
		pos:        decl.pos
	}
}

fn (t &Transformer) qualify_moved_clone_type_expr(expr ast.Expr, source_mod string) ast.Expr {
	match expr {
		ast.Ident {
			if _ := t.get_synth_type(expr.pos) {
				return ast.Expr(expr)
			}
			if !t.should_qualify_moved_clone_type_name(expr.name, source_mod) {
				return ast.Expr(expr)
			}
			return ast.Expr(ast.Ident{
				name: '${source_mod}__${expr.name}'
				pos:  expr.pos
			})
		}
		ast.GenericArgOrIndexExpr {
			return ast.Expr(ast.GenericArgOrIndexExpr{
				lhs:  t.qualify_moved_clone_type_expr(expr.lhs, source_mod)
				expr: t.qualify_moved_clone_type_expr(expr.expr, source_mod)
				pos:  expr.pos
			})
		}
		ast.GenericArgs {
			mut args := []ast.Expr{cap: expr.args.len}
			for arg in expr.args {
				args << t.qualify_moved_clone_type_expr(arg, source_mod)
			}
			return ast.Expr(ast.GenericArgs{
				lhs:  t.qualify_moved_clone_type_expr(expr.lhs, source_mod)
				args: args
				pos:  expr.pos
			})
		}
		ast.ModifierExpr {
			return ast.Expr(ast.ModifierExpr{
				kind: expr.kind
				expr: t.qualify_moved_clone_type_expr(expr.expr, source_mod)
				pos:  expr.pos
			})
		}
		ast.PrefixExpr {
			return ast.Expr(ast.PrefixExpr{
				op:   expr.op
				expr: t.qualify_moved_clone_type_expr(expr.expr, source_mod)
				pos:  expr.pos
			})
		}
		ast.Type {
			return ast.Expr(t.qualify_moved_clone_type_node(expr, source_mod))
		}
		else {
			return expr
		}
	}
}

fn (t &Transformer) should_qualify_moved_clone_type_name(name string, source_mod string) bool {
	if name == '' || name.contains('__') || name.contains('.')
		|| name in ['void', 'voidptr', 'byteptr', 'charptr', 'i8', 'i16', 'i32', 'int', 'i64', 'isize', 'u8', 'byte', 'u16', 'u32', 'u64', 'usize', 'f32', 'f64', 'char', 'rune', 'string', 'bool'] {
		return false
	}
	qualified := '${source_mod}__${name}'
	if _ := t.lookup_type(qualified) {
		return true
	}
	return false
}

fn (t &Transformer) should_qualify_moved_clone_fn_name(name string, source_mod string) bool {
	if name == '' || source_mod == '' || source_mod == 'main' || source_mod == 'builtin'
		|| name.contains('__') || name.contains('.') {
		return false
	}
	base_name := generic_base_name_without_specialization(name)
	if base_name == '' {
		return false
	}
	if _ := t.lookup_fn_cached(source_mod, base_name) {
		return true
	}
	if _ := t.generic_fn_decl_for_call('${source_mod}__${base_name}') {
		return true
	}
	if _ := t.generic_fn_decl_for_call('${source_mod}.${base_name}') {
		return true
	}
	return false
}

fn (t &Transformer) qualify_moved_clone_type_node(typ ast.Type, source_mod string) ast.Type {
	match typ {
		ast.ArrayType {
			return ast.Type(ast.ArrayType{
				elem_type: t.qualify_moved_clone_type_expr(typ.elem_type, source_mod)
			})
		}
		ast.ArrayFixedType {
			return ast.Type(ast.ArrayFixedType{
				len:       typ.len
				elem_type: t.qualify_moved_clone_type_expr(typ.elem_type, source_mod)
			})
		}
		ast.ChannelType {
			return ast.Type(ast.ChannelType{
				cap:       typ.cap
				elem_type: t.qualify_moved_clone_type_expr(typ.elem_type, source_mod)
			})
		}
		ast.FnType {
			mut params := []ast.Parameter{cap: typ.params.len}
			for param in typ.params {
				params << ast.Parameter{
					name:   param.name
					typ:    t.qualify_moved_clone_type_expr(param.typ, source_mod)
					is_mut: param.is_mut
					pos:    param.pos
				}
			}
			return ast.Type(ast.FnType{
				generic_params: typ.generic_params
				params:         params
				return_type:    t.qualify_moved_clone_type_expr(typ.return_type, source_mod)
			})
		}
		ast.GenericType {
			mut params := []ast.Expr{cap: typ.params.len}
			for param in typ.params {
				params << t.qualify_moved_clone_type_expr(param, source_mod)
			}
			return ast.Type(ast.GenericType{
				name:   t.qualify_moved_clone_type_expr(typ.name, source_mod)
				params: params
			})
		}
		ast.MapType {
			return ast.Type(ast.MapType{
				key_type:   t.qualify_moved_clone_type_expr(typ.key_type, source_mod)
				value_type: t.qualify_moved_clone_type_expr(typ.value_type, source_mod)
			})
		}
		ast.OptionType {
			return ast.Type(ast.OptionType{
				base_type: t.qualify_moved_clone_type_expr(typ.base_type, source_mod)
			})
		}
		ast.PointerType {
			return ast.Type(ast.PointerType{
				base_type: t.qualify_moved_clone_type_expr(typ.base_type, source_mod)
				lifetime:  typ.lifetime
			})
		}
		ast.ResultType {
			return ast.Type(ast.ResultType{
				base_type: t.qualify_moved_clone_type_expr(typ.base_type, source_mod)
			})
		}
		ast.ThreadType {
			return ast.Type(ast.ThreadType{
				elem_type: t.qualify_moved_clone_type_expr(typ.elem_type, source_mod)
			})
		}
		else {
			return typ
		}
	}
}

fn (t &Transformer) qualify_moved_clone_stmt_type_positions(stmt ast.Stmt, source_mod string) ast.Stmt {
	match stmt {
		ast.AssertStmt {
			return ast.Stmt(ast.AssertStmt{
				expr:  t.qualify_moved_clone_expr_type_positions(stmt.expr, source_mod)
				extra: t.qualify_moved_clone_expr_type_positions(stmt.extra, source_mod)
			})
		}
		ast.AssignStmt {
			mut lhs := []ast.Expr{cap: stmt.lhs.len}
			for expr in stmt.lhs {
				lhs << t.qualify_moved_clone_expr_type_positions(expr, source_mod)
			}
			mut rhs := []ast.Expr{cap: stmt.rhs.len}
			for expr in stmt.rhs {
				rhs << t.qualify_moved_clone_expr_type_positions(expr, source_mod)
			}
			return ast.Stmt(ast.AssignStmt{
				op:  stmt.op
				lhs: lhs
				rhs: rhs
				pos: stmt.pos
			})
		}
		ast.BlockStmt {
			mut stmts := []ast.Stmt{cap: stmt.stmts.len}
			for child in stmt.stmts {
				stmts << t.qualify_moved_clone_stmt_type_positions(child, source_mod)
			}
			return ast.Stmt(ast.BlockStmt{
				stmts: stmts
			})
		}
		ast.ComptimeStmt {
			return ast.Stmt(ast.ComptimeStmt{
				stmt: t.qualify_moved_clone_stmt_type_positions(stmt.stmt, source_mod)
			})
		}
		ast.ConstDecl {
			mut fields := []ast.FieldInit{cap: stmt.fields.len}
			for field in stmt.fields {
				fields << ast.FieldInit{
					name:  field.name
					value: t.qualify_moved_clone_expr_type_positions(field.value, source_mod)
				}
			}
			return ast.Stmt(ast.ConstDecl{
				fields: fields
			})
		}
		ast.DeferStmt {
			mut stmts := []ast.Stmt{cap: stmt.stmts.len}
			for child in stmt.stmts {
				stmts << t.qualify_moved_clone_stmt_type_positions(child, source_mod)
			}
			return ast.Stmt(ast.DeferStmt{
				mode:  stmt.mode
				stmts: stmts
			})
		}
		ast.ExprStmt {
			return ast.Stmt(ast.ExprStmt{
				expr: t.qualify_moved_clone_expr_type_positions(stmt.expr, source_mod)
			})
		}
		ast.ForInStmt {
			return ast.Stmt(ast.ForInStmt{
				key:   t.qualify_moved_clone_expr_type_positions(stmt.key, source_mod)
				value: t.qualify_moved_clone_expr_type_positions(stmt.value, source_mod)
				expr:  t.qualify_moved_clone_expr_type_positions(stmt.expr, source_mod)
			})
		}
		ast.ForStmt {
			mut stmts := []ast.Stmt{cap: stmt.stmts.len}
			for child in stmt.stmts {
				stmts << t.qualify_moved_clone_stmt_type_positions(child, source_mod)
			}
			return ast.Stmt(ast.ForStmt{
				init:  t.qualify_moved_clone_stmt_type_positions(stmt.init, source_mod)
				cond:  t.qualify_moved_clone_expr_type_positions(stmt.cond, source_mod)
				post:  t.qualify_moved_clone_stmt_type_positions(stmt.post, source_mod)
				stmts: stmts
			})
		}
		ast.GlobalDecl {
			mut fields := []ast.FieldDecl{cap: stmt.fields.len}
			for field in stmt.fields {
				fields << ast.FieldDecl{
					name:                field.name
					typ:                 t.qualify_moved_clone_type_expr(field.typ, source_mod)
					value:               t.qualify_moved_clone_expr_type_positions(field.value,
						source_mod)
					attributes:          field.attributes
					is_public:           field.is_public
					is_mut:              field.is_mut
					is_module_mut:       field.is_module_mut
					is_interface_method: field.is_interface_method
				}
			}
			return ast.Stmt(ast.GlobalDecl{
				attributes: stmt.attributes
				fields:     fields
				is_public:  stmt.is_public
			})
		}
		ast.LabelStmt {
			return ast.Stmt(ast.LabelStmt{
				name: stmt.name
				stmt: t.qualify_moved_clone_stmt_type_positions(stmt.stmt, source_mod)
			})
		}
		ast.ReturnStmt {
			mut exprs := []ast.Expr{cap: stmt.exprs.len}
			for expr in stmt.exprs {
				exprs << t.qualify_moved_clone_expr_type_positions(expr, source_mod)
			}
			return ast.Stmt(ast.ReturnStmt{
				exprs: exprs
			})
		}
		else {
			return stmt
		}
	}
}

fn (t &Transformer) qualify_moved_clone_expr_type_positions(expr ast.Expr, source_mod string) ast.Expr {
	match expr {
		ast.Ident {
			if expr.name.contains('_T_')
				&& t.should_qualify_moved_clone_fn_name(expr.name, source_mod) {
				return ast.Expr(ast.Ident{
					name: '${source_mod}__${expr.name}'
					pos:  expr.pos
				})
			}
			return ast.Expr(expr)
		}
		ast.ArrayInitExpr {
			mut exprs := []ast.Expr{cap: expr.exprs.len}
			for item in expr.exprs {
				exprs << t.qualify_moved_clone_expr_type_positions(item, source_mod)
			}
			return ast.Expr(ast.ArrayInitExpr{
				typ:         t.qualify_moved_clone_type_expr(expr.typ, source_mod)
				exprs:       exprs
				init:        t.qualify_moved_clone_expr_type_positions(expr.init, source_mod)
				cap:         t.qualify_moved_clone_expr_type_positions(expr.cap, source_mod)
				len:         t.qualify_moved_clone_expr_type_positions(expr.len, source_mod)
				update_expr: t.qualify_moved_clone_expr_type_positions(expr.update_expr, source_mod)
				pos:         expr.pos
			})
		}
		ast.AsCastExpr {
			return ast.Expr(ast.AsCastExpr{
				expr: t.qualify_moved_clone_expr_type_positions(expr.expr, source_mod)
				typ:  t.qualify_moved_clone_type_expr(expr.typ, source_mod)
				pos:  expr.pos
			})
		}
		ast.AssocExpr {
			mut fields := []ast.FieldInit{cap: expr.fields.len}
			for field in expr.fields {
				fields << ast.FieldInit{
					name:  field.name
					value: t.qualify_moved_clone_expr_type_positions(field.value, source_mod)
				}
			}
			return ast.Expr(ast.AssocExpr{
				typ:    t.qualify_moved_clone_type_expr(expr.typ, source_mod)
				expr:   t.qualify_moved_clone_expr_type_positions(expr.expr, source_mod)
				fields: fields
				pos:    expr.pos
			})
		}
		ast.CallExpr {
			mut args := []ast.Expr{cap: expr.args.len}
			for arg in expr.args {
				args << t.qualify_moved_clone_expr_type_positions(arg, source_mod)
			}
			return ast.Expr(ast.CallExpr{
				lhs:  t.qualify_moved_clone_call_lhs_type_positions(expr.lhs, source_mod)
				args: args
				pos:  expr.pos
			})
		}
		ast.CallOrCastExpr {
			return ast.Expr(ast.CallOrCastExpr{
				lhs:  t.qualify_moved_clone_call_lhs_type_positions(expr.lhs, source_mod)
				expr: t.qualify_moved_clone_expr_type_positions(expr.expr, source_mod)
				pos:  expr.pos
			})
		}
		ast.CastExpr {
			return ast.Expr(ast.CastExpr{
				typ:  t.qualify_moved_clone_type_expr(expr.typ, source_mod)
				expr: t.qualify_moved_clone_expr_type_positions(expr.expr, source_mod)
				pos:  expr.pos
			})
		}
		ast.ComptimeExpr {
			return ast.Expr(ast.ComptimeExpr{
				expr: t.qualify_moved_clone_expr_type_positions(expr.expr, source_mod)
				pos:  expr.pos
			})
		}
		ast.FieldInit {
			return ast.Expr(ast.FieldInit{
				name:  expr.name
				value: t.qualify_moved_clone_expr_type_positions(expr.value, source_mod)
			})
		}
		ast.IfExpr {
			mut stmts := []ast.Stmt{cap: expr.stmts.len}
			for stmt in expr.stmts {
				stmts << t.qualify_moved_clone_stmt_type_positions(stmt, source_mod)
			}
			return ast.Expr(ast.IfExpr{
				cond:      t.qualify_moved_clone_expr_type_positions(expr.cond, source_mod)
				stmts:     stmts
				else_expr: t.qualify_moved_clone_expr_type_positions(expr.else_expr, source_mod)
				pos:       expr.pos
			})
		}
		ast.IfGuardExpr {
			stmt := t.qualify_moved_clone_stmt_type_positions(ast.Stmt(expr.stmt), source_mod)
			if stmt is ast.AssignStmt {
				return ast.Expr(ast.IfGuardExpr{
					stmt: stmt
					pos:  expr.pos
				})
			}
			return expr
		}
		ast.IndexExpr {
			return ast.Expr(ast.IndexExpr{
				lhs:      t.qualify_moved_clone_expr_type_positions(expr.lhs, source_mod)
				expr:     t.qualify_moved_clone_expr_type_positions(expr.expr, source_mod)
				is_gated: expr.is_gated
				pos:      expr.pos
			})
		}
		ast.InfixExpr {
			return ast.Expr(ast.InfixExpr{
				op:  expr.op
				lhs: t.qualify_moved_clone_expr_type_positions(expr.lhs, source_mod)
				rhs: t.qualify_moved_clone_expr_type_positions(expr.rhs, source_mod)
				pos: expr.pos
			})
		}
		ast.InitExpr {
			mut fields := []ast.FieldInit{cap: expr.fields.len}
			for field in expr.fields {
				fields << ast.FieldInit{
					name:  field.name
					value: t.qualify_moved_clone_expr_type_positions(field.value, source_mod)
				}
			}
			return ast.Expr(ast.InitExpr{
				typ:    t.qualify_moved_clone_type_expr(expr.typ, source_mod)
				fields: fields
				pos:    expr.pos
			})
		}
		ast.KeywordOperator {
			mut exprs := []ast.Expr{cap: expr.exprs.len}
			for item in expr.exprs {
				exprs << t.qualify_moved_clone_expr_type_positions(item, source_mod)
			}
			return ast.Expr(ast.KeywordOperator{
				op:    expr.op
				exprs: exprs
				pos:   expr.pos
			})
		}
		ast.LockExpr {
			mut lock_exprs := []ast.Expr{cap: expr.lock_exprs.len}
			for item in expr.lock_exprs {
				lock_exprs << t.qualify_moved_clone_expr_type_positions(item, source_mod)
			}
			mut rlock_exprs := []ast.Expr{cap: expr.rlock_exprs.len}
			for item in expr.rlock_exprs {
				rlock_exprs << t.qualify_moved_clone_expr_type_positions(item, source_mod)
			}
			mut stmts := []ast.Stmt{cap: expr.stmts.len}
			for stmt in expr.stmts {
				stmts << t.qualify_moved_clone_stmt_type_positions(stmt, source_mod)
			}
			return ast.Expr(ast.LockExpr{
				lock_exprs:  lock_exprs
				rlock_exprs: rlock_exprs
				stmts:       stmts
				pos:         expr.pos
			})
		}
		ast.MapInitExpr {
			mut keys := []ast.Expr{cap: expr.keys.len}
			for key in expr.keys {
				keys << t.qualify_moved_clone_expr_type_positions(key, source_mod)
			}
			mut vals := []ast.Expr{cap: expr.vals.len}
			for val in expr.vals {
				vals << t.qualify_moved_clone_expr_type_positions(val, source_mod)
			}
			return ast.Expr(ast.MapInitExpr{
				typ:  t.qualify_moved_clone_type_expr(expr.typ, source_mod)
				keys: keys
				vals: vals
				pos:  expr.pos
			})
		}
		ast.MatchExpr {
			mut branches := []ast.MatchBranch{cap: expr.branches.len}
			for branch in expr.branches {
				mut conds := []ast.Expr{cap: branch.cond.len}
				for cond in branch.cond {
					conds << t.qualify_moved_clone_type_expr(cond, source_mod)
				}
				mut stmts := []ast.Stmt{cap: branch.stmts.len}
				for stmt in branch.stmts {
					stmts << t.qualify_moved_clone_stmt_type_positions(stmt, source_mod)
				}
				branches << ast.MatchBranch{
					cond:  conds
					stmts: stmts
					pos:   branch.pos
				}
			}
			return ast.Expr(ast.MatchExpr{
				expr:     t.qualify_moved_clone_expr_type_positions(expr.expr, source_mod)
				branches: branches
				pos:      expr.pos
			})
		}
		ast.ModifierExpr {
			return ast.Expr(ast.ModifierExpr{
				kind: expr.kind
				expr: t.qualify_moved_clone_expr_type_positions(expr.expr, source_mod)
				pos:  expr.pos
			})
		}
		ast.OrExpr {
			mut stmts := []ast.Stmt{cap: expr.stmts.len}
			for stmt in expr.stmts {
				stmts << t.qualify_moved_clone_stmt_type_positions(stmt, source_mod)
			}
			return ast.Expr(ast.OrExpr{
				expr:  t.qualify_moved_clone_expr_type_positions(expr.expr, source_mod)
				stmts: stmts
				pos:   expr.pos
			})
		}
		ast.ParenExpr {
			return ast.Expr(ast.ParenExpr{
				expr: t.qualify_moved_clone_expr_type_positions(expr.expr, source_mod)
				pos:  expr.pos
			})
		}
		ast.PostfixExpr {
			return ast.Expr(ast.PostfixExpr{
				op:   expr.op
				expr: t.qualify_moved_clone_expr_type_positions(expr.expr, source_mod)
				pos:  expr.pos
			})
		}
		ast.PrefixExpr {
			return ast.Expr(ast.PrefixExpr{
				op:   expr.op
				expr: t.qualify_moved_clone_expr_type_positions(expr.expr, source_mod)
				pos:  expr.pos
			})
		}
		ast.RangeExpr {
			return ast.Expr(ast.RangeExpr{
				op:    expr.op
				start: t.qualify_moved_clone_expr_type_positions(expr.start, source_mod)
				end:   t.qualify_moved_clone_expr_type_positions(expr.end, source_mod)
				pos:   expr.pos
			})
		}
		ast.SelectExpr {
			mut stmts := []ast.Stmt{cap: expr.stmts.len}
			for stmt in expr.stmts {
				stmts << t.qualify_moved_clone_stmt_type_positions(stmt, source_mod)
			}
			return ast.Expr(ast.SelectExpr{
				stmt:  t.qualify_moved_clone_stmt_type_positions(expr.stmt, source_mod)
				stmts: stmts
				next:  t.qualify_moved_clone_expr_type_positions(expr.next, source_mod)
				pos:   expr.pos
			})
		}
		ast.SelectorExpr {
			return ast.Expr(ast.SelectorExpr{
				lhs: t.qualify_moved_clone_expr_type_positions(expr.lhs, source_mod)
				rhs: expr.rhs
				pos: expr.pos
			})
		}
		ast.StringInterLiteral {
			mut inters := []ast.StringInter{cap: expr.inters.len}
			for inter in expr.inters {
				inters << ast.StringInter{
					format:       inter.format
					width:        inter.width
					precision:    inter.precision
					expr:         t.qualify_moved_clone_expr_type_positions(inter.expr, source_mod)
					format_expr:  t.qualify_moved_clone_expr_type_positions(inter.format_expr,
						source_mod)
					resolved_fmt: inter.resolved_fmt
				}
			}
			return ast.Expr(ast.StringInterLiteral{
				kind:   expr.kind
				values: expr.values
				inters: inters
				pos:    expr.pos
			})
		}
		ast.Tuple {
			mut exprs := []ast.Expr{cap: expr.exprs.len}
			for item in expr.exprs {
				exprs << t.qualify_moved_clone_expr_type_positions(item, source_mod)
			}
			return ast.Expr(ast.Tuple{
				exprs: exprs
				pos:   expr.pos
			})
		}
		ast.Type {
			return ast.Expr(t.qualify_moved_clone_type_node(expr, source_mod))
		}
		ast.UnsafeExpr {
			mut stmts := []ast.Stmt{cap: expr.stmts.len}
			for stmt in expr.stmts {
				stmts << t.qualify_moved_clone_stmt_type_positions(stmt, source_mod)
			}
			return ast.Expr(ast.UnsafeExpr{
				stmts: stmts
				pos:   expr.pos
			})
		}
		else {
			return expr
		}
	}
}

fn (t &Transformer) qualify_moved_clone_call_lhs_type_positions(expr ast.Expr, source_mod string) ast.Expr {
	match expr {
		ast.Ident {
			if t.should_qualify_moved_clone_fn_name(expr.name, source_mod) {
				return ast.Expr(ast.Ident{
					name: '${source_mod}__${expr.name}'
					pos:  expr.pos
				})
			}
			return ast.Expr(expr)
		}
		ast.GenericArgOrIndexExpr {
			return ast.Expr(ast.GenericArgOrIndexExpr{
				lhs:  t.qualify_moved_clone_call_lhs_type_positions(expr.lhs, source_mod)
				expr: t.qualify_moved_clone_type_expr(expr.expr, source_mod)
				pos:  expr.pos
			})
		}
		ast.GenericArgs {
			mut args := []ast.Expr{cap: expr.args.len}
			for arg in expr.args {
				args << t.qualify_moved_clone_type_expr(arg, source_mod)
			}
			return ast.Expr(ast.GenericArgs{
				lhs:  t.qualify_moved_clone_call_lhs_type_positions(expr.lhs, source_mod)
				args: args
				pos:  expr.pos
			})
		}
		else {
			return t.qualify_moved_clone_expr_type_positions(expr, source_mod)
		}
	}
}

fn (t &Transformer) monomorphized_clone_name(fn_key string, decl ast.FnDecl, spec_name string, bindings map[string]types.Type) string {
	if decl.is_method {
		short_name := t.specialized_receiver_method_name(decl, bindings) or { spec_name }
		return t.qualify_receiver_generic_method_call_name(fn_key, decl, short_name) or {
			short_name
		}
	}
	base_name := generic_base_name_without_specialization(fn_key)
	if base_name.ends_with('__${decl.name}') {
		return '${base_name.all_before_last('__')}__${spec_name}'
	}
	if base_name.ends_with('.${decl.name}') {
		return '${base_name.all_before_last('.').replace('.', '__')}__${spec_name}'
	}
	return spec_name
}

fn (t &Transformer) monomorphized_clone_ast_name(decl ast.FnDecl, clone_name string, bindings map[string]types.Type) string {
	if decl.is_method {
		return t.specialized_method_name_without_receiver(decl, bindings)
	}
	return clone_name
}

fn (t &Transformer) specialized_receiver_method_name(decl ast.FnDecl, bindings map[string]types.Type) ?string {
	receiver_name := t.specialized_receiver_type_name(decl.receiver.typ, bindings) or {
		return none
	}
	method_name := t.specialized_method_name_without_receiver(decl, bindings)
	if method_name == '' {
		return none
	}
	return '${receiver_name}__${method_name}'
}

fn (t &Transformer) specialized_method_name_without_receiver(decl ast.FnDecl, bindings map[string]types.Type) string {
	method_params := generic_param_names(decl.typ.generic_params)
	if method_params.len == 0 {
		return decl.name
	}
	mut all_placeholders := true
	mut concrete_parts := []string{cap: method_params.len}
	for gp_name in method_params {
		concrete := bindings[gp_name] or {
			concrete_parts << gp_name
			continue
		}
		concrete_parts << t.generic_specialization_token_from_type(concrete)
		if concrete.name() != gp_name {
			all_placeholders = false
		}
	}
	if all_placeholders {
		return decl.name + '_' + method_params.join('_')
	}
	return decl.name + '_T_' + concrete_parts.join('_')
}

fn (t &Transformer) specialized_receiver_type_name(expr ast.Expr, bindings map[string]types.Type) ?string {
	match expr {
		ast.GenericArgs {
			return t.specialized_receiver_type_name_from_parts(expr.lhs, expr.args, bindings)
		}
		ast.GenericArgOrIndexExpr {
			return t.specialized_receiver_type_name_from_parts(expr.lhs, [expr.expr], bindings)
		}
		ast.ModifierExpr {
			return t.specialized_receiver_type_name(expr.expr, bindings)
		}
		ast.PrefixExpr {
			return t.specialized_receiver_type_name(expr.expr, bindings)
		}
		ast.Type {
			match expr {
				ast.GenericType {
					return t.specialized_receiver_type_name_from_parts(expr.name, expr.params,
						bindings)
				}
				ast.PointerType {
					return t.specialized_receiver_type_name(expr.base_type, bindings)
				}
				else {}
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) specialized_receiver_type_name_from_parts(lhs ast.Expr, args []ast.Expr, bindings map[string]types.Type) ?string {
	base_name := t.get_receiver_type_name(lhs)
	if base_name == '' || args.len == 0 {
		return none
	}
	mut parts := []string{cap: args.len}
	for arg in args {
		concrete := t.concrete_type_from_receiver_generic_arg(arg, bindings) or { return none }
		parts << t.generic_specialization_token_from_type(concrete)
	}
	return '${base_name}_T_${parts.join('_')}'
}

fn (t &Transformer) concrete_type_from_receiver_generic_arg(arg ast.Expr, bindings map[string]types.Type) ?types.Type {
	if arg is ast.Ident {
		if concrete := bindings[arg.name] {
			return concrete
		}
	}
	if typ := t.type_from_param_type_expr(arg, []) {
		return substitute_type(typ, bindings)
	}
	if typ := t.get_expr_type(arg) {
		return substitute_type(typ, bindings)
	}
	return none
}

fn generic_base_name_without_specialization(name string) string {
	// Hot path: called per-method per-call-site during generic collection.
	// Hand-rolled byte scans replace string.contains/.index/.all_before, each of
	// which builds a fresh KMP failure-table heap allocation on every call. With
	// millions of calls (and most names having neither `[` nor `_T_`) that
	// allocation churn dominated the whole transform stage. This is behaviour-
	// identical to the previous index_u8(`[`)>0 / contains('_T_') / ends_with('_T')
	// version, just without the per-call allocations.
	mut end := name.len
	for i in 0 .. name.len {
		if name[i] == `[` {
			if i > 0 {
				end = i
			}
			break
		}
	}
	for i := 0; i + 3 <= end; i++ {
		if name[i] == `_` && name[i + 1] == `T` && name[i + 2] == `_` {
			return name[..i]
		}
	}
	if end >= 2 && name[end - 1] == `T` && name[end - 2] == `_` {
		return name[..end - 2]
	}
	if end == name.len {
		return name
	}
	return name[..end]
}

// method_short_name returns the final `__`-separated segment of s (after a
// `.`->`__` normalization). It matches the short form method_key_matches_type_name
// compares, so it can be used to bucket method keys for fast candidate lookup.
fn method_short_name(s string) string {
	norm := if s.index_u8(`.`) >= 0 { s.replace('.', '__') } else { s }
	d := last_double_underscore(norm)
	return if d >= 0 { norm[d + 2..] } else { norm }
}

// last_double_underscore returns the index of the last `__` in s, or -1.
// Hand-rolled (no allocation) replacement for s.contains('__') / .all_after_last('__'),
// which build KMP tables / allocate; used in hot per-call-site name matching.
fn last_double_underscore(s string) int {
	mut i := s.len - 2
	for i >= 0 {
		if s[i] == `_` && s[i + 1] == `_` {
			return i
		}
		i--
	}
	return -1
}

fn (mut t Transformer) generic_fn_decl_monomorphize_keys(decl ast.FnDecl, module_name string) []string {
	mut keys := []string{}
	if !decl.is_method {
		keys << decl.name
		if module_name != '' {
			keys << '${module_name}.${decl.name}'
			keys << '${module_name.replace('.', '__')}__${decl.name}'
			call_prefix := module_call_c_prefix(module_name)
			if call_prefix != '' {
				keys << '${call_prefix}__${decl.name}'
			}
		}
	} else {
		recv_name := t.get_receiver_type_name(decl.receiver.typ)
		if recv_name != '' {
			keys << '${recv_name}__${decl.name}'
			if module_name != '' && !recv_name.contains('__') {
				keys << '${module_name.replace('.', '__')}__${recv_name}__${decl.name}'
				call_prefix := module_call_c_prefix(module_name)
				if call_prefix != '' {
					keys << '${call_prefix}__${recv_name}__${decl.name}'
				}
			}
		}
	}
	return keys
}

fn (mut t Transformer) index_generic_fn_decl_for_monomorphize(mut decl_owner map[string]int, mut decl_node map[string]ast.FnDecl, decl ast.FnDecl, file_idx int, module_name string) {
	keys := t.generic_fn_decl_monomorphize_keys(decl, module_name)
	for key in keys {
		if key == '' {
			continue
		}
		if key in decl_node {
			continue
		}
		decl_owner[key] = file_idx
		decl_node[key] = decl
	}
}

fn (t &Transformer) resolve_monomorphize_decl_key(fn_key string, decl_node map[string]ast.FnDecl) ?string {
	if fn_key in decl_node {
		return fn_key
	}
	mut base_name := fn_key
	bracket_pos := base_name.index_u8(`[`)
	if bracket_pos > 0 {
		base_name = base_name[..bracket_pos]
	}
	if base_name in decl_node {
		return base_name
	}
	if base_name.contains('.') {
		c_name := base_name.replace('.', '__')
		if c_name in decl_node {
			return c_name
		}
	}
	if base_name.contains('__') {
		mut suffix_name := base_name.all_after('__')
		for suffix_name.contains('__') {
			if suffix_name in decl_node {
				return suffix_name
			}
			suffix_name = suffix_name.all_after('__')
		}
	}
	return none
}

fn (t &Transformer) resolve_generic_decl_key_for_call(base_name string, decl_node map[string]ast.FnDecl) ?string {
	if receiver_template := receiver_specialized_method_template_name(base_name) {
		if key := t.resolve_monomorphize_decl_key(receiver_template, decl_node) {
			return key
		}
	}
	mut name := generic_base_name_without_specialization(base_name)
	bracket_pos := name.index_u8(`[`)
	if bracket_pos > 0 {
		name = name[..bracket_pos]
	}
	if name == '' {
		return none
	}
	if !name.contains('__') && !name.contains('.') && t.cur_module != ''
		&& t.cur_module != 'builtin' {
		if t.cur_module != 'main' {
			call_prefix := module_call_c_prefix(t.cur_module)
			if call_prefix != '' {
				candidate := '${call_prefix}__${name}'
				if candidate in decl_node {
					return candidate
				}
			}
			module_prefix := t.cur_module.replace('.', '__')
			if module_prefix != '' {
				candidate := '${module_prefix}__${name}'
				if candidate in decl_node {
					return candidate
				}
			}
			candidate := '${t.cur_module}.${name}'
			if candidate in decl_node {
				return candidate
			}
		}
		if t.has_current_module_concrete_fn(name) {
			return none
		}
	}
	return t.resolve_monomorphize_decl_key(name, decl_node)
}

fn receiver_specialized_method_template_name(name string) ?string {
	if name == '' {
		return none
	}
	mut base := name
	bracket_pos := base.index_u8(`[`)
	if bracket_pos > 0 {
		base = base[..bracket_pos]
	}
	t_idx := base.index('_T_') or { return none }
	method_sep := first_double_underscore_after(base, t_idx + 3)
	if method_sep < 0 || method_sep + 2 >= base.len {
		return none
	}
	receiver_base := base[..t_idx]
	if receiver_base == '' {
		return none
	}
	return receiver_base + base[method_sep..]
}

fn first_double_underscore_after(s string, start int) int {
	mut i := if start < 0 { 0 } else { start }
	for i + 1 < s.len {
		if s[i] == `_` && s[i + 1] == `_` {
			return i
		}
		i++
	}
	return -1
}

fn (t &Transformer) has_current_module_concrete_fn(name string) bool {
	return t.has_concrete_fn_in_module(t.cur_module, name)
		|| (t.cur_module == 'main' && t.has_concrete_fn_in_module('', name))
}

fn (t &Transformer) has_concrete_fn_in_module(module_name string, name string) bool {
	if fn_type := t.lookup_fn_cached(module_name, name) {
		return fn_type.get_generic_params().len == 0
	}
	scope := t.get_module_scope(module_name) or { return false }
	obj := scope.objects[name] or { return false }
	if obj is types.Fn {
		typ := obj.get_typ()
		if typ is types.FnType {
			return typ.get_generic_params().len == 0
		}
	}
	return false
}

fn (mut t Transformer) collect_generic_call_specs(files []ast.File) {
	old_module := t.cur_module
	old_file := t.cur_file_name
	old_scope := t.scope
	old_file_idx := t.cur_generic_call_file_idx
	old_import_aliases := t.cur_import_aliases.clone()
	t.build_generic_fn_decl_index(files)
	for fi, file in files {
		t.cur_file_name = file.name
		t.cur_module = file.mod
		t.cur_generic_call_file_idx = fi
		t.cur_import_aliases = import_aliases_for_generic_collect(file.imports)
		if scope := t.get_module_scope(file.mod) {
			t.scope = scope
		} else {
			t.scope = unsafe { nil }
		}
		for stmt in file.stmts {
			t.collect_generic_call_specs_in_stmt(stmt)
		}
	}
	t.cur_module = old_module
	t.cur_file_name = old_file
	t.scope = old_scope
	t.cur_generic_call_file_idx = old_file_idx
	t.cur_import_aliases = old_import_aliases.clone()
}

fn (mut t Transformer) collect_generic_call_specs_from_flat(flat &ast.FlatAst, extra_stmts map[int][]ast.Stmt) {
	old_module := t.cur_module
	old_file := t.cur_file_name
	old_scope := t.scope
	old_file_idx := t.cur_generic_call_file_idx
	old_import_aliases := t.cur_import_aliases.clone()
	t.build_generic_fn_decl_index_from_flat(flat, extra_stmts)
	for fi in 0 .. flat.files.len {
		module_name := flat.file_mod(flat.files[fi])
		t.cur_file_name = flat.file_name(flat.files[fi])
		t.cur_module = module_name
		t.cur_generic_call_file_idx = fi
		t.cur_import_aliases = flat_import_aliases_for_generic_collect(flat, fi)
		if scope := t.get_module_scope(module_name) {
			t.scope = scope
		} else {
			t.scope = unsafe { nil }
		}
		stmts := flat.file_cursor(fi).stmts()
		for si in 0 .. stmts.len() {
			t.collect_generic_call_specs_in_stmt_cursor(stmts.at(si))
		}
		extra := extra_stmts[fi] or { []ast.Stmt{} }
		for stmt in extra {
			t.collect_generic_call_specs_in_stmt(stmt)
		}
	}
	t.cur_module = old_module
	t.cur_file_name = old_file
	t.scope = old_scope
	t.cur_generic_call_file_idx = old_file_idx
	t.cur_import_aliases = old_import_aliases.clone()
}

// build_generic_fn_decl_index (re)builds t.generic_fn_decl_index from every
// generic FnDecl across all files. Generic declarations never change during the
// fixpoint (monomorphize_pass only appends concrete clones), but the index maps
// keys to file indices, so it is rebuilt against the current file set.
fn (mut t Transformer) build_generic_fn_decl_index(files []ast.File) {
	t.generic_fn_decl_index = map[string]ast.FnDecl{}
	t.generic_call_candidate_names = map[string]bool{}
	mut dummy_owner := map[string]int{}
	for fi, file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				if decl_generic_param_names(stmt).len == 0 {
					continue
				}
				t.index_generic_fn_decl_for_monomorphize(mut dummy_owner, mut
					t.generic_fn_decl_index, stmt, fi, file.mod)
			}
		}
	}
	for key, _ in t.generic_fn_decl_index {
		t.register_generic_call_candidate_name(key)
	}
	for name, _ in t.generic_fn_value_names {
		t.register_generic_call_candidate_name(name)
	}
}

fn (mut t Transformer) build_generic_fn_decl_index_from_flat(flat &ast.FlatAst, extra_stmts map[int][]ast.Stmt) {
	t.generic_fn_decl_index = map[string]ast.FnDecl{}
	t.generic_call_candidate_names = map[string]bool{}
	mut dummy_owner := map[string]int{}
	for fi in 0 .. flat.files.len {
		module_name := flat.file_mod(flat.files[fi])
		stmts := flat.file_cursor(fi).stmts()
		for si in 0 .. stmts.len() {
			stmt_c := stmts.at(si)
			if stmt_c.kind() != .stmt_fn_decl {
				continue
			}
			stmt := stmt_c.fn_decl_signature()
			if decl_generic_param_names(stmt).len == 0 {
				continue
			}
			t.index_generic_fn_decl_for_monomorphize(mut dummy_owner, mut t.generic_fn_decl_index,
				stmt, fi, module_name)
		}
		extra := extra_stmts[fi] or { []ast.Stmt{} }
		for stmt in extra {
			if stmt !is ast.FnDecl {
				continue
			}
			decl := stmt as ast.FnDecl
			if decl_generic_param_names(decl).len == 0 {
				continue
			}
			t.index_generic_fn_decl_for_monomorphize(mut dummy_owner, mut t.generic_fn_decl_index,
				decl, fi, module_name)
		}
	}
	for key, _ in t.generic_fn_decl_index {
		t.register_generic_call_candidate_name(key)
	}
	for name, _ in t.generic_fn_value_names {
		t.register_generic_call_candidate_name(name)
	}
}

fn (mut t Transformer) register_generic_call_candidate_name(name string) {
	base := generic_base_name_without_specialization(name)
	if base == '' {
		return
	}
	t.generic_call_candidate_names[base] = true
	dot := base.last_index_u8(`.`)
	if dot >= 0 {
		if dot + 1 < base.len {
			t.generic_call_candidate_names[base[dot + 1..]] = true
		}
	}
	dunder := last_double_underscore(base)
	if dunder >= 0 {
		if dunder + 2 < base.len {
			t.generic_call_candidate_names[base[dunder + 2..]] = true
		}
	}
}

fn (t &Transformer) may_target_generic_call_name(name string) bool {
	if name == '' {
		return false
	}
	base := generic_base_name_without_specialization(name)
	return base in t.generic_call_candidate_names
}

// collect_generic_call_specs_in_new_clones is the incremental counterpart of
// collect_generic_call_specs. After monomorphize_pass appends concrete clones,
// only those clones can introduce generic calls the previous scan did not see —
// every other statement is byte-for-byte unchanged and was already walked. So
// this rebuilds the generic-decl index (cheap, shallow) but deep-walks only the
// clones recorded in t.last_mono_clones, each under its owning file's module,
// scope and import context. For v2 self-host this replaces a full ~6s/~700MB
// rescan with a walk of a few dozen functions.
fn (mut t Transformer) collect_generic_call_specs_in_new_clones(files []ast.File) {
	if t.last_mono_clones.len == 0 {
		return
	}
	old_module := t.cur_module
	old_file := t.cur_file_name
	old_scope := t.scope
	old_file_idx := t.cur_generic_call_file_idx
	old_import_aliases := t.cur_import_aliases.clone()
	t.build_generic_fn_decl_index(files)
	for fi, file in files {
		if fi !in t.last_mono_clones {
			continue
		}
		clones := t.last_mono_clones[fi]
		if clones.len == 0 {
			continue
		}
		t.cur_file_name = file.name
		t.cur_module = file.mod
		t.cur_generic_call_file_idx = fi
		t.cur_import_aliases = import_aliases_for_generic_collect(file.imports)
		if scope := t.get_module_scope(file.mod) {
			t.scope = scope
		} else {
			t.scope = unsafe { nil }
		}
		for stmt in clones {
			t.collect_generic_call_specs_in_stmt(stmt)
		}
	}
	t.cur_module = old_module
	t.cur_file_name = old_file
	t.scope = old_scope
	t.cur_generic_call_file_idx = old_file_idx
	t.cur_import_aliases = old_import_aliases.clone()
}

fn (mut t Transformer) collect_generic_call_specs_in_new_clones_from_flat(flat &ast.FlatAst, extra_stmts map[int][]ast.Stmt) {
	if t.last_mono_clones.len == 0 {
		return
	}
	old_module := t.cur_module
	old_file := t.cur_file_name
	old_scope := t.scope
	old_file_idx := t.cur_generic_call_file_idx
	old_import_aliases := t.cur_import_aliases.clone()
	t.build_generic_fn_decl_index_from_flat(flat, extra_stmts)
	for fi in 0 .. flat.files.len {
		clones := t.last_mono_clones[fi] or { continue }
		if clones.len == 0 {
			continue
		}
		module_name := flat.file_mod(flat.files[fi])
		t.cur_file_name = flat.file_name(flat.files[fi])
		t.cur_module = module_name
		t.cur_generic_call_file_idx = fi
		t.cur_import_aliases = flat_import_aliases_for_generic_collect(flat, fi)
		if scope := t.get_module_scope(module_name) {
			t.scope = scope
		} else {
			t.scope = unsafe { nil }
		}
		for stmt in clones {
			t.collect_generic_call_specs_in_stmt(stmt)
		}
	}
	t.cur_module = old_module
	t.cur_file_name = old_file
	t.scope = old_scope
	t.cur_generic_call_file_idx = old_file_idx
	t.cur_import_aliases = old_import_aliases.clone()
}

fn (mut t Transformer) collect_generic_call_specs_in_new_structs(files []ast.File) {
	if t.last_struct_clones.len == 0 {
		return
	}
	old_module := t.cur_module
	old_file := t.cur_file_name
	old_scope := t.scope
	old_file_idx := t.cur_generic_call_file_idx
	old_import_aliases := t.cur_import_aliases.clone()
	t.build_generic_fn_decl_index(files)
	for fi, file in files {
		clones := t.last_struct_clones[fi] or { continue }
		if clones.len == 0 {
			continue
		}
		t.cur_file_name = file.name
		t.cur_module = file.mod
		t.cur_generic_call_file_idx = fi
		t.cur_import_aliases = import_aliases_for_generic_collect(file.imports)
		if scope := t.get_module_scope(file.mod) {
			t.scope = scope
		} else {
			t.scope = unsafe { nil }
		}
		for stmt in clones {
			t.collect_generic_call_specs_in_stmt(stmt)
		}
	}
	t.cur_module = old_module
	t.cur_file_name = old_file
	t.scope = old_scope
	t.cur_generic_call_file_idx = old_file_idx
	t.cur_import_aliases = old_import_aliases.clone()
}

fn (mut t Transformer) collect_generic_call_specs_in_new_structs_from_flat(flat &ast.FlatAst, extra_stmts map[int][]ast.Stmt) {
	if t.last_struct_clones.len == 0 {
		return
	}
	old_module := t.cur_module
	old_file := t.cur_file_name
	old_scope := t.scope
	old_file_idx := t.cur_generic_call_file_idx
	old_import_aliases := t.cur_import_aliases.clone()
	t.build_generic_fn_decl_index_from_flat(flat, extra_stmts)
	for fi in 0 .. flat.files.len {
		clones := t.last_struct_clones[fi] or { continue }
		if clones.len == 0 {
			continue
		}
		module_name := flat.file_mod(flat.files[fi])
		t.cur_file_name = flat.file_name(flat.files[fi])
		t.cur_module = module_name
		t.cur_generic_call_file_idx = fi
		t.cur_import_aliases = flat_import_aliases_for_generic_collect(flat, fi)
		if scope := t.get_module_scope(module_name) {
			t.scope = scope
		} else {
			t.scope = unsafe { nil }
		}
		for stmt in clones {
			t.collect_generic_call_specs_in_stmt(stmt)
		}
	}
	t.cur_module = old_module
	t.cur_file_name = old_file
	t.scope = old_scope
	t.cur_generic_call_file_idx = old_file_idx
	t.cur_import_aliases = old_import_aliases.clone()
}

fn import_aliases_for_generic_collect(imports []ast.ImportStmt) map[string]string {
	mut aliases := map[string]string{}
	for imp in imports {
		alias := if imp.alias != '' { imp.alias } else { imp.name.all_after_last('.') }
		if alias == '' || imp.name == '' {
			continue
		}
		aliases[alias] = imp.name
	}
	return aliases
}

fn flat_import_aliases_for_generic_collect(flat &ast.FlatAst, fi int) map[string]string {
	if fi < 0 || fi >= flat.files.len {
		return map[string]string{}
	}
	imports := flat.file_cursor(fi).imports()
	mut aliases := map[string]string{}
	for i in 0 .. imports.len() {
		imp := imports.at(i)
		if imp.kind() != .stmt_import {
			continue
		}
		name := imp.name()
		alias := if imp.extra_str() != '' { imp.extra_str() } else { name.all_after_last('.') }
		if alias == '' || name == '' {
			continue
		}
		aliases[alias] = name
	}
	return aliases
}

fn flat_file_struct_decls_with_extra(flat &ast.FlatAst, extra_stmts map[int][]ast.Stmt, fi int) []ast.StructDecl {
	if fi < 0 || fi >= flat.files.len {
		return []ast.StructDecl{}
	}
	stmt_cursors := flat.file_cursor(fi).stmts()
	extra := extra_stmts[fi] or { []ast.Stmt{} }
	mut decls := []ast.StructDecl{cap: stmt_cursors.len() + extra.len}
	for i in 0 .. stmt_cursors.len() {
		stmt_c := stmt_cursors.at(i)
		if stmt_c.kind() == .stmt_struct_decl {
			decls << stmt_c.struct_decl()
		}
	}
	for stmt in extra {
		if stmt is ast.StructDecl {
			decls << stmt
		}
	}
	return decls
}

fn append_flat_extra_stmts(mut extra_stmts map[int][]ast.Stmt, fi int, stmts []ast.Stmt) {
	if stmts.len == 0 {
		return
	}
	mut bucket := extra_stmts[fi] or { []ast.Stmt{} }
	bucket << stmts
	extra_stmts[fi] = bucket
}

fn (mut t Transformer) collect_generic_call_specs_in_stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		t.collect_generic_call_specs_in_stmt(stmt)
	}
}

fn (mut t Transformer) collect_generic_call_specs_in_cursor_list(stmts ast.CursorList) {
	for i in 0 .. stmts.len() {
		t.collect_generic_call_specs_in_stmt_cursor(stmts.at(i))
	}
}

fn (mut t Transformer) collect_generic_call_specs_in_stmt_cursor(stmt ast.Cursor) {
	if !stmt.is_valid() {
		return
	}
	match stmt.kind() {
		.stmt_assert {
			t.collect_generic_call_specs_in_expr_cursor(stmt.edge(0))
			t.collect_generic_call_specs_in_expr_cursor(stmt.edge(1))
		}
		.stmt_assign {
			lhs_len := stmt.extra_int()
			for i in 0 .. lhs_len {
				t.collect_generic_call_specs_in_expr_cursor(stmt.edge(i))
			}
			for i in lhs_len .. stmt.edge_count() {
				t.collect_generic_call_specs_in_expr_cursor(stmt.edge(i))
			}
			t.collect_generic_scan_decl_assign_types_cursor(stmt)
		}
		.stmt_block {
			for i in 0 .. stmt.edge_count() {
				t.collect_generic_call_specs_in_stmt_cursor(stmt.edge(i))
			}
		}
		.stmt_comptime {
			t.collect_generic_call_specs_in_stmt_cursor(stmt.edge(0))
		}
		.stmt_const_decl {
			fields := stmt.list_at(0)
			for i in 0 .. fields.len() {
				t.collect_generic_call_specs_in_expr_cursor(fields.at(i).edge(0))
			}
		}
		.stmt_defer {
			for i in 0 .. stmt.edge_count() {
				t.collect_generic_call_specs_in_stmt_cursor(stmt.edge(i))
			}
		}
		.stmt_enum_decl {
			fields := stmt.list_at(2)
			for i in 0 .. fields.len() {
				t.collect_generic_call_specs_in_expr_cursor(fields.at(i).edge(1))
			}
		}
		.stmt_expr {
			t.collect_generic_call_specs_in_expr_cursor(stmt.edge(0))
		}
		.stmt_for_in {
			t.collect_generic_call_specs_in_expr_cursor(stmt.edge(0))
			t.collect_generic_call_specs_in_expr_cursor(stmt.edge(1))
			t.collect_generic_call_specs_in_expr_cursor(stmt.edge(2))
		}
		.stmt_for {
			init := stmt.edge(0)
			if init.kind() == .stmt_for_in {
				t.collect_generic_call_specs_in_for_stmt_cursor(stmt, init)
				return
			}
			t.collect_generic_call_specs_in_stmt_cursor(init)
			t.collect_generic_call_specs_in_expr_cursor(stmt.edge(1))
			t.collect_generic_call_specs_in_stmt_cursor(stmt.edge(2))
			for i in 3 .. stmt.edge_count() {
				t.collect_generic_call_specs_in_stmt_cursor(stmt.edge(i))
			}
		}
		.stmt_fn_decl {
			signature := stmt.fn_decl_signature()
			if decl_generic_param_names(signature).len == 0 {
				t.collect_generic_call_specs_in_fn_decl_cursor(stmt, signature)
			}
		}
		.stmt_global_decl {
			fields := stmt.list_at(1)
			for i in 0 .. fields.len() {
				t.collect_generic_call_specs_in_expr_cursor(fields.at(i).edge(1))
			}
		}
		.stmt_import, .stmt_module {}
		.stmt_return {
			for i in 0 .. stmt.edge_count() {
				t.collect_generic_call_specs_in_expr_cursor(stmt.edge(i))
			}
		}
		.stmt_struct_decl {
			fields := stmt.list_at(4)
			for i in 0 .. fields.len() {
				field := fields.at(i)
				t.collect_generic_call_specs_in_expr_cursor(field.edge(0))
				t.collect_generic_call_specs_in_expr_cursor(field.edge(1))
			}
			embedded := stmt.list_at(2)
			for i in 0 .. embedded.len() {
				t.collect_generic_call_specs_in_expr_cursor(embedded.at(i))
			}
			implemented := stmt.list_at(1)
			for i in 0 .. implemented.len() {
				t.collect_generic_call_specs_in_expr_cursor(implemented.at(i))
			}
		}
		else {}
	}
}

fn (mut t Transformer) collect_generic_call_specs_in_stmt(stmt ast.Stmt) {
	match stmt {
		ast.AssertStmt {
			t.collect_generic_call_specs_in_expr(stmt.expr)
			t.collect_generic_call_specs_in_expr(stmt.extra)
		}
		ast.AssignStmt {
			for expr in stmt.lhs {
				t.collect_generic_call_specs_in_expr(expr)
			}
			for expr in stmt.rhs {
				t.collect_generic_call_specs_in_expr(expr)
			}
			t.collect_generic_scan_decl_assign_types(stmt)
		}
		ast.BlockStmt {
			t.collect_generic_call_specs_in_stmts(stmt.stmts)
		}
		ast.ComptimeStmt {
			t.collect_generic_call_specs_in_stmt(stmt.stmt)
		}
		ast.ConstDecl {
			for field in stmt.fields {
				t.collect_generic_call_specs_in_expr(field.value)
			}
		}
		ast.DeferStmt {
			t.collect_generic_call_specs_in_stmts(stmt.stmts)
		}
		ast.EnumDecl {
			for field in stmt.fields {
				t.collect_generic_call_specs_in_expr(field.value)
			}
		}
		ast.ExprStmt {
			t.collect_generic_call_specs_in_expr(stmt.expr)
		}
		ast.ForInStmt {
			t.collect_generic_call_specs_in_expr(stmt.key)
			t.collect_generic_call_specs_in_expr(stmt.value)
			t.collect_generic_call_specs_in_expr(stmt.expr)
		}
		ast.ForStmt {
			if stmt.init is ast.ForInStmt {
				t.collect_generic_call_specs_in_for_stmt(stmt, stmt.init as ast.ForInStmt)
				return
			}
			t.collect_generic_call_specs_in_stmt(stmt.init)
			t.collect_generic_call_specs_in_expr(stmt.cond)
			t.collect_generic_call_specs_in_stmt(stmt.post)
			t.collect_generic_call_specs_in_stmts(stmt.stmts)
		}
		ast.FnDecl {
			if decl_generic_param_names(stmt).len == 0 {
				t.collect_generic_call_specs_in_fn_decl(stmt)
			}
		}
		ast.GlobalDecl {
			for field in stmt.fields {
				t.collect_generic_call_specs_in_expr(field.value)
			}
		}
		ast.ImportStmt {}
		ast.ModuleStmt {}
		ast.ReturnStmt {
			for expr in stmt.exprs {
				t.collect_generic_call_specs_in_expr(expr)
			}
		}
		ast.StructDecl {
			for field in stmt.fields {
				t.collect_generic_call_specs_in_expr(field.typ)
				t.collect_generic_call_specs_in_expr(field.value)
			}
			for embedded in stmt.embedded {
				t.collect_generic_call_specs_in_expr(embedded)
			}
			for implemented in stmt.implements {
				t.collect_generic_call_specs_in_expr(implemented)
			}
		}
		else {}
	}
}

fn (mut t Transformer) collect_generic_call_specs_in_for_stmt_cursor(stmt ast.Cursor, for_in ast.Cursor) {
	t.collect_generic_call_specs_in_expr_cursor(for_in.edge(0))
	t.collect_generic_call_specs_in_expr_cursor(for_in.edge(1))
	t.collect_generic_call_specs_in_expr_cursor(for_in.edge(2))
	t.collect_generic_call_specs_in_expr_cursor(stmt.edge(1))
	t.collect_generic_call_specs_in_stmt_cursor(stmt.edge(2))
	old_local_decl_types := t.local_decl_types.clone()
	t.seed_generic_scan_for_in_var_types_cursor(for_in)
	for i in 3 .. stmt.edge_count() {
		t.collect_generic_call_specs_in_stmt_cursor(stmt.edge(i))
	}
	t.local_decl_types = old_local_decl_types.clone()
}

fn (mut t Transformer) collect_generic_call_specs_in_for_stmt(stmt ast.ForStmt, for_in ast.ForInStmt) {
	t.collect_generic_call_specs_in_expr(for_in.key)
	t.collect_generic_call_specs_in_expr(for_in.value)
	t.collect_generic_call_specs_in_expr(for_in.expr)
	t.collect_generic_call_specs_in_expr(stmt.cond)
	t.collect_generic_call_specs_in_stmt(stmt.post)
	old_local_decl_types := t.local_decl_types.clone()
	t.seed_generic_scan_for_in_var_types(for_in)
	t.collect_generic_call_specs_in_stmts(stmt.stmts)
	t.local_decl_types = old_local_decl_types.clone()
}

fn (mut t Transformer) seed_generic_scan_for_in_var_types_cursor(for_in ast.Cursor) {
	iter_type := t.for_in_iter_expr_type_cursor(for_in.edge(2)) or { return }
	value_name := t.get_var_name_cursor(for_in.edge(1))
	if value_name != '' && value_name != '_' {
		t.remember_local_decl_type(value_name, t.for_in_value_type(iter_type))
	}
	key_name := t.get_var_name_cursor(for_in.edge(0))
	if key_name != '' && key_name != '_' {
		t.remember_local_decl_type(key_name, t.for_in_key_type_for_generic_scan(iter_type))
	}
}

fn (t &Transformer) for_in_iter_expr_type_cursor(expr ast.Cursor) ?types.Type {
	mut base_expr := expr
	for _ in 0 .. 8 {
		if base_expr.is_valid() && base_expr.kind() in [.expr_modifier, .expr_paren] {
			base_expr = base_expr.edge(0)
			continue
		}
		break
	}
	if base_expr.is_valid() && base_expr.kind() == .expr_ident {
		if typ := t.lookup_var_type(base_expr.name()) {
			return typ
		}
	}
	return t.get_expr_type_cursor(expr)
}

fn (t &Transformer) get_var_name_cursor(expr ast.Cursor) string {
	if !expr.is_valid() {
		return ''
	}
	if expr.kind() == .expr_ident {
		return expr.name()
	}
	if expr.kind() == .expr_modifier {
		return t.get_var_name_cursor(expr.edge(0))
	}
	return ''
}

fn (mut t Transformer) seed_generic_scan_for_in_var_types(for_in ast.ForInStmt) {
	iter_type := t.for_in_iter_expr_type(for_in.expr) or { return }
	value_name := t.get_var_name(for_in.value)
	if value_name != '' && value_name != '_' {
		t.remember_local_decl_type(value_name, t.for_in_value_type(iter_type))
	}
	key_name := t.get_var_name(for_in.key)
	if key_name != '' && key_name != '_' {
		t.remember_local_decl_type(key_name, t.for_in_key_type_for_generic_scan(iter_type))
	}
}

fn (t &Transformer) for_in_key_type_for_generic_scan(iter_type types.Type) types.Type {
	base := t.unwrap_alias_and_pointer_type(iter_type)
	if base is types.Map {
		return base.key_type
	}
	return types.Type(types.int_)
}

fn (mut t Transformer) collect_generic_call_specs_in_fn_decl_cursor(decl_c ast.Cursor, decl ast.FnDecl) {
	old_module := t.cur_module
	old_scope := t.scope
	old_fn_root_scope := t.fn_root_scope
	mut old_local_decl_types := t.local_decl_types.move()
	mut old_local_receiver_generic_bindings := t.local_receiver_generic_bindings.move()
	old_cur_fn_name := t.cur_fn_name_str
	old_recv_prefix := t.cur_fn_recv_prefix
	old_recv_param := t.cur_fn_recv_param
	old_recv_is_ptr := t.cur_fn_recv_is_ptr
	old_generic_params := t.cur_fn_generic_params.clone()
	mut old_monomorphized_bindings := t.cur_monomorphized_fn_bindings.move()
	t.local_decl_types = map[string]types.Type{}
	t.local_receiver_generic_bindings = map[string]map[string]types.Type{}
	t.cur_fn_name_str = decl.name
	t.cur_fn_recv_prefix = ''
	t.cur_fn_recv_param = ''
	t.cur_fn_recv_is_ptr = false
	t.cur_fn_generic_params = []string{}
	t.cur_module = t.collect_module_for_monomorphized_fn_decl(decl, old_module)
	mut recv_name := if decl.is_method { t.get_receiver_type_name(decl.receiver.typ) } else { '' }
	if t.cur_module != '' {
		prefix := '${t.cur_module}__'
		if recv_name.starts_with(prefix) {
			recv_name = recv_name[prefix.len..]
		}
	}
	scope_fn_name := if decl.is_method { '${recv_name}__${decl.name}' } else { decl.name }
	t.cur_monomorphized_fn_bindings = t.lookup_monomorphized_fn_bindings(t.cur_module,
		scope_fn_name) or {
		t.lookup_monomorphized_fn_bindings(t.cur_module, decl.name) or {
			map[string]types.Type{}
		}
	}
	fn_scope_key := if t.cur_module == '' {
		scope_fn_name
	} else {
		'${t.cur_module}__${scope_fn_name}'
	}
	if fn_scope := t.cached_fn_scopes[fn_scope_key] {
		t.scope = types.new_scope(fn_scope)
		t.fn_root_scope = t.scope
	} else {
		t.open_scope()
		t.fn_root_scope = t.scope
	}
	if decl.is_method && decl.receiver.name != '' && decl.receiver.name != '_' {
		if typ := t.type_from_param_type_expr(decl.receiver.typ, []) {
			t.remember_local_decl_type(decl.receiver.name, typ)
			t.register_local_var_type(decl.receiver.name, typ)
		}
	}
	if decl.is_method {
		t.collect_generic_call_specs_in_expr_cursor(decl_c.edge(0).edge(0))
	}
	fn_typ := decl_c.edge(1)
	params := fn_typ.list_at(1)
	for i in 0 .. params.len() {
		t.collect_generic_call_specs_in_expr_cursor(params.at(i).edge(0))
	}
	t.collect_generic_call_specs_in_expr_cursor(fn_typ.edge(2))
	t.seed_fn_param_decl_types(decl.typ.params, [])
	t.seed_fn_pointer_param_return_types(decl.typ.params, [])
	t.seed_scope_with_fn_params(decl)
	t.collect_generic_call_specs_in_cursor_list(decl_c.list_at(3))
	t.cur_module = old_module
	t.scope = old_scope
	t.fn_root_scope = old_fn_root_scope
	t.local_decl_types = old_local_decl_types.move()
	t.local_receiver_generic_bindings = old_local_receiver_generic_bindings.move()
	t.cur_fn_name_str = old_cur_fn_name
	t.cur_fn_recv_prefix = old_recv_prefix
	t.cur_fn_recv_param = old_recv_param
	t.cur_fn_recv_is_ptr = old_recv_is_ptr
	t.cur_fn_generic_params = old_generic_params.clone()
	t.cur_monomorphized_fn_bindings = old_monomorphized_bindings.move()
}

fn (mut t Transformer) collect_generic_call_specs_in_fn_decl(decl ast.FnDecl) {
	old_module := t.cur_module
	old_scope := t.scope
	old_fn_root_scope := t.fn_root_scope
	mut old_local_decl_types := t.local_decl_types.move()
	mut old_local_receiver_generic_bindings := t.local_receiver_generic_bindings.move()
	old_cur_fn_name := t.cur_fn_name_str
	old_recv_prefix := t.cur_fn_recv_prefix
	old_recv_param := t.cur_fn_recv_param
	old_recv_is_ptr := t.cur_fn_recv_is_ptr
	old_generic_params := t.cur_fn_generic_params.clone()
	mut old_monomorphized_bindings := t.cur_monomorphized_fn_bindings.move()
	t.local_decl_types = map[string]types.Type{}
	t.local_receiver_generic_bindings = map[string]map[string]types.Type{}
	t.cur_fn_name_str = decl.name
	t.cur_fn_recv_prefix = ''
	t.cur_fn_recv_param = ''
	t.cur_fn_recv_is_ptr = false
	t.cur_fn_generic_params = []string{}
	t.cur_module = t.collect_module_for_monomorphized_fn_decl(decl, old_module)
	mut recv_name := if decl.is_method { t.get_receiver_type_name(decl.receiver.typ) } else { '' }
	if t.cur_module != '' {
		prefix := '${t.cur_module}__'
		if recv_name.starts_with(prefix) {
			recv_name = recv_name[prefix.len..]
		}
	}
	scope_fn_name := if decl.is_method { '${recv_name}__${decl.name}' } else { decl.name }
	t.cur_monomorphized_fn_bindings = t.lookup_monomorphized_fn_bindings(t.cur_module,
		scope_fn_name) or {
		t.lookup_monomorphized_fn_bindings(t.cur_module, decl.name) or {
			map[string]types.Type{}
		}
	}
	fn_scope_key := if t.cur_module == '' {
		scope_fn_name
	} else {
		'${t.cur_module}__${scope_fn_name}'
	}
	if fn_scope := t.cached_fn_scopes[fn_scope_key] {
		t.scope = types.new_scope(fn_scope)
		t.fn_root_scope = t.scope
	} else {
		t.open_scope()
		t.fn_root_scope = t.scope
	}
	if decl.is_method && decl.receiver.name != '' && decl.receiver.name != '_' {
		if typ := t.type_from_param_type_expr(decl.receiver.typ, []) {
			t.remember_local_decl_type(decl.receiver.name, typ)
			t.register_local_var_type(decl.receiver.name, typ)
		}
	}
	if decl.is_method {
		t.collect_generic_call_specs_in_expr(decl.receiver.typ)
	}
	for param in decl.typ.params {
		t.collect_generic_call_specs_in_expr(param.typ)
	}
	t.collect_generic_call_specs_in_expr(decl.typ.return_type)
	t.seed_fn_param_decl_types(decl.typ.params, [])
	t.seed_fn_pointer_param_return_types(decl.typ.params, [])
	t.seed_scope_with_fn_params(decl)
	t.collect_generic_call_specs_in_stmts(decl.stmts)
	t.cur_module = old_module
	t.scope = old_scope
	t.fn_root_scope = old_fn_root_scope
	t.local_decl_types = old_local_decl_types.move()
	t.local_receiver_generic_bindings = old_local_receiver_generic_bindings.move()
	t.cur_fn_name_str = old_cur_fn_name
	t.cur_fn_recv_prefix = old_recv_prefix
	t.cur_fn_recv_param = old_recv_param
	t.cur_fn_recv_is_ptr = old_recv_is_ptr
	t.cur_fn_generic_params = old_generic_params.clone()
	t.cur_monomorphized_fn_bindings = old_monomorphized_bindings.move()
}

fn (t &Transformer) collect_module_for_monomorphized_fn_decl(decl ast.FnDecl, fallback string) string {
	if decl.name.contains('_T_') {
		base_name := decl.name.all_before('_T_')
		if base_name.contains('__') {
			module_name := base_name.all_before('__')
			if module_name != '' && t.get_module_scope(module_name) != none {
				return module_name
			}
		}
	}
	return fallback
}

fn (mut t Transformer) collect_generic_scan_decl_assign_types_cursor(stmt ast.Cursor) {
	if stmt.kind() != .stmt_assign || unsafe { token.Token(int(stmt.aux())) } != .decl_assign
		|| stmt.extra_int() != 1 || stmt.edge_count() != 2 {
		return
	}
	lhs := stmt.edge(0)
	rhs := stmt.edge(1)
	lhs_name := t.get_var_name_cursor(lhs)
	if lhs_name == '' {
		return
	}
	if decl_type := t.decl_assign_storage_type_cursor(lhs, rhs) {
		t.remember_local_decl_type(lhs_name, decl_type)
		t.register_local_var_type(lhs_name, decl_type)
	}
	if rhs_type := t.fn_pointer_call_return_type_cursor(rhs) {
		t.register_temp_var(lhs_name, rhs_type)
	} else if rhs_type := t.smartcast_type_for_expr_cursor(rhs) {
		t.register_local_var_type(lhs_name, rhs_type)
	} else if rhs_type := t.rune_arithmetic_expr_type_cursor(rhs) {
		t.register_local_var_type(lhs_name, rhs_type)
	} else if rhs.kind() == .expr_array_init {
		if rhs_type := t.get_array_init_expr_type_cursor(rhs) {
			t.register_local_var_type(lhs_name, rhs_type)
		}
	} else if rhs.kind() in [.expr_call, .expr_call_or_cast, .expr_init, .expr_ident, .expr_selector] {
		if rhs_type := t.get_expr_type_cursor(rhs) {
			t.register_local_var_type(lhs_name, rhs_type)
		}
	}
	if bindings := t.generic_bindings_from_generic_call_expr_cursor(rhs) {
		t.local_receiver_generic_bindings[lhs_name] = bindings.clone()
	} else if bindings := t.generic_bindings_from_generic_init_expr_cursor(rhs) {
		t.local_receiver_generic_bindings[lhs_name] = bindings.clone()
	}
}

fn (t &Transformer) decl_assign_storage_type_cursor(lhs ast.Cursor, rhs ast.Cursor) ?types.Type {
	if typ := t.fn_pointer_call_return_type_cursor(rhs) {
		return typ
	}
	if typ := t.smartcast_type_for_expr_cursor(rhs) {
		return typ
	}
	if rhs.kind() == .expr_array_init {
		if typ := t.get_array_init_expr_type_cursor(rhs) {
			return typ
		}
	}
	if typ := t.rune_arithmetic_expr_type_cursor(rhs) {
		return typ
	}
	if rhs.kind() in [.expr_index, .expr_unsafe, .expr_if, .expr_call, .expr_call_or_cast, .expr_cast,
		.expr_as_cast, .expr_init, .expr_ident, .expr_selector] {
		if typ := t.get_expr_type_cursor(rhs) {
			return typ
		}
	}
	if rhs.kind() == .expr_init {
		if typ := t.type_from_init_expr_cursor(rhs) {
			return typ
		}
	}
	if typ := t.get_expr_type_cursor(lhs) {
		return typ
	}
	if lhs.kind() == .expr_modifier {
		if typ := t.get_expr_type_cursor(lhs.edge(0)) {
			return typ
		}
	}
	return none
}

fn (t &Transformer) fn_pointer_call_return_type_cursor(expr ast.Cursor) ?types.Type {
	if expr.kind() !in [.expr_call, .expr_call_or_cast] {
		return none
	}
	lhs := expr.edge(0)
	if lhs.kind() == .expr_ident {
		name := lhs.name()
		if name in t.local_fn_pointer_return_types {
			ret := t.local_fn_pointer_return_types[name] or { return none }
			return ret
		}
		fn_type := t.lookup_fn_pointer_var_type(name) or { return none }
		if ret := fn_type.get_return_type() {
			return ret
		}
	}
	return none
}

fn (t &Transformer) rune_arithmetic_expr_type_cursor(expr ast.Cursor) ?types.Type {
	match expr.kind() {
		.expr_infix {
			op := unsafe { token.Token(int(expr.aux())) }
			if op !in [.plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor, .left_shift, .right_shift,
				.right_shift_unsigned] {
				return none
			}
			if t.expr_is_rune_arithmetic_operand_cursor(expr.edge(0))
				|| t.expr_is_rune_arithmetic_operand_cursor(expr.edge(1)) {
				return types.builtin_type('rune')
			}
		}
		.expr_paren, .expr_modifier {
			return t.rune_arithmetic_expr_type_cursor(expr.edge(0))
		}
		else {}
	}

	return none
}

fn (t &Transformer) expr_is_rune_arithmetic_operand_cursor(expr ast.Cursor) bool {
	if typ := t.get_expr_type_cursor(expr) {
		if transformer_type_is_rune(typ) {
			return true
		}
	}
	return t.rune_arithmetic_expr_type_cursor(expr) != none
}

fn (mut t Transformer) collect_generic_scan_decl_assign_types(stmt ast.AssignStmt) {
	if stmt.op != .decl_assign || stmt.lhs.len != 1 || stmt.rhs.len != 1 {
		return
	}
	lhs_name := t.get_var_name(stmt.lhs[0])
	if lhs_name == '' {
		return
	}
	rhs := stmt.rhs[0]
	if decl_type := t.decl_assign_storage_type(stmt.lhs[0], rhs) {
		t.remember_local_decl_type(lhs_name, decl_type)
		t.register_local_var_type(lhs_name, decl_type)
	}
	if rhs_type := t.fn_pointer_call_return_type(rhs) {
		t.register_temp_var(lhs_name, rhs_type)
	} else if rhs_type := t.smartcast_type_for_expr(rhs) {
		t.register_local_var_type(lhs_name, rhs_type)
	} else if rhs_type := t.rune_arithmetic_expr_type(rhs) {
		t.register_local_var_type(lhs_name, rhs_type)
	} else if rhs is ast.ArrayInitExpr {
		if rhs_type := t.get_array_init_expr_type(rhs) {
			t.register_local_var_type(lhs_name, rhs_type)
		}
	} else if rhs is ast.CallExpr || rhs is ast.CallOrCastExpr || rhs is ast.InitExpr
		|| rhs is ast.Ident || rhs is ast.SelectorExpr {
		if rhs_type := t.get_expr_type(rhs) {
			t.register_local_var_type(lhs_name, rhs_type)
		}
	}
	if bindings := t.generic_bindings_from_generic_call_expr(rhs) {
		t.local_receiver_generic_bindings[lhs_name] = bindings.clone()
	} else if bindings := t.generic_bindings_from_generic_init_expr(rhs) {
		t.local_receiver_generic_bindings[lhs_name] = bindings.clone()
	}
}

fn (t &Transformer) generic_bindings_from_generic_init_expr_cursor(expr ast.Cursor) ?map[string]types.Type {
	match expr.kind() {
		.expr_init, .expr_assoc {
			return t.generic_bindings_from_type_expr_cursor(expr.edge(0))
		}
		else {}
	}

	return none
}

fn (t &Transformer) generic_bindings_from_generic_init_expr(expr ast.Expr) ?map[string]types.Type {
	match expr {
		ast.InitExpr {
			return t.generic_bindings_from_generic_init_type_expr(expr.typ)
		}
		ast.AssocExpr {
			return t.generic_bindings_from_generic_init_type_expr(expr.typ)
		}
		else {}
	}

	return none
}

fn (t &Transformer) generic_bindings_from_generic_init_type_expr(expr ast.Expr) ?map[string]types.Type {
	match expr {
		ast.GenericArgs {
			generic_params := t.generic_template_type_param_names_from_type_lhs(expr.lhs) or {
				return none
			}
			return t.generic_type_arg_bindings(generic_params, expr.args)
		}
		ast.GenericArgOrIndexExpr {
			generic_params := t.generic_template_type_param_names_from_type_lhs(expr.lhs) or {
				return none
			}
			return t.generic_type_arg_bindings(generic_params, [expr.expr])
		}
		ast.ModifierExpr {
			return t.generic_bindings_from_generic_init_type_expr(expr.expr)
		}
		ast.PrefixExpr {
			return t.generic_bindings_from_generic_init_type_expr(expr.expr)
		}
		ast.Type {
			match expr {
				ast.GenericType {
					generic_params := t.generic_template_type_param_names_from_type_lhs(expr.name) or {
						return none
					}
					return t.generic_type_arg_bindings(generic_params, expr.params)
				}
				ast.PointerType {
					return t.generic_bindings_from_generic_init_type_expr(expr.base_type)
				}
				else {}
			}
		}
		else {}
	}

	return none
}

fn (mut t Transformer) collect_generic_call_specs_in_expr_cursor(expr ast.Cursor) {
	if !expr.is_valid() {
		return
	}
	match expr.kind() {
		.expr_array_init {
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(1))
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(2))
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(3))
			for i in 5 .. expr.edge_count() {
				t.collect_generic_call_specs_in_expr_cursor(expr.edge(i))
			}
		}
		.expr_as_cast {
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(0))
		}
		.expr_assoc {
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(1))
			for i in 2 .. expr.edge_count() {
				t.collect_generic_call_specs_in_expr_cursor(expr.edge(i).edge(0))
			}
		}
		.expr_call {
			lhs_c := expr.edge(0)
			mut args := []ast.Cursor{cap: expr.edge_count() - 1}
			for i in 1 .. expr.edge_count() {
				args << expr.edge(i)
			}
			t.collect_generic_call_spec_for_call_cursor(lhs_c, args)
			t.collect_generic_call_specs_in_expr_cursor(lhs_c)
			for i in 1 .. expr.edge_count() {
				t.collect_generic_call_specs_in_expr_cursor(expr.edge(i))
			}
		}
		.expr_call_or_cast {
			lhs_c := expr.edge(0)
			arg_c := expr.edge(1)
			args := if !arg_c.is_valid() || arg_c.kind() == .expr_empty {
				[]ast.Cursor{}
			} else {
				[arg_c]
			}
			t.collect_generic_call_spec_for_call_cursor(lhs_c, args)
			t.collect_generic_call_specs_in_expr_cursor(lhs_c)
			t.collect_generic_call_specs_in_expr_cursor(arg_c)
		}
		.expr_comptime {
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(0))
		}
		.expr_cast {
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(1))
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(0))
		}
		.aux_field_init {
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(0))
		}
		.expr_fn_literal {
			captured_len := expr.extra_int()
			for i in (1 + captured_len) .. expr.edge_count() {
				t.collect_generic_call_specs_in_stmt_cursor(expr.edge(i))
			}
		}
		.expr_generic_arg_or_index {
			t.collect_generic_struct_spec_from_type_expr_cursor(expr)
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(0))
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(1))
		}
		.expr_generic_args {
			t.collect_generic_struct_spec_from_type_expr_cursor(expr)
			for i in 0 .. expr.edge_count() {
				t.collect_generic_call_specs_in_expr_cursor(expr.edge(i))
			}
		}
		.expr_if {
			t.collect_generic_call_specs_in_if_expr_cursor(expr)
		}
		.expr_if_guard {
			t.collect_generic_call_specs_in_stmt_cursor(expr.edge(0))
		}
		.expr_index {
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(0))
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(1))
		}
		.expr_infix {
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(0))
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(1))
		}
		.expr_init {
			t.collect_generic_struct_spec_from_type_expr_cursor(expr.edge(0))
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(0))
			for i in 1 .. expr.edge_count() {
				t.collect_generic_call_specs_in_expr_cursor(expr.edge(i).edge(0))
			}
		}
		.expr_lambda {
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(0))
		}
		.expr_lock {
			packed := u32(expr.extra_int())
			lock_len := int(packed & 0xFFFF)
			rlock_len := int((packed >> 16) & 0xFFFF)
			for i in 0 .. (lock_len + rlock_len) {
				t.collect_generic_call_specs_in_expr_cursor(expr.edge(i))
			}
			for i in (lock_len + rlock_len) .. expr.edge_count() {
				t.collect_generic_call_specs_in_stmt_cursor(expr.edge(i))
			}
		}
		.expr_map_init {
			keys_len := expr.extra_int()
			for i in 0 .. keys_len {
				t.collect_generic_call_specs_in_expr_cursor(expr.edge(1 + i))
			}
			for i in (1 + keys_len) .. expr.edge_count() {
				t.collect_generic_call_specs_in_expr_cursor(expr.edge(i))
			}
		}
		.expr_match {
			t.collect_generic_call_specs_in_match_expr_cursor(expr)
		}
		.expr_modifier, .expr_paren, .expr_postfix, .expr_prefix {
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(0))
		}
		.expr_or {
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(0))
			for i in 1 .. expr.edge_count() {
				t.collect_generic_call_specs_in_stmt_cursor(expr.edge(i))
			}
		}
		.expr_range {
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(0))
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(1))
		}
		.expr_select {
			t.collect_generic_call_specs_in_stmt_cursor(expr.edge(0))
			for i in 2 .. expr.edge_count() {
				t.collect_generic_call_specs_in_stmt_cursor(expr.edge(i))
			}
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(1))
		}
		.expr_selector {
			t.collect_generic_call_specs_in_expr_cursor(expr.edge(0))
		}
		.expr_string_inter {
			inters := expr.list_at(1)
			for i in 0 .. inters.len() {
				inter := inters.at(i)
				t.collect_generic_call_specs_in_expr_cursor(inter.edge(0))
				t.collect_generic_call_specs_in_expr_cursor(inter.edge(1))
				if is_plain_string_interpolation_cursor(inter) {
					t.collect_explicit_str_interpolation_call_spec(inter.edge(0).expr())
				}
			}
		}
		.expr_tuple, .expr_keyword_operator {
			for i in 0 .. expr.edge_count() {
				t.collect_generic_call_specs_in_expr_cursor(expr.edge(i))
			}
		}
		.expr_unsafe {
			for i in 0 .. expr.edge_count() {
				t.collect_generic_call_specs_in_stmt_cursor(expr.edge(i))
			}
		}
		.typ_anon_struct, .typ_array_fixed, .typ_array, .typ_channel, .typ_fn, .typ_generic,
		.typ_map, .typ_nil, .typ_none, .typ_option, .typ_pointer, .typ_result, .typ_thread,
		.typ_tuple {
			t.collect_generic_call_specs_in_type_cursor(expr)
		}
		else {}
	}
}

fn (mut t Transformer) collect_generic_call_specs_in_expr(expr ast.Expr) {
	match expr {
		ast.ArrayInitExpr {
			t.collect_generic_call_specs_in_expr(expr.init)
			t.collect_generic_call_specs_in_expr(expr.cap)
			t.collect_generic_call_specs_in_expr(expr.len)
			for item in expr.exprs {
				t.collect_generic_call_specs_in_expr(item)
			}
		}
		ast.AsCastExpr {
			t.collect_generic_call_specs_in_expr(expr.expr)
		}
		ast.AssocExpr {
			t.collect_generic_call_specs_in_expr(expr.expr)
			for field in expr.fields {
				t.collect_generic_call_specs_in_expr(field.value)
			}
		}
		ast.CallExpr {
			t.collect_generic_call_spec_for_call(expr.lhs, expr.args)
			t.collect_generic_call_specs_in_expr(expr.lhs)
			for arg in expr.args {
				t.collect_generic_call_specs_in_expr(arg)
			}
		}
		ast.CallOrCastExpr {
			args := if expr.expr is ast.EmptyExpr { []ast.Expr{} } else { [expr.expr] }
			t.collect_generic_call_spec_for_call(expr.lhs, args)
			t.collect_generic_call_specs_in_expr(expr.lhs)
			t.collect_generic_call_specs_in_expr(expr.expr)
		}
		ast.ComptimeExpr {
			t.collect_generic_call_specs_in_expr(expr.expr)
		}
		ast.CastExpr {
			t.collect_generic_call_specs_in_expr(expr.expr)
			t.collect_generic_call_specs_in_expr(expr.typ)
		}
		ast.FieldInit {
			t.collect_generic_call_specs_in_expr(expr.value)
		}
		ast.FnLiteral {
			t.collect_generic_call_specs_in_stmts(expr.stmts)
		}
		ast.GenericArgOrIndexExpr {
			t.collect_generic_struct_spec_from_type_expr(expr)
			t.collect_generic_call_specs_in_expr(expr.lhs)
			t.collect_generic_call_specs_in_expr(expr.expr)
		}
		ast.GenericArgs {
			t.collect_generic_struct_spec_from_type_expr(expr)
			t.collect_generic_call_specs_in_expr(expr.lhs)
			for arg in expr.args {
				t.collect_generic_call_specs_in_expr(arg)
			}
		}
		ast.IfExpr {
			t.collect_generic_call_specs_in_if_expr(expr)
		}
		ast.IfGuardExpr {
			t.collect_generic_call_specs_in_stmt(expr.stmt)
		}
		ast.IndexExpr {
			t.collect_generic_call_specs_in_expr(expr.lhs)
			t.collect_generic_call_specs_in_expr(expr.expr)
		}
		ast.InfixExpr {
			t.collect_generic_call_specs_in_expr(expr.lhs)
			t.collect_generic_call_specs_in_expr(expr.rhs)
		}
		ast.InitExpr {
			t.collect_generic_struct_spec_from_type_expr(expr.typ)
			t.collect_generic_call_specs_in_expr(expr.typ)
			for field in expr.fields {
				t.collect_generic_call_specs_in_expr(field.value)
			}
		}
		ast.LambdaExpr {
			t.collect_generic_call_specs_in_expr(expr.expr)
		}
		ast.LockExpr {
			for lock_expr in expr.lock_exprs {
				t.collect_generic_call_specs_in_expr(lock_expr)
			}
			for lock_expr in expr.rlock_exprs {
				t.collect_generic_call_specs_in_expr(lock_expr)
			}
			t.collect_generic_call_specs_in_stmts(expr.stmts)
		}
		ast.MapInitExpr {
			for key in expr.keys {
				t.collect_generic_call_specs_in_expr(key)
			}
			for val in expr.vals {
				t.collect_generic_call_specs_in_expr(val)
			}
		}
		ast.MatchExpr {
			t.collect_generic_call_specs_in_match_expr(expr)
		}
		ast.ModifierExpr {
			t.collect_generic_call_specs_in_expr(expr.expr)
		}
		ast.OrExpr {
			t.collect_generic_call_specs_in_expr(expr.expr)
			t.collect_generic_call_specs_in_stmts(expr.stmts)
		}
		ast.ParenExpr {
			t.collect_generic_call_specs_in_expr(expr.expr)
		}
		ast.PostfixExpr {
			t.collect_generic_call_specs_in_expr(expr.expr)
		}
		ast.PrefixExpr {
			t.collect_generic_call_specs_in_expr(expr.expr)
		}
		ast.RangeExpr {
			t.collect_generic_call_specs_in_expr(expr.start)
			t.collect_generic_call_specs_in_expr(expr.end)
		}
		ast.SelectExpr {
			t.collect_generic_call_specs_in_stmt(expr.stmt)
			t.collect_generic_call_specs_in_stmts(expr.stmts)
			t.collect_generic_call_specs_in_expr(expr.next)
		}
		ast.SelectorExpr {
			t.collect_generic_call_specs_in_expr(expr.lhs)
		}
		ast.StringInterLiteral {
			for inter in expr.inters {
				t.collect_generic_call_specs_in_expr(inter.expr)
				t.collect_generic_call_specs_in_expr(inter.format_expr)
				if is_plain_string_interpolation(inter) {
					t.collect_explicit_str_interpolation_call_spec(inter.expr)
				}
			}
		}
		ast.Tuple {
			for item in expr.exprs {
				t.collect_generic_call_specs_in_expr(item)
			}
		}
		ast.Type {
			t.collect_generic_struct_spec_from_type_expr(expr)
			t.collect_generic_call_specs_in_type(expr)
		}
		ast.UnsafeExpr {
			t.collect_generic_call_specs_in_stmts(expr.stmts)
		}
		else {}
	}
}

fn (mut t Transformer) collect_generic_call_specs_in_if_expr_cursor(expr ast.Cursor) {
	t.collect_generic_call_specs_in_expr_cursor(expr.edge(0))
	stack_before := t.smartcast_stack.clone()
	counts_before := t.smartcast_expr_counts.clone()
	for ctx in t.generic_scan_smartcast_contexts_from_condition_cursor(expr.edge(0)) {
		t.push_smartcast_ctx(ctx)
	}
	for i in 2 .. expr.edge_count() {
		t.collect_generic_call_specs_in_stmt_cursor(expr.edge(i))
	}
	t.smartcast_stack = stack_before.clone()
	t.smartcast_expr_counts = counts_before.clone()
	t.collect_generic_call_specs_in_expr_cursor(expr.edge(1))
	t.smartcast_stack = stack_before.clone()
	t.smartcast_expr_counts = counts_before.clone()
}

fn (mut t Transformer) collect_generic_call_specs_in_if_expr(expr ast.IfExpr) {
	t.collect_generic_call_specs_in_expr(expr.cond)
	stack_before := t.smartcast_stack.clone()
	counts_before := t.smartcast_expr_counts.clone()
	for ctx in t.generic_scan_smartcast_contexts_from_condition(expr.cond) {
		t.push_smartcast_ctx(ctx)
	}
	t.collect_generic_call_specs_in_stmts(expr.stmts)
	t.smartcast_stack = stack_before.clone()
	t.smartcast_expr_counts = counts_before.clone()
	t.collect_generic_call_specs_in_expr(expr.else_expr)
	t.smartcast_stack = stack_before.clone()
	t.smartcast_expr_counts = counts_before.clone()
}

fn (t &Transformer) generic_scan_smartcast_contexts_from_condition(cond ast.Expr) []SmartcastContext {
	mut body_smartcasts := []SmartcastContext{}
	mut seen_smartcasts := map[string]bool{}
	for term in t.flatten_and_terms_unwrapped(cond) {
		if term is ast.InfixExpr {
			if ctx := t.smartcast_context_from_condition_term(term) {
				key := '${ctx.expr}|${ctx.variant}|${ctx.variant_full}|${ctx.sumtype}'
				if key !in seen_smartcasts {
					seen_smartcasts[key] = true
					body_smartcasts << ctx
				}
			}
		}
	}
	return body_smartcasts
}

fn (mut t Transformer) generic_match_cond_variant_from_generic_expr(lhs ast.Expr, args []ast.Expr) ?GenericMatchCondVariant {
	base_name := t.type_expr_name(lhs)
	base_full := t.type_expr_name_full(lhs)
	if base_name == '' || base_full == '' {
		return none
	}
	mut substituted_args := []ast.Expr{cap: args.len}
	for arg in args {
		substituted := t.substitute_type_in_expr(arg, t.cur_monomorphized_fn_bindings)
		if generic_type_expr_has_open_placeholder(substituted) {
			return none
		}
		substituted_args << substituted
	}
	suffix := t.generic_specialization_suffix(substituted_args)
	if suffix == '' || suffix == '_' {
		return none
	}
	variant_module := if lhs is ast.SelectorExpr && lhs.lhs is ast.Ident {
		(lhs.lhs as ast.Ident).name
	} else {
		''
	}
	return GenericMatchCondVariant{
		name:        base_name
		name_full:   base_full + suffix
		module_name: variant_module
		is_generic:  true
	}
}

fn generic_type_expr_has_open_placeholder(expr ast.Expr) bool {
	match expr {
		ast.Ident {
			return is_generic_placeholder_ident(expr.name)
		}
		ast.GenericArgs {
			if generic_type_expr_has_open_placeholder(expr.lhs) {
				return true
			}
			for arg in expr.args {
				if generic_type_expr_has_open_placeholder(arg) {
					return true
				}
			}
			return false
		}
		ast.GenericArgOrIndexExpr {
			return generic_type_expr_has_open_placeholder(expr.lhs)
				|| generic_type_expr_has_open_placeholder(expr.expr)
		}
		ast.ModifierExpr {
			return generic_type_expr_has_open_placeholder(expr.expr)
		}
		ast.PrefixExpr {
			return generic_type_expr_has_open_placeholder(expr.expr)
		}
		ast.Type {
			return generic_type_node_has_open_placeholder(expr)
		}
		else {
			return false
		}
	}
}

fn generic_type_node_has_open_placeholder(typ ast.Type) bool {
	match typ {
		ast.ArrayType {
			return generic_type_expr_has_open_placeholder(typ.elem_type)
		}
		ast.ArrayFixedType {
			return generic_type_expr_has_open_placeholder(typ.elem_type)
		}
		ast.MapType {
			return generic_type_expr_has_open_placeholder(typ.key_type)
				|| generic_type_expr_has_open_placeholder(typ.value_type)
		}
		ast.OptionType {
			return generic_type_expr_has_open_placeholder(typ.base_type)
		}
		ast.ResultType {
			return generic_type_expr_has_open_placeholder(typ.base_type)
		}
		ast.PointerType {
			return generic_type_expr_has_open_placeholder(typ.base_type)
		}
		ast.GenericType {
			if generic_type_expr_has_open_placeholder(typ.name) {
				return true
			}
			for param in typ.params {
				if generic_type_expr_has_open_placeholder(param) {
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

fn (t &Transformer) generic_scan_smartcast_contexts_from_condition_cursor(cond ast.Cursor) []SmartcastContext {
	mut body_smartcasts := []SmartcastContext{}
	mut seen_smartcasts := map[string]bool{}
	for term in t.flatten_and_terms_unwrapped_cursor(cond) {
		if term.kind() == .expr_infix {
			if ctx := t.smartcast_context_from_condition_term_cursor(term) {
				key := '${ctx.expr}|${ctx.variant}|${ctx.variant_full}|${ctx.sumtype}'
				if key !in seen_smartcasts {
					seen_smartcasts[key] = true
					body_smartcasts << ctx
				}
			}
		}
	}
	return body_smartcasts
}

fn (t &Transformer) flatten_and_terms_unwrapped_cursor(expr ast.Cursor) []ast.Cursor {
	if !expr.is_valid() {
		return []ast.Cursor{}
	}
	if expr.kind() in [.expr_paren, .expr_modifier] {
		return t.flatten_and_terms_unwrapped_cursor(expr.edge(0))
	}
	if expr.kind() == .expr_infix && unsafe { token.Token(int(expr.aux())) } == .and {
		mut terms := []ast.Cursor{}
		terms << t.flatten_and_terms_unwrapped_cursor(expr.edge(0))
		terms << t.flatten_and_terms_unwrapped_cursor(expr.edge(1))
		return terms
	}
	return [expr]
}

fn (t &Transformer) smartcast_context_from_condition_term_cursor(term ast.Cursor) ?SmartcastContext {
	if ctx := t.smartcast_context_from_is_check_cursor(term) {
		return ctx
	}
	return t.smartcast_context_from_tag_check_cursor(term)
}

fn (t &Transformer) smartcast_context_from_is_check_cursor(expr ast.Cursor) ?SmartcastContext {
	if expr.kind() != .expr_infix {
		return none
	}
	op := unsafe { token.Token(int(expr.aux())) }
	if op !in [.key_is, .eq] {
		return none
	}
	lhs_expr := smartcast_lhs_expr_cursor(expr.edge(0))
	rhs := expr.edge(1)
	mut variant_name := ''
	mut variant_module := ''
	if rhs.kind() == .expr_ident {
		variant_name = rhs.name()
	} else if rhs.kind() == .expr_selector {
		variant_name = selector_rhs_name_cursor(rhs)
		rhs_lhs := rhs.edge(0)
		if rhs_lhs.kind() == .expr_ident {
			variant_module = rhs_lhs.name()
		}
	} else {
		variant_name = t.type_expr_to_variant_name_cursor(rhs)
	}
	if variant_name == '' {
		return none
	}
	rhs_lhs := rhs.edge(0)
	rhs_is_enum_shorthand := rhs.kind() == .expr_selector
		&& (!rhs_lhs.is_valid() || rhs_lhs.kind() == .expr_empty)
	mut lhs_is_enum := t.get_enum_type_name_cursor(lhs_expr) != ''
	if lhs_type := t.get_expr_type_cursor(lhs_expr) {
		lhs_base := t.unwrap_alias_and_pointer_type(lhs_type)
		lhs_is_enum = lhs_is_enum || lhs_base is types.Enum
	}
	if lhs_is_enum || rhs_is_enum_shorthand {
		return none
	}
	if op == .eq {
		lookup_name := if variant_module != '' {
			'${variant_module}__${variant_name}'
		} else {
			variant_name
		}
		if t.lookup_type(lookup_name) == none && t.lookup_type(variant_name) == none {
			return none
		}
	}
	if lhs_type := t.get_expr_type_cursor(lhs_expr) {
		lhs_base := t.unwrap_alias_and_pointer_type(lhs_type)
		if lhs_base is types.Interface {
			mut concrete_lookup := if variant_module != '' {
				'${variant_module}__${variant_name}'
			} else {
				variant_name
			}
			mut concrete_type := t.lookup_type(concrete_lookup) or {
				concrete_lookup = variant_name
				t.lookup_type(concrete_lookup) or { return none }
			}
			if concrete_type is types.Interface {
				return none
			}
			if concrete_type.type_name() != '' {
				mut concrete_full := t.type_to_c_name(concrete_type)
				if concrete_full == '' {
					concrete_full = concrete_lookup
				}
				concrete_short := if concrete_full.contains('__') {
					concrete_full.all_after_last('__')
				} else {
					variant_name
				}
				expr_str := expr_cursor_to_string(lhs_expr)
				if expr_str == '' {
					return none
				}
				return SmartcastContext{
					expr:         expr_str
					variant:      concrete_short
					variant_full: concrete_full
					sumtype:      '__iface__${lhs_type.name()}'
				}
			}
		}
	}
	mut sumtype_name := t.generic_scan_sumtype_name_for_expr_cursor(lhs_expr)
	variant_lookup_name := sum_type_variant_name_with_module(variant_module, variant_name)
	rhs_is_type_like := variant_name.len > 0 && variant_name[0] >= `A` && variant_name[0] <= `Z`
	if sumtype_name == '' && (op == .key_is || rhs_is_type_like) {
		sumtype_name = t.find_sumtype_for_variant(variant_lookup_name)
	}
	if sumtype_name == '' {
		return none
	}
	variants := t.get_sum_type_variants(sumtype_name)
	mut has_variant := false
	for v in variants {
		if sum_type_variant_matches_for_sumtype(sumtype_name, v, variant_lookup_name) {
			has_variant = true
			break
		}
	}
	if !has_variant {
		return none
	}
	c_variant_name := t.variant_name_to_c(variant_name)
	qualified_variant := if variant_module != '' {
		'${variant_module}__${c_variant_name}'
	} else {
		c_variant_name
	}
	qualified_variant_full := if variant_module != '' {
		'${variant_module}__${c_variant_name}'
	} else if c_variant_name.contains('__') {
		c_variant_name
	} else if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
		'${t.cur_module}__${c_variant_name}'
	} else {
		c_variant_name
	}
	expr_str := expr_cursor_to_string(lhs_expr)
	if expr_str == '' {
		return none
	}
	return SmartcastContext{
		expr:         expr_str
		variant:      qualified_variant
		variant_full: qualified_variant_full
		sumtype:      sumtype_name
	}
}

fn smartcast_lhs_expr_cursor(expr ast.Cursor) ast.Cursor {
	if expr.kind() in [.expr_modifier, .expr_paren] {
		return smartcast_lhs_expr_cursor(expr.edge(0))
	}
	return expr
}

fn (t &Transformer) type_expr_to_variant_name_cursor(expr ast.Cursor) string {
	if !expr.is_valid() {
		return ''
	}
	match expr.kind() {
		.typ_array {
			elem := t.type_expr_to_variant_name_cursor(expr.edge(0))
			if elem != '' {
				return '[]${elem}'
			}
		}
		.typ_map {
			key := t.type_expr_to_variant_name_cursor(expr.edge(0))
			val := t.type_expr_to_variant_name_cursor(expr.edge(1))
			if key != '' && val != '' {
				return 'map[${key}]${val}'
			}
		}
		.expr_ident {
			name := expr.name()
			if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin'
				&& !name.contains('__') && !t.is_builtin_type_name(name) {
				return '${t.cur_module}__${name}'
			}
			return name
		}
		.expr_selector {
			return selector_type_name_cursor(expr, true)
		}
		else {}
	}

	return ''
}

fn selector_type_name_cursor(expr ast.Cursor, full bool) string {
	mut parts := []string{}
	collect_selector_type_name_cursor_parts(expr, mut parts)
	if parts.len == 0 {
		return ''
	}
	if !full || parts.len == 1 {
		return parts[parts.len - 1]
	}
	module_name := parts[parts.len - 2]
	type_name := parts[parts.len - 1]
	return '${module_name}__${type_name}'
}

fn collect_selector_type_name_cursor_parts(expr ast.Cursor, mut parts []string) bool {
	if !expr.is_valid() {
		return false
	}
	match expr.kind() {
		.expr_ident {
			parts << expr.name()
			return true
		}
		.expr_selector {
			if !collect_selector_type_name_cursor_parts(expr.edge(0), mut parts) {
				return false
			}
			rhs := selector_rhs_name_cursor(expr)
			if rhs == '' {
				return false
			}
			parts << rhs
			return true
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) get_enum_type_name_cursor(expr ast.Cursor) string {
	if typ := t.get_expr_type_cursor(expr) {
		base := t.unwrap_alias_and_pointer_type(typ)
		if base is types.Enum {
			return base.name
		}
	}
	return ''
}

fn (t &Transformer) smartcast_context_from_tag_check_cursor(expr ast.Cursor) ?SmartcastContext {
	if expr.kind() != .expr_infix || unsafe { token.Token(int(expr.aux())) } != .eq {
		return none
	}
	tag_sel := expr.edge(0)
	if tag_sel.kind() != .expr_selector || selector_rhs_name_cursor(tag_sel) != '_tag' {
		return none
	}
	tag_lit := expr.edge(1)
	if tag_lit.kind() != .expr_basic_literal
		|| unsafe { token.Token(int(tag_lit.aux())) } != .number {
		return none
	}
	tag := tag_lit.name().int()
	if tag < 0 {
		return none
	}
	sumtype_expr := tag_sel.edge(0)
	mut sumtype_name := t.generic_scan_sumtype_name_for_expr_cursor(sumtype_expr)
	if sumtype_name == '' {
		return none
	}
	mut variants := t.get_sum_type_variants(sumtype_name)
	if tag >= variants.len {
		storage_sumtype := t.find_sumtype_with_variant_at_tag(sumtype_name, tag)
		if storage_sumtype == '' {
			return none
		}
		sumtype_name = storage_sumtype
		variants = t.get_sum_type_variants(sumtype_name)
	}
	variant_name := variants[tag]
	variant_short := if variant_name.contains('__') {
		variant_name.all_after_last('__')
	} else {
		variant_name
	}
	sumtype_module := if sumtype_name.contains('__') {
		sumtype_name.all_before_last('__')
	} else {
		''
	}
	variant_full := if variant_name.contains('__') || sumtype_module == ''
		|| variant_short.starts_with('Array_') || variant_short.starts_with('Map_') {
		variant_name
	} else {
		'${sumtype_module}__${variant_short}'
	}
	expr_str := expr_cursor_to_string(sumtype_expr)
	if expr_str == '' {
		return none
	}
	return SmartcastContext{
		expr:         expr_str
		variant:      variant_full
		variant_full: variant_full
		sumtype:      sumtype_name
	}
}

fn (t &Transformer) generic_scan_sumtype_name_for_expr_cursor(expr ast.Cursor) string {
	expr_str := expr_cursor_to_string(expr)
	if expr_str != '' {
		if ctx := t.find_smartcast_for_expr(expr_str) {
			variant_name := ctx.variant
			if t.is_sum_type(variant_name) {
				return variant_name
			}
			variant_short := if variant_name.contains('__') {
				variant_name.all_after_last('__')
			} else {
				variant_name
			}
			if t.is_sum_type(variant_short) {
				return variant_short
			}
		}
	}
	typ := t.get_expr_type_cursor(expr) or { return '' }
	type_name := typ.name()
	if type_name.len > 1024 || !transformer_string_has_valid_data(type_name) {
		return ''
	}
	if t.is_sum_type(type_name) {
		return type_name
	}
	type_short := if type_name.contains('__') { type_name.all_after_last('__') } else { type_name }
	if t.is_sum_type(type_short) {
		return type_short
	}
	return ''
}

fn (mut t Transformer) collect_generic_call_specs_in_match_expr_cursor(expr ast.Cursor) {
	expr_c := expr.edge(0)
	t.collect_generic_call_specs_in_expr_cursor(expr_c)
	smartcast_expr := expr_cursor_to_string(expr_c)
	mut sumtype_name := t.generic_scan_sumtype_name_for_expr_cursor(expr_c)
	if sumtype_name != '' && expr.edge_count() > 1 {
		first_branch := expr.edge(1)
		conds := first_branch.list_at(0)
		if conds.len() > 0 {
			first_cond := conds.at(0)
			if first_cond.kind() in [.expr_basic_literal, .expr_string, .expr_string_inter] {
				sumtype_name = ''
			}
		}
	}
	for bi in 1 .. expr.edge_count() {
		branch := expr.edge(bi)
		conds := branch.list_at(0)
		for ci in 0 .. conds.len() {
			t.collect_generic_call_specs_in_expr_cursor(conds.at(ci))
		}
		stmts := branch.list_at(1)
		if sumtype_name == '' || conds.len() == 0 {
			t.collect_generic_call_specs_in_cursor_list(stmts)
			continue
		}
		ctxs := t.generic_match_smartcast_contexts_cursor(smartcast_expr, sumtype_name, conds)
		if ctxs.len == 0 {
			t.collect_generic_call_specs_in_cursor_list(stmts)
			continue
		}
		stack_before := t.smartcast_stack.clone()
		counts_before := t.smartcast_expr_counts.clone()
		for ctx in ctxs {
			t.smartcast_stack = stack_before.clone()
			t.smartcast_expr_counts = counts_before.clone()
			t.push_smartcast_ctx(ctx)
			t.collect_generic_call_specs_in_cursor_list(stmts)
		}
		t.smartcast_stack = stack_before.clone()
		t.smartcast_expr_counts = counts_before.clone()
	}
}

fn (mut t Transformer) collect_generic_call_specs_in_match_expr(expr ast.MatchExpr) {
	t.collect_generic_call_specs_in_expr(expr.expr)
	smartcast_expr := t.expr_to_string(expr.expr)
	mut sumtype_name := t.get_sumtype_name_for_expr(expr.expr)
	if sumtype_name != '' && expr.branches.len > 0 {
		first_branch := expr.branches[0]
		if first_branch.cond.len > 0 {
			first_cond := first_branch.cond[0]
			if first_cond is ast.BasicLiteral || first_cond is ast.StringLiteral
				|| first_cond is ast.StringInterLiteral {
				sumtype_name = ''
			}
		}
	}
	for branch in expr.branches {
		for cond in branch.cond {
			t.collect_generic_call_specs_in_expr(cond)
		}
		if sumtype_name == '' || branch.cond.len == 0 {
			t.collect_generic_call_specs_in_stmts(branch.stmts)
			continue
		}
		ctxs := t.generic_match_smartcast_contexts(smartcast_expr, sumtype_name, branch.cond)
		if ctxs.len == 0 {
			t.collect_generic_call_specs_in_stmts(branch.stmts)
			continue
		}
		stack_before := t.smartcast_stack.clone()
		counts_before := t.smartcast_expr_counts.clone()
		for ctx in ctxs {
			t.smartcast_stack = stack_before.clone()
			t.smartcast_expr_counts = counts_before.clone()
			t.push_smartcast_ctx(ctx)
			t.collect_generic_call_specs_in_stmts(branch.stmts)
		}
		t.smartcast_stack = stack_before.clone()
		t.smartcast_expr_counts = counts_before.clone()
	}
}

fn is_plain_string_interpolation_cursor(inter ast.Cursor) bool {
	if !inter.is_valid() || inter.kind() != .aux_string_inter {
		return false
	}
	format := unsafe { ast.StringInterFormat(int(inter.aux())) }
	if format != .unformatted {
		return false
	}
	if inter.edge_count() >= 4 && (inter.edge(2).extra_int() != 0 || inter.edge(3).extra_int() != 0) {
		return false
	}
	format_expr := inter.edge(1)
	return !format_expr.is_valid() || format_expr.kind() == .expr_empty
}

fn (mut t Transformer) generic_match_smartcast_contexts(smartcast_expr string, sumtype_name string, conds []ast.Expr) []SmartcastContext {
	mut variants := []GenericMatchCondVariant{cap: conds.len}
	for cond in conds {
		variant := t.generic_match_cond_variant_from_expr(cond) or { return []SmartcastContext{} }
		variants << variant
	}
	return t.generic_match_smartcast_contexts_from_variants(smartcast_expr, sumtype_name, variants)
}

fn (t &Transformer) generic_match_smartcast_contexts_cursor(smartcast_expr string, sumtype_name string, conds ast.CursorList) []SmartcastContext {
	mut variants := []GenericMatchCondVariant{cap: conds.len()}
	for ci in 0 .. conds.len() {
		variant := t.generic_match_cond_variant_from_cursor(conds.at(ci)) or {
			return []SmartcastContext{}
		}
		variants << variant
	}
	return t.generic_match_smartcast_contexts_from_variants(smartcast_expr, sumtype_name, variants)
}

struct GenericMatchCondVariant {
	name        string
	name_full   string
	module_name string
	is_generic  bool
}

fn (mut t Transformer) generic_match_cond_variant_from_expr(cond ast.Expr) ?GenericMatchCondVariant {
	if cond is ast.Ident {
		name_full := if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin'
			&& cond.name !in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'byte', 'rune', 'f32', 'f64', 'usize', 'isize', 'bool', 'string', 'voidptr', 'charptr', 'byteptr'] {
			'${t.cur_module}__${cond.name}'
		} else {
			cond.name
		}
		return GenericMatchCondVariant{
			name:      cond.name
			name_full: name_full
		}
	}
	if cond is ast.SelectorExpr {
		mut module_name := ''
		mut name_full := cond.rhs.name
		if cond.lhs is ast.Ident {
			module_name = (cond.lhs as ast.Ident).name
			name_full = '${module_name}__${cond.rhs.name}'
		}
		return GenericMatchCondVariant{
			name:        cond.rhs.name
			name_full:   name_full
			module_name: module_name
		}
	}
	if cond is ast.Type {
		if cond is ast.GenericType {
			return t.generic_match_cond_variant_from_generic_expr(cond.name, cond.params)
		}
		return GenericMatchCondVariant{
			name:      t.type_variant_name(cond)
			name_full: t.type_variant_name_full(cond)
		}
	}
	if cond is ast.GenericArgs {
		return t.generic_match_cond_variant_from_generic_expr(cond.lhs, cond.args)
	}
	if cond is ast.GenericArgOrIndexExpr {
		return t.generic_match_cond_variant_from_generic_expr(cond.lhs, [
			cond.expr,
		])
	}
	return none
}

fn (t &Transformer) generic_match_cond_variant_from_cursor(cond ast.Cursor) ?GenericMatchCondVariant {
	if !cond.is_valid() {
		return none
	}
	match cond.kind() {
		.expr_ident {
			name := cond.name()
			name_full := if t.cur_module != '' && t.cur_module != 'main'
				&& t.cur_module != 'builtin'
				&& name !in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'byte', 'rune', 'f32', 'f64', 'usize', 'isize', 'bool', 'string', 'voidptr', 'charptr', 'byteptr'] {
				'${t.cur_module}__${name}'
			} else {
				name
			}
			return GenericMatchCondVariant{
				name:      name
				name_full: name_full
			}
		}
		.expr_selector {
			rhs := cond.edge(1)
			if !rhs.is_valid() {
				return none
			}
			lhs := cond.edge(0)
			mut module_name := ''
			mut name_full := rhs.name()
			if lhs.is_valid() && lhs.kind() == .expr_ident {
				module_name = lhs.name()
				name_full = '${module_name}__${rhs.name()}'
			}
			return GenericMatchCondVariant{
				name:        rhs.name()
				name_full:   name_full
				module_name: module_name
			}
		}
		.expr_generic_args {
			return t.generic_match_cond_variant_from_generic_cursor(cond.edge(0), cursor_edges(cond,
				1))
		}
		.expr_generic_arg_or_index {
			return t.generic_match_cond_variant_from_generic_cursor(cond.edge(0), [
				cond.edge(1),
			])
		}
		.typ_generic {
			return t.generic_match_cond_variant_from_generic_cursor(cond.edge(0), cursor_edges(cond,
				1))
		}
		.typ_anon_struct, .typ_array_fixed, .typ_array, .typ_channel, .typ_fn, .typ_map, .typ_nil,
		.typ_none, .typ_option, .typ_pointer, .typ_result, .typ_thread, .typ_tuple {
			return GenericMatchCondVariant{
				name:      t.type_variant_name_cursor(cond)
				name_full: t.type_variant_name_full_cursor(cond)
			}
		}
		else {
			return none
		}
	}
}

fn (t &Transformer) generic_match_cond_variant_from_generic_cursor(lhs ast.Cursor, args []ast.Cursor) ?GenericMatchCondVariant {
	base_name := t.type_expr_name_cursor(lhs)
	base_full := t.type_expr_name_full_cursor(lhs)
	if base_name == '' || base_full == '' {
		return none
	}
	suffix := t.generic_specialization_suffix_from_type_cursors(args)
	if suffix == '' || suffix == '_' {
		return none
	}
	variant_module := if lhs.kind() == .expr_selector && lhs.edge(0).kind() == .expr_ident {
		lhs.edge(0).name()
	} else {
		''
	}
	return GenericMatchCondVariant{
		name:        base_name
		name_full:   base_full + suffix
		module_name: variant_module
		is_generic:  true
	}
}

fn (t &Transformer) type_variant_name_cursor(typ ast.Cursor) string {
	match typ.kind() {
		.typ_array {
			elem_name := t.type_expr_name_full_cursor(typ.edge(0))
			return 'Array_${elem_name}'
		}
		.typ_array_fixed {
			elem_name := t.type_expr_name_full_cursor(typ.edge(1))
			mut len_str := '0'
			len_expr := typ.edge(0)
			if len_expr.kind() == .expr_basic_literal {
				len_str = len_expr.name()
			}
			return 'Array_fixed_${elem_name}_${len_str}'
		}
		.typ_map {
			key_name := t.type_expr_name_full_cursor(typ.edge(0))
			val_name := t.type_expr_name_full_cursor(typ.edge(1))
			return 'Map_${key_name}_${val_name}'
		}
		.typ_pointer {
			base_name := t.type_expr_name_cursor(typ.edge(0))
			return '${base_name}ptr'
		}
		.typ_generic {
			return t.type_expr_name_cursor(typ.edge(0))
		}
		.typ_anon_struct {
			return 'AnonStructType'
		}
		.typ_channel {
			return 'ChannelType'
		}
		.typ_fn {
			return 'FnType'
		}
		.typ_nil {
			return 'NilType'
		}
		.typ_none {
			return 'NoneType'
		}
		.typ_option {
			return 'OptionType'
		}
		.typ_result {
			return 'ResultType'
		}
		.typ_thread {
			return 'ThreadType'
		}
		.typ_tuple {
			return 'TupleType'
		}
		else {
			return 'Type'
		}
	}
}

fn (t &Transformer) type_variant_name_full_cursor(typ ast.Cursor) string {
	match typ.kind() {
		.typ_array {
			elem_name := t.type_expr_name_full_cursor(typ.edge(0))
			return 'Array_${elem_name}'
		}
		.typ_array_fixed {
			elem_name := t.type_expr_name_full_cursor(typ.edge(1))
			mut len_str := '0'
			len_expr := typ.edge(0)
			if len_expr.kind() == .expr_basic_literal {
				len_str = len_expr.name()
			}
			return 'Array_fixed_${elem_name}_${len_str}'
		}
		.typ_map {
			key_name := t.type_expr_name_full_cursor(typ.edge(0))
			val_name := t.type_expr_name_full_cursor(typ.edge(1))
			return 'Map_${key_name}_${val_name}'
		}
		.typ_pointer {
			base_name := t.type_expr_name_full_cursor(typ.edge(0))
			return '${base_name}ptr'
		}
		else {
			return t.type_expr_name_full_cursor(typ)
		}
	}
}

fn (t &Transformer) type_expr_name_cursor(expr ast.Cursor) string {
	match expr.kind() {
		.expr_ident {
			return expr.name()
		}
		.expr_lifetime {
			return '^' + expr.name()
		}
		.expr_selector {
			return selector_type_name_cursor(expr, false)
		}
		.typ_anon_struct, .typ_array_fixed, .typ_array, .typ_channel, .typ_fn, .typ_generic,
		.typ_map, .typ_nil, .typ_none, .typ_option, .typ_pointer, .typ_result, .typ_thread,
		.typ_tuple {
			return t.type_variant_name_cursor(expr)
		}
		else {
			return ''
		}
	}
}

fn (t &Transformer) type_expr_name_full_cursor(expr ast.Cursor) string {
	match expr.kind() {
		.expr_ident {
			return t.qualify_type_name(expr.name())
		}
		.expr_lifetime {
			return 'lt__' + expr.name()
		}
		.expr_selector {
			return selector_type_name_cursor(expr, true)
		}
		.typ_anon_struct, .typ_array_fixed, .typ_array, .typ_channel, .typ_fn, .typ_generic,
		.typ_map, .typ_nil, .typ_none, .typ_option, .typ_pointer, .typ_result, .typ_thread,
		.typ_tuple {
			return t.type_variant_name_cursor(expr)
		}
		else {
			return ''
		}
	}
}

fn (t &Transformer) generic_match_smartcast_contexts_from_variants(smartcast_expr string, sumtype_name string, conds []GenericMatchCondVariant) []SmartcastContext {
	if smartcast_expr == '' {
		return []SmartcastContext{}
	}
	variants := t.get_sum_type_variants(sumtype_name)
	if variants.len == 0 {
		return []SmartcastContext{}
	}
	mut ctxs := []SmartcastContext{cap: conds.len}
	for cond in conds {
		c_variant_name := cond.name
		c_variant_name_full := cond.name_full
		c_variant_module := cond.module_name
		if c_variant_name == '' {
			return []SmartcastContext{}
		}
		qualified_variant := if c_variant_module != '' && !c_variant_name.starts_with('Array_')
			&& !c_variant_name.starts_with('Map_') {
			'${c_variant_module}__${c_variant_name}'
		} else {
			c_variant_name
		}
		qualified_variant_full := if c_variant_name_full != ''
			&& c_variant_name_full != c_variant_name {
			c_variant_name_full
		} else if c_variant_module != '' {
			'${c_variant_module}__${c_variant_name}'
		} else {
			c_variant_name
		}
		if !match_cond_variant_matches_sumtype(sumtype_name, variants, c_variant_name,
			qualified_variant, qualified_variant_full, cond.is_generic) {
			return []SmartcastContext{}
		}
		ctxs << SmartcastContext{
			expr:         smartcast_expr
			variant:      qualified_variant
			variant_full: qualified_variant_full
			sumtype:      t.qualify_type_name(sumtype_name)
		}
	}
	return ctxs
}

fn match_cond_variant_matches_sumtype(sumtype_name string, variants []string, variant_name string, qualified_variant string, qualified_variant_full string, is_generic bool) bool {
	for variant in variants {
		if is_generic
			&& sum_type_variant_matches_for_sumtype(sumtype_name, variant, qualified_variant_full) {
			return true
		}
		if sum_type_variant_matches_for_sumtype(sumtype_name, variant, qualified_variant) {
			return true
		}
		if is_generic
			&& sumtype_match_generic_base_is_unique(sumtype_name, variants, qualified_variant)
			&& sum_type_variant_matches_for_sumtype(sumtype_name, sumtype_match_variant_base_name(variant), qualified_variant) {
			return true
		}
		if variant_name.starts_with('Array_') && variant.starts_with('[]') {
			c_elem := variant_name[6..]
			v_elem := variant[2..]
			c_elem_short := if c_elem.contains('__') { c_elem.all_after_last('__') } else { c_elem }
			v_elem_short := if v_elem.contains('__') { v_elem.all_after_last('__') } else { v_elem }
			if c_elem == v_elem || c_elem_short == v_elem_short {
				return true
			}
		}
		if variant_name.starts_with('Array_fixed_') && variant.starts_with('[') {
			return true
		}
		if variant_name.starts_with('Map_') && variant.starts_with('map[') {
			return true
		}
	}
	return false
}

fn (mut t Transformer) collect_generic_call_specs_in_type_cursor(typ ast.Cursor) {
	if !typ.is_valid() {
		return
	}
	if typ.kind() == .typ_generic {
		t.collect_generic_struct_spec_from_type_expr_cursor(typ)
	}
	match typ.kind() {
		.typ_anon_struct {
			generic_params := typ.list_at(0)
			for i in 0 .. generic_params.len() {
				t.collect_generic_call_specs_in_expr_cursor(generic_params.at(i))
			}
			embedded := typ.list_at(1)
			for i in 0 .. embedded.len() {
				t.collect_generic_call_specs_in_expr_cursor(embedded.at(i))
			}
			fields := typ.list_at(2)
			for i in 0 .. fields.len() {
				field := fields.at(i)
				t.collect_generic_call_specs_in_expr_cursor(field.edge(0))
				t.collect_generic_call_specs_in_expr_cursor(field.edge(1))
			}
		}
		.typ_array_fixed {
			t.collect_generic_call_specs_in_expr_cursor(typ.edge(1))
			t.collect_generic_call_specs_in_expr_cursor(typ.edge(0))
		}
		.typ_array, .typ_channel, .typ_option, .typ_pointer, .typ_result, .typ_thread {
			t.collect_generic_call_specs_in_expr_cursor(typ.edge(0))
		}
		.typ_fn {
			generic_params := typ.list_at(0)
			for i in 0 .. generic_params.len() {
				t.collect_generic_call_specs_in_expr_cursor(generic_params.at(i))
			}
			params := typ.list_at(1)
			for i in 0 .. params.len() {
				t.collect_generic_call_specs_in_expr_cursor(params.at(i).edge(0))
			}
			t.collect_generic_call_specs_in_expr_cursor(typ.edge(2))
		}
		.typ_generic {
			t.collect_generic_call_specs_in_expr_cursor(typ.edge(0))
			for i in 1 .. typ.edge_count() {
				t.collect_generic_call_specs_in_expr_cursor(typ.edge(i))
			}
		}
		.typ_map {
			t.collect_generic_call_specs_in_expr_cursor(typ.edge(0))
			t.collect_generic_call_specs_in_expr_cursor(typ.edge(1))
		}
		.typ_tuple {
			for i in 0 .. typ.edge_count() {
				t.collect_generic_call_specs_in_expr_cursor(typ.edge(i))
			}
		}
		else {}
	}
}

fn (mut t Transformer) collect_generic_call_specs_in_type(typ ast.Type) {
	match typ {
		ast.ArrayFixedType {
			t.collect_generic_call_specs_in_expr(typ.elem_type)
			t.collect_generic_call_specs_in_expr(typ.len)
		}
		ast.ArrayType {
			t.collect_generic_call_specs_in_expr(typ.elem_type)
		}
		ast.FnType {
			for param in typ.params {
				t.collect_generic_call_specs_in_expr(param.typ)
			}
			t.collect_generic_call_specs_in_expr(typ.return_type)
		}
		ast.GenericType {
			t.collect_generic_call_specs_in_expr(typ.name)
			for param in typ.params {
				t.collect_generic_call_specs_in_expr(param)
			}
		}
		ast.MapType {
			t.collect_generic_call_specs_in_expr(typ.key_type)
			t.collect_generic_call_specs_in_expr(typ.value_type)
		}
		ast.OptionType {
			t.collect_generic_call_specs_in_expr(typ.base_type)
		}
		ast.PointerType {
			t.collect_generic_call_specs_in_expr(typ.base_type)
		}
		ast.ResultType {
			t.collect_generic_call_specs_in_expr(typ.base_type)
		}
		else {}
	}
}

fn (mut t Transformer) collect_generic_call_spec_for_call(lhs ast.Expr, raw_args []ast.Expr) {
	if lhs is ast.GenericArgs {
		t.collect_explicit_generic_call_spec(lhs.lhs, lhs.args, raw_args)
		return
	}
	if lhs is ast.GenericArgOrIndexExpr {
		t.collect_explicit_generic_call_spec(lhs.lhs, [lhs.expr], raw_args)
		return
	}
	if lhs is ast.IndexExpr {
		t.collect_explicit_generic_call_spec(lhs.lhs, [lhs.expr], raw_args)
		return
	}
	if lhs is ast.Ident {
		if !t.may_target_generic_call_name(lhs.name) {
			return
		}
		info := t.generic_aware_call_fn_info(lhs, lhs.name) or { return }
		t.register_inferred_generic_call_spec(lhs.name, info, raw_args)
		return
	}
	if lhs is ast.SelectorExpr {
		rhs_may_target_generic := t.may_target_generic_call_name(lhs.rhs.name)
		if rhs_may_target_generic {
			if resolved_static := t.resolve_static_type_method_call(lhs.lhs, lhs.rhs.name) {
				info := t.generic_aware_call_fn_info(lhs, resolved_static) or { return }
				t.register_inferred_generic_call_spec(resolved_static, info, raw_args)
				return
			}
			if lhs.lhs is ast.Ident {
				mod_name := lhs.lhs.name
				call_prefix := t.resolve_generic_module_call_prefix(mod_name) or { '' }
				if call_prefix != '' {
					base_name := '${call_prefix}__${lhs.rhs.name}'
					info := t.generic_aware_call_fn_info(lhs, base_name) or { return }
					t.register_inferred_generic_call_spec(base_name, info, raw_args)
					return
				}
			}
			if lhs.lhs is ast.Ident && t.is_module_ident(lhs.lhs.name) {
				mod_name := lhs.lhs.name
				base_name := '${mod_name}__${lhs.rhs.name}'
				info := t.generic_aware_call_fn_info(lhs, base_name) or { return }
				t.register_inferred_generic_call_spec(base_name, info, raw_args)
				return
			}
		}
		t.collect_promoted_embedded_generic_method_call_spec(lhs, raw_args)
		if receiver_generic_method := t.receiver_generic_method_decl_name(lhs.lhs, lhs.rhs.name) {
			info := t.generic_aware_call_fn_info(lhs, receiver_generic_method) or { CallFnInfo{} }
			t.register_inferred_generic_call_spec(receiver_generic_method, info, raw_args)
			t.register_receiver_generic_method_call_spec(receiver_generic_method, lhs.lhs, info,
				raw_args)
		}
		if resolved_method := t.resolve_method_call_name(lhs.lhs, lhs.rhs.name) {
			info := t.generic_aware_call_fn_info(lhs, resolved_method) or { CallFnInfo{} }
			t.register_inferred_generic_call_spec(resolved_method, info, raw_args)
			t.register_receiver_generic_method_call_spec(resolved_method, lhs.lhs, info, raw_args)
		} else {
			t.register_receiver_generic_method_call_spec_from_concrete_receiver(lhs, raw_args)
		}
	}
}

fn (mut t Transformer) collect_explicit_str_interpolation_call_spec(receiver ast.Expr) {
	base_name := t.explicit_str_method_base_name_for_interpolation(receiver) or { return }
	info := t.generic_aware_call_fn_info(explicit_str_selector_expr(receiver), base_name) or {
		CallFnInfo{}
	}
	t.register_receiver_generic_method_call_spec(base_name, receiver, info, []ast.Expr{})
}

fn (mut t Transformer) register_receiver_generic_method_call_spec_from_concrete_receiver(sel ast.SelectorExpr, raw_args []ast.Expr) {
	template_method := t.receiver_generic_template_method_from_concrete_receiver(sel.lhs,
		sel.rhs.name) or { return }
	info := t.generic_aware_call_fn_info(ast.Expr(sel), template_method) or { CallFnInfo{} }
	t.register_receiver_generic_method_call_spec(template_method, sel.lhs, info, raw_args)
}

fn (mut t Transformer) register_receiver_generic_method_call_spec_from_concrete_receiver_cursor(sel ast.Cursor, raw_args []ast.Cursor) {
	rhs_name := selector_rhs_name_cursor(sel)
	template_method := t.receiver_generic_template_method_from_concrete_receiver_cursor(sel.edge(0),
		rhs_name) or { return }
	info := t.generic_aware_call_fn_info_cursor(sel, template_method) or { CallFnInfo{} }
	t.register_receiver_generic_method_call_spec_cursor(template_method, sel.edge(0), info,
		raw_args)
}

fn (t &Transformer) receiver_generic_template_method_from_concrete_receiver(receiver ast.Expr, method_name string) ?string {
	if method_name == '' {
		return none
	}
	mut receiver_types := []types.Type{}
	if declared_type := t.declared_expr_type_for_method_receiver(receiver) {
		receiver_types << declared_type
	}
	if expr_type := t.get_expr_type(receiver) {
		receiver_types << expr_type
	}
	for receiver_type in receiver_types {
		concrete_type := t.unwrap_alias_and_pointer_type(receiver_type)
		mut type_names := []string{}
		type_name := concrete_type.name()
		if type_name != '' {
			type_names << type_name
		}
		c_name := t.type_to_c_name(concrete_type)
		if c_name != '' && c_name !in type_names {
			type_names << c_name
		}
		for concrete_name in type_names {
			if !concrete_name.contains('_T_') {
				continue
			}
			template_method := receiver_specialized_method_template_name('${concrete_name}__${method_name}') or {
				continue
			}
			lookup_key := t.resolve_generic_decl_key_for_call(template_method,
				t.generic_fn_decl_index) or { continue }
			decl := t.generic_fn_decl_index[lookup_key] or { continue }
			if decl.is_method && receiver_generic_param_names(decl).len > 0 {
				return lookup_key
			}
		}
	}
	return none
}

fn (t &Transformer) receiver_generic_template_method_from_concrete_receiver_cursor(receiver ast.Cursor, method_name string) ?string {
	if method_name == '' {
		return none
	}
	mut receiver_types := []types.Type{}
	if smartcast_type := t.smartcast_type_for_expr_cursor(receiver) {
		t.append_receiver_type_candidates(mut receiver_types, smartcast_type)
	}
	if declared_type := t.declared_expr_type_for_method_receiver_cursor(receiver) {
		t.append_receiver_type_candidates(mut receiver_types, declared_type)
	}
	if expr_type := t.get_expr_type_cursor(receiver) {
		t.append_receiver_type_candidates(mut receiver_types, expr_type)
	}
	for receiver_type in receiver_types {
		concrete_type := t.unwrap_alias_and_pointer_type(receiver_type)
		mut type_names := []string{}
		type_name := concrete_type.name()
		if type_name != '' {
			type_names << type_name
		}
		c_name := t.type_to_c_name(concrete_type)
		if c_name != '' && c_name !in type_names {
			type_names << c_name
		}
		type_name_alt := t.type_to_name(concrete_type)
		if type_name_alt != '' && type_name_alt !in type_names {
			type_names << type_name_alt
		}
		for concrete_name in type_names {
			if !concrete_name.contains('_T_') {
				continue
			}
			template_method := receiver_specialized_method_template_name('${concrete_name}__${method_name}') or {
				continue
			}
			lookup_key := t.resolve_generic_decl_key_for_call(template_method,
				t.generic_fn_decl_index) or { continue }
			decl := t.generic_fn_decl_index[lookup_key] or { continue }
			if decl.is_method && receiver_generic_param_names(decl).len > 0 {
				return lookup_key
			}
		}
	}
	return none
}

fn (mut t Transformer) collect_generic_call_spec_for_call_cursor(lhs ast.Cursor, raw_args []ast.Cursor) {
	if !lhs.is_valid() {
		return
	}
	match lhs.kind() {
		.expr_generic_args, .expr_generic_arg_or_index, .expr_index {
			lhs_parts := generic_call_lhs_parts_cursor(lhs)
			t.collect_explicit_generic_call_spec_cursor(lhs_parts.lhs, lhs_parts.args, raw_args)
		}
		.expr_paren, .expr_modifier {
			t.collect_generic_call_spec_for_call_cursor(lhs.edge(0), raw_args)
		}
		.expr_ident {
			name := lhs.name()
			if !t.may_target_generic_call_name(name) {
				return
			}
			info := t.generic_aware_call_fn_info_cursor(lhs, name) or { return }
			t.register_inferred_generic_call_spec_cursor(name, info, raw_args)
		}
		.expr_selector {
			rhs_name := selector_rhs_name_cursor(lhs)
			rhs_may_target_generic := t.may_target_generic_call_name(rhs_name)
			if rhs_may_target_generic {
				if resolved_static := t.resolve_static_type_method_call_cursor(lhs.edge(0),
					rhs_name)
				{
					info := t.generic_aware_call_fn_info_cursor(lhs, resolved_static) or { return }
					t.register_inferred_generic_call_spec_cursor(resolved_static, info, raw_args)
					return
				}
				base_lhs := lhs.edge(0)
				if base_lhs.kind() == .expr_ident {
					mod_name := base_lhs.name()
					call_prefix := t.resolve_generic_module_call_prefix(mod_name) or { '' }
					if call_prefix != '' {
						base_name := '${call_prefix}__${rhs_name}'
						info := t.generic_aware_call_fn_info_cursor(lhs, base_name) or { return }
						t.register_inferred_generic_call_spec_cursor(base_name, info, raw_args)
						return
					}
				}
				if base_lhs.kind() == .expr_ident && t.is_module_ident(base_lhs.name()) {
					base_name := '${base_lhs.name()}__${rhs_name}'
					info := t.generic_aware_call_fn_info_cursor(lhs, base_name) or { return }
					t.register_inferred_generic_call_spec_cursor(base_name, info, raw_args)
					return
				}
			}
			t.collect_promoted_embedded_generic_method_call_spec_cursor(lhs, raw_args)
			if receiver_generic_method := t.receiver_generic_method_decl_name_cursor(lhs.edge(0),
				rhs_name)
			{
				info := t.generic_aware_call_fn_info_cursor(lhs, receiver_generic_method) or {
					CallFnInfo{}
				}
				t.register_inferred_generic_call_spec_cursor(receiver_generic_method, info,
					raw_args)
				t.register_receiver_generic_method_call_spec_cursor(receiver_generic_method,
					lhs.edge(0), info, raw_args)
			}
			if resolved_method := t.resolve_method_call_name_cursor(lhs.edge(0), rhs_name) {
				info := t.generic_aware_call_fn_info_cursor(lhs, resolved_method) or {
					CallFnInfo{}
				}
				t.register_inferred_generic_call_spec_cursor(resolved_method, info, raw_args)
				t.register_receiver_generic_method_call_spec_cursor(resolved_method, lhs.edge(0),
					info, raw_args)
			} else {
				t.register_receiver_generic_method_call_spec_from_concrete_receiver_cursor(lhs,
					raw_args)
			}
		}
		else {}
	}
}

fn (t &Transformer) generic_aware_call_fn_info_cursor(lhs ast.Cursor, base_name string) ?CallFnInfo {
	lhs_expr := call_lhs_expr_from_cursor(lhs) or { ast.empty_expr }
	mut info := t.lookup_call_fn_info(lhs_expr) or {
		decl_info := t.generic_call_info_for_decl(base_name) or { return none }
		return t.drop_method_receiver_from_call_info_cursor(lhs, decl_info)
	}
	if decl_info := t.generic_call_info_for_decl(base_name) {
		decl_call_info := t.drop_method_receiver_from_call_info_cursor(lhs, decl_info)
		needs_decl_generic_signature := info.generic_params.len == 0
			&& decl_call_info.generic_params.len > 0
		if info.generic_params.len == 0 {
			info.generic_params = decl_call_info.generic_params.clone()
		}
		if info.generic_param_names_by_param.len == 0 {
			info.generic_param_names_by_param = decl_call_info.generic_param_names_by_param.clone()
		}
		if info.generic_param_indexes_by_param.len == 0 {
			info.generic_param_indexes_by_param =
				decl_call_info.generic_param_indexes_by_param.clone()
		}
		if info.param_names.len == 0 {
			info.param_names = decl_call_info.param_names.clone()
		}
		if info.param_types.len == 0 || needs_decl_generic_signature {
			info.param_types = decl_call_info.param_types.clone()
		}
		if !info.is_variadic {
			info.is_variadic = decl_call_info.is_variadic
		}
	}
	return info
}

fn (t &Transformer) drop_method_receiver_from_call_info_cursor(lhs ast.Cursor, info CallFnInfo) CallFnInfo {
	mut trimmed := info
	if lhs.kind() != .expr_selector {
		return trimmed
	}
	receiver := lhs.edge(0)
	method_name := selector_rhs_name_cursor(lhs)
	if t.resolve_static_type_method_call_cursor(receiver, method_name) != none {
		return trimmed
	}
	if t.resolve_method_call_name_cursor(receiver, method_name) == none {
		return trimmed
	}
	if trimmed.param_types.len == 0 {
		return trimmed
	}
	first_base := t.unwrap_alias_and_pointer_type(trimmed.param_types[0])
	if recv_type := t.get_expr_type_cursor(receiver) {
		recv_base := t.unwrap_alias_and_pointer_type(recv_type)
		recv_name := recv_base.name()
		first_name := first_base.name()
		if !transformer_string_has_valid_data(recv_name)
			|| !transformer_string_has_valid_data(first_name) {
			return trimmed
		}
		if recv_name != first_name {
			resolved := t.resolve_method_call_name_cursor(receiver, method_name) or {
				return trimmed
			}
			recv_key := resolved.all_before_last('__')
			if !method_receiver_type_matches_resolved_name(t.type_to_c_name(first_base), recv_key) {
				if !method_receiver_type_matches_resolved_name(first_name, recv_key) {
					return trimmed
				}
			}
		}
	} else {
		resolved := t.resolve_method_call_name_cursor(receiver, method_name) or { return trimmed }
		recv_key := resolved.all_before_last('__')
		first_name := first_base.name()
		if !method_receiver_type_matches_resolved_name(t.type_to_c_name(first_base), recv_key) {
			if !transformer_string_has_valid_data(first_name)
				|| !method_receiver_type_matches_resolved_name(first_name, recv_key) {
				return trimmed
			}
		}
	}
	return call_info_without_first_param(trimmed)
}

fn (t &Transformer) resolve_static_type_method_call_cursor(receiver ast.Cursor, method_name string) ?string {
	match receiver.kind() {
		.expr_ident {
			type_name := receiver.name()
			if type_name.len == 0 || type_name[0] < `A` || type_name[0] > `Z` {
				return none
			}
			c_type_name := if t.cur_module != '' && t.cur_module != 'main' {
				'${t.cur_module.replace('.', '__')}__${type_name}'
			} else {
				type_name
			}
			mut lookup_names := []string{cap: 2}
			lookup_names << c_type_name
			if c_type_name != type_name {
				lookup_names << type_name
			}
			return t.resolve_static_type_method_for_names(c_type_name, lookup_names, method_name)
		}
		.expr_selector {
			base_lhs := receiver.edge(0)
			if base_lhs.kind() != .expr_ident {
				return none
			}
			type_name := selector_rhs_name_cursor(receiver)
			if type_name.len == 0 || type_name[0] < `A` || type_name[0] > `Z` {
				return none
			}
			mod_ident := base_lhs.name()
			mut module_names := []string{cap: 2}
			if resolved_mod := t.resolve_module_name(mod_ident) {
				module_names << resolved_mod
			}
			if t.get_module_scope(mod_ident) != none && mod_ident !in module_names {
				module_names << mod_ident
			}
			for mod_name in module_names {
				c_type_name := '${mod_name.replace('.', '__')}__${type_name}'
				mut lookup_names := []string{cap: 2}
				lookup_names << c_type_name
				lookup_names << type_name
				if resolved := t.resolve_static_type_method_for_names(c_type_name, lookup_names,
					method_name)
				{
					return resolved
				}
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) resolve_method_call_name_cursor(receiver ast.Cursor, method_name string) ?string {
	if resolved_static := t.resolve_static_type_method_call_cursor(receiver, method_name) {
		return resolved_static
	}
	mut recv_type := types.Type(types.void_)
	if declared_type := t.declared_expr_type_for_method_receiver_cursor(receiver) {
		recv_type = if t.type_has_cached_method(declared_type, method_name) {
			declared_type
		} else {
			t.get_expr_type_cursor(receiver) or { declared_type }
		}
	} else {
		recv_type = t.get_expr_type_cursor(receiver) or {
			if method_name !in ['str', 'hex', 'clone', 'free', 'trim', 'bytes', 'bytestr', 'replace',
				'contains', 'len', 'index', 'last_index', 'is_blank', 'join', 'to_upper', 'to_lower',
				'repeat', 'vbytes', 'plus_two', 'write_u8', 'write_string', 'write_rune'] {
				if t.lookup_method_cached('array', method_name) != none {
					return 'array__${method_name}'
				}
			}
			if t.is_string_expr_cursor(receiver) {
				if t.lookup_method_cached('string', method_name) != none {
					return 'string__${method_name}'
				}
			}
			return none
		}
	}
	if method_name == 'clone' {
		if _ := t.unwrap_map_type(recv_type) {
			return 'map__clone'
		}
	}
	if alias_method := t.resolve_alias_receiver_method_name(recv_type, method_name) {
		return alias_method
	}
	base_type := t.unwrap_alias_and_pointer_type(recv_type)
	mut c_prefix := t.receiver_type_to_c_prefix(base_type)
	if c_prefix == 'array' && t.is_string_expr_cursor(receiver) {
		c_prefix = 'string'
	}
	if c_prefix == 'i8' && t.lookup_method_cached('i8', method_name) == none
		&& t.lookup_method_cached('u8', method_name) != none {
		c_prefix = 'u8'
	}
	if c_prefix == '' {
		return none
	}
	if base_type is types.Channel && method_name == 'close' {
		return 'chan__close'
	}
	type_name := base_type.name()
	mut lookup_names := []string{cap: 5}
	t.append_method_lookup_type_name(mut lookup_names, type_name)
	if lookup_names.len == 0 {
		return none
	}
	if !type_name.contains('__') && t.cur_module != '' && t.cur_module != 'main' {
		lookup_names << '${t.cur_module}__${type_name}'
	}
	if c_prefix == 'array' && type_name != 'array' && 'array' !in lookup_names {
		lookup_names << 'array'
	} else if c_prefix == 'map' && type_name != 'map' && 'map' !in lookup_names {
		lookup_names << 'map'
	} else if c_prefix == 'string' && type_name != 'string' && 'string' !in lookup_names {
		lookup_names << 'string'
	}
	base_method_name := generic_base_name_without_specialization(method_name)
	if base_method_name != method_name {
		for name in lookup_names {
			if t.lookup_method_cached(name, base_method_name) != none {
				return '${c_prefix}__${method_name}'
			}
		}
	}
	if specific_array_method := t.specific_array_method_c_name_cursor(receiver, method_name) {
		return specific_array_method
	}
	if resolved_declared_method := t.resolve_cached_method_fn_name_for_type(recv_type, method_name,
		c_prefix)
	{
		return resolved_declared_method
	}
	for name in lookup_names {
		if t.lookup_method_cached(name, method_name) != none {
			if c_prefix == 'array' && t.lookup_method_cached('array', method_name) == none {
				mut specific_name := t.type_to_c_name(base_type)
				if specific_name == 'Array_i8'
					&& method_name in ['bytestr', 'byterune', 'hex', 'utf8_to_utf32'] {
					specific_name = 'Array_u8'
				}
				return '${specific_name}__${method_name}'
			}
			return '${c_prefix}__${method_name}'
		}
	}
	for key in t.candidate_method_keys(lookup_names) {
		mut matches_receiver := false
		for name in lookup_names {
			if t.method_key_matches_type_name(key, name) {
				matches_receiver = true
				break
			}
		}
		if !matches_receiver {
			continue
		}
		methods_for_type := t.cached_methods[key] or { continue }
		for method in methods_for_type {
			if method.get_name() == method_name {
				if key != c_prefix && key.contains('__') && c_prefix.contains('__')
					&& key.all_after_last('__') == c_prefix.all_after_last('__') {
					return '${key}__${method_name}'
				}
				return '${c_prefix}__${method_name}'
			}
		}
	}
	if base_type is types.Struct {
		for embedded in base_type.embedded {
			emb_name := embedded.name
			if emb_name == '' {
				continue
			}
			if t.lookup_method_cached(emb_name, method_name) != none {
				return '${emb_name}__${method_name}'
			}
			short_name := emb_name.all_after_last('__')
			if short_name != emb_name && t.lookup_method_cached(short_name, method_name) != none {
				return '${emb_name}__${method_name}'
			}
		}
	}
	if method_name == 'clone' {
		if generated := t.clone_fn_name_for_type(recv_type) {
			return generated
		}
	}
	return none
}

fn (t &Transformer) declared_expr_type_for_method_receiver_cursor(expr ast.Cursor) ?types.Type {
	return t.declared_expr_type_for_method_receiver_cursor_inner(expr, expr_cursor_to_string(expr))
}

fn (t &Transformer) declared_expr_type_for_method_receiver_cursor_inner(expr ast.Cursor, ignored_smartcast_expr string) ?types.Type {
	expr_str := expr_cursor_to_string(expr)
	if expr_str != '' && expr_str != ignored_smartcast_expr {
		if ctx := t.find_smartcast_for_expr(expr_str) {
			if ctx.variant_full != '' {
				if typ := t.c_name_to_type(ctx.variant_full) {
					return typ
				}
			}
			if ctx.variant != '' {
				if typ := t.c_name_to_type(ctx.variant) {
					return typ
				}
			}
		}
	}
	match expr.kind() {
		.expr_ident {
			if typ := t.lookup_local_decl_type(expr.name()) {
				return typ
			}
			if typ := t.lookup_var_type(expr.name()) {
				return typ
			}
			return t.get_expr_type_cursor(expr)
		}
		.expr_selector {
			if lhs_type := t.declared_expr_type_for_method_receiver_cursor_inner(expr.edge(0),
				ignored_smartcast_expr)
			{
				if field_typ := t.field_type_from_receiver_type(lhs_type,
					selector_rhs_name_cursor(expr))
				{
					return field_typ
				}
			}
			return t.get_expr_type_cursor(expr)
		}
		.expr_index {
			if expr.edge(1).kind() == .expr_range {
				return t.get_expr_type_cursor(expr)
			}
			if lhs_type := t.declared_expr_type_for_method_receiver_cursor_inner(expr.edge(0),
				ignored_smartcast_expr)
			{
				mut base_type := lhs_type
				for {
					if base_type is types.Pointer {
						base_type = base_type.base_type
						continue
					}
					if base_type is types.Alias {
						base_type = base_type.base_type
						continue
					}
					break
				}
				if base_type is types.Array {
					return base_type.elem_type
				}
				if base_type is types.ArrayFixed {
					return base_type.elem_type
				}
				if base_type is types.Map {
					return base_type.value_type
				}
				if t.is_string_iterable_type(base_type) {
					return string_iter_value_type()
				}
			}
			return t.get_expr_type_cursor(expr)
		}
		.expr_paren, .expr_modifier {
			return t.declared_expr_type_for_method_receiver_cursor_inner(expr.edge(0),
				ignored_smartcast_expr)
		}
		else {
			return t.get_expr_type_cursor(expr)
		}
	}
}

fn (t &Transformer) specific_array_method_c_name_cursor(receiver ast.Cursor, method_name string) ?string {
	recv_type := t.get_expr_type_cursor(receiver) or { return none }
	base_type := t.unwrap_alias_and_pointer_type(recv_type)
	match base_type {
		types.Array, types.ArrayFixed {
			c_name := t.type_to_c_name(base_type)
			if c_name == '' {
				return none
			}
			base_name := base_type.name()
			if t.lookup_method_cached(base_name, method_name) != none {
				return '${c_name}__${method_name}'
			}
			if c_name != base_name && t.lookup_method_cached(c_name, method_name) != none {
				return '${c_name}__${method_name}'
			}
		}
		else {}
	}

	return none
}

fn (mut t Transformer) collect_promoted_embedded_generic_method_call_spec_cursor(sel ast.Cursor, raw_args []ast.Cursor) {
	recv := sel.edge(0)
	method_name := selector_rhs_name_cursor(sel)
	recv_type := t.get_expr_type_cursor(recv) or { return }
	owner_method_name := generic_base_name_without_specialization(method_name)
	if t.type_has_cached_method(recv_type, owner_method_name) {
		return
	}
	struct_type := t.live_struct_type_from_type(recv_type) or { return }
	for embedded in struct_type.embedded {
		emb_name := embedded.name
		if emb_name == '' {
			continue
		}
		mut resolved := ''
		if t.lookup_method_cached(emb_name, method_name) != none {
			resolved = '${emb_name}__${method_name}'
		} else {
			short_name := emb_name.all_after_last('__')
			if short_name != emb_name && t.lookup_method_cached(short_name, method_name) != none {
				resolved = '${emb_name}__${method_name}'
			}
		}
		if resolved == '' {
			continue
		}
		decl := t.generic_fn_decl_for_call(resolved) or { continue }
		field_name := embedded_field_name_from_type_name(emb_name)
		if field_name == '' {
			continue
		}
		mut bindings := t.generic_bindings_for_struct_field(recv_type, field_name) or {
			map[string]types.Type{}
		}
		if recv_expr := call_lhs_expr_from_cursor(recv) {
			if info := t.generic_aware_call_fn_info(ast.Expr(ast.SelectorExpr{
				lhs: t.synth_selector(recv_expr, field_name, t.field_type_from_receiver_type(recv_type,
					field_name) or { types.Type(t.live_embedded_struct_type(embedded)) })
				rhs: ast.Ident{
					name: method_name
					pos:  sel.edge(1).pos()
				}
				pos: sel.pos()
			}), resolved)
			{
				if arg_bindings := t.generic_bindings_from_call_args_cursor(info, raw_args) {
					for name, typ in arg_bindings {
						if name !in bindings {
							bindings[name] = typ
						}
					}
				}
			}
		}
		if generic_bindings_cover_params(bindings, decl_generic_param_names(decl)) {
			t.register_generic_bindings(resolved, bindings)
		}
	}
}

fn (mut t Transformer) collect_explicit_generic_call_spec_cursor(lhs ast.Cursor, type_args []ast.Cursor, raw_args []ast.Cursor) {
	base_name := t.generic_call_base_name_cursor(lhs) or { return }
	base_lhs := call_lhs_expr_from_cursor(lhs) or { ast.empty_expr }
	type_arg_exprs := type_arg_exprs_from_cursors(type_args)
	info := if type_arg_exprs.len > 0 {
		t.generic_aware_call_fn_info(ast.Expr(ast.GenericArgs{
			lhs:  base_lhs
			args: type_arg_exprs
		}), base_name) or { t.generic_aware_call_fn_info_cursor(lhs, base_name) or { return } }
	} else {
		t.generic_aware_call_fn_info_cursor(lhs, base_name) or { return }
	}
	mut bindings := t.generic_bindings_from_type_args_cursor(info, type_args) or { return }
	t.fill_missing_generic_bindings_from_call_args_cursor(info, raw_args, mut bindings)
	t.register_generic_bindings(base_name, bindings)
	if lhs.kind() == .expr_selector {
		t.register_receiver_generic_method_call_spec_with_bindings_cursor(base_name, lhs.edge(0),
			bindings, info, raw_args)
	}
}

fn type_arg_exprs_from_cursors(type_args []ast.Cursor) []ast.Expr {
	mut out := []ast.Expr{cap: type_args.len}
	for arg in type_args {
		type_arg := type_arg_expr_from_cursor(arg) or { continue }
		out << type_arg
	}
	return out
}

fn (mut t Transformer) register_inferred_generic_call_spec_cursor(base_name string, info CallFnInfo, raw_args []ast.Cursor) {
	bindings := t.generic_bindings_from_call_args_cursor(info, raw_args) or { return }
	t.register_generic_bindings(base_name, bindings)
}

fn (mut t Transformer) register_receiver_generic_method_call_spec_cursor(base_name string, receiver ast.Cursor, info CallFnInfo, raw_args []ast.Cursor) {
	t.register_receiver_generic_method_call_spec_with_bindings_cursor(base_name, receiver,
		map[string]types.Type{}, info, raw_args)
}

fn (mut t Transformer) register_receiver_generic_method_call_spec_with_bindings_cursor(base_name string, receiver ast.Cursor, seed_bindings map[string]types.Type, info CallFnInfo, raw_args []ast.Cursor) {
	decl := t.generic_fn_decl_for_call(base_name) or { return }
	if receiver_generic_param_names(decl).len == 0 {
		return
	}
	mut bindings := seed_bindings.clone()
	if receiver_bindings := t.generic_bindings_from_method_receiver_cursor(decl, receiver,
		base_name)
	{
		for name, typ in receiver_bindings {
			bindings[name] = typ
		}
	}
	if arg_bindings := t.generic_bindings_from_call_args_cursor(info, raw_args) {
		for name, typ in arg_bindings {
			if name !in bindings {
				bindings[name] = typ
			}
		}
	}
	for param_name in decl_generic_param_names(decl) {
		if param_name !in bindings {
			return
		}
	}
	t.register_generic_bindings(base_name, bindings)
}

fn (t &Transformer) receiver_generic_method_decl_name_cursor(receiver ast.Cursor, method_name string) ?string {
	if method_name == '' {
		return none
	}
	receiver_type := t.get_expr_type_cursor(receiver) or { return none }
	base_type := t.unwrap_alias_and_pointer_type(receiver_type)
	for raw_name in [base_type.name(), t.type_to_c_name(base_type),
		t.type_to_name(base_type)] {
		mut type_name := normalized_method_lookup_type_name(raw_name)
		if type_name == '' || !type_name.contains('_T_') {
			continue
		}
		type_name = type_name.all_before('_T_')
		for owner in [type_name, type_name.all_after_last('__')] {
			if owner == '' {
				continue
			}
			method_key := '${owner}__${method_name}'
			if decl := t.generic_fn_decl_for_call(method_key) {
				if receiver_generic_param_names(decl).len > 0 {
					return method_key
				}
			}
		}
	}
	return none
}

fn (t &Transformer) generic_bindings_from_method_receiver_cursor(decl ast.FnDecl, receiver ast.Cursor, base_name string) ?map[string]types.Type {
	receiver_params := receiver_generic_param_names(decl)
	if receiver_params.len == 0 {
		return none
	}
	if receiver.kind() == .expr_ident {
		if local_bindings := t.local_receiver_generic_bindings[receiver.name()] {
			mut complete := true
			for param_name in receiver_params {
				if param_name !in local_bindings {
					complete = false
					break
				}
			}
			if complete {
				return local_bindings.clone()
			}
		}
	}
	mut receiver_types := []types.Type{}
	if smartcast_type := t.smartcast_type_for_expr_cursor(receiver) {
		t.append_receiver_type_candidates(mut receiver_types, smartcast_type)
	}
	if declared_type := t.declared_expr_type_for_method_receiver_cursor(receiver) {
		t.append_receiver_type_candidates(mut receiver_types, declared_type)
	}
	if expr_type := t.get_expr_type_cursor(receiver) {
		t.append_receiver_type_candidates(mut receiver_types, expr_type)
	}
	for receiver_type in receiver_types {
		concrete_type := t.unwrap_alias_and_pointer_type(receiver_type)
		mut bindings := map[string]types.Type{}
		if concrete_type is types.Struct {
			if template := t.receiver_generic_template_struct(decl, base_name, concrete_type) {
				t.infer_receiver_generic_bindings_from_struct(template, concrete_type,
					receiver_params, mut bindings)
			}
		}
		mut complete := true
		for param_name in receiver_params {
			if param_name !in bindings {
				complete = false
				break
			}
		}
		if complete {
			return bindings
		}
	}
	return none
}

fn (mut t Transformer) defer_generic_call_spec_for_cloned_call(lhs ast.Expr, raw_args []ast.Expr) {
	if lhs is ast.Ident {
		if !t.may_target_generic_call_name(lhs.name) {
			return
		}
		info := t.generic_aware_call_fn_info(lhs, lhs.name) or { return }
		t.defer_inferred_generic_call_spec(lhs.name, info, raw_args)
		return
	}
	if lhs is ast.SelectorExpr {
		rhs_may_target_generic := t.may_target_generic_call_name(lhs.rhs.name)
		if rhs_may_target_generic {
			if resolved_static := t.resolve_static_type_method_call(lhs.lhs, lhs.rhs.name) {
				info := t.generic_aware_call_fn_info(lhs, resolved_static) or { return }
				t.defer_inferred_generic_call_spec(resolved_static, info, raw_args)
				return
			}
			if lhs.lhs is ast.Ident {
				mod_name := lhs.lhs.name
				call_prefix := t.resolve_generic_module_call_prefix(mod_name) or { '' }
				if call_prefix != '' {
					base_name := '${call_prefix}__${lhs.rhs.name}'
					info := t.generic_aware_call_fn_info(lhs, base_name) or { return }
					t.defer_inferred_generic_call_spec(base_name, info, raw_args)
					return
				}
			}
			if lhs.lhs is ast.Ident && t.is_module_ident(lhs.lhs.name) {
				mod_name := lhs.lhs.name
				base_name := '${mod_name}__${lhs.rhs.name}'
				info := t.generic_aware_call_fn_info(lhs, base_name) or { return }
				t.defer_inferred_generic_call_spec(base_name, info, raw_args)
				return
			}
		}
		t.defer_promoted_embedded_generic_method_call_spec(lhs, raw_args)
		if receiver_generic_method := t.receiver_generic_method_decl_name(lhs.lhs, lhs.rhs.name) {
			info := t.generic_aware_call_fn_info(lhs, receiver_generic_method) or { CallFnInfo{} }
			t.defer_inferred_generic_call_spec(receiver_generic_method, info, raw_args)
			t.defer_receiver_generic_method_call_spec(receiver_generic_method, lhs.lhs, info,
				raw_args)
		}
		if resolved_method := t.resolve_method_call_name(lhs.lhs, lhs.rhs.name) {
			info := t.generic_aware_call_fn_info(lhs, resolved_method) or { CallFnInfo{} }
			t.defer_inferred_generic_call_spec(resolved_method, info, raw_args)
			t.defer_receiver_generic_method_call_spec(resolved_method, lhs.lhs, info, raw_args)
		}
	}
}

fn (t &Transformer) receiver_generic_method_decl_name(receiver ast.Expr, method_name string) ?string {
	if method_name == '' {
		return none
	}
	receiver_type := t.get_expr_type(receiver) or { return none }
	base_type := t.unwrap_alias_and_pointer_type(receiver_type)
	for raw_name in [base_type.name(), t.type_to_c_name(base_type),
		t.type_to_name(base_type)] {
		mut type_name := normalized_method_lookup_type_name(raw_name)
		if type_name == '' || !type_name.contains('_T_') {
			continue
		}
		type_name = type_name.all_before('_T_')
		for owner in [type_name, type_name.all_after_last('__')] {
			if owner == '' {
				continue
			}
			method_key := '${owner}__${method_name}'
			if decl := t.generic_fn_decl_for_call(method_key) {
				if receiver_generic_param_names(decl).len > 0 {
					return method_key
				}
			}
		}
	}
	return none
}

fn (mut t Transformer) defer_inferred_generic_call_spec(base_name string, info CallFnInfo, raw_args []ast.Expr) {
	bindings := t.generic_bindings_from_call_args(info, raw_args) or { return }
	t.defer_generic_bindings(base_name, bindings)
}

fn (mut t Transformer) defer_generic_bindings(base_name string, bindings map[string]types.Type) {
	if base_name == '' || base_name.contains('_T_') || bindings.len == 0 {
		return
	}
	normalized_bindings := normalize_generic_bindings(bindings)
	for _, typ in normalized_bindings {
		if clone_type_contains_generic_placeholder(typ) {
			return
		}
	}
	t.deferred_generic_call_specs << DeferredGenericCallSpec{
		base_name: base_name
		bindings:  normalized_bindings
		file_idx:  t.cur_generic_call_file_idx
	}
}

fn (mut t Transformer) defer_receiver_generic_method_call_spec(base_name string, receiver ast.Expr, info CallFnInfo, raw_args []ast.Expr) {
	lookup_key := t.resolve_generic_decl_key_for_call(base_name, t.generic_fn_decl_index) or {
		return
	}
	decl := t.generic_fn_decl_index[lookup_key] or { return }
	if receiver_generic_param_names(decl).len == 0 {
		return
	}
	mut bindings := map[string]types.Type{}
	if receiver_bindings := t.generic_bindings_from_method_receiver(decl, receiver, base_name) {
		for name, typ in receiver_bindings {
			bindings[name] = typ
		}
	}
	if arg_bindings := t.generic_bindings_from_call_args(info, raw_args) {
		for name, typ in arg_bindings {
			if name !in bindings {
				bindings[name] = typ
			}
		}
	}
	if generic_bindings_cover_params(bindings, decl_generic_param_names(decl)) {
		t.defer_generic_bindings(lookup_key, bindings)
	}
}

fn (mut t Transformer) defer_promoted_embedded_generic_method_call_spec(sel ast.SelectorExpr, raw_args []ast.Expr) {
	recv_type := t.get_expr_type(sel.lhs) or { return }
	owner_method_name := generic_base_name_without_specialization(sel.rhs.name)
	if t.type_has_cached_method(recv_type, owner_method_name) {
		return
	}
	struct_type := t.live_struct_type_from_type(recv_type) or { return }
	for embedded in struct_type.embedded {
		emb_name := embedded.name
		if emb_name == '' {
			continue
		}
		mut resolved := ''
		if t.lookup_method_cached(emb_name, sel.rhs.name) != none {
			resolved = '${emb_name}__${sel.rhs.name}'
		} else {
			short_name := emb_name.all_after_last('__')
			if short_name != emb_name && t.lookup_method_cached(short_name, sel.rhs.name) != none {
				resolved = '${emb_name}__${sel.rhs.name}'
			}
		}
		if resolved == '' {
			continue
		}
		decl := t.generic_fn_decl_for_call(resolved) or { continue }
		field_name := embedded_field_name_from_type_name(emb_name)
		if field_name == '' {
			continue
		}
		mut bindings := t.generic_bindings_for_struct_field(recv_type, field_name) or {
			map[string]types.Type{}
		}
		if info := t.generic_aware_call_fn_info(ast.Expr(ast.SelectorExpr{
			lhs: t.synth_selector(sel.lhs, field_name, t.field_type_from_receiver_type(recv_type,
				field_name) or { types.Type(t.live_embedded_struct_type(embedded)) })
			rhs: sel.rhs
			pos: sel.pos
		}), resolved)
		{
			if arg_bindings := t.generic_bindings_from_call_args(info, raw_args) {
				for name, typ in arg_bindings {
					if name !in bindings {
						bindings[name] = typ
					}
				}
			}
		}
		if generic_bindings_cover_params(bindings, decl_generic_param_names(decl)) {
			t.defer_generic_bindings(resolved, bindings)
		}
	}
}

fn (mut t Transformer) collect_promoted_embedded_generic_method_call_spec(sel ast.SelectorExpr, raw_args []ast.Expr) {
	recv_type := t.get_expr_type(sel.lhs) or { return }
	owner_method_name := generic_base_name_without_specialization(sel.rhs.name)
	if t.type_has_cached_method(recv_type, owner_method_name) {
		return
	}
	struct_type := t.live_struct_type_from_type(recv_type) or { return }
	for embedded in struct_type.embedded {
		emb_name := embedded.name
		if emb_name == '' {
			continue
		}
		mut resolved := ''
		if t.lookup_method_cached(emb_name, sel.rhs.name) != none {
			resolved = '${emb_name}__${sel.rhs.name}'
		} else {
			short_name := emb_name.all_after_last('__')
			if short_name != emb_name && t.lookup_method_cached(short_name, sel.rhs.name) != none {
				resolved = '${emb_name}__${sel.rhs.name}'
			}
		}
		if resolved == '' {
			continue
		}
		decl := t.generic_fn_decl_for_call(resolved) or { continue }
		field_name := embedded_field_name_from_type_name(emb_name)
		if field_name == '' {
			continue
		}
		mut bindings := t.generic_bindings_for_struct_field(recv_type, field_name) or {
			map[string]types.Type{}
		}
		if info := t.generic_aware_call_fn_info(ast.Expr(ast.SelectorExpr{
			lhs: t.synth_selector(sel.lhs, field_name, t.field_type_from_receiver_type(recv_type,
				field_name) or { types.Type(t.live_embedded_struct_type(embedded)) })
			rhs: sel.rhs
			pos: sel.pos
		}), resolved)
		{
			if arg_bindings := t.generic_bindings_from_call_args(info, raw_args) {
				for name, typ in arg_bindings {
					if name !in bindings {
						bindings[name] = typ
					}
				}
			}
		}
		if generic_bindings_cover_params(bindings, decl_generic_param_names(decl)) {
			t.register_generic_bindings(resolved, bindings)
		}
	}
}

fn (mut t Transformer) collect_explicit_generic_call_spec(lhs ast.Expr, type_args []ast.Expr, raw_args []ast.Expr) {
	base_name := t.generic_call_base_name(lhs) or { return }
	info := t.generic_aware_call_fn_info(ast.Expr(ast.GenericArgs{
		lhs:  lhs
		args: type_args
	}), base_name) or { t.generic_aware_call_fn_info(lhs, base_name) or { return } }
	mut bindings := t.generic_bindings_from_type_args(info, type_args) or { return }
	t.fill_missing_generic_bindings_from_call_args(info, raw_args, mut bindings)
	t.register_generic_bindings(base_name, bindings)
	if lhs is ast.SelectorExpr {
		t.register_receiver_generic_method_call_spec_with_bindings(base_name, lhs.lhs, bindings,
			info, raw_args)
	}
}

fn (t &Transformer) generic_call_base_name(lhs ast.Expr) ?string {
	parts := generic_call_lhs_parts(lhs)
	base_lhs := parts.lhs
	if base_lhs is ast.Ident {
		return base_lhs.name
	}
	if base_lhs is ast.SelectorExpr {
		if resolved_static := t.resolve_static_type_method_call(base_lhs.lhs, base_lhs.rhs.name) {
			return resolved_static
		}
		if base_lhs.lhs is ast.Ident {
			if call_prefix := t.resolve_generic_module_call_prefix(base_lhs.lhs.name) {
				return '${call_prefix}__${base_lhs.rhs.name}'
			}
		}
		if base_lhs.lhs is ast.Ident && t.is_module_ident(base_lhs.lhs.name) {
			return '${base_lhs.lhs.name}__${base_lhs.rhs.name}'
		}
		if resolved_method := t.resolve_method_call_name(base_lhs.lhs, base_lhs.rhs.name) {
			return resolved_method
		}
	}
	return none
}

fn generic_call_lhs_parts(lhs ast.Expr) GenericIndexCallParts {
	match lhs {
		ast.GenericArgs {
			nested := generic_call_lhs_parts(lhs.lhs)
			mut args := nested.args.clone()
			args << lhs.args
			return GenericIndexCallParts{
				lhs:  nested.lhs
				args: args
			}
		}
		ast.GenericArgOrIndexExpr {
			nested := generic_call_lhs_parts(lhs.lhs)
			mut args := nested.args.clone()
			args << lhs.expr
			return GenericIndexCallParts{
				lhs:  nested.lhs
				args: args
			}
		}
		ast.IndexExpr {
			return generic_index_call_parts(lhs)
		}
		else {
			return GenericIndexCallParts{
				lhs:  lhs
				args: []ast.Expr{}
			}
		}
	}
}

fn (t &Transformer) resolve_generic_module_call_prefix(module_ident string) ?string {
	if call_prefix := t.resolve_module_call_prefix(module_ident) {
		return call_prefix
	}
	if full_name := t.cur_import_aliases[module_ident] {
		return module_call_c_prefix(full_name)
	}
	return none
}

fn (t &Transformer) generic_bindings_from_type_args(info CallFnInfo, type_args []ast.Expr) ?map[string]types.Type {
	if info.generic_params.len == 0 || type_args.len == 0 {
		return none
	}
	return t.generic_type_arg_bindings(info.generic_params, type_args)
}

fn (mut t Transformer) register_inferred_generic_call_spec(base_name string, info CallFnInfo, raw_args []ast.Expr) {
	bindings := t.generic_bindings_from_call_args(info, raw_args) or { return }
	t.register_generic_bindings(base_name, bindings)
}

fn (t &Transformer) fill_missing_generic_bindings_from_call_args(info CallFnInfo, raw_args []ast.Expr, mut bindings map[string]types.Type) {
	arg_bindings := t.generic_bindings_from_call_args(info, raw_args) or { return }
	for name, typ in arg_bindings {
		if name !in bindings {
			bindings[name] = typ
		}
	}
}

fn (mut t Transformer) register_receiver_generic_method_call_spec(base_name string, receiver ast.Expr, info CallFnInfo, raw_args []ast.Expr) {
	t.register_receiver_generic_method_call_spec_with_bindings(base_name, receiver,
		map[string]types.Type{}, info, raw_args)
}

fn (mut t Transformer) register_receiver_generic_method_call_spec_with_bindings(base_name string, receiver ast.Expr, seed_bindings map[string]types.Type, info CallFnInfo, raw_args []ast.Expr) {
	lookup_key := t.resolve_generic_decl_key_for_call(base_name, t.generic_fn_decl_index) or {
		return
	}
	decl := t.generic_fn_decl_index[lookup_key] or { return }
	if receiver_generic_param_names(decl).len == 0 {
		return
	}
	mut bindings := seed_bindings.clone()
	if receiver_bindings := t.generic_bindings_from_method_receiver(decl, receiver, base_name) {
		for name, typ in receiver_bindings {
			bindings[name] = typ
		}
	}
	if arg_bindings := t.generic_bindings_from_call_args(info, raw_args) {
		for name, typ in arg_bindings {
			if name !in bindings {
				bindings[name] = typ
			}
		}
	}
	for param_name in decl_generic_param_names(decl) {
		if param_name !in bindings {
			return
		}
	}
	t.register_generic_bindings(lookup_key, bindings)
}

fn (t &Transformer) generic_fn_decl_for_call(base_name string) ?ast.FnDecl {
	lookup_key := t.resolve_generic_decl_key_for_call(base_name, t.generic_fn_decl_index) or {
		return none
	}
	return t.generic_fn_decl_index[lookup_key] or { none }
}

fn (t &Transformer) generic_bindings_from_generic_call_expr(expr ast.Expr) ?map[string]types.Type {
	mut lhs := ast.empty_expr
	mut args := []ast.Expr{}
	match expr {
		ast.CallExpr {
			lhs = expr.lhs
			args = expr.args.clone()
		}
		ast.CallOrCastExpr {
			lhs = expr.lhs
			if expr.expr !is ast.EmptyExpr {
				args << expr.expr
			}
		}
		else {
			return none
		}
	}

	lhs_parts := generic_call_lhs_parts(lhs)
	if lhs_parts.args.len > 0 && t.call_or_cast_lhs_is_type(lhs_parts.lhs) {
		if bindings := t.generic_bindings_from_generic_type_parts(lhs_parts.lhs, lhs_parts.args) {
			return bindings
		}
	}
	base_name := t.generic_call_base_name(lhs_parts.lhs) or { return none }
	info_lhs := if lhs_parts.args.len > 0 {
		ast.Expr(ast.GenericArgs{
			lhs:  lhs_parts.lhs
			args: lhs_parts.args
		})
	} else {
		lhs_parts.lhs
	}
	info := t.generic_aware_call_fn_info(info_lhs, base_name) or { return none }
	mut bindings := t.generic_bindings_from_type_args(info, lhs_parts.args) or {
		map[string]types.Type{}
	}
	t.fill_missing_generic_bindings_from_call_args(info, args, mut bindings)
	if !generic_bindings_cover_params(bindings, info.generic_params) {
		return none
	}
	return bindings
}

struct GenericIndexCallCursorParts {
	lhs  ast.Cursor
	args []ast.Cursor
}

fn (t &Transformer) generic_bindings_from_generic_call_expr_cursor(expr ast.Cursor) ?map[string]types.Type {
	mut lhs := ast.Cursor{}
	mut args := []ast.Cursor{}
	match expr.kind() {
		.expr_call {
			lhs = expr.edge(0)
			for i in 1 .. expr.edge_count() {
				args << expr.edge(i)
			}
		}
		.expr_call_or_cast {
			lhs = expr.edge(0)
			arg := expr.edge(1)
			if arg.is_valid() && arg.kind() != .expr_empty {
				args << arg
			}
		}
		else {
			return none
		}
	}

	if bindings := t.generic_bindings_from_type_expr_cursor(lhs) {
		return bindings
	}
	lhs_parts := generic_call_lhs_parts_cursor(lhs)
	base_name := t.generic_call_base_name_cursor(lhs_parts.lhs) or { return none }
	info_lhs := call_lhs_expr_from_cursor(lhs_parts.lhs) or { return none }
	info := t.generic_aware_call_fn_info(info_lhs, base_name) or { return none }
	mut bindings := t.generic_bindings_from_type_args_cursor(info, lhs_parts.args) or {
		map[string]types.Type{}
	}
	t.fill_missing_generic_bindings_from_call_args_cursor(info, args, mut bindings)
	if !generic_bindings_cover_params(bindings, info.generic_params) {
		return none
	}
	return bindings
}

fn generic_call_lhs_parts_cursor(lhs ast.Cursor) GenericIndexCallCursorParts {
	match lhs.kind() {
		.expr_generic_args {
			nested := generic_call_lhs_parts_cursor(lhs.edge(0))
			mut args := nested.args.clone()
			args << cursor_edges(lhs, 1)
			return GenericIndexCallCursorParts{
				lhs:  nested.lhs
				args: args
			}
		}
		.expr_generic_arg_or_index, .expr_index {
			nested := generic_call_lhs_parts_cursor(lhs.edge(0))
			mut args := nested.args.clone()
			args << lhs.edge(1)
			return GenericIndexCallCursorParts{
				lhs:  nested.lhs
				args: args
			}
		}
		else {
			return GenericIndexCallCursorParts{
				lhs:  lhs
				args: []ast.Cursor{}
			}
		}
	}
}

fn (t &Transformer) generic_call_base_name_cursor(lhs ast.Cursor) ?string {
	if lhs.kind() == .expr_ident {
		return lhs.name()
	}
	if lhs.kind() == .expr_selector {
		base_lhs := lhs.edge(0)
		rhs_name := selector_rhs_name_cursor(lhs)
		base_lhs_expr := call_lhs_expr_from_cursor(base_lhs) or { return none }
		base_expr := ast.Expr(ast.SelectorExpr{
			lhs: base_lhs_expr
			rhs: ast.Ident{
				name: rhs_name
				pos:  lhs.edge(1).pos()
			}
			pos: lhs.pos()
		})
		if resolved_static := t.resolve_static_type_method_call(base_lhs_expr, rhs_name) {
			return resolved_static
		}
		if base_lhs.kind() == .expr_ident {
			if call_prefix := t.resolve_generic_module_call_prefix(base_lhs.name()) {
				return '${call_prefix}__${rhs_name}'
			}
		}
		if base_lhs.kind() == .expr_ident && t.is_module_ident(base_lhs.name()) {
			return '${base_lhs.name()}__${rhs_name}'
		}
		if resolved_method := t.resolve_method_call_name(base_lhs_expr, rhs_name) {
			return resolved_method
		}
		_ = base_expr
	}
	return none
}

fn call_lhs_expr_from_cursor(expr ast.Cursor) ?ast.Expr {
	match expr.kind() {
		.expr_ident {
			return ast.Expr(ast.Ident{
				name: expr.name()
				pos:  expr.pos()
			})
		}
		.expr_selector {
			lhs := call_lhs_expr_from_cursor(expr.edge(0)) or { return none }
			return ast.Expr(ast.SelectorExpr{
				lhs: lhs
				rhs: ast.Ident{
					name: selector_rhs_name_cursor(expr)
					pos:  expr.edge(1).pos()
				}
				pos: expr.pos()
			})
		}
		.expr_generic_args {
			lhs := call_lhs_expr_from_cursor(expr.edge(0)) or { return none }
			return ast.Expr(ast.GenericArgs{
				lhs:  lhs
				args: type_arg_exprs_from_cursor_edges(expr, 1)
				pos:  expr.pos()
			})
		}
		.expr_generic_arg_or_index, .expr_index {
			lhs := call_lhs_expr_from_cursor(expr.edge(0)) or { return none }
			arg := type_arg_expr_from_cursor(expr.edge(1)) or { return none }
			return ast.Expr(ast.GenericArgOrIndexExpr{
				lhs:  lhs
				expr: arg
				pos:  expr.pos()
			})
		}
		else {
			return none
		}
	}
}

fn type_arg_exprs_from_cursor_edges(expr ast.Cursor, start int) []ast.Expr {
	mut out := []ast.Expr{cap: expr.edge_count() - start}
	for i in start .. expr.edge_count() {
		arg := type_arg_expr_from_cursor(expr.edge(i)) or { continue }
		out << arg
	}
	return out
}

fn type_arg_expr_from_cursor(expr ast.Cursor) ?ast.Expr {
	match expr.kind() {
		.expr_ident {
			return ast.Expr(ast.Ident{
				name: expr.name()
				pos:  expr.pos()
			})
		}
		.expr_selector {
			return call_lhs_expr_from_cursor(expr)
		}
		.typ_generic {
			lhs := type_arg_expr_from_cursor(expr.edge(0)) or { return none }
			return ast.Expr(ast.Type(ast.GenericType{
				name:   lhs
				params: type_arg_exprs_from_cursor_edges(expr, 1)
			}))
		}
		.typ_array {
			elem := type_arg_expr_from_cursor(expr.edge(0)) or { return none }
			return ast.Expr(ast.Type(ast.ArrayType{
				elem_type: elem
			}))
		}
		.typ_pointer {
			base := type_arg_expr_from_cursor(expr.edge(0)) or { return none }
			return ast.Expr(ast.Type(ast.PointerType{
				base_type: base
				lifetime:  expr.name()
			}))
		}
		.typ_option {
			base := type_arg_expr_from_cursor(expr.edge(0)) or { return none }
			return ast.Expr(ast.Type(ast.OptionType{
				base_type: base
			}))
		}
		.typ_result {
			base := type_arg_expr_from_cursor(expr.edge(0)) or { return none }
			return ast.Expr(ast.Type(ast.ResultType{
				base_type: base
			}))
		}
		.typ_map {
			key := type_arg_expr_from_cursor(expr.edge(0)) or { return none }
			value := type_arg_expr_from_cursor(expr.edge(1)) or { return none }
			return ast.Expr(ast.Type(ast.MapType{
				key_type:   key
				value_type: value
			}))
		}
		else {
			return none
		}
	}
}

fn (t &Transformer) generic_bindings_from_type_args_cursor(info CallFnInfo, type_args []ast.Cursor) ?map[string]types.Type {
	if info.generic_params.len == 0 || type_args.len == 0 {
		return none
	}
	arg_bindings := t.generic_type_arg_bindings_cursor(info.generic_params, type_args) or {
		return none
	}
	return arg_bindings.bindings.clone()
}

fn (t &Transformer) fill_missing_generic_bindings_from_call_args_cursor(info CallFnInfo, raw_args []ast.Cursor, mut bindings map[string]types.Type) {
	arg_bindings := t.generic_bindings_from_call_args_cursor(info, raw_args) or { return }
	for name, typ in arg_bindings {
		if name !in bindings {
			bindings[name] = typ
		}
	}
}

fn (t &Transformer) generic_bindings_from_call_args_cursor(info CallFnInfo, call_args []ast.Cursor) ?map[string]types.Type {
	if info.generic_params.len == 0 {
		return none
	}
	mut bindings := map[string]types.Type{}
	for i, arg in call_args {
		param_idx := i
		if param_idx >= info.generic_param_indexes_by_param.len {
			break
		}
		generic_idx := info.generic_param_indexes_by_param[param_idx]
		if generic_idx < 0 || generic_idx >= info.generic_params.len {
			continue
		}
		generic_name := info.generic_params[generic_idx]
		if generic_name in bindings {
			continue
		}
		if !t.call_info_param_is_direct_generic(info, param_idx, generic_name) {
			continue
		}
		arg_type := t.call_arg_type_for_generic_infer_cursor(arg) or { continue }
		bindings[generic_name] = arg_type
	}
	t.infer_generic_bindings_from_call_args_cursor(info.param_types, call_args,
		info.generic_params, 0, mut bindings)
	if bindings.len != info.generic_params.len && info.param_types.len > call_args.len {
		mut tail_bindings := map[string]types.Type{}
		param_offset := info.param_types.len - call_args.len
		for i, arg in call_args {
			param_idx := i + param_offset
			if param_idx >= info.generic_param_indexes_by_param.len {
				break
			}
			generic_idx := info.generic_param_indexes_by_param[param_idx]
			if generic_idx < 0 || generic_idx >= info.generic_params.len {
				continue
			}
			generic_name := info.generic_params[generic_idx]
			if generic_name in tail_bindings {
				continue
			}
			if !t.call_info_param_is_direct_generic(info, param_idx, generic_name) {
				continue
			}
			arg_type := t.call_arg_type_for_generic_infer_cursor(arg) or { continue }
			tail_bindings[generic_name] = arg_type
		}
		t.infer_generic_bindings_from_call_args_cursor(info.param_types, call_args,
			info.generic_params, param_offset, mut tail_bindings)
		if tail_bindings.len > bindings.len {
			bindings = tail_bindings.move()
		}
	}
	for param_name in info.generic_params {
		if param_name in bindings {
			continue
		}
		if concrete := t.cur_monomorphized_fn_bindings[param_name] {
			bindings[param_name] = concrete
		}
	}
	if bindings.len != info.generic_params.len {
		return none
	}
	return bindings
}

fn (t &Transformer) infer_generic_bindings_from_call_args_cursor(param_types []types.Type, call_args []ast.Cursor, generic_params []string, param_offset int, mut bindings map[string]types.Type) {
	if param_offset < 0 {
		return
	}
	for i, arg in call_args {
		param_idx := i + param_offset
		if param_idx >= param_types.len {
			break
		}
		arg_type := t.call_arg_type_for_generic_infer_cursor(arg) or { continue }
		t.infer_generic_type_from_call_arg(param_types[param_idx], arg_type, generic_params, mut
			bindings)
	}
}

fn (t &Transformer) call_arg_type_for_generic_infer_cursor(arg ast.Cursor) ?types.Type {
	base_arg := if arg.kind() == .expr_modifier { arg.edge(0) } else { arg }
	if base_arg.kind() == .expr_ident {
		if ctx := t.find_smartcast_for_expr(base_arg.name()) {
			variant_type_name := if ctx.variant_full != '' { ctx.variant_full } else { ctx.variant }
			if typ := t.c_name_to_type(variant_type_name) {
				return typ
			}
		}
		if typ := t.lookup_local_decl_type(base_arg.name()) {
			return typ
		}
		if typ := t.lookup_var_type(base_arg.name()) {
			return typ
		}
	}
	if base_arg.kind() == .expr_basic_literal {
		kind := unsafe { token.Token(int(base_arg.aux())) }
		match kind {
			.number {
				if base_arg.name().contains('.') {
					return types.Type(types.f64_)
				}
				return types.Type(types.int_)
			}
			.string {
				return types.Type(types.string_)
			}
			.key_true, .key_false {
				return types.Type(types.bool_)
			}
			else {}
		}
	}
	return t.get_expr_type_cursor(base_arg)
}

fn (t &Transformer) receiver_generic_method_call_name(base_name string, receiver ast.Expr, info CallFnInfo, raw_args []ast.Expr) ?string {
	lookup_key := t.resolve_generic_decl_key_for_call(base_name, t.generic_fn_decl_index) or {
		return none
	}
	decl := t.generic_fn_decl_index[lookup_key] or { return none }
	receiver_params := receiver_generic_param_names(decl)
	if receiver_params.len == 0 {
		return none
	}
	mut bindings := map[string]types.Type{}
	if receiver_bindings := t.generic_bindings_from_method_receiver(decl, receiver, base_name) {
		for name, typ in receiver_bindings {
			bindings[name] = typ
		}
	}
	if arg_bindings := t.generic_bindings_from_call_args(info, raw_args) {
		for name, typ in arg_bindings {
			if name !in bindings {
				bindings[name] = typ
			}
		}
	}
	for param_name in decl_generic_param_names(decl) {
		if param_name !in bindings {
			return none
		}
	}
	method_name := t.specialized_receiver_method_name(decl, bindings) or { return none }
	if method_name == decl.name {
		return none
	}
	return t.qualify_receiver_generic_method_call_name(lookup_key, decl, method_name)
}

fn (t &Transformer) is_receiver_generic_method_call_base(base_name string) bool {
	lookup_key := t.resolve_generic_decl_key_for_call(base_name, t.generic_fn_decl_index) or {
		return false
	}
	decl := t.generic_fn_decl_index[lookup_key] or { return false }
	return decl.is_method && receiver_generic_param_names(decl).len > 0
}

fn (t &Transformer) qualify_receiver_generic_method_call_name(lookup_key string, decl ast.FnDecl, method_name string) ?string {
	if method_name == '' {
		return none
	}
	receiver_template := t.get_receiver_type_name(decl.receiver.typ)
	if receiver_template == '' {
		return method_name
	}
	template_prefix := lookup_key.all_before_last('__')
	module_prefix_suffix := '__${receiver_template}'
	if template_prefix.ends_with(module_prefix_suffix) {
		module_prefix := template_prefix[..template_prefix.len - module_prefix_suffix.len]
		if module_prefix != '' {
			return '${module_prefix}__${method_name}'
		}
	}
	return method_name
}

fn (t &Transformer) generic_bindings_from_method_receiver(decl ast.FnDecl, receiver ast.Expr, base_name string) ?map[string]types.Type {
	receiver_params := receiver_generic_param_names(decl)
	if receiver_params.len == 0 {
		return none
	}
	if receiver is ast.Ident {
		if local_bindings := t.local_receiver_generic_bindings[receiver.name] {
			mut complete := true
			for param_name in receiver_params {
				if param_name !in local_bindings {
					complete = false
					break
				}
			}
			if complete {
				return local_bindings.clone()
			}
		}
	}
	mut receiver_types := []types.Type{}
	if smartcast_type := t.smartcast_type_for_expr(receiver) {
		receiver_types << smartcast_type
	}
	if declared_type := t.declared_expr_type_for_method_receiver(receiver) {
		t.append_receiver_type_candidates(mut receiver_types, declared_type)
	}
	if expr_type := t.get_expr_type(receiver) {
		t.append_receiver_type_candidates(mut receiver_types, expr_type)
	}
	for receiver_type in receiver_types {
		concrete_type := t.unwrap_alias_and_pointer_type(receiver_type)
		mut bindings := map[string]types.Type{}
		if concrete_type is types.Struct {
			if template := t.receiver_generic_template_struct(decl, base_name, concrete_type) {
				t.infer_receiver_generic_bindings_from_struct(template, concrete_type,
					receiver_params, mut bindings)
			}
		}
		mut complete := true
		for param_name in receiver_params {
			if param_name !in bindings {
				complete = false
				break
			}
		}
		if complete {
			return bindings
		}
	}
	return none
}

fn (t &Transformer) append_receiver_type_candidates(mut receiver_types []types.Type, typ types.Type) {
	receiver_types << typ
	if t.cur_monomorphized_fn_bindings.len == 0 {
		return
	}
	receiver_types << substitute_type(typ, t.cur_monomorphized_fn_bindings)
}

fn (t &Transformer) receiver_generic_template_struct(decl ast.FnDecl, base_name string, concrete types.Struct) ?types.Struct {
	mut candidates := []string{}
	if base_name.contains('__') {
		append_unique_generic_lookup_name(mut candidates, base_name.all_before_last('__'))
	}
	recv_name := t.get_receiver_type_name(decl.receiver.typ)
	if recv_name != '' {
		append_unique_generic_lookup_name(mut candidates, recv_name)
	}
	if concrete.name != '' {
		append_unique_generic_lookup_name(mut candidates, concrete.name)
	}
	mut fallback := types.Struct{}
	mut has_fallback := false
	for candidate in candidates {
		mut lookup_name := candidate
		if lookup_name.contains('_T_') {
			lookup_name = lookup_name.all_before('_T_')
		}
		for name in [lookup_name, lookup_name.all_after_last('__')] {
			if name == '' {
				continue
			}
			if typ := t.lookup_type(name) {
				if typ is types.Struct && typ.generic_params.len > 0 {
					if typ.fields.len > 0 {
						return typ
					}
					if !has_fallback {
						fallback = typ
						has_fallback = true
					}
				}
			}
			if st := t.lookup_struct_type_any_module(name) {
				if st.generic_params.len > 0 {
					if st.fields.len > 0 {
						return st
					}
					if !has_fallback {
						fallback = st
						has_fallback = true
					}
				}
			}
		}
	}
	if has_fallback {
		return fallback
	}
	return none
}

fn append_unique_generic_lookup_name(mut names []string, name string) {
	if name == '' || name in names {
		return
	}
	names << name
}

fn (t &Transformer) infer_receiver_generic_bindings_from_struct(template types.Struct, concrete types.Struct, generic_params []string, mut bindings map[string]types.Type) {
	for template_field in template.fields {
		concrete_field := struct_field_by_name(concrete, template_field.name) or { continue }
		t.infer_generic_type_from_call_arg(template_field.typ, concrete_field.typ, generic_params, mut
			bindings)
	}
}

fn struct_field_by_name(st types.Struct, name string) ?types.Field {
	for field in st.fields {
		if field.name == name {
			return field
		}
	}
	return none
}

fn (mut t Transformer) register_generic_bindings(base_name string, bindings map[string]types.Type) {
	if base_name == '' || bindings.len == 0 {
		return
	}
	normalized_bindings := normalize_generic_bindings(bindings)
	for _, typ in normalized_bindings {
		if clone_type_contains_generic_placeholder(typ) {
			return
		}
	}
	signature := generic_bindings_signature(normalized_bindings)
	mut existing := t.env.generic_types[base_name] or { []map[string]types.Type{} }
	for item in existing {
		if generic_bindings_signature(item) == signature {
			return
		}
	}
	if t.cur_generic_call_file_idx >= 0 {
		t.generic_spec_owner_file[generic_spec_owner_key(base_name, normalized_bindings)] = t.cur_generic_call_file_idx
	}
	existing << normalized_bindings
	t.env.generic_types[base_name] = existing
}

fn normalize_generic_bindings(bindings map[string]types.Type) map[string]types.Type {
	mut out := map[string]types.Type{}
	for name, typ in bindings {
		out[name] = normalize_generic_concrete_type(typ)
	}
	return out
}

fn generic_spec_owner_key(base_name string, bindings map[string]types.Type) string {
	return '${base_name}:${generic_bindings_signature(bindings)}'
}

fn (mut t Transformer) register_monomorphized_fn_bindings(module_name string, fn_name string, bindings map[string]types.Type) {
	if fn_name == '' || bindings.len == 0 {
		return
	}
	normalized_bindings := normalize_generic_bindings(bindings)
	for key in monomorphized_fn_binding_lookup_keys(module_name, fn_name) {
		t.monomorphized_fn_bindings[key] = normalized_bindings.clone()
	}
}

fn (t &Transformer) lookup_monomorphized_fn_bindings(module_name string, fn_name string) ?map[string]types.Type {
	for key in monomorphized_fn_binding_lookup_keys(module_name, fn_name) {
		if bindings := t.monomorphized_fn_bindings[key] {
			return bindings.clone()
		}
	}
	return none
}

fn monomorphized_fn_binding_lookup_keys(module_name string, fn_name string) []string {
	if fn_name == '' {
		return []string{}
	}
	mut keys := []string{}
	append_unique_generic_lookup_name(mut keys, fn_name)
	for prefix in monomorphized_fn_binding_module_prefixes(module_name) {
		qualified_prefix := '${prefix}__'
		if fn_name.starts_with(qualified_prefix) {
			append_unique_generic_lookup_name(mut keys, fn_name[qualified_prefix.len..])
		} else {
			append_unique_generic_lookup_name(mut keys, '${qualified_prefix}${fn_name}')
		}
	}
	return keys
}

fn monomorphized_fn_binding_module_prefixes(module_name string) []string {
	if module_name == '' {
		return []string{}
	}
	mut prefixes := []string{}
	append_unique_generic_lookup_name(mut prefixes, module_name)
	append_unique_generic_lookup_name(mut prefixes, module_name.replace('.', '__'))
	call_prefix := module_call_c_prefix(module_name)
	append_unique_generic_lookup_name(mut prefixes, call_prefix)
	return prefixes
}

fn generic_bindings_signature(bindings map[string]types.Type) string {
	mut keys := bindings.keys()
	keys.sort()
	mut parts := []string{cap: keys.len}
	for key in keys {
		concrete := bindings[key] or { continue }
		parts << '${key}:${concrete.name()}'
	}
	return parts.join('|')
}

// substitute_type returns typ with any NamedType placeholder appearing in
// bindings replaced by its concrete binding, recursively.
pub fn substitute_type(typ types.Type, bindings map[string]types.Type) types.Type {
	mut seen := map[string]bool{}
	return substitute_type_with_seen(typ, bindings, mut seen)
}

fn substitution_safe_string(s string) string {
	if transformer_string_has_valid_data(s) {
		return s
	}
	return ''
}

fn substitute_type_with_seen(typ types.Type, bindings map[string]types.Type, mut seen map[string]bool) types.Type {
	if bindings.len == 0 || !types.type_has_valid_payload(typ) {
		return typ
	}
	match typ {
		types.NamedType {
			if !transformer_string_has_valid_data(typ) {
				return typ
			}
			name := string(typ)
			if concrete := bindings[name] {
				return concrete
			}
			return types.Type(types.NamedType(name))
		}
		types.Pointer {
			return types.Type(types.Pointer{
				lifetime:  typ.lifetime
				base_type: substitute_type_with_seen(typ.base_type, bindings, mut seen)
			})
		}
		types.Array {
			return types.Type(types.Array{
				elem_type: substitute_type_with_seen(typ.elem_type, bindings, mut seen)
			})
		}
		types.ArrayFixed {
			return types.Type(types.ArrayFixed{
				len:       typ.len
				elem_type: substitute_type_with_seen(typ.elem_type, bindings, mut seen)
			})
		}
		types.Map {
			return types.Type(types.Map{
				key_type:   substitute_type_with_seen(typ.key_type, bindings, mut seen)
				value_type: substitute_type_with_seen(typ.value_type, bindings, mut seen)
			})
		}
		types.OptionType {
			return types.Type(types.OptionType{
				base_type: substitute_type_with_seen(typ.base_type, bindings, mut seen)
			})
		}
		types.ResultType {
			return types.Type(types.ResultType{
				base_type: substitute_type_with_seen(typ.base_type, bindings, mut seen)
			})
		}
		types.Alias {
			return types.Type(types.Alias{
				name:      substitution_safe_string(typ.name)
				base_type: substitute_type_with_seen(typ.base_type, bindings, mut seen)
			})
		}
		types.Channel {
			if elem_type := typ.elem_type {
				return types.Type(types.Channel{
					elem_type: substitute_type_with_seen(elem_type, bindings, mut seen)
				})
			}
			return types.Type(types.Channel{})
		}
		types.Enum {
			mut fields := []types.Field{cap: typ.fields.len}
			for field in typ.fields {
				fields << substitute_field_type_with_seen(field, bindings, mut seen)
			}
			return types.Type(types.Enum{
				is_flag: typ.is_flag
				name:    substitution_safe_string(typ.name)
				fields:  fields
			})
		}
		types.Interface {
			mut fields := []types.Field{cap: typ.fields.len}
			for field in typ.fields {
				fields << substitute_field_type_with_seen(field, bindings, mut seen)
			}
			return types.Type(types.Interface{
				name:   substitution_safe_string(typ.name)
				fields: fields
			})
		}
		types.Struct {
			struct_name := substitution_safe_string(typ.name)
			if struct_name != '' {
				if struct_name in seen {
					return types.Type(typ)
				}
				seen[struct_name] = true
			}
			mut fields := []types.Field{cap: typ.fields.len}
			for field in typ.fields {
				fields << substitute_field_type_with_seen(field, bindings, mut seen)
			}
			mut embedded := []types.Struct{cap: typ.embedded.len}
			for embedded_type in typ.embedded {
				substituted :=
					substitute_type_with_seen(types.Type(embedded_type), bindings, mut seen)
				if substituted is types.Struct {
					embedded << substituted
				} else {
					embedded << embedded_type
				}
			}
			if struct_name != '' {
				seen.delete(struct_name)
			}
			return types.Type(types.Struct{
				name:           struct_name
				generic_params: if bindings.len == 0 { typ.generic_params } else { []string{} }
				implements:     typ.implements
				embedded:       embedded
				fields:         fields
				is_soa:         typ.is_soa
			})
		}
		types.SumType {
			mut variants := []types.Type{cap: typ.variants.len}
			for variant in typ.variants {
				variants << substitute_type_with_seen(variant, bindings, mut seen)
			}
			return types.Type(types.SumType{
				name:           substitution_safe_string(typ.name)
				generic_params: if bindings.len == 0 { typ.generic_params } else { []string{} }
				variants:       variants
			})
		}
		types.Primitive {
			return types.Type(typ)
		}
		types.Char, types.ISize, types.Nil, types.None, types.Rune, types.String, types.USize,
		types.Void {
			return types.Type(typ)
		}
		else {
			return typ
		}
	}
}

fn substitute_field_type_with_seen(field types.Field, bindings map[string]types.Type, mut seen map[string]bool) types.Field {
	return types.Field{
		name:                substitution_safe_string(field.name)
		typ:                 substitute_type_with_seen(field.typ, bindings, mut seen)
		default_expr:        field.default_expr
		attributes:          field.attributes
		is_public:           field.is_public
		is_mut:              field.is_mut
		is_module_mut:       field.is_module_mut
		is_interface_method: field.is_interface_method
		owner_module:        field.owner_module
	}
}

// substitute_type_in_expr rewrites an ast.Expr that *names a type* by replacing
// placeholder Idents (T, U, ...) with the concrete type expression from bindings.
// Type-bearing nodes (ArrayType, MapType, PointerType, OptionType, ResultType,
// GenericType, FnType) recurse into their child type exprs. Non-type-bearing
// nodes are returned unchanged.
//
// The receiver is mut because new synth positions are allocated for cloned
// nodes (so they do not collide with the originals' positions).
pub fn (mut t Transformer) substitute_type_in_expr(expr ast.Expr, bindings map[string]types.Type) ast.Expr {
	if bindings.len == 0 {
		return expr
	}
	match expr {
		ast.Ident {
			if concrete := bindings[expr.name] {
				return t.type_to_ast_expr(concrete, expr.pos)
			}
			return expr
		}
		ast.GenericArgOrIndexExpr {
			substituted := ast.Expr(ast.GenericArgOrIndexExpr{
				lhs:  t.substitute_type_in_expr(expr.lhs, bindings)
				expr: t.substitute_type_in_expr(expr.expr, bindings)
				pos:  expr.pos
			})
			t.collect_generic_struct_spec_from_type_expr(substituted)
			if concrete := t.concrete_generic_struct_type_expr(substituted) {
				return concrete
			}
			return substituted
		}
		ast.GenericArgs {
			mut new_args := []ast.Expr{cap: expr.args.len}
			for arg in expr.args {
				new_args << t.substitute_type_in_expr(arg, bindings)
			}
			substituted := ast.Expr(ast.GenericArgs{
				lhs:  t.substitute_type_in_expr(expr.lhs, bindings)
				args: new_args
				pos:  expr.pos
			})
			t.collect_generic_struct_spec_from_type_expr(substituted)
			if concrete := t.concrete_generic_struct_type_expr(substituted) {
				return concrete
			}
			return substituted
		}
		ast.Type {
			substituted_type := t.substitute_type_in_type_node(expr, bindings)
			substituted := ast.Expr(substituted_type)
			t.collect_generic_struct_spec_from_type_expr(substituted)
			if concrete := t.concrete_generic_struct_type_expr(substituted) {
				return concrete
			}
			return substituted
		}
		else {
			return expr
		}
	}
}

// substitute_type_in_type_node handles the inner sum type variants of ast.Type.
// Split out because ast.Type is a nested sum type within ast.Expr; matching on
// ast.Expr only yields the ast.Type wrapper, never its inner variants directly.
pub fn (mut t Transformer) substitute_type_in_type_node(typ ast.Type, bindings map[string]types.Type) ast.Type {
	match typ {
		ast.ArrayType {
			return ast.Type(ast.ArrayType{
				elem_type: t.substitute_type_in_expr(typ.elem_type, bindings)
			})
		}
		ast.ArrayFixedType {
			return ast.Type(ast.ArrayFixedType{
				len:       typ.len
				elem_type: t.substitute_type_in_expr(typ.elem_type, bindings)
			})
		}
		ast.MapType {
			return ast.Type(ast.MapType{
				key_type:   t.substitute_type_in_expr(typ.key_type, bindings)
				value_type: t.substitute_type_in_expr(typ.value_type, bindings)
			})
		}
		ast.PointerType {
			return ast.Type(ast.PointerType{
				base_type: t.substitute_type_in_expr(typ.base_type, bindings)
				lifetime:  typ.lifetime
			})
		}
		ast.OptionType {
			return ast.Type(ast.OptionType{
				base_type: t.substitute_type_in_expr(typ.base_type, bindings)
			})
		}
		ast.ResultType {
			return ast.Type(ast.ResultType{
				base_type: t.substitute_type_in_expr(typ.base_type, bindings)
			})
		}
		ast.GenericType {
			mut new_generic_params := []ast.Expr{cap: typ.params.len}
			for p in typ.params {
				new_generic_params << t.substitute_type_in_expr(p, bindings)
			}
			return ast.Type(ast.GenericType{
				name:   typ.name
				params: new_generic_params
			})
		}
		ast.ChannelType {
			return ast.Type(ast.ChannelType{
				cap:       typ.cap
				elem_type: t.substitute_type_in_expr(typ.elem_type, bindings)
			})
		}
		ast.ThreadType {
			return ast.Type(ast.ThreadType{
				elem_type: t.substitute_type_in_expr(typ.elem_type, bindings)
			})
		}
		ast.FnType {
			mut new_params := []ast.Parameter{cap: typ.params.len}
			for p in typ.params {
				new_params << ast.Parameter{
					name:   p.name
					typ:    t.substitute_type_in_expr(p.typ, bindings)
					is_mut: p.is_mut
					pos:    p.pos
				}
			}
			return ast.Type(ast.FnType{
				generic_params: typ.generic_params
				params:         new_params
				return_type:    t.substitute_type_in_expr(typ.return_type, bindings)
			})
		}
		else {
			return typ
		}
	}
}

fn (mut t Transformer) specialized_receiver_type_expr(expr ast.Expr, bindings map[string]types.Type, pos token.Pos) ?ast.Expr {
	match expr {
		ast.GenericArgs, ast.GenericArgOrIndexExpr {
			receiver_name := t.specialized_receiver_type_name(expr, bindings) or { return none }
			return ast.Expr(ast.Ident{
				name: receiver_name
				pos:  pos
			})
		}
		ast.ModifierExpr {
			specialized := t.specialized_receiver_type_expr(expr.expr, bindings, pos) or {
				return none
			}
			return ast.Expr(ast.ModifierExpr{
				kind: expr.kind
				expr: specialized
				pos:  expr.pos
			})
		}
		ast.PrefixExpr {
			if expr.op in [.amp, .mul] {
				specialized := t.specialized_receiver_type_expr(expr.expr, bindings, pos) or {
					return none
				}
				return ast.Expr(ast.PrefixExpr{
					op:   expr.op
					expr: specialized
					pos:  expr.pos
				})
			}
		}
		ast.Type {
			match expr {
				ast.PointerType {
					specialized := t.specialized_receiver_type_expr(expr.base_type, bindings, pos) or {
						return none
					}
					return ast.Expr(ast.Type(ast.PointerType{
						base_type: specialized
						lifetime:  expr.lifetime
					}))
				}
				ast.GenericType {
					generic_expr := ast.Expr(ast.Type(expr))
					receiver_params := receiver_generic_param_names_from_expr(generic_expr)
					if receiver_params.len == 0 {
						return none
					}
					receiver_name := t.specialized_receiver_type_name(generic_expr, bindings) or {
						return none
					}
					return ast.Expr(ast.Ident{
						name: receiver_name
						pos:  pos
					})
				}
				else {}
			}
		}
		else {}
	}

	return none
}

// type_to_ast_expr converts a types.Type back into an ast.Expr suitable for use
// in type positions (params, return types, casts). Returns a placeholder Ident
// with the type's name for primitives, plus structured nodes for compound types.
pub fn (mut t Transformer) type_to_ast_expr(typ types.Type, pos token.Pos) ast.Expr {
	match typ {
		types.Pointer {
			return ast.Expr(ast.Type(ast.PointerType{
				base_type: t.type_to_ast_expr(typ.base_type, pos)
				lifetime:  typ.lifetime
			}))
		}
		types.Array {
			return ast.Expr(ast.Type(ast.ArrayType{
				elem_type: t.type_to_ast_expr(typ.elem_type, pos)
			}))
		}
		types.ArrayFixed {
			len_pos := t.next_synth_pos()
			t.register_synth_type(len_pos, types.Type(types.int_))
			return ast.Expr(ast.Type(ast.ArrayFixedType{
				len:       ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: typ.len.str()
					pos:   len_pos
				})
				elem_type: t.type_to_ast_expr(typ.elem_type, pos)
			}))
		}
		types.Map {
			return ast.Expr(ast.Type(ast.MapType{
				key_type:   t.type_to_ast_expr(typ.key_type, pos)
				value_type: t.type_to_ast_expr(typ.value_type, pos)
			}))
		}
		types.OptionType {
			return ast.Expr(ast.Type(ast.OptionType{
				base_type: t.type_to_ast_expr(typ.base_type, pos)
			}))
		}
		types.ResultType {
			return ast.Expr(ast.Type(ast.ResultType{
				base_type: t.type_to_ast_expr(typ.base_type, pos)
			}))
		}
		types.FnType {
			param_types := typ.get_param_types()
			param_names := typ.get_param_names()
			mut params := []ast.Parameter{cap: param_types.len}
			for i, param_type in param_types {
				params << ast.Parameter{
					name: if i < param_names.len { param_names[i] } else { '' }
					typ:  t.type_to_ast_expr(param_type, pos)
					pos:  pos
				}
			}
			return ast.Expr(ast.Type(ast.FnType{
				params:      params
				return_type: if ret := typ.get_return_type() {
					t.type_to_ast_expr(ret, pos)
				} else {
					ast.empty_expr
				}
			}))
		}
		else {
			ident_pos := t.next_synth_pos()
			t.register_synth_type(ident_pos, typ)
			mut type_name := t.type_to_c_name(typ)
			if type_name == '' {
				type_name = typ.name()
			}
			return ast.Expr(ast.Ident{
				name: type_name
				pos:  ident_pos
			})
		}
	}
}

// specialized_fn_name returns the C-style specialized function name for a given
// generic FnDecl plus a concrete type bindings map, e.g. foo + {T:int} -> foo_T_int.
// Mirrors cleanc's specialized_fn_name so a future swap is name-compatible.
pub fn (t &Transformer) specialized_fn_name(decl ast.FnDecl, bindings map[string]types.Type) string {
	gp_names := decl_generic_param_names(decl)
	if gp_names.len == 0 {
		return decl.name
	}
	mut all_placeholders := true
	mut placeholder_parts := []string{cap: gp_names.len}
	mut concrete_parts := []string{cap: gp_names.len}
	for gp_name in gp_names {
		concrete := bindings[gp_name] or {
			// Missing binding: fall back to the placeholder itself so we still
			// generate a parseable name (caller is expected to skip incomplete specs).
			placeholder_parts << gp_name
			concrete_parts << gp_name
			continue
		}
		placeholder_parts << gp_name
		concrete_parts << t.generic_specialization_token_from_type(concrete)
		if concrete.name() != gp_name {
			all_placeholders = false
		}
	}
	if all_placeholders {
		return decl.name + '_' + placeholder_parts.join('_')
	}
	return decl.name + '_T_' + concrete_parts.join('_')
}

// decl_generic_param_names extracts the runtime (non-@'lifetime) generic
// parameter names from a FnDecl in declaration order.
pub fn decl_generic_param_names(decl ast.FnDecl) []string {
	mut names := []string{cap: decl.typ.generic_params.len}
	for gp in decl.typ.generic_params {
		if gp is ast.Ident {
			append_unique_generic_param_name(mut names, gp.name)
		}
	}
	if decl.is_method {
		for name in receiver_generic_param_names(decl) {
			append_unique_generic_param_name(mut names, name)
		}
	}
	return names
}

fn append_unique_generic_param_name(mut names []string, name string) {
	if name == '' || name.starts_with('^') || name in names {
		return
	}
	names << name
}

fn receiver_generic_param_names(decl ast.FnDecl) []string {
	if !decl.is_method {
		return []string{}
	}
	return receiver_generic_param_names_from_expr(decl.receiver.typ)
}

fn receiver_generic_param_names_from_expr(expr ast.Expr) []string {
	mut seen := map[string]bool{}
	mut names := []string{}
	collect_receiver_generic_param_names(expr, mut seen, mut names)
	return names
}

fn collect_receiver_generic_param_names(expr ast.Expr, mut seen map[string]bool, mut names []string) {
	match expr {
		ast.GenericArgOrIndexExpr {
			collect_receiver_generic_arg_names(expr.expr, mut seen, mut names)
		}
		ast.GenericArgs {
			for arg in expr.args {
				collect_receiver_generic_arg_names(arg, mut seen, mut names)
			}
		}
		ast.Ident {
			return
		}
		ast.ModifierExpr {
			collect_receiver_generic_param_names(expr.expr, mut seen, mut names)
		}
		ast.PrefixExpr {
			collect_receiver_generic_param_names(expr.expr, mut seen, mut names)
		}
		ast.Type {
			match expr {
				ast.GenericType {
					for param in expr.params {
						collect_receiver_generic_arg_names(param, mut seen, mut names)
					}
				}
				ast.PointerType {
					collect_receiver_generic_param_names(expr.base_type, mut seen, mut names)
				}
				else {}
			}
		}
		else {}
	}
}

fn collect_receiver_generic_arg_names(expr ast.Expr, mut seen map[string]bool, mut names []string) {
	match expr {
		ast.Ident {
			if is_generic_placeholder_ident(expr.name) && expr.name !in seen {
				seen[expr.name] = true
				names << expr.name
			}
		}
		ast.GenericArgOrIndexExpr {
			collect_receiver_generic_arg_names(expr.expr, mut seen, mut names)
		}
		ast.GenericArgs {
			for arg in expr.args {
				collect_receiver_generic_arg_names(arg, mut seen, mut names)
			}
		}
		ast.ModifierExpr {
			collect_receiver_generic_arg_names(expr.expr, mut seen, mut names)
		}
		ast.PrefixExpr {
			collect_receiver_generic_arg_names(expr.expr, mut seen, mut names)
		}
		ast.Type {
			match expr {
				ast.ArrayType {
					collect_receiver_generic_arg_names(expr.elem_type, mut seen, mut names)
				}
				ast.MapType {
					collect_receiver_generic_arg_names(expr.key_type, mut seen, mut names)
					collect_receiver_generic_arg_names(expr.value_type, mut seen, mut names)
				}
				ast.PointerType {
					collect_receiver_generic_arg_names(expr.base_type, mut seen, mut names)
				}
				ast.GenericType {
					for param in expr.params {
						collect_receiver_generic_arg_names(param, mut seen, mut names)
					}
				}
				else {}
			}
		}
		else {}
	}
}

fn is_generic_placeholder_ident(name string) bool {
	return name.len > 0 && name[0] >= `A` && name[0] <= `Z`
}

// clone_fn_decl_with_substitutions returns a deep clone of decl with:
//   - name replaced by new_name
//   - generic_params cleared (the clone is concrete)
//   - all type expressions in params + return + body substituted via bindings
//   - the body deep-cloned so the clone is structurally independent
//
// Sitting 1 scope: covers the common subset of stmt/expr variants. Unknown
// variants are returned shallow-copied (no substitution recurses into them).
// Sitting 2 extends coverage as real generic functions exercise more nodes.
pub fn (mut t Transformer) clone_fn_decl_with_substitutions(decl ast.FnDecl, bindings map[string]types.Type, new_name string, source_module string, target_module string) ast.FnDecl {
	old_module := t.cur_module
	old_scope := t.scope
	old_fn_root_scope := t.fn_root_scope
	scope_parent := t.get_module_scope(source_module) or { unsafe { nil } }
	clone_scope := types.new_scope(scope_parent)
	t.cur_module = source_module
	t.scope = clone_scope
	t.fn_root_scope = clone_scope
	mut new_params := []ast.Parameter{cap: decl.typ.params.len}
	for p in decl.typ.params {
		new_params << ast.Parameter{
			name:   p.name
			typ:    t.substitute_type_in_expr(p.typ, bindings)
			is_mut: p.is_mut
			pos:    p.pos
		}
	}
	new_return := t.substitute_type_in_expr(decl.typ.return_type, bindings)
	new_typ := ast.FnType{
		generic_params: []ast.Expr{}
		params:         new_params
		return_type:    new_return
	}
	mut old_local_decl_types := t.local_decl_types.move()
	t.local_decl_types = map[string]types.Type{}
	for p in new_params {
		if p.name == '' || p.name == '_' {
			continue
		}
		if param_type := t.type_from_param_type_expr(p.typ, []) {
			t.remember_local_decl_type(p.name, param_type)
			t.register_local_var_type(p.name, param_type)
		}
	}
	if decl.is_method && decl.receiver.name != '' && decl.receiver.name != '_' {
		receiver_typ := if receiver_generic_param_names(decl).len > 0 {
			t.specialized_receiver_type_expr(decl.receiver.typ, bindings, decl.receiver.pos) or {
				t.substitute_type_in_expr(decl.receiver.typ, bindings)
			}
		} else {
			t.substitute_type_in_expr(decl.receiver.typ, bindings)
		}
		if recv_type := t.type_from_param_type_expr(receiver_typ, []) {
			t.remember_local_decl_type(decl.receiver.name, recv_type)
			t.register_local_var_type(decl.receiver.name, recv_type)
		}
	}
	mut new_stmts := []ast.Stmt{cap: decl.stmts.len}
	for st in decl.stmts {
		cloned := t.clone_stmt_with_bindings_and_fields(st, bindings, []CloneComptimeFieldCtx{},
			false)
		t.remember_cloned_stmt_decl_types(cloned)
		new_stmts << cloned
	}
	t.local_decl_types = old_local_decl_types.move()
	new_stmts = t.fold_known_bool_stmts(new_stmts)
	new_receiver := ast.Parameter{
		name:   decl.receiver.name
		typ:    if decl.is_method && receiver_generic_param_names(decl).len > 0 {
			t.specialized_receiver_type_expr(decl.receiver.typ, bindings, decl.receiver.pos) or {
				t.substitute_type_in_expr(decl.receiver.typ, bindings)
			}
		} else {
			t.substitute_type_in_expr(decl.receiver.typ, bindings)
		}
		is_mut: decl.receiver.is_mut
		pos:    decl.receiver.pos
	}
	cloned := ast.FnDecl{
		attributes: decl.attributes
		is_public:  decl.is_public
		is_method:  decl.is_method
		is_static:  decl.is_static
		receiver:   new_receiver
		language:   decl.language
		name:       new_name
		typ:        new_typ
		stmts:      new_stmts
		pos:        decl.pos
	}
	t.cache_monomorphized_clone_scope(source_module, target_module, cloned, clone_scope)
	t.cur_module = old_module
	t.scope = old_scope
	t.fn_root_scope = old_fn_root_scope
	return cloned
}

struct GenericIndexCallParts {
	lhs  ast.Expr
	args []ast.Expr
}

fn generic_index_call_parts(expr ast.IndexExpr) GenericIndexCallParts {
	mut lhs := expr.lhs
	mut args := []ast.Expr{}
	match expr.lhs {
		ast.GenericArgs {
			lhs = expr.lhs.lhs
			args << expr.lhs.args
		}
		ast.GenericArgOrIndexExpr {
			lhs = expr.lhs.lhs
			args << expr.lhs.expr
		}
		ast.IndexExpr {
			nested := generic_index_call_parts(expr.lhs)
			lhs = nested.lhs
			args << nested.args
		}
		else {}
	}

	args << generic_type_args_from_index_expr(expr.expr)
	return GenericIndexCallParts{
		lhs:  lhs
		args: args
	}
}

fn generic_type_args_from_index_expr(expr ast.Expr) []ast.Expr {
	if expr is ast.EmptyExpr {
		return []ast.Expr{}
	}
	if expr is ast.Tuple {
		return expr.exprs.clone()
	}
	return [expr]
}

fn (mut t Transformer) remember_cloned_stmt_decl_types(stmt ast.Stmt) {
	match stmt {
		ast.AssignStmt {
			t.remember_cloned_assign_decl_types(stmt)
		}
		ast.BlockStmt {
			for child in stmt.stmts {
				t.remember_cloned_stmt_decl_types(child)
			}
		}
		ast.ComptimeStmt {
			t.remember_cloned_stmt_decl_types(stmt.stmt)
		}
		ast.DeferStmt {
			for child in stmt.stmts {
				t.remember_cloned_stmt_decl_types(child)
			}
		}
		ast.ExprStmt {
			if stmt.expr is ast.IfExpr {
				t.remember_cloned_if_expr_decl_types(stmt.expr as ast.IfExpr)
			} else if stmt.expr is ast.UnsafeExpr {
				unsafe_expr := stmt.expr as ast.UnsafeExpr
				for child in unsafe_expr.stmts {
					t.remember_cloned_stmt_decl_types(child)
				}
			}
		}
		ast.ForStmt {
			t.remember_cloned_stmt_decl_types(stmt.init)
			for child in stmt.stmts {
				t.remember_cloned_stmt_decl_types(child)
			}
			t.remember_cloned_stmt_decl_types(stmt.post)
		}
		ast.LabelStmt {
			t.remember_cloned_stmt_decl_types(stmt.stmt)
		}
		else {}
	}
}

fn (mut t Transformer) remember_cloned_if_expr_decl_types(expr ast.IfExpr) {
	mut body_smartcasts := []SmartcastContext{}
	mut seen_smartcasts := map[string]bool{}
	for term in t.flatten_and_terms_unwrapped(expr.cond) {
		if term is ast.InfixExpr {
			if ctx := t.smartcast_context_from_condition_term(term) {
				key := '${ctx.expr}|${ctx.variant}|${ctx.variant_full}|${ctx.sumtype}'
				if key !in seen_smartcasts {
					seen_smartcasts[key] = true
					body_smartcasts << ctx
				}
			}
		}
	}
	stack_before := t.smartcast_stack.clone()
	counts_before := t.smartcast_expr_counts.clone()
	for ctx in body_smartcasts {
		t.push_smartcast_ctx(ctx)
	}
	for child in expr.stmts {
		t.remember_cloned_stmt_decl_types(child)
	}
	t.smartcast_stack = stack_before.clone()
	t.smartcast_expr_counts = counts_before.clone()
	if expr.else_expr is ast.IfExpr {
		t.remember_cloned_if_expr_decl_types(expr.else_expr as ast.IfExpr)
	} else if expr.else_expr is ast.UnsafeExpr {
		unsafe_expr := expr.else_expr as ast.UnsafeExpr
		for child in unsafe_expr.stmts {
			t.remember_cloned_stmt_decl_types(child)
		}
	}
	t.smartcast_stack = stack_before.clone()
	t.smartcast_expr_counts = counts_before.clone()
}

fn (mut t Transformer) remember_cloned_assign_decl_types(stmt ast.AssignStmt) {
	if stmt.op != .decl_assign || stmt.lhs.len != 1 || stmt.rhs.len != 1 {
		return
	}
	lhs_name := t.get_var_name(stmt.lhs[0])
	if lhs_name == '' || lhs_name == '_' {
		return
	}
	rhs := stmt.rhs[0]
	if decl_type := t.decl_assign_storage_type(stmt.lhs[0], rhs) {
		t.remember_decl_assign_lhs_type(stmt.lhs, decl_type)
		return
	}
	if typ := t.get_expr_type(rhs) {
		t.remember_decl_assign_lhs_type(stmt.lhs, typ)
	}
}

fn (mut t Transformer) cache_monomorphized_clone_scope(source_module string, target_module string, cloned ast.FnDecl, scope &types.Scope) {
	mut scope_names := []string{cap: 2}
	if cloned.is_method {
		mut recv_name := t.get_receiver_type_name(cloned.receiver.typ)
		if source_module != '' {
			prefix := '${source_module}__'
			if recv_name.starts_with(prefix) {
				recv_name = recv_name[prefix.len..]
			}
		}
		if recv_name != '' {
			scope_names << '${recv_name}__${cloned.name}'
		}
	}
	scope_names << cloned.name
	derived_module := monomorphized_name_module_prefix(cloned.name)
	for scope_name in scope_names {
		t.cache_fn_scope_for_module('', scope_name, scope)
		if source_module != '' {
			t.cache_fn_scope_for_module(source_module, scope_name, scope)
		}
		if target_module != '' && target_module != source_module {
			t.cache_fn_scope_for_module(target_module, scope_name, scope)
		}
		if derived_module != '' && derived_module != source_module {
			t.cache_fn_scope_for_module(derived_module, scope_name, scope)
		}
	}
}

fn (mut t Transformer) cache_fn_scope_for_module(module_name string, scope_name string, scope &types.Scope) {
	if scope_name == '' {
		return
	}
	cache_key := if module_name == '' { scope_name } else { '${module_name}__${scope_name}' }
	t.cached_fn_scopes[cache_key] = scope
	t.env.set_fn_scope(module_name, scope_name, scope)
}

fn monomorphized_name_module_prefix(name string) string {
	if !name.contains('__') {
		return ''
	}
	prefix := name.all_before('__')
	if prefix == '' || prefix[0] < `a` || prefix[0] > `z` {
		return ''
	}
	return prefix
}

// clone_stmt_with_bindings deep-clones a stmt, substituting generic types in
// any embedded type expressions and propagating into nested stmts/exprs.
pub fn (mut t Transformer) clone_stmt_with_bindings(stmt ast.Stmt, bindings map[string]types.Type) ast.Stmt {
	match stmt {
		ast.ExprStmt {
			if reduced := t.clone_comptime_expr_stmt_with_bindings(stmt, bindings) {
				return reduced
			}
			return ast.Stmt(ast.ExprStmt{
				expr: t.clone_expr_with_bindings(stmt.expr, bindings)
			})
		}
		ast.ReturnStmt {
			mut new_exprs := []ast.Expr{cap: stmt.exprs.len}
			for e in stmt.exprs {
				new_exprs << t.clone_expr_with_bindings(e, bindings)
			}
			return ast.Stmt(ast.ReturnStmt{
				exprs: new_exprs
			})
		}
		ast.AssignStmt {
			mut new_lhs := []ast.Expr{cap: stmt.lhs.len}
			for e in stmt.lhs {
				new_lhs << t.clone_expr_with_bindings(e, bindings)
			}
			mut new_rhs := []ast.Expr{cap: stmt.rhs.len}
			for e in stmt.rhs {
				new_rhs << t.clone_expr_with_bindings(e, bindings)
			}
			return ast.Stmt(ast.AssignStmt{
				op:  stmt.op
				lhs: new_lhs
				rhs: new_rhs
				pos: stmt.pos
			})
		}
		ast.BlockStmt {
			mut new_inner := []ast.Stmt{cap: stmt.stmts.len}
			for s in stmt.stmts {
				new_inner << t.clone_stmt_with_bindings(s, bindings)
			}
			return ast.Stmt(ast.BlockStmt{
				stmts: new_inner
			})
		}
		ast.ForStmt {
			mut new_inner := []ast.Stmt{cap: stmt.stmts.len}
			for s in stmt.stmts {
				new_inner << t.clone_stmt_with_bindings(s, bindings)
			}
			return ast.Stmt(ast.ForStmt{
				init:  t.clone_stmt_with_bindings(stmt.init, bindings)
				cond:  t.clone_expr_with_bindings(stmt.cond, bindings)
				post:  t.clone_stmt_with_bindings(stmt.post, bindings)
				stmts: new_inner
			})
		}
		ast.ForInStmt {
			return ast.Stmt(ast.ForInStmt{
				key:   t.clone_expr_with_bindings(stmt.key, bindings)
				value: t.clone_expr_with_bindings(stmt.value, bindings)
				expr:  t.clone_expr_with_bindings(stmt.expr, bindings)
			})
		}
		ast.DeferStmt {
			mut new_inner := []ast.Stmt{cap: stmt.stmts.len}
			for s in stmt.stmts {
				new_inner << t.clone_stmt_with_bindings(s, bindings)
			}
			return ast.Stmt(ast.DeferStmt{
				mode:  stmt.mode
				stmts: new_inner
			})
		}
		ast.LabelStmt {
			return ast.Stmt(ast.LabelStmt{
				name: stmt.name
				stmt: t.clone_stmt_with_bindings(stmt.stmt, bindings)
			})
		}
		ast.AssertStmt {
			return ast.Stmt(ast.AssertStmt{
				expr:  t.clone_expr_with_bindings(stmt.expr, bindings)
				extra: t.clone_expr_with_bindings(stmt.extra, bindings)
			})
		}
		ast.ComptimeStmt {
			if reduced := t.clone_comptime_stmt_with_bindings(stmt.stmt, bindings) {
				return reduced
			}
			return ast.Stmt(ast.ComptimeStmt{
				stmt: t.clone_stmt_with_bindings(stmt.stmt, bindings)
			})
		}
		else {
			return stmt
		}
	}
}

fn (mut t Transformer) clone_comptime_stmt_with_bindings(stmt ast.Stmt, bindings map[string]types.Type) ?ast.Stmt {
	if stmt is ast.ForStmt {
		return t.clone_comptime_field_for_with_contexts(stmt, bindings, []CloneComptimeFieldCtx{})
	}
	if stmt is ast.ExprStmt {
		return t.clone_comptime_expr_stmt_with_bindings(stmt, bindings)
	}
	return none
}

fn (mut t Transformer) clone_comptime_field_for_with_contexts(stmt ast.ForStmt, bindings map[string]types.Type, parent_contexts []CloneComptimeFieldCtx) ?ast.Stmt {
	if stmt.init !is ast.ForInStmt {
		return none
	}
	for_in := stmt.init as ast.ForInStmt
	if for_in.expr !is ast.SelectorExpr {
		return none
	}
	sel := for_in.expr as ast.SelectorExpr
	if sel.rhs.name != 'fields' {
		return none
	}
	struct_type := t.clone_comptime_selector_struct_type(sel.lhs, bindings) or { return none }
	field_var := for_in.value.name()
	if field_var == '' {
		return none
	}
	t.temp_counter++
	break_label := '__v_ctf_break_${t.temp_counter}'
	mut lowered := []ast.Stmt{cap: struct_type.fields.len + 1}
	for i, field in struct_type.fields {
		t.temp_counter++
		continue_label := '__v_ctf_continue_${t.temp_counter}_${i}'
		ctx := CloneComptimeFieldCtx{
			var_name:       field_var
			field:          field
			field_idx:      i
			struct_type:    struct_type
			attrs:          t.clone_comptime_field_attribute_strings(struct_type.name, field)
			is_embed:       t.clone_comptime_field_is_embedded(struct_type, field)
			continue_label: continue_label
			break_label:    break_label
		}
		mut field_stmts := []ast.Stmt{cap: stmt.stmts.len + 1}
		mut contexts := parent_contexts.clone()
		contexts << ctx
		for body_stmt in stmt.stmts {
			field_stmts << t.clone_stmt_with_bindings_and_fields(body_stmt, bindings, contexts,
				false)
		}
		field_stmts << ast.Stmt(ast.LabelStmt{
			name: continue_label
			stmt: ast.empty_stmt
		})
		lowered << ast.Stmt(ast.ExprStmt{
			expr: ast.Expr(ast.UnsafeExpr{
				stmts: field_stmts
				pos:   sel.pos
			})
		})
	}
	lowered << ast.Stmt(ast.LabelStmt{
		name: break_label
		stmt: ast.empty_stmt
	})
	return ast.Stmt(ast.BlockStmt{
		stmts: lowered
	})
}

fn (t &Transformer) clone_comptime_selector_struct_type(expr ast.Expr, bindings map[string]types.Type) ?types.Struct {
	concrete := t.comptime_lhs_bound_type(expr, bindings) or {
		t.get_synth_type(expr.pos()) or {
			t.lookup_type_from_expr(expr) or { t.get_expr_type(expr) or { return none } }
		}
	}
	base := t.unwrap_alias_and_pointer_type(concrete)
	if base is types.Struct {
		return t.clone_comptime_full_struct_type(base)
	}
	return none
}

fn (t &Transformer) clone_comptime_full_struct_type(st types.Struct) types.Struct {
	if st.fields.len > 0 && !clone_type_contains_generic_placeholder(types.Type(st)) {
		return st
	}
	if st.name != '' {
		if live := t.lookup_struct_type_any_module(st.name) {
			if live.fields.len > 0 && !clone_type_contains_generic_placeholder(types.Type(live)) {
				return live
			}
		}
	}
	return st
}

fn (t &Transformer) clone_comptime_field_is_embedded(struct_type types.Struct, field types.Field) bool {
	for embedded in struct_type.embedded {
		embedded_name := embedded.name.all_after_last('__')
		if field.name == embedded.name || field.name == embedded_name {
			return true
		}
	}
	return false
}

fn (t &Transformer) clone_comptime_field_attribute_strings(struct_name string, field types.Field) []string {
	_ = struct_name
	return clone_comptime_attribute_strings(field.attributes)
}

fn clone_comptime_attribute_strings(attrs []ast.Attribute) []string {
	mut out := []string{cap: attrs.len}
	for attr in attrs {
		if attr.name != '' {
			if attr.value is ast.EmptyExpr {
				out << attr.name
			} else {
				out << '${attr.name}: ${attr.value.name().trim("'")}'
			}
		} else if attr.value !is ast.EmptyExpr {
			out << attr.value.name().trim("'")
		}
	}
	return out
}

fn clone_type_contains_generic_placeholder(typ types.Type) bool {
	mut seen := map[string]bool{}
	return clone_type_contains_generic_placeholder_with_seen(typ, mut seen)
}

fn clone_type_contains_generic_placeholder_with_seen(typ types.Type, mut seen map[string]bool) bool {
	if !types.type_has_valid_payload(typ) {
		return false
	}
	match typ {
		types.NamedType {
			return true
		}
		types.Array {
			return clone_type_child_contains_generic_placeholder(typ.elem_type, mut seen)
		}
		types.ArrayFixed {
			return clone_type_child_contains_generic_placeholder(typ.elem_type, mut seen)
		}
		types.Map {
			return clone_type_child_contains_generic_placeholder(typ.key_type, mut seen)
				|| clone_type_child_contains_generic_placeholder(typ.value_type, mut seen)
		}
		types.Pointer {
			return clone_type_child_contains_generic_placeholder(typ.base_type, mut seen)
		}
		types.OptionType {
			return clone_type_child_contains_generic_placeholder(typ.base_type, mut seen)
		}
		types.ResultType {
			return clone_type_child_contains_generic_placeholder(typ.base_type, mut seen)
		}
		types.Alias {
			name := typ.name
			if name != '' {
				if seen[name] {
					return false
				}
				seen[name] = true
			}
			return clone_type_child_contains_generic_placeholder(typ.base_type, mut seen)
		}
		types.Struct {
			name := typ.name
			if name != '' {
				if seen[name] {
					return false
				}
				seen[name] = true
			}
			for field in typ.fields {
				if clone_type_child_contains_generic_placeholder(field.typ, mut seen) {
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

fn clone_type_child_contains_generic_placeholder(typ types.Type, mut seen map[string]bool) bool {
	if !clone_type_has_safe_payload(typ) {
		return false
	}
	return clone_type_contains_generic_placeholder_with_seen(typ, mut seen)
}

fn transformer_type_tag_has_inline_payload(tag u64) bool {
	return tag == 4 || tag == 7 || tag == 11 || tag == 12 || tag == 15 || tag == 17 || tag == 18
		|| tag == 23 || tag == 24
}

fn transformer_type_word_is_payload(word u64) bool {
	return word >= 4096 && word < 0x0000800000000000
}

fn clone_type_word_is_payload(word u64) bool {
	return word >= 0x100000000 && word < 0x0000800000000000
}

fn transformer_type_has_safe_payload(typ types.Type) bool {
	if !types.type_has_valid_payload(typ) {
		return false
	}
	word0 := unsafe { *(&u64(&typ)) }
	word1 := unsafe { *(&u64(&u8(&typ) + 8)) }
	if word0 < 256 {
		if transformer_type_tag_has_inline_payload(word0) {
			return true
		}
		return transformer_type_word_is_payload(word1)
	}
	return transformer_type_word_is_payload(word0) || transformer_type_word_is_payload(word1)
}

fn clone_type_has_safe_payload(typ types.Type) bool {
	if !types.type_has_valid_payload(typ) {
		return false
	}
	word0 := unsafe { *(&u64(&typ)) }
	word1 := unsafe { *(&u64(&u8(&typ) + 8)) }
	if word0 < 256 {
		if transformer_type_tag_has_inline_payload(word0) {
			return true
		}
		return clone_type_word_is_payload(word1)
	}
	return clone_type_word_is_payload(word0) || clone_type_word_is_payload(word1)
}

fn (mut t Transformer) fold_known_bool_stmts(stmts []ast.Stmt) []ast.Stmt {
	mut known := map[string]bool{}
	return t.fold_known_bool_stmt_list(stmts, mut known)
}

fn (mut t Transformer) fold_known_bool_stmt_list(stmts []ast.Stmt, mut known map[string]bool) []ast.Stmt {
	mut out := []ast.Stmt{cap: stmts.len}
	for stmt in stmts {
		folded := t.fold_known_bool_stmt(stmt, mut known)
		if folded is ast.BlockStmt {
			for inner in folded.stmts {
				out << inner
			}
		} else {
			out << folded
		}
	}
	return out
}

fn (mut t Transformer) fold_known_bool_stmt(stmt ast.Stmt, mut known map[string]bool) ast.Stmt {
	match stmt {
		ast.AssignStmt {
			t.update_known_bool_from_assign(stmt, mut known)
			return stmt
		}
		ast.ExprStmt {
			if stmt.expr is ast.IfExpr {
				if selected := t.fold_known_bool_if_expr(stmt.expr as ast.IfExpr, mut known) {
					return selected
				}
				return ast.Stmt(ast.ExprStmt{
					expr: ast.Expr(t.fold_known_bool_if_expr_branches(stmt.expr as ast.IfExpr,
						known))
				})
			}
			if stmt.expr is ast.UnsafeExpr {
				unsafe_expr := stmt.expr as ast.UnsafeExpr
				return ast.Stmt(ast.ExprStmt{
					expr: ast.Expr(ast.UnsafeExpr{
						stmts: t.fold_known_bool_stmt_list(unsafe_expr.stmts, mut known)
						pos:   unsafe_expr.pos
					})
				})
			}
			return stmt
		}
		ast.BlockStmt {
			return ast.Stmt(ast.BlockStmt{
				stmts: t.fold_known_bool_stmt_list(stmt.stmts, mut known)
			})
		}
		ast.DeferStmt {
			mut scoped := known.clone()
			return ast.Stmt(ast.DeferStmt{
				mode:  stmt.mode
				stmts: t.fold_known_bool_stmt_list(stmt.stmts, mut scoped)
			})
		}
		else {
			return stmt
		}
	}
}

fn (mut t Transformer) fold_known_bool_if_expr_branches(expr ast.IfExpr, known map[string]bool) ast.IfExpr {
	mut branch_known := known.clone()
	mut new_stmts := t.fold_known_bool_stmt_list(expr.stmts, mut branch_known)
	mut new_else := expr.else_expr
	if expr.else_expr is ast.IfExpr {
		new_else = ast.Expr(t.fold_known_bool_if_expr_branches(expr.else_expr as ast.IfExpr, known))
	}
	return ast.IfExpr{
		cond:      expr.cond
		stmts:     new_stmts
		else_expr: new_else
		pos:       expr.pos
	}
}

fn (mut t Transformer) fold_known_bool_if_expr(expr ast.IfExpr, mut known map[string]bool) ?ast.Stmt {
	cond := known_bool_expr(expr.cond, known) or { return none }
	if cond {
		return ast.Stmt(ast.BlockStmt{
			stmts: t.fold_known_bool_stmt_list(expr.stmts, mut known)
		})
	}
	if expr.else_expr is ast.IfExpr {
		else_if := expr.else_expr as ast.IfExpr
		if else_if.cond is ast.EmptyExpr {
			return ast.Stmt(ast.BlockStmt{
				stmts: t.fold_known_bool_stmt_list(else_if.stmts, mut known)
			})
		}
		return t.fold_known_bool_if_expr(else_if, mut known)
	}
	if expr.else_expr is ast.EmptyExpr {
		return ast.Stmt(ast.BlockStmt{})
	}
	return ast.Stmt(ast.ExprStmt{
		expr: expr.else_expr
	})
}

fn (mut t Transformer) update_known_bool_from_assign(stmt ast.AssignStmt, mut known map[string]bool) {
	if stmt.lhs.len != 1 || stmt.rhs.len != 1 {
		return
	}
	ident_name := known_bool_assign_lhs_name(stmt.lhs[0])
	if ident_name == '' {
		return
	}
	if value := known_bool_expr(stmt.rhs[0], known) {
		known[ident_name] = value
		return
	}
	if ident_name in known {
		known.delete(ident_name)
	}
}

fn known_bool_assign_lhs_name(expr ast.Expr) string {
	match expr {
		ast.Ident {
			return expr.name
		}
		ast.ModifierExpr {
			return known_bool_assign_lhs_name(expr.expr)
		}
		else {
			return ''
		}
	}
}

fn known_bool_expr(expr ast.Expr, known map[string]bool) ?bool {
	match expr {
		ast.Ident {
			if expr.name == 'true' {
				return true
			}
			if expr.name == 'false' {
				return false
			}
			return known[expr.name] or { none }
		}
		ast.BasicLiteral {
			if expr.kind == .key_true {
				return true
			}
			if expr.kind == .key_false {
				return false
			}
		}
		ast.Keyword {
			if expr.tok == .key_true {
				return true
			}
			if expr.tok == .key_false {
				return false
			}
		}
		ast.ParenExpr {
			return known_bool_expr(expr.expr, known)
		}
		ast.ModifierExpr {
			return known_bool_expr(expr.expr, known)
		}
		ast.PrefixExpr {
			if expr.op == .not {
				value := known_bool_expr(expr.expr, known) or { return none }
				return !value
			}
		}
		else {}
	}

	return none
}

fn (mut t Transformer) clone_comptime_expr_stmt_with_bindings(stmt ast.ExprStmt, bindings map[string]types.Type) ?ast.Stmt {
	if stmt.expr is ast.ComptimeExpr {
		inner := stmt.expr as ast.ComptimeExpr
		if inner.expr is ast.IfExpr {
			return t.clone_comptime_if_with_bindings(inner.expr as ast.IfExpr, bindings)
		}
	}
	return none
}

fn (mut t Transformer) clone_comptime_if_with_bindings(expr ast.IfExpr, bindings map[string]types.Type) ?ast.Stmt {
	cond_result := t.eval_clone_comptime_cond(expr.cond, bindings) or { return none }
	if cond_result {
		mut selected := []ast.Stmt{cap: expr.stmts.len}
		for st in expr.stmts {
			selected << t.clone_stmt_with_bindings(st, bindings)
		}
		return ast.Stmt(ast.BlockStmt{
			stmts: selected
		})
	}
	if expr.else_expr is ast.IfExpr {
		else_if := expr.else_expr as ast.IfExpr
		if else_if.cond is ast.EmptyExpr {
			mut selected := []ast.Stmt{cap: else_if.stmts.len}
			for st in else_if.stmts {
				selected << t.clone_stmt_with_bindings(st, bindings)
			}
			return ast.Stmt(ast.BlockStmt{
				stmts: selected
			})
		}
		return t.clone_comptime_if_with_bindings(else_if, bindings)
	}
	if expr.else_expr is ast.EmptyExpr {
		return ast.Stmt(ast.BlockStmt{})
	}
	return ast.Stmt(ast.ExprStmt{
		expr: t.clone_expr_with_bindings(expr.else_expr, bindings)
	})
}

fn (mut t Transformer) clone_stmt_with_bindings_and_fields(stmt ast.Stmt, bindings map[string]types.Type, contexts []CloneComptimeFieldCtx, inside_runtime_loop bool) ast.Stmt {
	match stmt {
		ast.ExprStmt {
			if reduced := t.clone_comptime_expr_stmt_with_bindings_and_fields(stmt, bindings,
				contexts, inside_runtime_loop)
			{
				return reduced
			}
			return ast.Stmt(ast.ExprStmt{
				expr: t.clone_expr_with_bindings_and_fields(stmt.expr, bindings, contexts)
			})
		}
		ast.ReturnStmt {
			mut new_exprs := []ast.Expr{cap: stmt.exprs.len}
			for e in stmt.exprs {
				new_exprs << t.clone_expr_with_bindings_and_fields(e, bindings, contexts)
			}
			return ast.Stmt(ast.ReturnStmt{
				exprs: new_exprs
			})
		}
		ast.AssignStmt {
			mut new_lhs := []ast.Expr{cap: stmt.lhs.len}
			for e in stmt.lhs {
				new_lhs << t.clone_expr_with_bindings_and_fields(e, bindings, contexts)
			}
			mut new_rhs := []ast.Expr{cap: stmt.rhs.len}
			for e in stmt.rhs {
				new_rhs << t.clone_expr_with_bindings_and_fields(e, bindings, contexts)
			}
			return ast.Stmt(ast.AssignStmt{
				op:  stmt.op
				lhs: new_lhs
				rhs: new_rhs
				pos: stmt.pos
			})
		}
		ast.BlockStmt {
			mut new_inner := []ast.Stmt{cap: stmt.stmts.len}
			for s in stmt.stmts {
				new_inner << t.clone_stmt_with_bindings_and_fields(s, bindings, contexts,
					inside_runtime_loop)
			}
			return ast.Stmt(ast.BlockStmt{
				stmts: new_inner
			})
		}
		ast.ForStmt {
			mut new_inner := []ast.Stmt{cap: stmt.stmts.len}
			for s in stmt.stmts {
				new_inner << t.clone_stmt_with_bindings_and_fields(s, bindings, contexts, true)
			}
			return ast.Stmt(ast.ForStmt{
				init:  t.clone_stmt_with_bindings_and_fields(stmt.init, bindings, contexts, true)
				cond:  t.clone_expr_with_bindings_and_fields(stmt.cond, bindings, contexts)
				post:  t.clone_stmt_with_bindings_and_fields(stmt.post, bindings, contexts, true)
				stmts: new_inner
			})
		}
		ast.ForInStmt {
			return ast.Stmt(ast.ForInStmt{
				key:   t.clone_expr_with_bindings_and_fields(stmt.key, bindings, contexts)
				value: t.clone_expr_with_bindings_and_fields(stmt.value, bindings, contexts)
				expr:  t.clone_expr_with_bindings_and_fields(stmt.expr, bindings, contexts)
			})
		}
		ast.DeferStmt {
			mut new_inner := []ast.Stmt{cap: stmt.stmts.len}
			for s in stmt.stmts {
				new_inner << t.clone_stmt_with_bindings_and_fields(s, bindings, contexts,
					inside_runtime_loop)
			}
			return ast.Stmt(ast.DeferStmt{
				mode:  stmt.mode
				stmts: new_inner
			})
		}
		ast.LabelStmt {
			return ast.Stmt(ast.LabelStmt{
				name: stmt.name
				stmt: t.clone_stmt_with_bindings_and_fields(stmt.stmt, bindings, contexts,
					inside_runtime_loop)
			})
		}
		ast.AssertStmt {
			return ast.Stmt(ast.AssertStmt{
				expr:  t.clone_expr_with_bindings_and_fields(stmt.expr, bindings, contexts)
				extra: t.clone_expr_with_bindings_and_fields(stmt.extra, bindings, contexts)
			})
		}
		ast.ComptimeStmt {
			if stmt.stmt is ast.ForStmt {
				if reduced := t.clone_comptime_field_for_with_contexts(stmt.stmt as ast.ForStmt,
					bindings, contexts)
				{
					return reduced
				}
			}
			if stmt.stmt is ast.ExprStmt {
				if reduced := t.clone_comptime_expr_stmt_with_bindings_and_fields(stmt.stmt as ast.ExprStmt,
					bindings, contexts, inside_runtime_loop)
				{
					return reduced
				}
			}
			return ast.Stmt(ast.ComptimeStmt{
				stmt: t.clone_stmt_with_bindings_and_fields(stmt.stmt, bindings, contexts,
					inside_runtime_loop)
			})
		}
		ast.FlowControlStmt {
			if !inside_runtime_loop && contexts.len > 0 {
				ctx := contexts[contexts.len - 1]
				if stmt.op == .key_continue && ctx.continue_label != '' {
					return ast.Stmt(ast.FlowControlStmt{
						op:    .key_goto
						label: ctx.continue_label
					})
				}
				if stmt.op == .key_break && ctx.break_label != '' {
					return ast.Stmt(ast.FlowControlStmt{
						op:    .key_goto
						label: ctx.break_label
					})
				}
			}
			return stmt
		}
		else {
			return stmt
		}
	}
}

fn (mut t Transformer) clone_comptime_expr_stmt_with_bindings_and_fields(stmt ast.ExprStmt, bindings map[string]types.Type, contexts []CloneComptimeFieldCtx, inside_runtime_loop bool) ?ast.Stmt {
	if stmt.expr is ast.ComptimeExpr {
		inner := stmt.expr as ast.ComptimeExpr
		if inner.expr is ast.IfExpr {
			return t.clone_comptime_if_with_bindings_and_fields(inner.expr as ast.IfExpr, bindings,
				contexts, inside_runtime_loop)
		}
	}
	return none
}

fn (mut t Transformer) clone_comptime_if_with_bindings_and_fields(expr ast.IfExpr, bindings map[string]types.Type, contexts []CloneComptimeFieldCtx, inside_runtime_loop bool) ?ast.Stmt {
	cond_result := t.eval_clone_comptime_cond_with_fields(expr.cond, bindings, contexts) or {
		return none
	}
	if cond_result {
		mut selected := []ast.Stmt{cap: expr.stmts.len}
		for st in expr.stmts {
			selected << t.clone_stmt_with_bindings_and_fields(st, bindings, contexts,
				inside_runtime_loop)
		}
		return ast.Stmt(ast.BlockStmt{
			stmts: selected
		})
	}
	if expr.else_expr is ast.IfExpr {
		else_if := expr.else_expr as ast.IfExpr
		if else_if.cond is ast.EmptyExpr {
			mut selected := []ast.Stmt{cap: else_if.stmts.len}
			for st in else_if.stmts {
				selected << t.clone_stmt_with_bindings_and_fields(st, bindings, contexts,
					inside_runtime_loop)
			}
			return ast.Stmt(ast.BlockStmt{
				stmts: selected
			})
		}
		return t.clone_comptime_if_with_bindings_and_fields(else_if, bindings, contexts,
			inside_runtime_loop)
	}
	if expr.else_expr is ast.EmptyExpr {
		return ast.Stmt(ast.BlockStmt{})
	}
	return ast.Stmt(ast.ExprStmt{
		expr: t.clone_expr_with_bindings_and_fields(expr.else_expr, bindings, contexts)
	})
}

fn (t &Transformer) eval_clone_comptime_cond(expr ast.Expr, bindings map[string]types.Type) ?bool {
	match expr {
		ast.ComptimeExpr {
			return t.eval_clone_comptime_cond(expr.expr, bindings)
		}
		ast.InfixExpr {
			if expr.op == .and {
				left := t.eval_clone_comptime_cond(expr.lhs, bindings) or { return none }
				if !left {
					return false
				}
				right := t.eval_clone_comptime_cond(expr.rhs, bindings) or { return none }
				return right
			}
			if expr.op == .logical_or {
				left := t.eval_clone_comptime_cond(expr.lhs, bindings) or { return none }
				if left {
					return true
				}
				right := t.eval_clone_comptime_cond(expr.rhs, bindings) or { return none }
				return right
			}
			if expr.op == .key_is || expr.op == .not_is {
				lhs_type := t.comptime_lhs_bound_type(expr.lhs, bindings) or { return none }
				matches := t.clone_comptime_type_matches(lhs_type, expr.rhs)
				return if expr.op == .key_is { matches } else { !matches }
			}
			if expr.op == .eq || expr.op == .ne {
				lhs_value := t.comptime_lhs_bound_int(expr.lhs, bindings) or { return none }
				rhs_value := comptime_int_literal_value(expr.rhs) or { return none }
				matches := lhs_value == rhs_value
				return if expr.op == .eq { matches } else { !matches }
			}
		}
		ast.ParenExpr {
			return t.eval_clone_comptime_cond(expr.expr, bindings)
		}
		ast.PrefixExpr {
			if expr.op == .not {
				result := t.eval_clone_comptime_cond(expr.expr, bindings) or { return none }
				return !result
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) eval_clone_comptime_cond_with_fields(expr ast.Expr, bindings map[string]types.Type, contexts []CloneComptimeFieldCtx) ?bool {
	match expr {
		ast.ComptimeExpr {
			return t.eval_clone_comptime_cond_with_fields(expr.expr, bindings, contexts)
		}
		ast.SelectorExpr {
			return t.clone_comptime_field_bool(expr, contexts)
		}
		ast.InfixExpr {
			if expr.op == .and {
				left := t.eval_clone_comptime_cond_with_fields(expr.lhs, bindings, contexts) or {
					return none
				}
				if !left {
					return false
				}
				return t.eval_clone_comptime_cond_with_fields(expr.rhs, bindings, contexts)
			}
			if expr.op == .logical_or {
				left := t.eval_clone_comptime_cond_with_fields(expr.lhs, bindings, contexts) or {
					return none
				}
				if left {
					return true
				}
				return t.eval_clone_comptime_cond_with_fields(expr.rhs, bindings, contexts)
			}
			if expr.op == .key_is || expr.op == .not_is {
				lhs_type := t.comptime_lhs_bound_type_with_fields(expr.lhs, bindings, contexts) or {
					return none
				}
				matches := t.clone_comptime_type_matches(lhs_type, expr.rhs)
				return if expr.op == .key_is { matches } else { !matches }
			}
			if expr.op == .eq || expr.op == .ne {
				if lhs_value := t.comptime_lhs_bound_int_with_fields(expr.lhs, bindings, contexts) {
					rhs_value := comptime_int_literal_value(expr.rhs) or { return none }
					matches := lhs_value == rhs_value
					return if expr.op == .eq { matches } else { !matches }
				}
				if lhs_value := clone_comptime_lhs_bound_string(expr.lhs, contexts) {
					rhs_value := clone_comptime_string_literal_value(expr.rhs) or { return none }
					matches := lhs_value == rhs_value
					return if expr.op == .eq { matches } else { !matches }
				}
			}
		}
		ast.ParenExpr {
			return t.eval_clone_comptime_cond_with_fields(expr.expr, bindings, contexts)
		}
		ast.PrefixExpr {
			if expr.op == .not {
				result := t.eval_clone_comptime_cond_with_fields(expr.expr, bindings, contexts) or {
					return none
				}
				return !result
			}
		}
		else {}
	}

	return t.eval_clone_comptime_cond(expr, bindings)
}

fn (t &Transformer) comptime_lhs_bound_type_with_fields(expr ast.Expr, bindings map[string]types.Type, contexts []CloneComptimeFieldCtx) ?types.Type {
	if expr is ast.SelectorExpr {
		if expr.lhs is ast.Ident {
			if ctx := clone_comptime_field_ctx(contexts, expr.lhs.name) {
				if expr.rhs.name == 'typ' {
					return ctx.field.typ
				}
				if expr.rhs.name == 'unaliased_typ' {
					return generic_infer_unwrap_alias(t, ctx.field.typ)
				}
			}
		}
	}
	return t.comptime_lhs_bound_type(expr, bindings)
}

fn (t &Transformer) comptime_lhs_bound_int_with_fields(expr ast.Expr, bindings map[string]types.Type, contexts []CloneComptimeFieldCtx) ?int {
	if expr is ast.SelectorExpr {
		if expr.lhs is ast.Ident {
			if ctx := clone_comptime_field_ctx(contexts, expr.lhs.name) {
				if expr.rhs.name == 'indirections' {
					return generic_type_indirections(ctx.field.typ)
				}
				if expr.rhs.name == 'index' {
					return ctx.field_idx
				}
			}
		}
	}
	return t.comptime_lhs_bound_int(expr, bindings)
}

fn clone_comptime_lhs_bound_string(expr ast.Expr, contexts []CloneComptimeFieldCtx) ?string {
	if expr is ast.SelectorExpr {
		if expr.lhs is ast.Ident {
			if ctx := clone_comptime_field_ctx(contexts, expr.lhs.name) {
				if expr.rhs.name == 'name' {
					return ctx.field.name
				}
			}
		}
	}
	return none
}

fn clone_comptime_string_literal_value(expr ast.Expr) ?string {
	match expr {
		ast.StringLiteral {
			return expr.value.trim("'").trim('"')
		}
		ast.BasicLiteral {
			return expr.value.trim("'").trim('"')
		}
		else {
			return none
		}
	}
}

fn (t &Transformer) clone_comptime_field_bool(expr ast.SelectorExpr, contexts []CloneComptimeFieldCtx) ?bool {
	if expr.lhs !is ast.Ident {
		return none
	}
	lhs_ident := expr.lhs as ast.Ident
	ctx := clone_comptime_field_ctx(contexts, lhs_ident.name) or { return none }
	match expr.rhs.name {
		'is_mut' { return ctx.field.is_mut }
		'is_pub' { return ctx.field.is_public }
		'is_embed' { return ctx.is_embed }
		'is_shared' { return false }
		'is_atomic' { return false }
		'is_array' { return ctx.field.typ is types.Array || ctx.field.typ is types.ArrayFixed }
		'is_map' { return ctx.field.typ is types.Map }
		'is_option' { return ctx.field.typ is types.OptionType }
		'is_chan' { return ctx.field.typ is types.Channel }
		'is_enum' { return generic_infer_unwrap_alias(t, ctx.field.typ) is types.Enum }
		'is_struct' { return generic_infer_unwrap_alias(t, ctx.field.typ) is types.Struct }
		'is_alias' { return ctx.field.typ is types.Alias }
		else { return none }
	}
}

fn (t &Transformer) comptime_lhs_bound_int(expr ast.Expr, bindings map[string]types.Type) ?int {
	if expr is ast.SelectorExpr {
		if expr.lhs is ast.Ident {
			concrete := bindings[expr.lhs.name] or { return none }
			if expr.rhs.name == 'indirections' {
				return generic_type_indirections(concrete)
			}
		}
	}
	return none
}

fn generic_type_indirections(typ types.Type) int {
	match typ {
		types.Pointer {
			return 1 + generic_type_indirections(typ.base_type)
		}
		types.Alias {
			return generic_type_indirections(typ.base_type)
		}
		else {
			return 0
		}
	}
}

fn comptime_int_literal_value(expr ast.Expr) ?int {
	if expr is ast.BasicLiteral {
		return expr.value.int()
	}
	return none
}

fn (t &Transformer) comptime_lhs_bound_type(expr ast.Expr, bindings map[string]types.Type) ?types.Type {
	if expr is ast.Ident {
		return bindings[expr.name] or { none }
	}
	if expr is ast.SelectorExpr {
		if expr.lhs is ast.Ident {
			concrete := bindings[expr.lhs.name] or { return none }
			if expr.rhs.name == 'unaliased_typ' {
				return generic_infer_unwrap_alias(t, concrete)
			}
			return concrete
		}
	}
	return none
}

fn (t &Transformer) clone_comptime_type_matches(typ types.Type, rhs ast.Expr) bool {
	if rhs is ast.Ident {
		name := rhs.name
		if typ is types.Struct && t.struct_implements_name(typ, name) {
			return true
		}
		return typ.name() == name || t.type_to_c_name(typ) == name
			|| (name == 'byte' && typ.name() == 'u8')
			|| (name == 'u8' && typ.name() == 'byte')
			|| (name == 'int' && typ.name() == 'i32')
			|| (name == 'i32' && typ.name() == 'int')
	}
	if rhs is ast.ComptimeExpr {
		if rhs.expr is ast.Ident {
			return t.clone_comptime_type_matches_keyword(typ, rhs.expr.name)
		}
	}
	if rhs is ast.Type {
		match rhs {
			ast.OptionType {
				if typ is types.OptionType {
					return t.clone_comptime_type_matches(typ.base_type, rhs.base_type)
				}
				return false
			}
			ast.ResultType {
				if typ is types.ResultType {
					return t.clone_comptime_type_matches(typ.base_type, rhs.base_type)
				}
				return false
			}
			ast.PointerType {
				if typ is types.Pointer {
					return t.clone_comptime_type_matches(typ.base_type, rhs.base_type)
				}
				return false
			}
			ast.ArrayType {
				if typ is types.Array {
					return t.clone_comptime_type_matches(typ.elem_type, rhs.elem_type)
				}
				return false
			}
			ast.MapType {
				if typ is types.Map {
					return t.clone_comptime_type_matches(typ.key_type, rhs.key_type)
						&& t.clone_comptime_type_matches(typ.value_type, rhs.value_type)
				}
				return false
			}
			else {}
		}
	}
	if rhs is ast.SelectorExpr {
		name := rhs.name().replace('.', '__')
		if typ is types.Struct && (t.struct_implements_name(typ, rhs.name())
			|| t.struct_implements_name(typ, name)) {
			return true
		}
		return typ.name() == rhs.name() || typ.name() == name || t.type_to_c_name(typ) == name
	}
	return false
}

fn (t &Transformer) clone_comptime_type_matches_keyword(typ types.Type, keyword string) bool {
	match keyword {
		'int' {
			return typ is types.Primitive && typ.props.has(.integer)
		}
		'float' {
			return typ is types.Primitive && typ.props.has(.float)
		}
		'array' {
			return typ is types.Array || typ is types.ArrayFixed
		}
		'array_dynamic' {
			return typ is types.Array
		}
		'array_fixed' {
			return typ is types.ArrayFixed
		}
		'map' {
			return typ is types.Map
		}
		'struct' {
			return typ is types.Struct
		}
		'enum' {
			return typ is types.Enum
		}
		'alias' {
			return typ is types.Alias
		}
		'sumtype' {
			return typ is types.SumType
		}
		'pointer' {
			return typ is types.Pointer
		}
		'function' {
			return typ is types.FnType
		}
		'interface' {
			return typ is types.Interface
		}
		'option' {
			return typ is types.OptionType
		}
		'string' {
			return typ is types.String || typ.name() == 'string'
		}
		'shared' {
			return false
		}
		else {
			return false
		}
	}
}

fn clone_comptime_field_ctx(contexts []CloneComptimeFieldCtx, name string) ?CloneComptimeFieldCtx {
	for i := contexts.len - 1; i >= 0; i-- {
		ctx := contexts[i]
		if ctx.var_name == name {
			return ctx
		}
	}
	return none
}

fn (mut t Transformer) clone_monomorphized_pos(pos token.Pos) token.Pos {
	if pos.id == 0 {
		return pos
	}
	new_pos := t.next_synth_pos()
	return token.Pos{
		offset: pos.offset
		id:     new_pos.id
	}
}

fn (mut t Transformer) clone_monomorphized_pos_with_type(pos token.Pos, bindings map[string]types.Type) token.Pos {
	new_pos := t.clone_monomorphized_pos(pos)
	if new_pos.id == 0 {
		return new_pos
	}
	if typ := t.get_synth_type(pos) {
		t.register_synth_type(new_pos, substitute_type(typ, bindings))
		return new_pos
	}
	if pos.is_valid() {
		if typ := t.env.get_expr_type(pos.id) {
			t.register_synth_type(new_pos, substitute_type(t.normalize_type(typ), bindings))
		}
	}
	return new_pos
}

fn (mut t Transformer) clone_init_expr_pos_with_type(pos token.Pos, typ ast.Expr) token.Pos {
	mut new_pos := t.clone_monomorphized_pos(pos)
	if new_pos.id == 0 {
		new_pos = t.next_synth_pos()
	}
	if init_typ := t.type_from_init_expr(ast.InitExpr{
		typ: typ
		pos: new_pos
	})
	{
		t.register_synth_type(new_pos, init_typ)
	}
	return new_pos
}

fn (mut t Transformer) clone_generic_callable_value(lhs ast.Expr, args []ast.Expr, pos token.Pos) ?ast.Expr {
	return t.clone_generic_callable_value_with_outer_bindings(lhs, args, args,
		map[string]types.Type{}, pos)
}

fn (mut t Transformer) clone_generic_callable_value_with_outer_bindings(lhs ast.Expr, original_args []ast.Expr, args []ast.Expr, outer_bindings map[string]types.Type, pos token.Pos) ?ast.Expr {
	base_name := t.generic_call_base_name(lhs) or { return none }
	register_base_name := t.qualified_generic_callable_base_name(base_name)
	decl := t.generic_fn_decl_for_call(register_base_name) or { return none }
	info := t.generic_call_info_for_decl(register_base_name) or {
		CallFnInfo{
			generic_params: decl_generic_param_names(decl)
		}
	}
	mut bindings := map[string]types.Type{}
	if inferred := t.generic_bindings_from_type_args(info, args) {
		for name, typ in inferred {
			bindings[name] = typ
		}
	}
	t.fill_generic_bindings_from_outer_type_args(info, original_args, outer_bindings, mut bindings)
	if bindings.len > 0 {
		if lhs is ast.SelectorExpr {
			t.register_receiver_generic_method_call_spec_with_bindings(register_base_name, lhs.lhs,
				bindings, info, []ast.Expr{})
		}
		if generic_bindings_cover_params(bindings, decl_generic_param_names(decl)) {
			t.register_generic_bindings(register_base_name, bindings)
			if !decl.is_method {
				spec_name := t.specialized_fn_name(decl, bindings)
				clone_name := t.monomorphized_clone_name(register_base_name, decl, spec_name,
					bindings)
				return ast.Expr(ast.Ident{
					name: clone_name
					pos:  pos
				})
			}
		}
	}
	specialize_lhs := if register_base_name != base_name && lhs is ast.Ident {
		ast.Expr(ast.Ident{
			name: register_base_name
			pos:  pos
		})
	} else {
		lhs
	}
	return t.specialize_generic_callable_expr(specialize_lhs, args, pos)
}

fn (t &Transformer) qualified_generic_callable_base_name(base_name string) string {
	if base_name == '' || base_name.contains('__') || base_name.contains('.') || t.cur_module == ''
		|| t.cur_module == 'main' || t.cur_module == 'builtin' {
		return base_name
	}
	call_prefix := module_call_c_prefix(t.cur_module)
	if call_prefix != '' {
		candidate := '${call_prefix}__${base_name}'
		if _ := t.generic_fn_decl_for_call(candidate) {
			return candidate
		}
	}
	module_prefix := t.cur_module.replace('.', '__')
	if module_prefix != '' {
		candidate := '${module_prefix}__${base_name}'
		if _ := t.generic_fn_decl_for_call(candidate) {
			return candidate
		}
	}
	candidate := '${t.cur_module}.${base_name}'
	if _ := t.generic_fn_decl_for_call(candidate) {
		return candidate
	}
	return base_name
}

fn (t &Transformer) fill_generic_bindings_from_outer_type_args(info CallFnInfo, type_args []ast.Expr, outer_bindings map[string]types.Type, mut bindings map[string]types.Type) {
	if outer_bindings.len == 0 || info.generic_params.len == 0 {
		return
	}
	for i, generic_param in info.generic_params {
		if i >= type_args.len {
			continue
		}
		if type_args[i] is ast.Ident {
			type_arg_ident := type_args[i] as ast.Ident
			if concrete := outer_bindings[type_arg_ident.name] {
				bindings[generic_param] = concrete
			}
		}
	}
}

fn generic_bindings_cover_params(bindings map[string]types.Type, params []string) bool {
	if params.len == 0 {
		return false
	}
	for param in params {
		if param !in bindings {
			return false
		}
	}
	return true
}

fn (mut t Transformer) clone_expr_with_bindings_and_fields(expr ast.Expr, bindings map[string]types.Type, contexts []CloneComptimeFieldCtx) ast.Expr {
	match expr {
		ast.Ident {
			pos := t.clone_monomorphized_pos(expr.pos)
			if concrete := bindings[expr.name] {
				return t.type_to_ast_expr(concrete, pos)
			}
			if typ := t.get_expr_type(expr) {
				t.register_synth_type(pos, typ)
			}
			return ast.Expr(ast.Ident{
				name: expr.name
				pos:  pos
			})
		}
		ast.BasicLiteral {
			return ast.Expr(ast.BasicLiteral{
				kind:  expr.kind
				value: expr.value
				pos:   expr.pos
			})
		}
		ast.StringLiteral {
			return ast.Expr(ast.StringLiteral{
				kind:  expr.kind
				value: expr.value
				pos:   expr.pos
			})
		}
		ast.LifetimeExpr {
			return ast.Expr(ast.LifetimeExpr{
				name: expr.name
				pos:  expr.pos
			})
		}
		ast.EmptyExpr, ast.Keyword {
			return expr
		}
		ast.SelectorExpr {
			if expr.rhs.name == 'name' && expr.lhs is ast.Ident {
				lhs_ident := expr.lhs as ast.Ident
				if concrete := bindings[lhs_ident.name] {
					return t.clone_comptime_v_string_expr(t.types_type_to_v(concrete), expr.pos)
				}
			}
			if replacement := t.clone_comptime_field_selector_expr(expr, bindings, contexts) {
				return replacement
			}
			return ast.Expr(ast.SelectorExpr{
				lhs: t.clone_expr_with_bindings_and_fields(expr.lhs, bindings, contexts)
				rhs: ast.Ident{
					name: expr.rhs.name
					pos:  expr.rhs.pos
				}
				pos: expr.pos
			})
		}
		ast.CallExpr {
			pos := t.clone_monomorphized_pos_with_type(expr.pos, bindings)
			mut new_args := []ast.Expr{cap: expr.args.len}
			for a in expr.args {
				new_args << t.clone_expr_with_bindings_and_fields(a, bindings, contexts)
			}
			cloned_lhs := t.clone_expr_with_bindings_and_fields(expr.lhs, bindings, contexts)
			t.defer_generic_call_spec_for_cloned_call(cloned_lhs, new_args)
			return ast.Expr(ast.CallExpr{
				lhs:  cloned_lhs
				args: new_args
				pos:  pos
			})
		}
		ast.CallOrCastExpr {
			pos := t.clone_monomorphized_pos_with_type(expr.pos, bindings)
			mut new_expr := ast.empty_expr
			if expr.expr !is ast.EmptyExpr {
				new_expr = t.clone_expr_with_bindings_and_fields(expr.expr, bindings, contexts)
			}
			cloned_lhs := t.clone_expr_with_bindings_and_fields(expr.lhs, bindings, contexts)
			args := if new_expr is ast.EmptyExpr { []ast.Expr{} } else { [new_expr] }
			t.defer_generic_call_spec_for_cloned_call(cloned_lhs, args)
			return ast.Expr(ast.CallOrCastExpr{
				lhs:  cloned_lhs
				expr: new_expr
				pos:  pos
			})
		}
		ast.GenericArgs {
			mut new_args := []ast.Expr{cap: expr.args.len}
			for arg in expr.args {
				new_args << t.substitute_type_in_expr(arg, bindings)
			}
			if specialized := t.clone_generic_callable_value_with_outer_bindings(expr.lhs,
				expr.args, new_args, bindings, expr.pos)
			{
				return specialized
			}
			return ast.Expr(ast.GenericArgs{
				lhs:  t.clone_expr_with_bindings_and_fields(expr.lhs, bindings, contexts)
				args: new_args
				pos:  expr.pos
			})
		}
		ast.GenericArgOrIndexExpr {
			new_expr := t.substitute_type_in_expr(expr.expr, bindings)
			if specialized := t.clone_generic_callable_value_with_outer_bindings(expr.lhs, [
				expr.expr,
			], [new_expr], bindings, expr.pos)
			{
				return specialized
			}
			return ast.Expr(ast.GenericArgOrIndexExpr{
				lhs:  t.clone_expr_with_bindings_and_fields(expr.lhs, bindings, contexts)
				expr: new_expr
				pos:  expr.pos
			})
		}
		ast.ComptimeExpr {
			return ast.Expr(ast.ComptimeExpr{
				expr: t.clone_expr_with_bindings_and_fields(expr.expr, bindings, contexts)
				pos:  expr.pos
			})
		}
		ast.IfExpr {
			mut new_stmts := []ast.Stmt{cap: expr.stmts.len}
			for st in expr.stmts {
				new_stmts << t.clone_stmt_with_bindings_and_fields(st, bindings, contexts, false)
			}
			return ast.Expr(ast.IfExpr{
				cond:      t.clone_expr_with_bindings_and_fields(expr.cond, bindings, contexts)
				stmts:     new_stmts
				else_expr: t.clone_expr_with_bindings_and_fields(expr.else_expr, bindings, contexts)
				pos:       expr.pos
			})
		}
		ast.IfGuardExpr {
			cloned := t.clone_stmt_with_bindings_and_fields(ast.Stmt(expr.stmt), bindings,
				contexts, false)
			if cloned is ast.AssignStmt {
				return ast.Expr(ast.IfGuardExpr{
					stmt: cloned
					pos:  expr.pos
				})
			}
			return expr
		}
		ast.InfixExpr {
			return ast.Expr(ast.InfixExpr{
				op:  expr.op
				lhs: t.clone_expr_with_bindings_and_fields(expr.lhs, bindings, contexts)
				rhs: t.clone_expr_with_bindings_and_fields(expr.rhs, bindings, contexts)
				pos: expr.pos
			})
		}
		ast.PrefixExpr {
			return ast.Expr(ast.PrefixExpr{
				op:   expr.op
				expr: t.clone_expr_with_bindings_and_fields(expr.expr, bindings, contexts)
				pos:  expr.pos
			})
		}
		ast.ModifierExpr {
			return ast.Expr(ast.ModifierExpr{
				kind: expr.kind
				expr: t.clone_expr_with_bindings_and_fields(expr.expr, bindings, contexts)
				pos:  expr.pos
			})
		}
		ast.KeywordOperator {
			mut new_exprs := []ast.Expr{cap: expr.exprs.len}
			for e in expr.exprs {
				new_exprs << t.clone_expr_with_bindings_and_fields(e, bindings, contexts)
			}
			return ast.Expr(ast.KeywordOperator{
				op:    expr.op
				exprs: new_exprs
				pos:   expr.pos
			})
		}
		ast.PostfixExpr {
			return ast.Expr(ast.PostfixExpr{
				op:   expr.op
				expr: t.clone_expr_with_bindings_and_fields(expr.expr, bindings, contexts)
				pos:  expr.pos
			})
		}
		ast.ParenExpr {
			return ast.Expr(ast.ParenExpr{
				expr: t.clone_expr_with_bindings_and_fields(expr.expr, bindings, contexts)
				pos:  expr.pos
			})
		}
		ast.IndexExpr {
			generic_parts := generic_index_call_parts(expr)
			if generic_parts.args.len > 0 {
				mut new_args := []ast.Expr{cap: generic_parts.args.len}
				for arg in generic_parts.args {
					new_args << t.substitute_type_in_expr(arg, bindings)
				}
				if specialized := t.clone_generic_callable_value_with_outer_bindings(generic_parts.lhs,
					generic_parts.args, new_args, bindings, expr.pos)
				{
					return specialized
				}
			}
			pos := t.clone_monomorphized_pos_with_type(expr.pos, bindings)
			return ast.Expr(ast.IndexExpr{
				lhs:      t.clone_expr_with_bindings_and_fields(expr.lhs, bindings, contexts)
				expr:     t.clone_expr_with_bindings_and_fields(expr.expr, bindings, contexts)
				is_gated: expr.is_gated
				pos:      pos
			})
		}
		ast.RangeExpr {
			return ast.Expr(ast.RangeExpr{
				op:    expr.op
				start: t.clone_expr_with_bindings_and_fields(expr.start, bindings, contexts)
				end:   t.clone_expr_with_bindings_and_fields(expr.end, bindings, contexts)
				pos:   expr.pos
			})
		}
		ast.ArrayInitExpr {
			mut new_exprs := []ast.Expr{cap: expr.exprs.len}
			for item in expr.exprs {
				new_exprs << t.clone_expr_with_bindings_and_fields(item, bindings, contexts)
			}
			return ast.Expr(ast.ArrayInitExpr{
				typ:         t.substitute_type_in_expr(expr.typ, bindings)
				exprs:       new_exprs
				init:        t.clone_expr_with_bindings_and_fields(expr.init, bindings, contexts)
				cap:         t.clone_expr_with_bindings_and_fields(expr.cap, bindings, contexts)
				len:         t.clone_expr_with_bindings_and_fields(expr.len, bindings, contexts)
				update_expr: t.clone_expr_with_bindings_and_fields(expr.update_expr, bindings,
					contexts)
				pos:         expr.pos
			})
		}
		ast.MapInitExpr {
			mut new_keys := []ast.Expr{cap: expr.keys.len}
			for key in expr.keys {
				new_keys << t.clone_expr_with_bindings_and_fields(key, bindings, contexts)
			}
			mut new_vals := []ast.Expr{cap: expr.vals.len}
			for val in expr.vals {
				new_vals << t.clone_expr_with_bindings_and_fields(val, bindings, contexts)
			}
			return ast.Expr(ast.MapInitExpr{
				typ:  t.substitute_type_in_expr(expr.typ, bindings)
				keys: new_keys
				vals: new_vals
				pos:  expr.pos
			})
		}
		ast.InitExpr {
			mut new_fields := []ast.FieldInit{cap: expr.fields.len}
			for field in expr.fields {
				new_fields << ast.FieldInit{
					name:  field.name
					value: t.clone_expr_with_bindings_and_fields(field.value, bindings, contexts)
				}
			}
			new_typ := t.substitute_type_in_expr(expr.typ, bindings)
			new_pos := t.clone_init_expr_pos_with_type(expr.pos, new_typ)
			return ast.Expr(ast.InitExpr{
				typ:    new_typ
				fields: new_fields
				pos:    new_pos
			})
		}
		ast.FieldInit {
			return ast.Expr(ast.FieldInit{
				name:  expr.name
				value: t.clone_expr_with_bindings_and_fields(expr.value, bindings, contexts)
			})
		}
		ast.AssocExpr {
			mut new_fields := []ast.FieldInit{cap: expr.fields.len}
			for field in expr.fields {
				new_fields << ast.FieldInit{
					name:  field.name
					value: t.clone_expr_with_bindings_and_fields(field.value, bindings, contexts)
				}
			}
			return ast.Expr(ast.AssocExpr{
				typ:    t.substitute_type_in_expr(expr.typ, bindings)
				expr:   t.clone_expr_with_bindings_and_fields(expr.expr, bindings, contexts)
				fields: new_fields
				pos:    expr.pos
			})
		}
		ast.CastExpr {
			return ast.Expr(ast.CastExpr{
				typ:  t.substitute_type_in_expr(expr.typ, bindings)
				expr: t.clone_expr_with_bindings_and_fields(expr.expr, bindings, contexts)
				pos:  expr.pos
			})
		}
		ast.AsCastExpr {
			return ast.Expr(ast.AsCastExpr{
				expr: t.clone_expr_with_bindings_and_fields(expr.expr, bindings, contexts)
				typ:  t.substitute_type_in_expr(expr.typ, bindings)
				pos:  expr.pos
			})
		}
		ast.OrExpr {
			mut new_stmts := []ast.Stmt{cap: expr.stmts.len}
			for st in expr.stmts {
				new_stmts << t.clone_stmt_with_bindings_and_fields(st, bindings, contexts, false)
			}
			return ast.Expr(ast.OrExpr{
				expr:  t.clone_expr_with_bindings_and_fields(expr.expr, bindings, contexts)
				stmts: new_stmts
				pos:   expr.pos
			})
		}
		ast.MatchExpr {
			mut new_branches := []ast.MatchBranch{cap: expr.branches.len}
			for branch in expr.branches {
				mut conds := []ast.Expr{cap: branch.cond.len}
				for cond in branch.cond {
					conds << t.clone_expr_with_bindings_and_fields(cond, bindings, contexts)
				}
				mut stmts := []ast.Stmt{cap: branch.stmts.len}
				for st in branch.stmts {
					stmts << t.clone_stmt_with_bindings_and_fields(st, bindings, contexts, false)
				}
				new_branches << ast.MatchBranch{
					cond:  conds
					stmts: stmts
					pos:   branch.pos
				}
			}
			return ast.Expr(ast.MatchExpr{
				expr:     t.clone_expr_with_bindings_and_fields(expr.expr, bindings, contexts)
				branches: new_branches
				pos:      expr.pos
			})
		}
		ast.StringInterLiteral {
			mut inters := []ast.StringInter{cap: expr.inters.len}
			for inter in expr.inters {
				mut inter_expr := t.clone_expr_with_bindings_and_fields(inter.expr, bindings,
					contexts)
				inter_expr = t.repair_stale_string_str_interpolation_expr(inter_expr)
				format_expr := t.clone_expr_with_bindings_and_fields(inter.format_expr, bindings,
					contexts)
				cloned_inter := ast.StringInter{
					format:      inter.format
					width:       inter.width
					precision:   inter.precision
					expr:        inter_expr
					format_expr: format_expr
				}
				inters << ast.StringInter{
					format:       inter.format
					width:        inter.width
					precision:    inter.precision
					expr:         inter_expr
					format_expr:  format_expr
					resolved_fmt: if inter.resolved_fmt != '' {
						inter.resolved_fmt
					} else {
						t.resolve_sprintf_format(cloned_inter)
					}
				}
			}
			return ast.Expr(ast.StringInterLiteral{
				kind:   expr.kind
				values: expr.values
				inters: inters
				pos:    expr.pos
			})
		}
		ast.Tuple {
			mut exprs := []ast.Expr{cap: expr.exprs.len}
			for item in expr.exprs {
				exprs << t.clone_expr_with_bindings_and_fields(item, bindings, contexts)
			}
			return ast.Expr(ast.Tuple{
				exprs: exprs
				pos:   expr.pos
			})
		}
		ast.UnsafeExpr {
			mut stmts := []ast.Stmt{cap: expr.stmts.len}
			for st in expr.stmts {
				stmts << t.clone_stmt_with_bindings_and_fields(st, bindings, contexts, false)
			}
			return ast.Expr(ast.UnsafeExpr{
				stmts: stmts
				pos:   expr.pos
			})
		}
		ast.LockExpr {
			mut lock_exprs := []ast.Expr{cap: expr.lock_exprs.len}
			for item in expr.lock_exprs {
				lock_exprs << t.clone_expr_with_bindings_and_fields(item, bindings, contexts)
			}
			mut rlock_exprs := []ast.Expr{cap: expr.rlock_exprs.len}
			for item in expr.rlock_exprs {
				rlock_exprs << t.clone_expr_with_bindings_and_fields(item, bindings, contexts)
			}
			mut stmts := []ast.Stmt{cap: expr.stmts.len}
			for st in expr.stmts {
				stmts << t.clone_stmt_with_bindings_and_fields(st, bindings, contexts, false)
			}
			return ast.Expr(ast.LockExpr{
				lock_exprs:  lock_exprs
				rlock_exprs: rlock_exprs
				stmts:       stmts
				pos:         expr.pos
			})
		}
		ast.LambdaExpr {
			return ast.Expr(ast.LambdaExpr{
				args: expr.args
				expr: t.clone_expr_with_bindings_and_fields(expr.expr, bindings, contexts)
				pos:  expr.pos
			})
		}
		ast.SelectExpr {
			mut stmts := []ast.Stmt{cap: expr.stmts.len}
			for st in expr.stmts {
				stmts << t.clone_stmt_with_bindings_and_fields(st, bindings, contexts, false)
			}
			return ast.Expr(ast.SelectExpr{
				pos:   expr.pos
				stmt:  t.clone_stmt_with_bindings_and_fields(expr.stmt, bindings, contexts, false)
				stmts: stmts
				next:  t.clone_expr_with_bindings_and_fields(expr.next, bindings, contexts)
			})
		}
		ast.SqlExpr {
			return ast.Expr(ast.SqlExpr{
				expr:       t.clone_expr_with_bindings_and_fields(expr.expr, bindings, contexts)
				table_name: expr.table_name
				is_count:   expr.is_count
				is_create:  expr.is_create
				pos:        expr.pos
			})
		}
		ast.FnLiteral {
			mut stmts := []ast.Stmt{cap: expr.stmts.len}
			for st in expr.stmts {
				stmts << t.clone_stmt_with_bindings_and_fields(st, bindings, contexts, false)
			}
			mut fn_typ := expr.typ
			substituted_typ := t.substitute_type_in_type_node(ast.Type(expr.typ), bindings)
			if substituted_typ is ast.FnType {
				fn_typ = substituted_typ
			}
			return ast.Expr(ast.FnLiteral{
				typ:           fn_typ
				captured_vars: expr.captured_vars
				stmts:         stmts
				pos:           expr.pos
			})
		}
		ast.Type {
			return ast.Expr(t.substitute_type_in_type_node(expr, bindings))
		}
	}
}

fn (mut t Transformer) clone_comptime_field_selector_expr(sel ast.SelectorExpr, bindings map[string]types.Type, contexts []CloneComptimeFieldCtx) ?ast.Expr {
	if is_clone_comptime_selector_rhs_name(sel.rhs.name) {
		if contexts.len == 0 {
			return none
		}
		ctx := contexts[contexts.len - 1]
		sel_pos := t.next_synth_pos()
		t.register_synth_type(sel_pos, ctx.field.typ)
		return ast.Expr(ast.SelectorExpr{
			lhs: t.clone_expr_with_bindings_and_fields(sel.lhs, bindings, contexts)
			rhs: ast.Ident{
				name: ctx.field.name
				pos:  sel.rhs.pos
			}
			pos: sel_pos
		})
	}
	if sel.lhs is ast.SelectorExpr {
		inner := sel.lhs as ast.SelectorExpr
		if inner.lhs is ast.Ident {
			ctx := clone_comptime_field_ctx(contexts, inner.lhs.name) or { return none }
			if inner.rhs.name == 'name' {
				match sel.rhs.name {
					'str' { return t.clone_comptime_c_string_expr(ctx.field.name, sel.pos) }
					'len' { return t.clone_comptime_int_expr(ctx.field.name.len, sel.pos) }
					else {}
				}
			}
		}
	}
	if sel.lhs !is ast.Ident {
		return none
	}
	lhs_ident := sel.lhs as ast.Ident
	ctx := clone_comptime_field_ctx(contexts, lhs_ident.name) or { return none }
	match sel.rhs.name {
		'name' {
			return t.clone_comptime_v_string_expr(ctx.field.name, sel.pos)
		}
		'typ' {
			return t.clone_comptime_int_expr(clone_comptime_type_idx(ctx.field.typ), sel.pos)
		}
		'unaliased_typ' {
			return t.clone_comptime_int_expr(clone_comptime_type_idx(generic_infer_unwrap_alias(t,
				ctx.field.typ)), sel.pos)
		}
		'indirections' {
			return t.clone_comptime_int_expr(generic_type_indirections(ctx.field.typ), sel.pos)
		}
		'index' {
			return t.clone_comptime_int_expr(ctx.field_idx, sel.pos)
		}
		'attrs' {
			return t.clone_comptime_string_array_expr(ctx.attrs, sel.pos)
		}
		'is_mut' {
			return t.clone_comptime_bool_expr(ctx.field.is_mut, sel.pos)
		}
		'is_pub' {
			return t.clone_comptime_bool_expr(ctx.field.is_public, sel.pos)
		}
		'is_embed' {
			return t.clone_comptime_bool_expr(ctx.is_embed, sel.pos)
		}
		'is_shared' {
			return t.clone_comptime_bool_expr(false, sel.pos)
		}
		'is_atomic' {
			return t.clone_comptime_bool_expr(false, sel.pos)
		}
		'is_array' {
			return t.clone_comptime_bool_expr(ctx.field.typ is types.Array
				|| ctx.field.typ is types.ArrayFixed, sel.pos)
		}
		'is_map' {
			return t.clone_comptime_bool_expr(ctx.field.typ is types.Map, sel.pos)
		}
		'is_option' {
			return t.clone_comptime_bool_expr(ctx.field.typ is types.OptionType, sel.pos)
		}
		'is_chan' {
			return t.clone_comptime_bool_expr(ctx.field.typ is types.Channel, sel.pos)
		}
		'is_enum' {
			return t.clone_comptime_bool_expr(generic_infer_unwrap_alias(t, ctx.field.typ) is types.Enum,
				sel.pos)
		}
		'is_struct' {
			return t.clone_comptime_bool_expr(generic_infer_unwrap_alias(t, ctx.field.typ) is types.Struct,
				sel.pos)
		}
		'is_alias' {
			return t.clone_comptime_bool_expr(ctx.field.typ is types.Alias, sel.pos)
		}
		else {}
	}

	return none
}

fn is_clone_comptime_selector_rhs_name(name string) bool {
	return name == '__comptime_selector__' || name == 'TODO: comptime selector'
}

fn (mut t Transformer) clone_comptime_v_string_expr(value string, _pos token.Pos) ast.Expr {
	lit_pos := t.next_synth_pos()
	t.register_synth_type(lit_pos, types.Type(types.string_))
	return ast.Expr(ast.StringLiteral{
		kind:  .v
		value: value
		pos:   lit_pos
	})
}

fn (mut t Transformer) clone_comptime_c_string_expr(value string, _pos token.Pos) ast.Expr {
	lit_pos := t.next_synth_pos()
	if typ := types.builtin_type('charptr') {
		t.register_synth_type(lit_pos, typ)
	}
	return ast.Expr(ast.StringLiteral{
		kind:  .c
		value: value
		pos:   lit_pos
	})
}

fn (mut t Transformer) clone_comptime_int_expr(value int, _pos token.Pos) ast.Expr {
	lit_pos := t.next_synth_pos()
	t.register_synth_type(lit_pos, types.Type(types.int_))
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: value.str()
		pos:   lit_pos
	})
}

fn (mut t Transformer) clone_comptime_bool_expr(value bool, _pos token.Pos) ast.Expr {
	lit_pos := t.next_synth_pos()
	t.register_synth_type(lit_pos, types.Type(types.bool_))
	return ast.Expr(ast.BasicLiteral{
		kind:  if value { token.Token.key_true } else { token.Token.key_false }
		value: if value { 'true' } else { 'false' }
		pos:   lit_pos
	})
}

fn (mut t Transformer) clone_comptime_string_array_expr(values []string, _pos token.Pos) ast.Expr {
	array_pos := t.next_synth_pos()
	t.register_synth_type(array_pos, types.Type(types.Array{
		elem_type: types.Type(types.string_)
	}))
	mut exprs := []ast.Expr{cap: values.len}
	for value in values {
		item_pos := t.next_synth_pos()
		t.register_synth_type(item_pos, types.Type(types.string_))
		exprs << ast.Expr(ast.StringLiteral{
			kind:  .v
			value: value
			pos:   item_pos
		})
	}
	elem_pos := t.next_synth_pos()
	t.register_synth_type(elem_pos, types.Type(types.string_))
	return ast.Expr(ast.ArrayInitExpr{
		typ:   ast.Expr(ast.Type(ast.ArrayType{
			elem_type: ast.Expr(ast.Ident{
				name: 'string'
				pos:  elem_pos
			})
		}))
		exprs: exprs
		pos:   array_pos
	})
}

fn clone_comptime_type_idx(typ types.Type) int {
	name := typ.name()
	return match name {
		'void' { 0 }
		'voidptr' { 1 }
		'byteptr' { 2 }
		'charptr' { 3 }
		'i8' { 4 }
		'i16' { 5 }
		'i32' { 6 }
		'int' { 7 }
		'i64' { 8 }
		'isize' { 9 }
		'u16' { 10 }
		'u32' { 11 }
		'u64' { 12 }
		'usize' { 13 }
		'f32' { 14 }
		'f64' { 15 }
		'char' { 16 }
		'rune' { 17 }
		'string' { 18 }
		'bool' { 19 }
		else { 0 }
	}
}

// clone_expr_with_bindings deep-clones an expression, substituting generic
// types in embedded type positions (CastExpr targets, ArrayType elem types,
// etc.) and recursing through container nodes.
pub fn (mut t Transformer) clone_expr_with_bindings(expr ast.Expr, bindings map[string]types.Type) ast.Expr {
	match expr {
		ast.Ident {
			if concrete := bindings[expr.name] {
				return t.type_to_ast_expr(concrete, expr.pos)
			}
			if typ := t.get_expr_type(expr) {
				t.register_synth_type(expr.pos, typ)
			}
			return expr
		}
		ast.BasicLiteral, ast.EmptyExpr {
			return expr
		}
		ast.CallExpr {
			pos := t.clone_monomorphized_pos_with_type(expr.pos, bindings)
			mut new_args := []ast.Expr{cap: expr.args.len}
			for a in expr.args {
				new_args << t.clone_expr_with_bindings(a, bindings)
			}
			cloned_lhs := t.clone_expr_with_bindings(expr.lhs, bindings)
			t.defer_generic_call_spec_for_cloned_call(cloned_lhs, new_args)
			return ast.Expr(ast.CallExpr{
				lhs:  cloned_lhs
				args: new_args
				pos:  pos
			})
		}
		ast.CallOrCastExpr {
			pos := t.clone_monomorphized_pos_with_type(expr.pos, bindings)
			mut new_expr := ast.empty_expr
			if expr.expr !is ast.EmptyExpr {
				new_expr = t.clone_expr_with_bindings(expr.expr, bindings)
			}
			cloned_lhs := t.clone_expr_with_bindings(expr.lhs, bindings)
			args := if new_expr is ast.EmptyExpr { []ast.Expr{} } else { [new_expr] }
			t.defer_generic_call_spec_for_cloned_call(cloned_lhs, args)
			return ast.Expr(ast.CallOrCastExpr{
				lhs:  cloned_lhs
				expr: new_expr
				pos:  pos
			})
		}
		ast.GenericArgs {
			mut new_args := []ast.Expr{cap: expr.args.len}
			for arg in expr.args {
				new_args << t.substitute_type_in_expr(arg, bindings)
			}
			if specialized := t.clone_generic_callable_value_with_outer_bindings(expr.lhs,
				expr.args, new_args, bindings, expr.pos)
			{
				return specialized
			}
			return ast.Expr(ast.GenericArgs{
				lhs:  t.clone_expr_with_bindings(expr.lhs, bindings)
				args: new_args
				pos:  expr.pos
			})
		}
		ast.GenericArgOrIndexExpr {
			new_expr := t.substitute_type_in_expr(expr.expr, bindings)
			if specialized := t.clone_generic_callable_value_with_outer_bindings(expr.lhs, [
				expr.expr,
			], [new_expr], bindings, expr.pos)
			{
				return specialized
			}
			return ast.Expr(ast.GenericArgOrIndexExpr{
				lhs:  t.clone_expr_with_bindings(expr.lhs, bindings)
				expr: new_expr
				pos:  expr.pos
			})
		}
		ast.ComptimeExpr {
			return ast.Expr(ast.ComptimeExpr{
				expr: t.clone_expr_with_bindings(expr.expr, bindings)
				pos:  expr.pos
			})
		}
		ast.IfExpr {
			mut new_stmts := []ast.Stmt{cap: expr.stmts.len}
			for st in expr.stmts {
				new_stmts << t.clone_stmt_with_bindings(st, bindings)
			}
			return ast.Expr(ast.IfExpr{
				cond:      t.clone_expr_with_bindings(expr.cond, bindings)
				stmts:     new_stmts
				else_expr: t.clone_expr_with_bindings(expr.else_expr, bindings)
				pos:       expr.pos
			})
		}
		ast.SelectorExpr {
			if expr.rhs.name == 'name' && expr.lhs is ast.Ident {
				lhs_ident := expr.lhs as ast.Ident
				if concrete := bindings[lhs_ident.name] {
					return t.clone_comptime_v_string_expr(t.types_type_to_v(concrete), expr.pos)
				}
			}
			return ast.Expr(ast.SelectorExpr{
				lhs: t.clone_expr_with_bindings(expr.lhs, bindings)
				rhs: expr.rhs
			})
		}
		ast.InfixExpr {
			return ast.Expr(ast.InfixExpr{
				op:  expr.op
				lhs: t.clone_expr_with_bindings(expr.lhs, bindings)
				rhs: t.clone_expr_with_bindings(expr.rhs, bindings)
				pos: expr.pos
			})
		}
		ast.PrefixExpr {
			return ast.Expr(ast.PrefixExpr{
				op:   expr.op
				expr: t.clone_expr_with_bindings(expr.expr, bindings)
				pos:  expr.pos
			})
		}
		ast.ModifierExpr {
			return ast.Expr(ast.ModifierExpr{
				kind: expr.kind
				expr: t.clone_expr_with_bindings(expr.expr, bindings)
				pos:  expr.pos
			})
		}
		ast.KeywordOperator {
			mut new_exprs := []ast.Expr{cap: expr.exprs.len}
			for e in expr.exprs {
				new_exprs << t.clone_expr_with_bindings(e, bindings)
			}
			return ast.Expr(ast.KeywordOperator{
				op:    expr.op
				exprs: new_exprs
				pos:   expr.pos
			})
		}
		ast.PostfixExpr {
			return ast.Expr(ast.PostfixExpr{
				op:   expr.op
				expr: t.clone_expr_with_bindings(expr.expr, bindings)
				pos:  expr.pos
			})
		}
		ast.ParenExpr {
			return ast.Expr(ast.ParenExpr{
				expr: t.clone_expr_with_bindings(expr.expr, bindings)
			})
		}
		ast.IndexExpr {
			generic_parts := generic_index_call_parts(expr)
			if generic_parts.args.len > 0 {
				mut new_args := []ast.Expr{cap: generic_parts.args.len}
				for arg in generic_parts.args {
					new_args << t.substitute_type_in_expr(arg, bindings)
				}
				if specialized := t.clone_generic_callable_value_with_outer_bindings(generic_parts.lhs,
					generic_parts.args, new_args, bindings, expr.pos)
				{
					return specialized
				}
			}
			pos := t.clone_monomorphized_pos_with_type(expr.pos, bindings)
			return ast.Expr(ast.IndexExpr{
				lhs:      t.clone_expr_with_bindings(expr.lhs, bindings)
				expr:     t.clone_expr_with_bindings(expr.expr, bindings)
				is_gated: expr.is_gated
				pos:      pos
			})
		}
		ast.InitExpr {
			mut new_fields := []ast.FieldInit{cap: expr.fields.len}
			for field in expr.fields {
				new_fields << ast.FieldInit{
					name:  field.name
					value: t.clone_expr_with_bindings(field.value, bindings)
				}
			}
			new_typ := t.substitute_type_in_expr(expr.typ, bindings)
			new_pos := t.clone_init_expr_pos_with_type(expr.pos, new_typ)
			return ast.Expr(ast.InitExpr{
				typ:    new_typ
				fields: new_fields
				pos:    new_pos
			})
		}
		ast.CastExpr {
			return ast.Expr(ast.CastExpr{
				typ:  t.substitute_type_in_expr(expr.typ, bindings)
				expr: t.clone_expr_with_bindings(expr.expr, bindings)
				pos:  expr.pos
			})
		}
		ast.Type {
			return ast.Expr(t.substitute_type_in_type_node(expr, bindings))
		}
		else {
			return expr
		}
	}
}
