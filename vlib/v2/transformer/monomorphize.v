// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

// Generic monomorphization belongs in the transformer so every backend receives
// concrete FnDecls and call names.
import v2.ast
import v2.token
import v2.types

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
	for _ in 0 .. 8 {
		spec_count := t.monomorphized_specs.len
		t.collect_generic_call_specs(prepared)
		prepared = t.monomorphize_pass(prepared)
		if t.monomorphized_specs.len == spec_count {
			break
		}
	}
	return prepared
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
	for fn_key, bindings_list in t.env.generic_types {
		lookup_key := t.resolve_monomorphize_decl_key(fn_key, decl_node) or { continue }
		decl := decl_node[lookup_key] or { continue }
		fi := decl_owner[lookup_key] or { continue }
		for bindings in bindings_list {
			spec_name := t.specialized_fn_name(decl, bindings)
			if spec_name == decl.name {
				continue
			}
			spec_key := '${lookup_key}:${spec_name}'
			if spec_key in t.monomorphized_specs {
				continue
			}
			t.monomorphized_specs[spec_key] = true
			t.register_monomorphized_fn_bindings(files[fi].mod, spec_name, bindings)
			cloned := t.clone_fn_decl_with_substitutions(decl, bindings, spec_name)
			mut bucket := per_file_clones[fi] or { []ast.Stmt{} }
			bucket << ast.Stmt(cloned)
			per_file_clones[fi] = bucket
		}
	}
	if per_file_clones.len == 0 {
		return files
	}
	mut new_files := []ast.File{cap: files.len}
	for fi, file in files {
		extra := per_file_clones[fi] or {
			new_files << file
			continue
		}
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

fn (mut t Transformer) index_generic_fn_decl_for_monomorphize(mut decl_owner map[string]int, mut decl_node map[string]ast.FnDecl, decl ast.FnDecl, file_idx int, module_name string) {
	mut keys := []string{}
	if !decl.is_method {
		keys << decl.name
		if module_name != '' {
			keys << '${module_name}.${decl.name}'
			keys << '${module_name.replace('.', '__')}__${decl.name}'
		}
	} else {
		recv_name := t.get_receiver_type_name(decl.receiver.typ)
		if recv_name != '' {
			keys << '${recv_name}__${decl.name}'
			if module_name != '' && !recv_name.contains('__') {
				keys << '${module_name.replace('.', '__')}__${recv_name}__${decl.name}'
			}
		}
	}
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
		short_name := base_name.all_after_last('.')
		if decl := decl_node[short_name] {
			if !decl.is_method {
				return short_name
			}
		}
		c_name := base_name.replace('.', '__')
		if c_name in decl_node {
			return c_name
		}
	}
	if base_name.contains('__') {
		short_name := base_name.all_after_last('__')
		if decl := decl_node[short_name] {
			if !decl.is_method {
				return short_name
			}
		}
	}
	return none
}

fn (mut t Transformer) collect_generic_call_specs(files []ast.File) {
	old_module := t.cur_module
	old_file := t.cur_file_name
	old_scope := t.scope
	t.generic_fn_decl_index = map[string]ast.FnDecl{}
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
	for file in files {
		t.cur_file_name = file.name
		t.cur_module = file.mod
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
}

fn (mut t Transformer) collect_generic_call_specs_in_stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		t.collect_generic_call_specs_in_stmt(stmt)
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
				t.collect_generic_call_specs_in_expr(field.value)
			}
		}
		else {}
	}
}

fn (mut t Transformer) collect_generic_call_specs_in_fn_decl(decl ast.FnDecl) {
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
	t.cur_monomorphized_fn_bindings = t.lookup_monomorphized_fn_bindings(t.cur_module, decl.name) or {
		map[string]types.Type{}
	}
	mut recv_name := if decl.is_method { t.get_receiver_type_name(decl.receiver.typ) } else { '' }
	if t.cur_module != '' {
		prefix := '${t.cur_module}__'
		if recv_name.starts_with(prefix) {
			recv_name = recv_name[prefix.len..]
		}
	}
	scope_fn_name := if decl.is_method { '${recv_name}__${decl.name}' } else { decl.name }
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
	t.seed_fn_param_decl_types(decl.typ.params, [])
	t.seed_fn_pointer_param_return_types(decl.typ.params, [])
	t.seed_scope_with_fn_params(decl)
	t.collect_generic_call_specs_in_stmts(decl.stmts)
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
			t.collect_generic_call_specs_in_expr(expr.lhs)
			t.collect_generic_call_specs_in_expr(expr.expr)
		}
		ast.GenericArgs {
			t.collect_generic_call_specs_in_expr(expr.lhs)
			for arg in expr.args {
				t.collect_generic_call_specs_in_expr(arg)
			}
		}
		ast.IfExpr {
			t.collect_generic_call_specs_in_expr(expr.cond)
			t.collect_generic_call_specs_in_stmts(expr.stmts)
			t.collect_generic_call_specs_in_expr(expr.else_expr)
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
			}
		}
		ast.Tuple {
			for item in expr.exprs {
				t.collect_generic_call_specs_in_expr(item)
			}
		}
		ast.Type {
			t.collect_generic_call_specs_in_type(expr)
		}
		ast.UnsafeExpr {
			t.collect_generic_call_specs_in_stmts(expr.stmts)
		}
		else {}
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

fn (t &Transformer) generic_match_smartcast_contexts(smartcast_expr string, sumtype_name string, conds []ast.Expr) []SmartcastContext {
	if smartcast_expr == '' {
		return []SmartcastContext{}
	}
	variants := t.get_sum_type_variants(sumtype_name)
	if variants.len == 0 {
		return []SmartcastContext{}
	}
	mut ctxs := []SmartcastContext{cap: conds.len}
	for cond in conds {
		mut c_variant_name := ''
		mut c_variant_name_full := ''
		mut c_variant_module := ''
		if cond is ast.Ident {
			c_variant_name = cond.name
			c_variant_name_full = if t.cur_module != '' && t.cur_module != 'main'
				&& t.cur_module != 'builtin'
				&& cond.name !in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'byte', 'rune', 'f32', 'f64', 'usize', 'isize', 'bool', 'string', 'voidptr', 'charptr', 'byteptr'] {
				'${t.cur_module}__${cond.name}'
			} else {
				cond.name
			}
		} else if cond is ast.SelectorExpr {
			c_variant_name = cond.rhs.name
			if cond.lhs is ast.Ident {
				c_variant_module = (cond.lhs as ast.Ident).name
				c_variant_name_full = '${c_variant_module}__${cond.rhs.name}'
			} else {
				c_variant_name_full = cond.rhs.name
			}
		} else if cond is ast.Type {
			c_variant_name = t.type_variant_name(cond)
			c_variant_name_full = t.type_variant_name_full(cond)
		}
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
			qualified_variant) {
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

fn match_cond_variant_matches_sumtype(sumtype_name string, variants []string, variant_name string, qualified_variant string) bool {
	for variant in variants {
		if sum_type_variant_matches_for_sumtype(sumtype_name, variant, qualified_variant) {
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
		info := t.generic_aware_call_fn_info(lhs, lhs.name) or { return }
		t.register_inferred_generic_call_spec(lhs.name, info, raw_args)
		return
	}
	if lhs is ast.SelectorExpr {
		if resolved_static := t.resolve_static_type_method_call(lhs.lhs, lhs.rhs.name) {
			info := t.generic_aware_call_fn_info(lhs, resolved_static) or { return }
			t.register_inferred_generic_call_spec(resolved_static, info, raw_args)
			return
		}
		if lhs.lhs is ast.Ident && t.is_module_ident(lhs.lhs.name) {
			mod_name := lhs.lhs.name
			base_name := '${mod_name}__${lhs.rhs.name}'
			info := t.generic_aware_call_fn_info(lhs, base_name) or { return }
			t.register_inferred_generic_call_spec(base_name, info, raw_args)
			return
		}
		if resolved_method := t.resolve_method_call_name(lhs.lhs, lhs.rhs.name) {
			info := t.generic_aware_call_fn_info(lhs, resolved_method) or { CallFnInfo{} }
			t.register_inferred_generic_call_spec(resolved_method, info, raw_args)
			t.register_receiver_generic_method_call_spec(resolved_method, lhs.lhs, info, raw_args)
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
	if lhs is ast.Ident {
		return lhs.name
	}
	if lhs is ast.SelectorExpr {
		if resolved_static := t.resolve_static_type_method_call(lhs.lhs, lhs.rhs.name) {
			return resolved_static
		}
		if lhs.lhs is ast.Ident && t.is_module_ident(lhs.lhs.name) {
			return '${lhs.lhs.name}__${lhs.rhs.name}'
		}
		if resolved_method := t.resolve_method_call_name(lhs.lhs, lhs.rhs.name) {
			return resolved_method
		}
	}
	return none
}

fn (t &Transformer) generic_bindings_from_type_args(info CallFnInfo, type_args []ast.Expr) ?map[string]types.Type {
	if info.generic_params.len == 0 || type_args.len == 0 {
		return none
	}
	mut bindings := map[string]types.Type{}
	for i, generic_param in info.generic_params {
		if i >= type_args.len {
			break
		}
		if type_args[i] is ast.Ident {
			type_arg_ident := type_args[i] as ast.Ident
			if concrete := t.cur_monomorphized_fn_bindings[type_arg_ident.name] {
				bindings[generic_param] = concrete
				continue
			}
		}
		if concrete := t.get_synth_type(type_args[i].pos()) {
			bindings[generic_param] = concrete
			continue
		}
		if concrete := t.lookup_type_from_expr(type_args[i]) {
			bindings[generic_param] = concrete
			continue
		}
		if concrete := t.get_expr_type(type_args[i]) {
			bindings[generic_param] = concrete
			continue
		}
	}
	if bindings.len == 0 {
		return none
	}
	return bindings
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
	decl := t.generic_fn_decl_for_call(base_name) or { return }
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
			bindings[name] = typ
		}
	}
	for param_name in decl_generic_param_names(decl) {
		if param_name !in bindings {
			return
		}
	}
	t.register_generic_bindings(base_name, bindings)
}

fn (t &Transformer) generic_fn_decl_for_call(base_name string) ?ast.FnDecl {
	lookup_key := t.resolve_monomorphize_decl_key(base_name, t.generic_fn_decl_index) or {
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

	base_name := t.generic_call_base_name(lhs) or { return none }
	info := t.generic_aware_call_fn_info(lhs, base_name) or { return none }
	return t.generic_bindings_from_call_args(info, args)
}

fn (t &Transformer) receiver_generic_method_call_name(base_name string, receiver ast.Expr, info CallFnInfo, raw_args []ast.Expr) ?string {
	decl := t.generic_fn_decl_for_call(base_name) or { return none }
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
			bindings[name] = typ
		}
	}
	for param_name in decl_generic_param_names(decl) {
		if param_name !in bindings {
			return none
		}
	}
	mut receiver_prefix := base_name.all_before_last('__')
	if receiver_prefix == '' {
		return none
	}
	if receiver_prefix.contains('_T_') {
		receiver_prefix = receiver_prefix.all_before('_T_')
	}
	method_name := t.specialized_fn_name(decl, bindings)
	if method_name == decl.name {
		return none
	}
	return '${receiver_prefix}__${method_name}'
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
	receiver_type := t.get_expr_type(receiver) or {
		t.declared_expr_type_for_method_receiver(receiver) or { return none }
	}
	concrete_type := t.unwrap_alias_and_pointer_type(receiver_type)
	mut bindings := map[string]types.Type{}
	if concrete_type is types.Struct {
		if template := t.receiver_generic_template_struct(decl, base_name, concrete_type) {
			t.infer_receiver_generic_bindings_from_struct(template, concrete_type, receiver_params, mut
				bindings)
		}
	}
	for param_name in receiver_params {
		if param_name !in bindings {
			return none
		}
	}
	return bindings
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
					return typ
				}
			}
			if st := t.lookup_struct_type_any_module(name) {
				if st.generic_params.len > 0 {
					return st
				}
			}
		}
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
	for _, typ in bindings {
		if clone_type_contains_generic_placeholder(typ) {
			return
		}
	}
	signature := generic_bindings_signature(bindings)
	mut existing := t.env.generic_types[base_name] or { []map[string]types.Type{} }
	for item in existing {
		if generic_bindings_signature(item) == signature {
			return
		}
	}
	existing << bindings.clone()
	t.env.generic_types[base_name] = existing
}

fn (mut t Transformer) register_monomorphized_fn_bindings(module_name string, fn_name string, bindings map[string]types.Type) {
	if fn_name == '' || bindings.len == 0 {
		return
	}
	t.monomorphized_fn_bindings[fn_name] = bindings.clone()
	if module_name != '' {
		t.monomorphized_fn_bindings['${module_name}__${fn_name}'] = bindings.clone()
	}
}

fn (t &Transformer) lookup_monomorphized_fn_bindings(module_name string, fn_name string) ?map[string]types.Type {
	if bindings := t.monomorphized_fn_bindings[fn_name] {
		return bindings.clone()
	}
	if module_name != '' {
		if bindings := t.monomorphized_fn_bindings['${module_name}__${fn_name}'] {
			return bindings.clone()
		}
	}
	return none
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
				name:     substitution_safe_string(typ.name)
				variants: variants
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
		ast.Type {
			return ast.Expr(t.substitute_type_in_type_node(expr, bindings))
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
					base_name := t.get_receiver_type_name(generic_expr)
					if base_name == '' {
						return none
					}
					return ast.Expr(ast.Ident{
						name: base_name
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
pub fn (mut t Transformer) clone_fn_decl_with_substitutions(decl ast.FnDecl, bindings map[string]types.Type, new_name string) ast.FnDecl {
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
	mut new_stmts := []ast.Stmt{cap: decl.stmts.len}
	for st in decl.stmts {
		new_stmts << t.clone_stmt_with_bindings_and_fields(st, bindings, []CloneComptimeFieldCtx{},
			false)
	}
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
	return ast.FnDecl{
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
	if !types.type_has_valid_payload(typ) {
		return false
	}
	match typ {
		types.NamedType {
			return true
		}
		types.Array {
			return clone_type_child_contains_generic_placeholder(typ.elem_type)
		}
		types.ArrayFixed {
			return clone_type_child_contains_generic_placeholder(typ.elem_type)
		}
		types.Map {
			return clone_type_child_contains_generic_placeholder(typ.key_type)
				|| clone_type_child_contains_generic_placeholder(typ.value_type)
		}
		types.Pointer {
			return clone_type_child_contains_generic_placeholder(typ.base_type)
		}
		types.OptionType {
			return clone_type_child_contains_generic_placeholder(typ.base_type)
		}
		types.ResultType {
			return clone_type_child_contains_generic_placeholder(typ.base_type)
		}
		types.Alias {
			return clone_type_child_contains_generic_placeholder(typ.base_type)
		}
		types.Struct {
			for field in typ.fields {
				if clone_type_child_contains_generic_placeholder(field.typ) {
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

fn clone_type_child_contains_generic_placeholder(typ types.Type) bool {
	if !clone_type_has_safe_payload(typ) {
		return false
	}
	return clone_type_contains_generic_placeholder(typ)
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

fn (mut t Transformer) clone_expr_with_bindings_and_fields(expr ast.Expr, bindings map[string]types.Type, contexts []CloneComptimeFieldCtx) ast.Expr {
	match expr {
		ast.Ident {
			pos := t.clone_monomorphized_pos(expr.pos)
			if concrete := bindings[expr.name] {
				return t.type_to_ast_expr(concrete, pos)
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
			mut new_args := []ast.Expr{cap: expr.args.len}
			for a in expr.args {
				new_args << t.clone_expr_with_bindings_and_fields(a, bindings, contexts)
			}
			return ast.Expr(ast.CallExpr{
				lhs:  t.clone_expr_with_bindings_and_fields(expr.lhs, bindings, contexts)
				args: new_args
				pos:  expr.pos
			})
		}
		ast.CallOrCastExpr {
			mut new_expr := ast.empty_expr
			if expr.expr !is ast.EmptyExpr {
				new_expr = t.clone_expr_with_bindings_and_fields(expr.expr, bindings, contexts)
			}
			return ast.Expr(ast.CallOrCastExpr{
				lhs:  t.clone_expr_with_bindings_and_fields(expr.lhs, bindings, contexts)
				expr: new_expr
				pos:  expr.pos
			})
		}
		ast.GenericArgs {
			mut new_args := []ast.Expr{cap: expr.args.len}
			for arg in expr.args {
				new_args << t.substitute_type_in_expr(arg, bindings)
			}
			return ast.Expr(ast.GenericArgs{
				lhs:  t.clone_expr_with_bindings_and_fields(expr.lhs, bindings, contexts)
				args: new_args
				pos:  expr.pos
			})
		}
		ast.GenericArgOrIndexExpr {
			return ast.Expr(ast.GenericArgOrIndexExpr{
				lhs:  t.clone_expr_with_bindings_and_fields(expr.lhs, bindings, contexts)
				expr: t.substitute_type_in_expr(expr.expr, bindings)
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
			return ast.Expr(ast.IndexExpr{
				lhs:      t.clone_expr_with_bindings_and_fields(expr.lhs, bindings, contexts)
				expr:     t.clone_expr_with_bindings_and_fields(expr.expr, bindings, contexts)
				is_gated: expr.is_gated
				pos:      expr.pos
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
			return ast.Expr(ast.InitExpr{
				typ:    t.substitute_type_in_expr(expr.typ, bindings)
				fields: new_fields
				pos:    expr.pos
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
				inters << ast.StringInter{
					format:       inter.format
					width:        inter.width
					precision:    inter.precision
					expr:         t.clone_expr_with_bindings_and_fields(inter.expr, bindings,
						contexts)
					format_expr:  t.clone_expr_with_bindings_and_fields(inter.format_expr,
						bindings, contexts)
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
			return ast.Expr(ast.FnLiteral{
				typ:           expr.typ
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
			return expr
		}
		ast.BasicLiteral, ast.EmptyExpr {
			return expr
		}
		ast.CallExpr {
			mut new_args := []ast.Expr{cap: expr.args.len}
			for a in expr.args {
				new_args << t.clone_expr_with_bindings(a, bindings)
			}
			return ast.Expr(ast.CallExpr{
				lhs:  t.clone_expr_with_bindings(expr.lhs, bindings)
				args: new_args
				pos:  expr.pos
			})
		}
		ast.CallOrCastExpr {
			mut new_expr := ast.empty_expr
			if expr.expr !is ast.EmptyExpr {
				new_expr = t.clone_expr_with_bindings(expr.expr, bindings)
			}
			return ast.Expr(ast.CallOrCastExpr{
				lhs:  t.clone_expr_with_bindings(expr.lhs, bindings)
				expr: new_expr
				pos:  expr.pos
			})
		}
		ast.GenericArgs {
			mut new_args := []ast.Expr{cap: expr.args.len}
			for arg in expr.args {
				new_args << t.substitute_type_in_expr(arg, bindings)
			}
			return ast.Expr(ast.GenericArgs{
				lhs:  t.clone_expr_with_bindings(expr.lhs, bindings)
				args: new_args
				pos:  expr.pos
			})
		}
		ast.GenericArgOrIndexExpr {
			return ast.Expr(ast.GenericArgOrIndexExpr{
				lhs:  t.clone_expr_with_bindings(expr.lhs, bindings)
				expr: t.substitute_type_in_expr(expr.expr, bindings)
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
			return ast.Expr(ast.IndexExpr{
				lhs:      t.clone_expr_with_bindings(expr.lhs, bindings)
				expr:     t.clone_expr_with_bindings(expr.expr, bindings)
				is_gated: expr.is_gated
				pos:      expr.pos
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
			return ast.Expr(ast.InitExpr{
				typ:    t.substitute_type_in_expr(expr.typ, bindings)
				fields: new_fields
				pos:    expr.pos
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
