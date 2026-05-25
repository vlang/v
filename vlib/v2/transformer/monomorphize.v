// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

// Phased refactor: monomorphization is moving from the cleanc backend into the
// transformer. This file holds the foundation primitives used by the future
// pre-pass that will iterate env.generic_types, clone each generic FnDecl with
// concrete types substituted in, rename it (e.g. foo -> foo_T_int), and append
// it to the output stmts. Phase 1 lands these primitives without wiring; Phase
// 2 turns them on behind V2_TRANSFORMER_MONOMORPH=1; Phase 3 deletes cleanc's
// discovery/scan/active_generic_types machinery (~2500 lines).
import v2.ast
import v2.token
import v2.types

// monomorphize_pass walks env.generic_types, clones each generic FnDecl per
// binding map with concrete types substituted, and appends the clones to the
// owning file's stmts. Cleanc consults monomorphized_specs to avoid double-
// emitting the same names through its weak-spec path.
//
// Behind V2_TRANSFORMER_MONOMORPH=1. Returns a new []ast.File where each file
// containing a generic FnDecl has the clones appended.
pub fn (mut t Transformer) monomorphize_pass(files []ast.File) []ast.File {
	// Index generic FnDecls by name. For now, key on bare decl.name only;
	// cross-module dotted keys from env.generic_types are handled by also
	// indexing on '<mod>.<name>'.
	mut decl_owner := map[string]int{} // fn key -> file index
	mut decl_node := map[string]ast.FnDecl{}
	for fi, file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				if stmt.typ.generic_params.len == 0 {
					continue
				}
				decl_owner[stmt.name] = fi
				decl_node[stmt.name] = stmt
				if file.mod != '' {
					qualified := '${file.mod}.${stmt.name}'
					decl_owner[qualified] = fi
					decl_node[qualified] = stmt
				}
			}
		}
	}
	// Per-file accumulator for cloned stmts (keyed by file index).
	mut per_file_clones := map[int][]ast.Stmt{}
	for fn_key, bindings_list in t.env.generic_types {
		decl := decl_node[fn_key] or { continue }
		fi := decl_owner[fn_key] or { continue }
		for bindings in bindings_list {
			spec_name := t.specialized_fn_name(decl, bindings)
			if spec_name == decl.name {
				continue
			}
			if spec_name in t.monomorphized_specs {
				continue
			}
			t.monomorphized_specs[spec_name] = true
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

// substitute_type returns typ with any NamedType placeholder appearing in
// bindings replaced by its concrete binding, recursively.
pub fn substitute_type(typ types.Type, bindings map[string]types.Type) types.Type {
	if bindings.len == 0 {
		return typ
	}
	match typ {
		types.NamedType {
			name := string(typ)
			if concrete := bindings[name] {
				return concrete
			}
			return typ
		}
		types.Pointer {
			return types.Type(types.Pointer{
				lifetime:  typ.lifetime
				base_type: substitute_type(typ.base_type, bindings)
			})
		}
		types.Array {
			return types.Type(types.Array{
				elem_type: substitute_type(typ.elem_type, bindings)
			})
		}
		types.ArrayFixed {
			return types.Type(types.ArrayFixed{
				len:       typ.len
				elem_type: substitute_type(typ.elem_type, bindings)
			})
		}
		types.Map {
			return types.Type(types.Map{
				key_type:   substitute_type(typ.key_type, bindings)
				value_type: substitute_type(typ.value_type, bindings)
			})
		}
		types.OptionType {
			return types.Type(types.OptionType{
				base_type: substitute_type(typ.base_type, bindings)
			})
		}
		types.ResultType {
			return types.Type(types.ResultType{
				base_type: substitute_type(typ.base_type, bindings)
			})
		}
		types.Alias {
			return types.Type(types.Alias{
				name:      typ.name
				base_type: substitute_type(typ.base_type, bindings)
			})
		}
		else {
			return typ
		}
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
		else {
			ident_pos := t.next_synth_pos()
			return ast.Expr(ast.Ident{
				name: typ.name()
				pos:  ident_pos
			})
		}
	}
}

// specialized_fn_name returns the C-style specialized function name for a given
// generic FnDecl plus a concrete type bindings map, e.g. foo + {T:int} -> foo_T_int.
// Mirrors cleanc's specialized_fn_name so a future swap is name-compatible.
pub fn (t &Transformer) specialized_fn_name(decl ast.FnDecl, bindings map[string]types.Type) string {
	generic_param_names := decl_generic_param_names(decl)
	if generic_param_names.len == 0 {
		return decl.name
	}
	mut all_placeholders := true
	mut placeholder_parts := []string{cap: generic_param_names.len}
	mut concrete_parts := []string{cap: generic_param_names.len}
	for gp_name in generic_param_names {
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
			names << gp.name
		}
	}
	return names
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
		new_stmts << t.clone_stmt_with_bindings(st, bindings)
	}
	new_receiver := ast.Parameter{
		name:   decl.receiver.name
		typ:    t.substitute_type_in_expr(decl.receiver.typ, bindings)
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
		else {
			return stmt
		}
	}
}

// clone_expr_with_bindings deep-clones an expression, substituting generic
// types in embedded type positions (CastExpr targets, ArrayType elem types,
// etc.) and recursing through container nodes.
pub fn (mut t Transformer) clone_expr_with_bindings(expr ast.Expr, bindings map[string]types.Type) ast.Expr {
	match expr {
		ast.Ident, ast.BasicLiteral, ast.EmptyExpr {
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
		ast.SelectorExpr {
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
