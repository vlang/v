// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import os
import strings
import v.ast
import v.util

// has[int]() => has_T_int()
// has[int, string]() => has_T_int_string()
const c_fn_name_escape_seq = ['[', '_T_', ', ', '_', ']', '']

// c_manual_prelude_decl_names matches the C stdlib declarations emitted by `c_headers`.
// When one of these symbols is declared in V as `fn C.name(...)`, cgen must not emit an
// extra fallback prototype, or it can conflict with the prelude declaration.
const c_manual_prelude_decl_names = [
	'vfprintf',
	'vsnprintf',
	'fprintf',
	'printf',
	'snprintf',
	'sprintf',
	'sscanf',
	'scanf',
	'puts',
	'perror',
	'fputs',
	'getchar',
	'putchar',
	'getc',
	'fgetc',
	'ungetc',
	'fflush',
	'feof',
	'ferror',
	'clearerr',
	'setvbuf',
	'ftell',
	'rewind',
	'fopen',
	'fdopen',
	'freopen',
	'fileno',
	'fread',
	'fwrite',
	'fgets',
	'fclose',
	'popen',
	'pclose',
	'malloc',
	'calloc',
	'realloc',
	'aligned_alloc',
	'free',
	'rand',
	'srand',
	'atexit',
	'exit',
	'abs',
	'atoi',
	'atof',
	'getenv',
	'setenv',
	'unsetenv',
	'system',
	'remove',
	'rename',
	'realpath',
	'mkstemp',
	'qsort',
	'strcmp',
	'strncmp',
	'strdup',
	'strcasecmp',
	'strncasecmp',
	'strlen',
	'strerror',
	'memcpy',
	'memmove',
	'memset',
	'memcmp',
	'memchr',
	'strchr',
	'strrchr',
	'fseek',
	'getline',
	'__ctype_b_loc',
]

const vinix_c_linker_symbol_names = [
	'text_start',
	'text_end',
	'rodata_start',
	'rodata_end',
	'data_start',
	'data_end',
	'interrupt_thunks',
]

const c_compiler_builtin_decl_names = [
	'__builtin_return_address',
]

fn collect_function_defer_stmts(node &ast.FnDecl) []ast.DeferStmt {
	mut defer_stmts := []ast.DeferStmt{cap: node.defer_stmts.len}
	for defer_stmt in node.defer_stmts {
		if defer_stmt.mode == .function {
			defer_stmts << defer_stmt
		}
	}
	return defer_stmts
}

fn (g &Gen) method_decl_fkey(method ast.Fn) string {
	if method.source_fn != unsafe { nil } {
		fndecl := unsafe { &ast.FnDecl(method.source_fn) }
		return fndecl.fkey()
	}
	return method.fkey()
}

fn (g &Gen) generic_parent_method_fkey(sym ast.TypeSymbol, method_name string) string {
	match sym.info {
		ast.Struct, ast.Interface, ast.SumType {
			if sym.info.parent_type.has_flag(.generic) {
				parent_sym := g.table.sym(sym.info.parent_type)
				if method := parent_sym.find_method(method_name) {
					return g.method_decl_fkey(method)
				}
			}
		}
		ast.GenericInst {
			if sym.info.parent_idx > 0 {
				parent_sym := g.table.sym(ast.idx_to_type(sym.info.parent_idx))
				if method := parent_sym.find_method(method_name) {
					return g.method_decl_fkey(method)
				}
			}
		}
		else {}
	}

	return ''
}

fn (g &Gen) resolve_method_decl_fkey_for_type(typ ast.Type, method_name string) string {
	mut candidate_types := []ast.Type{}
	for candidate in [typ] {
		if candidate == 0 {
			continue
		}
		if candidate !in candidate_types {
			candidate_types << candidate
		}
		unaliased := g.table.unaliased_type(candidate)
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
		sym := g.table.sym(candidate)
		parent_fkey := g.generic_parent_method_fkey(sym, method_name)
		if parent_fkey != '' {
			return parent_fkey
		}
		if method := sym.find_method_with_generic_parent(method_name) {
			return g.method_decl_fkey(method)
		}
		if method := g.table.find_method(sym, method_name) {
			return g.method_decl_fkey(method)
		}
		method, embed_types := g.table.find_method_from_embeds(g.table.final_sym(candidate),
			method_name) or { ast.Fn{}, []ast.Type{} }
		if embed_types.len != 0 {
			return g.method_decl_fkey(method)
		}
	}
	return ''
}

fn free_method_calls_free_on_receiver(method ast.Fn) bool {
	if method.source_fn == unsafe { nil } {
		return false
	}
	fn_decl := unsafe { &ast.FnDecl(method.source_fn) }
	if fn_decl.receiver.name == '' {
		return false
	}
	root := ast.Node(ast.Stmt(*fn_decl))
	return free_method_calls_free_on_receiver_walk(root, fn_decl.receiver.name)
}

fn free_method_calls_free_on_receiver_walk(node ast.Node, receiver_name string) bool {
	if node is ast.Expr {
		match node {
			ast.AnonFn, ast.LambdaExpr {
				return false
			}
			ast.CallExpr {
				if node.name == 'free' && node.args.len == 1
					&& free_method_matches_receiver_expr(node.args[0].expr, receiver_name) {
					return true
				}
			}
			else {}
		}
	}
	for child in node.children() {
		if free_method_calls_free_on_receiver_walk(child, receiver_name) {
			return true
		}
	}
	return false
}

fn free_method_matches_receiver_expr(expr ast.Expr, receiver_name string) bool {
	return match expr {
		ast.Ident { expr.name == receiver_name }
		ast.CastExpr { free_method_matches_receiver_expr(expr.expr, receiver_name) }
		ast.ParExpr { free_method_matches_receiver_expr(expr.expr, receiver_name) }
		else { false }
	}
}

fn (g &Gen) prefers_msvc_compatible_code() bool {
	return g.is_cc_msvc || g.pref.os == .windows
}

fn (mut g Gen) node_decl_fkey(node ast.FnDecl) string {
	if node.is_method && !node.name.contains('_T_') {
		receiver_sym := g.table.sym(node.receiver.typ)
		receiver_has_concrete_generics := match receiver_sym.info {
			ast.Struct, ast.Interface, ast.SumType {
				receiver_sym.info.concrete_types.len > 0 || receiver_sym.generic_types.len > 0
			}
			ast.GenericInst {
				true
			}
			else {
				false
			}
		}

		needs_generic_method_fkey := node.generic_names.len > 0 || node.ninstances > 0
			|| node.receiver.typ.has_flag(.generic)
			|| g.type_has_unresolved_generic_parts(node.receiver.typ)
			|| receiver_has_concrete_generics
		if !needs_generic_method_fkey {
			return node.fkey()
		}
		resolved_fkey := g.resolve_method_decl_fkey_for_type(node.receiver.typ, node.name)
		if resolved_fkey != '' {
			$if trace_decl_fkey ? {
				if node.name in ['call_generic_fn', 'collect', 'next', 'next1', 'next2', 'next3',
					'method', 'another_method'] {
					eprintln('>>> node_decl_fkey resolved: ${node.name} | receiver: ${g.table.type_to_str(node.receiver.typ)} | fkey: ${resolved_fkey}')
				}
			}
			return resolved_fkey
		}
		if generic_method := g.matching_generic_file_method(node) {
			$if trace_decl_fkey ? {
				if node.name in ['call_generic_fn', 'collect', 'next', 'next1', 'next2', 'next3',
					'method', 'another_method'] {
					eprintln('>>> node_decl_fkey matched file generic: ${node.name} | receiver: ${g.table.type_to_str(node.receiver.typ)} | matched receiver: ${g.table.type_to_str(generic_method.receiver.typ)} | fkey: ${generic_method.fkey()}')
				}
			}
			return generic_method.fkey()
		}
	}
	return node.fkey()
}

fn (mut g Gen) method_receiver_signature(typ ast.Type) string {
	if typ == 0 {
		return ''
	}
	base_typ := g.table.unaliased_type(g.unwrap_generic(typ))
	sym := g.table.final_sym(base_typ)
	return '${sym.ngname}|${base_typ.nr_muls()}'
}

fn (mut g Gen) matching_generic_file_method(node ast.FnDecl) ?ast.FnDecl {
	if !node.is_method {
		return none
	}
	node_receiver_sig := g.method_receiver_signature(node.receiver.typ)
	if node_receiver_sig == '' {
		return none
	}
	for generic_fn in g.file.generic_fns {
		if !generic_fn.is_method || generic_fn.name != node.name {
			continue
		}
		$if trace_decl_fkey ? {
			if node.name in ['call_generic_fn', 'collect', 'next', 'next1', 'next2', 'next3',
				'method', 'another_method'] {
				eprintln('>>> matching_generic_file_method: ${node.name} | node receiver: ${g.table.type_to_str(node.receiver.typ)} | node sig: ${node_receiver_sig} | generic receiver: ${g.table.type_to_str(generic_fn.receiver.typ)} | generic sig: ${g.method_receiver_signature(generic_fn.receiver.typ)} | generic fkey: ${generic_fn.fkey()}')
			}
		}
		if g.method_receiver_signature(generic_fn.receiver.typ) == node_receiver_sig {
			return ast.FnDecl{
				...*generic_fn
			}
		}
	}
	return none
}

fn (g &Gen) generic_file_fn_by_fkey(fkey string) ?ast.FnDecl {
	for generic_fn in g.file.generic_fns {
		if generic_fn.fkey() == fkey {
			return ast.FnDecl{
				...*generic_fn
			}
		}
	}
	return none
}

// find_all_receiver_concrete_types finds all concrete type instantiations
// of the receiver's generic type by looking up GenericInst types in the type table.
// find_all_receiver_concrete_types finds all concrete type instantiations
// of the receiver's generic type by examining registered concrete types of
// sibling methods on the same receiver that have complete type sets.
fn (g &Gen) find_all_receiver_concrete_types(node ast.FnDecl, receiver_generic_count int) [][]ast.Type {
	receiver_typ_int := int(node.receiver.typ)
	prefix := '${receiver_typ_int}.'
	mut result := [][]ast.Type{}
	mut seen := map[string]bool{}
	// Look through all registered method generic types on this receiver
	for fkey, type_sets in g.table.fn_generic_types {
		if !fkey.starts_with(prefix) {
			continue
		}
		for concrete_types in type_sets {
			if concrete_types.any(it.has_flag(.generic)) {
				continue
			}
			// We need sets that have both receiver + method generics
			if concrete_types.len > receiver_generic_count {
				receiver_part := concrete_types[..receiver_generic_count]
				key := receiver_part.str()
				if key !in seen {
					seen[key] = true
					result << receiver_part
				}
			}
		}
	}
	// Fallback: if no sibling methods have complete type sets, look for
	// concrete instantiations of the receiver type in the type table
	// (GenericInst types whose parent matches the receiver).
	if result.len == 0 {
		receiver_parent := node.receiver.typ.set_nr_muls(0).set_flag(.generic)
		receiver_parent_sym := g.table.sym(receiver_parent)
		receiver_base_name := receiver_parent_sym.name.all_before('<')
		for _, sym in g.table.type_symbols {
			if sym.info is ast.Struct {
				if sym.info.concrete_types.len == receiver_generic_count
					&& (sym.info.parent_type == receiver_parent
					|| (sym.info.parent_type != 0
					&& g.table.sym(sym.info.parent_type).name.all_before('<') == receiver_base_name)) {
					cts := sym.info.concrete_types
					if !cts.any(it.has_flag(.generic)) {
						key := cts.str()
						if key !in seen {
							seen[key] = true
							result << cts.clone()
						}
					}
				}
			}
		}
	}
	return result
}

fn (mut g Gen) effective_fn_generic_names(node ast.FnDecl) []string {
	if node.generic_names.len > 0 {
		return node.generic_names.clone()
	}
	if !node.is_method {
		return []string{}
	}
	resolved_fkey := g.node_decl_fkey(node)
	if generic_method := g.generic_file_fn_by_fkey(resolved_fkey) {
		if generic_method.generic_names.len > 0 {
			$if trace_decl_fkey ? {
				if node.name in ['call_generic_fn', 'collect', 'next', 'next1', 'next2', 'next3',
					'method', 'another_method'] {
					eprintln('>>> effective_fn_generic_names from file generic: ${node.name} | receiver: ${g.table.type_to_str(node.receiver.typ)} | generic_names: ${generic_method.generic_names}')
				}
			}
			return generic_method.generic_names.clone()
		}
		receiver_generic_names := g.table.generic_type_names(generic_method.receiver.typ)
		if receiver_generic_names.len > 0 {
			$if trace_decl_fkey ? {
				if node.name in ['call_generic_fn', 'collect', 'next', 'next1', 'next2', 'next3',
					'method', 'another_method'] {
					eprintln('>>> effective_fn_generic_names from generic receiver: ${node.name} | receiver: ${g.table.type_to_str(generic_method.receiver.typ)} | generic_names: ${receiver_generic_names}')
				}
			}
			return receiver_generic_names
		}
	}
	receiver_generic_names := g.table.generic_type_names(node.receiver.typ)
	if receiver_generic_names.len == 0 {
		return []string{}
	}
	if g.table.fn_generic_types[g.node_decl_fkey(node)].len > 0 {
		return receiver_generic_names
	}
	return []string{}
}

// method_concrete_types_for_name returns the concrete types for the generic
// name suffix in c_fn_name. For methods where cur_concrete_types includes
// merged receiver + method types that were synthesized in gen_fn_decl
// (because the checker only registered method-own types), strip the
// receiver's types from the suffix since they're already encoded in the
// receiver type name (e.g., Foo_T_string).
fn (mut g Gen) method_concrete_types_for_name(node &ast.FnDecl) []ast.Type {
	if !node.is_method || g.cur_concrete_types.len == 0 {
		return g.cur_concrete_types
	}
	receiver_generic_names := g.table.generic_type_names(node.receiver.typ)
	if receiver_generic_names.len == 0 || g.cur_concrete_types.len <= receiver_generic_names.len {
		return g.cur_concrete_types
	}
	// Check if the checker registered incomplete types (method-only) or
	// complete types (receiver + method). If the registered types are
	// already complete, don't strip anything.
	// Always use the full concrete types (receiver + method). The call site
	// is responsible for generating matching names via method_name_concrete_types.
	return g.cur_concrete_types
}

fn (mut g Gen) resolve_current_fn_generic_type(typ ast.Type) ast.Type {
	if typ == 0 {
		return 0
	}
	_, generic_names := g.current_fn_generic_params()
	if generic_names.len == 0 || g.cur_concrete_types.len == 0 {
		return g.unwrap_generic(typ)
	}
	mut muttable := unsafe { &ast.Table(g.table) }
	if resolved := muttable.convert_generic_type(typ, generic_names, g.cur_concrete_types) {
		return g.unwrap_generic(resolved)
	}
	return g.unwrap_generic(g.recheck_concrete_type(typ))
}

fn (mut g Gen) resolved_call_concrete_type(typ ast.Type) ast.Type {
	return ast.mktyp(g.resolve_current_fn_generic_type(typ))
}

fn (mut g Gen) receiver_concrete_types_for_type(typ ast.Type) []ast.Type {
	if typ == 0 {
		return []ast.Type{}
	}
	sym := g.table.sym(typ)
	match sym.info {
		ast.Struct, ast.Interface, ast.SumType {
			mut concrete_types := sym.info.concrete_types.clone()
			if concrete_types.len == 0 && sym.generic_types.len == sym.info.generic_types.len
				&& sym.generic_types != sym.info.generic_types {
				concrete_types = sym.generic_types.clone()
			}
			return concrete_types.map(g.unwrap_generic(it))
		}
		ast.GenericInst {
			return sym.info.concrete_types.map(g.unwrap_generic(it))
		}
		else {
			return []ast.Type{}
		}
	}
}

fn (mut g Gen) method_receiver_only_specialization_types(method ast.Fn, concrete_receiver_type ast.Type) []ast.Type {
	mut receiver_concrete_types := g.receiver_concrete_types_for_type(concrete_receiver_type)
	if receiver_concrete_types.len == 0 {
		receiver_concrete_types =
			g.receiver_concrete_types_for_type(g.recheck_concrete_type(concrete_receiver_type))
	}
	mut parent_method := ast.Fn{}
	_, parent_method = g.receiver_generic_call_context(concrete_receiver_type, method.name)
	if parent_method.params.len == 0 {
		_, parent_method = g.receiver_generic_call_context(g.recheck_concrete_type(concrete_receiver_type),
			method.name)
	}
	method_receiver_generic_names := if method.params.len > 0 {
		g.table.generic_type_names(method.params[0].typ)
	} else {
		[]string{}
	}
	parent_receiver_generic_names := if parent_method.params.len > 0 {
		g.table.generic_type_names(parent_method.params[0].typ)
	} else {
		[]string{}
	}
	full_method := if parent_method.generic_names.len > method.generic_names.len
		|| (method_receiver_generic_names.len == 0 && parent_receiver_generic_names.len > 0) {
		parent_method
	} else {
		method
	}
	if receiver_concrete_types.len == 0 || full_method.params.len == 0 {
		return []ast.Type{}
	}
	receiver_generic_names := g.table.generic_type_names(full_method.params[0].typ)
	if receiver_generic_names.len == 0 {
		if receiver_concrete_types.len > 0
			&& full_method.generic_names.len == receiver_concrete_types.len {
			return receiver_concrete_types.clone()
		}
		return []ast.Type{}
	}
	if full_method.generic_names.len == 0
		|| (full_method.generic_names.len == receiver_generic_names.len
		&& full_method.generic_names == receiver_generic_names) {
		return receiver_concrete_types.clone()
	}
	return []ast.Type{}
}

fn (mut g Gen) specialized_method_name_from_receiver(method ast.Fn, concrete_receiver_type ast.Type, base_name string) string {
	specialization_types := g.method_receiver_only_specialization_types(method,
		concrete_receiver_type)
	if specialization_types.len == 0 {
		return base_name
	}
	specialized_suffix := g.generic_fn_name(specialization_types, '')
	if specialized_suffix != '' && base_name.ends_with(specialized_suffix) {
		return base_name
	}
	return g.generic_fn_name(specialization_types, base_name)
}

fn (mut g Gen) specialized_method_name_from_receiver_context(method ast.Fn, receiver_concrete_types []ast.Type, parent_method ast.Fn, base_name string) string {
	if receiver_concrete_types.len == 0 || method.params.len == 0 {
		return base_name
	}
	method_receiver_generic_names := g.table.generic_type_names(method.params[0].typ)
	parent_receiver_generic_names := if parent_method.params.len > 0 {
		g.table.generic_type_names(parent_method.params[0].typ)
	} else {
		[]string{}
	}
	full_method := if parent_method.generic_names.len > method.generic_names.len
		|| (method_receiver_generic_names.len == 0 && parent_receiver_generic_names.len > 0) {
		parent_method
	} else {
		method
	}
	receiver_generic_names := g.table.generic_type_names(full_method.params[0].typ)
	if receiver_generic_names.len == 0 {
		if full_method.generic_names.len != receiver_concrete_types.len {
			return base_name
		}
	} else if full_method.generic_names.len > 0
		&& (full_method.generic_names.len != receiver_generic_names.len
		|| full_method.generic_names != receiver_generic_names) {
		return base_name
	}
	specialized_suffix := g.generic_fn_name(receiver_concrete_types, '')
	if specialized_suffix != '' && base_name.ends_with(specialized_suffix) {
		return base_name
	}
	return g.generic_fn_name(receiver_concrete_types, base_name)
}

fn (mut g Gen) recover_method_call_concrete_types_from_name(raw_method_name string, method_name string, receiver_type ast.Type, method_for_generics ast.Fn, parent_generic_method ast.Fn) []ast.Type {
	if raw_method_name == '' || method_name == ''
		|| !raw_method_name.starts_with(method_name + '_T_') {
		return []ast.Type{}
	}
	method_receiver_generic_names := if method_for_generics.params.len > 0 {
		g.table.generic_type_names(method_for_generics.params[0].typ)
	} else {
		[]string{}
	}
	parent_receiver_generic_names := if parent_generic_method.params.len > 0 {
		g.table.generic_type_names(parent_generic_method.params[0].typ)
	} else {
		[]string{}
	}
	full_method := if
		parent_generic_method.generic_names.len > method_for_generics.generic_names.len
		|| (method_receiver_generic_names.len == 0 && parent_receiver_generic_names.len > 0) {
		parent_generic_method
	} else {
		method_for_generics
	}
	mut full_fkey := g.resolve_method_decl_fkey_for_type(receiver_type, method_name)
	if full_fkey == '' {
		full_fkey = g.method_decl_fkey(full_method)
	}
	if full_fkey == '' {
		return []ast.Type{}
	}
	for candidate in g.table.fn_generic_types[full_fkey] {
		if candidate.any(it.has_flag(.generic) || g.type_has_unresolved_generic_parts(it)) {
			continue
		}
		name_concrete_types := candidate.map(g.unwrap_generic(it))
		if g.generic_fn_name(name_concrete_types, method_name) == raw_method_name {
			return candidate.map(g.unwrap_generic(it))
		}
	}
	return []ast.Type{}
}

fn (mut g Gen) method_name_concrete_types(full_fkey string, concrete_types []ast.Type, receiver_concrete_types []ast.Type) []ast.Type {
	if full_fkey == '' || concrete_types.len == 0 {
		return concrete_types.clone()
	}
	normalized_concrete_types := concrete_types.map(g.unwrap_generic(it))
	normalized_receiver_types := receiver_concrete_types.map(g.unwrap_generic(it))
	mut matching_partial_candidate := []ast.Type{}
	for candidate in g.table.fn_generic_types[full_fkey] {
		if candidate.any(it.has_flag(.generic) || g.type_has_unresolved_generic_parts(it)) {
			continue
		}
		normalized_candidate := candidate.map(g.unwrap_generic(it))
		if normalized_candidate == normalized_concrete_types {
			return normalized_candidate
		}
		if normalized_receiver_types.len > 0
			&& normalized_candidate.len + normalized_receiver_types.len == normalized_concrete_types.len
			&& normalized_concrete_types[..normalized_receiver_types.len] == normalized_receiver_types
			&& normalized_concrete_types[normalized_receiver_types.len..] == normalized_candidate {
			// Return the full types (receiver + method) to match the function
			// definition which always uses complete types.
			return normalized_concrete_types
		}
		if normalized_candidate.len > normalized_concrete_types.len
			&& normalized_candidate[..normalized_concrete_types.len] == normalized_concrete_types {
			if matching_partial_candidate.len == 0 {
				matching_partial_candidate = normalized_candidate.clone()
			} else if matching_partial_candidate != normalized_candidate {
				return normalized_concrete_types
			}
		}
	}
	if matching_partial_candidate.len > 0 {
		return matching_partial_candidate
	}
	return normalized_concrete_types
}

fn (mut g Gen) raw_method_name_concrete_types(raw_method_name string, method_name string, raw_concrete_types []ast.Type, receiver_concrete_types []ast.Type) []ast.Type {
	if raw_method_name == '' || method_name == '' || raw_concrete_types.len == 0
		|| !raw_method_name.starts_with(method_name + '_T_') {
		return []ast.Type{}
	}
	mut resolved_raw_concrete_types := []ast.Type{}
	for concrete_type in raw_concrete_types {
		resolved_concrete_type := g.resolved_call_concrete_type(concrete_type)
		if resolved_concrete_type == 0 || resolved_concrete_type.has_flag(.generic)
			|| g.type_has_unresolved_generic_parts(resolved_concrete_type) {
			return []ast.Type{}
		}
		resolved_raw_concrete_types << resolved_concrete_type
	}
	if g.generic_fn_name(resolved_raw_concrete_types, method_name) == raw_method_name {
		return resolved_raw_concrete_types
	}
	if receiver_concrete_types.len > 0
		&& resolved_raw_concrete_types.len > receiver_concrete_types.len {
		method_only_types := resolved_raw_concrete_types[receiver_concrete_types.len..].clone()
		if g.generic_fn_name(method_only_types, method_name) == raw_method_name {
			return method_only_types
		}
	}
	if raw_concrete_types.any(it.has_flag(.generic) || g.type_has_unresolved_generic_parts(it)) {
		return resolved_raw_concrete_types
	}
	return []ast.Type{}
}

fn (g &Gen) raw_method_name_suffix(raw_method_name string, method_name string, generic_names []string) string {
	if raw_method_name == '' || method_name == ''
		|| !raw_method_name.starts_with(method_name + '_T_') {
		return ''
	}
	suffix_tokens := raw_method_name[method_name.len + 3..].split('_').filter(it.len > 0)
	for suffix_token in suffix_tokens {
		if suffix_token in generic_names {
			return ''
		}
	}
	return raw_method_name[method_name.len..]
}

fn (mut g Gen) is_used_by_main(node ast.FnDecl) bool {
	$if trace_unused_by_main ? {
		defer(fn) {
			used_by_main := $res()
			if !used_by_main {
				fkey := node.fkey()
				println('> trace_unused_by_main: mod: ${node.mod} | ${node.name} | fkey: ${fkey} | line_nr: ${node.pos.line_nr}')
			}
		}
	}
	if g.should_emit_c_fallback_decl(node) {
		return true
	}
	if node.is_c_extern {
		return true
	}
	if node.is_method && node.name in ['[]', '[]='] {
		return true
	}
	if node.is_test && node.name.all_after_last('.') in ['before_each', 'after_each'] {
		return true
	}
	if node.mod == 'builtin' && node.name in ['print', 'println', 'eprint', 'eprintln'] {
		return true
	}
	mut is_used_by_main := true
	if g.pref.skip_unused {
		if node.is_markused {
			// TODO for some reason markused walker doesn't set used_fns[key] true for
			// [markused] fndecls
			return true
		}
		fkey := g.node_decl_fkey(node)
		is_used_by_main = g.table.used_features.used_fns[fkey]
		if !is_used_by_main && g.effective_fn_generic_names(node).len > 0
			&& g.table.fn_generic_types[fkey].len > 0 {
			is_used_by_main = true
		}
		if !is_used_by_main && node.generic_names.len == 0 {
			recovered_generic_names, recovered_concrete_types :=
				g.recover_specialized_generic_context_for(node.name)
			if recovered_generic_names.len > 0
				&& recovered_generic_names.len == recovered_concrete_types.len {
				for generic_fn in g.file.generic_fns {
					if generic_fn.generic_names.len == 0 {
						continue
					}
					for base_name in [generic_fn.name, generic_fn.fkey()] {
						if g.generic_fn_name(recovered_concrete_types, base_name) == node.name {
							is_used_by_main = g.table.used_features.used_fns[generic_fn.fkey()]
							if is_used_by_main {
								break
							}
						}
					}
					if is_used_by_main {
						break
					}
				}
			}
			if !is_used_by_main && node.name.contains('_T_') {
				is_used_by_main = true
			}
		}
		$if trace_skip_unused_fns ? {
			println('> is_used_by_main: ${is_used_by_main} | node.name: ${node.name} | fkey: ${fkey} | node.is_method: ${node.is_method}')
		}
		if !is_used_by_main && node.is_method {
			receiver_type := g.table.final_type(node.receiver.typ.set_nr_muls(0))
			for isym in g.table.type_symbols {
				if isym.kind != .interface || isym.info !is ast.Interface {
					continue
				}
				if isym.idx !in g.table.used_features.used_syms {
					continue
				}
				inter_info := isym.info as ast.Interface
				impl_types := inter_info.implementor_types(true)
				if receiver_type !in impl_types {
					continue
				}
				mut interface_requires_method := false
				if interface_method := isym.find_method(node.name) {
					interface_requires_method = interface_method.no_body
				} else if interface_method := isym.find_method_with_generic_parent(node.name) {
					interface_requires_method = interface_method.no_body
				}
				if interface_requires_method {
					is_used_by_main = true
					break
				}
			}
		}
		if !is_used_by_main {
			$if trace_skip_unused_fns_in_c_code ? {
				g.writeln('// trace_skip_unused_fns_in_c_code, ${node.name}, fkey: ${fkey}')
			}
		}
	} else {
		$if trace_skip_unused_fns_in_c_code ? {
			g.writeln('// trace_skip_unused_fns_in_c_code, ${node.name}, fkey: ${node.fkey()}')
		}
	}
	return is_used_by_main
}

fn file_has_c_includes(file &ast.File) bool {
	if file == unsafe { nil } {
		return false
	}
	for stmt in file.stmts {
		if stmt is ast.HashStmt && stmt.kind in ['include', 'preinclude'] {
			return true
		}
	}
	return false
}

fn modules_with_c_includes(files []&ast.File) map[string]bool {
	mut mods := map[string]bool{}
	for file in files {
		if file_has_c_includes(file) {
			mods[file.mod.name] = true
			if file.mod.name.ends_with('.c') {
				mods[file.mod.name.all_before_last('.')] = true
			}
		}
	}
	return mods
}

fn file_imports_c_header_module(file &ast.File) bool {
	if file == unsafe { nil } {
		return false
	}
	for imp in file.imports {
		if imp.source_name.ends_with('.c') {
			return true
		}
	}
	return false
}

fn (g &Gen) module_has_c_header_module(file &ast.File) bool {
	if file_imports_c_header_module(file) {
		return true
	}
	if file == unsafe { nil } || g.table == unsafe { nil } || file.path == '' {
		return false
	}
	helper_dir := os.join_path(os.dir(file.path), 'c')
	for path in g.table.filelist {
		if path.starts_with(helper_dir + os.path_separator) {
			return true
		}
	}
	return false
}

fn (g &Gen) c_prelude_provides_decl(c_sym_name string) bool {
	return !g.pref.no_preludes && !g.pref.is_bare && c_sym_name in c_manual_prelude_decl_names
}

fn (g &Gen) should_emit_c_fallback_decl(node ast.FnDecl) bool {
	c_sym_name := node.name.all_after_first('C__').all_after_first('C.')
	if c_sym_name in c_compiler_builtin_decl_names {
		return false
	}
	if g.pref.os == .vinix && c_sym_name in vinix_c_linker_symbol_names {
		return false
	}
	if node.language != .c || node.is_c_extern || file_has_c_includes(node.source_file)
		|| node.mod in g.mods_with_c_includes || g.module_has_c_header_module(node.source_file)
		|| g.c_prelude_provides_decl(c_sym_name) {
		return false
	}
	if node.source_file == unsafe { nil } {
		return true
	}
	if !node.source_file.path.starts_with(g.pref.vlib) {
		return true
	}
	return node.mod == 'main' || node.source_file.is_test
}

fn (g &Gen) should_emit_c_signature_type_decls(node ast.FnDecl) bool {
	return node.is_c_extern || g.should_emit_c_fallback_decl(node)
		|| (node.no_body && node.attrs.contains('c'))
}

fn (mut g Gen) ensure_c_extern_signature_type_decls(node ast.FnDecl) {
	if !g.pref.skip_unused || !g.should_emit_c_signature_type_decls(node) {
		return
	}
	g.ensure_c_extern_signature_type_decl(node.return_type)
	for param in node.params {
		g.ensure_c_extern_signature_type_decl(param.typ)
	}
}

fn (mut g Gen) ensure_c_extern_signature_type_decl(typ_ ast.Type) {
	if typ_ == 0 {
		return
	}
	typ := typ_.clear_option_and_result().set_nr_muls(0).clear_flags(.generic, .variadic)
	if typ == 0 {
		return
	}
	sym := g.table.sym(typ)
	if sym.is_builtin || sym.name in ['byte', 'i32', 'C.FILE'] {
		return
	}
	if sym.idx in g.table.used_features.used_syms {
		return
	}
	if sym.cname in g.c_extern_signature_types {
		return
	}
	g.c_extern_signature_types[sym.cname] = true
	match sym.info {
		ast.Alias {
			g.ensure_c_extern_signature_type_decl(sym.info.parent_type)
			g.write_alias_typesymbol_declaration(sym)
		}
		ast.FnType {
			for param in sym.info.func.params {
				g.ensure_c_extern_signature_type_decl(param.typ)
			}
			g.ensure_c_extern_signature_type_decl(sym.info.func.return_type)
		}
		ast.Struct {
			if sym.language == .c && sym.cname.starts_with('C__') && !sym.info.is_anon {
				c_struct_name := sym.cname[3..]
				if sym.info.is_typedef {
					g.typedefs.writeln('typedef struct ${c_struct_name} ${c_struct_name};')
				} else {
					g.typedefs.writeln('struct ${c_struct_name};')
				}
			} else {
				g.typedefs.writeln('typedef struct ${sym.cname} ${sym.cname};')
			}
		}
		ast.Array {
			g.ensure_c_extern_signature_type_decl(sym.info.elem_type)
		}
		ast.ArrayFixed {
			g.ensure_c_extern_signature_type_decl(sym.info.elem_type)
		}
		else {}
	}
}

fn (mut g Gen) fn_decl(node ast.FnDecl) {
	$if trace_cgen_fn_decl ? {
		eprintln('>   g.tid: ${g.tid} | g.fid: ${g.fid:3} | g.file.path: ${g.file.path} | fn_decl: ${node.name}')
	}
	if node.should_be_skipped {
		return
	}
	if node.is_test {
		g.test_function_names << node.name
	}
	effective_generic_names := g.effective_fn_generic_names(node)
	if node.ninstances == 0 && effective_generic_names.len > 0
		&& g.table.fn_generic_types[g.node_decl_fkey(node)].len == 0 {
		$if trace_generics ? {
			eprintln('skipping generic fn with no concrete instances: ${node.mod} ${node.name}')
		}
		return
	}
	if !g.is_used_by_main(node) {
		return
	}
	g.ensure_c_extern_signature_type_decls(node)
	if g.is_builtin_mod && g.pref.gc_mode == .boehm_leak && node.kind == .malloc {
		g.definitions.write_string('#define builtin___v_malloc GC_MALLOC\n')
		return
	}

	if g.pref.parallel_cc {
		if node.is_anon {
			// g.write('static ')
			// g.definitions.write_string('static ')
		}
		if !node.is_anon {
			g.out_fn_start_pos << g.out.len
		}
	}
	prev_is_direct_array_access := g.is_direct_array_access
	g.is_direct_array_access = node.is_direct_arr || g.pref.no_bounds_checking
	defer {
		g.is_direct_array_access = prev_is_direct_array_access
	}

	// handle `@[ignore_overflow] fn abc() {}` and -check-overflow :
	prev_do_int_overflow_checks := g.do_int_overflow_checks
	g.do_int_overflow_checks = g.pref.is_check_overflow && !g.is_builtin_overflow_mod
		&& !node.is_ignore_overflow
	defer {
		g.do_int_overflow_checks = prev_do_int_overflow_checks
	}

	// Emit extern prototypes for headerless `fn C.some_name() int`
	// declarations. Header-backed C declarations keep relying on the included
	// prototypes to avoid redeclaration conflicts.
	old_inside_c_extern := g.inside_c_extern
	defer {
		g.inside_c_extern = old_inside_c_extern
	}
	if node.is_c_extern || g.should_emit_c_fallback_decl(node) {
		g.inside_c_extern = true
	}

	g.gen_attrs(node.attrs)
	mut skip := false
	pos := g.out.len
	defs_pos := g.definitions.len
	should_bundle_module := util.should_bundle_module(node.mod)
	if g.pref.build_mode == .build_module {
		// TODO: true for not just "builtin"
		// TODO: clean this up
		mod := if g.is_builtin_mod { 'builtin' } else { node.name.all_before_last('.') }
		module_built_short := g.module_built.all_after_last('/').all_after_last('.')
		// for now dont skip generic functions as they are being marked as static
		// when -usecache is enabled, until a better solution is implemented.
		if ((mod != g.module_built && node.mod != g.module_built && node.mod != module_built_short)
			|| should_bundle_module) && node.generic_names.len == 0 {
			// Skip functions that don't have to be generated for this module.
			// println('skip bm ${node.name} mod=${node.mod} module_built=${g.module_built}')
			skip = true
		}
		if g.is_builtin_mod && g.module_built == 'builtin' && node.mod == 'builtin' {
			skip = false
		}
		if !skip && g.pref.is_verbose {
			println('build module `${g.module_built}` fn `${node.name}`')
		}
	}
	if g.pref.use_cache {
		// We are using prebuilt modules, we do not need to generate
		// their functions in main.c.
		if node.mod != 'main' && node.mod != 'help' && !should_bundle_module && !g.pref.is_test
			&& node.generic_names.len == 0 {
			skip = true
		}
	}
	keep_fn_decl := g.fn_decl
	unsafe {
		g.fn_decl = &node
	}
	if node.is_main {
		g.has_main = true
	}
	// TODO: PERF remove this from here
	is_backtrace := node.name.starts_with('backtrace')
		&& node.name in ['backtrace_symbols', 'backtrace', 'backtrace_symbols_fd']
	if is_backtrace {
		g.write('\n#ifndef __cplusplus\n')
	}
	g.gen_fn_decl(node, skip)
	if is_backtrace {
		g.write('\n#endif\n')
	}
	g.fn_decl = keep_fn_decl
	if skip {
		g.go_back_to(pos)
		if g.pref.build_mode != .build_module && !g.pref.use_cache {
			g.definitions.go_back_to(defs_pos)
		}
	}
	if !g.pref.skip_unused {
		if node.language != .c {
			g.writeln('')
		}
	}
	// Write the next function into another parallel C file
	// g.out_idx++
	// if g.out_idx >= g.out_parallel.len {
	// g.out_idx = 0
	//}
}

fn (mut g Gen) post_process_generic_fns_for_files(files []&ast.File) {
	old_file := g.file
	old_fid := g.fid
	old_cur_concrete_types := g.cur_concrete_types.clone()
	mut emitted_generic_specializations := map[string]bool{}
	for {
		mut emitted_this_round := false
		for fid, file in files {
			g.fid = fid
			g.file = file
			for generic_fn in file.generic_fns {
				if generic_fn.is_anon {
					continue
				}
				effective_generic_names := g.effective_fn_generic_names(*generic_fn)
				if effective_generic_names.len == 0 {
					continue
				}
				fkey := g.node_decl_fkey(*generic_fn)
				generic_types_by_fn := g.table.fn_generic_types[fkey].clone()
				if generic_types_by_fn.len == 0 {
					continue
				}
				mut receiver_generic_names := []string{}
				mut all_receiver_cts := [][]ast.Type{}
				if generic_fn.is_method {
					receiver_generic_names = g.table.generic_type_names(generic_fn.receiver.typ)
					if receiver_generic_names.len > 0
						&& receiver_generic_names.len < effective_generic_names.len {
						all_receiver_cts = g.find_all_receiver_concrete_types(*generic_fn,
							receiver_generic_names.len)
					}
				}
				for concrete_types in generic_types_by_fn {
					if concrete_types.any(it.has_flag(.generic)
						|| g.type_has_unresolved_generic_parts(it))
					{
						continue
					}
					mut pending_specializations := [][]ast.Type{}
					if concrete_types.len == effective_generic_names.len {
						pending_specializations << concrete_types.clone()
					} else if concrete_types.len < effective_generic_names.len
						&& all_receiver_cts.len > 0
						&& concrete_types.len + receiver_generic_names.len == effective_generic_names.len {
						for receiver_cts in all_receiver_cts {
							mut merged := receiver_cts.clone()
							merged << concrete_types
							pending_specializations << merged
						}
					}
					for concrete_specialization in pending_specializations {
						specialization_key := g.generic_fn_name(concrete_specialization, fkey)
						if specialization_key in emitted_generic_specializations {
							continue
						}
						emitted_generic_specializations[specialization_key] = true
						g.cur_concrete_types = concrete_specialization.clone()
						g.fn_decl(*generic_fn)
						emitted_this_round = true
					}
				}
			}
		}
		if !emitted_this_round {
			break
		}
		g.cur_concrete_types = []
	}
	g.cur_concrete_types = old_cur_concrete_types
	g.file = old_file
	g.fid = old_fid
}

fn (mut g Gen) gen_fn_decl(node &ast.FnDecl, skip bool) {
	$if trace_cgen_gen_fn_decl ? {
		eprintln('>   g.tid: ${g.tid} | g.fid: ${g.fid:3} | g.file.path: ${g.file.path} | gen_fn_decl: ${node.name} | skip: ${skip}')
	}
	// TODO: For some reason, build fails with autofree with this line
	// as it's only informative, comment it for now
	// g.gen_attrs(it.attrs)
	if node.language == .c && !g.inside_c_extern {
		return
	}
	c_sym_name := node.name.all_after_first('C__').all_after_first('C.')
	if node.language == .c && c_sym_name in ['va_start', 'va_arg', 'va_end', 'va_copy'] {
		return
	}
	old_is_vlines_enabled := g.is_vlines_enabled
	g.is_vlines_enabled = true
	defer {
		g.is_vlines_enabled = old_is_vlines_enabled
	}
	function_defer_stmts := collect_function_defer_stmts(node)

	tmp_defer_vars := g.defer_vars // must be here because of workflow
	if g.anon_fn == unsafe { nil } {
		g.defer_vars = []string{}
	} else {
		if function_defer_stmts.len > 0 {
			g.defer_vars = []string{}
			defer(fn) {
				g.defer_vars = tmp_defer_vars
			}
		}
	}
	// Skip [if xxx] if xxx is not defined
	/*
	for attr in node.attrs {
		if !attr.is_comptime_define {
			continue
		}
		if attr.name !in g.pref.compile_defines_all {
			// println('skipping [if]')
			return
		}
	}
	*/

	g.returned_var_names.clear()
	old_g_autofree := g.is_autofree
	if node.is_manualfree {
		g.is_autofree = false
	}
	defer {
		g.is_autofree = old_g_autofree
	}
	effective_generic_names := g.effective_fn_generic_names(*node)
	if effective_generic_names.len > 0 && g.cur_concrete_types.len == 0 {
		return
	}
	// For methods with their own generics (e.g. fn (c Calc[S]) next[T](input T))
	// where cur_concrete_types only contains receiver generics (S=TypeA),
	// skip if cur_concrete_types doesn't match the expected number of generic names.
	if effective_generic_names.len > 0 && g.cur_concrete_types.len > 0
		&& g.cur_concrete_types.len != effective_generic_names.len {
		return
	}
	cur_fn_save := g.cur_fn
	cur_concrete_types_save := g.cur_concrete_types.clone()
	defer {
		g.cur_fn = cur_fn_save
		g.cur_concrete_types = cur_concrete_types_save
		g.clear_type_resolution_caches()
	}
	mut effective_cur_fn := *node
	if g.cur_concrete_types.len > 0 && effective_generic_names.len == g.cur_concrete_types.len
		&& effective_cur_fn.generic_names != effective_generic_names {
		effective_cur_fn = ast.FnDecl{
			...effective_cur_fn
			generic_names: effective_generic_names.clone()
		}
	}
	if node.generic_names.len == 0 && g.cur_concrete_types.len == 0
		&& (!node.is_method || node.receiver.typ.has_flag(.generic)
		|| node.name.contains('_T_')) {
		recovered_generic_names, recovered_concrete_types :=
			g.recover_specialized_generic_context_for(node.name)
		if recovered_generic_names.len > 0
			&& recovered_generic_names.len == recovered_concrete_types.len {
			effective_cur_fn = ast.FnDecl{
				...*node
				generic_names: recovered_generic_names.clone()
			}
			g.cur_concrete_types = recovered_concrete_types.clone()
		}
	}
	unsafe {
		// TODO: remove unsafe
		g.cur_fn = &effective_cur_fn
	}
	g.clear_type_resolution_caches()
	g.refresh_current_generic_fn_scope_vars(g.cur_fn)
	fn_start_pos := g.out.len
	is_closure := node.scope.has_inherited_vars()
	mut cur_closure_ctx := ''
	if is_closure {
		cur_closure_ctx = g.closure_ctx(node)
		// declare the struct before its implementation
		g.definitions.write_string(cur_closure_ctx)
		g.definitions.writeln(';')
	}

	g.write_v_source_line_info_stmt(node)
	fn_attrs := g.write_fn_attrs(node.attrs)
	// Live
	is_livefn := node.attrs.contains('live')
	is_livemain := g.pref.is_livemain && is_livefn
	is_liveshared := g.pref.is_liveshared && is_livefn
	is_livemode := g.pref.is_livemain || g.pref.is_liveshared
	is_live_wrap := is_livefn && is_livemode
	if is_livefn && !is_livemode {
		eprintln('INFO: compile with `v -live ${g.pref.path} `, if you want to use the @[live] function ${node.name} .')
	}

	mut name := g.c_fn_name(node)
	type_name := g.ret_styp(g.unwrap_generic(node.return_type))
	// Live functions are protected by a mutex, because otherwise they
	// can be changed by the live reload thread, *while* they are
	// running, with unpredictable results (usually just crashing).
	// For this purpose, the actual body of the live function,
	// is put under a non publicly accessible function, that is prefixed
	// with 'impl_live_' .
	if is_livemode {
		if is_livefn {
			g.hotcode_fn_names << name
		}
		g.hotcode_fpaths << g.file.path
	}
	mut impl_fn_name := name
	if is_live_wrap {
		impl_fn_name = 'impl_live_${name}'
	}
	last_fn_c_name_save := g.last_fn_c_name
	defer {
		g.last_fn_c_name = last_fn_c_name_save
	}
	g.last_fn_c_name = impl_fn_name

	if !g.inside_c_extern && node.trace_fns.len > 0 {
		for trace_fn, call_fn in node.trace_fns {
			if trace_fn in g.trace_fn_definitions {
				continue
			}
			trace_fn_ret_type := g.styp(call_fn.return_type)

			g.write('VV_LOC ${trace_fn_ret_type} ${c_name(trace_fn)}(')
			g.definitions.write_string('VV_LOC ${trace_fn_ret_type} ${c_name(trace_fn)}(')

			mut trace_call_args := []string{}
			if call_fn.is_fn_var {
				sig := g.fn_var_signature(ast.void_type, call_fn.func.return_type,
					call_fn.func.params.map(it.typ), call_fn.name)
				g.write(sig)
				g.definitions.write_string(sig)
				if call_fn.func.params.len > 0 {
					g.write(', ')
					g.definitions.write_string(', ')
					trace_call_args, _, _ = g.fn_decl_params(call_fn.func.params, unsafe { nil },
						call_fn.func.is_variadic, call_fn.func.is_c_variadic)
				}
			} else {
				trace_call_args, _, _ = g.fn_decl_params(call_fn.func.params, unsafe { nil },
					call_fn.func.is_variadic, call_fn.func.is_c_variadic)
			}

			g.writeln(') {')
			g.definitions.write_string(');\n')

			orig_fn_args := trace_call_args.join(', ')
			add_trace_hook := g.pref.is_trace
				&& call_fn.name !in ['v.debug.add_after_call', 'v.debug.add_before_call', 'v.debug.remove_after_call', 'v.debug.remove_before_call']
			if g.pref.is_callstack {
				if g.cur_fn.is_method || g.cur_fn.is_static_type_method {
					g.writeln('\tbuiltin__array_push((array*)&g_callstack, _MOV((v__debug__FnTrace[]){ ((v__debug__FnTrace){.name = _S("${g.table.type_to_str(g.cur_fn.receiver.typ)}.${g.cur_fn.name.all_after_last('__static__')}"),.file = _S("${call_fn.file}"),.line = ${call_fn.line},}) }));')
				} else {
					g.writeln('\tbuiltin__array_push((array*)&g_callstack, _MOV((v__debug__FnTrace[]){ ((v__debug__FnTrace){.name = _S("${g.cur_fn.name}"),.file = _S("${call_fn.file}"),.line = ${call_fn.line},}) }));')
				}
			}
			mut method_name := c_name(call_fn.name)
			if call_fn.name.contains('_') {
				parts := call_fn.name.split('_')
				if parts.len >= 2 {
					receiver_type_name := parts[0]
					if resolved_sym := g.table.find_sym(receiver_type_name) {
						if resolved_sym.is_builtin() {
							method_name = 'builtin__${method_name}'
						}
					}
				}
			}
			if call_fn.return_type == 0 || call_fn.return_type == ast.void_type {
				if add_trace_hook {
					g.writeln('\tif (!g_trace.in_hook) {')
					g.writeln('\t\tv__debug__before_call_hook(_S("${call_fn.name}"));')
					g.writeln('\t}')
				}
				g.writeln('\t${method_name}(${orig_fn_args});')
				if add_trace_hook {
					g.writeln('\tif (!g_trace.in_hook) {')
					g.writeln('\t\tv__debug__after_call_hook(_S("${call_fn.name}"));')
					g.writeln('\t}')
				}
				if g.pref.is_callstack {
					g.writeln('\tbuiltin__array_pop((array*)&g_callstack);')
				}
			} else {
				if add_trace_hook {
					g.writeln('\tif (!g_trace.in_hook) {')
					g.writeln('\t\tv__debug__before_call_hook(_S("${call_fn.name}"));')
					g.writeln('\t}')
				}
				g.writeln('\t${g.styp(call_fn.return_type)} ret = ${method_name}(${orig_fn_args});')
				if g.pref.is_callstack {
					g.writeln('\tbuiltin__array_pop((array*)&g_callstack);')
				}
				if add_trace_hook {
					g.writeln('\tif (!g_trace.in_hook) {')
					g.writeln('\t\tv__debug__after_call_hook(_S("${call_fn.name}"));')
					g.writeln('\t}')
				}
				g.writeln('\treturn ret;')
			}
			g.writeln2('}', '')
			g.trace_fn_definitions << trace_fn
		}
	}

	if is_live_wrap {
		if is_livemain {
			g.definitions.write_string('${type_name} (* ${impl_fn_name})(')
			g.write('${type_name} no_impl_${name}(')
		}
		if is_liveshared {
			if g.pref.os == .windows {
				g.export_funcs << impl_fn_name
				g.definitions.write_string('VV_EXP ${type_name} ${impl_fn_name}(')
				g.write('VV_EXP ${type_name} ${impl_fn_name}(')
			} else {
				g.definitions.write_string('${type_name} ${impl_fn_name}(')
				g.write('${type_name} ${impl_fn_name}(')
			}
		}
	} else if g.inside_c_extern {
		c_extern_fn_header := 'extern ${type_name} ${fn_attrs}${name.all_after_first('C__')}('
		g.definitions.write_string(c_extern_fn_header)
	} else {
		if !(node.is_pub || g.pref.is_debug) {
			// Private functions need to marked as static so that they are not exportable in the
			// binaries
			if g.pref.build_mode != .build_module && !g.pref.use_cache {
				// If we are building vlib/builtin, we need all private functions like array_get
				// to be public, so that all V programs can access them.
				if !(node.is_anon && g.pref.parallel_cc) {
					g.write('VV_LOC ')
					// g.definitions.write_string('${g.static_modifier} VV_LOC ')
					g.definitions.write_string('VV_LOC ')
				}
			}
		}
		// as a temp solution generic functions are marked static
		// when -usecache is enabled to fix duplicate symbols with clang
		// TODO: implement a better sulution
		needs_object_local_linkage := g.should_use_object_local_linkage(node.mod)
			&& (node.is_pub || g.pref.is_debug)
		visibility_kw := if needs_object_local_linkage {
			'static '
		} else if g.cur_concrete_types.len > 0
			&& (g.pref.build_mode == .build_module || g.pref.use_cache) {
			'static '
		} else {
			''
		}
		fn_header := '${visibility_kw}${type_name} ${fn_attrs}${name}('
		g.definitions.write_string(fn_header)
		g.write(fn_header)
	}
	arg_start_pos := g.out.len
	is_c_variadic := node.is_c_variadic || (node.language == .c && node.is_variadic)
	fargs, fargtypes, heap_promoted := g.fn_decl_params(node.params, node.scope, node.is_variadic,
		is_c_variadic)
	if is_closure {
		g.nr_closures++
		if g.pref.no_closures {
			g.error('a closure was generated for function', node.pos)
		}
	}
	arg_str := g.out.after(arg_start_pos)
	if node.no_body || ((g.pref.use_cache && g.pref.build_mode != .build_module) && node.is_builtin
		&& !g.pref.is_test) || skip {
		// Just a function header. Builtin function bodies are defined in builtin.o
		g.definitions.writeln(');') // NO BODY')
		if !g.inside_c_extern {
			g.writeln(');')
		}
		return
	}
	if node.params.len == 0 {
		g.definitions.write_string('void')
	}
	if attr := node.attrs.find_first('_linker_section') {
		g.definitions.writeln(') __attribute__ ((section ("${attr.arg}")));')
	} else {
		g.definitions.writeln(');')
	}
	g.writeln(') {')
	if is_closure {
		g.writeln('${cur_closure_ctx}* ${closure_ctx} = g_closure.closure_get_data();')
	}
	for i, is_promoted in heap_promoted {
		if is_promoted {
			g.writeln('${fargtypes[i]}* ${fargs[i]} = HEAP(${fargtypes[i]}, _v_toheap_${fargs[i]});')
		}
	}
	g.indent++
	for defer_stmt in function_defer_stmts {
		if defer_stmt.mode != .function {
			continue
		}
		g.writeln('bool ${g.defer_flag_var(defer_stmt)} = false;')
		for var in defer_stmt.defer_vars {
			if var.name in fargs || var.kind == .constant {
				continue
			}
			if var.kind == .variable {
				if var.name !in g.defer_vars {
					g.defer_vars << var.name
					mut deref := ''
					if v := var.scope.find_var(var.name) {
						if v.is_auto_heap {
							deref = '*'
						}
					}
					info := var.obj as ast.Var
					if g.table.sym(info.typ).kind != .function {
						if info.is_static {
							g.write('static ')
						}
						if info.is_volatile {
							g.write('volatile ')
						}
						decl_typ := if deref == '*' {
							info.typ.ref()
						} else {
							info.typ
						}
						default_expr := g.type_default(decl_typ)
						if g.type_default_vars.len > 0 {
							for decl in g.type_default_vars.str().trim_right('\n').split_into_lines() {
								g.writeln(decl)
							}
							g.type_default_vars.clear()
						}
						g.writeln('${g.styp(info.typ)}${deref} ${c_name(var.name)} = ${default_expr};')
					}
				}
			}
		}
	}
	g.indent--
	if is_live_wrap {
		// The live function just calls its implementation dual, while ensuring
		// that the call is wrapped by the mutex lock & unlock calls.
		// Adding the mutex lock/unlock inside the body of the implementation
		// function is not reliable, because the implementation function can do
		// an early exit, which will leave the mutex locked.
		live_mutex_ptr_type := if g.pref.os == .windows {
			'HANDLE*'
		} else {
			'pthread_mutex_t*'
		}
		mut fn_args_list := []string{}
		for ia, fa in fargs {
			fn_args_list << '${fargtypes[ia]} ${fa}'
		}
		mut live_fncall := '${impl_fn_name}(' + fargs.join(', ') + ');'
		mut live_fnreturn := ''
		if type_name != 'void' {
			live_fncall = '${type_name} res = ${live_fncall}'
			live_fnreturn = 'return res;'
		}
		if is_livemain {
			g.definitions.writeln('${type_name} no_impl_${name}(' + fn_args_list.join(', ') + ');')
		}
		g.definitions.writeln('${type_name} ${name}(' + fn_args_list.join(', ') + ');')
		g.hotcode_definitions.writeln('${type_name} ${name}(' + fn_args_list.join(', ') + '){')
		g.hotcode_definitions.writeln('  ${live_mutex_ptr_type} live_fn_mutex_ptr = v_live_fn_mutex_ptr();')
		g.hotcode_definitions.writeln('  pthread_mutex_lock(live_fn_mutex_ptr);')
		g.hotcode_definitions.writeln('  ${live_fncall}')
		g.hotcode_definitions.writeln('  pthread_mutex_unlock(live_fn_mutex_ptr);')
		g.hotcode_definitions.writeln('  ${live_fnreturn}')
		g.hotcode_definitions.writeln('}')
	}
	// Profiling mode? Start counting at the beginning of the function (save current time).
	if g.pref.is_prof && g.pref.build_mode != .build_module {
		g.profile_fn(node)
	}
	// we could be in an anon fn so save outer fn defer stmts
	prev_defer_stmts := g.defer_stmts
	g.defer_stmts = []
	ctmp := g.tmp_count
	g.tmp_count = 0
	defer {
		g.tmp_count = ctmp
	}
	prev_inside_ternary := g.inside_ternary
	g.inside_ternary = 0
	prev_indent := g.indent
	g.indent = 0
	defer {
		g.indent = prev_indent
	}
	g.stmts(node.stmts)
	g.inside_ternary = prev_inside_ternary
	if node.is_noreturn {
		g.writeln('\twhile(1);')
	}
	// clear g.fn_mut_arg_names

	if !node.has_return {
		g.write_defer_stmts_when_needed(node.scope, false, node.name_pos)
	}
	if node.is_anon {
		g.defer_stmts = prev_defer_stmts
	} else {
		g.defer_stmts = []
	}
	if node.return_type != ast.void_type && node.stmts.len > 0 && node.stmts.last() !is ast.Return
		&& !node.attrs.contains('_naked') {
		default_expr := g.type_default(node.return_type)
		// TODO: perf?
		if default_expr == '{0}' {
			g.writeln('\treturn (${type_name})${default_expr};')
		} else {
			g.writeln('\treturn ${default_expr};')
		}
	}
	g.writeln('}')
	if g.pref.printfn_list.len > 0 && g.last_fn_c_name in g.pref.printfn_list {
		println(g.out.after(fn_start_pos))
	}
	weak := if node.is_weak { 'VWEAK ' } else { '' }
	for attr in node.attrs {
		if attr.name == 'export' {
			g.writeln('// export alias: ${attr.arg} -> ${name}')
			g.export_funcs << attr.arg
			export_alias := '${weak}${type_name} ${fn_attrs}${attr.arg}(${arg_str})'
			g.definitions.writeln('VV_EXP ${export_alias}; // exported fn ${node.name}')
			g.writeln('${export_alias} {')
			if !g.pref.is_shared && 'no_main' in g.table.modules {
				g.writeln('\t_vno_main_init_caller();')
			}
			g.write2('\treturn ${name}(', fargs.join(', '))
			g.writeln2(');', '}')
		}
	}
}

fn (mut g Gen) c_fn_name(node &ast.FnDecl) string {
	mut name := node.name
	if name in ['+', '-', '*', '**', '/', '%', '<', '==', '[]', '[]='] {
		name = util.replace_op(name)
	}
	if node.is_method {
		unwrapped_rec_typ := g.unwrap_generic(node.receiver.typ)
		name = g.cc_type(unwrapped_rec_typ, false) + '_' + name
		receiver_sym := g.table.sym(unwrapped_rec_typ)
		if receiver_sym.is_builtin() {
			name = 'builtin__${name}'
		}
		if receiver_sym.kind == .placeholder {
			name = name.replace_each(c_fn_name_escape_seq)
		}
	}
	if node.is_anon && g.anon_fn != unsafe { nil } && g.anon_fn.has_ct_var {
		name = '${name}_${g.comptime.comptime_loop_id}'
	}
	if node.language == .c {
		name = util.no_dots(name)
	} else {
		name = c_fn_name(name)
	}
	if !node.is_method && node.mod == 'builtin' && !name.starts_with('builtin__') {
		name = 'builtin__${name}'
	}

	if node.generic_names.len > 0 {
		name = g.generic_fn_name(g.method_concrete_types_for_name(node), name)
		name = name.replace_each(c_fn_name_escape_seq)
	}

	if g.pref.translated || g.file.is_translated || node.is_file_translated {
		if cattr := node.attrs.find_first('c') {
			// This fixes unknown symbols errors when building separate .c => .v files into .o files
			// example:
			// @[c: 'P_TryMove'] fn p_trymove(thing &Mobj_t, x int, y int) bool
			// translates to:
			// bool P_TryMove(main__Mobj_t* thing, int x, int y);
			// In fn_call every time `p_trymove` is called, `P_TryMove` will be generated instead.
			name = cattr.arg
		}
	}
	return name
}

const closure_ctx = '_V_closure_ctx'

fn (g &Gen) anon_fn_generic_names(generic_names []string) []string {
	if g.cur_fn == unsafe { nil } || g.cur_fn.generic_names.len == 0
		|| g.cur_concrete_types.len == 0 {
		return generic_names.clone()
	}
	mut names := g.cur_fn.generic_names.clone()
	for generic_name in generic_names {
		if generic_name !in names {
			names << generic_name
		}
	}
	return names
}

fn (mut g Gen) gen_closure_fn_name(node ast.AnonFn) string {
	mut fn_name := node.decl.name
	if g.anon_fn_generic_names(node.decl.generic_names).len > 0 {
		fn_name = g.generic_fn_name(g.cur_concrete_types, fn_name)
	}
	if node.has_ct_var {
		fn_name += '_${g.comptime.comptime_loop_id}'
	}
	return fn_name
}

fn (mut g Gen) c_call_alias_signature(node ast.CallExpr, name string) string {
	ret_styp := g.styp(node.return_type)
	mut sig := '${ret_styp} (*${name})('
	if node.args.len == 0 {
		if !node.is_c_variadic {
			sig += 'void'
		}
	} else {
		for i, arg in node.args {
			arg_typ := if arg.typ != 0 {
				arg.typ
			} else if i < node.expected_arg_types.len {
				node.expected_arg_types[i]
			} else {
				ast.voidptr_type
			}
			sig += g.styp(arg_typ)
			if i < node.args.len - 1 || node.is_c_variadic {
				sig += ', '
			}
		}
		if node.is_c_variadic {
			sig += '...'
		}
	}
	sig += ')'
	return sig
}

fn (mut g Gen) c_call_name(node ast.CallExpr, cname string) string {
	if node.scope == unsafe { nil } {
		return cname
	}
	if node.scope.find_var(cname) == none && node.scope.find_global(cname) == none {
		return cname
	}
	g.global_tmp_count++
	alias_name := '__v_c_fn_${g.global_tmp_count}_${cname}'
	alias_sig := g.c_call_alias_signature(node, alias_name)
	cast_sig := g.c_call_alias_signature(node, '')
	g.definitions.writeln('${g.static_non_parallel}${alias_sig} = (${cast_sig})${cname};')
	if g.pref.parallel_cc {
		g.extern_out.writeln('extern ${alias_sig};')
	}
	return alias_name
}

fn (mut g Gen) closure_ctx(node ast.FnDecl) string {
	mut fn_name := node.name
	generic_names := if node.is_anon {
		g.anon_fn_generic_names(node.generic_names)
	} else {
		node.generic_names
	}
	if generic_names.len > 0 {
		fn_name = g.generic_fn_name(g.cur_concrete_types, fn_name)
	}
	return 'struct _V_${fn_name}_Ctx'
}

fn (mut g Gen) is_mut_closure_fixed_array(var ast.Param) bool {
	resolved_typ := g.recheck_concrete_type(var.typ)
	return var.is_mut && g.table.final_sym(resolved_typ).kind == .array_fixed
}

fn (mut g Gen) mut_closure_fixed_array_field_styp(var ast.Param) string {
	resolved_typ := g.recheck_concrete_type(var.typ)
	info := g.table.final_sym(resolved_typ).info as ast.ArrayFixed
	resolved_elem_type := g.recheck_concrete_type(info.elem_type)
	mut elem_styp := g.styp(resolved_elem_type.set_nr_muls(0))
	if resolved_elem_type.is_ptr() {
		elem_styp += '*'.repeat(resolved_elem_type.nr_muls())
	}
	return '${elem_styp}*'
}

fn (mut g Gen) closure_inherited_var_type(node ast.AnonFn, var ast.Param) ast.Type {
	resolved := g.resolve_current_fn_generic_param_type(var.name)
	if resolved != 0 {
		return g.unwrap_generic(g.recheck_concrete_type(resolved))
	}
	if node.decl.scope != unsafe { nil } && node.decl.scope.parent != unsafe { nil } {
		if scope_var := node.decl.scope.parent.find_var(var.name) {
			// If the scope variable has smartcasts and is a sumtype, use the
			// smartcast variant type instead of the original sumtype.
			if scope_var.smartcasts.len > 0 && g.table.type_kind(scope_var.typ) == .sum_type {
				return g.unwrap_generic(g.recheck_concrete_type(scope_var.smartcasts.last()))
			}
			// Only use resolved_expr_type for generic type resolution.
			// For non-generic types, scope_var.typ is already correct and
			// resolved_expr_type can produce wrong pointer levels (e.g. adding
			// an extra ref() for &receiver expressions).
			if scope_var.typ.has_flag(.generic)
				|| g.type_has_unresolved_generic_parts(scope_var.typ) {
				if scope_var.expr !is ast.EmptyExpr {
					resolved_expr_typ := g.resolved_expr_type(scope_var.expr, scope_var.typ)
					if resolved_expr_typ != 0 {
						return g.unwrap_generic(g.recheck_concrete_type(resolved_expr_typ))
					}
				}
			}
			if scope_var.typ != 0 {
				return g.unwrap_generic(g.recheck_concrete_type(scope_var.typ))
			}
		}
	}
	return g.unwrap_generic(g.recheck_concrete_type(var.typ))
}

fn (g &Gen) variadic_voidptr_promoted_type(typ ast.Type) ast.Type {
	base_typ := g.table.unaliased_type(typ).clear_flags()
	if base_typ in ast.int_promoted_type_idxs || base_typ == ast.bool_type {
		return ast.int_type
	}
	if base_typ == ast.f32_type {
		return ast.f64_type
	}
	return ast.no_type
}

fn (mut g Gen) gen_anon_fn(mut node ast.AnonFn) {
	is_amp := g.is_amp
	g.is_amp = false
	defer {
		g.is_amp = is_amp
	}
	g.gen_anon_fn_decl(mut node)
	fn_name := g.gen_closure_fn_name(node)
	if !node.decl.scope.has_inherited_vars() {
		g.write(fn_name)
		return
	}
	ctx_struct := g.closure_ctx(node.decl)
	// it may be possible to optimize `memdup` out if the closure never leaves current scope
	// TODO: in case of an assignment, this should only call "closure_set_data" and "closure_set_function" (and free the former data)
	g.write('builtin__closure__closure_create(${fn_name}, (${ctx_struct}*) builtin__memdup_uncollectable(&(${ctx_struct}){')
	g.indent++
	for var in node.inherited_vars {
		mut has_inherited := false
		mut is_ptr := false
		var_name := c_name(var.name)
		if g.is_mut_closure_fixed_array(var) {
			if obj := node.decl.scope.find_var(var.name) {
				if obj.has_inherited {
					has_inherited = true
					g.writeln('.${var_name} = ${closure_ctx}->${var_name},')
				}
			}
			if !has_inherited {
				g.writeln('.${var_name} = ${var_name},')
			}
			continue
		}
		if obj := node.decl.scope.find_var(var.name) {
			is_ptr = obj.typ.is_ptr()
			if obj.has_inherited {
				has_inherited = true
				resolved_var_typ := g.closure_inherited_var_type(node, var)
				var_sym := g.table.sym(resolved_var_typ)
				if var_sym.info is ast.ArrayFixed {
					g.write('.${var_name} = {')
					for i in 0 .. var_sym.info.size {
						g.write('${closure_ctx}->${var_name}[${i}],')
					}
					g.writeln('},')
				} else if g.resolved_ident_is_by_value_auto_deref_capture(ast.Ident{
					name:  var.name
					scope: node.decl.scope
				})
				{
					g.writeln('.${var_name} = *${closure_ctx}->${var_name},')
				} else {
					g.writeln('.${var_name} = ${closure_ctx}->${var_name},')
				}
			}
		}
		if !has_inherited {
			resolved_var_typ := g.closure_inherited_var_type(node, var)
			var_sym := g.table.sym(resolved_var_typ)
			if var_sym.info is ast.ArrayFixed {
				g.write('.${var_name} = {')
				for i in 0 .. var_sym.info.size {
					g.write('${var_name}[${i}],')
				}
				g.writeln('},')
			} else if g.is_autofree && !var.is_mut && var_sym.info is ast.Array {
				g.writeln('.${var_name} = builtin__array_clone(&${var_name}),')
			} else if g.is_autofree && !var.is_mut && var_sym.kind == .string {
				g.writeln('.${var_name} = builtin__string_clone(${var_name}),')
			} else {
				mut is_auto_heap := false
				mut is_auto_deref_capture := false
				mut field_name := ''
				if obj := node.decl.scope.parent.find(var.name) {
					if obj is ast.Var {
						is_auto_heap = !obj.is_stack_obj && obj.is_auto_heap
						is_auto_deref_capture = obj.is_auto_deref && !var.is_mut
							&& (is_ptr || obj.typ.is_ptr())
						if obj.smartcasts.len > 0 {
							if g.table.type_kind(obj.typ) == .sum_type {
								cast_sym := g.table.sym(obj.smartcasts.last())
								field_name += '._${cast_sym.cname}'
							}
						}
					}
				}
				if (is_auto_heap && !is_ptr) || is_auto_deref_capture || field_name != '' {
					g.writeln('.${var_name} = *${var_name}${field_name},')
				} else {
					g.writeln('.${var_name} = ${var_name},')
				}
			}
		}
	}
	g.indent--
	g.write('}, sizeof(${ctx_struct})))')

	g.empty_line = false
}

fn (mut g Gen) gen_anon_fn_decl(mut node ast.AnonFn) {
	mut fn_name := g.gen_closure_fn_name(node)
	if node.has_gen[fn_name] {
		return
	}
	mut builder := strings.new_builder(256)
	// Generate a closure struct
	if node.inherited_vars.len > 0 {
		ctx_struct := g.closure_ctx(node.decl)
		if ctx_struct !in g.closure_structs {
			g.closure_structs << ctx_struct
			g.definitions.writeln('${ctx_struct} {')
			for var in node.inherited_vars {
				mut resolved_var_typ := g.closure_inherited_var_type(node, var)
				// When the variable is auto-heap promoted, the closure struct
				// stores the dereferenced value (the init code does `*var`),
				// so strip one pointer level from the field type.
				// Similarly, when a `mut` parameter (is_auto_deref) is captured
				// without `mut`, the closure stores the dereferenced value.
				if node.decl.scope != unsafe { nil } && node.decl.scope.parent != unsafe { nil } {
					if scope_var := node.decl.scope.parent.find_var(var.name) {
						if scope_var.is_auto_heap && !scope_var.is_stack_obj
							&& resolved_var_typ.is_ptr() {
							resolved_var_typ = resolved_var_typ.deref()
						} else if scope_var.is_auto_deref && !var.is_mut
							&& resolved_var_typ.is_ptr() {
							resolved_var_typ = resolved_var_typ.deref()
						}
					}
				}
				var_sym := g.table.sym(resolved_var_typ)
				if g.is_mut_closure_fixed_array(var) {
					g.definitions.writeln('\t${g.mut_closure_fixed_array_field_styp(var)} ${c_name(var.name)};')
				} else if var_sym.info is ast.FnType {
					resolved_ret_type := g.recheck_concrete_type(var_sym.info.func.return_type)
					resolved_param_types :=
						var_sym.info.func.params.map(g.recheck_concrete_type(it.typ))
					mut sig := g.fn_var_signature(resolved_var_typ, resolved_ret_type,
						resolved_param_types, c_name(var.name))
					g.definitions.writeln('\t' + sig + ';')
				} else {
					styp := g.styp(resolved_var_typ)
					g.definitions.writeln('\t${styp} ${c_name(var.name)};')
				}
			}
			g.definitions.writeln('};\n')
		}
	}
	pos := g.out.len
	was_anon_fn := g.anon_fn
	prev_stmt_path_pos := g.stmt_path_pos.clone()
	prev_skip_stmt_pos := g.skip_stmt_pos
	decl := ast.FnDecl{
		...node.decl
		generic_names: g.anon_fn_generic_names(node.decl.generic_names)
	}
	g.stmt_path_pos = []
	g.skip_stmt_pos = false
	g.anon_fn = node
	old_inside_return := g.inside_return
	old_inside_return_expr := g.inside_return_expr
	g.inside_return = false
	g.inside_return_expr = false
	g.fn_decl(decl)
	g.inside_return_expr = old_inside_return_expr
	g.inside_return = old_inside_return
	g.anon_fn = was_anon_fn
	g.skip_stmt_pos = prev_skip_stmt_pos
	g.stmt_path_pos = prev_stmt_path_pos
	builder.write_string(g.out.cut_to(pos))
	out := builder.str()
	if out.len == 0 {
		return
	}
	node.has_gen[fn_name] = true
	g.anon_fn_definitions << out
	if g.pref.parallel_cc {
		g.extern_out.writeln('extern ${out.all_before(' {')};')
	}
}

fn (mut g Gen) fn_decl_params(params []ast.Param, scope &ast.Scope, is_variadic bool, is_c_variadic bool) ([]string, []string, []bool) {
	mut fparams := []string{}
	mut fparamtypes := []string{}
	mut heap_promoted := []bool{}
	param_count := if is_c_variadic && is_variadic && params.len > 0
		&& params.last().typ.has_flag(.variadic) {
		params.len - 1
	} else {
		params.len
	}
	if param_count == 0 {
		// in C, `()` is untyped, unlike `(void)`
		if !g.inside_c_extern && !is_c_variadic {
			g.write('void')
		}
	}
	for i, param in params {
		if i >= param_count {
			break
		}
		mut typ := g.unwrap_generic(param.typ)
		if g.pref.translated && g.file.is_translated && param.typ.has_flag(.variadic) {
			typ = g.table.sym(typ).array_info().elem_type.set_flag(.variadic)
		}
		if param.is_mut && param.orig_typ != 0 && param.orig_typ.has_flag(.generic)
			&& param.typ.has_flag(.generic) {
			mut surface_typ := g.unwrap_generic(param.orig_typ)
			// Only use ref() when the pointer comes from the generic type argument
			// (T=&int), not from the param signature (&T / ?&T).
			orig_was_ptr := param.orig_typ.nr_muls() > 0
			typ = if surface_typ.is_ptr() && !orig_was_ptr {
				surface_typ.ref()
			} else {
				surface_typ.set_nr_muls(1)
			}
			if typ.has_flag(.option) {
				typ = typ.set_flag(.option_mut_param_t)
			}
		}
		if param.is_mut && param.typ.has_flag(.generic) && typ.has_flag(.option) {
			typ = typ.set_flag(.option_mut_param_t).set_nr_muls(param.typ.nr_muls() - 1)
		}
		if param.is_mut && param.orig_typ != 0 && param.orig_typ.has_flag(.option)
			&& typ.has_flag(.option_mut_param_t) && typ.nr_muls() == 1
			&& !g.mut_option_param_assigned_directly(param.name) {
			typ = typ.ref()
		}
		param_type_sym := g.table.sym(typ)
		mut caname := if param.name in ['', '_'] {
			'_d${i + 1}'
		} else if param_type_sym.kind == .function && !typ.has_flag(.option) {
			c_fn_name(param.name)
		} else {
			c_name(param.name)
		}
		mut param_type_name := g.styp(typ)
		if param.typ.has_flag(.generic) {
			param_type_name = param_type_name.replace_each(c_fn_name_escape_seq)
		}
		if param_type_sym.kind == .function && !typ.has_flag(.option) {
			info := param_type_sym.info as ast.FnType
			func := info.func
			if !g.inside_c_extern {
				g.write('${g.ret_styp(func.return_type)} (*${caname})(')
			}
			g.definitions.write_string('${g.ret_styp(func.return_type)} (*${caname})(')
			g.fn_decl_params(func.params, unsafe { nil }, func.is_variadic, func.is_c_variadic)
			if !g.inside_c_extern {
				g.write(')')
			}
			g.definitions.write_string(')')
			fparams << caname
			fparamtypes << param_type_name
			heap_promoted << false
		} else {
			mut heap_prom := false
			if scope != unsafe { nil } {
				if param.name != '_' {
					if v := scope.find_var(param.name) {
						if !v.is_stack_obj && v.is_auto_heap {
							heap_prom = true
						}
					}
				}
			}
			var_name_prefix := if heap_prom { '_v_toheap_' } else { '' }
			const_prefix := if param.typ.is_any_kind_of_pointer() && !param.is_mut
				&& param.name.starts_with('const_') {
				'const '
			} else {
				''
			}
			const_param_type_name := if const_prefix != '' {
				g.const_pointer_param_type_name(typ, param_type_name)
			} else {
				param_type_name
			}
			s := '${const_prefix}${const_param_type_name} ${var_name_prefix}${caname}'
			if !g.inside_c_extern {
				g.write(s)
			}
			g.definitions.write_string(s)
			fparams << caname
			fparamtypes << param_type_name
			heap_promoted << heap_prom
		}
		if i < param_count - 1 {
			if !g.inside_c_extern {
				g.write(', ')
			}
			g.definitions.write_string(', ')
		}
	}
	if ((g.pref.translated && is_variadic) || is_c_variadic) && (!is_c_variadic || param_count > 0) {
		if param_count > 0 {
			if !g.inside_c_extern {
				g.write(', ')
			}
			g.definitions.write_string(', ')
		}
		if !g.inside_c_extern {
			g.write('... ')
		}
		g.definitions.write_string('... ')
	}
	return fparams, fparamtypes, heap_promoted
}

fn (g &Gen) mut_option_param_assigned_directly(name string) bool {
	if g.cur_fn == unsafe { nil } {
		return false
	}
	for stmt in g.cur_fn.stmts {
		if stmt is ast.AssignStmt {
			for left in stmt.left {
				if left is ast.Ident && left.name == name {
					return true
				}
			}
		}
	}
	return false
}

fn (mut g Gen) const_pointer_param_type_name(typ ast.Type, param_type_name string) string {
	unwrapped_typ := g.unwrap_generic(typ)
	return match unwrapped_typ {
		ast.voidptr_type { 'void*' }
		ast.byteptr_type { 'u8*' }
		ast.charptr_type { 'char*' }
		else { param_type_name }
	}
}

fn (mut g Gen) get_anon_fn_type_name(mut node ast.AnonFn, var_name string) string {
	mut builder := strings.new_builder(64)
	return_styp := g.styp(node.decl.return_type)
	builder.write_string('${return_styp} (*${var_name}) (')
	if node.decl.params.len == 0 {
		builder.write_string('void)')
	} else {
		for i, param in node.decl.params {
			param_styp := g.styp(param.typ)
			builder.write_string('${param_styp} ${param.name}')
			if i != node.decl.params.len - 1 {
				builder.write_string(', ')
			}
		}
		builder.write_string(')')
	}
	return builder.str()
}

fn (mut g Gen) call_expr(node ast.CallExpr) {
	if node.should_be_skipped {
		return
	}
	if node.is_c_type_cast {
		g.cast_expr(ast.CastExpr{
			typ:       node.return_type
			typname:   g.table.sym(g.unwrap_generic(node.return_type)).name
			expr:      node.args[0].expr
			expr_type: node.args[0].typ
			pos:       node.pos
		})
		return
	}
	is_shared := g.is_shared
	g.is_shared = false
	defer {
		g.is_shared = is_shared
	}
	// NOTE: everything could be done this way
	// see my comment in parser near anon_fn
	mut tmp_anon_fn_var := ''
	mut tmp_fn_result_var := ''
	mut needs_tmp_fn_result_cleanup := false
	if node.left is ast.AnonFn {
		if node.left.inherited_vars.len > 0 {
			tmp_anon_fn_var = g.new_tmp_var()
			fn_type := g.fn_var_signature(ast.void_type, node.left.decl.return_type,
				node.left.decl.params.map(it.typ), tmp_anon_fn_var)
			line := g.go_before_last_stmt().trim_space()
			g.empty_line = true
			g.write('${fn_type} = ')
			g.expr(ast.Expr(node.left))
			g.writeln(';')
			g.set_current_pos_as_last_stmt_pos()
			g.write(line)
			if node.or_block.kind == .absent {
				if g.out.last_n(1) != '\n' {
					g.writeln('')
				}
				g.write(tmp_anon_fn_var)
			}
		} else if node.or_block.kind == .absent {
			g.expr(ast.Expr(node.left))
		}
	} else if !g.inside_curry_call && node.left is ast.IndexExpr && node.name == '' {
		if node.or_block.kind == .absent {
			old_is_fn_index_call := g.is_fn_index_call
			g.is_fn_index_call = true
			g.expr(ast.Expr(node.left))
			g.is_fn_index_call = old_is_fn_index_call
		} else {
			// map1['key']() handling
			line := g.go_before_last_stmt()
			g.empty_line = true

			// temp var for map1['key'] where value is a fn to be called
			left_typ := g.table.value_type(node.left.left_type)
			tmp_res := g.new_tmp_var()
			fn_sym := g.table.sym(left_typ).info as ast.FnType
			fn_type := g.fn_var_signature(ast.void_type, fn_sym.func.return_type,
				fn_sym.func.params.map(it.typ), tmp_res)

			old_is_fn_index_call := g.is_fn_index_call
			g.is_fn_index_call = true
			g.write('${fn_type} = ')
			g.expr(ast.Expr(node.left))
			g.is_fn_index_call = old_is_fn_index_call
			g.writeln(';')

			tmp_res2 := g.new_tmp_var()
			// uses the `tmp_res` as fn name (where it is a ptr to fn var)
			g.write('${g.styp(node.return_type)} ${tmp_res2} = ${tmp_res}')
			g.last_tmp_call_var << tmp_res2
			old_inside_curry_call := g.inside_curry_call
			g.inside_curry_call = true
			// map1['key']()() handling
			g.expr(node)
			g.inside_curry_call = old_inside_curry_call
			g.write2(line, '*(${g.base_type(node.return_type)}*)${tmp_res2}.data')
			return
		}
	} else if !g.inside_curry_call && node.left is ast.CallExpr && node.name == '' {
		if node.or_block.kind == .absent {
			left_return_typ := g.recheck_concrete_type(g.unwrap_generic(node.left.return_type))
			if g.table.used_features.anon_fn && g.table.final_sym(left_return_typ).kind == .function {
				tmp_fn_result_var = g.new_tmp_var()
				fn_sym := g.table.final_sym(left_return_typ).info as ast.FnType
				fn_type := g.fn_var_signature(ast.void_type, fn_sym.func.return_type,
					fn_sym.func.params.map(it.typ), tmp_fn_result_var)
				line := g.go_before_last_stmt().trim_space()
				g.empty_line = true
				g.write('${fn_type} = ')
				g.expr(ast.Expr(node.left))
				g.writeln(';')
				g.set_current_pos_as_last_stmt_pos()
				g.write(line)
				if g.out.last_n(1) != '\n' {
					g.writeln('')
				}
				g.write(tmp_fn_result_var)
				needs_tmp_fn_result_cleanup = true
			} else {
				g.expr(ast.Expr(node.left))
			}
		} else {
			ret_typ := node.return_type

			line := g.go_before_last_stmt()
			g.empty_line = true

			tmp_res := g.new_tmp_var()
			g.write('${g.styp(ret_typ)} ${tmp_res} = ')

			g.last_tmp_call_var << tmp_res
			g.expr(ast.Expr(node.left))

			old_inside_curry_call := g.inside_curry_call
			g.inside_curry_call = true
			g.expr(node)
			g.inside_curry_call = old_inside_curry_call
			g.write2(line, '*(${g.base_type(ret_typ)}*)${tmp_res}.data')
			return
		}
	} else if !g.inside_curry_call && node.left is ast.SelectorExpr && node.name == '' {
		if node.or_block.kind == .absent {
			g.expr(ast.Expr(node.left))
		} else {
			ret_typ := node.return_type

			line := g.go_before_last_stmt()
			g.empty_line = true

			tmp_res := g.new_tmp_var()
			g.write('${g.styp(ret_typ)} ${tmp_res} = ')

			g.last_tmp_call_var << tmp_res
			g.expr(ast.Expr(node.left))

			old_inside_curry_call := g.inside_curry_call
			g.inside_curry_call = true
			g.expr(node)
			g.inside_curry_call = old_inside_curry_call
			g.write2(line, '*(${g.base_type(ret_typ)}*)${tmp_res}.data')
			return
		}
	}
	old_inside_call := g.inside_call
	g.inside_call = true
	// Reset inside_selector_lhs so that receiver expressions inside method
	// calls generate proper auto-heap dereferences. Without this, when a
	// method call is nested inside a selector (e.g. exa.payload()!.len),
	// the selector's inside_selector_lhs flag would leak into the receiver
	// generation and suppress the (*(exa)) deref.
	old_inside_selector_lhs := g.inside_selector_lhs
	g.inside_selector_lhs = false
	defer {
		g.inside_call = old_inside_call
		g.inside_selector_lhs = old_inside_selector_lhs
	}
	gen_keep_alive := node.is_keep_alive && node.return_type != ast.void_type
		&& g.pref.gc_mode in [.boehm_full, .boehm_incr, .boehm_full_opt, .boehm_incr_opt]
	gen_or := node.or_block.kind != .absent // && !g.is_autofree
	is_gen_or_and_assign_rhs := gen_or && !g.discard_or_result
	mut cur_line := if !g.inside_curry_call && (is_gen_or_and_assign_rhs || gen_keep_alive) { // && !g.is_autofree {
		// `x := foo() or { ...}`
		// cut everything that has been generated to prepend option variable creation
		line := g.go_before_last_stmt()
		g.out.write_string(util.tabs(g.indent))
		line
	} else {
		''
	}
	// g.write('/*EE line="${cur_line}"*/')
	tmp_opt := if gen_or || gen_keep_alive {
		if g.inside_curry_call && g.last_tmp_call_var.len > 0 {
			g.last_tmp_call_var.pop()
		} else if !g.inside_or_block {
			new_tmp := g.new_tmp_var()
			g.last_tmp_call_var << new_tmp
			new_tmp
		} else {
			g.new_tmp_var()
		}
	} else {
		''
	}
	mut tmp_fn_result_call_value := ''
	mut tmp_fn_result_call_line := ''
	if needs_tmp_fn_result_cleanup && !gen_or && !gen_keep_alive && !g.inside_curry_call
		&& node.return_type != 0 && node.return_type != ast.void_type {
		tmp_fn_result_call_line = g.go_before_last_stmt()
		if tmp_fn_result_call_line.ends_with(tmp_fn_result_var) {
			tmp_fn_result_call_line = tmp_fn_result_call_line[..tmp_fn_result_call_line.len - tmp_fn_result_var.len]
		}
		g.empty_line = true
		tmp_fn_result_call_value = g.new_tmp_var()
		g.write('${g.styp(node.return_type)} ${tmp_fn_result_call_value} = ${tmp_fn_result_var}')
	}
	mut effective_return_type := node.return_type
	if (gen_or || gen_keep_alive) && node.return_type != 0 {
		mut ret_typ := effective_return_type
		resolved_ret_typ := g.resolve_return_type(node)
		if resolved_ret_typ != ast.void_type && !resolved_ret_typ.has_flag(.generic)
			&& !g.type_has_unresolved_generic_parts(resolved_ret_typ) {
			if node.return_type.has_flag(.result) && !resolved_ret_typ.has_flag(.result) {
				ret_typ = resolved_ret_typ.set_flag(.result)
			} else if node.return_type.has_flag(.option) && !resolved_ret_typ.has_flag(.option) {
				ret_typ = resolved_ret_typ.set_flag(.option)
			} else {
				ret_typ = resolved_ret_typ
			}
		} else if g.table.sym(ret_typ).kind == .alias {
			unaliased_type := g.table.unaliased_type(ret_typ)
			if unaliased_type.has_option_or_result() {
				ret_typ = unaliased_type
			}
		} else if node.return_type_generic != 0 {
			unwrapped_ret_typ := g.unwrap_generic(node.return_type_generic)
			if !unwrapped_ret_typ.has_flag(.generic) {
				ret_sym := g.table.sym(unwrapped_ret_typ)
				if ret_sym.info is ast.Array && g.table.sym(node.return_type_generic).kind == .array {
					// Make []T returns T type when array was supplied to T
					if g.table.type_to_str(node.return_type_generic).count('[]') < g.table.type_to_str(unwrapped_ret_typ).count('[]') {
						ret_typ = g.unwrap_generic(ret_sym.info.elem_type).derive(unwrapped_ret_typ)
					}
				}
			} else if g.cur_fn != unsafe { nil } && g.cur_fn.is_method
				&& g.cur_fn.receiver.typ.has_flag(.generic) && g.cur_concrete_types.len > 0 {
				// unwrap_generic failed because cur_fn.generic_names is empty
				// (generics come from the receiver, not the method itself).
				// Use the receiver's generic type names to resolve.
				receiver_generic_names := g.table.generic_type_names(g.cur_fn.receiver.typ)
				if receiver_generic_names.len == g.cur_concrete_types.len {
					if gen_type := g.table.convert_generic_type(node.return_type_generic,
						receiver_generic_names, g.cur_concrete_types)
					{
						if !gen_type.has_flag(.generic) {
							ret_typ = gen_type
						}
					}
				}
			}
		}
		effective_return_type = ret_typ
		mut styp := g.styp(ret_typ)
		if gen_or && !is_gen_or_and_assign_rhs {
			cur_line = g.go_before_last_stmt()
		}
		if gen_or && g.infix_left_var_name.len > 0 {
			g.writeln('${styp} ${tmp_opt};')
			g.writeln('if (${g.infix_left_var_name}) {')
			g.indent++
			g.write('${tmp_opt} = ')
		} else if !g.inside_curry_call {
			if g.assign_ct_type[node.pos.pos] != 0 && node.or_block.kind != .absent {
				styp = g.styp(g.assign_ct_type[node.pos.pos].derive(ret_typ))
			}
			g.write('${styp} ${tmp_opt} = ')
			if node.left is ast.AnonFn {
				if node.left.inherited_vars.len > 0 {
					g.write(tmp_anon_fn_var)
				} else {
					g.expr(ast.Expr(node.left))
				}
			}
		}
	}
	if node.is_method && !node.is_field {
		if g.pref.experimental && node.args.len > 0 && node.kind == .writeln
			&& node.args[0].expr is ast.StringInterLiteral
			&& g.table.sym(node.receiver_type).name == 'strings.Builder' {
			g.string_inter_literal_sb_optimized(node)
		} else {
			g.method_call(node)
		}
	} else {
		g.fn_call(node)
	}
	if needs_tmp_fn_result_cleanup && !gen_or && !gen_keep_alive && !g.inside_curry_call {
		if node.return_type == ast.void_type {
			g.writeln(';')
			g.write('builtin__closure__closure_try_destroy((voidptr)${tmp_fn_result_var})')
			g.set_current_pos_as_last_stmt_pos()
		} else if tmp_fn_result_call_value != '' {
			g.writeln(';')
			g.writeln('builtin__closure__closure_try_destroy((voidptr)${tmp_fn_result_var});')
			g.write2(tmp_fn_result_call_line, tmp_fn_result_call_value)
		}
	}
	if gen_or && node.return_type != 0 {
		g.or_block(tmp_opt, node.or_block, effective_return_type)
		mut unwrapped_typ := effective_return_type.clear_option_and_result()
		if g.table.sym(unwrapped_typ).kind == .alias {
			unaliased_type := g.table.unaliased_type(unwrapped_typ)
			if unaliased_type.has_option_or_result() {
				unwrapped_typ = unaliased_type.clear_option_and_result()
			}
		}
		mut unwrapped_styp := g.styp(unwrapped_typ)
		if g.infix_left_var_name.len > 0 {
			g.indent--
			g.writeln('}')
			g.set_current_pos_as_last_stmt_pos()
		}
		if unwrapped_typ == ast.void_type {
			g.write('\n ${cur_line}')
		} else if !g.inside_curry_call {
			if !g.inside_const_opt_or_res {
				if g.assign_ct_type[node.pos.pos] != 0 && node.or_block.kind != .absent {
					unwrapped_styp =
						g.styp(g.assign_ct_type[node.pos.pos].derive(effective_return_type).clear_option_and_result())
				}
				if g.table.sym(effective_return_type).kind == .array_fixed
					&& unwrapped_styp.starts_with('_v_') {
					unwrapped_styp = unwrapped_styp[3..]
				}
				// Synthesized/transformed call nodes may lose `is_return_used`
				// even though the enclosing C generation path is still
				// emitting a value expression (struct fields, call args, etc).
				// In those cases `is_gen_or_and_assign_rhs` still tells us that
				// the unwrapped result is needed in the current expression.
				if node.is_return_used || is_gen_or_and_assign_rhs {
					is_fn := g.table.final_sym(unwrapped_typ).kind == .function
					// return value is used, so we need to write the unwrapped temporary var
					if is_fn && unwrapped_typ.nr_muls() > 0 {
						g.write('\n ${cur_line}((${unwrapped_styp}*)${tmp_opt}.data)')
					} else {
						g.write('\n ${cur_line}(*(${unwrapped_styp}*)${tmp_opt}.data)')
					}
				} else {
					g.write('\n ${cur_line}')
				}
			} else {
				if !g.inside_or_block && g.last_tmp_call_var.len > 0 && !cur_line.contains(' = ') {
					g.write('\n\t*(${unwrapped_styp}*)${g.last_tmp_call_var.pop()}.data = ${cur_line}(*(${unwrapped_styp}*)${tmp_opt}.data)')
				} else {
					g.write('\n ${cur_line}(*(${unwrapped_styp}*)${tmp_opt}.data)')
				}
			}
		}
	} else if gen_keep_alive && node.return_type != 0 {
		if node.return_type == ast.void_type {
			g.write('\n ${cur_line}')
		} else {
			g.write('\n ${cur_line} ${tmp_opt}')
		}
	}
	if node.is_noreturn {
		if g.inside_ternary == 0 {
			g.writeln(';')
			g.write('VUNREACHABLE()')
		} else {
			$if msvc {
				// MSVC has no support for the statement expressions used below
			} $else {
				g.write(', ({VUNREACHABLE();})')
			}
		}
	}
}

fn (mut g Gen) conversion_function_call(prefix string, postfix string, node ast.CallExpr) {
	g.write('${prefix}( (')
	g.expr(node.left)
	dot := if node.left_type.is_ptr() { '->' } else { '.' }
	g.write(')${dot}_typ )${postfix}')
}

@[inline]
fn (mut g Gen) write_raw_receiver_expr(node ast.Expr) {
	old_inside_selector_lhs := g.inside_selector_lhs
	g.inside_selector_lhs = true
	defer {
		g.inside_selector_lhs = old_inside_selector_lhs
	}
	g.expr(node)
}

@[inline]
fn (mut g Gen) gen_arg_from_type(node_type ast.Type, node ast.Expr) {
	is_auto_heap_ident := node is ast.Ident && g.resolved_ident_is_auto_heap(node)
	if node_type.has_flag(.shared_f) {
		if node_type.is_ptr() || is_auto_heap_ident {
			g.write('&')
		}
		if is_auto_heap_ident {
			g.write_raw_receiver_expr(node)
		} else {
			g.expr(node)
		}
		g.write('->val')
	} else {
		if node_type.is_ptr() || is_auto_heap_ident {
			if is_auto_heap_ident {
				g.write_raw_receiver_expr(node)
			} else {
				g.expr(node)
			}
		} else if !node.is_lvalue()
			|| (node is ast.Ident && g.table.is_interface_smartcast(node.obj)) {
			g.write('ADDR(${g.styp(node_type)}, ')
			g.expr(node)
			g.write(')')
		} else {
			g.write('&')
			g.expr(node)
		}
	}
}

fn (mut g Gen) gen_map_method_call(node ast.CallExpr, left_type ast.Type, left_sym ast.TypeSymbol) bool {
	match node.kind {
		.reserve {
			g.write('builtin__map_reserve(')
			g.gen_arg_from_type(left_type, node.left)
			g.write(', ')
			g.expr(node.args[0].expr)
			g.write(')')
		}
		.delete {
			elem_type_str := if left_sym.info is ast.Map {
				g.styp(left_sym.info.key_type)
			} else {
				// In generic contexts, the left type may resolve to the base
				// `map` struct rather than a concrete `map[K]V`. Resolve the
				// key type from the argument expression instead.
				arg_type := g.unwrap_generic(g.resolved_expr_type(node.args[0].expr,
					node.args[0].typ))
				g.styp(arg_type)
			}
			g.write('builtin__map_delete(')
			g.gen_arg_from_type(left_type, node.left)
			g.write(', &(${elem_type_str}[]){')
			g.expr(node.args[0].expr)
			g.write('})')
		}
		.free, .clear, .keys, .values {
			g.write('builtin__map_${node.name}(')
			g.gen_arg_from_type(left_type, node.left)
			g.write(')')
		}
		else {
			return false
		}
	}

	return true
}

fn (mut g Gen) gen_array_method_call(node ast.CallExpr, left_type ast.Type, left_sym ast.TypeSymbol) bool {
	if node.name == 'get' {
		g.gen_array_get(node)
		return true
	}
	match node.kind {
		.filter {
			g.gen_array_filter(node)
		}
		.sort {
			g.gen_array_sort(node)
		}
		.sorted {
			g.gen_array_sorted(node)
		}
		.sort_with_compare {
			if !g.gen_array_sort_with_compare(node) {
				return false
			}
		}
		.sorted_with_compare {
			if !g.gen_array_sorted_with_compare(node) {
				return false
			}
		}
		.insert {
			g.gen_array_insert(node)
		}
		.map {
			g.gen_array_map(node)
		}
		.prepend {
			g.gen_array_prepend(node)
		}
		.contains {
			g.gen_array_contains(left_type, node.left, node.args[0].typ, node.args[0].expr)
		}
		.index {
			g.gen_array_index(node, false)
		}
		.last_index {
			g.gen_array_index(node, true)
		}
		.wait {
			g.gen_array_wait(node)
		}
		.any {
			g.gen_array_any(node)
		}
		.count {
			g.gen_array_count(node)
		}
		.all {
			g.gen_array_all(node)
		}
		.delete, .drop, .delete_last, .delete_many {
			g.write('builtin__array_${node.name}(')
			g.gen_arg_from_type(left_type, node.left)
			if node.kind != .delete_last {
				g.write(', ')
				g.expr(node.args[0].expr)
				if node.kind == .delete_many {
					g.write(', ')
					g.expr(node.args[1].expr)
				}
			}
			g.write(')')
		}
		.grow_cap, .grow_len {
			g.write('builtin__array_${node.name}(')
			g.gen_arg_from_type(left_type, node.left)
			g.write(', ')
			g.expr(node.args[0].expr)
			g.write(')')
		}
		.first, .last, .pop_left, .pop {
			mut noscan := ''
			array_info := left_sym.info as ast.Array
			if node.kind in [.pop_left, .pop] {
				noscan = g.check_noscan(array_info.elem_type)
			}
			return_type := g.resolved_array_builtin_method_return_type(node, left_type,
				node.return_type)
			return_type_str := g.styp(return_type)
			g.write('(*(${return_type_str}*)builtin__array_${node.name}${noscan}(')
			if node.kind in [.pop_left, .pop] {
				g.gen_arg_from_type(left_type, node.left)
			} else {
				if node.left_type.is_ptr() || node.left.is_auto_deref_var() {
					g.write('(*')
					g.expr(node.left)
					g.write(')')
				} else {
					g.expr(node.left)
				}
				if left_type.has_flag(.shared_f) {
					g.write('.val')
				}
			}
			g.write('))')
		}
		.clone, .repeat {
			array_info := left_sym.info as ast.Array
			elem_sym :=
				g.table.final_sym(g.table.unaliased_type(g.unwrap_generic(array_info.elem_type)))
			if node.kind == .repeat && elem_sym.kind == .interface {
				fn_name := g.register_array_interface_repeat_fn(g.resolve_return_type(node))
				g.write('${fn_name}(')
				if node.left_type.is_ptr() {
					g.write('*'.repeat(node.left_type.nr_muls()))
				}
				g.expr(ast.Expr(node.left))
				g.write(', ')
				g.expr(node.args[0].expr)
				g.write(')')
				return true
			}
			array_depth := g.get_array_depth(array_info.elem_type)
			to_depth := if array_depth >= 0 { '_to_depth' } else { '' }
			mut is_range_slice := false
			if node.left is ast.IndexExpr && node.left.index is ast.RangeExpr && node.kind == .clone {
				is_range_slice = true
			}
			to_static := if is_range_slice { '_static' } else { '' }
			g.write('builtin__array_${node.name}${to_static}${to_depth}(')
			if node.kind == .clone {
				if is_range_slice {
					if node.left_type.is_ptr() {
						g.write('*'.repeat(node.left_type.nr_muls()))
					}
					g.expr(node.left)
				} else {
					g.gen_arg_from_type(left_type, node.left)
				}
			} else {
				if node.left_type.is_ptr() {
					g.write('*'.repeat(node.left_type.nr_muls()))
				}
				g.expr(ast.Expr(node.left))
			}
			if node.kind == .repeat {
				g.write(', ')
				g.expr(node.args[0].expr)
			}
			if array_depth >= 0 {
				g.write(', ${array_depth}')
			}
			g.write(')')
		}
		else {
			return false
		}
	}

	return true
}

fn (mut g Gen) gen_fixed_array_method_call(node ast.CallExpr, left_type ast.Type) bool {
	match node.kind {
		.filter {
			g.gen_array_filter(node)
		}
		.index {
			g.gen_array_index(node, false)
		}
		.last_index {
			g.gen_array_index(node, true)
		}
		.contains {
			g.gen_array_contains(left_type, node.left, node.args[0].typ, node.args[0].expr)
		}
		.any {
			g.gen_array_any(node)
		}
		.count {
			g.gen_array_count(node)
		}
		.all {
			g.gen_array_all(node)
		}
		.map {
			g.gen_array_map(node)
		}
		.sort {
			g.gen_array_sort(node)
		}
		.sorted {
			g.gen_array_sorted(node)
		}
		.sort_with_compare {
			if !g.gen_array_sort_with_compare(node) {
				return false
			}
		}
		.sorted_with_compare {
			if !g.gen_array_sorted_with_compare(node) {
				return false
			}
		}
		.reverse {
			g.gen_fixed_array_reverse(node)
		}
		.reverse_in_place {
			g.gen_fixed_array_reverse_in_place(node)
		}
		else {
			return false
		}
	}

	return true
}

fn (mut g Gen) gen_to_str_method_call(node ast.CallExpr, unwrapped_rec_type ast.Type) bool {
	left_node := node.left
	mut rec_type := g.unwrap_generic(g.type_resolver.get_type_or_default(left_node,
		left_node.type()))
	if rec_type in [ast.void_type, ast.no_type] {
		rec_type = unwrapped_rec_type
	}
	if rec_type.has_flag(.shared_f) {
		rec_type = rec_type.clear_flag(.shared_f).set_nr_muls(0)
	}
	if left_node is ast.ComptimeSelector {
		if left_node.typ_key != '' {
			rec_type = g.type_resolver.get_ct_type_or_default(left_node.typ_key, rec_type)
			g.gen_expr_to_string(left_node, rec_type)
			return true
		}
	} else if left_node is ast.PostfixExpr {
		rec_type = g.type_resolver.get_type_or_default(left_node.expr, rec_type)
		if left_node.op == .question {
			rec_type = rec_type.clear_flag(.option)
		}
		g.gen_expr_to_string(left_node, rec_type)
		return true
	} else if left_node is ast.ComptimeCall {
		if left_node.kind == .method {
			sym := g.table.sym(g.unwrap_generic(left_node.left_type))
			if m := sym.find_method(g.comptime.comptime_for_method.name) {
				rec_type = m.return_type
				g.gen_expr_to_string(left_node, rec_type)
				return true
			}
		}
	} else if left_node is ast.Ident {
		if left_node.obj is ast.Var {
			if left_node.obj.ct_type_var != .no_comptime {
				rec_type = g.type_resolver.get_type(left_node)
				// In generic contexts, scope var types may be stale
				if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0
					&& left_node.obj.ct_type_var == .generic_param {
					resolved := g.resolved_expr_type(left_node, rec_type)
					if resolved != 0 {
						rec_type = resolved
					}
				}
				g.gen_expr_to_string(left_node, rec_type)
				return true
			} else if left_node.obj.smartcasts.len > 0 {
				rec_type = g.unwrap_generic(left_node.obj.smartcasts.last())
				cast_sym := g.table.sym(rec_type)
				if cast_sym.info is ast.Aggregate {
					rec_type = cast_sym.info.types[g.aggregate_type_idx]
				}
				g.gen_expr_to_string(left_node, rec_type)
				return true
			} else if left_node.or_expr.kind == .propagate_option {
				g.gen_expr_to_string(left_node, g.unwrap_generic(node.left_type))
				return true
			}
		}
	} else if left_node is ast.None {
		g.gen_expr_to_string(left_node, ast.none_type)
		return true
	} else if node.left_type.has_flag(.option) {
		g.gen_expr_to_string(left_node, g.unwrap_generic(node.left_type))
		return true
	}
	if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
		resolved_rec_type := g.resolved_expr_type(left_node, node.left_type)
		if resolved_rec_type != 0 && resolved_rec_type != ast.void_type {
			g.gen_expr_to_string(left_node,
				g.unwrap_generic(g.recheck_concrete_type(resolved_rec_type)))
			return true
		}
	}
	rec_sym := g.table.sym(rec_type)
	if g.alias_uses_parent_str(rec_sym) {
		rec_type = (rec_sym.info as ast.Alias).parent_type
	}
	g.get_str_fn(rec_type)
	return false
}

fn (g &Gen) alias_uses_parent_str(sym ast.TypeSymbol) bool {
	if sym.info !is ast.Alias || sym.has_method('str') {
		return false
	}
	alias_info := sym.info as ast.Alias
	parent_sym := g.table.sym(alias_info.parent_type)
	return parent_sym.is_string() || parent_sym.is_number()
		|| parent_sym.kind in [.bool, .char, .byteptr, .charptr, .voidptr, .map]
}

fn (g &Gen) alias_keeps_parent_method_dispatch(sym ast.TypeSymbol, method_name string, kind ast.CallKind) bool {
	if sym.info !is ast.Alias || sym.has_method(method_name) {
		return false
	}
	return kind != .str || g.alias_uses_parent_str(sym)
}

// resolve_return_type resolves the generic return type of CallExpr
fn (mut g Gen) resolve_return_type(node ast.CallExpr) ast.Type {
	if node.is_method {
		if node.kind == .map && node.return_type != 0 && !node.return_type.has_flag(.generic)
			&& !g.type_has_unresolved_generic_parts(node.return_type)
			&& !(g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0) {
			return if node.or_block.kind == .absent {
				g.unwrap_generic(g.recheck_concrete_type(node.return_type))
			} else {
				g.unwrap_generic(g.recheck_concrete_type(node.return_type)).clear_option_and_result()
			}
		}
		mut left_type := g.resolved_expr_type(node.left, node.left_type)
		if left_type == 0 || left_type == ast.void_type {
			left_type = node.left_type
		}
		left_type = g.recheck_concrete_type(left_type)
		if left_type == 0 || left_type == ast.void_type {
			return ast.void_type
		}
		left_sym := g.table.sym(left_type)
		final_left_sym := g.table.final_sym(g.unwrap_generic(left_type))
		if final_left_sym.kind == .map && node.kind in [.keys, .values] {
			map_info := final_left_sym.info as ast.Map
			return_type := if node.kind == .keys {
				ast.idx_to_type(g.table.find_or_register_array(map_info.key_type))
			} else {
				ast.idx_to_type(g.table.find_or_register_array(map_info.value_type))
			}
			return if node.or_block.kind == .absent {
				return_type
			} else {
				return_type.clear_option_and_result()
			}
		}
		if final_left_sym.kind == .map && node.name in ['clone', 'move'] {
			return g.unwrap_generic(left_type)
		}
		if final_left_sym.kind in [.array, .array_fixed] && !(left_sym.has_method(node.name)
			|| left_sym.has_method_with_generic_parent(node.name)) && (node.name == 'get'
			|| node.kind in [.first, .last, .pop_left, .pop, .map, .filter, .reverse, .clone, .clone_to_depth, .repeat, .trim, .slice, .sorted, .sorted_with_compare]) {
			return_type := g.resolved_array_builtin_method_return_type(node, left_type,
				node.return_type)
			return if node.or_block.kind == .absent {
				return_type
			} else {
				return_type.clear_option_and_result()
			}
		}
		if node.is_field {
			selector := ast.SelectorExpr{
				expr:       node.left
				expr_type:  left_type
				field_name: node.name
			}
			fn_typ := g.resolved_selector_field_type(selector, left_type)
			if fn_typ != 0 {
				fn_sym := g.table.final_sym(fn_typ.clear_option_and_result())
				if fn_sym.info is ast.FnType {
					return_type :=
						g.unwrap_generic(g.recheck_concrete_type(fn_sym.info.func.return_type))
					if return_type != 0 {
						return if node.or_block.kind == .absent {
							return_type
						} else {
							return_type.clear_option_and_result()
						}
					}
				}
			}
		}
		func := left_sym.find_method_with_generic_parent(node.name) or {
			g.table.find_method(left_sym, node.name) or { return ast.void_type }
		}
		if func.return_type != 0 && !func.return_type.has_flag(.generic)
			&& !g.type_has_unresolved_generic_parts(func.return_type) {
			return if node.or_block.kind == .absent {
				func.return_type
			} else {
				func.return_type.clear_option_and_result()
			}
		}
		receiver_concrete_types, parent_method := g.receiver_generic_call_context(left_type,
			node.name)
		if func.generic_names.len > 0 {
			mut concrete_types := if node.concrete_types.len == func.generic_names.len
				&& node.concrete_types.all(it != 0 && !it.has_flag(.generic)
				&& !g.type_has_unresolved_generic_parts(it)) {
				node.concrete_types.map(g.unwrap_generic(it))
			} else if node.raw_concrete_types.len > 0 {
				node.raw_concrete_types.map(g.unwrap_generic(it))
			} else {
				node.concrete_types.map(g.unwrap_generic(it))
			}
			if concrete_types.len == 0 && receiver_concrete_types.len > 0 {
				concrete_types = receiver_concrete_types.clone()
			} else if receiver_concrete_types.len > 0 && concrete_types.len < func.generic_names.len
				&& receiver_concrete_types.len + concrete_types.len == func.generic_names.len {
				method_concrete_types := concrete_types.clone()
				concrete_types = receiver_concrete_types.clone()
				concrete_types << method_concrete_types
			}
			if receiver_concrete_types.len > 0 {
				for i, receiver_ct in receiver_concrete_types {
					if i < concrete_types.len {
						concrete_types[i] = receiver_ct
					}
				}
			}
			mut rec_len := 0
			if left_type.has_flag(.generic) {
				rec_sym := g.table.final_sym(g.unwrap_generic(left_type))
				match rec_sym.info {
					ast.Struct, ast.Interface, ast.SumType {
						rec_len += rec_sym.info.generic_types.len
					}
					else {}
				}
			}
			if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
				offset := if func.is_method { 1 } else { 0 }
				receiver_param_type := if func.params.len > 0 {
					func.params[0].typ
				} else {
					ast.no_type
				}
				mut generic_arg_idx := 0
				for i, arg in node.args {
					param := if func.is_variadic && i >= func.params.len - (offset + 1) {
						func.params.last()
					} else {
						func.params[offset + i]
					}
					if !param.typ.has_flag(.generic) || param.typ == receiver_param_type {
						continue
					}
					slot := rec_len + generic_arg_idx
					generic_arg_idx++
					// Only override if the current concrete type is still unresolved.
					// The checker already resolves concrete types correctly (e.g. T=int for []T with []int arg),
					// so we should not overwrite already-resolved values with the full argument type
					// (which would turn T=int into T=[]int, causing []T to resolve to [][]int).
					if slot < concrete_types.len {
						current_type := concrete_types[slot]
						if current_type != 0 && current_type != ast.void_type
							&& !current_type.has_flag(.generic)
							&& !g.type_has_unresolved_generic_parts(current_type) {
							// When the arg is a generic_param variable, the checker's concrete type
							// may be stale (from a different instantiation). Re-resolve from
							// g.cur_concrete_types and override if different.
							if arg.expr is ast.Ident && arg.expr.obj is ast.Var
								&& (arg.expr.obj as ast.Var).ct_type_var == .generic_param && g.table.sym(param.typ).name in func.generic_names {
								mut resolved :=
									g.resolve_current_fn_generic_param_type(arg.expr.name)
								if (arg.is_mut || (arg.expr.obj as ast.Var).is_mut)
									&& resolved.is_ptr() {
									resolved = resolved.deref()
								}
								if resolved != 0 && resolved != ast.void_type
									&& resolved != current_type {
									concrete_types[slot] = resolved
								}
							}
							continue
						}
					}
					mut resolved_arg_type := ast.void_type
					if arg.expr is ast.Ident && arg.expr.obj is ast.Var
						&& (arg.expr.obj as ast.Var).ct_type_var == .generic_param {
						resolved_arg_type = g.resolve_current_fn_generic_param_type(arg.expr.name)
						if (arg.is_mut || (arg.expr.obj as ast.Var).is_mut)
							&& resolved_arg_type.is_ptr() {
							resolved_arg_type = resolved_arg_type.deref()
						}
					}
					if resolved_arg_type == ast.void_type {
						resolved_arg_type = g.resolved_generic_call_arg_type(arg)
					}
					if arg.expr.is_auto_deref_var() && resolved_arg_type.is_ptr()
						&& !param.typ.is_ptr() {
						resolved_arg_type = resolved_arg_type.deref()
					}
					if resolved_arg_type.clear_option_and_result() == ast.none_type {
						continue
					}
					if resolved_arg_type != 0 && resolved_arg_type != ast.void_type
						&& !resolved_arg_type.has_flag(.generic)
						&& !g.type_has_unresolved_generic_parts(resolved_arg_type)
						&& slot < concrete_types.len {
						g.infer_generic_call_concrete_types_from_types(func.generic_names, mut
							concrete_types, arg, param.typ, resolved_arg_type)
					}
				}
			}

			mut call_ := unsafe { node }
			comptime_args := g.type_resolver.resolve_args(g.cur_fn, func, mut call_, concrete_types)
			if concrete_types.len > 0 {
				for k, v in comptime_args {
					slot := rec_len + k
					if slot < concrete_types.len {
						current_type := concrete_types[slot]
						if current_type == ast.void_type || current_type.has_flag(.generic)
							|| g.type_has_unresolved_generic_parts(current_type) {
							concrete_types[slot] = g.unwrap_generic(v)
						}
					}
				}
			}
			// Prefer the resolved method declaration over the cached call metadata here.
			// Generic rechecks can leave `node.return_type_generic` stale even when the
			// method's concrete type arguments have since been corrected.
			mut return_type_candidates := []ast.Type{}
			if func.return_type != 0 {
				return_type_candidates << func.return_type
			}
			if node.return_type_generic != ast.void_type && node.return_type_generic != 0
				&& node.return_type_generic !in return_type_candidates {
				return_type_candidates << node.return_type_generic
			}
			if parent_method.params.len > 0 && parent_method.return_type != 0
				&& parent_method.return_type !in return_type_candidates {
				return_type_candidates << parent_method.return_type
			}
			for return_type_generic in return_type_candidates {
				if gen_type := g.table.convert_generic_type(return_type_generic,
					func.generic_names, concrete_types)
				{
					if !gen_type.has_flag(.generic) {
						return if node.or_block.kind == .absent {
							gen_type
						} else {
							gen_type.clear_option_and_result()
						}
					}
				}
			}
		}
		if parent_method.params.len > 0 && receiver_concrete_types.len > 0 {
			parent_return_type_generic := if node.return_type_generic != ast.void_type
				&& node.return_type_generic != 0 {
				node.return_type_generic
			} else {
				parent_method.return_type
			}
			receiver_generic_names := g.table.generic_type_names(parent_method.params[0].typ)
			if receiver_generic_names.len == receiver_concrete_types.len {
				if gen_type := g.table.convert_generic_type(parent_return_type_generic,
					receiver_generic_names, receiver_concrete_types)
				{
					if !gen_type.has_flag(.generic) {
						return if node.or_block.kind == .absent {
							gen_type
						} else {
							gen_type.clear_option_and_result()
						}
					}
				}
			}
		}
	} else if node.is_static_method {
		// For static method calls like T.from(s) or T.parse(mut p) in generic contexts,
		// node.return_type may be stale from a previous checker instantiation.
		// Resolve from left_type which retains the .generic flag.
		if node.left_type.has_flag(.generic) && g.cur_fn != unsafe { nil }
			&& g.cur_concrete_types.len > 0 {
			resolved_left := g.unwrap_generic(g.recheck_concrete_type(node.left_type))
			if resolved_left != ast.void_type && resolved_left != 0 {
				mut ret := resolved_left
				if node.return_type.has_flag(.result) {
					ret = ret.set_flag(.result)
				} else if node.return_type.has_flag(.option) {
					ret = ret.set_flag(.option)
				}
				return if node.or_block.kind == .absent {
					ret
				} else {
					ret.clear_option_and_result()
				}
			}
		}
		ret_type := g.unwrap_generic(g.recheck_concrete_type(node.return_type))
		return if node.or_block.kind == .absent {
			ret_type
		} else {
			ret_type.clear_option_and_result()
		}
	} else {
		mut fn_var_type := ast.void_type
		lookup_name := if node.left is ast.Ident { node.left.name } else { node.name }
		resolved_current_type := g.resolve_current_fn_generic_param_type(lookup_name)
		if resolved_current_type != 0
			&& g.table.final_sym(g.unwrap_generic(resolved_current_type)).kind == .function {
			fn_var_type = g.unwrap_generic(g.recheck_concrete_type(resolved_current_type))
		} else if node.is_fn_var {
			fn_var_type = g.unwrap_generic(g.recheck_concrete_type(node.fn_var_type))
		}
		if fn_var_type == 0 {
			if obj := node.scope.find_var(lookup_name) {
				if g.table.final_sym(g.unwrap_generic(obj.typ)).kind == .function {
					fn_var_type = g.unwrap_generic(g.recheck_concrete_type(obj.typ))
				}
			}
		}
		if fn_var_type != 0 {
			fn_sym := g.table.final_sym(fn_var_type)
			if fn_sym.info is ast.FnType {
				return_type :=
					g.unwrap_generic(g.recheck_concrete_type(fn_sym.info.func.return_type))
				if return_type != 0 {
					return if node.or_block.kind == .absent {
						return_type
					} else {
						return_type.clear_option_and_result()
					}
				}
			}
		}
		if func := g.table.find_fn(node.name) {
			if func.generic_names.len > 0 {
				mut concrete_types := g.generic_fn_call_concrete_types(func, node)
				mut call_ := unsafe { node }
				comptime_args := g.type_resolver.resolve_args(g.cur_fn, func, mut call_,
					concrete_types)
				if concrete_types.len > 0 {
					for k, v in comptime_args {
						if k < concrete_types.len {
							current_type := concrete_types[k]
							if current_type == ast.void_type || current_type.has_flag(.generic)
								|| g.type_has_unresolved_generic_parts(current_type) {
								concrete_types[k] = g.unwrap_generic(v)
							}
						}
					}
				}
				return_type_generic := if node.return_type_generic != ast.void_type {
					node.return_type_generic
				} else {
					func.return_type
				}
				if gen_type := g.table.convert_generic_type(return_type_generic,
					func.generic_names, concrete_types)
				{
					if !gen_type.has_flag(.generic) {
						return if node.or_block.kind == .absent {
							gen_type
						} else {
							gen_type.clear_option_and_result()
						}
					}
				}
			}
		}
	}
	return ast.void_type
}

fn (mut g Gen) resolved_array_builtin_method_return_type(node ast.CallExpr, left_type ast.Type, fallback ast.Type) ast.Type {
	mut return_type := g.unwrap_generic(g.recheck_concrete_type(fallback))
	resolved_left_type := g.recheck_concrete_type(g.resolved_expr_type(node.left, left_type))
	if resolved_left_type == 0 {
		return return_type
	}
	resolved_left_sym := g.table.final_sym(g.unwrap_generic(resolved_left_type))
	if resolved_left_sym.kind !in [.array, .array_fixed] {
		return return_type
	}
	if node.name == 'get' {
		elem_type := if resolved_left_sym.info is ast.Array {
			resolved_left_sym.info.elem_type
		} else if resolved_left_sym.info is ast.ArrayFixed {
			resolved_left_sym.info.elem_type
		} else {
			return return_type
		}
		if elem_type == 0 {
			return return_type
		}
		return g.unwrap_generic(g.recheck_concrete_type(elem_type.set_flag(.option)))
	}
	match node.kind {
		.map {
			if node.args.len == 0 {
				return return_type
			}
			arg := node.args[0]
			arg_type := g.unwrap_generic(g.recheck_concrete_type(g.resolved_expr_type(arg.expr,
				arg.typ)))
			if arg_type == 0 {
				return return_type
			}
			arg_sym := g.table.final_sym(g.unwrap_generic(arg_type))
			mapped_type := match arg_sym.info {
				ast.FnType {
					if arg.expr is ast.SelectorExpr {
						arg_type
					} else {
						arg_sym.info.func.return_type
					}
				}
				else {
					arg_type
				}
			}

			resolved_mapped_type := g.unwrap_generic(g.recheck_concrete_type(mapped_type))
			if resolved_mapped_type == 0 {
				return return_type
			}
			return g.table.find_or_register_array(resolved_mapped_type)
		}
		.filter, .reverse, .clone, .clone_to_depth, .repeat, .trim, .slice, .sorted,
		.sorted_with_compare {
			// These methods return a new array by value, never a pointer.
			// Strip pointer flags from mut receivers.
			mut ret := g.unwrap_generic(resolved_left_type).set_nr_muls(0)
			// .filter on a fixed array returns a dynamic array since the
			// result size is determined at runtime.
			if node.kind == .filter && resolved_left_sym.kind == .array_fixed {
				info := resolved_left_sym.info as ast.ArrayFixed
				ret = ast.new_type(g.table.find_or_register_array(info.elem_type))
			}
			return ret
		}
		.first, .last, .pop_left, .pop {
			elem_type := if resolved_left_sym.info is ast.Array {
				resolved_left_sym.info.elem_type
			} else if resolved_left_sym.info is ast.ArrayFixed {
				resolved_left_sym.info.elem_type
			} else {
				return return_type
			}
			if elem_type == 0 {
				return return_type
			}
			return g.unwrap_generic(g.recheck_concrete_type(elem_type))
		}
		else {}
	}

	return return_type
}

fn (mut g Gen) resolve_receiver_name(node ast.CallExpr, unwrapped_rec_type ast.Type, final_left_sym ast.TypeSymbol,
	left_sym ast.TypeSymbol, typ_sym ast.TypeSymbol) string {
	mut receiver_type_name := util.no_dots(g.cc_type(unwrapped_rec_type, false))
	if final_left_sym.kind == .map && node.kind in [.clone, .move] {
		receiver_type_name = 'map'
	}
	if final_left_sym.kind == .array && !(left_sym.kind == .alias && left_sym.has_method(node.name))
		&& node.kind in [.clear, .repeat, .sort_with_compare, .sorted_with_compare, .push_many, .trim, .first, .last, .pop_left, .pop, .clone, .clone_to_depth, .reverse, .slice, .pointers] {
		if !(left_sym.info is ast.Alias && typ_sym.has_method(node.name)) {
			// `array_Xyz_clone` => `builtin__array_clone`
			receiver_type_name = 'array'
		}
	}
	return receiver_type_name
}

fn (mut g Gen) receiver_generic_call_context(left_type ast.Type, method_name string) ([]ast.Type, ast.Fn) {
	rec_sym := g.table.final_sym(left_type)
	mut receiver_concrete_types := []ast.Type{}
	mut parent_method := ast.Fn{}
	match rec_sym.info {
		ast.Struct, ast.Interface, ast.SumType {
			if rec_sym.info.concrete_types.len > 0 {
				receiver_concrete_types = rec_sym.info.concrete_types.map(g.unwrap_generic(it))
			} else if rec_sym.generic_types.len == rec_sym.info.generic_types.len
				&& rec_sym.generic_types != rec_sym.info.generic_types {
				receiver_concrete_types = rec_sym.generic_types.map(g.unwrap_generic(it))
			}
			if rec_sym.info.parent_type.has_flag(.generic) {
				parent_sym := g.table.sym(rec_sym.info.parent_type)
				if m := parent_sym.find_method(method_name) {
					parent_method = m
				}
			}
		}
		ast.GenericInst {
			receiver_concrete_types = rec_sym.info.concrete_types.map(g.unwrap_generic(it))
			if rec_sym.info.parent_idx > 0 {
				parent_sym := g.table.sym(ast.idx_to_type(rec_sym.info.parent_idx))
				if m := parent_sym.find_method(method_name) {
					parent_method = m
				}
			}
		}
		else {}
	}

	// For generic fn-type aliases (e.g. `type ParseFunction[T] = fn(string) !T`),
	// the concrete instantiation is stored as FnType with generic_types holding
	// the concrete types. Check generic_types on the sym and its parent.
	if receiver_concrete_types.len == 0 {
		non_final_sym := g.table.sym(left_type)
		if non_final_sym.info is ast.GenericInst {
			receiver_concrete_types = non_final_sym.info.concrete_types.map(g.unwrap_generic(it))
			if non_final_sym.info.parent_idx > 0 {
				parent_sym := g.table.sym(ast.idx_to_type(non_final_sym.info.parent_idx))
				if m := parent_sym.find_method(method_name) {
					parent_method = m
				}
			}
		} else if non_final_sym.generic_types.len > 0 && non_final_sym.parent_idx > 0
			&& !non_final_sym.generic_types.any(it.has_flag(.generic)) {
			// Concrete generic instantiation (e.g. ParseFunction[string])
			// where generic_types holds the resolved concrete types.
			receiver_concrete_types = non_final_sym.generic_types.map(g.unwrap_generic(it))
			parent_sym := g.table.sym(ast.idx_to_type(non_final_sym.parent_idx))
			if m := parent_sym.find_method(method_name) {
				parent_method = m
			}
		}
	}
	return receiver_concrete_types, parent_method
}

fn (mut g Gen) resolved_generic_call_arg_type(arg ast.CallArg) ast.Type {
	mut arg_type := g.resolved_expr_type(arg.expr, arg.typ)
	if arg.expr is ast.ComptimeCall && arg.expr.method_name == 'method'
		&& g.comptime.comptime_for_method != unsafe { nil } {
		sym := g.table.sym(g.unwrap_generic(arg.expr.left_type))
		if m := sym.find_method(g.comptime.comptime_for_method.name) {
			return m.return_type
		}
	}
	mut has_current_generic_type := false
	mut resolved_current_type := ast.void_type
	if arg.expr is ast.Ident {
		// In comptime variant loops, a smartcast variable's type is dynamically
		// resolved per variant iteration. Don't override it with the generic param
		// resolution (which gives the sumtype, not the variant).
		is_comptime_smartcast := arg.expr.obj is ast.Var && arg.expr.obj.ct_type_var == .smartcast
			&& g.comptime.comptime_for_variant_var.len > 0
		if !is_comptime_smartcast {
			resolved_current_type = g.resolve_current_fn_generic_param_type(arg.expr.name)
		}
		if resolved_current_type != 0 {
			has_current_generic_type = true
			is_mut_source_arg := arg.expr.obj is ast.Var && arg.expr.obj.is_arg
				&& arg.expr.obj.is_mut
			arg_type = if (arg.is_mut || is_mut_source_arg) && resolved_current_type.is_ptr() {
				resolved_current_type.deref()
			} else {
				resolved_current_type
			}
		}
		if arg.expr.obj is ast.Var {
			scope_type := g.resolved_scope_var_type(arg.expr)
			keep_source_generic_type := has_current_generic_type && arg.expr.obj.is_arg
				&& arg.expr.obj.is_mut && scope_type.is_ptr() && !arg_type.is_ptr()
			// When the generic param type was resolved from cur_concrete_types
			// and is fully concrete, trust it over potentially stale scope types.
			skip_scope_override := has_current_generic_type && arg_type != 0
				&& !arg_type.has_flag(.generic) && !g.type_has_unresolved_generic_parts(arg_type)
			if scope_type != 0 && !keep_source_generic_type && !skip_scope_override
				&& (arg_type == 0 || arg_type.has_flag(.generic)
				|| g.type_has_unresolved_generic_parts(arg_type)
				|| has_current_generic_type
				|| g.unwrap_generic(arg_type) != g.unwrap_generic(scope_type)
				|| arg_type == ast.usize_type
				|| (arg.expr.obj as ast.Var).smartcasts.len > 0) {
				arg_type = scope_type
			}
		}
	}
	if !arg.is_mut && arg.expr.is_auto_deref_var() && arg_type.is_ptr() {
		arg_type = arg_type.deref()
	}
	return g.unwrap_generic(g.recheck_concrete_type(arg_type))
}

fn (mut g Gen) generic_param_infer_type(param ast.Param) ast.Type {
	if param.orig_typ != 0 && (param.orig_typ.has_flag(.generic)
		|| g.type_has_unresolved_generic_parts(param.orig_typ)) {
		return param.orig_typ
	}
	return param.typ
}

fn (mut g Gen) infer_generic_call_concrete_types_from_types(generic_names []string, mut concrete_types []ast.Type, arg ast.CallArg, param_typ ast.Type, arg_typ ast.Type) {
	g.infer_generic_call_concrete_types_from_types_inner(generic_names, mut concrete_types, arg,
		param_typ, arg_typ, false)
}

fn (mut g Gen) infer_generic_call_concrete_types_from_types_inner(generic_names []string, mut concrete_types []ast.Type, arg ast.CallArg, param_typ ast.Type, arg_typ ast.Type, from_composite bool) {
	if param_typ == 0 || arg_typ == 0 {
		return
	}
	param_sym := g.table.sym(param_typ)
	arg_sym := g.table.final_sym(g.unwrap_generic(arg_typ))
	if (param_typ.has_flag(.option) && arg_typ.has_flag(.option))
		|| (param_typ.has_flag(.result) && arg_typ.has_flag(.result)) {
		param_inner := param_typ.clear_option_and_result()
		if param_inner.has_flag(.generic) {
			gt_name := g.table.sym(param_inner).name
			mut inferred_type := arg_typ.clear_option_and_result()
			if param_inner.nr_muls() > 0 && inferred_type.nr_muls() > 0 {
				inferred_type = inferred_type.set_nr_muls(0)
			}
			g.set_generic_call_concrete_type(generic_names, mut concrete_types, gt_name,
				inferred_type)
		}
		return
	}
	if param_typ.has_flag(.generic) && param_sym.name in generic_names {
		mut inferred_type := if arg_typ == ast.nil_type { ast.voidptr_type } else { arg_typ }
		mut strip_param_ptr_levels := true
		// `mut` lowers only the outer call argument to a pointer. Once inference
		// recurses through composite wrappers like `[]T`, nested generic types
		// should keep their original pointer indirection.
		if !from_composite && arg.is_mut && !param_typ.is_ptr() && inferred_type.is_ptr() {
			inferred_type = inferred_type.deref()
		}
		if arg.expr is ast.PrefixExpr && arg.expr.op == .amp && param_typ.nr_muls() > 0 {
			inner_type := g.resolved_expr_type(arg.expr.right, arg.expr.right_type)
			if inner_type != 0 {
				inferred_type = g.unwrap_generic(g.recheck_concrete_type(inner_type))
				if arg.expr.right.is_auto_deref_var() && inferred_type.is_ptr() {
					inferred_type = inferred_type.deref()
				}
				strip_param_ptr_levels = false
			}
		} else if arg.expr.is_auto_deref_var() && inferred_type.is_ptr() {
			inferred_type = inferred_type.deref()
		}
		if strip_param_ptr_levels && param_typ.nr_muls() > 0 && inferred_type.nr_muls() > 0 {
			param_muls := param_typ.nr_muls()
			arg_muls := inferred_type.nr_muls()
			inferred_type = if arg_muls >= param_muls {
				inferred_type.set_nr_muls(arg_muls - param_muls)
			} else {
				inferred_type.set_nr_muls(0)
			}
		}
		g.set_generic_call_concrete_type(generic_names, mut concrete_types, param_sym.name,
			inferred_type)
		return
	}
	if param_sym.kind == .array && arg_sym.info is ast.Array {
		param_info := param_sym.array_info()
		g.infer_generic_call_concrete_types_from_types_inner(generic_names, mut concrete_types,
			arg, param_info.elem_type, arg_sym.info.elem_type, true)
		return
	}
	if param_sym.kind == .array_fixed && arg_sym.info is ast.ArrayFixed {
		param_info := param_sym.info as ast.ArrayFixed
		g.infer_generic_call_concrete_types_from_types_inner(generic_names, mut concrete_types,
			arg, param_info.elem_type, arg_sym.info.elem_type, true)
		return
	}
	if param_sym.kind == .map && arg_sym.info is ast.Map {
		param_info := param_sym.map_info()
		g.infer_generic_call_concrete_types_from_types_inner(generic_names, mut concrete_types,
			arg, param_info.key_type, arg_sym.info.key_type, true)
		g.infer_generic_call_concrete_types_from_types_inner(generic_names, mut concrete_types,
			arg, param_info.value_type, arg_sym.info.value_type, true)
		return
	}
	if param_sym.info is ast.FnType && arg_sym.info is ast.FnType {
		g.update_generic_call_concrete_types_from_fn_types(generic_names, mut concrete_types, arg,
			param_sym.info, arg_sym.info)
		return
	}
	match arg_sym.info {
		ast.Struct {
			mut generic_types := []ast.Type{}
			mut arg_concrete_types := []ast.Type{}
			if param_sym.generic_types.len > 0 {
				generic_types = param_sym.generic_types.clone()
			} else {
				generic_types = arg_sym.info.generic_types.clone()
			}
			arg_concrete_types = arg_sym.info.concrete_types.clone()
			if generic_types.len == arg_concrete_types.len {
				for i, gt in generic_types {
					gt_name := g.table.sym(gt).name
					g.set_generic_call_concrete_type(generic_names, mut concrete_types, gt_name,
						arg_concrete_types[i])
				}
			}
		}
		ast.Interface {
			mut generic_types := []ast.Type{}
			mut arg_concrete_types := []ast.Type{}
			if param_sym.generic_types.len > 0 {
				generic_types = param_sym.generic_types.clone()
			} else {
				generic_types = arg_sym.info.generic_types.clone()
			}
			arg_concrete_types = arg_sym.info.concrete_types.clone()
			if generic_types.len == arg_concrete_types.len {
				for i, gt in generic_types {
					gt_name := g.table.sym(gt).name
					g.set_generic_call_concrete_type(generic_names, mut concrete_types, gt_name,
						arg_concrete_types[i])
				}
			}
		}
		ast.SumType {
			mut generic_types := []ast.Type{}
			mut arg_concrete_types := []ast.Type{}
			if param_sym.generic_types.len > 0 {
				generic_types = param_sym.generic_types.clone()
			} else {
				generic_types = arg_sym.info.generic_types.clone()
			}
			arg_concrete_types = arg_sym.info.concrete_types.clone()
			if generic_types.len == arg_concrete_types.len {
				for i, gt in generic_types {
					gt_name := g.table.sym(gt).name
					g.set_generic_call_concrete_type(generic_names, mut concrete_types, gt_name,
						arg_concrete_types[i])
				}
			}
		}
		ast.GenericInst {
			parent_sym := g.table.sym(ast.new_type(arg_sym.info.parent_idx))
			match parent_sym.info {
				ast.Struct {
					generic_types := parent_sym.info.generic_types.clone()
					if generic_types.len == arg_sym.info.concrete_types.len {
						for i, gt in generic_types {
							gt_name := g.table.sym(gt).name
							g.set_generic_call_concrete_type(generic_names, mut concrete_types,
								gt_name, arg_sym.info.concrete_types[i])
						}
					}
				}
				ast.Interface {
					generic_types := parent_sym.info.generic_types.clone()
					if generic_types.len == arg_sym.info.concrete_types.len {
						for i, gt in generic_types {
							gt_name := g.table.sym(gt).name
							g.set_generic_call_concrete_type(generic_names, mut concrete_types,
								gt_name, arg_sym.info.concrete_types[i])
						}
					}
				}
				ast.SumType {
					generic_types := parent_sym.info.generic_types.clone()
					if generic_types.len == arg_sym.info.concrete_types.len {
						for i, gt in generic_types {
							gt_name := g.table.sym(gt).name
							g.set_generic_call_concrete_type(generic_names, mut concrete_types,
								gt_name, arg_sym.info.concrete_types[i])
						}
					}
				}
				else {}
			}
		}
		else {}
	}
}

fn (mut g Gen) set_generic_call_concrete_type(generic_names []string, mut concrete_types []ast.Type, gt_name string, typ ast.Type) {
	idx := generic_names.index(gt_name)
	if idx < 0 || idx >= concrete_types.len {
		return
	}
	resolved_type := ast.mktyp(g.unwrap_generic(g.recheck_concrete_type(typ)))
	if resolved_type == 0 || resolved_type == ast.void_type || resolved_type.has_flag(.generic)
		|| g.type_has_unresolved_generic_parts(resolved_type) {
		return
	}
	current_type := concrete_types[idx]
	if current_type == 0 || current_type == ast.void_type || current_type.has_flag(.generic)
		|| g.type_has_unresolved_generic_parts(current_type) || current_type == resolved_type {
		concrete_types[idx] = resolved_type
	}
}

fn (mut g Gen) update_generic_call_concrete_types_from_fn_types(generic_names []string, mut concrete_types []ast.Type, arg ast.CallArg, param_fn ast.FnType, arg_fn ast.FnType) {
	if param_fn.func.params.len != arg_fn.func.params.len {
		return
	}
	for i, fn_param in param_fn.func.params {
		if fn_param.typ.has_flag(.generic) {
			gt_name := g.table.sym(fn_param.typ).name
			mut inferred := arg_fn.func.params[i].typ
			// Strip pointer muls that come from the generic param (e.g. &T -> &int => T=int)
			if fn_param.typ.nr_muls() > 0 && inferred.nr_muls() > 0 {
				param_muls := fn_param.typ.nr_muls()
				arg_muls := inferred.nr_muls()
				inferred = if arg_muls >= param_muls {
					inferred.set_nr_muls(arg_muls - param_muls)
				} else {
					inferred.set_nr_muls(0)
				}
			}
			g.set_generic_call_concrete_type(generic_names, mut concrete_types, gt_name, inferred)
		}
	}
	if param_fn.func.return_type.has_flag(.generic) {
		gt_name := g.table.sym(param_fn.func.return_type).name
		mut return_type := arg_fn.func.return_type
		if arg.expr is ast.LambdaExpr && return_type.has_flag(.generic) {
			return_type = g.type_resolver.unwrap_generic_expr(arg.expr.expr, return_type)
			if return_type.has_flag(.generic) {
				idx := generic_names.index(g.table.type_to_str(return_type))
				if idx >= 0 && idx < concrete_types.len {
					return_type = concrete_types[idx]
				} else {
					return_type = ast.void_type
				}
			}
		}
		g.set_generic_call_concrete_type(generic_names, mut concrete_types, gt_name, return_type)
	}
}

fn (g &Gen) call_expr_depends_on_comptime_context(node ast.CallExpr) bool {
	if g.type_resolver.info.is_comptime(node.left) {
		return true
	}
	for arg in node.args {
		if arg.ct_expr || g.type_resolver.info.is_comptime(arg.expr) {
			return true
		}
	}
	return false
}

fn (mut g Gen) generic_fn_call_concrete_types(func ast.Fn, node ast.CallExpr) []ast.Type {
	mut concrete_types := []ast.Type{}
	if node.raw_concrete_types.len > 0 {
		concrete_types = node.raw_concrete_types.map(g.resolved_call_concrete_type(it))
	} else if node.concrete_types.len == func.generic_names.len && node.concrete_types.all(it != 0
		&& !it.has_flag(.generic) && !g.type_has_unresolved_generic_parts(it)) {
		concrete_types = node.concrete_types.map(g.resolved_call_concrete_type(it))
	}
	explicit_raw_concrete_types := node.raw_concrete_types.len == func.generic_names.len
		&& concrete_types.len == func.generic_names.len && concrete_types.all(it != 0
		&& !it.has_flag(.generic) && !g.type_has_unresolved_generic_parts(it))
	mut trust_node_concrete_types := concrete_types.len == func.generic_names.len
		&& concrete_types.len > 0
	if trust_node_concrete_types && !explicit_raw_concrete_types
		&& g.call_expr_depends_on_comptime_context(node) {
		trust_node_concrete_types = false
	}
	if trust_node_concrete_types && !explicit_raw_concrete_types {
		if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
			trust_node_concrete_types = false
		}
	}
	if trust_node_concrete_types && !explicit_raw_concrete_types {
		mut muttable := unsafe { &ast.Table(g.table) }
		for i, arg in node.args {
			param := if func.is_variadic && i >= func.params.len - 1 {
				func.params.last()
			} else {
				func.params[i]
			}
			param_infer_typ := g.generic_param_infer_type(param)
			if !param_infer_typ.has_flag(.generic)
				&& !g.type_has_unresolved_generic_parts(param_infer_typ) {
				continue
			}
			arg_type := g.resolved_generic_call_arg_type(arg)
			if arg_type == 0 {
				continue
			}
			if arg_type in [ast.int_literal_type, ast.float_literal_type] {
				continue
			}
			param_sym := g.table.sym(param_infer_typ)
			if (arg.expr is ast.PrefixExpr && arg.expr.op == .amp && param_infer_typ.nr_muls() > 0)
				|| param_sym.info is ast.FnType {
				trust_node_concrete_types = false
				break
			}
			if param_sym.info is ast.FnType
				&& (arg_type.has_flag(.generic) || g.type_has_unresolved_generic_parts(arg_type)) {
				continue
			}
			if resolved_param_type := muttable.convert_generic_type(param_infer_typ,
				func.generic_names, concrete_types)
			{
				// For variadic params, the resolved type is []T_resolved (array wrapper),
				// but arg_type is just T_resolved (element). Extract element type to compare.
				mut cmp_type := g.unwrap_generic(resolved_param_type)
				if param_infer_typ.has_flag(.variadic) {
					cmp_sym := g.table.final_sym(cmp_type)
					if cmp_sym.info is ast.Array {
						cmp_type = cmp_sym.info.elem_type
					}
				}
				resolved_arg_type := g.unwrap_generic(arg_type)
				if cmp_type != resolved_arg_type {
					if g.table.final_sym(cmp_type).kind == .interface
						&& g.table.does_type_implement_interface(resolved_arg_type, cmp_type) {
						continue
					}
					trust_node_concrete_types = false
					break
				}
			} else {
				trust_node_concrete_types = false
				break
			}
		}
	}
	if !trust_node_concrete_types {
		concrete_types = []ast.Type{len: func.generic_names.len, init: ast.void_type}
		mut node_ := unsafe { node }
		comptime_args := g.type_resolver.resolve_args(g.cur_fn, func, mut node_, concrete_types)
		mut generic_arg_idx := 0
		for i, arg in node.args {
			param := if func.is_variadic && i >= func.params.len - 1 {
				func.params.last()
			} else {
				func.params[i]
			}
			param_infer_typ := g.generic_param_infer_type(param)
			if !param_infer_typ.has_flag(.generic)
				&& !g.type_has_unresolved_generic_parts(param_infer_typ) {
				continue
			}
			arg_type := g.resolved_generic_call_arg_type(arg)
			if arg_type != 0 && !arg_type.has_flag(.generic)
				&& !g.type_has_unresolved_generic_parts(arg_type) {
				if generic_arg_idx < concrete_types.len
					&& arg_type in [ast.int_literal_type, ast.float_literal_type] {
					current_type := concrete_types[generic_arg_idx]
					if current_type != 0 && current_type != ast.void_type
						&& !current_type.has_flag(.generic)
						&& !g.type_has_unresolved_generic_parts(current_type) {
						generic_arg_idx++
						continue
					}
				}
				g.infer_generic_call_concrete_types_from_types(func.generic_names, mut
					concrete_types, arg, param_infer_typ, arg_type)
				if param_infer_typ.has_flag(.generic) && generic_arg_idx < concrete_types.len {
					generic_arg_idx++
				}
			}
		}
		for k, v in comptime_args {
			if k < concrete_types.len {
				current_type := concrete_types[k]
				if current_type == ast.void_type || current_type.has_flag(.generic)
					|| g.type_has_unresolved_generic_parts(current_type) {
					concrete_types[k] = ast.mktyp(g.unwrap_generic(v))
				}
			}
		}
		// Fallback: use checker's inferred concrete types for any remaining void types
		// (e.g. when a generic return type R can't be re-inferred from a lambda arg in cgen)
		for i, ct in concrete_types {
			if ct == ast.void_type && i < node.concrete_types.len {
				fallback_type := g.resolved_call_concrete_type(node.concrete_types[i])
				if fallback_type != 0 && fallback_type != ast.void_type
					&& !fallback_type.has_flag(.generic)
					&& !g.type_has_unresolved_generic_parts(fallback_type) {
					concrete_types[i] = fallback_type
				}
			}
		}
	}
	return concrete_types.filter(it != ast.void_type)
}

fn (mut g Gen) current_fn_generic_params() ([]ast.Param, []string) {
	if g.cur_fn == unsafe { nil } || g.cur_fn.generic_names.len == 0
		|| g.cur_concrete_types.len == 0 {
		return []ast.Param{}, []string{}
	}
	if g.cur_fn.name.contains('_T_') {
		for generic_fn in g.file.generic_fns {
			if generic_fn.generic_names.len == 0 {
				continue
			}
			if g.generic_fn_name(g.cur_concrete_types, generic_fn.name) == g.cur_fn.name {
				return generic_fn.params, generic_fn.generic_names
			}
		}
		return g.cur_fn.params, g.cur_fn.generic_names
	}
	if func := g.table.find_fn(g.cur_fn.name) {
		if func.generic_names.len > 0 {
			return func.params, func.generic_names
		}
		return func.params, g.cur_fn.generic_names
	} else if g.cur_fn.mod != '' && !g.cur_fn.name.contains('.') {
		if func := g.table.find_fn('${g.cur_fn.mod}.${g.cur_fn.name}') {
			if func.generic_names.len > 0 {
				return func.params, func.generic_names
			}
			return func.params, g.cur_fn.generic_names
		}
	}
	return g.cur_fn.params, g.cur_fn.generic_names
}

fn (mut g Gen) current_fn_generic_names() []string {
	_, generic_names := g.current_fn_generic_params()
	return generic_names
}

fn (g &Gen) has_active_call_generic_context() bool {
	return g.active_call_generic_names.len > 0
		&& g.active_call_generic_names.len == g.active_call_concrete_types.len
}

fn (g &Gen) active_call_generic_concrete_types(generic_names []string) []ast.Type {
	if generic_names.len == 0 || !g.has_active_call_generic_context() {
		return []ast.Type{}
	}
	mut concrete_types := []ast.Type{cap: generic_names.len}
	for generic_name in generic_names {
		idx := g.active_call_generic_names.index(generic_name)
		if idx < 0 || idx >= g.active_call_concrete_types.len {
			return []ast.Type{}
		}
		concrete_types << g.active_call_concrete_types[idx]
	}
	return concrete_types
}

fn (g &Gen) active_call_lambda_concrete_types(node ast.LambdaExpr) []ast.Type {
	if node.func == unsafe { nil } || node.func.decl.generic_names.len == 0 {
		return []ast.Type{}
	}
	concrete_types := g.active_call_generic_concrete_types(node.func.decl.generic_names)
	if concrete_types.len > 0 {
		return concrete_types
	}
	if node.call_ctx != unsafe { nil }
		&& node.call_ctx.concrete_types.len == node.func.decl.generic_names.len {
		return node.call_ctx.concrete_types.clone()
	}
	return []ast.Type{}
}

fn (mut g Gen) resolve_active_call_concrete_type(typ ast.Type) ast.Type {
	if typ == 0 {
		return 0
	}
	if g.has_active_call_generic_context() {
		mut muttable := unsafe { &ast.Table(g.table) }
		resolved_typ := muttable.unwrap_generic_type_ex(typ, g.active_call_generic_names,
			g.active_call_concrete_types, true)
		if resolved_typ != typ {
			return g.unwrap_generic(resolved_typ)
		}
		if converted_typ := muttable.convert_generic_type(typ, g.active_call_generic_names,
			g.active_call_concrete_types)
		{
			return g.unwrap_generic(converted_typ)
		}
	}
	return g.resolve_current_fn_generic_type(typ)
}

fn (mut g Gen) call_args_with_context(node ast.CallExpr, generic_names []string, concrete_types []ast.Type) {
	old_generic_names := g.active_call_generic_names
	old_concrete_types := g.active_call_concrete_types
	if generic_names.len > 0 && generic_names.len == concrete_types.len {
		g.active_call_generic_names = generic_names
		g.active_call_concrete_types = concrete_types
	} else {
		g.active_call_generic_names = []string{}
		g.active_call_concrete_types = []ast.Type{}
	}
	defer {
		g.active_call_generic_names = old_generic_names
		g.active_call_concrete_types = old_concrete_types
	}
	g.call_args(node)
}

fn (mut g Gen) refresh_current_generic_fn_scope_vars(node &ast.FnDecl) {
	if node == unsafe { nil } || node.scope == unsafe { nil } || g.cur_concrete_types.len == 0
		|| node.generic_names.len != g.cur_concrete_types.len {
		return
	}
	if node.is_method {
		receiver_type := g.recheck_concrete_type(node.receiver.typ)
		if mut receiver_var := node.scope.find_var(node.receiver.name) {
			receiver_var.typ = receiver_type
			receiver_var.orig_type = ast.no_type
			receiver_var.smartcasts = []
			receiver_var.is_unwrapped = false
		}
	}
	for param in node.params {
		param_type := g.recheck_concrete_type(param.typ)
		if mut param_var := node.scope.find_var(param.name) {
			param_var.typ = param_type
			param_var.orig_type = ast.no_type
			param_var.smartcasts = []
			param_var.is_unwrapped = false
		}
	}
	g.refresh_current_generic_local_scope_vars(node.scope)
}

fn (mut g Gen) refresh_current_generic_local_scope_vars(scope &ast.Scope) {
	if scope == unsafe { nil } {
		return
	}
	mut scope_ := unsafe { &ast.Scope(scope) }
	for name, obj in scope_.objects {
		match obj {
			ast.Var {
				if obj.is_arg {
					continue
				}
				mut var := obj
				if var.generic_typ != 0 {
					refreshed_generic_type :=
						g.unwrap_generic(g.recheck_concrete_type(var.generic_typ))
					if refreshed_generic_type != 0 {
						var.typ = refreshed_generic_type
					}
				}
				has_smartcast_state := var.orig_type != ast.no_type || var.smartcasts.len > 0
					|| var.is_unwrapped
				if var.is_inherited && scope_.parent != unsafe { nil } && !has_smartcast_state {
					if parent_var := scope_.parent.find_var(name) {
						var.typ = parent_var.typ
						var.orig_type = parent_var.orig_type
						var.smartcasts = parent_var.smartcasts.clone()
						var.is_unwrapped = parent_var.is_unwrapped
					}
				} else if var.expr !is ast.EmptyExpr {
					should_resolve_expr_type := var.typ == 0
						|| var.typ == ast.void_type || var.typ.has_flag(.generic)
						|| g.type_has_unresolved_generic_parts(var.typ)
						|| (var.expr is ast.StructInit && var.expr.typ_str.len > 0
						&& g.current_fn_generic_names().index(var.expr.typ_str.all_after_last('.')) >= 0)
					if should_resolve_expr_type && !(var.expr is ast.Ident && var.expr.name == name) {
						mut resolved_type := g.resolved_expr_type(var.expr, var.typ)
						if resolved_type != 0 {
							// Mirror the checker's `:=` behavior for auto-deref vars.
							// Generic cgen refreshes re-evaluate initializer expressions,
							// so `mut x := param` must stay a value copy here as well.
							if g.is_auto_deref_source_ident(var.expr) && resolved_type.is_ptr() {
								resolved_type = resolved_type.deref()
							}
							var.typ = g.unwrap_generic(g.recheck_concrete_type(resolved_type))
						}
					} else if var.typ != 0 {
						var.typ = g.unwrap_generic(g.recheck_concrete_type(var.typ))
					}
					if !has_smartcast_state {
						var.orig_type = ast.no_type
						var.smartcasts = []
						var.is_unwrapped = false
					}
				} else if var.typ != 0
					&& (var.typ.has_flag(.generic) || g.type_has_unresolved_generic_parts(var.typ)) {
					var.typ = g.unwrap_generic(g.recheck_concrete_type(var.typ))
				}
				if has_smartcast_state {
					if var.typ != 0 {
						var.typ = g.unwrap_generic(g.recheck_concrete_type(var.typ))
					}
					if var.orig_type != ast.no_type {
						var.orig_type = g.recheck_concrete_type(var.orig_type)
					}
					if var.smartcasts.len > 0 {
						var.smartcasts =
							var.smartcasts.filter(it != 0).map(g.recheck_concrete_type(it))
					}
				}
				scope_.objects[name] = var
			}
			else {}
		}
	}
	for child in scope_.children {
		g.refresh_current_generic_local_scope_vars(child)
	}
}

fn (mut g Gen) resolve_generic_param_type_from_params(name string, params []ast.Param, generic_names []string) ast.Type {
	if params.len == 0 || generic_names.len == 0 {
		return 0
	}
	for param in params {
		if param.name != name {
			continue
		}
		mut muttable := unsafe { &ast.Table(g.table) }
		if resolved := muttable.convert_generic_type(param.typ, generic_names, g.cur_concrete_types) {
			return g.unwrap_generic(resolved)
		}
	}
	return 0
}

fn (mut g Gen) resolve_current_fn_generic_param_type(name string) ast.Type {
	if g.cur_fn == unsafe { nil } || g.cur_fn.generic_names.len == 0
		|| g.cur_concrete_types.len == 0 {
		return 0
	}
	if g.cur_fn.name.contains('_T_') {
		for generic_fn in g.file.generic_fns {
			if generic_fn.generic_names.len == 0 {
				continue
			}
			if g.generic_fn_name(g.cur_concrete_types, generic_fn.name) == g.cur_fn.name {
				return g.resolve_generic_param_type_from_params(name, generic_fn.params,
					generic_fn.generic_names)
			}
		}
		return g.resolve_generic_param_type_from_params(name, g.cur_fn.params,
			g.cur_fn.generic_names)
	}
	if func := g.table.find_fn(g.cur_fn.name) {
		if func.generic_names.len > 0 {
			return g.resolve_generic_param_type_from_params(name, func.params, func.generic_names)
		}
		return g.resolve_generic_param_type_from_params(name, func.params, g.cur_fn.generic_names)
	} else if g.cur_fn.mod != '' && !g.cur_fn.name.contains('.') {
		if func := g.table.find_fn('${g.cur_fn.mod}.${g.cur_fn.name}') {
			if func.generic_names.len > 0 {
				return g.resolve_generic_param_type_from_params(name, func.params,
					func.generic_names)
			}
			return g.resolve_generic_param_type_from_params(name, func.params,
				g.cur_fn.generic_names)
		}
	}
	return g.resolve_generic_param_type_from_params(name, g.cur_fn.params, g.cur_fn.generic_names)
}

fn (mut g Gen) resolve_current_fn_generic_param_value_type(name string) ast.Type {
	params, generic_names := g.current_fn_generic_params()
	if params.len == 0 || generic_names.len == 0 {
		return 0
	}
	for param in params {
		if param.name != name {
			continue
		}
		mut muttable := unsafe { &ast.Table(g.table) }
		resolved_param_type := if resolved := muttable.convert_generic_type(param.typ,
			generic_names, g.cur_concrete_types)
		{
			g.unwrap_generic(resolved)
		} else {
			g.unwrap_generic(g.recheck_concrete_type(param.typ))
		}
		base_type := g.table.value_type(resolved_param_type)
		if base_type == 0 {
			return 0
		}
		if resolved := muttable.convert_generic_type(base_type, generic_names, g.cur_concrete_types) {
			return g.unwrap_generic(resolved)
		}
		return g.unwrap_generic(g.recheck_concrete_type(base_type))
	}
	return 0
}

fn (mut g Gen) resolve_current_fn_generic_param_key_type(name string) ast.Type {
	params, generic_names := g.current_fn_generic_params()
	if params.len == 0 || generic_names.len == 0 {
		return 0
	}
	for param in params {
		if param.name != name {
			continue
		}
		mut muttable := unsafe { &ast.Table(g.table) }
		resolved_param_type := if resolved := muttable.convert_generic_type(param.typ,
			generic_names, g.cur_concrete_types)
		{
			g.unwrap_generic(resolved)
		} else {
			g.unwrap_generic(g.recheck_concrete_type(param.typ))
		}
		param_sym := g.table.final_sym(resolved_param_type)
		base_type := match param_sym.kind {
			.map { param_sym.map_info().key_type }
			.array, .array_fixed, .string { ast.int_type }
			else { ast.Type(0) }
		}

		if base_type == 0 {
			return 0
		}
		if resolved := muttable.convert_generic_type(base_type, generic_names, g.cur_concrete_types) {
			return g.unwrap_generic(resolved)
		}
		return g.unwrap_generic(g.recheck_concrete_type(base_type))
	}
	return 0
}

fn (mut g Gen) unwrap_receiver_type(node ast.CallExpr) (ast.Type, &ast.TypeSymbol) {
	mut left_type := g.unwrap_generic(node.left_type)
	mut match_variant_type := ast.Type(0)
	if node.left is ast.Ident {
		match_variant_type = g.current_sumtype_match_variant_type(node.left, node.left_type)
		if match_variant_type != 0 {
			left_type = match_variant_type
		} else {
			resolved_left_type := g.resolve_current_fn_generic_param_type(node.left.name)
			if resolved_left_type != 0 {
				left_type = resolved_left_type
			}
		}
	} else if node.left is ast.StructInit {
		if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
			si_typ := if node.left.generic_typ != 0 {
				node.left.generic_typ
			} else {
				node.left.typ
			}
			resolved := g.unwrap_generic(si_typ)
			if resolved != 0 && !resolved.has_flag(.generic) {
				left_type = resolved
			}
		}
	}
	if left_type == g.unwrap_generic(node.left_type) {
		resolved_left_type := g.type_resolver.get_type_or_default(node.left, node.left_type)
		if resolved_left_type != 0 {
			left_type = g.unwrap_generic(resolved_left_type)
		}
	}
	mut unwrapped_rec_type := node.receiver_type
	if g.cur_fn != unsafe { nil } && g.cur_fn.generic_names.len > 0 { // in generic fn
		unwrapped_rec_type = g.unwrap_generic(node.receiver_type)
		unwrapped_rec_type = g.type_resolver.unwrap_generic_expr(node.left, unwrapped_rec_type)
	} else { // in non-generic fn
		sym := g.table.sym(node.receiver_type)
		match sym.info {
			ast.Struct, ast.Interface, ast.SumType {
				generic_names := sym.info.generic_types.map(g.table.sym(it).name)
				// see comment at top of vlib/v/gen/c/utils.v
				mut muttable := unsafe { &ast.Table(g.table) }
				// If receiver_type is the generic parent but left_type is an instantiated version,
				// use left_type's concrete_types for the conversion
				mut concrete_types := sym.info.concrete_types.clone()
				if concrete_types.len == 0 && generic_names.len > 0 {
					left_sym := g.table.sym(left_type)
					if left_sym.info is ast.Interface {
						concrete_types = left_sym.info.concrete_types.clone()
					} else if left_sym.info is ast.Struct {
						concrete_types = left_sym.info.concrete_types.clone()
					} else if left_sym.info is ast.SumType {
						concrete_types = left_sym.info.concrete_types.clone()
					}
				}
				if utyp := muttable.convert_generic_type(node.receiver_type, generic_names,
					concrete_types)
				{
					unwrapped_rec_type = utyp
				} else if concrete_types.len > 0 && !left_type.has_flag(.generic) {
					// Fallback: if convert_generic_type fails (e.g. method uses
					// different generic param name than struct), use left_type directly
					unwrapped_rec_type = left_type
				}
			}
			else {}
		}
	}
	if node.from_embed_types.len == 0 && node.left is ast.Ident {
		if match_variant_type != 0 {
			unwrapped_rec_type = match_variant_type
		} else if node.left.obj is ast.Var {
			if node.left.obj.smartcasts.len > 0 {
				if node.left.obj.ct_type_var == .smartcast {
					unwrapped_rec_type =
						g.unwrap_generic(g.type_resolver.get_type(ast.Expr(node.left)))
				} else {
					unwrapped_rec_type = g.unwrap_generic(node.left.obj.smartcasts.last())
					cast_sym := g.table.sym(unwrapped_rec_type)
					if cast_sym.info is ast.Aggregate {
						unwrapped_rec_type = cast_sym.info.types[g.aggregate_type_idx]
					}
				}
			}
		}
	}
	mut typ_sym := g.table.sym(unwrapped_rec_type)
	mut left_sym := g.table.sym(left_type)
	if left_type != 0 && left_type != g.unwrap_generic(node.receiver_type)
		&& left_sym.kind != .aggregate && left_sym.has_method(node.name)
		&& node.from_embed_types.len == 0 {
		unwrapped_rec_type = left_type
		typ_sym = left_sym
	}
	// non-option alias type that undefined this method (not include `str`) need to use parent type
	if !left_type.has_flag(.option) && mut typ_sym.info is ast.Alias && node.kind != .str
		&& !typ_sym.has_method(node.name) {
		unwrapped_rec_type = typ_sym.info.parent_type
		typ_sym = g.table.sym(unwrapped_rec_type)
	} else if mut typ_sym.info is ast.Array && !typ_sym.has_method(node.name) && node.kind != .str {
		typ := g.table.unaliased_type(typ_sym.info.elem_type)
		typ_idx := g.table.find_type_idx(g.table.array_name(typ))
		if typ_idx > 0 {
			unwrapped_rec_type = ast.idx_to_type(typ_idx)
			typ_sym = g.table.sym(unwrapped_rec_type)
		}
	}
	// In generic contexts, node.from_embed_types and node.receiver_type may be stale
	// from a previous instantiation. Re-resolve embedded methods from the actual left_type.
	if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
		left_sym2 := g.table.final_sym(left_type)
		if !left_sym2.has_method(node.name) {
			_, embed_types := g.table.find_method_from_embeds(left_sym2, node.name) or {
				ast.Fn{}, []ast.Type{}
			}
			if embed_types.len > 0 {
				unwrapped_rec_type = embed_types.last()
				typ_sym = g.table.sym(unwrapped_rec_type)
			}
		} else if left_type != unwrapped_rec_type {
			// left_type has the method directly; use it instead of stale receiver type
			unwrapped_rec_type = left_type
			typ_sym = g.table.sym(unwrapped_rec_type)
		}
	} else if node.from_embed_types.len > 0 && !typ_sym.has_method(node.name) {
		unwrapped_rec_type = node.from_embed_types.last()
		typ_sym = g.table.sym(unwrapped_rec_type)
	}
	return unwrapped_rec_type, typ_sym
}

fn (mut g Gen) is_same_generic_receiver_family(left_type ast.Type, receiver_type ast.Type) bool {
	if left_type in [ast.void_type, ast.no_type] || receiver_type in [ast.void_type, ast.no_type] {
		return false
	}
	left_sym := g.table.final_sym(g.unwrap_generic(left_type))
	receiver_sym := g.table.final_sym(g.unwrap_generic(receiver_type))
	if left_sym.kind != receiver_sym.kind {
		return false
	}
	match left_sym.info {
		ast.Struct {
			if receiver_sym.info is ast.Struct {
				left_parent := left_sym.info.parent_type
				receiver_parent := receiver_sym.info.parent_type
				return left_parent != 0 && left_parent == receiver_parent
					&& left_parent.has_flag(.generic)
			}
		}
		ast.Interface {
			if receiver_sym.info is ast.Interface {
				left_parent := left_sym.info.parent_type
				receiver_parent := receiver_sym.info.parent_type
				return left_parent != 0 && left_parent == receiver_parent
					&& left_parent.has_flag(.generic)
			}
		}
		ast.SumType {
			if receiver_sym.info is ast.SumType {
				left_parent := left_sym.info.parent_type
				receiver_parent := receiver_sym.info.parent_type
				return left_parent != 0 && left_parent == receiver_parent
					&& left_parent.has_flag(.generic)
			}
		}
		else {}
	}

	return false
}

fn (mut g Gen) method_call(node ast.CallExpr) {
	// TODO: there are still due to unchecked exprs (opt/some fn arg)
	if node.left_type == 0 {
		g.checker_bug('CallExpr.left_type is 0 in method_call', node.pos)
	}
	if node.receiver_type == 0 {
		g.checker_bug('CallExpr.receiver_type is 0 in method_call', node.pos)
	}
	mut left_type := g.unwrap_generic(node.left_type)
	mut receiver_type := node.receiver_type
	match node.left {
		ast.Ident {
			match_variant_type := g.current_sumtype_match_variant_type(node.left, node.left_type)
			if match_variant_type != 0 {
				left_type = match_variant_type
			} else {
				resolved_left_type := g.resolve_current_fn_generic_param_type(node.left.name)
				if resolved_left_type != 0 {
					left_type = resolved_left_type
				} else if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
					scope_type := g.resolved_scope_var_type(node.left)
					if scope_type != 0 && !scope_type.has_flag(.generic)
						&& !g.type_has_unresolved_generic_parts(scope_type) {
						left_type = scope_type
					}
				}
			}
		}
		ast.SelectorExpr {
			// In generic contexts, the selector's type may be stale from a previous
			// instantiation. Re-resolve through the struct field type hierarchy.
			if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
				resolved := g.resolved_expr_type(node.left, node.left_type)
				if resolved != 0 && resolved != left_type {
					left_type = g.unwrap_generic(resolved)
				}
			}
		}
		ast.IndexExpr {
			if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
				resolved := g.resolved_expr_type(node.left, node.left_type)
				if resolved != 0 && resolved != left_type {
					left_type = g.unwrap_generic(resolved)
				}
			}
		}
		ast.CallExpr, ast.ParExpr, ast.PostfixExpr {
			if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
				resolved := g.resolved_expr_type(node.left, node.left_type)
				if resolved != 0 && resolved != left_type {
					left_type = g.unwrap_generic(resolved)
				}
			}
		}
		ast.StructInit {
			// In generic contexts, node.left_type may be stale from a previous
			// checker instantiation (the checker overwrites AST types on each pass).
			// Use the struct init's own .typ which preserves the generic flag.
			if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
				si_typ := if node.left.generic_typ != 0 {
					node.left.generic_typ
				} else {
					node.left.typ
				}
				resolved := g.unwrap_generic(si_typ)
				if resolved != 0 && !resolved.has_flag(.generic) {
					left_type = resolved
				}
			}
		}
		else {}
	}

	if left_type == g.unwrap_generic(node.left_type) {
		resolved_left_type := g.type_resolver.get_type_or_default(node.left, node.left_type)
		if resolved_left_type != 0 {
			left_type = g.unwrap_generic(resolved_left_type)
		}
	}
	raw_method_name := node.name
	mut method_name := node.name
	if t_idx := method_name.index('_T_') {
		base_method_name := method_name[..t_idx]
		if base_method_name.len > 0 && left_type != 0 {
			left_type_sym := g.table.sym(left_type)
			mut has_base_method := left_type_sym.has_method(base_method_name)
				|| left_type_sym.has_method_with_generic_parent(base_method_name)
			if !has_base_method && !left_type.has_flag(.option) && left_type_sym.info is ast.Alias {
				unaliased_left_type := g.table.unaliased_type(left_type)
				unaliased_left_sym := g.table.sym(unaliased_left_type)
				has_base_method = unaliased_left_sym.has_method(base_method_name)
					|| unaliased_left_sym.has_method_with_generic_parent(base_method_name)
			}
			if has_base_method {
				method_name = base_method_name
			}
		}
	}
	mut unwrapped_rec_type, _ := g.unwrap_receiver_type(node)
	if left_type != 0 && (left_type != g.unwrap_generic(node.receiver_type)
		|| left_type != unwrapped_rec_type) {
		resolved_left_sym := g.table.sym(left_type)
		if resolved_left_sym.kind != .aggregate && (resolved_left_sym.has_method(method_name)
			|| resolved_left_sym.has_method_with_generic_parent(method_name)) {
			unwrapped_rec_type = left_type
			receiver_type = left_type.derive(node.receiver_type).clear_flag(.generic)
		}
	}
	left_sym_for_alias := g.table.sym(left_type)
	if left_type != 0 && !left_type.has_flag(.option) && node.kind != .str
		&& left_sym_for_alias.info is ast.Alias && !left_sym_for_alias.has_method(method_name) {
		unaliased_left_type := g.table.unaliased_type(left_type)
		unaliased_left_sym := g.table.sym(unaliased_left_type)
		if unaliased_left_sym.has_method(method_name)
			|| unaliased_left_sym.has_method_with_generic_parent(method_name) {
			unwrapped_rec_type = unaliased_left_type
			receiver_type = unaliased_left_type.derive(node.receiver_type).clear_flag(.generic)
		}
	}
	// For interface methods inherited via embedding, the receiver_type was changed
	// to the parent interface, but the C function uses the original defining interface.
	unwrapped_rec_sym := g.table.sym(unwrapped_rec_type)
	if unwrapped_rec_sym.kind == .interface {
		if em := unwrapped_rec_sym.find_method(raw_method_name) {
			if em.from_embedded_type != 0 {
				unwrapped_rec_type = em.from_embedded_type
			}
		}
	}
	typ_sym := g.table.sym(unwrapped_rec_type)
	mut receiver_is_mut := false
	if method := typ_sym.find_method(method_name) {
		if method.params.len > 0 {
			receiver_type = receiver_type.derive(method.params[0].typ).clear_flag(.generic)
			receiver_is_mut = method.params[0].is_mut
		}
	}
	rec_cc_type := g.cc_type(unwrapped_rec_type, false)
	mut receiver_type_name := util.no_dots(rec_cc_type)
	if node.kind == .clone && node.left is ast.IndexExpr && node.left.index is ast.RangeExpr {
		resolved_return_type := g.unwrap_generic(g.recheck_concrete_type(node.return_type))
		resolved_return_sym := g.table.final_sym(resolved_return_type)
		if resolved_return_sym.kind == .array {
			array_info := resolved_return_sym.info as ast.Array
			array_depth := g.get_array_depth(array_info.elem_type)
			g.write('builtin__array_clone_static_to_depth(')
			g.expr(ast.Expr(node.left))
			g.write(', ${array_depth})')
			return
		}
	}
	if typ_sym.info is ast.Interface && typ_sym.info.defines_method(method_name) {
		// Speaker_name_table[s._interface_idx].speak(s._object)
		$if debug_interface_method_call ? {
			eprintln('>>> interface typ_sym.name: ${typ_sym.name} | receiver_type_name: ${receiver_type_name} | pos: ${node.pos}')
		}

		methods_struct_name := g.interface_methods_struct_name(left_type)
		g.write('((${methods_struct_name}*)(')
		if node.left.is_auto_deref_var() && left_type.nr_muls() > 1 {
			g.write2('(', '*'.repeat(left_type.nr_muls() - 1))
			g.expr(node.left)
			g.write(')')
		} else {
			g.expr(node.left)
		}
		dot := g.dot_or_ptr(left_type)
		mname := c_fn_name(method_name)
		g.write('${dot}_methods))->_method_${mname}(')
		if node.left.is_auto_deref_var() && left_type.nr_muls() > 1 {
			g.write2('(', '*'.repeat(left_type.nr_muls() - 1))
			g.expr(node.left)
			g.write(')')
		} else {
			g.expr(node.left)
		}
		g.write('${dot}_object')
		is_variadic := node.expected_arg_types.len > 0
			&& node.expected_arg_types.last().has_flag(.variadic)
		if node.args.len > 0 || is_variadic {
			g.write(', ')
			g.call_args(node)
		}
		g.write(')')
		if !node.return_type.has_option_or_result() {
			if g.table.final_sym(node.return_type).kind == .array_fixed {
				g.write('.ret_arr')
			}
		}
		return
	} else {
		thread_sym := typ_sym
		if thread_sym.info is ast.Thread {
			waiter_fn_name := g.gen_gohandle_name(thread_sym.info.return_type)
			g.create_waiter_handler(node.return_type, g.styp(thread_sym.info.return_type),
				waiter_fn_name)
		}
	}
	left_sym := g.table.sym(left_type)
	final_left_sym := g.table.final_sym(left_type)
	mut use_builtin_array_sort := false
	if final_left_sym.kind == .array && node.kind in [.sort, .sorted] && node.args.len > 0 {
		if method := left_sym.find_method(method_name) {
			use_builtin_array_sort = method.params.len == 1
		}
	} else if final_left_sym.kind == .array
		&& node.kind in [.sort_with_compare, .sorted_with_compare] && node.args.len == 1 {
		use_builtin_array_sort = true
	}
	if final_left_sym.kind == .array && (!(left_sym.has_method(method_name)
		|| left_sym.has_method_with_generic_parent(method_name))
		|| use_builtin_array_sort) {
		if g.gen_array_method_call(node, left_type, final_left_sym) {
			return
		}
	}
	if final_left_sym.kind == .array_fixed && !(left_sym.kind == .alias
		&& left_sym.has_method(method_name)) {
		if g.gen_fixed_array_method_call(node, left_type) {
			return
		}
	}
	if (final_left_sym.kind == .map || left_type.idx() == ast.map_type_idx)
		&& !(left_sym.kind == .alias && left_sym.has_method(method_name)) {
		if g.gen_map_method_call(node, left_type, final_left_sym) {
			return
		}
	}
	if left_sym.kind == .array_fixed && node.kind == .wait {
		g.gen_fixed_array_wait(node)
		return
	}
	if left_sym.kind in [.sum_type, .interface] {
		prefix_name := if left_sym.kind == .sum_type { 'sumtype' } else { 'interface' }
		match node.kind {
			.type_name {
				if left_sym.kind == .sum_type {
					g.conversion_function_call('builtin__tos3(v_typeof_sumtype_${typ_sym.cname}',
						')', node)
					return
				} else if left_sym.kind == .interface {
					g.conversion_function_call('v_typeof_interface_${typ_sym.cname}', '', node)
					return
				}
			}
			.type_idx {
				if left_sym.kind in [.sum_type, .interface] {
					g.conversion_function_call('v_typeof_${prefix_name}_idx_${typ_sym.cname}', '',
						node)
					return
				}
			}
			else {}
		}
	}

	mut is_free_method := false
	if node.kind == .str {
		if g.gen_to_str_method_call(node, unwrapped_rec_type) {
			return
		}
	} else if node.kind == .free {
		g.register_free_method(receiver_type)
		is_free_method = true
	}
	mut cast_n := 0
	old_inside_smartcast := g.inside_smartcast

	receiver_type_name = g.resolve_receiver_name(node, unwrapped_rec_type, final_left_sym,
		left_sym, typ_sym)
	mut name := ''
	if is_free_method {
		free_method_name := g.get_free_method(unwrapped_rec_type)
		name = free_method_name
	} else {
		name = util.no_dots('${receiver_type_name}_${method_name}')
	}
	if resolved_sym := g.table.find_sym(receiver_type_name) {
		if resolved_sym.is_builtin() && !receiver_type_name.starts_with('_') {
			name = 'builtin__${name}'
		}
	} else if receiver_type_name in ['int_literal', 'float_literal', 'vint_t'] {
		name = 'builtin__${name}'
	}
	if left_sym.kind == .chan && node.kind in [.close, .try_pop, .try_push] {
		name = 'sync__Channel_${method_name}'
	}
	mut is_range_slice := false
	if receiver_type.is_ptr() && !left_type.is_ptr() {
		if node.left is ast.IndexExpr {
			idx := node.left.index
			if idx is ast.RangeExpr {
				is_range_slice = true
			}
		}
	}
	mut receiver_concrete_types, mut parent_generic_method := g.receiver_generic_call_context(left_type,
		method_name)
	if receiver_concrete_types.len == 0 && receiver_type != 0
		&& g.unwrap_generic(receiver_type) != left_type {
		receiver_concrete_types, parent_generic_method = g.receiver_generic_call_context(g.unwrap_generic(receiver_type),
			method_name)
	}
	parent_method_generic_names_len := parent_generic_method.generic_names.len
	$if trace_method_generics ? {
		if method_name in ['next', 'next1', 'next2', 'next3', 'method', 'another_method',
			'call_generic_fn', 'collect', 'encode_value', 'check_struct_type_valid'] {
			eprintln('>>> method_call start: raw_node_name=${raw_method_name} | method_name=${method_name} | left_type: ${g.table.type_to_str(left_type)} | receiver_type: ${g.table.type_to_str(receiver_type)} | unwrapped_rec_type: ${g.table.type_to_str(unwrapped_rec_type)} | receiver_concrete_types: ${receiver_concrete_types} | parent_method_generic_names: ${parent_generic_method.generic_names} | node.raw_concrete_types: ${node.raw_concrete_types.map(g.table.type_to_str(it))} | node.concrete_types: ${node.concrete_types.map(g.table.type_to_str(it))}')
		}
	}
	mut concrete_types := if node.raw_concrete_types.len > 0 {
		node.raw_concrete_types.map(g.resolved_call_concrete_type(it))
	} else {
		[]ast.Type{}
	}
	mut concrete_types_from_resolved_args := false
	mut concrete_types_from_receiver := false
	mut trust_node_concrete_types := false
	mut method_generic_names_len := 0
	mut receiver_generic_names := []string{}
	mut receiver_generics_in_method := false
	mut has_method := false
	mut method_for_generics := ast.Fn{}
	if left_type != g.unwrap_generic(node.left_type)
		|| unwrapped_rec_type != g.unwrap_generic(receiver_type) {
		if concrete_types.any(it == 0 || it.has_flag(.generic)
			|| g.type_has_unresolved_generic_parts(it))
		{
			concrete_types = []
		}
	}
	if m := g.table.find_method(g.table.sym(left_type), method_name) {
		has_method = true
		method_for_generics = if parent_generic_method.generic_names.len > m.generic_names.len {
			parent_generic_method
		} else {
			m
		}
	} else if unwrapped_rec_type != left_type {
		// Method not found on left_type; try unwrapped_rec_type (e.g. from embed)
		if m2 := g.table.find_method(g.table.sym(unwrapped_rec_type), method_name) {
			has_method = true
			method_for_generics = if parent_generic_method.generic_names.len > m2.generic_names.len {
				parent_generic_method
			} else {
				m2
			}
		}
	}
	if !has_method && left_type != 0 {
		if m3 := g.table.find_method_with_embeds(g.table.sym(left_type), method_name) {
			has_method = true
			method_for_generics = if parent_generic_method.generic_names.len > m3.generic_names.len {
				parent_generic_method
			} else {
				m3
			}
		}
	}
	if !has_method && parent_method_generic_names_len > 0 {
		has_method = true
		method_for_generics = parent_generic_method
	}
	if has_method {
		if method_for_generics.params.len > 0 {
			receiver_type =
				receiver_type.derive(method_for_generics.params[0].typ).clear_flag(.generic)
			receiver_is_mut = method_for_generics.params[0].is_mut
			receiver_generic_names = g.table.generic_type_names(method_for_generics.params[0].typ)
		}
		method_generic_names_len = method_for_generics.generic_names.len
		receiver_generics_in_method = receiver_generic_names.len > 0
			&& method_generic_names_len >= receiver_generic_names.len
			&& method_for_generics.generic_names[..receiver_generic_names.len] == receiver_generic_names
		if node.concrete_types.len == method_generic_names_len && node.concrete_types.len > 0 {
			trust_node_concrete_types = node.raw_concrete_types.len == method_generic_names_len
				&& node.raw_concrete_types.len > 0
			// Inside a comptime $for method loop, the checker's concrete_types
			// are from the last iteration (the AST node is shared across
			// iterations). Don't trust them when an arg is a ComptimeCall.
			if trust_node_concrete_types && g.comptime.comptime_for_method != unsafe { nil } {
				if node.args.any(it.expr is ast.ComptimeCall) {
					trust_node_concrete_types = false
				}
			}
			if !trust_node_concrete_types {
				trust_node_concrete_types = true
				mut muttable := unsafe { &ast.Table(g.table) }
				for i, arg in node.args {
					param := if method_for_generics.is_variadic
						&& i >= method_for_generics.params.len - 1 {
						method_for_generics.params.last()
					} else {
						method_for_generics.params[i + 1]
					}
					param_infer_typ := g.generic_param_infer_type(param)
					if resolved_param_type := muttable.convert_generic_type(param_infer_typ,
						method_for_generics.generic_names, node.concrete_types)
					{
						mut arg_type := g.resolved_generic_call_arg_type(arg)
						if arg_type in [ast.int_literal_type, ast.float_literal_type] {
							continue
						}
						if arg.expr.is_auto_deref_var() && arg_type.is_ptr()
							&& !param_infer_typ.is_ptr() {
							arg_type = arg_type.deref()
						}
						param_sym := g.table.sym(param_infer_typ)
						if (arg.expr is ast.PrefixExpr && arg.expr.op == .amp
							&& param_infer_typ.nr_muls() > 0)
							|| (arg.expr.is_auto_deref_var()
							&& g.resolved_generic_call_arg_type(arg).is_ptr()
							&& !param_infer_typ.is_ptr())
							|| param_sym.info is ast.FnType {
							trust_node_concrete_types = false
							break
						}
						if g.unwrap_generic(resolved_param_type) != g.unwrap_generic(arg_type) {
							trust_node_concrete_types = false
							break
						}
					} else {
						trust_node_concrete_types = false
						break
					}
				}
			}
		}
		if method_generic_names_len > 0 && g.cur_fn != unsafe { nil }
			&& g.cur_concrete_types.len > 0 {
			mut resolved_arg_types := []ast.Type{}
			expected_arg_generic_slots :=
				int_max(method_generic_names_len - receiver_concrete_types.len, 0)
			if expected_arg_generic_slots > 0 {
				offset := if method_for_generics.is_method { 1 } else { 0 }
				for i, arg in node.args {
					param := if method_for_generics.is_variadic
						&& i >= method_for_generics.params.len - (offset + 1) {
						method_for_generics.params.last()
					} else {
						method_for_generics.params[offset + i]
					}
					param_infer_typ := g.generic_param_infer_type(param)
					if !param_infer_typ.has_flag(.generic)
						&& !g.type_has_unresolved_generic_parts(param_infer_typ) {
						continue
					}
					param_sym := g.table.sym(param_infer_typ)
					if param_sym.name !in method_for_generics.generic_names {
						continue
					}
					if param.typ == method_for_generics.params[0].typ {
						continue
					}
					if arg.expr is ast.Ident && arg.expr.obj is ast.Var
						&& (arg.expr.obj as ast.Var).ct_type_var == .generic_param {
						mut resolved_arg_type :=
							g.resolve_current_fn_generic_param_type(arg.expr.name)
						if (arg.is_mut || (arg.expr.obj as ast.Var).is_mut)
							&& resolved_arg_type.is_ptr() {
							resolved_arg_type = resolved_arg_type.deref()
						}
						if resolved_arg_type != 0 {
							resolved_arg_types << resolved_arg_type
						}
					}
				}
			}
			if !trust_node_concrete_types && resolved_arg_types.len == expected_arg_generic_slots {
				concrete_types = receiver_concrete_types.clone()
				concrete_types << resolved_arg_types
				concrete_types_from_resolved_args = true
			}
		}
	}
	if concrete_types.len == 0 && receiver_concrete_types.len > 0
		&& (!has_method || method_generic_names_len == 0 || receiver_generics_in_method) {
		concrete_types = receiver_concrete_types.clone()
		concrete_types_from_receiver = true
	}
	full_method_generic_names_len := if parent_method_generic_names_len > method_generic_names_len {
		parent_method_generic_names_len
	} else {
		method_generic_names_len
	}
	full_method := if parent_method_generic_names_len > method_generic_names_len {
		parent_generic_method
	} else {
		method_for_generics
	}
	full_method_generic_names := if parent_method_generic_names_len > method_generic_names_len {
		parent_generic_method.generic_names.clone()
	} else {
		method_for_generics.generic_names.clone()
	}
	if has_method {
		name = g.specialized_method_name_from_receiver_context(full_method,
			receiver_concrete_types, parent_generic_method, name)
	}
	if has_method && full_method_generic_names_len == 0 {
		concrete_types = []ast.Type{}
	}
	recovered_concrete_types := g.recover_method_call_concrete_types_from_name(raw_method_name,
		method_name, left_type, method_for_generics, parent_generic_method)
	if recovered_concrete_types.len > concrete_types.len || concrete_types.any(it.has_flag(.generic)
		|| g.type_has_unresolved_generic_parts(it)) {
		if recovered_concrete_types.len > 0 {
			concrete_types = recovered_concrete_types.clone()
		}
	}
	if concrete_types.len == 0 && has_method && full_method_generic_names_len > 0 {
		concrete_types = []ast.Type{len: full_method_generic_names_len, init: ast.void_type}
	}
	if concrete_types.len > 0 && !concrete_types_from_receiver && receiver_concrete_types.len > 0
		&& full_method_generic_names_len > concrete_types.len
		&& receiver_concrete_types.len + concrete_types.len == full_method_generic_names_len {
		method_concrete_types := concrete_types.clone()
		concrete_types = receiver_concrete_types.clone()
		concrete_types << method_concrete_types
	}
	if receiver_concrete_types.len > 0 {
		for i, receiver_ct in receiver_concrete_types {
			if i < concrete_types.len {
				concrete_types[i] = receiver_ct
			}
		}
	}
	if concrete_types.len > 0 {
		specialized_suffix := g.generic_fn_name(concrete_types, '')
		$if trace_method_generics ? {
			if method_name in ['next', 'next1', 'next2', 'next3', 'method', 'another_method',
				'call_generic_fn', 'collect', 'encode_value', 'check_struct_type_valid'] {
				eprintln('>>> method_call pre-name: ${method_name} | concrete_types: ${concrete_types.map(g.table.type_to_str(it))} | receiver_concrete_types: ${receiver_concrete_types.map(g.table.type_to_str(it))} | has_method: ${has_method} | method_generic_names_len: ${method_generic_names_len} | full_method_generic_names: ${full_method_generic_names} | name_before: ${name}')
			}
		}
		name_already_specialized := specialized_suffix != '' && name.ends_with(specialized_suffix)
		rec_len := receiver_concrete_types.len
		if has_method {
			expected_method_generic_names_len := if full_method_generic_names_len > method_generic_names_len {
				full_method_generic_names_len
			} else {
				method_generic_names_len
			}
			if expected_method_generic_names_len > concrete_types.len {
				concrete_types << []ast.Type{len: expected_method_generic_names_len - concrete_types.len, init: ast.void_type}
			}
			mut node_ := unsafe { node }
			comptime_args := g.type_resolver.resolve_args(g.cur_fn, method_for_generics, mut node_,
				concrete_types)
			receiver_param_type := if method_for_generics.params.len > 0 {
				method_for_generics.params[0].typ
			} else {
				ast.no_type
			}
			offset := if method_for_generics.is_method { 1 } else { 0 }
			mut method_generic_param_slots := []int{}
			mut raw_generic_idx := -1
			for i in 0 .. node.args.len {
				param := if method_for_generics.is_variadic
					&& i >= method_for_generics.params.len - (offset + 1) {
					method_for_generics.params.last()
				} else {
					method_for_generics.params[offset + i]
				}
				param_infer_typ := g.generic_param_infer_type(param)
				if !param_infer_typ.has_flag(.generic)
					&& !g.type_has_unresolved_generic_parts(param_infer_typ) {
					continue
				}
				raw_generic_idx++
				if param.typ == receiver_param_type {
					continue
				}
				method_generic_param_slots << raw_generic_idx
			}
			mut generic_arg_idx := 0
			for i, arg in node.args {
				param := if method_for_generics.is_variadic
					&& i >= method_for_generics.params.len - (offset + 1) {
					method_for_generics.params.last()
				} else {
					method_for_generics.params[offset + i]
				}
				param_infer_typ := g.generic_param_infer_type(param)
				if !param_infer_typ.has_flag(.generic)
					&& !g.type_has_unresolved_generic_parts(param_infer_typ) {
					continue
				}
				if param.typ == receiver_param_type {
					continue
				}
				slot := rec_len + generic_arg_idx
				if slot < concrete_types.len {
					mut arg_type := g.resolved_generic_call_arg_type(arg)
					if arg.expr.is_auto_deref_var() && arg_type.is_ptr()
						&& !param_infer_typ.is_ptr() {
						arg_type = arg_type.deref()
					}
					current_type := concrete_types[slot]
					if current_type != 0 && current_type != ast.void_type
						&& !current_type.has_flag(.generic)
						&& !g.type_has_unresolved_generic_parts(current_type)
						&& arg_type in [ast.int_literal_type, ast.float_literal_type] {
						generic_arg_idx++
						continue
					}
					mut explicit_slot := slot
					if receiver_concrete_types.len > 0 && node.raw_concrete_types.len > 0
						&& full_method_generic_names_len > node.raw_concrete_types.len
						&& receiver_concrete_types.len + node.raw_concrete_types.len == full_method_generic_names_len {
						explicit_slot = slot - rec_len
					}
					mut explicit_concrete_type := ast.no_type
					if explicit_slot >= 0 && explicit_slot < node.raw_concrete_types.len {
						explicit_concrete_type =
							g.resolved_call_concrete_type(node.raw_concrete_types[explicit_slot])
					} else if slot < node.concrete_types.len {
						explicit_concrete_type =
							g.resolved_call_concrete_type(node.concrete_types[slot])
					}
					if arg.expr is ast.PrefixExpr && arg.expr.op == .amp
						&& param_infer_typ.nr_muls() > 0 {
						inner_type := g.resolved_expr_type(arg.expr.right, arg.expr.right_type)
						mut resolved_inner_type :=
							g.unwrap_generic(g.recheck_concrete_type(inner_type))
						if arg.expr.right.is_auto_deref_var() && resolved_inner_type.is_ptr() {
							resolved_inner_type = resolved_inner_type.deref()
						}
						if resolved_inner_type != 0 && !resolved_inner_type.has_flag(.generic)
							&& !g.type_has_unresolved_generic_parts(resolved_inner_type) {
							concrete_types[slot] = resolved_inner_type
							generic_arg_idx++
							continue
						}
					}
					if explicit_concrete_type != 0 {
						node_concrete_type := explicit_concrete_type
						if g.type_has_unresolved_generic_parts(param_infer_typ)
							&& node_concrete_type == arg_type && !trust_node_concrete_types {
							// Composite generic params like `fn (T) U` need their inner concrete
							// type inferred from the wrapper type instead of reusing the wrapper.
						} else if node_concrete_type != 0 && !node_concrete_type.has_flag(.generic)
							&& !g.type_has_unresolved_generic_parts(node_concrete_type)
							&& (trust_node_concrete_types || node_concrete_type == arg_type) {
							concrete_types[slot] = node_concrete_type
							generic_arg_idx++
							continue
						}
					}
					if arg_type != 0 && !arg_type.has_flag(.generic)
						&& !g.type_has_unresolved_generic_parts(arg_type) {
						if g.type_has_unresolved_generic_parts(param_infer_typ) {
							prev_concrete_types := concrete_types.clone()
							g.infer_generic_call_concrete_types_from_types(full_method_generic_names, mut
								concrete_types, arg, param_infer_typ, arg_type)
							if concrete_types != prev_concrete_types {
								generic_arg_idx++
								continue
							}
							inferred_type := concrete_types[slot]
							if inferred_type != 0 && !inferred_type.has_flag(.generic)
								&& !g.type_has_unresolved_generic_parts(inferred_type) {
								generic_arg_idx++
								continue
							}
						}
						param_sym := g.table.sym(param_infer_typ)
						arg_sym := g.table.final_sym(g.unwrap_generic(arg_type))
						if param_sym.kind == .array && arg_sym.info is ast.Array {
							concrete_types[slot] = g.unwrap_generic(arg_sym.info.elem_type)
							generic_arg_idx++
							continue
						}
						if param_sym.kind == .array_fixed && arg_sym.info is ast.ArrayFixed {
							concrete_types[slot] = g.unwrap_generic(arg_sym.info.elem_type)
							generic_arg_idx++
							continue
						}
						if param_sym.kind == .map && arg_sym.info is ast.Map {
							concrete_types[slot] = g.unwrap_generic(arg_sym.info.key_type)
							if slot + 1 < concrete_types.len {
								concrete_types[slot + 1] = g.unwrap_generic(arg_sym.info.value_type)
							}
							generic_arg_idx += 2
							continue
						}
						concrete_types[slot] = ast.mktyp(arg_type)
					}
				}
				generic_arg_idx++
			}
			for raw_k, v in comptime_args {
				if raw_k !in method_generic_param_slots {
					continue
				}
				method_slot := method_generic_param_slots.index(raw_k)
				if method_slot == -1 {
					continue
				}
				slot := rec_len + method_slot
				if slot < concrete_types.len {
					current_type := concrete_types[slot]
					if !trust_node_concrete_types && !concrete_types_from_resolved_args
						&& (current_type == ast.void_type || current_type.has_flag(.generic)
						|| g.type_has_unresolved_generic_parts(current_type)) {
						concrete_types[slot] = g.unwrap_generic(v)
					}
				}
			}
			// When the receiver's generics are part of the method's generic params,
			// arg-resolved concrete types may be stale (scope var types from a
			// different checker pass). Validate node.concrete_types against
			// receiver_concrete_types: if receiver positions match, the checker
			// processed this instantiation, so trust the method's own positions.
			if receiver_generics_in_method && !trust_node_concrete_types
				&& !concrete_types_from_resolved_args
				&& node.concrete_types.len == full_method_generic_names_len && rec_len > 0 {
				mut receiver_validated := true
				for i in 0 .. rec_len {
					if i >= node.concrete_types.len {
						receiver_validated = false
						break
					}
					node_ct := ast.mktyp(g.unwrap_generic(node.concrete_types[i]))
					if node_ct != receiver_concrete_types[i] {
						receiver_validated = false
						break
					}
				}
				if receiver_validated {
					for i in rec_len .. concrete_types.len {
						if i < node.concrete_types.len {
							node_ct := g.resolved_call_concrete_type(node.concrete_types[i])
							if node_ct != 0 && !node_ct.has_flag(.generic)
								&& !g.type_has_unresolved_generic_parts(node_ct) {
								concrete_types[i] = node_ct
							}
						}
					}
				}
			}
			concrete_types = concrete_types.filter(it != ast.void_type)
			mut name_fkey := g.resolve_method_decl_fkey_for_type(left_type, method_name)
			if name_fkey == '' {
				name_fkey = g.method_decl_fkey(full_method)
			}
			if name_fkey != '' && full_method_generic_names_len > 0
				&& concrete_types.len == full_method_generic_names_len
				&& concrete_types.all(!it.has_flag(.generic)
				&& !g.type_has_unresolved_generic_parts(it)) {
				g.table.register_fn_concrete_types(name_fkey, concrete_types)
			}
			raw_method_suffix := g.raw_method_name_suffix(raw_method_name, method_name,
				full_method_generic_names)
			mut name_concrete_types := []ast.Type{}
			mut method_name_already_specialized := false
			mut can_use_raw_method_suffix := raw_method_suffix != ''
			if can_use_raw_method_suffix {
				expected_name_concrete_types := if recovered_concrete_types.len > 0 {
					recovered_concrete_types.clone()
				} else {
					g.method_name_concrete_types(name_fkey, concrete_types, receiver_concrete_types)
				}
				if expected_name_concrete_types.len > 0
					&& g.generic_fn_name(expected_name_concrete_types, method_name) != raw_method_name {
					can_use_raw_method_suffix = false
				}
			}
			if can_use_raw_method_suffix {
				method_name_already_specialized = name.ends_with(raw_method_suffix)
				if !name_already_specialized {
					name += raw_method_suffix
				}
			} else {
				raw_name_concrete_types := g.raw_method_name_concrete_types(raw_method_name,
					method_name, node.raw_concrete_types, receiver_concrete_types)
				mut placeholder_raw_name_concrete_types := []ast.Type{}
				if raw_name_concrete_types.len == 0 && node.raw_concrete_types.len > 0
					&& full_method_generic_names.any(raw_method_name.contains('_T_${it}')) {
					mut resolved_raw_concrete_types := []ast.Type{}
					mut all_raw_concrete_types_resolved := true
					for concrete_type in node.raw_concrete_types {
						resolved_concrete_type := g.resolved_call_concrete_type(concrete_type)
						if resolved_concrete_type == 0 || resolved_concrete_type.has_flag(.generic)
							|| g.type_has_unresolved_generic_parts(resolved_concrete_type) {
							all_raw_concrete_types_resolved = false
							break
						}
						resolved_raw_concrete_types << resolved_concrete_type
					}
					if all_raw_concrete_types_resolved {
						placeholder_raw_name_concrete_types = resolved_raw_concrete_types.clone()
					}
				}
				name_concrete_types = if raw_name_concrete_types.len > 0 {
					raw_name_concrete_types.clone()
				} else if placeholder_raw_name_concrete_types.len > 0 {
					placeholder_raw_name_concrete_types
				} else if recovered_concrete_types.len > 0 && (concrete_types.len == 0
					|| concrete_types.any(it == ast.void_type
					|| it.has_flag(.generic)
					|| g.type_has_unresolved_generic_parts(it))) {
					recovered_concrete_types.clone()
				} else {
					g.method_name_concrete_types(name_fkey, concrete_types, receiver_concrete_types)
				}
				method_suffix := g.generic_fn_name(name_concrete_types, '')
				method_name_already_specialized = method_suffix != ''
					&& name.ends_with(method_suffix)
				if !name_already_specialized && !method_name_already_specialized
					&& name_concrete_types.len > 0 {
					name = g.generic_fn_name(name_concrete_types, name)
				}
			}
			$if trace_method_generics ? {
				if method_name in ['next', 'next1', 'next2', 'next3', 'method', 'another_method',
					'call_generic_fn', 'collect', 'encode_value', 'check_struct_type_valid'] {
					eprintln('>>> method_call named: ${method_name} | name_concrete_types: ${name_concrete_types.map(g.table.type_to_str(it))} | final_name: ${name}')
				}
			}
		} else if !(node.kind == .str && receiver_concrete_types.len > 0)
			&& !name_already_specialized {
			name = g.generic_fn_name(concrete_types, name)
			$if trace_method_generics ? {
				if method_name in ['next', 'next1', 'next2', 'next3', 'method', 'another_method',
					'call_generic_fn', 'collect', 'encode_value', 'check_struct_type_valid'] {
					eprintln('>>> method_call named no-method: ${method_name} | concrete_types: ${concrete_types.map(g.table.type_to_str(it))} | final_name: ${name}')
				}
			}
		}
	}
	mut free_receiver_heap_copy := ''
	if is_free_method && has_method && node.left is ast.Ident && !left_type.is_ptr()
		&& free_method_calls_free_on_receiver(full_method) {
		left_ident := node.left as ast.Ident
		if !g.resolved_ident_is_auto_heap(left_ident) {
			stmt_line := g.go_before_last_stmt()
			alloc_size := '(sizeof(${rec_cc_type}) == 0) ? 1 : sizeof(${rec_cc_type})'
			g.empty_line = true
			free_receiver_heap_copy = g.new_tmp_var()
			g.writeln('${rec_cc_type}* ${free_receiver_heap_copy} = (${rec_cc_type}*)builtin__vcalloc(${alloc_size});')
			g.writeln('*${free_receiver_heap_copy} = ${c_name(left_ident.name)};')
			g.write(stmt_line)
		}
	}
	receiver_needs_ref := receiver_type.is_ptr() || receiver_is_mut
	// g.generate_tmp_autofree_arg_vars(node, name)
	if !receiver_needs_ref && left_type.is_ptr() && node.kind == .str {
		if left_type.is_int_valptr() {
			g.write('builtin__ptr_str(')
		} else {
			g.gen_expr_to_string(node.left, left_type)
			return
		}
	} else if receiver_needs_ref && left_type.is_ptr() && node.kind == .str
		&& !left_sym.has_method('str') {
		g.gen_expr_to_string(node.left, left_type)
		return
	} else {
		if g.cur_fn != unsafe { nil } && g.cur_fn.trace_fns.len > 0 {
			g.gen_trace_call(node, name)
			g.write('(')
		} else {
			g.write('${name}(')
		}
	}
	is_interface := left_sym.kind == .interface && g.table.sym(receiver_type).kind == .interface
	mut receiver_expr_is_addressable := node.left.is_lvalue()
	match node.left {
		ast.IndexExpr {
			if node.left.left_type != 0 {
				indexed_container_sym := g.table.final_sym(g.unwrap_generic(node.left.left_type))
				if indexed_container_sym.kind == .string && !node.left.left_type.is_ptr() {
					receiver_expr_is_addressable = false
				}
			}
		}
		else {}
	}

	if free_receiver_heap_copy == '' && receiver_needs_ref && (!left_type.is_ptr()
		|| node.from_embed_types.len != 0 || (left_type.has_flag(.shared_f) && node.kind != .str)) {
		// The receiver is a reference, but the caller provided a value
		// Add `&` automatically.
		// TODO: same logic in call_args()
		if !is_range_slice {
			if !receiver_expr_is_addressable {
				if node.left.is_as_cast() {
					g.inside_smartcast = true
					if node.left is ast.SelectorExpr && !left_type.is_ptr() {
						g.write('&')
					}
				} else {
					g.write('ADDR(${rec_cc_type}, ')
					cast_n++
				}
			} else if node.left is ast.Ident && g.table.is_interface_smartcast(node.left.obj) {
				g.write('ADDR(${rec_cc_type}, ')
				cast_n++
			} else if !(left_type.has_flag(.shared_f) && g.styp(left_type) == g.styp(receiver_type)) {
				g.write('&')
			}
		} else {
			if !left_type.is_ptr() {
				g.write('ADDR(${rec_cc_type}, ')
				cast_n++
			}
		}
	} else if free_receiver_heap_copy == '' && !receiver_needs_ref && left_type.is_ptr()
		&& node.kind != .str && node.from_embed_types.len == 0 {
		if !left_type.has_flag(.shared_f) {
			g.write('*'.repeat(left_type.nr_muls()))
		}
	} else if free_receiver_heap_copy == '' && !is_range_slice && node.from_embed_types.len == 0
		&& node.kind != .str {
		diff := left_type.nr_muls() - receiver_type.nr_muls()
		if diff > 0 {
			g.write('*'.repeat(diff))
		}
	}

	if g.is_autofree && node.free_receiver && !g.inside_lambda && !g.is_builtin_mod {
		// The receiver expression needs to be freed, use the temp var.
		fn_name := node.name.replace('.', '_')
		arg_name := '_arg_expr_${fn_name}_0_${node.pos.pos}'
		g.write('/*af receiver arg*/' + arg_name)
	} else {
		if free_receiver_heap_copy != '' {
			g.write(free_receiver_heap_copy)
		} else if node.left is ast.MapInit {
			g.write('(map[]){')
			g.expr(ast.Expr(node.left))
			g.write('}[0]')
		} else if !is_interface && node.from_embed_types.len > 0 {
			// For mut generic params where the concrete type resolves
			// to a multi-pointer (e.g. mut ctx X where X = &Context →
			// C param is Context**), fn_decl_params adds .ref() which
			// adds an extra pointer level not reflected in left_type.
			mut extra_muls := 0
			if node.left is ast.Ident && node.left.obj is ast.Var {
				obj := node.left.obj
				if obj.is_auto_deref && obj.is_arg && obj.generic_typ != 0 {
					resolved := g.unwrap_generic(obj.generic_typ)
					if resolved.nr_muls() > 1 && g.table.sym(resolved).kind == .struct {
						extra_muls = 1
					}
				}
			}
			n_ptr := node.left_type.nr_muls() - 1 + extra_muls
			if n_ptr > 0 {
				g.write2('(', '*'.repeat(n_ptr))
				g.expr(ast.Expr(node.left))
				g.write(')')
			} else {
				g.expr(ast.Expr(node.left))
			}
		} else if is_interface && node.from_embed_types.len > 0 {
			if g.out.last_n(1) == '&' {
				g.go_back(1)
			}
			if receiver_type.is_ptr() && left_type.is_ptr() {
				// (main__IFoo*)bar
				g.write2('(', g.table.sym(node.from_embed_types.last()).cname)
				g.write('*)')
				g.expr(ast.Expr(node.left))
			} else if receiver_type.is_ptr() && !left_type.is_ptr() {
				// (main__IFoo*)&bar
				g.write2('(', g.table.sym(node.from_embed_types.last()).cname)
				g.write('*)&')
				g.expr(ast.Expr(node.left))
			} else if !receiver_type.is_ptr() && left_type.is_ptr() {
				// *((main__IFoo*)bar)
				g.write2('*((', g.table.sym(node.from_embed_types.last()).cname)
				g.write('*)')
				g.expr(ast.Expr(node.left))
				g.write(')')
			} else {
				// *((main__IFoo*)&bar)
				g.write2('*((', g.table.sym(node.from_embed_types.last()).cname)
				g.write('*)&')
				g.expr(node.left)
				g.write(')')
			}
		} else {
			if is_free_method && !receiver_type.is_ptr() {
				g.write('&')
			}
			g.expr(node.left)
		}
		if !is_interface || node.from_embed_types.len == 0 {
			mut node_embed_types := node.from_embed_types.clone()
			if node.left is ast.Ident && g.comptime.get_ct_type_var(node.left) == .generic_var
				&& !final_left_sym.has_method(method_name)
				&& !final_left_sym.has_method_with_generic_parent(method_name) {
				_, embed_types := g.table.find_method_from_embeds(final_left_sym, method_name) or {
					ast.Fn{}, []ast.Type{}
				}
				if embed_types.len > 0 {
					node_embed_types = embed_types.clone()
				}
			}
			for i, embed in node_embed_types {
				embed_sym := g.table.sym(embed)
				embed_name := embed_sym.embed_name()
				is_left_ptr := if i == 0 {
					left_type.is_ptr()
				} else {
					node_embed_types[i - 1].is_ptr()
				}
				if is_left_ptr {
					g.write('->')
				} else {
					g.write('.')
				}
				g.write(embed_name)
			}
		}
		if left_type.has_flag(.shared_f) && g.styp(left_type) != g.styp(receiver_type) {
			g.write('->val')
		}
	}
	if cast_n > 0 {
		g.write(')'.repeat(cast_n))
	}
	is_variadic := node.expected_arg_types.len > 0
		&& node.expected_arg_types.last().has_flag(.variadic)
	if node.args.len > 0 || is_variadic {
		g.write(', ')
	}
	g.inside_smartcast = old_inside_smartcast
	g.call_args_with_context(node, full_method_generic_names, concrete_types)
	g.write(')')
	if node.return_type != 0 && !node.return_type.has_option_or_result()
		&& g.table.final_sym(node.return_type).kind == .array_fixed {
		// it's non-option fixed array, requires accessing .ret_arr member to get the array
		g.write('.ret_arr')
	}
}

fn (mut g Gen) fn_call(node ast.CallExpr) {
	// call struct field with fn type
	// TODO: test node.left instead
	// left & left_type will be `x` and `x type` in `x.fieldfn()`
	// will be `0` for `foo()`
	mut is_interface_call := false
	mut is_selector_call := false
	if node.is_method && node.left_type != 0 {
		mut fn_typ := ast.no_type
		left_sym := g.table.sym(node.left_type)
		if node.is_field {
			if field := g.table.find_field_with_embeds(left_sym, node.name) {
				fn_typ = g.unwrap_generic(field.typ)
			}
			if node.is_unwrapped_fn_selector {
				fn_typ = fn_typ.clear_option_and_result()
			}
		}
		if left_sym.kind == .interface || fn_typ.is_ptr() {
			is_interface_call = true
			g.write('(*')
		}
		if node.is_unwrapped_fn_selector {
			callback_sym := g.table.final_sym(fn_typ)
			if callback_sym.info is ast.FnType {
				g.write('(*(${g.styp(fn_typ)}*)')
			}
		}
		g.expr(node.left)
		if node.left_type.is_ptr() {
			g.write('->')
		} else {
			g.write('.')
		}
		for embed in node.from_embed_types {
			embed_sym := g.table.sym(embed)
			embed_name := embed_sym.embed_name()
			g.write(embed_name)
			if embed.is_ptr() {
				g.write('->')
			} else {
				g.write('.')
			}
		}
		is_selector_call = true
	}
	mut node_name := node.name // name to find on fn table
	mut name := node.name
	if node.is_static_method {
		// resolve static call T.name()
		if g.cur_fn != unsafe { nil } {
			_, name = g.table.convert_generic_static_type_name(node.name, g.cur_fn.generic_names,
				g.cur_concrete_types)
			if node.concrete_types.len > 0 {
				// Resolves T.from() to real symbol name to search on fn table
				node_name = name
			}
		}
	}
	mut is_builtin_print := !is_selector_call
		&& node.kind in [.print, .println, .eprint, .eprintln, .panic]
	print_method := name
	is_json_encode := node.kind == .json_encode
	is_json_encode_pretty := node.kind == .json_encode_pretty
	is_json_decode := node.kind == .json_decode
	is_json_fn := is_json_encode || is_json_encode_pretty || is_json_decode
	is_va_arg := node.kind == .va_arg
	mut json_type_str := ''
	mut json_obj := ''
	if is_json_fn {
		g.is_json_fn = true
		json_obj = g.new_tmp_var()
		mut tmp2 := ''
		cur_line := g.go_before_last_stmt()
		if is_json_encode || is_json_encode_pretty {
			mut unwrapped_typ := g.unwrap_generic(node.args[0].typ)
			resolved_json_arg_type := g.resolved_expr_type(node.args[0].expr, node.args[0].typ)
			if resolved_json_arg_type != 0 {
				unwrapped_typ = g.unwrap_generic(g.recheck_concrete_type(resolved_json_arg_type))
			}
			g.gen_json_for_type_with_pos(unwrapped_typ, node.args[0].expr.pos())
			json_type_str = g.styp(unwrapped_typ)
			// `json__encode` => `json__encode_User`
			encode_name := js_enc_name(json_type_str)
			g.empty_line = true
			g.writeln('// json.encode')
			g.write('cJSON* ${json_obj} = ${encode_name}(')
			g.call_args(node)
			g.writeln(');')
			tmp2 = if g.is_autofree {
				'_arg_expr_${node.name.replace('.', '_')}_${node.pos.pos}'
			} else {
				g.new_tmp_var()
			}
			if is_json_encode {
				g.writeln('string ${tmp2} = json__json_print(${json_obj});')
			} else {
				g.writeln('string ${tmp2} = json__json_print_pretty(${json_obj});')
			}
		} else {
			ast_type := node.args[0].expr as ast.TypeNode
			// `json.decode(User, s)` => json.decode_User(s)
			typ := c_name(g.styp(ast_type.typ))
			fn_name := c_fn_name(name) + '_' + typ
			g.gen_json_for_type_with_pos(ast_type.typ, node.args[0].expr.pos())
			g.empty_line = true
			g.writeln('// json.decode')
			g.write('cJSON* ${json_obj} = json__json_parse(')
			// Skip the first argument in json.decode which is a type
			// its name was already used to generate the function call
			g.is_js_call = true
			g.call_args(node)
			g.writeln(');')
			tmp2 = g.new_tmp_var()
			g.writeln('${result_name}_${typ} ${tmp2} = ${fn_name}(${json_obj});')
		}
		if !g.is_autofree {
			g.write('cJSON_Delete(${json_obj}); // del')
		}
		g.write('\n${cur_line}')
		name = ''
		json_obj = tmp2
	} else if is_va_arg {
		ast_type := node.args[0].expr as ast.TypeNode
		typ := g.styp(ast_type.typ)
		g.write('va_arg(')
		g.expr(node.args[1].expr)
		g.write(', ${typ})')
		return
	}
	if node.kind == .addr {
		name = '&'
	}
	if node.language == .c {
		// Skip "C."
		name = g.c_call_name(node, util.no_dots(name[2..]))
	} else {
		name = if is_selector_call { c_name(name) } else { c_fn_name(name) }
	}
	mut call_generic_names := []string{}
	mut call_concrete_types := []ast.Type{}
	if g.pref.translated || g.file.is_translated || node.is_file_translated {
		// For `@[c: 'P_TryMove'] fn p_trymove( ... `
		// every time `p_trymove` is called, `P_TryMove` must be generated instead.
		if f := g.table.find_fn(node.name) {
			// TODO: PERF fn lookup for each fn call in translated mode
			if cattr := f.attrs.find_first('c') {
				name = cattr.arg
			}
		}
	}
	if !is_selector_call {
		if func := g.table.find_fn(node_name) {
			if is_builtin_print {
				is_builtin_print = func.mod == 'builtin'
			}
			mut concrete_types := g.generic_fn_call_concrete_types(func, node)
			mut node_ := unsafe { node }
			comptime_args := g.type_resolver.resolve_args(g.cur_fn, func, mut node_, concrete_types)
			if concrete_types.len > 0 {
				specialized_suffix := g.generic_fn_name(concrete_types, '')
				name_already_specialized := specialized_suffix != ''
					&& name.ends_with(specialized_suffix)
				for k, v in comptime_args {
					if k < concrete_types.len {
						current_type := concrete_types[k]
						if current_type == ast.void_type || current_type.has_flag(.generic)
							|| g.type_has_unresolved_generic_parts(current_type) {
							concrete_types[k] = g.unwrap_generic(v)
						}
					}
				}
				node_.concrete_types = concrete_types
				if func.generic_names.len == concrete_types.len
					&& concrete_types.all(!it.has_flag(.generic)
					&& !g.type_has_unresolved_generic_parts(it)) {
					g.table.register_fn_concrete_types(func.fkey(), concrete_types)
				}
				if !name_already_specialized {
					name = g.generic_fn_name(concrete_types, name)
					name = name.replace_each(c_fn_name_escape_seq)
				}
			}
			if func.generic_names.len > 0 && func.generic_names.len == concrete_types.len {
				call_generic_names = func.generic_names.clone()
				call_concrete_types = concrete_types.clone()
			}
			if func.mod == 'builtin' && !name.starts_with('builtin__') && node.language != .c {
				name = 'builtin__${name}'
			}
		} else if is_builtin_print && g.pref.no_builtin {
			is_builtin_print = false
		}
	}
	if node.is_fn_a_const {
		name = g.c_const_name(node.const_name)
	}
	// TODO2
	// cgen shouldn't modify ast nodes, this should be moved
	// g.generate_tmp_autofree_arg_vars(node, name)
	// Handle `print(x)`
	mut print_auto_str := false
	mut print_arg_typ := if is_builtin_print && node.args.len > 0 {
		node.args[0].typ
	} else {
		ast.void_type
	}
	// In generic contexts, AST-stored types may be stale from a previous
	// instantiation. Use resolved_expr_type for Ident expressions.
	if is_builtin_print && node.args.len > 0 && g.cur_fn != unsafe { nil }
		&& g.cur_concrete_types.len > 0 && node.args[0].expr is ast.Ident {
		resolved := g.resolved_expr_type(node.args[0].expr, node.args[0].typ)
		if resolved != 0 {
			print_arg_typ = resolved
		}
	}
	if is_builtin_print && node.args.len > 0 && (print_arg_typ != ast.string_type
		|| g.comptime.comptime_for_method != unsafe { nil } || node.args[0].ct_expr) {
		g.inside_interface_deref = true
		defer(fn) {
			g.inside_interface_deref = false
		}
		mut typ := print_arg_typ
		if typ == 0 {
			g.checker_bug('print arg.typ is 0', node.pos)
		}
		if typ != ast.string_type || g.comptime.comptime_for_method != unsafe { nil } {
			expr := node.args[0].expr
			if expr is ast.ComptimeSelector {
				if expr.typ_key != '' {
					typ = g.type_resolver.get_ct_type_or_default(expr.typ_key, typ)
				}
			} else if expr is ast.ComptimeCall {
				if expr.kind == .method {
					sym := g.table.sym(g.unwrap_generic(expr.left_type))
					if m := sym.find_method(g.comptime.comptime_for_method.name) {
						typ = m.return_type
					}
				}
			} else if expr is ast.Ident && expr.obj is ast.Var {
				typ = expr.obj.typ
				if expr.name in g.type_resolver.type_map {
					resolved := g.type_resolver.get_ct_type_or_default(expr.name, typ)
					if resolved != 0 && resolved != ast.void_type {
						typ = resolved
					}
				}
				mut ct_resolved := false
				if expr.ct_expr || expr.obj.ct_type_var != .no_comptime {
					resolved := g.type_resolver.get_type_or_default(ast.Expr(expr), typ)
					if resolved != 0 && resolved != ast.void_type {
						typ = resolved
						ct_resolved = true
					}
				}
				// In generic contexts, scope var types may be stale from a
				// previous checker instantiation. When ct_type_var is
				// generic_var or generic_param, the comptime resolver above
				// may return a stale type (e.g. from shared AST mutation),
				// so we also try the generic scope resolution.
				if (!ct_resolved || expr.obj.ct_type_var in [.generic_var, .generic_param])
					&& g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
					// For generic_param, prefer resolving from the function's
					// parameter declaration to avoid stale AST types.
					if expr.obj.ct_type_var == .generic_param {
						param_resolved := g.resolve_current_fn_generic_param_type(expr.name)
						if param_resolved != 0 {
							typ = param_resolved
						}
					} else {
						resolved := g.resolved_expr_type(expr, expr.obj.typ)
						if resolved != 0 {
							typ = resolved
						}
					}
				}
				if expr.obj.smartcasts.len > 0 {
					typ = g.unwrap_generic(expr.obj.smartcasts.last())
					cast_sym := g.table.sym(typ)
					if cast_sym.info is ast.Aggregate {
						typ = cast_sym.info.types[g.aggregate_type_idx]
					} else if expr.obj.ct_type_var == .smartcast {
						typ = g.unwrap_generic(g.type_resolver.get_type(expr))
					}
				}
				// handling println(var or { ... })
				if typ.has_flag(.option) && expr.or_expr.kind != .absent {
					typ = typ.clear_flag(.option)
				}
				if typ.has_flag(.generic) || g.type_has_unresolved_generic_parts(typ) {
					resolved_typ := g.resolved_expr_type(expr, typ)
					if resolved_typ != 0 && resolved_typ != ast.void_type {
						typ = resolved_typ
					}
				}
			}
			typ_sym := g.table.sym(typ)
			needs_tmp_string := !typ.has_option_or_result()
				&& (g.is_autofree || g.pref.gc_mode == .boehm_leak)
			if typ_sym.kind == .interface && (typ_sym.info as ast.Interface).defines_method('str') {
				rec_type_name := util.no_dots(g.cc_type(typ, false))
				dot := if typ.is_ptr() { '->' } else { '.' }
				if needs_tmp_string {
					tmp := g.new_tmp_var()
					g.write('string ${tmp} = ${c_name(rec_type_name)}_name_table[')
					g.expr(expr)
					g.write('${dot}_typ]._method_str(')
					g.expr(expr)
					g.write('${dot}_object')
					g.writeln('); builtin__${c_fn_name(print_method)}(${tmp}); builtin__string_free(&${tmp});')
				} else {
					g.write('builtin__${c_fn_name(print_method)}(')
					g.write('${c_name(rec_type_name)}_name_table[')
					g.expr(expr)
					g.write('${dot}_typ]._method_str(')
					g.expr(expr)
					g.write('${dot}_object')
					g.writeln('));')
				}
				return
			}
			if needs_tmp_string {
				// Create a temporary variable so that the value can be freed
				tmp := g.new_tmp_var()
				g.write('string ${tmp} = ')
				g.gen_expr_to_string(expr, typ)
				g.writeln('; builtin__${c_fn_name(print_method)}(${tmp}); builtin__string_free(&${tmp});')
			} else {
				g.write('builtin__${c_fn_name(print_method)}(')
				g.gen_expr_to_string(expr, typ)
				g.write(')')
			}
			print_auto_str = true
		}
	}
	if !print_auto_str {
		if is_builtin_print && g.pref.gc_mode == .boehm_leak && node.args[0].typ == ast.string_type
			&& node.args[0].expr !in [ast.Ident, ast.StringLiteral, ast.SelectorExpr, ast.ComptimeSelector] {
			tmp := g.new_tmp_var()
			tmp_init := g.autofree_tmp_arg_init_stmt('string ${tmp} = ', node.args[0].expr)
			g.writeln(tmp_init)
			g.writeln('builtin__${c_fn_name(print_method)}(${tmp});')
			g.writeln('builtin__string_free(&${tmp});')
			return
		}
		if is_builtin_print && node.args[0].expr !is ast.CallExpr {
			// only need for `println(err)`
			// not need for `println(err.msg())`
			g.inside_interface_deref = true
			defer(fn) {
				g.inside_interface_deref = false
			}
		}
		if g.pref.is_debug && is_builtin_print && node.kind == .panic {
			paline, pafile, pamod, pafn := g.panic_debug_info(node.pos)
			g.write('builtin__panic_debug(${paline}, builtin__tos3("${pafile}"), builtin__tos3("${pamod}"), builtin__tos3("${pafn}"),  ')
			g.call_args(node)
			g.write(')')
		} else if node.name.ends_with('__static__from_string') && !g.table.known_fn(node.name) {
			mod_enum_name, idx := g.get_enum_type_idx_from_fn_name(node.name)
			fn_mod := mod_enum_name.all_before_last('.')
			full_fn_name := '${fn_mod}.${node.name}'
			fn_name := util.no_dots(full_fn_name)
			lock g.str_fn_names {
				if fn_name !in g.str_fn_names {
					g.gen_enum_static_from_string(fn_name, mod_enum_name, idx)
					g.str_fn_names << fn_name
				}
			}
			g.write('${fn_name}(')
			g.call_args(node)
			g.write(')')
		} else {
			// Simple function call
			// if free_tmp_arg_vars {
			// g.writeln(';')
			// g.write(cur_line + ' /* <== af cur line*/')
			// }
			mut is_fn_var := false
			if !is_selector_call {
				if obj := node.scope.find_var(node.name) {
					// Temp fix generate call fn error when the struct type of sumtype
					// has the fn field and is same to the struct name.
					mut is_cast_needed := true
					mut smartcast_types := obj.smartcasts.clone()
					if obj.orig_type.has_flag(.option) {
						mut unwrapped_fn_var_type := ast.no_type
						mut resolved_parent_value_type := ast.no_type
						if node.scope.parent != unsafe { nil } {
							if parent_var := node.scope.parent.find_var(node.name) {
								resolved_parent_value_type =
									g.unwrap_generic(g.recheck_concrete_type(parent_var.typ)).clear_option_and_result()
								if resolved_parent_value_type != 0
									&& g.table.final_sym(resolved_parent_value_type).kind == .function {
									unwrapped_fn_var_type = resolved_parent_value_type
								}
							}
						}
						mut resolved_expr_value_type := ast.no_type
						if unwrapped_fn_var_type == 0 && obj.expr !is ast.EmptyExpr {
							resolved_expr_value_type = g.unwrap_generic(g.recheck_concrete_type(g.resolved_expr_type(obj.expr,
								obj.typ))).clear_option_and_result()
							if resolved_expr_value_type != 0
								&& g.table.final_sym(resolved_expr_value_type).kind == .function {
								unwrapped_fn_var_type = resolved_expr_value_type
							}
						}
						resolved_scope_type := g.resolved_scope_var_type(ast.Ident{
							name:  node.name
							scope: node.scope
						})
						if unwrapped_fn_var_type == 0 && resolved_scope_type != 0 {
							resolved_scope_value_type :=
								g.unwrap_generic(g.recheck_concrete_type(resolved_scope_type)).clear_option_and_result()
							if resolved_scope_value_type != 0
								&& g.table.final_sym(resolved_scope_value_type).kind == .function {
								unwrapped_fn_var_type = resolved_scope_value_type
							}
						}
						if unwrapped_fn_var_type == 0 {
							unwrapped_fn_var_type =
								g.unwrap_generic(g.recheck_concrete_type(obj.typ.clear_option_and_result()))
						}
						if unwrapped_fn_var_type != 0
							&& g.table.final_sym(unwrapped_fn_var_type).kind == .function {
							smartcast_types = [unwrapped_fn_var_type]
						}
					}
					if node.is_method && node.left_type != 0 {
						left_sym := g.table.sym(node.left_type)
						if left_sym.kind == .struct && node.name == obj.name {
							is_cast_needed = false
						}
					}
					if smartcast_types.len > 0 && is_cast_needed {
						for typ in smartcast_types {
							sym := g.table.sym(g.unwrap_generic(typ))
							if obj.orig_type.has_flag(.option) && sym.kind == .function {
								g.write('(*(${sym.cname}*)(')
							} else {
								g.write('(*(${sym.cname})(')
							}
						}
						for i, typ in smartcast_types {
							cast_sym := g.table.sym(g.unwrap_generic(typ))
							mut is_ptr := false
							if i == 0 {
								if obj.is_inherited {
									g.write(closure_ctx + '->' + c_name(node.name))
								} else {
									g.write(node.name)
								}
								if obj.orig_type.is_ptr() {
									is_ptr = true
								}
							}
							dot := if is_ptr { '->' } else { '.' }
							if cast_sym.info is ast.Aggregate {
								sym := g.table.sym(cast_sym.info.types[g.aggregate_type_idx])
								g.write('${dot}_${sym.cname}')
							} else if cast_sym.kind == .function && obj.orig_type.has_flag(.option) {
								g.write('.data')
							} else {
								g.write('${dot}_${cast_sym.cname}')
							}
							g.write('))')
						}
						is_fn_var = true
					} else if obj.is_inherited {
						g.write(closure_ctx + '->' + c_name(node.name))
						is_fn_var = true
					}
				}
			}
			if !is_fn_var {
				if g.cur_fn != unsafe { nil } && g.cur_fn.trace_fns.len > 0 {
					if node.is_fn_var {
						g.gen_trace_fn_var_call(node, name, call_generic_names, call_concrete_types)
						return
					}
					g.gen_trace_call(node, name)
				} else {
					g.write(g.get_ternary_name(name))
				}
			}
			if node.is_unwrapped_fn_selector {
				g.write('.data)')
			}
			if is_interface_call {
				g.write(')')
			}
			mut tmp_cnt_save := -1
			if name != '&' {
				g.write('(')
			}
			if is_json_fn {
				g.write(json_obj)
			} else {
				if node.is_keep_alive
					&& g.pref.gc_mode in [.boehm_full, .boehm_incr, .boehm_full_opt, .boehm_incr_opt] {
					cur_line := g.go_before_last_stmt()
					tmp_cnt_save = g.keep_alive_call_pregen(node)
					g.write(cur_line)
					for i in 0 .. node.args.len {
						if i > 0 {
							g.write(', ')
						}
						g.write('__tmp_arg_${tmp_cnt_save + i}')
					}
				} else {
					g.call_args_with_context(node, call_generic_names, call_concrete_types)
				}
			}
			if name != '&' {
				g.write(')')
			}
			if node.return_type != 0 && !node.return_type.has_option_or_result()
				&& g.table.final_sym(node.return_type).kind == .array_fixed {
				// it's non-option fixed array, requires accessing .ret_arr member to get the array
				g.write('.ret_arr')
			}
			if tmp_cnt_save >= 0 {
				g.writeln(';')
				g.keep_alive_call_postgen(node, tmp_cnt_save)
			}
		}
	}
	g.is_json_fn = false
}

// gen_trace_call generates call to the wrapper trace fn if the call is traceable
fn (mut g Gen) gen_trace_call(node ast.CallExpr, name string) {
	hash_fn, _ := g.table.get_trace_fn_name(g.cur_fn, node)
	if _ := g.cur_fn.trace_fns[hash_fn] {
		g.write(c_name(hash_fn))
		if node.is_fn_var {
			g.write('(${node.name})')
		}
	} else {
		g.write(g.get_ternary_name(name))
	}
}

// gen_trace_fn_var_call generates the full traced call for function variables.
fn (mut g Gen) gen_trace_fn_var_call(node ast.CallExpr, name string, call_generic_names []string, call_concrete_types []ast.Type) {
	hash_fn, _ := g.table.get_trace_fn_name(g.cur_fn, node)
	if _ := g.cur_fn.trace_fns[hash_fn] {
		g.write('${c_name(hash_fn)}(${g.get_ternary_name(name)}')
		if node.args.len > 0 {
			g.write(', ')
		}
	} else {
		g.write('${g.get_ternary_name(name)}(')
	}
	g.call_args_with_context(node, call_generic_names, call_concrete_types)
	g.write(')')
}

fn (mut g Gen) autofree_tmp_arg_init_stmt(prepend string, expr ast.Expr) string {
	saved_out := g.out
	saved_stmt_path_pos := g.stmt_path_pos.clone()
	saved_empty_line := g.empty_line
	saved_indent := g.indent
	g.out = strings.new_builder(256)
	g.stmt_path_pos = [0]
	g.empty_line = true
	g.indent = 0
	defer {
		g.out = saved_out
		g.stmt_path_pos = saved_stmt_path_pos
		g.empty_line = saved_empty_line
		g.indent = saved_indent
	}
	return g.expr_string_surround(prepend, expr, ';').trim_space()
}

fn (mut g Gen) autofree_call_pregen(node ast.CallExpr) {
	// g.writeln('// autofree_call_pregen()')
	// Create a temporary var before fn call for each argument in order to free it (only if it's a complex expression,
	// like `foo(get_string())` or `foo(a + b)`
	mut free_tmp_arg_vars := g.is_autofree && !g.is_builtin_mod && node.args.len > 0
		&& !node.args[0].typ.has_option_or_result() // TODO: copy pasta checker.v
	if !free_tmp_arg_vars {
		return
	}
	if g.is_js_call {
		return
	}
	if g.inside_const {
		return
	}
	free_tmp_arg_vars = false // set the flag to true only if we have at least one arg to free
	g.tmp_count_af++
	mut scope := g.file.scope.innermost(node.pos.pos)
	// prepend the receiver for now (TODO turn the receiver into a CallArg everywhere?)
	mut args := [
		ast.CallArg{
			typ:             node.receiver_type
			expr:            node.left
			is_tmp_autofree: node.free_receiver
		},
	]
	args << node.args
	for i, arg in args {
		if !arg.is_tmp_autofree {
			if arg.expr is ast.CallExpr && arg.expr.name in ['json.encode', 'json.encode_pretty'] {
				t := '_arg_expr_${arg.expr.name.replace('.', '_')}_${arg.expr.pos.pos}'
				defer(fn) {
					g.writeln(';\n\tbuiltin__string_free(&${t});')
				}
			}
			continue
		}
		if arg.expr is ast.CallExpr {
			// Any argument can be an expression that has to be freed. Generate a tmp expression
			// for each of those recursively.
			g.autofree_call_pregen(arg.expr)
		}
		free_tmp_arg_vars = true
		fn_name := node.name.replace('.', '_') // can't use name...
		t := '_arg_expr_${fn_name}_${i}_${node.pos.pos}'
		used := false // scope.known_var(t)
		mut s := '${t} = '
		if used {
			// This means this tmp var name was already used (the same function was called and
			// `_arg_fnname_1` was already generated).
			// We do not need to declare this variable again, so just generate `t = ...`
			// instead of `string t = ...`, and we need to mark this variable as unused,
			// so that it's freed after the call. (Used tmp arg vars are not freed to avoid double frees).
			if mut x := scope.find_var(t) {
				x.is_used = false
			}
			s = '${t} = '
		} else {
			scope.register(ast.Var{
				name:            t
				typ:             arg.typ
				is_autofree_tmp: true
				pos:             node.pos
			})
			s = '${g.styp(arg.typ)} ${t} = '
		}
		g.is_autofree_tmp = true
		old_is_autofree := g.is_autofree
		if arg.expr is ast.CallExpr && arg.expr.is_method && arg.expr.left is ast.CallExpr {
			g.is_autofree = false
		}
		tmp_arg_init := g.autofree_tmp_arg_init_stmt(s, arg.expr)
		g.is_autofree = old_is_autofree
		g.is_autofree_tmp = false
		g.strs_to_free0 << tmp_arg_init
		// This tmp arg var will be freed with the rest of the vars at the end of the scope.
	}
}

fn (mut g Gen) call_arg_param(node ast.CallExpr, arg_idx int) ?ast.Param {
	base_name := if node.name.contains('_T_') { node.name.all_before('_T_') } else { node.name }
	if node.is_method {
		left_sym := g.table.sym(node.left_type)
		if method := g.table.find_method(left_sym, node.name) {
			return if method.is_variadic && arg_idx >= method.params.len - 2 {
				method.params.last()
			} else {
				method.params[arg_idx + 1]
			}
		}
		if method := left_sym.find_method_with_generic_parent(base_name) {
			return if method.is_variadic && arg_idx >= method.params.len - 2 {
				method.params.last()
			} else {
				method.params[arg_idx + 1]
			}
		}
		if node.receiver_type != 0 && node.receiver_type != node.left_type {
			receiver_sym := g.table.sym(node.receiver_type)
			if method := g.table.find_method(receiver_sym, node.name) {
				return if method.is_variadic && arg_idx >= method.params.len - 2 {
					method.params.last()
				} else {
					method.params[arg_idx + 1]
				}
			}
			if method := receiver_sym.find_method_with_generic_parent(base_name) {
				return if method.is_variadic && arg_idx >= method.params.len - 2 {
					method.params.last()
				} else {
					method.params[arg_idx + 1]
				}
			}
		}
		return none
	}
	if func := g.table.find_fn(node.name) {
		return if func.is_variadic && arg_idx >= func.params.len - 1 {
			func.params.last()
		} else {
			func.params[arg_idx]
		}
	}
	if base_name != node.name {
		if func := g.table.find_fn(base_name) {
			return if func.is_variadic && arg_idx >= func.params.len - 1 {
				func.params.last()
			} else {
				func.params[arg_idx]
			}
		}
	}
	return none
}

fn array_mut_arg_is_addressable(expr ast.Expr) bool {
	return match expr {
		ast.IndexExpr {
			if expr.index is ast.RangeExpr {
				false
			} else {
				expr.left.is_lvalue()
			}
		}
		ast.ParExpr {
			array_mut_arg_is_addressable(expr.expr)
		}
		else {
			expr.is_lvalue()
		}
	}
}

fn (mut g Gen) call_args(node ast.CallExpr) {
	g.expected_fixed_arr = true
	defer {
		g.expected_fixed_arr = false
	}
	args := if g.is_js_call {
		if node.args.len < 1 {
			g.error('node should have at least 1 arg', node.pos)
		}
		g.is_js_call = false
		node.args[1..]
	} else {
		node.args
	}
	mut expected_types := node.expected_arg_types.map(g.unwrap_generic(it))
	if g.has_active_call_generic_context() {
		for i in 0 .. expected_types.len {
			resolved_expected_type :=
				g.resolve_active_call_concrete_type(node.expected_arg_types[i].set_flag(.generic))
			if resolved_expected_type != 0 {
				expected_types[i] = resolved_expected_type
			}
		}
	}
	// For fn_var calls inside generic functions, the expected_arg_types may have been
	// resolved with concrete types from a previous instantiation. Re-resolve them from
	// the function pointer's type in the current generic context.
	if node.is_fn_var && g.cur_fn != unsafe { nil } && g.cur_fn.generic_names.len > 0
		&& g.cur_concrete_types.len > 0 {
		lookup_name := if node.left is ast.Ident { node.left.name } else { node.name }
		resolved_fn_type := g.resolve_current_fn_generic_param_type(lookup_name)
		mut fn_var_type := if resolved_fn_type != 0
			&& g.table.final_sym(g.unwrap_generic(resolved_fn_type)).kind == .function {
			g.unwrap_generic(g.recheck_concrete_type(resolved_fn_type))
		} else {
			g.unwrap_generic(g.recheck_concrete_type(node.fn_var_type))
		}
		if fn_var_type != 0 {
			fn_sym := g.table.final_sym(fn_var_type)
			if fn_sym.info is ast.FnType {
				for i in 0 .. expected_types.len {
					if i < fn_sym.info.func.params.len {
						resolved_param_type :=
							g.unwrap_generic(g.recheck_concrete_type(fn_sym.info.func.params[i].typ))
						if resolved_param_type != 0 {
							expected_types[i] = resolved_param_type
						}
					}
				}
			}
		}
	}
	// unwrap generics fn/method arguments to concretes
	if !g.has_active_call_generic_context() && node.concrete_types.len > 0
		&& node.concrete_types.all(!it.has_flag(.generic)) {
		if node.is_method {
			// First try to find method on left_type, then fallback to receiver_type for embedded types
			mut func := ast.Fn{}
			mut found := false
			if f := g.table.find_method(g.table.sym(node.left_type), node.name) {
				func = f
				found = true
			} else if node.receiver_type != 0 && node.receiver_type != node.left_type {
				// Method not found on left_type, try receiver_type (for embedded types)
				if f := g.table.find_method(g.table.sym(node.receiver_type), node.name) {
					func = f
					found = true
				}
			}
			if found {
				if func.generic_names.len > 0 {
					for i in 0 .. expected_types.len {
						mut muttable := unsafe { &ast.Table(g.table) }
						// Ensure the generic flag is set for conversion
						arg_type := node.expected_arg_types[i].set_flag(.generic)
						if utyp := muttable.convert_generic_type(arg_type, func.generic_names,
							node.concrete_types)
						{
							expected_types[i] = utyp
						}
					}
				}
			}
		} else {
			if func := g.table.find_fn(node.name) {
				if func.generic_names.len > 0 {
					for i in 0 .. expected_types.len {
						mut muttable := unsafe { &ast.Table(g.table) }
						// Ensure the generic flag is set for conversion
						arg_type := node.expected_arg_types[i].set_flag(.generic)
						if utyp := muttable.convert_generic_type(arg_type, func.generic_names,
							node.concrete_types)
						{
							expected_types[i] = utyp
						}
					}
				}
			}
		}
	}
	if node.is_field && node.concrete_types.len == 0 && g.cur_fn != unsafe { nil }
		&& g.cur_fn.generic_names.len > 0 && g.cur_concrete_types.len > 0 {
		left_sym := g.table.sym(node.left_type)
		if field := g.table.find_field_with_embeds(left_sym, node.name) {
			field_sym := g.table.sym(g.unwrap_generic(field.typ))
			if field_sym.kind == .function {
				if field_sym.info is ast.FnType {
					info := field_sym.info
					for i in 0 .. expected_types.len {
						if i < info.func.params.len {
							mut param_typ := info.func.params[i].typ
							if param_typ.has_flag(.generic) {
								if utyp := g.table.convert_generic_type(param_typ,
									g.cur_fn.generic_names, g.cur_concrete_types)
								{
									param_typ = utyp
								}
							}
							if expected_types[i] != param_typ {
								expected_types[i] = param_typ
							}
						}
					}
				}
			}
		}
	}
	if node.is_method {
		left_sym := g.table.sym(node.left_type)
		if left_sym.info is ast.Struct && left_sym.info.generic_types.len > 0
			&& left_sym.info.concrete_types.len > 0 {
			base_type_idx := g.table.find_type_idx(left_sym.name.all_before('['))
			mut func := ast.Fn{}
			mut found := false
			for {
				if base_type_idx > 0 {
					base_sym := g.table.sym(ast.idx_to_type(base_type_idx))
					if base_func := g.table.find_method(base_sym, node.name) {
						func = base_func
						found = true
						break
					}
				}
				if base_func := g.table.find_method(left_sym, node.name) {
					func = base_func
					found = true
				}
				break
			}
			if found && func.generic_names.len > 0 {
				for i in 0 .. expected_types.len {
					mut muttable := unsafe { &ast.Table(g.table) }
					// Ensure the generic flag is set for conversion
					arg_type := node.expected_arg_types[i].set_flag(.generic)
					if utyp := muttable.convert_generic_type(arg_type, func.generic_names,
						left_sym.info.concrete_types)
					{
						expected_types[i] = utyp
					}
				}
			}
		}
	}
	// Final safeguard: ensure any remaining generic types from the outer function are converted
	if g.cur_fn != unsafe { nil } && g.cur_fn.generic_names.len > 0 && g.cur_concrete_types.len > 0 {
		for i in 0 .. expected_types.len {
			sym := g.table.sym(expected_types[i])
			if sym.name in g.cur_fn.generic_names {
				mut muttable := unsafe { &ast.Table(g.table) }
				if utyp := muttable.convert_generic_type(expected_types[i].set_flag(.generic),
					g.cur_fn.generic_names, g.cur_concrete_types)
				{
					expected_types[i] = utyp
				}
			}
		}
	}
	// only v variadic, C variadic args will be appended like normal args
	is_variadic := node.language == .v && node.is_variadic && expected_types.len > 0
		&& expected_types.last().has_flag(.variadic)
	mut already_decomposed := false
	for i, arg in args {
		if is_variadic && i == expected_types.len - 1 {
			break
		}
		mut is_smartcast := false
		if arg.expr is ast.Ident {
			if arg.expr.obj is ast.Var {
				if i < node.expected_arg_types.len && node.expected_arg_types[i].has_flag(.generic)
					&& arg.expr.obj.ct_type_var !in [.generic_param, .no_comptime]
					&& (expected_types[i] == 0 || expected_types[i].has_flag(.generic)
					|| g.type_has_unresolved_generic_parts(expected_types[i])) {
					resolved_arg_type :=
						g.unwrap_generic(g.type_resolver.get_type(ast.Expr(arg.expr)))
					if resolved_arg_type != 0 {
						expected_types[i] = resolved_arg_type
					}
				} else if i < expected_types.len && arg.expr.obj.smartcasts.len > 0 {
					exp_sym := g.table.sym(expected_types[i])
					orig_sym := g.table.sym(arg.expr.obj.orig_type)
					if !expected_types[i].has_option_or_result() && orig_sym.kind != .interface
						&& (exp_sym.kind != .sum_type
						&& expected_types[i] != arg.expr.obj.orig_type) {
						expected_types[i] = g.unwrap_generic(arg.expr.obj.smartcasts.last())
						cast_sym := g.table.sym(expected_types[i])
						if cast_sym.info is ast.Aggregate {
							expected_types[i] = cast_sym.info.types[g.aggregate_type_idx]
						}
						is_smartcast = true
					}
				}
			}
		} else if arg.expr is ast.ArrayDecompose {
			mut d_count := 0
			remaining_params := expected_types.len - i
			if node.language == .v && node.is_variadic && arg.expr.expr_type.has_flag(.variadic)
				&& remaining_params > 0 && i < expected_types.len - 1 {
				tmp_array := g.new_tmp_var()
				line := g.go_before_last_stmt()
				array_typ := g.styp(arg.expr.expr_type)
				g.write('\t${array_typ} ${tmp_array} = ')
				g.expr(ast.Expr(arg.expr))
				g.writeln(';')
				g.write(line.trim_left('\t'))
				for d_i in i .. expected_types.len - 1 {
					g.write('*(${g.styp(expected_types[d_i])}*)builtin__array_get(${tmp_array}, ${d_count}), ')
					d_count++
				}
				g.write('builtin__array_slice(${tmp_array}, ${d_count}, 2147483647)')
			} else if !arg.expr.expr_type.has_flag(.variadic) && remaining_params > 0 {
				tmp_array := g.new_tmp_var()
				line := g.go_before_last_stmt()
				array_typ := g.styp(arg.expr.expr_type)
				g.write('\t${array_typ} ${tmp_array} = ')
				g.expr(arg.expr.expr)
				g.writeln(';')
				g.writeln('if (${tmp_array}.len < ${remaining_params}) {')
				elem_word := if remaining_params == 1 { 'element is' } else { 'elements are' }
				tmp_err_msg := g.new_tmp_var()
				g.writeln('\tstring ${tmp_err_msg};')
				g.writeln('\tif (${tmp_array}.len == 1) {')
				g.writeln('\t\t${tmp_err_msg} = builtin__str_intp(2, _MOV((StrIntpData[]){')
				g.writeln('\t\t\t{_S("array decompose: array has "), 0xfe07, {.d_i32 = ${tmp_array}.len}, 0, 0, 0},')
				g.writeln('\t\t\t{_S(" element but ${remaining_params} ${elem_word} needed"), 0, {0}, 0, 0, 0}}));')
				g.writeln('\t} else {')
				g.writeln('\t\t${tmp_err_msg} = builtin__str_intp(2, _MOV((StrIntpData[]){')
				g.writeln('\t\t\t{_S("array decompose: array has "), 0xfe07, {.d_i32 = ${tmp_array}.len}, 0, 0, 0},')
				g.writeln('\t\t\t{_S(" elements but ${remaining_params} ${elem_word} needed"), 0, {0}, 0, 0, 0}}));')
				g.writeln('\t}')
				g.writeln('\tbuiltin___v_panic(${tmp_err_msg});')
				g.writeln('}')
				g.write(line.trim_left('\t'))
				for d_i in i .. expected_types.len {
					g.write('*(${g.styp(expected_types[d_i])}*)builtin__array_get(${tmp_array}, ${d_count})')
					if d_i < expected_types.len - 1 {
						g.write(', ')
					}
					d_count++
				}
			} else {
				for d_i in i .. expected_types.len {
					g.write('*(${g.styp(expected_types[d_i])}*)builtin__array_get(')
					g.expr(ast.Expr(arg.expr))
					g.write(', ${d_count})')
					if d_i < expected_types.len - 1 {
						g.write(', ')
					}
					d_count++
				}
			}
			already_decomposed = true
			continue
		} else if arg.expr is ast.ComptimeSelector && i < node.expected_arg_types.len
			&& node.expected_arg_types[i].has_flag(.generic) {
			exp_option := node.expected_arg_types[i].has_flag(.option)
			expected_types[i] = g.unwrap_generic(g.type_resolver.get_type(ast.Expr(arg.expr)))
			if !exp_option {
				expected_types[i] = expected_types[i].clear_flag(.option)
			}
		} else if arg.expr is ast.CallExpr {
			if arg.expr.nr_ret_values > 1 {
				line := g.go_before_last_stmt().trim_space()
				g.empty_line = true
				ret_type := arg.expr.return_type
				tmp_var := g.new_tmp_var()
				g.write('${g.styp(ret_type)} ${tmp_var} = ')
				g.expr(ast.Expr(arg.expr))
				g.writeln(';')
				g.write(line)
				for n in 0 .. arg.expr.nr_ret_values {
					if n != arg.expr.nr_ret_values - 1 || i != args.len - 1 {
						g.write('${tmp_var}.arg${n}, ')
					} else {
						g.write('${tmp_var}.arg${n}')
					}
				}
				continue
			}
		}
		use_tmp_var_autofree := g.is_autofree && arg.typ == ast.string_type && arg.is_tmp_autofree
			&& !g.inside_const && !g.is_builtin_mod && g.inside_ternary == 0
		mut effective_arg := arg
		if !effective_arg.is_mut {
			if param := g.call_arg_param(node, i) {
				if param.is_mut {
					effective_arg = ast.CallArg{
						...effective_arg
						is_mut: true
					}
				}
			}
		}
		if i < expected_types.len {
			if param := g.call_arg_param(node, i) {
				param_needs_generic_resolution := g.type_needs_generic_resolution(param.typ)
					|| g.has_active_call_generic_context()
				expected_needs_generic_resolution := expected_types[i] == 0
					|| g.type_needs_generic_resolution(expected_types[i])
				if param_needs_generic_resolution || expected_needs_generic_resolution
					|| node.concrete_types.len > 0 || g.cur_concrete_types.len > 0 {
					resolved_expected_type :=
						g.resolve_active_call_concrete_type(param.typ.set_flag(.generic))
					if resolved_expected_type != 0 && resolved_expected_type != expected_types[i]
						&& !g.type_needs_generic_resolution(resolved_expected_type)
						&& (expected_needs_generic_resolution
						|| node.concrete_types.len > 0
						|| g.cur_concrete_types.len > 0) {
						expected_types[i] =
							g.unwrap_generic(g.recheck_concrete_type(resolved_expected_type))
					}
				}
			}
		}
		// g.write('/* af=${arg.is_tmp_autofree} */')
		// some c fn definitions dont have args (cfns.v) or are not updated in checker
		// when these are fixed we wont need this check
		if i < expected_types.len {
			mut wrote_tmp_arg := false
			if use_tmp_var_autofree && arg.is_tmp_autofree { // && !g.is_js_call {
				// We saved expressions in temp variables so that they can be freed later.
				// `foo(str + str2) => x := str + str2; foo(x); x.free()`
				// g.write('_arg_expr_${g.called_fn_name}_${i}')
				// Use these variables here.
				fn_name := node.name.replace('.', '_')
				// name := '_tt${g.tmp_count_af}_arg_expr_${fn_name}_${i}'
				name := '_arg_expr_${fn_name}_${i + 1}_${node.pos.pos}'
				scope := g.file.scope.innermost(node.pos.pos)
				if !g.is_autofree_tmp || scope.known_var(name) {
					tmp_arg := ast.CallArg{
						typ:    arg.typ
						is_mut: effective_arg.is_mut
						expr:   ast.Ident{
							name: name
							kind: .variable
							info: ast.IdentVar{
								typ:    arg.typ
								is_mut: effective_arg.is_mut
							}
						}
					}
					g.write('/*autofree arg*/')
					g.ref_or_deref_arg(tmp_arg, expected_types[i], node.language, is_smartcast)
					wrote_tmp_arg = true
				}
			}
			if !wrote_tmp_arg {
				g.ref_or_deref_arg(effective_arg, expected_types[i], node.language, is_smartcast)
			}
		} else {
			if use_tmp_var_autofree {
				n := if node.kind == .json_decode { i + 2 } else { i + 1 }
				// TODO: copypasta, move to an inline fn
				fn_name := node.name.replace('.', '_')
				name := '_arg_expr_${fn_name}_${n}_${node.pos.pos}'
				g.write('/*af arg2*/' + name)
			} else {
				g.expr(arg.expr)
			}
		}
		if i < args.len - 1 || is_variadic {
			g.write(', ')
		}
	}
	arg_nr := expected_types.len - 1
	if is_variadic {
		varg_type := expected_types.last()
		variadic_count := args.len - arg_nr
		arr_sym := g.table.sym(varg_type)
		mut arr_info := arr_sym.info as ast.Array
		if varg_type.has_flag(.generic) {
			if node.is_method {
				left_sym := g.table.sym(node.left_type)
				if fn_def := left_sym.find_method_with_generic_parent(node.name) {
					mut muttable := unsafe { &ast.Table(g.table) }
					if utyp := muttable.convert_generic_type(arr_info.elem_type,
						fn_def.generic_names, node.concrete_types)
					{
						arr_info.elem_type = utyp
					}
				} else {
					g.error('unable to find method ${node.name}', node.pos)
				}
			} else {
				if fn_def := g.table.find_fn(node.name) {
					mut muttable := unsafe { &ast.Table(g.table) }
					if utyp := muttable.convert_generic_type(arr_info.elem_type,
						fn_def.generic_names, node.concrete_types)
					{
						arr_info.elem_type = utyp
					}
				} else {
					g.error('unable to find function ${node.name}', node.pos)
				}
			}
		}
		elem_type := g.styp(arr_info.elem_type)
		if (g.pref.translated || g.file.is_translated) && args.len == 1 {
			// Handle `foo(c'str')` for `fn foo(args ...&u8)`
			// TODOC2V handle this in a better place
			g.expr(args[0].expr)
		} else if args.len > 0 && args.last().expr is ast.ArrayDecompose {
			if !already_decomposed {
				array_decompose := args.last().expr as ast.ArrayDecompose
				varg_array_type := varg_type.clear_flag(.variadic)
				if g.table.unaliased_type(array_decompose.expr_type) == g.table.unaliased_type(varg_array_type) {
					g.expr(args.last().expr)
				} else {
					g.write_converted_variadic_array_decompose_arg(array_decompose,
						varg_array_type, arr_info.elem_type, node.language)
				}
			}
		} else {
			if variadic_count > 0 {
				if node.is_c_variadic {
					// Handle passing e.g. C string literals to `...` C varargs:
					// void DEH_snprintf(char *buffer, size_t len, const char *fmt, ...)
					// deh_snprintf(buffer, 9, c'STCFN%.3d', j++)
					for j in arg_nr .. args.len {
						g.expr(args[j].expr)
						if j < args.len - 1 {
							g.write(', ')
						}
					}
				} else {
					// passing the current variadic arg to another call which expects the same array type
					if variadic_count == 1 && g.is_forwarded_variadic_arg(args[arg_nr])
						&& ((args[arg_nr].typ.has_flag(.variadic) && args[arg_nr].typ == varg_type)
						|| (varg_type.has_flag(.variadic)
						&& args[arg_nr].typ == varg_type.clear_flag(.variadic)
						&& !g.table.sumtype_has_variant(arr_info.elem_type, args[arg_nr].typ, false))) {
						g.ref_or_deref_arg_ex(args[arg_nr], arr_info.elem_type, node.language,
							false, true)
					} else {
						noscan := g.check_noscan(arr_info.elem_type)
						is_option := arr_info.elem_type.has_flag(.option)
						tmp_var := if is_option { g.new_tmp_var() } else { '' }
						base_type := g.base_type(varg_type)
						tmp := if is_option { g.go_before_last_stmt() } else { '' }
						if is_option {
							g.writeln('${g.styp(varg_type)} ${tmp_var};')
							g.write('builtin___option_ok((${base_type}[]) {')
						}
						g.write('builtin__new_array_from_c_array${noscan}(${variadic_count}, ${variadic_count}, sizeof(${elem_type}), _MOV((${elem_type}[${variadic_count}]){')
						for j in arg_nr .. args.len {
							g.ref_or_deref_arg_ex(args[j], arr_info.elem_type, node.language,
								false, true)
							if j < args.len - 1 {
								g.write(', ')
							}
						}
						g.write('}))')
						if is_option {
							g.writeln(' }, (${option_name}*)&${tmp_var}, sizeof(${base_type}));')
							g.write(tmp)
							g.write(tmp_var)
						}
					}
				}
			} else {
				g.write('builtin____new_array(0, 0, sizeof(${elem_type}))')
			}
		}
	}
}

fn (g &Gen) is_forwarded_variadic_arg(arg ast.CallArg) bool {
	if arg.typ.has_flag(.variadic) {
		return true
	}
	if arg.expr is ast.Ident && g.cur_fn != unsafe { nil } && g.cur_fn.is_variadic
		&& g.cur_fn.params.len > 0 && arg.expr.obj is ast.Var {
		var_obj := arg.expr.obj as ast.Var
		return var_obj.is_arg && arg.expr.name == g.cur_fn.params.last().name
	}
	return false
}

fn (mut g Gen) write_converted_variadic_array_decompose_arg(node ast.ArrayDecompose, varg_array_type ast.Type,
	elem_type ast.Type, lang ast.Language) {
	stmt_str := g.go_before_last_stmt().trim_space()
	g.empty_line = true
	src_array_type := g.unwrap_generic(node.expr_type)
	src_array_styp := g.styp(src_array_type)
	src_array_var := g.new_tmp_var()
	g.write('${src_array_styp} ${src_array_var} = ')
	g.expr(node.expr)
	g.writeln(';')
	elem_styp := g.styp(elem_type)
	noscan := g.check_noscan(elem_type)
	dst_var := g.new_tmp_var()
	g.writeln('${g.styp(varg_array_type)} ${dst_var} = builtin____new_array${noscan}(0, ${src_array_var}.len, sizeof(${elem_styp}));')
	src_array_info := g.table.final_sym(src_array_type).array_info()
	src_elem_type := src_array_info.elem_type
	idx_var := g.new_tmp_var()
	g.writeln('for (${ast.int_type_name} ${idx_var} = 0; ${idx_var} < ${src_array_var}.len; ++${idx_var}) {')
	g.indent++
	src_item_expr := ast.IndexExpr{
		left:      ast.Ident{
			name: src_array_var
			kind: .variable
			info: ast.IdentVar{
				typ: src_array_type
			}
		}
		left_type: src_array_type
		index:     ast.Ident{
			name: idx_var
			kind: .variable
			info: ast.IdentVar{
				typ: ast.int_type
			}
		}
		is_array:  true
		typ:       src_elem_type
	}
	src_item_arg := ast.CallArg{
		typ:  src_elem_type
		expr: src_item_expr
	}
	converted_var := g.new_tmp_var()
	g.write('${elem_styp} ${converted_var} = ')
	g.ref_or_deref_arg(src_item_arg, elem_type, lang, false)
	g.writeln(';')
	g.writeln('builtin__array_push${noscan}((array*)&${dst_var}, &${converted_var});')
	g.indent--
	g.writeln('}')
	g.write(stmt_str)
	g.write(dst_var)
}

// similar to `autofree_call_pregen()` but only to to handle [keep_args_alive] for C functions
fn (mut g Gen) keep_alive_call_pregen(node ast.CallExpr) int {
	g.empty_line = true
	g.writeln('// keep_alive_call_pregen()')
	// reserve the next tmp_vars for arguments
	tmp_cnt_save := g.tmp_count + 1
	g.tmp_count += node.args.len
	for i, arg in node.args {
		// save all arguments in temp vars (not only pointers) to make sure the
		// evaluation order is preserved
		expected_type := node.expected_arg_types[i]
		typ_sym := g.table.sym(expected_type)
		typ := g.styp(expected_type)
		if typ_sym.kind != .array_fixed {
			g.write('${typ} __tmp_arg_${tmp_cnt_save + i} = ')
			g.ref_or_deref_arg(arg, expected_type, node.language, false)
		} else {
			g.writeln('${typ} __tmp_arg_${tmp_cnt_save + i} = {0};')
			g.write('memcpy(&__tmp_arg_${tmp_cnt_save + i}, ')
			g.ref_or_deref_arg(arg, expected_type, node.language, false)
			g.writeln(', sizeof(${typ}));')
		}
		g.writeln(';')
	}
	g.empty_line = false
	return tmp_cnt_save
}

fn (mut g Gen) keep_alive_call_postgen(node ast.CallExpr, tmp_cnt_save int) {
	g.writeln('// keep_alive_call_postgen()')
	for i, expected_type in node.expected_arg_types {
		if expected_type.is_any_kind_of_pointer() {
			g.writeln('GC_reachable_here(__tmp_arg_${tmp_cnt_save + i});')
		}
	}
}

fn (mut g Gen) write_multi_ref_arg(arg ast.CallArg, arg_typ ast.Type, expected_type ast.Type, lang ast.Language) bool {
	if lang != .v || g.is_json_fn || !expected_type.is_ptr() || expected_type.has_option_or_result()
		|| expected_type.nr_muls() <= 1 || expected_type.nr_muls() <= arg_typ.nr_muls()
		|| arg_typ.has_flag(.shared_f) || expected_type.has_flag(.shared_f) {
		return false
	}
	expected_deref_type := expected_type.deref()
	expected_deref_sym := g.table.sym(expected_deref_type)
	arg_sym := g.table.sym(arg_typ)
	if expected_deref_sym.kind in [.interface, .sum_type] || arg_sym.kind == .function
		|| arg.expr is ast.None {
		return false
	}
	if arg.expr is ast.Ident
		&& (arg.expr.language == .c || g.table.is_interface_smartcast(arg.expr.obj)) {
		return false
	}
	// Build the first reference level from the original storage when possible,
	// then materialize deeper levels through compound-literal temporaries.
	extra_refs := expected_type.nr_muls() - arg_typ.nr_muls()
	for level := extra_refs - 1; level > 0; level-- {
		ref_type := arg_typ.set_nr_muls(arg_typ.nr_muls() + level)
		g.write('ADDR(${g.styp(ref_type)}, ')
	}
	old_arg_no_auto_deref := g.arg_no_auto_deref
	if arg.expr.is_auto_deref_var() {
		g.arg_no_auto_deref = true
	}
	defer {
		g.arg_no_auto_deref = old_arg_no_auto_deref
	}
	is_auto_heap_ident := arg.expr is ast.Ident && g.resolved_ident_is_auto_heap(arg.expr)
	if is_auto_heap_ident {
		g.write_raw_receiver_expr(arg.expr)
	} else if arg.expr.is_lvalue() {
		g.write('&')
		g.expr(arg.expr)
	} else {
		g.write('ADDR(${g.styp(arg_typ)}, ')
		g.expr_with_cast(arg.expr, arg_typ, arg_typ)
		g.write(')')
	}
	g.write(')'.repeat(extra_refs - 1))
	return true
}

fn (mut g Gen) write_mut_unwrapped_option_payload_arg(arg ast.CallArg, expected_type ast.Type) bool {
	if !arg.is_mut || !expected_type.is_ptr() || expected_type.has_option_or_result() {
		return false
	}
	if arg.expr !is ast.PostfixExpr {
		return false
	}
	postfix_expr := arg.expr as ast.PostfixExpr
	if postfix_expr.op != .question {
		return false
	}
	option_expr := postfix_expr.expr
	if option_expr !is ast.ComptimeSelector {
		return false
	}
	mut option_type := g.resolved_expr_type(option_expr, ast.void_type)
	if option_type == ast.void_type {
		option_type = g.type_resolver.get_type_or_default(option_expr, ast.void_type)
	}
	option_type = g.unwrap_generic(g.recheck_concrete_type(option_type))
	if !option_type.has_flag(.option) {
		return false
	}
	payload_type := g.unwrap_generic(g.recheck_concrete_type(option_type.clear_option_and_result()))
	payload_styp := g.base_type(option_type)
	option_styp := g.styp(option_type)
	default_value := g.type_default(payload_type)
	stmt_str := g.go_before_last_stmt().trim_space()
	option_ptr_var := g.new_tmp_var()
	g.empty_line = true
	g.write('${option_styp}* ${option_ptr_var} = &')
	g.expr(option_expr)
	g.writeln(';')
	g.writeln('if (${option_ptr_var}->state == 2) {')
	g.writeln('\tbuiltin___option_ok(&(${payload_styp}[]){ ${default_value} }, (${option_styp}*)${option_ptr_var}, sizeof(${payload_styp}));')
	g.writeln('}')
	if stmt_str != '' {
		g.write2(stmt_str, ' ')
	}
	g.write('(${g.styp(expected_type)})${option_ptr_var}->data')
	return true
}

@[inline]
fn (mut g Gen) ref_or_deref_arg(arg ast.CallArg, expected_type_ ast.Type, lang ast.Language, is_smartcast bool) {
	g.ref_or_deref_arg_ex(arg, expected_type_, lang, is_smartcast, false)
}

fn (mut g Gen) ref_or_deref_arg_ex(arg ast.CallArg, expected_type_ ast.Type, lang ast.Language, is_smartcast bool, is_variadic_arg bool) {
	expected_type := g.unwrap_generic(expected_type_)
	mut arg_typ := if arg.ct_expr {
		g.unwrap_generic(g.type_resolver.get_type(arg.expr))
	} else {
		g.unwrap_generic(arg.typ)
	}
	in_generic_context := g.has_current_generic_context()
	if arg.expr is ast.Ident {
		needs_resolved_ident_type := arg_typ == 0
			|| g.type_needs_generic_resolution(arg_typ) || (arg.expr.obj is ast.Var
			&& (arg.expr.obj.ct_type_var in [.generic_param, .generic_var]
			|| arg.expr.obj.is_inherited))
		if needs_resolved_ident_type {
			resolved_arg_typ := g.resolved_expr_type(ast.Expr(arg.expr), arg_typ)
			if resolved_arg_typ != 0 {
				arg_typ = g.unwrap_generic(g.recheck_concrete_type(resolved_arg_typ))
			}
		}
		if expected_type.has_option_or_result() && !arg_typ.has_option_or_result() {
			resolved_scope_type := g.resolved_scope_var_type(arg.expr)
			if resolved_scope_type != 0 && resolved_scope_type.has_option_or_result() {
				arg_typ = g.unwrap_generic(g.recheck_concrete_type(resolved_scope_type))
			}
		}
		if arg.expr.obj is ast.Var && arg.expr.obj.is_arg && arg.expr.obj.is_mut && !arg.is_mut
			&& arg_typ.is_ptr() && !expected_type.is_any_kind_of_pointer() && in_generic_context {
			resolved_param_type := g.resolve_current_fn_generic_param_type(arg.expr.name)
			if resolved_param_type != 0 && !resolved_param_type.is_ptr() {
				arg_typ = resolved_param_type
			}
		}
	}
	needs_resolved_expr_type := arg.expr is ast.SelectorExpr
		|| arg.expr is ast.IndexExpr || arg.expr is ast.ComptimeSelector
		|| (arg.expr is ast.Ident && in_generic_context && (arg_typ == 0
		|| g.type_needs_generic_resolution(arg_typ) || (arg.expr.obj is ast.Var
		&& (arg.expr.obj.ct_type_var != .no_comptime || arg.expr.obj.generic_typ != 0
		|| arg.expr.obj.is_inherited || arg.expr.obj.smartcasts.len > 0
		|| arg.expr.obj.is_unwrapped))))
	if needs_resolved_expr_type {
		resolved_arg_typ := g.resolved_expr_type(arg.expr, arg_typ)
		if resolved_arg_typ != 0 && (arg_typ == 0 || g.type_needs_generic_resolution(arg_typ)
			|| in_generic_context
			|| g.unwrap_generic(resolved_arg_typ) != g.unwrap_generic(arg_typ)) {
			arg_typ = g.unwrap_generic(g.recheck_concrete_type(resolved_arg_typ))
		}
	}
	// Array slice expressions (e.g. b[..8]) always return a value in C (via builtin__array_slice),
	// even when the container is a pointer (e.g. mut []u8 param). Deref so ref_or_deref_arg
	// correctly sees this as a non-pointer arg that needs &.
	if arg.expr is ast.IndexExpr && arg.expr.index is ast.RangeExpr && arg_typ.is_ptr() {
		arg_typ = arg_typ.deref()
	}
	arg_sym := g.table.sym(arg_typ)
	exp_is_ptr := expected_type.is_any_kind_of_pointer()
	arg_is_ptr := arg_typ.is_any_kind_of_pointer()
	if expected_type == 0 {
		g.checker_bug('ref_or_deref_arg expected_type is 0', arg.pos)
	}
	old_expected_arg_mut := g.expected_arg_mut
	g.expected_arg_mut = arg.is_mut
	defer {
		g.expected_arg_mut = old_expected_arg_mut
	}
	exp_sym := g.table.sym(expected_type)
	if g.write_mut_unwrapped_option_payload_arg(arg, expected_type) {
		return
	}
	// Cast function pointer arguments when the expected and actual types differ.
	// V's checker allows coercing function types (e.g. Renderer* → voidptr in callbacks),
	// but C compilers with -Werror=incompatible-pointer-types reject the mismatch.
	// Only cast for V language calls with named function type aliases (not anonymous fn types).
	if exp_sym.kind == .function && arg_sym.kind == .function && !arg.is_mut && lang == .v
		&& !exp_sym.name.starts_with('fn ') && !expected_type.has_flag(.option) {
		exp_styp := g.styp(expected_type)
		arg_styp := g.styp(arg_typ)
		if exp_styp != arg_styp {
			g.write('(${exp_styp})')
			g.expr(arg.expr)
			return
		}
	}
	if arg.is_mut && exp_sym.kind in [.interface, .sum_type] {
		expected_wrap_type := if expected_type.is_ptr() {
			expected_type.deref()
		} else {
			expected_type
		}
		if arg_sym.kind == exp_sym.kind && arg_typ.idx() == expected_wrap_type.idx()
			&& arg.expr in [ast.Ident, ast.SelectorExpr] {
			g.prevent_sum_type_unwrapping_once = g.is_expr_smartcast_to_sumtype(arg.expr,
				expected_wrap_type)
			if arg_typ.is_ptr() {
				g.expr(arg.expr)
			} else {
				g.write('&')
				g.expr(arg.expr)
			}
		} else {
			if exp_sym.kind == .interface {
				// A converted `mut` interface argument can escape the callee
				// (for example into `[]&Interface`), so the wrapper must outlive
				// the current block scope.
				g.write('HEAP(${g.table.sym(expected_wrap_type).cname}, ')
			} else {
				g.write('ADDR(${g.table.sym(expected_wrap_type).cname}, ')
			}
			g.expr_with_cast(arg.expr, arg_typ, expected_wrap_type)
			g.write(')')
		}
		return
	}
	if expected_type.is_ptr() && !expected_type.has_option_or_result() {
		expected_ref_inner_type := expected_type.deref()
		expected_ref_inner_sym := g.table.sym(expected_ref_inner_type)
		if expected_ref_inner_sym.kind in [.interface, .sum_type] {
			mut effective_arg_expr := arg.expr
			mut effective_arg_typ := arg_typ
			if expected_ref_inner_sym.kind == .sum_type && arg.expr is ast.CastExpr {
				cast_expr := arg.expr
				cast_target_type := g.unwrap_generic(if cast_expr.typ != 0 {
					cast_expr.typ
				} else {
					arg_typ
				})
				if cast_target_type.idx() == expected_ref_inner_type.idx()
					&& cast_expr.expr_type != 0 {
					effective_arg_expr = cast_expr.expr
					effective_arg_typ = g.unwrap_generic(cast_expr.expr_type)
				}
			}
			effective_arg_sym := g.table.sym(effective_arg_typ)
			effective_arg_is_auto_heap_ident := effective_arg_expr is ast.Ident
				&& g.resolved_ident_is_auto_heap(effective_arg_expr)
			if (effective_arg_typ.is_ptr() && effective_arg_typ.deref() == expected_ref_inner_type)
				|| (effective_arg_is_auto_heap_ident
				&& effective_arg_typ.idx() == expected_ref_inner_type.idx()) {
				g.prevent_sum_type_unwrapping_once = g.is_expr_smartcast_to_sumtype(effective_arg_expr,
					expected_ref_inner_type)
				if effective_arg_is_auto_heap_ident {
					g.write_raw_receiver_expr(effective_arg_expr)
				} else {
					g.expr(effective_arg_expr)
				}
			} else if effective_arg_sym.kind == expected_ref_inner_sym.kind
				&& effective_arg_typ.idx() == expected_ref_inner_type.idx() {
				g.prevent_sum_type_unwrapping_once = g.is_expr_smartcast_to_sumtype(effective_arg_expr,
					expected_ref_inner_type)
				if effective_arg_expr in [ast.Ident, ast.SelectorExpr] {
					g.write('&')
					g.expr(effective_arg_expr)
				} else {
					g.write('ADDR(${g.styp(expected_ref_inner_type)}, ')
					g.expr_with_cast(effective_arg_expr, effective_arg_typ, expected_ref_inner_type)
					g.write(')')
				}
			} else if effective_arg_typ == ast.nil_type
				|| effective_arg_typ.is_voidptr()
				|| (effective_arg_expr is ast.UnsafeExpr && effective_arg_expr.expr is ast.Nil) {
				g.write('((void*)0)')
			} else {
				if expected_ref_inner_sym.kind == .sum_type {
					// `&Variant` -> `&SumType` needs a stable heap-allocated wrapper and
					// must preserve aliasing with the original variant payload.
					g.expr_with_cast(effective_arg_expr, effective_arg_typ, expected_type)
				} else {
					// interface conversions don't box by reference, so HEAP is needed to
					// ensure the pointer remains valid after the current scope ends.
					g.write('HEAP(${expected_ref_inner_sym.cname}, ')
					g.expr_with_cast(effective_arg_expr, effective_arg_typ, expected_ref_inner_type)
					g.write(')')
				}
			}
			return
		}
	}
	if arg_sym.kind == .interface && exp_sym.kind == .interface && arg_typ != expected_type
		&& !exp_is_ptr && !arg.is_mut && !expected_type.has_flag(.option) {
		g.expr_with_cast(arg.expr, arg_typ, expected_type)
		return
	}
	if g.write_multi_ref_arg(arg, arg_typ, expected_type, lang) {
		return
	}
	is_auto_heap_ident := arg.expr is ast.Ident && g.resolved_ident_is_auto_heap(arg.expr)
	mut needs_closing := false
	old_inside_smartcast := g.inside_smartcast
	if arg.is_mut && arg.expr.is_auto_deref_var() && arg_typ.is_ptr() && expected_type.is_ptr() {
		g.arg_no_auto_deref = true
		g.expr_with_cast(arg.expr, arg_typ, expected_type)
		g.arg_no_auto_deref = false
		return
	}
	if arg.is_mut && !exp_is_ptr {
		g.write('&/*mut*/')
	} else if arg.is_mut && arg_typ.is_ptr() && expected_type.is_ptr()
		&& g.table.sym(arg_typ).kind == .struct && expected_type == arg_typ.ref() {
		if arg.expr is ast.PrefixExpr && arg.expr.op == .amp {
			g.arg_no_auto_deref = true
			g.expr(ast.Expr(arg.expr))
			g.arg_no_auto_deref = false
		} else {
			g.write('&/*mut*/')
			g.expr(arg.expr)
		}
		return
	} else if exp_is_ptr && !expected_type.has_option_or_result() && !arg_is_ptr
		&& !(arg_sym.kind == .alias && g.table.unaliased_type(arg_typ).is_pointer()
		&& expected_type.is_pointer()) {
		if is_auto_heap_ident {
			g.write_raw_receiver_expr(arg.expr)
			return
		}
		if arg.is_mut {
			if exp_sym.kind == .array {
				if array_mut_arg_is_addressable(arg.expr) {
					g.write('&')
					if expected_type.has_flag(.option_mut_param_t) {
						g.expr_with_opt(arg.expr, arg_typ, expected_type)
					} else {
						g.expr(arg.expr)
					}
				} else {
					// Special case for mutable arrays. We can't `&` function
					// results,	have to use `(array[]){ expr }[0]` hack.
					g.write('&(array[]){')
					g.expr(arg.expr)
					g.write('}[0]')
				}
				return
			} else if arg_sym.kind == .sum_type && exp_sym.kind == .sum_type
				&& arg.expr in [ast.Ident, ast.SelectorExpr] {
				g.write('&')
				g.expr(arg.expr)
				return
			} else if arg_sym.kind == .interface && exp_sym.kind == .interface
				&& arg.expr in [ast.Ident, ast.SelectorExpr] {
				g.write('&')
				g.expr(arg.expr)
				return
			}
		}
		if !g.is_json_fn {
			if arg_typ == 0 {
				g.checker_bug('ref_or_deref_arg arg.typ is 0', arg.pos)
			}
			arg_typ_sym := g.table.sym(arg_typ)
			expected_deref_type := if expected_type.is_ptr() {
				expected_type.deref()
			} else {
				expected_type
			}
			promoted_variadic_type := if is_variadic_arg && expected_deref_type.is_voidptr() {
				g.variadic_voidptr_promoted_type(arg_typ)
			} else {
				ast.no_type
			}
			deref_sym := g.table.sym(expected_deref_type)
			if arg_typ_sym.kind != .function && deref_sym.kind !in [.sum_type, .interface]
				&& lang != .c {
				if arg.expr.is_lvalue() {
					if promoted_variadic_type != ast.no_type {
						g.write('(voidptr)ADDR(${g.styp(promoted_variadic_type)}, ')
						g.expr_with_cast(arg.expr, arg_typ, promoted_variadic_type)
						g.write(')')
						return
					}
					if expected_type.has_flag(.option) {
						if expected_type.has_flag(.option_mut_param_t) {
							g.write('&')
						}
						g.expr_with_opt(arg.expr, arg_typ, expected_type)
						return
					} else if arg.expr is ast.Ident && arg.expr.language == .c {
						g.write('(voidptr)')
					} else if !(!arg.is_mut && arg_sym.kind == .alias
						&& g.table.unaliased_type(arg_typ).is_any_kind_of_pointer()) {
						g.write('(voidptr)&')
					}
				} else {
					mut atype := expected_deref_type
					if atype.has_flag(.generic) {
						atype = g.unwrap_generic(atype)
					}
					if atype.has_flag(.generic) || arg.expr is ast.StructInit {
						g.write('(voidptr)&')
					} else if arg.expr is ast.None {
						g.expr_with_opt(ast.Expr(arg.expr), arg_typ, expected_type)
						return
					} else if expected_deref_type.is_voidptr() && !arg_typ.is_any_kind_of_pointer() {
						boxed_type := if promoted_variadic_type != ast.no_type {
							promoted_variadic_type
						} else {
							arg_typ
						}
						g.write('(voidptr)ADDR(${g.styp(boxed_type)}, ')
						g.expr_with_cast(arg.expr, arg_typ, boxed_type)
						g.write(')')
						return
					} else if arg.expr.is_literal() {
						g.write('(voidptr)ADDR(${g.styp(arg_typ)}, ')
						g.expr(arg.expr)
						g.write(')')
						return
					} else {
						if arg_typ_sym.kind in [.sum_type, .interface] {
							atype = arg_typ
						}
						if arg.expr.is_as_cast() {
							g.inside_smartcast = true
						} else if arg_typ_sym.is_int() && arg.expr !is ast.CastExpr {
							g.write('(voidptr)&')
						} else {
							g.write('ADDR(${g.styp(atype)}, ')
							needs_closing = true
						}
					}
				}
			} else if arg_sym.kind == .sum_type && exp_sym.kind == .sum_type {
				// Automatically passing sum types by reference if the argument expects it,
				// not only the argument is mutable.
				if arg.expr is ast.SelectorExpr {
					g.write('&')
					g.expr(ast.Expr(arg.expr))
					return
				} else if arg.expr is ast.CastExpr {
					g.write('ADDR(${g.styp(expected_deref_type)}, ')
					g.expr_with_cast(ast.Expr(arg.expr), arg_typ, expected_type)
					g.write(')')
					return
				}
			} else if arg_sym.kind == .interface && exp_sym.kind == .interface {
				if exp_is_ptr && !arg_is_ptr {
					g.write('&')
				}
			}
		}
	} else if arg_typ.has_flag(.shared_f) && !expected_type.has_flag(.shared_f) {
		if expected_type.is_ptr() {
			g.write('&')
		}
		g.expr(arg.expr)
		g.write('->val')
		return
	} else if expected_type.has_flag(.option) {
		if expected_type.has_flag(.option_mut_param_t)
			&& arg_typ.nr_muls() <= expected_type.nr_muls() && !(arg.expr is ast.Ident
			&& (arg.expr.obj is ast.Var && arg.expr.obj.is_inherited)) {
			g.write('&')
		}
		if (arg_sym.info is ast.Alias || exp_sym.info is ast.Alias) && expected_type != arg_typ {
			g.expr_opt_with_alias(arg.expr, arg_typ, expected_type)
		} else {
			if arg.expr is ast.Ident && arg.expr.obj is ast.Var {
				if arg.expr.obj.smartcasts.len > 0 && !arg_typ.has_option_or_result() {
					arg_typ = arg.expr.obj.smartcasts.last()
				}
			}
			// If the argument is a smartcast variable whose original type is option
			// and matches the expected option type, pass the option value directly.
			if arg.expr is ast.Ident {
				if scope_var := arg.expr.scope.find_var(arg.expr.name) {
					if scope_var.smartcasts.len > 0 && scope_var.orig_type.has_flag(.option)
						&& expected_type.has_flag(.option)
						&& g.unwrap_generic(scope_var.orig_type).idx() == g.unwrap_generic(expected_type).idx() {
						g.write(c_name(arg.expr.name))
						return
					}
				}
			}
			g.expr_with_opt(arg.expr, arg_typ, expected_type)
		}
		return
	} else if arg.expr is ast.ArrayInit {
		if arg.expr.is_fixed {
			if !arg.expr.has_index {
				g.write('(${g.styp(arg.expr.typ)})')
			}
		}
	} else if arg.expr is ast.ComptimeSelector && arg_typ.has_flag(.option)
		&& !expected_type.has_flag(.option) {
		// allow to pass val.$(filed.name) where T is expected, doing automatic unwrap in this case
		styp := g.base_type(arg_typ)
		g.write('*(${styp}*)')
		g.expr_with_cast(ast.Expr(arg.expr), arg_typ, expected_type)
		g.write('.data')
		return
	} else if arg.expr is ast.Ident && arg_sym.info is ast.Struct && arg_sym.info.is_anon
		&& !expected_type.has_flag(.generic) {
		// make anon struct struct compatible with another anon struct declaration
		g.write('*(${g.cc_type(expected_type, false)}*)&')
	} else if arg.expr is ast.Ident {
		if arg.expr.obj is ast.Var {
			if arg.expr.obj.is_arg && arg.expr.obj.is_mut && !arg.is_mut && arg_typ.is_ptr()
				&& !expected_type.is_any_kind_of_pointer() {
				unwrapped_expected := g.unwrap_generic(g.recheck_concrete_type(expected_type))
				nr_derefs := arg_typ.nr_muls()
				if nr_derefs > 0 && unwrapped_expected == g.unwrap_generic(arg_typ.set_nr_muls(0)) {
					g.write('${'*'.repeat(nr_derefs)}')
					g.expr(ast.Expr(arg.expr))
					return
				}
			}
		} else if arg.expr.kind == .constant && arg_typ.is_ptr()
			&& !expected_type.is_any_kind_of_pointer()
			&& g.unwrap_generic(g.recheck_concrete_type(expected_type)).idx() == arg_typ.deref().idx() {
			// Const declared with `&` (e.g. `const x = &Foo{}`) is stored as a pointer in C,
			// but when passed to a function expecting a value parameter, it must be dereferenced.
			g.write('(*')
			g.expr(ast.Expr(arg.expr))
			g.write(')')
			return
		}
	}
	// check if the argument must be dereferenced or not
	g.arg_no_auto_deref = is_smartcast && !arg_is_ptr && !exp_is_ptr && arg.should_be_ptr
	// When a smartcast selector/ident is passed to a function expecting the original sumtype,
	// the type resolution above resolves arg_typ to the sumtype (matching expected_type),
	// so expr_with_cast won't handle the sumtype wrapping. We need to prevent the smartcast
	// unwrapping in g.expr() since the value is already the correct sumtype at runtime.
	if !arg_typ.has_flag(.option) && exp_sym.kind == .sum_type && arg_typ == expected_type {
		g.prevent_sum_type_unwrapping_once = g.is_expr_smartcast_to_sumtype(arg.expr, expected_type)
	}
	if arg_typ.has_flag(.option) {
		g.expr_with_opt(arg.expr, arg_typ, expected_type.set_flag(.option))
	} else if expected_type.has_flag(.option) && !arg_typ.has_flag(.option) {
		g.expr_with_opt(arg.expr, arg_typ, expected_type)
	} else {
		g.expr_with_cast(arg.expr, arg_typ, expected_type)
	}
	g.arg_no_auto_deref = false
	g.inside_smartcast = old_inside_smartcast
	if needs_closing {
		g.write(')')
	}
}

fn (mut g Gen) is_gui_app() bool {
	match g.pref.subsystem {
		.windows { return true }
		.console { return false }
		.auto {}
	}

	if g.pref.os == .windows {
		if g.force_main_console {
			return false
		}
		for cf in g.table.cflags {
			if cf.value.to_lower_ascii() == 'gdi32' {
				return true
			}
		}
	}
	return false
}

fn (g &Gen) fileis(s string) bool {
	return g.file.path.contains(s)
}

fn (mut g Gen) write_fn_attrs(attrs []ast.Attr) string {
	mut fn_attrs := ''
	for attr in attrs {
		match attr.name {
			'inline' {
				g.write('inline ')
			}
			'noinline' {
				// since these are supported by GCC, clang and MSVC, we can consider them officially supported.
				g.write('__NOINLINE ')
			}
			'weak' {
				if attrs.any(it.name == 'export') {
					// only the exported wrapper should be weak; otherwise x86_64-w64-mingw32-gcc complains
					continue
				}
				// a `@[weak]` tag tells the C compiler, that the next declaration will be weak, i.e. when linking,
				// if there is another declaration of a symbol with the same name (a 'strong' one), it should be
				// used instead, *without linker errors about duplicate symbols*.
				g.write('VWEAK ')
			}
			'noreturn' {
				// a `@[noreturn]` tag tells the compiler, that a function
				// *DOES NOT RETURN* to its callsites.
				// See: https://en.cppreference.com/w/c/language/_Noreturn
				// Such functions should have no return type. They can be used
				// in places where `panic(err)` or `exit(0)` can be used.
				// panic/1 and exit/0 themselves will also be marked as
				// `@[noreturn]` soon.
				// These functions should have busy `for{}` loops injected
				// at their end, when they do not end by calling other fns
				// marked by `@[noreturn]`.
				g.write('VNORETURN ')
			}
			'irq_handler' {
				g.write('__IRQHANDLER ')
			}
			'_cold' {
				// GCC/clang attributes
				// prefixed by _ to indicate they're for advanced users only and not really supported by V.
				// source for descriptions: https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#Common-Function-Attributes
				// The cold attribute on functions is used to inform the compiler that the function is unlikely
				// to be executed. The function is optimized for size rather than speed and on many targets it
				// is placed into a special subsection of the text section so all cold functions appear close
				// together, improving code locality of non-cold parts of program.
				g.write('__attribute__((cold)) ')
			}
			'_constructor' {
				// The constructor attribute causes the function to be called automatically before execution
				// enters main ().
				g.write('__attribute__((constructor)) ')
			}
			'_destructor' {
				// The destructor attribute causes the function to be called automatically after main ()
				// completes or exit () is called.
				g.write('__attribute__((destructor)) ')
			}
			'_flatten' {
				// Generally, inlining into a function is limited. For a function marked with this attribute,
				// every call inside this function is inlined, if possible.
				g.write('__attribute__((flatten)) ')
			}
			'_hot' {
				// The hot attribute on a function is used to inform the compiler that the function is a hot
				// spot of the compiled program.
				g.write('__attribute__((hot)) ')
			}
			'_malloc' {
				// This tells the compiler that a function is malloc-like, i.e., that the pointer P returned by
				// the function cannot alias any other pointer valid when the function returns, and moreover no
				// pointers to valid objects occur in any storage addressed by P.
				g.write('__attribute__((malloc)) ')
			}
			'_pure' {
				// Calls to functions whose return value is not affected by changes to the observable state
				// of the program and that have no observable effects on such state other than to return a
				// value may lend themselves to optimizations such as common subexpression elimination.
				// Declaring such functions with the const attribute allows GCC to avoid emitting some calls in
				// repeated invocations of the function with the same argument values.
				g.write('__attribute__((const)) ')
			}
			'_naked' {
				g.write('__attribute__((naked)) ')
			}
			'windows_stdcall' {
				// windows attributes (msvc/mingw)
				// prefixed by windows to indicate they're for advanced users only and not really supported by V.
				fn_attrs += call_convention_attribute('stdcall')
			}
			'_fastcall' {
				fn_attrs += call_convention_attribute('fastcall')
			}
			'callconv' {
				fn_attrs += call_convention_attribute(attr.arg)
			}
			'console' {
				g.force_main_console = true
			}
			else {
				// nothing but keep V happy
			}
		}
	}
	return fn_attrs
}

fn call_convention_attribute(cconvention string) string {
	return 'VCALLCONV(${cconvention}) '
}

fn (mut g Gen) write_fntype_decl(fn_name string, info ast.FnType, nr_muls int) {
	ret_styp := g.styp(info.func.return_type)
	mut call_conv_attr := ''
	for attr in info.func.attrs {
		match attr.name {
			'callconv' {
				call_conv_attr = call_convention_attribute(attr.arg)
			}
			else {}
		}
	}
	g.write('${ret_styp} (${call_conv_attr}${'*'.repeat(nr_muls + 1)}${fn_name}) (')
	def_pos := g.definitions.len
	g.fn_decl_params(info.func.params, unsafe { nil }, false, false)
	g.definitions.go_back(g.definitions.len - def_pos)
	g.write(')')
}
