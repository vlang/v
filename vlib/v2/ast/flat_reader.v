// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v2.token

// to_files materializes a FlatAst back into legacy []File. This is the
// inverse of flatten_files() and is used for round-trip verification.
// Reading happens lazily (one node at a time) so callers can also walk a
// flat AST without ever rehydrating the legacy form.
pub fn (flat &FlatAst) to_files() []File {
	r := FlatReader{
		flat: unsafe { flat }
	}
	mut files := []File{cap: flat.files.len}
	for ff in flat.files {
		files << r.read_file(ff)
	}
	return files
}

// to_files_range rehydrates the FlatFiles in [start, end) without touching
// the rest of the FlatAst. Used by the streaming builder, where each
// parse_batch appends to a shared FlatBuilder and only needs to return the
// freshly added files to the caller.
pub fn (flat &FlatAst) to_files_range(start int, end int) []File {
	mut lo := start
	mut hi := end
	if lo < 0 {
		lo = 0
	}
	if hi > flat.files.len {
		hi = flat.files.len
	}
	if hi <= lo {
		return []File{}
	}
	r := FlatReader{
		flat: unsafe { flat }
	}
	mut files := []File{cap: hi - lo}
	for i in lo .. hi {
		files << r.read_file(flat.files[i])
	}
	return files
}

// FlatReader is a thin cursor over a FlatAst that reconstructs legacy nodes.
struct FlatReader {
	flat &FlatAst
}

@[inline]
fn (r &FlatReader) node(id FlatNodeId) FlatNode {
	return r.flat.nodes[id]
}

@[inline]
fn (r &FlatReader) edge(n FlatNode, i int) FlatNodeId {
	return r.flat.edges[n.first_edge + i].child_id
}

@[inline]
fn (r &FlatReader) get_str(idx int) string {
	if idx < 0 || idx >= r.flat.strings.len {
		return ''
	}
	return r.flat.strings[idx]
}

// list_children returns the child ids of an aux_list node.
fn (r &FlatReader) list_children(id FlatNodeId) []FlatNodeId {
	if id < 0 {
		return []FlatNodeId{}
	}
	n := r.node(id)
	mut out := []FlatNodeId{cap: n.edge_count}
	for i in 0 .. n.edge_count {
		out << r.edge(n, i)
	}
	return out
}

fn (r &FlatReader) read_file(ff FlatFile) File {
	n := r.node(ff.file_id)
	attrs_id := r.edge(n, 0)
	imports_id := r.edge(n, 1)
	stmts_id := r.edge(n, 2)
	mut attrs := []Attribute{}
	for cid in r.list_children(attrs_id) {
		attrs << r.read_attribute(cid)
	}
	mut imports := []ImportStmt{}
	for cid in r.list_children(imports_id) {
		s := r.read_stmt(cid)
		if s is ImportStmt {
			imports << s
		}
	}
	mut stmts := []Stmt{}
	for cid in r.list_children(stmts_id) {
		stmts << r.read_stmt(cid)
	}
	return File{
		name:           r.get_str(ff.name_idx)
		mod:            r.get_str(ff.mod_idx)
		selector_names: ff.selector_names
		attributes:     attrs
		imports:        imports
		stmts:          stmts
	}
}

// read_file_imports rehydrates only the static `imports` list of a file
// without materializing the full File. Consumers that still need to walk
// comptime-conditional imports can combine this with read_file_stmts.
pub fn (flat &FlatAst) read_file_imports(ff FlatFile) []ImportStmt {
	r := FlatReader{
		flat: unsafe { flat }
	}
	n := r.node(ff.file_id)
	imports_id := r.edge(n, 1)
	mut imports := []ImportStmt{}
	for cid in r.list_children(imports_id) {
		s := r.read_stmt(cid)
		if s is ImportStmt {
			imports << s
		}
	}
	return imports
}

// read_file_stmts rehydrates only the top-level statements of a file.
// Used by consumers that walk file.stmts for comptime-conditional imports
// or other top-level analysis without needing attrs/imports/selector_names.
pub fn (flat &FlatAst) read_file_stmts(ff FlatFile) []Stmt {
	r := FlatReader{
		flat: unsafe { flat }
	}
	n := r.node(ff.file_id)
	stmts_id := r.edge(n, 2)
	mut stmts := []Stmt{}
	for cid in r.list_children(stmts_id) {
		stmts << r.read_stmt(cid)
	}
	return stmts
}

// decode_stmt rehydrates a single legacy ast.Stmt from a FlatNodeId. Useful
// for cursor-driven consumers that walk the flat graph via Cursor and only
// need to materialize a typed Stmt for the specific decls they hand to
// helpers still keyed on legacy ADTs. Returns empty_stmt for invalid ids.
pub fn (flat &FlatAst) decode_stmt(id FlatNodeId) Stmt {
	r := FlatReader{
		flat: unsafe { flat }
	}
	return r.read_stmt(id)
}

// decode_expr is the Expr analogue of decode_stmt.
pub fn (flat &FlatAst) decode_expr(id FlatNodeId) Expr {
	r := FlatReader{
		flat: unsafe { flat }
	}
	return r.read_expr(id)
}

// decode_fn_decl_signature rehydrates a FnDecl with `stmts = []` — the body
// is left empty. Callers that walk the body via Cursor (e.g. markused) can
// use this to avoid the per-fn body decode, which dominates collect_defs
// time in flat mode. Returns an empty FnDecl for ids that don't point at a
// stmt_fn_decl node.
pub fn (flat &FlatAst) decode_fn_decl_signature(id FlatNodeId) FnDecl {
	if id < 0 || id >= flat.nodes.len {
		return FnDecl{}
	}
	n := flat.nodes[id]
	if n.kind != .stmt_fn_decl {
		return FnDecl{}
	}
	r := FlatReader{
		flat: unsafe { flat }
	}
	recv_id := r.edge(n, 0)
	typ_id := r.edge(n, 1)
	attrs_id := r.edge(n, 2)
	fn_typ := r.read_fn_type(typ_id)
	return FnDecl{
		attributes: r.read_attr_list(attrs_id)
		is_public:  (n.flags & flag_is_public) != 0
		is_method:  (n.flags & flag_is_method) != 0
		is_static:  (n.flags & flag_is_static) != 0
		receiver:   r.read_parameter(recv_id)
		language:   unsafe { Language(int(n.aux)) }
		name:       r.get_str(n.name_id)
		typ:        fn_typ
		pos:        n.pos
	}
}

// file_mod returns the module name for a FlatFile via the interned strings.
@[inline]
pub fn (flat &FlatAst) file_mod(ff FlatFile) string {
	if ff.mod_idx < 0 || ff.mod_idx >= flat.strings.len {
		return ''
	}
	return flat.strings[ff.mod_idx]
}

// file_name returns the source filename for a FlatFile.
@[inline]
pub fn (flat &FlatAst) file_name(ff FlatFile) string {
	if ff.name_idx < 0 || ff.name_idx >= flat.strings.len {
		return ''
	}
	return flat.strings[ff.name_idx]
}

fn (r &FlatReader) read_attribute(id FlatNodeId) Attribute {
	n := r.node(id)
	return Attribute{
		name:          r.get_str(n.name_id)
		value:         r.read_expr(r.edge(n, 0))
		comptime_cond: r.read_expr(r.edge(n, 1))
	}
}

fn (r &FlatReader) read_field_init(id FlatNodeId) FieldInit {
	n := r.node(id)
	return FieldInit{
		name:  r.get_str(n.name_id)
		value: r.read_expr(r.edge(n, 0))
	}
}

fn (r &FlatReader) read_field_decl(id FlatNodeId) FieldDecl {
	n := r.node(id)
	attrs_id := r.edge(n, 2)
	mut attrs := []Attribute{}
	for cid in r.list_children(attrs_id) {
		attrs << r.read_attribute(cid)
	}
	return FieldDecl{
		name:                r.get_str(n.name_id)
		typ:                 r.read_expr(r.edge(n, 0))
		value:               r.read_expr(r.edge(n, 1))
		attributes:          attrs
		is_public:           (n.flags & flag_is_public) != 0
		is_mut:              (n.flags & flag_is_mut) != 0
		is_module_mut:       (n.flags & flag_field_is_module_mut) != 0
		is_interface_method: (n.flags & flag_field_is_interface_method) != 0
	}
}

fn (r &FlatReader) read_parameter(id FlatNodeId) Parameter {
	n := r.node(id)
	return Parameter{
		name:   r.get_str(n.name_id)
		typ:    r.read_expr(r.edge(n, 0))
		is_mut: (n.flags & flag_is_mut) != 0
		pos:    n.pos
	}
}

fn (r &FlatReader) read_match_branch(id FlatNodeId) MatchBranch {
	n := r.node(id)
	cond_id := r.edge(n, 0)
	stmts_id := r.edge(n, 1)
	mut cond := []Expr{}
	for cid in r.list_children(cond_id) {
		cond << r.read_expr(cid)
	}
	mut stmts := []Stmt{}
	for cid in r.list_children(stmts_id) {
		stmts << r.read_stmt(cid)
	}
	return MatchBranch{
		cond:  cond
		stmts: stmts
		pos:   n.pos
	}
}

fn (r &FlatReader) read_string_inter(id FlatNodeId) StringInter {
	n := r.node(id)
	mut width := 0
	mut precision := 0
	if n.edge_count >= 4 {
		width = r.read_int(r.edge(n, 2))
		precision = r.read_int(r.edge(n, 3))
	} else {
		packed := n.extra
		width = (packed >> 16) & 0xFFFF
		// sign-extend the low 16 bits to recover negative precision values
		precision = packed & 0xFFFF
		if precision & 0x8000 != 0 {
			precision |= ~0xFFFF
		}
		// sign-extend width as well
		if width & 0x8000 != 0 {
			width |= ~0xFFFF
		}
	}
	return StringInter{
		format:       unsafe { StringInterFormat(int(n.aux)) }
		width:        width
		precision:    precision
		expr:         r.read_expr(r.edge(n, 0))
		format_expr:  r.read_expr(r.edge(n, 1))
		resolved_fmt: r.get_str(n.name_id)
	}
}

fn (r &FlatReader) read_int(id FlatNodeId) int {
	return r.node(id).extra
}

fn (r &FlatReader) read_expr_list(id FlatNodeId) []Expr {
	mut out := []Expr{}
	for cid in r.list_children(id) {
		out << r.read_expr(cid)
	}
	return out
}

fn (r &FlatReader) read_stmt_list(id FlatNodeId) []Stmt {
	mut out := []Stmt{}
	for cid in r.list_children(id) {
		out << r.read_stmt(cid)
	}
	return out
}

fn (r &FlatReader) read_attr_list(id FlatNodeId) []Attribute {
	mut out := []Attribute{}
	for cid in r.list_children(id) {
		out << r.read_attribute(cid)
	}
	return out
}

fn (r &FlatReader) read_field_decl_list(id FlatNodeId) []FieldDecl {
	mut out := []FieldDecl{}
	for cid in r.list_children(id) {
		out << r.read_field_decl(cid)
	}
	return out
}

fn (r &FlatReader) read_field_init_list(id FlatNodeId) []FieldInit {
	mut out := []FieldInit{}
	for cid in r.list_children(id) {
		out << r.read_field_init(cid)
	}
	return out
}

fn (r &FlatReader) read_parameter_list(id FlatNodeId) []Parameter {
	mut out := []Parameter{}
	for cid in r.list_children(id) {
		out << r.read_parameter(cid)
	}
	return out
}

// read_fn_type (s252) decodes a `.typ_fn` node straight into a FnType, without
// the read_type→`Type(FnType{...})`→`is FnType` round-trip. Boxing a large
// struct into the Type sum type and then unboxing it via smartcast corrupts the
// FnType's slice headers (generic_params/params) on the arm64 self-host (the
// documented chained-access/smartcast bug). This path is only ever exercised by
// the flat decode, so the default self-host never hit it. Edge layout matches
// the encoder's FnType arm and read_type's `.typ_fn`: 0=generics, 1=params,
// 2=return_type.
fn (r &FlatReader) read_fn_type(id FlatNodeId) FnType {
	if id < 0 {
		return FnType{}
	}
	n := r.node(id)
	if n.kind != .typ_fn {
		return FnType{}
	}
	return FnType{
		generic_params: r.read_expr_list(r.edge(n, 0))
		params:         r.read_parameter_list(r.edge(n, 1))
		return_type:    r.read_expr(r.edge(n, 2))
	}
}

// read_ident (s254) decodes an expr_ident node straight into an Ident, avoiding
// the read_expr→Expr→`is Ident` smartcast-unbox. Copying a struct out of the Expr
// sum type via smartcast corrupts its fields (here the `name` string header) on
// the arm64 self-host (same chained-access/smartcast bug as the FnType unbox in
// s252). Returns an empty Ident for ids that don't point at an expr_ident.
fn (r &FlatReader) read_ident(id FlatNodeId) Ident {
	if id < 0 || id >= r.flat.nodes.len {
		return Ident{}
	}
	n := r.node(id)
	if n.kind != .expr_ident {
		return Ident{}
	}
	return Ident{
		pos:  n.pos
		name: r.get_str(n.name_id)
	}
}

// read_assign_stmt (s254) decodes a stmt_assign node straight into an AssignStmt,
// avoiding the read_stmt→Stmt→`is AssignStmt` unbox (which corrupts the lhs/rhs
// slice headers on arm64). Mirrors the `.stmt_assign` arm of read_stmt. Returns
// an empty AssignStmt for ids that don't point at a stmt_assign.
fn (r &FlatReader) read_assign_stmt(id FlatNodeId) AssignStmt {
	if id < 0 || id >= r.flat.nodes.len {
		return AssignStmt{}
	}
	n := r.node(id)
	if n.kind != .stmt_assign {
		return AssignStmt{}
	}
	lhs_len := n.extra
	mut lhs := []Expr{cap: lhs_len}
	for i in 0 .. lhs_len {
		lhs << r.read_expr(r.edge(n, i))
	}
	mut rhs := []Expr{cap: n.edge_count - lhs_len}
	for i in lhs_len .. n.edge_count {
		rhs << r.read_expr(r.edge(n, i))
	}
	return AssignStmt{
		op:  unsafe { token.Token(int(n.aux)) }
		lhs: lhs
		rhs: rhs
		pos: n.pos
	}
}

fn (r &FlatReader) read_string_list(id FlatNodeId) []string {
	if id < 0 {
		return []string{}
	}
	n := r.node(id)
	mut out := []string{cap: n.edge_count}
	for i in 0 .. n.edge_count {
		child := r.node(r.edge(n, i))
		out << r.get_str(child.name_id)
	}
	return out
}

fn (r &FlatReader) read_string_inter_list(id FlatNodeId) []StringInter {
	mut out := []StringInter{}
	for cid in r.list_children(id) {
		out << r.read_string_inter(cid)
	}
	return out
}

fn (r &FlatReader) read_stmt(id FlatNodeId) Stmt {
	if id < 0 {
		return empty_stmt
	}
	n := r.node(id)
	match n.kind {
		.stmt_asm {
			return Stmt(AsmStmt{
				arch: r.get_str(n.name_id)
			})
		}
		.stmt_assert {
			return Stmt(AssertStmt{
				expr:  r.read_expr(r.edge(n, 0))
				extra: r.read_expr(r.edge(n, 1))
			})
		}
		.stmt_assign {
			lhs_len := n.extra
			mut lhs := []Expr{cap: lhs_len}
			for i in 0 .. lhs_len {
				lhs << r.read_expr(r.edge(n, i))
			}
			mut rhs := []Expr{cap: n.edge_count - lhs_len}
			for i in lhs_len .. n.edge_count {
				rhs << r.read_expr(r.edge(n, i))
			}
			return Stmt(AssignStmt{
				op:  unsafe { token.Token(int(n.aux)) }
				lhs: lhs
				rhs: rhs
				pos: n.pos
			})
		}
		.stmt_block {
			mut stmts := []Stmt{cap: n.edge_count}
			for i in 0 .. n.edge_count {
				stmts << r.read_stmt(r.edge(n, i))
			}
			return Stmt(BlockStmt{
				stmts: stmts
			})
		}
		.stmt_comptime {
			return Stmt(ComptimeStmt{
				stmt: r.read_stmt(r.edge(n, 0))
			})
		}
		.stmt_const_decl {
			fields_id := r.edge(n, 0)
			return Stmt(ConstDecl{
				is_public: (n.flags & flag_is_public) != 0
				fields:    r.read_field_init_list(fields_id)
			})
		}
		.stmt_defer {
			mode := if (n.flags & flag_defer_func) != 0 {
				DeferMode.function
			} else {
				DeferMode.scoped
			}
			mut stmts := []Stmt{cap: n.edge_count}
			for i in 0 .. n.edge_count {
				stmts << r.read_stmt(r.edge(n, i))
			}
			return Stmt(DeferStmt{
				mode:  mode
				stmts: stmts
			})
		}
		.stmt_directive {
			mut ct_cond := ''
			if n.edge_count > 0 {
				cc_node := r.node(r.edge(n, 0))
				ct_cond = r.get_str(cc_node.name_id)
			}
			return Stmt(Directive{
				name:    r.get_str(n.name_id)
				value:   r.get_str(n.extra)
				ct_cond: ct_cond
			})
		}
		.stmt_empty {
			return Stmt(EmptyStmt(u8(n.extra)))
		}
		.stmt_enum_decl {
			as_type := r.read_expr(r.edge(n, 0))
			attrs_id := r.edge(n, 1)
			fields_id := r.edge(n, 2)
			return Stmt(EnumDecl{
				is_public:  (n.flags & flag_is_public) != 0
				name:       r.get_str(n.name_id)
				as_type:    as_type
				attributes: r.read_attr_list(attrs_id)
				fields:     r.read_field_decl_list(fields_id)
			})
		}
		.stmt_expr {
			return Stmt(ExprStmt{
				expr: r.read_expr(r.edge(n, 0))
			})
		}
		.stmt_flow_control {
			return Stmt(FlowControlStmt{
				op:    unsafe { token.Token(int(n.aux)) }
				label: r.get_str(n.name_id)
			})
		}
		.stmt_fn_decl {
			recv_id := r.edge(n, 0)
			typ_id := r.edge(n, 1)
			attrs_id := r.edge(n, 2)
			stmts_id := r.edge(n, 3)
			fn_typ := r.read_fn_type(typ_id)
			return Stmt(FnDecl{
				attributes: r.read_attr_list(attrs_id)
				is_public:  (n.flags & flag_is_public) != 0
				is_method:  (n.flags & flag_is_method) != 0
				is_static:  (n.flags & flag_is_static) != 0
				receiver:   r.read_parameter(recv_id)
				language:   unsafe { Language(int(n.aux)) }
				name:       r.get_str(n.name_id)
				typ:        fn_typ
				stmts:      r.read_stmt_list(stmts_id)
				pos:        n.pos
			})
		}
		.stmt_for_in {
			return Stmt(ForInStmt{
				key:   r.read_expr(r.edge(n, 0))
				value: r.read_expr(r.edge(n, 1))
				expr:  r.read_expr(r.edge(n, 2))
			})
		}
		.stmt_for {
			init := r.read_stmt(r.edge(n, 0))
			cond := r.read_expr(r.edge(n, 1))
			post := r.read_stmt(r.edge(n, 2))
			mut stmts := []Stmt{cap: n.edge_count - 3}
			for i in 3 .. n.edge_count {
				stmts << r.read_stmt(r.edge(n, i))
			}
			return Stmt(ForStmt{
				init:  init
				cond:  cond
				post:  post
				stmts: stmts
			})
		}
		.stmt_global_decl {
			attrs_id := r.edge(n, 0)
			fields_id := r.edge(n, 1)
			return Stmt(GlobalDecl{
				attributes: r.read_attr_list(attrs_id)
				fields:     r.read_field_decl_list(fields_id)
				is_public:  (n.flags & flag_is_public) != 0
			})
		}
		.stmt_import {
			mut symbols := []Expr{cap: n.edge_count}
			for i in 0 .. n.edge_count {
				symbols << r.read_expr(r.edge(n, i))
			}
			return Stmt(ImportStmt{
				name:       r.get_str(n.name_id)
				alias:      r.get_str(n.extra)
				is_aliased: (n.flags & flag_is_aliased) != 0
				symbols:    symbols
			})
		}
		.stmt_interface_decl {
			attrs_id := r.edge(n, 0)
			generic_params_id := r.edge(n, 1)
			embedded_id := r.edge(n, 2)
			fields_id := r.edge(n, 3)
			return Stmt(InterfaceDecl{
				is_public:      (n.flags & flag_is_public) != 0
				attributes:     r.read_attr_list(attrs_id)
				name:           r.get_str(n.name_id)
				generic_params: r.read_expr_list(generic_params_id)
				embedded:       r.read_expr_list(embedded_id)
				fields:         r.read_field_decl_list(fields_id)
			})
		}
		.stmt_label {
			return Stmt(LabelStmt{
				name: r.get_str(n.name_id)
				stmt: r.read_stmt(r.edge(n, 0))
			})
		}
		.stmt_module {
			return Stmt(ModuleStmt{
				name: r.get_str(n.name_id)
			})
		}
		.stmt_return {
			mut exprs := []Expr{cap: n.edge_count}
			for i in 0 .. n.edge_count {
				exprs << r.read_expr(r.edge(n, i))
			}
			return Stmt(ReturnStmt{
				exprs: exprs
			})
		}
		.stmt_struct_decl {
			attrs_id := r.edge(n, 0)
			implements_id := r.edge(n, 1)
			embedded_id := r.edge(n, 2)
			generic_params_id := r.edge(n, 3)
			fields_id := r.edge(n, 4)
			return Stmt(StructDecl{
				attributes:     r.read_attr_list(attrs_id)
				is_public:      (n.flags & flag_is_public) != 0
				is_union:       (n.flags & flag_is_union) != 0
				implements:     r.read_expr_list(implements_id)
				embedded:       r.read_expr_list(embedded_id)
				language:       unsafe { Language(int(n.aux)) }
				name:           r.get_str(n.name_id)
				generic_params: r.read_expr_list(generic_params_id)
				fields:         r.read_field_decl_list(fields_id)
				pos:            n.pos
			})
		}
		.stmt_type_decl {
			base_type := r.read_expr(r.edge(n, 0))
			generic_params_id := r.edge(n, 2)
			variants_id := r.edge(n, 3)
			return Stmt(TypeDecl{
				is_public:      (n.flags & flag_is_public) != 0
				language:       unsafe { Language(int(n.aux)) }
				name:           r.get_str(n.name_id)
				generic_params: r.read_expr_list(generic_params_id)
				base_type:      base_type
				variants:       r.read_expr_list(variants_id)
			})
		}
		.stmt_attributes {
			list_id := r.edge(n, 0)
			return Stmt(r.read_attr_list(list_id))
		}
		else {
			return empty_stmt
		}
	}
}

fn (r &FlatReader) read_expr(id FlatNodeId) Expr {
	if id < 0 {
		return empty_expr
	}
	n := r.node(id)
	// Types and aux nodes can appear where an Expr is expected.
	match n.kind {
		.typ_anon_struct, .typ_array_fixed, .typ_array, .typ_channel, .typ_fn, .typ_generic,
		.typ_map, .typ_nil, .typ_none, .typ_option, .typ_pointer, .typ_result, .typ_thread,
		.typ_tuple {
			return Expr(r.read_type(id))
		}
		.aux_field_init {
			return Expr(r.read_field_init(id))
		}
		else {}
	}

	match n.kind {
		.expr_array_init {
			typ := r.read_expr(r.edge(n, 0))
			init := r.read_expr(r.edge(n, 1))
			cap := r.read_expr(r.edge(n, 2))
			len := r.read_expr(r.edge(n, 3))
			update_expr := r.read_expr(r.edge(n, 4))
			mut exprs := []Expr{cap: n.edge_count - 5}
			for i in 5 .. n.edge_count {
				exprs << r.read_expr(r.edge(n, i))
			}
			return Expr(ArrayInitExpr{
				typ:         typ
				exprs:       exprs
				init:        init
				cap:         cap
				len:         len
				update_expr: update_expr
				pos:         n.pos
			})
		}
		.expr_as_cast {
			return Expr(AsCastExpr{
				expr: r.read_expr(r.edge(n, 0))
				typ:  r.read_expr(r.edge(n, 1))
				pos:  n.pos
			})
		}
		.expr_assoc {
			typ := r.read_expr(r.edge(n, 0))
			expr := r.read_expr(r.edge(n, 1))
			mut fields := []FieldInit{cap: n.edge_count - 2}
			for i in 2 .. n.edge_count {
				fields << r.read_field_init(r.edge(n, i))
			}
			return Expr(AssocExpr{
				typ:    typ
				expr:   expr
				fields: fields
				pos:    n.pos
			})
		}
		.expr_basic_literal {
			return Expr(BasicLiteral{
				kind:  unsafe { token.Token(int(n.aux)) }
				value: r.get_str(n.name_id)
				pos:   n.pos
			})
		}
		.expr_call {
			lhs := r.read_expr(r.edge(n, 0))
			mut args := []Expr{cap: n.edge_count - 1}
			for i in 1 .. n.edge_count {
				args << r.read_expr(r.edge(n, i))
			}
			return Expr(CallExpr{
				lhs:  lhs
				args: args
				pos:  n.pos
			})
		}
		.expr_call_or_cast {
			return Expr(CallOrCastExpr{
				lhs:  r.read_expr(r.edge(n, 0))
				expr: r.read_expr(r.edge(n, 1))
				pos:  n.pos
			})
		}
		.expr_cast {
			return Expr(CastExpr{
				typ:  r.read_expr(r.edge(n, 0))
				expr: r.read_expr(r.edge(n, 1))
				pos:  n.pos
			})
		}
		.expr_comptime {
			return Expr(ComptimeExpr{
				expr: r.read_expr(r.edge(n, 0))
				pos:  n.pos
			})
		}
		.expr_empty {
			return Expr(EmptyExpr(u8(n.extra)))
		}
		.expr_fn_literal {
			fn_typ_id := r.edge(n, 0)
			fn_typ := r.read_fn_type(fn_typ_id)
			cap_len := n.extra
			mut captured := []Expr{cap: cap_len}
			for i in 0 .. cap_len {
				captured << r.read_expr(r.edge(n, 1 + i))
			}
			mut stmts := []Stmt{cap: n.edge_count - 1 - cap_len}
			for i in (1 + cap_len) .. n.edge_count {
				stmts << r.read_stmt(r.edge(n, i))
			}
			return Expr(FnLiteral{
				typ:           fn_typ
				captured_vars: captured
				stmts:         stmts
				pos:           n.pos
			})
		}
		.expr_generic_arg_or_index {
			return Expr(GenericArgOrIndexExpr{
				lhs:  r.read_expr(r.edge(n, 0))
				expr: r.read_expr(r.edge(n, 1))
				pos:  n.pos
			})
		}
		.expr_generic_args {
			lhs := r.read_expr(r.edge(n, 0))
			mut args := []Expr{cap: n.edge_count - 1}
			for i in 1 .. n.edge_count {
				args << r.read_expr(r.edge(n, i))
			}
			return Expr(GenericArgs{
				lhs:  lhs
				args: args
				pos:  n.pos
			})
		}
		.expr_ident {
			return Expr(Ident{
				pos:  n.pos
				name: r.get_str(n.name_id)
			})
		}
		.expr_if {
			cond := r.read_expr(r.edge(n, 0))
			else_expr := r.read_expr(r.edge(n, 1))
			mut stmts := []Stmt{cap: n.edge_count - 2}
			for i in 2 .. n.edge_count {
				stmts << r.read_stmt(r.edge(n, i))
			}
			return Expr(IfExpr{
				cond:      cond
				else_expr: else_expr
				stmts:     stmts
				pos:       n.pos
			})
		}
		.expr_if_guard {
			// s254: read the assign straight into an AssignStmt; the old
			// `read_stmt → if child is AssignStmt { child }` unbox corrupted its
			// lhs/rhs slice headers on arm64.
			return Expr(IfGuardExpr{
				stmt: r.read_assign_stmt(r.edge(n, 0))
				pos:  n.pos
			})
		}
		.expr_index {
			return Expr(IndexExpr{
				lhs:      r.read_expr(r.edge(n, 0))
				expr:     r.read_expr(r.edge(n, 1))
				is_gated: (n.flags & flag_is_gated) != 0
				pos:      n.pos
			})
		}
		.expr_infix {
			return Expr(InfixExpr{
				op:  unsafe { token.Token(int(n.aux)) }
				lhs: r.read_expr(r.edge(n, 0))
				rhs: r.read_expr(r.edge(n, 1))
				pos: n.pos
			})
		}
		.expr_init {
			typ := r.read_expr(r.edge(n, 0))
			mut fields := []FieldInit{cap: n.edge_count - 1}
			for i in 1 .. n.edge_count {
				fields << r.read_field_init(r.edge(n, i))
			}
			return Expr(InitExpr{
				typ:    typ
				fields: fields
				pos:    n.pos
			})
		}
		.expr_keyword {
			return Expr(Keyword{
				tok: unsafe { token.Token(int(n.aux)) }
			})
		}
		.expr_keyword_operator {
			mut exprs := []Expr{cap: n.edge_count}
			for i in 0 .. n.edge_count {
				exprs << r.read_expr(r.edge(n, i))
			}
			return Expr(KeywordOperator{
				op:    unsafe { token.Token(int(n.aux)) }
				exprs: exprs
				pos:   n.pos
			})
		}
		.expr_lambda {
			expr := r.read_expr(r.edge(n, 0))
			mut args := []Ident{cap: n.edge_count - 1}
			for i in 1 .. n.edge_count {
				// s254: read each arg straight into an Ident (the old
				// `read_expr → if e is Ident { args << e }` unbox corrupted the
				// Ident's `name` on arm64).
				arg_id := r.edge(n, i)
				if arg_id >= 0 && arg_id < r.flat.nodes.len && r.node(arg_id).kind == .expr_ident {
					args << r.read_ident(arg_id)
				}
			}
			return Expr(LambdaExpr{
				args: args
				expr: expr
				pos:  n.pos
			})
		}
		.expr_lifetime {
			return Expr(LifetimeExpr{
				name: r.get_str(n.name_id)
				pos:  n.pos
			})
		}
		.expr_lock {
			packed := u32(n.extra)
			lock_len := int(packed & 0xFFFF)
			rlock_len := int((packed >> 16) & 0xFFFF)
			mut lock_exprs := []Expr{cap: lock_len}
			for i in 0 .. lock_len {
				lock_exprs << r.read_expr(r.edge(n, i))
			}
			mut rlock_exprs := []Expr{cap: rlock_len}
			for i in lock_len .. (lock_len + rlock_len) {
				rlock_exprs << r.read_expr(r.edge(n, i))
			}
			mut stmts := []Stmt{cap: n.edge_count - lock_len - rlock_len}
			for i in (lock_len + rlock_len) .. n.edge_count {
				stmts << r.read_stmt(r.edge(n, i))
			}
			return Expr(LockExpr{
				lock_exprs:  lock_exprs
				rlock_exprs: rlock_exprs
				stmts:       stmts
				pos:         n.pos
			})
		}
		.expr_map_init {
			typ := r.read_expr(r.edge(n, 0))
			keys_len := n.extra
			mut keys := []Expr{cap: keys_len}
			for i in 0 .. keys_len {
				keys << r.read_expr(r.edge(n, 1 + i))
			}
			mut vals := []Expr{cap: n.edge_count - 1 - keys_len}
			for i in (1 + keys_len) .. n.edge_count {
				vals << r.read_expr(r.edge(n, i))
			}
			return Expr(MapInitExpr{
				typ:  typ
				keys: keys
				vals: vals
				pos:  n.pos
			})
		}
		.expr_match {
			expr := r.read_expr(r.edge(n, 0))
			mut branches := []MatchBranch{cap: n.edge_count - 1}
			for i in 1 .. n.edge_count {
				branches << r.read_match_branch(r.edge(n, i))
			}
			return Expr(MatchExpr{
				expr:     expr
				branches: branches
				pos:      n.pos
			})
		}
		.expr_modifier {
			return Expr(ModifierExpr{
				kind: unsafe { token.Token(int(n.aux)) }
				expr: r.read_expr(r.edge(n, 0))
				pos:  n.pos
			})
		}
		.expr_or {
			expr := r.read_expr(r.edge(n, 0))
			mut stmts := []Stmt{cap: n.edge_count - 1}
			for i in 1 .. n.edge_count {
				stmts << r.read_stmt(r.edge(n, i))
			}
			return Expr(OrExpr{
				expr:  expr
				stmts: stmts
				pos:   n.pos
			})
		}
		.expr_paren {
			return Expr(ParenExpr{
				expr: r.read_expr(r.edge(n, 0))
				pos:  n.pos
			})
		}
		.expr_postfix {
			return Expr(PostfixExpr{
				op:   unsafe { token.Token(int(n.aux)) }
				expr: r.read_expr(r.edge(n, 0))
				pos:  n.pos
			})
		}
		.expr_prefix {
			return Expr(PrefixExpr{
				op:   unsafe { token.Token(int(n.aux)) }
				expr: r.read_expr(r.edge(n, 0))
				pos:  n.pos
			})
		}
		.expr_range {
			return Expr(RangeExpr{
				op:    unsafe { token.Token(int(n.aux)) }
				start: r.read_expr(r.edge(n, 0))
				end:   r.read_expr(r.edge(n, 1))
				pos:   n.pos
			})
		}
		.expr_select {
			stmt := r.read_stmt(r.edge(n, 0))
			next := r.read_expr(r.edge(n, 1))
			mut stmts := []Stmt{cap: n.edge_count - 2}
			for i in 2 .. n.edge_count {
				stmts << r.read_stmt(r.edge(n, i))
			}
			return Expr(SelectExpr{
				pos:   n.pos
				stmt:  stmt
				stmts: stmts
				next:  next
			})
		}
		.expr_selector {
			// s254: read the rhs straight into an Ident; the previous
			// `read_expr → if rhs is Ident { rhs }` unbox corrupted the Ident's
			// `name` on the arm64 self-host, yielding garbage `missing X.<name>`
			// checker errors for selector types like `strings.Builder`.
			return Expr(SelectorExpr{
				lhs: r.read_expr(r.edge(n, 0))
				rhs: r.read_ident(r.edge(n, 1))
				pos: n.pos
			})
		}
		.expr_sql {
			return Expr(SqlExpr{
				expr:       r.read_expr(r.edge(n, 0))
				table_name: r.get_str(n.name_id)
				is_count:   (n.flags & flag_is_count) != 0
				is_create:  (n.flags & flag_is_create) != 0
				pos:        n.pos
			})
		}
		.expr_string_inter {
			values_id := r.edge(n, 0)
			inters_id := r.edge(n, 1)
			return Expr(StringInterLiteral{
				kind:   unsafe { StringLiteralKind(int(n.aux)) }
				values: r.read_string_list(values_id)
				inters: r.read_string_inter_list(inters_id)
				pos:    n.pos
			})
		}
		.expr_string {
			return Expr(StringLiteral{
				kind:  unsafe { StringLiteralKind(int(n.aux)) }
				value: r.get_str(n.name_id)
				pos:   n.pos
			})
		}
		.expr_tuple {
			mut exprs := []Expr{cap: n.edge_count}
			for i in 0 .. n.edge_count {
				exprs << r.read_expr(r.edge(n, i))
			}
			return Expr(Tuple{
				exprs: exprs
				pos:   n.pos
			})
		}
		.expr_unsafe {
			mut stmts := []Stmt{cap: n.edge_count}
			for i in 0 .. n.edge_count {
				stmts << r.read_stmt(r.edge(n, i))
			}
			return Expr(UnsafeExpr{
				stmts: stmts
				pos:   n.pos
			})
		}
		else {
			return empty_expr
		}
	}
}

fn (r &FlatReader) read_type(id FlatNodeId) Type {
	if id < 0 {
		return Type(NilType{})
	}
	n := r.node(id)
	match n.kind {
		.typ_anon_struct {
			generic_params_id := r.edge(n, 0)
			embedded_id := r.edge(n, 1)
			fields_id := r.edge(n, 2)
			return Type(AnonStructType{
				generic_params: r.read_expr_list(generic_params_id)
				embedded:       r.read_expr_list(embedded_id)
				fields:         r.read_field_decl_list(fields_id)
			})
		}
		.typ_array_fixed {
			return Type(ArrayFixedType{
				len:       r.read_expr(r.edge(n, 0))
				elem_type: r.read_expr(r.edge(n, 1))
			})
		}
		.typ_array {
			return Type(ArrayType{
				elem_type: r.read_expr(r.edge(n, 0))
			})
		}
		.typ_channel {
			return Type(ChannelType{
				cap:       r.read_expr(r.edge(n, 0))
				elem_type: r.read_expr(r.edge(n, 1))
			})
		}
		.typ_fn {
			generic_params_id := r.edge(n, 0)
			params_id := r.edge(n, 1)
			return Type(FnType{
				generic_params: r.read_expr_list(generic_params_id)
				params:         r.read_parameter_list(params_id)
				return_type:    r.read_expr(r.edge(n, 2))
			})
		}
		.typ_generic {
			name := r.read_expr(r.edge(n, 0))
			mut params := []Expr{cap: n.edge_count - 1}
			for i in 1 .. n.edge_count {
				params << r.read_expr(r.edge(n, i))
			}
			return Type(GenericType{
				name:   name
				params: params
			})
		}
		.typ_map {
			return Type(MapType{
				key_type:   r.read_expr(r.edge(n, 0))
				value_type: r.read_expr(r.edge(n, 1))
			})
		}
		.typ_nil {
			return Type(NilType{})
		}
		.typ_none {
			return Type(NoneType{})
		}
		.typ_option {
			return Type(OptionType{
				base_type: r.read_expr(r.edge(n, 0))
			})
		}
		.typ_pointer {
			return Type(PointerType{
				base_type: r.read_expr(r.edge(n, 0))
				lifetime:  r.get_str(n.name_id)
			})
		}
		.typ_result {
			return Type(ResultType{
				base_type: r.read_expr(r.edge(n, 0))
			})
		}
		.typ_thread {
			return Type(ThreadType{
				elem_type: r.read_expr(r.edge(n, 0))
			})
		}
		.typ_tuple {
			mut types := []Expr{cap: n.edge_count}
			for i in 0 .. n.edge_count {
				types << r.read_expr(r.edge(n, i))
			}
			return Type(TupleType{
				types: types
			})
		}
		else {
			return Type(NilType{})
		}
	}
}
