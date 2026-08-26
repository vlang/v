// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// V source formatter (vfmt) for the v3 flat AST.
//
// This is a port of the original v2 `gen/v` tree-AST formatter to v3's flat,
// node-array AST (see vlib/v3/flat/flat.v). It walks the nodes produced by
// vlib/v3/parser and reconstructs V source. Formatting is purely syntactic:
// every type is stored on a node as a `typ` string, so no type checker is
// required.
//
// Formatter-mode parsing retains source-only data that compiler backends do not
// need: comments, unfurled compile-time branches, opaque asm/SQL bodies, and
// literal prefixes. This keeps formatting syntax-preserving without type checking.
module v

import os
import strings
import v3.flat
import v3.pref
import v3.scanner
import v3.token

// Gen holds the formatter state for one output buffer.
pub struct Gen {
mut:
	a               &flat.FlatAst = unsafe { nil }
	out             strings.Builder
	indent          int
	on_newline      bool
	in_init         bool
	file_id         int
	source          string
	comments        []flat.Comment
	comment_i       int
	source_end      int = -1
	migrate_json2   bool
	json_qualifier  string
	json_import_id  int = -1
	skip_decls      map[int]bool
	selective_json  bool
	implied_imports []string
	in_array_init   bool
	is_debug        bool
	// suppress_mut skips the `mut ` prefix on an assignment (used for C-style
	// `for` loop init clauses, whose variable the parser always marks mutable).
	suppress_mut bool
	// attrs maps a declaration node id to its `@[...]` attribute strings. The
	// parser stores attributes on a separate floating `.directive` node rather
	// than as a child, so they are collected up-front in collect_attrs.
	attrs map[int][]string
}

// FormatOptions controls optional formatter output.
pub struct FormatOptions {
pub:
	is_debug bool
}

// Gen.new returns a fresh formatter.
pub fn Gen.new() &Gen {
	return &Gen{
		out:    strings.new_builder(1000)
		indent: -1
	}
}

// reset clears the buffer so a Gen instance can be reused.
pub fn (mut g Gen) reset() {
	g.out.go_back_to(0)
	g.indent = -1
	g.on_newline = false
	g.in_init = false
	g.file_id = 0
	g.source = ''
	g.comments.clear()
	g.comment_i = 0
	g.source_end = -1
	g.migrate_json2 = false
	g.json_qualifier = ''
	g.json_import_id = -1
	g.skip_decls = map[int]bool{}
	g.selective_json = false
	g.implied_imports.clear()
	g.in_array_init = false
}

// format_file parses-independent convenience: format the file whose trailing
// `.file` node id is `file_id` within `a`.
pub fn format_file(a &flat.FlatAst, file_id flat.NodeId) string {
	mut g := Gen.new()
	return g.gen_file(a, file_id)
}

// format finds every trailing `.file` node in `a` and formats them in order,
// separated by a blank line. Useful when a single file was parsed on its own.
pub fn format(a &flat.FlatAst) string {
	return format_with_options(a, FormatOptions{})
}

// format_with_options formats every trailing file node in `a` with `options`.
pub fn format_with_options(a &flat.FlatAst, options FormatOptions) string {
	mut g := Gen.new()
	g.is_debug = options.is_debug
	mut out := strings.new_builder(1000)
	mut first := true
	for id in a.file_node_ids {
		n := a.node(flat.NodeId(id))
		if n.kind == .file && n.children_count > 0 {
			if !first {
				out.writeln('')
			}
			out.write_string(g.gen_file(a, flat.NodeId(id)))
			first = false
		}
	}
	return out.str()
}

// gen_file formats the top-level declarations of the given trailing file node.
pub fn (mut g Gen) gen_file(a &flat.FlatAst, file_id flat.NodeId) string {
	g.reset()
	g.a = a
	g.collect_attrs()
	fnode := a.node(file_id)
	g.file_id = fnode.pos.id
	g.source = a.formatter_file_sources[g.file_id] or { '' }
	for comment in a.comments {
		if comment.pos.id == g.file_id {
			g.comments << comment
		}
	}
	g.setup_json_migration(fnode)
	g.collect_implied_imports(fnode)
	g.top_level(a.children_of(fnode))
	g.emit_comments_before(fnode.pos.end + 1)
	formatted := g.out.str()
	if g.comments.any(it.text.trim_space().starts_with('// vfmt off')) {
		return restore_vfmt_disabled_regions(formatted, g.source.replace('\r\n', '\n'))
	}
	return formatted
}

// output_string returns the generated V source code.
pub fn (mut g Gen) output_string() string {
	return g.out.str()
}

// collect_attrs indexes every floating attribute directive by the declaration
// node id it annotates.
fn (mut g Gen) collect_attrs() {
	g.attrs = map[int][]string{}
	for n in g.a.nodes {
		if n.kind == .directive && n.value.starts_with('@attributes:') {
			decl_id := n.value.all_after('@attributes:').int()
			g.attrs[decl_id] = n.generic_params()
		}
	}
}

// setup_json_migration enables the formatter's conservative json-to-json2 rewrite.
// Migration is all-or-nothing for a file: if a qualifier can be shadowed, an import
// lives in a conditional branch, a call is used as a value, or a comment would move,
// the legacy source is retained unchanged.
fn (mut g Gen) setup_json_migration(fnode &flat.Node) {
	if !g.a.formatter_migrate_json2
		|| g.source.split_into_lines().any(it.trim_space().starts_with('import json //'))
		|| g.source.contains('json.decode( //') {
		return
	}
	direct_ids := g.a.children_of(fnode)
	mut direct := map[int]bool{}
	for id in direct_ids {
		direct[int(id)] = true
		if g.a.node(id).kind == .module_decl && g.a.node(id).value == 'json2' {
			return
		}
	}
	mut legacy_imports := []flat.NodeId{}
	mut json2_imports := []flat.NodeId{}
	mut called := map[int]bool{}
	mut module_receivers := map[int]bool{}
	for i, n in g.a.nodes {
		if n.pos.id != g.file_id {
			continue
		}
		if n.kind == .call && n.children_count > 0 {
			called[int(g.a.child(n, 0))] = true
		}
		if n.kind == .selector && n.children_count > 0 {
			receiver := g.a.child(n, 0)
			if g.a.node(receiver).kind == .ident && g.a.node(receiver).value in ['json', 'json2'] {
				module_receivers[int(receiver)] = true
			}
		}
		if n.kind == .import_decl {
			if n.value == 'json' {
				legacy_imports << flat.NodeId(i)
			} else if n.value == 'json2' {
				json2_imports << flat.NodeId(i)
			} else if n.typ == 'json2' {
				return
			}
		}
	}
	if legacy_imports.len != 1 {
		return
	}
	legacy_id := legacy_imports[0]
	legacy := g.a.node(legacy_id)
	if !direct[int(legacy_id)] || (legacy.typ.len > 0 && legacy.typ != 'json') {
		return
	}
	for id in json2_imports {
		if !direct[int(id)] {
			return
		}
	}
	if json2_imports.len > 1 {
		return
	}
	mut qualifier := 'json2'
	if json2_imports.len == 1 {
		existing := g.a.node(json2_imports[0])
		if existing.typ == '_' || g.comment_on_same_line_after(legacy.pos.offset, legacy.pos.end) {
			return
		}
		if existing.typ.len > 0 && existing.typ != 'json2' {
			qualifier = existing.typ
		}
	}
	for i, n in g.a.nodes {
		if n.pos.id != g.file_id {
			continue
		}
		if n.kind == .ident && n.value == 'json2' && !module_receivers[i] {
			return
		}
		if n.kind == .param && n.value == qualifier {
			return
		}
		if n.kind == .fn_decl && n.value in ['encode', 'decode', 'encode_pretty']
			&& legacy.children_count > 0 {
			return
		}
		if n.kind == .field_decl && n.value == n.typ && n.children_count == 0 {
			return
		}
		if n.kind == .selector && n.children_count > 0 {
			receiver := g.a.child_node(n, 0)
			if receiver.kind == .ident && receiver.value == 'json' {
				if n.value !in ['encode', 'decode', 'encode_pretty'] || !called[i] {
					return
				}
				if n.value == 'decode' && g.comments_inside(n.pos.offset, n.pos.end) {
					return
				}
			}
		}
	}
	g.migrate_json2 = true
	g.json_qualifier = qualifier
	g.json_import_id = int(legacy_id)
	g.selective_json = legacy.children_count > 0
	if json2_imports.len == 1 {
		g.skip_decls[int(legacy_id)] = true
	}
}

fn (g &Gen) comments_inside(start int, end int) bool {
	for comment in g.comments {
		if comment.pos.offset > start && comment.pos.offset < end {
			return true
		}
	}
	return false
}

fn (g &Gen) comment_on_same_line_after(start int, end int) bool {
	line := g.source_line(start)
	for comment in g.comments {
		if comment.pos.offset >= end && g.source_line(comment.pos.offset) == line {
			return true
		}
	}
	return false
}

// top_level renders the file's top-level declarations, inserting a blank line
// between them (but keeping consecutive imports grouped).
fn (mut g Gen) top_level(ids []flat.NodeId) {
	mut decls := []flat.NodeId{}
	g.collect_top_level(ids, mut decls)
	mut prev := flat.NodeKind.empty
	mut wrote_any := false
	mut last_import := -1
	for i, id in decls {
		if g.a.node(id).kind == .import_decl && !g.skip_decls[int(id)] {
			last_import = i
		}
	}
	mut injected_imports := false
	for i, id in decls {
		if g.skip_decls[int(id)] {
			continue
		}
		kind := g.a.node(id).kind
		mut injected_now := false
		if !injected_imports && last_import < 0 && g.implied_imports.len > 0
			&& kind !in [.module_decl, .directive] {
			if wrote_any {
				g.writeln('')
			}
			g.emit_implied_imports()
			g.writeln('')
			injected_imports = true
			injected_now = true
			wrote_any = true
			prev = .import_decl
		}
		if wrote_any {
			if !injected_now && !(prev == .import_decl && kind == .import_decl) && !(prev == kind
				&& kind in [.enum_decl, .expr_stmt]) {
				g.writeln('')
			}
		}
		g.indent++
		g.stmt(id)
		g.indent--
		if i == last_import && g.implied_imports.len > 0 {
			g.emit_implied_imports()
			injected_imports = true
		}
		prev = kind
		wrote_any = true
	}
	if !injected_imports && g.implied_imports.len > 0 {
		if wrote_any {
			g.writeln('')
		}
		g.emit_implied_imports()
	}
}

fn (mut g Gen) emit_implied_imports() {
	for name in g.implied_imports {
		g.writeln('import ${name}')
	}
}

fn (mut g Gen) collect_implied_imports(fnode &flat.Node) {
	mut imported := map[string]bool{}
	mut declared := map[string]bool{}
	for id in g.a.children_of(fnode) {
		n := g.a.node(id)
		if n.kind == .import_decl {
			local_name := if n.typ.len > 0 { n.typ } else { n.value.all_after_last('.') }
			imported[local_name] = true
		} else if n.kind == .module_decl {
			declared[n.value.all_after_last('.')] = true
		} else if n.kind == .fn_decl {
			declared[n.value.all_after_last('.')] = true
		}
	}
	for n in g.a.nodes {
		if n.pos.id != g.file_id {
			continue
		}
		if n.kind == .param {
			declared[n.value] = true
		}
		if n.kind == .decl_assign && n.children_count > 0 {
			for child in g.a.children_of(n) {
				cn := g.a.node(child)
				if cn.kind == .ident {
					declared[cn.value] = true
				}
			}
		}
	}
	mut implied := map[string]bool{}
	for n in g.a.nodes {
		if n.pos.id != g.file_id || n.kind != .selector || n.children_count == 0 {
			continue
		}
		receiver := g.a.child_node(n, 0)
		name := receiver.value
		if receiver.kind == .ident && name.len > 0 && name !in ['C', 'JS'] && !imported[name]
			&& !declared[name] && os.is_dir(os.join_path(@VEXEROOT, 'vlib', name)) {
			implied[name] = true
		}
	}
	g.implied_imports = implied.keys()
	g.implied_imports.sort()
}

// collect_top_level flattens the file's declaration list. A folded top-level
// `$if` is represented by the parser as a bare `.block` wrapping declarations;
// its condition is lost, so the members are emitted directly at file scope.
fn (mut g Gen) collect_top_level(ids []flat.NodeId, mut out []flat.NodeId) {
	for id in ids {
		if int(id) < 0 {
			continue
		}
		n := g.a.node(id)
		if n.kind == .empty {
			continue
		}
		if n.kind == .block {
			g.collect_top_level(g.a.children_of(n), mut out)
		} else {
			out << id
		}
	}
}

// stmt_list_ids renders a list of statements, one indentation level deeper.
fn (mut g Gen) stmt_list_ids(ids []flat.NodeId) {
	for id in ids {
		if int(id) < 0 {
			continue
		}
		if g.a.node(id).kind == .empty {
			continue
		}
		g.indent++
		g.stmt(id)
		g.indent--
	}
}

fn (mut g Gen) stmt(id flat.NodeId) {
	if int(id) < 0 {
		return
	}
	n := g.a.node(id)
	if g.is_debug {
		eprintln('stmt ${n.kind} | pos: ${n.pos.offset}')
	}
	stmt_start, stmt_end := g.stmt_source_span(n)
	g.emit_comments_before(stmt_start)
	g.source_end = int_max(g.source_end, stmt_start)
	match n.kind {
		.module_decl {
			g.emit_attrs(id)
			g.writeln('module ${n.value}')
		}
		.import_decl {
			g.import_decl(id)
		}
		.fn_decl, .c_fn_decl {
			g.fn_decl(id)
		}
		.struct_decl {
			g.struct_decl(id)
		}
		.enum_decl {
			g.enum_decl(id)
		}
		.type_decl {
			g.type_decl(id)
		}
		.interface_decl {
			g.interface_decl(id)
		}
		.const_decl {
			g.const_decl(id)
		}
		.global_decl {
			g.global_decl(id)
		}
		.directive {
			g.directive_stmt(id)
		}
		.expr_stmt {
			// some parser artifacts (e.g. around dropped comments) leave an
			// expr_stmt with no renderable expression; emit nothing for those.
			before := g.out.len
			g.expr(g.a.child(n, 0))
			if g.out.len != before && !g.in_init && !g.on_newline {
				g.writeln('')
			}
		}
		.assign, .selector_assign, .index_assign, .decl_assign {
			g.assign_stmt(id)
		}
		.return_stmt {
			exprs := g.a.children_of(n)
			if exprs.len == 0 {
				g.writeln('return')
			} else {
				g.write('return ')
				g.expr_list(exprs, ', ')
				if !g.on_newline {
					g.writeln('')
				}
			}
		}
		.block {
			g.block_stmt(id)
		}
		.for_stmt {
			g.for_stmt(id)
		}
		.for_in_stmt {
			g.for_in_stmt(id)
		}
		.break_stmt {
			g.flow_control('break', n.value)
		}
		.continue_stmt {
			g.flow_control('continue', n.value)
		}
		.goto_stmt {
			g.writeln('goto ${n.value}')
		}
		.label_stmt {
			g.writeln('${n.value}:')
		}
		.match_stmt {
			g.match_node(id)
			if !g.in_init {
				g.writeln('')
			}
		}
		.defer_stmt {
			g.defer_stmt(id)
		}
		.assert_stmt {
			g.assert_stmt(id)
		}
		.comptime_if {
			g.comptime_if(id)
			if !g.in_init {
				g.writeln('')
			}
		}
		.comptime_for {
			g.comptime_for(id)
			if !g.in_init {
				g.writeln('')
			}
		}
		.select_stmt {
			g.select_stmt(id)
			if !g.in_init {
				g.writeln('')
			}
		}
		.asm_stmt {
			if source := g.a.formatter_sources[int(id)] {
				g.writeln(source.trim_space())
			} else {
				g.writeln('asm {')
				g.writeln('}')
			}
		}
		.debugger_stmt {
			g.writeln('\$dbg')
		}
		.if_expr {
			g.if_expr(id)
			if !g.in_init {
				g.writeln('')
			}
		}
		.empty {}
		else {
			g.expr(id)
			if !g.in_init {
				g.writeln('')
			}
		}
	}
	if n.kind == .asm_stmt && int(id) in g.a.formatter_sources {
		g.skip_comments_before(stmt_end + 1)
	} else {
		g.emit_trailing_comments(stmt_end)
	}
	g.source_end = int_max(g.source_end, stmt_end)
}

fn (mut g Gen) expr(id flat.NodeId) {
	if int(id) < 0 {
		return
	}
	n := g.a.node(id)
	if g.is_debug {
		eprintln('expr ${n.kind} | pos: ${n.pos.offset}')
	}
	g.emit_comments_before(n.pos.offset)
	g.source_end = int_max(g.source_end, n.pos.offset)
	match n.kind {
		.empty {}
		.int_literal, .float_literal, .bool_literal {
			g.write(n.value)
		}
		.char_literal {
			if n.value.starts_with('c:') {
				g.write("c'${n.value[2..]}'")
			} else {
				// char values are stored verbatim (escapes not decoded)
				g.write('`${n.value}`')
			}
		}
		.string_literal {
			if n.typ.starts_with('raw:') {
				quote := if n.typ.ends_with('"') { '"' } else { "'" }
				g.write('r${quote}${n.value}${quote}')
			} else {
				g.write(quote_string(n.value))
			}
		}
		.string_interp {
			g.string_interp(id)
		}
		.ident {
			if n.typ == '__v3_formatter_raw' {
				source := g.a.formatter_sources[int(id)] or { n.value }
				g.write(source.trim_space())
			} else {
				g.write(if g.in_array_init && n.value == 'it' { 'index' } else { n.value })
			}
		}
		.nil_literal {
			g.write('nil')
		}
		.none_expr {
			g.write('none')
		}
		.enum_val {
			g.write('.${n.value}')
		}
		.infix {
			g.expr(g.a.child(n, 0))
			g.write(' ${op_str(n.op)} ')
			g.expr(g.a.child(n, 1))
		}
		.prefix {
			g.prefix_expr(id)
		}
		.postfix {
			child := g.a.child(n, 0)
			cn := g.a.node(child)
			if n.op == .not && n.typ.len > 0 && cn.kind == .array_literal && cn.typ == n.typ {
				g.expr(child)
			} else if n.op == .not {
				g.expr(child)
				g.write('!')
			} else {
				g.expr(child)
				g.write(op_str(n.op))
			}
		}
		.paren {
			g.write('(')
			g.expr(g.a.child(n, 0))
			g.write(')')
		}
		.call {
			g.call_expr(id)
		}
		.selector {
			g.expr(g.a.child(n, 0))
			if n.value == '$' {
				// comptime field access `recv.$(name_expr)`
				g.write('.\$(')
				g.expr(g.a.child(n, 1))
				g.write(')')
			} else {
				g.write('.${n.value}')
			}
		}
		.index {
			g.index_expr(id)
		}
		.if_expr {
			g.if_expr(id)
		}
		.match_stmt {
			g.match_node(id)
		}
		.select_stmt {
			g.select_stmt(id)
		}
		.struct_init {
			g.struct_init(id)
		}
		.assoc {
			g.assoc(id)
		}
		.array_literal {
			if prefix := g.a.formatter_sources[int(id)] {
				g.write(prefix.trim_space())
			}
			g.write('[')
			g.expr_list(g.a.children_of(n), ', ')
			g.write(']')
		}
		.array_init {
			g.write(n.typ)
			g.write('{')
			was_in_array_init := g.in_array_init
			g.in_array_init = true
			g.init_fields(g.a.children_of(n))
			g.in_array_init = was_in_array_init
			g.write('}')
		}
		.map_init {
			g.map_init(id)
		}
		.field_init {
			if n.value.len > 0 {
				g.write('${n.value}: ')
			}
			g.expr(g.a.child(n, 0))
		}
		.fn_literal {
			g.fn_literal(id)
		}
		.lambda_expr {
			g.lambda(id)
		}
		.or_expr {
			g.or_expr(id)
		}
		.cast_expr {
			g.write(n.value)
			g.write('(')
			g.expr(g.a.child(n, 0))
			g.write(')')
		}
		.as_expr {
			g.expr(g.a.child(n, 0))
			g.write(' as ${n.value}')
		}
		.is_expr {
			if g.a.child_node(n, 0).is_mut {
				g.write('mut ')
			}
			g.expr(g.a.child(n, 0))
			g.write(' is ${n.value}')
		}
		.in_expr {
			g.expr(g.a.child(n, 0))
			g.write(' in ')
			g.expr(g.a.child(n, 1))
		}
		.range {
			g.expr(g.a.child(n, 0))
			g.write(' .. ')
			if n.children_count > 1 {
				g.expr(g.a.child(n, 1))
			}
		}
		.spawn_expr {
			g.write('spawn ')
			g.expr(g.a.child(n, 0))
		}
		.lock_expr {
			g.lock_expr(id)
		}
		.sizeof_expr {
			g.write('sizeof(${n.value})')
		}
		.typeof_expr {
			if n.value.len > 0 {
				g.write('typeof[${n.value}]()')
			} else {
				g.write('typeof(')
				g.expr(g.a.child(n, 0))
				g.write(')')
			}
		}
		.dump_expr {
			g.write('dump(')
			g.expr(g.a.child(n, 0))
			g.write(')')
		}
		.offsetof_expr {
			g.write('__offsetof(${n.value}, ${n.typ})')
		}
		.block {
			g.block_expr(id)
		}
		.sql_expr {
			g.sql_expr(id)
		}
		.defer_result {
			if n.value.len > 0 {
				g.write('\$res(${n.value})')
			} else {
				g.write('\$res()')
			}
		}
		else {
			g.write('/* ${n.kind} */')
		}
	}
	if (n.kind == .sql_expr || n.typ == '__v3_formatter_raw')
		&& int(id) in g.a.formatter_sources {
		g.skip_comments_before(n.pos.end + 1)
	} else {
		g.emit_trailing_comments(n.pos.end)
	}
	g.source_end = int_max(g.source_end, n.pos.end)
}

fn (mut g Gen) prefix_expr(id flat.NodeId) {
	n := g.a.node(id)
	child := g.a.child(n, 0)
	cn := g.a.node(child)
	// `!is` / `!in` are parsed as a `.not` prefix wrapping the is/in expression.
	if n.op == .not && cn.kind == .is_expr {
		if g.a.child_node(cn, 0).is_mut {
			g.write('mut ')
		}
		g.expr(g.a.child(cn, 0))
		g.write(' !is ${cn.value}')
		return
	}
	if n.op == .not && cn.kind == .in_expr {
		g.expr(g.a.child(cn, 0))
		g.write(' !in ')
		g.expr(g.a.child(cn, 1))
		return
	}
	if n.value == '...' {
		g.write('...')
		g.expr(child)
		return
	}
	if n.value == 'shared' {
		g.write('shared ')
		g.expr(child)
		return
	}
	g.write(op_str(n.op))
	g.expr(child)
}

fn (mut g Gen) call_expr(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	if children.len == 0 {
		return
	}
	if kind := g.json_migration_call_kind(children[0]) {
		g.json_migration_call(kind, children[1..])
		return
	}
	g.expr(children[0])
	g.write('(')
	args := children[1..]
	for i, aid in args {
		a := g.a.node(aid)
		if a.is_mut {
			g.write('mut ')
		}
		g.expr(aid)
		if i < args.len - 1 {
			g.write(', ')
		}
	}
	g.write(')')
}

fn (g &Gen) json_migration_call_kind(callee_id flat.NodeId) ?string {
	if !g.migrate_json2 {
		return none
	}
	callee := g.a.node(callee_id)
	if callee.kind == .selector && callee.children_count > 0 {
		receiver := g.a.child_node(callee, 0)
		if receiver.kind == .ident && receiver.value == 'json'
			&& callee.value in ['encode', 'decode', 'encode_pretty'] {
			return callee.value
		}
	}
	if g.selective_json && callee.kind == .ident
		&& callee.value in ['encode', 'decode', 'encode_pretty'] {
		return callee.value
	}
	return none
}

fn (mut g Gen) json_migration_call(kind string, args []flat.NodeId) {
	if kind == 'decode' && args.len >= 2 {
		g.write('${g.json_qualifier}.decode[')
		type_arg := g.a.node(args[0])
		if source_type := g.source_span(type_arg.pos.offset, type_arg.pos.end) {
			g.write(source_type.trim_space())
		} else {
			g.expr(args[0])
		}
		g.write('](')
		g.expr_list(args[1..], ', ')
		g.write(')')
		return
	}
	g.write('${g.json_qualifier}.encode(')
	g.expr_list(args, ', ')
	if args.len > 0 {
		g.write(', ')
	}
	if kind == 'encode_pretty' {
		g.write('prettify: true, ')
	}
	g.write('escape_unicode: true)')
}

fn (mut g Gen) index_expr(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	if children.len == 0 {
		return
	}
	g.expr(children[0])
	if n.value == 'range' {
		g.write('[')
		if children.len > 1 && int(children[1]) >= 0 {
			g.expr(children[1])
		}
		g.write('..')
		if children.len > 2 && int(children[2]) >= 0 {
			g.expr(children[2])
		}
		g.write(']')
		return
	}
	gate := if n.op == .gated_index { '#' } else { '' }
	g.write('${gate}[')
	g.expr_list(children[1..], ', ')
	g.write(']')
}

fn (mut g Gen) struct_init(id flat.NodeId) {
	n := g.a.node(id)
	fields := g.a.children_of(n)
	g.write(n.value)
	if fields.len == 0 {
		g.write('{}')
		return
	}
	first := g.a.node(fields[0])
	if first.value.len > 0 {
		if g.source_line(n.pos.offset) == g.source_line(n.pos.end) {
			g.write('{ ')
			for i, fid in fields {
				f := g.a.node(fid)
				g.write('${f.value}: ')
				g.expr(g.a.child(f, 0))
				if i < fields.len - 1 {
					g.write(', ')
				}
			}
			g.write(' }')
			return
		}
		// named fields, one per line
		g.writeln('{')
		in_init := g.in_init
		g.in_init = true
		g.indent++
		for fid in fields {
			f := g.a.node(fid)
			g.write('${f.value}: ')
			g.expr(g.a.child(f, 0))
			g.writeln('')
		}
		g.indent--
		g.in_init = in_init
		g.write('}')
	} else {
		// positional
		g.write('{')
		for i, fid in fields {
			g.expr(g.a.child(g.a.node(fid), 0))
			if i < fields.len - 1 {
				g.write(', ')
			}
		}
		g.write('}')
	}
}

fn (mut g Gen) assoc(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	g.write(n.value)
	g.writeln('{')
	g.indent++
	g.write('...')
	if children.len > 0 {
		g.expr(children[0])
	}
	g.writeln('')
	for fid in children[1..] {
		f := g.a.node(fid)
		g.write('${f.value}: ')
		g.expr(g.a.child(f, 0))
		g.writeln('')
	}
	g.indent--
	g.write('}')
}

fn (mut g Gen) init_fields(ids []flat.NodeId) {
	for i, fid in ids {
		f := g.a.node(fid)
		if f.value.len > 0 {
			g.write('${f.value}: ')
		}
		g.expr(g.a.child(f, 0))
		if i < ids.len - 1 {
			g.write(', ')
		}
	}
}

fn (mut g Gen) map_init(id flat.NodeId) {
	n := g.a.node(id)
	if n.value.len > 0 {
		g.write(n.value)
		g.write('{}')
		return
	}
	children := g.a.children_of(n)
	if children.len == 0 {
		g.write('{}')
		return
	}
	g.write('{')
	mut i := 0
	for i + 1 < children.len {
		g.expr(children[i])
		g.write(': ')
		g.expr(children[i + 1])
		if i + 2 < children.len {
			g.write(', ')
		}
		i += 2
	}
	g.write('}')
}

fn (mut g Gen) fn_literal(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	mut i := 0
	mut captures := []flat.NodeId{}
	for i < children.len && g.a.node(children[i]).kind == .ident {
		captures << children[i]
		i++
	}
	mut params := []flat.NodeId{}
	for i < children.len && g.a.node(children[i]).kind == .param {
		params << children[i]
		i++
	}
	body := children[i..]
	g.write('fn')
	gp := n.generic_params()
	if gp.len > 0 {
		g.write('[${gp.join(', ')}]')
	}
	if captures.len > 0 {
		g.write(' [')
		for capture_i, capture_id in captures {
			capture := g.a.node(capture_id)
			if capture.is_mut {
				g.write('mut ')
			}
			if capture.typ in ['shared', 'atomic'] {
				g.write('${capture.typ} ')
			}
			g.expr(capture_id)
			if capture_i < captures.len - 1 {
				g.write(', ')
			}
		}
		g.write(']')
	}
	g.write(' ')
	g.params(params)
	if n.typ.len > 0 && n.typ != 'void' {
		g.write(' ${n.typ}')
	}
	g.writeln(' {')
	g.stmt_list_ids(body)
	g.write('}')
}

fn (mut g Gen) lambda(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	if children.len == 0 {
		g.write('|| ')
		return
	}
	body := children.last()
	params := children[..children.len - 1]
	g.write('|')
	for i, pid in params {
		p := g.a.node(pid)
		if p.is_mut {
			g.write('mut ')
		}
		g.write(p.value)
		if i < params.len - 1 {
			g.write(', ')
		}
	}
	g.write('| ')
	g.expr(body)
}

fn (mut g Gen) or_expr(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	if children.len == 0 {
		return
	}
	if n.value == '!' {
		g.expr(children[0])
		g.write('!')
		return
	}
	if n.value == '?' {
		g.expr(children[0])
		g.write('?')
		return
	}
	g.expr(children[0])
	g.write(' or {')
	if children.len > 1 {
		blk := g.a.node(children[1])
		stmts := g.a.children_of(blk)
		if stmts.len == 0 {
			g.write('}')
		} else {
			g.writeln('')
			g.stmt_list_ids(stmts)
			g.write('}')
		}
	} else {
		g.write('}')
	}
}

fn (mut g Gen) block_expr(id flat.NodeId) {
	n := g.a.node(id)
	stmts := g.a.children_of(n)
	prefix := if n.value == 'unsafe' { 'unsafe ' } else { '' }
	if g.in_init || stmts.len <= 1 {
		g.write('${prefix}{ ')
		in_init := g.in_init
		g.in_init = true
		for s in stmts {
			g.stmt(s)
		}
		g.in_init = in_init
		g.write(' }')
	} else {
		g.writeln('${prefix}{')
		g.stmt_list_ids(stmts)
		g.write('}')
	}
}

fn (mut g Gen) block_stmt(id flat.NodeId) {
	n := g.a.node(id)
	stmts := g.a.children_of(n)
	prefix := if n.value == 'unsafe' { 'unsafe ' } else { '' }
	g.writeln('${prefix}{')
	g.stmt_list_ids(stmts)
	g.writeln('}')
}

fn (mut g Gen) lock_expr(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	if children.len == 0 {
		g.write('lock {')
		g.write('}')
		return
	}
	body := children.last()
	objs := children[..children.len - 1]
	kw := if n.value.starts_with('rlock') { 'rlock' } else { 'lock' }
	g.write(kw)
	if objs.len > 0 {
		g.write(' ')
		g.expr_list(objs, ', ')
	}
	g.writeln(' {')
	g.stmt_list_ids(g.a.children_of(g.a.node(body)))
	g.write('}')
}

fn (mut g Gen) sql_expr(id flat.NodeId) {
	n := g.a.node(id)
	if source := g.a.formatter_sources[int(id)] {
		g.write(source.trim_space())
		return
	}
	g.write('sql')
	if n.children_count > 0 {
		g.write(' ')
		g.expr(g.a.child(n, 0))
	}
	g.writeln(' {')
	g.write('}')
}

fn (mut g Gen) string_interp(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	// prefer single quotes unless a literal segment contains `'` but no `"`
	mut has_single := false
	mut has_double := false
	for cid in children {
		c := g.a.node(cid)
		if c.kind == .string_literal {
			if c.value.contains("'") {
				has_single = true
			}
			if c.value.contains('"') {
				has_double = true
			}
		}
	}
	quote := if has_single && !has_double { `"` } else { `'` }
	quote_str := if quote == `"` { '"' } else { "'" }
	g.write(quote_str)
	for cid in children {
		c := g.a.node(cid)
		if c.kind == .string_literal {
			g.write(escape_string(c.value, quote))
		} else if c.kind == .directive && c.value == 'string_interp_format' {
			g.write('\${')
			g.expr(g.a.child(c, 0))
			g.write(':${c.typ}')
			g.write('}')
		} else {
			g.write('\${')
			g.expr(cid)
			g.write('}')
		}
	}
	g.write(quote_str)
}

fn (mut g Gen) assign_stmt(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	if children.len == 0 {
		return
	}
	is_decl := n.kind == .decl_assign
	opstr := if is_decl { ':=' } else { op_str(n.op) }
	modifier, count := parse_assign_meta(n.value)
	if modifier == 'atomic' {
		g.write('atomic ')
	} else if n.is_mut && modifier == 'volatile' && !g.suppress_mut {
		g.write('mut volatile ')
	} else if modifier.len > 0 {
		g.write('${modifier} ')
		if n.is_mut && !g.suppress_mut {
			g.write('mut ')
		}
	} else if n.is_mut && !g.suppress_mut {
		g.write('mut ')
	}
	if count <= 1 {
		g.expr(children[0])
		g.write(' ${opstr} ')
		g.expr_list(children[1..], ', ')
	} else {
		mut lhs := []flat.NodeId{}
		mut rhs := []flat.NodeId{}
		for i, c in children {
			if i % 2 == 0 {
				lhs << c
			} else {
				rhs << c
			}
		}
		g.expr_list(lhs, ', ')
		g.write(' ${opstr} ')
		g.expr_list(rhs, ', ')
	}
	if !g.in_init && !g.on_newline {
		g.writeln('')
	}
}

fn (mut g Gen) flow_control(kw string, label string) {
	if label.len > 0 {
		g.writeln('${kw} ${label}')
	} else {
		g.writeln(kw)
	}
}

fn (mut g Gen) for_stmt(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	init := if children.len > 0 { children[0] } else { flat.empty_node }
	cond := if children.len > 1 { children[1] } else { flat.empty_node }
	post := if children.len > 2 { children[2] } else { flat.empty_node }
	body := if children.len > 3 { children[3..] } else { []flat.NodeId{} }
	in_init := g.in_init
	g.in_init = true
	g.write('for')
	if n.value == 'c_style' {
		g.write(' ')
		if !g.is_empty(init) {
			g.suppress_mut = true
			g.stmt(init)
			g.suppress_mut = false
		}
		g.write('; ')
		if !g.is_empty(cond) {
			g.expr(cond)
		}
		g.write('; ')
		if !g.is_empty(post) {
			g.stmt(post)
		}
		g.write(' ')
	} else if !g.is_empty(cond) {
		g.write(' ')
		g.expr(cond)
		g.write(' ')
	} else {
		g.write(' ')
	}
	g.in_init = in_init
	g.writeln('{')
	g.stmt_list_ids(body)
	g.writeln('}')
}

fn (mut g Gen) for_in_stmt(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	if children.len < 3 {
		g.writeln('for {')
		g.writeln('}')
		return
	}
	header := if n.value.len > 0 { n.value.int() } else { 3 }
	v0 := children[0]
	v1 := children[1]
	mut_val := n.op == .amp
	g.write('for ')
	if g.is_empty(v1) {
		if mut_val {
			g.write('mut ')
		}
		g.expr(v0)
	} else {
		g.expr(v0)
		g.write(', ')
		if mut_val {
			g.write('mut ')
		}
		g.expr(v1)
	}
	g.write(' in ')
	mut body_start := 3
	if header >= 4 && children.len >= 4 {
		g.expr(children[2])
		g.write(' .. ')
		g.expr(children[3])
		body_start = 4
	} else {
		g.expr(children[2])
	}
	body := unsafe { children[body_start..] }
	g.writeln(' {')
	g.stmt_list_ids(body)
	g.writeln('}')
}

fn (mut g Gen) if_expr(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	if children.len < 2 {
		return
	}
	cond := children[0]
	cn := g.a.node(cond)
	g.write('if ')
	in_init := g.in_init
	g.in_init = true
	if cn.kind == .decl_assign {
		g.stmt(cond)
	} else {
		g.expr(cond)
	}
	g.in_init = in_init
	g.write(' ')
	then_blk := g.a.node(children[1])
	g.writeln('{')
	g.stmt_list_ids(g.a.children_of(then_blk))
	g.write('}')
	if children.len > 2 {
		else_id := children[2]
		en := g.a.node(else_id)
		if en.kind == .if_expr {
			g.write(' else ')
			g.if_expr(else_id)
		} else {
			g.writeln(' else {')
			g.stmt_list_ids(g.a.children_of(en))
			g.write('}')
		}
	}
}

fn (mut g Gen) match_node(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	if children.len == 0 {
		return
	}
	g.write('match ')
	in_init := g.in_init
	g.in_init = true
	g.expr(children[0])
	g.in_init = in_init
	g.writeln(' {')
	g.indent++
	for bid in children[1..] {
		b := g.a.node(bid)
		bchildren := g.a.children_of(b)
		if b.value == 'else' {
			g.write('else')
			g.writeln(' {')
			g.stmt_list_ids(bchildren)
			g.writeln('}')
		} else {
			ncond := b.value.int()
			conds := if ncond <= bchildren.len { bchildren[..ncond] } else { bchildren }
			rest := if ncond <= bchildren.len { bchildren[ncond..] } else { []flat.NodeId{} }
			for i, c in conds {
				if i > 0 {
					g.write(', ')
				}
				g.match_cond(c)
			}
			g.writeln(' {')
			g.stmt_list_ids(rest)
			g.writeln('}')
		}
	}
	g.indent--
	g.write('}')
}

fn (mut g Gen) defer_stmt(id flat.NodeId) {
	n := g.a.node(id)
	g.writeln('defer {')
	if n.children_count > 0 {
		blk := g.a.child_node(n, 0)
		g.stmt_list_ids(g.a.children_of(blk))
	}
	g.writeln('}')
}

fn (mut g Gen) assert_stmt(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	if children.len == 0 {
		return
	}
	g.write('assert ')
	in_init := g.in_init
	g.in_init = true
	g.expr(children[0])
	g.in_init = in_init
	if children.len > 1 {
		g.write(', ')
		g.expr(children[1])
	}
	if !g.in_init {
		g.writeln('')
	}
}

fn (mut g Gen) comptime_if(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	g.write('\$if ${n.value.trim_space()} {')
	g.writeln('')
	if children.len > 0 {
		then_blk := g.a.node(children[0])
		g.stmt_list_ids(g.a.children_of(then_blk))
	}
	g.write('}')
	if children.len > 1 {
		el := g.a.node(children[1])
		if el.kind == .comptime_if {
			g.write(' \$else ')
			g.comptime_if(children[1])
		} else {
			g.writeln(' \$else {')
			g.stmt_list_ids(g.a.children_of(el))
			g.write('}')
		}
	}
}

fn (mut g Gen) comptime_for(id flat.NodeId) {
	n := g.a.node(id)
	parts := n.value.split('|')
	loopvar := if parts.len > 0 { parts[0] } else { 'x' }
	kind := if parts.len > 1 { parts[1] } else { 'fields' }
	g.write('\$for ${loopvar} in ${n.typ}.${kind} {')
	g.writeln('')
	if n.children_count > 0 {
		blk := g.a.child_node(n, 0)
		g.stmt_list_ids(g.a.children_of(blk))
	}
	g.write('}')
}

fn (mut g Gen) select_stmt(id flat.NodeId) {
	n := g.a.node(id)
	g.writeln('select {')
	g.indent++
	for bid in g.a.children_of(n) {
		b := g.a.node(bid)
		bchildren := g.a.children_of(b)
		if b.value == 'else' {
			g.write('else')
			g.writeln(' {')
			g.stmt_list_ids(bchildren)
			g.writeln('}')
			continue
		}
		ncond := select_branch_cond_count(b.value)
		conds := if ncond <= bchildren.len { bchildren[..ncond] } else { bchildren }
		rest := if ncond <= bchildren.len { bchildren[ncond..] } else { []flat.NodeId{} }
		g.select_branch_header(b.value, conds)
		g.writeln(' {')
		g.stmt_list_ids(rest)
		g.writeln('}')
	}
	g.indent--
	g.write('}')
}

// match_cond renders a match branch condition. A type pattern such as `[]T` or
// `map[K]V` is parsed as an empty composite-literal node; emit it as a bare type
// (without the `{}`) so it is not mistaken for the branch body.
fn (mut g Gen) match_cond(id flat.NodeId) {
	n := g.a.node(id)
	if n.kind == .array_init && n.children_count == 0 && n.typ.len > 0 {
		g.write(n.typ)
		return
	}
	if n.kind == .map_init && n.children_count == 0 && n.value.len > 0 {
		g.write(n.value)
		return
	}
	g.expr(id)
}

// select_branch_header renders a select branch's guard: a receive binding
// (`x := <-ch` / `x = <-ch`) or a plain send/expression condition.
fn (mut g Gen) select_branch_header(value string, conds []flat.NodeId) {
	if conds.len == 2
		&& (value == 'recv' || value == 'recv_assign' || value.starts_with('recv_compound:')) {
		g.expr(conds[0])
		if value == 'recv' {
			g.write(' := ')
		} else if value == 'recv_assign' {
			g.write(' = ')
		} else {
			// `recv_compound:<op>` preserves a compound receive such as `x += <-ch`.
			g.write(' ${value.all_after('recv_compound:')} ')
		}
		g.expr(conds[1])
		return
	}
	g.expr_list(conds, ' ')
}

// Declarations ---------------------------------------------------------------

fn (mut g Gen) import_decl(id flat.NodeId) {
	n := g.a.node(id)
	if g.migrate_json2 && int(id) == g.json_import_id {
		g.writeln('import json2')
		return
	}
	g.write('import ${n.value}')
	last_seg := n.value.all_after_last('.')
	if n.typ.len > 0 && n.typ != last_seg {
		g.write(' as ${n.typ}')
	}
	symbols := g.a.children_of(n)
	if symbols.len > 0 {
		g.write(' { ')
		g.expr_list(symbols, ', ')
		g.write(' }')
	}
	g.writeln('')
}

fn (mut g Gen) fn_decl(id flat.NodeId) {
	n := g.a.node(id)
	g.emit_attrs(id)
	if n.op == .arrow {
		g.write('pub ')
	}
	children := g.a.children_of(n)
	mut i := 0
	mut params := []flat.NodeId{}
	for i < children.len && g.a.node(children[i]).kind == .param {
		params << children[i]
		i++
	}
	body := children[i..]
	mut recv := flat.empty_node
	if params.len > 0 && g.a.node(params[0]).op == .dot {
		recv = params[0]
		params = unsafe { params[1..] }
	}
	g.write('fn ')
	name := n.value
	if int(recv) >= 0 {
		rn := g.a.node(recv)
		mut receiver_type := g.receiver_type(rn)
		g.write('(')
		if rn.is_mut {
			g.write('mut ')
		}
		if receiver_type.starts_with('shared ') {
			g.write('shared ')
			receiver_type = receiver_type[7..]
		}
		g.write(rn.value)
		g.write(' ')
		g.write(receiver_type)
		g.write(') ')
		g.write(name.all_after_last('.'))
	} else if n.kind == .c_fn_decl {
		if name.starts_with('JS:') {
			g.write('JS.${name[3..]}')
		} else {
			g.write('C.${name}')
		}
	} else {
		g.write(name)
	}
	gp := n.generic_params()
	if gp.len > 0 {
		g.write('[${gp.join(', ')}]')
	}
	g.params(params)
	if n.typ.len > 0 && n.typ != 'void' {
		g.write(' ${n.typ}')
	}
	if n.kind == .c_fn_decl {
		g.writeln('')
		return
	}
	g.writeln(' {')
	g.stmt_list_ids(body)
	g.writeln('}')
}

fn (mut g Gen) params(ids []flat.NodeId) {
	g.write('(')
	for i, pid in ids {
		p := g.a.node(pid)
		mut typ := g.param_type(p)
		if p.is_mut {
			g.write('mut ')
		}
		if typ.starts_with('shared ') {
			g.write('shared ')
			typ = typ[7..]
		}
		if p.value.len > 0 {
			g.write(p.value)
			g.write(' ')
		}
		g.write(typ)
		if i < ids.len - 1 {
			g.write(', ')
		}
	}
	g.write(')')
}

fn (g &Gen) param_type(p &flat.Node) string {
	mut t := p.typ
	if p.is_mut && p.op != .amp && t.starts_with('&') {
		t = t[1..]
	}
	return t
}

fn (g &Gen) receiver_type(rn &flat.Node) string {
	mut t := rn.typ
	if rn.is_mut && t.starts_with('&') {
		t = t[1..]
	}
	return t
}

fn (mut g Gen) struct_decl(id flat.NodeId) {
	n := g.a.node(id)
	g.emit_attrs(id)
	if n.op == .arrow {
		g.write('pub ')
	}
	tags := n.typ
	kw := if tag_has(tags, 'union') { 'union ' } else { 'struct ' }
	g.write(kw)
	g.write(n.value)
	gp := n.generic_params()
	if gp.len > 0 {
		g.write('[${gp.join(', ')}]')
	}
	impls := tag_value(tags, 'implements')
	if impls.len > 0 {
		g.write(' implements ${impls.replace('|', ', ')}')
	}
	g.struct_fields(g.a.children_of(n))
}

fn (mut g Gen) struct_fields(fields []flat.NodeId) {
	if fields.len > 0 {
		g.writeln(' {')
	} else {
		g.write(' {')
	}
	g.indent++
	mut cur_access := ''
	for fid in fields {
		f := g.a.node(fid)
		g.emit_comments_before(f.pos.offset)
		g.source_end = int_max(g.source_end, f.pos.offset)
		if f.kind != .field_decl {
			// e.g. a `$if` block inside the struct body
			g.stmt(fid)
			continue
		}
		gp := f.generic_params()
		flags := if gp.len > 0 { gp[0] } else { '' }
		access := access_label(flags)
		if access != cur_access {
			// access specifiers sit one level out from the fields they head
			g.indent--
			match access {
				'mut' { g.writeln('mut:') }
				'pub' { g.writeln('pub:') }
				'pub mut' { g.writeln('pub mut:') }
				else {}
			}
			g.indent++
			cur_access = access
		}
		is_embed := f.value == f.typ && f.children_count == 0
		if is_embed {
			g.write(f.value)
		} else {
			if flags.contains('v') {
				g.write('volatile ')
			}
			g.write(f.value)
			g.write(' ')
			g.write(f.typ)
			if f.children_count > 0 {
				g.write(' = ')
				g.expr(g.a.child(f, 0))
			}
		}
		if gp.len > 1 {
			g.write(' @[${gp[1..].join('; ')}]')
		}
		g.emit_trailing_comments(f.pos.end)
		if !g.on_newline {
			g.writeln('')
		}
		g.source_end = int_max(g.source_end, f.pos.end)
	}
	g.indent--
	g.writeln('}')
}

fn (mut g Gen) enum_decl(id flat.NodeId) {
	n := g.a.node(id)
	g.emit_attrs(id)
	if n.op == .arrow {
		g.write('pub ')
	}
	g.write('enum ${n.value}')
	gp := n.generic_params()
	if gp.len > 0 && gp[0].len > 0 {
		g.write(' as ${gp[0]}')
	}
	g.writeln(' {')
	g.indent++
	for fid in g.a.children_of(n) {
		f := g.a.node(fid)
		g.emit_comments_before(f.pos.offset)
		g.source_end = int_max(g.source_end, f.pos.offset)
		g.write(f.value)
		if f.children_count > 0 {
			g.write(' = ')
			g.expr(g.a.child(f, 0))
		}
		fattrs := f.generic_params()
		if fattrs.len > 0 {
			g.write(' @[${fattrs.join('; ')}]')
		}
		g.emit_trailing_comments(f.pos.end)
		if !g.on_newline {
			g.writeln('')
		}
		g.source_end = int_max(g.source_end, f.pos.end)
	}
	g.indent--
	g.writeln('}')
}

fn (mut g Gen) type_decl(id flat.NodeId) {
	n := g.a.node(id)
	g.emit_attrs(id)
	if n.op == .arrow {
		g.write('pub ')
	}
	g.write('type ${n.value}')
	gp := n.generic_params()
	if gp.len > 0 {
		g.write('[${gp.join(', ')}]')
	}
	variants := g.a.children_of(n)
	if variants.len > 0 {
		g.write(' = ')
		for i, vid in variants {
			g.write(g.a.node(vid).value)
			if i < variants.len - 1 {
				g.write(' | ')
			}
		}
	} else if n.typ.len > 0 {
		g.write(' = ${n.typ}')
	}
	g.writeln('')
}

fn (mut g Gen) interface_decl(id flat.NodeId) {
	n := g.a.node(id)
	g.emit_attrs(id)
	if n.op == .arrow {
		g.write('pub ')
	}
	g.write('interface ${n.value}')
	gp := n.generic_params()
	if gp.len > 0 {
		g.write('[${gp.join(', ')}]')
	}
	g.writeln(' {')
	g.indent++
	mut cur_mut := false
	for fid in g.a.children_of(n) {
		f := g.a.node(fid)
		g.emit_comments_before(f.pos.offset)
		g.source_end = int_max(g.source_end, f.pos.offset)
		if f.is_mut != cur_mut {
			if f.is_mut {
				g.indent--
				g.writeln('mut:')
				g.indent++
			}
			cur_mut = f.is_mut
		}
		if f.op == .dot {
			// method
			g.write(f.value)
			mgp := f.generic_params()
			if mgp.len > 0 {
				g.write('[${mgp.join(', ')}]')
			}
			g.params(g.a.children_of(f))
			if f.typ.len > 0 {
				g.write(' ${f.typ}')
			}
		} else {
			g.write(f.value)
			if f.typ.len > 0 {
				g.write(' ${f.typ}')
			}
		}
		g.emit_trailing_comments(f.pos.end)
		if !g.on_newline {
			g.writeln('')
		}
		g.source_end = int_max(g.source_end, f.pos.end)
	}
	g.indent--
	g.writeln('}')
}

fn (mut g Gen) const_decl(id flat.NodeId) {
	n := g.a.node(id)
	g.emit_attrs(id)
	pub_prefix := if n.op == .arrow { 'pub ' } else { '' }
	fields := g.a.children_of(n)
	if fields.len == 1 {
		f := g.a.node(fields[0])
		g.write('${pub_prefix}const ${f.value}')
		if f.children_count > 0 {
			g.write(' = ')
			g.expr(g.a.child(f, 0))
		} else if f.typ.len > 0 {
			g.write(' ${f.typ}')
		}
		g.writeln('')
		return
	}
	g.writeln('${pub_prefix}const (')
	g.indent++
	for fid in fields {
		f := g.a.node(fid)
		g.write('${f.value} = ')
		g.expr(g.a.child(f, 0))
		g.writeln('')
	}
	g.indent--
	g.writeln(')')
}

fn (mut g Gen) global_decl(id flat.NodeId) {
	n := g.a.node(id)
	g.emit_attrs(id)
	group_pub := n.op == .arrow
	if group_pub {
		g.write('pub ')
	}
	g.writeln('__global (')
	g.indent++
	for fid in g.a.children_of(n) {
		f := g.a.node(fid)
		if f.op == .arrow && !group_pub {
			g.write('pub ')
		}
		if 'const' in f.generic_params() {
			g.write('const ')
		}
		g.write(f.value)
		if f.children_count > 0 {
			if f.typ.len > 0 {
				g.write(' ${f.typ}')
			}
			g.write(' = ')
			g.expr(g.a.child(f, 0))
		} else {
			g.write(' ${f.typ}')
		}
		g.writeln('')
	}
	g.indent--
	g.writeln(')')
}

fn (mut g Gen) directive_stmt(id flat.NodeId) {
	n := g.a.node(id)
	if n.value.starts_with('@attributes:') || n.value == 'string_interp_format' {
		return
	}
	g.write('#${n.value}')
	if n.typ.len > 0 {
		g.write(' ${n.typ}')
	}
	g.writeln('')
}

fn (mut g Gen) emit_attrs(id flat.NodeId) {
	attrs := g.attrs[int(id)] or { []string{} }
	if attrs.len > 0 {
		g.writeln('@[${attrs.join('; ')}]')
	}
}

// Helpers --------------------------------------------------------------------

fn (g &Gen) stmt_source_span(n &flat.Node) (int, int) {
	if n.kind in [.expr_stmt, .assign, .selector_assign, .index_assign, .decl_assign, .return_stmt, .assert_stmt]
		&& n.children_count > 0 {
		mut start := n.pos.offset
		mut end := 0
		mut found := false
		for child_id in g.a.children_of(n) {
			child := g.a.node(child_id)
			if !child.pos.is_valid() {
				continue
			}
			if !found || child.pos.offset < start {
				start = child.pos.offset
			}
			end = int_max(end, child.pos.end)
			found = true
		}
		if found {
			return start, end
		}
	}
	return n.pos.offset, n.pos.end
}

fn (mut g Gen) emit_comments_before(limit int) {
	for g.comment_i < g.comments.len && g.comments[g.comment_i].pos.offset < limit {
		comment := g.comments[g.comment_i]
		is_inline := g.source_end >= 0
			&& g.source_line(g.source_end) == g.source_line(comment.pos.offset)
		if is_inline {
			mut removed_newlines := 0
			for g.out.len > 0 && g.out.last_n(1) == '\n' {
				g.out.go_back(1)
				removed_newlines++
			}
			g.on_newline = false
			if g.out.len > 0 && g.out.last_n(1) !in [' ', '\t', '\n'] {
				g.write(' ')
			}
			g.write_comment(comment.text)
			for _ in 1 .. removed_newlines {
				g.writeln('')
			}
		} else {
			if !g.on_newline && g.out.len > 0 {
				g.writeln('')
			}
			if g.source_end >= 0
				&& g.source_line(comment.pos.offset) > g.source_line(g.source_end) + 1
				&& (g.out.len == 0 || !g.out.last_n(int_min(2, g.out.len)).ends_with('\n\n')) {
				g.writeln('')
			}
			g.write_comment(comment.text)
		}
		g.source_end = int_max(g.source_end, comment.pos.end)
		g.comment_i++
	}
}

fn (mut g Gen) emit_trailing_comments(end int) {
	for g.comment_i < g.comments.len {
		comment := g.comments[g.comment_i]
		if comment.pos.offset < end || g.source_line(comment.pos.offset) != g.source_line(end)
			|| comment.pos.offset > g.source.len
			|| g.source[end..comment.pos.offset].trim_space().len > 0 {
			return
		}
		if g.out.len > 0 && g.out.last_n(1) == '\n' {
			g.out.go_back(1)
			g.on_newline = false
		}
		if g.out.len > 0 && g.out.last_n(1) !in [' ', '\t', '\n'] {
			g.write(' ')
		}
		g.write_comment(comment.text)
		g.source_end = comment.pos.end
		g.comment_i++
	}
}

fn (mut g Gen) skip_comments_before(limit int) {
	for g.comment_i < g.comments.len && g.comments[g.comment_i].pos.offset < limit {
		g.source_end = int_max(g.source_end, g.comments[g.comment_i].pos.end)
		g.comment_i++
	}
}

fn (g &Gen) source_line(offset int) int {
	file := g.a.source_files[g.file_id] or { return 0 }
	return file.find_line(offset)
}

fn (g &Gen) source_span(start int, end int) ?string {
	if start < 0 || end < start || end > g.source.len {
		return none
	}
	return g.source[start..end]
}

fn (mut g Gen) write_comment(text string) {
	mut normalized := text
	if text.starts_with('//') && text.len > 2 && text[2] !in [` `, `\t`, `/`, `!`, `#`, `*`] {
		normalized = '// ${text[2..]}'
	}
	lines := normalized.split('\n')
	for i, line in lines {
		if i == lines.len - 1 && line == '' {
			continue
		}
		g.writeln(line)
	}
}

struct VfmtDirective {
	is_off bool
	offset int
}

fn vfmt_directives(source string) []VfmtDirective {
	mut directives := []VfmtDirective{}
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file('vfmt_source', source.len)
	file.index_lines(source)
	mut s := scanner.new_scanner(pref.new_preferences(), .scan_comments)
	s.init(file, source)
	for {
		tok := s.scan()
		if tok == .comment {
			text := s.lit.trim_space()
			if text.starts_with('// vfmt off') {
				directives << VfmtDirective{
					is_off: true
					offset: s.pos
				}
			} else if text.starts_with('// vfmt on') {
				directives << VfmtDirective{
					offset: s.pos
				}
			}
		}
		if tok == .eof {
			break
		}
	}
	return directives
}

fn next_vfmt_directive(directives []VfmtDirective, offset int, is_off bool) ?VfmtDirective {
	for directive in directives {
		if directive.offset >= offset && directive.is_off == is_off {
			return directive
		}
	}
	return none
}

fn line_after(source string, offset int) int {
	return source.index_after('\n', offset) or { source.len - 1 } + 1
}

fn restore_vfmt_disabled_regions(formatted string, source string) string {
	source_directives := vfmt_directives(source)
	formatted_directives := vfmt_directives(formatted)
	mut source_pos := 0
	mut formatted_pos := 0
	mut out := strings.new_builder(formatted.len + source.len / 8)
	for {
		source_off := next_vfmt_directive(source_directives, source_pos, true) or { break }
		formatted_off := next_vfmt_directive(formatted_directives, formatted_pos, true) or { break }
		source_content := line_after(source, source_off.offset)
		formatted_content := line_after(formatted, formatted_off.offset)
		out.write_string(formatted[formatted_pos..formatted_content])
		source_on := next_vfmt_directive(source_directives, source_content, false) or {
			out.write_string(source[source_content..])
			return out.str()
		}
		formatted_on := next_vfmt_directive(formatted_directives, formatted_content, false) or {
			out.write_string(source[source_content..])
			return out.str()
		}
		mut source_resume := line_after(source, source_on.offset)
		mut formatted_resume := line_after(formatted, formatted_on.offset)
		for source_resume < source.len && source[source_resume] == `\n` {
			source_resume++
		}
		for formatted_resume < formatted.len && formatted[formatted_resume] == `\n` {
			formatted_resume++
		}
		out.write_string(source[source_content..source_resume])
		source_pos = source_resume
		formatted_pos = formatted_resume
	}
	out.write_string(formatted[formatted_pos..])
	return out.str()
}

fn (mut g Gen) expr_list(ids []flat.NodeId, sep string) {
	mut first := true
	for id in ids {
		if int(id) < 0 {
			continue
		}
		if !first {
			g.write(sep)
		}
		g.expr(id)
		first = false
	}
}

fn (g &Gen) is_empty(id flat.NodeId) bool {
	if int(id) < 0 {
		return true
	}
	return g.a.node(id).kind == .empty
}

@[inline]
fn (mut g Gen) write(str string) {
	if g.on_newline && g.indent > 0 {
		for _ in 0 .. g.indent {
			g.out.write_u8(`\t`)
		}
	}
	g.out.write_string(str)
	g.on_newline = false
}

@[inline]
fn (mut g Gen) writeln(str string) {
	if g.on_newline && g.indent > 0 {
		for _ in 0 .. g.indent {
			g.out.write_u8(`\t`)
		}
	}
	g.out.writeln(str)
	g.on_newline = true
}

// op_str returns the source spelling of a flat operator.
fn op_str(op flat.Op) string {
	return match op {
		.none { '' }
		.plus { '+' }
		.minus { '-' }
		.mul { '*' }
		.div { '/' }
		.mod { '%' }
		.eq { '==' }
		.ne { '!=' }
		.lt { '<' }
		.gt { '>' }
		.le { '<=' }
		.ge { '>=' }
		.amp { '&' }
		.pipe { '|' }
		.xor { '^' }
		.left_shift { '<<' }
		.right_shift { '>>' }
		.right_shift_unsigned { '>>>' }
		.logical_and { '&&' }
		.logical_or { '||' }
		.not { '!' }
		.bit_not { '~' }
		.assign { '=' }
		.plus_assign { '+=' }
		.minus_assign { '-=' }
		.mul_assign { '*=' }
		.div_assign { '/=' }
		.mod_assign { '%=' }
		.amp_assign { '&=' }
		.pipe_assign { '|=' }
		.xor_assign { '^=' }
		.left_shift_assign { '<<=' }
		.right_shift_assign { '>>=' }
		.right_shift_unsigned_assign { '>>>=' }
		.inc { '++' }
		.dec { '--' }
		.dot { '.' }
		.arrow { '<-' } // channel send/receive
		.gated_index { '#' }
		.power { '**' }
		.power_assign { '**=' }
	}
}

// quote_string wraps a decoded string-literal value in quotes, preferring
// single quotes and falling back to double quotes when the value contains a
// `'` but no `"`. The contents are re-escaped because the parser stores fully
// decoded values.
fn quote_string(s string) string {
	if s.contains("'") && !s.contains('"') {
		return '"${escape_string(s, `"`)}"'
	}
	return "'${escape_string(s, `'`)}'"
}

// escape_string re-escapes a decoded string/char value for emission inside a
// literal delimited by `quote`. `$` is always escaped so a literal dollar is
// never reinterpreted as interpolation.
fn escape_string(s string, quote u8) string {
	mut b := strings.new_builder(s.len + 8)
	for c in s {
		match c {
			`\\` {
				b.write_string('\\\\')
			}
			`$` {
				b.write_string('\\\$')
			}
			`\n` {
				b.write_string('\\n')
			}
			`\r` {
				b.write_string('\\r')
			}
			`\t` {
				b.write_string('\\t')
			}
			0 {
				b.write_string('\\0')
			}
			else {
				if c == quote {
					b.write_u8(`\\`)
				}
				b.write_u8(c)
			}
		}
	}
	return b.str()
}

// access_label maps a struct field flag code (from generic_params()[0]) to its
// section keyword. Codes: `m`=mut, `p`=pub (order is mut-then-pub).
fn access_label(flags string) string {
	has_mut := flags.contains('m')
	has_pub := flags.contains('p')
	if has_pub && has_mut {
		return 'pub mut'
	}
	if has_pub {
		return 'pub'
	}
	if has_mut {
		return 'mut'
	}
	return ''
}

// tag_has reports whether a comma-joined struct tag string contains `name`.
fn tag_has(tags string, name string) bool {
	for part in tags.split(',') {
		if part.trim_space() == name {
			return true
		}
	}
	return false
}

// tag_value returns the value of a `key=value` entry in a comma-joined tag
// string, or '' when absent.
fn tag_value(tags string, key string) string {
	for part in tags.split(',') {
		p := part.trim_space()
		if p.starts_with('${key}=') {
			return p.all_after('${key}=')
		}
	}
	return ''
}

// parse_assign_meta decodes an assignment node's `value` field into an optional
// leading modifier keyword and the number of left-hand-side targets.
fn parse_assign_meta(value string) (string, int) {
	mut v := value
	mut modifier := ''
	for m in ['static', 'shared', 'atomic', 'volatile'] {
		if v == m {
			return m, 1
		}
		if v.starts_with('${m}:') {
			modifier = m
			v = v.all_after('${m}:')
			break
		}
	}
	if v.len == 0 {
		return modifier, 1
	}
	if v[0] >= `0` && v[0] <= `9` {
		return modifier, v.int()
	}
	return modifier, 1
}

// select_branch_cond_count returns how many leading children of a select branch
// are conditions (the remainder is the branch body).
fn select_branch_cond_count(value string) int {
	return match true {
		value == 'recv' || value == 'recv_assign' || value.starts_with('recv_compound') { 2 }
		value == '' { 1 }
		else { 0 }
	}
}
