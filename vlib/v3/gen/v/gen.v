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

const formatter_array_first_break = 85
const formatter_array_wrap_break = 93
const formatter_max_line_len = 100

struct FormatterTypeSource {
	text  string
	start int
	end   int
}

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
	is_new_int      bool
	is_translated   bool
	in_c_function   bool
	backend         string = 'c'
	formatter_types map[string]FormatterTypeSource
	array_breaks    []bool
	array_depth     int
	// suppress_mut skips the `mut ` prefix on an assignment (used for C-style
	// `for` loop init clauses, whose variable the parser always marks mutable).
	suppress_mut bool
	// attrs maps a declaration node id to its `@[...]` attribute strings. The parser stores
	// attributes on a separate floating `.directive` node rather than as a child, so they are
	// collected up-front in collect_attrs. attr_sources retains formatter-only source groups.
	attrs        map[int][]string
	attr_sources map[int]string
}

// FormatOptions controls optional formatter output.
pub struct FormatOptions {
pub:
	is_debug   bool
	is_new_int bool
	backend    string = 'c'
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
	g.is_translated = false
	g.in_c_function = false
	g.formatter_types = map[string]FormatterTypeSource{}
	g.array_breaks.clear()
	g.array_depth = 0
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
	g.is_new_int = options.is_new_int
	g.backend = options.backend
	mut out := strings.new_builder(1000)
	mut first := true
	for i, id in a.file_node_ids {
		// file_node_ids contains (marker, trailing) pairs. A trailing node can
		// legitimately have no children when its file contains only comments.
		if i % 2 == 0 {
			continue
		}
		n := a.node(flat.NodeId(id))
		if n.kind != .file {
			continue
		}
		formatted := g.gen_file(a, flat.NodeId(id))
		if formatted.len == 0 {
			continue
		}
		if !first {
			out.writeln('')
		}
		out.write_string(formatted)
		first = false
	}
	return out.str()
}

// gen_file formats the top-level declarations of the given trailing file node.
pub fn (mut g Gen) gen_file(a &flat.FlatAst, file_id flat.NodeId) string {
	g.reset()
	g.a = a
	g.collect_attrs()
	fnode := a.node(file_id)
	g.is_translated = g.file_has_attr(fnode, 'translated')
	g.file_id = fnode.pos.id
	g.source = a.formatter_file_sources[g.file_id] or { '' }
	g.collect_formatter_types()
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

fn (mut g Gen) collect_formatter_types() {
	for id, n in g.a.nodes {
		if n.kind != .struct_decl || n.pos.id != g.file_id {
			continue
		}
		source := g.a.formatter_sources[id] or { continue }
		if !n.value.starts_with('AnonStruct_') && !n.value.starts_with('AnonUnion_') {
			continue
		}
		g.formatter_types[n.value] = FormatterTypeSource{
			text:  source.trim_space()
			start: n.pos.offset
			end:   n.pos.end
		}
	}
}

// output_string returns the generated V source code.
pub fn (mut g Gen) output_string() string {
	return g.out.str()
}

// collect_attrs indexes every floating attribute directive by the declaration
// node id it annotates.
fn (mut g Gen) collect_attrs() {
	g.attrs = map[int][]string{}
	g.attr_sources = map[int]string{}
	for i, n in g.a.nodes {
		if n.kind == .directive && n.value.starts_with('@attributes:') {
			decl_id := n.value.all_after('@attributes:').int()
			g.attrs[decl_id] = n.generic_params()
			if source := g.a.formatter_sources[i] {
				g.attr_sources[decl_id] = source
			}
		}
	}
}

fn (g &Gen) file_has_attr(fnode &flat.Node, name string) bool {
	for id in g.a.children_of(fnode) {
		if g.a.node(id).kind != .module_decl {
			continue
		}
		attrs := g.attrs[int(id)] or { return false }
		return attrs.any(it.all_before(':').trim_space() == name)
	}
	return false
}

fn (g &Gen) collect_json_migration_declarations(ids []flat.NodeId, mut declared_names map[string]bool) {
	for id in ids {
		n := g.a.node(id)
		if n.kind in [.const_decl, .global_decl] {
			for field_id in g.a.children_of(n) {
				declared_names[g.a.node(field_id).value] = true
			}
		} else if n.kind == .fn_decl && !n.value.contains('.') {
			declared_names[n.value] = true
		} else if n.kind == .c_fn_decl && n.value.starts_with('V:') && !n.value[2..].contains('.') {
			declared_names[n.value[2..]] = true
		} else if n.kind in [.block, .comptime_if] {
			g.collect_json_migration_declarations(g.a.children_of(n), mut declared_names)
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
	directives := vfmt_directives(g.source)
	direct_ids := g.a.children_of(fnode)
	mut direct := map[int]bool{}
	mut declared_names := map[string]bool{}
	for id in direct_ids {
		direct[int(id)] = true
		n := g.a.node(id)
		if n.kind == .module_decl && n.value == 'json2' {
			return
		}
	}
	g.collect_json_migration_declarations(direct_ids, mut declared_names)
	mut legacy_imports := []flat.NodeId{}
	mut json2_imports := []flat.NodeId{}
	mut called := map[int]bool{}
	mut selector_receivers := map[int]bool{}
	for i, n in g.a.nodes {
		if n.pos.id != g.file_id {
			continue
		}
		if n.kind == .call && n.children_count > 0 {
			called[int(g.a.child(n, 0))] = true
		}
		if n.kind == .selector && n.children_count > 0 {
			receiver := g.a.child(n, 0)
			if g.a.node(receiver).kind == .ident {
				selector_receivers[int(receiver)] = true
			}
		}
		if n.kind == .import_decl {
			if n.value in ['json', 'json2'] && vfmt_is_disabled_at(directives, n.pos.offset) {
				return
			}
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
	if declared_names[qualifier] {
		return
	}
	for i, n in g.a.nodes {
		if n.pos.id != g.file_id {
			continue
		}
		// Existing module selector receivers are safe; any other identifier with the
		// qualifier can be a lexical collision, so keep the legacy import conservatively.
		if n.kind == .ident && n.value == qualifier && !selector_receivers[i] {
			return
		}
		if n.kind == .param && n.value == qualifier {
			return
		}
		if n.kind == .comptime_for && n.value.all_before('|') == qualifier {
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
				if vfmt_is_disabled_at(directives, n.pos.offset) {
					return
				}
				if n.value !in ['encode', 'decode', 'encode_pretty'] || !called[i] {
					return
				}
				if n.value == 'decode' && g.comments_inside(n.pos.offset, n.pos.end) {
					return
				}
			}
		}
		if legacy.children_count > 0 && n.kind == .call && n.children_count > 0 {
			callee := g.a.child_node(n, 0)
			if callee.kind == .ident && callee.value in ['encode', 'decode', 'encode_pretty']
				&& vfmt_is_disabled_at(directives, callee.pos.offset) {
				return
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
	mut previous := flat.empty_node
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
			previous = flat.empty_node
		}
		if wrote_any {
			adjacent_consts := prev == .const_decl && kind == .const_decl && int(previous) >= 0
				&& !g.source_has_blank_line_between(g.a.node(previous).pos.end, g.a.node(id).pos.offset)
			if !injected_now && !(prev == .import_decl && kind == .import_decl) && !(prev == kind
				&& kind in [.expr_stmt, .global_decl]) && !adjacent_consts {
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
		previous = id
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
		} else if n.kind in [.const_decl, .global_decl] {
			for field_id in g.a.children_of(n) {
				declared[g.a.node(field_id).value] = true
			}
		}
	}
	mut implied := map[string]bool{}
	mut vlib_dirs := map[string]bool{}
	mut vlib_listed := false
	for id, n in g.a.nodes {
		if n.pos.id != g.file_id || n.kind != .selector || n.children_count == 0 {
			continue
		}
		receiver := g.a.child_node(n, 0)
		name := receiver.value
		if receiver.kind == .ident && name.len > 0 && name !in ['C', 'JS'] && !imported[name]
			&& !declared[name] && !g.a.formatter_local_sels[id] {
			// Match the module directory case-sensitively. `os.is_dir` answers the filesystem,
			// and on a case-insensitive one (macOS, Windows) `vlib/Time` is the `time` module —
			// so a static call on a type named `Time` implied a bogus `import Time`, which the
			// formatter then wrote into the file and broke the build.
			if !vlib_listed {
				vlib_listed = true
				for entry in os.ls(os.join_path(@VEXEROOT, 'vlib')) or { []string{} } {
					vlib_dirs[entry] = true
				}
			}
			if vlib_dirs[name] && os.is_dir(os.join_path(@VEXEROOT, 'vlib', name)) {
				implied[name] = true
			}
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
	// A statement list is always emitted as a multi-line braced body, so it opens
	// a fresh statement scope where every statement must terminate with a newline.
	// Compact/inline renderings never reach this function; they emit statements
	// directly with `in_init` set. Reset `in_init` here so that a body nested
	// inside an initializer (e.g. an anonymous fn used as a struct field value)
	// does not have its statements collapsed onto a single line.
	saved_in_init := g.in_init
	g.in_init = false
	defer {
		g.in_init = saved_in_init
	}
	mut previous := flat.empty_node
	mut i := 0
	for i < ids.len {
		id := ids[i]
		i++
		if int(id) < 0 {
			continue
		}
		if g.a.node(id).kind == .empty {
			continue
		}
		if int(previous) >= 0 {
			_, previous_end := g.stmt_source_span(g.a.node(previous))
			current_start, _ := g.stmt_source_span(g.a.node(id))
			if g.source_has_blank_line_between(previous_end, current_start) {
				if !g.on_newline {
					g.writeln('')
				}
				if g.out.len < 2 || g.out.last_n(2) != '\n\n' {
					g.out.writeln('')
					g.on_newline = true
				}
			}
		}
		g.indent++
		mut next_i := i
		for next_i < ids.len && (int(ids[next_i]) < 0 || g.a.node(ids[next_i]).kind == .empty) {
			next_i++
		}
		if g.a.node(id).kind == .label_stmt && next_i < ids.len && g.is_loop_statement(ids[next_i]) {
			label := g.a.node(id)
			loop := g.a.node(ids[next_i])
			_, label_end := g.stmt_source_span(label)
			loop_start, _ := g.stmt_source_span(loop)
			if !g.has_comment_between(label_end, loop_start) {
				g.emit_comments_before(label.pos.offset)
				g.source_end = int_max(g.source_end, label.pos.offset)
				g.write('${label.value}: ')
				g.source_end = int_max(g.source_end, label_end)
				g.stmt(ids[next_i])
				g.indent--
				previous = ids[next_i]
				i = next_i + 1
				continue
			}
		}
		g.stmt(id)
		g.indent--
		previous = id
	}
}

fn (g &Gen) is_loop_statement(id flat.NodeId) bool {
	n := g.a.node(id)
	return n.kind in [.for_stmt, .for_in_stmt]
		|| (n.kind == .block && n.value == 'for_c_style_multi')
}

fn (g &Gen) source_has_blank_line_between(start int, end int) bool {
	if gap := g.source_span(start, end) {
		lines := gap.replace('\r\n', '\n').split('\n')
		for i in 1 .. lines.len - 1 {
			if lines[i].trim_space().len == 0 {
				return true
			}
		}
	}
	return false
}

fn (mut g Gen) emit_blank_line_between(previous flat.NodeId, current flat.NodeId) {
	if int(previous) < 0 {
		return
	}
	prev := g.a.node(previous)
	cur := g.a.node(current)
	if !prev.pos.is_valid() || !cur.pos.is_valid() {
		return
	}
	mut limit := cur.pos.offset
	if g.comment_i < g.comments.len {
		// a comment in the gap keeps the blank lines that follow it, so only the
		// part of the gap before that comment decides the separator here
		comment_start := g.comments[g.comment_i].pos.offset
		if comment_start >= prev.pos.end && comment_start < limit {
			limit = comment_start
		}
	}
	if !g.source_has_blank_line_between(prev.pos.end, limit) {
		return
	}
	g.write_blank_line()
}

// emit_blank_line_after_comment writes a blank line when the source kept one
// between the comment that was just emitted and the code starting at `limit`.
fn (mut g Gen) emit_blank_line_after_comment(limit int) {
	gap := g.source_span(g.source_end, limit) or { return }
	// Only the blank run directly after the comment counts: `limit` is a node
	// position, which for some statements starts past their first token.
	mut newlines := 0
	for c in gap {
		if c == `\n` {
			newlines++
		} else if c !in [` `, `\t`, `\r`] {
			break
		}
	}
	if newlines < 2 {
		return
	}
	g.write_blank_line()
}

// write_blank_line ends the current line and adds an empty one, unless the
// output already ends with a blank line.
fn (mut g Gen) write_blank_line() {
	if !g.on_newline {
		g.writeln('')
	}
	if g.out.len < 2 || g.out.last_n(2) != '\n\n' {
		g.out.writeln('')
		g.on_newline = true
	}
}

fn (g &Gen) source_has_line_break_between(start int, end int) bool {
	if gap := g.source_span(start, end) {
		return gap.contains('\n') || gap.contains('\r')
	}
	return false
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
	comment_i := g.comment_i
	g.emit_comments_before(stmt_start)
	if g.comment_i > comment_i {
		// keep a blank line the source had between the last comment and this
		// statement (e.g. a file header comment above `module`)
		g.emit_blank_line_after_comment(stmt_start)
	}
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
				for i, expr in exprs {
					g.expr(g.statement_expr_without_outer_parens(expr))
					if i < exprs.len - 1 {
						g.write(', ')
					}
				}
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
			g.writeln('\$dbg;')
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
				g.write(c_string_literal_text(n.value))
			} else {
				g.write(g.rune_literal(n))
			}
		}
		.string_literal {
			g.write(g.string_literal_text(n))
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
			g.infix_expr(id)
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
			g.paren_expr(id)
		}
		.call {
			g.call_expr(id)
		}
		.selector {
			receiver := g.a.child(n, 0)
			if g.should_rewrite_c_string_selector(n, receiver) {
				g.write('c')
				g.expr(receiver)
			} else {
				g.expr(receiver)
				if source := g.a.formatter_sources[int(id)] {
					// comptime method shorthand `recv.$method(args)`
					g.write('.${source}')
				} else if n.value == '$' {
					// comptime field access `recv.$(name_expr)`
					g.write('.\$(')
					g.expr(g.a.child(n, 1))
					g.write(')')
				} else {
					g.write('.${n.value}')
				}
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
		.comptime_if {
			g.comptime_if_expr(id)
		}
		.struct_init {
			g.struct_init(id)
		}
		.assoc {
			g.assoc(id)
		}
		.array_literal {
			g.array_literal(id)
		}
		.array_init {
			if n.typ.len > 0 {
				g.write(g.type_text(n.typ))
			} else if n.value.len > 0 {
				g.write('[]${g.type_text(n.value)}')
			}
			g.write('{')
			g.init_fields(g.a.children_of(n))
			g.indent++
			g.advance_source_end_before_pending_comment(n.pos.end)
			g.emit_comments_before(n.pos.end)
			g.indent--
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
			g.write(g.type_text(n.value))
			g.write('(')
			g.expr(g.a.child(n, 0))
			g.write(')')
		}
		.as_expr {
			g.expr(g.a.child(n, 0))
			g.write(' as ${g.type_text(n.value)}')
		}
		.is_expr {
			if g.a.child_node(n, 0).is_mut {
				g.write('mut ')
			}
			g.expr(g.a.child(n, 0))
			g.write(' is ${g.type_text(n.value)}')
		}
		.in_expr {
			g.expr(g.a.child(n, 0))
			g.write(' in ')
			g.expr(g.a.child(n, 1))
		}
		.range {
			g.expr(g.a.child(n, 0))
			g.write(if n.value == '...' { '...' } else { ' .. ' })
			if n.children_count > 1 {
				g.expr(g.a.child(n, 1))
			}
		}
		.spawn_expr {
			g.write(if n.value == 'go' { 'go ' } else { 'spawn ' })
			g.expr(g.a.child(n, 0))
		}
		.lock_expr {
			g.lock_expr(id)
		}
		.sizeof_expr {
			if source := g.a.formatter_sources[int(id)] {
				g.write(source.trim_space())
			} else {
				g.write('sizeof(${g.type_text(n.value)})')
			}
		}
		.typeof_expr {
			if n.value.len > 0 {
				g.write('typeof[${g.type_text(n.value)}]()')
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
	if (n.kind == .sql_expr || n.typ == '__v3_formatter_raw') && int(id) in g.a.formatter_sources {
		g.skip_comments_before(n.pos.end + 1)
	} else {
		g.emit_trailing_comments(n.pos.end)
	}
	g.source_end = int_max(g.source_end, n.pos.end)
}

// infix_expr prints a binary expression, keeping the line break the source had
// before a `&&`/`||` continuation instead of joining the operands into one long
// line.
fn (mut g Gen) infix_expr(id flat.NodeId) {
	n := g.a.node(id)
	lhs := g.a.child(n, 0)
	rhs := g.a.child(n, 1)
	g.expr(lhs)
	if g.infix_continues_on_next_line(n, lhs, rhs) {
		g.indent++
		g.writeln('')
		g.write('${op_str(n.op)} ')
		g.expr(rhs)
		g.indent--
		return
	}
	g.write(' ${op_str(n.op)} ')
	g.expr(rhs)
}

fn (g &Gen) infix_continues_on_next_line(n &flat.Node, lhs flat.NodeId, rhs flat.NodeId) bool {
	if n.op !in [.logical_and, .logical_or] {
		return false
	}
	lhs_end := g.rightmost_source_end(lhs)
	rhs_start := g.leftmost_source_start(rhs)
	if lhs_end < 0 || rhs_start < lhs_end || g.has_comment_between(lhs_end, rhs_start) {
		return false
	}
	return g.source_has_line_break_between(lhs_end, rhs_start)
}

// leftmost_source_start returns the first source offset covered by `id`, walking
// down its leading operands. A few nodes are synthesized around their operand
// (`!in` wraps `in` in a `!`, for instance) and carry the position of a later
// token, so the node's own span alone is not enough.
fn (g &Gen) leftmost_source_start(id flat.NodeId) int {
	mut current := id
	mut start := -1
	for int(current) >= 0 {
		n := g.a.node(current)
		if n.pos.is_valid() && (start < 0 || n.pos.offset < start) {
			start = n.pos.offset
		}
		if n.children_count == 0 {
			break
		}
		current = g.a.child(n, 0)
	}
	return start
}

// rightmost_source_end returns the last source offset covered by `id`, walking
// down its trailing operands. See [leftmost_source_start] for why the node's own
// span is not enough.
fn (g &Gen) rightmost_source_end(id flat.NodeId) int {
	mut current := id
	mut end := -1
	for int(current) >= 0 {
		n := g.a.node(current)
		if n.pos.is_valid() && n.pos.end > end {
			end = n.pos.end
		}
		if n.children_count == 0 {
			break
		}
		current = g.a.child(n, int(n.children_count) - 1)
	}
	return end
}

fn (g &Gen) innermost_parenthesized_expr(id flat.NodeId) flat.NodeId {
	mut current := id
	for int(current) >= 0 {
		n := g.a.node(current)
		if n.kind != .paren || n.value.len > 0 || n.children_count == 0 {
			break
		}
		current = g.a.child(n, 0)
	}
	return current
}

fn (g &Gen) statement_expr_without_outer_parens(id flat.NodeId) flat.NodeId {
	n := g.a.node(id)
	if n.kind != .paren || n.value.len > 0 || g.has_comment_between(n.pos.offset, n.pos.end) {
		return id
	}
	return g.innermost_parenthesized_expr(id)
}

fn (mut g Gen) paren_expr(id flat.NodeId) {
	n := g.a.node(id)
	if n.value in ['_likely_', '_unlikely_'] {
		g.write('${n.value}(')
		g.expr(g.a.child(n, 0))
		g.write(')')
		return
	}
	inner := g.innermost_parenthesized_expr(id)
	has_comments := g.has_comment_between(n.pos.offset, n.pos.end)
	if has_comments {
		g.writeln('(')
		g.indent++
		g.expr(inner)
		g.advance_source_end_before_pending_comment(n.pos.end)
		g.emit_comments_before(n.pos.end)
		if !g.on_newline {
			g.writeln('')
		}
		g.indent--
		g.write(')')
		return
	}
	requires_parens := g.a.node(inner).kind != .ident
	if requires_parens {
		g.write('(')
	}
	g.expr(inner)
	if requires_parens {
		g.write(')')
	}
}

fn (g &Gen) rune_literal(n &flat.Node) string {
	if source := g.source_span(n.pos.offset, n.pos.end) {
		literal := source.trim_space()
		if literal.len >= 2 && literal[0] == `\`` && literal[literal.len - 1] == `\`` {
			return literal
		}
	}
	return '`${escape_string(n.value, `\``)}`'
}

fn (mut g Gen) array_literal(id flat.NodeId) {
	n := g.a.node(id)
	if prefix := g.a.formatter_sources[int(id)] {
		g.write(prefix.trim_space())
	}
	g.write('[')
	children := g.a.children_of(n)
	if children.len == 0 {
		if g.comments_inside(n.pos.offset, n.pos.end) {
			g.writeln('')
			g.indent++
			g.emit_comments_before(n.pos.end)
			g.indent--
		}
		g.write(']')
		return
	}
	g.array_depth++
	first := g.a.node(children[0])
	// An array whose source starts on the line below `[` keeps that layout even
	// when an earlier array at this nesting depth was written on one line.
	source_break := g.source_line(first.pos.offset) > g.source_line(n.pos.offset)
	if g.array_depth > g.array_breaks.len {
		// The width heuristic is decided once per nesting depth, so a row of
		// small nested arrays stays uniform instead of breaking at whichever
		// element happens to cross the limit.
		first_width := g.array_expr_width(children[0])
		g.array_breaks << source_break
			|| (first_width > 0 && g.output_line_len() + first_width > formatter_array_first_break)
	}
	line_break := source_break || g.array_breaks[g.array_depth - 1]
	mut indented := false
	for i, child in children {
		if i == 0 {
			if line_break {
				g.writeln('')
				g.indent++
				indented = true
			}
		} else if line_break {
			g.writeln(',')
		} else {
			width := g.array_expr_width(child)
			current_len := g.output_line_len()
			if current_len > formatter_array_wrap_break
				|| (width > 0 && current_len + 2 + width > formatter_max_line_len) {
				g.writeln(',')
				if !indented {
					g.indent++
					indented = true
				}
			} else {
				g.write(', ')
			}
		}
		g.expr(child)
	}
	last := g.a.node(children.last())
	has_trailing_comments := g.has_comment_between(last.pos.end, n.pos.end)
	if line_break {
		g.writeln(',')
		if has_trailing_comments {
			g.advance_source_end_before_pending_comment(n.pos.end)
			g.emit_comments_before(n.pos.end)
		}
		g.indent--
	} else if has_trailing_comments {
		g.writeln(',')
		if !indented {
			g.indent++
		}
		g.advance_source_end_before_pending_comment(n.pos.end)
		g.emit_comments_before(n.pos.end)
		g.indent--
	} else if indented {
		g.indent--
	}
	g.write(']')
	g.array_depth--
	if g.array_depth == 0 {
		g.array_breaks.clear()
	}
}

fn (g &Gen) array_expr_width(id flat.NodeId) int {
	n := g.a.node(id)
	if n.kind in [.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal,
		.ident, .enum_val] {
		return n.value.len
	}
	if source := g.source_span(n.pos.offset, n.pos.end) {
		trimmed := source.trim_space()
		if !trimmed.contains('\n') {
			return trimmed.len
		}
	}
	return 0
}

fn (g &Gen) output_line_len() int {
	mut line_len := 0
	for i := g.out.len - 1; i >= 0; i-- {
		ch := g.out.byte_at(i)
		if ch == `\n` {
			break
		}
		line_len += if ch == `\t` { 4 } else { 1 }
	}
	return line_len
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
	if n.value.starts_with('__v3_formatter_isreftype:') && children.len == 2 {
		form := n.value.all_after('__v3_formatter_isreftype:')
		arg := g.a.node(children[1])
		if form == 'bracket' {
			g.write('isreftype[${g.type_text(arg.value)}]()')
			return
		}
		g.write('isreftype(')
		if form == 'type' {
			g.write(g.type_text(arg.value))
		} else {
			g.expr(children[1])
		}
		g.write(')')
		return
	}
	if kind := g.json_migration_call_kind(children[0]) {
		g.json_migration_call(kind, children[1..])
		return
	}
	g.expr(children[0])
	g.write('(')
	args := children[1..]
	if g.call_args_expanded(id, args) {
		g.expanded_call_args(id, args)
		g.write(')')
		return
	}
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

fn (g &Gen) call_args_expanded(id flat.NodeId, args []flat.NodeId) bool {
	if args.len == 0 {
		return false
	}
	if g.a.formatter_expanded_calls[int(id)] {
		return true
	}
	source := g.a.formatter_sources[int(id)] or { return false }
	if source.len < 2 || source[0] != `(` || source[source.len - 1] != `)` {
		return false
	}
	mut has_named_args := false
	for arg in args {
		if g.a.node(arg).kind == .field_init {
			has_named_args = true
			break
		}
	}
	if !has_named_args {
		return false
	}
	// The opening parenthesis is already present in the output line and in source.
	projected_width := g.output_line_len() + source.len - 1
	return source.contains('\n') || projected_width > formatter_max_line_len
}

fn call_args_start_on_new_line(source string) bool {
	for i in 1 .. source.len {
		match source[i] {
			` `, `\t`, `\r` { continue }
			`\n` { return true }
			else { return false }
		}
	}
	return false
}

fn (mut g Gen) expanded_call_args(id flat.NodeId, args []flat.NodeId) {
	mut first_named := args.len
	for i, arg in args {
		if g.a.node(arg).kind == .field_init {
			first_named = i
			break
		}
	}
	if first_named == args.len {
		g.writeln('')
		g.indent++
		g.expanded_regular_call_args(args)
		g.emit_comments_before(g.a.formatter_node_ends[int(id)] or { g.a.node(id).pos.end })
		g.indent--
		return
	}
	source := g.a.formatter_sources[int(id)] or { '' }
	if first_named == 0 || call_args_start_on_new_line(source) {
		g.writeln('')
		g.indent++
		g.expanded_regular_call_args(args[..first_named])
	} else {
		for i, arg in args[..first_named] {
			g.call_arg(arg)
			if i < first_named - 1 {
				g.write(', ')
			}
		}
		g.writeln(',')
		g.indent++
	}
	for arg in args[first_named..] {
		g.named_init_field(arg)
	}
	g.emit_comments_before(g.a.formatter_node_ends[int(id)] or { g.a.node(id).pos.end })
	g.indent--
}

fn (mut g Gen) expanded_regular_call_args(args []flat.NodeId) {
	for i, arg in args {
		g.call_arg(arg)
		g.write(',')
		g.emit_trailing_comments(g.a.node(arg).pos.end)
		if g.on_newline {
			continue
		}
		if i == args.len - 1
			|| g.source_line(g.a.node(args[i + 1]).pos.offset) > g.source_line(g.a.node(arg).pos.end) {
			g.writeln('')
		} else {
			g.write(' ')
		}
	}
}

fn (mut g Gen) call_arg(id flat.NodeId) {
	if g.a.node(id).is_mut {
		g.write('mut ')
	}
	g.expr(id)
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
	gate := if n.op == .gated_index { '#' } else { '' }
	if n.value == 'range' {
		g.write('${gate}[')
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
	g.write('${gate}[')
	g.expr_list(children[1..], ', ')
	g.write(']')
}

fn (mut g Gen) struct_init(id flat.NodeId) {
	n := g.a.node(id)
	if source := g.a.formatter_sources[int(id)] {
		g.write(source.trim_space())
		g.skip_comments_before(n.pos.end)
		g.source_end = int_max(g.source_end, n.pos.end)
		return
	}
	fields := g.a.children_of(n)
	g.write(g.type_text(n.value))
	if fields.len == 0 {
		g.write('{')
		if g.comments_inside(n.pos.offset, n.pos.end) {
			g.writeln('')
			g.indent++
			g.emit_comments_before(n.pos.end)
			g.indent--
		}
		g.write('}')
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
			g.named_init_field(fid)
		}
		g.emit_comments_before(n.pos.end)
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
		last := g.a.child_node(g.a.node(fields.last()), 0)
		if g.has_comment_between(last.pos.end, n.pos.end) {
			g.write(',')
			g.indent++
			g.advance_source_end_before_pending_comment(n.pos.end)
			g.emit_comments_before(n.pos.end)
			g.indent--
		}
		g.write('}')
	}
}

fn (mut g Gen) named_init_field(id flat.NodeId) {
	f := g.a.node(id)
	value := g.a.child(f, 0)
	v := g.a.node(value)
	g.emit_comments_before(v.pos.offset)
	g.write('${f.value}: ')
	g.expr(value)
	g.emit_trailing_comments(v.pos.end)
	if !g.on_newline {
		g.writeln('')
	}
	g.source_end = int_max(g.source_end, v.pos.end)
}

fn (mut g Gen) assoc(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	if g.source_line(n.pos.offset) == g.source_line(n.pos.end)
		&& !g.comments_inside(n.pos.offset, n.pos.end) {
		g.write('${g.type_text(n.value)}{ ...')
		if children.len > 0 {
			g.expr(children[0])
		}
		for fid in children[1..] {
			f := g.a.node(fid)
			g.write(', ${f.value}: ')
			g.expr(g.a.child(f, 0))
		}
		g.write(' }')
		return
	}
	g.write(g.type_text(n.value))
	g.writeln('{')
	g.indent++
	if children.len > 0 {
		g.emit_comments_before(g.a.node(children[0]).pos.offset)
	}
	g.write('...')
	if children.len > 0 {
		g.expr(children[0])
		g.emit_trailing_comments(g.a.node(children[0]).pos.end)
	}
	if !g.on_newline {
		g.writeln('')
	}
	for fid in children[1..] {
		g.named_init_field(fid)
	}
	g.advance_source_end_before_pending_comment(n.pos.end)
	g.emit_comments_before(n.pos.end)
	g.indent--
	g.write('}')
}

fn (mut g Gen) init_fields(ids []flat.NodeId) {
	for i, fid in ids {
		f := g.a.node(fid)
		if f.value.len > 0 {
			g.write('${f.value}: ')
		}
		was_in_array_init := g.in_array_init
		g.in_array_init = was_in_array_init || f.value == 'init'
		g.expr(g.a.child(f, 0))
		g.in_array_init = was_in_array_init
		if i < ids.len - 1 {
			g.write(', ')
		}
	}
}

fn (mut g Gen) map_init(id flat.NodeId) {
	n := g.a.node(id)
	if n.value.len > 0 {
		g.write(g.type_text(n.value))
	}
	children := g.a.children_of(n)
	if children.len == 0 {
		g.write('{')
		if g.source_line(n.pos.offset) < g.source_line(n.pos.end) {
			g.writeln('')
			g.indent++
			g.emit_comments_before(n.pos.end)
			g.indent--
		}
		g.write('}')
		return
	}
	g.writeln('{')
	g.indent++
	mut max_key_width := 0
	mut i := 0
	for i + 1 < children.len {
		width := g.map_key_width(children[i])
		if width > max_key_width {
			max_key_width = width
		}
		i += 2
	}
	i = 0
	for i + 1 < children.len {
		g.expr(children[i])
		g.write(': ')
		width := g.map_key_width(children[i])
		if width < max_key_width {
			g.write(' '.repeat(max_key_width - width))
		}
		g.expr(children[i + 1])
		if !g.on_newline {
			g.writeln('')
		}
		i += 2
	}
	g.emit_comments_before(n.pos.end)
	g.indent--
	g.write('}')
}

fn (g &Gen) map_key_width(id flat.NodeId) int {
	n := g.a.node(id)
	text := match n.kind {
		.string_literal {
			g.string_literal_text(n)
		}
		.char_literal {
			if n.value.starts_with('c:') {
				c_string_literal_text(n.value)
			} else {
				g.rune_literal(n)
			}
		}
		.enum_val {
			'.${n.value}'
		}
		.int_literal, .float_literal, .bool_literal, .ident {
			n.value
		}
		else {
			if source := g.source_span(n.pos.offset, n.pos.end) {
				source.trim_space()
			} else {
				n.value
			}
		}
	}
	return utf8_str_visible_length(text)
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
	// V spells a closure `fn [captures] [T](params)`: the capture list comes first, and the
	// generic list binds straight to the parameters with no space. Writing the generic list
	// first produced `fn[T] [captures] (params)`, which does not parse, and the run after that
	// read the captures as the generic list and dropped everything the two lists disagreed on.
	if gp.len > 0 {
		g.write(' [${gp.join(', ')}]')
	} else {
		g.write(' ')
	}
	g.params(id, params)
	if n.typ.len > 0 && n.typ != 'void' {
		g.write(' ${g.type_text(n.typ)}')
	}
	if body.len == 0 && g.empty_braced_body_is_compact(n, n.pos.end)
		&& !g.has_comment_between(n.pos.offset, n.pos.end) {
		g.write(' {}')
		return
	}
	g.writeln(' {')
	g.stmt_list_ids(body)
	g.indent++
	g.emit_comments_before(n.pos.end)
	g.indent--
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
	if children.len <= 1 {
		g.write(' or {}')
		return
	}
	blk := g.a.node(children[1])
	stmts := g.a.children_of(blk)
	is_compact := stmts.len <= 1 && g.source_block_is_compact(blk)
		&& !g.has_comment_between(blk.pos.offset, blk.pos.end)
	if is_compact {
		g.write(' or {')
		if stmts.len > 0 {
			g.write(' ')
			g.compact_stmt(stmts[0])
			g.write(' ')
		}
		g.write('}')
		return
	}
	g.writeln(' or {')
	g.stmt_list_ids(stmts)
	g.indent++
	g.advance_source_end_before_pending_comment(blk.pos.end)
	g.emit_comments_before(blk.pos.end)
	g.indent--
	g.write('}')
}

fn (mut g Gen) compact_stmt(id flat.NodeId) {
	in_init := g.in_init
	g.in_init = true
	g.stmt(id)
	g.in_init = in_init
	if g.on_newline && g.out.len > 0 && g.out.last_n(1) == '\n' {
		g.out.go_back(1)
		g.on_newline = false
	}
}

fn (mut g Gen) block_expr(id flat.NodeId) {
	n := g.a.node(id)
	stmts := g.a.children_of(n)
	if n.value == 'comma_exprs' {
		// `a, b` in expression position is a comma group, not a `{}` block.
		g.comma_exprs(n)
		return
	}
	prefix := if n.value == 'unsafe' { 'unsafe ' } else { '' }
	is_compact := if n.value == 'unsafe' {
		stmts.len <= 1 && g.source_block_is_compact(n)
			&& !g.has_comment_between(n.pos.offset, n.pos.end)
	} else {
		g.in_init || stmts.len <= 1
	}
	if is_compact {
		g.write('${prefix}{')
		has_padding := stmts.len > 0 || n.value != 'unsafe'
		if has_padding {
			g.write(' ')
		}
		in_init := g.in_init
		g.in_init = true
		for s in stmts {
			g.stmt(s)
		}
		g.in_init = in_init
		if has_padding {
			g.write(' ')
		}
		g.write('}')
	} else {
		g.writeln('${prefix}{')
		g.stmt_list_ids(stmts)
		g.indent++
		g.advance_source_end_before_pending_comment(n.pos.end)
		g.emit_comments_before(n.pos.end)
		g.indent--
		g.write('}')
	}
}

// comma_exprs prints the children of a synthetic `comma_exprs` block as the
// comma separated expression list they were parsed from.
fn (mut g Gen) comma_exprs(n &flat.Node) {
	mut first := true
	for stmt_id in g.a.children_of(n) {
		stmt := g.a.node(stmt_id)
		if !first {
			g.write(', ')
		}
		first = false
		if stmt.kind == .expr_stmt && stmt.children_count == 1 {
			g.expr(g.a.child(stmt, 0))
		} else {
			in_init := g.in_init
			g.in_init = true
			g.stmt(stmt_id)
			g.in_init = in_init
		}
	}
}

fn (g &Gen) source_block_is_compact(n &flat.Node) bool {
	if source := g.source_span(n.pos.offset, n.pos.end) {
		return !source.contains('\n') && !source.contains('\r')
	}
	return false
}

fn (mut g Gen) block_stmt(id flat.NodeId) {
	n := g.a.node(id)
	stmts := g.a.children_of(n)
	if n.value == 'for_c_style_multi' && stmts.len == 2 {
		// Compiler lowering scopes a multi-variable initializer through a synthetic
		// block; source formatting puts it back in the loop header.
		init := g.a.node(stmts[0])
		loop := g.a.node(stmts[1])
		if init.kind in [.assign, .decl_assign] && loop.kind == .for_stmt {
			g.for_stmt_with_init(stmts[1], stmts[0])
			return
		}
	}
	if n.value == 'comma_exprs' {
		// The parser groups `a, b` statements (e.g. the value of an `if`
		// expression branch) in a synthetic block; print the original commas.
		g.comma_exprs(n)
		if !g.in_init {
			g.writeln('')
		}
		return
	}
	prefix := if n.value == 'unsafe' { 'unsafe ' } else { '' }
	if n.value == 'unsafe' && stmts.len <= 1 && g.source_block_is_compact(n)
		&& !g.has_comment_between(n.pos.offset, n.pos.end) {
		g.write('${prefix}{')
		if stmts.len > 0 {
			g.write(' ')
			in_init := g.in_init
			g.in_init = true
			g.stmt(stmts[0])
			g.in_init = in_init
			g.write(' ')
		}
		g.writeln('}')
		return
	}
	g.writeln('${prefix}{')
	g.stmt_list_ids(stmts)
	g.indent++
	g.advance_source_end_before_pending_comment(n.pos.end)
	g.emit_comments_before(n.pos.end)
	g.indent--
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
	if n.value.starts_with('lock_modes:') {
		modes := n.value.all_after('lock_modes:')
		mut has_write := false
		mut has_read := false
		for i in 0 .. objs.len {
			if i < modes.len && modes[i] == `r` {
				has_read = true
			} else {
				has_write = true
			}
		}
		if has_write {
			g.write('lock ')
			mut wrote := false
			for i, obj in objs {
				if i < modes.len && modes[i] == `r` {
					continue
				}
				if wrote {
					g.write(', ')
				}
				g.expr(obj)
				wrote = true
			}
		}
		if has_read {
			if has_write {
				g.write('; ')
			}
			g.write('rlock ')
			mut wrote := false
			for i, obj in objs {
				if i >= modes.len || modes[i] != `r` {
					continue
				}
				if wrote {
					g.write(', ')
				}
				g.expr(obj)
				wrote = true
			}
		}
	} else {
		kw := if n.value.starts_with('rlock') { 'rlock' } else { 'lock' }
		g.write(kw)
		if objs.len > 0 {
			g.write(' ')
			g.expr_list(objs, ', ')
		}
	}
	g.writeln(' {')
	body_node := g.a.node(body)
	g.stmt_list_ids(g.a.children_of(body_node))
	g.indent++
	g.advance_source_end_before_pending_comment(body_node.pos.end)
	g.emit_comments_before(body_node.pos.end)
	g.indent--
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

// interp_text_spans_lines reports whether the literal text of the interpolated
// string written as `source` contains a physical line break. Segment values are
// stored decoded, so this has to work on the source: a `\n` escape is not a
// physical break. Everything inside `${...}` is skipped too, since a break there
// only wraps the embedded expression.
fn interp_text_spans_lines(source string) bool {
	mut i := 0
	mut depth := 0
	for i < source.len {
		c := source[i]
		if c == `\\` {
			i += 2
			continue
		}
		if depth == 0 {
			if c == `\n` || c == `\r` {
				return true
			}
			if c == `$` && i + 1 < source.len && source[i + 1] == `{` {
				depth = 1
				i += 2
				continue
			}
			i++
			continue
		}
		// inside `${...}`: step over nested literals so that a brace of their
		// own does not throw off the depth count
		if c in [`'`, `"`, `\``] {
			i = interp_skip_nested_literal(source, i)
			continue
		}
		if c == `{` {
			depth++
		} else if c == `}` {
			depth--
		}
		i++
	}
	return false
}

// interp_skip_nested_literal returns the offset just past the string or rune
// literal opening at `start` inside an interpolated expression.
fn interp_skip_nested_literal(source string, start int) int {
	quote := source[start]
	mut i := start + 1
	for i < source.len {
		if source[i] == `\\` {
			i += 2
			continue
		}
		if source[i] == quote {
			return i + 1
		}
		i++
	}
	return source.len
}

fn (mut g Gen) string_interp(id flat.NodeId) {
	n := g.a.node(id)
	if source := g.source_span(n.pos.offset, n.pos.end) {
		// Physical newlines are part of a multiline literal's value and layout,
		// so keep such a literal exactly as written instead of re-escaping them
		// to `\n`. The comments of any embedded expression are copied along with
		// it, so they must not be emitted a second time later on.
		if interp_text_spans_lines(source) {
			g.write(source)
			g.skip_comments_before(n.pos.end)
			return
		}
	}
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
	if n.typ.starts_with('js:') {
		g.write('js')
	}
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
	opstr := if is_decl {
		':='
	} else {
		g.a.formatter_assignment_ops[int(id)] or { op_str(n.op) }
	}
	modifier, count := parse_assign_meta(n.value)
	if modifier == 'atomic' {
		g.write('atomic ')
	} else if n.is_mut && modifier in ['static', 'volatile'] && !g.suppress_mut {
		g.write('mut ${modifier} ')
	} else if modifier.len > 0 {
		g.write('${modifier} ')
		if n.is_mut && !g.suppress_mut {
			g.write('mut ')
		}
	} else if n.is_mut && !g.suppress_mut {
		g.write('mut ')
	}
	if count <= 1 {
		if is_decl && !n.is_mut && g.a.node(children[0]).is_mut && !g.suppress_mut {
			g.write('mut ')
		}
		g.expr(children[0])
		g.assign_rhs(opstr, [children[0]], children[1..])
	} else {
		mut lhs := []flat.NodeId{}
		mut rhs := []flat.NodeId{}
		rhs_count := children.len - count
		mut child_index := 0
		for i in 0 .. count {
			if child_index >= children.len {
				break
			}
			lhs << children[child_index]
			child_index++
			if i < rhs_count && child_index < children.len {
				rhs << children[child_index]
				child_index++
			}
		}
		for i, lhs_id in lhs {
			if is_decl && g.a.node(lhs_id).is_mut && (!n.is_mut || i > 0) && !g.suppress_mut {
				g.write('mut ')
			}
			g.expr(lhs_id)
			if i < lhs.len - 1 {
				g.write(', ')
			}
		}
		g.assign_rhs(opstr, lhs, rhs)
	}
	if attr := g.a.formatter_sources[int(id)] {
		g.write(' ${attr.trim_space()}')
	}
	if !g.in_init && !g.on_newline {
		g.writeln('')
	}
}

// assign_rhs prints the assignment operator and the right hand side, keeping the
// right hand side on its own line when the source wrapped it there.
fn (mut g Gen) assign_rhs(opstr string, lhs []flat.NodeId, rhs []flat.NodeId) {
	if g.assign_rhs_on_next_line(lhs, rhs) {
		g.write(' ${opstr}')
		g.indent++
		g.writeln('')
		g.expr_list(rhs, ', ')
		g.indent--
		return
	}
	g.write(' ${opstr} ')
	g.expr_list(rhs, ', ')
}

fn (g &Gen) assign_rhs_on_next_line(lhs []flat.NodeId, rhs []flat.NodeId) bool {
	if g.in_init || lhs.len == 0 || rhs.len == 0 {
		return false
	}
	lhs_end := g.rightmost_source_end(lhs.last())
	rhs_start := g.leftmost_source_start(rhs[0])
	if lhs_end < 0 || rhs_start < lhs_end || g.has_comment_between(lhs_end, rhs_start) {
		return false
	}
	return g.source_has_line_break_between(lhs_end, rhs_start)
}

fn (mut g Gen) flow_control(kw string, label string) {
	if label.len > 0 {
		g.writeln('${kw} ${label}')
	} else {
		g.writeln(kw)
	}
}

fn (mut g Gen) for_stmt(id flat.NodeId) {
	g.for_stmt_with_init(id, flat.empty_node)
}

fn (mut g Gen) for_stmt_with_init(id flat.NodeId, init_override flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	init := if init_override != flat.empty_node {
		init_override
	} else if children.len > 0 {
		children[0]
	} else {
		flat.empty_node
	}
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
			g.for_post_clause(post)
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
	g.indent++
	g.advance_source_end_before_pending_comment(n.pos.end)
	g.emit_comments_before(n.pos.end)
	g.indent--
	g.writeln('}')
}

fn (mut g Gen) for_post_clause(id flat.NodeId) {
	n := g.a.node(id)
	if n.kind != .block {
		g.stmt(id)
		return
	}
	// Comma-separated post expressions are represented as a synthetic block.
	for i, stmt_id in g.a.children_of(n) {
		stmt := g.a.node(stmt_id)
		if stmt.kind == .expr_stmt && stmt.children_count > 0 {
			g.expr(g.a.child(stmt, 0))
		} else {
			g.stmt(stmt_id)
		}
		if i < int(n.children_count) - 1 {
			g.write(', ')
		}
	}
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
	mutability := g.a.formatter_for_in_mut[int(id)] or { u8(0) }
	first_is_mut := if mutability > 0 {
		mutability & 1 != 0
	} else {
		g.is_empty(v1) && mut_val
	}
	second_is_mut := if mutability > 0 {
		mutability & 2 != 0
	} else {
		!g.is_empty(v1) && mut_val
	}
	g.write('for ')
	if g.is_empty(v1) {
		if first_is_mut {
			g.write('mut ')
		}
		g.expr(v0)
	} else {
		if first_is_mut {
			g.write('mut ')
		}
		g.expr(v0)
		g.write(', ')
		if second_is_mut {
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
	g.indent++
	g.advance_source_end_before_pending_comment(n.pos.end)
	g.emit_comments_before(n.pos.end)
	g.indent--
	g.writeln('}')
}

fn (mut g Gen) if_expr(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	if children.len < 2 {
		return
	}
	start_line_len := g.output_line_len()
	is_compact := g.if_expr_is_compact(n, children, start_line_len)
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
	if is_compact {
		g.compact_expr_block(children[1])
	} else {
		g.writeln('{')
		g.source_end = int_max(g.source_end, then_blk.pos.offset)
		g.stmt_list_ids(g.a.children_of(then_blk))
		g.indent++
		g.emit_comments_before(then_blk.pos.end)
		g.indent--
		g.write('}')
	}
	if children.len > 2 {
		else_id := children[2]
		en := g.a.node(else_id)
		if en.kind == .if_expr {
			g.write(' else ')
			g.if_expr(else_id)
		} else if is_compact {
			g.write(' else ')
			g.compact_expr_block(else_id)
		} else {
			g.writeln(' else {')
			g.source_end = int_max(g.source_end, en.pos.offset)
			g.stmt_list_ids(g.a.children_of(en))
			g.indent++
			g.emit_comments_before(en.pos.end)
			g.indent--
			g.write('}')
		}
	}
}

fn (g &Gen) if_expr_is_compact(n &flat.Node, children []flat.NodeId, start_line_len int) bool {
	if children.len !in [2, 3] || g.has_comment_between(n.pos.offset, n.pos.end) {
		return false
	}
	if source := g.source_span(n.pos.offset, n.pos.end) {
		if source.contains('\n') || source.contains('\r')
			|| start_line_len + source.trim_space().len > formatter_max_line_len {
			return false
		}
	} else {
		return false
	}
	if g.compact_block_expr_ids(children[1]) == none {
		return false
	}
	return children.len == 2
		|| (g.a.node(children[2]).kind == .block && g.compact_block_expr_ids(children[2]) != none)
}

fn (g &Gen) compact_block_expr_ids(id flat.NodeId) ?[]flat.NodeId {
	n := g.a.node(id)
	if n.kind != .block {
		return none
	}
	return g.compact_expr_ids(g.a.children_of(n))
}

fn (mut g Gen) compact_expr_block(id flat.NodeId) {
	n := g.a.node(id)
	expressions := g.compact_block_expr_ids(id) or { []flat.NodeId{} }
	g.write('{')
	if expressions.len > 0 {
		g.write(' ')
		in_init := g.in_init
		g.in_init = true
		g.expr_list(expressions, ', ')
		g.in_init = in_init
		g.write(' ')
	}
	g.write('}')
	g.source_end = int_max(g.source_end, n.pos.end)
}

fn (mut g Gen) match_node(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	if children.len == 0 {
		return
	}
	g.write('match ')
	subject := g.a.node(children[0])
	if subject.is_mut {
		g.write('mut ')
	}
	in_init := g.in_init
	g.in_init = true
	g.expr(children[0])
	g.in_init = in_init
	g.writeln(' {')
	g.indent++
	for bid in children[1..] {
		b := g.a.node(bid)
		g.emit_comments_before(b.pos.offset)
		g.source_end = int_max(g.source_end, b.pos.offset)
		bchildren := g.a.children_of(b)
		if b.value == 'else' {
			if g.match_branch_is_compact(b, bchildren) {
				g.write('else ')
				g.compact_match_branch(b, bchildren)
				g.writeln('')
			} else {
				g.write('else')
				g.writeln(' {')
				g.stmt_list_ids(bchildren)
				g.indent++
				g.advance_source_end_before_pending_comment(b.pos.end)
				g.emit_comments_before(b.pos.end)
				g.indent--
				g.write('}')
				// A comment sitting after the branch's closing brace belongs to that branch.
				// Left for the next branch's leading-comment pass it would be re-emitted on its
				// own line, and the following run would then read it as a commented branch and
				// insert a blank line before it, so formatting twice differed from once.
				g.emit_trailing_comments(b.pos.end)
				if !g.on_newline {
					g.writeln('')
				}
			}
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
			if g.match_branch_is_compact(b, rest) {
				g.write(' ')
				g.compact_match_branch(b, rest)
				g.writeln('')
			} else {
				g.writeln(' {')
				g.stmt_list_ids(rest)
				g.indent++
				g.advance_source_end_before_pending_comment(b.pos.end)
				g.emit_comments_before(b.pos.end)
				g.indent--
				g.write('}')
				// A comment sitting after the branch's closing brace belongs to that branch.
				// Left for the next branch's leading-comment pass it would be re-emitted on its
				// own line, and the following run would then read it as a commented branch and
				// insert a blank line before it, so formatting twice differed from once.
				g.emit_trailing_comments(b.pos.end)
				if !g.on_newline {
					g.writeln('')
				}
			}
		}
	}
	g.indent--
	g.write('}')
}

fn (g &Gen) match_branch_is_compact(branch &flat.Node, body []flat.NodeId) bool {
	if g.has_comment_between(branch.pos.offset, branch.pos.end) {
		return false
	}
	if source := g.source_span(branch.pos.offset, branch.pos.end) {
		if source.contains('\n') || source.contains('\r')
			|| g.indent * 4 + source.trim_space().len > formatter_max_line_len {
			return false
		}
	} else {
		return false
	}
	return g.compact_expr_ids(body) != none
}

fn (g &Gen) compact_expr_ids(ids []flat.NodeId) ?[]flat.NodeId {
	if ids.len == 0 {
		return []flat.NodeId{}
	}
	if ids.len != 1 {
		return none
	}
	stmt := g.a.node(ids[0])
	if stmt.kind == .expr_stmt && stmt.children_count == 1 {
		return [g.a.child(stmt, 0)]
	}
	if stmt.kind != .block {
		return none
	}
	mut expressions := []flat.NodeId{}
	for stmt_id in g.a.children_of(stmt) {
		child := g.a.node(stmt_id)
		if child.kind != .expr_stmt || child.children_count != 1 {
			return none
		}
		expressions << g.a.child(child, 0)
	}
	return expressions
}

fn (mut g Gen) compact_match_branch(branch &flat.Node, body []flat.NodeId) {
	expressions := g.compact_expr_ids(body) or { []flat.NodeId{} }
	g.write('{')
	if expressions.len > 0 {
		g.write(' ')
		in_init := g.in_init
		g.in_init = true
		g.expr_list(expressions, ', ')
		g.in_init = in_init
		g.write(' ')
	}
	g.write('}')
	g.source_end = int_max(g.source_end, branch.pos.end)
}

fn (mut g Gen) defer_stmt(id flat.NodeId) {
	n := g.a.node(id)
	prefix := if n.value == 'function' { 'defer(fn) {' } else { 'defer {' }
	mut stmts := []flat.NodeId{}
	if n.children_count > 0 {
		stmts = g.a.children_of(g.a.child_node(n, 0))
	}
	is_compact := stmts.len <= 1 && g.source_block_is_compact(n)
		&& !g.has_comment_between(n.pos.offset, n.pos.end)
	if is_compact {
		g.write(prefix)
		if stmts.len > 0 {
			g.write(' ')
			in_init := g.in_init
			g.in_init = true
			g.stmt(stmts[0])
			g.in_init = in_init
			g.write(' ')
		}
		g.writeln('}')
		return
	}
	g.writeln(prefix)
	g.stmt_list_ids(stmts)
	g.indent++
	g.advance_source_end_before_pending_comment(n.pos.end)
	g.emit_comments_before(n.pos.end)
	g.indent--
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
	g.expr(g.statement_expr_without_outer_parens(children[0]))
	g.in_init = in_init
	if children.len > 1 {
		g.write(', ')
		g.expr(children[1])
	}
	// The condition can carry the statement's trailing comment, which ends the line itself.
	// Terminating again would leave a stray separator after every commented `assert`.
	if !g.in_init && !g.on_newline {
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
		g.indent++
		g.advance_source_end_before_pending_comment(then_blk.pos.end)
		g.emit_comments_before(then_blk.pos.end)
		g.indent--
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
			g.indent++
			g.advance_source_end_before_pending_comment(el.pos.end)
			g.emit_comments_before(el.pos.end)
			g.indent--
			g.write('}')
		}
	}
}

fn (mut g Gen) comptime_if_expr(id flat.NodeId) {
	n := g.a.node(id)
	children := g.a.children_of(n)
	g.write('\$if ${n.value.trim_space()} ')
	if children.len == 0 {
		g.write('{}')
		return
	}
	g.comptime_if_expr_branch(children[0])
	if children.len > 1 {
		el := g.a.node(children[1])
		g.write(' \$else ')
		if el.kind == .comptime_if {
			g.comptime_if_expr(children[1])
		} else {
			g.comptime_if_expr_branch(children[1])
		}
	}
}

fn (mut g Gen) comptime_if_expr_branch(id flat.NodeId) {
	n := g.a.node(id)
	if n.kind == .block {
		g.writeln('{')
		g.stmt_list_ids(g.a.children_of(n))
		g.indent++
		g.advance_source_end_before_pending_comment(n.pos.end)
		g.emit_comments_before(n.pos.end)
		g.indent--
		g.write('}')
		return
	}
	g.write('{ ')
	g.expr(id)
	g.write(' }')
}

fn (mut g Gen) comptime_for(id flat.NodeId) {
	n := g.a.node(id)
	parts := n.value.split('|')
	loopvar := if parts.len > 0 { parts[0] } else { 'x' }
	kind := if parts.len > 1 { parts[1] } else { 'fields' }
	g.write('\$for ${loopvar} in ${g.type_text(n.typ)}.${kind} {')
	g.writeln('')
	if n.children_count > 0 {
		blk := g.a.child_node(n, 0)
		g.stmt_list_ids(g.a.children_of(blk))
		g.indent++
		g.advance_source_end_before_pending_comment(blk.pos.end)
		g.emit_comments_before(blk.pos.end)
		g.indent--
	}
	g.write('}')
}

fn (g &Gen) should_rewrite_c_string_selector(n &flat.Node, receiver flat.NodeId) bool {
	if formatter_backend_is_js(g.backend) || n.value != 'str'
		|| g.a.node(receiver).kind != .string_literal {
		return false
	}
	file := g.a.source_files[g.file_id] or { return false }
	return !file.name.ends_with('.js.v')
		&& !file.name.ends_with(os.join_path('v', 'gen', 'js', 'tests', 'js.v'))
}

fn formatter_backend_is_js(backend string) bool {
	return backend in ['js', 'js_node', 'js_browser', 'js_freestanding']
}

fn (mut g Gen) select_stmt(id flat.NodeId) {
	n := g.a.node(id)
	g.writeln('select {')
	g.indent++
	for bid in g.a.children_of(n) {
		b := g.a.node(bid)
		g.emit_comments_before(b.pos.offset)
		g.source_end = int_max(g.source_end, b.pos.offset)
		bchildren := g.a.children_of(b)
		if b.value == 'else' {
			g.write('else')
			g.writeln(' {')
			g.stmt_list_ids(bchildren)
			g.indent++
			g.emit_comments_before(b.pos.end)
			g.indent--
			g.writeln('}')
			g.source_end = int_max(g.source_end, b.pos.end)
			continue
		}
		ncond := select_branch_cond_count(b.value)
		conds := if ncond <= bchildren.len { bchildren[..ncond] } else { bchildren }
		rest := if ncond <= bchildren.len { bchildren[ncond..] } else { []flat.NodeId{} }
		g.select_branch_header(b.value, conds)
		g.writeln(' {')
		g.stmt_list_ids(rest)
		g.indent++
		g.emit_comments_before(b.pos.end)
		g.indent--
		g.writeln('}')
		g.source_end = int_max(g.source_end, b.pos.end)
	}
	g.emit_comments_before(n.pos.end)
	g.indent--
	g.write('}')
}

// match_cond renders a match branch condition. A type pattern such as `[]T` or
// `map[K]V` is parsed as an empty composite-literal node; emit it as a bare type
// (without the `{}`) so it is not mistaken for the branch body.
fn (mut g Gen) match_cond(id flat.NodeId) {
	n := g.a.node(id)
	if n.kind == .array_init && n.children_count == 0 && n.typ.len > 0 {
		g.write(g.type_text(n.typ))
		return
	}
	if n.kind == .map_init && n.children_count == 0 && n.value.len > 0 {
		g.write(g.type_text(n.value))
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
		first := g.a.node(symbols[0])
		is_expanded := first.pos.is_valid()
			&& g.source_line(first.pos.offset) > g.source_line(n.pos.offset)
		if is_expanded {
			g.writeln(' {')
			g.indent++
			for symbol in symbols {
				s := g.a.node(symbol)
				g.emit_comments_before(s.pos.offset)
				g.write(s.value)
				g.write(',')
				g.emit_trailing_comments(s.pos.end)
				if !g.on_newline {
					g.writeln('')
				}
				g.source_end = int_max(g.source_end, s.pos.end)
			}
			g.advance_source_end_before_pending_comment(n.pos.end)
			g.emit_comments_before(n.pos.end)
			g.indent--
			g.write('}')
		} else {
			g.write(' { ')
			g.expr_list(symbols, ', ')
			g.write(' }')
		}
	}
	g.writeln('')
}

fn (mut g Gen) fn_decl(id flat.NodeId) {
	n := g.a.node(id)
	was_in_c_function := g.in_c_function
	g.in_c_function = n.kind == .c_fn_decl && !n.value.starts_with('JS:')
		&& !n.value.starts_with('V:')
	defer {
		g.in_c_function = was_in_c_function
	}
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
		mut receiver_type := g.type_text(g.receiver_type(rn))
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
		} else if name.starts_with('C:') {
			g.write('C.${name[2..]}')
		} else if name.starts_with('V:') {
			g.write(name[2..])
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
	g.params(id, params)
	if n.typ.len > 0 && n.typ != 'void' {
		g.write(' ${g.type_text(n.typ)}')
	}
	if n.kind == .c_fn_decl {
		g.writeln('')
		return
	}
	formatter_end := g.a.formatter_node_ends[int(id)] or { n.pos.end }
	if body.len == 0 && g.empty_braced_body_is_compact(n, formatter_end)
		&& !g.has_comment_between(n.pos.offset, formatter_end) {
		g.writeln(' {}')
		return
	}
	g.writeln(' {')
	g.stmt_list_ids(body)
	g.indent++
	g.emit_comments_before(formatter_end)
	g.indent--
	g.writeln('}')
}

fn (g &Gen) empty_braced_body_is_compact(n &flat.Node, end int) bool {
	if source := g.source_span(n.pos.offset, end) {
		close_pos := source.last_index_u8(`}`)
		if close_pos < 0 {
			return false
		}
		open_pos := source[..close_pos].last_index_u8(`{`)
		if open_pos < 0 {
			return false
		}
		body := source[open_pos + 1..close_pos]
		return !body.contains('\n') && !body.contains('\r')
	}
	return false
}

fn (mut g Gen) params(parent_id flat.NodeId, ids []flat.NodeId) {
	g.write('(')
	g.indent++
	parent := g.a.node(parent_id)
	mut previous_end := parent.pos.offset
	for i, pid in ids {
		p := g.a.node(pid)
		line_break := g.source_has_line_break_between(previous_end, p.pos.offset)
		g.emit_comments_before(p.pos.offset)
		if !g.on_newline {
			if line_break {
				g.writeln('')
			} else if i > 0 {
				g.write(' ')
			}
		}
		mut typ := g.type_text(g.param_type(p))
		if p.is_mut {
			g.write('mut ')
		}
		if typ.starts_with('shared ') {
			g.write('shared ')
			typ = typ[7..]
		}
		if typ.starts_with('atomic ') {
			g.write('atomic ')
			typ = typ[7..]
		}
		if p.value.len > 0 {
			g.write(p.value)
			g.write(' ')
		}
		g.write(typ)
		if i < ids.len - 1 {
			g.write(',')
		}
		param_end := g.a.formatter_node_ends[int(pid)] or { p.pos.end }
		g.source_end = int_max(g.source_end, param_end)
		g.emit_trailing_comments(param_end)
		previous_end = param_end
	}
	param_list_end := g.a.formatter_param_list_end[int(parent_id)] or { 0 }
	g.emit_comments_before(param_list_end)
	if !g.on_newline && g.source_has_line_break_between(previous_end, param_list_end) {
		g.writeln('')
	}
	g.indent--
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
	g.write(g.type_text(n.value))
	gp := n.generic_params()
	if gp.len > 0 {
		g.write('[${gp.join(', ')}]')
	}
	impls := tag_value(tags, 'implements')
	if impls.len > 0 {
		g.write(' implements ${impls.replace('|', ', ')}')
	}
	g.struct_fields(g.a.children_of(n), n.pos.end)
}

fn (mut g Gen) struct_fields(fields []flat.NodeId, end int) {
	if fields.len > 0 {
		g.writeln(' {')
	} else {
		g.write(' {')
	}
	g.indent++
	alignments := g.aggregate_field_alignments(fields, false)
	mut cur_access := ''
	mut previous := flat.empty_node
	for fid in fields {
		f := g.a.node(fid)
		g.emit_blank_line_between(previous, fid)
		if f.kind != .field_decl {
			// e.g. a `$if` block inside the struct body
			g.emit_comments_before(f.pos.offset)
			g.source_end = int_max(g.source_end, f.pos.offset)
			g.stmt(fid)
			previous = fid
			continue
		}
		gp := f.generic_params()
		flags := if gp.len > 0 { gp[0] } else { '' }
		access := access_label(flags)
		if access != cur_access {
			// Doc comments written below `pub:` belong to the field, so only the
			// comments before the specifier itself are emitted ahead of it.
			if spec_end := g.access_specifier_end(g.source_end, f.pos.offset) {
				comment_i := g.comment_i
				g.emit_comments_before(spec_end)
				if g.comment_i > comment_i {
					g.emit_blank_line_after_comment(spec_end)
				}
				g.source_end = int_max(g.source_end, spec_end)
			}
			// access specifiers sit one level out from the fields they head
			g.indent--
			match access {
				'mut' { g.writeln('mut:') }
				'pub' { g.writeln('pub:') }
				'pub mut' { g.writeln('pub mut:') }
				'__global' { g.writeln('__global:') }
				else {}
			}
			g.indent++
			cur_access = access
		}
		g.emit_comments_before(f.pos.offset)
		g.source_end = int_max(g.source_end, f.pos.offset)
		is_embed := flags.contains('e')
		if is_embed {
			g.write(g.type_text(f.value))
		} else {
			if flags.contains('v') {
				g.write('volatile ')
			}
			g.write(f.value)
			width := alignments[int(fid)] or { f.value.len }
			g.write(' '.repeat(width - f.value.len + 1))
			g.write(g.type_text(f.typ))
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
		previous = fid
	}
	g.emit_comments_before(end)
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
	end := g.a.formatter_node_ends[int(id)] or { n.pos.end }
	fields := g.a.children_of(n)
	if fields.len == 0 && g.empty_braced_body_is_compact(n, end)
		&& !g.has_comment_between(n.pos.offset, end) {
		g.writeln(' {}')
		return
	}
	g.writeln(' {')
	g.indent++
	alignments := g.enum_field_alignments(fields)
	mut previous := flat.empty_node
	for fid in fields {
		f := g.a.node(fid)
		g.emit_blank_line_between(previous, fid)
		g.emit_comments_before(f.pos.offset)
		g.source_end = int_max(g.source_end, f.pos.offset)
		g.write(f.value)
		if f.children_count > 0 {
			width := alignments[int(fid)] or { f.value.len }
			g.write(' '.repeat(width - f.value.len))
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
		previous = fid
	}
	g.emit_comments_before(end)
	g.indent--
	g.writeln('}')
}

fn (g &Gen) enum_field_alignments(fields []flat.NodeId) map[int]int {
	mut alignments := map[int]int{}
	mut group := []flat.NodeId{}
	mut previous := flat.empty_node
	for fid in fields {
		f := g.a.node(fid)
		if int(previous) >= 0
			&& g.source_has_blank_line_between(g.a.node(previous).pos.end, f.pos.offset) {
			g.store_enum_field_alignments(mut alignments, group)
			group.clear()
		}
		group << fid
		previous = fid
	}
	g.store_enum_field_alignments(mut alignments, group)
	return alignments
}

fn (g &Gen) store_enum_field_alignments(mut alignments map[int]int, fields []flat.NodeId) {
	mut width := 0
	for fid in fields {
		f := g.a.node(fid)
		if f.children_count > 0 {
			width = int_max(width, f.value.len)
		}
	}
	for fid in fields {
		if g.a.node(fid).children_count > 0 {
			alignments[int(fid)] = width
		}
	}
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
		g.sum_type_variants(variants)
	} else if n.typ.len > 0 {
		g.write(' = ${g.type_text(n.typ)}')
	}
	if !g.on_newline {
		g.writeln('')
	}
}

fn (mut g Gen) sum_type_variants(variants []flat.NodeId) {
	multiline := g.sum_type_is_multiline(variants)
	g.write(' = ')
	for i, vid in variants {
		variant := g.a.node(vid)
		if i > 0 {
			if multiline {
				if !g.on_newline {
					g.writeln('')
				}
				g.indent++
				g.emit_comments_before(variant.pos.offset)
				g.write('| ')
			} else {
				g.write(' | ')
			}
		} else {
			g.emit_comments_before(variant.pos.offset)
		}
		g.write(variant.value)
		g.source_end = int_max(g.source_end, variant.pos.end)
		g.emit_trailing_comments(variant.pos.end)
		if i > 0 && multiline {
			g.indent--
		}
	}
}

fn (g &Gen) sum_type_is_multiline(variants []flat.NodeId) bool {
	mut projected := g.output_line_len() + 3
	for i, vid in variants {
		variant := g.a.node(vid)
		projected += variant.value.len
		if i > 0 {
			projected += 3
			previous := g.a.node(variants[i - 1])
			if g.source_has_line_break_between(previous.pos.end, variant.pos.offset)
				|| g.has_comment_between(previous.pos.end, variant.pos.offset) {
				return true
			}
		}
	}
	return projected > formatter_max_line_len
}

fn (g &Gen) has_comment_between(start int, end int) bool {
	for i := g.comment_i; i < g.comments.len; i++ {
		comment := g.comments[i]
		if comment.pos.offset >= end {
			return false
		}
		if comment.pos.offset >= start {
			return true
		}
	}
	return false
}

fn (mut g Gen) advance_source_end_before_pending_comment(limit int) {
	if g.comment_i >= g.comments.len || g.comments[g.comment_i].pos.offset >= limit {
		return
	}
	mut previous := g.comments[g.comment_i].pos.offset
	for previous > 0 && g.source[previous - 1].is_space() {
		previous--
	}
	if previous > 0 {
		g.source_end = previous - 1
	}
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
	fields := g.a.children_of(n)
	if fields.len == 0 && g.empty_braced_body_is_compact(n, n.pos.end)
		&& !g.has_comment_between(n.pos.offset, n.pos.end) {
		g.writeln(' {}')
		return
	}
	g.writeln(' {')
	g.indent++
	alignments := g.aggregate_field_alignments(fields, true)
	mut cur_mut := false
	mut previous := flat.empty_node
	for fid in fields {
		f := g.a.node(fid)
		g.emit_blank_line_between(previous, fid)
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
			g.params(fid, g.a.children_of(f))
			if f.typ.len > 0 {
				g.write(' ${g.type_text(f.typ)}')
			}
		} else {
			g.write(f.value)
			if f.typ.len > 0 {
				width := alignments[int(fid)] or { f.value.len }
				g.write(' '.repeat(width - f.value.len + 1))
				g.write(g.type_text(f.typ))
			}
		}
		g.emit_trailing_comments(f.pos.end)
		if !g.on_newline {
			g.writeln('')
		}
		g.source_end = int_max(g.source_end, f.pos.end)
		previous = fid
	}
	g.emit_comments_before(n.pos.end)
	g.indent--
	g.writeln('}')
}

// access_specifier_end returns the offset just past the `:` of the access
// specifier (`pub:`, `mut:`, ...) that sits between `start` and `end`, ignoring
// any comments in that gap. Returns none when no specifier is found.
fn (g &Gen) access_specifier_end(start int, end int) ?int {
	gap := g.source_span(start, end) or { return none }
	mut i := 0
	mut result := -1
	for i < gap.len {
		c := gap[i]
		if c == `/` && i + 1 < gap.len && gap[i + 1] == `/` {
			for i < gap.len && gap[i] != `\n` {
				i++
			}
			continue
		}
		if c == `/` && i + 1 < gap.len && gap[i + 1] == `*` {
			i += 2
			for i + 1 < gap.len && !(gap[i] == `*` && gap[i + 1] == `/`) {
				i++
			}
			i += 2
			continue
		}
		if c == `:` {
			result = i + 1
		}
		i++
	}
	if result < 0 {
		return none
	}
	return start + result
}

// aggregate_field_alignments returns the widest field name in each adjacent
// access section. Blank lines and standalone comments start a fresh alignment
// group, matching the established formatter layout.
fn (g &Gen) aggregate_field_alignments(fields []flat.NodeId, is_interface bool) map[int]int {
	mut alignments := map[int]int{}
	mut group := []flat.NodeId{}
	mut group_access := ''
	mut previous := flat.empty_node
	for fid in fields {
		f := g.a.node(fid)
		mut section := ''
		mut alignable := false
		if is_interface {
			section = if f.is_mut { 'mut' } else { '' }
			alignable = f.kind == .interface_field && f.op != .dot && f.typ.len > 0
		} else if f.kind == .field_decl {
			gp := f.generic_params()
			flags := if gp.len > 0 { gp[0] } else { '' }
			section = access_label(flags)
			alignable = !flags.contains('e')
		}
		if !alignable {
			g.store_field_alignment(mut alignments, group)
			group.clear()
			group_access = ''
			previous = flat.empty_node
			continue
		}
		if group.len > 0 && (section != group_access || g.field_alignment_break(previous, fid)) {
			g.store_field_alignment(mut alignments, group)
			group.clear()
		}
		if group.len == 0 {
			group_access = section
		}
		group << fid
		previous = fid
	}
	g.store_field_alignment(mut alignments, group)
	return alignments
}

fn (g &Gen) store_field_alignment(mut alignments map[int]int, fields []flat.NodeId) {
	mut width := 0
	for fid in fields {
		width = int_max(width, g.a.node(fid).value.len)
	}
	for fid in fields {
		alignments[int(fid)] = width
	}
}

fn (g &Gen) field_alignment_break(previous flat.NodeId, current flat.NodeId) bool {
	if int(previous) < 0 {
		return false
	}
	prev := g.a.node(previous)
	cur := g.a.node(current)
	if !prev.pos.is_valid() || !cur.pos.is_valid() {
		return false
	}
	prev_line := g.source_line(prev.pos.end)
	if g.source_line(cur.pos.offset) <= prev_line + 1 {
		return false
	}
	if gap := g.source_span(prev.pos.end, cur.pos.offset) {
		lines := gap.replace('\r\n', '\n').split('\n')
		for i in 1 .. lines.len - 1 {
			if lines[i].trim_space().len == 0 {
				return true
			}
		}
	}
	for comment in g.comments {
		if comment.pos.offset >= prev.pos.end && comment.pos.offset < cur.pos.offset
			&& g.source_line(comment.pos.offset) > prev_line {
			return true
		}
	}
	return false
}

fn (mut g Gen) const_decl(id flat.NodeId) {
	n := g.a.node(id)
	g.emit_attrs(id)
	pub_prefix := if n.op == .arrow { 'pub ' } else { '' }
	for fid in g.a.children_of(n) {
		g.const_field(fid, pub_prefix)
	}
}

fn (mut g Gen) const_field(fid flat.NodeId, pub_prefix string) {
	f := g.a.node(fid)
	g.emit_comments_before(f.pos.offset)
	g.write('${pub_prefix}const ${f.value}')
	if f.children_count > 0 {
		g.write(' = ')
		g.expr(g.a.child(f, 0))
	} else if f.typ.len > 0 {
		g.write(' ${g.type_text(f.typ)}')
	}
	// `g.expr` already terminated the line when it flushed a trailing `//`
	// comment; a second newline here would add a blank line on every `vfmt`
	// pass.
	if !g.on_newline {
		g.writeln('')
	}
	g.source_end = int_max(g.source_end, f.pos.end)
}

fn (mut g Gen) global_decl(id flat.NodeId) {
	n := g.a.node(id)
	g.emit_attrs(id)
	group_pub := n.op == .arrow
	if group_pub {
		g.write('pub ')
	}
	fields := g.a.children_of(n)
	if n.value == 'ungrouped' {
		g.write('__global ')
		if fields.len > 0 {
			g.global_field(fields[0], group_pub, 0)
		} else {
			g.writeln('')
		}
		return
	}
	g.writeln('__global (')
	g.indent++
	mut field_width := 0
	for fid in fields {
		field_width = int_max(field_width, global_field_head(g.a.node(fid), group_pub).len)
	}
	for fid in fields {
		g.global_field(fid, group_pub, field_width)
	}
	g.advance_source_end_before_pending_comment(n.pos.end)
	g.emit_comments_before(n.pos.end)
	g.indent--
	g.writeln(')')
}

fn global_field_head(f &flat.Node, group_pub bool) string {
	mut head := ''
	if f.op == .arrow && !group_pub {
		head += 'pub '
	}
	if 'const' in f.generic_params() {
		head += 'const '
	}
	return head + f.value
}

fn (mut g Gen) global_field(fid flat.NodeId, group_pub bool, align_width int) {
	f := g.a.node(fid)
	g.emit_comments_before(f.pos.offset)
	g.source_end = int_max(g.source_end, f.pos.offset)
	head := global_field_head(f, group_pub)
	g.write(head)
	padding := if align_width > 0 { align_width - head.len + 1 } else { 1 }
	if f.children_count > 0 {
		if f.typ.len > 0 {
			g.write(' '.repeat(padding))
			g.write(g.type_text(f.typ))
			g.write(' = ')
		} else {
			g.write(' '.repeat(padding))
			g.write('= ')
		}
		g.expr(g.a.child(f, 0))
	} else {
		g.write(' '.repeat(padding))
		g.write(g.type_text(f.typ))
	}
	g.emit_trailing_comments(f.pos.end)
	if !g.on_newline {
		g.writeln('')
	}
	g.source_end = int_max(g.source_end, f.pos.end)
}

fn (mut g Gen) directive_stmt(id flat.NodeId) {
	n := g.a.node(id)
	if n.value.starts_with('@attributes:') || n.value == 'string_interp_format' {
		return
	}
	g.emit_attrs(id)
	g.write('#${n.value}')
	if n.typ.len > 0 {
		g.write(' ${n.typ}')
	}
	g.writeln('')
}

fn (mut g Gen) emit_attrs(id flat.NodeId) {
	if source := g.attr_sources[int(id)] {
		groups := formatter_attribute_groups(source)
		if groups.len > 0 {
			mut argument_groups := []string{}
			mut bare_attrs := []string{}
			for group in groups {
				parts := formatter_attribute_parts(group)
				if parts.len == 0 {
					continue
				}
				if parts.all(is_bare_formatter_attribute(it)) {
					bare_attrs << parts
				} else {
					argument_groups << parts.join('; ')
				}
			}
			bare_attrs.sort()
			for group in argument_groups {
				g.writeln('@[${group}]')
			}
			if bare_attrs.len > 0 {
				g.writeln('@[${bare_attrs.join('; ')}]')
			}
			return
		}
	}
	attrs := g.attrs[int(id)] or { []string{} }
	if attrs.len > 0 {
		g.writeln('@[${attrs.join('; ')}]')
	}
}

fn formatter_attribute_groups(source string) []string {
	mut groups := []string{}
	mut start := -1
	mut depth := 0
	mut quote := u8(0)
	mut escaped := false
	for i := 0; i < source.len; i++ {
		c := source[i]
		if start < 0 {
			if c == `[` {
				start = if i > 0 && source[i - 1] == `@` { i - 1 } else { i }
				depth = 1
			}
			continue
		}
		if quote != 0 {
			if escaped {
				escaped = false
			} else if c == `\\` {
				escaped = true
			} else if c == quote {
				quote = 0
			}
			continue
		}
		if c == `'` || c == `"` {
			quote = c
		} else if c == `[` {
			depth++
		} else if c == `]` {
			depth--
			if depth == 0 {
				groups << source[start..i + 1].trim_space()
				start = -1
			}
		}
	}
	return groups
}

fn formatter_attribute_parts(group string) []string {
	open_index := group.index_u8(`[`)
	if open_index < 0 {
		return []
	}
	mut close_index := group.len
	for close_index > open_index + 1 && group[close_index - 1].is_space() {
		close_index--
	}
	if close_index <= open_index + 1 || group[close_index - 1] != `]` {
		return []
	}
	content := group[open_index + 1..close_index - 1]
	mut parts := []string{}
	mut start := 0
	mut paren_depth := 0
	mut bracket_depth := 0
	mut quote := u8(0)
	mut escaped := false
	for i := 0; i < content.len; i++ {
		c := content[i]
		if quote != 0 {
			if escaped {
				escaped = false
			} else if c == `\\` {
				escaped = true
			} else if c == quote {
				quote = 0
			}
			continue
		}
		if c == `'` || c == `"` {
			quote = c
		} else if c == `(` {
			paren_depth++
		} else if c == `)` {
			paren_depth--
		} else if c == `[` {
			bracket_depth++
		} else if c == `]` {
			bracket_depth--
		} else if (c == `;` || c == `,`) && paren_depth == 0 && bracket_depth == 0 {
			piece := content[start..i].trim_space()
			if piece.len > 0 {
				parts << piece
			}
			start = i + 1
		}
	}
	piece := content[start..].trim_space()
	if piece.len > 0 {
		parts << piece
	}
	return parts
}

fn is_bare_formatter_attribute(part string) bool {
	if part.len == 0 {
		return false
	}
	for c in part.bytes() {
		if !c.is_alnum() && c != `_` {
			return false
		}
	}
	return true
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
				g.out.writeln('')
			}
		} else {
			if !g.on_newline && g.out.len > 0 {
				g.writeln('')
			}
			if g.source_end >= 0
				&& g.source_line(comment.pos.offset) > g.source_line(g.source_end) + 1
				&& (g.out.len == 0 || !g.out.last_n(int_min(2, g.out.len)).ends_with('\n\n')) {
				g.out.writeln('')
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
		if i == 0 {
			g.writeln(line)
			continue
		}
		// A block comment's continuation lines already carry their own leading whitespace from
		// the source, so they are written through verbatim. Indenting them again would add a
		// level on every run and the formatter would never reach a fixed point.
		g.writeln_verbatim(line)
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

fn vfmt_is_disabled_at(directives []VfmtDirective, offset int) bool {
	mut is_disabled := false
	for directive in directives {
		if directive.offset > offset {
			break
		}
		is_disabled = directive.is_off
	}
	return is_disabled
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

fn closing_lines_end(source string, offset int) int {
	mut pos := offset
	for pos < source.len {
		line_end := source.index_after('\n', pos) or { source.len }
		line := source[pos..line_end].trim_space()
		if line.len == 0 || line.bytes().any(it !in [`}`, `]`, `)`, `,`, `;`]) {
			break
		}
		pos = if line_end < source.len { line_end + 1 } else { source.len }
	}
	return pos
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
		source_resume = closing_lines_end(source, source_resume)
		formatted_resume = closing_lines_end(formatted, formatted_resume)
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

// writeln_verbatim writes a line without the current indentation, for text that already carries
// its own.
fn (mut g Gen) writeln_verbatim(str string) {
	g.out.writeln(str)
	g.on_newline = true
}

@[inline]
fn (mut g Gen) writeln(str string) {
	// A blank separator line gets no indentation: writing it would leave a line of whitespace,
	// which V source never carries and which the next run reads back differently, so formatting
	// twice differed from once.
	if g.on_newline && g.indent > 0 && str.len > 0 {
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

fn (g &Gen) string_literal_text(n &flat.Node) string {
	if source := g.source_span(n.pos.offset, n.pos.end) {
		// Physical newlines are part of a multiline literal's value and layout.
		if source.contains('\n') || source.contains('\r') {
			return source
		}
	}
	if n.typ.starts_with('raw:') {
		quote := if n.typ.ends_with('"') { '"' } else { "'" }
		return 'r${quote}${n.value}${quote}'
	}
	if n.typ.starts_with('js:') {
		return 'js${quote_string(n.value)}'
	}
	return quote_string(n.value)
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

fn c_string_literal_text(value string) string {
	raw := value.all_after('c:')
	// C-string nodes retain source escapes rather than a fully decoded value. Normalize quote
	// escapes before choosing a delimiter, while protecting literal doubled backslashes.
	unescaped := raw.replace('\\\\', '\x01').replace_each(["\\'", "'", '\\"', '"'])
	quote := if unescaped.contains("'") && !unescaped.contains('"') { '"' } else { "'" }
	escaped := unescaped.replace_each(['\x01', '\\\\', quote, '\\${quote}'])
	return 'c${quote}${escaped}${quote}'
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
			7 {
				b.write_string('\\a')
			}
			8 {
				b.write_string('\\b')
			}
			11 {
				b.write_string('\\v')
			}
			12 {
				b.write_string('\\f')
			}
			else {
				// NUL is written as `\\x00`, never `\\0`: `u8.hex()` always emits two digits, so
				// the escape cannot absorb a following character, whereas `\\0` before an octal
				// digit re-parses as one octal escape and silently changes the string's bytes
				// (`'x\\x0041y'` would come back as `'x\\041y'`, i.e. `x!y`).
				if c < 32 || c == 127 {
					b.write_string('\\x${c.hex()}')
				} else if c == quote {
					b.write_u8(`\\`)
					b.write_u8(c)
				} else {
					b.write_u8(c)
				}
			}
		}
	}
	return b.str()
}

// access_label maps a struct field flag code (from generic_params()[0]) to its
// section keyword. Codes: `m`=mut, `p`=pub, `g`=global.
fn access_label(flags string) string {
	if flags.contains('g') {
		return '__global'
	}
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

fn demangle_formatter_local_types(typ string) string {
	marker := '@local@'
	if !typ.contains(marker) {
		return typ
	}
	mut out := strings.new_builder(typ.len)
	mut i := 0
	for i < typ.len {
		marker_start := typ.index_after(marker, i) or {
			out.write_string(typ[i..])
			break
		}
		out.write_string(typ[i..marker_start])
		i = marker_start + marker.len
		for i < typ.len && is_type_ident_char(typ[i]) {
			i++
		}
	}
	return out.str()
}

fn (mut g Gen) type_text(typ string) string {
	demangled := demangle_formatter_local_types(typ)
	mut expanded := demangled
	if g.formatter_types.len > 0 {
		mut out := strings.new_builder(demangled.len)
		mut i := 0
		for i < demangled.len {
			if is_type_ident_char(demangled[i]) {
				start := i
				for i < demangled.len && is_type_ident_char(demangled[i]) {
					i++
				}
				name := demangled[start..i]
				if source := g.formatter_types[name] {
					out.write_string(source.text)
					g.skip_comments_in_source(source.start, source.end)
					g.source_end = int_max(g.source_end, source.end)
				} else {
					out.write_string(name)
				}
				continue
			}
			out.write_u8(demangled[i])
			i++
		}
		expanded = out.str()
	}
	if !g.is_new_int || (!g.is_translated && !g.in_c_function) || !expanded.contains('int') {
		return restore_fn_type_space(expanded)
	}
	mut out := strings.new_builder(expanded.len)
	mut i := 0
	for i < expanded.len {
		if i + 3 <= expanded.len && expanded[i..i + 3] == 'int'
			&& (i == 0 || !is_type_ident_char(expanded[i - 1]))
			&& (i + 3 == expanded.len || !is_type_ident_char(expanded[i + 3]))
			&& !type_ident_is_qualified(expanded, i) {
			out.write_string('i32')
			i += 3
			continue
		}
		out.write_u8(expanded[i])
		i++
	}
	return restore_fn_type_space(out.str())
}

// restore_fn_type_space rewrites `fn(` back to `fn (`. The type system spells a function type
// without the space internally, but V source style keeps it — `type Cb = fn (int) int`, and the
// same in field, parameter and return positions — so a type rendered straight from the internal
// name would reformat every function type in the file.
fn restore_fn_type_space(typ string) string {
	if !typ.contains('fn(') {
		return typ
	}
	mut out := strings.new_builder(typ.len + 4)
	mut i := 0
	for i < typ.len {
		// Only a standalone `fn` introduces a function type; `myfn(` is an ordinary name.
		if typ[i] == `f` && i + 3 <= typ.len && typ[i + 1] == `n` && typ[i + 2] == `(`
			&& (i == 0 || !is_type_ident_char(typ[i - 1])) {
			out.write_string('fn (')
			i += 3
			continue
		}
		out.write_u8(typ[i])
		i++
	}
	return out.str()
}

fn (mut g Gen) skip_comments_in_source(start int, end int) {
	if g.comment_i < g.comments.len && g.comments[g.comment_i].pos.offset < start {
		return
	}
	for g.comment_i < g.comments.len && g.comments[g.comment_i].pos.offset < end {
		g.source_end = int_max(g.source_end, g.comments[g.comment_i].pos.end)
		g.comment_i++
	}
}

fn is_type_ident_char(c u8) bool {
	return c == `_` || (c >= `0` && c <= `9`) || (c >= `A` && c <= `Z`) || (c >= `a` && c <= `z`)
}

fn type_ident_is_qualified(typ string, start int) bool {
	mut i := start
	for i > 0 {
		i--
		if typ[i] in [` `, `\t`, `\r`, `\n`] {
			continue
		}
		return typ[i] == `.`
	}
	return false
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
