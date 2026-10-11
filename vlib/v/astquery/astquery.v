// Module `astquery` answers questions about a parsed V file that an editor, a
// language server or an agent has to answer constantly: what is declared here,
// where is a name mentioned, and what sits under the cursor.
//
// It reads the flat AST that `v.parser` produces and never type checks, so every
// answer is available even for a file that does not compile. That is the point:
// a tool has to be able to look at broken code.
//
// Declaration results omit visibility modifiers. Struct fields retain their
// names and types regardless of access modifiers, which are AST metadata rather
// than separate declarations.
module astquery

import os
import v.flat
import v.parser
import v.pref
import v.scanner
import v.token

// max_doc_gap is how far above a declaration a comment may sit and still be read
// as its documentation. A larger gap means the comment documents something else.
const max_doc_gap = 20

// parse reads one V file into a fresh AST with the preferences a formatter and an
// inspector agree on.
pub fn parse(path string) &flat.FlatAst {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	prefs.is_fmt = true
	mut p := parser.Parser.new(prefs)
	return p.parse_file(path)
}

// DeclKind classifies a declaration found by `declarations`.
pub enum DeclKind {
	module
	import
	fn
	method
	struct
	field
	enum
	enum_value
	interface
	interface_method
	type_alias
	sumtype
	const
	global
}

// str returns the lowercase name of the kind, as used in JSON output.
pub fn (k DeclKind) str() string {
	return match k {
		.module { 'module' }
		.import { 'import' }
		.fn { 'fn' }
		.method { 'method' }
		.struct { 'struct' }
		.field { 'field' }
		.enum { 'enum' }
		.enum_value { 'enum_value' }
		.interface { 'interface' }
		.interface_method { 'interface_method' }
		.type_alias { 'type_alias' }
		.sumtype { 'sumtype' }
		.const { 'const' }
		.global { 'global' }
	}
}

// Declaration is one named declaration, with where it starts and what it is.
pub struct Declaration {
pub:
	kind DeclKind
	// name is the declared name. A method is `receiver` plus `name`.
	name string
	// receiver is the type a method belongs to, empty otherwise.
	receiver string
	// type_name is the field type, the function return type, or empty.
	type_name string
	file      string
	// line and column are 1-based, as an editor reports them.
	line   int
	column int
	// end_line and end_column bound the declaration.
	end_line   int
	end_column int
	// doc is the doc comment directly above the declaration, without its `//`
	// markers, when there is one.
	doc string
}

// Occurrence is one mention of a name.
pub struct Occurrence {
pub:
	name string
	// node_kind is the AST kind that carries the name, for example `ident` or
	// `selector`.
	node_kind string
	file      string
	line      int
	// column and end_column bound the name itself, one past the end.
	column     int
	end_column int
	// declaration is true when the mention is the declaration itself.
	declaration bool
	// type_name is what the parser resolved the mention to, when known.
	type_name string
}

// file_nodes returns the `.file` node ids that hold user code in `a`.
fn file_nodes(a &flat.FlatAst) []flat.NodeId {
	mut ids := []flat.NodeId{}
	for raw_id in a.file_node_ids {
		id := flat.NodeId(raw_id)
		node := a.node(id)
		if node.kind == .file && node.children_count > 0 {
			ids << id
		}
	}
	return ids
}

// span_of returns the source path and the 1-based line and column of `pos`.
fn span_of(a &flat.FlatAst, pos token.Pos) (string, int, int) {
	file := a.source_files[pos.id] or {
		return '', 0, 0
	}
	position := file.position(pos)
	return file.name, position.line, position.column
}

// declarations returns every named declaration in the user code of `a`.
//
// Order is the order the parser produced, which is source order for a single
// file. A declaration nested in a `$if` block is reported where it appears.
// Struct field names and types are reported regardless of access modifiers,
// including fields named `pub` or `priv`.
pub fn declarations(a &flat.FlatAst) []Declaration {
	mut out := []Declaration{}
	for file_id in file_nodes(a) {
		out << collect_declarations(a, a.node(file_id))
	}
	return out
}

// collect_declarations walks one subtree and returns what it finds, a parent
// before its members.
fn collect_declarations(a &flat.FlatAst, node &flat.Node) []Declaration {
	mut out := []Declaration{}
	match node.kind {
		.module_decl, .import_decl, .struct_decl, .enum_decl, .interface_decl {
			out << declaration(a, node, kind_of(node.kind), node.value, '', '')
		}
		.type_decl {
			// `type X = Y` is an alias and `type X := Y` a defined type; a sum
			// type has more than one variant child.
			kind := if named_children(a, node, .ident).len > 1 {
				DeclKind.sumtype
			} else {
				DeclKind.type_alias
			}
			out << declaration(a, node, kind, node.value, '', '')
		}
		.fn_decl, .c_fn_decl {
			// A method declaration spells itself `Receiver.name`; a plain
			// function has no dot in its value.
			is_method := node.value.contains('.')
			out << declaration(a, node, if is_method {
				DeclKind.method
			} else {
				DeclKind.fn
			}, node.value.all_after_last('.'), node.typ, if is_method {
				node.value.all_before_last('.')
			} else {
				''
			})
		}
		.const_decl, .global_decl {
			// The declaration node itself carries no name; its children do.
			kind := if node.kind == .const_decl {
				DeclKind.const
			} else {
				DeclKind.global
			}
			for child in a.children_of(node) {
				child_node := a.node(child)
				if child_node.kind in [.const_field, .ident] {
					out << declaration(a, child_node, kind, child_node.value, '', '')
				}
			}
		}
		else {}
	}
	// Fields, enum values and interface methods live in the declaration nodes, so
	// they are reported next to the declaration they belong to.
	if node.kind == .struct_decl {
		children := a.children_of(node)
		mut i := 0
		for i < children.len {
			child_node := a.node(children[i])
			if child_node.kind != .field_decl {
				i++
				continue
			}
			out << declaration(a, child_node, .field, child_node.value, child_node.typ, '')
			i++
		}
	}
	if node.kind == .enum_decl {
		for child in a.children_of(node) {
			child_node := a.node(child)
			if child_node.kind == .enum_field {
				out << declaration(a, child_node, .enum_value, child_node.value,
					child_node.typ, '')
			}
		}
	}
	if node.kind == .interface_decl {
		for child in a.children_of(node) {
			child_node := a.node(child)
			if child_node.kind == .interface_field {
				out << declaration(a, child_node, .interface_method, child_node.value,
					child_node.typ, '')
			}
		}
	}
	for child in a.children_of(node) {
		out << collect_declarations(a, a.node(child))
	}
	return out
}

// kind_of maps the AST declaration kinds that need no further inspection.
fn kind_of(node_kind flat.NodeKind) DeclKind {
	return match node_kind {
		.module_decl { DeclKind.module }
		.import_decl { DeclKind.import }
		.struct_decl { DeclKind.struct }
		.enum_decl { DeclKind.enum }
		.interface_decl { DeclKind.interface }
		else { DeclKind.fn }
	}
}

// named_children returns the values of the children of `node` whose kind is
// `kind` and whose value is not empty.
fn named_children(a &flat.FlatAst, node &flat.Node, kind flat.NodeKind) []string {
	mut names := []string{}
	for child in a.children_of(node) {
		child_node := a.node(child)
		if child_node.kind == kind && child_node.value != '' {
			names << child_node.value
		}
	}
	return names
}

// declaration builds a Declaration for one node and attaches its doc comment.
//
// `type_name` is the field type or the function return type; `receiver` is the
// type a method belongs to, and is empty for everything else.
fn declaration(a &flat.FlatAst, node &flat.Node, kind DeclKind, name string,
	type_name string, receiver string) Declaration {
	file, line, column := span_of(a, node.pos)
	_, end_line, end_column := span_of(a, token.Pos{
		offset: node.pos.end
		id:     node.pos.id
	})
	return Declaration{
		kind:       kind
		name:       name
		receiver:   receiver
		type_name:  type_name
		file:       file
		line:       line
		column:     column
		end_line:   end_line
		end_column: end_column
		doc:        doc_comment_above(a, file, line)
	}
}

// doc_comment_above returns the text of the comment block directly above `pos`,
// without its markers.
//
// Only an immediately preceding run of comment lines counts, so an unrelated
// comment further up cannot be mistaken for the documentation of this
// declaration.
fn doc_comment_above(a &flat.FlatAst, file string, line int) string {
	if line <= 1 {
		return ''
	}
	// Index the comments of this file by line first: a declaration usually has
	// far more comments above it in the file than belong to it.
	mut by_line := map[int]string{}
	for comment in a.comments {
		comment_file := a.source_files[comment.pos.id] or {
			continue
		}
		if comment_file.name != file {
			continue
		}
		comment_line := comment_file.position(comment.pos).line
		by_line[comment_line] = comment.text
	}
	// Walk upward from the line right above the declaration. The run has to
	// start there: a blank line, a non-comment line, or the top of the window
	// ends it, which is what separates documentation from an unrelated note.
	mut collected := []string{}
	mut comment_line := line - 1
	for comment_line >= line - max_doc_gap {
		text := by_line[comment_line] or {
			break
		}
		collected.prepend(clean_comment(text))
		comment_line--
	}
	if collected.len == 0 {
		return ''
	}
	return collected.join('\n')
}

// clean_comment strips the comment markers and the space after them.
fn clean_comment(text string) string {
	mut body := text.trim_space()
	for prefix in ['///', '//!', '//'] {
		if body.starts_with(prefix) {
			return body[prefix.len..].trim_space()
		}
	}
	return body
}

// references returns every mention of `name` in the user code of `a`.
//
// The name is matched against identifier positions in the AST, not against raw
// text, so a comment or a string literal that happens to contain it is not
// reported.
pub fn references(a &flat.FlatAst, name string) []Occurrence {
	mut out := []Occurrence{}
	if name == '' {
		return out
	}
	mut cache := map[int]string{}
	for file_id in file_nodes(a) {
		out << collect_references(a, a.node(file_id), name, mut cache)
	}
	return out
}

// collect_references walks one subtree looking for `name`.
fn collect_references(a &flat.FlatAst, node &flat.Node, name string, mut cache map[int]string) []Occurrence {
	mut out := []Occurrence{}
	if mentions(node, name) {
		if at := occurrence(a, node, name, mut cache) {
			out << at
		}
	}
	for child in a.children_of(node) {
		out << collect_references(a, a.node(child), name, mut cache)
	}
	return out
}

// mentions reports whether `node` refers to `name` as an identifier.
//
// A few node kinds carry a spelled name in `value` without naming an identifier:
// a literal holds its own text, and a declaration may hold a dotted
// `Receiver.name`. Those are matched by their last segment instead, so a plain
// substring match would wrongly report a string literal as a mention.
fn mentions(node &flat.Node, name string) bool {
	if node.kind in literal_kinds || node.kind == .directive {
		return false
	}
	if node.value == name {
		return true
	}
	return node.value.all_after_last('.') == name
}

// literal_kinds are the node kinds whose `value` is text rather than a name.
const literal_kinds = [flat.NodeKind.string_literal, .char_literal, .string_interp]

// occurrence builds an Occurrence for one node, marking the declaration itself
// when the node is where `name` is declared.
//
// The span has to point at the name itself, and an offset into `node.value`
// cannot find it: the node's source range and its value text start in different
// places. A method declaration spans the name while its value reads `Type.name`,
// so a value offset walked into the signature; a selector spans `receiver.name`
// while its value reads only `name`, so the offset stayed on the receiver. Both
// landed the write on unrelated bytes.
fn occurrence(a &flat.FlatAst, node &flat.Node, name string, mut cache map[int]string) ?Occurrence {
	found := name_offset(source_text(a, int(node.pos.id), mut cache), node, name)
	if found < 0 {
		return none
	}
	pos := token.Pos{
		offset: i32(found)
		end:    i32(found + name.len)
		id:     node.pos.id
	}
	file, line, column := span_of(a, pos)
	return Occurrence{
		name:        name
		node_kind:   '${node.kind}'
		file:        file
		line:        line
		column:      column
		end_column:  column + name.len
		declaration: declares(node, name)
		type_name:   node.typ
	}
}

// source_text returns the text of the file `id` names, or an empty string.
//
// Formatter parses retain the source text measured by the parser. Otherwise read
// the file. Results are cached by file id because the walk asks once per node.
fn source_text(a &flat.FlatAst, id int, mut cache map[int]string) string {
	if id in cache {
		return cache[id]
	}
	mut text := a.formatter_file_sources[id] or { '' }
	if text.len == 0 {
		if file := a.source_files[id] {
			text = os.read_file(file.name) or { '' }
		}
	}
	cache[id] = text
	return text
}

// name_offset returns the byte offset of `name` inside the range `node` spans,
// or -1 when the name is not there.
//
// The node's own range is the only frame that can be trusted, because it is what
// the parser measured from the file. A selector's name follows its receiver;
// matching complete tokens avoids selecting a substring of the receiver's name.
// Escaped names retain their `@` in scanner literals; the edit starts after it.
fn name_offset(source string, node &flat.Node, name string) int {
	start := int(node.pos.offset)
	end := int(node.pos.end)
	if source.len == 0 || start < 0 || start >= source.len || name.len == 0 {
		return -1
	}
	limit := if end > start {
		int_min(end, source.len)
	} else {
		int_min(start + name.len, source.len)
	}
	mut s := scanner.new_scanner(&pref.Preferences{}, .normal)
	s.init(unsafe { nil }, source[start..limit])
	mut found := -1
	for {
		tok := s.scan()
		if tok == .eof {
			break
		}
		escaped := s.lit.starts_with('@') && s.lit[1..] == name
		if (tok == .name || tok.is_keyword()) && (s.lit == name || escaped) {
			found = start + s.pos + if escaped { 1 } else { 0 }
			if node.kind != .selector {
				break
			}
		}
	}
	return found
}

// declares reports whether `node` is where `name` is declared.
fn declares(node &flat.Node, name string) bool {
	return match node.kind {
		.module_decl, .import_decl, .struct_decl, .enum_decl, .interface_decl, .type_decl {
			node.value == name
		}
		.fn_decl, .c_fn_decl {
			node.value == name || node.value.all_after_last('.') == name
		}
		.field_decl, .enum_field, .interface_field, .const_field, .param {
			node.value == name
		}
		else {
			false
		}
	}
}

// occurrence_at returns the innermost mention of `name` that covers
// `line`:`column`, which is what a cursor position asks for.
pub fn occurrence_at(a &flat.FlatAst, name string, line int, column int) ?Occurrence {
	mut best := Occurrence{}
	mut found := false
	for candidate in references(a, name) {
		if candidate.line != line || candidate.column > column || candidate.end_column < column {
			continue
		}
		// Nodes nest, so the mentions arrive widest first; keep the narrowest.
		if !found || candidate.end_column - candidate.column < best.end_column - best.column {
			best = candidate
			found = true
		}
	}
	return if found { best } else { none }
}
