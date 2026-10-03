// Looking up the documentation of a standard library symbol.
//
// A symbol is found by parsing the module's own sources with `v.astquery`, the
// same AST the rest of this server reads. That keeps the answer honest for a
// compiler checkout — it documents the code that is actually there — and it
// avoids depending on the doc generator, whose module lives under `cmd/tools`
// and would drag the whole checker into this tool's build.
//
// A module is a directory: one under the compiler's `vlib`, or one installed for
// the current user, or one in the project being served.
module main

import os
import v.astjson
import v.astquery

// mod_roots are the directories a standard library module can live in, in the
// order the compiler itself searches them.
fn mod_roots(ws &Workspace) []string {
	return [
		os.join_path(ws.vroot, 'vlib'),
		os.vmodules_dir(),
		ws.project_root,
	]
}

// module_path resolves `name` to the directory holding it, or returns none.
//
// A dotted name resolves by walking one directory per segment, which is how the
// compiler reads `net.http`.
pub fn module_path(ws &Workspace, name string) ?string {
	mut candidates := []string{}
	for root in mod_roots(ws) {
		mut dir := root
		for segment in name.split('.') {
			dir = os.join_path_single(dir, segment)
		}
		candidates << dir
	}
	// The project itself wins over an installed module of the same name, so a
	// project that vendors a module documents its own copy.
	for dir in candidates.reverse() {
		if os.is_dir(dir) && (os.ls(dir) or { [] }).len > 0 {
			return dir
		}
	}
	return none
}

// module_files returns the module's sources, excluding its tests.
fn module_files(dir string) []string {
	entries := os.ls(dir) or {
		return []string{}
	}
	mut files := []string{}
	for entry in entries {
		if !entry.ends_with('.v') && !entry.ends_with('.vsh') {
			continue
		}
		if entry.ends_with('_test.v') || entry.ends_with('_test.vsh') {
			continue
		}
		files << os.join_path_single(dir, entry)
	}
	files.sort()
	return files
}

// module_doc is the doc comment block a module's first file carries above its
// `module` line.
fn module_doc(files []string) string {
	for file in files {
		source := os.read_file(file) or {
			continue
		}
		// The module comment is the run of `//` lines before `module <name>`.
		mut lines := source.split_into_lines()
		mut i := 0
		for i < lines.len {
			if lines[i].trim_space().starts_with('module ') {
				break
			}
			i++
		}
		if i == 0 {
			continue
		}
		mut collected := []string{}
		mut j := i - 1
		for j >= 0 {
			trimmed := lines[j].trim_space()
			if !trimmed.starts_with('//') {
				break
			}
			collected.prepend(strip_comment_marker(trimmed))
			j--
		}
		if collected.len > 0 {
			return collected.join('\n')
		}
	}
	return ''
}

// strip_comment_marker removes the `//` and the space after it.
fn strip_comment_marker(line string) string {
	mut body := line
	for prefix in ['///', '//!', '//'] {
		if body.starts_with(prefix) {
			return body[prefix.len..].trim_space()
		}
	}
	return body
}

// symbol_doc is one documented symbol of a module.
pub struct SymbolDoc {
pub:
	// name is the symbol as a caller writes it, for example `Builder` or
	// `read_file`.
	name string
	// kind is what the symbol declares: `fn`, `struct`, and so on.
	kind string
	// signature is the declaration's own rendering, good enough to check a call
	// against.
	signature string
	// file and line point at the declaration.
	file string
	line int
	// doc is the doc comment directly above the declaration.
	doc string
}

// symbol_docs returns every documented symbol of the module at `dir`.
//
// Only exported names are reported: a caller outside the module cannot reach a
// private one, so listing them would only be noise.
pub fn symbol_docs(ws &Workspace, dir string) []SymbolDoc {
	mut out := []SymbolDoc{}
	for file in module_files(dir) {
		for decl in astquery.declarations(astquery.parse(file)) {
			if decl.doc == '' || !is_exported(file, decl) {
				continue
			}
			out << SymbolDoc{
				name:      decl.name
				kind:      decl.kind.str()
				signature: signature_of(decl)
				file:      ws.relative(decl.file)
				line:      decl.line
				doc:       decl.doc
			}
		}
	}
	return out
}

// is_exported reports whether `decl` is `pub`.
//
// The flat AST folds the marker into an attribute bitmask rather than keeping it
// on the node, so the source line is the authority.
fn is_exported(file string, decl astquery.Declaration) bool {
	source := os.read_file(file) or {
		return false
	}
	lines := source.split_into_lines()
	if decl.line < 1 || decl.line > lines.len {
		return false
	}
	// The marker sits on the declaration's own line, or on the line above it when
	// a doc comment or another attribute is in between.
	for line_no in [decl.line, decl.line - 1] {
		if line_no < 1 || line_no > lines.len {
			continue
		}
		trimmed := lines[line_no - 1].trim_space()
		if trimmed.starts_with('pub ') || trimmed.starts_with('pub(') || trimmed == 'pub' {
			return true
		}
	}
	return false
}

// signature_of renders a declaration the way a caller would write it.
fn signature_of(decl astquery.Declaration) string {
	prefix := decl.kind.str()
	return match decl.kind.str() {
		'fn', 'method' {
			receiver := if decl.receiver == '' { '' } else { '${decl.receiver}.' }
			'fn ${receiver}${decl.name}() ${decl.type_name}'.trim_space()
		}
		'field', 'enum_value' {
			'${prefix} ${decl.name} ${decl.type_name}'.trim_space()
		}
		else {
			'${prefix} ${decl.name}'
		}
	}
}

// stdlib_doc_json answers `v_stdlib_doc`.
//
// `symbol` is either a module name such as `strings` or a symbol such as
// `strings.Builder`. Without a member it lists the module's documented symbols;
// with one it returns that symbol in full.
fn stdlib_doc_json(ws &Workspace, symbol string) string {
	trimmed := symbol.trim_space()
	mut name := trimmed
	mut member := ''
	// `strings.Builder` splits at the last dot, so a nested type name survives.
	dot := name.last_index('.') or { -1 }
	if dot > 0 {
		member = name[dot + 1..]
		name = name[..dot]
	}
	dir := module_path(ws, name) or {
		return error_json('`${name}` is not a module this V installation can import')
	}
	files := module_files(dir)
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('symbol')
	w.string(symbol)
	w.key('module')
	w.string(name)
	w.key('module_path')
	w.string(ws.relative(dir))
	w.key('found')
	w.boolean(true)
	w.key('module_doc')
	w.string(module_doc(files))
	if member == '' {
		docs := symbol_docs(ws, dir)
		w.key('symbol_count')
		w.number(docs.len)
		w.key('symbols')
		w.begin_array()
		for doc in docs {
			w.array_raw(symbol_doc_json(doc))
		}
		w.end_array()
		w.end_object()
		return w.str()
	}
	// A symbol is addressed by its bare name; a method also answers to its
	// receiver-qualified form, so `Builder.write` finds `Builder.write`.
	mut matches := []SymbolDoc{}
	for doc in symbol_docs(ws, dir) {
		if doc.name == member {
			matches << doc
		}
	}
	if matches.len == 0 {
		w.key('found')
		w.boolean(false)
		w.key('hint')
		w.string("`${member}` is not documented in `${name}`; call it without a member to list the module's documented symbols")
		w.end_object()
		return w.str()
	}
	w.key('symbol_count')
	w.number(matches.len)
	w.key_raw('member', symbol_doc_json(matches[0]))
	if matches.len > 1 {
		w.key('other_members')
		w.begin_array()
		for doc in matches[1..] {
			w.array_raw(symbol_doc_json(doc))
		}
		w.end_array()
	}
	w.end_object()
	return w.str()
}

// symbol_doc_json renders one documented symbol.
fn symbol_doc_json(doc SymbolDoc) string {
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('name')
	w.string(doc.name)
	w.key('kind')
	w.string(doc.kind)
	w.key('signature')
	w.string(doc.signature)
	w.key('file')
	w.string(doc.file)
	w.key('line')
	w.number(doc.line)
	w.key('doc')
	w.string(doc.doc)
	w.end_object()
	return w.str()
}
