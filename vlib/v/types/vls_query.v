module types

import os
import v.flat

// VlsMethod is what a `-line-info` request of the mini-VLS protocol asks for.
pub enum VlsMethod {
	hover
	definition
	signature_help
	completion
	inlay_hints
}

// VlsQuery is a parsed `-line-info <file>:<line>:<code><column>` request.
pub struct VlsQuery {
pub:
	method VlsMethod
	path   string
	line   int // 1-based
	col    int // 0-based byte column, as VLS sends it
	// The file or directory being checked, as the command line wrote it: the
	// answers write the paths of its files the same way.
	target string
}

// parse_vls_line_info parses the value of `-line-info`. The line is 1-based and
// the column is the 0-based byte column of the cursor. The code before the
// column selects the query: `hv^` hover, `gd^` definition, `fn^` signature
// help, `ih^` inlay hints of the whole file, and a bare column completion.
pub fn parse_vls_line_info(spec string) !VlsQuery {
	format_err := 'wrong format, use `-line-info "file.v:24:hv^7"`'
	parts := spec.split(':')
	if parts.len < 3 {
		return error(format_err)
	}
	path := parts[..parts.len - 2].join(':')
	line := parts[parts.len - 2].int()
	third := parts[parts.len - 1]
	if (!path.ends_with('.v') && !path.ends_with('.vv')) || line < 1 || third.len == 0 {
		return error(format_err)
	}
	method, col_text := match true {
		third.starts_with('hv^') { VlsMethod.hover, third[3..] }
		third.starts_with('gd^') { VlsMethod.definition, third[3..] }
		third.starts_with('fn^') { VlsMethod.signature_help, third[3..] }
		third.starts_with('ih^') { VlsMethod.inlay_hints, third[3..] }
		third[0].is_digit() { VlsMethod.completion, third }
		else { return error(format_err) }
	}
	return VlsQuery{
		method: method
		path:   path
		line:   line
		col:    col_text.int()
	}
}

// parse_vls_line_infos parses a `-line-info` value that asks one question, or
// several separated by tabs: a client that verifies many positions, as a rename
// does, asks them all from one check. `target` is what the command line checks.
pub fn parse_vls_line_infos(spec string, target string) ![]VlsQuery {
	mut queries := []VlsQuery{}
	for part in spec.split('\t') {
		query := parse_vls_line_info(part)!
		queries << VlsQuery{
			...query
			target: target
		}
	}
	return queries
}

// VlsTarget is the node a query is about, and the source of its file.
struct VlsTarget {
	id      flat.NodeId
	file_id int
	start   int // the byte span of the name the cursor is on
	end     int
	source  string
}

// vls_answer answers `q` from the checked program: the text a query prints,
// or '' when the position has nothing to say. It runs after the semantic
// check, errors or not, as the mini-VLS protocol expects an answer for code
// that is still being written.
pub fn (mut tc TypeChecker) vls_answer(q VlsQuery) string {
	file_id := tc.vls_file_id(q.path) or { return '' }
	file := tc.a.source_files[file_id] or { return '' }
	source := os.read_file(file.name) or { return '' }
	// Inlay hints are about the whole file, whatever position comes with them.
	if q.method == .inlay_hints {
		return tc.vls_inlay_hints(file_id, source)
	}
	if q.line > file.line_count() {
		return ''
	}
	line_start := file.line_start(q.line)
	// A column past the end of its line is no position of that line.
	line_end := source.index_after('\n', line_start) or { source.len }
	if q.col > line_end - line_start {
		return ''
	}
	offset := line_start + q.col
	// Signature help is about the call the cursor is in, not a name under it.
	if q.method == .signature_help {
		return tc.vls_signature_help(file_id, offset)
	}
	if q.method == .completion {
		return tc.vls_completion(file_id, offset, source)
	}
	target := tc.vls_target_at(file_id, offset, source) or {
		return tc.vls_answer_type_word(q, file_id, source, offset)
	}
	return match q.method {
		.hover { tc.vls_hover(target) }
		.definition { tc.vls_definition(target, q.target) }
		else { '' }
	}
}

// vls_answer_type_word answers for a type written in a declaration: the type
// of a field or of a parameter, a return type, a receiver's type. The parser
// keeps those as text, without nodes of their own.
fn (mut tc TypeChecker) vls_answer_type_word(q VlsQuery, file_id int, source string, offset int) string {
	tc.vls_enter_file(file_id)
	for word in vls_type_words_at(source, offset) {
		match q.method {
			.hover {
				if declaration := tc.vls_type_declaration(word) {
					return vls_hover_json(declaration, '')
				}
			}
			.definition {
				if at := tc.vls_type_definition(word) {
					return tc.vls_position_text(at.file_id, at.offset, q.target)
				}
			}
			else {
				return ''
			}
		}
	}
	return ''
}

// vls_type_words_at returns the names a type written at `offset` may have:
// with its module first, `models.User` whether the cursor is on `models` or on
// `User`, then the bare word.
fn vls_type_words_at(source string, offset int) []string {
	word := vls_word_at(source, offset) or { return [] }
	mut start := offset
	for start > 0 && vls_is_name_byte(source[start - 1]) {
		start--
	}
	end := start + word.len
	mut words := []string{}
	if start > 1 && source[start - 1] == `.` {
		if before := vls_word_at(source, start - 1) {
			words << '${before}.${word}'
		}
	}
	if end + 1 < source.len && source[end] == `.` && vls_is_name_byte(source[end + 1]) {
		if after := vls_word_at(source, end + 1) {
			words << '${word}.${after}'
		}
	}
	words << word
	return words
}

fn vls_is_name_byte(c u8) bool {
	return c.is_letter() || c.is_digit() || c == `_`
}

// vls_word_at returns the identifier the byte `offset` of `source` is on, or
// just past.
fn vls_word_at(source string, offset int) ?string {
	if offset < 0 || offset > source.len {
		return none
	}
	mut start := offset
	for start > 0 && (source[start - 1].is_letter() || source[start - 1].is_digit()
		|| source[start - 1] == `_`) {
		start--
	}
	mut end := offset
	for end < source.len && (source[end].is_letter() || source[end].is_digit() || source[end] == `_`) {
		end++
	}
	if start == end || source[start].is_digit() {
		return none
	}
	return source[start..end]
}

// vls_file_id finds the file of the program that `path` names.
fn (tc &TypeChecker) vls_file_id(path string) ?int {
	want := os.real_path(path)
	for id, file in tc.a.source_files {
		if os.real_path(file.name) == want {
			return id
		}
	}
	return none
}

// vls_target_at returns the innermost node of `file_id` whose name span holds
// `offset`: from its first byte to just past its last one, so that a cursor
// at the end of a name, where it is while typing, still counts.
fn (tc &TypeChecker) vls_target_at(file_id int, offset int, source string) ?VlsTarget {
	mut best := VlsTarget{
		id: flat.NodeId(-1)
	}
	mut best_len := max_int
	for idx in tc.a.user_code_start .. tc.a.nodes.len {
		node := tc.a.nodes[idx]
		if node.pos.id != file_id {
			continue
		}
		mut start, mut end := 0, 0
		if node.kind == .field_init {
			start, end = tc.vls_field_init_name_span(node, source) or { continue }
		} else if node.kind == .param && !vls_spells_its_name(node, source) {
			// A method's receiver: its node has the position of the method.
			start, end = tc.vls_receiver_name_span(flat.NodeId(idx), node, source) or { continue }
		} else {
			start, end = vls_name_span(node, source) or { continue }
		}
		if offset < start || offset > end {
			continue
		}
		if end - start < best_len {
			best_len = end - start
			best = VlsTarget{
				id:      flat.NodeId(idx)
				file_id: file_id
				start:   start
				end:     end
				source:  source
			}
		}
	}
	if int(best.id) < 0 {
		return none
	}
	return best
}

// vls_field_init_name_span is the span of `name` in `name: value`, a field of
// a struct literal or a named argument: the parser keeps no position for it.
fn (tc &TypeChecker) vls_field_init_name_span(node flat.Node, source string) ?(int, int) {
	if node.value == '' || node.children_count == 0 {
		return none
	}
	value := tc.a.child_node(&node, 0)
	start := vls_field_name_start(source, int(value.pos.offset), node.value) or { return none }
	return start, start + node.value.len
}

// vls_spells_its_name reports whether the span of `node` is its name.
fn vls_spells_its_name(node flat.Node, source string) bool {
	start := int(node.pos.offset)
	end := int(node.pos.end)
	return start >= 0 && end - start == node.value.len && node.value.len > 0
		&& vls_holds_at(source, start, node.value)
}

// vls_receiver_name_span is the span of the name of the method receiver `id`,
// declared in `source`.
fn (tc &TypeChecker) vls_receiver_name_span(id flat.NodeId, node flat.Node, source string) ?(int, int) {
	at := tc.vls_receiver_name_at(id, &node, source)?
	return at.offset, at.offset + node.value.len
}

// vls_embeds_its_type reports whether a struct field whose name ends at
// `name_end` is written as a type alone, `Base` or `Box[int]`: it embeds that
// type, and stands for it.
fn vls_embeds_its_type(source string, name_end int) bool {
	if name_end < source.len && source[name_end] == `[` {
		return true
	}
	mut i := name_end
	for i < source.len && source[i] in [` `, `\t`] {
		i++
	}
	return i >= source.len || source[i] in [`\n`, `\r`, `}`] || vls_holds_at(source, i, '//')
}

// vls_name_span returns the byte span of the name a node stands for: the
// identifier itself, the member of a selector, the type of a cast or of a
// struct literal, the value of an enum shorthand without its dot, the name a
// declaration introduces.
fn vls_name_span(node flat.Node, source string) ?(int, int) {
	start := int(node.pos.offset)
	end := int(node.pos.end)
	if node.kind in [.field_decl, .const_field, .interface_field, .fn_decl] {
		// A declaration starts with its name, which its node may not span; a
		// method's node names its receiver's type too.
		name := if node.kind == .fn_decl { vls_declared_fn_name(node.value) } else { node.value }
		name_end := start + name.len
		if name.len == 0 || !vls_holds_at(source, start, name) {
			return none
		}
		if node.kind == .field_decl && vls_embeds_its_type(source, name_end) {
			return none
		}
		return start, name_end
	}
	if start < 0 || end > source.len || start >= end {
		return none
	}
	match node.kind {
		.ident {
			return start, end
		}
		.param {
			// A receiver's position is not its name: only a parameter whose
			// span spells its name is a target.
			if end - start != node.value.len || !vls_holds_at(source, start, node.value) {
				return none
			}
			return start, end
		}
		.selector {
			member := node.value
			if member.len == 0 || member.len > end - start
				|| !vls_holds_at(source, end - member.len, member) {
				return none
			}
			return end - member.len, end
		}
		.enum_val {
			if source[start] == `.` {
				return start + 1, end
			}
			return start, end
		}
		.enum_field {
			// The field where its enum declares it: the node spans its name.
			return start, end
		}
		.cast_expr, .struct_init {
			name := node.value
			if name.len == 0 || start + name.len > end || !vls_holds_at(source, start, name) {
				return none
			}
			return start, start + name.len
		}
		.is_expr, .as_expr {
			// `x is Type`: the type ends the node.
			name := node.value
			if name.len == 0 || name.len > end - start || !vls_holds_at(source, end - name.len, name) {
				return none
			}
			return end - name.len, end
		}
		else {
			return none
		}
	}
}

// vls_declared_fn_name is the name a function declaration writes after its
// receiver: `label` of the method `Box.label`, `new` of the static method
// `User.new`, which the parser names apart from the methods.
fn vls_declared_fn_name(value string) string {
	if _, method := flat.decode_static_type_method_name(value) {
		return method
	}
	return value.all_after_last('.')
}

// vls_holds_at reports whether `source` holds `word` at `start`. It compares in
// place: the questions of a query look at every node of a file, and a copy of a
// part of the file for each would cost as much as the file, each time.
fn vls_holds_at(source string, start int, word string) bool {
	if start < 0 || start + word.len > source.len {
		return false
	}
	for i in 0 .. word.len {
		if source[start + i] != word[i] {
			return false
		}
	}
	return true
}

// vls_blanks_start returns where the spaces and tabs that end at `end` start.
fn vls_blanks_start(source string, end int) int {
	mut i := end
	for i > 0 && (source[i - 1] == ` ` || source[i - 1] == `\t`) {
		i--
	}
	return i
}

// vls_last_index_before returns where `word` last starts in `source` and ends
// before `end`.
fn vls_last_index_before(source string, end int, word string) ?int {
	mut i := end - word.len
	for i >= 0 {
		if vls_holds_at(source, i, word) {
			return i
		}
		i--
	}
	return none
}

// vls_hover_json is the hover answer: the declaration in a `v` code block,
// then its documentation.
fn vls_hover_json(declaration string, doc string) string {
	mut lines := ['```v', declaration, '```']
	if doc.len > 0 {
		lines << ''
		lines << doc
	}
	value := lines.join('\n').replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')
	return '{"contents":{"kind":"markdown","value":"${value}"}}'
}
