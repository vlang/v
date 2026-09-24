module types

import strings
import v.flat

// The LSP InlayHintKind values.
const vls_hint_kind_type = 1
const vls_hint_kind_parameter = 2

// The keywords that can come before an argument, where it then starts.
const vls_argument_keywords = ['mut', 'shared']

// VlsInlayHint is a label the editor shows before the byte `offset` of the file.
struct VlsInlayHint {
	offset  int
	label   string
	kind    int
	tooltip string
}

// vls_inlay_hints answers an `ih^` request with the hints of the whole file:
// the types of variables and consts, the names of the parameters in calls, the
// implicit `err` of the `or {}` and `else {}` blocks after a Result, the bounds
// of ranges, the values enum fields get implicitly, the field names in a
// positional struct literal and the fields a keyed one lists out of order. The
// answer is one JSON line, with 0-based lines and byte columns:
// `{"inlay_hints":[{"line":0,"col":0,"label":": int","kind":1,"tooltip":""}]}`
fn (mut tc TypeChecker) vls_inlay_hints(file_id int, source string) string {
	file := tc.a.source_files[file_id] or { return '' }
	tc.vls_enter_file(file_id)
	mut hints := []VlsInlayHint{}
	for idx in tc.a.user_code_start .. tc.a.nodes.len {
		node := tc.a.nodes[idx]
		if node.pos.id != file_id {
			continue
		}
		id := flat.NodeId(idx)
		match node.kind {
			.decl_assign { tc.vls_decl_assign_hints(id, node, mut hints) }
			.for_in_stmt { tc.vls_for_in_hints(id, node, source, mut hints) }
			.const_field { tc.vls_const_hint(node, mut hints) }
			.enum_decl { tc.vls_enum_hints(node, mut hints) }
			.call { tc.vls_param_name_hints(id, node, source, mut hints) }
			.or_expr { tc.vls_or_err_hint(node, source, mut hints) }
			.if_expr { tc.vls_else_err_hint(node, source, mut hints) }
			.index { tc.vls_slice_hints(node, source, mut hints) }
			.struct_init { tc.vls_struct_init_hints(id, node, source, mut hints) }
			else {}
		}
	}
	hints.sort_with_compare(fn (a &VlsInlayHint, b &VlsInlayHint) int {
		return a.offset - b.offset
	})
	mut seen := map[string]bool{}
	mut sb := strings.new_builder(hints.len * 64 + 32)
	sb.write_string('{"inlay_hints":[')
	for hint in hints {
		key := '${hint.offset}:${hint.label}'
		if key in seen {
			continue
		}
		if seen.len > 0 {
			sb.write_string(',')
		}
		seen[key] = true
		line, col := file.find_line_and_column(hint.offset)
		sb.write_string('{"line":${line - 1},"col":${col - 1},"label":"${vls_json_escape(hint.label)}","kind":${hint.kind},"tooltip":"${vls_json_escape(hint.tooltip)}"}')
	}
	sb.write_string(']}')
	return sb.str()
}

// vls_decl_assign_hints shows the type of each variable `x := ...` declares,
// and `if x := ...` too.
fn (tc &TypeChecker) vls_decl_assign_hints(id flat.NodeId, node flat.Node, mut hints []VlsInlayHint) {
	if tc.vls_in_generic_fn(id) {
		return
	}
	for lhs_id in tc.multi_assign_lhs_ids(node) {
		tc.vls_variable_type_hint(lhs_id, mut hints)
	}
}

// vls_for_in_hints shows the types of the variables of a `for ... in` loop,
// and the bounds of a range it walks: `0 ≤.. <3`.
fn (tc &TypeChecker) vls_for_in_hints(id flat.NodeId, node flat.Node, source string, mut hints []VlsInlayHint) {
	// The loop's two variables, one of them maybe absent, and what it walks.
	if node.children_count < 3 {
		return
	}
	if !tc.vls_in_generic_fn(id) {
		tc.vls_variable_type_hint(tc.a.child(&node, 0), mut hints)
		tc.vls_variable_type_hint(tc.a.child(&node, 1), mut hints)
	}
	container_id := tc.a.child(&node, 2)
	if !tc.valid_node_id(container_id) {
		return
	}
	container := tc.a.node(container_id)
	if container.kind != .range || container.children_count != 2 {
		return
	}
	low := tc.a.child_node(container, 0)
	high := tc.a.child_node(container, 1)
	at := vls_index_between(source, '..', int(low.pos.end), int(high.pos.offset)) or { return }
	hints << vls_type_label_hint(at, '≤')
	hints << vls_type_label_hint(at + 2, '<')
}

// vls_variable_type_hint shows the type of the variable the ident `id`
// declares after its name.
fn (tc &TypeChecker) vls_variable_type_hint(id flat.NodeId, mut hints []VlsInlayHint) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind != .ident || node.value == '_' {
		return
	}
	typ := tc.vls_local_type(id) or { return }
	tc.vls_type_hint(int(node.pos.end), typ, mut hints)
}

fn (tc &TypeChecker) vls_type_hint(offset int, typ Type, mut hints []VlsInlayHint) {
	if typ is Void || type_contains_unknown(typ) {
		return
	}
	name := vls_strip_main_module(tc.vls_type_text(typ))
	if name == '' || name == 'void' {
		return
	}
	hints << vls_type_label_hint(offset, ': ${name}')
}

fn vls_type_label_hint(offset int, label string) VlsInlayHint {
	return VlsInlayHint{
		offset: offset
		label:  label
		kind:   vls_hint_kind_type
	}
}

fn vls_parameter_label_hint(offset int, label string) VlsInlayHint {
	return VlsInlayHint{
		offset: offset
		label:  label
		kind:   vls_hint_kind_parameter
	}
}

// vls_in_generic_fn reports whether `id` is in the body of a generic function,
// whose variables have the types of the instance being checked.
fn (tc &TypeChecker) vls_in_generic_fn(id flat.NodeId) bool {
	mut cur := tc.vls_parent_id(id)
	for tc.valid_node_id(cur) {
		node := tc.a.node(cur)
		if node.kind == .fn_decl {
			return node.generic_params().len > 0 || node.value.contains('[')
		}
		cur = tc.vls_parent_id(cur)
	}
	return false
}

// vls_const_hint shows the type of a const after its name.
fn (tc &TypeChecker) vls_const_hint(node flat.Node, mut hints []VlsInlayHint) {
	name := node.value.all_after_last('.')
	if name == '_' || node.value.starts_with('C.') {
		return
	}
	typ := tc.vls_const_type(node.value) or { return }
	tc.vls_type_hint(int(node.pos.offset) + name.len, typ, mut hints)
}

// vls_enum_hints shows the value of each field of an enum that has none
// written: the one after the previous field's, or the bit of a `@[flag]` enum.
fn (tc &TypeChecker) vls_enum_hints(node flat.Node, mut hints []VlsInlayHint) {
	values := tc.vls_enum_values(node)
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .enum_field || field.children_count > 0 {
			continue
		}
		value := values[field.value] or { continue }
		hints << vls_type_label_hint(int(field.pos.end), ' = ${value}')
	}
}

// vls_param_name_hints shows before each argument of a call the name of its
// parameter, unless the argument says it already: a variable of that name, or
// the fields of `f(name: value)`.
fn (mut tc TypeChecker) vls_param_name_hints(call_id flat.NodeId, call flat.Node, source string, mut hints []VlsInlayHint) {
	if call.children_count < 2 {
		return
	}
	resolved := tc.vls_call_target(call_id, tc.a.child(&call, 0)) or { return }
	if resolved.starts_with('C.') || resolved.starts_with('JS.') {
		return
	}
	sig := tc.vls_signature(resolved) or { return }
	for i in 1 .. call.children_count {
		if i - 1 >= sig.names.len {
			break
		}
		name := sig.names[i - 1]
		arg := tc.a.child_node(&call, i)
		if arg.kind == .field_init {
			// `f(name: value)` fills the struct of this parameter.
			if fields := tc.vls_struct_fields(sig.types[i - 1]) {
				inits := []flat.NodeId{len: int(call.children_count) - i, init: tc.a.child(&call,
					i + index)}
				tc.vls_field_order_hints(fields, inits, source, mut hints)
			}
			break
		}
		if name in ['', '_'] || (arg.kind == .ident && arg.value == name) {
			continue
		}
		start := int(arg.pos.offset)
		if start < 0 || start > source.len {
			continue
		}
		hints << vls_parameter_label_hint(vls_keyword_start(source, start), '${name}: ')
	}
}

// vls_keyword_start moves `offset` back over a `mut ` or `shared ` written
// before an argument, as the argument starts there.
fn vls_keyword_start(source string, offset int) int {
	before_end := vls_blanks_start(source, offset)
	for keyword in vls_argument_keywords {
		start := before_end - keyword.len
		if vls_holds_at(source, start, keyword)
			&& (start == 0 || !vls_is_name_byte(source[start - 1])) {
			return start
		}
	}
	return offset
}

// vls_or_err_hint marks the implicit `err` of an `or {}` block of more than one
// line after a call that returns a Result.
fn (mut tc TypeChecker) vls_or_err_hint(node flat.Node, source string, mut hints []VlsInlayHint) {
	if node.children_count < 2 || !tc.vls_returns_result(tc.a.child(&node, 0)) {
		return
	}
	block := tc.a.child_node(&node, 1)
	start := int(block.pos.offset)
	end := int(block.pos.end)
	if block.kind != .block || start < 0 || end > source.len || start >= end
		|| source[start] != `{` || !source[start..end].contains('\n') {
		return
	}
	hints << vls_parameter_label_hint(start + 1, ' err →')
}

// vls_else_err_hint marks the implicit `err` of the `else` block of an
// `if x := f() {`, when `f` returns a Result and the block goes on past the
// line of its `{`.
fn (mut tc TypeChecker) vls_else_err_hint(node flat.Node, source string, mut hints []VlsInlayHint) {
	if node.children_count < 3 {
		return
	}
	guard := tc.a.child_node(&node, 0)
	if guard.kind != .decl_assign || !tc.vls_returns_result(tc.multi_assign_rhs_id(guard, 0)) {
		return
	}
	// `err` is only in the `else` of the guard itself, not in an `else if`.
	block := tc.a.child_node(&node, 2)
	start := int(block.pos.offset)
	if block.kind != .block || start < 0 || start >= source.len || source[start] != `{` {
		return
	}
	line_end := source.index_after('\n', start) or { source.len }
	rest := source[start + 1..line_end].trim_space()
	if rest == '' || rest.starts_with('//') {
		hints << vls_parameter_label_hint(start + 1, ' err →')
	}
}

// vls_returns_result reports whether `id` is a call of a function that
// returns a Result.
fn (mut tc TypeChecker) vls_returns_result(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	if resolved := tc.vls_call_target(id, tc.a.child(node, 0)) {
		if ret := tc.fn_ret_types[resolved] {
			return ret is ResultType
		}
	}
	typ := tc.expr_type(id) or { return false }
	return typ is ResultType
}

// vls_slice_hints marks the bounds of a slice, `a[1 ≤..< 3]`: the low one is
// in it, the high one is not.
fn (tc &TypeChecker) vls_slice_hints(node flat.Node, source string, mut hints []VlsInlayHint) {
	if node.value != 'range' || node.children_count < 2 {
		return
	}
	// `a[..3]` has an empty low bound, and `a[1..]` none high.
	low := tc.a.child_node(&node, 1)
	has_low := low.kind != .empty
	has_high := node.children_count > 2
	from := if has_low { int(low.pos.end) } else { int(tc.a.child_node(&node, 0).pos.end) }
	to := if has_high { int(tc.a.child_node(&node, 2).pos.offset) } else { source.len }
	at := vls_index_between(source, '..', from, to) or { return }
	if has_low {
		hints << vls_type_label_hint(at, '≤')
	}
	if has_high {
		hints << vls_type_label_hint(at + 2, '<')
	}
}

// vls_index_between is where `needle` first is in `source` from `from`, if it
// ends by `to`.
fn vls_index_between(source string, needle string, from int, to int) ?int {
	if from < 0 || from > source.len {
		return none
	}
	at := source.index_after(needle, from) or { return none }
	if at + needle.len > to {
		return none
	}
	return at
}

// vls_struct_init_hints names the fields of a positional struct literal,
// `Point{x: 3, y: 4}`, and warns about a field a keyed one lists before one
// its struct declares first.
fn (tc &TypeChecker) vls_struct_init_hints(id flat.NodeId, node flat.Node, source string, mut hints []VlsInlayHint) {
	// The checker keeps no type for a struct literal: it is the one it names.
	typ := tc.expr_type(id) or { tc.parse_type(node.value.all_before('[')) }
	fields := tc.vls_struct_fields(typ) or { return }
	mut inits := []flat.NodeId{}
	mut position := 0
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		field_init := tc.a.node(child_id)
		if field_init.kind != .field_init || field_init.children_count == 0 {
			continue
		}
		if field_init.value != '' {
			inits << child_id
			continue
		}
		if position < fields.len {
			value := tc.a.child_node(field_init, 0)
			hints << vls_parameter_label_hint(int(value.pos.offset), '${vls_field_name(fields[position])}: ')
		}
		position++
	}
	tc.vls_field_order_hints(fields, inits, source, mut hints)
}

// vls_struct_fields is the fields of the struct `typ` is, through aliases and
// pointers: for an instance of a generic one, those of its declaration.
fn (tc &TypeChecker) vls_struct_fields(typ Type) ?[]StructField {
	base := vls_unwrap_type(typ)
	if base !is Struct {
		return none
	}
	name := (base as Struct).name
	return tc.structs[name] or { tc.structs[name.all_before('[')] or { return none } }
}

// vls_field_name is the name code gives a field: an embedded struct's is its
// type's, without its module.
fn vls_field_name(field StructField) string {
	return field.name.all_after_last('.')
}

// vls_field_order_hints warns about each field of `inits`, the keyed fields of
// a struct literal or the named arguments of a call, listed before one that
// `fields` declares first.
fn (tc &TypeChecker) vls_field_order_hints(fields []StructField, inits []flat.NodeId, source string, mut hints []VlsInlayHint) {
	names := fields.map(vls_field_name(it))
	mut max_seen := -1
	for id in inits {
		field_init := tc.a.node(id)
		if field_init.kind != .field_init || field_init.children_count == 0 {
			continue
		}
		declared := names.index(field_init.value)
		if declared < 0 {
			continue
		}
		if declared >= max_seen {
			max_seen = declared
			continue
		}
		value_start := int(tc.a.child_node(field_init, 0).pos.offset)
		name_start := vls_field_name_start(source, value_start, field_init.value) or { continue }
		hints << VlsInlayHint{
			offset:  name_start
			label:   '⚠ '
			kind:    vls_hint_kind_parameter
			tooltip: 'Field "${field_init.value}" is out of declaration order'
		}
	}
}

// vls_field_name_start finds `name: ` before the value of a field of a struct
// literal, which starts at `value_start`: the parser keeps no position for the
// name.
fn vls_field_name_start(source string, value_start int, name string) ?int {
	if value_start <= 0 || value_start > source.len {
		return none
	}
	colon_end := vls_blanks_start(source, value_start)
	if colon_end == 0 || source[colon_end - 1] != `:` {
		return none
	}
	name_end := vls_blanks_start(source, colon_end - 1)
	name_start := name_end - name.len
	if name_start < 0 || !vls_holds_at(source, name_start, name)
		|| (name_start > 0 && vls_is_name_byte(source[name_start - 1])) {
		return none
	}
	return name_start
}

// vls_strip_main_module turns `main.Point` into `Point` everywhere in a type,
// `[]main.Point` and `map[string]main.Point` too, but leaves `domain.Point`.
fn vls_strip_main_module(name string) string {
	mut sb := strings.new_builder(name.len)
	mut i := 0
	for i < name.len {
		if name[i..].starts_with('main.')
			&& (i == 0 || !(vls_is_name_byte(name[i - 1]) || name[i - 1] == `.`)) {
			i += 5
			continue
		}
		sb.write_u8(name[i])
		i++
	}
	return sb.str()
}
