module types

import v.flat

// VlsDetail is one completion item of V1's mini-VLS protocol.
struct VlsDetail {
	kind        int // the LSP CompletionItemKind
	label       string
	detail      string
	declaration string
}

// vls_completion answers a completion request for the member being written
// after a dot: the fields and methods of the value before it, or the public
// functions, types and consts of the module it names. A dot with no name after
// it gets one before the file is parsed (see vls_complete_source); a client may
// write a placeholder name there itself.
fn (mut tc TypeChecker) vls_completion(file_id int, offset int, source string) string {
	selector_id := tc.vls_selector_at(file_id, offset, source) or {
		return tc.vls_call_name_completion(file_id, offset, source)
	}
	selector := tc.a.node(selector_id)
	if selector.children_count == 0 {
		return ''
	}
	tc.vls_enter_file(file_id)
	receiver_id := tc.a.child(selector, 0)
	receiver := tc.a.node(receiver_id)
	mut details := []VlsDetail{}
	if types := tc.vls_constraint_set_types(receiver_id) {
		details = tc.vls_common_details(types)
	} else if receiver.kind == .ident && tc.vls_expr_type(receiver_id) == none {
		// Only a module that the file imports has members to offer: V1 listed
		// none for `strings.` in a file without `import strings`.
		module_name := tc.file_imports[file_import_key(tc.cur_file, receiver.value)] or {
			return ''
		}
		// In the order V1 listed them, which vls_module_details keeps.
		return vls_details_json(tc.vls_module_details(module_name, source))
	} else {
		typ := tc.vls_expr_type(receiver_id) or { return '' }
		details = tc.vls_type_details(typ)
	}
	details.sort(a.label < b.label)
	return vls_details_json(details)
}

// vls_constraint_set_types are the types of the set that the type of the value
// `id` names as its constraint, when its type is such a type parameter.
fn (tc &TypeChecker) vls_constraint_set_types(id flat.NodeId) ?[]Type {
	mut typ := tc.expr_type(id) or { Type(Unknown{}) }
	node := tc.a.node(id)
	if node.kind == .ident {
		if decl_id := tc.vls_local_declaration(id) {
			decl := tc.a.node(decl_id)
			if decl.kind == .param {
				typ = tc.parse_type(decl.typ)
			}
		}
	}
	constraint := tc.vls_type_constraint(id, typ)?
	if constraint.is_interface {
		return none
	}
	return constraint.types
}

// vls_common_details are the members that every one of `types` has, as the
// first of them describes them.
fn (mut tc TypeChecker) vls_common_details(types []Type) []VlsDetail {
	if types.len == 0 {
		return []VlsDetail{}
	}
	mut common := tc.vls_type_details(types[0])
	for typ in types[1..] {
		labels := tc.vls_type_details(typ).map(it.label)
		common = common.filter(it.label in labels)
	}
	return common
}

// vls_call_name_completion answers for the name of a called function, as V1
// did: that function alone, with its signature.
fn (mut tc TypeChecker) vls_call_name_completion(file_id int, offset int, source string) string {
	call_id := tc.vls_call_at(file_id, offset) or { return '' }
	call := tc.a.node(call_id)
	if call.children_count == 0 {
		return ''
	}
	callee_id := tc.a.child(call, 0)
	start, end := vls_name_span(tc.a.node(callee_id), source) or { return '' }
	if offset < start || offset > end {
		return ''
	}
	tc.vls_enter_file(file_id)
	resolved := tc.vls_call_target(call_id, callee_id) or { return '' }
	declaration := tc.vls_fn_signature(resolved) or { return '' }
	return vls_details_json([
		VlsDetail{
			kind:        3
			label:       resolved.all_after_last('.').all_after_last('@')
			declaration: declaration
		},
	])
}

fn vls_details_json(details []VlsDetail) string {
	items := details.map('{"kind":${it.kind},"label":"${vls_json_escape(it.label)}","detail":"${vls_json_escape(it.detail)}","declaration":"${vls_json_escape(it.declaration)}","documentation":""}')
	return '{"details": [${items.join(',')}]}'
}

// vls_selector_at returns the selector of `file_id` whose member the byte
// `offset` is on, or just past, or else the one whose dot `offset` is just
// past: `x.` at the end of a line, whose member the next line names.
fn (tc &TypeChecker) vls_selector_at(file_id int, offset int, source string) ?flat.NodeId {
	mut after_dot := -1
	for idx in tc.a.user_code_start .. tc.a.nodes.len {
		node := tc.a.nodes[idx]
		if node.kind != .selector || node.pos.id != file_id {
			continue
		}
		start, end := vls_name_span(node, source) or { continue }
		if offset >= start && offset <= end {
			return flat.NodeId(idx)
		}
		if after_dot < 0 && node.children_count > 0 && offset > 0 && offset <= source.len
			&& source[offset - 1] == `.` {
			receiver := tc.a.child_node(&node, 0)
			if int(receiver.pos.end) == offset - 1 {
				after_dot = idx
			}
		}
	}
	if after_dot < 0 {
		return none
	}
	return flat.NodeId(after_dot)
}

// vls_type_details lists the members of a value of type `typ`: the fields and
// methods of a struct or an interface, the methods of an enum, a sum type or an
// alias, and the length of a string, an array or a map. Builtin declares the
// methods of arrays and maps for any element, `first() voidptr`: they are left
// out, as V1 left them, and the index of VLS lists them for the element type.
fn (tc &TypeChecker) vls_type_details(typ Type) []VlsDetail {
	mut details := []VlsDetail{}
	mut seen := map[string]bool{}
	mut t := typ
	for _ in 0 .. 16 {
		if t is Pointer {
			t = t.base_type
		} else if t is Alias {
			alias_name := t.name()
			t = t.base_type
			tc.vls_method_details(alias_name, mut details, mut seen)
		} else {
			break
		}
	}
	if t is Array || t is ArrayFixed || t is Map || t is String {
		details << VlsDetail{
			kind:   10
			label:  'len'
			detail: 'int'
		}
		if t is Array {
			details << VlsDetail{
				kind:   10
				label:  'cap'
				detail: 'int'
			}
		}
		if t is String {
			tc.vls_method_details('string', mut details, mut seen)
		}
		return details
	}
	owner_id := match t {
		Struct, Interface, Enum, SumType {
			t.source_type_id()
		}
		else {
			// A primitive type has methods only: `int.hex`, `f64.str`.
			tc.vls_method_details(t.name(), mut details, mut seen)
			return details
		}
	}
	owner := flat.source_type_name(owner_id)
	fields := tc.structs[owner_id] or { tc.interface_fields[owner_id] or { []StructField{} } }
	for field in fields {
		if t is Struct && !tc.vls_field_is_usable(typ, owner_id, field.name) {
			continue
		}
		details << VlsDetail{
			kind:   5
			label:  field.name
			detail: tc.vls_type_text(field.typ)
		}
	}
	tc.vls_method_details(owner, mut details, mut seen)
	return details
}

// vls_field_is_usable reports whether the file being completed may use the
// field `field` of the struct `owner` on a value of type `typ`, by the rules
// the checker applies to a selector: through an alias, the alias's module
// decides; a struct private to another module has no field to use there, but
// for the public ones of an anonymous struct; else the field's `pub` does.
fn (tc &TypeChecker) vls_field_is_usable(typ Type, owner u32, field string) bool {
	receiver := unwrap_all_pointers(typ)
	if receiver is Alias {
		return !tc.alias_struct_field_is_private_outside_module(receiver, field)
	}
	if visibility := tc.private_type_declaration(owner) {
		return tc.is_synthesized_anon_struct(owner)
			&& tc.anonymous_struct_field_is_public(owner, field, visibility.module_name)
	}
	return !tc.struct_field_is_private_outside_module(owner, field)
}

// vls_method_details adds the methods of the type `owner` the file being
// completed may call, the ones in `seen` excepted.
fn (tc &TypeChecker) vls_method_details(owner string, mut details []VlsDetail, mut seen map[string]bool) {
	prefix := '${owner}.'
	for key, ret in tc.fn_ret_types {
		if !key.starts_with(prefix) {
			continue
		}
		method := key[prefix.len..]
		if method.contains('.') || method.contains('@') || method in seen
			|| !tc.vls_visible(key) {
			continue
		}
		seen[method] = true
		details << VlsDetail{
			kind:   2
			label:  method
			detail: tc.vls_return_detail(key, ret)
		}
	}
}

// vls_return_detail is the return type of the function `name` as a completion
// item shows it: `void` when it returns nothing, and as declared when generic.
fn (tc &TypeChecker) vls_return_detail(name string, ret Type) string {
	if sig := tc.vls_signature(name) {
		if sig.ret.len > 0 {
			return sig.ret.trim_space()
		}
	}
	if ret is Void {
		return 'void'
	}
	return tc.vls_type_text(ret)
}

// VlsRankedDetail is a completion item, and where it comes among the others.
struct VlsRankedDetail {
	rank   int
	detail VlsDetail
}

// vls_module_details lists what the module `module_name` makes public, as V1
// listed it: its functions, then its types, then its consts, each in the order
// of their declarations, but for the types that `source`, the file being
// completed, names: those come first, in the order that it names them first,
// where V1 registered them.
fn (tc &TypeChecker) vls_module_details(module_name string, source string) []VlsDetail {
	prefix := '${module_name}.'
	mut fns := []VlsRankedDetail{}
	for key, owner_module in tc.fn_type_modules {
		if owner_module != module_name || !key.starts_with(prefix) {
			continue
		}
		name := key[prefix.len..]
		if name.contains('.') || name.contains('@') || !tc.vls_visible(key) {
			continue
		}
		ret := tc.fn_ret_types[key] or { Type(void_) }
		fns << VlsRankedDetail{
			rank:   if decl_id := tc.vls_fn_decl_id(key) { int(decl_id) } else { max_int }
			detail: VlsDetail{
				kind:        3
				label:       name
				detail:      tc.vls_return_detail(key, ret)
				declaration: tc.vls_fn_signature(key) or { '' }
			}
		}
	}
	named := tc.vls_module_members_named(source, module_name)
	mut types := []VlsRankedDetail{}
	for key_id, index in tc.first_type_declaration_ids {
		key := flat.source_type_name(key_id)
		if !key.starts_with(prefix) || key[prefix.len..].contains('.') || !tc.vls_visible(key) {
			continue
		}
		decl := tc.a.nodes[index]
		// V1 offered no function type, `type Callback = fn (int)`.
		if decl.kind == .type_decl && vls_is_fn_type_text(decl.typ_text()) {
			continue
		}
		kind := match decl.kind {
			.struct_decl { 22 }
			.enum_decl { 13 }
			.interface_decl { 8 }
			else { 7 }
		}
		label := key[prefix.len..]
		types << VlsRankedDetail{
			rank:   named[label] or { named.len + index }
			detail: VlsDetail{
				kind:  kind
				label: label
			}
		}
	}
	mut consts := []VlsRankedDetail{}
	for key, _ in tc.const_types {
		if !key.starts_with(prefix) || key[prefix.len..].contains('.') || !tc.vls_visible(key) {
			continue
		}
		consts << VlsRankedDetail{
			rank:   if expr_id := tc.const_exprs[key] { int(expr_id) } else { max_int }
			detail: VlsDetail{
				kind:  21
				label: key[prefix.len..]
			}
		}
	}
	mut details := []VlsDetail{cap: fns.len + types.len + consts.len}
	vls_add_ranked_details(mut details, mut fns)
	vls_add_ranked_details(mut details, mut types)
	vls_add_ranked_details(mut details, mut consts)
	return details
}

// vls_add_ranked_details adds the items `ranked` to `details`, by their rank.
fn vls_add_ranked_details(mut details []VlsDetail, mut ranked []VlsRankedDetail) {
	ranked.sort_with_compare(fn (a &VlsRankedDetail, b &VlsRankedDetail) int {
		if a.rank != b.rank {
			return if a.rank < b.rank { -1 } else { 1 }
		}
		return compare_strings(a.detail.label, b.detail.label)
	})
	for item in ranked {
		details << item.detail
	}
}

// vls_module_members_named returns the names that `source`, a file of the
// program, gives members of the module `module_name`, `Name` of `mod.Name`, by
// the order in which it first names them.
fn (tc &TypeChecker) vls_module_members_named(source string, module_name string) map[string]int {
	tokens := vls_source_tokens(source)
	mut named := map[string]int{}
	for i := 0; i + 2 < tokens.len; i++ {
		if tokens[i].kind != .name || tokens[i + 1].kind != .dot || tokens[i + 2].kind != .name
			|| (i > 0 && tokens[i - 1].kind == .dot) {
			continue
		}
		imported := tc.file_imports[file_import_key(tc.cur_file, tokens[i].lit)] or { continue }
		member := tokens[i + 2].lit
		if imported == module_name && member !in named {
			named[member] = named.len
		}
	}
	return named
}

// vls_is_fn_type_text reports whether the type written `text` is a function
// type, `fn (int) string`.
fn vls_is_fn_type_text(text string) bool {
	return text.starts_with('fn') && (text.len == 2 || !vls_is_name_byte(text[2]))
}

// vls_visible reports whether the file being completed may use `name`: a
// declaration of its own module, or a public one.
fn (tc &TypeChecker) vls_visible(name string) bool {
	visibility := tc.declaration_visibility[name] or { return true }
	return visibility.is_pub || visibility.module_name == tc.cur_module
}
