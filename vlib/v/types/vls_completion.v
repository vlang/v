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
// functions, types and consts of the module it names. The code has to parse:
// a client asking right after the dot writes a placeholder name there first.
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
	if receiver.kind == .ident && tc.vls_expr_type(receiver_id) == none {
		module_name := tc.imports[receiver.value] or { receiver.value }
		details = tc.vls_module_details(module_name)
	} else {
		typ := tc.vls_expr_type(receiver_id) or { return '' }
		details = tc.vls_type_details(typ)
	}
	details.sort(a.label < b.label)
	return vls_details_json(details)
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
// `offset` is on, or just past.
fn (tc &TypeChecker) vls_selector_at(file_id int, offset int, source string) ?flat.NodeId {
	for idx in tc.a.user_code_start .. tc.a.nodes.len {
		node := tc.a.nodes[idx]
		if node.kind != .selector || node.pos.id != file_id {
			continue
		}
		start, end := vls_name_span(node, source) or { continue }
		if offset >= start && offset <= end {
			return flat.NodeId(idx)
		}
	}
	return none
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
			alias_name := t.name
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
	owner := match t {
		Struct, Interface, Enum, SumType {
			t.name()
		}
		else {
			// A primitive type has methods only: `int.hex`, `f64.str`.
			tc.vls_method_details(t.name(), mut details, mut seen)
			return details
		}
	}
	fields := tc.structs[owner] or { tc.interface_fields[owner] or { []StructField{} } }
	for field in fields {
		details << VlsDetail{
			kind:   5
			label:  field.name
			detail: tc.vls_type_text(field.typ)
		}
	}
	tc.vls_method_details(owner, mut details, mut seen)
	return details
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

// vls_module_details lists what the module `module_name` makes public: its
// functions, types and consts.
fn (tc &TypeChecker) vls_module_details(module_name string) []VlsDetail {
	prefix := '${module_name}.'
	mut details := []VlsDetail{}
	for key, owner_module in tc.fn_type_modules {
		if owner_module != module_name || !key.starts_with(prefix) {
			continue
		}
		name := key[prefix.len..]
		if name.contains('.') || name.contains('@') || !tc.vls_visible(key) {
			continue
		}
		ret := tc.fn_ret_types[key] or { Type(void_) }
		details << VlsDetail{
			kind:        3
			label:       name
			detail:      tc.vls_return_detail(key, ret)
			declaration: tc.vls_fn_signature(key) or { '' }
		}
	}
	for key, index in tc.first_type_declaration_ids {
		if !key.starts_with(prefix) || key[prefix.len..].contains('.') || !tc.vls_visible(key) {
			continue
		}
		kind := match tc.a.nodes[index].kind {
			.struct_decl { 22 }
			.enum_decl { 13 }
			.interface_decl { 8 }
			else { 7 }
		}
		details << VlsDetail{
			kind:  kind
			label: key[prefix.len..]
		}
	}
	for key, _ in tc.const_types {
		if !key.starts_with(prefix) || key[prefix.len..].contains('.') || !tc.vls_visible(key) {
			continue
		}
		details << VlsDetail{
			kind:  21
			label: key[prefix.len..]
		}
	}
	return details
}

// vls_visible reports whether the file being completed may use `name`: a
// declaration of its own module, or a public one.
fn (tc &TypeChecker) vls_visible(name string) bool {
	visibility := tc.declaration_visibility[name] or { return true }
	return visibility.is_pub || visibility.module_name == tc.cur_module
}
