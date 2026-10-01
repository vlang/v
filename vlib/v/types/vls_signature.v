module types

import v.flat

// vls_signature_help answers a signature help request with the signature of
// the call the cursor is in, on its name or among its arguments, and the
// argument the cursor is on as the active parameter. V1 answered only on the
// call's name, always with the first parameter active, and wrote the
// parameters as one object with repeated keys; this writes one per parameter.
fn (mut tc TypeChecker) vls_signature_help(file_id int, offset int) string {
	call_id := tc.vls_call_at(file_id, offset) or { return '' }
	call := tc.a.node(call_id)
	if call.children_count == 0 {
		return ''
	}
	tc.vls_enter_file(file_id)
	resolved := tc.vls_call_target(call_id, tc.a.child(call, 0)) or { return '' }
	sig := tc.vls_signature(resolved) or { return '' }
	// The arguments that end before the cursor come before the active one.
	mut active := 0
	for i in 1 .. call.children_count {
		arg := tc.a.child_node(call, i)
		if int(arg.pos.end) < offset {
			active = i
		}
	}
	if sig.params.len > 0 && active >= sig.params.len {
		active = sig.params.len - 1
	}
	label := '${sig.name}(${sig.params.join(', ')})${sig.ret}'
	params := sig.params.map('{"label":"${vls_json_escape(it)}"}').join(',')
	return '{"signatures":[{"label":"${vls_json_escape(label)}","parameters":[${params}]}],"activeSignature":0,"activeParameter":${active}}'
}

// vls_call_at returns the innermost call of `file_id` whose text holds the byte
// `offset`: from the start of its name to its closing parenthesis.
fn (tc &TypeChecker) vls_call_at(file_id int, offset int) ?flat.NodeId {
	mut best := flat.NodeId(-1)
	mut best_len := max_int
	for idx in tc.a.user_code_start .. tc.a.nodes.len {
		node := tc.a.nodes[idx]
		if node.kind != .call || node.pos.id != file_id {
			continue
		}
		start := int(node.pos.offset)
		end := int(node.pos.end)
		if offset < start || offset > end || end - start >= best_len {
			continue
		}
		best = flat.NodeId(idx)
		best_len = end - start
	}
	if int(best) < 0 {
		return none
	}
	return best
}

// vls_json_escape escapes a string for a JSON string literal.
fn vls_json_escape(s string) string {
	return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\t', '\\t')
}
