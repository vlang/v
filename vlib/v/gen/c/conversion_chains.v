module c

import v.flat

// Only adjacent calls to the standard conversions qualify: the intermediate
// string cannot escape, while the source and the result keep independent storage.
fn (mut g FlatGen) gen_temporary_bytes_chain(node &flat.Node, resolved string) bool {
	$if ownership ? {
		return false
	}
	if g.tc.autofree_enabled() || node.children_count != 2
		|| (resolved != '' && resolved !in ['string.bytes', 'string__bytes']) {
		return false
	}
	inner_id := g.a.child(node, 1)
	inner := g.a.nodes[int(inner_id)]
	if inner.kind != .call || inner.children_count != 2 { return false }
	callee_id := g.a.child(&inner, 0)
	callee := g.a.nodes[int(callee_id)]
	if callee.kind != .ident { return false }
	name := callee.value
	is_builder := name in ['strings.Builder.str', 'strings__Builder__str']
	if !is_builder && name !in ['Array_u8__bytestr', '[]u8.bytestr'] { return false }
	if selected := g.resolved_call_name(inner_id) {
		if selected != name && g.cname(selected) != g.cname(name) { return false }
	}
	if _ := g.fn_value_call_param_types(callee_id) { return false }
	base := '__v3_internal_symbol_bytes_chain_${g.tmp_count}'
	g.tmp_count++
	g.write('({ Array${if is_builder { '*' } else { '' }} ${base}_source = ')
	g.gen_call_args(name, &inner, 1)
	g.write('; ')
	if is_builder {
		// str() appends a terminator before clearing. Preserve growth, slice/COW
		// handling, and the builder's reusable buffer, including its trailing zero.
		g.write('u8 ${base}_zero = 0; array__push(${base}_source, &${base}_zero); ')
	}
	length := if is_builder { '${base}_source->len - 1' } else { '(${base}_source).len' }
	data := if is_builder { '${base}_source->data' } else { '(${base}_source).data' }
	// Use the same allocator as string.bytes(), including its empty-array flags.
	// String buffers cannot be adopted as arrays: arrays own allocation metadata.
	g.write('Array ${base}_result = __new_array_noscan(${length}, 0, sizeof(u8)); ')
	g.write('if (${base}_result.len > 0) memcpy(${base}_result.data, ${data}, (size_t)${base}_result.len); ')
	if is_builder { g.write('array__clear(${base}_source); ') }
	g.write('${base}_result; })')
	return true
}
