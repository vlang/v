module c

import v.flat

// The standard builder copies its argument. Keep every converted part alive,
// and delay all writes until receiver/operand evaluation has finished.
fn (mut g FlatGen) gen_builder_interpolation_call(node &flat.Node, target string, resolved string) bool {
	$if ownership ? {
		return false
	}
	if g.tc.autofree_enabled() || node.children_count != 3
		|| target !in ['strings.Builder.write_string', 'strings__Builder__write_string']
		|| (resolved != '' && resolved != 'strings.Builder.write_string') {
		return false
	}
	arg_id := g.a.child(node, 2)
	arg := g.a.nodes[int(arg_id)]
	mut parts := []flat.NodeId{}
	interpolation := arg.kind == .string_interp && arg.value == '__v3_string_join'
	if interpolation {
		parts = g.a.children_of(&arg)
	} else if g.is_string_plus_call(arg) {
		// Preserve nested joins: they may snapshot bytes before later side effects.
		parts = [g.a.child(&arg, 1), g.a.child(&arg, 2)]
	} else if _ := g.interpolation_buffer_size(arg_id, 0) {
		parts = [arg_id]
	} else {
		return false
	}
	if parts.len == 0 { return false }
	params := g.param_types_for('strings.Builder.write_string', 'write_string')
	if params.len != 2 { return false }
	name := '__v3_internal_symbol_builder_${g.tmp_count}'
	g.tmp_count++
	g.write('({ Array* ${name}_receiver = ')
	g.gen_arg_for_expected_type(g.a.child(node, 1), params[0])
	g.write('; string ${name}[${parts.len}]; ')
	g.gen_interpolation_part_assignments(parts, name, interpolation)
	write := g.direct_call_name('strings.Builder.write_string')
	// A string can point into spare capacity, a sliced prefix, or even the
	// receiver header. Snapshot all pieces together before the first mutation.
	g.write('if (v3_builder_parts_alias(${name}_receiver, ${parts.len}, ${name})) { ')
	g.write('string ${name}_copy = string_plus_many(${parts.len}, ${name}); ')
	g.write('${write}(${name}_receiver, ${name}_copy); if (!${name}_copy.is_lit) v_free(${name}_copy.str); ')
	g.write('} else { for (int ${name}_i = 0; ${name}_i < ${parts.len}; ++${name}_i) ')
	g.write('${write}(${name}_receiver, ${name}[${name}_i]); } (void)0; })')
	return true
}

fn (mut g FlatGen) builder_interpolation_helpers() {
	g.writeln('static inline bool v3_interpolation_ranges_overlap(u64 a, u64 a_len, u64 b, u64 b_len) { return a_len && b_len && (a <= b ? b - a < a_len : a - b < b_len); }')
	g.writeln('static inline bool v3_builder_parts_alias(Array* b, int count, const string* parts) {')
	g.writeln(' u64 header = (u64)(uintptr_t)b, data = (u64)(uintptr_t)b->data - (u64)b->offset, size = (u64)b->offset + (u64)b->cap;')
	g.writeln(' for (int i = 0; i < count; ++i) { if (parts[i].len <= 0) continue; u64 source = (u64)(uintptr_t)parts[i].str, len = (u64)parts[i].len;')
	g.writeln('  if (v3_interpolation_ranges_overlap(source, len, header, sizeof(Array)) || (b->data && v3_interpolation_ranges_overlap(source, len, data, size))) return true;')
	g.writeln(' } return false; }')
}
