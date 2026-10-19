module c

import v.flat
import v.types { unalias_type }
import v.gen.c.naming { sum_field_name }

fn (mut g FlatGen) gen_sum_local_acquisition_expr(node &flat.Node) bool {
	if node.children_count != 2 { return false }
	sum := unalias_type(g.tc.parse_type(node.typ))
	if sum !is types.SumType { return false }
	source_id := g.a.child(node, 1)
	source := g.a.nodes[int(source_id)]
	if source.kind != .ident || !g.ierror_local_pointer_is_owned(source.value) { return false }
	pointer := g.usable_expr_type(source_id)
	variant := g.sum_variant_for_actual(sum.name_id, pointer) or { return false }
	field := sum_field_name(flat.source_type_name(variant))
	tag := g.sum_type_index(sum.name_id, variant)
	tmp := g.tmp_name()
	ct := g.value_c_type(sum)
	g.write('({ ${ct} ${tmp} = ')
	g.gen_expr(g.a.child(node, 0))
	g.write('; if (${tmp}.typ == ${tag} && (void*)${tmp}.${field} == (void*)')
	g.gen_expr(source_id)
	g.write(') { ${tmp}._pointer_variant_is_owned = true; } ${tmp}; })')
	return true
}

// gen_sum_local_transfer_return acquires a promoted value's allocation only at
// the return that actually carries its address. In-scope sum wrappers borrow it.
fn (mut g FlatGen) gen_sum_local_transfer_return(node flat.Node, id flat.NodeId) bool {
	if node.children_count != 1 || g.cur_fn_ret_is_optional { return false }
	sum := unalias_type(g.cur_fn_ret)
	if sum !is types.SumType { return false }
	fn_name := qualify_name_in_module(g.tc.cur_module, g.cur_fn_name)
	source_id := transformed_return_source_id(node.value) or { id }
	sources := g.tc.ownership_sum_return_sources(fn_name, source_id)
	if sources.len == 0 { return false }
	ct := g.return_c_type()
	tmp := g.tmp_name()
	mut guards := map[string]string{}
	mut transfers := []string{}
	for source in sources {
		typ := g.tc.parse_type(source.type_name)
		clean_source := unalias_type(typ)
		if clean_source is types.SumType {
			local_ct := g.local_storage_c_type(source.name) or { continue }
			cname := g.local_cname(source.name)
			name := if cgen_c_type_pointer_depth(local_ct) > cgen_c_type_pointer_depth(g.tc.c_type(typ)) {
				'(*${cname})'
			} else {
				cname
			}
			for variant in g.tc.sum_types.type_ids(clean_source.name()) or { []u32{} } {
				pointer := unalias_type(g.tc.parse_canonical_type(variant))
				if pointer !is types.Pointer { continue }
				returned_variant := g.sum_variant_for_actual(sum.name_id, g.tc.parse_canonical_type(variant)) or { continue }
				field := sum_field_name(flat.source_type_name(variant))
				returned_field := sum_field_name(flat.source_type_name(returned_variant))
				tag := g.sum_type_index(clean_source.name_id, variant)
				returned_tag := g.sum_type_index(sum.name_id, returned_variant)
				guard := '${name}._pointer_variant_is_owned && ${name}.typ == ${tag} && ${tmp}.typ == ${returned_tag} && (void*)${tmp}.${returned_field} == (void*)${name}.${field}'
				transfers << 'if (${guard}) { ${tmp}._pointer_variant_is_owned = true; ${name}._pointer_variant_is_owned = false; }'
			}
			continue
		}
		// The source was a value in the original body. Only an actual owning
		// heap indirection created by escape lowering can be transferred.
		if !g.ierror_local_pointer_is_owned(source.name) { continue }
		local_ct := g.local_storage_c_type(source.name) or { continue }
		if cgen_c_type_pointer_depth(local_ct) <= cgen_c_type_pointer_depth(g.tc.c_type(typ)) {
			continue
		}
		ptr := types.Type(types.Pointer{ base_type: g.tc.intern_type_reference(typ) })
		variant := g.sum_variant_for_actual(sum.name_id, ptr) or { continue }
		field := sum_field_name(flat.source_type_name(variant))
		tag := g.sum_type_index(sum.name_id, variant)
		name := g.local_cname(source.name)
		guards[source.name] = '(${tmp}.typ == ${tag} && (void*)${tmp}.${field} == (void*)${name})'
	}
	if guards.len == 0 && transfers.len == 0 { return false }
	ret_id := g.a.child(&node, 0)
	ret_node := g.a.nodes[int(ret_id)]
	expr := g.return_expr_string(node, ret_id, ret_node, ct)
	g.gen_return_expr_loop_control_copybacks()
	g.writeln('${ct} ${tmp} = ${expr};')
	for _, guard in guards {
		g.writeln('if (${guard}) { ${tmp}._pointer_variant_is_owned = true; }')
	}
	for transfer in transfers { g.writeln(transfer) }
	// Defers see the original live value. Destruction retains its existing order,
	// except that the allocation acquired above is left for the caller to drop.
	drops := g.cur_return_drops
	g.cur_return_drops = []types.OwnershipDropEntry{}
	g.gen_return_cleanup_with_result(tmp)
	g.cur_return_drops = drops
	for entry in drops {
		if guard := guards[entry.name] {
			g.writeln('if (!${guard}) {')
			g.indent++
			g.gen_ownership_drops([entry])
			g.indent--
			g.writeln('}')
		} else {
			g.gen_ownership_drops([entry])
		}
	}
	g.writeln('return ${tmp};')
	return true
}
