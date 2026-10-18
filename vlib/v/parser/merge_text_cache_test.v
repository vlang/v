// vtest build: !v3_no_parallel?
module parser

import v.flat
import v.pref

fn test_merge_text_probes_preserve_canonical_storage_across_independent_passes() {
	mut a := flat.FlatAst.new()
	for word in ['alpha', 'bravo', 'cider'] {
		a.intern_text(word)
	}
	mut source := []u8{len: 5}
	borrowed := unsafe { tos(source.data, source.len) }
	for word in ['alpha', 'bravo', 'cider', 'alpha', 'ghost'] {
		for i, ch in word {
			source[i] = ch
		}
		// Reusing one source address across passes must not reuse a previous cache hit.
		mut values := new_parse_merge_text_cache()
		mut types := new_parse_merge_text_cache()
		for attempt in 0 .. 2 {
			if attempt == 1 {
				gc_collect()
			}
			value, hit := a.probe_text_ptr_cached(borrowed, mut values.ptrs, mut values.values)
			id, typ, type_hit := a.probe_type_text_ptr_cached(borrowed, mut types.ptrs,
				mut types.values, mut types.ids)
			assert value == word
			assert typ == word
			assert hit == (word != 'ghost')
			assert type_hit == hit
			if hit {
				assert id > 0
				assert value.str == a.text(flat.TextId(id)).str
				assert typ.str == value.str
			} else {
				assert id == 0
			}
		}
		value, hit := a.probe_text_ptr_cached('', mut values.ptrs, mut values.values)
		assert hit
		assert value == ''
		assert a.text_values.len == 3
		unsafe {
			// The cache buffers own none of the canonical AST strings.
			free(values)
			free(types)
		}
		assert a.text(flat.TextId(1)) == 'alpha'
		assert a.text(flat.TextId(2)) == 'bravo'
		assert a.text(flat.TextId(3)) == 'cider'
	}
}

fn test_merge_text_probes_promote_payloads_without_attribute_names() {
	mut master := Parser.new(pref.new_preferences())
	mut worker := Parser.new(pref.new_preferences())
	master.a.intern_text('Counter')
	unsafe { master.a.nodes.grow_len(2) }
	// Both values hit the master table; only typed payload ownership needs a tail pass.
	scope := parser_worker_scope_begin(true)
	enum_payload := flat.node_payload_with_type_metadata([]string{}, flat.source_type_u64,
		[]u32{})
	debugger_payload := flat.node_payload_with_debugger_types([]string{},
		[flat.source_type_int, flat.source_type_u64])
	worker.a.add_node(flat.Node{
		kind:    .enum_decl
		value:   'Counter'
		payload: enum_payload
	})
	worker.a.add_node(flat.Node{
		kind:    .debugger_stmt
		payload: debugger_payload
	})
	parser_worker_scope_leave(scope)
	mut args := ParseMergeCopyArgs{
		master: voidptr(master)
		worker: voidptr(worker)
	}
	parse_merge_copy_thread(voidptr(&args))
	assert args.miss_nodes == [0, 1]
	master.a.intern_node_texts_at(args.miss_nodes)
	assert master.a.nodes[0].payload != enum_payload
	assert master.a.nodes[1].payload != debugger_payload
	parser_worker_scope_free(scope)
	assert master.a.nodes[0].enum_backing_type_id() == flat.source_type_u64
	assert master.a.nodes[0].generic_params().len == 0
	assert master.a.nodes[0].generic_constraint_ids().len == 0
	assert master.a.nodes[1].debugger_value_type_ids() == [flat.source_type_int, flat.source_type_u64]
	assert master.a.nodes[1].generic_params().len == 0
	assert master.a.nodes[1].generic_constraint_ids().len == 0
}
