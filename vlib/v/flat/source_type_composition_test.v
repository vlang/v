module flat

fn test_source_type_composition_collisions_preserve_owned_exact_tuples() {
	left := source_type_id('composition_collision.Left')
	middle := source_type_id('composition_collision.Middle')
	replacement := source_type_id('composition_collision.Replacement')
	right := source_type_id('composition_collision.Right')
	original := [left, middle, right]
	mut borrowed := original.clone()
	// Force the otherwise impractical full-hash collision. Equal bucket/hash,
	// tuple length and endpoints must still compare every numeric component.
	hash := u64(0x4f39_a7b2_529d_06cd)
	slot := int(hash & u64(source_type_composition_slots - 1))
	first := source_type_composition_miss(slot, .multi_return, borrowed, hash)
	borrowed[1] = replacement
	changed := source_type_composition_miss(slot, .multi_return, borrowed, hash)
	assert first != changed
	assert source_type_name(first) == '(composition_collision.Left, composition_collision.Middle, composition_collision.Right)'
	assert source_type_name(changed) == '(composition_collision.Left, composition_collision.Replacement, composition_collision.Right)'
	assert source_type_composition_find(slot, .multi_return, original, hash).id == first
	assert source_type_composition_find(slot, .multi_return, borrowed, hash).id == changed
	// The same numeric tuple and forced hash can describe a different spelling
	// constructor, and a prefix of the tuple is a different cache key.
	function := source_type_composition_miss(slot, .void_function_type, original, hash)
	shorter := source_type_composition_miss(slot, .multi_return, original[..2], hash)
	assert function != first
	assert shorter != first
	assert source_type_name(function) == 'fn(composition_collision.Left, composition_collision.Middle, composition_collision.Right)'
	assert source_type_name(shorter) == '(composition_collision.Left, composition_collision.Middle)'
	assert source_type_composition_find(slot, .multi_return, original, hash).id == first
	assert source_type_composition_find(slot, .void_function_type, original, hash).id == function
	assert source_type_composition_find(slot, .multi_return, original[..2], hash).id == shorter
}

fn test_source_type_composition_concatenates_every_part() {
	parts := [source_type_id('Box['), source_type_id('int'), source_type_id(', '),
		source_type_id('[]string'), source_type_id(']'), u32(0)]
	for _ in 0 .. 2 {
		assert source_type_composition(.concat, parts) == source_type_id('Box[int, []string]')
		assert source_type_composition(.concat, parts[..1]) == parts[0]
		assert source_type_composition(.concat, []u32{}) == 0
		assert source_type_composition(.concat, [u32(0)]) == 0
	}
}

fn test_source_type_composition_caches_empty_spelling() {
	// Force a known key so the exact cache entry can be checked, even when its
	// spelling ID is zero. A miss must differ from a cached empty spelling.
	hash := u64(0x61b3_46c2_68aa_b208)
	slot := int(hash & u64(source_type_composition_slots - 1))
	parts := []u32{}
	assert source_type_composition_miss(slot, .concat, parts, hash) == 0
	entry := source_type_composition_find(slot, .concat, parts, hash)
	assert !isnil(entry)
	assert entry.id == 0
	assert source_type_composition_miss(slot, .concat, parts, hash) == 0
	assert source_type_composition_find(slot, .concat, parts, hash) == entry
	assert isnil(source_type_composition_find(slot, .concat, [u32(0)], hash))
}

fn test_source_type_composition_inferred_expression_keeps_numeric_node_id() {
	for node_id in [u32(0), 42, u32(0x8000_0000), u32(0xffff_ffff)] {
		for _ in 0 .. 2 {
			assert source_type_composition_one(.inferred_expression, node_id) == source_type_id('typeof(__expr_${int(node_id)})')
		}
	}
}

fn test_source_type_composition_fastc_thread_handle_preserves_codec() {
	inputs := ['', 'void', 'void ', 'int', 'pkg.Type', '&[]u8', 'a_b', 'a_5fb', 'é']
	expected := ['__vf_thread_void', '__vf_thread_void', '__vf_thread_kvoid_20', '__vf_thread_kint',
		'__vf_thread_kpkg_2eType', '__vf_thread_k_26_5b_5du8', '__vf_thread_ka_5fb',
		'__vf_thread_ka_5f5fb', '__vf_thread_k_c3_a9']
	for i, raw in inputs {
		for _ in 0 .. 2 {
			assert source_type_composition_one(.fastc_thread_handle, source_type_id(raw)) == source_type_id(expected[i])
		}
	}
	assert source_type_composition_one(.fastc_thread_handle, source_type_id([u8(0), 255].bytestr())) == source_type_id('__vf_thread_k_00_ff')
}

fn test_source_type_composition_fastc_component_preserves_raw_codec() {
	inputs := ['', 'int', 'pkg.Type', '*', '_ptr', 'a*b c.d', 'fn (pkg.T*)', '\t T\n', 'é.*']
	expected := ['', 'int', 'pkg__Type', '_ptr', '_ptr', 'a_ptrb_c__d', 'fn_(pkg__T_ptr)', '\t_T\n',
		'é___ptr']
	for i, raw in inputs {
		id := source_type_id(raw)
		for _ in 0 .. 2 {
			assert source_type_composition_one(.fastc_composite_part, id) == source_type_id(expected[i])
		}
	}
	zero := [u8(0)].bytestr()
	last := [u8(255)].bytestr()
	raw := zero + ' *.' + last
	assert source_type_composition_one(.fastc_composite_part, source_type_id(raw)) == source_type_id(zero + '__ptr__' + last)
}

fn test_source_type_composition_lazy_name_queries_keep_numeric_keys() {
	id := source_type_id('ptrmain.A__B')
	for _ in 0 .. 2 {
		assert source_type_composition_one(.ptr_name_base, id) == source_type_id('main.A__B')
		assert source_type_composition_one(.main_removed, id) == source_type_id('ptrA__B')
		assert source_type_composition_one(.last_double_underscore_prefix, id) == source_type_id('ptrmain.A')
		assert source_type_composition_one(.last_double_underscore_suffix, id) == source_type_id('B')
		assert source_type_composition_one(.doubleunderscore_dotted, id) == source_type_id('ptrmain.A.B')
		assert source_type_composition_one(.last_double_underscore_prefix, source_type_id('__')) == 0
		assert source_type_composition_one(.last_double_underscore_suffix, source_type_id('__')) == 0
		assert source_type_composition_one(.main_removed, source_type_id('main.')) == 0
		assert source_type_composition_one(.ptr_name_base, source_type_id('ptr')) == 0
	}
}

fn test_source_type_composition_numeric_keys_share_immutable_entry_allocation() {
	slot := 3985
	hash := u64(0x7f31e004ab9c)
	mut borrowed := [source_type_id('compact_composition_tail_a'), u32(0),
		source_type_id('compact_composition_tail_b')]
	original := borrowed.clone()
	id := source_type_composition_miss(slot, .concat, borrowed, hash)
	entry := source_type_composition_find(slot, .concat, original, hash)
	assert !isnil(entry)
	assert entry.id == id
	assert entry.len == original.len
	assert unsafe { &u8(entry.parts()) } == unsafe {
		&u8(entry) + sizeof(SourceTypeCompositionEntry)
	}
	borrowed[0] = source_type_id('compact_composition_tail_changed')
	for index, part in original {
		assert unsafe { entry.parts()[index] } == part
	}
	assert source_type_composition_find(slot, .concat, original, hash) == entry
	assert isnil(source_type_composition_find(slot, .concat, borrowed, hash))
	empty := []u32{}
	assert source_type_composition_miss(slot, .concat, empty, hash) == 0
	empty_entry := source_type_composition_find(slot, .concat, empty, hash)
	assert !isnil(empty_entry)
	assert empty_entry.len == 0
	assert source_type_composition_find(slot, .concat, original, hash) == entry
}

struct SourceTypeCompositionScopeResult {
	generic u32
	joined  u32
	pointer u32
}

fn source_type_composition_in_recycled_scope() SourceTypeCompositionScopeResult {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		left := source_type_id('composition_scratch_scope632.Left'.clone())
		right := source_type_id('composition_scratch_scope632.Right'.clone())
		mut parts := [left, right]
		generic := source_type_composition(.generic, parts)
		joined := source_type_composition(.multi_return, parts)
		pointer := source_type_composition_one(.pointer, left)
		for id in [generic, joined, pointer] {
			assert !unsafe { prealloc_scope_owns(scope, source_type_name(id).str) }
		}
		parts[0] = right
		assert source_type_composition(.generic, [left, right]) == generic
		unsafe { prealloc_scope_end(scope) }
		for _ in 0 .. 4 {
			reused := unsafe { prealloc_scope_begin() }
			mut overwrite := []u8{len: 192 * 1024}
			for index in 0 .. overwrite.len { overwrite[index] = u8(0x65) }
			unsafe { prealloc_scope_end(reused) }
		}
		unsafe { prealloc_thread_cleanup() }
		return SourceTypeCompositionScopeResult{
			generic: generic
			joined:  joined
			pointer: pointer
		}
	} $else {
		left := source_type_id('composition_scratch_scope632.Left')
		right := source_type_id('composition_scratch_scope632.Right')
		return SourceTypeCompositionScopeResult{
			generic: source_type_composition(.generic, [left, right])
			joined:  source_type_composition(.multi_return, [left, right])
			pointer: source_type_composition_one(.pointer, left)
		}
	}
}

fn test_source_type_composition_outlives_render_and_caller_scratch_arenas() {
	handle := spawn source_type_composition_in_recycled_scope()
	ids := handle.wait()
	left := source_type_id('composition_scratch_scope632.Left')
	right := source_type_id('composition_scratch_scope632.Right')
	assert ids.generic == source_type_id('composition_scratch_scope632.Left[composition_scratch_scope632.Right]')
	assert ids.joined == source_type_id('(composition_scratch_scope632.Left, composition_scratch_scope632.Right)')
	assert ids.pointer == source_type_id('&composition_scratch_scope632.Left')
	assert source_type_composition(.generic, [left, right]) == ids.generic
	assert source_type_composition(.multi_return, [left, right]) == ids.joined
	assert source_type_composition_one(.pointer, left) == ids.pointer
}

fn test_source_type_composition_small_render_preserves_raw_pieces() {
	kinds := [SourceTypeComposition.pointer, .array, .option, .result, .channel, .mutable_channel,
		.mutable_parameter, .variadic_parameter, .shared_type, .atomic_type, .thread]
	prefixes := ['&', '[]', '?', '!', 'chan ', 'chan mut ', 'mut ', '...', 'shared ', 'atomic ',
		'thread ']
	raw_inputs := ['', ' int ', 'T[', 'é', [u8(0), 255].bytestr()]
	for index, kind in kinds {
		for raw in raw_inputs {
			parts := [source_type_id(raw)]
			expected := source_type_id(prefixes[index] + raw)
			short := source_type_composition_small_id(kind, parts) or {
				panic('short unary composition used the long rendering path')
			}
			assert short == expected
			assert source_type_composition(kind, parts) == expected
		}
	}
	for kind in [SourceTypeComposition.qualified, .named_parameter, .map, .fixed_array,
		.fixed_array_suffix] {
		for left in raw_inputs {
			for right in raw_inputs {
				parts := [source_type_id(left), source_type_id(right)]
				expected := match kind {
					.qualified { '${left}.${right}' }
					.named_parameter { '${left} ${right}' }
					.map { 'map[${left}]${right}' }
					.fixed_array { '[${left}]${right}' }
					.fixed_array_suffix { '${right}[${left}]' }
					else { panic('unexpected binary test constructor') }
				}
				short := source_type_composition_small_id(kind, parts) or {
					panic('short binary composition used the long rendering path')
				}
				assert short == source_type_id(expected)
				assert source_type_composition(kind, parts) == short
			}
		}
	}
}

fn test_source_type_composition_small_render_bounds_and_owned_bytes() {
	retained := source_type_composition_small_id(.pointer,
		[source_type_id('stack_composition_owned_729')]) or { panic('short rendering failed') }
	for length in [0, 1, 253, 254, 255, 256, 1024] {
		raw := 'x'.repeat(length)
		parts := [source_type_id(raw)]
		if short := source_type_composition_small_id(.pointer, parts) {
			assert length < 255
			assert short == source_type_id('&' + raw)
		} else {
			assert length >= 255
		}
		assert source_type_composition(.pointer, parts) == source_type_id('&' + raw)
		left := 'l'.repeat(length / 2)
		right := 'r'.repeat(length - length / 2)
		binary := [source_type_id(left), source_type_id(right)]
		if short := source_type_composition_small_id(.qualified, binary) {
			assert length < 255
			assert short == source_type_id('${left}.${right}')
		} else {
			assert length >= 255
		}
		assert source_type_composition(.qualified, binary) == source_type_id('${left}.${right}')
	}
	assert source_type_name(retained) == '&stack_composition_owned_729'
	if _ := source_type_composition_small_id(.generic, [source_type_id('T')]) {
		assert false, 'complex compositions must retain the scoped rendering path'
	}
}
