module flat

fn publish_source_types_in_parallel(names []string, ready chan bool, start chan bool) []u32 {
	ready <- true
	_ := <-start
	mut ids := []u32{cap: names.len}
	for name in names {
		id := source_type_id(name)
		assert source_type_name(id) == name
		assert source_type_shape(id).has(.map_type)
		payload := node_payload_with_type_metadata([name], id, [id])
		entry := node_payload_at(payload)
		assert entry.generic_params == [name]
		assert entry.enum_backing_type == id
		assert entry.debugger_value_types.ids == [id]
		ids << id
	}
	return ids
}

fn test_source_type_and_payload_publication_under_writer_contention() {
	mut names := []string{cap: 2048}
	for n in 0 .. 2048 {
		names << 'map[string][]parallel_publication.Item${n}'
	}
	ready := chan bool{cap: 4}
	start := chan bool{cap: 4}
	mut workers := []thread []u32{}
	for _ in 0 .. 4 {
		workers << spawn publish_source_types_in_parallel(names, ready, start)
	}
	for _ in 0 .. 4 {
		_ := <-ready
	}
	for _ in 0 .. 4 {
		start <- true
	}
	mut expected := []u32{}
	for worker in workers {
		ids := worker.wait()
		if expected.len == 0 {
			expected = ids.clone()
		} else {
			assert ids == expected
		}
	}
	for n, name in names {
		assert source_type_id(name) == expected[n]
	}
}

fn test_type_text_forks_share_immutable_spelling_ids() {
	mut parent := TypeTextBindings{}
	parent.set('value', 'pkg.Sum')
	mut left := parent.fork()
	mut right := parent.fork()
	left.set('value', '?pkg.Left[T]')
	right.set('value', 'map[string]pkg.Right')
	left_id := left.type_id('value') or { panic('missing left binding') }
	right_id := right.type_id('value') or { panic('missing right binding') }
	assert left_id != right_id
	assert parent.texts.text(right_id) == right.value('value')
	assert left.value('value') == '?pkg.Left[T]'
	assert right.value('value') == 'map[string]pkg.Right'
	assert parent.value('value') == 'pkg.Sum'
	parent.set_id('imported', right_id)
	assert parent.value('imported') == 'map[string]pkg.Right'
	assert parent.value_id('imported') == right_id
	assert parent.value_id('absent') == 0
}

fn test_indexed_scope_shadowing_keeps_empty_bindings_present() {
	mut outer := TypeTextBindings{}
	outer.set('placeholder', '')
	outer.set('value', 'pkg.Alias')
	mut inner := outer.clone()
	inner.set('value', '[]T')
	inner.delete('placeholder')
	assert inner.value('value') == '[]T'
	assert outer.value('value') == 'pkg.Alias'
	assert outer.contains('placeholder')
	assert outer.get('placeholder') or { panic('empty binding was lost') } == ''
	assert !inner.contains('placeholder')
	assert outer.value_id('placeholder') == 0
	assert outer.type_id('placeholder') or { panic('empty identity was lost') } == 0
}

fn test_source_type_shape_preserves_exact_spelling_probes() {
	wrapped := source_type_shape(source_type_id('map[string][]pkg.Item'))
	assert wrapped.has(.dotted)
	assert wrapped.has(.bracketed)
	assert !wrapped.has(.wrapped)
	assert source_type_shape(source_type_id('?pkg.Item')).has(.wrapped)
	assert source_type_shape(source_type_id('[]typeof(x)')).has(.typeof_expr)
	assert source_type_shape(source_type_id('typeof_field')).has(.typeof_name)
	assert !source_type_shape(source_type_id('typeof_field')).has(.typeof_expr)
	assert !source_type_shape(source_type_id('typeof (x)')).has(.typeof_expr)
	assert !source_type_shape(0).has(.dotted)
}

fn test_type_text_pointer_probe_validates_reused_input_bytes() {
	mut texts := TypeTextTable{}
	mut bytes := '[]First'.bytes()
	// Borrow one buffer deliberately to exercise reuse of the same input address.
	borrowed := unsafe { tos(bytes.data, bytes.len) }
	first := texts.intern(borrowed)
	for i, value in '[]Other'.bytes() {
		bytes[i] = value
	}
	second := texts.intern(borrowed)
	assert first != second
	assert texts.text(first) == '[]First'
	assert texts.text(second) == '[]Other'
}

fn test_source_type_recent_entry_checks_bytes_and_survives_table_growth() {
	mut bytes := 'cache.Abc'.bytes()
	borrowed := unsafe { tos(bytes.data, bytes.len) }
	first := source_type_id(borrowed)
	// Changing an unsampled middle byte deliberately preserves the cache slot.
	bytes[6] = `x`
	second := source_type_id(borrowed)
	assert first != second
	assert source_type_name(first) == 'cache.Abc'
	assert source_type_name(second) == 'cache.xbc'
	for i in 0 .. 12000 {
		source_type_id('cache.Growing${i}')
	}
	assert source_type_id('cache.Abc'.clone()) == first
	assert source_type_id('cache.xbc'.clone()) == second
	assert source_type_shape(first).has(.dotted)
}

fn test_ast_type_ids_use_the_shared_spelling_namespace() {
	mut first := FlatAst.new()
	first.intern_text('ordinary payload')
	second := FlatAst.new()
	id := first.type_text_id('pkg.Source[T]')
	assert id == second.type_text_id('pkg.Source[T]')
	assert source_type_name(id) == 'pkg.Source[T]'
	assert first.node_type_text_id('pkg.Source[T]', id) == id
	changed := first.node_type_text_id('pkg.Other', id)
	assert changed != id
	assert source_type_name(changed) == 'pkg.Other'
	assert first.node_type_text_id('', changed) == 0
	assert first.type_text_id('') == 0
}

fn test_source_type_shapes_and_builtin_ids_share_the_spelling_table() {
	assert source_type_int == source_type_id('int'.clone())
	assert source_type_name(source_type_string) == 'string'
	assert source_type_shape(source_type_int).has(.builtin_alias)
	shape := source_type_shape(source_type_id('?[]pkg.Item'))
	assert shape.has(.wrapped)
	assert shape.has(.bracketed)
	assert shape.has(.dotted)
	assert !shape.has(.typeof_expr)
	assert source_type_shape(source_type_id('typeof(value)')).has(.typeof_expr)
	assert !source_type_shape(0).has(.builtin_alias)
	assert source_type_shape(source_type_id('?')).has(.optional)
	assert source_type_shape(source_type_id('!pkg.Item')).has(.optional)
	assert !source_type_shape(source_type_id('&pkg.Item')).has(.optional)
	assert source_type_shape(source_type_id('&pkg.Item')).has(.pointer)
	assert source_type_shape(source_type_id('shared pkg.Item')).has(.shared_type)
	assert source_type_shape(source_type_id('...pkg.Item')).has(.variadic)
	assert source_type_shape(source_type_id(' pkg.Item\t')).has(.padded)
	assert !source_type_shape(source_type_id('pkg.Item')).has(.padded)
}

fn test_source_type_resolution_shape_preserves_wrappers_and_exact_spelling() {
	for text in ['int', '?', '!', '&[]u8', '?map[string][]u8', '[]map[int]&string', '[32]u8',
		'&[2][3]int', 'map[[4]u8][]map[string]bool', 'i128', 'u128', 'uint', 'char', 'nil'] {
		assert source_type_shape(source_type_id(text)).has(.context_independent)
	}
	for text in ['map', 'array', 'map[string]Item', 'map[Key]int', 'map[string]int ', ' int',
		'[size]int', '[2 + 3]int', '[0x10]int', 'map[string', '[2', '[2]', 'shared int', '...int',
		'fn () int', 'typeof(value)', 'pkg.int', '&&'] {
		assert !source_type_shape(source_type_id(text)).has(.context_independent)
	}
}

fn test_binding_value_ids_preserve_present_empty_bindings() {
	mut bindings := TypeTextBindings{}
	bindings.set('empty', '')
	bindings.set_id('value', source_type_int)
	assert bindings.value_id('missing') == 0
	assert bindings.value_id('empty') == 0
	assert bindings.contains('empty')
	assert bindings.value_id('value') == source_type_int
	mut nodes := NodeTypeTextBindings{}
	nodes.set_id(12, source_type_string)
	assert nodes.value_id(12) == source_type_string
	assert nodes.value_id(13) == 0
	assert nodes.type_id(12) or { panic('missing node type identity') } == source_type_string
	assert nodes.type_id(13) == none
	nodes.set_id(13, 0)
	assert nodes.type_id(13) or { panic('empty node identity was lost') } == 0
}

fn test_source_type_prefix_flags_describe_raw_spelling() {
	assert source_type_shape(source_type_id('[]')).has(.array_type)
	assert source_type_shape(source_type_id('[]pkg.Item')).has(.array_type)
	assert !source_type_shape(source_type_id(' []pkg.Item')).has(.array_type)
	assert !source_type_shape(0).has(.array_type)
	assert source_type_shape(source_type_id('map[')).has(.map_type)
	assert source_type_shape(source_type_id('map[pkg.Item')).has(.map_type)
	assert !source_type_shape(source_type_id(' map[pkg.Item')).has(.map_type)
	assert !source_type_shape(0).has(.map_type)
	assert source_type_shape(source_type_id('[')).has(.leading_bracket)
	assert source_type_shape(source_type_id('[pkg.Item')).has(.leading_bracket)
	assert !source_type_shape(source_type_id(' [pkg.Item')).has(.leading_bracket)
	assert !source_type_shape(0).has(.leading_bracket)
	assert source_type_shape(source_type_id('?')).has(.option_type)
	assert source_type_shape(source_type_id('?pkg.Item')).has(.option_type)
	assert !source_type_shape(source_type_id(' ?pkg.Item')).has(.option_type)
	assert !source_type_shape(0).has(.option_type)
	assert source_type_shape(source_type_id('!')).has(.result_type)
	assert source_type_shape(source_type_id('!pkg.Item')).has(.result_type)
	assert !source_type_shape(source_type_id(' !pkg.Item')).has(.result_type)
	assert !source_type_shape(0).has(.result_type)
	assert source_type_shape(source_type_id('mut ')).has(.mutable_type)
	assert source_type_shape(source_type_id('mut pkg.Item')).has(.mutable_type)
	assert !source_type_shape(source_type_id(' mut pkg.Item')).has(.mutable_type)
	assert !source_type_shape(0).has(.mutable_type)
	assert source_type_shape(source_type_id('atomic ')).has(.atomic_type)
	assert source_type_shape(source_type_id('atomic pkg.Item')).has(.atomic_type)
	assert !source_type_shape(source_type_id(' atomic pkg.Item')).has(.atomic_type)
	assert !source_type_shape(0).has(.atomic_type)
	assert source_type_shape(source_type_id('chan ')).has(.channel_type)
	assert source_type_shape(source_type_id('chan pkg.Item')).has(.channel_type)
	assert !source_type_shape(source_type_id(' chan pkg.Item')).has(.channel_type)
	assert !source_type_shape(0).has(.channel_type)
	assert source_type_shape(source_type_id('thread ')).has(.thread_type)
	assert source_type_shape(source_type_id('thread pkg.Item')).has(.thread_type)
	assert !source_type_shape(source_type_id(' thread pkg.Item')).has(.thread_type)
	assert !source_type_shape(0).has(.thread_type)
	assert source_type_shape(source_type_id('fn(')).has(.function_compact)
	assert source_type_shape(source_type_id('fn(pkg.Item')).has(.function_compact)
	assert !source_type_shape(source_type_id(' fn(pkg.Item')).has(.function_compact)
	assert !source_type_shape(0).has(.function_compact)
	assert source_type_shape(source_type_id('fn (')).has(.function_spaced)
	assert source_type_shape(source_type_id('fn (pkg.Item')).has(.function_spaced)
	assert !source_type_shape(source_type_id(' fn (pkg.Item')).has(.function_spaced)
	assert !source_type_shape(0).has(.function_spaced)
	assert source_type_shape(source_type_id('&[]')).has(.pointer_array)
	assert source_type_shape(source_type_id('&[]pkg.Item')).has(.pointer_array)
	assert !source_type_shape(source_type_id(' &[]pkg.Item')).has(.pointer_array)
	assert !source_type_shape(0).has(.pointer_array)
	assert source_type_shape(source_type_id('&map[')).has(.pointer_map)
	assert source_type_shape(source_type_id('&map[pkg.Item')).has(.pointer_map)
	assert !source_type_shape(source_type_id(' &map[pkg.Item')).has(.pointer_map)
	assert !source_type_shape(0).has(.pointer_map)
	assert source_type_shape(source_type_id('&?')).has(.pointer_option)
	assert source_type_shape(source_type_id('&?pkg.Item')).has(.pointer_option)
	assert !source_type_shape(source_type_id(' &?pkg.Item')).has(.pointer_option)
	assert !source_type_shape(0).has(.pointer_option)
	assert source_type_shape(source_type_id('&&')).has(.pointer_chain)
	assert source_type_shape(source_type_id('&&pkg.Item')).has(.pointer_chain)
	assert !source_type_shape(source_type_id(' &&pkg.Item')).has(.pointer_chain)
	assert !source_type_shape(0).has(.pointer_chain)

	wrapped := source_type_shape(source_type_id('?[]pkg.Item'))
	assert wrapped.has(.option_type)
	assert wrapped.has(.optional)
	assert !wrapped.has(.array_type)
	assert !wrapped.has(.leading_bracket)
	assert !source_type_shape(source_type_id('map [int]string')).has(.map_type)
	assert !source_type_shape(source_type_id('fn  ()')).has(.function_spaced)
	assert !source_type_shape(source_type_id('fn')).has(.function_compact)
	assert !source_type_shape(source_type_id('shared')).has(.shared_type)
	assert !source_type_shape(source_type_id('threaded')).has(.thread_type)
	assert !source_type_shape(source_type_id('typeof(')).has(.typeof_expr)
}

fn test_source_type_shape_caches_spelling_only_generic_probes() {
	for name in ['T', ' A ', '\nZ\t', 'Q'] {
		assert source_type_shape(source_type_id(name)).has(.bare_generic)
	}
	for name in ['', 'C', ' C ', 'JS', 't', 'TT', 'pkg.T', '[T]', 'T\x00'] {
		assert !source_type_shape(source_type_id(name)).has(.bare_generic)
	}
	for name in ['struct', 'pkg.struct', 'one.two.struct'] {
		assert source_type_shape(source_type_id(name)).has(.anonymous_struct)
	}
	for name in ['Struct', 'astruct', 'struct ', 'pkg.struct.field', 'x__struct'] {
		assert !source_type_shape(source_type_id(name)).has(.anonymous_struct)
	}
	assert source_type_shape(source_type_id('mod__fn_T_int')).has(.generic_c_name)
	assert !source_type_shape(source_type_id('mod__fn_Tint')).has(.generic_c_name)
}

fn test_type_text_fork_cache_checks_reused_buffer_bytes() {
	mut parent := TypeTextTable{}
	mut bytes := '[]First'.bytes()
	borrowed := unsafe { tos(bytes.data, bytes.len) }
	first := parent.intern(borrowed)
	mut fork := parent.fork()
	for i, value in '[]Other'.bytes() {
		bytes[i] = value
	}
	second := fork.intern(borrowed)
	assert second != first
	assert source_type_name(first) == '[]First'
	assert source_type_name(second) == '[]Other'
	assert parent.intern(borrowed) == second
	for i, value in '[]First'.bytes() {
		bytes[i] = value
	}
	assert parent.intern(borrowed) == first
	assert fork.intern(borrowed) == first
}

fn test_type_text_recent_headers_do_not_own_global_spelling_bytes() {
	mut texts := TypeTextTable{}
	input := 'type_texts.CachedHeader'.clone()
	id := texts.intern(input)
	slot := int((usize(input.str) >> 4 ^ usize(input.len)) & 127)
	canonical := source_type_name(id)
	assert texts.recent_texts[slot].str == canonical.str
	mut fork := texts.fork()
	assert fork.recent[slot] == id
	assert fork.recent_texts[slot].str == canonical.str
	// Dropping either cached header must leave the process-owned bytes intact.
	mut parent_header := texts.recent_texts[slot]
	mut fork_header := fork.recent_texts[slot]
	unsafe {
		parent_header.free()
		fork_header.free()
	}
	assert parent_header == 'type_texts.CachedHeader'
	assert fork_header == 'type_texts.CachedHeader'
	assert source_type_name(id) == 'type_texts.CachedHeader'
	assert texts.intern(input) == id
	assert fork.intern(input) == id
}

fn test_type_text_recent_headers_survive_the_input_arena() {
	$if prealloc {
		mut texts := TypeTextTable{}
		scope := unsafe { prealloc_scope_begin() }
		input := 'type_texts.DisposableInput'.clone()
		assert unsafe { prealloc_scope_owns(scope, input.str) }
		id := texts.intern(input)
		slot := int((usize(input.str) >> 4 ^ usize(input.len)) & 127)
		unsafe { prealloc_scope_leave(scope) }
		assert !unsafe { prealloc_scope_owns(scope, texts.recent_texts[slot].str) }
		mut fork := texts.fork()
		assert fork.recent[slot] == id
		assert !unsafe { prealloc_scope_owns(scope, fork.recent_texts[slot].str) }
		unsafe { prealloc_scope_free_after(scope) }
		assert texts.recent_texts[slot] == 'type_texts.DisposableInput'
		assert fork.recent_texts[slot] == 'type_texts.DisposableInput'
		assert source_type_name(id) == 'type_texts.DisposableInput'
		assert texts.intern('type_texts.DisposableInput') == id
		assert fork.intern('type_texts.DisposableInput') == id
	}
}
