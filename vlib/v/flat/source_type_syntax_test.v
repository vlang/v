module flat

fn assert_source_type_syntax(text string, kind SourceTypeSyntaxKind, base string, args []string) {
	id := source_type_id(text)
	syntax := source_type_syntax(id)
	assert syntax == source_type_syntax(id)
	assert_source_type_syntax_scalar_views(syntax, source_type_syntax_parse(id, text))
	assert syntax.kind() == kind
	assert syntax.base() == source_type_id(base)
	mut expected := []u32{}
	for arg in args { expected << source_type_id(arg) }
	assert syntax.args() == expected
}

fn test_source_type_syntax_numeric_constructors() {
	assert_source_type_syntax('', .empty, '', [])
	assert_source_type_syntax('pkg.Name', .name, '', [])
	assert_source_type_syntax('&  pkg.Name', .pointer, 'pkg.Name', [])
	assert_source_type_syntax('[]?pkg.Name', .array, '?pkg.Name', [])
	assert_source_type_syntax('[ 2 + n ]?pkg.Name', .fixed_array, '?pkg.Name', [' 2 + n '])
	assert_source_type_syntax('?[3]u8', .option, '[3]u8', [])
	assert_source_type_syntax('[3]?u8', .fixed_array, '?u8', ['3'])
	assert_source_type_syntax('![]int', .result, '[]int', [])
	assert_source_type_syntax('map[string]fn (&Node) ?[]T', .map_type, 'fn (&Node) ?[]T', ['string'])
	assert_source_type_syntax('fn (mut value &Dog, ...[]T) map[string]T', .function_type,
		'map[string]T', ['mut value &Dog', '...[]T'])
	assert_source_type_syntax('fn()', .function_type, 'void', [])
	assert !source_type_syntax(source_type_id('fn()')).function_has_result()
	assert source_type_syntax(source_type_id('fn() void')).function_has_result()
	assert_source_type_syntax('(int, map[string]fn (T) []U)', .tuple, '', ['int',
		'map[string]fn (T) []U'])
	assert_source_type_syntax('chan mut T', .channel, 'mut T', [])
	assert_source_type_syntax('thread []T', .thread, '[]T', [])
	assert_source_type_syntax('mut &T', .mutable_type, '&T', [])
	assert_source_type_syntax('shared []T', .shared_type, '[]T', [])
	assert_source_type_syntax('atomic int', .atomic_type, 'int', [])
	assert_source_type_syntax('...T', .variadic, 'T', [])
	assert_source_type_syntax('pkg.Pair[fn (int, T) []U, [n + 1]V]', .generic, 'pkg.Pair',
		['fn (int, T) []U', '[n + 1]V'])
	assert_source_type_syntax('Box[([)]', .invalid, '', [])
}

fn test_source_type_syntax_raw_and_trimmed_views_are_distinct() {
	syntax := source_type_syntax(source_type_id('  && pkg.Box[T]  '))
	assert syntax.trimmed() == source_type_id('&& pkg.Box[T]')
	assert syntax.pointer_base() == source_type_id('pkg.Box[T]')
	assert syntax.pointer_depth() == 2
	assert syntax.raw_base() == 0
	assert syntax.unqualified() == source_type_id('Box[T]  ')
	assert syntax.qualifier() == source_type_id('  && pkg')
	assert syntax.generic_base() == source_type_id('  && pkg.Box')
	assert syntax.words() == [source_type_id('pkg'), source_type_id('Box'), source_type_id('T')]
	assert syntax.symbols() == [source_type_id('pkg.Box'), source_type_id('T')]
	assert source_type_syntax(source_type_id('&  T')).raw_base() == source_type_id('  T')
	assert source_type_syntax(source_type_id('map[T]  U')).raw_base() == source_type_id('  U')
	assert source_type_syntax(source_type_id('[3]  U')).raw_base() == source_type_id('  U')
	assert source_type_syntax(source_type_id('main.A.B')).main_base() == source_type_id('A.B')
	assert source_type_syntax(source_type_id(' C ')).bare_generic()
}

fn test_source_type_syntax_retains_permissive_query_boundaries() {
	missing := source_type_syntax(source_type_id('Box[T'))
	assert missing.kind() == .invalid
	assert missing.application_valid()
	assert missing.application_base() == source_type_id('Box')
	assert missing.application_args() == [source_type_id('T')]
	assert !missing.generic_app_valid()
	map_type := source_type_syntax(source_type_id('mut &map[K]V'))
	assert map_type.application_args() == [source_type_id('K'), source_type_id('V')]
	assert map_type.generic_app_args() == [source_type_id('K')]
	assert !source_type_syntax(source_type_id('map[K]')).application_valid()
	assert !source_type_syntax(source_type_id('u8[0x10]')).application_valid()
	assert !source_type_syntax(source_type_id('u8[count]')).generic_app_valid()
	assert source_type_syntax(source_type_id('Box[C.int]')).generic_app_valid()
	assert source_type_syntax(source_type_id('Box[T]junk')).generic_app_valid()
	assert !source_type_syntax(source_type_id('fn (Box[T]) int')).generic_app_valid()
	postfix := source_type_syntax(source_type_id('  T[3][ 5 ] '))
	assert postfix.postfix_base() == source_type_id('T')
	assert postfix.postfix_dims() == [source_type_id('5'), source_type_id('3')]
	function := source_type_syntax(source_type_id('fn (map[T, fn (U)],, ) Box[U'))
	assert function.kind() == .invalid
	assert function.function_closed()
	assert function.function_params() == [source_type_id('map[T, fn (U)]')]
	assert function.function_result() == source_type_id('Box[U')
	assert source_type_syntax(source_type_id('(T,,U)')).tuple_components() == [
		source_type_id('T'),
		u32(0),
		source_type_id('U'),
	]
}

fn test_source_type_syntax_parameter_normalization() {
	cases := ['mut value &Dog', 'value []T', 'mut []T', 'shared Thing', 'arg fn (int) int', 'int']
	expected := ['&&Dog', '[]T', '&[]T', 'shared Thing', 'fn (int) int', 'int']
	mutable := ['mut &Dog', '[]T', 'mut []T', 'shared Thing', 'fn (int) int', 'int']
	for i, text in cases {
		syntax := source_type_syntax(source_type_id(text))
		assert syntax.parameter_type() == source_type_id(expected[i])
		assert syntax.parameter_mut_type() == source_type_id(mutable[i])
	}
}

fn test_source_type_syntax_exact_legacy_views() {
	function := source_type_syntax(source_type_id('fn (,T,)'))
	assert function.function_params() == [source_type_id('T')]
	assert function.checker_function_params() == [u32(0), source_type_id('T')]
	assert function.cgen_function_params() == [u32(0), source_type_id('T'), u32(0)]
	assert source_type_syntax(source_type_id('(T,)')).tuple_components() == [
		source_type_id('T'),
		u32(0),
	]
	assert source_type_syntax(source_type_id('(T,)')).checker_tuple_components() == [source_type_id('T')]
	assert source_type_syntax(source_type_id('Box[3]tail')).cgen_app_valid()
	assert !source_type_syntax(source_type_id('Box[3]tail')).generic_app_valid()
	assert source_type_syntax(source_type_id('Box[T]')).bracket_from() == source_type_id('[T]')
	assert source_type_syntax(source_type_id('[ n + 1 ] T')).bracket_raw_inner() == source_type_id(' n + 1 ')
	assert source_type_syntax(source_type_id('[ n + 1 ] T')).bracket_prefix() == source_type_id('[ n + 1 ]')
	assert source_type_syntax(source_type_id('[ n + 1 ] T')).bracket_suffix() == source_type_id(' T')
	assert source_type_syntax(source_type_id('[ n + 1 ] T')).bracket_inner_fixed_length()
	assert source_type_syntax(source_type_id('fn (X) T')).function_space_prefix()
	assert source_type_syntax(source_type_id('builtin.pkg.Type')).builtin_base() == source_type_id('pkg.Type')
	assert source_type_syntax(source_type_id('main.sub.Type')).first_dot_prefix() == source_type_id('main')
	assert source_type_syntax(source_type_id('main.sub.Type')).first_dot_suffix() == source_type_id('sub.Type')
	assert source_type_syntax(source_type_id('&& &T')).pointer_raw_base() == source_type_id(' &T')
	assert source_type_syntax(source_type_id('map[K]V')).named_type() == false
	assert source_type_syntax(source_type_id('pkg.Type')).named_type()
	assert source_type_syntax(source_type_id('value map[K]V')).top_space_head() == source_type_id('value')
	assert source_type_syntax(source_type_id('value map[K]V')).top_space_tail() == source_type_id('map[K]V')
	qualified := source_type_id('main.sub.Type')
	assert source_type_has_qualified_prefix(qualified, source_type_id('main'))
	assert source_type_has_qualified_prefix(qualified, source_type_id('main.sub'))
	assert !source_type_has_qualified_prefix(qualified, source_type_id('mai'))
	suffix := source_type_qualified_suffix(qualified, source_type_id('main.sub')) or { panic('missing prefix') }
	assert suffix == source_type_id('Type')
	empty_suffix := source_type_qualified_suffix(source_type_id('main.'), source_type_id('main')) or { panic('missing empty suffix') }
	assert empty_suffix == 0
}

fn test_source_type_syntax_numeric_reconstruction_preserves_spelling() {
	t := source_type_id('T')
	u := source_type_id('U')
	assert source_type_name(source_type_composition_one(.thread, t)) == 'thread T'
	assert source_type_name(source_type_composition_one(.shared_type, t)) == 'shared T'
	assert source_type_name(source_type_composition_one(.atomic_type, t)) == 'atomic T'
	assert source_type_name(source_type_composition_two(.qualified, t, u)) == 'T.U'
	assert source_type_name(source_type_composition_two(.named_parameter, t, u)) == 'T U'
	assert source_type_name(source_type_composition_two(.concat, t, u)) == 'TU'
	assert source_type_name(source_type_composition(.generic, [t, u])) == 'T[U]'
	assert source_type_name(source_type_composition(.function_type_spaced, [t, u])) == 'fn (T) U'
	assert source_type_name(source_type_composition(.void_function_type_spaced, [t])) == 'fn (T)'
}

fn test_source_type_syntax_numeric_slots_compare_ids() {
	first := source_type_id('syntax_slots.First[T]')
	second := source_type_id('syntax_slots.Second[U]')
	a := source_type_syntax(first)
	b := source_type_syntax(second)
	assert a != b
	assert &source_type_syntax_find(first).syntax == a
	assert &source_type_syntax_find(second).syntax == b
	assert a.base() == source_type_id('syntax_slots.First')
	assert b.base() == source_type_id('syntax_slots.Second')
	assert a.args() == [source_type_id('T')]
	assert b.args() == [source_type_id('U')]
}

fn test_source_type_syntax_zero_slot_is_stable() {
	empty := source_type_syntax(0)
	assert empty == source_type_syntax(source_type_id(''))
	assert empty.kind() == .empty
	assert empty.trimmed() == 0
	assert empty.args().len == 0
	assert &source_type_syntax_find(0).syntax == empty
}

fn test_source_type_syntax_page_boundary_preserves_entries() {
	mut before := source_type_id('syntax_page_boundary_0')
	for i in 1 .. source_type_chunk_size {
		if (before - 1) & (source_type_chunk_size - 1) == source_type_chunk_size - 1 {
			break
		}
		before = source_type_id('syntax_page_boundary_${i}')
	}
	after := source_type_id('syntax_page_boundary_after')
	assert (before - 1) & (source_type_chunk_size - 1) == source_type_chunk_size - 1
	assert after == before + 1
	left := source_type_syntax(before)
	right := source_type_syntax(after)
	assert left != right
	assert left.trimmed() == before
	assert right.trimmed() == after
	assert source_type_syntax(before) == left
	assert source_type_syntax(after) == right
}

fn test_source_type_syntax_does_not_retain_mutable_input_bytes() {
	mut bytes := 'MutableType[T]'.bytes()
	borrowed := unsafe { tos(bytes.data, bytes.len) }
	id := source_type_id(borrowed)
	syntax := source_type_syntax(id)
	bytes[0] = `X`
	changed := source_type_syntax(source_type_id(borrowed))
	assert changed != syntax
	assert syntax.base() == source_type_id('MutableType')
	assert changed.base() == source_type_id('XutableType')
	assert source_type_syntax(id) == syntax
}

fn source_type_syntax_worker(worker int) []&SourceTypeSyntax {
	mut results := []&SourceTypeSyntax{}
	for i in 0 .. 128 {
		results << source_type_syntax(source_type_id('syntax_parallel.Shared_${i}[map[string]T]'))
		unique := source_type_syntax(source_type_id('syntax_parallel.Worker_${worker}_${i}[U]'))
		assert unique.args() == [source_type_id('U')]
	}
	return results
}

fn test_source_type_syntax_parallel_publication_is_immutable() {
	mut threads := []thread []&SourceTypeSyntax{}
	for worker in 0 .. 4 { threads << spawn source_type_syntax_worker(worker) }
	mut expected := []&SourceTypeSyntax{}
	for worker, handle in threads {
		results := handle.wait()
		if worker == 0 {
			expected = results.clone()
		} else {
			assert results == expected
		}
		for syntax in results {
			assert syntax.kind() == .generic
			assert syntax.args() == [source_type_id('map[string]T')]
		}
	}
}

fn source_type_syntax_in_disposable_scope() u32 {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		id := source_type_id('syntax_arena.Pair[[]T, fn (U) V]'.clone())
		source_type_syntax(id)
		unsafe {
			prealloc_scope_leave(scope)
			prealloc_scope_free_after(scope)
			prealloc_thread_cleanup()
		}
		return id
	} $else {
		id := source_type_id('syntax_arena.Pair[[]T, fn (U) V]'.clone())
		source_type_syntax(id)
		return id
	}
}

fn test_source_type_syntax_components_outlive_the_creating_worker() {
	handle := spawn source_type_syntax_in_disposable_scope()
	syntax := source_type_syntax(handle.wait())
	assert syntax.args() == [source_type_id('[]T'), source_type_id('fn (U) V')]
	assert syntax.symbols() == [source_type_id('syntax_arena.Pair'), source_type_id('T'),
		source_type_id('fn'), source_type_id('U'), source_type_id('V')]
}

fn test_source_type_syntax_common_name_fast_path_retains_identity_views() {
	for text in ['int', 'Name', 'T', 'C', 'T2x', '3', 'under_score', 'fnName'] {
		id := source_type_id(text)
		syntax := source_type_syntax(id)
		assert syntax.kind() == .name
		assert syntax.trimmed() == id
		assert syntax.unqualified() == id
		assert syntax.generic_base() == id
		assert syntax.parameter_type() == id
		assert syntax.generic_parameter_payload() == id
		assert syntax.symbols() == [id]
		assert syntax.words() == [id]
		assert syntax.cgen_parts() == [id]
		assert syntax.plain_comma_parts() == [id]
		assert syntax.array_chars_base() == id
		assert syntax.source_wrapper_base() == id
		assert !syntax.fixed_array_type()
	}
	assert source_type_syntax(source_type_id('under_score')).underscore_prefixes() == [source_type_id('under')]
	assert source_type_syntax(source_type_id('fnName')).fn_prefix()
	assert source_type_syntax(source_type_id('T2x')).cgen_generic_placeholder()
	assert !source_type_syntax(source_type_id('Name')).cgen_generic_placeholder()
}

fn test_source_type_syntax_exact_cached_array_and_typeof_queries() {
	assert source_type_syntax(source_type_id('[2]int')).fixed_array_type()
	assert !source_type_syntax(source_type_id('[]int')).fixed_array_type()
	assert !source_type_syntax(source_type_id('[?]int')).fixed_array_type()
	assert source_type_syntax(source_type_id('int[3]')).fixed_array_type()
	assert !source_type_syntax(source_type_id('int[n]')).fixed_array_type()
	// The old fallback uses the first bracket interior if postfix parsing fails.
	assert source_type_syntax(source_type_id('int[3]junk]')).fixed_array_type()
	typ := source_type_syntax(source_type_id(' typeof(items[2]) '))
	assert typ.typeof_valid()
	assert typ.typeof_inner() == source_type_id('items[2]')
	assert source_type_syntax(typ.typeof_inner()).postfix_index_base() == source_type_id('items')
	array := source_type_syntax(source_type_id('[2]int[ 3 ]'))
	assert array.last_bracket_prefix() == source_type_id('[2]int')
	assert array.last_bracket_length() == source_type_id('3')
	assert array.trailing_bracket_closed()
	assert array.trailing_bracket_prefix() == source_type_id('[2]int')
	assert source_type_syntax(source_type_id('[2]')).trailing_bracket_closed()
	assert source_type_syntax(source_type_id('[2]')).trailing_bracket_prefix() == 0
	assert !source_type_syntax(source_type_id('int[2]junk')).trailing_bracket_closed()
	assert source_type_syntax(source_type_id('][]T')).array_chars_base() == source_type_id('T')
}

fn test_source_type_syntax_cached_qualification_stream_preserves_every_byte() {
	raw := '3.pkg.T &value_name[]map[string]foo.Bar @variant@ tail.'
	syntax := source_type_syntax(source_type_id(raw))
	assert syntax.symbol_parts().len == syntax.symbol_is_name().len
	mut rebuilt := u32(0)
	mut names := []u32{}
	for i, part in syntax.symbol_parts() {
		rebuilt = source_type_composition_two(.concat, rebuilt, part)
		if syntax.symbol_is_name()[i] { names << part }
	}
	assert rebuilt == source_type_id(raw)
	assert names == [source_type_id('pkg.T'), source_type_id('value_name'), source_type_id('map'),
		source_type_id('string'), source_type_id('foo.Bar'), source_type_id('variant'),
		source_type_id('tail.')]
	assert syntax.named_variant()
	assert source_type_syntax(source_type_id('3Name')).symbol_parts() == [
		source_type_id('3'),
		source_type_id('Name'),
	]
	assert source_type_syntax(source_type_id('3Name')).symbol_is_name() == [false, true]
}

fn test_source_type_syntax_cached_query_variants_retain_legacy_grammar() {
	assert source_type_syntax(source_type_id('main.Box[main.T, domain.T, main.main.U]')).main_unlocked() == source_type_id('Box[T, domain.T, main.U]')
	assert source_type_syntax(source_type_id('main.Box[main.T]')).main_removed() == source_type_id('Box[T]')
	encoded := source_type_syntax(source_type_id('_123int'))
	assert encoded.encoded_fixed_length() == source_type_id('123')
	assert encoded.encoded_fixed_base() == source_type_id('int')
	assert source_type_syntax(source_type_id('mut T')).wrapper_prefix() == source_type_id('mut ')
	assert source_type_syntax(source_type_id('mut T')).wrapper_base() == source_type_id('T')
	assert source_type_syntax(source_type_id('atomic T')).wrapper_prefix() == 0
	assert source_type_syntax(source_type_id('T[0x10]')).checker_fixed_array_may_be()
	assert !source_type_syntax(source_type_id('T[0x10]')).fixed_array_type()
	assert source_type_syntax(source_type_id('0x10')).fixed_length()
	assert source_type_syntax(source_type_id('map[Alias')).bracket_source_inner() == source_type_id('Alias')
	assert source_type_syntax(source_type_id('map[T]fn ()')).map_function_fragment()
	assert source_type_syntax(source_type_id('X[3]]')).suffix_bracket_length() == source_type_id('3]')
	assert source_type_syntax(source_type_id('X[3]]tail')).last_bracket_span_inner() == source_type_id('3]')
	assert source_type_syntax(source_type_id('&?pkg.Name')).qualified_modifiers() == source_type_id('&?')
	assert source_type_syntax(source_type_id('Name\t')).plain_alias_target()
	assert !source_type_syntax(source_type_id('Name ')).plain_alias_target()
	assert source_type_syntax(source_type_id('Array_fixed_u8_12')).c_fixed_typedef_element() == source_type_id('u8')
	assert source_type_syntax(source_type_id('Array_fixed_u8_12')).c_fixed_typedef_length() == source_type_id('12')
}

fn test_source_type_syntax_receiver_and_parameter_queries_preserve_raw_parts() {
	receiver := source_type_syntax(source_type_id('Base[T]junk[U]'))
	assert receiver.receiver_app_valid()
	assert receiver.receiver_app_base() == source_type_id('Base')
	assert receiver.receiver_app_args() == [source_type_id('T]junk[U')]
	assert receiver.receiver_raw_app_args() == [source_type_id('T]junk[U')]
	assert source_type_syntax(source_type_id('Box[T, ]')).checker_raw_bracket_components() == [
		source_type_id('T'),
		source_type_id(' '),
	]
	assert source_type_syntax(source_type_id('Box[T, ]')).checker_bracket_components() == [
		source_type_id('T'),
		u32(0),
	]
	parameter := source_type_syntax(source_type_id('mut value []T'))
	assert parameter.parameter_named()
	assert parameter.parameter_is_mut()
	assert parameter.parameter_name() == source_type_id('value')
	assert parameter.legacy_parameter_type() == source_type_id('[]T')
	assert parameter.declaration_parameter_payload() == source_type_id('[]T')
	assert source_type_syntax(source_type_id('fn (, T, )')).substitution_function_params() == [
		u32(0),
		source_type_id(' T'),
		source_type_id(' '),
	]
	assert source_type_syntax(source_type_id('fn () \tT')).function_space_result() == source_type_id('\tT')
	assert source_type_syntax(source_type_id('fn () \tT ')).checker_parse_function_result() == source_type_id('\tT ')
}

fn test_source_type_syntax_distinct_generic_lexers() {
	syntax := source_type_syntax(source_type_id('123T &Type C.T T'))
	assert syntax.legacy_generic_words() == [source_type_id('T'), source_type_id('Type'),
		source_type_id('C'), source_type_id('T'), source_type_id('T')]
	assert syntax.generic_mentions() == [source_type_id('T')]
	assert syntax.vls_generic_words() == [source_type_id('T'), source_type_id('C'),
		source_type_id('T')]
	assert source_type_legacy_generic_param_matches(source_type_id('&Type'), source_type_id('Ty'))
	assert source_type_legacy_generic_param_matches(source_type_id('&Type'), 0)
	assert source_type_legacy_generic_param_matches(source_type_id('Box[T, U]'), source_type_id('T, U'))
	assert source_type_legacy_generic_param_matches(source_type_id('Box[T, U]'), source_type_id('U'))
	assert source_type_syntax(source_type_id('mod.T')).generic_mentions().len == 0
}

fn test_source_type_syntax_written_applications_include_nested_offsets() {
	syntax := source_type_syntax(source_type_id('map[string][]Box[Box[User]]'))
	assert syntax.written_applications() == [source_type_id('Box[Box[User]]'),
		source_type_id('Box[User]')]
	assert syntax.written_application_offsets() == [u32(13), u32(17)]
	assert source_type_syntax(source_type_id('Box[T] Broken[U')).written_applications() == [source_type_id('Box[T]')]
	assert source_type_syntax(source_type_id('&mut Box[T]')).constraint_receiver_base() == source_type_id('Box[T]')
	assert source_type_syntax(source_type_id('Interface [ T ]')).space_removed() == source_type_id('Interface[T]')
}

fn test_source_type_syntax_cgen_and_reflection_queries_keep_malformed_behavior() {
	assert source_type_syntax(source_type_id('Box[fn (T) U, map[V]W]')).cgen_placeholder_candidates() == [source_type_id('V')]
	assert source_type_syntax(source_type_id('Box[T]tail[U')).cgen_placeholder_candidates() == [source_type_id('T')]
	assert source_type_syntax(source_type_id(']Box[T]')).cgen_placeholder_candidates().len == 0
	assert source_type_syntax(source_type_id('T,')).reflection_parts() == [source_type_id('T')]
	assert source_type_syntax(source_type_id('T, ')).reflection_parts() == [
		source_type_id('T'),
		u32(0),
	]
	assert source_type_syntax(source_type_id('(T,)')).reflection_tuple_parts() == [source_type_id('T')]
	assert source_type_syntax(source_type_id('__v_ result _T')).cgen_decl_unusable()
	assert source_type_syntax(source_type_id('[3]void')).cgen_decl_unusable()
	assert !source_type_syntax(source_type_id('Result')).cgen_decl_unusable()
	assert source_type_syntax(source_type_id('SomeResult')).result_suffix()
}

fn test_source_type_syntax_cached_encoding_and_array_views() {
	assert source_type_syntax(source_type_id('Name***')).c_pointer_depth() == 3
	assert source_type_syntax(source_type_id('Name***')).c_pointer_raw_base() == source_type_id('Name')
	assert source_type_syntax(source_type_id('voidptrvoidptr')).voidptr_parts() == [
		u32(0),
		u32(0),
		u32(0),
	]
	assert source_type_syntax(source_type_id('__vf_thread_kT')).thread_value_payload() == source_type_id('T')
	assert source_type_syntax(source_type_id('_A__B___C_')).flattened_receiver_parts() == [
		u32(0),
		source_type_id('A__B__'),
		source_type_id('C'),
		u32(0),
	]
	assert source_type_syntax(source_type_id('A__B')).doubleunderscore_dotted() == source_type_id('A.B')
	assert source_type_syntax(source_type_id('Foo???')).question_trimmed() == source_type_id('Foo')
	assert source_type_syntax(source_type_id('42')).decimal_value() == 42
	assert source_type_syntax(source_type_id('4x')).decimal_value() == 0
	assert source_type_syntax(source_type_id('mod.map[K]V.[]T')).receiver_collection_id() == source_type_id('[]T')
	assert source_type_syntax(source_type_id('[2]int[3]junk')).backward_bracket_prefix() == source_type_id('[2]int')
	assert source_type_syntax(source_type_id('[2]int[3]junk')).backward_bracket_inner() == source_type_id('3]jun')
}

fn test_source_type_syntax_c_pointer_count_includes_interior_stars() {
	spellings := ['', 'T', '*', 'T***', 'T*U*', 'fn(*T) *U', '  *T*  ', '*[2*3]T*']
	counts := [0, 0, 1, 3, 2, 2, 2, 3]
	for i, spelling in spellings {
		syntax := source_type_syntax(source_type_id(spelling))
		assert syntax.c_pointer_count() == counts[i]
	}
	assert source_type_syntax(source_type_id('T*U*')).c_pointer_depth() == 1
	assert source_type_syntax(source_type_id('fn(*T) *U')).c_pointer_depth() == 0
}

fn test_source_type_syntax_c_abi_prefixes_use_raw_spelling() {
	for spelling in ['__v_result', '__v_result_T', '__v_resultsuffix'] {
		assert source_type_syntax(source_type_id(spelling)).c_result_prefix()
	}
	for spelling in ['', '__v_resul', ' __v_result_T', 'mod.__v_result_T'] {
		assert !source_type_syntax(source_type_id(spelling)).c_result_prefix()
	}
	for spelling in ['__v_option_', '__v_option_T', '__v_option_[T'] {
		assert source_type_syntax(source_type_id(spelling)).c_option_prefix()
	}
	for spelling in ['__v_option', ' __v_option_T', 'mod.__v_option_T'] {
		assert !source_type_syntax(source_type_id(spelling)).c_option_prefix()
	}
	for spelling in ['multi_return_', 'multi_return_T', 'multi_return_[T'] {
		assert source_type_syntax(source_type_id(spelling)).c_multi_return_prefix()
	}
	for spelling in ['multi_return', ' multi_return_T', 'mod.multi_return_T'] {
		assert !source_type_syntax(source_type_id(spelling)).c_multi_return_prefix()
	}
	for spelling in ['map_', 'map_int', 'map_[T'] {
		assert source_type_syntax(source_type_id(spelling)).c_lower_map_prefix()
	}
	for spelling in ['', 'map', 'map__', 'map__int', ' map_int', 'Map_int', 'mod.map_int'] {
		assert !source_type_syntax(source_type_id(spelling)).c_lower_map_prefix()
	}
}

fn test_source_type_syntax_inferred_annotations_and_markers() {
	syntax := source_type_syntax(source_type_id('A typeof(__expr_17) B typeof(__expr_42) C'))
	assert syntax.inferred_parts() == [source_type_id('A '), source_type_id(' B '),
		source_type_id(' C')]
	assert syntax.inferred_annotations() == [source_type_id('typeof(__expr_17)'),
		source_type_id('typeof(__expr_42)')]
	assert syntax.inferred_expr_ids() == [u32(17), u32(42)]
	assert source_type_syntax(source_type_id('__expr_17')).expr_id() == 17
	assert source_type_syntax(source_type_id('Box[?!T]')).nested_option_result_marker() == 4
	assert source_type_syntax(source_type_id('Box[?T]')).nested_option_result_marker() == -1
	assert source_type_syntax(source_type_id('typeof(x)')).typeof_prefix()
	assert !source_type_syntax(source_type_id('Box[typeof(x)]')).typeof_prefix()
}

fn test_source_type_capture_caches_empty_and_failed_results_separately() {
	actual := source_type_id('Box[[]T]')
	prefix := source_type_id('Box[')
	suffix := source_type_id(']')
	for _ in 0 .. 2 {
		captured := source_type_capture(actual, prefix, suffix) or { panic('capture failed') }
		assert captured == source_type_id('[]T')
		empty := source_type_capture(source_type_id('Box[]'), prefix, suffix) or { panic('empty capture failed') }
		assert empty == 0
		if _ := source_type_capture(source_type_id('Box['), prefix, suffix) {
			assert false
		}
		if _ := source_type_capture(actual, source_type_id('Bag['), suffix) {
			assert false
		}
	}
}

fn test_source_type_encoded_prefix_cache_preserves_dot_codec() {
	for _ in 0 .. 2 {
		assert source_type_c_encoded_prefix_matches(source_type_id('pkg__Name_T'), source_type_id('pkg.Name'))
		assert source_type_c_encoded_prefix_matches(source_type_id('pkg.Name_T'), source_type_id('pkg.Name'))
		assert !source_type_c_encoded_prefix_matches(source_type_id('pkg_Name_T'), source_type_id('pkg.Name'))
		assert !source_type_c_encoded_prefix_matches(source_type_id('pkg__Name_T'), source_type_id('pkg.Other'))
		assert source_type_c_encoded_prefix_matches(source_type_id('pkg__Name_T'), 0)
		assert !source_type_c_encoded_prefix_matches(0, source_type_id('pkg.Name'))
	}
	assert source_type_syntax(source_type_id('pkg.Name2')).encoded_alnum_dotted()
	assert !source_type_syntax(source_type_id('pkg_Name')).encoded_alnum_dotted()
	assert source_type_syntax(source_type_id('ptrpkg.Name')).ptr_name_base() == source_type_id('pkg.Name')
}

fn test_source_type_syntax_exact_eval_and_driver_tuple_views() {
	syntax := source_type_syntax(source_type_id('(]T, U, )'))
	assert syntax.eval_tuple_parts() == [source_type_id(']T'), source_type_id('U')]
	assert syntax.implicit_tuple_components().len == 0
	assert source_type_syntax(source_type_id('(T,,U)')).eval_tuple_parts() == [
		source_type_id('T'),
		u32(0),
		source_type_id('U'),
	]
	assert source_type_syntax(source_type_id('(T,,U)')).implicit_tuple_components().len == 0
	assert source_type_syntax(source_type_id('(T, []U)')).implicit_tuple_components() == [
		source_type_id('T'),
		source_type_id('[]U'),
	]
	assert source_type_syntax(source_type_id('(T)')).implicit_tuple_components() == [source_type_id('T')]
	assert source_type_syntax(source_type_id('T, U')).eval_tuple_parts().len == 0
}

fn test_source_type_syntax_markused_and_incremental_name_views() {
	syntax := source_type_syntax(source_type_id('9pkg.Name[T] @thing@Item[U broken[V'))
	assert syntax.generic_application_heads() == [source_type_id('9pkg.Name'), source_type_id('Item'),
		source_type_id('broken')]
	assert syntax.markused_application_heads() == [source_type_id('9pkg.Name'),
		source_type_id('@thing@Item'), source_type_id('broken')]
	assert syntax.contains_capital()
	assert syntax.markused_generic_components() == [source_type_id('pkg.Name'), source_type_id('T'),
		source_type_id('thing@Item'), source_type_id('U'), source_type_id('V')]
	assert source_type_syntax(source_type_id('123T')).vls_generic_words() == [source_type_id('T')]
	assert source_type_syntax(source_type_id('123Type')).vls_generic_words().len == 0
	assert source_type_syntax(source_type_id('main.@Name')).main_at_base() == source_type_id('Name')
	assert source_type_syntax(source_type_id('main.@')).main_at_base() == 0
	assert source_type_syntax(source_type_id('main.Name')).main_at_base() == source_type_id('main.Name')
}

fn test_source_type_syntax_incremental_generic_application() {
	for text in ['Box[int]', '[]Box[int]', 'Box[', '3[', '_map[', 'map9[', 'pkg.Box[', 'map[Box[int]]',
		'map[map[T]Box['] {
		id := source_type_id(text)
		assert source_type_syntax(id).incremental_generic_application(), text
		assert source_type_syntax(id).incremental_generic_application(), text
	}
	for text in ['', 'Box', '123T', '[]int', '[3]int', 'map[string]int', 'pkg.map[int]', 'Box [T]',
		'.[]', 'map[', 'map[map[T]]', 'Box\t[T]', 'é[T]'] {
		id := source_type_id(text)
		assert !source_type_syntax(id).incremental_generic_application(), text
		assert !source_type_syntax(id).incremental_generic_application(), text
	}
}

fn test_source_type_shape_shared_parameter_candidate_exact_first_byte() {
	for text in ['s', 'string', 'shared int', ' int', '\tint', '\nint', '\rint'] {
		assert source_type_shape(source_type_id(text)).has(.shared_parameter_candidate), text
	}
	for text in ['', 'int', 'Shared int', '&shared int', '\vshared int', '\fshared int'] {
		assert !source_type_shape(source_type_id(text)).has(.shared_parameter_candidate), text
	}
}

fn test_source_type_syntax_fixed_array_codec_metadata() {
	encoded := source_type_syntax(source_type_id('FixedArray_EXPRHEX307846_FASTC_ARRAY_OF_u8'))
	assert encoded.fastc_fixed_array_length_valid()
	assert encoded.fastc_fixed_array_length() == source_type_id('0xF')
	assert source_type_syntax(source_type_id('FixedArray_EXPRHEXf_FASTC_ARRAY_OF_u8')).fastc_fixed_array_length() == source_type_id([u8(15)].bytestr())
	assert !source_type_syntax(source_type_id('FixedArray_EXPRHEXg_FASTC_ARRAY_OF_u8')).fastc_fixed_array_length_valid()
	empty := source_type_syntax(source_type_id('FixedArray_EXPRHEX_FASTC_ARRAY_OF_u8'))
	assert empty.fastc_fixed_array_length_valid()
	assert empty.fastc_fixed_array_length() == 0
	assert source_type_syntax(source_type_id('T[U]junk[V]')).receiver_app_inner() == source_type_id('U]junk[V')
	assert source_type_syntax(source_type_id('T[U[V]')).first_bracket_source_inner() == source_type_id('U[V')
	assert source_type_syntax(source_type_id('SystemError')).system_error_suffix()
}

fn test_source_type_syntax_legacy_mut_character_set_trim() {
	assert source_type_syntax(source_type_id('mut Thing')).mut_chars_base() == source_type_id('Thing')
	assert source_type_syntax(source_type_id('tuple')).mut_chars_base() == source_type_id('ple')
	assert source_type_syntax(source_type_id('u8')).mut_chars_base() == source_type_id('8')
	assert source_type_syntax(source_type_id('mut')).mut_chars_base() == 0
	assert source_type_syntax(source_type_id('Thing')).mut_chars_base() == source_type_id('Thing')
	for raw in ['', ' ', 'mut', 'tumu  ', 'main.Foo', 'time.Time', 'tuple', 'u8', 'mut Thing',
		' mut &Thing', 'mut\tThing', '\tmut Thing', '\nmut Thing', 'émut', 'mut[broken', 't!',
		'u?T', 'm_t', 'mutmutThing', 'Thingmut'] {
		syntax := source_type_syntax(source_type_id(raw))
		expected := source_type_id(raw.trim_left('mut '))
		assert syntax.mut_chars_base() == expected
		assert syntax.mut_chars_base() == expected
	}
	zero := SourceTypeSyntax{}
	assert zero.mut_chars_base() == 0
}

fn test_source_type_cached_pair_predicates_preserve_exact_queries() {
	for _ in 0 .. 2 {
		assert source_type_encoded_receiver_matches(source_type_id('pkg__Name_Sub_T'), source_type_id('Name_Sub'))
		assert source_type_encoded_receiver_matches(source_type_id('___'), 0)
		assert !source_type_encoded_receiver_matches(source_type_id('pkg__Name'), source_type_id('Name'))
		assert !source_type_legacy_generic_param_matches(source_type_id('Box[T,U]'), source_type_id('U'))
		assert source_type_legacy_generic_param_matches(source_type_id(' Box[T, U] '), source_type_id('U'))
		assert source_type_contains_word(source_type_id('fn(C.Node, Node)'), source_type_id('C.Node'))
		assert source_type_contains_word(source_type_id('C.Node'), source_type_id('Node'))
		assert !source_type_contains_word(source_type_id('OtherNode'), source_type_id('Node'))
		assert !source_type_contains_word(source_type_id('Node'), 0)
	}
}

fn test_source_type_parser_legacy_decompositions() {
	syntax := source_type_syntax(source_type_id(']Box[pkg.T].Leaf'))
	assert syntax.top_level_dot_valid()
	assert syntax.top_level_dot_prefix() == source_type_id(']Box[pkg.T]')
	assert syntax.top_level_dot_suffix() == source_type_id('Leaf')
	assert !source_type_syntax(source_type_id('Box[pkg.T]')).top_level_dot_valid()
	fn_decl := source_type_syntax(source_type_id('fn(fn(T)) ?&T'))
	assert fn_decl.parser_fn_decl_valid()
	assert fn_decl.parser_fn_decl_params() == source_type_id('fn(T')
	assert fn_decl.parser_fn_decl_result() == source_type_id(') ?&T')
	assert !source_type_syntax(source_type_id('fn() T')).parser_fn_decl_valid()
	assert source_type_syntax(source_type_id('fn(T) ?&T')).parser_fn_decl_result() == source_type_id('T')
}

fn test_source_type_syntax_offsets_and_remaining_query_views() {
	syntax := source_type_syntax(source_type_id('OtherThing Thing Thing pkg.T T'))
	assert syntax.symbols() == [source_type_id('OtherThing'), source_type_id('Thing'),
		source_type_id('Thing'), source_type_id('pkg.T'), source_type_id('T')]
	assert syntax.symbols_first_offsets() == [u32(0), 5, 5, 23, 5]
	assert source_type_composition_one(.fastc_composite_part, source_type_id('fn (T)')) == source_type_id('fn_(T)')
	assert source_type_composition_one(.fastc_composite_part, source_type_id('&pkg.T*')) == source_type_id('&pkg__T_ptr')
	assert source_type_syntax(source_type_id('T?')).contains_question()
	assert source_type_syntax(source_type_id('[]chan T')).channel_fragment()
	assert source_type_syntax(source_type_id('void (*)()')).c_function_pointer_hole()
	assert source_type_syntax(source_type_id('A | fn (B | C) | {D | E} |')).union_parts() == [
		source_type_id('A'),
		source_type_id('fn (B | C)'),
		source_type_id('{D | E}'),
	]
	variant := source_type_syntax(source_type_id('pkg.Box[inner.T].Leaf'))
	assert variant.named_variant_dot_prefix() == source_type_id('pkg.Box[inner.T]')
	assert variant.named_variant_dot_suffix() == source_type_id('Leaf')
	assert !source_type_syntax(source_type_id(']pkg.Leaf')).named_variant_dot_valid()
	assert source_type_syntax(source_type_id('Thing_T__method')).mangled_open_generic_marker()
	assert !source_type_syntax(source_type_id('Thing_T_method')).mangled_open_generic_marker()
}

fn test_source_type_syntax_integer_conversion_preserves_raw_int() {
	for raw in ['', '0', '42', '4x', '1_000', '0xff', '0b101', '0o17', '-42', '+17', ' 42 ', '\t-17\n',
		'-0xff', 'not-a-number', '2147483648', '9223372036854775807', '0x', '--1', '3.14', '12tail'] {
		id := source_type_id(raw)
		syntax := source_type_syntax(id)
		assert syntax.integer_value() == raw.int()
		assert source_type_syntax(id) == syntax
		assert source_type_syntax(id).integer_value() == raw.int()
	}
}

fn test_source_type_syntax_fastc_thread_value_codec() {
	inputs := ['__vf_thread_void', '__vf_thread_k', '__vf_thread_kint', '__vf_thread_kpkg_2eType',
		'__vf_thread_k_26_5b_5du8', '__vf_thread_ka_5fb', '__vf_thread_k_c3_a9', '__vf_thread_k_00_ff',
		'__vf_thread_ké', '__vf_thread_kZ']
	expected := ['', '', 'int', 'pkg.Type', '&[]u8', 'a_b', 'é', [u8(0), 255].bytestr(), 'é',
		'Z']
	for i, raw in inputs {
		for _ in 0 .. 2 {
			syntax := source_type_syntax(source_type_id(raw))
			assert syntax.thread_value_type_valid()
			assert syntax.thread_value_type() == source_type_id(expected[i])
		}
	}
	for raw in ['', 'void', '__vf_thread_other', '__vf_thread_k_', '__vf_thread_k_0', '__vf_thread_k_0F',
		'__vf_thread_k_F0', '__vf_thread_k_gg'] {
		assert !source_type_syntax(source_type_id(raw)).thread_value_type_valid()
	}
}

fn test_source_type_syntax_named_variant_decoder_metadata() {
	for raw in ['', 'Thing', '&Thing', '@variant@Some', '&&@variant@Some', 'Sum@variant@',
		'&Sum@variant@Some[int]', '&&pkg.Sum@variant@Some_2[U]tail', ' Sum@variant@Some ',
		'Sum@variant@[int]', 'Sum@variant@?', 'Sum@variant@One@variant@Two', '& &Sum@variant@Some',
		'Sum@variant@é'] {
		id := source_type_id(raw)
		for _ in 0 .. 2 {
			syntax := source_type_syntax(id)
			assert syntax.named_variant() == raw.contains(named_variant_marker)
			if owner, name := decode_named_variant_type_name(raw) {
				assert syntax.named_variant_valid()
				assert syntax.named_variant_owner() == source_type_id(owner)
				assert syntax.named_variant_name() == source_type_id(name)
			} else {
				assert !syntax.named_variant_valid()
				assert syntax.named_variant_owner() == 0
				assert syntax.named_variant_name() == 0
			}
		}
	}
	variant := source_type_syntax(source_type_id('&&pkg.Sum@variant@Some_2[U]tail'))
	assert variant.named_variant_owner() == source_type_id('pkg.Sum[U]tail')
	assert variant.named_variant_name() == source_type_id('Some_2')
	empty_name := source_type_syntax(source_type_id('Sum@variant@[int]'))
	assert empty_name.named_variant_valid()
	assert empty_name.named_variant_owner() == source_type_id('Sum[int]')
	assert empty_name.named_variant_name() == 0
}

fn test_source_type_syntax_qualified_name_fast_path_preserves_views() {
	for raw in ['pkg.Foo', 'main.sub.Type', 'builtin.pkg.Type', 'C.int', 'T.Foo', 'pkg.T',
		'pkg.JS.sub.C.Type', 'pkg.C.sub.JS.Type', 'domain.Foo', 'pkg.main.Type', 'main.main.Type',
		'pkg.Type.typ', 'fnFancy.Type', '_pkg.Box_2', 'Array_fixed_pkg.Type_3', '__vf_thread_kpkg.Type',
		'ptrpkg.Type', 'pkg.under_score'] {
		id := source_type_id(raw)
		syntax := source_type_syntax(id)
		mut parts := []u32{}
		for part in raw.split('.') { parts << source_type_id(part) }
		mut prefixes := []u32{}
		mut suffixes := []u32{}
		for i, c in raw {
			if c == `.` {
				prefixes << source_type_id(raw[..i])
				suffixes << source_type_id(raw[i + 1..])
			}
		}
		assert syntax.unqualified() == source_type_id(raw.all_after_last('.'))
		assert syntax.qualifier() == source_type_id(raw.all_before_last('.'))
		assert syntax.words() == source_type_syntax_tokens(raw, false)
		assert syntax.legacy_generic_words() == source_type_syntax_legacy_words(raw)
		assert syntax.dotted_parts() == parts
		assert syntax.first_dot_prefix() == source_type_id(raw.all_before('.'))
		assert syntax.first_dot_suffix() == source_type_id(raw.all_after('.'))
		assert syntax.qualifier_prefixes() == prefixes
		assert syntax.qualifier_suffixes() == suffixes
		assert syntax.main_base() == if raw.starts_with('main.') {
			source_type_id(raw[5..])
		} else {
			u32(0)
		}
		assert syntax.builtin_base() == if raw.starts_with('builtin.') {
			source_type_id(raw[8..])
		} else {
			u32(0)
		}
		assert syntax.c_qualified() == raw.starts_with('C.')
		assert syntax.builtin_prefix() == raw.starts_with('builtin.')
		assert syntax.main_fragment() == raw.contains('main.')
		assert syntax.main_unlocked() == source_type_syntax_main_unlocked(id, raw)
		assert syntax.main_removed() == source_type_id(raw.replace('main.', ''))
		assert syntax.canonical_dotted_name()
		assert syntax.qualified_name_chars()
		assert !syntax.plain_type_name()
		assert !syntax.lowercase_value_ident()
		assert !syntax.cgen_generic_placeholder()
		parallel_fn, parallel_short := source_type_syntax_parallel_preseed(raw)
		assert syntax.parallel_preseed_fn() == parallel_fn
		assert syntax.parallel_preseed_short() == parallel_short
		assert syntax.field_owner_module() == source_type_id(if raw.contains('.C.') {
			raw.all_before('.C.')
		} else if raw.contains('.JS.') {
			raw.all_before('.JS.')
		} else {
			raw.all_before_last('.')
		})
		assert syntax.vls_generic_words() == source_type_syntax_vls_generic_words(raw)
		assert source_type_composition_one(.fastc_composite_part, id) == source_type_id(raw.replace('.', '__'))
		assert syntax.typ_suffix() == raw.ends_with('.typ')
		assert syntax.typ_suffix_base() == if raw.ends_with('.typ') {
			syntax.qualifier()
		} else {
			u32(0)
		}
		assert syntax.named_variant_dot_valid()
		assert syntax.named_variant_dot_prefix() == syntax.qualifier()
		assert syntax.named_variant_dot_suffix() == syntax.unqualified()
		assert syntax.top_level_dot_valid()
		assert syntax.top_level_dot_prefix() == syntax.first_dot_prefix()
		assert syntax.top_level_dot_suffix() == syntax.first_dot_suffix()
		assert syntax.symbols() == [id]
		assert syntax.symbol_parts() == [id]
		assert syntax.symbol_is_name() == [true]
		assert syntax.symbols_first_offsets() == [u32(0)]
		assert syntax.parameter_type() == id
		assert syntax.legacy_parameter_type() == id
		assert syntax.declaration_parameter_payload() == id
		assert syntax.kind() == .name
		assert syntax.base() == 0
		assert syntax.args().len == 0
		assert source_type_syntax(id) == syntax
	}
}

fn test_source_type_syntax_name_fast_path_keeps_malformed_and_numeric_names_distinct() {
	assert source_type_syntax(source_type_id('123T')).vls_generic_words() == [source_type_id('T')]
	assert source_type_syntax(source_type_id('123T')).legacy_generic_words() == [source_type_id('T')]
	assert source_type_syntax(source_type_id('123T')).words() == [source_type_id('123T')]
	for raw in ['.Foo', 'Foo.', 'Foo..Bar', 'Foo.2Bar', '2Foo.Bar', ' pkg.Foo ', 'pkg.[T]', 'pkg.Foo[T]'] {
		syntax := source_type_syntax(source_type_id(raw))
		assert syntax.unqualified() == source_type_id(raw.all_after_last('.'))
		assert syntax.qualifier() == source_type_id(raw.all_before_last('.'))
		assert syntax.words() == source_type_syntax_tokens(raw, false)
		assert syntax.legacy_generic_words() == source_type_syntax_legacy_words(raw)
		assert syntax.vls_generic_words() == source_type_syntax_vls_generic_words(raw)
	}
	assert source_type_syntax(source_type_id('.Foo')).dots_base() == source_type_id('Foo')
	assert !source_type_syntax(source_type_id('2Foo.Bar')).canonical_dotted_name()
	assert !source_type_syntax(source_type_id(' pkg.Foo ')).named_type()
	assert source_type_syntax(source_type_id('pkg.Foo[T]')).generic_app_valid()
}

fn test_source_type_syntax_reused_splits_keep_padding_and_empty_parameters() {
	for raw in ['Box[T]', ' Box[T] ', '  Box[T]', 'fn (T)', 'Box[T]suffix'] {
		syntax := source_type_syntax(source_type_id(raw))
		base, args, valid := source_type_syntax_cgen_application_raw(source_type_query_trim(raw))
		raw_base, raw_args, raw_valid := source_type_syntax_cgen_application_raw(raw)
		assert syntax.cgen_app_base() == base
		assert syntax.cgen_app_args() == args
		assert syntax.cgen_app_valid() == valid
		assert syntax.cgen_raw_app_base() == raw_base
		assert syntax.cgen_raw_app_args() == raw_args
		assert syntax.cgen_raw_app_valid() == raw_valid
	}
	assert source_type_syntax(source_type_id('  Box[T]')).cgen_raw_app_base() == source_type_id('  Box')
	assert source_type_syntax(source_type_id('  Box[T]')).cgen_app_base() == source_type_id('Box')
	function := source_type_syntax(source_type_id('fn (,T,) U'))
	assert function.function_params() == [source_type_id('T')]
	assert function.checker_function_params() == [u32(0), source_type_id('T')]
	assert function.cgen_function_params() == [u32(0), source_type_id('T'), u32(0)]
	assert source_type_syntax(source_type_id('fn( )')).checker_function_params() == [u32(0)]
	assert !source_type_syntax(source_type_id('fn( )')).checker_parse_function_has_params()
}

fn test_source_type_syntax_array_views_have_independent_headers() {
	syntax := source_type_syntax(source_type_id('ViewPair[T, []U]'))
	expected := [source_type_id('T'), source_type_id('[]U')]
	mut view := syntax.args()
	assert view == expected
	view.trim(0)
	assert view.len == 0
	assert syntax.args() == expected
	mut copy := syntax.args().clone()
	copy[0] = source_type_id('Changed')
	assert syntax.args() == expected
	assert source_type_syntax(source_type_id('')).args().len == 0
	parts := source_type_syntax(source_type_id('&Name')).symbol_parts()
	kinds := source_type_syntax(source_type_id('&Name')).symbol_is_name()
	assert parts == [source_type_id('&'), source_type_id('Name')]
	assert kinds == [false, true]
}

fn source_type_syntax_expected_underscore_parts(text string, suffix bool) []u32 {
	mut parts := []u32{}
	for i, c in text {
		if c == `_` {
			parts << source_type_id(if suffix { text[i + 1..] } else { text[..i] })
		}
	}
	return parts
}

fn test_source_type_syntax_lazy_underscore_parts_preserve_raw_bytes() {
	for text in ['', '_', '__', '_lead', 'tail_', 'a__b_', ' padded_name_ ', 'pkg.under_score.Name',
		'&?map[_K]fn (a_b) x_y[bad_'] {
		id := source_type_id(text)
		syntax := source_type_syntax(id)
		prefixes := syntax.underscore_prefixes()
		suffixes := syntax.underscore_suffixes()
		assert prefixes == source_type_syntax_expected_underscore_parts(text, false)
		assert suffixes == source_type_syntax_expected_underscore_parts(text, true)
		assert prefixes.data == syntax.underscore_prefixes().data
		assert suffixes.data == syntax.underscore_suffixes().data
		assert syntax.last_underscore_prefix() == source_type_id(text.all_before_last('_'))
		assert syntax.last_underscore_suffix() == source_type_id(text.all_after_last('_'))
	}
	// A manually initialized zero-value record retains its empty view behavior.
	zero := SourceTypeSyntax{}
	assert zero.underscore_prefixes().len == 0
	assert zero.underscore_suffixes().len == 0
}

fn test_source_type_syntax_underscore_directions_are_independently_lazy() {
	id := source_type_id('syntax_lazy_underscore_direction__fresh_')
	syntax := source_type_syntax(id)
	prefix_slot := source_type_syntax_underscore_slot(id, false)
	suffix_slot := source_type_syntax_underscore_slot(id, true)
	assert isnil(source_type_syntax_underscore_find(prefix_slot, id, false))
	assert isnil(source_type_syntax_underscore_find(suffix_slot, id, true))
	prefixes := syntax.underscore_prefixes()
	assert !isnil(source_type_syntax_underscore_find(prefix_slot, id, false))
	assert isnil(source_type_syntax_underscore_find(suffix_slot, id, true))
	suffixes := syntax.underscore_suffixes()
	assert !isnil(source_type_syntax_underscore_find(suffix_slot, id, true))
	assert prefixes == source_type_syntax_expected_underscore_parts(source_type_name(id), false)
	assert suffixes == source_type_syntax_expected_underscore_parts(source_type_name(id), true)
	// A missing underscore also has a published negative cache entry.
	empty_id := source_type_id('SyntaxLazyUnderscoreAbsentFresh')
	empty := source_type_syntax(empty_id)
	assert empty.underscore_prefixes().len == 0
	assert !isnil(source_type_syntax_underscore_find(source_type_syntax_underscore_slot(empty_id, false), empty_id, false))
	assert isnil(source_type_syntax_underscore_find(source_type_syntax_underscore_slot(empty_id, true), empty_id, true))
}

fn source_type_syntax_underscore_worker(id u32, suffix bool) []u32 {
	mut result := []u32{}
	for _ in 0 .. 64 {
		syntax := source_type_syntax(id)
		result = if suffix { syntax.underscore_suffixes() } else { syntax.underscore_prefixes() }
	}
	return result
}

fn test_source_type_syntax_underscore_parallel_publication_is_stable() {
	id := source_type_id('syntax_lazy_underscore_parallel__fresh_')
	mut threads := []thread []u32{}
	for worker in 0 .. 8 {
		threads << spawn source_type_syntax_underscore_worker(id, worker % 2 != 0)
	}
	syntax := source_type_syntax(id)
	for worker, handle in threads {
		parts := handle.wait()
		expected := if worker % 2 != 0 {
			syntax.underscore_suffixes()
		} else {
			syntax.underscore_prefixes()
		}
		assert parts == expected
		assert parts.data == expected.data
	}
}

fn source_type_syntax_underscore_in_disposable_scope() u32 {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		id := source_type_id('syntax_lazy_underscore_arena__fresh_'.clone())
		source_type_syntax(id).underscore_prefixes()
		source_type_syntax(id).underscore_suffixes()
		unsafe {
			prealloc_scope_leave(scope)
			prealloc_scope_free_after(scope)
			prealloc_thread_cleanup()
		}
		return id
	} $else {
		id := source_type_id('syntax_lazy_underscore_arena__fresh_'.clone())
		source_type_syntax(id).underscore_prefixes()
		source_type_syntax(id).underscore_suffixes()
		return id
	}
}

fn test_source_type_syntax_underscore_parts_outlive_the_creating_worker() {
	handle := spawn source_type_syntax_underscore_in_disposable_scope()
	id := handle.wait()
	syntax := source_type_syntax(id)
	assert syntax.underscore_prefixes() == source_type_syntax_expected_underscore_parts(source_type_name(id), false)
	assert syntax.underscore_suffixes() == source_type_syntax_expected_underscore_parts(source_type_name(id), true)
}

fn source_type_syntax_expected_flattened_receiver_parts(text string) []u32 {
	mut parts := []u32{}
	mut start := 0
	mut i := 0
	for i < text.len {
		if text[i] == `_` {
			if i + 1 < text.len && text[i + 1] == `_` {
				i += 2
				continue
			}
			parts << source_type_id(text[start..i])
			start = i + 1
		}
		i++
	}
	parts << source_type_id(text[start..])
	return parts
}

fn test_source_type_syntax_lazy_flattened_receiver_parts_preserve_pair_skipping() {
	for text in ['', 'Name', '_', '__', '___', '____', '_____', '_A__B___C_', '__A_B__',
		' leading_name__tail_ ', 'pkg.Foo__Bar_Baz', '&?map[_K]fn (a__b) c_d[bad_'] {
		syntax := source_type_syntax(source_type_id(text))
		parts := syntax.flattened_receiver_parts()
		assert parts == source_type_syntax_expected_flattened_receiver_parts(text)
		assert parts.data == syntax.flattened_receiver_parts().data
		mut changed_header := parts
		changed_header.trim(0)
		assert syntax.flattened_receiver_parts() == source_type_syntax_expected_flattened_receiver_parts(text)
	}
	zero := SourceTypeSyntax{}
	assert zero.flattened_receiver_parts().len == 0
	assert source_type_syntax(0).flattened_receiver_parts() == [u32(0)]
}

fn test_source_type_syntax_flattened_receiver_parts_are_independently_lazy() {
	id := source_type_id('syntax_lazy_flattened__receiver_fresh_')
	syntax := source_type_syntax(id)
	slot := int(id & 4095)
	assert isnil(source_type_syntax_flattened_receiver_find(slot, id))
	parts := syntax.flattened_receiver_parts()
	assert !isnil(source_type_syntax_flattened_receiver_find(slot, id))
	assert parts == source_type_syntax_expected_flattened_receiver_parts(source_type_name(id))
	assert isnil(source_type_syntax_underscore_find(source_type_syntax_underscore_slot(id, false), id, false))
	assert isnil(source_type_syntax_underscore_find(source_type_syntax_underscore_slot(id, true), id, true))
	plain_id := source_type_id('SyntaxLazyFlattenedReceiverNoSeparatorFresh')
	plain := source_type_syntax(plain_id)
	assert plain.flattened_receiver_parts() == [plain_id]
	assert isnil(source_type_syntax_flattened_receiver_find(int(plain_id & 4095), plain_id))
}

fn source_type_syntax_flattened_receiver_worker(id u32) []u32 {
	mut result := []u32{}
	for _ in 0 .. 64 { result = source_type_syntax(id).flattened_receiver_parts() }
	return result
}

fn test_source_type_syntax_flattened_receiver_parallel_publication_is_stable() {
	id := source_type_id('syntax_lazy_flattened__parallel_fresh_')
	mut threads := []thread []u32{}
	for _ in 0 .. 8 { threads << spawn source_type_syntax_flattened_receiver_worker(id) }
	for handle in threads {
		parts := handle.wait()
		expected := source_type_syntax(id).flattened_receiver_parts()
		assert parts == expected
		assert parts.data == expected.data
	}
}

fn source_type_syntax_flattened_receiver_in_disposable_scope() u32 {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		id := source_type_id('syntax_lazy_flattened__arena_fresh_'.clone())
		source_type_syntax(id).flattened_receiver_parts()
		unsafe {
			prealloc_scope_leave(scope)
			prealloc_scope_free_after(scope)
			prealloc_thread_cleanup()
		}
		return id
	} $else {
		id := source_type_id('syntax_lazy_flattened__arena_fresh_'.clone())
		source_type_syntax(id).flattened_receiver_parts()
		return id
	}
}

fn test_source_type_syntax_flattened_receiver_parts_outlive_the_creating_worker() {
	handle := spawn source_type_syntax_flattened_receiver_in_disposable_scope()
	id := handle.wait()
	assert source_type_syntax(id).flattened_receiver_parts() == source_type_syntax_expected_flattened_receiver_parts(source_type_name(id))
}

fn test_source_type_syntax_name_builder_singletons_have_process_owned_backing() {
	for text in ['Name', 'T', '_name', 'pkg.Name', 'main.pkg_Name'] {
		id := source_type_id(text)
		syntax := source_type_syntax(id)
		assert syntax.symbols() == [id]
		assert syntax.symbol_is_name() == [true]
		assert syntax.symbols_first_offsets() == [u32(0)]
		assert syntax.symbols().data == syntax.payload
		assert syntax.symbols_first_offsets().data == voidptr(&g_source_type_syntax_zero_offset)
		assert syntax.symbol_is_name().data == syntax.symbol_is_name().data
	}
	for text in ['123', '123T', '123_T'] {
		id := source_type_id(text)
		expected_parts, expected_names := source_type_syntax_symbol_parts(text)
		assert source_type_syntax(id).symbol_parts() == expected_parts
		assert source_type_syntax(id).symbol_is_name() == expected_names
	}
}

fn source_type_syntax_name_singletons_in_disposable_scope() u32 {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		id := source_type_id('SyntaxNameSingletonArenaFresh'.clone())
		syntax := source_type_syntax(id)
		assert syntax.symbols()[0] == id
		syntax.symbol_is_name()
		unsafe {
			prealloc_scope_leave(scope)
			prealloc_scope_free_after(scope)
			prealloc_thread_cleanup()
		}
		return id
	} $else {
		id := source_type_id('SyntaxNameSingletonArenaFresh'.clone())
		source_type_syntax(id)
		return id
	}
}

fn test_source_type_syntax_name_singleton_payloads_outlive_creating_worker() {
	handle := spawn source_type_syntax_name_singletons_in_disposable_scope()
	id := handle.wait()
	syntax := source_type_syntax(id)
	assert syntax.symbols() == [id]
	assert syntax.symbol_is_name() == [true]
	assert syntax.symbols_first_offsets() == [u32(0)]
}

fn source_type_syntax_expected_open_receiver_marker(raw string) bool {
	markers := ['T', 'U', 'K', 'V', 'A', 'B', 'C', 'X', 'Y', 'Z']
	if raw.contains('[') {
		for marker in markers {
			if raw.contains('[${marker}]') || raw.contains('[${marker},')
				|| raw.contains(', ${marker}]') || raw.contains(',${marker}]') {
				return true
			}
		}
	}
	for part in raw.split('_') {
		if part in markers { return true }
	}
	return false
}

fn test_source_type_syntax_open_receiver_marker_preserves_raw_patterns() {
	for raw in ['', 'T', 'Q', 'TT', '_T_', '__T__', 'a__T__b', 'a_Tx', ' T ', '[T]', '[T,', ',T]',
		', T]', ',  T]', ',T][', ', T][', 'T,][', '[Q]', 'Box[Q,T]', 'Box[Q, T]', 'Box[Q,  T]',
		'[Tq]', '[T,broken', 'pkg.Name_T', 'pkg.T', 'pkg.Name__T_more', 'é_T_é', '\tT', 'a___T___b'] {
		expected := source_type_syntax_expected_open_receiver_marker(raw)
		assert source_type_syntax_open_generic_receiver_marker(raw) == expected
		assert source_type_syntax(source_type_id(raw)).open_generic_receiver_marker() == expected
	}
	for marker in ['T', 'U', 'K', 'V', 'A', 'B', 'C', 'X', 'Y', 'Z', 'Q', 't', 'é'] {
		for prefix in ['', '_', '__', '[', ',', ', ', ',  ', 'pkg.', 'a_'] {
			for suffix in ['', '_', '__', ']', ',', '][', '_b', 'x]'] {
				raw := prefix + marker + suffix
				assert source_type_syntax_open_generic_receiver_marker(raw) ==
					source_type_syntax_expected_open_receiver_marker(raw)
			}
		}
	}
}

fn test_source_type_syntax_name_markused_components_reuse_identity_exactly() {
	for raw in ['Name', 'pkg.Name', 'main.Some_Name', 'pkg.lower', '_Name', '_', 'T', 'C.T', 'JS.Value',
		'123T', '123_T', '123name', '123', 'main.123', 'pkg..Name', 'pkg.Name@Other', ' Name',
		'&pkg.Name', '[]pkg.Name', 'map[string]pkg.Name'] {
		id := source_type_id(raw)
		expected := source_type_syntax_markused_components(raw)
		parsed := source_type_syntax_parse(id, raw)
		assert parsed.markused_generic_components == expected
		assert source_type_syntax(id).markused_generic_components() == expected
		if raw in ['Name', 'pkg.Name', 'main.Some_Name', '_Name', 'T', 'C.T', 'JS.Value'] {
			entry := source_type_read_entry(id)
			assert parsed.markused_generic_components == [id]
			assert parsed.markused_generic_components.data == voidptr(&entry.id)
		}
	}
}

fn test_source_type_syntax_name_fixed_length_specialization_matches_general_query() {
	for raw in ['Name', 'name', '_name', '_Name', '_', 'T', 't', 'C', 'C.T', 'C.name', 'main.int',
		'main.T', 'pkg.some_Length', 'u8', 'int', 'usize', 'void', 'bool', 'string', 'fn', 'chan',
		'shared', 'mut', '123', '123T', '123_T', '0x10', 'pkg.123', 'pkg..name', ' pkg.name ',
		'[]name', '&name', '(name)', 'A+B'] {
		id := source_type_id(raw)
		expected := source_type_syntax_transform_length(raw)
		assert source_type_syntax_parse(id, raw).transform_fixed_length == expected
		assert source_type_syntax(id).transform_fixed_length() == expected
	}
}

fn test_source_type_syntax_symbol_name_singleton_tag_stays_private_and_owned() {
	for raw in ['Name', '_Name', 'pkg.Name', '123', '123T', '123_T', '', '&pkg.Name', 'pkg..Name',
		'pkg.Name@Other', ' Name', '[]Name'] {
		id := source_type_id(raw)
		expected_parts, expected_names := source_type_syntax_symbol_parts(raw)
		syntax := source_type_syntax(id)
		assert syntax.symbol_parts() == expected_parts
		assert syntax.symbol_is_name() == expected_names
		assert source_type_syntax(id) == syntax
	}
}

fn test_source_type_syntax_lazy_name_views_preserve_raw_queries() {
	for raw in ['', 'Name', 'main.Name', 'main.main.Name', 'domain.Name', 'main.', '__', 'A__B',
		'__A', 'A__', 'A___B', 'A____B', 'ptr', 'ptrName', 'ptrmain.Name', 'ptrA__B', ' ptrA__B ',
		'\tmain.A__B\n', '&ptrpkg.Name', 'Box[main.A__B', 'fn (main.A__B) ptrC', 'émain.A__B'] {
		id := source_type_id(raw)
		syntax := source_type_syntax(id)
		for _ in 0 .. 2 {
			assert syntax.last_double_underscore_prefix() == source_type_id(raw.all_before_last('__'))
			assert syntax.last_double_underscore_suffix() == source_type_id(raw.all_after_last('__'))
			assert syntax.doubleunderscore_dotted() == source_type_id(raw.replace('__', '.'))
			assert syntax.main_removed() == source_type_id(raw.replace('main.', ''))
			expected_ptr := if raw.starts_with('ptr') && raw.len > 3 {
				source_type_id(raw[3..])
			} else {
				u32(0)
			}
			assert syntax.ptr_name_base() == expected_ptr
		}
	}
	mut bytes := 'ptrmain.Lazy__Name'.bytes()
	borrowed := unsafe { tos(bytes.data, bytes.len) }
	syntax := source_type_syntax(source_type_id(borrowed))
	bytes[0] = 88
	assert syntax.ptr_name_base() == source_type_id('main.Lazy__Name')
	assert syntax.main_removed() == source_type_id('ptrLazy__Name')
	assert syntax.last_double_underscore_prefix() == source_type_id('ptrmain.Lazy')
	assert syntax.last_double_underscore_suffix() == source_type_id('Name')
	assert syntax.doubleunderscore_dotted() == source_type_id('ptrmain.Lazy.Name')
	zero := SourceTypeSyntax{}
	assert zero.ptr_name_base() == 0
	assert zero.main_removed() == 0
	assert zero.last_double_underscore_prefix() == 0
	assert zero.last_double_underscore_suffix() == 0
	assert zero.doubleunderscore_dotted() == 0
}

fn source_type_syntax_lazy_name_views_worker() []u32 {
	syntax := source_type_syntax(source_type_id('ptrmain.Parallel__Name'))
	return [syntax.ptr_name_base(), syntax.main_removed(), syntax.last_double_underscore_prefix(),
		syntax.last_double_underscore_suffix(), syntax.doubleunderscore_dotted()]
}

fn test_source_type_syntax_lazy_name_views_parallel_publication() {
	mut workers := []thread []u32{}
	for _ in 0 .. 4 { workers << spawn source_type_syntax_lazy_name_views_worker() }
	expected := [source_type_id('main.Parallel__Name'), source_type_id('ptrParallel__Name'),
		source_type_id('ptrmain.Parallel'), source_type_id('Name'),
		source_type_id('ptrmain.Parallel.Name')]
	for worker in workers {
		assert worker.wait() == expected
	}
}

fn test_source_type_syntax_lexical_views_preserve_each_raw_grammar() {
	for raw in ['', 'Name', 'pkg.Name', 'pkg.1T', 'A..B.', '123T', '123_T', '  fn (x Box[T], C.T) ![]T ',
		'FooBar Bar Bar', '"T.name"+1U', 'map[Alias'] {
		id := source_type_id(raw)
		syntax := source_type_syntax(id)
		symbols, offsets := source_type_syntax_symbols(raw)
		parts, names := source_type_syntax_symbol_parts(raw)
		mut dotted := []u32{}
		for part in raw.split('.') { dotted << source_type_syntax_child_id(id, raw, part) }
		assert syntax.symbols() == symbols
		assert syntax.symbols_first_offsets() == offsets
		assert syntax.words() == source_type_syntax_tokens(raw, false)
		assert syntax.legacy_generic_words() == source_type_syntax_legacy_words(raw)
		assert syntax.symbol_parts() == parts
		assert syntax.symbol_is_name() == names
		assert syntax.dotted_parts() == dotted
	}
	empty := SourceTypeSyntax{}
	assert empty.symbols().len == 0
	assert empty.symbols_first_offsets().len == 0
	assert empty.words().len == 0
	assert empty.legacy_generic_words().len == 0
	assert empty.symbol_parts().len == 0
	assert empty.symbol_is_name().len == 0
	assert empty.dotted_parts().len == 0
}

fn test_source_type_syntax_lexical_views_publish_only_on_request() {
	id := source_type_id('  LazyLexicalCheckpoint479.T, []U! ')
	syntax := source_type_syntax(id)
	for kind in [SourceTypeLexicalKind.symbols, .words, .legacy_words, .dotted, .parts] {
		assert isnil(source_type_syntax_lexical_find(source_type_syntax_lexical_slot(id, kind), id, kind))
	}
	words := syntax.words()
	assert words.len > 0
	assert words.data == syntax.words().data
	assert isnil(source_type_syntax_lexical_find(source_type_syntax_lexical_slot(id, .symbols), id, .symbols))
	assert isnil(source_type_syntax_lexical_find(source_type_syntax_lexical_slot(id, .parts), id, .parts))
	parts := syntax.symbol_parts()
	names := syntax.symbol_is_name()
	assert parts.len == names.len
	assert parts.data == syntax.symbol_parts().data
	assert names.data == syntax.symbol_is_name().data
	zero_words := source_type_syntax_lexical(0, .words)
	assert source_type_syntax_lexical_ids(zero_words, false).len == 0
	assert zero_words == source_type_syntax_lexical(0, .words)
}

fn source_type_syntax_lexical_in_disposable_scope() u32 {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		id := source_type_id('  LazyLexicalWorker489.ns.fn (T) []U '.clone())
		syntax := source_type_syntax(id)
		syntax.symbols()
		syntax.symbols_first_offsets()
		syntax.words()
		syntax.legacy_generic_words()
		syntax.dotted_parts()
		syntax.symbol_parts()
		syntax.symbol_is_name()
		syntax.qualifier_prefixes()
		syntax.qualifier_suffixes()
		unsafe {
			prealloc_scope_leave(scope)
			prealloc_scope_free_after(scope)
			prealloc_thread_cleanup()
		}
		return id
	} $else {
		id := source_type_id('  LazyLexicalWorker489.ns.fn (T) []U '.clone())
		syntax := source_type_syntax(id)
		syntax.symbols()
		syntax.symbols_first_offsets()
		syntax.words()
		syntax.legacy_generic_words()
		syntax.dotted_parts()
		syntax.symbol_parts()
		syntax.symbol_is_name()
		syntax.qualifier_prefixes()
		syntax.qualifier_suffixes()
		return id
	}
}

fn test_source_type_syntax_lexical_views_outlive_parallel_workers() {
	mut handles := []thread u32{}
	for _ in 0 .. 4 { handles << spawn source_type_syntax_lexical_in_disposable_scope() }
	ids := handles.wait()
	for id in ids {
		raw := source_type_name(id)
		syntax := source_type_syntax(id)
		symbols, offsets := source_type_syntax_symbols(raw)
		parts, names := source_type_syntax_symbol_parts(raw)
		assert syntax.symbols() == symbols
		assert syntax.symbols_first_offsets() == offsets
		assert syntax.symbol_parts() == parts
		assert syntax.symbol_is_name() == names
		assert syntax.words() == source_type_syntax_tokens(raw, false)
		assert syntax.legacy_generic_words() == source_type_syntax_legacy_words(raw)
		prefixes, suffixes := source_type_syntax_expected_qualifier_parts(raw)
		assert syntax.qualifier_prefixes() == prefixes
		assert syntax.qualifier_suffixes() == suffixes
	}
}

fn source_type_syntax_expected_qualifier_parts(raw string) ([]u32, []u32) {
	mut prefixes := []u32{}
	mut suffixes := []u32{}
	for i, c in raw {
		if c == `.` {
			prefixes << source_type_id(raw[..i])
			suffixes << source_type_id(raw[i + 1..])
		}
	}
	return prefixes, suffixes
}

fn test_source_type_syntax_lazy_qualifiers_preserve_every_raw_dot() {
	for raw in ['', 'Name', '.', '.A', 'A.', '..', 'A..B.', 'pkg.Name', 'pkg.sub.Name', ' pkg.sub.Name ',
		'fn (pkg.A) []other.B', 'map[C.int]pkg.Name', 'é.pkg.名', 'pkg.JS.sub.C.Type',
		'pkg.C.sub.JS.Type', '.C.Name', '.JS.Name'] {
		id := source_type_id(raw)
		syntax := source_type_syntax(id)
		prefixes, suffixes := source_type_syntax_expected_qualifier_parts(raw)
		assert syntax.qualifier_prefixes() == prefixes
		assert syntax.qualifier_suffixes() == suffixes
		assert syntax.qualifier_prefixes().data == syntax.qualifier_prefixes().data
		assert syntax.qualifier_suffixes().data == syntax.qualifier_suffixes().data
		assert syntax.first_dot_prefix() == source_type_id(raw.all_before('.'))
		assert syntax.first_dot_suffix() == source_type_id(raw.all_after('.'))
		assert syntax.qualifier() == source_type_id(raw.all_before_last('.'))
		assert syntax.unqualified() == source_type_id(raw.all_after_last('.'))
		for i, prefix in prefixes {
			assert source_type_has_qualified_prefix(id, prefix)
			assert source_type_qualified_suffix(id, prefix) or { panic('missing qualifier') } == suffixes[i]
		}
		assert !source_type_has_qualified_prefix(id, id)
		assert source_type_qualified_suffix(id, id) == none
	}
	zero := SourceTypeSyntax{}
	assert zero.qualifier_prefixes().len == 0
	assert zero.qualifier_suffixes().len == 0
}

fn source_type_syntax_copied_qualifier_parts(id u32) ([]u32, []u32) {
	copied := *source_type_syntax(id)
	return copied.qualifier_prefixes(), copied.qualifier_suffixes()
}

fn test_source_type_syntax_qualifier_scalar_and_singleton_queries_remain_lazy() {
	id := source_type_id('QualifierLazyFresh701.middle.tail')
	syntax := source_type_syntax(id)
	slot := source_type_syntax_lexical_slot(id, .qualifiers)
	assert isnil(source_type_syntax_lexical_find(slot, id, .qualifiers))
	assert source_type_has_qualified_prefix(id, syntax.first_dot_prefix())
	assert source_type_has_qualified_prefix(id, syntax.qualifier())
	assert source_type_qualified_suffix(id, syntax.first_dot_prefix()) or { panic('missing first') } == syntax.first_dot_suffix()
	assert source_type_qualified_suffix(id, syntax.qualifier()) or { panic('missing last') } == syntax.unqualified()
	assert isnil(source_type_syntax_lexical_find(slot, id, .qualifiers))
	prefixes := syntax.qualifier_prefixes()
	entry := source_type_syntax_lexical_find(slot, id, .qualifiers)
	assert !isnil(entry)
	suffixes := syntax.qualifier_suffixes()
	assert entry == source_type_syntax_lexical_find(slot, id, .qualifiers)
	assert prefixes.data == syntax.qualifier_prefixes().data
	assert suffixes.data == syntax.qualifier_suffixes().data
	for raw in ['QualifierSingletonFresh701.Name', '.QualifierSingletonFresh701',
		'QualifierSingletonFresh701.'] {
		single_id := source_type_id(raw)
		single := source_type_syntax(single_id)
		single_slot := source_type_syntax_lexical_slot(single_id, .qualifiers)
		assert single.qualifier_prefixes() == [single.first_dot_prefix()]
		assert single.qualifier_suffixes() == [single.first_dot_suffix()]
		copied_prefixes, copied_suffixes := source_type_syntax_copied_qualifier_parts(single_id)
		assert copied_prefixes == single.qualifier_prefixes()
		assert copied_suffixes == single.qualifier_suffixes()
		prefix_data := if single.first_dot_prefix() == 0 {
			voidptr(&g_source_type_syntax_zero_offset)
		} else {
			voidptr(&source_type_read_entry(single.first_dot_prefix()).id)
		}
		suffix_data := if single.first_dot_suffix() == 0 {
			voidptr(&g_source_type_syntax_zero_offset)
		} else {
			voidptr(&source_type_read_entry(single.first_dot_suffix()).id)
		}
		assert copied_prefixes.data == prefix_data
		assert copied_suffixes.data == suffix_data
		assert isnil(source_type_syntax_lexical_find(single_slot, single_id, .qualifiers))
	}
}

fn test_source_type_syntax_name_publication_preserves_all_packed_views() {
	for raw, expected_name_packing in {
		'Name':                         true
		'T':                            true
		'C':                            true
		'Z':                            true
		'pkg.Name':                     true
		'T.Value':                      true
		'pkg.T':                        true
		'_Name':                        true
		'123T':                         true
		'123T456U':                     true
		'123_T':                        true
		'voidptr':                      true
		'AvoidptrvoidptrTail':          true
		'pkg.voidptrName':              true
		'Map_voidptr_FASTC_ARRAY_OF_T': true
		'':                             false
		' Name ':                       false
		'main.123':                     false
		'pkg..Name':                    false
		'.A':                           false
		'A.':                           false
		'map[T]U':                      false
		'fn (T, U) V':                  false
		'(T, U)':                       false
		'Box[T':                        false
		'[3]T':                         false
		'T | U':                        false
	} {
		id := source_type_id(raw)
		parsed := source_type_syntax_parse(id, raw)
		assert parsed.name_packing == expected_name_packing
		syntax := source_type_syntax(id)
		assert_source_type_syntax_scalar_views(syntax, parsed)
		assert syntax.args() == parsed.args
		assert syntax.application_args() == parsed.application_args
		assert syntax.generic_app_args() == parsed.generic_app_args
		assert syntax.postfix_dims() == parsed.postfix_dims
		assert syntax.function_params() == parsed.function_params
		assert syntax.tuple_components() == parsed.tuple_components
		assert syntax.cgen_app_args() == parsed.cgen_app_args
		assert syntax.cgen_function_params() == parsed.cgen_function_params
		assert syntax.checker_function_params() == parsed.checker_function_params
		assert syntax.checker_tuple_components() == parsed.checker_tuple_components
		assert syntax.cgen_raw_app_args() == parsed.cgen_raw_app_args
		assert syntax.cgen_parts() == parsed.cgen_parts
		assert syntax.checker_raw_tuple_components() == parsed.checker_raw_tuple_components
		assert syntax.bracket_raw_parts() == parsed.bracket_raw_parts
		assert syntax.bracket_generic_parts() == parsed.bracket_generic_parts
		assert syntax.tuple_function_components() == parsed.tuple_function_components
		assert syntax.plain_comma_parts() == parsed.plain_comma_parts
		assert syntax.generic_mentions() == parsed.generic_mentions
		assert syntax.receiver_app_args() == parsed.receiver_app_args
		assert syntax.checker_bracket_components() == parsed.checker_bracket_components
		assert syntax.voidptr_parts() == parsed.voidptr_parts
		assert syntax.inferred_parts() == parsed.inferred_parts
		assert syntax.inferred_annotations() == parsed.inferred_annotations
		assert syntax.inferred_expr_ids() == parsed.inferred_expr_ids
		assert syntax.substitution_function_params() == parsed.substitution_function_params
		assert syntax.written_applications() == parsed.written_applications
		assert syntax.written_application_offsets() == parsed.written_application_offsets
		assert syntax.checker_raw_bracket_components() == parsed.checker_raw_bracket_components
		assert syntax.cgen_placeholder_candidates() == parsed.cgen_placeholder_candidates
		assert syntax.reflection_parts() == parsed.reflection_parts
		assert syntax.reflection_tuple_parts() == parsed.reflection_tuple_parts
		assert syntax.receiver_raw_app_args() == parsed.receiver_raw_app_args
		assert syntax.vls_generic_words() == parsed.vls_generic_words
		assert syntax.eval_tuple_parts() == parsed.eval_tuple_parts
		assert syntax.implicit_tuple_components() == parsed.implicit_tuple_components
		assert syntax.generic_application_heads() == parsed.generic_application_heads
		assert syntax.markused_application_heads() == parsed.markused_application_heads
		assert syntax.markused_generic_components() == parsed.markused_generic_components
		assert syntax.raw_tuple_parse_components() == parsed.raw_tuple_parse_components
		assert syntax.union_parts() == parsed.union_parts
		if parsed.name_packing {
			assert syntax.cgen_parts().data == syntax.payload
			assert syntax.plain_comma_parts().data == syntax.payload
			assert syntax.inferred_parts().data == syntax.payload
			assert syntax.reflection_parts().data == syntax.payload
			assert syntax.union_parts().data == syntax.payload
			assert parsed.args.len == 0
			assert parsed.application_args.len == 0
			assert parsed.generic_app_args.len == 0
			assert parsed.postfix_dims.len == 0
			assert parsed.function_params.len == 0
			assert parsed.tuple_components.len == 0
			assert parsed.cgen_app_args.len == 0
			assert parsed.cgen_function_params.len == 0
			assert parsed.checker_function_params.len == 0
			assert parsed.checker_tuple_components.len == 0
			assert parsed.cgen_raw_app_args.len == 0
			assert parsed.checker_raw_tuple_components.len == 0
			assert parsed.bracket_raw_parts.len == 0
			assert parsed.bracket_generic_parts.len == 0
			assert parsed.tuple_function_components.len == 0
			assert parsed.receiver_app_args.len == 0
			assert parsed.checker_bracket_components.len == 0
			assert parsed.inferred_annotations.len == 0
			assert parsed.inferred_expr_ids.len == 0
			assert parsed.substitution_function_params.len == 0
			assert parsed.written_applications.len == 0
			assert parsed.written_application_offsets.len == 0
			assert parsed.checker_raw_bracket_components.len == 0
			assert parsed.reflection_tuple_parts.len == 0
			assert parsed.receiver_raw_app_args.len == 0
			assert parsed.eval_tuple_parts.len == 0
			assert parsed.implicit_tuple_components.len == 0
			assert parsed.generic_application_heads.len == 0
			assert parsed.markused_application_heads.len == 0
			assert parsed.raw_tuple_parse_components.len == 0
			assert parsed.generic_mentions.len == 0 || parsed.generic_mentions == [id]
			assert parsed.cgen_placeholder_candidates.len == 0 || parsed.cgen_placeholder_candidates == [id]
		}
	}
}

fn test_source_type_syntax_composite_identity_parts_keep_legacy_bytes() {
	for raw in ['', ' ', '\t', '\n', ' T ', '&T', '[]T', '?T', '!T', 'T*', 'T;', 'fn (T) U', 'Box[T]',
		'Box[T]suffix', 'Box[', ')T', 'T)', 'T{U}', ' map[T]U ', 'T,', ',T', 'T, ', 'T,,U', 'Box[T, U]',
		'fn (T, U) V', 'T | U', '|', ' | ', 'T|', 'T[U|V]', 'T{U|V}', 'voidptr', 'voidptrvoidptr',
		'&voidptr', 'TvoidptrU', 'typeof(__expr_7)', 'A typeof(__expr_7) B', 'typeof(__expr_',
		'typeof(__expr_7', 'typeof(__expr_7))', 'é[名]'] {
		id := source_type_id(raw)
		syntax := source_type_syntax(id)
		assert_source_type_syntax_scalar_views(syntax, source_type_syntax_parse(id, raw))
		assert syntax.plain_comma_parts() == source_type_syntax_plain_parts(raw, ',')
		assert syntax.voidptr_parts() == source_type_syntax_plain_parts(raw, 'voidptr')
		assert syntax.cgen_parts() == source_type_syntax_legacy_parts(raw)
		assert syntax.reflection_parts() == source_type_syntax_reflection_parts(raw)
		assert syntax.union_parts() == source_type_syntax_union_parts(raw)
		inferred_parts, annotations, expressions := source_type_syntax_inferred(raw)
		assert syntax.inferred_parts() == inferred_parts
		assert syntax.inferred_annotations() == annotations
		assert syntax.inferred_expr_ids() == expressions
	}
}

fn test_source_type_syntax_composite_identity_parts_publish_independent_headers() {
	id := source_type_id('&CompositeIdentityPartsFresh702')
	syntax := source_type_syntax(id)
	mut views := [syntax.plain_comma_parts(), syntax.voidptr_parts(), syntax.cgen_parts(),
		syntax.reflection_parts(), syntax.union_parts(), syntax.inferred_parts()]
	for mut view in views {
		assert view == [id]
		view.trim(0)
		assert view.len == 0
	}
	assert syntax.plain_comma_parts() == [id]
	assert syntax.voidptr_parts() == [id]
	assert syntax.cgen_parts() == [id]
	assert syntax.reflection_parts() == [id]
	assert syntax.union_parts() == [id]
	assert syntax.inferred_parts() == [id]
	assert syntax.inferred_annotations().len == 0
	assert syntax.inferred_expr_ids().len == 0
}

// Compare the packed representation with the independently computed cold queries.
fn assert_source_type_syntax_scalar_views(syntax &SourceTypeSyntax, parsed SourceTypeSyntaxParsed) {
	assert syntax.qualifier_count() == parsed.qualifier_count
	assert syntax.kind() == parsed.kind
	assert syntax.trimmed() == parsed.trimmed
	assert syntax.base() == parsed.base
	assert syntax.unqualified() == parsed.unqualified
	assert syntax.qualifier() == parsed.qualifier
	assert syntax.generic_base() == parsed.generic_base
	assert syntax.pointer_base() == parsed.pointer_base
	assert syntax.pointer_depth() == parsed.pointer_depth
	assert syntax.function_has_result() == parsed.function_has_result
	assert syntax.raw_base() == parsed.raw_base
	assert syntax.main_base() == parsed.main_base
	assert syntax.bare_generic() == parsed.bare_generic
	assert syntax.application_base() == parsed.application_base
	assert syntax.application_valid() == parsed.application_valid
	assert syntax.generic_app_base() == parsed.generic_app_base
	assert syntax.generic_app_valid() == parsed.generic_app_valid
	assert syntax.postfix_base() == parsed.postfix_base
	assert syntax.parameter_type() == parsed.parameter_type
	assert syntax.parameter_mut_type() == parsed.parameter_mut_type
	assert syntax.bracket_closed() == parsed.bracket_closed
	assert syntax.bracket_base() == parsed.bracket_base
	assert syntax.bracket_inner() == parsed.bracket_inner
	assert syntax.bracket_raw_inner() == parsed.bracket_raw_inner
	assert syntax.bracket_prefix() == parsed.bracket_prefix
	assert syntax.function_closed() == parsed.function_closed
	assert syntax.function_result() == parsed.function_result
	assert syntax.tuple_like() == parsed.tuple_like
	assert syntax.function_space_prefix() == parsed.function_space_prefix
	assert syntax.spaced() == parsed.spaced
	assert syntax.first_bracket_closed() == parsed.first_bracket_closed
	assert syntax.first_bracket_base() == parsed.first_bracket_base
	assert syntax.last_space_word() == parsed.last_space_word
	assert syntax.named_type() == parsed.named_type
	assert syntax.c_qualified() == parsed.c_qualified
	assert syntax.first_dot_prefix() == parsed.first_dot_prefix
	assert syntax.first_dot_suffix() == parsed.first_dot_suffix
	assert syntax.builtin_prefix() == parsed.builtin_prefix
	assert syntax.main_fragment() == parsed.main_fragment
	assert syntax.function_prefix() == parsed.function_prefix
	assert syntax.fn_prefix() == parsed.fn_prefix
	assert syntax.shared_fragment() == parsed.shared_fragment
	assert syntax.pointer_raw_base() == parsed.pointer_raw_base
	assert syntax.builtin_base() == parsed.builtin_base
	assert syntax.cgen_app_base() == parsed.cgen_app_base
	assert syntax.cgen_app_valid() == parsed.cgen_app_valid
	assert syntax.tuple_contains_comma() == parsed.tuple_contains_comma
	assert syntax.top_space_head() == parsed.top_space_head
	assert syntax.top_space_tail() == parsed.top_space_tail
	assert syntax.top_space_valid() == parsed.top_space_valid
	assert syntax.typeof_expr_prefix() == parsed.typeof_expr_prefix
	assert syntax.bracket_suffix() == parsed.bracket_suffix
	assert syntax.bracket_from() == parsed.bracket_from
	assert syntax.bracket_inner_fixed_length() == parsed.bracket_inner_fixed_length
	assert syntax.first_bracket_prefix() == parsed.first_bracket_prefix
	assert syntax.void_pointer_postfix() == parsed.void_pointer_postfix
	assert syntax.canonical_dotted_name() == parsed.canonical_dotted_name
	assert syntax.decimal() == parsed.decimal
	assert syntax.contains_comma() == parsed.contains_comma
	assert syntax.function_fragment() == parsed.function_fragment
	assert syntax.dots_base() == parsed.dots_base
	assert syntax.cgen_raw_app_base() == parsed.cgen_raw_app_base
	assert syntax.cgen_raw_app_valid() == parsed.cgen_raw_app_valid
	assert syntax.generic_parameter_payload() == parsed.generic_parameter_payload
	assert syntax.legacy_parameter_type() == parsed.legacy_parameter_type
	assert syntax.legacy_parameter_mut_type() == parsed.legacy_parameter_mut_type
	assert syntax.first_bracket_length() == parsed.first_bracket_length
	assert syntax.c_pointer_base() == parsed.c_pointer_base
	assert syntax.c_ptr_name_suffix() == parsed.c_ptr_name_suffix
	assert syntax.c_fn_type_fragment() == parsed.c_fn_type_fragment
	assert syntax.underscored() == parsed.underscored
	assert syntax.source_wrapper_base() == parsed.source_wrapper_base
	assert syntax.declaration_head() == parsed.declaration_head
	assert syntax.fn_fragment() == parsed.fn_fragment
	assert syntax.compact_fn_spelling() == parsed.compact_fn_spelling
	assert syntax.first_bracket_raw_inner() == parsed.first_bracket_raw_inner
	assert syntax.cgen_generic_placeholder() == parsed.cgen_generic_placeholder
	assert syntax.fixed_array_type() == parsed.fixed_array_type
	assert syntax.typeof_valid() == parsed.typeof_valid
	assert syntax.typeof_inner() == parsed.typeof_inner
	assert syntax.postfix_index_base() == parsed.postfix_index_base
	assert syntax.last_bracket_prefix() == parsed.last_bracket_prefix
	assert syntax.last_bracket_length() == parsed.last_bracket_length
	assert syntax.trailing_bracket_prefix() == parsed.trailing_bracket_prefix
	assert syntax.trailing_bracket_closed() == parsed.trailing_bracket_closed
	assert syntax.array_chars_base() == parsed.array_chars_base
	assert syntax.lowercase_value_ident() == parsed.lowercase_value_ident
	assert syntax.typeof_prefix() == parsed.typeof_prefix
	assert syntax.first_capital() == parsed.first_capital
	assert syntax.named_variant() == parsed.named_variant
	assert syntax.named_variant_valid() == parsed.named_variant_valid
	assert syntax.named_variant_owner() == parsed.named_variant_owner
	assert syntax.named_variant_name() == parsed.named_variant_name
	assert syntax.main_unlocked() == parsed.main_unlocked
	assert syntax.encoded_fixed_length() == parsed.encoded_fixed_length
	assert syntax.encoded_fixed_base() == parsed.encoded_fixed_base
	assert syntax.map_name_prefix() == parsed.map_name_prefix
	assert syntax.suffix_bracket_length() == parsed.suffix_bracket_length
	assert syntax.suffix_bracket_valid() == parsed.suffix_bracket_valid
	assert syntax.map_function_fragment() == parsed.map_function_fragment
	assert syntax.bracket_source_inner() == parsed.bracket_source_inner
	assert syntax.c_fixed_typedef_element() == parsed.c_fixed_typedef_element
	assert syntax.c_fixed_typedef_length() == parsed.c_fixed_typedef_length
	assert syntax.c_fixed_typedef_valid() == parsed.c_fixed_typedef_valid
	assert syntax.fixed_array_query_length() == parsed.fixed_array_query_length
	assert syntax.last_bracket_span_valid() == parsed.last_bracket_span_valid
	assert syntax.last_bracket_span_inner() == parsed.last_bracket_span_inner
	assert syntax.plain_type_name() == parsed.plain_type_name
	assert syntax.generic_app_legacy_suffix() == parsed.generic_app_legacy_suffix
	assert syntax.wrapper_prefix() == parsed.wrapper_prefix
	assert syntax.wrapper_base() == parsed.wrapper_base
	assert syntax.checker_fixed_array_may_be() == parsed.checker_fixed_array_may_be
	assert syntax.dollar_prefix() == parsed.dollar_prefix
	assert syntax.define_prefix() == parsed.define_prefix
	assert syntax.define_fragment() == parsed.define_fragment
	assert syntax.paren_prefix() == parsed.paren_prefix
	assert syntax.fixed_length() == parsed.fixed_length
	assert syntax.qualified_modifiers() == parsed.qualified_modifiers
	assert syntax.plain_alias_target() == parsed.plain_alias_target
	assert syntax.c_array_payload() == parsed.c_array_payload
	assert syntax.c_fixed_array_payload() == parsed.c_fixed_array_payload
	assert syntax.fastc_array_marker() == parsed.fastc_array_marker
	assert syntax.fastc_array_marker_payload() == parsed.fastc_array_marker_payload
	assert syntax.c_map_payload() == parsed.c_map_payload
	assert syntax.c_ptr_name_base() == parsed.c_ptr_name_base
	assert syntax.c_tagged_type() == parsed.c_tagged_type
	assert syntax.backward_bracket_prefix() == parsed.backward_bracket_prefix
	assert syntax.backward_bracket_closed() == parsed.backward_bracket_closed
	assert syntax.opening_paren() == parsed.opening_paren
	assert syntax.byte_len() == parsed.byte_len
	assert syntax.receiver_app_valid() == parsed.receiver_app_valid
	assert syntax.receiver_app_base() == parsed.receiver_app_base
	assert syntax.declaration_parameter_payload() == parsed.declaration_parameter_payload
	assert syntax.declaration_parameter_mut() == parsed.declaration_parameter_mut
	assert syntax.prefix_dimension_tail() == parsed.prefix_dimension_tail
	assert syntax.prefix_dimensions_closed() == parsed.prefix_dimensions_closed
	assert syntax.backward_bracket_after_prefix_dims() == parsed.backward_bracket_after_prefix_dims
	assert syntax.backward_bracket_inner() == parsed.backward_bracket_inner
	assert syntax.option_result_void_prefix() == parsed.option_result_void_prefix
	assert syntax.nested_option_result_marker() == parsed.nested_option_result_marker
	assert syntax.c_pointer_depth() == parsed.c_pointer_depth
	assert syntax.c_pointer_count() == parsed.c_pointer_count
	assert syntax.c_pointer_raw_base() == parsed.c_pointer_raw_base
	assert syntax.thread_value_prefix() == parsed.thread_value_prefix
	assert syntax.thread_value_payload() == parsed.thread_value_payload
	assert syntax.thread_value_type_valid() == parsed.thread_value_type_valid
	assert syntax.thread_value_type() == parsed.thread_value_type
	assert syntax.contains_closing_bracket() == parsed.contains_closing_bracket
	assert syntax.atomic_val_fragment() == parsed.atomic_val_fragment
	assert syntax.expr_prefix() == parsed.expr_prefix
	assert syntax.expr_id() == parsed.expr_id
	assert syntax.parameter_name() == parsed.parameter_name
	assert syntax.parameter_named() == parsed.parameter_named
	assert syntax.parameter_is_mut() == parsed.parameter_is_mut
	assert syntax.transform_fixed_length() == parsed.transform_fixed_length
	assert syntax.receiver_collection_id() == parsed.receiver_collection_id
	assert syntax.ampersand_trimmed() == parsed.ampersand_trimmed
	assert syntax.contains_doubleunderscore() == parsed.contains_doubleunderscore
	assert syntax.parallel_preseed_fn() == parsed.parallel_preseed_fn
	assert syntax.parallel_preseed_short() == parsed.parallel_preseed_short
	assert syntax.closing_bracket() == parsed.closing_bracket
	assert syntax.constraint_receiver_base() == parsed.constraint_receiver_base
	assert syntax.result_suffix() == parsed.result_suffix
	assert syntax.cgen_decl_unusable() == parsed.cgen_decl_unusable
	assert syntax.cgen_decl_authority() == parsed.cgen_decl_authority
	assert syntax.generic_encoded_fragment() == parsed.generic_encoded_fragment
	assert syntax.fn_ptr_encoded_prefix() == parsed.fn_ptr_encoded_prefix
	assert syntax.c_result_prefix() == parsed.c_result_prefix
	assert syntax.c_option_prefix() == parsed.c_option_prefix
	assert syntax.c_multi_return_prefix() == parsed.c_multi_return_prefix
	assert syntax.c_lower_map_prefix() == parsed.c_lower_map_prefix
	assert syntax.open_generic_receiver_marker() == parsed.open_generic_receiver_marker
	assert syntax.space_removed() == parsed.space_removed
	assert syntax.qualified_name_chars() == parsed.qualified_name_chars
	assert syntax.field_owner_module() == parsed.field_owner_module
	assert syntax.const_name_prefix() == parsed.const_name_prefix
	assert syntax.pthread_prefix() == parsed.pthread_prefix
	assert syntax.thread_bracket_prefix() == parsed.thread_bracket_prefix
	assert syntax.fixed_typedef_bracket() == parsed.fixed_typedef_bracket
	assert syntax.question_trimmed() == parsed.question_trimmed
	assert syntax.c_array_fixed_prefix() == parsed.c_array_fixed_prefix
	assert syntax.function_space_result() == parsed.function_space_result
	assert syntax.decimal_value() == parsed.decimal_value
	assert syntax.integer_value() == parsed.integer_value
	assert syntax.encoded_alnum_dotted() == parsed.encoded_alnum_dotted
	assert syntax.internal_symbol_prefix() == parsed.internal_symbol_prefix
	assert syntax.main_at_base() == parsed.main_at_base
	assert syntax.incremental_generic_application() == parsed.incremental_generic_application
	assert syntax.system_error_suffix() == parsed.system_error_suffix
	assert syntax.receiver_app_inner() == parsed.receiver_app_inner
	assert syntax.fastc_fixed_array_length_valid() == parsed.fastc_fixed_array_length_valid
	assert syntax.fastc_fixed_array_length() == parsed.fastc_fixed_array_length
	assert syntax.contains_capital() == parsed.contains_capital
	assert syntax.first_bracket_source_inner() == parsed.first_bracket_source_inner
	assert syntax.c_function_pointer_hole() == parsed.c_function_pointer_hole
	assert syntax.channel_fragment() == parsed.channel_fragment
	assert syntax.array_receiver_fragment() == parsed.array_receiver_fragment
	assert syntax.mangled_open_generic_marker() == parsed.mangled_open_generic_marker
	assert syntax.array_suffix() == parsed.array_suffix
	assert syntax.empty_function_prefix() == parsed.empty_function_prefix
	assert syntax.c_shared_fragment() == parsed.c_shared_fragment
	assert syntax.typ_suffix() == parsed.typ_suffix
	assert syntax.typ_suffix_base() == parsed.typ_suffix_base
	assert syntax.checker_parse_function_result() == parsed.checker_parse_function_result
	assert syntax.checker_parse_function_has_params() == parsed.checker_parse_function_has_params
	assert syntax.top_level_dot_valid() == parsed.top_level_dot_valid
	assert syntax.top_level_dot_prefix() == parsed.top_level_dot_prefix
	assert syntax.top_level_dot_suffix() == parsed.top_level_dot_suffix
	assert syntax.parser_fn_decl_valid() == parsed.parser_fn_decl_valid
	assert syntax.parser_fn_decl_params() == parsed.parser_fn_decl_params
	assert syntax.parser_fn_decl_result() == parsed.parser_fn_decl_result
	assert syntax.contains_question() == parsed.contains_question
	assert syntax.contains_union() == parsed.contains_union
	assert syntax.named_variant_dot_valid() == parsed.named_variant_dot_valid
	assert syntax.named_variant_dot_prefix() == parsed.named_variant_dot_prefix
	assert syntax.named_variant_dot_suffix() == parsed.named_variant_dot_suffix
}

fn test_source_type_syntax_zero_record_has_empty_numeric_views() {
	syntax := SourceTypeSyntax{}
	assert_source_type_syntax_scalar_views(&syntax, SourceTypeSyntaxParsed{})
	assert syntax.args().len == 0
	assert syntax.symbols().len == 0
	assert syntax.words().len == 0
	assert syntax.application_args().len == 0
	assert syntax.generic_app_args().len == 0
	assert syntax.postfix_dims().len == 0
	assert syntax.function_params().len == 0
	assert syntax.tuple_components().len == 0
	assert syntax.qualifier_prefixes().len == 0
	assert syntax.qualifier_suffixes().len == 0
	assert syntax.cgen_app_args().len == 0
	assert syntax.cgen_function_params().len == 0
	assert syntax.checker_function_params().len == 0
	assert syntax.checker_tuple_components().len == 0
	assert syntax.cgen_raw_app_args().len == 0
	assert syntax.cgen_parts().len == 0
	assert syntax.checker_raw_tuple_components().len == 0
	assert syntax.bracket_raw_parts().len == 0
	assert syntax.bracket_generic_parts().len == 0
	assert syntax.tuple_function_components().len == 0
	assert syntax.plain_comma_parts().len == 0
	assert syntax.underscore_prefixes().len == 0
	assert syntax.symbol_parts().len == 0
	assert syntax.dotted_parts().len == 0
	assert syntax.underscore_suffixes().len == 0
	assert syntax.generic_mentions().len == 0
	assert syntax.receiver_app_args().len == 0
	assert syntax.checker_bracket_components().len == 0
	assert syntax.legacy_generic_words().len == 0
	assert syntax.symbols_first_offsets().len == 0
	assert syntax.voidptr_parts().len == 0
	assert syntax.inferred_parts().len == 0
	assert syntax.inferred_annotations().len == 0
	assert syntax.inferred_expr_ids().len == 0
	assert syntax.substitution_function_params().len == 0
	assert syntax.flattened_receiver_parts().len == 0
	assert syntax.written_applications().len == 0
	assert syntax.written_application_offsets().len == 0
	assert syntax.checker_raw_bracket_components().len == 0
	assert syntax.cgen_placeholder_candidates().len == 0
	assert syntax.reflection_parts().len == 0
	assert syntax.reflection_tuple_parts().len == 0
	assert syntax.receiver_raw_app_args().len == 0
	assert syntax.vls_generic_words().len == 0
	assert syntax.eval_tuple_parts().len == 0
	assert syntax.implicit_tuple_components().len == 0
	assert syntax.generic_application_heads().len == 0
	assert syntax.markused_application_heads().len == 0
	assert syntax.markused_generic_components().len == 0
	assert syntax.raw_tuple_parse_components().len == 0
	assert syntax.union_parts().len == 0
}

fn test_source_type_syntax_sparse_scalars_and_flags_cross_word_boundaries() {
	id := source_type_id('&SparseBoundaryFresh703')
	syntax := source_type_syntax(id)
	assert syntax.wrapper_base() == source_type_id('SparseBoundaryFresh703')
	assert syntax.qualified_modifiers() == source_type_id('&')
	// These adjacent numeric queries occupy the last and first slots of two words.
	assert syntax.scalar_present[0] & (u64(1) << 63) != 0
	assert syntax.scalar_present[1] & 1 != 0
	assert syntax.trimmed() == id
	assert syntax.scalar_identity[0] != 0
	assert syntax.scalar_identity[1] != 0
	copied := *syntax
	assert_source_type_syntax_scalar_views(&copied, source_type_syntax_parse(id, source_type_name(id)))
	assert copied.cgen_parts().data == syntax.cgen_parts().data
	mut parts := copied.cgen_parts()
	parts.trim(0)
	assert copied.cgen_parts() == [id]
	assert syntax.cgen_parts() == [id]
	assert copied.wrapper_base() == syntax.wrapper_base()
	assert copied.qualified_modifiers() == syntax.qualified_modifiers()

	thread_id := source_type_id('__vf_thread_kT]')
	thread_syntax := source_type_syntax(thread_id)
	assert thread_syntax.thread_value_prefix()
	assert thread_syntax.thread_value_type_valid()
	assert thread_syntax.contains_closing_bracket()
	assert thread_syntax.thread_value_type() == source_type_id('T]')
	// The three flags straddle bit 63 and bit 64.
	assert thread_syntax.bool_flags[0] & (u64(3) << 62) == u64(3) << 62
	assert thread_syntax.bool_flags[1] & 1 != 0
	assert_source_type_syntax_scalar_views(thread_syntax, source_type_syntax_parse(thread_id,
		'__vf_thread_kT]'))
}

fn test_source_type_syntax_sparse_signed_integer_queries_preserve_bits() {
	// string.int() currently parses 32 bits even on targets where int is wider.
	// These spellings must retain the cold parser's conversion behavior exactly.
	for raw in ['-9223372036854775808', '-9223372036854775807', '-4294967296', '-2147483648', '-7',
		'0', '7', '2147483647', '4294967296', '9223372036854775807', '__expr_-9223372036854775808',
		'__expr_-9223372036854775807', '__expr_-4294967296', '__expr_-2147483648', '__expr_-7',
		'__expr_0', '__expr_7', '__expr_2147483647', '__expr_4294967296', '__expr_9223372036854775807',
		'??T', '&!?T'] {
		id := source_type_id(raw)
		syntax := source_type_syntax(id)
		assert_source_type_syntax_scalar_views(syntax, source_type_syntax_parse(id, raw))
		assert syntax.integer_value() == raw.int()
		if raw.starts_with('__expr_') {
			assert syntax.expr_id() == raw['__expr_'.len..].int()
		}
	}
	assert source_type_syntax(source_type_id('Name')).nested_option_result_marker() == -1
	assert source_type_syntax(source_type_id('??T')).nested_option_result_marker() == 0
	assert source_type_syntax(source_type_id('&!?T')).nested_option_result_marker() == 1
}

fn test_source_type_syntax_sparse_full_width_integer_slots() {
	if sizeof(int) != sizeof(i64) {
		return
	}
	for raw in ['-9223372036854775808', '-9223372036854775807', '-4294967296', '-1', '0', '1',
		'4294967296', '9223372036854775807'] {
		value := raw.i64()
		// The owner occupies the first aligned slot; the remaining slots hold all int queries.
		payload := [u64(0), u64(value), u64(value), u64(value), u64(value), u64(value), u64(value),
			u64(value), u64(value)]!
		// This aligned stack storage stays alive for every accessor and copied header below.
		payload_ptr := unsafe { voidptr(&payload[0]) }
		syntax := SourceTypeSyntax{
			payload:            payload_ptr
			scalar_present:     [u64(1) << 8, (u64(1) << 8) | (u64(1) << 13) | (u64(1) << 14) |
				(u64(1) << 15) | (u64(1) << 19) | (u64(1) << 29) | (u64(1) << 30)]!
			scalar_count:       8
			scalar_first_count: 1
		}
		assert syntax.pointer_depth() == int(value)
		assert syntax.byte_len() == int(value)
		assert syntax.nested_option_result_marker() == int(value)
		assert syntax.c_pointer_depth() == int(value)
		assert syntax.c_pointer_count() == int(value)
		assert syntax.expr_id() == int(value)
		assert syntax.decimal_value() == int(value)
		assert syntax.integer_value() == int(value)
		copied := syntax
		assert copied.pointer_depth() == int(value)
		assert copied.integer_value() == int(value)
	}
}
