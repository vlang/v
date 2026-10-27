module flat

fn assert_source_type_syntax(text string, kind SourceTypeSyntaxKind, base string, args []string) {
	id := source_type_id(text)
	syntax := source_type_syntax(id)
	assert syntax == source_type_syntax(id)
	assert syntax.kind == kind
	assert syntax.base == source_type_id(base)
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
	assert !source_type_syntax(source_type_id('fn()')).function_has_result
	assert source_type_syntax(source_type_id('fn() void')).function_has_result
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
	assert syntax.trimmed == source_type_id('&& pkg.Box[T]')
	assert syntax.pointer_base == source_type_id('pkg.Box[T]')
	assert syntax.pointer_depth == 2
	assert syntax.raw_base == 0
	assert syntax.unqualified == source_type_id('Box[T]  ')
	assert syntax.qualifier == source_type_id('  && pkg')
	assert syntax.generic_base == source_type_id('  && pkg.Box')
	assert syntax.words() == [source_type_id('pkg'), source_type_id('Box'), source_type_id('T')]
	assert syntax.symbols() == [source_type_id('pkg.Box'), source_type_id('T')]
	assert source_type_syntax(source_type_id('&  T')).raw_base == source_type_id('  T')
	assert source_type_syntax(source_type_id('map[T]  U')).raw_base == source_type_id('  U')
	assert source_type_syntax(source_type_id('[3]  U')).raw_base == source_type_id('  U')
	assert source_type_syntax(source_type_id('main.A.B')).main_base == source_type_id('A.B')
	assert source_type_syntax(source_type_id(' C ')).bare_generic
}

fn test_source_type_syntax_retains_permissive_query_boundaries() {
	missing := source_type_syntax(source_type_id('Box[T'))
	assert missing.kind == .invalid
	assert missing.application_valid
	assert missing.application_base == source_type_id('Box')
	assert missing.application_args() == [source_type_id('T')]
	assert !missing.generic_app_valid
	map_type := source_type_syntax(source_type_id('mut &map[K]V'))
	assert map_type.application_args() == [source_type_id('K'), source_type_id('V')]
	assert map_type.generic_app_args() == [source_type_id('K')]
	assert !source_type_syntax(source_type_id('map[K]')).application_valid
	assert !source_type_syntax(source_type_id('u8[0x10]')).application_valid
	assert !source_type_syntax(source_type_id('u8[count]')).generic_app_valid
	assert source_type_syntax(source_type_id('Box[C.int]')).generic_app_valid
	assert source_type_syntax(source_type_id('Box[T]junk')).generic_app_valid
	assert !source_type_syntax(source_type_id('fn (Box[T]) int')).generic_app_valid
	postfix := source_type_syntax(source_type_id('  T[3][ 5 ] '))
	assert postfix.postfix_base == source_type_id('T')
	assert postfix.postfix_dims() == [source_type_id('5'), source_type_id('3')]
	function := source_type_syntax(source_type_id('fn (map[T, fn (U)],, ) Box[U'))
	assert function.kind == .invalid
	assert function.function_closed
	assert function.function_params() == [source_type_id('map[T, fn (U)]')]
	assert function.function_result == source_type_id('Box[U')
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
		assert syntax.parameter_type == source_type_id(expected[i])
		assert syntax.parameter_mut_type == source_type_id(mutable[i])
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
	assert source_type_syntax(source_type_id('Box[3]tail')).cgen_app_valid
	assert !source_type_syntax(source_type_id('Box[3]tail')).generic_app_valid
	assert source_type_syntax(source_type_id('Box[T]')).bracket_from == source_type_id('[T]')
	assert source_type_syntax(source_type_id('[ n + 1 ] T')).bracket_raw_inner == source_type_id(' n + 1 ')
	assert source_type_syntax(source_type_id('[ n + 1 ] T')).bracket_prefix == source_type_id('[ n + 1 ]')
	assert source_type_syntax(source_type_id('[ n + 1 ] T')).bracket_suffix == source_type_id(' T')
	assert source_type_syntax(source_type_id('[ n + 1 ] T')).bracket_inner_fixed_length
	assert source_type_syntax(source_type_id('fn (X) T')).function_space_prefix
	assert source_type_syntax(source_type_id('builtin.pkg.Type')).builtin_base == source_type_id('pkg.Type')
	assert source_type_syntax(source_type_id('main.sub.Type')).first_dot_prefix == source_type_id('main')
	assert source_type_syntax(source_type_id('main.sub.Type')).first_dot_suffix == source_type_id('sub.Type')
	assert source_type_syntax(source_type_id('&& &T')).pointer_raw_base == source_type_id(' &T')
	assert source_type_syntax(source_type_id('map[K]V')).named_type == false
	assert source_type_syntax(source_type_id('pkg.Type')).named_type
	assert source_type_syntax(source_type_id('value map[K]V')).top_space_head == source_type_id('value')
	assert source_type_syntax(source_type_id('value map[K]V')).top_space_tail == source_type_id('map[K]V')
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

fn test_source_type_syntax_collisions_compare_ids() {
	first := source_type_id('syntax_collision.First[T]')
	second := source_type_id('syntax_collision.Second[U]')
	slot := 27
	a := source_type_syntax_miss(slot, first)
	b := source_type_syntax_miss(slot, second)
	assert a != b
	assert &source_type_syntax_find(slot, first).syntax == a
	assert &source_type_syntax_find(slot, second).syntax == b
	assert a.base == source_type_id('syntax_collision.First')
	assert b.base == source_type_id('syntax_collision.Second')
	assert a.args() == [source_type_id('T')]
	assert b.args() == [source_type_id('U')]
}

fn test_source_type_syntax_does_not_retain_mutable_input_bytes() {
	mut bytes := 'MutableType[T]'.bytes()
	borrowed := unsafe { tos(bytes.data, bytes.len) }
	id := source_type_id(borrowed)
	syntax := source_type_syntax(id)
	bytes[0] = `X`
	changed := source_type_syntax(source_type_id(borrowed))
	assert changed != syntax
	assert syntax.base == source_type_id('MutableType')
	assert changed.base == source_type_id('XutableType')
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
			assert syntax.kind == .generic
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
		assert syntax.kind == .name
		assert syntax.trimmed == id
		assert syntax.unqualified == id
		assert syntax.generic_base == id
		assert syntax.parameter_type == id
		assert syntax.generic_parameter_payload == id
		assert syntax.symbols() == [id]
		assert syntax.words() == [id]
		assert syntax.cgen_parts() == [id]
		assert syntax.plain_comma_parts() == [id]
		assert syntax.array_chars_base == id
		assert syntax.source_wrapper_base == id
		assert !syntax.fixed_array_type
	}
	assert source_type_syntax(source_type_id('under_score')).underscore_prefixes() == [source_type_id('under')]
	assert source_type_syntax(source_type_id('fnName')).fn_prefix
	assert source_type_syntax(source_type_id('T2x')).cgen_generic_placeholder
	assert !source_type_syntax(source_type_id('Name')).cgen_generic_placeholder
}

fn test_source_type_syntax_exact_cached_array_and_typeof_queries() {
	assert source_type_syntax(source_type_id('[2]int')).fixed_array_type
	assert !source_type_syntax(source_type_id('[]int')).fixed_array_type
	assert !source_type_syntax(source_type_id('[?]int')).fixed_array_type
	assert source_type_syntax(source_type_id('int[3]')).fixed_array_type
	assert !source_type_syntax(source_type_id('int[n]')).fixed_array_type
	// The old fallback uses the first bracket interior if postfix parsing fails.
	assert source_type_syntax(source_type_id('int[3]junk]')).fixed_array_type
	typ := source_type_syntax(source_type_id(' typeof(items[2]) '))
	assert typ.typeof_valid
	assert typ.typeof_inner == source_type_id('items[2]')
	assert source_type_syntax(typ.typeof_inner).postfix_index_base == source_type_id('items')
	array := source_type_syntax(source_type_id('[2]int[ 3 ]'))
	assert array.last_bracket_prefix == source_type_id('[2]int')
	assert array.last_bracket_length == source_type_id('3')
	assert array.trailing_bracket_closed
	assert array.trailing_bracket_prefix == source_type_id('[2]int')
	assert source_type_syntax(source_type_id('[2]')).trailing_bracket_closed
	assert source_type_syntax(source_type_id('[2]')).trailing_bracket_prefix == 0
	assert !source_type_syntax(source_type_id('int[2]junk')).trailing_bracket_closed
	assert source_type_syntax(source_type_id('][]T')).array_chars_base == source_type_id('T')
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
	assert syntax.named_variant
	assert source_type_syntax(source_type_id('3Name')).symbol_parts() == [
		source_type_id('3'),
		source_type_id('Name'),
	]
	assert source_type_syntax(source_type_id('3Name')).symbol_is_name() == [false, true]
}

fn test_source_type_syntax_cached_query_variants_retain_legacy_grammar() {
	assert source_type_syntax(source_type_id('main.Box[main.T, domain.T, main.main.U]')).main_unlocked == source_type_id('Box[T, domain.T, main.U]')
	assert source_type_syntax(source_type_id('main.Box[main.T]')).main_removed == source_type_id('Box[T]')
	encoded := source_type_syntax(source_type_id('_123int'))
	assert encoded.encoded_fixed_length == source_type_id('123')
	assert encoded.encoded_fixed_base == source_type_id('int')
	assert source_type_syntax(source_type_id('mut T')).wrapper_prefix == source_type_id('mut ')
	assert source_type_syntax(source_type_id('mut T')).wrapper_base == source_type_id('T')
	assert source_type_syntax(source_type_id('atomic T')).wrapper_prefix == 0
	assert source_type_syntax(source_type_id('T[0x10]')).checker_fixed_array_may_be
	assert !source_type_syntax(source_type_id('T[0x10]')).fixed_array_type
	assert source_type_syntax(source_type_id('0x10')).fixed_length
	assert source_type_syntax(source_type_id('map[Alias')).bracket_source_inner == source_type_id('Alias')
	assert source_type_syntax(source_type_id('map[T]fn ()')).map_function_fragment
	assert source_type_syntax(source_type_id('X[3]]')).suffix_bracket_length == source_type_id('3]')
	assert source_type_syntax(source_type_id('X[3]]tail')).last_bracket_span_inner == source_type_id('3]')
	assert source_type_syntax(source_type_id('&?pkg.Name')).qualified_modifiers == source_type_id('&?')
	assert source_type_syntax(source_type_id('Name\t')).plain_alias_target
	assert !source_type_syntax(source_type_id('Name ')).plain_alias_target
	assert source_type_syntax(source_type_id('Array_fixed_u8_12')).c_fixed_typedef_element == source_type_id('u8')
	assert source_type_syntax(source_type_id('Array_fixed_u8_12')).c_fixed_typedef_length == source_type_id('12')
}

fn test_source_type_syntax_receiver_and_parameter_queries_preserve_raw_parts() {
	receiver := source_type_syntax(source_type_id('Base[T]junk[U]'))
	assert receiver.receiver_app_valid
	assert receiver.receiver_app_base == source_type_id('Base')
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
	assert parameter.parameter_named
	assert parameter.parameter_is_mut
	assert parameter.parameter_name == source_type_id('value')
	assert parameter.legacy_parameter_type == source_type_id('[]T')
	assert parameter.declaration_parameter_payload == source_type_id('[]T')
	assert source_type_syntax(source_type_id('fn (, T, )')).substitution_function_params() == [
		u32(0),
		source_type_id(' T'),
		source_type_id(' '),
	]
	assert source_type_syntax(source_type_id('fn () \tT')).function_space_result == source_type_id('\tT')
	assert source_type_syntax(source_type_id('fn () \tT ')).checker_parse_function_result == source_type_id('\tT ')
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
	assert source_type_syntax(source_type_id('&mut Box[T]')).constraint_receiver_base == source_type_id('Box[T]')
	assert source_type_syntax(source_type_id('Interface [ T ]')).space_removed == source_type_id('Interface[T]')
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
	assert source_type_syntax(source_type_id('__v_ result _T')).cgen_decl_unusable
	assert source_type_syntax(source_type_id('[3]void')).cgen_decl_unusable
	assert !source_type_syntax(source_type_id('Result')).cgen_decl_unusable
	assert source_type_syntax(source_type_id('SomeResult')).result_suffix
}

fn test_source_type_syntax_cached_encoding_and_array_views() {
	assert source_type_syntax(source_type_id('Name***')).c_pointer_depth == 3
	assert source_type_syntax(source_type_id('Name***')).c_pointer_raw_base == source_type_id('Name')
	assert source_type_syntax(source_type_id('voidptrvoidptr')).voidptr_parts() == [
		u32(0),
		u32(0),
		u32(0),
	]
	assert source_type_syntax(source_type_id('__vf_thread_kT')).thread_value_payload == source_type_id('T')
	assert source_type_syntax(source_type_id('_A__B___C_')).flattened_receiver_parts() == [
		u32(0),
		source_type_id('A__B__'),
		source_type_id('C'),
		u32(0),
	]
	assert source_type_syntax(source_type_id('A__B')).doubleunderscore_dotted == source_type_id('A.B')
	assert source_type_syntax(source_type_id('Foo???')).question_trimmed == source_type_id('Foo')
	assert source_type_syntax(source_type_id('42')).decimal_value == 42
	assert source_type_syntax(source_type_id('4x')).decimal_value == 0
	assert source_type_syntax(source_type_id('mod.map[K]V.[]T')).receiver_collection_id == source_type_id('[]T')
	assert source_type_syntax(source_type_id('[2]int[3]junk')).backward_bracket_prefix == source_type_id('[2]int')
	assert source_type_syntax(source_type_id('[2]int[3]junk')).backward_bracket_inner == source_type_id('3]jun')
}

fn test_source_type_syntax_c_pointer_count_includes_interior_stars() {
	spellings := ['', 'T', '*', 'T***', 'T*U*', 'fn(*T) *U', '  *T*  ', '*[2*3]T*']
	counts := [0, 0, 1, 3, 2, 2, 2, 3]
	for i, spelling in spellings {
		syntax := source_type_syntax(source_type_id(spelling))
		assert syntax.c_pointer_count == counts[i]
	}
	assert source_type_syntax(source_type_id('T*U*')).c_pointer_depth == 1
	assert source_type_syntax(source_type_id('fn(*T) *U')).c_pointer_depth == 0
}

fn test_source_type_syntax_c_abi_prefixes_use_raw_spelling() {
	for spelling in ['__v_result', '__v_result_T', '__v_resultsuffix'] {
		assert source_type_syntax(source_type_id(spelling)).c_result_prefix
	}
	for spelling in ['', '__v_resul', ' __v_result_T', 'mod.__v_result_T'] {
		assert !source_type_syntax(source_type_id(spelling)).c_result_prefix
	}
	for spelling in ['__v_option_', '__v_option_T', '__v_option_[T'] {
		assert source_type_syntax(source_type_id(spelling)).c_option_prefix
	}
	for spelling in ['__v_option', ' __v_option_T', 'mod.__v_option_T'] {
		assert !source_type_syntax(source_type_id(spelling)).c_option_prefix
	}
	for spelling in ['multi_return_', 'multi_return_T', 'multi_return_[T'] {
		assert source_type_syntax(source_type_id(spelling)).c_multi_return_prefix
	}
	for spelling in ['multi_return', ' multi_return_T', 'mod.multi_return_T'] {
		assert !source_type_syntax(source_type_id(spelling)).c_multi_return_prefix
	}
	for spelling in ['map_', 'map_int', 'map_[T'] {
		assert source_type_syntax(source_type_id(spelling)).c_lower_map_prefix
	}
	for spelling in ['', 'map', 'map__', 'map__int', ' map_int', 'Map_int', 'mod.map_int'] {
		assert !source_type_syntax(source_type_id(spelling)).c_lower_map_prefix
	}
}

fn test_source_type_syntax_inferred_annotations_and_markers() {
	syntax := source_type_syntax(source_type_id('A typeof(__expr_17) B typeof(__expr_42) C'))
	assert syntax.inferred_parts() == [source_type_id('A '), source_type_id(' B '),
		source_type_id(' C')]
	assert syntax.inferred_annotations() == [source_type_id('typeof(__expr_17)'),
		source_type_id('typeof(__expr_42)')]
	assert syntax.inferred_expr_ids() == [u32(17), u32(42)]
	assert source_type_syntax(source_type_id('__expr_17')).expr_id == 17
	assert source_type_syntax(source_type_id('Box[?!T]')).nested_option_result_marker == 4
	assert source_type_syntax(source_type_id('Box[?T]')).nested_option_result_marker == -1
	assert source_type_syntax(source_type_id('typeof(x)')).typeof_prefix
	assert !source_type_syntax(source_type_id('Box[typeof(x)]')).typeof_prefix
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
	assert source_type_syntax(source_type_id('pkg.Name2')).encoded_alnum_dotted
	assert !source_type_syntax(source_type_id('pkg_Name')).encoded_alnum_dotted
	assert source_type_syntax(source_type_id('ptrpkg.Name')).ptr_name_base == source_type_id('pkg.Name')
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
	assert syntax.contains_capital
	assert syntax.markused_generic_components() == [source_type_id('pkg.Name'), source_type_id('T'),
		source_type_id('thing@Item'), source_type_id('U'), source_type_id('V')]
	assert source_type_syntax(source_type_id('123T')).vls_generic_words() == [source_type_id('T')]
	assert source_type_syntax(source_type_id('123Type')).vls_generic_words().len == 0
	assert source_type_syntax(source_type_id('main.@Name')).main_at_base == source_type_id('Name')
	assert source_type_syntax(source_type_id('main.@')).main_at_base == 0
	assert source_type_syntax(source_type_id('main.Name')).main_at_base == source_type_id('main.Name')
}

fn test_source_type_syntax_incremental_generic_application() {
	for text in ['Box[int]', '[]Box[int]', 'Box[', '3[', '_map[', 'map9[', 'pkg.Box[', 'map[Box[int]]',
		'map[map[T]Box['] {
		id := source_type_id(text)
		assert source_type_syntax(id).incremental_generic_application, text
		assert source_type_syntax(id).incremental_generic_application, text
	}
	for text in ['', 'Box', '123T', '[]int', '[3]int', 'map[string]int', 'pkg.map[int]', 'Box [T]',
		'.[]', 'map[', 'map[map[T]]', 'Box\t[T]', 'é[T]'] {
		id := source_type_id(text)
		assert !source_type_syntax(id).incremental_generic_application, text
		assert !source_type_syntax(id).incremental_generic_application, text
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
	assert encoded.fastc_fixed_array_length_valid
	assert encoded.fastc_fixed_array_length == source_type_id('0xF')
	assert source_type_syntax(source_type_id('FixedArray_EXPRHEXf_FASTC_ARRAY_OF_u8')).fastc_fixed_array_length == source_type_id([u8(15)].bytestr())
	assert !source_type_syntax(source_type_id('FixedArray_EXPRHEXg_FASTC_ARRAY_OF_u8')).fastc_fixed_array_length_valid
	empty := source_type_syntax(source_type_id('FixedArray_EXPRHEX_FASTC_ARRAY_OF_u8'))
	assert empty.fastc_fixed_array_length_valid
	assert empty.fastc_fixed_array_length == 0
	assert source_type_syntax(source_type_id('T[U]junk[V]')).receiver_app_inner == source_type_id('U]junk[V')
	assert source_type_syntax(source_type_id('T[U[V]')).first_bracket_source_inner == source_type_id('U[V')
	assert source_type_syntax(source_type_id('SystemError')).system_error_suffix
}

fn test_source_type_syntax_legacy_mut_character_set_trim() {
	assert source_type_syntax(source_type_id('mut Thing')).mut_chars_base == source_type_id('Thing')
	assert source_type_syntax(source_type_id('tuple')).mut_chars_base == source_type_id('ple')
	assert source_type_syntax(source_type_id('u8')).mut_chars_base == source_type_id('8')
	assert source_type_syntax(source_type_id('mut')).mut_chars_base == 0
	assert source_type_syntax(source_type_id('Thing')).mut_chars_base == source_type_id('Thing')
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
	assert syntax.top_level_dot_valid
	assert syntax.top_level_dot_prefix == source_type_id(']Box[pkg.T]')
	assert syntax.top_level_dot_suffix == source_type_id('Leaf')
	assert !source_type_syntax(source_type_id('Box[pkg.T]')).top_level_dot_valid
	fn_decl := source_type_syntax(source_type_id('fn(fn(T)) ?&T'))
	assert fn_decl.parser_fn_decl_valid
	assert fn_decl.parser_fn_decl_params == source_type_id('fn(T')
	assert fn_decl.parser_fn_decl_result == source_type_id(') ?&T')
	assert !source_type_syntax(source_type_id('fn() T')).parser_fn_decl_valid
	assert source_type_syntax(source_type_id('fn(T) ?&T')).parser_fn_decl_result == source_type_id('T')
}

fn test_source_type_syntax_offsets_and_remaining_query_views() {
	syntax := source_type_syntax(source_type_id('OtherThing Thing Thing pkg.T T'))
	assert syntax.symbols() == [source_type_id('OtherThing'), source_type_id('Thing'),
		source_type_id('Thing'), source_type_id('pkg.T'), source_type_id('T')]
	assert syntax.symbols_first_offsets() == [u32(0), 5, 5, 23, 5]
	assert source_type_syntax(source_type_id('fn (T)')).fastc_composite_part == source_type_id('fn_(T)')
	assert source_type_syntax(source_type_id('&pkg.T*')).fastc_composite_part == source_type_id('&pkg__T_ptr')
	assert source_type_syntax(source_type_id('T?')).contains_question
	assert source_type_syntax(source_type_id('[]chan T')).channel_fragment
	assert source_type_syntax(source_type_id('void (*)()')).c_function_pointer_hole
	assert source_type_syntax(source_type_id('A | fn (B | C) | {D | E} |')).union_parts() == [
		source_type_id('A'),
		source_type_id('fn (B | C)'),
		source_type_id('{D | E}'),
	]
	variant := source_type_syntax(source_type_id('pkg.Box[inner.T].Leaf'))
	assert variant.named_variant_dot_prefix == source_type_id('pkg.Box[inner.T]')
	assert variant.named_variant_dot_suffix == source_type_id('Leaf')
	assert !source_type_syntax(source_type_id(']pkg.Leaf')).named_variant_dot_valid
	assert source_type_syntax(source_type_id('Thing_T__method')).mangled_open_generic_marker
	assert !source_type_syntax(source_type_id('Thing_T_method')).mangled_open_generic_marker
}

fn test_source_type_syntax_integer_conversion_preserves_raw_int() {
	for raw in ['', '0', '42', '4x', '1_000', '0xff', '0b101', '0o17', '-42', '+17', ' 42 ', '\t-17\n',
		'-0xff', 'not-a-number', '2147483648', '9223372036854775807', '0x', '--1', '3.14', '12tail'] {
		id := source_type_id(raw)
		syntax := source_type_syntax(id)
		assert syntax.integer_value == raw.int()
		assert source_type_syntax(id) == syntax
		assert source_type_syntax(id).integer_value == raw.int()
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
			assert syntax.thread_value_type_valid
			assert syntax.thread_value_type == source_type_id(expected[i])
		}
	}
	for raw in ['', 'void', '__vf_thread_other', '__vf_thread_k_', '__vf_thread_k_0', '__vf_thread_k_0F',
		'__vf_thread_k_F0', '__vf_thread_k_gg'] {
		assert !source_type_syntax(source_type_id(raw)).thread_value_type_valid
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
			assert syntax.named_variant == raw.contains(named_variant_marker)
			if owner, name := decode_named_variant_type_name(raw) {
				assert syntax.named_variant_valid
				assert syntax.named_variant_owner == source_type_id(owner)
				assert syntax.named_variant_name == source_type_id(name)
			} else {
				assert !syntax.named_variant_valid
				assert syntax.named_variant_owner == 0
				assert syntax.named_variant_name == 0
			}
		}
	}
	variant := source_type_syntax(source_type_id('&&pkg.Sum@variant@Some_2[U]tail'))
	assert variant.named_variant_owner == source_type_id('pkg.Sum[U]tail')
	assert variant.named_variant_name == source_type_id('Some_2')
	empty_name := source_type_syntax(source_type_id('Sum@variant@[int]'))
	assert empty_name.named_variant_valid
	assert empty_name.named_variant_owner == source_type_id('Sum[int]')
	assert empty_name.named_variant_name == 0
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
		assert syntax.unqualified == source_type_id(raw.all_after_last('.'))
		assert syntax.qualifier == source_type_id(raw.all_before_last('.'))
		assert syntax.words() == source_type_syntax_tokens(raw, false)
		assert syntax.legacy_generic_words() == source_type_syntax_legacy_words(raw)
		assert syntax.dotted_parts() == parts
		assert syntax.first_dot_prefix == source_type_id(raw.all_before('.'))
		assert syntax.first_dot_suffix == source_type_id(raw.all_after('.'))
		assert syntax.qualifier_prefixes() == prefixes
		assert syntax.qualifier_suffixes() == suffixes
		assert syntax.main_base == if raw.starts_with('main.') {
			source_type_id(raw[5..])
		} else {
			u32(0)
		}
		assert syntax.builtin_base == if raw.starts_with('builtin.') {
			source_type_id(raw[8..])
		} else {
			u32(0)
		}
		assert syntax.c_qualified == raw.starts_with('C.')
		assert syntax.builtin_prefix == raw.starts_with('builtin.')
		assert syntax.main_fragment == raw.contains('main.')
		assert syntax.main_unlocked == source_type_syntax_main_unlocked(id, raw)
		assert syntax.main_removed == source_type_id(raw.replace('main.', ''))
		assert syntax.canonical_dotted_name
		assert syntax.qualified_name_chars
		assert !syntax.plain_type_name
		assert !syntax.lowercase_value_ident
		assert !syntax.cgen_generic_placeholder
		parallel_fn, parallel_short := source_type_syntax_parallel_preseed(raw)
		assert syntax.parallel_preseed_fn == parallel_fn
		assert syntax.parallel_preseed_short == parallel_short
		assert syntax.field_owner_module == source_type_id(if raw.contains('.C.') {
			raw.all_before('.C.')
		} else if raw.contains('.JS.') {
			raw.all_before('.JS.')
		} else {
			raw.all_before_last('.')
		})
		assert syntax.vls_generic_words() == source_type_syntax_vls_generic_words(raw)
		assert syntax.fastc_composite_part == source_type_id(raw.replace('.', '__'))
		assert syntax.typ_suffix == raw.ends_with('.typ')
		assert syntax.typ_suffix_base == if raw.ends_with('.typ') {
			syntax.qualifier
		} else {
			u32(0)
		}
		assert syntax.named_variant_dot_valid
		assert syntax.named_variant_dot_prefix == syntax.qualifier
		assert syntax.named_variant_dot_suffix == syntax.unqualified
		assert syntax.top_level_dot_valid
		assert syntax.top_level_dot_prefix == syntax.first_dot_prefix
		assert syntax.top_level_dot_suffix == syntax.first_dot_suffix
		assert syntax.symbols() == [id]
		assert syntax.symbol_parts() == [id]
		assert syntax.symbol_is_name() == [true]
		assert syntax.symbols_first_offsets() == [u32(0)]
		assert syntax.parameter_type == id
		assert syntax.legacy_parameter_type == id
		assert syntax.declaration_parameter_payload == id
		assert syntax.kind == .name
		assert syntax.base == 0
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
		assert syntax.unqualified == source_type_id(raw.all_after_last('.'))
		assert syntax.qualifier == source_type_id(raw.all_before_last('.'))
		assert syntax.words() == source_type_syntax_tokens(raw, false)
		assert syntax.legacy_generic_words() == source_type_syntax_legacy_words(raw)
		assert syntax.vls_generic_words() == source_type_syntax_vls_generic_words(raw)
	}
	assert source_type_syntax(source_type_id('.Foo')).dots_base == source_type_id('Foo')
	assert !source_type_syntax(source_type_id('2Foo.Bar')).canonical_dotted_name
	assert !source_type_syntax(source_type_id(' pkg.Foo ')).named_type
	assert source_type_syntax(source_type_id('pkg.Foo[T]')).generic_app_valid
}

fn test_source_type_syntax_reused_splits_keep_padding_and_empty_parameters() {
	for raw in ['Box[T]', ' Box[T] ', '  Box[T]', 'fn (T)', 'Box[T]suffix'] {
		syntax := source_type_syntax(source_type_id(raw))
		base, args, valid := source_type_syntax_cgen_application_raw(source_type_query_trim(raw))
		raw_base, raw_args, raw_valid := source_type_syntax_cgen_application_raw(raw)
		assert syntax.cgen_app_base == base
		assert syntax.cgen_app_args() == args
		assert syntax.cgen_app_valid == valid
		assert syntax.cgen_raw_app_base == raw_base
		assert syntax.cgen_raw_app_args() == raw_args
		assert syntax.cgen_raw_app_valid == raw_valid
	}
	assert source_type_syntax(source_type_id('  Box[T]')).cgen_raw_app_base == source_type_id('  Box')
	assert source_type_syntax(source_type_id('  Box[T]')).cgen_app_base == source_type_id('Box')
	function := source_type_syntax(source_type_id('fn (,T,) U'))
	assert function.function_params() == [source_type_id('T')]
	assert function.checker_function_params() == [u32(0), source_type_id('T')]
	assert function.cgen_function_params() == [u32(0), source_type_id('T'), u32(0)]
	assert source_type_syntax(source_type_id('fn( )')).checker_function_params() == [u32(0)]
	assert !source_type_syntax(source_type_id('fn( )')).checker_parse_function_has_params
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
