module flat

struct SourceTypeQueryCase {
	flag     SourceTypeQuery
	positive []string
	negative []string
}

fn source_type_query_cases() []SourceTypeQueryCase {
	// These expectations preserve the original consumers' byte grammar, including
	// permissive syntax screens that are not full type parsers.
	return [
		SourceTypeQueryCase{
			flag:     .complete
			positive: ['', 'int', 'fn([2]int) map[K]V', '([)]', '[(])', '()[]']
			negative: ['(', ')', '[', ']', ')(', '][', '([)', '[[]', '(()]', '[])']
		},
		SourceTypeQueryCase{
			flag:     .channel
			positive: ['chan', 'chan int', ' chan int \n', '&chan int', '& mut & chan int',
				'mut & mut &chan int', '\tmut \n &\rmut \t & chan int\r', 'mut chan', '&\vmut \f&\tchan\n']
			negative: ['', ' ', '&', 'mut ', 'chan\tint', 'channel int', 'mut\tchan int',
				'&mut\tchan int', 'shared chan int', '?chan int', '!chan int', 'Chan int']
		},
		SourceTypeQueryCase{
			flag:     .shared_modifier
			positive: ['shared T', '[]shared T', 'map[K]shared T', '&shared T', '!shared T', '\tshared T',
				'x.shared T []shared T', 'ashared shared T']
			negative: ['', 'shared', 'Shared T', 'shared\tT', 'xshared T', 'x_shared T', 'x.shared T',
				'9shared T', 'sharedish T']
		},
		SourceTypeQueryCase{
			flag:     .thread_prefix
			positive: ['thread', 'thread int', 'threaded', 'thread_name', 'threads', 'thread[T]']
			negative: ['', ' thread', '\tthread int', '&thread int', 'Thread', 'pkg.thread']
		},
		SourceTypeQueryCase{
			flag:     .generic_token
			positive: ['T', 'C', 'Z', 'pkg.T', 'map[K]V', '[T]int', '(T,U)', ' T ', 'éT']
			negative: ['', 'Thing', 'Type', 'T_', '_T', 'T1', '1T', 'TT', 'generic', 'pkg.Type']
		},
		SourceTypeQueryCase{
			flag:     .generic_word
			positive: ['generic', 'nongeneric', 'genericity', 'Genericgeneric', '[]generic']
			negative: ['', 'Generic', 'GENERIC', 'gen eric', 'T', 'generi']
		},
		SourceTypeQueryCase{
			flag:     .generic_name
			positive: ['T', 'U', 'V', 'K', 'pkg.T', '.U', 'foo..V', 'T.U']
			negative: ['', 'A', 'C', 'Z', 'TT', ' T', 'T ', 'pkg. T', ' pkg.T ', 'T.', '[]T', '&T']
		},
		SourceTypeQueryCase{
			flag:     .generic_fn_name
			positive: ['A', 'C', 'Z', 'pkg.A', 'foo..Z', '.K', 'pkg .T', ' T ', '\tT\n', ' pkg. T',
				'pkg. T ', ' pkg. T ', '\v\fpkg.\tZ\r\n']
			negative: ['', ' ', '\t\r\n', 'AA', 'pkg.AA', 'pkg. T', 'pkg. Z', '.', 'pkg.', '&T',
				'[]T', 'T\nX', 'pkg.t', 'é']
		},
		SourceTypeQueryCase{
			flag:     .struct_candidate
			positive: ['Foo', 'pkg.Foo', '&Foo', '&&Foo', '&usize', '&&usize', ' int', '& int',
				'chan', '&chan', 'mut int', 'shared int']
			negative: ['', '&', '&&', 'int', 'usize', '[]Foo', 'map[K]V', '[N]Foo', 'fn()', 'fn ()',
				'chan Foo', '?Foo', '!Foo', '&int', '&&int', '&[]Foo', '&map[K]V', '&[2]Foo', '&fn()',
				'&fn ()', '&chan Foo', '&?Foo', '&!Foo']
		},
		SourceTypeQueryCase{
			flag:     .fixed_array_prefix
			positive: ['[1]int', '[N]T', '[2]', '[[1]int]Foo', '[?]', '[0]map[K]V', '[)]']
			negative: ['', '[', '[]int', '[]', 'int[2]', '&[2]int', ' [2]int', 'map[K]V', '][2]']
		},
		SourceTypeQueryCase{
			flag:     .fixed_array_postfix
			positive: ['int[2]', 'int[N]', 'Foo[T]', 'int[]', '&[2]', 'int[]]']
			negative: ['', ']', 'int', 'int[2] ', 'map[K]', '[2]', '[]', '[]int[2]', '&int', 'int[2',
				'int]']
		},
		SourceTypeQueryCase{
			flag:     .c_pointer_suffix
			positive: ['*', 'int*', 'int**', ' int*', 'Array_int*', '&int*']
			negative: ['', 'int', '&int', 'int* ', 'voidptr', 'byteptr', 'charptr', 'int*[2]']
		},
		SourceTypeQueryCase{
			flag:     .unknown_fragment
			positive: ['unknown', '[]unknown', 'unknown_type', 'pkg.unknown', 'notunknown', '?unknown']
			negative: ['', 'Unknown', 'UNKNOWN', 'unk nown', 'typeof(x)', 'array', 'map', 'struct',
				'Option[int]', '__v_option[int]', 'Result[int]']
		},
		SourceTypeQueryCase{
			flag:     .c_prefix
			positive: ['C.Foo', 'C.', 'C.Foo[T]']
			negative: [' C.Foo', 'c.Foo', 'pkg.C.Foo', '&C.Foo']
		},
		SourceTypeQueryCase{
			flag:     .main_prefix
			positive: ['main.Foo', 'main.']
			negative: [' main.Foo', 'pkg.main.Foo', 'mainly.Foo']
		},
		SourceTypeQueryCase{
			flag:     .fn_ptr_prefix
			positive: ['fn_ptr:', 'fn_ptr:fn()']
			negative: [' fn_ptr:', 'pkg.fn_ptr:', 'fn_ptr']
		},
		SourceTypeQueryCase{
			flag:     .c_array_prefix
			positive: ['Array_', 'Array_int*']
			negative: [' Array_int', '&Array_int', 'FixedArray_int']
		},
		SourceTypeQueryCase{
			flag:     .c_fixed_array_prefix
			positive: ['FixedArray_', 'FixedArray_int_2']
			negative: [' FixedArray_int', 'Array_int', 'pkg.FixedArray_int']
		},
		SourceTypeQueryCase{
			flag:     .c_map_prefix
			positive: ['Map_', 'Map_string_int']
			negative: [' Map_int', 'map[int]int', 'pkg.Map_int']
		},
		SourceTypeQueryCase{
			flag:     .thread_suffix
			positive: ['pkg.thread', '.thread', 'some.thread.thread']
			negative: ['thread', 'pkg.thread ', 'pkg.thread[int]']
		},
		SourceTypeQueryCase{
			flag:     .ctx_suffix
			positive: ['_Ctx', 'Foo_Ctx']
			negative: ['Foo_Ctx ', 'Foo_Ctx*', 'Foo_ctx']
		},
		SourceTypeQueryCase{
			flag:     .c_double_pointer_suffix
			positive: ['**', 'int**', 'int***']
			negative: ['int*', 'int** ', '**int']
		},
		SourceTypeQueryCase{
			flag:     .c_pointer_infix
			positive: ['*', 'int*', 'int*[2]']
			negative: ['', '&int', 'voidptr']
		},
		SourceTypeQueryCase{
			flag:     .map_arc_key
			positive: ['Arc[T]', 'pkg.Arc[T]', 'Map_Arc_Foo', 'Arc_']
			negative: ['arc[T]', 'Arc', 'Arc [T]', 'ARC_']
		},
		SourceTypeQueryCase{
			flag:     .inferred_witness
			positive: ['typeof(__expr_1)', '[]typeof(__expr_2)']
			negative: ['typeof (__expr_1)', 'typeof(x)', 'typeof(__exp_1)']
		},
		SourceTypeQueryCase{
			flag:     .c_thread_prefix
			positive: ['__vf_thread_', '__vf_thread_int']
			negative: [' __vf_thread_int', 'thread int', 'pkg.__vf_thread_int']
		},
		SourceTypeQueryCase{
			flag:     .js_prefix
			positive: ['JS.Foo', 'JS.']
			negative: [' JS.Foo', 'js.Foo', 'pkg.JS.Foo']
		},
		SourceTypeQueryCase{
			flag:     .pointer_option_ref
			positive: ['&?&', '&?&T']
			negative: [' &?&T', '?&T', '&&?&T']
		},
		SourceTypeQueryCase{
			flag:     .shared_array
			positive: ['[]shared T', '[]shared ']
			negative: [' []shared T', '[]shared\tT', '&[]shared T']
		},
		SourceTypeQueryCase{
			flag:     .anon_struct_prefix
			positive: ['AnonStruct_', 'AnonStruct_1']
			negative: ['pkg.AnonStruct_1', ' AnonStruct_1', 'AnonStruct']
		},
		SourceTypeQueryCase{
			flag:     .anon_union_prefix
			positive: ['AnonUnion_', 'AnonUnion_1']
			negative: ['pkg.AnonUnion_1', ' AnonUnion_1', 'AnonUnion']
		},
		SourceTypeQueryCase{
			flag:     .bracket_suffix
			positive: [']', 'Thing[T]', 'Thing[]]']
			negative: ['', '[', 'Thing[T] ', '[2]int']
		},
	]
}

fn test_source_type_queries_preserve_original_spelling_screens() {
	for fixture in source_type_query_cases() {
		for text in fixture.positive {
			assert source_type_query(source_type_id(text)).has(fixture.flag), '${fixture.flag}: ${text}'
		}
		for text in fixture.negative {
			assert !source_type_query(source_type_id(text)).has(fixture.flag), '${fixture.flag}: ${text}'
		}
	}
}

fn test_source_type_query_empty_identity_and_flag_storage() {
	assert source_type_id('') == 0
	assert source_type_query(0) == SourceTypeQuery.complete
	assert sizeof(SourceTypeShape) == sizeof(u32)
	assert sizeof(SourceTypeQuery) == sizeof(u32)
	for code in `A` .. `Z` + 1 {
		text := u8(code).ascii_str()
		query := source_type_query(source_type_id(text))
		assert query.has(.generic_token)
		assert query.has(.generic_fn_name)
		assert query.has(.generic_name) == (text in ['T', 'U', 'V', 'K'])
	}
}

fn test_source_type_queries_do_not_retain_mutable_caller_spelling() {
	mut bytes := 'chan int'.bytes()
	text := unsafe { tos(bytes.data, bytes.len) }
	id := source_type_id(text)
	query := source_type_query(id)
	assert query.has(.channel)
	assert !query.has(.struct_candidate)
	bytes[0] = `C`
	changed_id := source_type_id(text)
	assert changed_id != id
	assert source_type_name(id) == 'chan int'
	assert source_type_query(id) == query
	assert !source_type_query(changed_id).has(.channel)
	assert source_type_query(changed_id).has(.struct_candidate)
	assert source_type_id('chan int'.clone()) == id
}

fn intern_source_type_query_fixture(worker int) []u32 {
	mut shared_ids := []u32{cap: 128}
	for i in 0 .. 128 {
		// Unique insertions race with publication of the same immutable query.
		unique_id := source_type_id('source_type_queries.Worker_${worker}_${i}[T]')
		shared_id := source_type_id('source_type_queries.Shared_${i}[T]')
		for id in [unique_id, shared_id] {
			query := source_type_query(id)
			assert query.has(.complete)
			assert query.has(.generic_token)
			assert query.has(.struct_candidate)
			assert query.has(.fixed_array_postfix)
			assert !query.has(.generic_name)
			assert !query.has(.generic_fn_name)
		}
		shared_ids << shared_id
	}
	return shared_ids
}

fn test_source_type_query_publication_keeps_shared_identity_across_workers() {
	mut threads := []thread []u32{}
	for worker in 0 .. 4 {
		threads << spawn intern_source_type_query_fixture(worker)
	}
	mut expected := []u32{}
	for worker, handle in threads {
		ids := handle.wait()
		if worker == 0 {
			expected = ids.clone()
		} else {
			assert ids == expected
		}
		for id in ids {
			assert source_type_query(id).has(.generic_token)
			assert source_type_query(id).has(.fixed_array_postfix)
		}
	}
}
