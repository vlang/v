module types

import os
import v.flat
import v.parser
import v.pref

fn comptime_const_checker(source string) &TypeChecker {
	path := os.join_path(os.vtmp_dir(), 'comptime_const_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	return tc
}

fn test_comptime_const_scalar_is_available_before_body_checking() {
	tc := comptime_const_checker('@[comptime]\nfn size(a int) int { mut n := 0 for i in 0 .. a { n += i } return n }\nconst n = size(5)\nfn main() { _ = [n]u8{} }')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.const_int_value('n', [])? == 10
	assert tc.comptime_const_values['n']!.integer == 10
}

fn test_comptime_const_reports_executed_failures_at_declaration() {
	for body, expected in {
		'return runtime_value()':                  'requires @[comptime]'
		'panic("bad")':                            'unsupported call'
		'for {}':                                  'steps'
		'return compute(1)':                       'recursion limit'
		'return a / (a - a)':                      'division by zero'
		'values := [1]int{} return values[a + 1]': 'array index out of bounds'
		'return bad':                              'cycle in constant'
		'return int(sizeof(string))':              'unsupported target-dependent sizeof'
	} {
		tc := comptime_const_checker('fn runtime_value() int { return 3 }\n@[comptime]\nfn compute(a int) int { ${body} }\nconst bad = compute(1)\nfn main() { _ = bad }')
		errors := tc.errors.filter(it.msg.contains('cannot evaluate const `bad`'))
		assert errors.len == 1, tc.errors.str()
		assert errors[0].msg.contains(expected), errors[0].msg
		assert errors[0].msg.contains('in `compute`'), errors[0].msg
	}
}

fn test_comptime_const_has_bounded_storage_and_rejects_dynamic_values() {
	for item in [
		['return [100001]u8{}', '[100001]u8', 'storage limit'],
		['return [1, 2]', '[]int', 'only fixed arrays'],
		['return 1.0', 'f64', 'unsupported operation'],
		['return [1]u128{}', '[1]u128', '128-bit integers'],
	] {
		tc := comptime_const_checker('@[comptime]\nfn compute() ${item[1]} { ${item[0]} }\nconst bad = compute()\nfn main() { _ = bad }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `bad`') && it.msg.contains(item[2])), tc.errors.str()
	}
}

fn test_comptime_const_target_int_width() {
	defer { set_platform_int_bits(64) }
	for bits in [32, 64] {
		set_platform_int_bits(bits)
		tc := comptime_const_checker('@[comptime]\nfn target_size() int { return int(sizeof(int)) }\nconst n = target_size()\nfn main() { _ = [n]u8{} }')
		assert tc.errors.len == 0, tc.errors.str()
		assert tc.const_int_value('n', [])? == bits / 8
	}
}

fn test_comptime_const_does_not_evaluate_unmarked_runtime_calls() {
	tc := comptime_const_checker('fn compute() int { return 4 }\nconst n = compute()\nfn main() { _ = n }')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values.len == 0
	assert tc.const_int_value('n', []) == none
}

fn test_comptime_const_rejects_union_storage() {
	tc := comptime_const_checker('union Storage { n int }\n@[comptime]\nfn compute() Storage { return Storage{n: 1} }\nconst bad = compute()\nfn main() { _ = bad }')
	assert tc.errors.any(it.msg.contains('cannot evaluate const `bad`') && it.msg.contains('unions are not supported')), tc.errors.str()
}

fn test_comptime_const_inferred_fixed_array_type_survives_return() {
	tc := comptime_const_checker('struct Flags { bits [2]bool }\n@[comptime]\nfn make_flags() Flags { return Flags{bits: [true, false]!} }\nconst flags = make_flags()\nfn main() { _ = flags }')
	assert tc.errors.len == 0, tc.errors.str()
	value := tc.comptime_const_values['flags']!
	bits := value.fields['bits']!
	assert bits.typ.source_type_id() == flat.source_type_id('[2]bool')
	assert bits.elements[0].integer == 1
	assert bits.elements[1].integer == 0
}

fn test_comptime_const_values_survive_parent_index_refresh() {
	mut tc := comptime_const_checker('@[comptime]\nfn compute() int { return 17 }\nconst n = compute()\nfn main() { _ = n }')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values['n']!.integer == 17
	tc.refresh_direct_parent_index(tc.a)
	assert tc.comptime_const_values['n']!.integer == 17
	tc.collect(tc.a)
	assert tc.comptime_const_values.len == 0
}

fn test_comptime_const_platform_sized_arithmetic_wraps_at_target_width() {
	defer { set_platform_int_bits(64) }
	for bits in [32, 64] {
		set_platform_int_bits(bits)
		tc := comptime_const_checker('@[comptime]\nfn compute(a uint, b uint) uint { return (a + b) / 2 }\nconst n = compute(4294967295, 1)\nfn main() { _ = n }')
		assert tc.errors.len == 0, tc.errors.str()
		assert tc.comptime_const_values['n']!.integer == if bits == 32 {
			i64(0)
		} else {
			i64(2147483648)
		}
	}
}

fn test_comptime_const_rune_arithmetic_uses_32_bits() {
	tc := comptime_const_checker('@[comptime]\nfn compute(a rune, b rune) rune { return (a + b) / 2 }\nconst n = compute(rune(4294967295), rune(1))\nfn main() { _ = n }')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values['n']!.integer == 0
}

fn test_comptime_const_narrow_arithmetic_comparison() {
	tc := comptime_const_checker('@[comptime]\nfn compute(a u8, b u8) bool { return a + b == u8(4) }\nconst n = compute(250, 10)\nfn main() { _ = n }')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values['n']!.integer == 1
}

fn test_comptime_const_mixed_signed_unsigned_comparison() {
	for comparison, expected in {
		'a < b':  i64(0)
		'a > b':  i64(1)
		'a <= b': i64(0)
		'a >= b': i64(1)
	} {
		tc := comptime_const_checker('@[comptime]\nfn compute(a u64, b int) bool { return ${comparison} }\nconst n = compute(18446744073709551615, -1)\nfn main() { _ = n }')
		assert tc.errors.len == 0, tc.errors.str()
		assert tc.comptime_const_values['n']!.integer == expected
	}
}

fn test_comptime_const_platform_sized_comparisons() {
	defer { set_platform_int_bits(64) }
	for bits in [32, 64] {
		set_platform_int_bits(bits)
		tc := comptime_const_checker('@[comptime]\nfn compute(a int, b uint) bool { return a < b }\nconst n = compute(-1, 1)\nfn main() { _ = n }')
		assert tc.errors.len == 0, tc.errors.str()
		assert tc.comptime_const_values['n']!.integer == 0
	}
}

fn test_comptime_const_legacy_signed_size_logical_shift() {
	defer { set_platform_int_bits(64) }
	for bits in [32, 64] {
		set_platform_int_bits(bits)
		tc := comptime_const_checker('module builtin\n@[comptime]\nfn compute(a isize) uint { return a >>> 1 }\nconst n = compute(isize(-1))')
		assert tc.errors.len == 0, tc.errors.str()
		assert tc.comptime_const_values['n']!.integer == if bits == 32 {
			i64(2147483647)
		} else {
			i64(9223372036854775807)
		}
	}
}

fn test_comptime_const_integer_alias_operations() {
	tc := comptime_const_checker('type Number = int\n@[comptime]\nfn compute(a Number) Number { mut n := a for i in Number(0) .. Number(2) { n += i } n++ return -n }\nconst n = compute(Number(3))\nfn main() { _ = n }')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values['n']!.integer == -5
}

fn test_comptime_const_narrow_multiplication_keeps_c_promotion_width() {
	tc := comptime_const_checker('@[comptime]\nfn compute(a u16, b u16) int { return int(a * b) }\nconst n = compute(65535, 65535)\nfn main() { _ = n }')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values['n']!.integer == -131071
}

fn test_comptime_const_aggregate_operands_are_typed_before_body_checks() {
	defer { set_platform_int_bits(64) }
	for bits in [32, 64] {
		set_platform_int_bits(bits)
		for body, expected in {
			'mut a := [u64(0xffffffffffffffff)]! a[0] /= 3 return a[0]':                  u64(6148914691236517205)
			'mut a := State{value: u64(0xffffffffffffffff)} a.value /= 3 return a.value': u64(6148914691236517205)
			'a := [u64(0xffffffffffffffff)]! return a[0] / 3':                            u64(6148914691236517205)
			'a := State{value: u64(0xffffffffffffffff)} return a.value / 3':              u64(6148914691236517205)
			'return high_word() / 3':                                                     u64(6148914691236517205)
			'return high_value / 3':                                                      u64(6148914691236517205)
			'a := [u64(0xffffffffffffffff)]! return a[0] % 7':                            u64(1)
			'mut a := [u64(0xffffffffffffffff)]! a[0] %= 3 return a[0]':                  u64(0)
			'a := [u64(1)]! return a[0] << 40':                                           u64(1099511627776)
			'a := State{small: u32(0xffffffff)} return u64(a.small + 1)':                 u64(0)
			'return (high_word() * 2) / 3':                                               u64(6148914691236517204)
			'a := [u64(0xffffffffffffffff)]! if (a[0] + 0) > 0 { return 1 } return 0':    u64(1)
		} {
			tc := comptime_const_checker('const n = compute()\nconst high_value = u64(0xffffffffffffffff)\nstruct State {\nmut:\nvalue u64\nsmall u32\n}\n@[comptime]\nfn high_word() u64 { return u64(0xffffffffffffffff) }\n@[comptime]\nfn compute() u64 { ${body} }\nfn main() { _ = n }')
			assert tc.errors.len == 0, tc.errors.str()
			assert u64(tc.comptime_const_values['n']!.integer) == expected, body
		}
	}
}

fn test_comptime_const_if_values_keep_their_branch_result_type() {
	for body in [
		'return (if flag { u64(0) } else { 0xffffffffffffffff }) / 3',
		'return (if flag { high_word() } else { 0xffffffffffffffff }) / 3',
		'return (if flag { value := high_word() value } else { 0xffffffffffffffff }) / 3',
	] {
		tc := comptime_const_checker('const n = compute(false)\n@[comptime]\nfn high_word() u64 { return u64(0) }\n@[comptime]\nfn compute(flag bool) u64 { ${body} }\nfn main() { _ = n }')
		assert tc.errors.len == 0, tc.errors.str()
		assert u64(tc.comptime_const_values['n']!.integer) == u64(6148914691236517205), body
	}
}

fn test_comptime_const_narrow_alias_comparison() {
	tc := comptime_const_checker('type Byte = u8\n@[comptime]\nfn compute(a Byte, b Byte) bool { return a + b == Byte(4) }\nconst n = compute(Byte(250), Byte(10))\nfn main() { _ = n }')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values['n']!.integer == 1
}

fn test_comptime_const_comparison_keeps_literal_width() {
	for literal in ['0x7fffffffffffffff', '0xffffffffffffffff', '18446744073709551615'] {
		tc := comptime_const_checker('@[comptime]\nfn compute(a u32) bool { return a < ${literal} }\nconst n = compute(4294967295)\nfn main() { _ = n }')
		assert tc.errors.len == 0, tc.errors.str()
		assert tc.comptime_const_values['n']!.integer == 1
	}
}

fn test_comptime_const_unary_literal_operations_use_c_integer_widths() {
	for expression, expected in {
		'~0xffffffff':                i64(0)
		'-~0xfffffffe':               i64(4294967295)
		'~(0xffffffff & 0xffffffff)': i64(0)
		'~(0xffffffff | 0)':          i64(0)
		'~(0xffffffff ^ 0)':          i64(0)
		'0xffffffff ^ -1':            i64(0)
		'0xffffffff | -1':            i64(4294967295)
	} {
		tc := comptime_const_checker('@[comptime]\nfn compute() int { return ${expression} }\nconst n = compute()\nfn main() { _ = n }')
		assert tc.errors.len == 0, tc.errors.str()
		assert tc.comptime_const_values['n']!.integer == expected
	}
	for expression, input in {
		'a < -0x8000000000000000': 0
		'a < ~0xffffffff':         -1
	} {
		tc := comptime_const_checker('@[comptime]\nfn compute(a int) bool { return ${expression} }\nconst n = compute(${input})\nfn main() { _ = n }')
		assert tc.errors.len == 0, tc.errors.str()
		assert tc.comptime_const_values['n']!.integer == 1
	}
}

fn test_comptime_const_reads_apply_each_constants_storage_type() {
	tc := comptime_const_checker('const byte_value = u8(250) + u8(10)\nconst chained_value = byte_value + u8(253)\n@[comptime]\nfn read_values() int { return int(byte_value) * 1000 + int(chained_value) }\nconst result = read_values()\nfn main() { _ = result }')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values['result']!.integer == 4001
	assert tc.const_int_value('result', [])? == 4001
}

fn test_comptime_const_completed_reads_apply_the_storage_type() {
	tc := comptime_const_checker('@[comptime]\nfn byte_seed() u8 { return 250 }\nconst byte_value = byte_seed() + u8(10)\n@[comptime]\nfn read_value() int { return int(byte_value) }\nconst result = read_value()\nfn main() { _ = result }')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values['result']!.integer == 4
	assert tc.const_int_value('byte_value', [])? == 4
}

fn test_comptime_const_integer_lengths_evaluate_whole_initializers() {
	tc := comptime_const_checker('@[comptime]\nfn seed() int { return 256 }\nconst direct = u8(seed()) + 1\nconst source = seed()\nconst indirect = u8(source) + 1\nconst alias = indirect\nconst quotient = 256 / (256 - u8(source))\nfn main() { _ = [direct]u8{} _ = [indirect]u8{} _ = [alias]u8{} _ = [quotient]u8{} }')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values['direct']!.integer == 1
	for name in ['direct', 'indirect', 'alias', 'quotient'] {
		assert tc.const_int_value(name, [])? == 1, name
		assert tc.const_int_expr(tc.const_exprs[name]!, tc.cur_module, [])? == 1, name
	}
}

fn test_comptime_const_else_if_values_keep_their_branch_result_type() {
	for flag in [1, 2] {
		tc := comptime_const_checker('@[comptime]\nfn choose(flag int) u64 { return (if flag == 0 { u64(0) } else if flag == 1 { u64(0xffffffffffffffff) } else { 0xffffffffffffffff }) / 3 }\nconst result = choose(${flag})\nfn main() { _ = result }')
		assert tc.errors.len == 0, tc.errors.str()
		assert u64(tc.comptime_const_values['result']!.integer) == u64(6148914691236517205)
	}
}

fn test_comptime_const_symbolic_array_lengths_keep_the_evaluator_context() {
	tc := comptime_const_checker('@[comptime]\nfn seed() int { return 3 }\nconst n = seed()\n@[comptime]\nfn measure() int { a := [(n)]u8{} b := [n + 1]u8{} c := [int(n)]u8{} return a.len + b.len + c.len }\nconst total = measure()\nfn main() { _ = total }')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values['total']!.integer == 10
}

fn test_comptime_const_symbolic_array_length_cycles_are_diagnosed() {
	for length in ['n', 'n + 1', '(n)', 'int(n)', '(int(n) + 1)', 'indirect'] {
		tc := comptime_const_checker('const n = size()\nconst indirect = n\n@[comptime]\nfn size() int { return [${length}]u8{}.len }\nfn main() { _ = n }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `n`') && it.msg.contains('cycle in constant')), '${length}: ${tc.errors}'
	}
}

fn test_comptime_const_symbolic_lengths_revisit_earlier_legacy_operands() {
	tc := comptime_const_checker('@[comptime]\nfn seed() int { return 256 }\nconst source = seed()\nconst left_quotient = 256 / (256 - u8(250 + 6)) + source\nfn main() { _ = source }')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.const_int_value('u8(250 + 50) + source', [])? == 300
	assert tc.const_int_value('256 / (256 - u8(250 + 6)) + source', [])? == 257
	assert tc.const_int_value('left_quotient', [])? == 257
	assert tc.const_int_expr(tc.const_exprs['left_quotient']!, tc.cur_module, [])? == 257
}

fn test_comptime_const_sizeof_keeps_unsigned_storage_type() {
	original_bits := platform_int_bits()
	defer { set_platform_int_bits(original_bits) }
	for bits in [32, 64] {
		set_platform_int_bits(bits)
		for body, expected in {
			'n := sizeof(u8) return u64(~n)':              u64(4294967294)
			'n := sizeof[Byte]() + u32(1) return u64(~n)': u64(4294967293)
		} {
			tc := comptime_const_checker('type Byte = u8\n@[comptime]\nfn compute() u64 { ${body} }\nconst result = compute()\nfn main() { _ = result }')
			assert tc.errors.len == 0, tc.errors.str()
			assert u64(tc.comptime_const_values['result']!.integer) == expected
		}
	}
}

fn test_comptime_const_sizeof_keeps_direct_c_expression_width() {
	original_bits := platform_int_bits()
	defer { set_platform_int_bits(original_bits) }
	for bits in [32, 64] {
		set_platform_int_bits(bits)
		maximum := if bits == 32 { u64(4294967295) } else { u64(18446744073709551615) }
		for expression, expected in {
			'u64(~sizeof(u8))':              maximum - 1
			'u64(~(sizeof[u8]() + u32(1)))': maximum - 2
		} {
			tc := comptime_const_checker('@[comptime]\nfn compute() u64 { return ${expression} }\nconst result = compute()\nfn main() { _ = result }')
			assert tc.errors.len == 0, tc.errors.str()
			assert u64(tc.comptime_const_values['result']!.integer) == expected
		}
	}
}

fn test_comptime_const_converts_symbolic_fixed_array_results() {
	tc := comptime_const_checker('const n = 2\nconst table = make()\n@[comptime]\nfn make() [n]u8 { return [u8(1), 2]! }\nfn main() { _ = table }')
	assert tc.errors.len == 0, tc.errors.str()
	value := tc.comptime_const_values['table']!
	assert value.elements.len == 2
	assert value.elements[0].integer == 1
	assert value.elements[1].integer == 2
}

fn test_comptime_const_converts_symbolic_parameters_nested_arrays_and_aliases() {
	for source in [
		'const n = 2\nconst table = copy([u8(1), 2]!)\n@[comptime]\nfn copy(value [n]u8) [n]u8 { mut local := value local[1] = 3 return local }',
		'const n = 2\nconst table = make()\n@[comptime]\nfn make() [n][n]u8 { return [[u8(1), 2]!, [u8(3), 4]!]! }',
		'const n = 2\ntype Pair = [n]u8\nconst table = make()\n@[comptime]\nfn make() Pair { return Pair([u8(1), 2]!) }',
		'const n = 2\nconst zero = 0\nconst table = make()\n@[comptime]\nfn make() [n]u8 { return [u16(1), 2]! }',
	] {
		tc := comptime_const_checker(source + '\nfn main() { _ = table }')
		assert tc.errors.len == 0, tc.errors.str()
		value := tc.comptime_const_values['table']!
		assert value.elements.len == 2
		if source.contains('type Pair') {
			assert value.typ is Alias
			assert value.typ.source_type_id() == flat.source_type_id('Pair')
		}
		if source.contains('[u16(1), 2]!') {
			// Literal elements use the return context, including ordinary integer conversions.
			assert value.elements.map(int(it.integer)) == [1, 2]
			assert value.elements.all(it.typ.source_type_id() == flat.source_type_u8)
		}
	}
}

fn test_comptime_const_rejects_incompatible_array_shapes_and_elements() {
	for item in [
		['[n]u8', '[u8(1), 2, 3]!'],
		['[n]u8', '[n]u16{}'],
		['[zero][3]u8', '[zero][2]u8{}'],
		['[zero]u8', '[zero]u16{}'],
	] {
		tc := comptime_const_checker('const n = 2\nconst zero = 0\nconst table = make()\n@[comptime]\nfn make() ${item[0]} { return ${item[1]} }\nfn main() { _ = table }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `table`') && it.msg.contains('unsupported conversion')), tc.errors.str()
	}
}

fn test_comptime_const_symbolic_array_conversion_keeps_cycle_guards() {
	for returned in ['[n]u8', 'Pair'] {
		expression := if returned == 'Pair' { 'Pair([u8(1), 2]!)' } else { '[u8(1), 2]!' }
		tc := comptime_const_checker('const n = size()\ntype Pair = [n]u8\n@[comptime]\nfn size() int { return make().len }\n@[comptime]\nfn make() ${returned} { return ${expression} }\nfn main() { _ = n }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `n`') && it.msg.contains('cycle in constant')), tc.errors.str()
	}
}

fn test_comptime_const_imported_array_shapes_keep_declaration_scope() ! {
	root := os.join_path(os.vtmp_dir(), 'comptime_const_array_scope_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	helper := os.join_path(root, 'helper.v')
	main := os.join_path(root, 'main.v')
	os.write_file(helper, 'module helper\nconst n = 2\npub type Pair = [n]u8\npub const table = make()\n@[comptime]\npub fn make() [n]u8 { return [u8(1), 2]! }\n@[comptime]\npub fn last(value [3]u8) int { return value[2] }')!
	os.write_file(main, 'module main\nimport helper as source\nconst n = 3\nconst result = compute()\n@[comptime]\nfn compute() int { mut local := source.table local = source.make() aliased := source.Pair([u8(1), 2]!) mut values := [n]u8{} values[2] = 7 return local[1] + aliased[0] + source.last(values) }\nfn main() { _ = result }')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([helper, main])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	assert 'helper.table' in tc.comptime_const_values
	assert tc.comptime_const_values['result']!.integer == 10
	// Re-evaluate with the completed imported value already cached.
	tc.cur_file = main
	tc.cur_module = 'main'
	value := tc.eval_comptime_const(tc.const_exprs['result']!)!
	assert value.integer == 10
}

fn test_comptime_const_struct_fields_keep_declaration_scope() ! {
	root := os.join_path(os.vtmp_dir(), 'comptime_const_struct_scope_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	helper := os.join_path(root, 'helper.v')
	main := os.join_path(root, 'main.v')
	os.write_file(helper, 'module helper\nconst n = 2\npub struct S { pub: items [n]u8 value int }\npub struct Outer { pub: child S tagged [n]u8 }\n@[comptime]\npub fn measure(s S) int { return s.items.len }\n@[comptime]\npub fn measure_outer(s Outer) int { return s.child.items.len * 10 + s.tagged.len }')!
	os.write_file(main, 'module main\nimport helper as source\nconst n = 3\nconst result = source.measure(source.S{})\nconst nested = source.measure_outer(source.Outer{})\nconst explicit = compute(5)\n@[comptime]\nfn compute(n int) int { value := source.S{value: n} outer := source.Outer{child: value} return source.measure_outer(outer) * 10 + value.value }\nfn main() { _ = result _ = nested _ = explicit _ = n }')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([helper, main])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values['result']!.integer == 2
	assert tc.comptime_const_values['nested']!.integer == 22
	assert tc.comptime_const_values['explicit']!.integer == 225
}

fn test_comptime_const_left_shift_keeps_c_expression_width() {
	original_bits := platform_int_bits()
	defer { set_platform_int_bits(original_bits) }
	set_platform_int_bits(64)
	for body, expected in {
		'return (1 << 30) > bound':                            i64(1)
		'return (1 << 31) > bound':                            i64(0)
		'return (0 << 31) > bound':                            i64(0)
		'return (1 << (30 + 1)) > bound':                      i64(0)
		'return (1 << ((31 | 32) & 31)) > bound':              i64(0)
		'return (1 << (32 ^ 63)) > bound':                     i64(0)
		'return (1 << (93 / 3)) > bound':                      i64(0)
		'return (1 << (63 % 32)) > bound':                     i64(0)
		'return (1 << ~(-32)) > bound':                        i64(0)
		'return (1 << shift_count) > bound':                   i64(0)
		'return (1 << required_count) > bound':                i64(0)
		'return ((1 << 31) + 0) > bound':                      i64(0)
		'return ((1 << 31) & 1) > bound':                      i64(0)
		'return (1 << 63) > 0':                                i64(1)
		'return (1 << 64) > bound':                            i64(1)
		'return ~(1 << 31) > 0':                               i64(1)
		'return ((1) << 31) > bound':                          i64(1)
		'return (1 << (1 << 5)) > bound':                      i64(1)
		'return (1 << int(31)) > bound':                       i64(1)
		'return (1 << get_count()) > bound':                   i64(1)
		'return (1 << count) > bound':                         i64(1)
		'return (1 << ordinary_count) > bound':                i64(0)
		'return (u64(1) << 31) > bound':                       i64(1)
		'value := 1 << 31 return value > bound':               i64(1)
		'shift_count := 30 return (1 << shift_count) > bound': i64(0)
	} {
		tc := comptime_const_checker('const shift_count = 31\nconst ordinary_count = u8(250 + 37)\nconst required_count = get_count()\n@[comptime]\nfn get_count() u8 { return u8(250 + 37) }\n@[comptime]\nfn above(bound int, count int) bool { ${body} }\nconst result = above(-1, 31)\nfn main() { _ = result }')
		assert tc.errors.len == 0, tc.errors.str()
		assert tc.comptime_const_values['result']!.integer == expected, body
	}
}

fn test_comptime_const_left_shift_widens_before_target_storage_conversion() {
	original_bits := platform_int_bits()
	defer { set_platform_int_bits(original_bits) }
	for bits in [32, 64] {
		set_platform_int_bits(bits)
		for body, expected in {
			'return u64(1 << 32)':                u64(4294967296)
			'return u64(4294967297 << 31)':       u64(9223372039002259456)
			'value := 1 << 32 return u64(value)': if bits == 32 { u64(0) } else { u64(4294967296) }
		} {
			tc := comptime_const_checker('@[comptime]\nfn compute() u64 { ${body} }\nconst result = compute()\nfn main() { _ = result }')
			assert tc.errors.len == 0, tc.errors.str()
			assert u64(tc.comptime_const_values['result']!.integer) == expected, '${bits}: ${body}'
		}
	}
}

fn test_comptime_const_left_shift_count_keeps_cycle_guards() {
	tc := comptime_const_checker('const count = make_count()\n@[comptime]\nfn make_count() int { count := 31 return int(1 << count) }\nfn main() { _ = count }')
	assert tc.errors.any(it.msg.contains('cannot evaluate const `count`') && it.msg.contains('cycle in constant')), tc.errors.str()
}

fn test_comptime_const_rejects_translated_context_after_metadata_update() ! {
	original_bits := platform_int_bits()
	defer { set_platform_int_bits(original_bits) }
	set_platform_int_bits(64)
	mut tc := comptime_const_checker('@[comptime]\nfn above(bound int) bool { return (1 << 31) > bound }\nconst result = above(-1)\nfn main() { _ = result }')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values['result']!.integer == 0
	mut evaluator := tc.new_comptime_const_eval(tc.const_exprs['result']!)!
	for _, file in tc.a.source_files { tc.translated_files[file.name] = true }
	if _ := tc.eval_comptime_const(tc.const_exprs['result']!) {
		assert false, 'translated evaluation succeeded'
	} else {
		assert err.msg().contains('required evaluation of translated code is not supported')
	}
	if _ := evaluator.expr(tc.const_exprs['result']!) {
		assert false, 'existing evaluator accepted translated context'
	} else {
		assert err.msg().contains('required evaluation of translated code is not supported')
	}
}

fn test_comptime_const_selector_locals_precede_merged_header_imports() ! {
	root := os.join_path(os.vtmp_dir(), 'comptime_const_selector_scope_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	dep := os.join_path(root, 'dep.v')
	cache := os.join_path(root, 'v3_module_cache_fixture')
	os.mkdir_all(cache)!
	helper := os.join_path(cache, 'helper.vh')
	main := os.join_path(root, 'main.v')
	os.write_file(dep, 'module dep\npub const value = 2')!
	// Cached headers combine imports and annotated bodies from sibling files.
	os.write_file(helper, 'module helper\nimport dep as item\nconst value = 3\npub struct Cell { pub: value int }\n@[comptime]\npub fn local_value() int { item := Cell{value: 7} return item.value }\n@[comptime]\npub fn parameter_value(item Cell) int { return item.value }\n@[comptime]\npub fn imported_value() int { return item.value }\n@[comptime]\npub fn module_shadow() int { helper := Cell{value: 13} return helper.value }')!
	os.write_file(main, 'module main\nimport helper\nconst local_result = helper.local_value()\nconst parameter_result = helper.parameter_value(helper.Cell{value: 11})\nconst imported_result = helper.imported_value()\nconst module_shadow_result = helper.module_shadow()\nfn main() { _ = local_result _ = parameter_result _ = imported_result _ = module_shadow_result }')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([dep, helper, main])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values['local_result']!.integer == 7
	assert tc.comptime_const_values['parameter_result']!.integer == 11
	assert tc.comptime_const_values['imported_result']!.integer == 2
	assert tc.comptime_const_values['module_shadow_result']!.integer == 13
}

fn test_comptime_const_reads_current_module_qualified_constants() {
	for module_name in ['helper', 'main', 'builtin'] {
		tc := comptime_const_checker('module ${module_name}\nconst n = 2\n@[comptime]\nfn qualified() int { return ${module_name}.n }\nconst qualified_result = qualified()\nfn main() { _ = qualified_result }')
		assert tc.errors.len == 0, tc.errors.str()
		key_prefix := if module_name in ['main', 'builtin'] { '' } else { '${module_name}.' }
		assert tc.comptime_const_values['${key_prefix}qualified_result']!.integer == 2
	}
}

fn test_comptime_const_translated_declaration_contexts_and_native_controls() ! {
	root := os.join_path(os.vtmp_dir(), 'comptime_const_translated_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	helper := os.join_path(root, 'helper.v')
	main := os.join_path(root, 'main.v')
	for translated in [false, true] {
		prefix := if translated { '@[translated]\n' } else { '' }
		os.write_file(helper, '${prefix}module helper\npub const value = u8(250 + 10)\npub struct Box { pub: value int }\npub type Pair = [2]u8\n@[comptime]\npub fn negate(a u8) int { value := -a return int(value) }')!
		for name, body in {
			'call':   'return helper.negate(250)'
			'const':  'return int(helper.value)'
			'cached': 'return int(helper.value)'
			'struct': 'return helper.Box{}.value'
			'alias':  'return helper.Pair([u8(1), 2]!).len'
			'unused': 'return if false { helper.negate(250) } else { 7 }'
		} {
			os.write_file(main, 'module main\nimport helper\n@[comptime]\nfn outer() int { ${body} }\nconst result = outer()\nfn main() { _ = result }')!
			mut p := parser.Parser.new(pref.new_preferences())
			a := p.parse_files([helper, main])
			assert p.diagnostics.len == 0, p.diagnostics.str()
			mut tc := TypeChecker.new(a)
			tc.collect(a)
			if name == 'cached' {
				tc.comptime_const_values['helper.value'] = ComptimeConstValue{ typ: u8_, integer: 9 }
			}
			tc.check_semantics()
			if translated && name != 'unused' {
				assert tc.errors.any(it.msg.contains('cannot evaluate const `result`') && it.msg.contains('required evaluation of translated code is not supported')), '${name}: ${tc.errors}'
			} else {
				assert tc.errors.len == 0, '${name}: ${tc.errors}'
				expected := match name {
					'call' { 6 }
					'const' { 4 }
					'cached' { 9 }
					'struct' { 0 }
					'alias' { 2 }
					else { 7 }
				}
				assert tc.comptime_const_values['result']!.integer == expected, name
			}
		}
	}
}

fn test_comptime_const_literal_logical_shift_uses_u32() {
	original_bits := platform_int_bits()
	defer { set_platform_int_bits(original_bits) }
	for bits in [32, 64] {
		set_platform_int_bits(bits)
		wide := if bits == 32 { u64(2147483645) } else { u64(9223372036854775805) }
		for body, expected in {
			'n := -5 >>> 1 return u64(n)':    u64(2147483645)
			'return u64(((-5)) >>> count)':   u64(2147483645)
			'return u64(-5 >>> named_count)': u64(2147483645)
			'return u64(-5 >>> 0)':           u64(4294967291)
			'return u64(-5 >>> 31)':          u64(1)
			'return u64(-5 >>> 32)':          u64(0)
			'return u64(-5 >>> -1)':          u64(0)
			'return u64(~(5 >>> 1))':         u64(4294967293)
			'return u64(-(5) >>> 1)':         wide
			'return u64(int(-5) >>> 1)':      wide
			'n := -5 return u64(n >>> 1)':    wide
			'return u64((-5 + 0) >>> 1)':     wide
			'return u64(i64(-5) >>> 1)':      u64(9223372036854775805)
			'return u64(i32(-5) >>> 1)':      u64(2147483645)
			'return u64(i8(-5) >>> 1)':       u64(125)
		} {
			tc := comptime_const_checker('const named_count = 1\nconst result = compute(1)\n@[comptime]\nfn compute(count int) u64 { ${body} }\nfn main() { _ = result }')
			assert tc.errors.len == 0, '${bits}: ${body}: ${tc.errors}'
			assert u64(tc.comptime_const_values['result']!.integer) == expected, '${bits}: ${body}'
		}
	}
}

fn test_comptime_const_inferred_array_evaluates_elements_once_in_order() {
	for body, expected in {
		'mut n := 0 a := [if true { n += 1 n } else { 0 }]! return a[0]':                                                        1
		'mut n := 0 a := [if true { n += 1 n } else { 0 }, if true { n += 1 n } else { 0 }]! return a[0] * 100 + a[1] * 10 + n': 122
		'mut n := 0 a := [[if true { n += 1 n } else { 0 }]!]! return a[0][0] * 10 + n':                                         11
		'n := u8(128) a := [~n]! return int(a[0])':                                                                              127
	} {
		tc := comptime_const_checker('const result = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = result }')
		assert tc.errors.len == 0, tc.errors.str()
		assert tc.comptime_const_values['result']!.integer == expected, body
	}
}

fn test_comptime_const_rejects_overloaded_scalar_operators() {
	declarations := 'type Count = int\ntype Word = string\ntype Flag = bool\nstruct Cell {\nmut:\nvalue Count\n}\nfn (a Count) + (b Count) Count { return Count(int(a) + int(b) + 10) }\nfn (a Count) < (b Count) bool { return int(a) > int(b) }\nfn (a Count) == (b Count) bool { return int(a) != int(b) }\nfn (a Word) + (b Word) Word { return Word(string(a) + "!" + string(b)) }\nfn (a Word) == (b Word) bool { return string(a) != string(b) }\nfn (a Flag) == (b Flag) bool { return bool(a) != bool(b) }'
	for body in [
		'return int(Count(1) + Count(2))',
		'return int(Count(1) + 2)',
		'mut a := Count(1) a += Count(2) return int(a)',
		'mut a := Cell{value: Count(1)} a.value += Count(2) return int(a.value)',
		'mut a := [Count(1)]! a[0] += Count(2) return int(a[0])',
		'if Count(1) < Count(2) { return 1 } return 0',
		'if Count(1) > Count(2) { return 1 } return 0',
		'if Count(1) <= Count(2) { return 1 } return 0',
		'if Count(1) >= Count(2) { return 1 } return 0',
		'if Count(1) == Count(2) { return 1 } return 0',
		'if Count(1) != Count(2) { return 1 } return 0',
		'return (Word("a") + Word("b")).len',
		'if Word("a") == Word("b") { return 1 } return 0',
		'if Flag(true) == Flag(false) { return 1 } return 0',
	] {
		tc := comptime_const_checker('${declarations}\nconst bad = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = bad }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `bad`') && it.msg.contains('overloaded operator')), '${body}: ${tc.errors}'
	}
}

fn test_comptime_const_keeps_builtin_alias_operators() {
	declarations := 'type Count = int\ntype Derived = Count\ntype Plain = int\ntype Flag = bool\nfn (a Count) + (b Count) Count { return Count(int(a) + int(b) + 10) }\nfn (a Count) - (b Count) Count { return Count(int(a) - int(b) + 10) }\nfn (a Flag) == (b Flag) bool { return bool(a) != bool(b) }'
	for body, expected in {
		'return int(1 + Count(2))':                              3
		'return int(Count(1)) + int(Count(2))':                  3
		'return int(Derived(1) + Derived(2))':                   3
		'mut a := Plain(1) a += Plain(2) return int(a)':         3
		'return int(-Count(2))':                                 -2
		'mut a := Count(1) a++ return int(a)':                   2
		'if !Flag(true) { return 1 } return 0':                  0
		'if false { return int(Count(1) + Count(2)) } return 7': 7
	} {
		tc := comptime_const_checker('${declarations}\nconst n = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = n }')
		assert tc.errors.len == 0, '${body}: ${tc.errors}'
		assert tc.comptime_const_values['n']!.integer == expected, body
	}
}

fn test_comptime_const_resolves_mutation_targets_once() {
	declarations := 'struct Cell {\nmut:\nvalue int\nvalues [3]int\n}\nstruct Box {\nmut:\ncells [3]Cell\n}'
	for body, expected in {
		'mut a := [3]int{} mut i := 0 a[if true { i += 1 i } else { 0 }] += 7 return a[1] * 100 + a[2] * 10 + i':                                             701
		'mut a := [3]int{} mut i := 0 a[if true { i += 1 i } else { 0 }]++ return a[1] * 100 + a[2] * 10 + i':                                                101
		'mut a := [3][3]int{} mut i := 0 a[if true { i += 1 i } else { 0 }][1] = 7 return a[1][1] * 100 + a[2][1] * 10 + i':                                  701
		'mut a := [3]int{} a[if true { a[0] = 5 1 } else { 0 }] = 7 return a[0] * 10 + a[1]':                                                                 57
		'mut a := [2]int{} value := a[if true { a[0] = 5 0 } else { 1 }] return value':                                                                       5
		'mut a := [3]Cell{} mut i := 0 a[if true { i += 1 i } else { 0 }].value += 7 return a[1].value * 100 + i':                                            701
		'mut a := Box{} mut i := 0 a.cells[if true { i += 1 i } else { 0 }].values[if true { i += 1 i } else { 0 }]++ return a.cells[1].values[2] * 100 + i': 102
		'mut a := [3]Cell{} copy := a mut i := 0 a[if true { i += 1 i } else { 0 }].value += 7 return copy[1].value * 1000 + a[1].value * 100 + i':           701
		'mut a := [u8(0), 255, 0]! mut i := 0 a[if true { i += 1 i } else { 0 }]++ return int(a[1]) * 100 + i':                                               1
		'mut a := [3]int{} mut i := 1 a[i] += if true { i = 2 a[0] = 5 7 } else { 0 } return a[0] * 1000 + a[1] * 100 + a[2] * 10 + i':                       5072
	} {
		tc := comptime_const_checker('${declarations}\nconst n = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = n }')
		assert tc.errors.len == 0, '${body}: ${tc.errors}'
		assert tc.comptime_const_values['n']!.integer == expected, body
	}
}

fn test_comptime_const_rejects_dependent_conditional_index_ordering() {
	for body in [
		'mut a := [3]int{} mut i := 0 a[if true { i += 1 i } else { 0 }] += i return a[1]',
		'mut a := [3]int{} mut order := 0 a[if true { order = order * 10 + 1 1 } else { 0 }] = if true { order = order * 10 + 2 7 } else { 0 } return order',
		'mut a := [3]int{} mut order := 0 a[if true { order = order * 10 + 1 1 } else { 0 }] += if true { order = order * 10 + 2 7 } else { 0 } return order',
		'mut a := [2][2]int{} mut i := 0 a[i][if true { i = 1 0 } else { 0 }]++ return a[1][0]',
		'a := [[10, 11]!, [20, 21]!]! mut i := 0 return a[i][if true { i = 1 0 } else { 0 }]',
		'mut i := 0 return table[i][if true { i = 1 0 } else { 0 }]',
		'mut i := 0 return table[$if linux { i } $else { 0 }][if true { i = 1 0 } else { 0 }]',
		'mut a := [3]int{} a[1 + if true { 0 } else { 1 }] += 7 return a[1]',
		'mut a := [2]int{} return Values(a)[if true { a[0] = 5 0 } else { 1 }]',
	] {
		tc := comptime_const_checker('type Values = [2]int\nconst table = [[10, 11]!, [20, 21]!]!\nconst bad = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = bad }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `bad`') && it.msg.contains('conditional index ordering')), '${body}: ${tc.errors}'
	}
}

fn test_comptime_const_conditional_index_workarounds_and_bounds() {
	for body, expected in {
		'return table[if true { 1 } else { 0 }][0]':                                                                           20
		'mut a := [3]int{} mut i := 0 index := if true { i += 1 i } else { 0 } a[index] += i return a[1] * 10 + i':            11
		'mut a := [3]int{} mut i := 0 a[(if true { i += 1 i } else { 0 })] += amount return a[1] * 10 + i':                    71
		'a := [[10, 11]!, [20, 21]!]! mut i := 0 first := i second := if true { i = 1 0 } else { 0 } return a[first][second]': 10
		'mut a := [3]int{} mut i := 0 if false { a[if true { i += 1 i } else { 0 }] += i } return 7':                          7
		'mut a := [3]int{} a[1] += if true { a[1] = 5 7 } else { 0 } return a[1]':                                             12
	} {
		tc := comptime_const_checker('const amount = 7\nconst table = [[10, 11]!, [20, 21]!]!\nconst n = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = n }')
		assert tc.errors.len == 0, '${body}: ${tc.errors}'
		assert tc.comptime_const_values['n']!.integer == expected, body
	}
	for update in ['= 7', '+= 7', '++'] {
		tc := comptime_const_checker('const bad = compute()\n@[comptime]\nfn compute() int { mut a := [3]int{} mut i := 2 a[if true { i += 1 i } else { 0 }] ${update} return a[0] }\nfn main() { _ = bad }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `bad`') && it.msg.contains('array index out of bounds')), tc.errors.str()
	}
}

fn test_comptime_const_range_bounds_keep_independent_integer_types() {
	for body, expected in {
		'for _ in u8(0) .. u16(256) { return 1 } return 0':                                                                                 1
		'limit := i16(-1) for _ in u8(0) .. limit { return 1 } return 0':                                                                   0
		'limit := i64(-1) for _ in u64(0) .. limit { return 1 } return 0':                                                                  0
		'for _ in i64(-1) .. u64(1) { return 1 } return 0':                                                                                 1
		'limit := u64(0xffffffffffffffff) for _ in i64(-1) .. limit { return 1 } return 0':                                                 1
		'for _ in u64(0) .. 1 { return 1 } return 0':                                                                                       1
		'for _ in int(0) .. u8(255) + u8(1) { return 1 } return 0':                                                                         0
		'for _ in u8(0) .. (u16(256)) { return 1 } return 0':                                                                               1
		'mut limit := u16(2) mut count := 0 for _ in u8(0) .. limit { count++ limit = 256 if count == 2 { break } } return count':          2
		'mut limit := u16(2) mut count := 0 for _ in u8(0) .. limit { count++ limit = 256 if count == 2 { break } continue } return count': 2
		'mut limit := u16(256) mut count := 0 for _ in u8(0) .. limit { count++ limit = 1 } return count':                                  1
		'for _ in i64(-1) .. sizeof(u8) { return 1 } return 0':                                                                             1
		'for _ in i64(-1) .. (sizeof(u8)) { return 1 } return 0':                                                                           1
	} {
		tc := comptime_const_checker('const result = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = result }')
		assert tc.errors.len == 0, '${body}: ${tc.errors}'
		assert tc.comptime_const_values['result']!.integer == expected, body
	}
}

fn test_comptime_const_range_comparisons_reject_operator_methods() {
	tc := comptime_const_checker('type Step = u8\nfn (a Step) < (b Step) bool { return false }\nconst result = compute()\n@[comptime]\nfn compute() int { for _ in Step(0) .. Step(2) { return 1 } return 0 }\nfn main() { _ = result }')
	assert tc.errors.any(it.msg.contains('cannot evaluate const `result`') && it.msg.contains('overloaded operator')), tc.errors.str()
}

fn test_comptime_const_range_bounds_keep_runtime_stability() {
	declarations := 'struct RangeBounds {\nmut:\nend int\n}\n@[comptime]\nfn range_bound(n int) int { return n }'
	for body, expected in {
		'mut n := 3 mut count := 0 for _ in 0 .. n { n = 1 count++ } return count':                                               1
		'mut bounds := RangeBounds{end: 3} mut count := 0 for _ in 0 .. bounds.end { bounds.end = 1 count++ } return count':      1
		'mut bounds := [3]!\nmut count := 0 for _ in 0 .. bounds[0] { bounds[0] = 1 count++ } return count':                      1
		'mut n := 3 mut count := 0 for _ in 0 .. int(n) { n = 1 count++ } return count':                                          1
		'mut n := 3 mut count := 0 for _ in 0 .. (n) { n = 1 count++ } return count':                                             3
		'mut n := 3 mut count := 0 for _ in 0 .. n + 0 { n = 1 count++ } return count':                                           3
		'mut n := 3 mut count := 0 for _ in 0 .. (n + 0) { n = 1 count++ } return count':                                         3
		'mut n := 3 mut count := 0 for _ in 0 .. range_bound(n) { n = 1 count++ } return count':                                  3
		'mut n := 3 mut count := 0 for _ in 0 .. (if true { n } else { 0 }) { n = 1 count++ } return count':                      3
		'mut n := 3 mut count := 0 for _ in 0 .. n { n = 1 count++ continue } return count':                                      1
		'mut bounds := [3, 1]!\nmut index := 0 mut count := 0 for _ in 0 .. bounds[index] { index = 1 count++ } return count':    1
		'mut bounds := [3]!\nmut index := 0 mut count := 0 for _ in 0 .. bounds[index] { index = 1 count++ break } return count': 1
	} {
		tc := comptime_const_checker('${declarations}\nconst result = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = result }')
		assert tc.errors.len == 0, '${body}: ${tc.errors}'
		assert tc.comptime_const_values['result']!.integer == expected, body
	}
}

fn test_comptime_const_rejects_nominal_index_overloads() {
	declarations := 'type GetterPair = [2]int\ntype SetterPair = [2]int\ntype Pair = [2]int\nfn (a GetterPair) [] (i int) int { return 7 }\nfn (mut a SetterPair) []= (i int, value int) {}\nfn (a Pair) [] (i int) int { return 7 }\nfn (mut a Pair) []= (i int, value int) {}\nstruct IndexedBox {\nmut:\npair Pair\n}\nconst pair = Pair([1, 2]!)'
	for body in [
		'a := GetterPair([1, 2]!) return a[0]',
		'return GetterPair([1, 2]!)[0]',
		'return pair[0]',
		'a := [Pair([1, 2]!)]! return a[0][0]',
		'a := IndexedBox{pair: Pair([1, 2]!)} return a.pair[0]',
		'mut a := SetterPair([1, 2]!) a[0] = 9 return 1',
		'mut a := Pair([1, 2]!) a[0] = 9 return 1',
		'mut a := Pair([1, 2]!) a[0] += 9 return 1',
		'mut a := Pair([1, 2]!) a[0]++ return 1',
		'mut a := [IndexedBox{pair: Pair([1, 2]!)}]! a[0].pair[1] += 9 return 1',
	] {
		tc := comptime_const_checker('${declarations}\nconst bad = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = bad }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `bad`') && it.msg.contains('overloaded indexing')), '${body}: ${tc.errors}'
	}
}

fn test_comptime_const_builtin_alias_indexing_and_unexecuted_overloads() {
	declarations := 'type PlainPair = [2]int\ntype Pair = [2]int\nfn (a Pair) [] (i int) int { return 7 }\nfn (mut a Pair) []= (i int, value int) {}'
	for body, expected in {
		'a := PlainPair([1, 2]!) return a[0]':                                  1
		'mut a := PlainPair([1, 2]!) a[0] += 4 a[1]++ return a[0] * 10 + a[1]': 53
		'a := Pair([1, 2]!) if false { return a[0] } return 7':                 7
		'mut a := Pair([1, 2]!) if false { a[0] = 9 } return 7':                7
	} {
		tc := comptime_const_checker('${declarations}\nconst result = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = result }')
		assert tc.errors.len == 0, '${body}: ${tc.errors}'
		assert tc.comptime_const_values['result']!.integer == expected, body
	}
}

fn test_comptime_const_string_concatenation_keeps_nominal_dispatch() {
	declarations := 'type Word = string\nfn (a Word) == (b Word) bool { return false }'
	for body in [
		's := Word("a") + Word("b") if s == Word("ab") { return 1 } return 0',
		'if (Word("a") + Word("b")) == Word("ab") { return 1 } return 0',
		's := (Word("a") + Word("b")) + Word("c") if s == Word("abc") { return 1 } return 0',
		'mut s := Word("a") s += Word("b") if s == Word("ab") { return 1 } return 0',
	] {
		tc := comptime_const_checker('${declarations}\nconst bad = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = bad }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `bad`') && it.msg.contains('overloaded operator')), '${body}: ${tc.errors}'
	}
}

fn test_comptime_const_string_concatenation_plain_string_controls() {
	declarations := 'type Word = string\ntype PlainWord = string\nfn (a Word) == (b Word) bool { return false }'
	for body, expected in {
		's := Word("a") + "b" if s == Word("ab") { return 1 } return 0':                      1
		's := "a" + Word("b") if s == Word("ab") { return 1 } return 0':                      1
		's := string(Word("a") + Word("b")) if s == Word("ab") { return 1 } return 0':        1
		's := PlainWord("a") + PlainWord("b") if s == PlainWord("ab") { return 1 } return 0': 1
		'if false { s := Word("a") + Word("b") if s == Word("ab") { return 1 } } return 7':   7
	} {
		tc := comptime_const_checker('${declarations}\nconst result = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = result }')
		assert tc.errors.len == 0, '${body}: ${tc.errors}'
		assert tc.comptime_const_values['result']!.integer == expected, body
	}
}

fn test_comptime_const_rejects_gated_indexing_in_all_access_paths() {
	for body in [
		'a := [1, 2]! return a#[-1]',
		'return table#[0]',
		'mut a := [1, 2]! return a#[if true { a[0] = 9 0 } else { 1 }]',
		'a := [[1, 2]!, [3, 4]!]! return a[0]#[-1]',
		'a := [[1, 2]!, [3, 4]!]! return a#[0][1]',
		'mut a := [1, 2]! a#[0] = 9 return a[0]',
		'mut a := [1, 2]! a#[0] += 9 return a[0]',
		'mut a := [1, 2]! a#[0]++ return a[0]',
		'mut a := [[1, 2]!, [3, 4]!]! a[0]#[1] += 9 return a[0][1]',
	] {
		tc := comptime_const_checker('const table = [1, 2]!\nconst bad = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = bad }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `bad`') && it.msg.contains('gated indexing')), '${body}: ${tc.errors}'
	}
}

fn test_comptime_const_ordinary_indexing_and_unexecuted_gated_accesses() {
	for body, expected in {
		'mut a := [1, 2]! return a[if true { a[0] = 9 0 } else { 1 }]': 9
		'a := [1, 2]! if false { return a#[-1] } return 7':             7
		'mut a := [1, 2]! if false { a#[0] = 9 } return a[0]':          1
	} {
		tc := comptime_const_checker('const result = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = result }')
		assert tc.errors.len == 0, '${body}: ${tc.errors}'
		assert tc.comptime_const_values['result']!.integer == expected, body
	}
}

fn test_comptime_const_integer_promotion_preserves_local_storage_width() {
	for body, expected in {
		'value := n + u8(1) return value':                              256
		'value := (n + 0) + u8(1) return value':                        256
		'local := n value := local + u8(1) return value':               256
		'mut local := 1 local = n value := local + u8(1) return value': 256
		'value := 1 + u8(255) return int(value)':                       0
		'value := small + u8(255) return int(value)':                   0
	} {
		tc := comptime_const_checker('const small = 1\nconst result = compute(255)\n@[comptime]\nfn compute(n int) int { ${body} }\nfn main() { _ = result }')
		assert tc.errors.len == 0, '${body}: ${tc.errors}'
		assert tc.comptime_const_values['result']!.integer == expected, body
	}
}

fn test_comptime_const_integer_promotion_rejects_shadowing_and_preserves_guards() {
	for body in [
		'n := 1 return n + u8(1)',
		'n := 1 return (n + 0) + u8(1)',
		'n := 1 return u8(1) + n',
		'n := 1 return (int(n)) + u8(1)',
	] {
		tc := comptime_const_checker('const n = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = n }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `n`') && it.msg.contains('integer promotion of a local that shadows a constant')), '${body}: ${tc.errors}'
	}
	for helper in [
		'fn compute(n int) int { value := n + u8(1) return value }',
		'fn compute(n int) int { return if true { n + u8(1) } else { 0 } }',
		'fn compute(input int) int { return if true { n := input n + u8(1) } else { 0 } }',
	] {
		tc := comptime_const_checker('const n = 1\nconst result = compute(255)\n@[comptime]\n${helper}\nfn main() { _ = result }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `result`') && it.msg.contains('integer promotion of a local that shadows a constant')), '${helper}: ${tc.errors}'
	}
	for body in ['return n + u8(1)', 'return u8(1) + n'] {
		tc := comptime_const_checker('const n = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = n }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `n`') && it.msg.contains('cycle in constant')), '${body}: ${tc.errors}'
	}
}

fn test_comptime_const_conditional_type_queries_do_not_start_fresh_evaluation() {
	for body in [
		'return if true { 0 } else { n + u8(1) }',
		'return if true { 0 } else { n := 1 n + u8(1) }',
		'return if true { 0 } else { required + u8(1) }',
		'return if true { 0 } else { indirect + u8(1) }',
		'return if true { 0 } else { seed() + u8(1) }',
	] {
		tc := comptime_const_checker('const required = seed()\nconst indirect = required\nconst n = compute()\n@[comptime]\nfn seed() int { return 1 }\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = n }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `n`') && it.msg.contains('required evaluation during conditional type inference is not supported')), '${body}: ${tc.errors}'
	}
}

fn test_comptime_const_narrowing_conditional_integer_values_are_explicit() {
	for body in [
		'return if true { 255 + u8(1) } else { 0 }',
		'return (if true { 255 + u8(1) } else { 0 })',
		'value := if true { 255 + u8(1) } else { 0 } return value',
	] {
		tc := comptime_const_checker('const result = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = result }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `result`') && it.msg.contains('narrowing conditional integer values')), '${body}: ${tc.errors}'
	}
	for body in [
		'return if true { 254 + u8(1) } else { 0 }',
		'value := if true { 254 + u8(1) } else { 0 } return value',
	] {
		tc := comptime_const_checker('const result = compute()\n@[comptime]\nfn compute() int { ${body} }\nfn main() { _ = result }')
		assert tc.errors.len == 0, '${body}: ${tc.errors}'
		assert tc.comptime_const_values['result']!.integer == 255, body
	}
}

fn test_comptime_const_array_conversions_preserve_nominal_dispatch() {
	declarations := 'type Count = int\ntype Pair = [2]int\nfn (a Count) + (b Count) Count { return Count(int(a) + int(b) + 10) }\nfn (a Pair) [] (i int) int { _ = i return 7 }\n@[comptime]\nfn plain() [2]int { return [1, 2]! }\n@[comptime]\nfn nested() [1][2]int { return [[1, 2]!]! }'
	for helpers, diagnostic in {
		'@[comptime]\nfn consume(a [2]Count) int { return int(a[0] + a[1]) }\nconst bad = consume(plain())':                                                                                                                                                                               'overloaded operator'
		'@[comptime]\nfn counts() [2]Count { return plain() }\n@[comptime]\nfn compute() int { a := counts() return int(a[0] + a[1]) }\nconst bad = compute()':                                                                                                                            'overloaded operator'
		'@[comptime]\nfn compute() int { a := [2]Count(plain()) return int(a[0] + a[1]) }\nconst bad = compute()':                                                                                                                                                                         'overloaded operator'
		'@[comptime]\nfn consume(a [1][2]Count) int { return int(a[0][0] + a[0][1]) }\nconst bad = consume(nested())':                                                                                                                                                                     'overloaded operator'
		'@[comptime]\nfn consume(a [1]Pair) int { return a[0][0] }\nconst bad = consume(nested())':                                                                                                                                                                                        'overloaded indexing'
		'@[comptime]\nfn counts() [2]Count { return [Count(1), Count(2)]! }\n@[comptime]\nfn plain_sum(a [2]int) int { return a[0] + a[1] }\n@[comptime]\nfn compute() int { a := counts() sum := plain_sum(a) if sum != 3 { return 0 } return int(a[0] + a[1]) }\nconst bad = compute()': 'overloaded operator'
	} {
		tc := comptime_const_checker('${declarations}\n${helpers}\nfn main() { _ = bad }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `bad`') && it.msg.contains(diagnostic)), '${helpers}: ${tc.errors}'
	}
}

fn test_comptime_const_array_conversions_apply_leaf_types_and_values() {
	tc := comptime_const_checker('type Count = int\ntype Byte = u8\ntype Signed = i8\nfn (a Count) + (b Count) Count { return Count(int(a) + int(b) + 10) }\n@[comptime]\nfn plain() [2]int { return [1, 2]! }\n@[comptime]\nfn counts() [2]Count { return plain() }\n@[comptime]\nfn plain_copy(a [2]int) [2]int { return a }\n@[comptime]\nfn nested_counts() [1][2]Count { return [plain()]! }\n@[comptime]\nfn bytes() [2]Byte { return [u8(4), u8(255)]! }\n@[comptime]\nfn signed_values() [1]Signed { return [i8(-1)]! }\nconst nominal = counts()\nconst plain_again = plain_copy(nominal)\nconst nested_nominal = nested_counts()\nconst unsigned_values = bytes()\nconst negative_values = signed_values()\nfn main() { _ = nominal _ = plain_again _ = nested_nominal _ = unsigned_values _ = negative_values }')
	assert tc.errors.len == 0, tc.errors.str()
	nominal := tc.comptime_const_values['nominal']!
	plain_again := tc.comptime_const_values['plain_again']!
	nested_nominal := tc.comptime_const_values['nested_nominal']!
	for i in 0 .. 2 {
		assert nominal.elements[i].typ.source_type_id() == flat.source_type_id('Count')
		assert nominal.elements[i].integer == i + 1
		assert plain_again.elements[i].typ.source_type_id() == flat.source_type_int
		assert plain_again.elements[i].integer == i + 1
		assert nested_nominal.elements[0].elements[i].typ.source_type_id() == flat.source_type_id('Count')
		assert nested_nominal.elements[0].elements[i].integer == i + 1
	}
	unsigned_values := tc.comptime_const_values['unsigned_values']!
	assert unsigned_values.elements[0].typ.source_type_id() == flat.source_type_id('Byte')
	assert unsigned_values.elements[1].typ.source_type_id() == flat.source_type_id('Byte')
	assert unsigned_values.elements[0].integer == 4
	assert unsigned_values.elements[1].integer == 255
	negative_values := tc.comptime_const_values['negative_values']!
	assert negative_values.elements[0].typ.source_type_id() == flat.source_type_id('Signed')
	assert negative_values.elements[0].integer == -1
}

fn test_comptime_const_unchanged_array_element_types_do_not_consume_copy_budget() {
	tc := comptime_const_checker('@[comptime]\nfn large_table() [60000]u8 { return [60000]u8{} }\n@[comptime]\nfn echo_array(value [60000]u8) [60000]u8 { return value }\n@[comptime]\nfn repeated_copies() int { values := large_table() mut count := 0 for _ in 0 .. 20 { current := echo_array(values) count += current.len } return count }\nconst answer = repeated_copies()\nfn main() { _ = answer }')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values['answer']!.integer == 1200000
}

fn test_comptime_const_rejects_integer_staging_before_value_branches() {
	for body in [
		'return int((a + b) + (if true { u8(1) } else { u8(1) }))',
		'return int(((a + b)) + (if true { u8(1) } else { u8(1) }))',
		'return int((if true { u8(1) } else { u8(1) }) + (a + b))',
		'return int((a + b) + (u8(2) + (if true { u8(1) } else { u8(1) })))',
		'return int((a + b) + identity(if true { u8(1) } else { u8(1) }))',
		'return add_args(a + b, if true { u8(1) } else { u8(1) })',
		'pair := Pair{x: a + b, y: if true { u8(1) } else { u8(1) }} return pair.x + pair.y',
	] {
		tc := comptime_const_checker('struct Pair { x int\ny int }\n@[comptime]\nfn identity(value u8) u8 { return value }\n@[comptime]\nfn add_args(x int, y int) int { return x + y }\n@[comptime]\nfn compute(a u8, b u8) int { ${body} }\nconst bad = compute(250, 10)\nfn main() { _ = bad }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `bad`') && it.msg.contains('integer operand staging')), '${body}: ${tc.errors}'
		assert 'bad' !in tc.comptime_const_values, body
	}
}

fn test_comptime_const_integer_staging_checks_effective_c_signedness() {
	for body in [
		'return ((1 << 40) + offset) < (if true { i64(-1) } else { i64(-1) })',
		'return (1 << 40) < (if true { i64(-1) } else { i64(-1) })',
	] {
		tc := comptime_const_checker('@[comptime]\nfn compute(offset int) bool { ${body} }\nconst bad = compute(0)\nfn main() { _ = bad }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `bad`') && it.msg.contains('integer operand staging')), '${body}: ${tc.errors}'
		assert 'bad' !in tc.comptime_const_values, body
	}
}

fn test_comptime_const_nested_index_reads_check_staged_integers() {
	local := comptime_const_checker('@[comptime]\nfn compute() int { mut table := [300][1]int{} table[4][0] = 7 table[260][0] = 9 return table[u8(250) + u8(10)][if true { 0 } else { 0 }] }\nconst bad = compute()\nfn main() { _ = bad }')
	assert local.errors.any(it.msg.contains('cannot evaluate const `bad`') && it.msg.contains('integer operand staging')), local.errors.str()
	mut entries := []string{len: 257, init: '[0]!'}
	entries[0] = '[7]!'
	entries[256] = '[9]!'
	nonlocal := comptime_const_checker('const table = [${entries.join(', ')}]!\n@[comptime]\nfn compute() int { return table[u8(255) + u8(1)][if true { 0 } else { 0 }] }\nconst bad = compute()\nfn main() { _ = bad }')
	assert nonlocal.errors.any(it.msg.contains('cannot evaluate const `bad`') && it.msg.contains('integer operand staging')), nonlocal.errors.str()
}

fn test_comptime_const_staging_preserves_explicit_storage_and_unexecuted_branches() {
	for body, expected in {
		'intermediate := a + b return int(intermediate + (if true { u8(1) } else { u8(1) }))':                 5
		'return int((a + b) + ($if true { u8(1) } $else { u8(1) }))':                                          261
		'return int((a + b) + ($if true { u8(1) } $else { if true { u8(1) } else { u8(1) } }))':               261
		'if false && ((a + b) + (if true { u8(1) } else { u8(1) })) > 0 { return 99 } return 7':               7
		'if true || ((a + b) + (if true { u8(1) } else { u8(1) })) > 0 { return 7 } return 99':                7
		'values := [a + b, if true { u8(1) } else { u8(1) }]!\nreturn int(values[0]) + int(values[1])':        5
		'mut table := [300]int{} table[u8(250) + u8(10)] = if true { 7 } else { 0 } return table[4]':          0
		'mut table := [300][1]int{} table[u8(250) + u8(10)][if true { 0 } else { 0 }] = 7 return table[4][0]': 0
	} {
		tc := comptime_const_checker('@[comptime]\nfn compute(a u8, b u8) int { ${body} }\nconst result = compute(250, 10)\nfn main() { _ = result }')
		assert tc.errors.len == 0, '${body}: ${tc.errors}'
		assert tc.comptime_const_values['result']!.integer == expected, body
	}
	for body in [
		'return int((a + b) + (if true { u8(1) } else { u8(1) }))',
		'return int((if true { u8(1) } else { u8(1) }) + (a + b))',
	] {
		tc := comptime_const_checker('@[comptime]\nfn compute(a u8, b u8) int { ${body} }\nconst result = compute(1, 2)\nfn main() { _ = result }')
		assert tc.errors.len == 0, '${body}: ${tc.errors}'
		assert tc.comptime_const_values['result']!.integer == 4, body
	}
}

fn test_comptime_const_rejects_generic_struct_storage() {
	for item in [
		['struct Box[T] { value T\ntag int = 7 }', 'Box[int]', 'return Box[int]{}'],
		['struct Box[T] { value T }', 'Box[int]', 'return Box[int]{}'],
		['struct Box[T] { value T }', 'Box[int]', 'return Box[int]{value: 7}'],
		['struct Box[T] { value T }', 'Box[int]', 'return Box[int]{7}'],
		['struct Box[T] { value T }', 'Box[[2]u8]', 'return Box[[2]u8]{}'],
		['struct Box[T] { value T }\ntype IntBox = Box[int]', 'IntBox', 'return IntBox{}'],
		['struct Box[T] { value T }\nstruct Outer { box Box[int] }', 'Outer', 'return Outer{}'],
		['struct Box[T] { value T }', '[1]Box[int]', 'return [1]Box[int]{}'],
		['union Storage[T] { value T }', 'Storage[int]', 'return Storage[int]{}'],
	] {
		tc := comptime_const_checker('${item[0]}\n@[comptime]\nfn compute() ${item[1]} { ${item[2]} }\nconst bad = compute()\nfn main() { _ = bad }')
		assert tc.errors.any(it.msg.contains('cannot evaluate const `bad`') && it.msg.contains('generic structs are not supported')), '${item}: ${tc.errors}'
		assert 'bad' !in tc.comptime_const_values, item.str()
	}
}

fn test_comptime_const_generic_struct_restriction_is_independent_of_materialization() {
	mut tc := comptime_const_checker('struct Box[T] { value T }\nconst value = Box[int]{}\nfn main() { _ = value }')
	assert tc.errors.len == 0, tc.errors.str()
	name := flat.source_type_id('Box[int]')
	fields := tc.struct_fields_for_type(name)
	assert fields.len == 1
	assert fields[0].typ.source_type_id() == flat.source_type_int
	for materialized in [false, true] {
		if materialized {
			tc.structs[name] = fields
		} else {
			tc.structs.delete(name)
		}
		if _ := tc.eval_comptime_const(tc.const_exprs['value']!) {
			assert false, 'generic struct was evaluated with materialized=${materialized}'
		} else {
			assert err.msg().contains('generic structs are not supported'), err.msg()
		}
	}
}

fn test_comptime_const_plain_structs_and_unexecuted_generic_values() {
	for body, expected in {
		'box := Plain{} return box.value':                                  0
		'box := Plain{value: 7} return box.value':                          7
		'box := Plain{7} return box.value':                                 7
		'box := PlainAlias{value: 9} return box.value':                     9
		'if false { box := Box[int]{value: 9} return box.value } return 7': 7
	} {
		tc := comptime_const_checker('struct Plain { value int }\ntype PlainAlias = Plain\nstruct Box[T] { value T }\n@[comptime]\nfn compute() int { ${body} }\nconst result = compute()\nfn main() { _ = result }')
		assert tc.errors.len == 0, '${body}: ${tc.errors}'
		assert tc.comptime_const_values['result']!.integer == expected, body
	}
}

fn test_comptime_const_empty_structs_require_declared_metadata() {
	mut tc := comptime_const_checker('struct Empty {}\n@[comptime]\nfn make_empty() Empty { return Empty{} }\nconst empty = make_empty()\nfn main() { _ = empty }')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values['empty']!.fields.len == 0
	tc.structs.delete(flat.source_type_id('Empty'))
	if _ := tc.eval_comptime_const(tc.const_exprs['empty']!) {
		assert false, 'missing struct metadata was treated as an empty declaration'
	} else {
		assert err.msg().contains('struct field metadata is unavailable'), err.msg()
	}
}

fn test_comptime_const_ordinary_integer_queries_apply_declared_storage() {
	for declarations in [
		'const value = u16(65530) + u16(10)\nconst chained = value + u16(65533)',
		'const chained = value + u16(65533)\nconst value = u16(65530) + u16(10)',
	] {
		tc := comptime_const_checker('type Word = u16\n${declarations}\nconst alias_value = Word(65530) + Word(10)\nconst signed_value = i8(120) + i8(10)\nconst ratio = (u16(65530) + u16(10)) / u16(2)\nconst remainder = (u16(65530) + u16(10)) % u16(3)\nconst narrow_cast = int(u8(250 + 50))\nconst alias_cast = int(Word(65530 + 10))\nconst narrow_result = int(i8(-128) / i8(-1))\nconst unsigned_ratio = u64(0xffffffffffffffff) / u64(3)\nfn main() { _ = [value]u8{} _ = [chained]u8{} _ = [alias_value]u8{} _ = [ratio]u8{} _ = [remainder]u8{} _ = [narrow_cast]u8{} _ = [alias_cast]u8{} _ = signed_value _ = narrow_result _ = unsigned_ratio }')
		assert tc.errors.len == 0, tc.errors.str()
		assert tc.comptime_const_values.len == 0
		for name, expected in {
			'value':          4
			'chained':        1
			'alias_value':    4
			'signed_value':   -126
			'ratio':          2
			'remainder':      1
			'narrow_cast':    44
			'alias_cast':     4
			'narrow_result':  -128
			'unsigned_ratio': 6148914691236517205
		} {
			assert tc.const_int_value(name, [])? == expected, name
		}
		assert tc.const_int_value('value + chained', [])? == 5
		assert tc.const_int_value('main.value', [])? == 4
		assert tc.const_int_value('int(u8(250 + 50))', [])? == 44
		mut context := ConstIntContext{ check_only: true }
		assert tc.const_int_value_with_context('value', tc.cur_module, [], mut context)? == 4
		assert context.evaluator == unsafe { nil }
		assert context.failure == ''
	}
}

fn test_comptime_const_ordinary_integer_queries_use_declaration_file() ! {
	root := os.join_path(os.vtmp_dir(), 'comptime_const_ordinary_bounds_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	helper := os.join_path(root, 'helper.v')
	main := os.join_path(root, 'main.v')
	words := os.join_path(root, 'words.v')
	os.write_file(words, 'module words\npub type Word = u16')!
	for translated in [false, true] {
		prefix := if translated { '@[translated]\n' } else { '' }
		os.write_file(helper, '${prefix}module bounds\nimport words as storage\npub const value = u16(65530) + u16(10)\npub const chained = value + u16(65533)\npub const ratio = (u16(65530) + u16(10)) / u16(2)\npub const remainder = (u16(65530) + u16(10)) % u16(3)\npub const alias_cast = int(storage.Word(65530 + 10))')!
		os.write_file(main, 'module main\nimport bounds as sizes\nconst local_value = sizes.value + u16(65533)\nfn main() { _ = [sizes.value]u8{} _ = [sizes.chained]u8{} _ = [sizes.ratio]u8{} _ = [sizes.alias_cast]u8{} _ = local_value }')!
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_files([words, helper, main])
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		tc.check_semantics()
		assert tc.errors.len == 0, tc.errors.str()
		tc.cur_file = main
		tc.cur_module = 'main'
		assert tc.const_int_value('sizes.value', [])? == if translated { 65540 } else { 4 }
		assert tc.const_int_value('sizes.chained', [])? == if translated { 131073 } else { 1 }
		assert tc.const_int_value('sizes.ratio', [])? == if translated { 32770 } else { 2 }
		assert tc.const_int_value('sizes.remainder', [])? == if translated { 2 } else { 1 }
		assert tc.const_int_value('sizes.alias_cast', [])? == if translated { 65540 } else { 4 }
		assert tc.const_int_value_in_module('value', 'bounds', [])? == if translated {
			65540
		} else {
			4
		}
		assert tc.const_int_value('local_value', [])? == if translated { 131073 } else { 1 }
		assert tc.comptime_const_values.len == 0
	}
}

fn test_comptime_const_requirement_scan_ignores_callback_bodies() {
	for initializer in [
		'fn () int { return seed() }',
		'(fn () int { return seed() })',
		'|| seed()',
	] {
		tc := comptime_const_checker('@[comptime]\nfn seed() int { return 7 }\nconst callback = ${initializer}\nconst direct = seed()\nfn main() { _ = callback() _ = direct }')
		assert tc.errors.len == 0, '${initializer}: ${tc.errors}'
		assert !tc.requires_comptime_const(tc.const_exprs['callback']!), initializer
		assert 'callback' !in tc.comptime_const_values, initializer
		assert tc.requires_comptime_const(tc.const_exprs['direct']!), initializer
		assert tc.comptime_const_values['direct']!.integer == 7
	}
}

fn test_comptime_const_ordinary_integer_queries_before_annotations() ! {
	original_bits := platform_int_bits()
	set_platform_int_bits(64)
	defer { set_platform_int_bits(original_bits) }
	path := os.join_path(os.vtmp_dir(), 'comptime_const_unchecked_bounds_${os.getpid()}.v')
	os.write_file(path, 'const divisor = 2\nconst named_ratio = (u16(65530) + u16(10)) / divisor\nconst parenthesized_ratio = (u16(65530) + u16(10)) / (2)\nconst wide_remainder = 1 + int((u32(4294967295) + u32(1)) % u64(3))\nconst wide_division = 1 + int((u32(4294967295) + u32(1)) / u64(2))\nconst unary_division = 1 + int((-u32(1) + u32(1)) / u64(2))\nconst sizeof_remainder = 1 + int((sizeof(u8) + u32(4294967295)) % u64(3))\nfn main() { _ = [named_ratio]u8{} _ = [parenthesized_ratio]u8{} _ = [wide_remainder]u8{} _ = [wide_division]u8{} _ = [unary_division]u8{} }')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	for checked in [false, true] {
		if checked { tc.check_semantics() }
		assert tc.errors.len == 0, tc.errors.str()
		for name, expected in {
			'named_ratio':         2
			'parenthesized_ratio': 2
			'wide_remainder':      1
			'wide_division':       1
			'unary_division':      1
			'sizeof_remainder':    2
		} {
			assert tc.const_int_value(name, [])? == expected, '${name}, checked=${checked}'
		}
		assert tc.comptime_const_values.len == 0
	}
}

fn test_comptime_const_ordinary_shift_queries_use_native_widths() ! {
	original_bits := platform_int_bits()
	defer { set_platform_int_bits(original_bits) }
	path := os.join_path(os.vtmp_dir(), 'comptime_const_shift_bounds_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	source := [
		'type Byte = u8',
		'const logical_bound = 1 + int((-5 >>> 1) % u32(3))',
		'const logical_literal = -5 >>> 1',
		'const logical_byte = i8(-5) >>> 1',
		'const logical_word = i16(-5) >>> 1',
		'const signed_right = i8(-5) >> 1',
		'const narrow_chain = (u8(200) << 1) >> 1',
		'const signed_chain = (i8(100) << 1) >> 1',
		'const alias_chain = (Byte(200) << 1) >> 1',
		'const wide_count_chain = (u8(200) << u64(1)) >> u64(1)',
		'const unsigned_high = u64(0xffffffffffffffff) >> 63',
		'const unsigned_word = u32(0xffffffff) >>> 31',
		'const large_left = 1 + int(u8(1) << int(8))',
		'const large_right = 1 + int(u8(255) >> int(8))',
		'const large_logical = 1 + int(i8(-1) >>> int(8))',
		'const negative_left = 1 + int(u16(3) << -1)',
		'const negative_right = 1 + int(i16(-3) >> -1)',
		'const negative_logical = 1 + int(i16(-3) >>> -1)',
		'const wide_literal_remainder = 1 + int((1 << 63) % u64(3))',
		'const count_a = u8(250)',
		'const count_b = u8(37)',
		'const count_unit = u8(1)',
		'const stored_count = (count_a + count_b) / count_unit',
		'const literal_named = 1 << stored_count',
		'const literal_cast = 1 << u8(31)',
		'const literal_raw_counts = 1 << ((count_a + count_b) / count_unit)',
		'const literal_wide = 1 << 40',
		'fn main() { _ = [logical_bound]u8{} _ = [narrow_chain]u8{} _ = [large_left]u8{} }',
	].join('\n')
	os.write_file(path, source)!
	for bits in [32, 64] {
		set_platform_int_bits(bits)
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		for checked in [false, true] {
			if checked { tc.check_semantics() }
			assert tc.errors.len == 0, tc.errors.str()
			for name, expected in {
				'logical_bound':          3
				'logical_literal':        2147483645
				'logical_byte':           125
				'logical_word':           32765
				'signed_right':           -3
				'narrow_chain':           72
				'signed_chain':           -28
				'alias_chain':            72
				'wide_count_chain':       72
				'unsigned_high':          1
				'unsigned_word':          1
				'large_left':             1
				'large_right':            1
				'large_logical':          1
				'negative_left':          1
				'negative_right':         1
				'negative_logical':       1
				'wide_literal_remainder': 3
				'stored_count':           31
			} {
				assert tc.const_int_value(name, [])? == expected, '${name}, bits=${bits}, checked=${checked}'
			}
			for name, expected_type in {
				'literal_named':      flat.source_type_u64
				'literal_cast':       flat.source_type_int
				'literal_raw_counts': flat.source_type_int
				'literal_wide':       flat.source_type_u64
			} {
				mut context := ConstIntContext{ check_only: true }
				typ := tc.const_int_c_expression_type(tc.const_exprs[name]!, 0, tc.cur_module, [], mut context)?
				assert typ.source_type_id() == expected_type, '${name}, bits=${bits}, checked=${checked}'
				assert context.evaluator == unsafe { nil }
				assert context.failure == ''
			}
			assert tc.comptime_const_values.len == 0
		}
	}
}

fn test_comptime_const_ordinary_shift_queries_keep_translated_promotions() {
	for translated in [false, true] {
		prefix := if translated { '@[translated]\n' } else { '' }
		tc := comptime_const_checker('${prefix}module main\nconst chained = (u8(200) << 1) >> 1\nconst large = u8(1) << int(8)\nfn main() { _ = chained _ = large }')
		assert tc.errors.len == 0, tc.errors.str()
		assert tc.const_int_value('chained', [])? == if translated { 200 } else { 72 }
		assert tc.const_int_value('large', [])? == if translated { 256 } else { 0 }
		assert tc.comptime_const_values.len == 0
	}
}

fn test_comptime_const_unsigned_inclusive_loops_match_post_guards() {
	for body, expected in {
		'mut n := 0 for i := u8(254); i <= u8(255); i++ { n++ if i == 0 { return 99 } } return n':                      2
		'mut n := 0 for i := u8(1); i >= u8(0); i-- { n++ if i == 255 { return 99 } } return n':                        2
		'mut n := 0 for i := u8(254); i <= u8(255); i++ { n++ continue } return n':                                     2
		'mut n := 0 for i := u8(1); i >= u8(0); i-- { n++ continue } return n':                                         2
		'mut i := u8(254) for ; i <= u8(255); i++ {} return int(i)':                                                    255
		'mut i := u8(1) for ; i >= u8(0); i-- {} return int(i)':                                                        0
		'mut n := 0 for i := u8(0); i <= u8(255); i++ { n++ i = 255 if n > 2 { return 99 } } return n':                 1
		'mut n := 0 for i := u8(255); i <= u8(255); i++ { n++ if n == 1 { i = 254 } if n > 2 { return 99 } } return n': 2
		'mut n := 0 for i := u8(0); i >= u8(0); i-- { n++ if n == 1 { i = 1 } if n > 2 { return 99 } } return n':       2
		'for i := u8(255); i <= u8(255); i++ { break } return 7':                                                       7
		'for i := u8(255); i <= u8(255); i++ { return 7 } return 0':                                                    7
		'for i := u8(254); u8(255) >= i; i++ { if i == 0 { return 99 } } return 2':                                     99
		'for i := u8(254); (i <= u8(255)); i++ { if i == 0 { return 99 } } return 2':                                   99
		'for i := u8(254); i <= u8(255); i += 1 { if i == 0 { return 99 } } return 2':                                  99
	} {
		for constant_first in [false, true] {
			decl := '@[comptime]\nfn compute() int { ${body} }\n'
			constant := 'const n = compute()\n'
			source := if constant_first { constant + decl } else { decl + constant }
			tc := comptime_const_checker(source + 'fn main() { _ = n }')
			assert tc.errors.len == 0, '${body}, constant_first=${constant_first}: ${tc.errors}'
			assert tc.comptime_const_values['n']!.integer == expected, '${body}, constant_first=${constant_first}'
		}
	}
}

fn test_comptime_const_unsigned_inclusive_loop_widths_and_rune_exclusion() ! {
	original_bits := platform_int_bits()
	defer { set_platform_int_bits(original_bits) }
	path := os.join_path(os.vtmp_dir(), 'comptime_const_loop_widths_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for bits in [32, 64] {
		set_platform_int_bits(bits)
		for item in [
			['u8', '254', '255'],
			['u16', '65534', '65535'],
			['u32', '4294967294', '4294967295'],
			['u64', '0xfffffffffffffffe', '0xffffffffffffffff'],
			['uint', '-2', '-1'],
			['usize', '-2', '-1'],
		] {
			os.write_file(path, '@[comptime]\nfn compute() int { mut n := 0 for i := ${item[0]}(${item[1]}); i <= ${item[0]}(${item[2]}); i++ { n++ if i == 0 { return 99 } } return n }\nconst n = compute()\nfn main() { _ = n }')!
			mut p := parser.Parser.new(pref.new_preferences())
			a := p.parse_file(path)
			if item[0] == 'usize' {
				assert p.diagnostics.len == 2, p.diagnostics.str()
				assert p.diagnostics.all(it.severity == 'warning:' && it.message == '`usize` is deprecated; use `uint` instead'), p.diagnostics.str()
			} else {
				assert p.diagnostics.len == 0, p.diagnostics.str()
			}
			mut tc := TypeChecker.new(a)
			tc.collect(a)
			tc.check_semantics()
			assert tc.errors.len == 0, '${item}, bits=${bits}: ${tc.errors}'
			assert tc.comptime_const_values['n']!.integer == 2, '${item}, bits=${bits}'
		}
		rune_loop := comptime_const_checker('@[comptime]\nfn compute() int { for i := rune(0xfffffffe); i <= rune(0xffffffff); i++ { if i == 0 { return 99 } } return 2 }\nconst n = compute()\nfn main() { _ = n }')
		assert rune_loop.errors.len == 0, rune_loop.errors.str()
		assert rune_loop.comptime_const_values['n']!.integer == 99
	}
}

fn test_comptime_const_alias_inclusive_loop_boundary_restriction() {
	for body in [
		'for i := Byte(254); i <= Byte(255); i++ { if i == 0 { return 99 } } return 2',
		'value := Byte(254) for i := value; i <= Byte(255); i++ { if i == 0 { return 99 } } return 2',
		'for i := start; i <= Byte(255); i++ { if i == 0 { return 99 } } return 2',
		'for i := Byte(1); i >= Byte(0); i-- { if i == 255 { return 99 } } return 2',
	] {
		decl := '@[comptime]\nfn compute(start Byte) int { ${body} }\n'
		for constant_first in [false, true] {
			constant := 'const n = compute(Byte(254))\n'
			source := 'type Byte = u8\n' + if constant_first {
				constant + decl
			} else {
				decl + constant
			}
			tc := comptime_const_checker(source + 'fn main() { _ = n }')
			assert tc.errors.any(it.msg.contains('cannot evaluate const `n`') && it.msg.contains('alias counters are not supported')), '${body}, constant_first=${constant_first}: ${tc.errors}'
			assert 'n' !in tc.comptime_const_values
		}
		ordinary := comptime_const_checker('type Byte = u8\n' + decl + 'fn main() { _ = compute(Byte(254)) }')
		assert ordinary.errors.len == 0, ordinary.errors.str()
		assert ordinary.comptime_const_values.len == 0
	}
	for body, expected in {
		'mut n := 0 for i := Byte(1); i <= Byte(2); i++ { n++ } return n':                                                2
		'for i := Byte(255); i <= Byte(255); i++ { break } return 7':                                                     7
		'for i := Byte(255); i <= Byte(255); i++ { return 7 } return 0':                                                  7
		'mut n := 0 for i := Byte(255); i <= Byte(255); i++ { n++ if n == 1 { i = Byte(254) } else { break } } return n': 2
		'for i := Byte(255); i <= Byte(254); i++ { return 99 } return 7':                                                 7
	} {
		tc := comptime_const_checker('type Byte = u8\n@[comptime]\nfn compute() int { ${body} }\nconst n = compute()\nfn main() { _ = n }')
		assert tc.errors.len == 0, '${body}: ${tc.errors}'
		assert tc.comptime_const_values['n']!.integer == expected, body
	}
}

fn test_comptime_const_fixed_literals_use_expected_types_before_body_checking() ! {
	path := os.join_path(os.vtmp_dir(), 'comptime_const_literal_context_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	declarations := 'type Word = u16\ntype BytePair = [2]u8\nstruct Record { bytes [2]u8\nnested [1][2]u16 }\n'
	helpers := '
@[comptime]
fn bytes() [2]u8 { return [1, 2]! }
@[comptime]
fn nested() [2][2]u16 { return [[3, 4]!, [5, 6]!]! }
@[comptime]
fn take(values [2]u8) [2]u8 { return values }
@[comptime]
fn choose(first bool) [2]u8 { return if first { [7, 8]! } else if !first { [9, 10]! } else { [0, 0]! } }
@[comptime]
fn parenthesized() [2]u8 { return ([11, 12]!) }
@[comptime]
fn record() Record { return Record{bytes: [13, 14]!, nested: [[15, 16]!]!} }
@[comptime]
fn assigned() [2]u8 { mut values := [2]u8{} values = [17, 18]!
 return values }
@[comptime]
fn words() [2]Word { return [19, 20]! }
@[comptime]
fn casted() [2]u8 { return [2]u8([21, 22]!) }
@[comptime]
fn restored() [2]u8 { unused := nested() if unused[0][0] != 3 { return [0, 0]! } return [23, 24]! }
@[comptime]
fn named() BytePair { return [25, 26]! }
@[comptime]
fn typed_elements() [2]u8 { return [u16(27), u16(28)]! }
'
	constants := 'const direct = bytes()\nconst matrix = nested()\nconst argument = take([5, 6]!)\nconst yes = choose(true)\nconst no = choose(false)\nconst parens = parenthesized()\nconst fields = record()\nconst local = assigned()\nconst aliases = words()\nconst cast = casted()\nconst restored_frame = restored()\nconst named_alias = named()\nconst typed = typed_elements()\n'
	for constant_first in [false, true] {
		source := declarations + if constant_first {
			constants + helpers
		} else {
			helpers + constants
		}
		os.write_file(path, source + 'fn main() { _ = direct _ = matrix _ = argument _ = yes _ = no _ = parens _ = fields _ = local _ = aliases _ = cast _ = restored_frame _ = named_alias _ = typed }')!
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		for checked in [false, true] {
			if checked { tc.check_semantics() }
			assert tc.errors.len == 0, '${constant_first}, ${checked}: ${tc.errors}'
			for name, expected in {
				'direct':         [1, 2]
				'argument':       [5, 6]
				'yes':            [7, 8]
				'no':             [9, 10]
				'parens':         [11, 12]
				'local':          [17, 18]
				'cast':           [21, 22]
				'restored_frame': [23, 24]
				'named_alias':    [25, 26]
				'typed':          [27, 28]
			} {
				value := tc.eval_comptime_const(tc.const_exprs[name]!)!
				assert value.elements.map(int(it.integer)) == expected, '${name}, ${constant_first}, ${checked}'
				assert value.elements.all(it.typ.source_type_id() == flat.source_type_u8), '${name}, ${constant_first}, ${checked}'
			}
			matrix := tc.eval_comptime_const(tc.const_exprs['matrix']!)!
			assert matrix.elements[0].elements.map(int(it.integer)) == [3, 4]
			assert matrix.elements[1].elements.map(int(it.integer)) == [5, 6]
			assert matrix.elements[0].elements.all(it.typ.source_type_id() == flat.source_type_u16)
			assert matrix.elements[1].elements.all(it.typ.source_type_id() == flat.source_type_u16)
			fields := tc.eval_comptime_const(tc.const_exprs['fields']!)!
			assert fields.fields['bytes']!.elements.map(int(it.integer)) == [13, 14]
			assert fields.fields['nested']!.elements[0].elements.map(int(it.integer)) == [
				15,
				16,
			]
			aliases := tc.eval_comptime_const(tc.const_exprs['aliases']!)!
			assert aliases.elements.map(int(it.integer)) == [19, 20]
			assert aliases.elements.all(it.typ.source_type_id() == flat.source_type_id('Word'))
			named := tc.eval_comptime_const(tc.const_exprs['named_alias']!)!
			assert named.typ.source_type_id() == flat.source_type_id('BytePair')
		}
	}
}

fn test_comptime_const_contextual_array_storage_and_single_evaluation() {
	for constant_first in [false, true] {
		helpers := '
struct Record { mut: bytes [2]u8 }
@[comptime]
fn once() [2]u8 {
 mut count := 0
 return [if true { count++; u8(count) } else { u8(0) },
  if true { count++; u8(count) } else { u8(0) }]!
}
@[comptime]
fn update() [2]u8 {
 mut records := [2]Record{}
 records[1].bytes = [3, 4]!
 mut matrix := [2][2]u8{}
 matrix[1] = [5, 6]!
 if records[0].bytes[0] != 0 || matrix[0][0] != 0 { return [0, 0]! }
 return [records[1].bytes[0], matrix[1][1]]!
}
@[comptime]
fn branches(first bool) [2]u8 {
 mut values := [2]u8{}
 values = if first { [7, 8]! } else { [9, 10]! }
 return values
}
'
		constants := 'const counted = once()\nconst stored = update()\nconst then_value = branches(true)\nconst else_value = branches(false)\n'
		source := if constant_first { constants + helpers } else { helpers + constants }
		tc := comptime_const_checker(source + 'fn main() { _ = counted _ = stored _ = then_value _ = else_value }')
		assert tc.errors.len == 0, '${constant_first}: ${tc.errors}'
		for name, expected in {
			'counted':    [1, 2]
			'stored':     [3, 6]
			'then_value': [7, 8]
			'else_value': [9, 10]
		} {
			value := tc.comptime_const_values[name]!
			assert value.elements.map(int(it.integer)) == expected, '${name}, ${constant_first}'
			assert value.elements.all(it.typ.source_type_id() == flat.source_type_u8)
		}
	}
}

fn test_comptime_const_literal_signature_context_uses_declaration_file() ! {
	dir := os.join_path(os.vtmp_dir(), 'comptime_const_literal_signature_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	storage := os.join_path(dir, 'storage.v')
	helper := os.join_path(dir, 'helper.v')
	main := os.join_path(dir, 'main.v')
	os.write_file(storage, 'module storage\npub type Word = u16\npub const width = 2')!
	os.write_file(helper, 'module helper\nimport storage as shape\npub const width = 3\n@[comptime]\npub fn take(values [shape.width]shape.Word) [shape.width]shape.Word { return values }\n@[comptime]\npub fn make() [shape.width]shape.Word { return [3, 4]! }')!
	os.write_file(main, 'module main\nimport helper as source\nconst width = 4\nconst values = source.take([1, 2]!)\nconst made = source.make()\nfn main() { _ = values _ = made }')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([storage, helper, main])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	for checked in [false, true] {
		if checked { tc.check_semantics() }
		assert tc.errors.len == 0, '${checked}: ${tc.errors}'
		tc.cur_file = main
		tc.cur_module = 'main'
		for name, expected in {
			'values': [1, 2]
			'made':   [3, 4]
		} {
			value := tc.eval_comptime_const(tc.const_exprs[name]!)!
			assert value.elements.map(int(it.integer)) == expected, '${name}, ${checked}'
			assert value.elements.all(it.typ.source_type_id() == flat.source_type_id('storage.Word'))
		}
	}
}

fn test_comptime_const_literal_context_keeps_array_conversion_and_nominal_checks() {
	declarations := 'type Count = int\ntype Pair = [2]int\nfn (a Count) + (b Count) Count { return Count(int(a) + int(b) + 10) }\nfn (a Pair) [] (i int) int { _ = i return 7 }\n@[comptime]\nfn plain() [2]int { return [1, 2]! }\n'
	for helpers, diagnostic in {
		'@[comptime]\nfn compute() [2]u8 { return plain() }':                                                                                           'unsupported conversion'
		'@[comptime]\nfn compute() [2]u8 { return [2]int{} }':                                                                                          'unsupported conversion'
		'@[comptime]\nfn compute() [2]u8 { return [1]! }':                                                                                              'unsupported conversion'
		'@[comptime]\nfn take(values [2]u8) [2]u8 { return values }\n@[comptime]\nfn compute() [2]u8 { return take(plain()) }':                         'unsupported conversion'
		'@[comptime]\nfn counts() [2]Count { return [1, 2]! }\n@[comptime]\nfn compute() int { values := counts() return int(values[0] + values[1]) }': 'overloaded operator'
		'@[comptime]\nfn pair() Pair { return [1, 2]! }\n@[comptime]\nfn compute() int { values := pair() return values[0] }':                          'overloaded indexing'
	} {
		for constant_first in [false, true] {
			constant := 'const bad = compute()\n'
			source := declarations + if constant_first {
				constant + helpers
			} else {
				helpers + '\n' + constant
			}
			tc := comptime_const_checker(source + '\nfn main() { _ = bad }')
			assert tc.errors.any(it.msg.contains('cannot evaluate const `bad`') && it.msg.contains(diagnostic)), '${helpers}, ${constant_first}: ${tc.errors}'
			assert 'bad' !in tc.comptime_const_values
		}
	}
	invalid_literal := comptime_const_checker('@[comptime]\nfn bytes() [2]u8 { return [256, 2]! }\nconst values = bytes()\nfn main() { _ = values }')
	assert invalid_literal.errors.any(it.msg.contains('invalid array element')), invalid_literal.errors.str()
}
