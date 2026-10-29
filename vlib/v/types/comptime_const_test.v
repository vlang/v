module types

import os
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
