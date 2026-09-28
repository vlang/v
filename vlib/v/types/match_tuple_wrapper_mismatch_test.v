module types

import os
import v.flat
import v.parser
import v.pref

fn test_none_error_pair_keeps_option_constraint_until_concrete_type() {
	tc := TypeChecker.new(&flat.FlatAst{})
	none_type := Type(None{})
	error_type := Type(Interface{ name: 'IError' })
	partial := tc.promoted_multi_tail_type(none_type, error_type) or { panic('missing promotion') }
	assert partial is None
	result_type := Type(ResultType{ base_type: Type(string_) })
	if _ := tc.promoted_multi_tail_type(partial, result_type) {
		assert false
	}
	option_type := Type(OptionType{ base_type: Type(string_) })
	promoted_option := tc.promoted_multi_tail_type(partial, option_type) or { panic('missing option') }
	assert promoted_option == option_type
}

fn test_match_tuple_branches_reject_optional_slot_mismatch_in_both_orders() {
	path := os.join_path(os.vtmp_dir(), 'v3_match_tuple_wrapper_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for arms in [
		'true { wrapped() } else { 2, "x" }',
		'true { 2, "x" } else { wrapped() }',
	] {
		source := 'fn wrapped() (int, ?string) { return 1, ?string(none) }\nfn pick(flag bool) { a, b := match flag { ${arms} }; _ = a; _ = b }\n'
		os.write_file(path, source)!
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		_ = tc.check_semantics_opt(false)
		assert tc.errors.any(it.msg.contains('return type mismatch')), tc.errors.str()
		assert !tc.errors.any(it.msg.contains('undefined')), tc.errors.str()
	}
}

fn test_invalid_all_none_match_slot_keeps_other_bindings() {
	path := os.join_path(os.vtmp_dir(), 'v3_match_all_none_slot_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'fn pick(flag bool) { a, b := match flag { true { none, 1 } else { none, 2 } }; _ = a; _ = b }\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.any(it.msg.contains('cannot assign a `none` value')), tc.errors.str()
	assert !tc.errors.any(it.msg.contains('undefined')), tc.errors.str()
}
