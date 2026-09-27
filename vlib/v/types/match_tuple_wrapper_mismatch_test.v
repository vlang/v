module types

import os
import v.parser
import v.pref

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
	}
}
