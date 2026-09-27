module types

import os
import v.parser
import v.pref

fn test_const_cycle_check_still_checks_function_arguments() {
	path := os.join_path(os.vtmp_dir(), 'v3_const_function_cycle_${os.getpid()}.v')
	os.write_file(path, 'const answer = answer(answer)\nfn answer(x int) int { return x }\nfn main() {}\n')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.any(it.msg == 'cycle in constant `answer`'), tc.errors.str()
}
