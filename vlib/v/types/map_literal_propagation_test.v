module types

import os
import v.parser
import v.pref

fn check_map_literal_propagation(source string) &TypeChecker {
	path := os.join_path(os.vtmp_dir(), 'map_propagation_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	return tc
}

fn test_const_map_rejects_result_propagation() {
	tc := check_map_literal_propagation("const m = {'vlang': 'cool'}!\nfn main() { println(m['vlang']) }\n")
	assert tc.errors.any(it.msg.contains('unexpected `!`, expression of type `map[string]string`')), tc.errors.str()
}

fn test_local_map_rejects_result_propagation() {
	tc := check_map_literal_propagation("fn main() { m := {'vlang': 'cool'}!; println(m) }\n")
	assert tc.errors.any(it.msg.contains('unexpected `!`, expression of type `map[string]string`')), tc.errors.str()
	assert !tc.errors.any(it.msg.contains('cannot copy map')), tc.errors.str()
}

fn test_map_literal_without_propagation_is_valid() {
	tc := check_map_literal_propagation("const m = {'vlang': 'cool'}\nfn main() { println(m['vlang']) }\n")
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_parenthesized_map_rejects_result_propagation() {
	for source in [
		"const m = (({'vlang': 'cool'}))!\nfn main() { println(m['vlang']) }\n",
		"fn main() { m := (({'vlang': 'cool'}))!; println(m) }\n",
	] {
		tc := check_map_literal_propagation(source)
		assert tc.errors.any(it.msg.contains('unexpected `!`, expression of type `map[string]string`')), tc.errors.str()
		assert !tc.errors.any(it.msg.contains('cannot copy map')), tc.errors.str()
	}
}
