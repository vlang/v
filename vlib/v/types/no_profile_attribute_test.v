module types

import os
import v.parser
import v.pref

fn no_profile_attribute_errors(index int, source string) []TypeError {
	path := os.join_path(os.vtmp_dir(), 'no_profile_attribute_${os.getpid()}_${index}.v')
	os.write_file(path, 'module main\n' + source + '\n') or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	return tc.errors.clone()
}

fn test_no_profile_accepts_functions_methods_and_generics() {
	source := '@[no_profile]\nfn plain() {}\nstruct Worker {}\n@[no_profile]\nfn (w Worker) run() {}\n@[no_profile]\nfn identity[T](x T) T { return x }\nfn main() { plain(); Worker{}.run(); assert identity(42) == 42 }'
	errors := no_profile_attribute_errors(0, source)
	assert errors.len == 0, errors.str()
}

fn test_no_profile_rejects_non_function_targets() {
	for index, source in [
		'@[no_profile]\nstruct Worker {}\nfn main() {}',
		'struct Worker {\n@[no_profile]\nvalue int\n}\nfn main() {}',
		'@[no_profile]\nenum State { ready }\nfn main() {}',
		'enum State {\n@[no_profile]\nready\n}\nfn main() {}',
		'@[no_profile]\ntype Number = int\nfn main() {}',
		'@[no_profile]\nconst answer = 42\nfn main() {}',
	] {
		errors := no_profile_attribute_errors(index + 1, source)
		assert errors.any(it.msg == 'attribute `no_profile` can only be applied to functions and methods'), errors.str()
	}
}

fn test_no_profile_rejects_arguments() {
	for index, attribute in ['no_profile: true', "no_profile: 'yes'"] {
		errors := no_profile_attribute_errors(index + 20, '@[${attribute}]\nfn ignored() {}\nfn main() { ignored() }')
		assert errors.any(it.msg == 'attribute `no_profile` does not accept arguments'), errors.str()
	}
}
