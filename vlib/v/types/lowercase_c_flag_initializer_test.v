module types

import os
import v.parser
import v.pref

fn test_c_flag_initializers_require_known_bit_positions() {
	for i, backing in ['', ' as u64'] {
		path := os.join_path(os.vtmp_dir(), 'c_flag_initializer_${i}_${os.getpid()}.v')
		os.write_file(path, '@[flag]\nenum CFlags${backing} { first = C.enum_first_opt second }\n@[flag]\nenum KnownFlags${backing} { first = 3 second }\nfn main() {}') or { panic(err) }
		defer { os.rm(path) or {} }
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		_ = tc.check_semantics_opt(false)
		assert tc.errors.any(it.msg == 'flag enum values must be known at compile time'), tc.errors.str()
		assert tc.errors.len == 1, tc.errors.str()
	}
}
