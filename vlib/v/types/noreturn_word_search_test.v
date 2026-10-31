module types

import v.flat

fn noreturn_byte_search_reference(tc &TypeChecker, name string) bool {
	if resolved_name_never_returns(name) {
		return true
	}
	if name in tc.a.noreturn_fns {
		return true
	}
	mut has_dot := false
	for i := name.len - 1; i >= 0; i-- {
		if name[i] == `.` {
			if has_dot && tc.a.noreturn_fns[name[i + 1..]] {
				return true
			}
			has_dot = true
		}
	}
	return false
}

fn test_noreturn_word_search_matches_byte_search_with_long_spans() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	for declarations in [map[string]bool{}, {
		'pkg.fail':     true
		'pkg.disabled': false
		'.fail':        true
		'fail':         true
		'':             true
	}] {
		a.noreturn_fns = declarations.clone()
		for padding in 0 .. 48 {
			stem := 'a'.repeat(padding)
			for tail in ['', '.', '..', 'pkg.fail', 'pkg.disabled', 'notpkg.fail', '.fail', 'fail',
				'panic', 'exit', 'C.exit'] {
				for prefix in ['', '.', '..', stem, stem + '.', stem + '..', stem + '.long_module.'] {
					name := prefix + tail
					assert tc.name_never_returns(name) == noreturn_byte_search_reference(tc, name), name
				}
			}
		}
	}
}
