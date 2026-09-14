module types

import os
import v.parser
import v.pref

fn test_synthetic_vsh_import_has_no_source_diagnostics() {
	path := os.join_path(os.vtmp_dir(), 'v3_synthetic_vsh_import_${os.getpid()}.vsh')
	os.write_file(path, '// a comment without it\nfn helper() {}\n')!
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_import_diagnostics()
	tc.check_unused_import_diagnostics()
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.notices.len == 0, tc.notices.str()
}

fn test_explicit_import_keeps_source_diagnostics() {
	path := os.join_path(os.vtmp_dir(), 'v3_explicit_import_${os.getpid()}.v')
	os.write_file(path, 'import time math\n')!
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_import_diagnostics()
	assert tc.errors.any(it.msg == 'cannot import multiple modules at a time'), tc.errors.str()
}

fn test_explicit_unused_import_keeps_source_diagnostics() {
	path := os.join_path(os.vtmp_dir(), 'v3_explicit_unused_import_${os.getpid()}.v')
	os.write_file(path, 'import os\n')!
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_import_diagnostics()
	tc.check_unused_import_diagnostics()
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.notices.any(it.msg.contains("module 'os' is imported but never used")), tc.notices.str()
}
