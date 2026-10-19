module parser

import os
import v.pref
import v.scanner
import v.token

fn test_comptime_prepass_resolver_preserves_line_and_column() {
	source := 'module main\n\n  const value = 42\n'
	prefs := pref.new_preferences()
	mut p := Parser.new(prefs)
	pos := source.index('const') or { panic('missing declaration') }
	assert p.resolve_parallel_comptime_prepass_at_token('@LINE', pos, source, 'prepass.v', 'main') == "'3'"
	assert p.resolve_parallel_comptime_prepass_at_token('@COLUMN', pos, source, 'prepass.v', 'main') == "'3'"
}

fn test_comptime_prepass_block_skip_matches_scanner_boundaries() {
	for body in [
		'{ if true { value := 123 + 456 } }',
		'{ text := "} {"; raw := r"\${ untouched }"; ch := `}` }',
		'{ text := "value: \${if true { "nested \${42}" } else { "}" }}" }',
		'{ /* } /* { */ } */ value := 1 // }\n }',
		'{\n#flag -DIGNORED={\nvalue := 1\n}',
	] {
		source := body + '\nconst after_body = 42\n'
		prefs := pref.new_preferences()
		mut fs := token.FileSet.new()
		file := fs.add_file('prepass.v', source.len)
		mut expected := scanner.new_scanner(prefs, .normal)
		expected.init(file, source)
		assert expected.scan() == .lcbr
		mut actual := expected
		mut depth := 1
		for depth > 0 {
			tok := expected.scan()
			assert tok != .eof
			if tok == .lcbr {
				depth++
			} else if tok == .rcbr {
				depth--
			}
		}
		skip_parallel_comptime_block(mut actual)
		assert actual.offset == expected.offset, body
		for _ in 0 .. 5 {
			assert actual.scan() == expected.scan(), body
			assert actual.lit == expected.lit, body
		}
	}
}

fn test_comptime_const_name_scan_matches_full_scanner() {
	for source in [
		'module main\nconst before = 1\nfn f() { const local = 2; value := local }\nconst after = 3\n',
		'module names\nfn f() { some_const := 1; module_name := 2; αconst := 3; xαconst := 4 }\nconst actual = 5\n',
		'module main\n// const ignored = 1\n/* const ignored = 2 /* module other */ */\nconst actual = 3\n',
		'module main\nfn f() { text := "const ignored = 1"; raw := r"module other"; ch := `c` }\nconst actual = 2\n',
		'module main\nfn f() { text := "value: \${if true { "nested \${42}" } else { "const ignored" }}" }\nconst actual = 3\n',
		'module main\n#flag -Dconst=ignored\nconst actual = 4\n',
		'module names\nstruct Escaped {\n @module u32\n @const int\n}\nconst actual = 5\n',
	] {
		prefs := pref.new_preferences()
		mut fs := token.FileSet.new()
		file := fs.add_file('const_names.v', source.len)
		mut expected := scanner.new_scanner(prefs, .normal)
		expected.init(file, source)
		mut actual := expected
		for {
			mut expected_tok := expected.scan()
			for expected_tok !in [.key_module, .key_const, .eof] {
				expected_tok = expected.scan()
			}
			assert scan_parallel_const_name_token(mut actual) == expected_tok, source
			assert actual.offset == expected.offset, source
			if expected_tok == .eof {
				break
			}
			// Declaration parsing resumes with the ordinary scanner at each hit.
			for _ in 0 .. 4 {
				assert actual.scan() == expected.scan(), source
				assert actual.lit == expected.lit, source
			}
		}
	}
}

fn test_comptime_const_name_scan_keeps_module_after_escaped_keywords() {
	path := os.join_path(os.vtmp_dir(), 'escaped_const_names_${os.getpid()}.v')
	os.write_file(path, 'module names\nstruct Escaped {\n @module u32\n @const int\n}\nconst actual = 5\n')!
	defer { os.rm(path) or {} }
	mut p := Parser.new(pref.new_preferences())
	names := p.parallel_comptime_const_names([path], 0, 1)
	assert names.len == 1, names.str()
	assert names['names\nactual'], names.str()
}
