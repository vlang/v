module parser

import v3.pref
import v3.scanner
import v3.token

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
