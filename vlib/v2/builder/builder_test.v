module builder

fn test_replace_generated_c_fn_ignores_braces_in_strings_chars_and_comments() {
	source := [
		'int target() {',
		'\tputs("{ inside string }");',
		"\tchar c = '}';",
		'\t// line comment with { }',
		'\t/* block comment with { } */',
		'\treturn 1;',
		'}',
		'int after() {',
		'\treturn 2;',
		'}',
	].join('\n')
	replacement := ['int target() {', '\treturn 99;', '}'].join('\n')
	expected := [
		'int target() {',
		'\treturn 99;',
		'}',
		'',
		'int after() {',
		'\treturn 2;',
		'}',
	].join('\n')
	assert replace_generated_c_fn(source, 'int target()', replacement) == expected
}
