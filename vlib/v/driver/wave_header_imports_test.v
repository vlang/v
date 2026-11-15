module driver

fn test_header_imports_are_the_import_lines_before_the_first_declaration() {
	source := '// A comment.\n@[has_globals]\nmodule demo\n\nimport os\nimport strings.textscanner\nimport v.errors as compiler_errors\nimport term { bold, red }\n/* a block\n   import hidden */\nimport time // trailing\n\nconst x = 1\n\nimport late\n'
	assert source_header_imports(source, []) == ['os', 'strings.textscanner', 'v.errors', 'term',
		'time']
}

fn test_header_imports_follow_the_define_guards_they_can_decide() {
	source := 'module demo\n\nimport os\n\$if !skip_fastc ? {\n\timport v.gen.fastc\n\timport hash as contenthash\n}\n\$if with_eval ? {\n\timport v.eval\n}\n\$if windows {\n\timport winreg\n}\nimport time\n\nfn main() {}\n'
	assert source_header_imports(source, []) == ['os', 'v.gen.fastc', 'hash', 'time']
	assert source_header_imports(source, ['skip_fastc']) == ['os', 'time']
	assert source_header_imports(source, ['skip_fastc', 'with_eval']) == ['os', 'v.eval', 'time']
}

fn test_header_imports_end_at_text_the_scan_does_not_know() {
	assert source_header_imports('import os\n\$else {\n\timport time\n}\nimport io\n', []) == [
		'os',
	]
	assert source_header_imports('import os\nfn f() {}\nimport io\n', []) == ['os']
	assert source_header_imports('', []) == []string{}
	assert source_header_imports('import', []) == []string{}
	assert source_header_imports('\$if x ? {\n\timport a\n', ['x']) == ['a']
	assert source_header_imports('import a {\n\tb,\n\tc\n}\nimport d\n', []) == ['a', 'd']
}
