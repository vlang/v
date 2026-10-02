module types

// module_import_lines visits only the lines where `import ` starts; it must
// find the same lines as asking source_line_has_multiple_module_imports of
// every line of the file, as the checker did before.

fn every_line(source string) []int {
	mut lines := []int{}
	mut line_number := 1
	mut line_start := 0
	for line_start <= source.len {
		line_end := source.index_after('\n', line_start) or { source.len }
		if source_line_has_multiple_module_imports(source[line_start..line_end]) {
			lines << line_number
		}
		if line_end >= source.len {
			break
		}
		line_start = line_end + 1
		line_number++
	}
	return lines
}

fn test_the_lines_of_several_imports_are_those_of_a_scan_of_every_line() {
	sources := [
		'',
		'import os, strings',
		'module main\n\nimport os, strings\nimport math\n\nfn main() {}\n',
		'import os\nimport math strings\n',
		'\timport os,strings\n  import a.b c\n',
		'x := "import os, strings"\n// import a, b\nimport c, d',
		'import os, strings\r\nimport a as b\r\nimport {x}\r\n',
		'import  os, math\nimport import a, b\n',
		'import os, math\n\n\n\n   import os, math',
		'fn main() {\n\timport_x := 1\n}\nimport\nimport \nimport a,',
		'import a, b\nimport c, d\nimport e, f\n',
	]
	for source in sources {
		assert module_import_lines(source) == every_line(source), source
	}
}

fn test_only_a_line_that_starts_with_import_holds_several() {
	source := 'module main\nimport os, strings\nconst x = "import a, b"\n  import c d\n'
	assert module_import_lines(source) == [2, 4]
}
