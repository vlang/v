// vtest vflags: -no-retry-compilation

import os

fn test_escaped_braced_interpolation_stays_literal_next_to_interpolation() {
	value := 123
	assert '${value}\n\${missing}' == '123\n' + r'${missing}'
	assert '\${missing}-${value}-\${other}' == r'${missing}' + '-123-' + r'${other}'
}

fn test_escaped_nested_if_text_stays_literal() {
	cond := true
	a := 'A'
	b := 'B'
	assert '\${if cond { ${a} } else { ${b} }}' == r'${if cond { ' + a + r' } else { ' + b + r' }}'
}

fn test_decoded_dollar_escape_stays_literal_in_nested_string() {
	path := os.join_path(os.vtmp_dir(), 'v3_decoded_dollar_escape_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, r"fn main() {
 x := 42
 assert '${'\x24{x}'}' == r'${x}'
 assert '${'\u0024{x}'}' == r'${x}'
 assert '${'\044{x}'}' == r'${x}'
}")!
	result := os.exec([@VEXE, '-no-retry-compilation', '-gc', 'none', 'run', path])
	assert result.exit_code == 0, result.output
}
