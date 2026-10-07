module errors

import os
import v.flat
import v.token

fn testsuite_begin() {
	// The expected paths are relative to the working directory.
	os.unsetenv('VERROR_PATHS')
}

fn test_json_escape_keeps_a_diagnostic_on_one_line() {
	assert json_escape('plain `text`') == 'plain `text`'
	assert json_escape('a "quoted" \\ path') == 'a \\"quoted\\" \\\\ path'
	assert json_escape('line\nbreak\r\tand tab') == 'line\\nbreak\\r\\tand tab'
	assert json_escape('\x01\x1b[31m') == '\\u0001\\u001b[31m'
	assert json_escape('üñí') == 'üñí'
	assert json_escape('€ and 😀') == '€ and 😀'
}

// A message can quote a malformed source file. Each byte outside a valid UTF-8 sequence
// becomes U+FFFD, so that a strict JSON reader still accepts the line.
fn test_json_escape_replaces_invalid_utf8() {
	assert json_escape('invalid character `\xff`') == 'invalid character `\\ufffd`'
	// A lone continuation byte, and a sequence cut off by the end of the text.
	assert json_escape('a\x80b') == 'a\\ufffdb'
	assert json_escape('x\xe2\x82') == 'x\\ufffd\\ufffd'
	// A cut off sequence does not swallow the ASCII after it.
	assert json_escape('\xc3"') == '\\ufffd\\"'
	// Overlong and surrogate encodings are not UTF-8.
	assert json_escape('\xc0\xaf') == '\\ufffd\\ufffd'
	assert json_escape('\xed\xa0\x80') == '\\ufffd\\ufffd\\ufffd'
	// Valid text around an invalid byte is kept.
	assert json_escape('ü\xffñ') == 'ü\\ufffdñ'
}

fn test_json_message_has_no_position() {
	assert json_message('error:', 'no `main` function', []string{}) == '{"severity":"error","message":"no `main` function"}'
	assert json_message('warning:', 'first', ['a "detail"', 'second line']) == '{"severity":"warning","message":"first","details":"a \\"detail\\"\\nsecond line"}'
}

fn test_json_severity_is_error_warning_or_notice() {
	assert json_message('builder error:', 'cannot import module', []string{}) == '{"severity":"error","label":"builder error","message":"cannot import module"}'
	assert json_message('cgen error:', 'void', []string{}) == '{"severity":"error","label":"cgen error","message":"void"}'
	assert json_message('conflicting declaration:', 'f', []string{}) == '{"severity":"error","label":"conflicting declaration","message":"f"}'
	assert json_message('notice:', 'n', []string{}) == '{"severity":"notice","message":"n"}'
}

fn test_json_located_message_spans_the_reported_column() {
	assert json_located_message('notice:', 'unparsed', []string{}, 'dir/a.v', 3, 7) == '{"file":"dir/a.v","line":3,"col":7,"end_line":3,"end_col":8,"severity":"notice","message":"unparsed"}'
}

fn test_json_error_reports_the_span_of_a_position() {
	source := 'fn main() {\n\tx := 1 +\n\t\t2\n}\n'
	path := os.join_path(os.getwd(), 'json_span_test_input.v')
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines(source)
	a := &flat.FlatAst{
		source_files: {
			1: file
		}
	}
	// `x` on line 2.
	x := source.index('x') or { panic('no x') }
	assert json_error('error:', 'unused `x`', []string{}, a, flat.NodeId(-1), token.new_span(1,
		x, x + 1)) == '{"file":"json_span_test_input.v","line":2,"col":2,"end_line":2,"end_col":3,"severity":"error","message":"unused `x`"}'
	// An empty span covers the byte it points at, like the caret of the text form.
	assert json_error('error:', 'here', []string{}, a, flat.NodeId(-1), token.new_pos(1,
		x)) == '{"file":"json_span_test_input.v","line":2,"col":2,"end_line":2,"end_col":3,"severity":"error","message":"here"}'
	// `1 +\n\t\t2` ends on line 3; the end is the column after its last byte.
	one := source.index('1') or { panic('no 1') }
	two := source.index('2') or { panic('no 2') }
	assert json_error('warning:', 'split', []string{}, a, flat.NodeId(-1), token.new_span(1,
		one, two + 1)) == '{"file":"json_span_test_input.v","line":2,"col":7,"end_line":3,"end_col":4,"severity":"warning","message":"split"}'
	// A position of no known file leaves a diagnostic without a location.
	assert json_error('error:', 'lost', []string{}, a, flat.NodeId(-1), token.new_pos(9,
		0)) == '{"severity":"error","message":"lost"}'
}

// Like the text form, the location follows the `#line` directives of the file.
fn test_json_error_follows_line_directives() {
	source := 'fn main() {\n\tx := 1\n#line 3 "app.zbr"\n\tprintln(b)\n#line 9 "other.zbr"\n\t_ = x\n}\n'
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(os.join_path(os.getwd(), 'err.v'), source.len)
	file.index_lines(source)
	file.add_line_directive(source.index('#line 3') or { panic('no #line 3') }, 3, 'app.zbr')
	file.add_line_directive(source.index('#line 9') or { panic('no #line 9') }, 9, 'other.zbr')
	a := &flat.FlatAst{
		source_files: {
			1: file
		}
	}
	b := source.index('b)') or { panic('no b') }
	assert json_error('error:', 'undefined ident: `b`', []string{}, a, flat.NodeId(-1),
		token.new_span(1, b, b + 1)) == '{"file":"app.zbr","line":3,"col":10,"end_line":3,"end_col":11,"severity":"error","message":"undefined ident: `b`"}'
	// A span that ends in another logical file stays on the line of its start.
	assert json_error('error:', 'cut', []string{}, a, flat.NodeId(-1), token.new_span(1, b,
		source.index('x\n}') or { panic('no x') })) == '{"file":"app.zbr","line":3,"col":10,"end_line":3,"end_col":11,"severity":"error","message":"cut"}'
	// Before the first directive, the location is the physical one.
	x := source.index('x') or { panic('no x') }
	assert json_error('error:', 'unused `x`', []string{}, a, flat.NodeId(-1), token.new_span(1,
		x, x + 1)) == '{"file":"err.v","line":2,"col":2,"end_line":2,"end_col":3,"severity":"error","message":"unused `x`"}'
}

fn test_json_error_lists_every_template_call_site() {
	main_source := "fn main() {\n\t\$tmpl('outer.txt')\n}\n"
	outer_source := "@{\$tmpl('inner.txt')}\n"
	inner_source := '@missing\n'
	root := os.getwd()
	mut file_set := token.FileSet.new()
	mut main_file := file_set.add_file(os.join_path(root, 'main.v'), main_source.len)
	main_file.index_lines(main_source)
	mut outer_file := file_set.add_file(os.join_path(root, 'outer.txt'), outer_source.len)
	outer_file.index_lines(outer_source)
	mut inner_file := file_set.add_file(os.join_path(root, 'inner.txt'), inner_source.len)
	inner_file.index_lines(inner_source)
	a := &flat.FlatAst{
		source_files:        {
			1: main_file
			2: outer_file
			3: inner_file
		}
		template_call_sites: {
			3: token.new_pos(2, outer_source.index('\$tmpl') or { 0 })
			2: token.new_pos(1, main_source.index('\$tmpl') or { 0 })
		}
	}
	missing := inner_source.index('missing') or { 0 }
	assert json_parser_diagnostic('error:', 'undefined ident: `missing`', []string{}, a,
		token.new_span(3, missing, missing + 7)) == '{"file":"inner.txt","line":1,"col":2,"end_line":1,"end_col":9,"severity":"error","message":"undefined ident: `missing`","called_from":[{"file":"outer.txt","line":1,"col":3},{"file":"main.v","line":2,"col":2}]}'
}

fn test_json_output_is_off_until_requested() {
	assert !json_output()
	set_json_output(true)
	assert json_output()
	set_json_output(false)
	assert !json_output()
}
