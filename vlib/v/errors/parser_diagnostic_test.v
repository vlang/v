module errors

import os
import v.ansi
import v.flat
import v.token

// The parser diagnostic renders through the same source excerpt as the checker's,
// so what separates the two is only the `kind:` word and the call-site stack.
fn test_formatted_parser_diagnostic_renders_the_location_and_the_excerpt() {
	root := os.join_path(os.vtmp_dir(), 'errors_parser_diag_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'main.v')
	source := 'fn main() {\n\tprintln("界")\n}\n'
	os.write_file(path, source) or { panic(err) }
	mut files := token.FileSet.new()
	mut file := files.add_file(path, source.len)
	file.index_lines(source)
	a := &flat.FlatAst{
		source_files: {
			1: file
		}
	}
	defer {
		ansi.set_colors_enabled(true)
	}
	ansi.set_colors_enabled(false)
	offset := source.index('println') or { 0 }
	pos := token.new_span(1, offset, offset + 'println'.len)
	output := formatted_parser_diagnostic('error:', 'unexpected token', a, pos)
	assert output.starts_with('${relative_error_path(path)}:2:2: error: unexpected token\n'), output
	assert output.contains('    2 |     println("界")'), output
	assert output.contains('    3 | }'), output
	assert !output.contains('\x1b['), output
	// Only one diagnostic is rendered; no `called from` without a template stack.
	assert output.count('called from ') == 0, output
}

fn test_formatted_parser_diagnostic_colors_the_kind_and_the_span() {
	root := os.join_path(os.vtmp_dir(), 'errors_parser_diag_colors_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
		ansi.set_colors_enabled(true)
	}
	path := os.join_path(root, 'main.v')
	source := 'fn main() {\n\tx := 1\n}\n'
	os.write_file(path, source) or { panic(err) }
	mut files := token.FileSet.new()
	mut file := files.add_file(path, source.len)
	file.index_lines(source)
	a := &flat.FlatAst{
		source_files: {
			1: file
		}
	}
	offset := source.index('x :=') or { 0 }
	pos := token.new_span(1, offset, offset + 3)
	ansi.set_colors_enabled(false)
	plain := formatted_parser_diagnostic('error:', 'bad', a, pos)
	ansi.set_colors_enabled(true)
	colored := formatted_parser_diagnostic('error:', 'bad', a, pos)
	assert colored.starts_with('${ansi.bold('${relative_error_path(path)}:2:2:')} ${formatted_message('error:', 'bad')}\n'), colored
	assert colored.contains(ansi.color('error:', 'x :')), colored
	assert colored.contains(ansi.bold(ansi.color('error:', '~~~'))), colored
	// The colors are decoration only: stripping them gives the plain text.
	mut stripped := colored
	for code in ['1', '22', '31', '33', '35', '39', '94'] {
		stripped = stripped.replace('\x1b[${code}m', '')
	}
	assert stripped == plain, '${stripped}\n---\n${plain}'
}

// A position that names no source file at all is rendered without an excerpt,
// which is the V1-compatible one-line form.
fn test_formatted_parser_diagnostic_falls_back_without_a_source_file() {
	a := &flat.FlatAst{}
	defer {
		ansi.set_colors_enabled(true)
	}
	ansi.set_colors_enabled(false)
	output := formatted_parser_diagnostic('error:', 'no file for this', a, token.new_pos(99,
		0))
	assert output == formatted_message('error:', 'no file for this')
	assert output == 'error: no file for this'
}
