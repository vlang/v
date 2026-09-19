module errors

import os
import v.ansi
import v.flat
import v.token

fn without_diagnostic_colors(text string) string {
	mut result := text
	for code in ['1', '22', '31', '33', '35', '39', '94'] {
		result = result.replace('\x1b[${code}m', '')
	}
	return result
}

fn test_diagnostic_palette_matches_v1() {
	defer {
		ansi.set_colors_enabled(true)
	}
	palette := {
		'error:':                   '31'
		'builder error:':           '31'
		'notice:':                  '33'
		'details:':                 '94'
		'warning:':                 '35'
		'conflicting declaration:': '35'
	}
	for kind, code in palette {
		ansi.set_colors_enabled(true)
		assert ansi.color(kind, 'text') == '\x1b[${code}mtext\x1b[39m'
		assert formatted_message(kind, 'message') ==
			'\x1b[1m\x1b[${code}m${kind}\x1b[39m\x1b[22m message'
		ansi.set_colors_enabled(false)
		assert ansi.color(kind, 'text') == 'text'
		assert formatted_message(kind, 'message') == '${kind} message'
	}
}

fn test_positionless_and_missing_source_diagnostics_use_colors() {
	defer {
		ansi.set_colors_enabled(true)
	}
	a := &flat.FlatAst{}
	for enabled in [false, true] {
		ansi.set_colors_enabled(enabled)
		for pos in [token.Pos{}, token.new_pos(99, 0)] {
			output := formatted_error('builder error:', 'duplicate', a, flat.empty_node, pos)
			assert output == formatted_message('builder error:', 'duplicate')
			assert without_diagnostic_colors(output) == 'builder error: duplicate'
		}
		assert formatted_parser_error('missing', a, token.new_pos(99, 0)) ==
			formatted_message('error:', 'missing')
	}
}

fn test_source_colors_preserve_context_and_display_width() {
	path := os.join_path(os.vtmp_dir(), 'v3_diagnostic_colors_${os.getpid()}.v')
	source := 'fn main() {\n\tprintln("界")\n}\n'
	os.write_file(path, source)!
	defer {
		os.rm(path) or {}
		ansi.set_colors_enabled(true)
	}
	mut files := token.FileSet.new()
	mut file := files.add_file(path, source.len)
	file.index_lines(source)
	for text in ['println', '界'] {
		offset := source.index(text) or { panic(err) }
		pos := token.new_span(1, offset, offset + text.len)
		position := file.position(pos)
		location := '${relative_error_path(path)}:${position.line}:${position.column}:'
		for kind in ['error:', 'conflicting declaration:'] {
			ansi.set_colors_enabled(false)
			plain := formatted_source_error(kind, 'message', file, pos)
			assert plain.starts_with('${location} ${kind} message\n'), plain
			assert plain.contains('    2 |     println("界")'), plain
			assert !plain.contains('\x1b['), plain
			ansi.set_colors_enabled(true)
			colored := formatted_source_error(kind, 'message', file, pos)
			assert colored.starts_with('${ansi.bold(location)} ${formatted_message(kind, 'message')}\n'), colored
			assert colored.contains(ansi.color(kind, text)), colored
			underline := '~'.repeat(diagnostic_display_width(text))
			assert colored.contains(ansi.bold(ansi.color(kind, underline))), colored
			assert without_diagnostic_colors(colored) == plain, colored
		}
	}
}
