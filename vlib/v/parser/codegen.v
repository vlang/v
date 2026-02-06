@[has_globals]
module parser

import v.ast

__global codegen_files = unsafe { []&ast.File{} }

// codegen allows you to generate V code, so that it can be parsed,
// checked, markused, cgen-ed etc further, just like user's V code.
pub fn (mut p Parser) codegen(code string) {
	$if debug_codegen ? {
		eprintln('parser.codegen: ${code}')
	}
	p.codegen_text += code
}

fn (mut p Parser) handle_codegen_for_file() {
	if p.pref.is_fmt || p.codegen_text == '' {
		return
	}
	ptext := 'module ' + p.mod.all_after_last('.') + '\n' + p.codegen_text
	codegen_files << parse_text(ptext, p.file_path, mut p.table, p.scanner.comments_mode,
		p.pref)
}

fn handle_codegen_for_multiple_files(mut files []&ast.File) {
	if codegen_files.len == 0 {
		return
	}
	files << codegen_files
	codegen_files.clear()
}
