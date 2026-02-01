module parser

import v.ast
import v.token
import v.errors
import v.util

fn (mut p Parser) language_not_allowed_error(language ast.Language, pos token.Pos) {
	upcase_language := language.str().to_upper_ascii()
	p.error_with_pos('${upcase_language} code is not allowed in .${p.file_backend_mode}.v files, please move it to a .${language}.v file',
		pos)
}

fn (mut p Parser) language_not_allowed_warning(language ast.Language, pos token.Pos) {
	upcase_language := language.str().to_upper_ascii()
	p.warn_with_pos('${upcase_language} code will not be allowed in pure .v files, please move it to a .${language}.v file instead',
		pos)
}

fn (mut p Parser) check_for_impure_v(language ast.Language, pos token.Pos) {
	if language == .v {
		// pure V code is always allowed everywhere
		return
	} else {
		match p.file_backend_mode {
			.c {
				if language != .c {
					p.language_not_allowed_error(language, pos)
					return
				}
			}
			.js {
				if language != .js {
					p.language_not_allowed_error(language, pos)
					return
				}
			}
			else {}
		}
	}
	if !p.pref.warn_impure_v {
		// the stricter mode is not ON yet => allow everything for now
		return
	}
	if p.file_backend_mode != language {
		if p.file_backend_mode == .v {
			if p.pref.is_bare {
				return
			}
			p.language_not_allowed_warning(language, pos)
			return
		}
	}
}

fn (mut p Parser) error(s string) ast.NodeError {
	return p.error_with_pos(s, p.tok.pos())
}

fn (mut p Parser) warn(s string) {
	p.warn_with_pos(s, p.tok.pos())
}

fn (mut p Parser) note(s string) {
	p.note_with_pos(s, p.tok.pos())
}

fn (mut p Parser) error_with_pos(s string, pos token.Pos) ast.NodeError {
	// print_backtrace()
	mut kind := 'error:'
	file_path := if pos.file_idx < 0 { p.file_path } else { p.table.filelist[pos.file_idx] }
	if p.pref.fatal_errors {
		util.show_compiler_message(kind, pos: pos, file_path: file_path, message: s)
		exit(1)
	}
	if p.pref.output_mode == .stdout && !p.pref.check_only && !p.is_vls {
		// Report error through error_handler for consistency
		p.error_handler.report(errors.CompilerMessage{
			file_path: file_path
			pos:       pos
			message:   s
		}, .error)
		if p.pref.is_verbose {
			print_backtrace()
			kind = 'parser error:'
		}
		util.show_compiler_message(kind, pos: pos, file_path: file_path, message: s)
		exit(1)
	} else {
		// Report error through error_handler
		p.error_handler.report(errors.CompilerMessage{
			file_path: file_path
			pos:       pos
			message:   s
		}, .error)

		// To avoid getting stuck after an error, the parser
		// will proceed to the next token.
		if p.pref.check_only || p.pref.only_check_syntax {
			if p.tok.kind != .eof {
				p.next()
			}
		}
	}
	if p.pref.output_mode == .silent && p.tok.kind != .eof {
		// Normally, parser errors mean that the parser exits immediately, so there can be only 1 parser error.
		// In the silent mode however, the parser continues to run, even though it would have stopped. Some
		// of the parser logic does not expect that, and may loop forever.
		// The p.next() here is needed, so the parser is more robust, and *always* advances, even in the -silent mode.
		p.next()
	}

	return ast.NodeError{
		idx: p.error_handler.error_count() - 1
		pos: pos
	}
}

fn (mut p Parser) error_with_error(error errors.ErrorMessage) {
	// Use the new unified error handler
	p.error_handler.report(error.CompilerMessage, .error)

	if p.pref.output_mode == .silent {
		// Normally, parser errors mean that the parser exits immediately, so there can be only 1 parser error.
		// In the silent mode however, the parser continues to run, even though it would have stopped. Some
		// of the parser logic does not expect that, and may loop forever.
		// The p.next() here is needed, so the parser is more robust, and *always* advances, even in the -silent mode.
		p.next()
	}
}

fn (mut p Parser) warn_with_pos(s string, pos token.Pos) {
	if p.pref.skip_warnings {
		return
	}
	file_path := if pos.file_idx < 0 { p.file_path } else { p.table.filelist[pos.file_idx] }
	// Use the new unified error handler
	p.error_handler.report(errors.CompilerMessage{
		file_path: file_path
		pos:       pos
		message:   s
	}, .warning)
}

fn (mut p Parser) note_with_pos(s string, pos token.Pos) {
	if p.pref.skip_warnings {
		return
	}
	if p.is_generated {
		return
	}
	file_path := if pos.file_idx < 0 { p.file_path } else { p.table.filelist[pos.file_idx] }
	// Use the new unified error handler
	p.error_handler.report(errors.CompilerMessage{
		file_path: file_path
		pos:       pos
		message:   s
	}, .notice)
}

@[params]
struct ParamsForUnexpected {
pub:
	got            string
	expecting      string
	prepend_msg    string
	additional_msg string
}

fn (mut p Parser) unexpected(params ParamsForUnexpected) ast.NodeError {
	return p.unexpected_with_pos(p.tok.pos(), params)
}

fn (mut p Parser) unexpected_with_pos(pos token.Pos, params ParamsForUnexpected) ast.NodeError {
	mut msg := if params.got != '' {
		'unexpected ${params.got}'
	} else {
		'unexpected ${p.tok}'
	}
	if params.expecting != '' {
		msg += ', expecting ${params.expecting}'
	}
	if params.prepend_msg != '' {
		msg = '${params.prepend_msg} ' + msg
	}
	if params.additional_msg != '' {
		msg += ', ${params.additional_msg}'
	}
	return p.error_with_pos(msg, pos)
}

fn (mut p Parser) chan_type_error() {
	p.error_with_pos('`chan` has no type specified. Use `chan Type` instead of `chan`',
		p.prev_tok.pos())
}
