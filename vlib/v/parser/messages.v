module parser

import v.ast
import v.util
import v.token
import v.errors

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
	if p.pref.fatal_errors {
		util.show_compiler_message(kind, pos: pos, file_path: p.file_path, message: s)
		exit(1)
	}
	if p.pref.output_mode == .stdout && !p.pref.check_only && !p.is_vls {
		if p.pref.is_verbose {
			print_backtrace()
			kind = 'parser error:'
		}
		util.show_compiler_message(kind, pos: pos, file_path: p.file_path, message: s)
		exit(1)
	} else {
		p.errors << errors.Error{
			file_path: p.file_path
			pos:       pos
			reporter:  .parser
			message:   s
		}

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
		idx: p.errors.len - 1
		pos: pos
	}
}

fn (mut p Parser) error_with_error(error errors.Error) {
	mut kind := 'error:'
	if p.pref.fatal_errors {
		util.show_compiler_message(kind, error.CompilerMessage)
		exit(1)
	}
	if p.pref.output_mode == .stdout && !p.pref.check_only {
		if p.pref.is_verbose {
			print_backtrace()
			kind = 'parser error:'
		}
		util.show_compiler_message(kind, error.CompilerMessage)
		exit(1)
	} else {
		if p.pref.message_limit >= 0 && p.errors.len >= p.pref.message_limit {
			p.should_abort = true
			return
		}
		p.errors << error
	}
	if p.pref.output_mode == .silent {
		// Normally, parser errors mean that the parser exits immediately, so there can be only 1 parser error.
		// In the silent mode however, the parser continues to run, even though it would have stopped. Some
		// of the parser logic does not expect that, and may loop forever.
		// The p.next() here is needed, so the parser is more robust, and *always* advances, even in the -silent mode.
		p.next()
	}
}

fn (mut p Parser) warn_with_pos(s string, pos token.Pos) {
	if p.pref.warns_are_errors {
		p.error_with_pos(s, pos)
		return
	}
	if p.pref.skip_warnings {
		return
	}
	if p.pref.output_mode == .stdout && !p.pref.check_only {
		util.show_compiler_message('warning:', pos: pos, file_path: p.file_path, message: s)
	} else {
		if p.pref.message_limit >= 0 && p.warnings.len >= p.pref.message_limit {
			p.should_abort = true
			return
		}
		p.warnings << errors.Warning{
			file_path: p.file_path
			pos:       pos
			reporter:  .parser
			message:   s
		}
	}
}

fn (mut p Parser) note_with_pos(s string, pos token.Pos) {
	if p.pref.skip_warnings {
		return
	}
	if p.pref.skip_notes {
		return
	}
	if p.is_generated {
		return
	}
	if p.pref.notes_are_errors {
		p.error_with_pos(s, pos)
		return
	}
	if p.pref.output_mode == .stdout && !p.pref.check_only {
		util.show_compiler_message('notice:', pos: pos, file_path: p.file_path, message: s)
	} else {
		p.notices << errors.Notice{
			file_path: p.file_path
			pos:       pos
			reporter:  .parser
			message:   s
		}
	}
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
