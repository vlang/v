module compiler

import (
	os
	filepath
	term
)
// ////////////////////////////////////////////////////////////////////////////////////////////////
// NB: The code in this file is organized in layers (between the ///// lines).
// This allows for easier keeping in sync of error/warn functions.
// The functions in each of the layers, call the functions from the layers *below*.
// The functions in each of the layers, also have more details about the warn/error situation,
// so they can display more informative message, so please call the lowest level variant you can.
// ////////////////////////////////////////////////////////////////////////////////////////////////
// TLDR: If you have a token index, call:
// p.error_with_token_index(msg, token_index)
// ... not just :
// p.error(msg)
// ////////////////////////////////////////////////////////////////////////////////////////////////


fn (p mut Parser) error(s string) {
	// no positioning info, so just assume that the last token was the culprit:
	p.error_with_token_index(s, p.token_idx - 1)
}

fn (p mut Parser) warn_or_error(s string) {
	if p.pref.is_prod {
		p.error(s)
	} else {
		p.warn(s)
	}
}

fn (p mut Parser) warn(s string) {
	p.warn_with_token_index(s, p.token_idx - 1)
}

fn (p mut Parser) production_error_with_token_index(e string, tokenindex int) {
	if p.pref.is_prod {
		p.error_with_token_index(e, tokenindex)
	}
	else {
		p.warn_with_token_index(e, tokenindex)
	}
}

fn (p mut Parser) error_with_token_index(s string, tokenindex int) {
	p.error_with_position(s, p.scanner.get_scanner_pos_of_token(p.tokens[tokenindex]))
}

fn (p mut Parser) warn_with_token_index(s string, tokenindex int) {
	p.warn_with_position(s, p.scanner.get_scanner_pos_of_token(p.tokens[tokenindex]))
}

fn (p mut Parser) error_with_position(s string, sp ScannerPos) {
	p.print_error_context()
	e := normalized_error(s)
	p.scanner.goto_scanner_position(sp)
	p.scanner.error_with_col(e, sp.pos - sp.last_nl_pos)
}

fn (p mut Parser) warn_with_position(s string, sp ScannerPos) {
	if p.scanner.is_fmt {
		return
	}
	// on a warning, restore the scanner state after printing the warning:
	cpos := p.scanner.get_scanner_pos()
	e := normalized_error(s)
	p.scanner.goto_scanner_position(sp)
	p.scanner.warn_with_col(e, sp.pos - sp.last_nl_pos)
	p.scanner.goto_scanner_position(cpos)
}

fn (s &Scanner) error(msg string) {
	s.error_with_col(msg, 0)
}

fn (s &Scanner) warn(msg string) {
	s.warn_with_col(msg, 0)
}

fn (s &Scanner) warn_with_col(msg string, col int) {
	fullpath := s.get_error_filepath()
	color_on := s.is_color_output_on()
	final_message := if color_on { term.bold(term.bright_blue(msg)) } else { msg }
	eprintln('warning: ${fullpath}:${s.line_nr+1}:${col}: $final_message')
}

fn (s &Scanner) error_with_col(msg string, col int) {
	fullpath := s.get_error_filepath()
	color_on := s.is_color_output_on()
	final_message := if color_on { term.red(term.bold(msg)) } else { msg }
	// The filepath:line:col: format is the default C compiler
	// error output format. It allows editors and IDE's like
	// emacs to quickly find the errors in the output
	// and jump to their source with a keyboard shortcut.
	// NB: using only the filename may lead to inability of IDE/editors
	// to find the source file, when the IDE has a different working folder than v itself.
	eprintln('${fullpath}:${s.line_nr + 1}:${col}: $final_message')
	if s.print_line_on_error && s.nlines > 0 {
		context_start_line := imax(0, (s.line_nr - error_context_before))
		context_end_line := imin(s.nlines - 1, (s.line_nr + error_context_after + 1))
		for cline := context_start_line; cline < context_end_line; cline++ {
			line := '${(cline+1):5d}| ' + s.line(cline)
			coloredline := if cline == s.line_nr && color_on { term.red(line) } else { line }
			eprintln(coloredline)
			if cline != s.line_nr {
				continue
			}
			// The pointerline should have the same spaces/tabs as the offending
			// line, so that it prints the ^ character exactly on the *same spot*
			// where it is needed. That is the reason we can not just
			// use strings.repeat(` `, col) to form it.
			mut pointerline := []string
			for i, c in line {
				if i < col {
					x := if c.is_space() { c } else { ` ` }
					pointerline << x.str()
					continue
				}
				pointerline << if color_on { term.bold(term.blue('^')) } else { '^' }
				break
			}
			eprintln('      ' + pointerline.join(''))
		}
	}
	exit(1)
}

// ////////////////////////////////////////////////////////////////////////////////////////////////
// / Misc error helper functions, can be called by any of the functions above
[inline]
fn (p &Parser) cur_tok_index() int {
	return p.token_idx - 1
}

[inline]
fn imax(a, b int) int {
	return if a > b { a } else { b }
}

[inline]
fn imin(a, b int) int {
	return if a < b { a } else { b }
}

fn (s &Scanner) get_error_filepath() string {
	verror_paths_override := os.getenv('VERROR_PATHS')
	use_relative_paths := match verror_paths_override {
		'relative'{
			true
		}
		'absolute'{
			false
		}
		else {
			s.print_rel_paths_on_error}}
	if use_relative_paths {
		workdir := os.getwd() + filepath.separator
		if s.file_path.starts_with(workdir) {
			return s.file_path.replace(workdir, '')
		}
		return s.file_path
	}
	return os.realpath(s.file_path)
}

fn (s &Scanner) is_color_output_on() bool {
	return s.print_colored_error && term.can_show_color_on_stderr()
}

fn (p mut Parser) print_error_context() {
	// Dump all vars and types for debugging
	if p.pref.is_debug {
		// os.write_to_file('/var/tmp/lang.types', '')//pes(p.table.types))
		os.write_file('fns.txt', p.table.debug_fns())
	}
	if p.pref.is_verbose || p.pref.is_debug {
		println('pass=$p.pass fn=`$p.cur_fn.name`\n')
	}
	p.cgen.save()
	// V up hint
	cur_path := os.getwd()
	if !p.pref.is_repl && !p.pref.is_test && (p.file_path.contains('v/compiler') || cur_path.contains('v/compiler')) {
		println('\n=========================')
		println('It looks like you are building V. It is being frequently updated every day.')
		println("If you didn\'t modify V\'s code, most likely there was a change that ")
		println('lead to this error.')
		println('\nRun `v up`, that will most likely fix it.')
		// println('\nIf this doesn\'t help, re-install V from source or download a precompiled' + ' binary from\nhttps://vlang.io.')
		println("\nIf this doesn\'t help, please create a GitHub issue.")
		println('=========================\n')
	}
	if p.pref.is_debug {
		print_backtrace()
	}
	// p.scanner.debug_tokens()
}

fn ienv_default(ename string, idefault int) int {
	es := os.getenv(ename)
	if es.len == 0 { return idefault }
	return es.int()
}

// print_current_tokens/1 pretty prints the current token context, like this:
// // Your label: tokens[  32] = Token{ .line:   8, .pos:   93, .tok:  85 } = mut
// // Your label: tokens[> 33] = Token{ .line:   8, .pos:   95, .tok:   1 } = b
// // Your label: tokens[  34] = Token{ .line:   8, .pos:   98, .tok:  31 } = :=
// It is useful while debugging the v compiler itself. > marks p.token_idx
fn (p &Parser) print_current_tokens(label string){
	btokens := ienv_default('V_BTOKENS', 5)
	atokens := ienv_default('V_ATOKENS', 5)
	ctoken_idx := p.token_idx
	stoken_idx := imax(0, ctoken_idx - btokens)
	etoken_idx := imin( ctoken_idx + atokens + 1, p.tokens.len)
	for i := stoken_idx; i < etoken_idx; i++ {
		idx := if i == ctoken_idx {
			'>${i:3d}'
		} else {
			' ${i:3d}'
		}
		eprintln('$label: tokens[$idx] = ' + p.tokens[ i ].detailed_str())
	}
}

fn normalized_error(s string) string {
	mut res := s
	if !res.contains('__') {
		// `[]int` instead of `array_int`
		res = res.replace('array_', '[]')
	}
	res = res.replace('__', '.')
	res = res.replace('Option_', '?')
	res = res.replace('main.', '')
	res = res.replace('ptr_', '&')
	res = res.replace('_dot_', '.')
	if res.contains('_V_MulRet_') {
		res = res.replace('_V_MulRet_', '(')
		res = res.replace('_V_', ', ')
		res = res[..res.len - 1] + ')"' //"// quote balance comment. do not remove
	}
	return res
}

// ////////////////////////////////////////////////////////////////////////////////////////////////
// The goal of ScannerPos is to track the current scanning position,
// so that if there is an error found later, v could show a more accurate
// position about where the error initially was.
// NB: The fields of ScannerPos *should be kept synchronized* with the
// corresponding fields in Scanner.
struct ScannerPos {
mut:
	pos         int
	line_nr     int
	last_nl_pos int
}

pub fn (s ScannerPos) str() string {
	return 'ScannerPos{ ${s.pos:5d} , ${s.line_nr:5d} , ${s.last_nl_pos:5d} }'
}

fn (s &Scanner) get_scanner_pos() ScannerPos {
	return ScannerPos{
		pos: s.pos
		line_nr: s.line_nr
		last_nl_pos: s.last_nl_pos
	}
}

fn (s mut Scanner) goto_scanner_position(scp ScannerPos) {
	s.pos = scp.pos
	s.line_nr = scp.line_nr
	s.last_nl_pos = scp.last_nl_pos
}

fn (s &Scanner) get_last_nl_from_pos(_pos int) int {
	pos := if _pos >= s.text.len { s.text.len - 1 } else { _pos }
	for i := pos; i >= 0; i-- {
		if s.text[i] == `\n` {
			return i
		}
	}
	return 0
}

fn (s &Scanner) get_scanner_pos_of_token(tok &Token) ScannerPos {
	return ScannerPos{
		pos: tok.pos
		line_nr: tok.line_nr
		last_nl_pos: s.get_last_nl_from_pos(tok.pos)
	}
}

// /////////////////////////////
fn (p mut Parser) mutable_arg_error(i int, arg Var, f Fn) {
	mut dots_example := 'mut $p.lit'
	if i > 0 {
		dots_example = '.., ' + dots_example
	}
	if i < f.args.len - 1 {
		dots_example = dots_example + ',..'
	}
	p.error('`$arg.name` is a mutable argument, you need to provide `mut`: ' + '`$f.name ($dots_example)`')
}

const (
	warn_match_arrow = '=> is no longer needed in match statements, use\n' + 'match foo {
	1 { bar }
	2 { baz }
	else { ... }
}'
	// make_receiver_mutable =
	err_used_as_value = 'used as value'
	and_or_error = 'use `()` to make the boolean expression clear\n' + 'for example: `(a && b) || c` instead of `a && b || c`'
	err_modify_bitfield = 'to modify a bitfield flag use the methods: set, clear, toggle. and to check for flag use: has'
)
