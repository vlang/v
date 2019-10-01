module main

import (
	os
	term
)

//////////////////////////////////////////////////////////////////////////////////////////////////
// NB: The code in this file is organized in layers (between the ///// lines).
// This allows for easier keeping in sync of error/warn functions.
// The functions in each of the layers, call the functions from the layers *below*.
// The functions in each of the layers, also have more details about the warn/error situation,
// so they can display more informative message, so please call the lowest level variant you can.
//////////////////////////////////////////////////////////////////////////////////////////////////
// TLDR: If you have a token index, call:
//     p.error_with_token_index(msg, token_index)
// ... not just :
//     p.error(msg)
//////////////////////////////////////////////////////////////////////////////////////////////////

fn (p mut Parser) error(s string) {
	// no positioning info, so just assume that the last token was the culprit:
	p.error_with_token_index(s, p.token_idx-1 )
}

fn (p mut Parser) warn(s string) {
	p.warn_with_token_index(s, p.token_idx-1 )
}

//////////////////////////////////////////////////////////////////////////////////////////////////

fn (p mut Parser) production_error_with_token_index(e string, tokenindex int) {
	if p.pref.is_prod {
		p.error_with_token_index( e, tokenindex )
	}else {
		p.warn_with_token_index( e, tokenindex )
	}
}

//////////////////////////////////////////////////////////////////////////////////////////////////

fn (p mut Parser) error_with_token_index(s string, tokenindex int) {
	p.error_with_position(s, p.scanner.get_scanner_pos_of_token( p.tokens[ tokenindex ] ) )
}

fn (p mut Parser) warn_with_token_index(s string, tokenindex int) {
	p.warn_with_position(s, p.scanner.get_scanner_pos_of_token( p.tokens[ tokenindex ] ) )
}

//////////////////////////////////////////////////////////////////////////////////////////////////

fn (p mut Parser) error_with_position(s string, sp ScannerPos) {
	p.print_error_context()
	e := normalized_error( s )
	p.scanner.goto_scanner_position( sp )
	p.scanner.error_with_col(e, sp.pos - sp.last_nl_pos)
}

fn (p mut Parser) warn_with_position(s string, sp ScannerPos) {
	// on a warning, restore the scanner state after printing the warning:
	cpos := p.scanner.get_scanner_pos()
	e := normalized_error( s )
	p.scanner.goto_scanner_position( sp )
	p.scanner.warn_with_col(e, sp.pos - sp.last_nl_pos)
	p.scanner.goto_scanner_position( cpos )
}

//////////////////////////////////////////////////////////////////////////////////////////////////

fn (s &Scanner) error(msg string) {
	s.error_with_col(msg, 0)
}

fn (s &Scanner) warn(msg string) {
	s.warn_with_col(msg, 0)
}

//////////////////////////////////////////////////////////////////////////////////////////////////
fn (s &Scanner) warn_with_col(msg string, col int) {
	fullpath := s.get_error_filepath()		
	color_on := s.is_color_output_on()
	final_message := if color_on { term.bold(term.bright_blue( msg )) } else { msg }
	eprintln('warning: ${fullpath}:${s.line_nr+1}:${col}: $final_message')
}

fn (s &Scanner) error_with_col(msg string, col int) {
	fullpath := s.get_error_filepath()		
	color_on := s.is_color_output_on()
	final_message := if color_on { term.red( term.bold( msg ) ) } else { msg }
	// The filepath:line:col: format is the default C compiler
	// error output format. It allows editors and IDE's like
	// emacs to quickly find the errors in the output
	// and jump to their source with a keyboard shortcut.
	// NB: using only the filename may lead to inability of IDE/editors
	// to find the source file, when the IDE has a different working folder than v itself.
	eprintln('${fullpath}:${s.line_nr + 1}:${col}: $final_message')
	
	if s.should_print_line_on_error && s.file_lines.len > 0 {
		context_start_line := imax(0,                (s.line_nr - error_context_before + 1 ))
		context_end_line   := imin(s.file_lines.len, (s.line_nr + error_context_after + 1 ))
		for cline := context_start_line; cline < context_end_line; cline++ {
			line := '${(cline+1):5d}| ' + s.file_lines[ cline ]
			coloredline := if cline == s.line_nr && color_on { term.red(line) } else { line }
			eprintln( coloredline )
			if cline != s.line_nr { continue }
			// The pointerline should have the same spaces/tabs as the offending
			// line, so that it prints the ^ character exactly on the *same spot*
			// where it is needed. That is the reason we can not just
			// use strings.repeat(` `, col) to form it.
			mut pointerline := []string		
			for i , c in line {
				if i < col {
					x := if c.is_space() { c } else { ` ` }
					pointerline << x.str()
					continue
				}
				pointerline << if color_on { term.bold( term.blue('^') ) } else { '^' }
				break
			}
			eprintln( '      ' + pointerline.join('') )
		}
	}
	exit(1)
}

//////////////////////////////////////////////////////////////////////////////////////////////////

/// Misc error helper functions, can be called by any of the functions above

[inline] fn (p &Parser) cur_tok_index() int { return p.token_idx - 1 }
[inline] fn imax(a,b int) int { 	return if a > b { a } else { b } }
[inline] fn imin(a,b int) int { 	return if a < b { a } else { b } }

fn (s &Scanner) get_error_filepath() string {
	if s.should_print_relative_paths_on_error {
		return s.file_path
	}
	return os.realpath( s.file_path )
}

fn (s &Scanner) is_color_output_on() bool {
	return s.should_print_errors_in_color && term.can_show_color_on_stderr()	
}

fn (p mut Parser) print_error_context(){
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
	if !p.pref.is_repl && !p.pref.is_test && ( p.file_path.contains('v/compiler') || cur_path.contains('v/compiler') ){
		println('\n=========================')
		println('It looks like you are building V. It is being frequently updated every day.')
		println('If you didn\'t modify V\'s code, most likely there was a change that ')
		println('lead to this error.')
		println('\nRun `v up`, that will most likely fix it.')
		//println('\nIf this doesn\'t help, re-install V from source or download a precompiled' + ' binary from\nhttps://vlang.io.')
		println('\nIf this doesn\'t help, please create a GitHub issue.')
		println('=========================\n')
	}
	if p.pref.is_debug {
		print_backtrace()
	}
	// p.scanner.debug_tokens()
}

fn normalized_error( s string ) string {
	// Print `[]int` instead of `array_int` in errors
	return s.replace('array_', '[]')
	.replace('__', '.')
	.replace('Option_', '?')
	.replace('main.', '')
}

//////////////////////////////////////////////////////////////////////////////////////////////////

// The goal of ScannerPos is to track the current scanning position,
// so that if there is an error found later, v could show a more accurate
// position about where the error initially was.
// NB: The fields of ScannerPos *should be kept synchronized* with the
// corresponding fields in Scanner.

struct ScannerPos {
mut:
   pos int
   line_nr int
   last_nl_pos int
}

fn (s ScannerPos) str() string {
	return 'ScannerPos{ ${s.pos:5d} , ${s.line_nr:5d} , ${s.last_nl_pos:5d} }'
}

fn (s &Scanner) get_scanner_pos() ScannerPos {
	return ScannerPos{ pos: s.pos line_nr: s.line_nr last_nl_pos: s.last_nl_pos }
}

fn (s mut Scanner) goto_scanner_position(scp ScannerPos) {
	s.pos = scp.pos
	s.line_nr = scp.line_nr
	s.last_nl_pos = scp.last_nl_pos
}

// get_scanner_pos_of_token rescans *the whole source* till it reaches {t.line_nr, t.col} .
fn (s mut Scanner) get_scanner_pos_of_token(t &Tok) ScannerPos {
	// This rescanning is done just once on error, so it is fine for now.
	// Be careful for the performance implications, if you want to
	// do it more frequently. The alternative would be to store
	// the scanpos (12 bytes) for each token, and there are potentially many tokens.
	tline := t.line_nr
	tcol  := if t.line_nr == 0 { t.col + 1 } else { t.col - 1 }
	// save the current scanner position, it will be restored later
	cpos := s.get_scanner_pos()
	mut sptoken := ScannerPos{}
	// Starting from the start, scan the source lines
	// till the desired tline is reached, then
	// s.pos + tcol would be the proper position
	// of the token. Continue scanning for some more lines of context too.
	s.goto_scanner_position(ScannerPos{})
	s.file_lines = []string
	mut prevlinepos := 0
	for {
		prevlinepos = s.pos
		if s.pos >= s.text.len { break }		
		if s.line_nr > tline + 10 { break }
		////////////////////////////////////////
		if tline == s.line_nr {
			sptoken = s.get_scanner_pos()
			sptoken.pos += tcol
		}
		s.ignore_line() s.eat_single_newline()
		sline := s.text.substr( prevlinepos, s.pos ).trim_right('\r\n')
		s.file_lines << sline
	}
	//////////////////////////////////////////////////
	s.goto_scanner_position(cpos)
	return sptoken
}

fn (s mut Scanner) eat_single_newline(){
	if s.pos >= s.text.len { return }
	if s.expect('\r\n', s.pos) { s.pos += 2 return }
	if s.text[ s.pos ] == `\n` { s.pos ++ return }
	if s.text[ s.pos ] == `\r` { s.pos ++ return }
}
