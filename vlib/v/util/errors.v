// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module util

import os
import term
import v.ast

// The filepath:line:col: format is the default C compiler error output format.
// It allows editors and IDE's like emacs to quickly find the errors in the
// output and jump to their source with a keyboard shortcut.
// NB: using only the filename may lead to inability of IDE/editors
// to find the source file, when the IDE has a different working folder than
// v itself.

const (
	error_context_before = 2 // how many lines of source context to print before the pointer line
	error_context_after = 2 // ^^^ same, but after
)

pub const (
	emanager = new_error_manager()
)

//

pub struct EManager {
pub mut:
	support_color bool // should the error and other messages
	// have ANSI terminal escape color codes in them.
	// By default, v tries to autodetect, if the terminal supports colors.
	// Use -color and -nocolor options to override the detection decision.
}

pub fn (e &EManager) set_support_color(b bool) {
	e.support_color = b
}

pub fn new_error_manager() &EManager {
	return &EManager{ support_color: term.can_show_color_on_stderr() }
}

pub fn formatted_error(kind string /*error or warn*/, emsg string, filepath string, pos ast.Position) string {
	mut path := filepath
	verror_paths_override := os.getenv('VERROR_PATHS')
	if verror_paths_override == 'absolute' {
		path = os.real_path( path )
	}else{
		// Get relative path
		workdir := os.getwd() + os.path_separator
		if path.starts_with(workdir) {
			path = path.replace(workdir, '')
		}
	}
	//
	mut source_context := ''
	source := util.read_file(filepath) or { '' }
	source_lines := source.split_into_lines()
	mut p := util.imax(0, util.imin(source.len -1, pos.pos))
	for ; p>=0; p-- {
		if source[p] == `\r` || source[p] == `\n` {
			break
		}
	}
	column := util.imax(0, pos.pos - p - 1)
	position := '${path}:${pos.line_nr+1}:${util.imax(1,column+1)}:'
	//
	bline := util.imax(0, pos.line_nr - error_context_before)
	aline := util.imin(source_lines.len-1, pos.line_nr + error_context_after)
	mut clines := []string
	tab_spaces := '    '
	for iline := bline; iline <= aline; iline++ {
		sline := source_lines[iline]
		mut cline := '${iline+1:5d}| ' + sline.replace('\t', tab_spaces)
		if iline == pos.line_nr && emanager.support_color {
			cline = term.red( cline )
		}
		clines << cline
		//
		if iline == pos.line_nr {
			// The pointerline should have the same spaces/tabs as the offending
			// line, so that it prints the ^ character exactly on the *same spot*
			// where it is needed. That is the reason we can not just
			// use strings.repeat(` `, col) to form it.
			mut pointerline := []string
			for i, c in sline {
				if i < column {
					mut x := c
					if x == `\t` {
						pointerline << tab_spaces
					}else{
						x = if x.is_space() { c } else { ` ` }
						pointerline << x.str()
					}
					continue
				}
				if pos.len > 1 {
					underline := '~'.repeat(pos.len)
					pointerline << if emanager.support_color { term.bold(term.blue(underline)) } else { underline }
				}else{
					pointerline << if emanager.support_color { term.bold(term.blue('^')) } else { '^' }
				}
				break
			}
			clines << '       ' + pointerline.join('')
		}
	}
	source_context += clines.join('\n')
	//
	final_position := if emanager.support_color {
		term.bold(position)
	} else {
		position
	}
	mut final_kind := kind
	if emanager.support_color {
		final_kind = if kind.contains('error') {
			term.bold(term.red(kind))
		}else{
			term.bold(term.bright_blue(kind))
		}
	}
	final_msg := emsg // if emanager.support_color { term.bold(emsg) } else { emsg }
	final_context := if source_context.len > 0 { '\n$source_context' } else { '' }
	//
	return '$final_position $final_kind $final_msg $final_context'.trim_space()
}

pub fn verror(kind string, s string) {
	if emanager.support_color {
		eprintln( term.bold(term.red(kind)) + ': $s' )
	}else{
		eprintln('${kind}: $s')
	}
	exit(1)
}
