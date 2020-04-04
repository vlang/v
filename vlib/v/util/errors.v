// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module util

import os
import term
import v.token

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

pub fn formated_error(kind string /*error or warn*/, emsg string, filepath string, pos token.Position) string {
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
	column := 0
	position := '${path}:${pos.line_nr+1}:$column:'
	// QTODO: retrieve source lines around pos.line_nr and add them here
	mut source_context := ''
	//
	final_position := if emanager.support_color {
		term.bold(position) // term.white(position))
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
	final_msg := emsg
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
