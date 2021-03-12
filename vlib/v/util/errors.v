// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module util

import math.mathutil as mu
import os
import strings
import term
import v.token

// The filepath:line:col: format is the default C compiler error output format.
// It allows editors and IDE's like emacs to quickly find the errors in the
// output and jump to their source with a keyboard shortcut.
// NB: using only the filename may lead to inability of IDE/editors
// to find the source file, when the IDE has a different working folder than
// v itself.
// error_context_before - how many lines of source context to print before the pointer line
// error_context_after - ^^^ same, but after
const (
	error_context_before = 2
	error_context_after  = 2
)

// emanager.support_color - should the error and other messages
// have ANSI terminal escape color codes in them.
// By default, v tries to autodetect, if the terminal supports colors.
// Use -color and -nocolor options to override the detection decision.
pub const (
	emanager = new_error_manager()
)

pub struct EManager {
mut:
	support_color bool
}

pub fn new_error_manager() &EManager {
	return &EManager{
		support_color: term.can_show_color_on_stderr() && term.can_show_color_on_stdout()
	}
}

pub fn (e &EManager) set_support_color(b bool) {
	unsafe {
		mut me := e
		me.support_color = b
	}
}

pub fn bold(msg string) string {
	if !util.emanager.support_color {
		return msg
	}
	return term.bold(msg)
}

fn color(kind string, msg string) string {
	if !util.emanager.support_color {
		return msg
	}
	if kind.contains('error') {
		return term.red(msg)
	} else {
		return term.magenta(msg)
	}
}

// formatted_error - `kind` may be 'error' or 'warn'
pub fn formatted_error(kind string, omsg string, filepath string, pos token.Position) string {
	emsg := omsg.replace('main.', '')
	mut path := filepath
	verror_paths_override := os.getenv('VERROR_PATHS')
	if verror_paths_override == 'absolute' {
		path = os.real_path(path)
	} else {
		// Get relative path
		workdir := os.getwd() + os.path_separator
		if path.starts_with(workdir) {
			path = path.replace(workdir, '')
		}
	}
	//
	source, column := filepath_pos_to_source_and_column(filepath, pos)
	position := '$path:${pos.line_nr + 1}:${mu.max(1, column + 1)}:'
	scontext := source_context(kind, source, column, pos).join('\n')
	final_position := bold(position)
	final_kind := bold(color(kind, kind))
	final_msg := emsg
	final_context := if scontext.len > 0 { '\n$scontext' } else { '' }
	//
	return '$final_position $final_kind $final_msg$final_context'.trim_space()
}

pub fn filepath_pos_to_source_and_column(filepath string, pos token.Position) (string, int) {
	// TODO: optimize this; may be use a cache.
	// The column should not be so computationally hard to get.
	source := read_file(filepath) or { '' }
	mut p := mu.max(0, mu.min(source.len - 1, pos.pos))
	if source.len > 0 {
		for ; p >= 0; p-- {
			if source[p] == `\n` || source[p] == `\r` {
				break
			}
		}
	}
	column := mu.max(0, pos.pos - p - 1)
	return source, column
}

pub fn source_context(kind string, source string, column int, pos token.Position) []string {
	mut clines := []string{}
	if source.len == 0 {
		return clines
	}
	source_lines := source.split_into_lines()
	bline := mu.max(0, pos.line_nr - util.error_context_before)
	aline := mu.max(0, mu.min(source_lines.len - 1, pos.line_nr + util.error_context_after))
	tab_spaces := '    '
	for iline := bline; iline <= aline; iline++ {
		sline := source_lines[iline]
		start_column := mu.max(0, mu.min(column, sline.len))
		end_column := mu.max(0, mu.min(column + mu.max(0, pos.len), sline.len))
		cline := if iline == pos.line_nr {
			sline[..start_column] + color(kind, sline[start_column..end_column]) +
				sline[end_column..]
		} else {
			sline
		}
		clines << '${iline + 1:5d} | ' + cline.replace('\t', tab_spaces)
		//
		if iline == pos.line_nr {
			// The pointerline should have the same spaces/tabs as the offending
			// line, so that it prints the ^ character exactly on the *same spot*
			// where it is needed. That is the reason we can not just
			// use strings.repeat(` `, col) to form it.
			mut pointerline_builder := strings.new_builder(sline.len)
			for i := 0; i < start_column; {
				if sline[i].is_space() {
					pointerline_builder.write_b(sline[i])
					i++
				} else {
					char_len := utf8_char_len(sline[i])
					spaces := ' '.repeat(utf8_str_visible_length(sline[i..i + char_len]))
					pointerline_builder.write_string(spaces)
					i += char_len
				}
			}
			underline_len := utf8_str_visible_length(sline[start_column..end_column])
			underline := if underline_len > 1 { '~'.repeat(underline_len) } else { '^' }
			pointerline_builder.write_string(bold(color(kind, underline)))
			clines << '      | ' + pointerline_builder.str().replace('\t', tab_spaces)
		}
	}
	return clines
}

pub fn verror(kind string, s string) {
	final_kind := bold(color(kind, kind))
	eprintln('$final_kind: $s')
	exit(1)
}

pub fn vlines_escape_path(path string, ccompiler string) string {
	is_cc_tcc := ccompiler.contains('tcc')
	if is_cc_tcc {
		// tcc currently has a bug, causing all #line files,
		// to be prefixed with the *same folder as the .tmp.c file*
		// this ../../ escaping, is a temporary workaround for that
		return '../../../../../..' + cescaped_path(os.real_path(path))
	}
	return cescaped_path(os.real_path(path))
}
