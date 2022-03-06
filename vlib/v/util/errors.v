// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
[has_globals]
module util

import os
import strings
import term
import v.token
import v.mathutil as mu

// The filepath:line:col: format is the default C compiler error output format.
// It allows editors and IDE's like emacs to quickly find the errors in the
// output and jump to their source with a keyboard shortcut.
// Note: using only the filename may lead to inability of IDE/editors
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
	}
	if kind.contains('notice') {
		return term.yellow(msg)
	}
	return term.magenta(msg)
}

const normalised_workdir = os.wd_at_startup.replace('\\', '/') + '/'

// formatted_error - `kind` may be 'error' or 'warn'
pub fn formatted_error(kind string, omsg string, filepath string, pos token.Pos) string {
	emsg := omsg.replace('main.', '')
	mut path := filepath
	verror_paths_override := os.getenv('VERROR_PATHS')
	if verror_paths_override == 'absolute' {
		path = os.real_path(path)
	} else {
		// always use `/` in the error paths, to ensure the compiler output does not vary in the tests:
		path = path.replace('\\', '/')
		if path.starts_with(util.normalised_workdir) {
			// Get a relative path to the compiler's workdir, when possible:
			path = path.replace_once(util.normalised_workdir, '')
		}
	}
	//
	position := '$path:${pos.line_nr + 1}:${mu.max(1, pos.col + 1)}:'
	scontext := source_file_context(kind, filepath, pos).join('\n')
	final_position := bold(position)
	final_kind := bold(color(kind, kind))
	final_msg := emsg
	final_context := if scontext.len > 0 { '\n$scontext' } else { '' }
	//
	return '$final_position $final_kind $final_msg$final_context'.trim_space()
}

[heap]
struct LinesCache {
mut:
	lines map[string][]string
}

__global lines_cache = &LinesCache{}

pub fn cached_file2sourcelines(path string) []string {
	if res := lines_cache.lines[path] {
		return res
	}
	source := read_file(path) or { '' }
	res := set_source_for_path(path, source)
	return res
}

// set_source_for_path should be called for every file, over which you want to use util.formatted_error
pub fn set_source_for_path(path string, source string) []string {
	lines := source.split_into_lines()
	lines_cache.lines[path] = lines
	return lines
}

pub fn source_file_context(kind string, filepath string, pos token.Pos) []string {
	mut clines := []string{}
	source_lines := unsafe { cached_file2sourcelines(filepath) }
	if source_lines.len == 0 {
		return clines
	}
	bline := mu.max(0, pos.line_nr - util.error_context_before)
	aline := mu.max(0, mu.min(source_lines.len - 1, pos.line_nr + util.error_context_after))
	tab_spaces := '    '
	for iline := bline; iline <= aline; iline++ {
		sline := source_lines[iline]
		start_column := mu.max(0, mu.min(pos.col, sline.len))
		end_column := mu.max(0, mu.min(pos.col + mu.max(0, pos.len), sline.len))
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
					pointerline_builder.write_byte(sline[i])
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

[noreturn]
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
