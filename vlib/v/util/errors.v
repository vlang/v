// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
@[has_globals]
module util

import os
import strings
import term
import v.errors
import v.token

// The filepath:line:col: format is the default C compiler error output format.
// It allows editors and IDE's like emacs to quickly find the errors in the
// output and jump to their source with a keyboard shortcut.
// Note: using only the filename may lead to inability of IDE/editors
// to find the source file, when the IDE has a different working folder than
// v itself.
// error_context_before - how many lines of source context to print before the pointer line
// error_context_after - ^^^ same, but after
const error_context_before = 2
const error_context_after = 2

// emanager.support_color - should the error and other messages
// have ANSI terminal escape color codes in them.
// By default, v tries to autodetect, if the terminal supports colors.
// Use -color and -nocolor options to override the detection decision.
pub const emanager = new_error_manager()

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
	if !emanager.support_color {
		return msg
	}
	return term.bold(msg)
}

pub fn color(kind string, msg string) string {
	if !emanager.support_color {
		return msg
	}
	if kind.contains('error') {
		return term.red(msg)
	}
	if kind.contains('notice') {
		return term.yellow(msg)
	}
	if kind.contains('details') {
		return term.bright_blue(msg)
	}
	return term.magenta(msg)
}

const normalised_workdir = os.wd_at_startup.replace('\\', '/') + '/'

const verror_paths_absolute = os.getenv('VERROR_PATHS') == 'absolute'

// path_styled_for_error_messages converts the given file `path`, into one suitable for displaying
// in error messages, produced by the V compiler.
//
// When the file path is prefixed by the working folder, usually that means, that the resulting
// path, will be relative to the current working folder. Relative paths are shorter and stabler,
// because they only depend on the project, and not on the parent folders.
// If the current working folder of the compiler is NOT a prefix of the given path, then this
// function will return an absolute path instead. Absolute paths are longer, and also platform/user
// dependent, but they have the advantage of being more easily processible by tools on the same
// machine.
//
// The V user can opt out of that relativisation, by setting the environment variable VERROR_PATHS,
// to `absolute`. That is useful for starting the V compiler from an IDE or another program, where
// the concept of a "current working folder", is not as clear as working manually with the compiler
// in a shell. By setting VERROR_PATHS=absolute, the IDE/editor can ensure, that the produced error
// messages will have file locations that are easy to find and jump to locally.
//
// NOTE: path_styled_for_error_messages will *always* use `/` in the error paths, no matter the OS,
// to ensure stable compiler error output in the tests.
pub fn path_styled_for_error_messages(path string) string {
	mut rpath := os.real_path(path)
	rpath = rpath.replace('\\', '/')
	if verror_paths_absolute {
		return rpath
	}
	if rpath.starts_with(normalised_workdir) {
		rpath = rpath.replace_once(normalised_workdir, '')
	}
	return rpath
}

// formatted_error - `kind` may be 'error' or 'warn'
pub fn formatted_error(kind string, omsg string, filepath string, pos token.Pos) string {
	emsg := omsg.replace('main.', '')
	path := path_styled_for_error_messages(filepath)
	position := if filepath != '' {
		'${path}:${pos.line_nr + 1}:${int_max(1, pos.col + 1)}:'
	} else {
		''
	}
	scontext := source_file_context(kind, filepath, pos).join('\n')
	final_position := bold(position)
	final_kind := bold(color(kind, kind))
	final_msg := emsg
	final_context := if scontext.len > 0 { '\n${scontext}' } else { '' }

	return '${final_position} ${final_kind} ${final_msg}${final_context}'.trim_space()
}

@[heap]
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
	bline := int_max(0, pos.line_nr - error_context_before)
	aline := int_max(0, int_min(source_lines.len - 1, pos.line_nr + error_context_after))
	tab_spaces := '    '
	for iline := bline; iline <= aline; iline++ {
		sline := source_lines[iline] or { '' }
		start_column := int_max(0, int_min(pos.col, sline.len))
		end_column := int_max(0, int_min(pos.col + int_max(0, pos.len), sline.len))
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
					pointerline_builder.write_u8(sline[i])
					i++
				} else {
					char_len := utf8_char_len(sline[i])
					spaces := ' '.repeat(utf8_str_visible_length(sline#[i..i + char_len]))
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

@[noreturn]
pub fn verror(kind string, s string) {
	final_kind := bold(color(kind, kind))
	eprintln('${final_kind}: ${s}')
	exit(1)
}

pub fn vlines_escape_path(path string, ccompiler string) string {
	return cescaped_path(os.real_path(path))
}

pub fn show_compiler_message(kind string, err errors.CompilerMessage) {
	ferror := formatted_error(kind, err.message, err.file_path, err.pos)
	eprintln(ferror)
	if err.details.len > 0 {
		eprintln(bold('Details: ') + color('details', err.details))
	}
}
