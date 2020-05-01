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
pub mut:
	support_color bool
}

pub fn (e &EManager) set_support_color(b bool) {
	e.support_color = b
}

pub fn new_error_manager() &EManager {
	return &EManager{
		support_color: term.can_show_color_on_stderr()
	}
}

// formatted_error - `kind` may be 'error' or 'warn'
pub fn formatted_error(kind, emsg, filepath string, pos token.Position) string {
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
	source := read_file(filepath) or {
		''
	}
	mut p := imax(0, imin(source.len - 1, pos.pos))
	if source.len > 0 {
		for ; p >= 0; p-- {
			if source[p] == `\r` || source[p] == `\n` {
				break
			}
		}
	}
	column := imax(0, pos.pos - p - 1)
	position := '${path}:${pos.line_nr+1}:${util.imax(1,column+1)}:'
	scontext := source_context(kind, source, column, pos).join('\n')
	final_position := if emanager.support_color { term.bold(position) } else { position }
	mut final_kind := kind
	if emanager.support_color {
		final_kind = if kind.contains('error') {
			term.bold(term.red(kind))
		} else {
			term.bold(term.magenta(kind))
		}
	}
	final_msg := emsg // if emanager.support_color { term.bold(emsg) } else { emsg }
	final_context := if scontext.len > 0 { '\n$scontext' } else { '' }
	//
	return '$final_position $final_kind $final_msg $final_context'.trim_space()
}

pub fn source_context(kind, source string, column int, pos token.Position) []string {
	mut clines := []string{}
	if source.len == 0 {
		return clines
	}
	source_lines := source.split_into_lines()
	bline := imax(0, pos.line_nr - error_context_before)
	aline := imax(0, imin(source_lines.len - 1, pos.line_nr + error_context_after))
	tab_spaces := '    '
	for iline := bline; iline <= aline; iline++ {
		sline := source_lines[iline]
		mut cline := sline.replace('\t', tab_spaces)
		if iline == pos.line_nr && emanager.support_color {
			cline = if kind.contains('error') {
				term.red(cline)
			} else {
				term.magenta(cline)
			}
		}
		clines << '${iline+1:5d} | ' + cline
		//
		if iline == pos.line_nr {
			// The pointerline should have the same spaces/tabs as the offending
			// line, so that it prints the ^ character exactly on the *same spot*
			// where it is needed. That is the reason we can not just
			// use strings.repeat(` `, col) to form it.
			mut pointerline := []string{}
			for i, bchar in sline {
				if i < column {
					mut x := bchar
					if x == `\t` {
						pointerline << tab_spaces
					} else {
						x = if x.is_space() {
							bchar
						} else {
							` `
						}
						pointerline << x.str()
					}
					continue
				}
				if pos.len > 1 {
					max_len := sline.len - pointerline.len // rest of the line
					len := if pos.len > max_len { max_len } else { pos.len }
					underline := '~'.repeat(len)
					pointerline << if emanager.support_color {
						term.bold(term.blue(underline))
					} else {
						underline
					}
				} else {
					pointerline << if emanager.support_color {
						term.bold(term.blue('^'))
					} else {
						'^'
					}
				}
				break
			}
			clines << '      | ' + pointerline.join('')
		}
	}
	return clines
}

pub fn verror(kind, s string) {
	if emanager.support_color {
		eprintln(term.bold(term.red(kind)) + ': $s')
	} else {
		eprintln('${kind}: $s')
	}
	exit(1)
}

pub fn find_working_diff_command() ?string {
	for diffcmd in ['colordiff', 'diff', 'colordiff.exe', 'diff.exe'] {
		p := os.exec('$diffcmd --version') or {
			continue
		}
		if p.exit_code == 0 {
			return diffcmd
		}
	}
	return error('no working diff command found')
}

pub fn color_compare_files(diff_cmd, file1, file2 string) string {
	if diff_cmd != '' {
		full_cmd := '$diff_cmd --minimal --text --unified=2 ' +
		        ' --show-function-line="fn " "$file1" "$file2" '
		x := os.exec(full_cmd) or {
			return 'comparison command: `${full_cmd}` failed'
        }
        return x.output
    }
    return ''
}
