module errors

import os
import strings
import term
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

// cached_file2sourcelines returns source lines for the given file path
// Note: For performance, consider using util.cached_file2sourcelines() instead
pub fn cached_file2sourcelines(path string) []string {
	source := os.read_file(path) or { '' }
	return source.split_into_lines()
}

// DisplayManager manages error display settings
pub struct DisplayManager {
mut:
	support_color bool
}

// new_display_manager creates a new DisplayManager instance
pub fn new_display_manager() &DisplayManager {
	return &DisplayManager{
		support_color: term.can_show_color_on_stderr() && term.can_show_color_on_stdout()
	}
}

pub fn (d &DisplayManager) set_support_color(b bool) {
	unsafe {
		mut me := d
		me.support_color = b
	}
}

pub fn (d &DisplayManager) bold(msg string) string {
	if !d.support_color {
		return msg
	}
	return term.bold(msg)
}

pub fn (d &DisplayManager) color(kind string, msg string) string {
	if !d.support_color {
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
	manager := new_display_manager()
	final_position := manager.bold(position)
	final_kind := manager.bold(manager.color(kind, kind))
	final_msg := emsg
	final_context := if scontext.len > 0 { '\n${scontext}' } else { '' }

	return '${final_position} ${final_kind} ${final_msg}${final_context}'.trim_space()
}

pub fn source_file_context(kind string, filepath string, pos token.Pos) []string {
	mut clines := []string{}
	source_lines := cached_file2sourcelines(filepath)
	if source_lines.len == 0 {
		return clines
	}
	bline := int_max(0, pos.line_nr - error_context_before)
	aline := int_max(0, int_min(source_lines.len - 1, pos.line_nr + error_context_after))
	tab_spaces := '    '
	manager := new_display_manager()
	for iline := bline; iline <= aline; iline++ {
		sline := source_lines[iline] or { '' }
		start_column := int_max(0, int_min(pos.col, sline.len))
		end_column := int_max(0, int_min(pos.col + int_max(0, pos.len), sline.len))
		cline := if iline == pos.line_nr {
			sline[..start_column] + manager.color(kind, sline[start_column..end_column]) +
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
			pointerline_builder.write_string(manager.bold(manager.color(kind, underline)))
			clines << '      | ' + pointerline_builder.str().replace('\t', tab_spaces)
		}
	}
	return clines
}

// show_compiler_message displays a compiler message (error, warning, or notice)
pub fn show_compiler_message(kind string, err CompilerMessage) {
	manager := new_display_manager()
	ferror := formatted_error(kind, err.message, err.file_path, err.pos)
	eprintln(ferror)
	if err.details.len > 0 {
		eprintln(manager.bold('Details: ') + manager.color('details', err.details))
	}
	// Display call stack if available
	if err.call_stack.len > 0 {
		for item in err.call_stack {
			caller_path := path_styled_for_error_messages(item.file_path)
			eprintln(manager.bold('called from') + ' ${caller_path}:${item.pos.line_nr +
				1}:${int_max(1, item.pos.col + 1)}')
			// Display code context for the caller location
			scontext := source_file_context(kind, item.file_path, item.pos).join('\n')
			if scontext.len > 0 {
				eprintln(scontext)
			}
		}
	}
}
