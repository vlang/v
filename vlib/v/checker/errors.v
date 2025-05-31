// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import time
import v.ast
import v.token
import v.errors
import v.util

// call this *before* calling error or warn
fn (mut c Checker) add_error_detail(s string) {
	c.error_details << s
}

fn (mut c Checker) add_error_detail_with_pos(msg string, pos token.Pos) {
	c.add_error_detail(util.formatted_error('details:', msg, c.file.path, pos))
}

fn (mut c Checker) add_instruction_for_option_type() {
	c.add_error_detail_with_pos('prepend ? before the declaration of the return type of `${c.table.cur_fn.name}`',
		c.table.cur_fn.return_type_pos)
}

fn (mut c Checker) add_instruction_for_result_type() {
	c.add_error_detail_with_pos('prepend ! before the declaration of the return type of `${c.table.cur_fn.name}`',
		c.table.cur_fn.return_type_pos)
}

fn (mut c Checker) warn(s string, pos token.Pos) {
	allow_warnings := !(c.pref.is_prod || c.pref.warns_are_errors) // allow warnings only in dev builds
	c.warn_or_error(s, pos, allow_warnings)
}

fn (mut c Checker) warn_alloc(s string, pos token.Pos) {
	if c.assign_stmt_attr == 'freed' {
		return
	}

	if !c.is_builtin_mod && c.mod !in ['strings', 'math', 'math.bits', 'builtin', 'strconv'] {
		c.warn('allocation (${s})', pos)
	}
}

fn (mut c Checker) error(message string, pos token.Pos) {
	if (c.pref.translated || c.file.is_translated) && message.starts_with('mismatched types') {
		// TODO: move this
		return
	}
	mut msg := message.replace('`Array_', '`[]')
	if c.pref.is_vweb {
		// Show in which veb action the error occurred (for easier debugging)
		veb_action := c.table.cur_fn.name.replace('veb_tmpl_', '')
		mut j := 0
		for _, ch in veb_action {
			if ch.is_digit() {
				break
			}
			j++
		}
		msg += ' (veb action: ${veb_action[..j]})'
	}
	c.warn_or_error(msg, pos, false)
}

fn (mut c Checker) fatal(message string, pos token.Pos) {
	if (c.pref.translated || c.file.is_translated) && message.starts_with('mismatched types') {
		// TODO: move this
		return
	}
	msg := message.replace('`Array_', '`[]')
	c.pref.fatal_errors = true
	c.warn_or_error(msg, pos, false)
}

fn (mut c Checker) note(message string, pos token.Pos) {
	if c.pref.message_limit >= 0 && c.nr_notices >= c.pref.message_limit {
		c.should_abort = true
		return
	}
	if c.is_generated {
		return
	}
	if c.pref.notes_are_errors {
		c.error(message, pos)
	}
	mut details := ''
	if c.error_details.len > 0 {
		details = c.error_details.join('\n')
		c.error_details = []
	}
	// deduplicate notices for the same line
	kpos := '${c.file.path}:${pos.line_nr}:${message}'
	if kpos !in c.notice_lines {
		c.notice_lines[kpos] = true
		note := errors.Notice{
			reporter:  errors.Reporter.checker
			pos:       pos
			file_path: c.file.path
			message:   message
			details:   details
		}
		c.file.notices << note
		c.notices << note
		c.nr_notices++
	}
}

fn (mut c Checker) warn_or_error(message string, pos token.Pos, warn bool) {
	if !warn {
		$if checker_exit_on_first_error ? {
			eprintln('\n\n>> checker error: ${message}, pos: ${pos}')
			print_backtrace()
			exit(1)
		}
		if c.pref.is_verbose {
			print_backtrace()
		}
	}
	mut details := ''
	if c.error_details.len > 0 {
		details = c.error_details.join('\n')
		c.error_details = []
	}
	if warn && !c.pref.skip_warnings {
		c.nr_warnings++
		if c.pref.message_limit >= 0 && c.nr_warnings >= c.pref.message_limit {
			c.should_abort = true
			return
		}
		// deduplicate warnings for the same line
		kpos := '${c.file.path}:${pos.line_nr}:${message}'
		if kpos !in c.warning_lines {
			c.warning_lines[kpos] = true
			wrn := errors.Warning{
				reporter:  errors.Reporter.checker
				pos:       pos
				file_path: c.file.path
				message:   message
				details:   details
			}
			c.file.warnings << wrn
			c.warnings << wrn
		}
		return
	}
	if !warn {
		if c.pref.fatal_errors {
			util.show_compiler_message('error:', errors.CompilerMessage{
				pos:       pos
				file_path: c.file.path
				message:   message
				details:   details
			})
			exit(1)
		}
		c.nr_errors++
		if c.pref.message_limit >= 0 && c.errors.len >= c.pref.message_limit {
			c.should_abort = true
			return
		}
		// deduplicate errors for the same line
		kpos := '${c.file.path}:${pos.line_nr}:${message}'
		if kpos !in c.error_lines {
			c.error_lines[kpos] = true
			err := errors.Error{
				reporter:  errors.Reporter.checker
				pos:       pos
				file_path: c.file.path
				message:   message
				details:   details
			}
			c.file.errors << err
			c.errors << err
		}
	}
}

// for debugging only
fn (c &Checker) fileis(s string) bool {
	return c.file.path.contains(s)
}

fn (mut c Checker) trace[T](fbase string, x &T) {
	if c.file.path_base == fbase {
		println('> c.trace | ${fbase:-10s} | ${x}')
	}
}

fn (mut c Checker) deprecate(kind string, name string, attrs []ast.Attr, pos token.Pos) {
	// println('deprecate kind=${kind} name=${name} attrs=$attrs')
	// print_backtrace()
	mut deprecation_message := ''
	now := time.now()
	mut after_time := now
	for attr in attrs {
		if attr.arg == '' {
			continue
		}
		if attr.name == 'deprecated' {
			deprecation_message = attr.arg
		} else if attr.name == 'deprecated_after' {
			after_time = time.parse_iso8601(attr.arg) or {
				c.error('invalid time format', attr.pos)
				now
			}
		}
	}
	start_message := '${kind} `${name}`'
	error_time := after_time.add_days(180)
	if error_time < now {
		c.error(semicolonize('${start_message} has been deprecated since ${after_time.ymmdd()}',
			deprecation_message), pos)
	} else if after_time < now {
		c.warn(semicolonize('${start_message} has been deprecated since ${after_time.ymmdd()}, it will be an error after ${error_time.ymmdd()}',
			deprecation_message), pos)
	} else if after_time == now {
		// print_backtrace()
		c.warn(semicolonize('${start_message} has been deprecated', deprecation_message),
			pos)
		// c.warn(semicolonize('${start_message} has been deprecated!11 m=${deprecation_message}',
		// deprecation_message), pos)
	} else {
		c.note(semicolonize('${start_message} will be deprecated after ${after_time.ymmdd()}, and will become an error after ${error_time.ymmdd()}',
			deprecation_message), pos)
	}
}

fn semicolonize(main string, details string) string {
	if details == '' {
		return main
	}
	return '${main}; ${details}'
}
