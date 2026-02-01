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
	c.error_handler.add_detail(s)
}

fn (mut c Checker) add_error_detail_with_pos(msg string, pos token.Pos) {
	file_path := if pos.file_idx < 0 { c.file.path } else { c.table.filelist[pos.file_idx] }
	c.error_handler.add_detail(util.formatted_error('details:', msg, file_path, pos))
}

fn (mut c Checker) add_instruction_for_option_type() {
	c.add_error_detail_with_pos('prepend ? before the declaration of the return type of `${c.table.cur_fn.name}`',
		c.table.cur_fn.return_type_pos)
}

fn (mut c Checker) add_instruction_for_result_type() {
	c.add_error_detail_with_pos('prepend ! before the declaration of the return type of `${c.table.cur_fn.name}`',
		c.table.cur_fn.return_type_pos)
}

@[params]
pub struct MessageOptions {
pub:
	call_stack []errors.CallStackItem
}

fn (mut c Checker) warn(s string, pos token.Pos, options MessageOptions) {
	file_path := if pos.file_idx < 0 { c.file.path } else { c.table.filelist[pos.file_idx] }
	// Use the new unified error handler
	c.error_handler.report(errors.CompilerMessage{
		file_path:  file_path
		pos:        pos
		reporter:   errors.Reporter.checker
		message:    s
		call_stack: options.call_stack
	}, .warning)
	c.nr_warnings++
}

fn (mut c Checker) warn_alloc(s string, pos token.Pos) {
	if c.assign_stmt_attr == 'freed' {
		return
	}

	if !c.is_builtin_mod && c.mod !in ['strings', 'math', 'math.bits', 'builtin', 'strconv'] {
		c.warn('allocation (${s})', pos)
	}
}

fn (mut c Checker) error(message string, pos token.Pos, options MessageOptions) {
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
	file_path := if pos.file_idx < 0 { c.file.path } else { c.table.filelist[pos.file_idx] }

	// Use the new unified error handler
	c.error_handler.report(errors.CompilerMessage{
		file_path:  file_path
		pos:        pos
		reporter:   errors.Reporter.checker
		message:    msg
		call_stack: options.call_stack
	}, .error)
	c.nr_errors++
}

fn (mut c Checker) fatal(message string, pos token.Pos, options MessageOptions) {
	if (c.pref.translated || c.file.is_translated) && message.starts_with('mismatched types') {
		// TODO: move this
		return
	}
	msg := message.replace('`Array_', '`[]')

	file_path := if pos.file_idx < 0 { c.file.path } else { c.table.filelist[pos.file_idx] }

	// Use provided call_stack or fall back to file.call_stack
	actual_call_stack := if options.call_stack.len > 0 {
		options.call_stack
	} else {
		c.file.call_stack
	}

	// Use the new unified error handler
	c.error_handler.report(errors.CompilerMessage{
		file_path:  file_path
		pos:        pos
		reporter:   errors.Reporter.checker
		message:    msg
		call_stack: actual_call_stack
	}, .error)
}

fn (mut c Checker) note(message string, pos token.Pos) {
	if c.is_generated {
		return
	}
	file_path := if pos.file_idx < 0 { c.file.path } else { c.table.filelist[pos.file_idx] }

	// Use the new unified error handler
	c.error_handler.report(errors.CompilerMessage{
		file_path:  file_path
		pos:        pos
		reporter:   errors.Reporter.checker
		message:    message
		call_stack: c.file.call_stack
	}, .notice)
	c.nr_notices++
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
