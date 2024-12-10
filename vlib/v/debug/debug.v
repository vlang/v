// Copyright (c) 2019-2024 Felipe Pena. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
@[has_globals]
module debug

import os
import readline
import strings
import term

@[markused]
__global g_debugger = Debugger{}

// Debugger holds the V debug information for REPL
@[heap]
struct Debugger {
mut:
	is_tty     bool = os.is_atty(0) > 0 // is tty?
	exited     bool     // user exiting flag
	last_cmd   string   // save the last cmd
	last_args  string   // save the last args
	watch_vars []string // save the watched vars
	cmdline    readline.Readline = readline.Readline{
		completion_list: [
			'anon?',
			'bt',
			'clear',
			'continue',
			'generic?',
			'heap',
			'help',
			'list',
			'mem',
			'memory',
			'method?',
			'mod',
			'print',
			'quit',
			'scope',
			'unwatch',
			'watch',
		]
	}
}

// DebugContextVar holds the scope variable information
pub struct DebugContextVar {
	name  string // var name
	typ   string // its type name
	value string // its str value
}

// DebugContextInfo has the context info for the debugger repl
pub struct DebugContextInfo {
	is_anon           bool                       // cur fn is anon?
	is_generic        bool                       // cur fn is a generic?
	is_method         bool                       // cur fn is a bool?
	receiver_typ_name string                     // cur receiver type name (method only)
	line              int                        // cur line number
	file              string                     // cur file name
	mod               string                     // cur module name
	fn_name           string                     // cur function name
	scope             map[string]DebugContextVar // scope var data
}

fn flush_println(s string) {
	println(s)
	flush_stdout()
}

// show_variable prints the variable info if found into the cur context
fn (d DebugContextInfo) show_variable(var_name string, is_watch bool) {
	if info := d.scope[var_name] {
		flush_println('${var_name} = ${info.value} (${info.typ})')
	} else if !is_watch {
		eprintln('[error] var `${var_name}` not found')
	}
}

fn (d DebugContextInfo) show_watched_vars(watch_vars []string) {
	for var in watch_vars {
		d.show_variable(var, true)
	}
}

// show_scope prints the cur context scope variables
fn (d DebugContextInfo) show_scope() {
	for k, v in d.scope {
		flush_println('${k} = ${v.value} (${v.typ})')
	}
}

// DebugContextInfo.ctx displays info about the current fn context
fn (d DebugContextInfo) ctx() string {
	mut s := strings.new_builder(512)
	if d.is_method {
		s.write_string('[${d.mod}] (${d.receiver_typ_name}) ${d.fn_name}')
	} else {
		s.write_string('[${d.mod}] ${d.fn_name}')
	}
	return s.str()
}

// print_help prints the debugger REPL commands help
fn (mut d Debugger) print_help() {
	println('vdbg commands:')
	println('  anon?\t\t\tcheck if the current context is anon')
	println('  bt\t\t\tprints a backtrace')
	println('  c, continue\t\tcontinue debugging')
	println('  generic?\t\tcheck if the current context is generic')
	println('  heap\t\t\tshow heap memory usage')
	println('  h, help, ?\t\tshow this help')
	println('  l, list [lines]\tshow some lines from current break (default: 3)')
	println('  mem, memory\t\tshow memory usage')
	println('  method?\t\tcheck if the current context is a method')
	println('  m, mod\t\tshow current module name')
	println('  p, print <var>\tprints an variable')
	println('  q, quit\t\texits debugging session in the code')
	println('  scope\t\t\tshow the vars in the current scope')
	println('  u, unwatch <var>\tunwatches a variable')
	println('  w, watch <var>\twatches a variable')
	println('  clear\t\t\tclears the terminal window')
	flush_println('')
}

// read_line provides the user prompt based on tty flag
fn (mut d Debugger) read_line(prompt string) (string, bool) {
	if d.is_tty {
		mut is_ctrl := false
		line := d.cmdline.read_line(prompt) or {
			is_ctrl = true
			''
		}
		return line.trim_right('\r\n '), is_ctrl
	} else {
		print(prompt)
		flush_stdout()
		return os.get_raw_line().trim_right('\r\n '), false
	}
}

fn (mut d Debugger) parse_input(input string, is_ctrl bool) (string, string) {
	splitted := input.split(' ')
	if !is_ctrl && splitted[0] == '' {
		return d.last_cmd, d.last_args
	} else {
		cmd := if is_ctrl { d.last_cmd } else { splitted[0] }
		args := if splitted.len > 1 { splitted[1] } else { '' }
		d.last_cmd = cmd
		d.last_args = args
		return cmd, args
	}
}

// print_context_lines prints N lines before and after the current location
fn (mut d Debugger) print_context_lines(path string, line int, lines int) ! {
	file_content := os.read_file(path)!
	chunks := file_content.split('\n')
	offset := int_max(line - lines, 1)
	for n, s in chunks[offset - 1..int_min(chunks.len, line + lines)] {
		ind := if n + offset == line { '>' } else { ' ' }
		flush_println('${n + offset:04}${ind} ${s}')
	}
}

// print_memory_use prints the GC memory use
fn (d &Debugger) print_memory_use() {
	flush_println(gc_memory_use().str())
}

// print_heap_usage prints the GC heap usage
fn (d &Debugger) print_heap_usage() {
	h := gc_heap_usage()
	flush_println('heap size: ${h.heap_size}')
	flush_println('free bytes: ${h.free_bytes}')
	flush_println('total bytes: ${h.total_bytes}')
}

// watch_var adds a variable to watch_list
fn (mut d Debugger) watch_var(var string) bool {
	if var !in d.watch_vars {
		d.watch_vars << var
		return true
	}
	return false
}

// unwatch_var removes a variable from watch list
fn (mut d Debugger) unwatch_var(var string) {
	item := d.watch_vars.index(var)
	if item >= 0 {
		d.watch_vars.delete(item)
	}
}

// interact displays the V debugger REPL for user interaction
@[markused]
pub fn (mut d Debugger) interact(info DebugContextInfo) ! {
	if d.exited {
		return
	}

	flush_println('Break on ${info.ctx()} in ${info.file}:${info.line}')
	if d.watch_vars.len > 0 {
		info.show_watched_vars(d.watch_vars)
	}
	for {
		input, is_ctrl := d.read_line('${info.file}:${info.line} vdbg> ')
		cmd, args := d.parse_input(input, is_ctrl)
		match cmd {
			'anon?' {
				flush_println(info.is_anon.str())
			}
			'bt' {
				print_backtrace_skipping_top_frames(2)
				flush_stdout()
			}
			'c', 'continue' {
				break
			}
			'generic?' {
				flush_println(info.is_generic.str())
			}
			'heap' {
				d.print_heap_usage()
			}
			'?', 'h', 'help' {
				d.print_help()
			}
			'l', 'list' {
				lines := if args != '' { args.int() } else { 3 }
				if lines < 0 {
					eprintln('[error] cannot use negative line amount')
				} else {
					d.print_context_lines(info.file, info.line, lines)!
				}
			}
			'method?' {
				flush_println(info.is_method.str())
			}
			'mem', 'memory' {
				d.print_memory_use()
			}
			'm', 'mod' {
				flush_println(info.mod)
			}
			'p', 'print' {
				if args == '' {
					eprintln('[error] var name is expected as parameter')
				} else {
					info.show_variable(args, false)
				}
			}
			'scope' {
				info.show_scope()
			}
			'q', 'quit' {
				d.exited = true
				break
			}
			'u', 'unwatch' {
				if args == '' {
					eprintln('[error] var name is expected as parameter')
				} else {
					d.unwatch_var(args)
				}
			}
			'w', 'watch' {
				if args == '' {
					eprintln('[error] var name is expected as parameter')
				} else {
					if d.watch_var(args) {
						info.show_variable(args, false)
					}
				}
			}
			'clear' {
				term.erase_clear()
			}
			'' {
				if is_ctrl {
					flush_println('')
				}
			}
			else {
				eprintln('unknown command `${cmd}`')
			}
		}
	}
}
