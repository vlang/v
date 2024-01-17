// Copyright (c) 2019-2024 Felipe Pena. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
@[has_globals]
module debug

import os
import math
import readline
import strings

__global g_debugger = Debugger{}

// Debugger holds the V debug information for REPL
@[heap]
struct Debugger {
mut:
	exited     bool     // user exiting flag
	last_cmd   string   // save the last cmd
	last_args  string   // save the last args
	watch_vars []string // save the watched vars
	cmdline    readline.Readline
}

// DebugContextVar holds the scope variable information
pub struct DebugContextVar {
	name  string // var name
	typ   string // its type name
	value string // its str value
}

// DebugContextInfo has the context info for the debugger repl
pub struct DebugContextInfo {
	is_anon           bool   // cur fn is anon?
	is_generic        bool   // cur fn is a generic?
	is_method         bool   // cur fn is a bool?
	receiver_typ_name string // cur receiver type name (method only)
	line              int    // cur line number
	file              string // cur file name
	mod               string // cur module name
	fn_name           string // cur function name
	scope             map[string]DebugContextVar // scope var data
}

// show_variable prints the variable info if found into the cur context
fn (d DebugContextInfo) show_variable(var_name string) {
	if info := d.scope[var_name] {
		println('${var_name} = ${info.value} (${info.typ})')
	}
}

fn (d DebugContextInfo) show_watched_vars(watch_vars []string) {
	for var in watch_vars {
		d.show_variable(var)
	}
}

// show_scope prints the cur context scope variables
fn (d DebugContextInfo) show_scope() {
	for k, v in d.scope {
		println('${k} = ${v.value} (${v.typ})')
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
	println('  u, unwatch <var>\t\tunwatches a variable')
	println('  w, watch <var>\t\t\twatches a variable')
	println('')
}

// print_context_lines prints N lines before and after the current location
fn (mut d Debugger) print_context_lines(path string, line int, lines int) ! {
	file_content := os.read_file(path)!
	chunks := file_content.split('\n')
	offset := math.max(line - lines, 1)
	for n, s in chunks[offset - 1..math.min(chunks.len, line + lines)] {
		ind := if n + offset == line { '>' } else { ' ' }
		println('${n + offset:04}${ind} ${s}')
	}
}

// print_memory_use prints the GC memory use
fn (d &Debugger) print_memory_use() {
	println(gc_memory_use())
}

// print_heap_usage prints the GC heap usage
fn (d &Debugger) print_heap_usage() {
	h := gc_heap_usage()
	println('heap size: ${h.heap_size}')
	println('free bytes: ${h.free_bytes}')
	println('total bytes: ${h.total_bytes}')
}

// watch_var adds a variable to watch_list
fn (mut d Debugger) watch_var(var string) {
	if var !in d.watch_vars {
		d.watch_vars << var
	}
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

	prompt := '${info.file}:${info.line} vdbg> '
	println('Break on ${info.ctx()} in ${info.file}:${info.line}')
	if d.watch_vars.len > 0 {
		info.show_watched_vars(d.watch_vars)
	}
	for {
		mut is_ctrl := false
		input := d.cmdline.read_line(prompt) or {
			is_ctrl = true
			''
		}
		splitted := input.split(' ')

		mut cmd := ''
		mut args := ''
		if !is_ctrl && splitted[0] == '' {
			cmd = d.last_cmd
			args = d.last_args
		} else {
			cmd = if is_ctrl { d.last_cmd } else { splitted[0] }
			args = if splitted.len > 1 { splitted[1] } else { '' }
			d.last_cmd = cmd
			d.last_args = args
		}
		match cmd {
			'anon?' {
				println(info.is_anon)
			}
			'bt' {
				print_backtrace_skipping_top_frames(2)
			}
			'c', 'continue' {
				break
			}
			'generic?' {
				println(info.is_generic)
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
					println('[error] cannot use negative line amount')
				} else {
					d.print_context_lines(info.file, info.line, lines)!
				}
			}
			'method?' {
				println(info.is_method)
			}
			'mem', 'memory' {
				d.print_memory_use()
			}
			'm', 'mod' {
				println(info.mod)
			}
			'p', 'print' {
				if args == '' {
					println('[error] var name is expected as parameter')
				} else {
					info.show_variable(args)
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
					println('[error] var name is expected as parameter')
				} else {
					d.unwatch_var(args)
				}
			}
			'w', 'watch' {
				if args == '' {
					println('[error] var name is expected as parameter')
				} else {
					d.watch_var(args)
				}
			}
			'' {
				if is_ctrl {
					println('')
				}
			}
			else {
				println('unknown command `${cmd}`')
			}
		}
	}
}
