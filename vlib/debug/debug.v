// Copyright (c) 2019-2024 Felipe Pena. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module debug

import os
import math
import time
import readline
import strings

// print_help prints the debugger REPL commands help
fn print_help() {
	println('vdbg commands:')
	println('  anon?\t\t\tcheck if the current context is anon')
	println('  bt\t\t\tprints a backtrace')
	println('  c, continue\t\tcontinue debugging')
	println('  generic?\t\tcheck if the current context is generic')
	println('  heap\t\t\tshow heap memory usage')
	println('  h, help, ?\t\tshow this help')
	println('  l, list\t\tshow some lines from current break')
	println('  mem, memory\t\tshow memory usage')
	println('  method?\t\tcheck if the current context is a method')
	println('  m, mod\t\tshow current module name')
	println('  p, print <arg>\tprints an variable')
	println('  q, quit\t\texits debugging session in the code')
	println('  scope\t\t\tshow the vars in the inner most scope')
	println('  s, profile\t\tstart CPU profiling session')
	println('  e, profileEnd\t\tstop current CPU profiling session')
	println('')
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
	scope             map[string]DebugContextVar // inner most scope var data
}

// show_variable prints the variable info if found into the cur context
fn (d DebugContextInfo) show_variable(var_name string) {
	if info := d.scope[var_name] {
		println('${var_name} = ${info.value} (${info.typ})')
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

// print_current_file prints 5 lines before current location
fn print_current_file(path string, line int) ! {
	file_content := os.read_file(path)!
	chunks := file_content.split('\n')
	offset := math.max(line - 4, 1)
	for n, s in chunks[math.max(line - 5, 0)..math.min(chunks.len, line)] {
		println('${n + offset:04} ${s}')
	}
}

// print_memory_use prints the GC memory use
fn print_memory_use() {
	println(gc_memory_use())
}

// print_heap_usage prints the GC heap usage
fn print_heap_usage() {
	h := gc_heap_usage()
	println('heap size: ${h.heap_size}')
	println('free bytes: ${h.free_bytes}')
	println('total bytes: ${h.total_bytes}')
}

// debugger is the implementation for C backend's debugger statement (debugger;)
@[unsafe]
pub fn debugger(info DebugContextInfo) ! {
	mut static profile := u64(0)
	mut static exited := 0
	mut static last_cmd := ''
	mut static last_args := [2]string{}

	if exited != 0 {
		return
	}

	prompt := '${info.file}:${info.line} vdbg> '
	mut r := readline.Readline{}

	println('Break on ${info.ctx()} in ${info.file}:${info.line}')
	for {
		mut is_ctrl := false
		input := r.read_line(prompt) or {
			is_ctrl = true
			''
		}
		splitted := input.split(' ')

		mut cmd := ''
		mut args := [2]string{}
		if !is_ctrl && splitted[0] == '' {
			cmd = last_cmd
			args = last_args
		} else {
			cmd = if is_ctrl { last_cmd } else { splitted[0] }
			args[0] = if splitted.len > 1 { splitted[1] } else { '' }
			args[1] = if splitted.len > 2 { splitted[2] } else { '' }
			last_cmd = cmd
			last_args = args
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
				print_heap_usage()
			}
			'?', 'h', 'help' {
				print_help()
			}
			'l', 'list' {
				print_current_file(info.file, info.line)!
			}
			'method?' {
				println(info.is_method)
			}
			'mem', 'memory' {
				print_memory_use()
			}
			'm', 'mod' {
				println(info.mod)
			}
			'p', 'print' {
				if args[0] == '' {
					println('[error] var name is expected as parameter')
				} else {
					info.show_variable(args[0])
				}
			}
			'scope' {
				info.show_scope()
			}
			's', 'profile' {
				profile = time.sys_mono_now()
				println('profiler :: starting profiler')
			}
			'e', 'profileEnd' {
				println('profiler :: elapsed time: ${time.Duration(time.sys_mono_now() - profile)}')
			}
			'q', 'quit' {
				exited = 1
				break
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
