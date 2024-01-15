// Copyright (c) 2019-2024 V devs. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module debug

import os
import math
import time
import readline
import strings

const prompt = 'vdbg> '

fn print_current_file(path string, line int) ! {
	file_content := os.read_file(path)!
	chunks := file_content.split('\n')
	offset := math.max(line - 4, 1)
	for n, s in chunks[math.max(line - 5, 0)..math.min(chunks.len, line)] {
		println('${n + offset:04} ${s}')
	}
}

fn print_help() {
	println('vdbg commands:')
	println('  anon?\t\t\tcheck if the current context is anon')
	println('  bt\t\t\tprints a backtrace')
	println('  c, continue\t\tcontinue debugging')
	println('  f,file\t\tshow current file name')
	println('  fn,func\t\tshow current function name')
	println('  generic?\t\tcheck if the current context is generic')
	println('  h, help\t\tshow this help')
	println('  list\t\t\tshow 5 lines from current file')
	println('  l, line\t\tshow current line number')
	println('  method?\t\tcheck if the current context is a method')
	println('  m,mod\t\t\tshow current module name')
	println('  p,print <arg>\t\tprints an variable')
	println('  q,quit\t\texits debugging session in the code')
	println('  scope\t\t\tshow the vars in the inner most scope')
	println('  s,profile\t\tstart CPU profiling session')
	println('  e,profileEnd\t\tstop current CPU profiling session')
	println('')
}

pub struct DebugContextVar {
	name  string
	typ   string
	value string
}

// DebugContextInfo has the context info for the debugger repl
pub struct DebugContextInfo {
	is_anon           bool   // context: cur fn is anon?
	is_generic        bool   // context: cur fn is a generic?
	is_method         bool   // context: cur fn is a bool?
	receiver_typ_name string // context: cur receiver type name (method only)
	line              int    // cur line number
	file              string // cur file name
	mod               string // cur module name
	fn_name           string // cur function name
	scope             map[string]DebugContextVar // inner most scope var data
}

fn (d DebugContextInfo) show_variable(args []string) {
	if info := d.scope[args[0]] {
		println('${args[0]} = ${info.value} (${info.typ})')
	}
}

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
	if d.is_generic {
		s.write_string(' [generic]')
	}
	return s.str()
}

// debugger is the implementation for C backend's debugger statement (debugger;)
@[unsafe]
pub fn debugger(info DebugContextInfo) ! {
	mut static profile := u64(0)
	mut static exited := 0
	if exited != 0 {
		return
	}

	mut r := readline.Readline{}

	println('debugger at ${info.file}:${info.line} - ${info.ctx()}')
	for {
		input := r.read_line(debug.prompt) or { '' }
		splitted := input.split(' ')
		cmd := splitted[0]
		args := splitted[1..]
		match cmd {
			'anon?' {
				println(info.is_anon)
			}
			'bt' {
				print_backtrace_skipping_top_frames(2)
			}
			'', 'c', 'continue' {
				break
			}
			'f', 'file' {
				println(info.file)
			}
			'fn', 'func' {
				println(info.fn_name)
			}
			'generic?' {
				println(info.is_generic)
			}
			'h', 'help' {
				print_help()
			}
			'list' {
				print_current_file(info.file, info.line)!
			}
			'l', 'line' {
				println(info.line.str())
			}
			'method?' {
				println(info.is_method)
			}
			'm', 'mod' {
				println(info.mod)
			}
			'p', 'print' {
				info.show_variable(args)
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
			else {
				println('unknown command `${cmd}`')
			}
		}
	}
}
