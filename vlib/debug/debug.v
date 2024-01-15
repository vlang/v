// Copyright (c) 2019-2024 V devs. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module debug

import os
import math
import time
import readline

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
	println('vdbg commands: ')
	println('  bt\t\t\tprints a backtrace')
	println('  c, continue\t\tcontinue debugging')
	println('  f,file\t\tshow current file name')
	println('  fn,func\t\tshow current function name')
	println('  h, help\t\tshow this help')
	println('  list\t\t\tshow 5 lines from current file')
	println('  l, line\t\tshow current line number')
	println('  m,mod\t\t\tshow current module name')
	println('  q,quit\t\texits debugging session in the code')
	println('  s,profile\t\tstart CPU profiling session')
	println('  e,profileEnd\t\tstop current CPU profiling session')
	println('')
}

@[unsafe]
pub fn vdebugger(line_no int, file string, mod string, fn_name string) ! {
	mut static profile := u64(0)
	mut static exited := 0
	if exited != 0 {
		return
	}

	println('debugger at ${file}:${line_no} - ctx: ${mod}:${fn_name}')

	for {
		cmd := readline.read_line(debug.prompt) or { '' }
		match cmd {
			'bt' {
				print_backtrace_skipping_top_frames(2)
			}
			'', 'c', 'continue' {
				break
			}
			'f', 'file' {
				println(file)
			}
			'fn', 'func' {
				println(fn_name)
			}
			'h', 'help' {
				print_help()
			}
			'list' {
				print_current_file(file, line_no)!
			}
			'l', 'line' {
				println(line_no.str())
			}
			'm', 'mod' {
				println(mod)
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
