// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import os
import term

struct Repl {
mut:
	indent         int
	in_func        bool
	lines          []string
	temp_lines     []string
	functions_name []string
	functions      []string
}

fn (r mut Repl) checks(line string) bool {
	mut in_string := false
	was_indent := r.indent > 0

	for i := 0; i < line.len; i++ {
		if line[i] == `\'` && (i == 0 || line[i - 1] != `\\`) {
			in_string = !in_string
		}
		if line[i] == `{` && !in_string {
			r.indent++
		}
		if line[i] == `}` && !in_string {
			r.indent--
			if r.indent == 0 {
				r.in_func = false
			}
		}
		if i + 2 < line.len && r.indent == 0 && line[i + 1] == `f` && line[i + 2] == `n` {
			r.in_func = true
		}
	}
	return r.in_func || (was_indent && r.indent <= 0) || r.indent > 0
}

fn (r &Repl) function_call(line string) bool {
	for function in r.functions_name {
		if line.starts_with(function) {
			return true
		}
	}
	return false
}

fn repl_help() {
version_hash := vhash()
println('
V $Version $version_hash
  help                   Displays this information.
  Ctrl-C, Ctrl-D, exit   Exits the REPL.
  clear                  Clears the screen.
')
}

fn run_repl() []string {
	version_hash := vhash()
	println('V $Version $version_hash')
	println('Use Ctrl-C or `exit` to exit')
	file := '.vrepl.v'
	temp_file := '.vrepl_temp.v'
	defer {
		os.rm(file)
		os.rm(temp_file)
		os.rm(file.left(file.len - 2))
		os.rm(temp_file.left(temp_file.len - 2))
	}
	mut r := Repl{}
	vexe := os.args[0]
	for {
		if r.indent == 0 {
			print('>>> ')
		}
		else {
			print('... ')
		}
		mut line := os.get_raw_line()
		if line.trim_space() == '' && line.ends_with('\n') {
			continue
		}
		line = line.trim_space()
		if line.len == -1 || line == '' || line == 'exit' {
			break
		}
		if line == '\n' {
			continue
		}
		if line == 'clear' {
			term.erase_display('2')
			continue
		}
		if line == 'help' {
			repl_help()
			continue
		}
		if line.starts_with('fn') {
			r.in_func = true
			r.functions_name << line.all_after('fn').all_before('(').trim_space()
		}
		was_func := r.in_func
		if r.checks(line) {
			if r.in_func || was_func {
				r.functions << line
			}
			else {
				r.temp_lines << line
			}
			if r.indent > 0 {
				continue
			}
			line = ''
		}
		// Save the source only if the user is printing something,
		// but don't add this print call to the `lines` array,
		// so that it doesn't get called during the next print.
		if line.starts_with('print') {
			source_code := r.functions.join('\n') + r.lines.join('\n') + '\n' + line
			os.write_file(file, source_code)
			s := os.exec('$vexe run $file -repl') or {
				verror(err)
				return []string
			}
			vals := s.output.split('\n')
			for i:=0; i < vals.len; i++ {
				println(vals[i])
			}
		}
		else {
			mut temp_line := line
			mut temp_flag := false
			func_call := r.function_call(line)
			if !(line.contains(' ') || line.contains(':') || line.contains('=') || line.contains(',') || line == '') && !func_call {
				temp_line = 'println($line)'
				temp_flag = true
			}
			temp_source_code := r.functions.join('\n') + r.lines.join('\n') + r.temp_lines.join('\n') + '\n' + temp_line
			os.write_file(temp_file, temp_source_code)
			s := os.exec('$vexe run $temp_file -repl') or {
				verror(err)
				return []string
			}
			if !func_call && s.exit_code == 0 {
				for r.temp_lines.len > 0 {
					if !r.temp_lines[0].starts_with('print') {
						r.lines << r.temp_lines[0]
					}
					r.temp_lines.delete(0)
				}
				r.lines << line
			}
			else {
				for r.temp_lines.len > 0 {
					r.temp_lines.delete(0)
				}
			}
			vals := s.output.split('\n')
			for i:=0; i<vals.len; i++ {
				println(vals[i])
			}
		}
	}
	return r.lines
}
