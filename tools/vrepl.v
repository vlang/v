// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import (
	os
	term
	readline
)

struct Repl {
mut:
	indent         int
	in_func        bool
	line           string
	lines          []string
	temp_lines     []string
	functions_name []string
	functions      []string
}

fn (r mut Repl) checks() bool {
	mut in_string := false
	mut is_cut := false
	was_indent := r.indent > 0

	for i := 0; i < r.line.len; i++ {
		if r.line[i] == `\'` && (i == 0 || r.line[i - 1] != `\\`) {
			in_string = !in_string
		}
		if r.line[i] == `{` && !in_string {
			r.line = r.line[..i + 1] + '\n' + r.line[i + 1..]
			is_cut = true
			i++
			r.indent++
		}
		if r.line[i] == `}` && !in_string {
			r.line = r.line[..i] + '\n' + r.line[i..]
			is_cut = true
			i++
			r.indent--
			if r.indent == 0 {
				r.in_func = false
			}
		}
		if i + 2 < r.line.len && r.indent == 0 && r.line[i + 1] == `f` && r.line[i + 2] == `n` {
			r.in_func = true
		}
	}
	return r.in_func || (was_indent && r.indent <= 0) || r.indent > 0 || is_cut
}

fn (r &Repl) function_call(line string) bool {
	for function in r.functions_name {
		if line.starts_with(function) {
			return true
		}
	}
	return false
}

pub fn repl_help() {
	version := v_version()
	println(version)
	println('
  help                   Displays this information.
  Ctrl-C, Ctrl-D, exit   Exits the REPL.
  clear                  Clears the screen.
')
}

pub fn run_repl() []string {
	version := v_version()
	println(version)
	println('Use Ctrl-C or `exit` to exit')
	file := '.vrepl.v'
	temp_file := '.vrepl_temp.v'
	mut prompt := '>>> '
	defer {
		os.rm(file)
		os.rm(temp_file)
		os.rm(file[..file.len - 2])
		os.rm(temp_file[..temp_file.len - 2])
	}
	mut r := Repl{}
	mut readline := readline.Readline{}
	vexe := os.getenv('VEXE')
	for {
		if r.indent == 0 {
			prompt = '>>> '
		}
		else {
			prompt = '... '
		}
		mut line := readline.read_line(prompt) or {
			break
		}
		if line.trim_space() == '' && line.ends_with('\n') {
			continue
		}
		line = line.trim_space()
		if line.len <= -1 || line == '' || line == 'exit' {
			break
		}
		r.line = line
		if r.line == '\n' {
			continue
		}
		if r.line == 'clear' {
			term.erase_display('2')
			continue
		}
		if r.line == 'help' {
			repl_help()
			continue
		}
		if r.line.starts_with('fn') {
			r.in_func = true
			r.functions_name << r.line.all_after('fn').all_before('(').trim_space()
		}
		was_func := r.in_func
		if r.checks() {
			for rline in r.line.split('\n') {
				if r.in_func || was_func {
					r.functions << rline
				}
				else {
					r.temp_lines << rline
				}
			}
			if r.indent > 0 {
				continue
			}
			r.line = ''
		}
		// Save the source only if the user is printing something,
		// but don't add this print call to the `lines` array,
		// so that it doesn't get called during the next print.
		if r.line.starts_with('print') {
			source_code := r.functions.join('\n') + r.lines.join('\n') + '\n' + r.line
			os.write_file(file, source_code)
			s := os.exec('"$vexe" run $file -repl') or {
				rerror(err)
				return []
			}
			print_output(s)
		}
		else {
			mut temp_line := r.line
			mut temp_flag := false
			func_call := r.function_call(r.line)
			if !(r.line.contains(' ') || r.line.contains(':') || r.line.contains('=') || r.line.contains(',') || r.line == '') && !func_call {
				temp_line = 'println($r.line)'
				temp_flag = true
			}
			temp_source_code := r.functions.join('\n') + r.lines.join('\n') + '\n' + r.temp_lines.join('\n') + '\n' + temp_line
			os.write_file(temp_file, temp_source_code)
			s := os.exec('"$vexe" run $temp_file -repl') or {
				println("SDFSDF")
				rerror(err)
				return []
			}
			if !func_call && s.exit_code == 0 && !temp_flag {
				for r.temp_lines.len > 0 {
					if !r.temp_lines[0].starts_with('print') {
						r.lines << r.temp_lines[0]
					}
					r.temp_lines.delete(0)
				}
				r.lines << r.line
			}
			else {
				for r.temp_lines.len > 0 {
					r.temp_lines.delete(0)
				}
			}
			print_output(s)
		}
	}
	return r.lines
}

fn print_output(s os.Result) {
	lines := s.output.split('\n')
	for line in lines {
		if line.starts_with('.vrepl_temp.v') {
			// Hide the temporary file name
			idx := line.index(' ') or {
				println(line)
				return
			}
			println(line[idx+1..])
		}	 else {
			println(line)
		}
	}

}

fn main() {
	if !os.exists(os.getenv('VEXE')) {
		println('Usage:')
		println('  VEXE=vexepath vrepl\n')
		println('  ... where vexepath is the full path to the v executable file')
		return
	}
	run_repl()
}

pub fn rerror(s string) {
	println('V repl error: $s')
	os.flush_stdout()
	exit(1)
}

fn v_version() string {
	vexe := os.getenv('VEXE')
	vversion_res := os.exec('$vexe --version') or { panic('"$vexe --version" is not working') }
	return vversion_res.output
}
