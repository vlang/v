// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import term
import rand
import readline
import os.cmdline
import v.util

struct Repl {
mut:
	readline readline.Readline
	indent   int    // indentation level
	in_func  bool   // are we inside a new custom user function
	line     string // the current line entered by the user
	//
	modules        []string // all the import modules
	includes       []string // all the #include statements
	functions      []string // all the user function declarations
	functions_name []string // all the user function names
	lines          []string // all the other lines/statements
	temp_lines     []string // all the temporary expressions/printlns
	vstartup_lines []string // lines in the `VSTARTUP` file
}

const is_stdin_a_pipe = (is_atty(0) == 0)

const vexe = os.getenv('VEXE')

const vstartup = os.getenv('VSTARTUP')

fn new_repl() Repl {
	return Repl{
		readline: readline.Readline{}
		modules: ['os', 'time', 'math']
		vstartup_lines: os.read_file(vstartup) or { '' }.trim_right('\n\r').split_into_lines()
	}
}

fn (mut r Repl) checks() bool {
	mut in_string := false
	was_indent := r.indent > 0
	for i := 0; i < r.line.len; i++ {
		if r.line[i] == `\'` && (i == 0 || r.line[i - 1] != `\\`) {
			in_string = !in_string
		}
		if r.line[i] == `{` && !in_string {
			r.line = r.line[..i + 1] + '\n' + r.line[i + 1..]
			i++
			r.indent++
		}
		if r.line[i] == `}` && !in_string {
			r.line = r.line[..i] + '\n' + r.line[i..]
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
	return r.in_func || (was_indent && r.indent <= 0) || r.indent > 0
}

fn (r &Repl) function_call(line string) bool {
	for function in r.functions_name {
		is_function_definition := line.replace(' ', '').starts_with('$function:=')
		if line.starts_with(function) && !is_function_definition {
			return true
		}
	}
	return false
}

fn (r &Repl) current_source_code(should_add_temp_lines bool, not_add_print bool) string {
	mut all_lines := []string{}
	for mod in r.modules {
		all_lines << 'import $mod\n'
	}
	if vstartup != '' {
		mut lines := []string{}
		if !not_add_print {
			lines = r.vstartup_lines.filter(!it.starts_with('print'))
		} else {
			lines = r.vstartup_lines
		}
		all_lines << lines
	}
	all_lines << r.includes
	all_lines << r.functions
	all_lines << r.lines

	if should_add_temp_lines {
		all_lines << r.temp_lines
	}
	return all_lines.join('\n')
}

fn repl_help() {
	println(util.full_v_version(false))
	println('
	|help                   Displays this information.
	|list                   Show the program so far.
	|reset                  Clears the accumulated program, so you can start a fresh.
	|Ctrl-C, Ctrl-D, exit   Exits the REPL.
	|clear                  Clears the screen.
'.strip_margin())
}

fn run_repl(workdir string, vrepl_prefix string) {
	if !is_stdin_a_pipe {
		println(util.full_v_version(false))
		println('Use Ctrl-C or `exit` to exit, or `help` to see other available commands')
	}

	if vstartup != '' {
		result := repl_run_vfile(vstartup) or {
			os.Result{
				output: '$vstartup file not found'
			}
		}
		print('\n')
		print_output(result)
	}

	file := os.join_path(workdir, '.${vrepl_prefix}vrepl.v')
	temp_file := os.join_path(workdir, '.${vrepl_prefix}vrepl_temp.v')
	mut prompt := '>>> '
	defer {
		if !is_stdin_a_pipe {
			println('')
		}
		cleanup_files([file, temp_file])
	}
	mut r := new_repl()
	for {
		if r.indent == 0 {
			prompt = '>>> '
		} else {
			prompt = '... '
		}
		oline := r.get_one_line(prompt) or { break }
		line := oline.trim_space()
		if line == '' && oline.ends_with('\n') {
			continue
		}
		if line.len <= -1 || line == '' || line == 'exit' {
			break
		}
		r.line = line
		if r.line == '\n' {
			continue
		}
		if r.line == 'clear' {
			term.erase_clear()
			continue
		}
		if r.line == 'help' {
			repl_help()
			continue
		}
		if r.line.contains(':=') && r.line.contains('fn(') {
			r.in_func = true
			r.functions_name << r.line.all_before(':= fn(').trim_space()
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
				} else {
					r.temp_lines << rline
				}
			}
			if r.indent > 0 {
				continue
			}
			r.line = ''
		}
		if r.line == 'debug_repl' {
			eprintln('repl: $r')
			continue
		}
		if r.line == 'reset' {
			r = new_repl()
			continue
		}
		if r.line == 'list' {
			source_code := r.current_source_code(true, true)
			println('//////////////////////////////////////////////////////////////////////////////////////')
			println(source_code)
			println('//////////////////////////////////////////////////////////////////////////////////////')
			continue
		}
		// Save the source only if the user is printing something,
		// but don't add this print call to the `lines` array,
		// so that it doesn't get called during the next print.
		if r.line.starts_with('=') {
			r.line = 'println(' + r.line[1..] + ')'
		}
		if r.line.starts_with('print') {
			source_code := r.current_source_code(false, false) + '\n$r.line\n'
			os.write_file(file, source_code) or { panic(err) }
			s := repl_run_vfile(file) or { return }
			print_output(s)
		} else {
			mut temp_line := r.line
			mut temp_flag := false
			func_call := r.function_call(r.line)
			filter_line := r.line.replace(r.line.find_between("'", "'"), '').replace(r.line.find_between('"',
				'"'), '')
			possible_statement_patterns := [
				'++',
				'--',
				'<<',
				'//',
				'/*',
				'fn ',
				'pub ',
				'mut ',
				'enum ',
				'const ',
				'struct ',
				'interface ',
				'import ',
				'#include ',
				'for ',
				'or ',
				'insert',
				'delete',
				'prepend',
				'sort',
				'clear',
				'trim',
			]
			mut is_statement := false
			if filter_line.count('=') % 2 == 1 {
				is_statement = true
			} else {
				for pattern in possible_statement_patterns {
					if filter_line.contains(pattern) {
						is_statement = true
						break
					}
				}
			}
			// NB: starting a line with 2 spaces escapes the println heuristic
			if oline.starts_with('  ') {
				is_statement = true
			}
			if !is_statement && !func_call && r.line != '' {
				temp_line = 'println($r.line)'
				temp_flag = true
			}
			mut temp_source_code := ''
			if temp_line.starts_with('import ') {
				mod := r.line.fields()[1]
				if mod !in r.modules {
					temp_source_code = '$temp_line\n' + r.current_source_code(false, true)
				}
			} else if temp_line.starts_with('#include ') {
				temp_source_code = '$temp_line\n' + r.current_source_code(false, false)
			} else {
				for i, l in r.lines {
					if (l.starts_with('for ') || l.starts_with('if ')) && l.contains('println') {
						r.lines.delete(i)
						break
					}
				}
				temp_source_code = r.current_source_code(true, false) + '\n$temp_line\n'
			}
			os.write_file(temp_file, temp_source_code) or { panic(err) }
			s := repl_run_vfile(temp_file) or { return }
			if !func_call && s.exit_code == 0 && !temp_flag {
				for r.temp_lines.len > 0 {
					if !r.temp_lines[0].starts_with('print') {
						r.lines << r.temp_lines[0]
					}
					r.temp_lines.delete(0)
				}
				if r.line.starts_with('import ') {
					mod := r.line.fields()[1]
					if mod !in r.modules {
						r.modules << mod
					}
				} else if r.line.starts_with('#include ') {
					r.includes << r.line
				} else {
					r.lines << r.line
				}
			} else {
				for r.temp_lines.len > 0 {
					r.temp_lines.delete(0)
				}
			}
			print_output(s)
		}
	}
}

fn print_output(s os.Result) {
	lines := s.output.trim_right('\n\r').split_into_lines()
	for line in lines {
		if line.contains('.vrepl_temp.v:') {
			// Hide the temporary file name
			sline := line.all_after('.vrepl_temp.v:')
			idx := sline.index(' ') or {
				println(sline)
				return
			}
			println(sline[idx + 1..])
		} else if line.contains('.vrepl.v:') {
			// Ensure that .vrepl.v: is at the start, ignore the path
			// This is needed to have stable .repl tests.
			idx := line.index('.vrepl.v:') or { return }
			println(line[idx..])
		} else {
			println(line)
		}
	}
}

fn main() {
	// Support for the parameters replfolder and replprefix is needed
	// so that the repl can be launched in parallel by several different
	// threads by the REPL test runner.
	args := cmdline.options_after(os.args, ['repl'])
	replfolder := os.real_path(cmdline.option(args, '-replfolder', os.temp_dir()))
	replprefix := cmdline.option(args, '-replprefix', 'noprefix.${rand.ulid()}.')
	if !os.exists(os.getenv('VEXE')) {
		println('Usage:')
		println('  VEXE=vexepath vrepl\n')
		println('  ... where vexepath is the full path to the v executable file')
		return
	}
	run_repl(replfolder, replprefix)
}

fn rerror(s string) {
	println('V repl error: $s')
	os.flush()
}

fn (mut r Repl) get_one_line(prompt string) ?string {
	if is_stdin_a_pipe {
		iline := os.get_raw_line()
		if iline.len == 0 {
			return none
		}
		return iline
	}
	rline := r.readline.read_line(prompt) or { return none }
	return rline
}

fn cleanup_files(files []string) {
	for file in files {
		os.rm(file) or { }
		$if windows {
			os.rm(file[..file.len - 2] + '.exe') or { }
			$if msvc {
				os.rm(file[..file.len - 2] + '.ilk') or { }
				os.rm(file[..file.len - 2] + '.pdb') or { }
			}
		} $else {
			os.rm(file[..file.len - 2]) or { }
		}
	}
}

fn repl_run_vfile(file string) ?os.Result {
	$if trace_repl_temp_files ? {
		eprintln('>> repl_run_vfile file: $file')
	}
	s := os.execute('"$vexe" -repl run "$file"')
	if s.exit_code < 0 {
		rerror(s.output)
		return error(s.output)
	}
	return s
}
