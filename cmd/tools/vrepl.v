// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import term
import rand
import readline
import os.cmdline
import v.util.version

struct Repl {
mut:
	readline     readline.Readline
	indent       int    // indentation level
	in_func      bool   // inside function decl
	in_struct    bool   // inside struct decl
	in_enum      bool   // inside enum decl
	in_interface bool   // inside interface decl
	line         string // the current line entered by the user
	is_pin       bool   // does the repl 'pin' entered source code
	folder       string // the folder in which the repl will write its temporary source files
	last_output  string // the last repl output

	modules         map[string][]string // all the import modules
	alias           map[string]string   // all the alias used in the import
	includes        []string            // all the #include statements
	functions       []string            // all the user function declarations
	functions_name  []string            // all the user function names
	structs         []string            // all the struct definitions
	enums           []string            // all the enum definitions
	consts          []string            // all the const definitions
	types           []string            // all the type definitions
	interfaces      []string            // all the interface definitions
	lines           []string            // all the other lines/statements
	temp_lines      []string            // all the temporary expressions/printlns
	vstartup_lines  []string            // lines in the `VSTARTUP` file
	eval_func_lines []string            // same line of the `VSTARTUP` file, but used to test fn type
}

const is_stdin_a_pipe = os.is_atty(0) == 0
const vexe = os.getenv('VEXE')
const vquiet = os.getenv('VQUIET') != ''
const vstartup = os.getenv('VSTARTUP')
const repl_folder = os.join_path(os.vtmp_dir(), 'repl')

const possible_statement_patterns = [
	'++',
	'--',
	'//',
	'/*',
	'assert ',
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
	'if ',
	'or ',
	' as ',
]

enum FnType {
	none
	void
	fn_type
}

enum DeclType {
	include   // #include ...
	const     // const ...
	type      // type ...
	enum      // enum ...
	fn        // fn ...
	struct    // struct ...
	interface // interface ...
	stmt      // statement
}

fn new_repl(folder string) Repl {
	vstartup_source := os.read_file(vstartup) or { '' }.trim_right('\n\r').split_into_lines()
	os.mkdir_all(folder) or {}
	return Repl{
		readline:       readline.Readline{
			skip_empty: true
		}
		folder:         folder
		modules:        {
			'os':   []
			'time': []
			'math': []
		}
		vstartup_lines: vstartup_source
		// Test file used to check if a function as a void return or a value return.
		eval_func_lines: vstartup_source
	}
}

fn endline_if_missed(line string) string {
	if line.ends_with('\n') {
		return line
	}
	return line + '\n'
}

fn starts_with_type_decl(line string, type_name string) bool {
	if line.starts_with(type_name + ' ') || line.starts_with(type_name + '\t') {
		return true
	}
	if line.starts_with('pub ') || line.starts_with('pub\t') {
		substring := line[3..].trim_space()
		if substring.starts_with(type_name + ' ') || substring.starts_with(type_name + '\t') {
			return true
		}
	}
	return false
}

fn repl_help() {
	println(version.full_v_version(false))
	println('
	|help                   Displays this information.
	|list                   Show the program so far.
	|reset                  Clears the accumulated program, so you can start a fresh.
	|Ctrl-C, Ctrl-D, exit   Exits the REPL.
	|clear                  Clears the screen.
	|pin                    Pins the entered program to the top.
	|!sh [COMMAND]          Execute on REPL shell commands.
'.strip_margin())
}

fn run_shell(command string) {
	if command.len >= 2 && command[0..2] == 'cd' {
		command_splited := command.split(' ')
		assert command_splited.len >= 2
		dir := command_splited[command_splited.len - 1]

		os.chdir(dir) or { eprintln('`${command}` failed, err: ${err}') }
	} else {
		os.system(command)
	}
}

fn (mut r Repl) checks() bool {
	mut in_string := false
	was_indent := r.indent > 0
	for i := 0; i < r.line.len; i++ {
		if r.line[i] == `'` && (i == 0 || r.line[i - 1] != `\\`) {
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
				r.in_struct = false
				r.in_enum = false
				r.in_interface = false
			}
		}
	}
	return (was_indent && r.indent <= 0) || r.indent > 0
}

fn (r &Repl) function_call(line string) (bool, FnType) {
	for function in r.functions_name {
		is_function_definition := line.replace(' ', '').starts_with('${function}:=')
		if line.starts_with(function) && !is_function_definition {
			// TODO(vincenzopalazzo) store the type of the function here
			fntype := r.check_fn_type_kind(line)
			return true, fntype
		}
	}

	if line.contains(':=') {
		// an assignment to a variable:
		// `z := abc()`
		return false, FnType.none
	}

	// Check if it is a Vlib call
	// TODO(vincenzopalazzo): auto import the module?
	if r.is_function_call(line) {
		fntype := r.check_fn_type_kind(line)
		return true, fntype
	}
	return false, FnType.none
}

// TODO(vincenzopalazzo) Remove this fancy check and add a regex
fn (r &Repl) is_function_call(line string) bool {
	return !line.starts_with('[') && line.contains('.') && line.contains('(')
		&& (line.ends_with(')') || line.ends_with('?') || line.ends_with('!'))
}

// Convert the list of modules that we parsed already,
// to a sequence of V source code lines
fn (r &Repl) import_to_source_code() []string {
	mut imports_line := []string{}
	for mod, value in r.modules {
		mut import_str := 'import ${mod}'
		if mod in r.alias {
			import_str += ' as ${r.alias[mod]}'
		}
		if value.len > 0 {
			import_str += '{ '
			for val in value {
				import_str += '${val}, '
			}
			import_str += '}'
		}
		imports_line << endline_if_missed(import_str)
	}
	return imports_line
}

fn (r &Repl) current_source_code(should_add_temp_lines bool, not_add_print bool) string {
	mut all_lines := r.import_to_source_code()

	if vstartup != '' {
		mut lines := []string{}
		if !not_add_print {
			lines = r.vstartup_lines.filter(!it.starts_with('print'))
		} else {
			lines = r.vstartup_lines.clone()
		}
		all_lines << lines
	}
	all_lines << r.includes
	all_lines << r.types
	all_lines << r.enums
	all_lines << r.consts
	all_lines << r.structs
	all_lines << r.interfaces
	all_lines << r.functions
	all_lines << r.lines

	if should_add_temp_lines {
		all_lines << r.temp_lines
	}
	return all_lines.join('\n')
}

fn (r &Repl) insert_source_code(typ DeclType, lines []string) string {
	mut all_lines := r.import_to_source_code()

	if vstartup != '' {
		all_lines << r.vstartup_lines.filter(!it.starts_with('print'))
	}
	all_lines << r.includes
	if typ == .include {
		all_lines << lines
	}
	all_lines << r.types
	if typ == .type {
		all_lines << lines
	}
	all_lines << r.enums
	if typ == .enum {
		all_lines << lines
	}
	all_lines << r.consts
	if typ == .const {
		all_lines << lines
	}
	all_lines << r.structs
	if typ == .struct {
		all_lines << lines
	}
	all_lines << r.interfaces
	if typ == .interface {
		all_lines << lines
	}
	all_lines << r.functions
	if typ == .fn {
		all_lines << lines
	}
	all_lines << r.lines
	if typ == .stmt {
		all_lines << lines
	}
	return all_lines.join('\n')
}

// the new_line is probably a function call, but some function calls
// do not return anything, while others return results.
// This function checks which one we have:
fn (r &Repl) check_fn_type_kind(new_line string) FnType {
	source_code := r.current_source_code(true, false) + '\nprintln(${new_line})'
	check_file := os.join_path(r.folder, '${rand.ulid()}.vrepl.check.v')
	os.write_file(check_file, source_code) or { panic(err) }
	defer {
		os.rm(check_file) or {}
	}
	// -w suppresses the unused import warnings
	// -check just does syntax and checker analysis without generating/running code
	os_response := os.execute('${os.quoted_path(vexe)} -w -check ${os.quoted_path(check_file)}')
	str_response := convert_output(os_response.output)
	if os_response.exit_code != 0 && str_response.contains('can not print void expressions') {
		return FnType.void
	}
	return FnType.fn_type
}

// parse the import statement in `line`, updating the Repl alias maps
fn (mut r Repl) parse_import(line string) {
	if !line.contains('import') {
		eprintln("the line doesn't contain an `import` keyword")
		return
	}
	tokens := r.line.fields()
	// module name
	mod := tokens[1]
	// set alias
	if line.contains('as ') && tokens.len >= 4 {
		alias := tokens[3]
		if mod !in r.alias {
			r.alias[mod] = alias
		}
	}

	// set value
	if line.contains('{') && line.contains('}') {
		values := line.split('{')[1].split('}')[0]
		for value in values.split(',') {
			r.modules[mod] << value
		}
	} else {
		if mod !in r.modules {
			r.modules[mod] = []string{}
		}
	}
}

// clear the screen, then list source code
fn (mut r Repl) pin() {
	term.erase_clear()
	r.list_source()
}

// print source code
fn (mut r Repl) list_source() {
	source_code := r.current_source_code(true, true)
	println('\n${source_code.replace('\n\n', '\n')}')
}

fn highlight_console_command(command string) string {
	return term.bright_white(term.bright_bg_black(' ${command} '))
}

fn highlight_repl_command(command string) string {
	return term.bright_white(term.bg_blue(' ${command} '))
}

fn print_welcome_screen() {
	if vquiet {
		return
	}
	cmd_exit := highlight_repl_command('exit')
	cmd_list := highlight_repl_command('list')
	cmd_help := highlight_repl_command('help')
	cmd_v_help := highlight_console_command('v help')
	cmd_v_run := highlight_console_command('v run main.v')
	file_main := highlight_console_command('main.v')
	vbar := term.bright_green('|')
	width, _ := term.get_terminal_size() // get the size of the terminal
	vlogo := [
		term.bright_blue(r' ____    ____ '),
		term.bright_blue(r' \   \  /   / '),
		term.bright_blue(r'  \   \/   /  '),
		term.bright_blue(r'   \      /   '),
		term.bright_blue(r'    \    /    '),
		term.bright_blue(r'     \__/     '),
	]
	help_text := [
		'Welcome to the V REPL (for help with V itself, type ${cmd_exit}, then run ${cmd_v_help}).',
		'Note: the REPL is highly experimental. For best V experience, use a text editor, ',
		'save your code in a ${file_main} file and execute: ${cmd_v_run}',
		'${version.full_v_version(false)} . Use ${cmd_list} to see the accumulated program so far.',
		'Use Ctrl-C or ${cmd_exit} to exit, or ${cmd_help} to see other available commands.',
	]
	if width >= 97 {
		eprintln('${vlogo[0]}')
		eprintln('${vlogo[1]} ${vbar}  ${help_text[0]}')
		eprintln('${vlogo[2]} ${vbar}  ${help_text[1]}')
		eprintln('${vlogo[3]} ${vbar}  ${help_text[2]}')
		eprintln('${vlogo[4]} ${vbar}  ${help_text[3]}')
		eprintln('${vlogo[5]} ${vbar}  ${help_text[4]}')
		eprintln('')
	} else {
		if width >= 14 {
			left_margin := ' '.repeat(int(width / 2 - 7))
			for l in vlogo {
				println(left_margin + l)
			}
		}
		println(help_text.join('\n'))
	}
}

fn run_repl(workdir string, vrepl_prefix string) int {
	if !is_stdin_a_pipe {
		print_welcome_screen()
	}

	if vstartup != '' {
		result := repl_run_vfile(vstartup) or {
			os.Result{
				output: '${vstartup} file not found'
			}
		}
		print('\n')
		print_output(result.output)
	}
	temp_file := os.join_path(workdir, '.${vrepl_prefix}vrepl_temp.v')
	mut prompt := '>>> '
	defer {
		if !is_stdin_a_pipe {
			println('')
		}
		cleanup_files(temp_file)
	}
	mut r := new_repl(workdir)

	for {
		if r.indent == 0 {
			prompt = '>>> '
		} else {
			prompt = '... '
		}
		oline := r.get_one_line(prompt) or { break }
		line := oline.trim_space()
		if line == '' {
			continue
		}
		if line.len <= -1 || line == 'exit' {
			break
		}
		r.line = line
		if r.line == 'clear' {
			term.erase_clear()
			continue
		}
		if r.line == 'help' {
			repl_help()
			continue
		}

		if r.line.len > 4 && r.line[0..3] == '!sh' {
			run_shell(r.line[4..r.line.len])
			continue
		}

		if r.line.contains(':=') && r.line.contains('fn(') {
			r.in_func = true
			r.functions_name << r.line.all_before(':= fn(').trim_space()
		}

		starts_with_fn := starts_with_type_decl(r.line, 'fn')
		if starts_with_fn {
			r.in_func = true
			r.functions_name << r.line.all_after('fn').all_before('(').trim_space()
		}
		was_func := r.in_func

		starts_with_struct := starts_with_type_decl(r.line, 'struct')
		if starts_with_struct {
			r.in_struct = true
		}
		was_struct := r.in_struct

		starts_with_enum := starts_with_type_decl(r.line, 'enum')
		if starts_with_enum {
			r.in_enum = true
		}
		was_enum := r.in_enum

		starts_with_interface := starts_with_type_decl(r.line, 'interface')
		if starts_with_interface {
			r.in_interface = true
		}
		was_interface := r.in_interface

		if r.checks() {
			for rline in r.line.split('\n') {
				r.temp_lines << rline
			}
			if r.indent > 0 {
				continue
			}
			r.line = ''
		}
		if r.line == 'debug_repl' {
			eprintln('repl: ${r}')
			continue
		}
		if r.line == 'reset' {
			r = new_repl(workdir)
			continue
		}
		if r.line == 'list' {
			r.list_source()
			continue
		}
		if r.line == 'pin' {
			r.is_pin = !r.is_pin
			if r.is_pin {
				r.pin()
				println('')
			}
			continue
		}
		if r.line.starts_with('=') {
			r.line = 'println(' + r.line[1..] + ')'
		}
		if r.line.starts_with('print(') || r.line.starts_with('println(') {
			// >>> println('hello')
			source_code := r.current_source_code(false, false) + '\n${r.line}\n'
			os.write_file(temp_file, source_code) or { panic(err) }
			s := repl_run_vfile(temp_file) or { return 1 }
			if s.output.len > r.last_output.len {
				cur_line_output := s.output[r.last_output.len..]
				print_output(cur_line_output)
				if s.exit_code == 0 && !cur_line_output.contains('warning:') {
					r.last_output = s.output.clone()
					r.lines << r.line
				}
			}
		} else if r.line.contains('os.input(') {
			// >>> s := os.input('name: ')
			prompt_str := r.line.all_after('os.input(').all_before(')').trim('\'"')
			line_t := r.get_one_line(prompt_str) or { break }.trim_right('\n')
			trans_line := r.line.all_before('os.input(') + "'${line_t}'"
			source_code := r.current_source_code(false, false) + '\n${trans_line}\n'
			os.write_file(temp_file, source_code) or { panic(err) }
			s := repl_run_vfile(temp_file) or { return 1 }
			if s.exit_code == 0 {
				r.lines << trans_line
			}
		} else {
			func_call, fntype := r.function_call(r.line)
			filter_line := r.line.replace(r.line.find_between("'", "'"), '').replace(r.line.find_between('"',
				'"'), '')
			mut is_statement := false
			if filter_line.count('=') % 2 == 1
				&& (filter_line.count('!=') + filter_line.count('>=') + filter_line.count('<=')) == 0 {
				is_statement = true
			} else {
				for pattern in possible_statement_patterns {
					if filter_line.contains(pattern) {
						is_statement = true
						break
					}
				}
			}
			// Note: starting a line with 2 spaces escapes the println heuristic
			if oline.starts_with('  ') {
				is_statement = true
			}
			// The parentheses do not match
			if r.line.count('(') != r.line.count(')') {
				is_statement = true
			}

			if !is_statement && (!func_call || fntype == FnType.fn_type) && r.line != '' {
				print_line := 'println(${r.line})'
				source_code := r.current_source_code(false, false) + '\n${print_line}\n'
				os.write_file(temp_file, source_code) or { panic(err) }
				s := repl_run_vfile(temp_file) or { return 1 }
				if s.exit_code == 0 {
					if s.output.len > r.last_output.len {
						cur_line_output := s.output[r.last_output.len..]
						print_output(cur_line_output)
						if !cur_line_output.contains('warning:') {
							r.last_output = s.output.clone()
							r.lines << print_line
						}
					}
					continue
				} else {
					if s.output.len > r.last_output.len {
						cur_line_output := s.output[r.last_output.len..]
						if cur_line_output.contains('undefined ident:') {
							print_output(cur_line_output)
							continue
						}
					}
				}
			}

			starts_with_const := starts_with_type_decl(r.line, 'const')
			starts_with_type := starts_with_type_decl(r.line, 'type')
			starts_with_import := r.line.starts_with('import ') || r.line.starts_with('import\t')
			starts_with_include := r.line.starts_with('#include ')
				|| r.line.starts_with('#include\t')
			mut temp_source_code := ''

			if starts_with_import {
				mod := r.line.fields()[1]
				if mod !in r.modules {
					temp_source_code = '${r.line}\n' + r.current_source_code(false, true)
				}
			} else if r.line.len == 0 {
				if was_func {
					temp_source_code = r.insert_source_code(DeclType.fn, r.temp_lines)
				} else if was_struct {
					temp_source_code = r.insert_source_code(DeclType.struct, r.temp_lines)
				} else if was_enum {
					temp_source_code = r.insert_source_code(DeclType.enum, r.temp_lines)
				} else if was_interface {
					temp_source_code = r.insert_source_code(DeclType.interface, r.temp_lines)
				} else {
					temp_source_code = r.insert_source_code(DeclType.stmt, r.temp_lines)
				}
			} else if starts_with_include {
				temp_source_code = r.insert_source_code(DeclType.include, [r.line])
			} else if starts_with_fn {
				temp_source_code = r.insert_source_code(DeclType.fn, [r.line])
			} else if starts_with_const {
				temp_source_code = r.insert_source_code(DeclType.const, [r.line])
			} else if starts_with_enum {
				temp_source_code = r.insert_source_code(DeclType.enum, [r.line])
			} else if starts_with_struct {
				temp_source_code = r.insert_source_code(DeclType.struct, [r.line])
			} else if starts_with_interface {
				temp_source_code = r.insert_source_code(DeclType.interface, [r.line])
			} else if starts_with_type {
				temp_source_code = r.insert_source_code(DeclType.type, [r.line])
			} else {
				temp_source_code = r.current_source_code(true, false) + '\n${r.line}\n'
			}
			os.write_file(temp_file, temp_source_code) or { panic(err) }
			s := repl_run_vfile(temp_file) or { return 1 }
			if s.exit_code == 0 {
				if starts_with_import {
					r.parse_import(r.line)
				} else if r.line.len == 0 {
					if was_func {
						r.functions << r.temp_lines
					} else if was_struct {
						r.structs << r.temp_lines
					} else if was_enum {
						r.enums << r.temp_lines
					} else if was_interface {
						r.interfaces << r.temp_lines
					} else {
						r.lines << r.temp_lines
					}
				} else if starts_with_include {
					r.includes << r.line
				} else if starts_with_fn {
					r.functions << r.line
				} else if starts_with_const {
					r.consts << r.line
				} else if starts_with_enum {
					r.enums << r.line
				} else if starts_with_type {
					r.types << r.line
				} else if starts_with_struct {
					r.structs << r.line
				} else if starts_with_interface {
					r.interfaces << r.line
				} else {
					r.lines << r.line
				}
			}
			r.temp_lines.clear()
			if r.is_pin {
				r.pin()
				println('')
			}
			if s.output.len > r.last_output.len {
				len := r.last_output.len
				if s.exit_code == 0 {
					r.last_output = s.output.clone()
				}
				cur_line_output := s.output[len..]
				print_output(cur_line_output)
			}
		}
	}
	return 0
}

fn convert_output(os_result string) string {
	lines := os_result.trim_right('\n\r').split_into_lines()
	mut content := ''
	for line in lines {
		if line.contains('.vrepl_temp.v:') {
			// Hide the temporary file name
			sline := line.all_after('.vrepl_temp.v:')
			idx := sline.index(' ') or {
				content += endline_if_missed(sline)
				return content
			}
			content += endline_if_missed(sline[idx + 1..])
		} else {
			content += endline_if_missed(line)
		}
	}
	return content
}

fn print_output(os_result string) {
	content := convert_output(os_result)
	print(content)
}

fn main() {
	// Support for the parameters replfolder and replprefix is needed
	// so that the repl can be launched in parallel by several different
	// threads by the REPL test runner.
	args := cmdline.options_after(os.args, ['repl'])
	replfolder := os.real_path(cmdline.option(args, '-replfolder', repl_folder))
	replprefix := cmdline.option(args, '-replprefix', 'noprefix.${rand.ulid()}.')
	if !os.exists(os.getenv('VEXE')) {
		println('Usage:')
		println('  VEXE=vexepath vrepl\n')
		println('  ... where vexepath is the full path to the v executable file')
		return
	}
	if !is_stdin_a_pipe {
		os.setenv('VCOLORS', 'always', true)
	}
	exit(run_repl(replfolder, replprefix))
}

fn rerror(s string) {
	println('V repl error: ${s}')
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

fn cleanup_files(file string) {
	os.rm(file) or {}
	$if windows {
		os.rm(file[..file.len - 2] + '.exe') or {}
		$if msvc {
			os.rm(file[..file.len - 2] + '.ilk') or {}
			os.rm(file[..file.len - 2] + '.pdb') or {}
		}
	} $else {
		os.rm(file[..file.len - 2]) or {}
	}
}

fn repl_run_vfile(file string) !os.Result {
	$if trace_repl_temp_files ? {
		eprintln('>> repl_run_vfile file: ${file}')
	}
	s := os.execute('${os.quoted_path(vexe)} -message-limit 1 -repl run ${os.quoted_path(file)}')
	if s.exit_code < 0 {
		rerror(s.output)
		return error(s.output)
	}
	return s
}
