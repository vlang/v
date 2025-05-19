import os
import v.vmod
import flag
import time
import math
import log

const version = '0.0.2'
const default_command = '${os.quoted_path(@VEXE)} -no-skip-unused' // Command used to compile the program, using -no-skip-unused to ease the reducing
const default_error_msg = 'C compilation error' // the pattern to reproduce
// Temporary files
const tmp_folder = os.join_path(os.vtmp_dir(), 'vreduce')

fn main() {
	log.use_stdout()
	mut fp := flag.new_flag_parser(os.args)
	fp.skip_executable()
	fp.application('v reduce path/to/file_to_reduce.v')
	fp.description('This tool will reduce the code file and try to make the smallest one it can that reproduces the error when the command is executed')
	fp.version(version)

	error_msg := fp.string('error_msg', `e`, default_error_msg, 'the error message you want to reproduce, default: \'${default_error_msg}\'')
	mut command := fp.string('command', `c`, default_command, 'the command used to try to reproduce the error, default: \'${default_command}\', will replace PATH with the path of the folder where it is run')
	copy_project := fp.bool('cp', `p`, false, 'if used v reduce will copy the whole folder of the project')
	timeout := fp.int('to', `t`, 0, 'sets a timeout for the command, default=0 : no timeout')
	do_fmt := fp.bool('fmt', `w`, false, 'enable v fmt for the output (rpdc_file_name.v)')
	file_paths := fp.finalize() or {
		eprintln(err)
		println(fp.usage())
		return
	}

	if file_paths.len != 2 {
		println(fp.usage())
		exit(0)
	}

	mut file_path := file_paths[1]
	if file_path == '' || !os.exists(file_path) {
		log.error('You need to specify a valid file to reduce.')
		if file_path != '' {
			log.error('Path `${file_path}` is not a valid .v file.')
		}
		println(fp.usage())
		exit(1)
	}

	log.info("Starting to reduce the file: '${file_path}'\n    with command: `${command}`,\n    trying to reproduce: `${error_msg}`")

	if do_fmt {
		log.info('Will do `v fmt -w rpdc_file_name.v` after the reduction.')
	} else {
		log.info('Will NOT do `v fmt -w rpdc_file_name.v` (use the `--fmt` or `-w` flag to enable it)')
	}

	content := os.read_file(file_path)!
	show_code_stats(content, label: 'Original code size')

	// copy project	
	if os.exists(tmp_folder) {
		os.rmdir_all(tmp_folder)!
	}
	os.mkdir(tmp_folder)!
	if copy_project {
		mut vmod_cacher := vmod.new_mod_file_cacher()
		project_folder := vmod_cacher.get_by_file(file_path).vmod_folder
		os.cp_all('${project_folder}/.', tmp_folder + '/', true)!
		// the path of the target file from the project folder
		file_path = os.walk_ext(project_folder, os.file_name(file_path))[0] or {
			panic('File not found in the project folder')
		}
		file_path = file_path[project_folder.len + 1..] // will remove the / too
	}
	full_file_path := '${tmp_folder}/${file_path}'
	if command == default_command {
		command = '${default_command} ${full_file_path}'
	} else {
		command = command.replace('PATH', '${tmp_folder}/')
	}

	// start tests
	tmp_code := create_code(parse(content))
	warn_on_false(string_reproduces(tmp_code, error_msg, command, full_file_path, true,
		timeout), 'string_reproduces', @LOCATION)
	show_code_stats(tmp_code, label: 'Code size without comments')

	// reduce the code
	reduce_scope(content, error_msg, command, do_fmt, full_file_path, timeout)

	// cleanse
	if os.exists(tmp_folder) {
		os.rmdir_all(tmp_folder)!
	}
}

// Return true if the command ran on the file produces the pattern
fn string_reproduces(file_content string, pattern string, command string, file_path string, debug bool, timeout int) bool {
	if !os.exists(tmp_folder) {
		os.mkdir(tmp_folder) or { panic(err) }
	}
	os.write_file(file_path, file_content) or { panic(err) }
	mut output := ''
	if timeout == 0 {
		res := os.execute(command)
		output = res.output
	} else {
		split := command.split(' ')
		mut prog := os.new_process(split[0])
		prog.use_pgroup = true
		prog.set_args(split[1..])
		prog.set_redirect_stdio()
		prog.run()
		mut sw := time.new_stopwatch()
		sw.start()
		for prog.is_alive() {
			// check if there is any input from the user (it does not block, if there is not):
			time.sleep(1 * time.millisecond)
			mut b := true
			for b {
				b = false
				if oline := prog.pipe_read(.stdout) {
					if oline != '' {
						output += oline
						b = true
					}
				}
				if eline := prog.pipe_read(.stderr) {
					if eline != '' {
						output += eline
						b = true
					}
				}
				if sw.elapsed().seconds() > f32(timeout) {
					break
				}
			}
			if sw.elapsed().seconds() > f32(timeout) {
				if debug {
					println('Timeout')
				}
				prog.signal_pgkill()
				prog.close()
				prog.wait()
				return false
			}
		}
		prog.signal_pgkill()
		prog.close()
		prog.wait()
	}
	if output.contains(pattern) {
		// println('reproduces')
		return true
	} else {
		// println('does not reproduce')
		if debug {
			println(output)
			println('executed command: ${command}')
		}
		return false
	}
}

type Elem = string | Scope

@[heap]
struct Scope {
mut:
	fn_scope    bool   // contains a function (string: signature{}, children: function body)
	ignored     bool   // is the scope ignored when creating the file
	tmp_ignored bool   // used when testing if it can be ignored in the file
	children    []Elem // code blocks (strings & children scope
}

// Parse a V file and create a scope tree to represent it
fn parse(file_content string) Scope { // The parser is surely incomplete for the V syntax, but should work for most of the cases, if not, please open an issue or submit a PR
	mut stack := []&Scope{} // add the last parent to the stack
	stack << &Scope{}
	mut top := stack[0] // stores stack[stack.len-1] (the element on the top of the stack)
	mut scope_level := 0 // Counts the scope depth of the current position in the file_content
	mut i := 0 // index of the current char in the file_content
	mut current_string := ''
	for i < file_content.len {
		top = stack[stack.len - 1] // the element on the top of the stack
		if file_content[i] == `/` && file_content[i + 1] == `/` {
			for file_content[i] != `\n` { // comment -> skip until newline
				i++
			}
		} else if file_content[i] == `\n` && file_content[i - 1] == `\n` {
			i++ // remove excess newlines
		} else if file_content[i] == `\t` {
			i++ // remove tabs for easier processing
		} else if file_content[i] == `f` && file_content[i + 1] == `n` && file_content[i + 2] == ` ` && file_content[i - 1] or {
			`\n`
		} == `\n` {
			top.children << current_string
			// no increase in scope because not handled with {}
			current_string = ''
			top.children << &Scope{
				fn_scope: true
			}
			stack << &(top.children[top.children.len - 1] as Scope)
			current_string += file_content[i].ascii_str() // f
			i++
			current_string += file_content[i].ascii_str() // n
			i++
		} else if file_content[i] == `/` && file_content[i + 1] == `*` {
			i++
			i++
			i++
			for !(file_content[i - 1] == `*` && file_content[i] == `/`) { // multiline comment -> skip next multiline end sequence
				i++
			}
			i++
		} else if file_content[i] == `\`` && file_content[i - 1] != `\\` {
			current_string += file_content[i].ascii_str()
			i++
			for file_content[i] != `\``
				|| (file_content[i - 1] == `\\` && file_content[i - 2] != `\\`) { // string -> skip until next `
				current_string += file_content[i].ascii_str()
				i++
			}
			current_string += file_content[i].ascii_str() // `
			i++
		} else if file_content[i] == `'` {
			current_string += file_content[i].ascii_str() // '
			i++
			for file_content[i] != `'`
				|| (file_content[i - 1] == `\\` && file_content[i - 2] != `\\`) { // string -> skip until next '
				current_string += file_content[i].ascii_str()
				i++
			}
			current_string += file_content[i].ascii_str() // '
			i++
		} else if file_content[i] == `"` {
			current_string += file_content[i].ascii_str() // "
			i++
			for file_content[i] != `"`
				|| (file_content[i - 1] == `\\` && file_content[i - 2] != `\\`) { // string -> skip until next "
				current_string += file_content[i].ascii_str()
				i++
			}
			current_string += file_content[i].ascii_str() // "
			i++
		} else if file_content[i] == `{` {
			current_string += file_content[i].ascii_str()
			i++
			top.children << current_string
			scope_level += 1
			current_string = ''
			top.children << &Scope{}
			stack << &(top.children[top.children.len - 1] as Scope)
		} else if file_content[i] == `}` {
			scope_level -= 1
			assert scope_level >= 0, 'The scopes are not well detected ${stack[0]}'
			if current_string != '' {
				top.children << current_string
			}
			if stack.last().children == [] {
				stack[stack.len - 2].children.delete(stack[stack.len - 2].children.len - 1) // delete the empty scope (the last children because top of the stack)
			}
			stack.pop()
			top = stack[stack.len - 1]
			current_string = ''
			current_string += file_content[i].ascii_str() // }
			i++
			if scope_level == 0 && stack.len == 2 { // the function and the body scope
				top.children << current_string
				stack.pop()
				top = stack[stack.len - 1]
				current_string = ''
			}
		} else {
			current_string += file_content[i].ascii_str()
			i++
		}
		// nothing here: to avoid complexity, no need to predict what happened before in the ifs, everything will be handled properly by the ifs
	}
	top = stack[stack.len - 1]
	top.children << current_string // last part of the file
	warn_on_false(scope_level == 0, 'scope_level == 0 /* the scopes are not well detected*/',
		@LOCATION)
	warn_on_false(stack.len == 1, 'stack.len == 1 /* the stack should only have the body scope */',
		@LOCATION)
	return *stack[0]
}

// Create the file from a scope tree
fn create_code(sc Scope) string {
	mut output_code := ''
	mut stack := []Elem{}
	stack << sc
	for stack.len > 0 {
		item := stack.pop()
		if item is Scope {
			if !item.ignored && !item.tmp_ignored {
				stack << item.children.reverse() // to traverse the tree in the good order
			} else {
			}
		} else if item is string { // string
			output_code += item
		} else {
			panic('Should never happen')
		}
	}
	return output_code
}

// Reduces the code contained in the scope tree and writes the reduced code to `rpdc_file_name.v`
fn reduce_scope(content string, error_msg string, command string, do_fmt bool, file_path string, timeout int) {
	mut sc := parse('') // will get filled in the start of the loop
	log.info('Cleaning the scopes')
	mut text_code := content
	mut outer_modified_smth := true
	rpdc_file_path := 'rpdc_${os.file_name(file_path)#[..-2]}.v'
	for outer_modified_smth {
		sc = parse(text_code)
		outer_modified_smth = false
		mut modified_smth := true // was a modification successful in reducing the code in the last iteration
		for modified_smth { // as long as there are successful modifications
			modified_smth = false
			log.info('NEXT ITERATION, loop 1')
			mut stack := []&Elem{}
			for i in 0 .. sc.children.len {
				stack << &sc.children[i]
			}
			mut item_nb := 0
			for stack.len > 0 { // traverse the tree and disable (ignore) scopes that are not needed for reproduction
				mut item := stack.pop()
				item_nb += 1
				eprint('\ritem n: ${item_nb}')
				if mut item is Scope {
					if !item.ignored {
						item.tmp_ignored = true // try to ignore it
						code := create_code(sc)
						item.tmp_ignored = false // dont need it anymore
						if string_reproduces(code, error_msg, command, file_path, false,
							timeout)
						{
							item.ignored = true
							modified_smth = true
							outer_modified_smth = true
							println('')
							show_code_stats(code)
						} else { // if can remove it, no need to go through its children
							for i in 0 .. item.children.len {
								stack.insert(0, &item.children[i]) // breadth first search
							}
						}
					}
				}
			}
			println('')

			text_code = create_code(sc)
			os.write_file(rpdc_file_path, text_code) or { panic(err) }
			if do_fmt {
				os.execute('v fmt -w ${rpdc_file_path}')
				final_content := os.read_file(rpdc_file_path) or { panic(err) }
				show_code_stats(final_content, label: 'Code size after formatting')
			}
			println('The WIP reduced code is now in ${rpdc_file_path}')
		}

		log.info('Processing remaining lines')
		split_code := text_code.split_into_lines() // dont forget to add back the \n
		// Create the binary tree of the lines
		depth := int(math.log2(split_code.len)) + 1
		mut c := 0
		mut line_stack := []&Scope{}
		line_stack << &Scope{}
		for c < split_code.len {
			l1 := line_stack.len
			if l1 <= depth { // or equal because of the first node
				if line_stack[l1 - 1].children.len < 2 {
					line_stack[l1 - 1].children << &Scope{}
					l2 := line_stack[l1 - 1].children.len
					line_stack << &(line_stack[l1 - 1].children[l2 - 1] as Scope)
				} else {
					line_stack.pop()
				}
			} else {
				if line_stack[l1 - 1].children.len != 0 { // if there is already a string
					line_stack.pop()
				} else {
					line_stack[l1 - 1].children << split_code[c] + '\n' // the \n were removed by the split
					c++
					line_stack.pop() // already a string
				}
			}
		}

		// Traverse the tree and prune the useless lines / line groups for the reproduction
		mut line_tree := *line_stack[0]
		warn_on_false(string_reproduces(create_code(line_tree), error_msg, command, file_path,
			true, timeout), 'string_reproduces', @LOCATION) // should be the same
		log.info('Pruning the lines/line groups')
		modified_smth = true
		for modified_smth {
			modified_smth = false
			log.info('NEXT ITERATION, loop 2')
			mut stack := []&Elem{}
			for i in 0 .. line_tree.children.len {
				stack << &line_tree.children[i]
			}
			mut item_nb := 0
			for stack.len > 0 { // traverse the binary tree (of the lines)
				mut item := stack.pop()
				item_nb += 1
				eprint('\ritem n: ${item_nb}')
				if mut item is Scope {
					if !item.ignored {
						item.tmp_ignored = true
						code := create_code(line_tree)
						item.tmp_ignored = false // dont need it anymore
						if string_reproduces(code, error_msg, command, file_path, false,
							timeout)
						{
							item.ignored = true
							modified_smth = true
							outer_modified_smth = true
							println('')
							show_code_stats(code)
						} else { // if can remove it, can remove its children
							for i in 0 .. item.children.len {
								stack << &item.children[i]
							}
						}
					}
				}
			}
			println('')
			text_code = create_code(line_tree)
			os.write_file(rpdc_file_path, text_code) or { panic(err) }
			if do_fmt {
				os.execute('v fmt -w ${rpdc_file_path}')
				final_content := os.read_file(rpdc_file_path) or { panic(err) }
				show_code_stats(final_content, label: 'Code size after formatting')
			}
			println('The WIP reduced code is now in ${rpdc_file_path}')
		}
	}

	warn_on_false(string_reproduces(text_code, error_msg, command, file_path, true, timeout),
		'string_reproduces', @LOCATION)
	os.write_file(rpdc_file_path, text_code) or { panic(err) }
	if do_fmt {
		os.execute('v fmt -w ${rpdc_file_path}')
		final_content := os.read_file(rpdc_file_path) or { panic(err) }
		show_code_stats(final_content, label: 'Code size after formatting')
	}
	println('The reduced code is now in ${rpdc_file_path}')
}

@[params]
struct ShowParams {
	label string = 'Code size'
}

fn show_code_stats(code string, params ShowParams) {
	lines := code.split_into_lines()
	log.info('${params.label}: ${code.len} chars, ${lines.len} lines.')
}

fn warn_on_false(res bool, what string, loc string) {
	if !res {
		log.warn('${what} is false, at ${loc}; see output above')
	}
}
