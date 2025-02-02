import os
import flag
import math

const default_command = 'v -no-skip-unused' // Command used to compile the program, using -no-skip-unused to ease the reducing
const default_error_msg = 'C error found' // the pattern to reproduce
// Temporary files
const tmp_folder = '.tmp_v_reduce'
const tmp_reduced_code_file_name = '__v_reduced_code.v'
const path = '${tmp_folder}/${tmp_reduced_code_file_name}'

fn main() {
	mut fp := flag.new_flag_parser(os.args)
	fp.skip_executable()
	fp.application('v reduce')
	fp.description('This tool will reduce the code file and try to make the smallest one it can that reproduces the error when the command is executed')
	fp.version('')

	file_path := fp.string('file_path', `f`, '', 'the path of the file you want to reduce')
	error_msg := fp.string('error_msg', `e`, default_error_msg, 'the error message you want to reproduce, default: \'${default_error_msg}\'')
	command := fp.string('command', `c`, default_command, 'the command used to try to reproduce the error, default: \'${default_command}\'')
	do_fmt := fp.bool('fmt', `w`, false, 'enable v fmt for the output (rpdc.v)')
	_ := fp.finalize() or {
		eprintln(err)
		println(fp.usage())
		return
	}
	if file_path == '' || !os.exists(file_path) {
		eprintln('You need to specify a valid file to reduce (with the `-f` flag) (more information with `-h` or `--help`)')
		exit(1)
	}

	println("Starting to reduce the file '${file_path}' with command '${command}' reproducing '${error_msg}'")

	if do_fmt {
		println('Will do v fmt to the output rpdc.v file')
	} else {
		println('Will not do v fmt to the output rpdc.v file (use the `--fmt` or `-w` flag to enable it)')
	}

	// Opening the file
	file := os.read_file(file_path)!
	assert string_reproduces(file, error_msg, command)
	println('Original code size: ${file.len}')
	mut tree := parse(file)

	// start tests
	tmp_code := create_code(tree)
	assert string_reproduces(tmp_code, error_msg, command)
	println('Code size without comments: ${tmp_code.len}')

	// reduce the code
	reduce_scope(mut tree, error_msg, command, do_fmt)

	// clean up
	os.rmdir_all(tmp_folder) or { panic(err) }
}

// Return true if the command ran on the file produces the pattern
fn string_reproduces(file string, pattern string, command string) bool {
	if !os.exists(tmp_folder) {
		os.mkdir(tmp_folder) or { panic(err) }
	}
	os.write_file(path, file) or { panic(err) }
	res := os.execute(command + ' ' + path)
	if res.output.contains(pattern) {
		// println('reproduces')
		return true
	} else {
		// println('does not reproduce')
		// println(res.output)
		return false
	}
}

type Elem = string | Scope

@[heap]
struct Scope {
mut:
	ignored     bool   // is the scope ignored when creating the file
	tmp_ignored bool   // used when testing if it can be ignored in the file
	children    []Elem // code blocks (strings & children scope
}

// Parse a V file and create a scope tree to represent it
fn parse(file string) Scope { // The parser is surely incomplete for the V syntax, but should work for most of the cases, if not, please open an issue or submit a PR
	mut stack := []&Scope{} // add the last parent to the stack
	stack << &Scope{}
	mut top := stack[0] // stores stack[stack.len-1] (the element on the top of the stack)
	mut scope_level := 0 // Counts the scope depth of the current position in the file
	mut i := 0 // index of the current char in the file
	mut current_string := ''
	for i < file.len {
		top = stack[stack.len - 1] // the element on the top of the stack
		if file[i] == `/` && file[i + 1] == `/` {
			for file[i] != `\n` { // comment -> skip until newline
				i++
			}
		} else if file[i] == `/` && file[i + 1] == `*` {
			i++
			i++
			i++
			for !(file[i - 1] == `*` && file[i] == `/`) { // multiline comment -> skip next multiline end sequence
				i++
			}
			i++
		} else if file[i] == `\`` && file[i - 1] != `\\` {
			current_string += file[i].ascii_str()
			i++
			for file[i] != `\`` || (file[i - 1] == `\\` && file[i - 2] != `\\`) { // string -> skip until next `
				current_string += file[i].ascii_str()
				i++
			}
			current_string += file[i].ascii_str() // `
			i++
		} else if file[i] == `'` {
			current_string += file[i].ascii_str() // '
			i++
			for file[i] != `'` || (file[i - 1] == `\\` && file[i - 2] != `\\`) { // string -> skip until next '
				current_string += file[i].ascii_str()
				i++
			}
			current_string += file[i].ascii_str() // '
			i++
		} else if file[i] == `"` {
			current_string += file[i].ascii_str() // "
			i++
			for file[i] != `"` || (file[i - 1] == `\\` && file[i - 2] != `\\`) { // string -> skip until next "
				current_string += file[i].ascii_str()
				i++
			}
			current_string += file[i].ascii_str() // "
			i++
		} else if file[i] == `{` {
			current_string += file[i].ascii_str()
			i++
			top.children << current_string
			scope_level += 1
			current_string = ''
			top.children << &Scope{}
			stack << &(top.children[top.children.len - 1] as Scope)
		} else if file[i] == `}` {
			scope_level -= 1
			assert scope_level >= 0, 'The scopes are not well detected ${stack[0]}'
			top.children << current_string
			stack.pop()
			top = stack[stack.len - 1]
			current_string = ''
			current_string += file[i].ascii_str() // }
			i++
		} else {
			current_string += file[i].ascii_str()
			i++
		}
		// nothing here: to avoid complexity, no need to predict what happened before in the ifs, everything will be handled properly by the ifs
	}
	top = stack[stack.len - 1]
	top.children << current_string // last part of the file
	assert scope_level == 0, 'The scopes are not well detected'
	assert stack.len == 1, 'The stack should only have the BODY scope'
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

// Reduces the code contained in the scope tree and writes the reduced code to `rpdc.v`
fn reduce_scope(mut sc Scope, error_msg string, command string, do_fmt bool) {
	println('Cleaning the scopes')
	mut modified_smth := true // was a modification successful in reducing the code in the last iteration
	for modified_smth { // as long as there are successful modifications
		modified_smth = false
		println('NEXT ITERATION')
		mut stack := []&Elem{}
		for i in 0 .. sc.children.len {
			stack << &sc.children[i]
		}
		for stack.len > 0 { // traverse the tree and disable (ignore) scopes that are not needed for reproduction
			mut item := stack.pop()
			if mut item is Scope {
				if !item.ignored {
					item.tmp_ignored = true // try to ignore it
					code := create_code(sc)
					item.tmp_ignored = false // dont need it anymore
					if string_reproduces(code, error_msg, command) {
						item.ignored = true
						modified_smth = true
						println('Code size: ${code.len} chars')
					} else { // if can remove it, no need to go though it's children
						for i in 0 .. item.children.len {
							stack << &item.children[i]
						}
					}
				}
			}
		}
	}

	println('Processing remaining lines')
	tmp_code := create_code(sc).split_into_lines() // dont forget to add back the \n
	// Create the binary tree of the lines
	depth := int(math.log2(tmp_code.len)) + 1
	mut c := 0
	mut line_stack := []&Scope{}
	line_stack << &Scope{}
	for c < tmp_code.len {
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
				line_stack[l1 - 1].children << tmp_code[c] + '\n' // the \n were removed by the split
				c++
				line_stack.pop() // already a string
			}
		}
	}

	// Traverse the tree and prune the useless lines / line groups for the reproduction
	mut line_tree := *line_stack[0]
	assert string_reproduces(create_code(line_tree), error_msg, command) // should be the same
	println('Pruning the lines/line groups')
	modified_smth = true
	for modified_smth {
		modified_smth = false
		println('NEXT ITERATION')
		mut stack := []&Elem{}
		for i in 0 .. line_tree.children.len {
			stack << &line_tree.children[i]
		}
		for stack.len > 0 { // traverse the binary tree (of the lines)
			mut item := stack.pop()
			if mut item is Scope {
				if !item.ignored {
					item.tmp_ignored = true
					code := create_code(line_tree)
					item.tmp_ignored = false // dont need it anymore
					if string_reproduces(code, error_msg, command) {
						item.ignored = true
						modified_smth = true
						println('Code size: ${code.len} chars')
					} else { // if can remove it, can remove it's children
						for i in 0 .. item.children.len {
							stack << &item.children[i]
						}
					}
				}
			}
		}
	}

	mre := create_code(line_tree) // final minimal reproductible example
	assert string_reproduces(mre, error_msg, command)
	os.write_file('rpdc.v', mre) or { panic(err) }
	if do_fmt {
		os.execute('v fmt -w rpdc.v')
	}
	println('The reduced code is now in rpdc.v')
}
