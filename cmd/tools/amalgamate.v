// amalgamate multiple C source files into a single
// C source file.  See https://sqlite.org/amalgamation.html
// for a description of file amalgamation.
//
// If an input file is not specified, source is read
// from stdin.
//
// If an output file is not specified, source is output
// to stdout.

module main

import flag
import os
import regex

const app_name = 'amalgamate'
const app_version = '0.0.1'

// pre-compile the include statement regex
const re = regex.regex_opt(r'^\s*#\s*include\s*"([^"]+)"')!

struct Config {
mut:
	input_files []string
	output_file string
	search_dirs []string
	blacklist   []string
}

struct Context {
	config Config
mut:
	processed_files []string
}

fn parse_arguments() Config {
	mut cfg := Config{}

	mut parser := flag.new_flag_parser(os.args)
	parser.skip_executable()
	parser.application(app_name)
	parser.version(app_version)

	parser.arguments_description('[file ...]')

	parser.description('combine multiple .c and .h files into one.')
	parser.description('')
	parser.description('Combine input, coming from either stdin or input files, into one')
	parser.description('large file.  Include statements are processed and the contents')
	parser.description('copied in place.  Only #include "file.h" statements cause their')
	parser.description('contents to be copied, not #include <file.h> statements.  If no')
	parser.description('input files are specified, read from stdin.')

	parser.footer('\nAn example showing multiple blacklisted files and multiple search')
	parser.footer('directories.')
	parser.footer('')
	parser.footer('    amalgamate -o output_file.c -b ignore_me.h \\')
	parser.footer('        -b ignore_me_2.h -b other/ignore_me.h \\')
	parser.footer('        -s relative/search/dir -s /absolute/search/dir \\')
	parser.footer('        file1.c file2.c')
	parser.footer('')

	cfg.output_file = parser.string('output', `o`, '', 'output file.  If not specified,\n' +
		flag.space + 'defaults to stdout.\n', val_desc: '<filename>')

	cfg.blacklist = parser.string_multi('blacklist', `b`,
		'blacklist a file name.  This prevents\n' + flag.space +
		'the named file from being included.\n' + flag.space +
		'This can be specified more that once.\n', val_desc: '<include_file>')

	cfg.search_dirs = parser.string_multi('search_path', `s`,
		'add a directory to the search path.\n' + flag.space +
		'An include file is searched for in\n' + flag.space +
		'the current working directory and\n' + flag.space +
		'if not found, the directories in this\n' + flag.space +
		'list are searched, in order, until the\n' + flag.space +
		'file is found or the search list is\n' + flag.space +
		'exhausted.  This can be specified\n' + flag.space + 'more that once.\n',
		val_desc: '<search_dir>'
	)

	cfg.input_files = parser.finalize() or {
		// this only reports the first unrecognized argument
		eprintln('${err}\n')
		eprintln('${parser.usage()}\n')
		exit(1)
	}

	return cfg
}

fn main() {
	cfg := parse_arguments()

	mut ctx := Context{
		config: cfg
	}

	ctx.amalgamate() or {
		eprintln('error: ${err}')
		exit(1)
	}
}

fn (mut c Context) amalgamate() ! {
	mut source := ''

	if c.config.input_files.len == 0 {
		// source += '/* ########## stdin */\n'
		// if there are no input files, read from stdin
		local_dir := os.getwd()
		source += c.handle_includes(local_dir, os.get_raw_lines_joined())!
		// source += '/* ########## stdin end */\n'
	} else {
		// read each input file, in order, and
		// handle all of its includes.
		for file in c.config.input_files {
			if file in c.config.blacklist {
				// skip blacklisted files
				continue
			}

			found_file := c.find_file(file)!

			if found_file in c.processed_files {
				// skip over files already read
				continue
			}

			// source += '/* ########## ${file} */\n'
			c.processed_files << found_file
			local_dir := os.dir(found_file)
			file_source_code := os.read_file(found_file)!
			source += c.handle_includes(local_dir, file_source_code)!
			// source += '/* ########## ${file} end */\n'
		}
	}

	if c.config.output_file == '' {
		print(source)
	} else {
		os.write_file(c.config.output_file, source)!
	}

	return
}

fn (c Context) find_file(file string) !string {
	mut full_path := os.real_path(file)

	if os.is_file(full_path) {
		return full_path
	}

	for dir in c.config.search_dirs {
		full_path = os.real_path(os.join_path_single(dir, file))

		if os.is_file(full_path) {
			return full_path
		}
	}

	return error('file "${file}" not found')
}

// handle_includes looks for lines that start with #include
// and inserts the lines from the named include file.
//
// The pattern matches file names for local header files,
// not system header files as are denoted by < and >.
fn (mut c Context) handle_includes(local_dir string, input_source string) !string {
	source_lines := input_source.split_into_lines()
	mut output_lines := []string{}

	for line in source_lines {
		start, _ := re.match_string(line)

		if start >= 0 {
			file := line[re.groups[0]..re.groups[1]]
			mut found_file := ''

			if file in c.config.blacklist {
				// leave blacklisted files alone
				if file in c.processed_files {
					// we don't want a second include
					output_lines << '\n'
				} else {
					output_lines << line
					c.processed_files << file
				}
				continue
			}

			if !os.is_abs_path(file) {
				found_file = c.find_file(os.join_path_single(local_dir, file)) or {
					// keep looking
					''
				}
			}

			if found_file == '' {
				found_file = c.find_file(file)!
			}

			if found_file in c.processed_files {
				// skip over files already read
				continue
			}
			c.processed_files << found_file
			file_source_code := os.read_file(found_file)!
			// output_lines << '/* ########## ${file} begin */\n'
			output_lines << c.handle_includes(os.dir(found_file), file_source_code)!
			// output_lines << '/* ########## ${file} end */\n'
		} else {
			output_lines << line
		}
	}

	return output_lines.join_lines() + '\n'
}
