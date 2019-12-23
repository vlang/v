// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import (
	os
	filepath
	compiler
)

struct FormatOptions {
	is_w       bool
	is_diff    bool
	is_verbose bool
	is_all     bool
}

fn main() {
	foptions := FormatOptions{
		is_w: '-w' in os.args
		is_diff: '-diff' in os.args
		is_verbose: '-verbose' in os.args || '--verbose' in os.args
		is_all: '-all' in os.args || '--all' in os.args
	}
	toolexe := os.executable()
	compiler.set_vroot_folder(filepath.dir(filepath.dir(toolexe)))
	args := compiler.env_vflags_and_os_args()
	if foptions.is_verbose {
		eprintln('vfmt toolexe: $toolexe')
		eprintln('vfmt args: ' + os.args.str())
		eprintln('vfmt env_vflags_and_os_args: ' + args.str())
	}
	mut files := []string
	for i := 1; i < args.len; i++ {
		a := args[i]
		if a == 'fmt' {
			continue
		}
		if !a.starts_with('-') {
			file := a
			if !os.exists(file) {
				compiler.verror('"$file" does not exist.')
			}
			if !file.ends_with('.v') {
				compiler.verror('v fmt can only be used on .v files.\nOffending file: "$file" .')
			}
			files << a
		}
	}
	if files.len == 0 {
		usage()
		exit(0)
	}
	if foptions.is_all {
		os.setenv('VFMT_OPTION_ALL', 'yes', true)
	}
	for file in files {
		format_file(file, foptions)
	}
}

fn format_file(file string, foptions FormatOptions) {
	mut v := compiler.new_v_compiler_with_args([file])
	if foptions.is_verbose {
		eprintln('vfmt format_file: $file | v.dir: $v.dir')
	}
	v.compile()
	formatted_file_path := os.getenv('VFMT_FILE_RESULT')
	// eprintln('File: $file .')
	// eprintln('Formatted file is: $formatted_file_path .')
	if foptions.is_diff {
		if find_diff:=os.exec('diff -v'){
			os.system('diff "$formatted_file_path" "$file" ')
			return
		}
		eprintln('No working "diff" CLI command found.')
		return
	}
	if foptions.is_w {
		os.mv_by_cp(formatted_file_path, file) or {
			panic(err)
		}
		eprintln('Reformatted file in place: $file .')
	}
	else {
		content := os.read_file(formatted_file_path) or {
			panic(err)
		}
		print(content)
	}
}

fn usage() {
	print('Usage: tools/vfmt [flags] path_to_source.v [path_to_other_source.v]
Formats the given V source files, and prints their formatted source to stdout.
Options:
  -diff display only diffs between the formatted source and the original source.
  -w    write result to (source) file(s) instead of to stdout.
')
}
