// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import (
	os
	os.cmdline
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
	toolexe := os.executable()
	compiler.set_vroot_folder(filepath.dir(filepath.dir(toolexe)))
	args := compiler.env_vflags_and_os_args()
	foptions := FormatOptions{
		is_w: '-w' in args
		is_diff: '-diff' in args
		is_verbose: '-verbose' in args || '--verbose' in args
		is_all: '-all' in args || '--all' in args
	}
	possible_files := cmdline.only_non_options(cmdline.after(args, ['fmt']))
	if foptions.is_verbose {
		eprintln('vfmt toolexe: $toolexe')
		eprintln('vfmt args: ' + os.args.str())
		eprintln('vfmt env_vflags_and_os_args: ' + args.str())
		eprintln('vfmt possible_files: ' + possible_files.str())
	}
	mut files := []string
	for file in possible_files {
		if !os.exists(file) {
			compiler.verror('"$file" does not exist.')
		}
		if !file.ends_with('.v') {
			compiler.verror('v fmt can only be used on .v files.\nOffending file: "$file" .')
		}
		files << file
	}
	if files.len == 0 {
		usage()
		exit(0)
	}
	for file in files {
		format_file(os.realpath(file), foptions)
	}
}

fn format_file(file string, foptions FormatOptions) {
	mut cfile := file
	fcontent := os.read_file(file) or {
		return
	}
	is_test_file := file.ends_with('_test.v')
	is_module_file := fcontent.contains('module ') && !fcontent.contains('module main')
	should_use_hack := is_module_file && !is_test_file
	mod_folder := filepath.basedir(file)
	mut mod_name := 'main'
	if is_module_file {
		mod_name = filepath.filename(mod_folder)
	}
	if should_use_hack {
		// TODO: remove the need for this. NB: HUGE HACK!
		internal_module_test_content := 'module ${mod_name} fn test_vfmt(){ assert true }'
		internal_module_test_file := filepath.join(mod_folder,'__hacky_vfmt_inner_test.v')
		if os.exists(internal_module_test_file) {
			os.rm(internal_module_test_file)
		}
		os.write_file(internal_module_test_file, internal_module_test_content)
		cfile = internal_module_test_file
	}
	mut v := compiler.new_v_compiler_with_args([cfile])
	v.v_fmt_file = file
	if foptions.is_all {
		v.v_fmt_all = true
	}
	if foptions.is_verbose {
		eprintln('vfmt format_file: file: $file')
		eprintln('vfmt format_file: cfile: $cfile')
		eprintln('vfmt format_file: v.dir: $v.dir')
		eprintln('vfmt format_file: is_test_file: $is_test_file')
		eprintln('vfmt format_file: is_module_file: $is_module_file')
		eprintln('vfmt format_file: mod_name: $mod_name')
		eprintln('vfmt format_file: mod_folder: $mod_folder')
		eprintln('vfmt format_file: should_use_hack: $should_use_hack')
		eprintln('-------------------------------------------')
	}
	v.compile()
	formatted_file_path := v.v_fmt_file_result
	if should_use_hack {
		os.rm(cfile)
	}
	// eprintln('File: $file .')
	// eprintln('Formatted file is: $formatted_file_path .')
	if foptions.is_diff {
		diff_cmd := find_working_diff_command() or {
			eprintln('No working "diff" CLI command found.')
			return
		}
		os.system('$diff_cmd "$formatted_file_path" "$file" ')
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

fn find_working_diff_command() ?string {
	for diffcmd in ['colordiff', 'diff', 'colordiff.exe', 'diff.exe'] {
		p := os.exec('$diffcmd --version') or {
			continue
		}
		if p.exit_code == 0 {
			return diffcmd
		}
	}
	return error('no working diff command found')
}
