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
	is_l       bool
	is_c       bool
	is_w       bool
	is_diff    bool
	is_verbose bool
	is_all     bool
	is_worker  bool
	is_debug   bool
	is_noerror bool
}

const (
	platform_and_file_extensions = [['windows', '_win.v', '_windows.v'],
	['linux', '_lin.v', '_linux.v', '_nix.v'],
	['macos', '_mac.v', '_darwin.v'],
	['freebsd', '_bsd.v', '_freebsd.v'],
	['solaris', '_solaris.v'],
	['haiku', '_haiku.v'],
	]
	FORMATTED_FILE_TOKEN = '\@\@\@' + 'FORMATTED_FILE: '
)

fn main() {
	toolexe := os.executable()
	compiler.set_vroot_folder(filepath.dir(filepath.dir(toolexe)))
	args := compiler.env_vflags_and_os_args()
	foptions := FormatOptions{
		is_c: '-c' in args
		is_l: '-l' in args
		is_w: '-w' in args
		is_diff: '-diff' in args
		is_verbose: '-verbose' in args || '--verbose' in args
		is_all: '-all' in args || '--all' in args
		is_worker: '-worker' in args
		is_debug: '-debug' in args
		is_noerror: '-noerror' in args
	}
	if foptions.is_verbose {
		eprintln('vfmt foptions: $foptions')
	}
	if foptions.is_worker {
		// -worker should be added by a parent vfmt process.
		// We launch a sub process for each file because
		// the v compiler can do an early exit if it detects
		// a syntax error, but we want to process ALL passed
		// files if possible.
		foptions.format_file(cmdline.option(args, '-worker', ''))
		exit(0)
	}
	// we are NOT a worker at this stage, i.e. we are a parent vfmt process
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
	mut cli_args_no_files := []string
	for a in os.args {
		if !a in files {
			cli_args_no_files << a
		}
	}
	mut errors := 0
	for file in files {
		fpath := os.realpath(file)
		mut worker_command_array := cli_args_no_files.clone()
		worker_command_array << ['-worker', fpath]
		worker_cmd := worker_command_array.join(' ')
		if foptions.is_verbose {
			eprintln('vfmt worker_cmd: $worker_cmd')
		}
		worker_result := os.exec(worker_cmd) or {
			errors++
			continue
		}
		if worker_result.exit_code != 0 {
			eprintln(worker_result.output)
			if worker_result.exit_code == 1 {
				eprintln('vfmt error while formatting file: $file .')
			}
			errors++
			continue
		}
		if worker_result.output.len > 0 {
			if worker_result.output.contains(FORMATTED_FILE_TOKEN) {
				wresult := worker_result.output.split(FORMATTED_FILE_TOKEN)
				formatted_warn_errs := wresult[0]
				formatted_file_path := wresult[1]
				foptions.post_process_file(fpath, formatted_file_path)
				if formatted_warn_errs.len > 0 {
					eprintln(formatted_warn_errs)
				}
				continue
			}
		}
		errors++
	}
	if errors > 0 {
		eprintln('Encountered a total of: ${errors} errors.')
		if foptions.is_noerror {
			exit(0)
		}
		exit(1)
	}
}

fn (foptions &FormatOptions) format_file(file string) {
	tmpfolder := os.tmpdir()
	mut compiler_params := []string
	target_os := file_to_target_os(file)
	if target_os != '' {
		compiler_params << ['-os', target_os]
	}
	mut cfile := file
	mut mod_folder_parent := tmpfolder
	is_test_file := file.ends_with('_test.v')
	mod_name,is_module_file := file_to_mod_name_and_is_module_file(file)
	use_tmp_main_program := is_module_file && !is_test_file
	mod_folder := filepath.basedir(file)
	if use_tmp_main_program {
		// TODO: remove the need for this
		// This makes a small program that imports the module,
		// so that the module files will get processed by the
		// vfmt implementation.
		mod_folder_parent = filepath.basedir(mod_folder)
		mut main_program_content := if mod_name == 'builtin' || mod_name == 'main' { 'fn main(){}\n' } else { 'import ${mod_name}\n' + 'fn main(){}\n' }
		main_program_file := filepath.join(tmpfolder,'vfmt_tmp_${mod_name}_program.v')
		if os.exists(main_program_file) {
			os.rm(main_program_file)
		}
		os.write_file(main_program_file, main_program_content)
		cfile = main_program_file
		compiler_params << ['-user_mod_path', mod_folder_parent]
	}
	compiler_params << cfile
	if foptions.is_verbose {
		eprintln('vfmt format_file: file: $file')
		eprintln('vfmt format_file: cfile: $cfile')
		eprintln('vfmt format_file: is_test_file: $is_test_file')
		eprintln('vfmt format_file: is_module_file: $is_module_file')
		eprintln('vfmt format_file: mod_name: $mod_name')
		eprintln('vfmt format_file: mod_folder: $mod_folder')
		eprintln('vfmt format_file: mod_folder_parent: $mod_folder_parent')
		eprintln('vfmt format_file: use_tmp_main_program: $use_tmp_main_program')
		eprintln('vfmt format_file: compiler_params: $compiler_params')
		eprintln('-------------------------------------------')
	}
	formatted_file_path := foptions.compile_file(file, compiler_params)
	if use_tmp_main_program {
		if !foptions.is_debug {
			os.rm(cfile)
		}
	}
	eprintln('${FORMATTED_FILE_TOKEN}${formatted_file_path}')
}

fn (foptions &FormatOptions) post_process_file(file string, formatted_file_path string) {
	if formatted_file_path.len == 0 {
		return
	}
	if foptions.is_diff {
		diff_cmd := find_working_diff_command() or {
			eprintln('No working "diff" CLI command found.')
			return
		}
		os.system('$diff_cmd --minimal  --text   --unified=2 --show-function-line="fn " "$file" "$formatted_file_path" ')
		return
	}
	fc := os.read_file(file) or {
		eprintln('File $file could not be read')
		return
	}
	formatted_fc := os.read_file(formatted_file_path) or {
		eprintln('File $formatted_file_path could not be read')
		return
	}
	is_formatted_different := fc != formatted_fc
	if foptions.is_c {
		if is_formatted_different {
			eprintln('File is not formatted: $file')
			exit(2)
		}
		return
	}
	if foptions.is_l {
		if is_formatted_different {
			eprintln('File needs formatting: $file')
		}
		return
	}
	if foptions.is_w {
		if is_formatted_different {
			os.mv_by_cp(formatted_file_path, file) or {
				panic(err)
			}
			eprintln('Reformatted file: $file')
		}
		else {
			eprintln('Already formatted file: $file')
		}
		return
	}
	print(formatted_fc)
}

fn usage() {
	print('Usage: tools/vfmt [flags] fmt path_to_source.v [path_to_other_source.v]
Formats the given V source files, and prints their formatted source to stdout.
Options:
  -c    check if file is already formatted.
        If it is not, print filepath, and exit with code 2.
  -diff display only diffs between the formatted source and the original source.
  -l    list files whose formatting differs from vfmt.
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

fn (foptions &FormatOptions) compile_file(file string, compiler_params []string) string {
	if foptions.is_verbose {
		eprintln('> new_v_compiler_with_args            file: ' + file)
		eprintln('> new_v_compiler_with_args compiler_params: ' + compiler_params.join(' '))
	}
	mut v := compiler.new_v_compiler_with_args(compiler_params)
	v.v_fmt_file = file
	if foptions.is_all {
		v.v_fmt_all = true
	}
	v.compile()
	return v.v_fmt_file_result
}

pub fn (f FormatOptions) str() string {
	return 'FormatOptions{ ' + ' is_l: $f.is_l' + ' is_w: $f.is_w' + ' is_diff: $f.is_diff' + ' is_verbose: $f.is_verbose' + ' is_all: $f.is_all' + ' is_worker: $f.is_worker' + ' is_debug: $f.is_debug' + ' }'
}

fn file_to_target_os(file string) string {
	for extensions in platform_and_file_extensions {
		for ext in extensions {
			if file.ends_with(ext) {
				return extensions[0]
			}
		}
	}
	return ''
}

fn file_to_mod_name_and_is_module_file(file string) (string,bool) {
	mut mod_name := 'main'
	mut is_module_file := false
	raw_fcontent := os.read_file(file) or {
		return mod_name,is_module_file
	}
	fcontent := raw_fcontent.replace('\r\n', '\n')
	flines := fcontent.split('\n')
	for fline in flines {
		line := fline.trim_space()
		if line.starts_with('module ') {
			if !line.starts_with('module main') {
				is_module_file = true
				mod_name = line.replace('module ', ' ').trim_space()
			}
			break
		}
	}
	return mod_name,is_module_file
}
