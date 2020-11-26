// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import os.cmdline
import rand
import v.ast
import v.pref
import v.fmt
import v.util
import v.parser
import v.table
import vhelp

struct FormatOptions {
	is_l       bool
	is_c       bool // NB: This refers to the '-c' fmt flag, NOT the C backend
	is_w       bool
	is_diff    bool
	is_verbose bool
	is_all     bool
	is_worker  bool
	is_debug   bool
	is_noerror bool
	is_verify  bool // exit(1) if the file is not vfmt'ed
}

const (
	formatted_file_token         = '\@\@\@' + 'FORMATTED_FILE: '
	platform_and_file_extensions = [
		['windows', '_windows.v'],
		['linux', '_lin.v', '_linux.v', '_nix.v'],
		['macos', '_mac.v', '_darwin.v'],
		['freebsd', '_bsd.v', '_freebsd.v'],
		['netbsd', '_bsd.v', '_netbsd.v'],
		['openbsd', '_bsd.v', '_openbsd.v'],
		['solaris', '_solaris.v'],
		['haiku', '_haiku.v'],
		['qnx', '_qnx.v'],
	]
)

fn main() {
	// if os.getenv('VFMT_ENABLE') == '' {
	// eprintln('v fmt is disabled for now')
	// exit(1)
	// }
	toolexe := os.executable()
	util.set_vroot_folder(os.dir(os.dir(os.dir(toolexe))))
	args := util.join_env_vflags_and_os_args()
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
		is_verify: '-verify' in args
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
	possible_files := cmdline.only_non_options(cmdline.options_after(args, ['fmt']))
	if foptions.is_verbose {
		eprintln('vfmt toolexe: $toolexe')
		eprintln('vfmt args: ' + os.args.str())
		eprintln('vfmt env_vflags_and_os_args: ' + args.str())
		eprintln('vfmt possible_files: ' + possible_files.str())
	}
	mut files := []string{}
	for file in possible_files {
		if os.is_dir(file) {
			files << os.walk_ext(file, '.v')
			files << os.walk_ext(file, '.vsh')
			continue
		}
		if !file.ends_with('.v') && !file.ends_with('.vv') && !file.ends_with('.vsh') {
			verror('v fmt can only be used on .v files.\nOffending file: "$file"')
			continue
		}
		if !os.exists(file) {
			verror('"$file" does not exist')
			continue
		}
		files << file
	}
	if files.len == 0 {
		vhelp.show_topic('fmt')
		exit(0)
	}
	mut cli_args_no_files := []string{}
	for a in os.args {
		if a !in files {
			cli_args_no_files << a
		}
	}
	mut errors := 0
	for file in files {
		fpath := os.real_path(file)
		mut worker_command_array := cli_args_no_files.clone()
		worker_command_array << ['-worker', util.quote_path(fpath)]
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
			if worker_result.output.contains(formatted_file_token) {
				wresult := worker_result.output.split(formatted_file_token)
				formatted_warn_errs := wresult[0]
				formatted_file_path := wresult[1].trim_right('\n\r')
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
		eprintln('Encountered a total of: $errors errors.')
		if foptions.is_noerror {
			exit(0)
		}
		exit(1)
	}
}

fn (foptions &FormatOptions) format_file(file string) {
	mut prefs := pref.new_preferences()
	prefs.is_fmt = true
	if foptions.is_verbose {
		eprintln('vfmt2 running fmt.fmt over file: $file')
	}
	table := table.new_table()
	// checker := checker.new_checker(table, prefs)
	file_ast := parser.parse_file(file, table, .parse_comments, prefs, &ast.Scope{
		parent: 0
	})
	// checker.check(file_ast)
	formatted_content := fmt.fmt(file_ast, table, foptions.is_debug)
	file_name := os.file_name(file)
	ulid := rand.ulid()
	vfmt_output_path := os.join_path(os.temp_dir(), 'vfmt_${ulid}_$file_name')
	os.write_file(vfmt_output_path, formatted_content)
	if foptions.is_verbose {
		eprintln('fmt.fmt worked and $formatted_content.len bytes were written to $vfmt_output_path .')
	}
	eprintln('$formatted_file_token$vfmt_output_path')
}

fn print_compiler_options(compiler_params &pref.Preferences) {
	eprintln('         os: ' + compiler_params.os.str())
	eprintln('  ccompiler: $compiler_params.ccompiler')
	eprintln('       path: $compiler_params.path ')
	eprintln('   out_name: $compiler_params.out_name ')
	eprintln('      vroot: $compiler_params.vroot ')
	eprintln('lookup_path: $compiler_params.lookup_path ')
	eprintln('   out_name: $compiler_params.out_name ')
	eprintln('     cflags: $compiler_params.cflags ')
	eprintln('    is_test: $compiler_params.is_test ')
	eprintln('  is_script: $compiler_params.is_script ')
}

fn (foptions &FormatOptions) post_process_file(file string, formatted_file_path string) {
	if formatted_file_path.len == 0 {
		return
	}
	if foptions.is_diff {
		diff_cmd := util.find_working_diff_command() or {
			eprintln(err)
			return
		}
		if foptions.is_verbose {
			eprintln('Using diff command: $diff_cmd')
		}
		println(util.color_compare_files(diff_cmd, file, formatted_file_path))
		return
	}
	if foptions.is_verify {
		diff_cmd := util.find_working_diff_command() or {
			eprintln(err)
			return
		}
		x := util.color_compare_files(diff_cmd, file, formatted_file_path)
		if x.len != 0 {
			println("$file is not vfmt'ed")
			exit(1)
		}
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
		} else {
			eprintln('Already formatted file: $file')
		}
		return
	}
	print(formatted_fc)
}

fn (f FormatOptions) str() string {
	return 'FormatOptions{ is_l: $f.is_l, is_w: $f.is_w, is_diff: $f.is_diff, is_verbose: $f.is_verbose,' +
		' is_all: $f.is_all, is_worker: $f.is_worker, is_debug: $f.is_debug, is_noerror: $f.is_noerror,' +
		' is_verify: $f.is_verify" }'
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

fn file_to_mod_name_and_is_module_file(file string) (string, bool) {
	mut mod_name := 'main'
	mut is_module_file := false
	flines := read_source_lines(file) or {
		return mod_name, is_module_file
	}
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
	return mod_name, is_module_file
}

fn read_source_lines(file string) ?[]string {
	source_lines := os.read_lines(file) or {
		return error('can not read $file')
	}
	return source_lines
}

fn get_compile_name_of_potential_v_project(file string) string {
	// This function get_compile_name_of_potential_v_project returns:
	// a) the file's folder, if file is part of a v project
	// b) the file itself, if the file is a standalone v program
	pfolder := os.real_path(os.dir(file))
	// a .v project has many 'module main' files in one folder
	// if there is only one .v file, then it must be a standalone
	all_files_in_pfolder := os.ls(pfolder) or {
		panic(err)
	}
	mut vfiles := []string{}
	for f in all_files_in_pfolder {
		vf := os.join_path(pfolder, f)
		if f.starts_with('.') || !f.ends_with('.v') || os.is_dir(vf) {
			continue
		}
		vfiles << vf
	}
	if vfiles.len == 1 {
		return file
	}
	// /////////////////////////////////////////////////////////////
	// At this point, we know there are many .v files in the folder
	// We will have to read them all, and if there are more than one
	// containing `fn main` then the folder contains multiple standalone
	// v programs. If only one contains `fn main` then the folder is
	// a project folder, that should be compiled with `v pfolder`.
	mut main_fns := 0
	for f in vfiles {
		slines := read_source_lines(f) or {
			panic(err)
		}
		for line in slines {
			if line.contains('fn main()') {
				main_fns++
				if main_fns > 1 {
					return file
				}
			}
		}
	}
	return pfolder
}

fn verror(s string) {
	util.verror('vfmt error', s)
}
