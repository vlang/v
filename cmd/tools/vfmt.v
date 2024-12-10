// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import os.cmdline
import rand
import term
import v.ast
import v.pref
import v.fmt
import v.util
import v.util.diff
import v.parser
import v.help

struct FormatOptions {
	is_l       bool
	is_c       bool // Note: This refers to the '-c' fmt flag, NOT the C backend
	is_w       bool
	is_diff    bool
	is_verbose bool
	is_debug   bool
	is_noerror bool
	is_verify  bool // exit(1) if the file is not vfmt'ed
	is_worker  bool // true *only* in the worker processes. Note: workers can crash.
	is_backup  bool // make a `file.v.bak` copy *before* overwriting a `file.v` in place with `-w`
	in_process bool // do not fork a worker process; potentially faster, but more prone to crashes for invalid files
mut:
	diff_cmd string // filled in when -diff or -verify is passed
}

const formatted_file_token = '\@\@\@' + 'FORMATTED_FILE: '
const vtmp_folder = os.vtmp_dir()
const term_colors = term.can_show_color_on_stderr()

fn main() {
	// if os.getenv('VFMT_ENABLE') == '' {
	// eprintln('v fmt is disabled for now')
	// exit(1)
	// }
	toolexe := os.executable()
	util.set_vroot_folder(os.dir(os.dir(os.dir(toolexe))))
	args := util.join_env_vflags_and_os_args()
	mut foptions := FormatOptions{
		is_c:       '-c' in args
		is_l:       '-l' in args
		is_w:       '-w' in args
		is_diff:    '-diff' in args
		is_verbose: '-verbose' in args || '--verbose' in args
		is_worker:  '-worker' in args
		is_debug:   '-debug' in args
		is_noerror: '-noerror' in args
		is_verify:  '-verify' in args
		is_backup:  '-backup' in args
		in_process: '-inprocess' in args
	}
	if term_colors {
		os.setenv('VCOLORS', 'always', true)
	}
	foptions.vlog('vfmt foptions: ${foptions}')
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
		eprintln('vfmt toolexe: ${toolexe}')
		eprintln('vfmt args: ' + os.args.str())
		eprintln('vfmt env_vflags_and_os_args: ' + args.str())
		eprintln('vfmt possible_files: ' + possible_files.str())
	}
	files := util.find_all_v_files(possible_files) or {
		verror(err.msg())
		return
	}
	if os.is_atty(0) == 0 && files.len == 0 {
		foptions.format_pipe()
		exit(0)
	}
	if files.len == 0 || '-help' in args || '--help' in args {
		help.print_and_exit('fmt')
	}
	mut cli_args_no_files := []string{}
	for idx, a in os.args {
		if idx == 0 {
			cli_args_no_files << os.quoted_path(a)
			continue
		}
		if a !in files {
			cli_args_no_files << a
		}
	}
	mut errors := 0
	mut has_internal_error := false
	mut prefs := setup_preferences()
	for file in files {
		fpath := os.real_path(file)
		if foptions.is_verify && foptions.in_process {
			// For a small amount of files, it is faster to process
			// everything directly in the same process, single threaded,
			// when vfmt is compiled with `-gc none`:
			if !foptions.verify_file(prefs, fpath) {
				println("${file} is not vfmt'ed")
				errors++
			}
			continue
		}
		mut worker_command_array := cli_args_no_files.clone()
		worker_command_array << ['-worker', util.quote_path(fpath)]
		worker_cmd := worker_command_array.join(' ')
		foptions.vlog('vfmt worker_cmd: ${worker_cmd}')
		worker_result := os.execute(worker_cmd)
		// Guard against a possibly crashing worker process.
		if worker_result.exit_code != 0 {
			eprintln(worker_result.output)
			if worker_result.exit_code == 1 {
				eprintln('Internal vfmt error while formatting file: ${file}.')
				has_internal_error = true
				continue
			}
			errors++
			continue
		}
		if worker_result.output.len > 0 {
			if worker_result.output.contains(formatted_file_token) {
				wresult := worker_result.output.split(formatted_file_token)
				formatted_warn_errs := wresult[0]
				formatted_file_path := wresult[1].trim_right('\n\r')
				foptions.post_process_file(fpath, formatted_file_path) or { errors = errors + 1 }
				if formatted_warn_errs.len > 0 {
					eprintln(formatted_warn_errs)
				}
				continue
			}
		}
		errors++
	}
	ecode := if has_internal_error { 5 } else { 0 }
	if errors > 0 {
		if !foptions.is_diff {
			eprintln('Encountered a total of: ${errors} formatting errors.')
		}
		match true {
			foptions.is_noerror { exit(0 + ecode) }
			foptions.is_verify { exit(1 + ecode) }
			foptions.is_c { exit(2 + ecode) }
			else { exit(1 + ecode) }
		}
	}
	exit(ecode)
}

fn (foptions &FormatOptions) verify_file(prefs &pref.Preferences, fpath string) bool {
	fcontent := foptions.formated_content_from_file(prefs, fpath)
	content := os.read_file(fpath) or { return false }
	return fcontent == content
}

fn setup_preferences() &pref.Preferences {
	mut prefs := pref.new_preferences()
	prefs.is_fmt = true
	prefs.skip_warnings = true
	return prefs
}

fn setup_preferences_and_table() (&pref.Preferences, &ast.Table) {
	return setup_preferences(), ast.new_table()
}

fn (foptions &FormatOptions) vlog(msg string) {
	if foptions.is_verbose {
		eprintln(msg)
	}
}

fn (foptions &FormatOptions) formated_content_from_file(prefs &pref.Preferences, file string) string {
	mut table := ast.new_table()
	file_ast := parser.parse_file(file, mut table, .parse_comments, prefs)
	formated_content := fmt.fmt(file_ast, mut table, prefs, foptions.is_debug)
	return formated_content
}

fn (foptions &FormatOptions) format_file(file string) {
	file_name := os.file_name(file)
	ulid := rand.ulid()
	vfmt_output_path := os.join_path(vtmp_folder, 'vfmt_${ulid}_${file_name}')
	if file.contains('_vfmt_off') {
		os.cp(file, vfmt_output_path) or { panic(err) }
		foptions.vlog('format_file copied the file ${file} as it was, 1:1, since its name contains `_vfmt_off`.')
		eprintln('${formatted_file_token}${vfmt_output_path}')
		return
	}
	foptions.vlog('vfmt2 running fmt.fmt over file: ${file}')
	prefs, mut table := setup_preferences_and_table()
	file_ast := parser.parse_file(file, mut table, .parse_comments, prefs)
	// checker.new_checker(table, prefs).check(file_ast)
	formatted_content := fmt.fmt(file_ast, mut table, prefs, foptions.is_debug)
	os.write_file(vfmt_output_path, formatted_content) or { panic(err) }
	foptions.vlog('fmt.fmt worked and ${formatted_content.len} bytes were written to ${vfmt_output_path} .')
	eprintln('${formatted_file_token}${vfmt_output_path}')
}

fn (foptions &FormatOptions) format_pipe() {
	foptions.vlog('vfmt2 running fmt.fmt over stdin')
	prefs, mut table := setup_preferences_and_table()
	input_text := os.get_raw_lines_joined()
	file_ast := parser.parse_text(input_text, '', mut table, .parse_comments, prefs)
	// checker.new_checker(table, prefs).check(file_ast)
	formatted_content := fmt.fmt(file_ast, mut table, prefs, foptions.is_debug,
		source_text: input_text
	)
	print(formatted_content)
	flush_stdout()
	foptions.vlog('fmt.fmt worked and ${formatted_content.len} bytes were written to stdout.')
}

fn print_compiler_options(compiler_params &pref.Preferences) {
	eprintln('         os: ' + compiler_params.os.str())
	eprintln('  ccompiler: ${compiler_params.ccompiler}')
	eprintln('       path: ${compiler_params.path} ')
	eprintln('   out_name: ${compiler_params.out_name} ')
	eprintln('      vroot: ${compiler_params.vroot} ')
	eprintln('lookup_path: ${compiler_params.lookup_path} ')
	eprintln('   out_name: ${compiler_params.out_name} ')
	eprintln('     cflags: ${compiler_params.cflags} ')
	eprintln('    is_test: ${compiler_params.is_test} ')
	eprintln('  is_script: ${compiler_params.is_script} ')
}

fn (mut foptions FormatOptions) post_process_file(file string, formatted_file_path string) ! {
	if formatted_file_path == '' {
		return
	}
	fc := os.read_file(file) or {
		eprintln('File ${file} could not be read')
		return
	}
	formatted_fc := os.read_file(formatted_file_path) or {
		eprintln('File ${formatted_file_path} could not be read')
		return
	}
	is_formatted_different := fc != formatted_fc
	if foptions.is_diff {
		if !is_formatted_different {
			return
		}
		println(diff.compare_files(file, formatted_file_path)!)
		return error('')
	}
	if foptions.is_verify {
		if !is_formatted_different {
			return
		}
		println("${file} is not vfmt'ed")
		return error('')
	}
	if foptions.is_c {
		if is_formatted_different {
			eprintln('File is not formatted: ${file}')
			return error('')
		}
		return
	}
	if foptions.is_l {
		if is_formatted_different {
			eprintln('File needs formatting: ${file}')
		}
		return
	}
	if foptions.is_w {
		if is_formatted_different {
			if foptions.is_backup {
				file_bak := '${file}.bak'
				os.cp(file, file_bak) or {}
			}
			mut perms_to_restore := u32(0)
			$if !windows {
				fm := os.inode(file)
				perms_to_restore = fm.bitmask()
			}
			os.mv_by_cp(formatted_file_path, file) or { panic(err) }
			$if !windows {
				os.chmod(file, int(perms_to_restore)) or { panic(err) }
			}
			eprintln('Reformatted file: ${file}')
		} else {
			eprintln('Already formatted file: ${file}')
		}
		return
	}
	print(formatted_fc)
	flush_stdout()
}

fn read_source_lines(file string) ![]string {
	source_lines := os.read_lines(file) or { return error('can not read ${file}') }
	return source_lines
}

@[noreturn]
fn verror(s string) {
	util.verror('vfmt error', s)
}

fn (f FormatOptions) str() string {
	return
		'FormatOptions{ is_l: ${f.is_l}, is_w: ${f.is_w}, is_diff: ${f.is_diff}, is_verbose: ${f.is_verbose},' +
		' is_worker: ${f.is_worker}, is_debug: ${f.is_debug}, is_noerror: ${f.is_noerror},' +
		' is_verify: ${f.is_verify}" }'
}
