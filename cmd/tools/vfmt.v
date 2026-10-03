// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import os.cmdline
import rand
import term
import v.util
import v.vmod
import v.util.diff
import v.util.vflags
import v.errors as compiler_errors
import v.flat
import v.gen.v as compiler_fmt
import v.parser as compiler_parser
import v.pref as compiler_pref

struct FormatOptions {
	is_l                bool
	is_c                bool // Note: This refers to the '-c' fmt flag, NOT the C backend
	is_w                bool
	is_diff             bool
	is_verbose          bool
	is_debug            bool
	is_noerror          bool
	is_verify           bool     // exit(1) if the file is not vfmt'ed
	is_worker           bool     // true *only* in the worker processes. Note: workers can crash.
	is_backup           bool     // make a `file.v.bak` copy *before* overwriting a `file.v` in place with `-w`
	in_process          bool     // do not fork a worker process; potentially faster, but more prone to crashes for invalid files
	is_new_int          bool     // rewrite int to i32 in translated modules and C declarations
	no_migrate_json2    bool     // opt out of the default rewrite of removed `json` usage to `json2` (`-no-migrate-json2`)
	module_search_paths []string // the expanded `-path` roots, where the compiler also looks for imported modules
	backend             string = 'c'
mut:
	diff_cmd string // filled in when -diff or -verify is passed
}

const formatted_file_token = '\@\@\@' + 'FORMATTED_FILE: '
const vtmp_folder = os.vtmp_dir()
const term_colors = term.can_show_color_on_stderr()

fn formatter_backend(args []string) !string {
	mut backend := 'c'
	for i, arg in args {
		requested := if arg in ['-b', '-backend'] && i + 1 < args.len {
			args[i + 1]
		} else if arg.starts_with('-b=') || arg.starts_with('-backend=') {
			arg.all_after('=')
		} else {
			continue
		}
		backend = match requested {
			'c', 'fastc', 'wasm' {
				requested
			}
			'js', 'js_node', 'js_browser', 'js_freestanding' {
				'js'
			}
			'native', 'go', 'arm64', 'eval' {
				'c'
			}
			else {
				return error('Unknown V backend: ${requested}\nValid -backend choices are: c, fastc, go, js, js_node, js_browser, js_freestanding, native, arm64, eval, wasm')
			}
		}
	}
	return backend
}

fn main() {
	// if os.getenv('VFMT_ENABLE') == '' {
	// eprintln('v fmt is disabled for now')
	// exit(1)
	// }
	toolexe := os.executable()
	util.set_vroot_folder(os.dir(os.dir(os.dir(toolexe))))
	args := vflags.join_env_vflags_and_os_args()
	backend := formatter_backend(args) or {
		eprintln(err.msg())
		exit(1)
	}
	mut foptions := FormatOptions{
		is_c:                '-c' in args
		is_l:                '-l' in args
		is_w:                '-w' in args
		is_diff:             '-diff' in args
		is_verbose:          '-verbose' in args || '--verbose' in args
		is_worker:           '-worker' in args
		is_debug:            '-debug' in args
		is_noerror:          '-noerror' in args
		is_verify:           '-verify' in args
		is_backup:           '-backup' in args
		in_process:          '-inprocess' in args
		is_new_int:          '-new_int' in args
		no_migrate_json2:    '-no-migrate-json2' in args
		module_search_paths: compiler_pref.expand_module_search_paths(cmdline.option(args,
			'-path', ''), os.dir(os.getenv('VEXE')))
		backend:             backend
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
	if '-help' in args || '--help' in args {
		print_vfmt_help_and_exit()
	}
	files := util.find_all_v_files(possible_files) or {
		verror(err.msg())
		return
	}
	if os.is_atty(0) == 0 && files.len == 0 {
		foptions.format_pipe()
		exit(0)
	}
	if files.len == 0 {
		print_vfmt_help_and_exit()
	}
	mut cli_args_no_files := []string{}
	for idx, a in os.args {
		if idx == 0 {
			cli_args_no_files << a
			continue
		}
		if a !in files {
			cli_args_no_files << a
		}
	}
	mut errors := 0
	mut has_internal_error := false
	for file in files {
		fpath := os.real_path(file)
		if foptions.is_verify && foptions.in_process {
			// For a small amount of files, it is faster to process
			// everything directly in the same process, single threaded,
			// when vfmt is compiled with `-gc none`:
			if !foptions.verify_file(fpath) {
				println("${file} is not vfmt'ed")
				errors++
			}
			continue
		}
		mut worker_command_array := cli_args_no_files.clone()
		worker_command_array << ['-worker', fpath]
		worker_cmd := worker_command_array.join(' ')
		foptions.vlog('vfmt worker_cmd: ${worker_cmd}')
		worker_result := os.exec(worker_command_array)
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
	if has_internal_error {
		// When some files could not be processed due to internal vfmt errors,
		// exit with code 5 regardless of format-diff errors in other files.
		// This prevents exit codes like 7 (2+5) that confuse downstream CI checks.
		exit(5)
	}
	if errors > 0 {
		if !foptions.is_diff {
			eprintln('Encountered a total of: ${errors} formatting errors.')
		}
		match true {
			foptions.is_noerror { exit(0) }
			foptions.is_verify { exit(1) }
			foptions.is_c { exit(2) }
			else { exit(1) }
		}
	}
	exit(0)
}

fn (foptions &FormatOptions) verify_file(fpath string) bool {
	content := os.read_file(fpath) or { return false }
	fcontent := foptions.formatted_content_from_file(fpath, false) or { return false }
	return fcontent == content
}

fn (foptions &FormatOptions) vlog(msg string) {
	if foptions.is_verbose {
		eprintln(msg)
	}
}

fn (foptions &FormatOptions) should_migrate_json2(file string) bool {
	if foptions.no_migrate_json2 {
		return false
	}
	// `.vv` files are fixtures (formatter and compiler test inputs) whose legacy
	// source is the point; tests are migrated like any other code.
	return !file.ends_with('.vv')
}

fn imports_json(a &flat.FlatAst) bool {
	return a.nodes.any(it.kind == .import_decl && it.value == 'json')
}

// resolves_project_json_module reports whether `import json` in `file` resolves to
// an existing module. vlib has no `json` module anymore, so such a module belongs
// to the project (beside the file, in a parent directory, or in a module root).
// The compiler keeps using it, so its calls must not be rewritten to `json2`.
fn (foptions &FormatOptions) resolves_project_json_module(file string) bool {
	mut lookup := &compiler_pref.Preferences{
		vroot: os.dir(os.getenv('VEXE'))
	}
	if foptions.module_search_paths.len > 0 {
		// `-path` replaces vlib and ~/.vmodules as the module roots, but the compiler
		// still looks beside the importing file and in its parent directories first
		// (`resolve_local_or_project_module_path` and `resolve_ancestor_module_path`
		// in v.driver); `pref.get_module_path` is only its last fallback.
		// Like `module_dir_belongs_to_other_project`, a parent directory's `json` only
		// counts when it belongs to the importer's project, or declares `json` itself.
		mut roots := foptions.module_search_paths.clone()
		importer_vmod_root := util.nearest_vmod_root(file) or { '' }
		for dir in importer_and_parent_dirs(file) {
			if json_dir_belongs_to_importer(os.join_path(dir, 'json'), importer_vmod_root) {
				roots << dir
			}
		}
		lookup.module_search_paths = roots
	}
	return lookup.get_module_path('json', file) != ''
}

// json_dir_belongs_to_importer reports whether a `json` directory in a parent folder
// is the module an import of the importer (in the project at `importer_vmod_root`)
// resolves to: it is inside that project, or its own `v.mod` declares `json`.
fn json_dir_belongs_to_importer(candidate string, importer_vmod_root string) bool {
	if importer_vmod_root.len == 0 {
		return true
	}
	real_candidate := os.real_path(candidate).replace('\\', '/')
	real_importer := os.real_path(importer_vmod_root).replace('\\', '/')
	if real_candidate == real_importer || real_candidate.starts_with(real_importer + '/') {
		return true
	}
	root := util.nearest_vmod_root(candidate) or { return false }
	manifest := vmod.from_file(os.join_path_single(root, 'v.mod')) or { return false }
	return manifest.name == 'json'
}

// importer_and_parent_dirs lists the directory of `file` and its parents, up to a
// directory with a module search stop marker.
fn importer_and_parent_dirs(file string) []string {
	mut dirs := []string{}
	mut dir := os.dir(os.real_path(file))
	for {
		dirs << dir
		if compiler_pref.is_module_search_stop_dir(dir) {
			break
		}
		parent := os.dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return dirs
}

fn (foptions &FormatOptions) formatted_content_from_file(file string, report_diagnostics bool) !string {
	return foptions.formatted_content_with_imports_from(file, report_diagnostics, file)
}

// formatted_content_with_imports_from formats `file`, resolving its imports as if it
// were `import_file`: stdin is staged in a temporary folder, but its imports belong
// to the caller's working directory.
fn (foptions &FormatOptions) formatted_content_with_imports_from(file string, report_diagnostics bool, import_file string) !string {
	foptions.vlog('vfmt running v.gen.v over file: ${file}')
	mut prefs := compiler_pref.new_preferences()
	prefs.is_fmt = true
	prefs.migrate_json2 = foptions.should_migrate_json2(file)
	prefs.preserve_comptime_conditionals = true
	prefs.supports_inline_asm = true
	mut p := compiler_parser.Parser.new(prefs)
	mut a := p.parse_file(file)
	if a.formatter_migrate_json2 && imports_json(a)
		&& foptions.resolves_project_json_module(import_file) {
		a.formatter_migrate_json2 = false
	}
	if report_compiler_parser_diagnostics(p.diagnostics, a, report_diagnostics) {
		return error('the file contains parser errors')
	}
	return compiler_fmt.format_with_options(a,
		is_debug:   foptions.is_debug
		is_new_int: foptions.is_new_int
		backend:    foptions.backend
	)
}

fn report_compiler_parser_diagnostics(diagnostics []compiler_parser.Diagnostic, a &flat.FlatAst, should_report bool) bool {
	mut has_errors := false
	for diagnostic in diagnostics {
		severity := if diagnostic.severity == '' { 'error:' } else { diagnostic.severity }
		if severity != 'error:' {
			continue
		}
		has_errors = true
		if !should_report {
			continue
		}
		if diagnostic.pos.is_valid() && diagnostic.pos.id in a.source_files {
			eprintln(compiler_errors.formatted_parser_diagnostic(severity, diagnostic.message, a, diagnostic.pos))
		} else {
			eprintln('${diagnostic.file}:${diagnostic.line}:${diagnostic.column}: ${severity} ${diagnostic.message}')
		}
	}
	return has_errors
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
	formatted_content := foptions.formatted_content_from_file(file, !foptions.is_verify
		&& !foptions.is_c) or {
		if foptions.is_verify || foptions.is_c {
			_ = foptions.formatted_content_from_file(file, true) or { exit(2) }
		}
		exit(2)
	}
	os.write_file(vfmt_output_path, formatted_content) or { panic(err) }
	foptions.vlog('vfmt wrote ${formatted_content.len} bytes to ${vfmt_output_path}.')
	eprintln('${formatted_file_token}${vfmt_output_path}')
}

fn (foptions &FormatOptions) format_pipe() {
	input_text := os.get_raw_lines_joined()
	stdin_path := os.join_path(vtmp_folder, 'vfmt_stdin_${rand.ulid()}.v')
	os.write_file(stdin_path, input_text) or {
		eprintln('vfmt could not stage stdin: ${err}')
		exit(1)
	}
	defer {
		os.rm(stdin_path) or {}
	}
	formatted_content := foptions.formatted_content_with_imports_from(stdin_path, true,
		os.join_path(os.getwd(), 'vfmt_stdin.v')) or { exit(1) }
	print(formatted_content)
	flush_stdout()
	foptions.vlog('vfmt wrote ${formatted_content.len} bytes to stdout.')
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

@[noreturn]
fn print_vfmt_help_and_exit() {
	println('Usage: v fmt [options] <file|directory>...')
	println('Options: -w, -verify, -diff, -l, -c, -backup, -inprocess')
	exit(0)
}

@[noreturn]
fn verror(s string) {
	util.verror('vfmt error', s)
}

fn (f FormatOptions) str() string {
	return 'FormatOptions{ is_l: ${f.is_l}, is_w: ${f.is_w}, is_diff: ${f.is_diff}, is_verbose: ${f.is_verbose},' + ' is_worker: ${f.is_worker}, is_debug: ${f.is_debug}, is_noerror: ${f.is_noerror},' + ' is_verify: ${f.is_verify}, backend: ${f.backend}" }'
}
