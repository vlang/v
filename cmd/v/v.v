// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import hash
import os
import term
import v.help
import v.pref
import v.util
import v.util.version
import v.builder
import v.builder.cbuilder

@[markused]
const external_tools = [
	'ast',
	'bin2v',
	'bug',
	'bug-report',
	'bug-report-send',
	'build-examples',
	'build-tools',
	'build-vbinaries',
	'bump',
	'check-md',
	'complete',
	'compress',
	'cover',
	'diff',
	'doc',
	'doctor',
	'download',
	'fmt',
	'git-fmt-hook',
	'gret',
	'ls',
	'missdoc',
	'quest',
	'reduce',
	'repl',
	'repeat',
	'retry',
	'self',
	'setup-freetype',
	'shader',
	'share',
	'should-compile-all',
	'sqlite',
	'symlink',
	'scan',
	'test',
	'test-all', // runs most of the tests and other checking tools, that will be run by the CI
	'test-cleancode',
	'test-fmt',
	'test-parser',
	'test-self',
	'time',
	'timeout',
	'tracev',
	'up',
	'vet',
	'wipe-cache',
	'watch',
	'where',
]

struct MacosV3CErrorReport {
	ccompiler  string
	c_output   string
	c_file     string
	report_dir string
}

@[unsafe]
fn timers_pointer(p &util.Timers) &util.Timers {
	// TODO: the static variable here is used as a workaround for the current incompatibility of -usecache and globals in the main module:
	mut static ptimers := unsafe { &util.Timers(nil) }
	if p != unsafe { nil } {
		ptimers = p
	}
	return ptimers
}

fn main() {
	unbuffer_stdout()
	mut timers_should_print := false
	$if time_v ? {
		timers_should_print = true
	}
	if '-show-timings' in os.args {
		timers_should_print = true
	}
	mut timers := unsafe {
		timers_pointer(util.new_timers(
			should_print: timers_should_print
			label:        'main'
		))
	}
	timers.start('v start')
	timers.show('v start')
	timers.start('TOTAL')
	// use at_exit here, instead of defer, since some code paths later do early exit(0) or exit(1), for showing errors, or after `v run`
	at_exit(fn () {
		mut timers := unsafe { timers_pointer(nil) }
		timers.show('TOTAL')
	})!
	timers.start('v parsing CLI args')
	args := os.args[1..]

	if args.len == 0 || args[0] in ['-', 'repl'] {
		if args.len == 0 {
			// Running `./v` without args launches repl
			if os.is_atty(0) == 0 {
				mut args_and_flags := util.join_env_vflags_and_os_args()[1..].clone()
				args_and_flags << ['run', '-']
				pref.parse_args_and_show_errors(external_tools, args_and_flags, true)
			}
		}
		util.launch_tool(false, 'vrepl', os.args[1..])
		return
	}
	mut args_and_flags := util.join_env_vflags_and_os_args()[1..]
	prefs, command := pref.parse_args_and_show_errors(external_tools, args_and_flags, true)
	maybe_delegate_to_vvmrc(command, prefs)
	maybe_delegate_to_ownership(command, prefs, args_and_flags)
	macos_v3_c_error_report := maybe_delegate_to_macos_v3(command, prefs)
	if prefs.use_cache && os.user_os() == 'windows' {
		eprintln('-usecache is currently disabled on windows')
		exit(1)
	}
	timers.show('v parsing CLI args')

	setup_vbuild_env_vars(prefs)

	// Start calling the correct functions/external tools
	// Note for future contributors: Please add new subcommands in the `match` block below.
	if command in external_tools {
		// External tools
		util.launch_tool(prefs.is_verbose, 'v' + command, os.args[1..])
		return
	}
	match command {
		'run', 'crun', 'build', 'build-module' {
			rebuild(prefs, macos_v3_c_error_report)
			return
		}
		'help' {
			invoke_help_and_exit(args)
		}
		'version' {
			println(version.full_v_version(prefs.is_verbose))
			return
		}
		'new', 'init' {
			util.launch_tool(prefs.is_verbose, 'vcreate', os.args[1..])
			return
		}
		'install', 'link', 'list', 'outdated', 'remove', 'search', 'show', 'unlink', 'update',
		'upgrade' {
			util.launch_tool(prefs.is_verbose, 'vpm', os.args[1..])
			return
		}
		'vlib-docs' {
			util.launch_tool(prefs.is_verbose, 'vdoc', ['doc', 'vlib'])
		}
		'interpret' {
			eprintln('The eval backend has been removed.')
			exit(1)
		}
		'get' {
			eprintln('V Error: Use `v install` to install modules from vpm.vlang.io')
			exit(1)
		}
		'translate' {
			util.launch_tool(prefs.is_verbose, 'translate', os.args[1..])
			// exit(1)
			// return
		}
		else {
			if command.ends_with('.v') || os.exists(command) {
				// println('command')
				// println(prefs.path)
				rebuild(prefs, macos_v3_c_error_report)
				return
			}
		}
	}

	if prefs.is_help {
		invoke_help_and_exit(args)
	}

	other_commands := ['run', 'crun', 'build', 'build-module', 'help', 'version', 'new', 'init',
		'install', 'link', 'list', 'outdated', 'remove', 'search', 'show', 'unlink', 'update',
		'upgrade', 'vlib-docs', 'translate']
	mut all_commands := []string{}
	all_commands << external_tools
	all_commands << other_commands
	all_commands.sort()
	eprintln(util.new_suggestion(command, all_commands, similarity_threshold: 0.2).say('v: unknown command `${command}`'))
	eprintln('Run ${term.highlight_command('v help')} for usage.')
	exit(1)
}

fn invoke_help_and_exit(remaining []string) {
	match remaining.len {
		0, 1 { help.print_and_exit('default', exit_code: 0) }
		2 { help.print_and_exit(remaining[1], exit_code: 0) }
		else {}
	}

	eprintln('${term.highlight_command('v help')}: provide only one help topic.')
	eprintln('For usage information, use ${term.highlight_command('v help')}.')
	exit(1)
}

fn maybe_delegate_to_ownership(command string, prefs &pref.Preferences, merged_args []string) {
	is_ownership := '-ownership' in merged_args
	is_autofree := prefs.autofree
	if !ownership_delegation_is_requested(is_ownership, is_autofree, prefs.old_compiler,
		os.user_os()) {
		return
	}
	if is_autofree && !is_ownership && autofree_requires_standard_compiler(prefs) {
		return
	}
	if !is_ownership_relevant_command(command, prefs) {
		// `-autofree` is also an established option for command modes such as
		// `run` and `test`. Leave modes that do not compile directly on the regular
		// command path instead of rejecting them in the ownership dispatcher.
		if is_autofree && !is_ownership {
			return
		}
		mode := if is_autofree { '-autofree' } else { '-ownership' }
		eprintln('v: `${mode}` currently supports direct compilation only. Use `v ${mode} module_dir`.')
		exit(1)
	}
	ownership_args := merged_args.filter(it != '-ownership')
	launch_v3_ownership_compiler(prefs.is_verbose, ownership_args)
}

fn autofree_requires_standard_compiler(prefs &pref.Preferences) bool {
	// Autofree selects no-GC by default, but an explicit collector still belongs
	// to V1 until ownership mode implements it.
	return v3_has_v1_only_preferences(prefs) || (prefs.gc_set_by_flag && prefs.gc_mode != .no_gc)
}

fn v3_has_v1_only_preferences(prefs &pref.Preferences) bool {
	return prefs.sanitize || prefs.is_livemain || prefs.is_liveshared
		|| prefs.is_prof || prefs.output_cross_c || prefs.experimental
		|| prefs.use_os_system_to_run || prefs.is_apk || prefs.json_errors
		|| prefs.no_preludes || prefs.is_quiet || prefs.skip_warnings
		|| prefs.skip_notes || prefs.fatal_errors || prefs.print_watched_files
		|| prefs.print_autofree_vars || prefs.is_vlines || prefs.warn_impure_v
		|| prefs.trace_calls || prefs.trace_fns.len > 0 || prefs.test_runner.len > 0
		|| prefs.exclude.len > 0 || prefs.ldflags.len > 0 || prefs.nofloat
		|| prefs.fast_math || prefs.compress || prefs.is_bare || prefs.no_closures
		|| prefs.assert_failure_mode != .default || prefs.macosx_version_min != '0'
		|| prefs.build_options.any(it in ['-m32', '-m64']) || prefs.backend.is_js()
		|| (prefs.backend == .wasm && prefs.is_run) || prefs.path.ends_with('.vv')
}

fn ownership_delegation_is_requested(is_ownership bool, is_autofree bool, old_compiler bool, host_os string) bool {
	if old_compiler {
		return false
	}
	if is_ownership {
		return true
	}
	return is_autofree && host_os == 'macos'
}

fn is_ownership_relevant_command(command string, prefs &pref.Preferences) bool {
	if prefs.path == '' || prefs.is_run || prefs.is_crun {
		return false
	}
	return prefs.path == command && (command.ends_with('.v') || os.exists(command))
}

@[noreturn]
fn launch_v3_ownership_compiler(is_verbose bool, args []string) {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	util.set_vroot_folder(vroot)
	tool_name := 'v3_ownership'
	v3_main_source := os.join_path(vroot, 'vlib', 'v3', 'v3.v')
	v3_src_dir := os.join_path(vroot, 'vlib', 'v3')
	v3_exe := cached_v3_ownership_executable_path(vroot)
	v3_exe_dir := os.dir(v3_exe)
	os.mkdir_all(v3_exe_dir) or {
		eprintln('cannot create `${v3_exe_dir}`: ${err}')
		exit(1)
	}
	if util.should_recompile_tool(vexe, v3_src_dir, tool_name, v3_exe) {
		compilation_command := '${os.quoted_path(vexe)} -nocache -gc none -d ownership -o ${os.quoted_path(v3_exe)} ${os.quoted_path(v3_main_source)}'
		if is_verbose {
			println('Compiling ${tool_name} with: "${compilation_command}"')
		}
		current_work_dir := os.getwd()
		caller_vflags := os.getenv('VFLAGS')
		caller_vosargs := os.getenv('VOSARGS')
		// The bootstrap command already supplies its compiler configuration. Do not
		// let target flags recursively select this ownership launcher again.
		os.unsetenv('VFLAGS')
		os.unsetenv('VOSARGS')
		os.chdir(vroot) or {}
		tool_compilation := os.execute(compilation_command)
		os.chdir(current_work_dir) or {}
		os.setenv('VFLAGS', caller_vflags, true)
		os.setenv('VOSARGS', caller_vosargs, true)
		if tool_compilation.exit_code != 0 {
			eprintln('cannot compile `${v3_main_source}`: ${tool_compilation.exit_code}\n${tool_compilation.output}')
			exit(1)
		}
	}
	mut forwarded_args := ['-ownership']
	$if macos {
		// The embedded/default V3 path disables its conservative compiler-memory
		// guard on macOS too. Keep `-autofree` on the same footing when it uses the
		// dedicated ownership-enabled V3 binary.
		if '-no-memory-limit' !in args && '--no-memory-limit' !in args {
			forwarded_args << '-no-memory-limit'
		}
	}
	for arg in args {
		forwarded_args << arg
	}
	quoted_args := forwarded_args.map(os.quoted_path(it)).join(' ')
	if is_verbose {
		println('Launching ${tool_name}: ${os.quoted_path(v3_exe)} ${quoted_args}')
	}
	os.setenv('VCHILD', 'true', true)
	os.setenv('VEXE', os.real_path(vexe), true)
	res := os.system('${os.quoted_path(v3_exe)} ${quoted_args}')
	exit(res)
}

fn cached_v3_ownership_executable_path(vroot string) string {
	vroot_hash := hash.sum64_string(os.real_path(vroot), 0).hex_full()
	return util.path_of_executable(os.join_path(os.vtmp_dir(), 'v', 'delegated_v3', vroot_hash,
		'v3_ownership'))
}

fn rebuild(prefs &pref.Preferences, macos_v3_c_error_report ?MacosV3CErrorReport) {
	match prefs.backend {
		.c {
			$if no_bootstrapv ? {
				// TODO: improve the bootstrapping with a split C backend here.
				// C code generated by `VEXE=v cmd/tools/builders/c_builder -os cross -o c.c cmd/tools/builders/c_builder.v`
				// is enough to bootstrap the C backend, and thus the rest, but currently bootstrapping relies on
				// `v -os cross -o v.c cmd/v` having a functional C codegen inside instead.
				util.launch_tool(prefs.is_verbose, 'builders/c_builder', os.args[1..])
			}
			if failed := macos_v3_c_error_report {
				builder.compile_with_external_c_error_report('build', prefs, cbuilder.compile_c, builder.ExternalCErrorBugReport{
					ccompiler:   failed.ccompiler
					c_output:    failed.c_output
					c_file:      failed.c_file
					tag:         'V3'
					cleanup_dir: failed.report_dir
				})
			} else {
				builder.compile('build', prefs, cbuilder.compile_c)
			}
		}
		.js_node, .js_freestanding, .js_browser {
			util.launch_tool(prefs.is_verbose, 'builders/js_builder', os.args[1..])
		}
		.interpret {
			eprintln('The eval backend has been removed.')
			exit(1)
		}
		.wasm {
			util.launch_tool(prefs.is_verbose, 'builders/wasm_builder', os.args[1..])
		}
	}
}

@[manualfree]
fn setup_vbuild_env_vars(prefs &pref.Preferences) {
	mut facts := []string{cap: 10}
	facts << prefs.os.lower()
	facts << prefs.ccompiler_type.str()
	facts << prefs.arch.str()
	if prefs.is_prod {
		facts << 'prod'
	}
	github_job := os.getenv('GITHUB_JOB')
	if github_job != '' {
		facts << github_job
	}
	pref.set_build_flags_and_defines(facts, prefs.compile_defines_all)
	unsafe { github_job.free() }
	unsafe { facts.free() }
}
