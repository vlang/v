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
	kind      string // '' = generated-C compilation error; 'compiler_error' = V3 internal compiler error
	ccompiler string
	c_output  string
	// Content only. The process that owned the staged report already bounded the source
	// and deleted the directory, so the retry never reads a path or deletes a directory
	// named by the (inheritable, forgeable) environment.
	v_file                 string // informational base filename (no directory)
	v_source               string // the full failing file (bounded only when larger than the byte budget)
	v_source_truncated     bool   // true when v_source is a bounded excerpt rather than the whole file
	v_source_focus         int    // failing line's 1-based position within v_source (0 = none)
	input_digests          map[string]string
	input_digests_complete bool
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
				pref.parse_args_for_launcher(external_tools, args_and_flags, true)
			}
		}
		util.launch_tool(false, 'vrepl', os.args[1..])
		return
	}
	mut args_and_flags := util.join_env_vflags_and_os_args()[1..]
	prefs, command, command_idx := pref.parse_args_for_launcher_with_command_index(external_tools,
		args_and_flags, true)
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
		mut tool_args := os.args[1..].clone()
		if command == 'self' {
			// vself forwards compiler flags to the compiler it builds. Pass merged
			// VFLAGS once as arguments, then keep them out of vself's own recompilation.
			// Preserve the parser's authoritative command boundary so vself never
			// interprets a flag value as one of its positional arguments.
			tool_args = args_and_flags.clone()
			os.setenv('VSELF_COMMAND_INDEX', command_idx.str(), true)
			os.unsetenv('VFLAGS')
			os.unsetenv('VOSARGS')
		}
		util.launch_tool(prefs.is_verbose, 'v' + command, tool_args)
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
	if prefs.is_fastc {
		// FastC owns its whole invocation and must never launch the AST-based
		// ownership compiler. Its direct parser reports unsupported modes.
		return
	}
	$if macos {
		if macos_v3_test_ownership_uses_v1(prefs, merged_args) {
			return
		}
	}
	if !ownership_delegation_is_requested(is_ownership, is_autofree, prefs.old_compiler,
		prefs.new_compiler, os.user_os()) {
		return
	}
	if is_autofree && !is_ownership && (autofree_requires_standard_compiler(prefs)
		|| autofree_args_require_standard_compiler(merged_args, command)) {
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
	ownership_args := v3_ownership_forwarded_args(prefs, merged_args)
	launch_v3_ownership_compiler(prefs.is_verbose, ownership_args)
}

fn autofree_args_require_standard_compiler(args []string, command string) bool {
	$if macos {
		return macos_v3_has_v1_only_leading_option(args, command)
	}
	return false
}

fn v3_ownership_forwarded_args(prefs &pref.Preferences, merged_args []string) []string {
	mut ownership_args := merged_args.filter(it != '-ownership')
	if !v3_args_have_ownership_define(ownership_args) {
		ownership_args.prepend('ownership')
		ownership_args.prepend('-d')
	}
	$if macos {
		return macos_v3_forwarded_args(prefs, ownership_args)
	}
	return ownership_args
}

fn v3_args_have_ownership_define(args []string) bool {
	for i, arg in args {
		if arg == '-downership' {
			return true
		}
		if arg == '-d' && i + 1 < args.len && args[i + 1] == 'ownership' {
			return true
		}
	}
	return false
}

fn autofree_requires_standard_compiler(prefs &pref.Preferences) bool {
	// Autofree selects no-GC by default, but an explicit collector still belongs
	// to V1 until ownership mode implements it.
	return v3_has_v1_only_preferences(prefs) || (prefs.gc_set_by_flag && prefs.gc_mode != .no_gc)
}

fn v3_has_v1_only_preferences(prefs &pref.Preferences) bool {
	if prefs.cmain.len > 0 || prefs.custom_prelude.len > 0 || prefs.is_check_return
		|| prefs.div_by_zero_is_zero || prefs.obfuscate_removed || prefs.no_std
		|| prefs.is_vls || prefs.new_transform || prefs.is_livemain
		|| prefs.is_liveshared || prefs.show_asserts || prefs.show_callgraph
		|| prefs.show_depgraph || prefs.hide_auto_str || prefs.no_rsp
		|| prefs.message_limit != 200 || prefs.warn_about_allocs
		|| prefs.c_error_bug_report_url.len > 0 || prefs.wasm_validate
		|| prefs.wasm_stack_top != 1024 + (16 * 1024) || prefs.line_info.len > 0
		|| prefs.use_coroutines || prefs.checker_match_exhaustive_cutoff_limit != 12
		|| (prefs.backend == .c && !prefs.is_fastc && prefs.os != ._auto
		&& prefs.os != pref.get_host_os())
		|| prefs.build_options.any(it.starts_with('-debug-tcc')) || prefs.is_musl
		|| prefs.build_options.any(it in ['-musl', '-glibc']) || !prefs.relaxed_gcc14 {
		return true
	}
	return prefs.sanitize || prefs.output_cross_c || prefs.experimental
		|| prefs.use_os_system_to_run || prefs.is_apk || prefs.is_vsh
		|| prefs.json_errors || prefs.no_preludes || prefs.is_quiet
		|| prefs.skip_warnings || prefs.skip_notes || prefs.fatal_errors
		|| prefs.print_watched_files || prefs.dump_modules.len > 0
		|| prefs.dump_files.len > 0 || prefs.dump_defines.len > 0
		|| prefs.print_autofree_vars || prefs.is_vlines || prefs.warn_impure_v
		|| prefs.trace_calls || prefs.trace_fns.len > 0 || prefs.test_runner.len > 0
		|| prefs.exclude.len > 0 || prefs.ldflags.len > 0 || prefs.nofloat
		|| prefs.fast_math || prefs.compress || prefs.is_bare || prefs.no_closures
		|| prefs.disable_explicit_mutability || prefs.assert_failure_mode != .default
		|| prefs.macosx_version_min != '0'
		|| prefs.build_options.any(it in ['-m32', '-m64']) || prefs.backend.is_js()
		|| (prefs.backend == .wasm && prefs.is_run) || prefs.path.ends_with('.vv')
}

fn ownership_delegation_is_requested(is_ownership bool, is_autofree bool, old_compiler bool, new_compiler bool, host_os string) bool {
	if old_compiler {
		return false
	}
	if is_ownership {
		return true
	}
	// Let the embedded dispatcher reject the unsupported explicit combination;
	// ownership delegation would otherwise strip -new-compiler before it can do so.
	if new_compiler {
		return false
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
		compilation_command := '${os.quoted_path(vexe)} -no-parallel -nocache -gc none -d ownership -o ${os.quoted_path(v3_exe)} ${os.quoted_path(v3_main_source)}'
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
					kind:                   failed.kind
					ccompiler:              failed.ccompiler
					c_output:               failed.c_output
					v_file:                 failed.v_file
					v_source:               failed.v_source
					v_source_truncated:     failed.v_source_truncated
					v_source_focus:         failed.v_source_focus
					source_inline:          true
					input_digests:          failed.input_digests
					input_digests_complete: failed.input_digests_complete
					tag:                    'V3'
				})
			} else {
				builder.compile('build', prefs, cbuilder.compile_c)
			}
		}
		.js_node, .js_freestanding, .js_browser {
			// The js backends are V1-only and never receive a V3 fallback report; the
			// content-only report (if any inherited one is present) names no directory to
			// clean, so there is nothing to do here but hand off.
			util.launch_tool(prefs.is_verbose, 'builders/js_builder', os.args[1..])
		}
		.interpret {
			eprintln('The eval backend has been removed.')
			exit(1)
		}
		.wasm {
			if failed := macos_v3_c_error_report {
				// The wasm builder runs as an external tool via os.execvp, which replaces
				// this process, so this process cannot submit the V3->V1 fallback report or
				// print the notice after the retry. Forward the already-bounded content
				// through the environment; the builder submits/notifies on its own build
				// success without ever reading a path or deleting a directory named by the
				// (inheritable, forgeable) environment.
				builder.export_external_v3_report_to_env(builder.ExternalCErrorBugReport{
					kind:                   failed.kind
					ccompiler:              failed.ccompiler
					c_output:               failed.c_output
					v_file:                 failed.v_file
					v_source:               failed.v_source
					v_source_truncated:     failed.v_source_truncated
					v_source_focus:         failed.v_source_focus
					source_inline:          true
					input_digests:          failed.input_digests
					input_digests_complete: failed.input_digests_complete
					tag:                    'V3'
				})
			}
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
