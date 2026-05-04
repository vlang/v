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
const delegated_v2_exe_env = 'V_V2_EXE'

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
	maybe_delegate_to_v2(command, prefs)
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
			rebuild(prefs)
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
			eprintln('use v -v2 -eval file.v')
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
				rebuild(prefs)
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

fn maybe_delegate_to_v2(command string, prefs &pref.Preferences) {
	is_ownership := '-ownership' in os.args
	if !prefs.use_v2 && !is_ownership {
		return
	}
	if !is_v2_relevant_command(command, prefs) {
		eprintln('v: `-v2`/`-ownership` currently support direct compilation only. Use `v -v2 hello.v` or `v -ownership module_dir`.')
		exit(1)
	}
	launch_v2_compiler(prefs.is_verbose, os.args[1..].filter(it != '-v2'), is_ownership)
}

fn is_v2_relevant_command(command string, prefs &pref.Preferences) bool {
	if prefs.path == '' || prefs.is_run || prefs.is_crun {
		return false
	}
	return prefs.path == command && (command.ends_with('.v') || os.exists(command))
}

@[noreturn]
fn launch_v2_compiler(is_verbose bool, args []string, is_ownership bool) {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	util.set_vroot_folder(vroot)
	tool_name := if is_ownership { 'v2_ownership' } else { 'v2' }
	mut v2_exe := os.getenv(delegated_v2_exe_env)
	if v2_exe == '' {
		v2_source := os.join_path(vroot, 'cmd', 'v2', 'v2.v')
		v2_exe = cached_v2_executable_path(vroot, is_ownership)
		v2_exe_dir := os.dir(v2_exe)
		os.mkdir_all(v2_exe_dir) or {
			eprintln('cannot create `${v2_exe_dir}`: ${err}')
			exit(1)
		}
		if util.should_recompile_tool(vexe, v2_source, tool_name, v2_exe) {
			d_flag := if is_ownership { '-d ownership ' } else { '' }
			compilation_command := '${os.quoted_path(vexe)} ${d_flag}-o ${os.quoted_path(v2_exe)} ${os.quoted_path(v2_source)}'
			if is_verbose {
				println('Compiling ${tool_name} with: "${compilation_command}"')
			}
			current_work_dir := os.getwd()
			os.chdir(vroot) or {}
			tool_compilation := os.execute(compilation_command)
			os.chdir(current_work_dir) or {}
			if tool_compilation.exit_code != 0 {
				eprintln('cannot compile `${v2_source}`: ${tool_compilation.exit_code}\n${tool_compilation.output}')
				exit(1)
			}
		}
	} else if !os.is_file(v2_exe) {
		eprintln('v: `${delegated_v2_exe_env}` points to a missing executable: `${v2_exe}`')
		exit(1)
	}
	os.setenv('VCHILD', 'true', true)
	os.setenv('VEXE', os.real_path(v2_exe), true)
	os.execvp(v2_exe, args) or {
		eprintln('> error while executing: ${v2_exe} ${args}')
		panic(err)
	}
	exit(2)
}

fn cached_v2_executable_path(vroot string, is_ownership bool) string {
	vroot_hash := hash.sum64_string(os.real_path(vroot), 0).hex_full()
	exe_name := if is_ownership { 'v2_ownership' } else { 'v2' }
	return util.path_of_executable(os.join_path(os.vtmp_dir(), 'v', 'delegated_v2', vroot_hash,
		exe_name))
}

fn rebuild(prefs &pref.Preferences) {
	match prefs.backend {
		.c {
			$if no_bootstrapv ? {
				// TODO: improve the bootstrapping with a split C backend here.
				// C code generated by `VEXE=v cmd/tools/builders/c_builder -os cross -o c.c cmd/tools/builders/c_builder.v`
				// is enough to bootstrap the C backend, and thus the rest, but currently bootstrapping relies on
				// `v -os cross -o v.c cmd/v` having a functional C codegen inside instead.
				util.launch_tool(prefs.is_verbose, 'builders/c_builder', os.args[1..])
			}
			builder.compile('build', prefs, cbuilder.compile_c)
		}
		.js_node, .js_freestanding, .js_browser {
			util.launch_tool(prefs.is_verbose, 'builders/js_builder', os.args[1..])
		}
		.interpret {
			eprintln('use v -v2 -eval file.v')
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
