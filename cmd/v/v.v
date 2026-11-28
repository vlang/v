// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import v.cmdexec
import v.driver
import v.help
import v.pref
import v.util

const v_version = '0.5.2'
const external_commands = [
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
	'clean',
	'complete',
	'compress',
	'cover',
	'diff',
	'doc',
	'doctor',
	'download',
	'env',
	'fmt',
	'generate',
	'git-fmt-hook',
	'gret',
	'init',
	'install',
	'link',
	'list',
	'ls',
	'mcp',
	'missdoc',
	'mod',
	'new',
	'outdated',
	'pbgen',
	'quest',
	'reduce',
	'remove',
	'repl',
	'repeat',
	'retry',
	'search',
	'self',
	'setup-freetype',
	'shader',
	'share',
	'skills',
	'should-compile-all',
	'show',
	'sqlite',
	'symlink',
	'scan',
	'test',
	'test-all',
	'test-cleancode',
	'test-fmt',
	'test-parser',
	'test-self',
	'time',
	'timeout',
	'tool',
	'tracev',
	'translate',
	'unlink',
	'up',
	'update',
	'upgrade',
	'vendor',
	'vet',
	'vlib-docs',
	'wasm',
	'watch',
	'where',
	'why',
	'wipe-cache',
]

fn main() {
	$if windows {
		disable_windows_loader_dialogs()
	}
	$if prealloc {
		// The compiler's arenas live until the process ends; the OS reclaims them
		// with less work than unmapping each block in the exit handler.
		unsafe { prealloc_keep_memory_at_exit() }
	}
	vexe := os.real_path(os.executable())
	// Ownership helpers reset VEXE for their own tool dispatch, but module cache
	// isolation must keep identifying the primary compiler that launched them.
	primary := os.getenv('V3_PRIMARY_VEXE')
	os.setenv('V3_PRIMARY_VEXE', if ownership_checker_is_compiled() && primary != '' {
		primary
	} else {
		vexe
	}, true)
	os.setenv('VEXE', vexe, true)
	os.setenv('VCHILD', 'true', true)
	mut args := remove_obsolete_skip_unused_flag(merged_v_args())
	if args.len == 0 {
		run_external_tool(args, -1, 'repl')
		return
	}
	if reuse_shared_test_compiler(args) {
		return
	}
	compiler_args := compiler_args_before_program(args)
	command_index, command := find_command(args)
	// Tool arguments belong to the tool, including a completion query for the removed flag.
	option_args := if command in external_commands || command == 'help' {
		args[..command_index]
	} else {
		compiler_args
	}
	// Reject the removed flag before tool dispatch can consume compiler options.
	if compiler_option_requested(option_args, '-usecache') {
		eprintln('unknown option `-usecache`; caching is enabled by default, use `-nocache` to disable it')
		exit(1)
	}
	// Refuse the TinyCC and -prod combination before dispatch.
	if command !in external_commands
		&& command !in ['version', '-version', '--version', 'help', '-h', '--help', 'new', 'init',
			'install', 'link', 'list', 'outdated', 'remove', 'registry', 'search', 'show', 'unlink',
			'update', 'upgrade', 'vlib-docs', 'get', 'interpret']
		&& pref.tcc_with_prod_requested(compiler_args) {
		eprintln('tcc is not allowed with -prod')
		exit(1)
	}
	if command in ['version', '-version', '--version'] {
		println('V ${v_version} ${@VCURRENTHASH}')
		return
	}
	if command in ['help', '-h', '--help'] {
		print_help(args, command_index)
		return
	}
	if command == 'get' {
		eprintln('V Error: Use `v install` to install modules from vpm.vlang.io')
		exit(1)
	}
	if command == 'interpret' {
		eprintln('The eval backend has been removed.')
		exit(1)
	}
	if command == 'wipe-cache' {
		run_wipe_cache(args, command_index)
		return
	}
	if command in external_commands
		|| command in ['new', 'init', 'generate', 'install', 'link', 'list', 'outdated', 'remove',
			'registry', 'search', 'show', 'unlink', 'update', 'upgrade', 'vlib-docs'] {
		if command == 'test' {
			runs := project_test_runs(args, command_index)
			if runs.len > 1 {
				exit(run_project_tests(runs))
			}
			run_external_tool(runs[0], command_index + runs[0].len - args.len, command)
			return
		}
		run_external_tool(args, command_index, command)
		return
	}
	if input := wasm_input(args) {
		prefix_end := if input > 0 && args[input - 1] == 'run' { input - 1 } else { input }
		mut runner_args := args[..prefix_end].clone()
		runner_args << 'wasm'
		runner_args << args[input..]
		run_external_tool(runner_args, prefix_end, 'wasm')
		return
	}
	args = with_project_backend(args)
	if full_compiler_is_required(compiler_args_before_program(args)) {
		launch_full_compiler(args)
	}
	if ownership_compiler_is_required(compiler_args_before_program(args))
		&& !ownership_checker_is_compiled() {
		launch_ownership_compiler(args)
	}
	driver.run(args)
}

// compiler_option_requested reports whether `option` is one of the compiler options, and not
// an argument of the program that `v run` starts. Like the driver, it reads compiler options
// after the input too, except when the input is run: then they belong to the program. The
// driver runs the input of `run` and `crun`, a `.vsh` script, and the input that follows
// `-raw-vsh-tmp-prefix`; `-` is the input that reads the program from stdin.
fn compiler_option_requested(args []string, option string) bool {
	return compiler_option_index(args, option) >= 0
}

fn compiler_option_index(args []string, option string) int {
	mut option_value_follows := false
	mut runs_input := false
	mut input_seen := false
	for i, arg in args {
		if option_value_follows {
			option_value_follows = false
			continue
		}
		if is_inline_source_option(arg) {
			return -1
		}
		if arg == option {
			return i
		}
		if arg in ['-prof', '-profile'] {
			option_value_follows = profile_option_consumes_value(args, i)
			continue
		}
		if arg == '-raw-vsh-tmp-prefix' && !input_seen {
			runs_input = true
		}
		if arg == '-cf' || pref.option_may_consume_value(arg) {
			option_value_follows = true
			continue
		}
		if arg.starts_with('-') && arg != '-' {
			continue
		}
		if !input_seen && !runs_input && arg in ['run', 'crun'] {
			runs_input = true
			continue
		}
		if !input_seen && !runs_input && arg in ['build', 'test'] {
			continue
		}
		if runs_input || arg.ends_with('.vsh') || arg.ends_with('.wasm') {
			return -1
		}
		input_seen = true
	}
	return -1
}

// remove_obsolete_skip_unused_flag keeps the old spelling as a notice-only no-op.
// Option values, tool arguments and arguments of the executed program are preserved.
fn remove_obsolete_skip_unused_flag(args []string) []string {
	mut result := args.clone()
	mut noticed := false
	for {
		command_index, command := find_command(result)
		options := if command in external_commands || command == 'help' {
			result[..command_index]
		} else {
			result
		}
		index := compiler_option_index(options, '-skip-unused')
		if index < 0 {
			return result
		}
		if !noticed {
			eprintln('notice: `-skip-unused` is no longer needed; unused code is skipped by default. Use `-no-skip-unused` to retain it.')
			noticed = true
		}
		result.delete(index)
	}
}

fn ownership_checker_is_compiled() bool {
	$if ownership ? {
		return true
	}
	return false
}

fn ownership_compiler_is_required(args []string) bool {
	mut define_follows := false
	for arg in args {
		if define_follows {
			if arg.all_before('=').trim_space() == 'ownership' {
				return true
			}
			define_follows = false
			continue
		}
		if arg in ['-ownership', '--ownership', '-autofree', '-downership'] {
			return true
		}
		define_follows = arg in ['-d', '-define']
	}
	return false
}

fn compiler_bootstrap_requested(args []string, vroot string) bool {
	entries := ['cmd/v', 'vlib/v'].map(os.real_path(os.join_path(vroot, it)))
	mut option_value_follows := false
	for arg in args {
		if option_value_follows {
			option_value_follows = false
			continue
		}
		if arg == '-cf' || pref.option_may_consume_value(arg) {
			option_value_follows = true
			continue
		}
		if arg.starts_with('-') || arg in ['run', 'crun'] {
			continue
		}
		if !os.exists(arg) {
			return false
		}
		input := os.real_path(arg)
		if os.is_dir(input) {
			return input in entries
		}
		return os.file_name(input) == 'v.v' && os.dir(input) in entries
	}
	return false
}

fn ownership_compiler_bootstrap_input(args []string, vroot string) bool {
	return compiler_bootstrap_requested(args, vroot)
}

// ownership_bootstrap_can_use_current_compiler distinguishes compiling in support from
// actually checking the compiler source with an ownership mode.
fn ownership_bootstrap_can_use_current_compiler(args []string, vroot string) bool {
	if args.any(it in ['-ownership', '--ownership', '-autofree']) {
		return false
	}
	return ownership_compiler_bootstrap_input(args, vroot)
}

// ownership_compiler_build_args turn `cmd/v` into the V3 executable that contains the optional
// ownership checker.
const ownership_compiler_build_args = ['-d', 'ownership', '-gc', 'none', '-prealloc']

// launch_ownership_compiler builds and starts a V3 executable that contains the optional
// ownership checker. The regular compiler stays small and preserves normal value semantics;
// only explicit ownership/autofree compilations pay for the additional checker.
@[noreturn]
fn launch_ownership_compiler(args []string) {
	vexe := os.real_path(os.executable())
	vroot := find_vroot(vexe) or {
		find_vroot(@VEXEROOT) or {
			eprintln('the V source tree could not be found')
			exit(1)
		}
	}
	// A regular V3 compiler is deliberately allowed to create the ownership-enabled
	// executable. Do not recursively dispatch that bootstrap compilation to itself.
	if ownership_bootstrap_can_use_current_compiler(args, vroot) {
		driver.run(args)
		exit(0)
	}
	launch_optional_compiler(vexe, vroot, 'v3_ownership', 'V3 ownership', ownership_compiler_build_args, args)
}

// launch_optional_compiler shares source validation, locking, and cache publication with tools.
@[noreturn]
fn launch_optional_compiler(vexe string, vroot string, name string, description string, build_args []string, args []string) {
	compiler_source := os.join_path(vroot, 'cmd', 'v')
	if tool_cache_is_disabled() {
		run_uncached_optional_compiler(vexe, compiler_source, name, description, build_args, args)
	}
	entry := tool_cache_entry(vexe, vroot, name, compiler_source, build_args,
		optional_compiler: true
	) or {
		eprintln('cannot find a writable cache for the ${description} compiler')
		eprintln(err.msg())
		exit(1)
	}
	reason := tool_cache_stale_reason(entry)
	if reason != '' {
		if tool_cache_is_verbose() {
			eprintln('> recompiling `${name}`, because ${reason}')
		}
		build_tool_binary(vexe, entry) or {
			eprintln('cannot build the ${description} compiler:\n${err.msg().trim_space()}')
			exit(1)
		}
	}
	exec_cached_tool(entry.binary, args)
}

// run_uncached_optional_compiler honours `VTOOLS_NO_CACHE` for optional compilers, the way
// `launch_external_tool` does for the other tools: nothing is read from or written to the tool
// cache. The compiler is built into a new private folder in `VTMP`, where every compilation
// already keeps its generated C, run once with `args`, and removed again.
@[noreturn]
fn run_uncached_optional_compiler(vexe string, compiler_source string, name string, description string, compiler_build_args []string, args []string) {
	stage_dir := create_tool_cache_stage_dir(os.vtmp_dir()) or {
		eprintln('cannot build the ${description} compiler: ${err.msg()}')
		exit(1)
	}
	binary := os.join_path(stage_dir, '${name}${tool_exe_suffix()}')
	if tool_cache_is_verbose() {
		eprintln('> ${tool_cache_disable_env} is set, building `${name}` in `${stage_dir}`')
	}
	mut build_args := compiler_build_args.clone()
	build_args << ['-o', binary, compiler_source]
	mut build := os.new_process(vexe)
	build.set_args(build_args)
	build.set_environment(tool_build_environment(true))
	build.wait()
	build_code := build.code
	build.close()
	if build_code != 0 || !os.is_file(binary) {
		remove_tool_stage_dir(stage_dir)
		eprintln('cannot build the ${description} compiler')
		exit(1)
	}
	mut compiler := os.new_process(binary)
	compiler.set_args(args)
	compiler.wait()
	code := compiler.code
	compiler.close()
	remove_tool_stage_dir(stage_dir)
	exit(code)
}

fn find_command(args []string) (int, string) {
	mut option_value_follows := false
	for i, arg in args {
		if option_value_follows {
			option_value_follows = false
			continue
		}
		if is_inline_source_option(arg) {
			break
		}
		if arg in ['-prof', '-profile'] {
			option_value_follows = profile_option_consumes_value(args, i)
			continue
		}
		if arg == '-cf' || pref.option_may_consume_value(arg) {
			option_value_follows = true
			continue
		}
		if arg in external_commands
			|| arg in ['version', '-version', '--version', 'help', '-h', '--help', 'get', 'interpret',
				'new', 'init', 'generate', 'install', 'link', 'list', 'outdated', 'remove', 'registry',
				'search', 'show', 'unlink', 'update', 'upgrade', 'vlib-docs'] {
			return i, arg
		}
		if !arg.starts_with('-') {
			break
		}
	}
	return -1, ''
}

fn run_external_tool(args []string, command_index int, command string) {
	vroot := find_vroot(os.executable()) or {
		find_vroot(@VEXEROOT) or {
			eprintln('the V source tree could not be found')
			exit(1)
		}
	}
	tool_name := match command {
		'translate' {
			'translate'
		}
		'new', 'init' {
			'vcreate'
		}
		'install', 'link', 'list', 'outdated', 'remove', 'registry', 'search', 'show', 'unlink',
		'update', 'upgrade', 'why', 'vendor' {
			'vpm'
		}
		'vlib-docs' {
			'vdoc'
		}
		else {
			'v' + command
		}
	}

	base := os.join_path(vroot, 'cmd', 'tools', tool_name)
	tool_source := find_external_tool_source(base) or {
		eprintln('cannot find the `${command}` tool source in `${vroot}`')
		exit(1)
	}
	mut prefix_args := []string{}
	if command_index > 0 {
		prefix_args << args[..command_index]
	}
	mut tool_args := []string{}
	if command_index >= 0 {
		tool_args = external_tool_runtime_args(command, prefix_args, args[command_index..])
	}
	if command in test_session_commands {
		export_vtest_build_environment(vroot, prefix_args)
	}
	launch_external_tool(vroot, tool_name, tool_source, prefix_args, tool_args)
}

// test_session_commands start a `cmd/tools/` program that evaluates the
// `// vtest build:` constraints of the files it compiles, through
// `cmd/tools/testing`, against the `VBUILD_FACTS`/`VBUILD_DEFINES` environment.
const test_session_commands = ['test', 'test-self', 'test-cleancode', 'test-fmt', 'build-examples',
	'build-tools', 'build-vbinaries']

// export_vtest_build_environment records the facts and defines of the
// compilation that the prefix compiler options describe, as the compiler itself
// resolves them, so that the test runner skips and builds the same files that
// `v file_test.v` would.
fn export_vtest_build_environment(vroot string, prefix_args []string) {
	environment := driver.vtest_build_environment(vroot, prefix_args)
	pref.set_build_flags_and_defines(environment.facts, environment.defines)
}

fn external_tool_runtime_args(command string, prefix_args []string, command_args []string) []string {
	// These tools consume the same argv as their standalone executable. The launcher owns
	// the subcommand name, so remove it here without guessing from user arguments.
	if command in ['cover', 'diff', 'download', 'gret', 'repeat', 'retry', 'time', 'timeout', 'wasm'] {
		return command_args[1..].clone()
	}
	mut tool_args := []string{}
	// `v build-tools` consumes compiler options itself and applies them to every
	// tool in its inventory. `v self` likewise treats prefix compiler options as
	// options for the replacement compiler, not just for the launcher helper.
	// `v test` needs them for each test compilation and its failure reproduction command.
	// Keep those options visible after the launcher has built the cached executable.
	if command in ['build-examples', 'build-tools', 'self', 'test', 'test-all', 'test-self'] {
		tool_args << prefix_args
	}
	if command == 'watch' {
		// `v watch` reconstructs the compiler command from its own argv (see
		// `all_args_before_watch_cmd` in cmd/tools/vwatch.v), so prefix compiler options like
		// `-d veb_livereload` have to reach the tool again instead of only affecting
		// the compilation of the watcher binary.
		tool_args << prefix_args
	}
	if command == 'fmt' {
		backend_args, _ := split_tool_backend_args(prefix_args)
		tool_args << backend_args
	}
	tool_args << command_args
	return tool_args
}

fn find_external_tool_source(base string) ?string {
	if os.is_file(base + '.v') {
		return base + '.v'
	}
	if os.is_dir(base) {
		return base
	}
	return none
}

// launch_external_tool starts a `cmd/tools/` program, reusing the binary that was compiled
// for a previous invocation whenever all of its sources are unchanged. Compiling a tool takes
// seconds, while running one usually takes milliseconds, so tools that are invoked once per
// file (`v fmt -verify`, `v vet`) are unusable without this.
fn launch_external_tool(vroot string, tool_name string, tool_source string, prefix_args []string, tool_args []string) {
	compile_args := external_tool_compile_args(tool_name, prefix_args)
	build_args := external_tool_build_args(tool_name, prefix_args)
	if !tool_cache_is_disabled() {
		vexe := os.real_path(os.executable())
		if entry := tool_cache_entry(vexe, vroot, tool_name, tool_source, build_args) {
			reason := tool_cache_stale_reason(entry)
			if reason == '' {
				if tool_cache_is_verbose() {
					eprintln('> reusing the cached `${tool_name}` at `${entry.binary}`')
				}
				exec_cached_tool(entry.binary, tool_args)
			}
			// Install what the tool needs from outside vlib first: this can make a recorded
			// `cannot import module` failure below stale, since it stamps the missing module.
			install_external_tool_modules(tool_name, tool_source, compile_args)
			if recorded := unbuildable_tool_failure(entry) {
				// Rebuilding a tool that is already known to not compile would cost seconds on
				// every single invocation, so report the recorded failure straight away instead.
				eprintln(recorded.trim_space())
				exit(1)
			}
			if tool_cache_is_verbose() {
				eprintln('> recompiling `${tool_name}`, because ${reason}')
			}
			build_tool_binary(vexe, entry) or {
				eprintln(err.msg().trim_space())
				exit(1)
			}
			exec_cached_tool(entry.binary, tool_args)
		} else {
			if tool_cache_is_verbose() {
				eprintln('> running `${tool_name}` from source, because ${err.msg()}')
			}
		}
	}
	install_external_tool_modules(tool_name, tool_source, compile_args)
	exit(run_uncached_external_tool(os.executable(), tool_name, tool_source, build_args, tool_args))
}

// run_uncached_external_tool keeps directory inputs from publishing a shared executable
// beside their sources. Wait for both children so Windows has released their image locks
// before removing the private build, including when compilation or the tool fails.
fn run_uncached_external_tool(vexe string, tool_name string, tool_source string, compiler_build_args []string, tool_args []string) int {
	stage_dir := create_tool_cache_stage_dir(os.vtmp_dir()) or {
		eprintln('cannot build `${tool_name}`: ${err.msg()}')
		return 1
	}
	defer { remove_tool_stage_dir(stage_dir) }
	binary := os.join_path(stage_dir, '${tool_name}${tool_exe_suffix()}')
	mut build_args := compiler_build_args.clone()
	build_args << ['-o', binary, tool_source]
	mut environment := os.environ()
	// The prefix already includes VFLAGS. Keep the original environment for the tool,
	// but do not prepend the flags twice or retry its build with another compiler.
	environment['VFLAGS'] = ''
	mut build := os.new_process(vexe)
	build.set_args(build_args)
	build.set_environment(environment)
	build.wait()
	build_code := build.code
	build.close()
	if build_code != 0 {
		return build_code
	}
	if !os.is_file(binary) {
		eprintln('compiler exited successfully but did not produce `${binary}`')
		return 1
	}
	$if windows {
		// `v up` must defer wiping VTMP until its executable has been closed too.
		return run_windows_cached_tool(binary, tool_args)
	}
	mut tool := os.new_process(binary)
	tool.set_args(tool_args)
	tool.wait()
	code := tool.code
	tool.close()
	return code
}

// install_external_tool_modules installs the modules from outside vlib that a tool imports,
// like `markdown` for `vdoc`, right before the tool is compiled. A fresh V installation does
// not have them yet, and compiling the tool without them fails with a confusing
// `cannot import module` error. Sandboxed packaging has no network access, and must provide
// such modules itself, just like it does for `v build-tools`.
fn install_external_tool_modules(tool_name string, tool_source string, compile_args []string) {
	if os.getenv('VTEST_SANDBOXED_PACKAGING') != '' {
		return
	}
	// A `-path` replaces the default module roots, including VMODULES, where modules are
	// installed. So a build with a `-path` is left to the compiler, which reports a module
	// that is really missing.
	if '-path' in compile_args {
		return
	}
	util.ensure_modules_for_tool_are_installed(tool_name, tool_source, tool_cache_is_verbose()) or {
		eprintln(err.msg())
		exit(1)
	}
}

// external_tool_compile_args applies launcher-only build policy to a `cmd/tools/` helper.
// Diagnostic tools used to be built this way by `util.launch_tool`: keep them GC-free so
// they can start even when libgc cannot allocate executable pages or cannot be loaded.
fn external_tool_compile_args(tool_name string, prefix_args []string) []string {
	mut compile_args := prefix_args.clone()
	if tool_name in ['vfmt', 'vself', 'vtest'] {
		// Backend/target options describe formatting rules, the replacement
		// compiler, or test programs. Build the helper for the host C backend.
		_, native_args := split_tool_backend_args(compile_args)
		compile_args = external_tool_args_without_target(native_args)
	}
	if tool_name == 'vfmt' {
		// vfmt reads `-path` from VFLAGS itself, to find the modules a file imports.
		// Building it with them only made a new build of the tool for every `-path`.
		compile_args = external_tool_args_without_path(compile_args)
	}
	if tool_name in ['vself', 'vup', 'vdoctor', 'vsymlink'] {
		compile_args = external_tool_args_without_gc(compile_args)
		if '-g' !in compile_args {
			compile_args << '-g'
		}
		compile_args << ['-gc', 'none']
	}
	if pref.tcc_with_prod_requested(compile_args) {
		compile_args = external_tool_args_without_prod(compile_args)
	}
	return compile_args
}

// external_tool_args_without_prod removes only the private helper's production option.
// Tool runtime arguments retain the caller's requested compiler configuration.
fn external_tool_args_without_prod(args []string) []string {
	mut result := []string{cap: args.len}
	mut value_follows := false
	for i, arg in args {
		if value_follows {
			result << arg
			value_follows = false
			continue
		}
		if arg == '-prod' { continue }
		result << arg
		value_follows = if arg in ['-prof', '-profile'] {
			profile_option_consumes_value(args, i)
		} else {
			arg == '-cf' || pref.option_may_consume_value(arg)
		}
	}
	return result
}

fn external_tool_args_without_target(args []string) []string {
	mut result := []string{cap: args.len}
	mut skip_target_value := false
	mut value_follows := false
	for i, arg in args {
		if skip_target_value {
			skip_target_value = false
			continue
		}
		if value_follows {
			result << arg
			value_follows = false
			continue
		}
		if arg in ['-os', '-arch'] {
			skip_target_value = true
			continue
		}
		if arg == '-cross' || arg.starts_with('-os=') || arg.starts_with('-arch=') {
			continue
		}
		result << arg
		value_follows = if arg in ['-prof', '-profile'] {
			profile_option_consumes_value(args, i)
		} else {
			arg == '-cf' || pref.option_may_consume_value(arg)
		}
	}
	return result
}

// external_tool_args_without_path removes `-path` and its value.
fn external_tool_args_without_path(args []string) []string {
	mut result := []string{cap: args.len}
	mut skip_path_value := false
	mut value_follows := false
	for i, arg in args {
		if skip_path_value {
			skip_path_value = false
			continue
		}
		if value_follows {
			result << arg
			value_follows = false
			continue
		}
		if arg == '-path' {
			skip_path_value = true
			continue
		}
		result << arg
		value_follows = if arg in ['-prof', '-profile'] {
			profile_option_consumes_value(args, i)
		} else {
			arg == '-cf' || pref.option_may_consume_value(arg)
		}
	}
	return result
}

fn split_tool_backend_args(args []string) ([]string, []string) {
	mut backend_args := []string{}
	mut other_args := []string{}
	mut backend_value_follows := false
	mut value_follows := false
	for i, arg in args {
		if value_follows {
			other_args << arg
			value_follows = false
		} else if backend_value_follows {
			backend_args << arg
			backend_value_follows = false
		} else if arg in ['-b', '-backend'] {
			backend_args << arg
			backend_value_follows = true
		} else if arg.starts_with('-b=') || arg.starts_with('-backend=') {
			backend_args << arg
		} else if arg == '-native' {
			backend_args << ['-b', 'native']
		} else {
			other_args << arg
			value_follows = if arg in ['-prof', '-profile'] {
				profile_option_consumes_value(args, i)
			} else {
				arg == '-cf' || pref.option_may_consume_value(arg)
			}
		}
	}
	return backend_args, other_args
}

fn external_tool_args_without_gc(args []string) []string {
	mut result := []string{cap: args.len}
	mut skip_gc_value := false
	for arg in args {
		if skip_gc_value {
			skip_gc_value = false
			continue
		}
		if arg == '-gc' {
			skip_gc_value = true
			continue
		}
		if arg.starts_with('-gc=') {
			continue
		}
		result << arg
	}
	return result
}

// external_tool_build_args keeps compiler options that affect a tool binary while dropping
// modes that deliberately do not produce one. Those modes still apply to the requested tool
// command, but passing `-check` to the private tool build makes the compiler exit successfully
// without creating the executable that the launcher must run.
fn external_tool_build_args(tool_name string, prefix_args []string) []string {
	return external_tool_compile_args(tool_name, prefix_args).filter(it !in ['-check', '-check-syntax',
		'-c'])
}

fn print_help(args []string, command_index int) {
	if command_index >= 0 && command_index + 1 < args.len && args[command_index + 1] == 'self' {
		println('Usage: v self [options]')
		println('Rebuild V with the passed options.')
		return
	}
	topic := if command_index >= 0 && command_index + 1 < args.len {
		args[command_index + 1]
	} else {
		'default'
	}
	help.print_and_exit(topic, exit_code: 0)
}

fn merged_v_args() []string {
	mut args := []string{}
	if vflags := os.getenv_opt('VFLAGS') {
		args << cmdexec.split_args(vflags) or {
			eprintln('invalid VFLAGS: ${err.msg()}')
			exit(1)
		}
	}
	args << os.args[1..]
	return args
}

fn is_inline_source_option(arg string) bool {
	return arg == '-e' || arg.starts_with('-e=') || arg == '-http'
}

// compiler_args_before_program excludes inline source and program arguments
// from launcher-only compiler selection and option cleanup. Inline execution
// starts a nested `run` invocation, so both boundaries must be preserved.
fn compiler_args_before_program(args []string) []string {
	mut option_value_follows := false
	mut runs_input := false
	for i, arg in args {
		if option_value_follows {
			option_value_follows = false
			continue
		}
		if is_inline_source_option(arg) {
			return args[..i]
		}
		if arg in ['-prof', '-profile'] {
			option_value_follows = profile_option_consumes_value(args, i)
			continue
		}
		if arg == '-raw-vsh-tmp-prefix' {
			runs_input = true
		}
		if arg == '-cf' || pref.option_may_consume_value(arg) {
			option_value_follows = true
			continue
		}
		if !runs_input && arg in ['run', 'crun'] {
			runs_input = true
			continue
		}
		if !arg.starts_with('-') || arg == '-' {
			if runs_input || arg.ends_with('.vsh') || arg.ends_with('.wasm') {
				return args[..i + 1]
			}
			break
		}
	}
	return args
}

fn selected_backend(args []string) string {
	mut backend := 'c'
	mut target_arch := pref.host_arch()
	mut backend_value_follows := false
	mut arch_value_follows := false
	mut option_value_follows := false
	for i, arg in args {
		if backend_value_follows {
			backend = arg
			backend_value_follows = false
			continue
		}
		if arch_value_follows {
			target_arch = arg
			arch_value_follows = false
			continue
		}
		if option_value_follows {
			option_value_follows = false
			continue
		}
		if arg == '-arch' {
			arch_value_follows = true
			continue
		}
		if arg == '-native' {
			backend = 'native'
			continue
		}
		if arg in ['-b', '-backend'] {
			backend_value_follows = true
			continue
		}
		for option in ['-b=', '-backend='] {
			if arg.starts_with(option) {
				backend = arg.all_after(option)
				break
			}
		}
		if arg in ['-prof', '-profile'] {
			option_value_follows = profile_option_consumes_value(args, i)
			continue
		}
		if arg == '-cf' || pref.option_may_consume_value(arg) {
			option_value_follows = true
		}
	}
	return match backend {
		'js_browser', 'js_node' { 'js' }
		'native' { pref.native_backend_for_arch(target_arch) }
		else { backend }
	}
}

// profile_option_consumes_value mirrors the driver's optional profile output argument.
fn profile_option_consumes_value(args []string, idx int) bool {
	next := args[idx + 1] or { return false }
	if next == '-' {
		return true
	}
	if next.starts_with('-') {
		return false
	}
	if next in ['run', 'build', 'test', 'doc'] || next.ends_with('.v')
		|| next.ends_with('.vv') || next.ends_with('.vsh') || os.is_dir(next) {
		return false
	}
	for later in args[idx + 2..] {
		if !later.starts_with('-') {
			return true
		}
	}
	return false
}

fn find_vroot(executable string) ?string {
	mut current := if os.is_dir(executable) {
		os.real_path(executable)
	} else {
		os.real_path(os.dir(executable))
	}
	for _ in 0 .. 32 {
		if os.is_file(os.join_path(current, 'GNUmakefile'))
			&& os.is_dir(os.join_path(current, 'vlib', 'v')) {
			return current
		}
		parent := os.dir(current)
		if parent == current {
			break
		}
		current = parent
	}
	return none
}
