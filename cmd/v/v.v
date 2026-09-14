// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import strings
import v.cmdexec
import v.driver
import v.help
import v.pref

const v_version = '0.5.2'
const v1_fallback_binary = 'v1_fallback'
// Modules that V3 ships at a new place inside vlib, mapped to where the V 0.5.2
// tree behind the fallback compiler still keeps them. A program written for V3
// fails there with `cannot import module ...` until the fallback is pointed at a
// copy that carries them under their current name.
const v1_fallback_module_shims = {
	'json2': 'x/json2'
}
const v3_fallback_file_env = 'V_MACOS_V3_FALLBACK_FILE'
const v3_c_error_dir_env = 'V_MACOS_V3_C_ERROR_DIR'
const v3_no_fallback_env = 'V_MACOS_V3_NO_FALLBACK'
const v3_retry_env = 'V_MACOS_V3_RETRY'

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
	'init',
	'install',
	'link',
	'list',
	'ls',
	'missdoc',
	'new',
	'outdated',
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
	'should-compile-all',
	'show',
	'sqlite',
	'symlink',
	'scan',
	'test-all',
	'test-cleancode',
	'test-fmt',
	'test-parser',
	'test-self',
	'time',
	'timeout',
	'tracev',
	'translate',
	'unlink',
	'up',
	'update',
	'upgrade',
	'vet',
	'vlib-docs',
	'watch',
	'where',
	'wipe-cache',
]

struct RetryState {
	fallback_file string
	c_error_dir   string
	args          []string
}

@[unsafe]
fn retry_state(value &RetryState) &RetryState {
	mut static state := unsafe { &RetryState(nil) }
	if value != unsafe { nil } {
		state = value
	}
	return state
}

fn main() {
	os.setenv('VEXE', os.real_path(os.executable()), true)
	os.setenv('VCHILD', 'true', true)
	mut args := merged_v_args()
	if args.len == 0 {
		run_external_tool(args, -1, 'repl')
		return
	}
	command_index, command := find_command(args)
	if command in ['version', '-version', '--version'] {
		println('V ${v_version} ${@VCURRENTHASH}')
		return
	}
	if '-old-compiler' in args {
		launch_v1(clean_compiler_selection_flags(args), '`-old-compiler` was requested', RetryState{})
	}
	if '-new-compiler' in args {
		os.setenv(v3_no_fallback_env, '1', true)
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
	if command in external_commands
		|| command in ['new', 'init', 'install', 'link', 'list', 'outdated', 'remove', 'search',
			'show', 'unlink', 'update', 'upgrade', 'vlib-docs'] {
		run_external_tool(args, command_index, command)
		return
	}
	args = clean_compiler_selection_flags(args)
	run_with_fallback(args, args)
}

// fallback_is_disabled reports whether retrying with the compatibility compiler was turned off.
fn fallback_is_disabled() bool {
	return os.getenv(v3_no_fallback_env) == '1' || os.getenv(v3_retry_env) == '1'
}

fn run_with_fallback(driver_args []string, retry_args []string) {
	if fallback_is_disabled() {
		driver.run(driver_args)
		return
	}
	fallback_file := os.join_path(os.vtmp_dir(), 'v3_fallback_${os.getpid()}')
	c_error_dir := fallback_file + '.c_error'
	os.rm(fallback_file) or {}
	os.rmdir_all(c_error_dir) or {}
	os.setenv(v3_fallback_file_env, fallback_file, true)
	os.setenv(v3_c_error_dir_env, c_error_dir, true)
	state := &RetryState{
		fallback_file: fallback_file
		c_error_dir:   c_error_dir
		args:          retry_args.clone()
	}
	unsafe { retry_state(state) }
	at_exit(retry_with_v1_at_exit) or {
		eprintln('cannot register the V compatibility fallback: ${err}')
		exit(1)
	}
	driver.run(driver_args)
	os.rm(fallback_file) or {}
	os.rmdir_all(c_error_dir) or {}
}

fn find_command(args []string) (int, string) {
	mut option_value_follows := false
	for i, arg in args {
		if option_value_follows {
			option_value_follows = false
			continue
		}
		if arg == '-cf' || pref.option_may_consume_value(arg) {
			option_value_follows = true
			continue
		}
		if arg in external_commands
			|| arg in ['version', '-version', '--version', 'help', '-h', '--help', 'get', 'interpret',
				'new', 'init', 'install', 'link', 'list', 'outdated', 'remove', 'search', 'show',
				'unlink', 'update', 'upgrade', 'vlib-docs'] {
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
		'install', 'link', 'list', 'outdated', 'remove', 'search', 'show', 'unlink', 'update', 'upgrade' {
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
	tool_source := if os.is_dir(base) {
		base
	} else if os.is_file(base + '.v') {
		base + '.v'
	} else {
		eprintln('cannot find the `${command}` tool source in `${vroot}`')
		exit(1)
	}
	mut prefix_args := []string{}
	if command_index > 0 {
		prefix_args << args[..command_index]
	}
	mut tool_args := []string{}
	if command_index >= 0 {
		tool_args << args[command_index..]
	}
	launch_external_tool(vroot, tool_name, tool_source, prefix_args, tool_args, args)
}

// launch_external_tool starts a `cmd/tools/` program, reusing the binary that was compiled
// for a previous invocation whenever all of its sources are unchanged. Compiling a tool takes
// seconds, while running one usually takes milliseconds, so tools that are invoked once per
// file (`v fmt -verify`, `v vet`) are unusable without this.
fn launch_external_tool(vroot string, tool_name string, tool_source string, prefix_args []string, tool_args []string, args []string) {
	retry_args := clean_compiler_selection_flags(args)
	if !tool_cache_is_disabled() {
		vexe := os.real_path(os.executable())
		build_args := clean_compiler_selection_flags(prefix_args)
		if entry := tool_cache_entry(vexe, vroot, tool_name, tool_source, build_args) {
			reason := tool_cache_stale_reason(entry)
			if reason == '' {
				if tool_cache_is_verbose() {
					eprintln('> reusing the cached `${tool_name}` at `${entry.binary}`')
				}
				exec_cached_tool(entry.binary, tool_args)
			}
			if recorded := unbuildable_tool_failure(entry) {
				// Rebuilding a tool that is already known to not compile would cost seconds on
				// every single invocation, so report the recorded failure straight away instead.
				if fallback_is_disabled() {
					eprintln(recorded.trim_space())
					exit(1)
				}
				launch_v1(retry_args, unbuildable_tool_reason(tool_name, entry), RetryState{})
			}
			if tool_cache_is_verbose() {
				eprintln('> recompiling `${tool_name}`, because ${reason}')
			}
			build_tool_binary(vexe, entry) or {
				eprintln(err.msg().trim_space())
				if fallback_is_disabled() {
					exit(1)
				}
				launch_v1(retry_args, unbuildable_tool_reason(tool_name, entry), RetryState{})
			}
			exec_cached_tool(entry.binary, tool_args)
		}
	}
	mut driver_args := []string{}
	driver_args << prefix_args
	driver_args << ['run', tool_source]
	driver_args << tool_args
	run_with_fallback(clean_compiler_selection_flags(driver_args), retry_args)
}

// unbuildable_tool_reason explains why a tool has to run on the compatibility compiler.
fn unbuildable_tool_reason(tool_name string, entry ToolCacheEntry) string {
	return 'the V compiler cannot build `cmd/tools/${tool_name}` (recorded in `${entry.unbuildable}`)'
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

fn clean_compiler_selection_flags(args []string) []string {
	return args.filter(it !in ['-old-compiler', '-new-compiler'])
}

fn retry_with_v1_at_exit() {
	state := unsafe { retry_state(nil) }
	if state == unsafe { nil } || !os.is_file(state.fallback_file) {
		return
	}
	payload := os.read_file(state.fallback_file) or { return }
	reason := payload.all_before('\n').trim_space()
	if reason !in ['compiler_error', 'c_compilation_error', 'inline_asm'] {
		return
	}
	os.setenv(v3_retry_env, '1', true)
	launch_v1(state.args, 'V compilation failed (${reason})', *state)
}

@[noreturn]
fn launch_v1(args []string, reason string, report_state RetryState) {
	fallback := ensure_v1_fallback(reason) or {
		eprintln(err.msg())
		exit(1)
	}
	os.setenv('VEXE', fallback, true)
	os.setenv('VCHILD', 'true', true)
	if overlay := v1_fallback_module_overlay(os.dir(fallback)) {
		os.setenv('VMODULES', v1_fallback_vmodules_env(overlay), true)
	}
	eprintln('${reason}; retrying with `${fallback}`.')
	os.unsetenv(v3_fallback_file_env)
	os.unsetenv(v3_c_error_dir_env)
	mut process := os.new_process(fallback)
	process.set_args(args)
	process.wait()
	if process.status == .aborted || process.code < 0 {
		eprintln('failed to launch the V 0.5.2 compatibility compiler `${fallback}`: ${process.err}')
		process.close()
		exit(1)
	}
	code := process.code
	process.close()
	if code == 0 && report_state.fallback_file != '' {
		submit_v3_fallback_report(fallback, report_state)
	}
	if code != 0 && v1_fallback_exit_identifies_compiler_failure(args) {
		report_v1_fallback_failure(report_state)
	}
	os.rm(report_state.fallback_file) or {}
	os.rmdir_all(report_state.c_error_dir) or {}
	exit(code)
}

// v1_fallback_exit_identifies_compiler_failure reports whether a nonzero exit
// can only have come from the compatibility compiler. Commands that run a
// program, tests, or an external tool can return their child's status after a
// successful compilation, so their nonzero exits are ambiguous and must not be
// described as compiler failures.
fn v1_fallback_exit_identifies_compiler_failure(args []string) bool {
	mut option_value_follows := false
	for arg in args {
		if option_value_follows {
			option_value_follows = false
			continue
		}
		if arg == '-e' || arg.starts_with('-e=') || arg == '-' {
			return false
		}
		if arg == '-cf' || pref.option_may_consume_value(arg) {
			option_value_follows = true
			continue
		}
		if arg in external_commands || arg in ['run', 'crun', 'test'] {
			return false
		}
		if !arg.starts_with('-') {
			return !arg.ends_with('_test.v') && !arg.ends_with('_test.vv')
				&& !arg.ends_with('.vsh')
		}
	}
	return true
}

// report_v1_fallback_failure explains whose errors the user is looking at. V
// keeps its own diagnostics quiet while a fallback is pending, so when the retry
// fails too, everything on screen comes from the compatibility compiler, which is
// confusing whenever the two disagree about the program.
fn report_v1_fallback_failure(state RetryState) {
	if state.fallback_file == '' {
		return
	}
	payload := os.read_file(state.fallback_file) or { return }
	for note in v1_fallback_failure_notes(payload) {
		eprintln(note)
	}
}

// v1_fallback_failure_notes turns a staged fallback payload into the notes shown
// after a failed retry.
fn v1_fallback_failure_notes(payload string) []string {
	// The stage is only recorded when the payload carries a second line.
	stage := if payload.contains('\n') { payload.all_after('\n').trim_space() } else { '' }
	stopped_in := if stage == '' { '' } else { ' during ${stage}' }
	return [
		'note: the V ${v_version} compatibility compiler failed too, so the errors above are its own.',
		'note: V stopped${stopped_in} and kept its diagnostics quiet for this retry; re-run with `-new-compiler` to see them.',
	]
}

fn submit_v3_fallback_report(fallback string, state RetryState) {
	payload := os.read_file(state.fallback_file) or { return }
	kind := payload.all_before('\n').trim_space()
	if kind == 'inline_asm'
		|| os.getenv('V_C_ERROR_BUG_REPORT_DISABLED').trim_space().to_lower() in ['1', 'true', 'yes',
			'on'] {
		return
	}
	custom_url := os.getenv('V_C_ERROR_BUG_REPORT_URL').trim_space().trim_right('/')
	if custom_url == '' && (os.getenv('GITHUB_ACTIONS') == 'true' || os.getenv('GITHUB_JOB') != '') {
		return
	}
	report_url := if custom_url == '' { 'https://bugs.vlang.io/bug-report' } else { custom_url }
	stage := payload.all_after('\n').trim_space()
	mut ccompiler := stage
	mut c_output := ''
	mut report_kind := 'v3-compiler-error'
	if kind == 'c_compilation_error' {
		report_kind = 'v-c-compiler-error'
		ccompiler = os.read_file(os.join_path(state.c_error_dir, 'compiler')) or { '' }
		c_output = os.read_file(os.join_path(state.c_error_dir, 'output')) or { '' }
		if c_output.len > 64 * 1024 {
			c_output = c_output[..64 * 1024] + '\n... report truncated before upload ...\n'
		}
	}
	report_file := os.join_path(os.vtmp_dir(), 'v3-fallback-report-${os.getpid()}.json')
	report := '{"kind":${json_quote(report_kind)},"v_version":${json_quote('V ${v_version} ${@VCURRENTHASH}')},"target_os":${json_quote(os.user_os())},"target_backend":"c","arch":${json_quote(@PLATFORM)},"ccompiler":${json_quote(ccompiler)},"build_options":${json_quote(state.args.join(' '))},"c_error":${json_quote(c_output)},"c_file":"","c_line":0,"c_context":[],"v_file":"","v_line":0,"v_context":[],"v_source":""}'
	os.write_file(report_file, report) or {
		eprintln('V3 compiler bug report was not staged: ${err}')
		return
	}
	defer {
		os.rm(report_file) or {}
	}
	mut sender := os.new_process(fallback)
	sender.set_args(['bug-report-send', '--url', report_url, '--file', report_file])
	sender.set_redirect_stdio_merged()
	sender.wait()
	code := sender.code
	output := sender.stdout_slurp().trim_space()
	err := sender.err
	sender.close()
	if code != 0 {
		details := if output == '' { err } else { output }
		eprintln('V3 compiler bug report was not sent to ${report_url}: ${details}')
		return
	}
	eprintln('note: V could not build this program with the default compiler, so it used V 0.5.2 instead.')
	eprintln('A metadata-only bug report (no source) was submitted to ${report_url} so this can be fixed.')
	if output != '' {
		eprintln(output)
	}
	eprintln('Set V_C_ERROR_BUG_REPORT_DISABLED=1 to opt out of these automatic reports.')
}

fn json_quote(value string) string {
	mut out := strings.new_builder(value.len + 2)
	out.write_u8(`\"`)
	for byte in value.bytes() {
		match byte {
			`\"` {
				out.write_string('\\"')
			}
			`\\` {
				out.write_string('\\\\')
			}
			`\b` {
				out.write_string('\\b')
			}
			`\f` {
				out.write_string('\\f')
			}
			`\n` {
				out.write_string('\\n')
			}
			`\r` {
				out.write_string('\\r')
			}
			`\t` {
				out.write_string('\\t')
			}
			else {
				out.write_u8(if byte < 0x20 { ` ` } else { byte })
			}
		}
	}
	out.write_u8(`\"`)
	return out.str()
}

fn ensure_v1_fallback(reason string) !string {
	vroot := find_vroot(os.executable()) or {
		find_vroot(@VEXEROOT) or {
			return error('${reason}, but the V source tree could not be found. Run `make v1` in the V source directory.')
		}
	}
	fallback := os.join_path(vroot, v1_fallback_binary + $if windows { '.exe' } $else { '' })
	if installed := resolve_v1_fallback(fallback) {
		return installed
	} else {
		make_command := find_make() or {
			return error('${reason}, but `${fallback}` is missing and make was not found. Install make, then run `make v1` in `${vroot}`.')
		}
		eprintln('${reason}, but `${fallback}` is missing; running `make v1` now...')
		mut process := os.new_process(make_command)
		process.set_args(['VEXE=${os.real_path(os.executable())}', 'v1'])
		process.set_work_folder(vroot)
		process.wait()
		code := process.code
		process.close()
		if code != 0 {
			return error('`make v1` failed with exit code ${code}. Run it manually in `${vroot}` for more details.')
		}
	}
	return resolve_v1_fallback(fallback) or {
		return error('`make v1` completed without installing a usable V ${v_version} fallback at `${fallback}`.')
	}
}

fn resolve_v1_fallback(fallback string) ?string {
	if !os.is_executable(fallback) {
		return none
	}
	root_file := fallback + '.vroot'
	if os.is_file(root_file) {
		fallback_root := os.read_file(root_file) or { '' }.trim_space()
		cached_fallback := os.join_path(fallback_root, 'v' + $if windows { '.exe' } $else { '' })
		if os.is_executable(cached_fallback) && v1_fallback_has_expected_version(cached_fallback)
			&& v1_fallback_has_crypto_subtle(fallback_root) {
			return cached_fallback
		}
	}
	return none
}

// v1_fallback_module_overlay stages every module of `v1_fallback_module_shims`
// that `fallback_root` only carries under its previous name, and returns the
// directory to append to VMODULES. The fallback tree is only ever read from: it
// can be shared between users or read only, as a system packaged V 0.5.2 is.
// Without this a retried build of any program that imports `json2` stops on the
// fallback with a module-not-found error that has nothing to do with why V3
// gave up.
fn v1_fallback_module_overlay(fallback_root string) ?string {
	return v1_fallback_module_overlay_in(fallback_root, v1_fallback_overlay_dirs())
}

// v1_fallback_module_overlay_in stages the modules into the first of
// `candidates` that accepts them.
fn v1_fallback_module_overlay_in(fallback_root string, candidates []string) ?string {
	vlib_dir := os.join_path(fallback_root, 'vlib')
	mut sources := map[string]string{}
	for name, previous_path in v1_fallback_module_shims {
		if os.is_dir(os.join_path(vlib_dir, name)) {
			// This tree is new enough to carry the module under its current name.
			continue
		}
		mut source := vlib_dir
		for segment in previous_path.split('/') {
			source = os.join_path(source, segment)
		}
		if os.is_dir(source) {
			sources[name] = source
		}
	}
	if sources.len == 0 {
		return none
	}
	mut failure := ''
	for overlay in candidates {
		mut staged_all := true
		for name, source in sources {
			stage_v1_fallback_module(overlay, name, source) or {
				if failure == '' {
					failure = '`${overlay}`: ${err.msg()}'
				}
				staged_all = false
				break
			}
		}
		if staged_all {
			// VMODULES is read by a process with its own working directory, so
			// only an absolute path means the same thing there.
			return os.abs_path(overlay)
		}
	}
	// Continuing without the overlay is still better than refusing to run the
	// fallback at all, since only a program that imports one of these modules is
	// affected. Say so, rather than letting it fail as a missing module.
	reason := if failure == '' { 'there was nowhere to put them' } else { failure }
	eprintln('note: the V ${v_version} fallback will not be able to import `${sources.keys().join('`, `')}`, because none of its copies could be staged (${reason}).')
	return none
}

// stage_v1_fallback_module copies `source` to `<overlay>/<name>` unless it is
// already there. The copy is staged beside its destination and renamed into
// place, so that a concurrent `v` never sees a half written directory as an
// importable module.
fn stage_v1_fallback_module(overlay string, name string, source string) ! {
	shim := os.join_path(overlay, name)
	if os.is_dir(shim) {
		return
	}
	os.mkdir_all(overlay)!
	staged := os.join_path(overlay, '.${name}.staged.${os.getpid()}')
	os.rmdir_all(staged) or {}
	os.cp_all(source, staged, true) or {
		os.rmdir_all(staged) or {}
		return err
	}
	os.mv(staged, shim, overwrite: false) or {
		os.rmdir_all(staged) or {}
		if !os.is_dir(shim) {
			return err
		}
		// A concurrent `v` staged the same module first, which is the same result.
	}
}

// v1_fallback_overlay_dirs lists the overlay locations to try, most durable
// first. `os.cache_dir()` and `os.vtmp_dir()` are deliberately not used: both
// panic when they cannot create their folder, which is the read only HOME this
// has to survive.
fn v1_fallback_overlay_dirs() []string {
	// `v/` under the cache root is where `install_v1_fallback.sh` puts the
	// fallback itself, so its modules stay in the same namespace.
	mut roots := []string{}
	if xdg_cache := os.getenv_opt('XDG_CACHE_HOME') {
		if xdg_cache != '' {
			roots << os.join_path(xdg_cache, 'v')
		}
	}
	home := os.home_dir()
	if home != '' {
		roots << os.join_path(home, '.cache', 'v')
	}
	roots << os.join_path(os.temp_dir(), 'v_${os.getuid()}')
	mut dirs := []string{cap: roots.len}
	for root in roots {
		dir := os.join_path(root, 'v1-fallback-modules', v_version)
		if dir !in dirs {
			dirs << dir
		}
	}
	return dirs
}

// v1_fallback_vmodules_env appends `overlay` to the module paths the fallback
// searches. It goes last, so a module the user installed themselves still wins,
// and vlib already wins over every vmodules path.
fn v1_fallback_vmodules_env(overlay string) string {
	mut paths := os.vmodules_paths()
	if overlay !in paths {
		paths << overlay
	}
	return paths.join(os.path_delimiter)
}

fn v1_fallback_has_crypto_subtle(root string) bool {
	module_dir := os.join_path(root, 'vlib', 'crypto', 'subtle')
	return os.is_file(os.join_path(module_dir, 'aliasing.v'))
		&& os.is_file(os.join_path(module_dir, 'comparison.v'))
}

fn v1_fallback_has_expected_version(executable string) bool {
	result := os.execute('${os.quoted_path(executable)} version')
	return result.exit_code == 0 && result.output.starts_with('V ${v_version} ')
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

fn find_make() ?string {
	for name in ['make', 'gmake'] {
		if executable := os.find_abs_path_of_executable(name) {
			return executable
		}
	}
	return none
}
