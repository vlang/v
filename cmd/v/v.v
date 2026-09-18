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
// Modules that the fallback installer copies to their current public paths.
// Cached fallback trees are not used until they carry every listed module.
const v1_fallback_compatibility_modules = ['json2']
const v1_fallback_compatibility_marker = '.v1-fallback-complete'
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
	'test',
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
	if ownership_compiler_is_required(args) && !ownership_checker_is_compiled() {
		launch_ownership_compiler(args)
	}
	run_with_fallback(args, args)
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
	compiler_source := os.join_path(vroot, 'cmd', 'v')
	// A regular V3 compiler is deliberately allowed to create the ownership-enabled
	// executable. Do not recursively dispatch that bootstrap compilation to itself.
	if args.any(os.exists(it) && os.real_path(it) == os.real_path(compiler_source)) {
		driver.run(args)
		exit(0)
	}
	entry := tool_cache_entry(vexe, vroot, 'v3_ownership', compiler_source, ['-d', 'ownership',
		'-gc', 'none']) or {
		eprintln('cannot find a writable cache for the V3 ownership compiler')
		exit(1)
	}
	reason := tool_cache_stale_reason(entry)
	if reason != '' {
		if tool_cache_is_verbose() {
			eprintln('> recompiling `v3_ownership`, because ${reason}')
		}
		build_tool_binary(vexe, entry) or {
			eprintln('cannot build the V3 ownership compiler:\n${err.msg().trim_space()}')
			exit(1)
		}
	}
	exec_cached_tool(entry.binary, args)
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
		'install', 'link', 'list', 'outdated', 'remove', 'search', 'show', 'unlink', 'update',
		'upgrade' {
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
	launch_external_tool(vroot, tool_name, tool_source, prefix_args, tool_args)
}

fn external_tool_runtime_args(command string, prefix_args []string, command_args []string) []string {
	mut tool_args := []string{}
	// `v build-tools` consumes compiler options itself and applies them to every
	// tool in its inventory. `v self` likewise treats prefix compiler options as
	// options for the replacement compiler, not just for the launcher helper.
	// `v test` needs them for each test compilation and its failure reproduction command.
	// Keep those options visible after the launcher has built the cached executable.
	if command in ['build-tools', 'self', 'test'] {
		tool_args << prefix_args
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
	if !tool_cache_is_disabled() {
		vexe := os.real_path(os.executable())
		build_args := external_tool_build_args(tool_name, prefix_args)
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
		}
	}
	mut driver_args := []string{}
	driver_args << compile_args
	driver_args << ['run', tool_source]
	driver_args << tool_args
	driver.run(driver_args)
}

// external_tool_compile_args applies launcher-only build policy to a `cmd/tools/` helper.
// Diagnostic tools used to be built this way by `util.launch_tool`: keep them GC-free so
// they can start even when libgc cannot allocate executable pages or cannot be loaded.
fn external_tool_compile_args(tool_name string, prefix_args []string) []string {
	mut compile_args := clean_compiler_selection_flags(prefix_args)
	if tool_name in ['vself', 'vup', 'vdoctor', 'vsymlink'] {
		compile_args = external_tool_args_without_gc(compile_args)
		if '-g' !in compile_args {
			compile_args << '-g'
		}
		compile_args << ['-gc', 'none']
	}
	return compile_args
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
// command, but passing `-check` to the private cache build makes the compiler exit successfully
// without creating the executable that the launcher must run.
fn external_tool_build_args(tool_name string, prefix_args []string) []string {
	return external_tool_compile_args(tool_name, prefix_args).filter(it !in ['-check', '-c'])
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
	c_diagnostics := v3_c_error_diagnostics(report_state)
	if c_diagnostics != '' {
		eprint(c_diagnostics)
	}
	fallback := ensure_v1_fallback(reason) or {
		report_v3_fallback_unavailable(args, reason, report_state, err.msg(), c_diagnostics != '')
		exit(1)
	}
	os.setenv('VEXE', fallback, true)
	os.setenv('VCHILD', 'true', true)
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
	// These notes describe diagnostics that were suppressed, not C errors printed above.
	if code != 0 && c_diagnostics == '' {
		report_v1_fallback_exit(report_state, v1_fallback_exit_identifies_compiler_failure(args))
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
	compiler_args := args[..v1_fallback_compiler_prefix_len(args)]
	backend := v1_fallback_selected_backend(compiler_args)
	mut skip_running := os.getenv('VNORUN') != ''
	mut direct_test := false
	mut option_value_follows := false
	for i, arg in args {
		if option_value_follows {
			option_value_follows = false
			continue
		}
		if arg == '-e' || arg.starts_with('-e=') || arg == '-' {
			return false
		}
		if arg in ['-prof', '-profile'] {
			option_value_follows = v1_fallback_profile_option_consumes_value(args, i)
			continue
		}
		if arg in ['-skip-running', '-check', '-check-syntax', '-generate-c-project'] {
			skip_running = true
		}
		if arg in ['-o', '-output'] {
			output := args[i + 1] or { '' }
			if output == '-' || (backend == 'c' && output.ends_with('.c')) {
				skip_running = true
			}
			option_value_follows = true
			continue
		}
		if arg == '-cf' || pref.option_may_consume_value(arg) {
			option_value_follows = true
			continue
		}
		if direct_test && !arg.starts_with('-') {
			return true
		}
		if arg in external_commands || arg == 'test' {
			return false
		}
		if arg in ['run', 'crun'] {
			return skip_running
		}
		if !arg.starts_with('-') {
			is_test := pref.is_test_file_for_backend(arg, backend) || arg.ends_with('_test.vv')
			if is_test {
				direct_test = true
				continue
			}
			return skip_running || !arg.ends_with('.vsh')
		}
	}
	if direct_test {
		// The retry runs under V 0.5.2, which executes direct tests even when an
		// explicit executable output is requested. Model the retry, not V3 here.
		return skip_running
	}
	return true
}

fn v1_fallback_compiler_prefix_len(args []string) int {
	mut option_value_follows := false
	for i, arg in args {
		if option_value_follows {
			option_value_follows = false
			continue
		}
		if arg in ['-prof', '-profile'] {
			option_value_follows = v1_fallback_profile_option_consumes_value(args, i)
			continue
		}
		if arg == '-cf' || pref.option_may_consume_value(arg) {
			option_value_follows = true
			continue
		}
		if arg == '-' || arg in external_commands || arg in ['test', 'run', 'crun']
			|| arg.ends_with('.vsh') {
			return i
		}
	}
	return args.len
}

fn v1_fallback_selected_backend(args []string) string {
	mut backend := 'c'
	mut backend_value_follows := false
	mut option_value_follows := false
	for i, arg in args {
		if backend_value_follows {
			backend = arg
			backend_value_follows = false
			continue
		}
		if option_value_follows {
			option_value_follows = false
			continue
		}
		if arg in ['-b', '-backend', '-compile-backend', '--compile-backend'] {
			backend_value_follows = true
			continue
		}
		for option in ['-b=', '-backend=', '-compile-backend=', '--compile-backend='] {
			if arg.starts_with(option) {
				backend = arg.all_after(option)
				break
			}
		}
		if arg in ['-prof', '-profile'] {
			option_value_follows = v1_fallback_profile_option_consumes_value(args, i)
			continue
		}
		if arg == '-cf' || pref.option_may_consume_value(arg) {
			option_value_follows = true
		}
	}
	return if backend in ['js_browser', 'js_node'] { 'js' } else { backend }
}

// v1_fallback_profile_option_consumes_value mirrors the driver's compatibility
// rule for V1's optional `-profile [file]` argument.
fn v1_fallback_profile_option_consumes_value(args []string, idx int) bool {
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

// report_v1_fallback_exit explains why V3's diagnostics are absent after an
// unsuccessful compatibility retry. A run-like command may have compiled and
// returned its program's status, so only identify compiler output when the
// command cannot have run user code.
fn report_v1_fallback_exit(state RetryState, compiler_failure bool) {
	if state.fallback_file == '' {
		return
	}
	payload := os.read_file(state.fallback_file) or { return }
	for note in v1_fallback_exit_notes(payload, compiler_failure) {
		eprintln(note)
	}
}

// v1_fallback_exit_notes turns a staged fallback payload into the notes shown
// after an unsuccessful retry.
fn v1_fallback_exit_notes(payload string, compiler_failure bool) []string {
	// The stage is only recorded when the payload carries a second line.
	stage := if payload.contains('\n') { payload.all_after('\n').trim_space() } else { '' }
	stopped_in := if stage == '' { '' } else { ' during ${stage}' }
	fallback_note := if compiler_failure {
		'note: the V ${v_version} compatibility compiler failed too, so the errors above are its own.'
	} else {
		'note: the V ${v_version} compatibility retry exited unsuccessfully, so any errors above are its own; the exit status may instead come from the program.'
	}
	return [
		fallback_note,
		'note: V stopped${stopped_in} and kept its diagnostics quiet for this retry; re-run with `-new-compiler` to see them.',
	]
}

fn submit_v3_fallback_report(fallback string, state RetryState) {
	payload := os.read_file(state.fallback_file) or { return }
	kind := payload.all_before('\n').trim_space()
	if kind == 'inline_asm'
		|| os.getenv('V_C_ERROR_BUG_REPORT_DISABLED').trim_space().to_lower() in ['1', 'true',
			'yes', 'on'] {
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
	}
	cache_parent := v1_fallback_cache_parent()!
	cached_launcher := v1_fallback_cached_launcher(cache_parent)
	if installed := resolve_v1_fallback(cached_launcher) {
		return installed
	} else {
		make_command := find_make() or {
			return error('${reason}, but no usable V ${v_version} fallback was found and make is unavailable. Install make, then run `make v1` in `${vroot}`.')
		}
		eprintln('${reason}, but no usable V ${v_version} fallback was found; running `make v1` now...')
		mut process := os.new_process(make_command)
		process.set_args(['v1'])
		process.set_environment(v1_fallback_make_environment(os.real_path(os.executable()), cache_parent, cached_launcher))
		process.set_work_folder(vroot)
		process.wait()
		code := process.code
		process.close()
		if code != 0 {
			return error('`make v1` failed with exit code ${code}. Run it manually in `${vroot}` for more details.')
		}
	}
	return resolve_installed_v1_fallback(fallback, cached_launcher) or {
		return error('`make v1` completed without installing a usable V ${v_version} fallback at `${cached_launcher}`.')
	}
}

fn v1_fallback_make_environment(bootstrap string, cache_parent string, output string) map[string]string {
	mut environment := os.environ()
	// Keep path data out of make variable syntax and shell command text. The
	// installer reads these inherited values directly without re-evaluating them.
	environment['VEXE'] = './v'
	environment['V1_FALLBACK_BOOTSTRAP'] = bootstrap
	environment['V1_FALLBACK_CACHE_DIR'] = cache_parent
	environment['V1_FALLBACK_OUTPUT'] = output
	return environment
}

fn v1_fallback_cache_parent() !string {
	configured := os.getenv('V1_FALLBACK_CACHE_DIR')
	if configured != '' {
		return os.abs_path(configured)
	}
	xdg := os.getenv('XDG_CACHE_HOME')
	if xdg != '' {
		return os.abs_path(os.join_path(xdg, 'v', 'v1-fallback'))
	}
	home := os.getenv('HOME')
	if home != '' {
		return os.abs_path(os.join_path(home, '.cache', 'v', 'v1-fallback'))
	}
	return v1_fallback_private_temp_cache_parent(os.temp_dir())
}

fn v1_fallback_private_temp_cache_parent(temp_root string) !string {
	$if windows {
		return v1_fallback_private_windows_temp_cache_parent(temp_root)
	} $else {
		root_attributes := os.stat(temp_root) or {
			return error('could not inspect the temporary directory `${temp_root}`: ${err}')
		}
		if !v1_fallback_temp_root_owner_is_trusted(root_attributes.uid, u32(os.geteuid())) {
			return error('temporary directory `${temp_root}` is not owned by the current user or root')
		}
		if root_attributes.mode & 0o022 != 0 && root_attributes.mode & os.s_isvtx == 0 {
			return error('temporary directory `${temp_root}` is writable by other users without the sticky bit')
		}
		candidate := os.join_path(temp_root, 'v1-fallback-cache-${os.geteuid()}')
		os.mkdir(candidate, mode: 0o700) or {}
		attributes := os.lstat(candidate) or {
			return error('could not create a private V1 fallback cache at `${candidate}`: ${err}')
		}
		if os.is_link(candidate) || attributes.get_filetype() != .directory {
			return error('refusing unsafe V1 fallback cache path `${candidate}`: expected a real directory')
		}
		if attributes.uid != u32(os.geteuid()) || attributes.get_mode().bitmask() != 0o700 {
			return error('refusing unsafe V1 fallback cache path `${candidate}`: expected user-owned mode 0700')
		}
		return candidate
	}
}

fn v1_fallback_temp_root_owner_is_trusted(owner u32, effective_user u32) bool {
	return owner == 0 || owner == effective_user
}

fn v1_fallback_cached_launcher(cache_parent string) string {
	return os.join_path(cache_parent, v_version, v1_fallback_binary + $if windows { '.exe' } $else { '' })
}

fn resolve_installed_v1_fallback(fallback string, cached_launcher string) ?string {
	if installed := resolve_v1_fallback(fallback) {
		return installed
	}
	if cached_launcher != fallback {
		return resolve_v1_fallback(cached_launcher)
	}
	return none
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
			&& v1_fallback_has_crypto_subtle(fallback_root)
			&& v1_fallback_has_moved_modules(fallback_root) {
			return cached_fallback
		}
	}
	return none
}

fn v1_fallback_has_crypto_subtle(root string) bool {
	module_dir := os.join_path(root, 'vlib', 'crypto', 'subtle')
	return os.is_file(os.join_path(module_dir, 'aliasing.v'))
		&& os.is_file(os.join_path(module_dir, 'comparison.v'))
}

fn v1_fallback_has_moved_modules(root string) bool {
	for name in v1_fallback_compatibility_modules {
		module_dir := os.join_path(root, 'vlib', name)
		if !os.is_file(os.join_path(module_dir, '${name}.v')) {
			return false
		}
		marker := os.read_file(os.join_path(module_dir, v1_fallback_compatibility_marker)) or {
			return false
		}
		if marker.trim_space() != v_version {
			return false
		}
	}
	return true
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
