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

fn run_with_fallback(driver_args []string, retry_args []string) {
	if os.getenv(v3_no_fallback_env) == '1' || os.getenv(v3_retry_env) == '1' {
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
		c_error_dir: c_error_dir
		args: retry_args.clone()
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
	mut driver_args := []string{}
	if command_index > 0 {
		driver_args << args[..command_index]
	}
	driver_args << ['run', tool_source]
	if command_index >= 0 {
		driver_args << args[command_index..]
	}
	run_with_fallback(clean_compiler_selection_flags(driver_args), clean_compiler_selection_flags(args))
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
	os.rm(report_state.fallback_file) or {}
	os.rmdir_all(report_state.c_error_dir) or {}
	exit(code)
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
		if os.is_executable(cached_fallback) && v1_fallback_has_expected_version(cached_fallback) {
			return cached_fallback
		}
	}
	return none
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
