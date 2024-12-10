@[has_globals]
module diff

import os
import time

pub enum DiffTool {
	auto
	diff      // core package on Unix-like systems.
	colordiff // `diff` wrapper.
	delta     // viewer for git and diff output.
	// fc // built-in tool on windows. // TODO: enable when its command output can be read.
}

@[params]
pub struct CompareOptions {
pub:
	tool DiffTool
	// Custom args used with the diff command.
	args string
	// Sets the environment variable whose value can overwrite a diff command passed to a compare function.
	// It also enables the use of commands that are not in the list of known diff tools.
	// Set it to `none` to disable it.
	env_overwrite_var ?string = 'VDIFF_CMD'
}

@[params]
pub struct CompareTextOptions {
	CompareOptions
pub:
	base_name   string = 'base'
	target_name string = 'target'
}

// Default options for `diff` and `colordiff`.
// Short `diff` args are supported more widely (e.g. on OpenBSD, ref. https://man.openbsd.org/diff.1).
// `-d -a -U 2` ^= `--minimal --text --unified=2`
const default_diff_args = $if openbsd || freebsd { '-d -a -U 2' } $else { '-d -a -U 2 -F "fn "' }
const known_diff_tool_defaults = {
	// When searching for an automatically available diff tool, the tools are searched in this order.
	DiffTool.delta: ''
	.colordiff:     default_diff_args
	.diff:          default_diff_args
	// .fc:        '/lnt'
}

// List of detected diff tools.
__global cache_of_available_tools = []DiffTool{}

// Allows public checking for the available tools and prevents repeated searches
// when using compare functions with automatic diff tool detection.
pub fn available_tools() []DiffTool {
	if cache_of_available_tools.len == 0 {
		cache_of_available_tools = find_working_diff_tools()
	}
	return cache_of_available_tools
}

// compare_files returns a string displaying the differences between two files.
pub fn compare_files(path1 string, path2 string, opts CompareOptions) !string {
	p1, p2 := os.quoted_path(os.real_path(path1)), os.quoted_path(os.real_path(path2))
	if v := opts.env_overwrite_var {
		env_cmd := os.getenv(v)
		if env_cmd != '' {
			tool, args := env_cmd.split_once(' ') or { env_cmd, opts.args }
			os.find_abs_path_of_executable(tool) or {
				return error('error: failed to find comparison command `${tool}`')
			}
			return run_tool('${tool} ${args} ${p1} ${p2}', @LOCATION)
		}
	}
	tool, cmd := opts.find_tool()!
	mut args := opts.args
	if args == '' {
		args = if defaults := known_diff_tool_defaults[tool] { defaults } else { '' }
		if opts.tool == .diff {
			// Ensure that the diff command supports the color option.
			// E.g., some BSD installations or macOS diff (based on FreeBSD diff)
			// might not include additional diffutils by default.
			res := run_tool('${cmd} ${args} --color=always ${p1} ${p2}', @LOCATION)
			if !res.contains('unrecognized option') {
				return res
			}
		}
	}
	return run_tool('${cmd} ${args} ${p1} ${p2}', @LOCATION)
}

// compare_text returns a string displaying the differences between two strings.
pub fn compare_text(text1 string, text2 string, opts CompareTextOptions) !string {
	ctime := time.sys_mono_now()
	tmp_dir := os.join_path_single(os.vtmp_dir(), ctime.str())
	os.mkdir(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	path1 := os.join_path_single(tmp_dir, opts.base_name)
	path2 := os.join_path_single(tmp_dir, opts.target_name)
	// When comparing strings and not files, prevent `\ No newline at end of file` in the output.
	if !text1.ends_with('\n') || !text2.ends_with('\n') {
		os.write_file(path1, text1 + '\n')!
		os.write_file(path2, text2 + '\n')!
	} else {
		os.write_file(path1, text1)!
		os.write_file(path2, text2)!
	}
	return compare_files(path1, path2, opts.CompareOptions)!
}

fn (opts CompareOptions) find_tool() !(DiffTool, string) {
	tool := if opts.tool == .auto {
		auto_tool := available_tools()[0] or {
			return error('error: failed to find comparison command')
		}

		auto_tool
	} else {
		opts.tool
	}
	cmd := tool.cmd()
	if opts.tool == .auto {
		// At this point it was already ensured that the automatically detected tool is available.
		return tool, cmd
	}
	os.find_abs_path_of_executable(cmd) or {
		return error('error: failed to find comparison command `${cmd}`')
	}
	return tool, cmd
}

// Returns a list of programmatically-compatible known diff tools. Its result is intended to be stored
// in a constant to prevent repeated searches when compare functions with automatic diff tool detection
// are used. Using a public constant will also allow for external checking of available tools.
fn find_working_diff_tools() []DiffTool {
	mut tools := []DiffTool{}
	for tool in known_diff_tool_defaults.keys() {
		cmd := tool.cmd()
		os.find_abs_path_of_executable(cmd) or { continue }
		if tool == .delta {
			// Sanity check that the `delta` executable is actually the diff tool.
			res := os.execute_opt('${cmd} --help') or { continue }
			help_desc := res.output.trim_space().all_before('\n')
			if !help_desc.contains('diff') {
				dbg('delta does not appear to be the diff tool `${help_desc}`', @LOCATION)
				continue
			}
		}
		tools << tool
	}
	return tools
}

fn (dt DiffTool) cmd() string {
	cmd := dt.str()
	return $if windows { '${cmd}.exe' } $else { cmd }
}

fn run_tool(cmd string, dbg_location string) string {
	dbg('cmd=`${cmd}`', dbg_location)
	res := os.execute(cmd)
	dbg('res=`${res}`', dbg_location)
	return res.output.trim_right('\r\n')
}

pub fn find_working_diff_command() !string {
	env_difftool := os.getenv('VDIFF_TOOL')
	env_diffopts := os.getenv('VDIFF_OPTIONS')
	if env_difftool != '' {
		os.find_abs_path_of_executable(env_difftool) or {
			return error('could not find specified VDIFF_TOOL `${env_difftool}`')
		}
		return '${env_difftool} ${env_diffopts}'
	}
	known_diff_tools := ['colordiff', 'gdiff', 'diff', 'colordiff.exe', 'diff.exe', 'opendiff',
		'code', 'code.cmd'] // NOTE: code.cmd is the Windows variant of the `code` cli tool
	mut diff_cmd := ''
	for cmd in known_diff_tools {
		os.find_abs_path_of_executable(cmd) or { continue }
		diff_cmd = cmd
		break
	}
	if diff_cmd == '' {
		return error('No working "diff" command found')
	}
	if diff_cmd in ['code', 'code.cmd'] {
		// Make sure the diff flag `-d` is included in any case.
		return '${diff_cmd} ${env_diffopts} -d'
	}
	// Don't add spaces to the cmd if there are no `env_diffopts`.
	return if env_diffopts != '' { '${diff_cmd} ${env_diffopts}' } else { diff_cmd }
}

// color_compare_files returns a colored diff between two files.
pub fn color_compare_files(diff_cmd string, path1 string, path2 string) string {
	tool := diff_cmd.all_before(' ')
	os.find_abs_path_of_executable(tool) or { return 'comparison command: `${tool}` not found' }
	p1, p2 := os.quoted_path(os.real_path(path1)), os.quoted_path(os.real_path(path2))
	if tool == 'diff' {
		// Ensure that the diff command supports the color option.
		// E.g., some BSD installations do not include `diffutils` as a core package alongside `diff`.
		res := os.execute('${diff_cmd} --color=always ${default_diff_args} ${p1} ${p2}')
		if !res.output.starts_with('diff: unrecognized option') {
			return res.output.trim_right('\r\n')
		}
	}
	cmd := '${diff_cmd} ${default_diff_args} ${p1} ${p2}'
	return os.execute(cmd).output.trim_right('\r\n')
}

// color_compare_strings returns a colored diff between two strings.
pub fn color_compare_strings(diff_cmd string, unique_prefix string, expected string, found string) string {
	tmp_dir := os.join_path_single(os.vtmp_dir(), unique_prefix)
	os.mkdir(tmp_dir) or {}
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	ctime := time.sys_mono_now()
	e_file := os.join_path_single(tmp_dir, '${ctime}.expected.txt')
	f_file := os.join_path_single(tmp_dir, '${ctime}.found.txt')
	os.write_file(e_file, expected) or { panic(err) }
	os.write_file(f_file, found) or { panic(err) }
	res := color_compare_files(diff_cmd, e_file, f_file)
	return res
}

@[if vdiff_debug ?]
fn dbg(msg string, location string) {
	println('[DIFF DEBUG] ${location}: ${msg}')
}
