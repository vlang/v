module scripting

import os
import term
import time

const term_colors = term.can_show_color_on_stdout()

pub fn set_verbose(on bool) {
	// setting a global here would be the obvious solution,
	// but V does not have globals normally.
	if on {
		os.setenv('VERBOSE', '1', true)
	} else {
		os.unsetenv('VERBOSE')
	}
}

pub fn cprint(omessage string) {
	mut message := omessage
	if term_colors {
		message = term.cyan(message)
	}
	print(message)
	flush_stdout()
}

pub fn cprint_strong(omessage string) {
	mut message := omessage
	if term_colors {
		message = term.bright_green(message)
	}
	print(message)
	flush_stdout()
}

pub fn cprintln(omessage string) {
	cprint(omessage)
	println('')
	flush_stdout()
}

pub fn cprintln_strong(omessage string) {
	cprint_strong(omessage)
	println('')
	flush_stdout()
}

pub fn verbose_trace(label string, message string) {
	if os.getenv('VERBOSE').len > 0 {
		slabel := '${time.now().format_ss_milli()} ${label}'
		cprintln('# ${slabel:-43s} : ${message}')
	}
}

pub fn verbose_trace_strong(label string, omessage string) {
	if os.getenv('VERBOSE').len > 0 {
		slabel := '${time.now().format_ss_milli()} ${label}'
		mut message := omessage
		if term_colors {
			message = term.bright_green(message)
		}
		cprintln('# ${slabel:-43s} : ${message}')
	}
}

pub fn verbose_trace_exec_result(x os.Result) {
	if os.getenv('VERBOSE').len > 0 {
		cprintln('#   cmd.exit_code : ${x.exit_code.str():-4s}  cmd.output:')
		mut lnum := 1
		lines := x.output.split_into_lines()
		for oline in lines {
			mut line := oline
			if term_colors {
				line = term.bright_green(line)
			}
			cprintln('# ${lnum:3d}: ${line}')
			lnum++
		}
		cprintln('# ----------------------------------------------------------------------')
	}
}

fn modfn(mname string, fname string) string {
	return '${mname}.${fname}'
}

pub fn chdir(path string) {
	verbose_trace_strong(modfn(@MOD, @FN), 'cd ${path}')
	os.chdir(path) or {
		verbose_trace(modfn(@MOD, @FN), '## failed.')
		return
	}
}

pub fn mkdir(path string) ! {
	verbose_trace_strong(modfn(@MOD, @FN), 'mkdir ${path}')
	os.mkdir(path) or {
		verbose_trace(modfn(@MOD, @FN), '## failed.')
		return err
	}
}

pub fn mkdir_all(path string) ! {
	verbose_trace_strong(modfn(@MOD, @FN), 'mkdir -p ${path}')
	os.mkdir_all(path) or {
		verbose_trace(modfn(@MOD, @FN), '## failed.')
		return err
	}
}

pub fn rmrf(path string) {
	verbose_trace_strong(modfn(@MOD, @FN), 'rm -rf ${path}')
	if os.exists(path) {
		if os.is_dir(path) {
			os.rmdir_all(path) or { panic(err) }
		} else {
			os.rm(path) or { panic(err) }
		}
	}
}

// execute a command, and return a result, or an error, if it failed in any way.
pub fn exec(cmd string) !os.Result {
	verbose_trace_strong(modfn(@MOD, @FN), cmd)
	x := os.execute(cmd)
	if x.exit_code != 0 {
		verbose_trace(modfn(@MOD, @FN), '## failed.')
		return error(x.output)
	}
	verbose_trace_exec_result(x)
	return x
}

// run a command, tracing its results, and returning ONLY its output
pub fn run(cmd string) string {
	verbose_trace_strong(modfn(@MOD, @FN), cmd)
	x := os.execute(cmd)
	if x.exit_code < 0 {
		verbose_trace(modfn(@MOD, @FN), '## failed.')
		return ''
	}
	verbose_trace_exec_result(x)
	if x.exit_code == 0 {
		return x.output.trim_right('\r\n')
	}
	return ''
}

// frun runs a command, tracing its results, and returning ONLY its output, or an error, if it failed
pub fn frun(cmd string) !string {
	verbose_trace_strong(modfn(@MOD, @FN), cmd)
	x := os.execute(cmd)
	if x.exit_code != 0 {
		verbose_trace(modfn(@MOD, @FN), '## failed.')
		verbose_trace(modfn(@MOD, @FN), '## failure   code: ${x.exit_code}')
		verbose_trace(modfn(@MOD, @FN), '## failure output: ${x.output}')
		return error_with_code('failed cmd: ${cmd}', x.exit_code)
	}
	verbose_trace_exec_result(x)
	return x.output.trim_right('\r\n')
}

pub fn exit_0_status(cmd string) bool {
	verbose_trace_strong(modfn(@MOD, @FN), cmd)
	x := os.execute(cmd)
	if x.exit_code < 0 {
		verbose_trace(modfn(@MOD, @FN), '## failed.')
		return false
	}
	verbose_trace_exec_result(x)
	if x.exit_code == 0 {
		return true
	}
	return false
}

pub fn tool_must_exist(toolcmd string) {
	verbose_trace(modfn(@MOD, @FN), toolcmd)
	where_is_cmd := if os.user_os() == 'windows' { 'where' } else { 'type' }
	if exit_0_status('${where_is_cmd} ${toolcmd}') {
		return
	}
	eprintln('Missing tool: ${toolcmd}')
	eprintln('Please try again after you install it.')
	exit(1)
}

pub fn used_tools_must_exist(tools []string) {
	for t in tools {
		tool_must_exist(t)
	}
}

pub fn show_sizes_of_files(files []string) {
	for f in files {
		size := os.file_size(f)
		println('${size} ${f}') // println('${size:10d} $f')
	}
}
