module scripting

import os
import term

const (
	term_colors = term.can_show_color_on_stdout()
)

pub fn set_verbose(on bool) {
	// setting a global here would be the obvious solution,
	// but V does not have globals normally.
	if on {
		os.setenv('VERBOSE', '1', true)
	} else {
		os.unsetenv('VERBOSE')
	}
}

pub fn cprintln(message string) {
	mut omessage := message
	omessage = if scripting.term_colors { term.green(omessage) } else { omessage }
	println(omessage)
}

pub fn verbose_trace(label string, message string) {
	if os.getenv('VERBOSE').len > 0 {
		slabel := 'scripting.$label'
		cprintln('# ${slabel:-25s} : $message')
	}
}

pub fn verbose_trace_exec_result(x os.Result) {
	if os.getenv('VERBOSE').len > 0 {
		cprintln('#   cmd.exit_code : ${x.exit_code.str():-4s}  cmd.output:')
		cprintln('# ----------------------------------- #')
		mut lnum := 1
		lines := x.output.split_into_lines()
		for line in lines {
			cprintln('# ${lnum:3d}: $line')
			lnum++
		}
		cprintln('# ----------------------------------- #')
	}
}

pub fn chdir(path string) {
	verbose_trace(@FN, 'cd $path')
	os.chdir(path)
}

pub fn rmrf(path string) {
	verbose_trace(@FN, 'rm -rf $path')
	if os.exists(path) {
		if os.is_dir(path) {
			os.rmdir_all(path) or { panic(err) }
		} else {
			os.rm(path) or { panic(err) }
		}
	}
}

// execute a command, and return a result, or an error, if it failed in any way.
pub fn exec(cmd string) ?os.Result {
	verbose_trace(@FN, cmd)
	x := os.execute(cmd)
	if x.exit_code != 0 {
		verbose_trace(@FN, '## failed.')
		return error(x.output)
	}
	verbose_trace_exec_result(x)
	return x
}

// run a command, tracing its results, and returning ONLY its output
pub fn run(cmd string) string {
	verbose_trace(@FN, cmd)
	x := os.execute(cmd)
	if x.exit_code < 0 {
		verbose_trace(@FN, '## failed.')
		return ''
	}
	verbose_trace_exec_result(x)
	if x.exit_code == 0 {
		return x.output.trim_right('\r\n')
	}
	return ''
}

pub fn exit_0_status(cmd string) bool {
	verbose_trace(@FN, cmd)
	x := os.execute(cmd)
	if x.exit_code < 0 {
		verbose_trace(@FN, '## failed.')
		return false
	}
	verbose_trace_exec_result(x)
	if x.exit_code == 0 {
		return true
	}
	return false
}

pub fn tool_must_exist(toolcmd string) {
	verbose_trace(@FN, toolcmd)
	if exit_0_status('type $toolcmd') {
		return
	}
	eprintln('Missing tool: $toolcmd')
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
		println('$size $f') // println('${size:10d} $f')
	}
}
