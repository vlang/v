module scripting

import os

pub fn verbose_trace(label string, message string){
	if os.getenv('VERBOSE').len > 0 {
		slabel := 'scripting.${label}'
		println('# ${slabel:30s} : $message')
	}
}

pub fn verbose_trace_exec_result(x os.Result) {
	if os.getenv('VERBOSE').len > 0 {
		println('#   cmd.exit_code : ${x.exit_code.str()}')
		println('#   cmd.output    :')
		println('# ----------------------------------- #')
		mut lnum := 1
		lines := x.output.split_into_lines()
		for line in lines {
			println('# ${lnum:3d}: $line')
			lnum++
		}
		println('# ----------------------------------- #')
	}     
}

pub fn chdir(path string) {
	verbose_trace(@FN, 'cd $path')
	os.chdir( path )
}

pub fn run(cmd string) string {
	verbose_trace(@FN, cmd)
	x := os.exec(cmd) or { return '' }
	verbose_trace_exec_result( x )
	if x.exit_code == 0 { return x.output }
	return ''
}

pub fn command_exits_with_zero_status(cmd string) bool {
	verbose_trace(@FN, cmd)
	x := os.exec(cmd) or { return false }
	verbose_trace_exec_result( x )
	if x.exit_code == 0 { return true }
	return false
}

pub fn tool_must_exist(toolcmd string) {
	verbose_trace(@FN, toolcmd)
	if command_exits_with_zero_status( 'type $toolcmd' ) { return }
	eprintln('Missing tool: $toolcmd')
	eprintln('Please try again after you install it.')
	exit(1)
}

pub fn used_tools_must_exist(tools []string) {
	for t in tools {
		tool_must_exist(t)
	}
}

pub fn check_v_commit_timestamp_before_self_rebuilding(v_timestamp int) {
	if v_timestamp >= 1561805697 { return }
	eprintln('##################################################################')
	eprintln('# WARNING: v self rebuilding, before 5b7a1e8 (2019-06-29 12:21)  #')
	eprintln('#          required the v executable to be built *inside*        #')
	eprintln('#          the toplevel compiler/ folder.                        #')
	eprintln('#                                                                #')
	eprintln('#          That is not supported by this tool.                   #')
	eprintln('#          You will have to build it manually there.             #')
	eprintln('##################################################################')
}
