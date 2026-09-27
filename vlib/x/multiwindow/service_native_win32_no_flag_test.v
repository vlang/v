// vtest build: !musl? && !self_ubuntu_musl_ci?
// vtest retry: 0
module multiwindow

import os

fn test_win32_nonreadback_no_flag_facade_stays_disabled() {
	vlib_dir := os.join_path(@DIR, '..', '..')
	base := os.join_path(os.temp_dir(), 'win32_package2_no_flag_${os.getpid()}')
	source_path := '${base}.v'
	binary_path := '${base}.bin'
	defer {
		os.rm(source_path) or {}
		os.rm(binary_path) or {}
		os.rm('${binary_path}.exe') or {}
	}
	source := [
		'module main',
		'',
		'import gg',
		'',
		'fn main() {',
		'\tmut app := gg.App{}',
		'\tapp.monitor_ids() or {',
		'\t\tprintln(err.msg())',
		'\t\treturn',
		'\t}',
		"\tprintln('unexpected success')",
		'}',
	].join('\n')
	os.write_file(source_path, source) or { panic(err) }
	compile :=
		os.execute('${os.quoted_path(@VEXE)} -gc none -subsystem console -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(binary_path)} ${os.quoted_path(source_path)}')
	assert compile.exit_code == 0, 'no-flag consumer failed to compile:\n${compile.output}'
	executable := if os.exists(binary_path) { binary_path } else { '${binary_path}.exe' }
	run := os.execute(os.quoted_path(executable))
	assert run.exit_code == 0, 'no-flag consumer failed to run:\n${run.output}'
	assert run.output.trim_space() == 'gg.multiwindow: compile with `-d gg_multiwindow` to enable gg.App'
}
