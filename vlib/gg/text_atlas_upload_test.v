// vtest build: macos && !sanitized_job?
module gg

import gg.testdata.multiwindow_probe_watchdog
import os
import v.cmdexec
import time

fn test_new_font_glyphs_upload_once_before_their_first_commit() {
	root := os.join_path(os.vtmp_dir(), 'gg_atlas_first_commit_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	executable := os.join_path(root, 'atlas_first_commit')
	fixture := os.join_path(@DIR, 'testdata', 'text_atlas_first_commit.vv')
	compiled := cmdexec.run(@VEXE, ['-d', 'darwin_sokol_glcore33', '-o', executable, fixture])
	assert compiled.exit_code == 0, compiled.output
	run := multiwindow_probe_watchdog.run(
		executable: executable
		timeout:    30 * time.second
		start_file: os.join_path(root, 'start')
	) or { panic(err) }
	assert !run.timed_out && run.reaped && run.confinement_empty, run.combined_output()
	assert !run.forced_cleanup, run.combined_output()
	if run.exit_code != 0 && run.combined_output().contains('glpixelformat') {
		eprintln('SKIP: font atlas GPU regression unavailable: GL context creation failed')
		return
	}
	assert run.exit_code == 0, run.combined_output()
	lines := run.stdout.split_into_lines()
	assert lines.len > 0, 'font atlas probe produced no completion record\n${run.combined_output()}'
	assert lines.last() == 'FONT_ATLAS_FIRST_COMMIT_PASS', run.combined_output()
	println('FONT_ATLAS_GPU_TRACE_PASS')
}
