// vtest build: macos && !sanitized_job?
import os
import stbi

@[direct_array_access]
fn png_has_non_black_pixels(path string) !bool {
	img := stbi.load(path)!
	defer {
		img.free()
	}
	for i in 0 .. img.width * img.height {
		unsafe {
			if img.data[i * 4] != 0 || img.data[i * 4 + 1] != 0 || img.data[i * 4 + 2] != 0 {
				return true
			}
		}
	}
	return false
}

fn test_streaming_r8_zero_buffer_stays_black_on_gl_backend() {
	vexe := @VEXE
	vroot := os.dir(vexe)
	sample_path := os.join_path(vroot, 'vlib/gg/testdata/streaming_r8_zero_buffer_issue_10989.vv')
	temp_dir := os.join_path(os.temp_dir(), 'v_gg_issue_10989_${os.getpid()}')
	os.mkdir_all(temp_dir) or { panic(err) }
	defer {
		os.rmdir_all(temp_dir) or {}
	}
	exe_path := os.join_path(temp_dir, 'issue10989_repro')
	// Force the GL backend so this exercises the packed texture upload path from issue #10989.
	compile_cmd := '${os.quoted_path(vexe)} -d gg_record -d darwin_sokol_glcore33 -o ${os.quoted_path(exe_path)} ${os.quoted_path(sample_path)}'
	compile_res := os.execute(compile_cmd)
	assert compile_res.exit_code == 0, compile_res.output
	run_cmd := 'VGG_STOP_AT_FRAME=2 VGG_SCREENSHOT_FRAMES=2 VGG_SCREENSHOT_FOLDER=${os.quoted_path(temp_dir)} ${os.quoted_path(exe_path)}'
	run_res := os.execute(run_cmd)
	if run_res.exit_code != 0 {
		if run_res.output.contains('glpixelformat') || run_res.exit_code == 134 {
			// OpenGL context creation failed (e.g. headless CI runners without GPU).
			// Skip the test rather than failing.
			eprintln('skipping: GL context creation failed on this system')
			return
		}
		assert run_res.exit_code == 0, run_res.output
	}
	screenshot_path := os.join_path(temp_dir, 'issue10989_repro_2.png')
	assert os.exists(screenshot_path)
	assert png_has_non_black_pixels(screenshot_path)! == false
}
