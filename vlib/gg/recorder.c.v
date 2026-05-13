module gg

import encoding.base64
import sokol.sapp
import os

fn screenshot_stdout_payload(frame u64, png []u8) string {
	return '${gg_record_stdout_prefix} frame=${frame} format=png encoding=base64 data=${base64.encode(png)}'
}

fn emit_recorded_frame_to_stdout(frame u64) ! {
	screenshot_file_path := os.join_path(os.vtmp_dir(),
		'vgg_record_stdout_${os.getpid()}_${frame}.png')
	sapp.screenshot_png(screenshot_file_path)!
	defer {
		os.rm(screenshot_file_path) or {}
	}
	png := os.read_bytes(screenshot_file_path)!
	println(screenshot_stdout_payload(frame, png))
	flush_stdout()
}

// record_frame records the current frame to a file or stdout.
// record_frame acts according to settings specified in `gg.recorder_settings`.
@[if gg_record ?]
pub fn (mut ctx Context) record_frame() {
	if ctx.frame in recorder_settings.screenshot_frames {
		match recorder_settings.screenshot_output {
			.stdout {
				$if gg_record_trace ? {
					eprintln('>>> screenshotting frame ${ctx.frame} to stdout')
				}
				emit_recorded_frame_to_stdout(ctx.frame) or { panic(err) }
			}
			.file {
				screenshot_file_path := '${recorder_settings.screenshot_prefix}${ctx.frame}.png'
				$if gg_record_trace ? {
					eprintln('>>> screenshotting ${screenshot_file_path}')
				}
				sapp.screenshot_png(screenshot_file_path) or { panic(err) }
			}
		}
	}
	if ctx.frame == recorder_settings.stop_at_frame {
		$if gg_record_trace ? {
			eprintln('>>> exiting at frame ${ctx.frame}')
		}
		flush_stdout()
		exit(0)
	}
}

fn new_gg_recorder_settings() &SSRecorderSettings {
	$if gg_record ? {
		return new_gg_recorder_settings_from_env(os.environ(), os.executable())
	} $else {
		return &SSRecorderSettings{}
	}
}

const recorder_settings = new_gg_recorder_settings()
