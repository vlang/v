module gg

import sokol.sapp
import os

// record_frame records the current frame to a file.
// record_frame acts according to settings specified in `gg.recorder_settings`.
[if gg_record ?]
pub fn (mut ctx Context) record_frame() {
	if ctx.frame in gg.recorder_settings.screenshot_frames {
		screenshot_file_path := '${gg.recorder_settings.screenshot_prefix}${ctx.frame}.png'
		$if gg_record_trace ? {
			eprintln('>>> screenshoting ${screenshot_file_path}')
		}

		sapp.screenshot_png(screenshot_file_path) or { panic(err) }
	}
	if ctx.frame == gg.recorder_settings.stop_at_frame {
		$if gg_record_trace ? {
			eprintln('>>> exiting at frame ${ctx.frame}')
		}
		exit(0)
	}
}

fn new_gg_recorder_settings() &SSRecorderSettings {
	$if gg_record ? {
		stop_frame := os.getenv_opt('VGG_STOP_AT_FRAME') or { '-1' }.i64()
		frames := os.getenv('VGG_SCREENSHOT_FRAMES').split_any(',').map(it.u64())
		folder := os.getenv('VGG_SCREENSHOT_FOLDER')
		prefix := os.join_path_single(folder, os.file_name(os.executable()).all_before('.') + '_')
		return &SSRecorderSettings{
			stop_at_frame: stop_frame
			screenshot_frames: frames
			screenshot_folder: folder
			screenshot_prefix: prefix
		}
	} $else {
		return &SSRecorderSettings{}
	}
}

const recorder_settings = new_gg_recorder_settings()
