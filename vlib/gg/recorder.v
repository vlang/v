module gg

import os

enum ScreenshotOutput {
	file
	stdout
}

const gg_record_stdout_prefix = '__V_GG_IMAGE__'

@[heap]
pub struct SSRecorderSettings {
pub mut:
	stop_at_frame     i64 = -1
	screenshot_frames []u64
	screenshot_folder string
	screenshot_prefix string
	screenshot_output ScreenshotOutput = .file
}

fn parse_screenshot_frames(value string) []u64 {
	mut frames := []u64{}
	for raw_frame in value.split(',') {
		frame := raw_frame.trim_space()
		if frame == '' {
			continue
		}
		frames << frame.u64()
	}
	return frames
}

fn parse_screenshot_output(value string) ScreenshotOutput {
	return match value.trim_space().to_lower() {
		'stdout' { .stdout }
		else { .file }
	}
}

fn recorder_env_value(env map[string]string, key string, fallback string) string {
	value := env[key]
	if value == '' {
		return fallback
	}
	return value
}

fn new_gg_recorder_settings_from_env(env map[string]string, executable string) &SSRecorderSettings {
	mut stop_frame := recorder_env_value(env, 'VGG_STOP_AT_FRAME', '-1').i64()
	mut frames := parse_screenshot_frames(env['VGG_SCREENSHOT_FRAMES'])
	folder := env['VGG_SCREENSHOT_FOLDER']
	output := parse_screenshot_output(env['VGG_SCREENSHOT_OUTPUT'])
	if output == .stdout && frames.len == 0 {
		frames << u64(1)
	}
	if output == .stdout && stop_frame < 0 && frames.len > 0 {
		stop_frame = i64(frames[frames.len - 1])
	}
	prefix := os.join_path_single(folder, os.file_name(executable).all_before('.') + '_')
	return &SSRecorderSettings{
		stop_at_frame:     stop_frame
		screenshot_frames: frames
		screenshot_folder: folder
		screenshot_prefix: prefix
		screenshot_output: output
	}
}
