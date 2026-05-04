// vtest build: !docker-ubuntu-musl // needs GL/gl.h
module gg

import encoding.base64
import time

fn test_swap_interval_frame_budget() {
	assert swap_interval_frame_budget(-1) == time.Duration(0)
	assert swap_interval_frame_budget(0) == time.Duration(0)
	assert swap_interval_frame_budget(1) == time.second / 60
	assert swap_interval_frame_budget(2) == time.second / 30
	assert swap_interval_frame_budget(3) == time.second / 20
}

fn test_parse_screenshot_frames_ignores_blank_entries() {
	assert parse_screenshot_frames('') == []u64{}
	assert parse_screenshot_frames('1, 2,,4 ') == [u64(1), 2, 4]
}

fn test_new_gg_recorder_settings_stdout_defaults_to_first_frame() {
	settings := new_gg_recorder_settings_from_env({
		'VGG_SCREENSHOT_OUTPUT': 'stdout'
	}, '/tmp/minimal_demo')
	assert settings.screenshot_output == .stdout
	assert settings.screenshot_frames == [u64(1)]
	assert settings.stop_at_frame == 1
}

fn test_new_gg_recorder_settings_stdout_uses_last_frame_as_default_stop() {
	settings := new_gg_recorder_settings_from_env({
		'VGG_SCREENSHOT_OUTPUT': 'stdout'
		'VGG_SCREENSHOT_FRAMES': '2, 4'
	}, '/tmp/minimal_demo')
	assert settings.screenshot_frames == [u64(2), 4]
	assert settings.stop_at_frame == 4
}

fn test_new_gg_recorder_settings_preserves_explicit_stop_frame() {
	settings := new_gg_recorder_settings_from_env({
		'VGG_SCREENSHOT_OUTPUT': 'stdout'
		'VGG_SCREENSHOT_FRAMES': '3'
		'VGG_STOP_AT_FRAME':     '9'
	}, '/tmp/minimal_demo')
	assert settings.stop_at_frame == 9
}

fn test_screenshot_stdout_payload_contains_expected_marker() {
	png := [u8(0x89), `P`, `N`, `G`]
	encoded := base64.encode(png)
	payload := screenshot_stdout_payload(7, png)
	assert payload == '${gg_record_stdout_prefix} frame=7 format=png encoding=base64 data=${encoded}'
}
