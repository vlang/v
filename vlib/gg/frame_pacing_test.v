// vtest build: !docker-ubuntu-musl // needs GL/gl.h
module gg

import encoding.base64
import os
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

fn test_gg_record_captures_after_frame_fn_to_keep_d3d11_readback_pre_present() {
	source := os.read_file(os.join_path(@DIR, 'gg.c.v')) or { panic(err) }
	frame_fn_source :=
		source.all_after('fn gg_frame_fn').all_before('fn swap_interval_frame_budget')
	draw_call := 'ctx.config.frame_fn(ctx.user_data)'
	draw_index := frame_fn_source.index(draw_call) or {
		panic('gg_frame_fn must call the user frame function')
	}
	record_index_after_draw := frame_fn_source[draw_index + draw_call.len..].index('ctx.record_frame()') or {
		panic('gg_record must capture inside gg_frame_fn')
	}
	record_index := draw_index + draw_call.len + record_index_after_draw
	assert draw_index < record_index, 'gg_record must capture after frame_fn so D3D11 readback happens before Present'
}

fn test_gg_record_stop_only_helper_runs_before_ui_idle_return() {
	source := os.read_file(os.join_path(@DIR, 'gg.c.v')) or { panic(err) }
	frame_fn_source :=
		source.all_after('fn gg_frame_fn').all_before('fn swap_interval_frame_budget')
	ticks_branch_start := frame_fn_source.index('if ctx.ticks > 3 {') or {
		panic('gg_frame_fn must keep the ui idle return branch')
	}
	ticks_branch_source := frame_fn_source[ticks_branch_start..].all_before('}')
	stop_index := ticks_branch_source.index('ctx.stop_recording_if_needed()') or {
		panic('ui idle return branch must stop recording when the stop frame is reached')
	}
	return_index := ticks_branch_source.index('return') or {
		panic('ui idle return branch must return')
	}
	assert stop_index < return_index
	assert !ticks_branch_source.contains('ctx.record_frame()'), 'ui idle return branch must not capture screenshots'
}
