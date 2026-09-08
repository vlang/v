// vtest build: gg_multiwindow? && !musl? && !self_ubuntu_musl_ci?
module gg

#include "@VMODROOT/vlib/gg/testdata/multiwindow_gl_readback_helpers_fake_gl.h"

fn C.v_gg_multiwindow_fake_gl_window_case(stale_errors int, operation_error int, permanent_error int, pack_alignment &int, pack_state_restored &int, bind_buffer_calls &int, get_integer_calls &int, read_calls &int, get_error_calls &int) int
fn C.v_gg_multiwindow_fake_gl_image_case(stale_errors int, operation_error int, permanent_error int, pack_alignment &int, read_framebuffer &int, draw_framebuffer &int, read_buffer &int, pack_state_restored &int, bind_buffer_calls &int, get_integer_calls &int, read_calls &int, delete_calls &int, get_error_calls &int) int

fn test_gl_window_readback_drains_only_stale_errors_and_restores_pack_state() {
	mut pack_alignment := 0
	mut pack_state_restored := 0
	mut bind_buffer_calls := 0
	mut get_integer_calls := 0
	mut read_calls := 0
	mut get_error_calls := 0
	result := C.v_gg_multiwindow_fake_gl_window_case(3, 0, 0, &pack_alignment,
		&pack_state_restored, &bind_buffer_calls, &get_integer_calls, &read_calls, &get_error_calls)
	assert result == 0
	assert pack_alignment == 8
	assert pack_state_restored == 1
	assert bind_buffer_calls == 2
	assert get_integer_calls == 5
	assert read_calls == 1
	assert get_error_calls == 5

	for operation_error in [1, 2] {
		pack_alignment = 0
		pack_state_restored = 0
		bind_buffer_calls = 0
		get_integer_calls = 0
		read_calls = 0
		get_error_calls = 0
		failed := C.v_gg_multiwindow_fake_gl_window_case(0, operation_error, 0, &pack_alignment,
			&pack_state_restored, &bind_buffer_calls, &get_integer_calls, &read_calls,
			&get_error_calls)
		assert failed == 3
		assert pack_alignment == 8
		assert pack_state_restored == 1
		assert bind_buffer_calls == 2
		assert get_integer_calls == 5
		assert read_calls == 1
		assert get_error_calls == 3
	}

	pack_alignment = 0
	pack_state_restored = 0
	bind_buffer_calls = 0
	get_integer_calls = 0
	read_calls = 0
	get_error_calls = 0
	permanent := C.v_gg_multiwindow_fake_gl_window_case(0, 0, 1, &pack_alignment,
		&pack_state_restored, &bind_buffer_calls, &get_integer_calls, &read_calls, &get_error_calls)
	assert permanent == 3
	assert pack_alignment == 8
	assert pack_state_restored == 1
	assert bind_buffer_calls == 0
	assert get_integer_calls == 0
	assert read_calls == 0
	assert get_error_calls == 64
}

fn test_gl_image_readback_drains_only_stale_errors_and_restores_separate_framebuffer_state() {
	mut pack_alignment := 0
	mut read_framebuffer := 0
	mut draw_framebuffer := 0
	mut read_buffer := 0
	mut pack_state_restored := 0
	mut bind_buffer_calls := 0
	mut get_integer_calls := 0
	mut read_calls := 0
	mut delete_calls := 0
	mut get_error_calls := 0
	result := C.v_gg_multiwindow_fake_gl_image_case(2, 0, 0, &pack_alignment, &read_framebuffer,
		&draw_framebuffer, &read_buffer, &pack_state_restored, &bind_buffer_calls,
		&get_integer_calls, &read_calls, &delete_calls, &get_error_calls)
	assert result == 0
	assert pack_alignment == 8
	assert read_framebuffer == 17
	assert draw_framebuffer == 29
	assert read_buffer == 19
	assert pack_state_restored == 1
	assert bind_buffer_calls == 2
	assert get_integer_calls == 8
	assert read_calls == 1
	assert delete_calls == 1
	assert get_error_calls == 4

	for operation_error in [1, 2, 3] {
		pack_alignment = 0
		read_framebuffer = 0
		draw_framebuffer = 0
		read_buffer = 0
		pack_state_restored = 0
		bind_buffer_calls = 0
		get_integer_calls = 0
		read_calls = 0
		delete_calls = 0
		get_error_calls = 0
		failed := C.v_gg_multiwindow_fake_gl_image_case(0, operation_error, 0, &pack_alignment,
			&read_framebuffer, &draw_framebuffer, &read_buffer, &pack_state_restored,
			&bind_buffer_calls, &get_integer_calls, &read_calls, &delete_calls, &get_error_calls)
		assert failed == 3
		assert pack_alignment == 8
		assert read_framebuffer == 17
		assert draw_framebuffer == 29
		assert read_buffer == 19
		assert pack_state_restored == 1
		assert bind_buffer_calls == if operation_error == 3 {
			0
		} else {
			2
		}
		assert get_integer_calls == 8
		assert read_calls == if operation_error == 3 {
			0
		} else {
			1
		}
		assert delete_calls == 1
		assert get_error_calls == if operation_error == 1 {
			3
		} else if operation_error == 2 {
			4
		} else {
			2
		}
	}

	pack_alignment = 0
	read_framebuffer = 0
	draw_framebuffer = 0
	read_buffer = 0
	pack_state_restored = 0
	bind_buffer_calls = 0
	get_integer_calls = 0
	read_calls = 0
	delete_calls = 0
	get_error_calls = 0
	permanent := C.v_gg_multiwindow_fake_gl_image_case(0, 0, 1, &pack_alignment, &read_framebuffer,
		&draw_framebuffer, &read_buffer, &pack_state_restored, &bind_buffer_calls,
		&get_integer_calls, &read_calls, &delete_calls, &get_error_calls)
	assert permanent == 3
	assert pack_alignment == 8
	assert read_framebuffer == 17
	assert draw_framebuffer == 29
	assert read_buffer == 19
	assert pack_state_restored == 1
	assert bind_buffer_calls == 0
	assert get_integer_calls == 0
	assert read_calls == 0
	assert delete_calls == 0
	assert get_error_calls == 64
}
