module ui

import os

const capture_probe = 'ui-stdout-capture'

// with_captured_stdout returns everything that `action` writes to stdout.
// On Windows `print` goes through the console handle whenever stdout is a
// terminal, so a capture is only possible once stdout is redirected. The
// self-test below detects that case instead of failing every assertion.
fn with_captured_stdout(action fn ()) string {
	flush_stdout()
	mut capture := os.stdio_capture() or { return '' }
	action()
	flush_stdout()
	lines, _ := capture.finish()
	return lines.join('')
}

fn stdout_is_capturable() bool {
	return with_captured_stdout(fn () {
		print(capture_probe)
	}) == capture_probe
}

fn test_context_defaults() {
	ctx := Context{}
	assert ctx.frame_count == 0
	assert ctx.window_width == 0
	assert ctx.window_height == 0
	assert !ctx.paused
	assert !ctx.enable_rgb
	assert ctx.enable_ansi256
	assert !ctx.enable_su
	assert ctx.supports_alternate_buffer
	assert ctx.supports_sgr_mouse
	assert ctx.supports_sync_updates
	assert ctx.supports_window_title
}

fn test_config_defaults() {
	cfg := Config{}
	assert cfg.buffer_size == 256
	assert cfg.frame_rate == 30
	assert !cfg.use_x11
	assert !cfg.hide_cursor
	assert !cfg.capture_events
	assert !cfg.mouse_enabled
	assert !cfg.skip_init_checks
	assert cfg.use_alternate_buffer
	assert cfg.reset.len == 13
}

fn test_write_ignores_an_empty_string() {
	mut ctx := Context{}
	ctx.write('')
	assert ctx.print_buf.len == 0
}

fn test_write_appends_to_the_print_buffer() {
	mut ctx := Context{}
	ctx.write('ab')
	ctx.write('cd')
	assert ctx.print_buf.bytestr() == 'abcd'
}

fn test_set_cursor_position_writes_row_then_column() {
	mut ctx := Context{}
	ctx.set_cursor_position(3, 7)
	assert ctx.print_buf.bytestr() == '\x1b[7;3H'
}

fn test_bold_sets_the_sgr_bold_attribute() {
	mut ctx := Context{}
	ctx.bold()
	assert ctx.print_buf.bytestr() == '\x1b[1m'
}

fn test_cursor_visibility_uses_the_dectcem_parameter() {
	mut ctx := Context{}
	ctx.show_cursor()
	ctx.hide_cursor()
	assert ctx.print_buf.bytestr() == '\x1b[?25h\x1b[?25l'
}

fn test_the_resets_restore_the_defaults() {
	mut ctx := Context{}
	ctx.reset()
	ctx.reset_color()
	ctx.reset_bg_color()
	assert ctx.print_buf.bytestr() == '\x1b[0m\x1b[39m\x1b[49m'
}

fn test_clear_erases_the_screen_and_the_scrollback() {
	mut ctx := Context{}
	ctx.clear()
	assert ctx.print_buf.bytestr() == '\x1b[2J\x1b[3J'
}

fn test_draw_point_positions_the_cursor_then_writes_a_space() {
	mut ctx := Context{}
	ctx.draw_point(2, 4)
	assert ctx.print_buf.bytestr() == '\x1b[4;2H '
}

fn test_draw_text_positions_the_cursor_then_writes_the_string() {
	mut ctx := Context{}
	ctx.draw_text(1, 2, 'abc')
	assert ctx.print_buf.bytestr() == '\x1b[2;1Habc'
}

fn test_draw_line_of_equal_rows_is_one_position_and_spaces() {
	mut ctx := Context{}
	ctx.draw_line(0, 3, 5, 3)
	assert ctx.print_buf.bytestr() == '\x1b[3;0H      '
}

fn test_draw_line_walks_every_point_of_the_segment() {
	mut ctx := Context{}
	ctx.draw_line(0, 0, 3, 3)
	assert ctx.print_buf.bytestr() == '\x1b[0;0H \x1b[1;1H \x1b[2;2H \x1b[3;3H '
}

fn test_draw_line_walks_backwards_too() {
	mut ctx := Context{}
	ctx.draw_line(3, 3, 0, 0)
	assert ctx.print_buf.bytestr() == '\x1b[3;3H \x1b[2;2H \x1b[1;1H \x1b[0;0H '
}

fn test_draw_dashed_line_skips_every_other_point() {
	mut ctx := Context{}
	ctx.draw_dashed_line(0, 0, 4, 4)
	assert ctx.print_buf.bytestr() == '\x1b[0;0H \x1b[2;2H \x1b[4;4H '
}

fn test_draw_rect_fills_every_row() {
	mut ctx := Context{}
	ctx.draw_rect(0, 0, 2, 2)
	assert ctx.print_buf.bytestr() == '\x1b[0;0H   \x1b[1;0H   \x1b[2;0H   '
}

fn test_draw_empty_rect_draws_the_four_edges() {
	mut ctx := Context{}
	ctx.draw_empty_rect(0, 0, 2, 2)
	assert ctx.print_buf.bytestr() == '\x1b[0;0H   \x1b[2;0H   \x1b[0;0H \x1b[1;0H \x1b[2;0H \x1b[0;2H \x1b[1;2H \x1b[2;2H '
}

fn test_draw_empty_dashed_rect_draws_dashed_edges() {
	mut ctx := Context{}
	ctx.draw_empty_dashed_rect(0, 0, 3, 3)
	assert ctx.print_buf.bytestr() == '\x1b[0;0H \x1b[0;2H \x1b[0;0H \x1b[2;0H \x1b[3;1H \x1b[3;3H \x1b[1;3H \x1b[3;3H '
}

fn test_horizontal_separator_spans_the_window_width() {
	mut ctx := Context{}
	ctx.window_width = 5
	ctx.horizontal_separator(2)
	assert ctx.print_buf.bytestr() == '\x1b[2;0H-----'
}

fn test_horizontal_separator_of_a_zero_width_window_writes_no_separator() {
	mut ctx := Context{}
	ctx.horizontal_separator(0)
	assert ctx.print_buf.bytestr() == '\x1b[0;0H'
}

fn test_flush_writes_the_buffer_and_clears_it() {
	mut ctx := Context{}
	ctx.write('x')
	if !stdout_is_capturable() {
		return
	}
	flush_stdout()
	mut capture := os.stdio_capture() or { return }
	ctx.flush()
	flush_stdout()
	lines, _ := capture.finish()
	assert lines.join('') == 'x'
	assert ctx.print_buf.len == 0
}

fn test_flush_of_an_empty_buffer_writes_nothing() {
	if !stdout_is_capturable() {
		return
	}
	mut ctx := Context{}
	flush_stdout()
	mut capture := os.stdio_capture() or { return }
	ctx.flush()
	flush_stdout()
	lines, _ := capture.finish()
	assert lines.join('') == ''
	assert ctx.print_buf.len == 0
}

fn test_set_window_title_writes_an_osc_sequence() {
	if !stdout_is_capturable() {
		return
	}
	mut ctx := Context{}
	flush_stdout()
	mut capture := os.stdio_capture() or { return }
	ctx.set_window_title('v')
	flush_stdout()
	lines, _ := capture.finish()
	assert lines.join('') == '\x1b]0;v\x07'
}

fn test_set_window_title_writes_nothing_when_the_title_is_unsupported() {
	if !stdout_is_capturable() {
		return
	}
	mut ctx := Context{}
	ctx.supports_window_title = false
	flush_stdout()
	mut capture := os.stdio_capture() or { return }
	ctx.set_window_title('v')
	flush_stdout()
	lines, _ := capture.finish()
	assert lines.join('') == ''
}
