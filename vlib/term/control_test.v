module term

import os

const capture_marker = 'term-stdout-capture'

// capture_stdout returns everything that `action` writes to stdout.
// On Windows `print` goes through the console handle whenever stdout is a
// terminal, so a capture is only possible once stdout is redirected. The
// self-test below detects that case instead of failing every assertion.
fn capture_stdout(action fn ()) string {
	flush_stdout()
	mut capture := os.stdio_capture() or { return '' }
	action()
	flush_stdout()
	lines, _ := capture.finish()
	return lines.join('')
}

fn stdout_is_capturable() bool {
	return capture_stdout(fn () {
		print(capture_marker)
	}) == capture_marker
}

fn assert_terminal_output(expected string, action fn ()) {
	if !stdout_is_capturable() {
		return
	}
	assert capture_stdout(action) == expected
}

fn test_set_cursor_position_writes_row_semicolon_column() {
	assert_terminal_output('\x1b[11;10H', fn () {
		set_cursor_position(Coord{
			x: 10
			y: 11
		})
	})
	assert_terminal_output('\x1b[0;0H', fn () {
		set_cursor_position(Coord{})
	})
}

fn test_move_writes_the_count_before_the_direction() {
	assert_terminal_output('\x1b[3A', fn () {
		move(3, 'A')
	})
	assert_terminal_output('\x1b[1D', fn () {
		move(1, 'D')
	})
}

fn test_cursor_helpers_each_pick_their_own_direction() {
	assert_terminal_output('\x1b[3A', fn () {
		cursor_up(3)
	})
	assert_terminal_output('\x1b[4B', fn () {
		cursor_down(4)
	})
	assert_terminal_output('\x1b[5C', fn () {
		cursor_forward(5)
	})
	assert_terminal_output('\x1b[6D', fn () {
		cursor_back(6)
	})
}

fn test_erase_display_writes_the_parameter_before_j() {
	assert_terminal_output('\x1b[0J', fn () {
		erase_display('0')
	})
	assert_terminal_output('\x1b[1J', fn () {
		erase_display('1')
	})
	assert_terminal_output('\x1b[2J', fn () {
		erase_display('2')
	})
}

fn test_erase_display_helpers_each_pick_their_own_parameter() {
	assert_terminal_output('\x1b[0J', fn () {
		erase_toend()
	})
	assert_terminal_output('\x1b[1J', fn () {
		erase_tobeg()
	})
	assert_terminal_output('\x1b[3J', fn () {
		erase_del_clear()
	})
}

fn test_erase_clear_homes_the_cursor_and_clears_the_screen() {
	assert_terminal_output('\x1b[H\x1b[J', fn () {
		erase_clear()
	})
}

fn test_erase_line_writes_the_parameter_before_k() {
	assert_terminal_output('\x1b[0K', fn () {
		erase_line('0')
	})
	assert_terminal_output('\x1b[2K', fn () {
		erase_line('2')
	})
}

fn test_erase_line_helpers_each_pick_their_own_parameter() {
	assert_terminal_output('\x1b[0K', fn () {
		erase_line_toend()
	})
	assert_terminal_output('\x1b[1K', fn () {
		erase_line_tobeg()
	})
	assert_terminal_output('\x1b[2K', fn () {
		erase_line_clear()
	})
}

fn test_cursor_visibility_uses_the_dectcem_parameter() {
	assert_terminal_output('\x1b[?25h', fn () {
		show_cursor()
	})
	assert_terminal_output('\x1b[?25l', fn () {
		hide_cursor()
	})
}

fn test_clear_previous_line_returns_one_line_and_erases_it() {
	assert_terminal_output('\r\x1b[1A\x1b[2K', fn () {
		clear_previous_line()
	})
}
