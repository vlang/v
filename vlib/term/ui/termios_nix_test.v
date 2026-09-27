module ui

import os

fn test_terminal_capabilities_disable_xterm_features_for_linux_console() {
	caps := terminal_capabilities_for('linux')
	assert !caps.enable_ansi256
	assert !caps.supports_alternate_buffer
	assert !caps.supports_sgr_mouse
	assert !caps.supports_sync_updates
	assert !caps.supports_window_title
}

fn test_terminal_capabilities_keep_xterm_defaults() {
	caps := terminal_capabilities_for('xterm-256color')
	assert caps.enable_ansi256
	assert caps.supports_alternate_buffer
	assert caps.supports_sgr_mouse
	assert caps.supports_sync_updates
	assert caps.supports_window_title
}

fn test_get_cursor_position_reads_valid_row_column_data() ! {
	unsafe {
		original_stdin_fd := C.dup(C.STDIN_FILENO)
		if original_stdin_fd == -1 {
			return error('error duplicating stdin: ${C.strerror(C.errno)}')
		}
		defer {
			C.dup2(original_stdin_fd, C.STDIN_FILENO)
			C.close(original_stdin_fd)
		}
		mut pipe := os.pipe()!

		fake_cursor_pos_data := '\033[45;70R'
		written_bytes := pipe.write_string(fake_cursor_pos_data)!
		if written_bytes == -1 {
			pipe.close()
			return error('error writing into pipe: ${C.strerror(C.errno)}')
		}

		C.close(pipe.write_fd)

		if C.dup2(pipe.read_fd, C.STDIN_FILENO) == -1 {
			C.close(pipe.read_fd)
			return error('error redirecting stdin with dup2: ${C.strerror(C.errno)}')
		}

		C.close(pipe.read_fd)

		cursor_pos_x, cursor_pos_y := get_cursor_position()
		assert cursor_pos_x == 45
		assert cursor_pos_y == 70
	}
}

fn test_get_cursor_position_reads_empty_position_data() ! {
	unsafe {
		original_stdin_fd := C.dup(C.STDIN_FILENO)
		if original_stdin_fd == -1 {
			return error('error duplicating stdin: ${C.strerror(C.errno)}')
		}
		defer {
			C.dup2(original_stdin_fd, C.STDIN_FILENO)
			C.close(original_stdin_fd)
		}
		mut pipe := os.pipe()!

		fake_cursor_pos_data := ''
		written_bytes := pipe.write_string(fake_cursor_pos_data)!
		if written_bytes == -1 {
			pipe.close()
			return error('error writing into pipe: ${C.strerror(C.errno)}')
		}

		C.close(pipe.write_fd)

		if C.dup2(pipe.read_fd, C.STDIN_FILENO) == -1 {
			C.close(pipe.read_fd)
			return error('error redirecting stdin with dup2: ${C.strerror(C.errno)}')
		}

		C.close(pipe.read_fd)

		cursor_pos_x, cursor_pos_y := get_cursor_position()
		assert cursor_pos_x == -1
		assert cursor_pos_y == -1
	}
}
