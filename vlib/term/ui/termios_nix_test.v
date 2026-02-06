module ui

import os

fn test_get_cursor_position_reads_valid_row_column_data() ! {
	mut original_stdin_fd := -1
	unsafe {
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
	mut original_stdin_fd := -1
	unsafe {
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
