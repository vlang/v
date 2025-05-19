module ui

fn test_get_cursor_position_reads_valid_row_column_data() ! {
	mut pipeset := [0, 0]
	mut original_stdin_fd := -1
	unsafe {
		if C.pipe(&pipeset[0]) == -1 {
			return error('unable to create pipe: ${C.strerror(C.errno)}')
		}

		fake_cursor_pos_data := '\033[45;70R'
		written_bytes := C.write(pipeset[1], fake_cursor_pos_data.str, fake_cursor_pos_data.len)
		if written_bytes == -1 {
			C.close(pipeset[0])
			C.close(pipeset[1])
			return error('error writing into pipe: ${C.strerror(C.errno)}')
		}

		C.close(pipeset[1])

		if C.dup2(pipeset[0], C.STDIN_FILENO) == -1 {
			C.close(pipeset[0])
			return error('error redirecting stdin with dup2: ${C.strerror(C.errno)}')
		}

		C.close(pipeset[0])

		cursor_pos_x, cursor_pos_y := get_cursor_position()
		assert cursor_pos_x == 45
		assert cursor_pos_y == 70
	}
}

fn test_get_cursor_position_reads_empty_position_data() ! {
	mut pipeset := [0, 0]
	mut original_stdin_fd := -1
	unsafe {
		if C.pipe(&pipeset[0]) == -1 {
			return error('unable to create pipe: ${C.strerror(C.errno)}')
		}

		fake_cursor_pos_data := ''
		written_bytes := C.write(pipeset[1], fake_cursor_pos_data.str, fake_cursor_pos_data.len)
		if written_bytes == -1 {
			C.close(pipeset[0])
			C.close(pipeset[1])
			return error('error writing into pipe: ${C.strerror(C.errno)}')
		}

		C.close(pipeset[1])

		if C.dup2(pipeset[0], C.STDIN_FILENO) == -1 {
			C.close(pipeset[0])
			return error('error redirecting stdin with dup2: ${C.strerror(C.errno)}')
		}

		C.close(pipeset[0])

		cursor_pos_x, cursor_pos_y := get_cursor_position()
		assert cursor_pos_x == -1
		assert cursor_pos_y == -1
	}
}
