module fastc

import os

// poison_int fills all 8 bytes of a V `int`, so a C call that writes only a 4-byte
// `int` leaves the upper half visible.
fn poison_int(value &int) {
	unsafe {
		*(&u64(value)) = u64(0x1122334455667788)
	}
}

fn test_start_capture_widens_the_c_int_results() {
	mut pid := 0
	mut read_fd := 0
	poison_int(&pid)
	poison_int(&read_fd)
	assert fastc_start_capture(['/bin/sh', '-c', 'echo captured'], &pid, &read_fd)
	// A pid and a descriptor are small, non-negative numbers: the whole `int` is checked,
	// not only the low 32 bits that the C side writes.
	assert pid > 0 && pid < 1 << 22, 'pid = 0x${u64(pid):x}'
	assert read_fd >= 0 && read_fd < 1 << 16, 'read_fd = 0x${u64(read_fd):x}'
	output := os.fd_slurp(read_fd).join('')
	os.fd_close(read_fd)
	assert fastc_wait_exit_code(pid) == 0
	assert output.trim_space() == 'captured'
}
