module fastc

import os

// poison_bytes writes a non-zero pattern into exactly `size` bytes at `value`, and no more.
fn poison_bytes(value voidptr, size int) {
	unsafe {
		mut bytes := &u8(value)
		for i in 0 .. size {
			bytes[i] = u8(0x11 * (i % 15 + 1))
		}
	}
}

// poison_int fills every byte of a V `int`, whatever its width on this target, so a C call
// that writes only a 4-byte `int` into an 8-byte one leaves the upper half visible. Where
// `int` is 4 bytes there is no upper half, and nothing past the `int` is written.
fn poison_int(value &int) {
	poison_bytes(value, int(sizeof(int)))
}

struct PoisonGuard {
mut:
	before u32 = 0xdeadbeef
	narrow i32
	after  u32 = 0xdeadbeef
	wide   int
	last   u32 = 0xdeadbeef
}

// The 4-byte field stands for the `int` of a 32-bit target: an 8-byte store through its
// address, which the helper used to make, lands on the field after it.
fn test_poison_stays_inside_the_value_it_is_given() {
	mut guard := PoisonGuard{}
	poison_bytes(&guard.narrow, int(sizeof(i32)))
	assert guard.narrow != 0
	assert guard.before == 0xdeadbeef && guard.after == 0xdeadbeef, 'after = 0x${guard.after:x}'
	poison_int(&guard.wide)
	assert guard.wide != 0
	if sizeof(int) == 8 {
		assert u64(guard.wide) >> 32 != 0, 'the upper half of an 8-byte int is not poisoned'
	}
	assert guard.after == 0xdeadbeef && guard.last == 0xdeadbeef, 'last = 0x${guard.last:x}'
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
