// vtest build: linux && gg_multiwindow? && sokol_wayland?
module gg

import sokol.sapp
import x.multiwindow

#include <fcntl.h>
#include <unistd.h>

struct WaylandPipeAbiBuffer {
mut:
	before u32
	fds    [2]i32
	after  u32
}

fn test_wayland_pipe_bindings_and_native_descriptor_storage() {
	// Load both backends' real C declarations without starting a display server.
	// Do not redeclare C.pipe here: that would mask which binding is being tested.
	assert sizeof(sapp.Range) > 0
	assert sizeof(multiwindow.WindowConfig) > 0
	mut probe := WaylandPipeAbiBuffer{
		before: 0x13579bdf
		fds:    [i32(-1), i32(-1)]!
		after:  0x2468ace0
	}
	assert C.pipe(&probe.fds[0]) == 0
	defer {
		if probe.fds[0] >= 0 {
			C.close(probe.fds[0])
		}
		if probe.fds[1] >= 0 {
			C.close(probe.fds[1])
		}
	}
	assert probe.before == 0x13579bdf
	assert probe.after == 0x2468ace0
	assert probe.fds[0] >= 0
	assert probe.fds[1] >= 0
	assert probe.fds[0] != probe.fds[1]
	read_flags := C.fcntl(probe.fds[0], C.F_GETFL)
	write_flags := C.fcntl(probe.fds[1], C.F_GETFL)
	assert read_flags >= 0
	assert write_flags >= 0
	assert (read_flags & C.O_ACCMODE) == C.O_RDONLY
	assert (write_flags & C.O_ACCMODE) == C.O_WRONLY

	// A small payload fits in an empty pipe; close its writer before reading so
	// both the payload read and the EOF check are bounded without a compositor.
	payload := [u8(0), 0x7f, 0x80, 0xff, `V`, `\n`]!
	assert C.write(probe.fds[1], &payload[0], usize(payload.len)) == isize(payload.len)
	assert C.close(probe.fds[1]) == 0
	probe.fds[1] = -1
	mut received := [16]u8{}
	assert C.read(probe.fds[0], &received[0], usize(received.len)) == isize(payload.len)
	for i, expected_byte in payload {
		assert received[i] == expected_byte
	}
	assert C.read(probe.fds[0], &received[0], usize(received.len)) == 0
	assert probe.before == 0x13579bdf
	assert probe.after == 0x2468ace0
}
