module main

import os

#include "@DIR/interop.h"
#flag @DIR/interop.c
#include <fcntl.h>
#include <errno.h>

fn C.fcntl(fd i32, cmd i32, arg ...int) i32

$if v_bare_libc ?|| (!freestanding && !no_system_libc ?) {
	#flag -DTEST_HOST_LIBC=1
}

fn C.runtime_c_allocate() voidptr
fn C.runtime_c_resize(voidptr, uint) voidptr
fn C.runtime_c_release(voidptr)
fn C.runtime_c_errno_address() uint
fn C.runtime_c_interop() i32
fn C.runtime_c_buffered_output()

// Keep the checks active in production, where assert statements are omitted.
fn require_runtime(condition bool) {
	if !condition { exit(93) }
}

fn check_descriptor_errors() {
	mut pipe := os.pipe() or { panic(err) }
	defer { pipe.close() }
	flags := C.fcntl(pipe.read_fd, C.F_GETFL)
	require_runtime(flags >= 0 && C.fcntl(pipe.read_fd, C.F_SETFL, flags | C.O_NONBLOCK) == 0)
	empty, count := os.fd_read(pipe.read_fd, 16)
	require_runtime(count == -1 && os.last_error().code() == C.EAGAIN && empty == '')
	os.fd_write(pipe.read_fd, 'x')
	require_runtime(os.last_error().code() == C.EBADF)
	os.fd_write(pipe.write_fd, 'x')
	data, n := os.fd_read(pipe.read_fd, 16)
	require_runtime(n == 1 && data == 'x')
	require_runtime(os.fd_close(pipe.write_fd) == 0)
	pipe.write_fd = -1
	_, eof := os.fd_read(pipe.read_fd, 16)
	require_runtime(eof == 0)
}

fn worker() uint {
	// Exercise libc TLS on a V-created thread and both allocation directions.
	pointer := C.runtime_c_allocate()
	require_runtime(pointer != unsafe { nil })
	require_runtime(unsafe { &u8(pointer)[0] } == `C`)
	resized := C.runtime_c_resize(pointer, 128)
	require_runtime(resized != unsafe { nil })
	require_runtime(unsafe { &u8(resized)[0] } == `C`)
	allocated := unsafe { malloc(64) }
	grown := unsafe { v_realloc(allocated, 128) }
	$if freestanding && !gcboehm ? {
		unsafe { free(resized) }
		C.runtime_c_release(grown)
	} $else {
		C.runtime_c_release(resized)
		unsafe { free(grown) }
	}
	return C.runtime_c_errno_address()
}

fn main() {
	if os.args.len > 1 {
		C.runtime_c_buffered_output()
		if os.args[1] == 'exit' { exit(0) }
		return
	}
	check_descriptor_errors()
	$if v_bare_libc ?|| (!freestanding && !no_system_libc ?) {
		require_runtime(C.runtime_c_interop() == 1)
	} $else {
		require_runtime(C.runtime_c_interop() == 0)
	}
	one := spawn worker()
	two := spawn worker()
	left := one.wait()
	right := two.wait()
	require_runtime(left != 0 && right != 0 && left != right)
	require_runtime(os.setenv('V_RUNTIME_SELECTION', 'bare store', true) == 0)
	require_runtime(os.getenv('V_RUNTIME_SELECTION') == 'bare store')
	$if vlibc ? {
		require_runtime(vlibc_sys_getpid() > 0)
	}
	mut process := os.new_process('/bin/sh')
	process.set_args(['-c', 'printf selection-output; printf selection-error >&2'])
	process.set_redirect_stdio()
	process.run()
	process.wait()
	require_runtime(process.is_pending(.stdout))
	require_runtime(process.stdout_read() == 'selection-output')
	require_runtime(process.is_pending(.stderr))
	require_runtime(process.stderr_read() == 'selection-error')
	process.close()
	println('runtime interop passed')
}
