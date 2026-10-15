#include <errno.h>

fn set_errno_again() { C.errno = C.EAGAIN }

fn set_errno_reset() { C.errno = C.ECONNRESET }

fn set_errno_pipe() { C.errno = C.EPIPE }

fn set_errno_again_after_other_bodies() { C.errno = C.EAGAIN }

fn read_errno() int { return C.errno }

fn test_undeclared_errno_assignments_across_function_bodies() {
	previous := C.errno
	defer { C.errno = previous }
	set_errno_again()
	assert read_errno() == C.EAGAIN
	set_errno_reset()
	assert read_errno() == C.ECONNRESET
	set_errno_pipe()
	assert read_errno() == C.EPIPE
	set_errno_again_after_other_bodies()
	assert read_errno() == C.EAGAIN
}
