#include <errno.h>

fn test_assigning_c_errno_to_same_named_local_var() {
	old_errno := C.errno
	defer {
		C.errno = old_errno
	}
	C.errno = C.ENOENT
	errno := C.errno
	assert errno == C.ENOENT
}
