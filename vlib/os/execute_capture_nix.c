#if defined(__linux__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <sys/types.h>
#include <unistd.h>
#define V_OS_EXECUTE_SCOPE
#include "execute_capture_nix.h"

#if defined(__linux__)
// Use the SDK's pipe2 declaration here rather than redeclaring its array
// parameter in generated C. Creation and CLOEXEC must be one operation.
#ifdef __cplusplus
extern "C"
#endif
// Standalone V3 object builds may embed this helper in multiple objects.
// Weak ELF linkage keeps those objects linkable while retaining the pipe2 ABI.
__attribute__((weak)) int v_os_process_pipe_cloexec(int fds[2]) {
	return pipe2(fds, O_CLOEXEC);
}
#endif
