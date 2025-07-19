module os

#include <sys/ptrace.h>

fn C.ptrace(int, u32, voidptr, int) int

// debugger_present returns a bool indicating if the process is being debugged.
pub fn debugger_present() bool {
	$if openbsd {
		// check if a child process could trace its parent process,
		// if not a debugger must be present
		pid := fork()
		if pid == 0 {
			ppid := getppid()
			/*
			 * On OpenBSD, impossible to trace a parent process from its child.
			 * Possible errors:
			 *   - EBUSY: process already traced
			 *   - EPERM: process is not a child of tracer (sysctl kern.global_ptrace=0)
			 *   - EINVAL: process is an ancestor of the current process and not init
			 *     (sysctl kern.global_ptrace=1)
			 */
			if C.ptrace(C.PT_ATTACH, ppid, unsafe { nil }, 0) < 0 {
				if C.errno == C.EBUSY {
					// external debugger must be present
					exit(1)
				} else {
					// no external debugger
					exit(0)
				}
			}
		} else {
			mut status := 0
			// wait until the child process dies
			C.waitpid(pid, &status, 0)
			// check the exit code of the child process check
			if C.WEXITSTATUS(status) == 0 {
				return false
			} else {
				return true
			}
		}
	}
	return false
}
