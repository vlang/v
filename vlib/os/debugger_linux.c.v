module os

#include <sys/ptrace.h>

fn C.ptrace(u32, u32, voidptr, voidptr) u64

// debugger_present returns a bool indicating if the process is being debugged
pub fn debugger_present() bool {
	$if cross ? {
		return false
	}
	$if !cross ? {
		$if linux {
			// check if a child process could trace its parent process,
			// if not a debugger must be present
			pid := fork()
			if pid == 0 {
				ppid := getppid()
				if C.ptrace(C.PTRACE_ATTACH, ppid, 0, 0) == 0 {
					C.waitpid(ppid, 0, 0)

					// detach ptrace, otherwise further checks would indicate a debugger is present (ptrace is the Debugger then)
					C.ptrace(C.PTRACE_DETACH, ppid, 0, 0)

					// no external debugger
					exit(0)
				} else {
					// an error occured, a external debugger must be present
					exit(1)
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
	}
	return false
}
