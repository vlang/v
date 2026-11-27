#ifndef _WIN32
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <string.h>

// v_cmdexec_child_has_exited tells whether the child `pid` has ended, WITHOUT reaping it:
// 1 when it has, 0 when it is still running, -1 when that cannot be told (it is not a
// child of this process, it was reaped already, or the system has no waitid/WNOWAIT).
// A child that has ended and was not reaped keeps its pid and, as the leader of a process
// group, the group's id: a signal sent to the group afterwards cannot reach a stranger.
// waitpid(WNOHANG), which os.Process.is_alive uses, reaps what it finds.
int v_cmdexec_child_has_exited(int pid) {
// (P_PID is an enumerator in glibc and musl, not a macro: it cannot be asked for here.)
#if defined(WNOWAIT) && defined(WEXITED)
	siginfo_t info;
	// With WNOHANG and nothing to report, waitid returns 0 and older systems leave the
	// structure as it was: a zeroed si_pid then says "still running".
	memset(&info, 0, sizeof(info));
	if (waitid(P_PID, (id_t)pid, &info, WEXITED | WNOHANG | WNOWAIT) != 0) {
		return -1;
	}
	return info.si_pid == (pid_t)pid ? 1 : 0;
#else
	(void)pid;
	return -1;
#endif
}

#endif
