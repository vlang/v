// posix_spawn_nix.c is compiled by V as a separate object (see process_nix.c.v). The
// <spawn.h> include has to live in this file and not in a `.c.v` file: an `#include` in any
// module's V source makes V emit its whole platform header block (<sys/poll.h> among it)
// into every program that imports the module, and that breaks programs that declare their
// own `struct pollfd` / `poll` (vlib/v/cmdexec/cmdexec_poll_test.v).
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <spawn.h>
#include <stddef.h>
#include <sys/types.h>
#include <unistd.h>

// v_os_posix_spawn starts `exe` with posix_spawn and returns the child pid, or -1 when the
// spawn was refused and the caller has to fork instead.
//   use_pgroup:   the child leads its own process group
//   stdin_path:   when not NULL, the child's stdin is that file opened read-only
//   redirect:     when not 0, dup2 stdin_fd (unless stdin_path is set), out_fd, err_fd onto 0, 1, 2
//   close_fds:    descriptors above 2 that the child must not inherit (n_close entries)
int v_os_posix_spawn(const char* exe, char* const* argv, char* const* envp, int use_pgroup,
                     const char* stdin_path, int redirect, int stdin_fd, int out_fd, int err_fd,
                     const int* close_fds, int n_close) {
	posix_spawn_file_actions_t actions;
	posix_spawnattr_t attr;
	if (posix_spawn_file_actions_init(&actions) != 0) {
		return -1;
	}
	if (posix_spawnattr_init(&attr) != 0) {
		posix_spawn_file_actions_destroy(&actions);
		return -1;
	}
	int ok = 1;
	if (use_pgroup) {
		ok = posix_spawnattr_setflags(&attr, POSIX_SPAWN_SETPGROUP) == 0 &&
		     posix_spawnattr_setpgroup(&attr, 0) == 0;
	}
	if (ok && stdin_path != NULL) {
		ok = posix_spawn_file_actions_addopen(&actions, 0, stdin_path, O_RDONLY, 0) == 0;
	}
	if (ok && redirect) {
		if (stdin_path == NULL) {
			ok = posix_spawn_file_actions_adddup2(&actions, stdin_fd, 0) == 0;
		}
		ok = ok && posix_spawn_file_actions_adddup2(&actions, out_fd, 1) == 0;
		ok = ok && posix_spawn_file_actions_adddup2(&actions, err_fd, 2) == 0;
		for (int i = 0; ok && i < n_close; i++) {
			ok = posix_spawn_file_actions_addclose(&actions, close_fds[i]) == 0;
		}
	}
	pid_t child = 0;
	if (ok) {
		ok = posix_spawn(&child, exe, &actions, &attr, argv, envp) == 0;
	}
	posix_spawn_file_actions_destroy(&actions);
	posix_spawnattr_destroy(&attr);
	return ok ? (int)child : -1;
}
