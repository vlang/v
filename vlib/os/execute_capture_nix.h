// v_os_execute_set_cloexec marks an fd as close-on-exec. When multiple threads
// each call os.execute, every pipe() they create is briefly visible to all of
// them; without FD_CLOEXEC, one thread's spawned child can inherit another
// thread's pipe write end, keeping that pipe open after the intended writer
// exits. The reader then never observes EOF and the captured output ends up
// empty (seen on macOS arm64 in CI). Setting CLOEXEC closes the fd
// automatically across exec, which fixes the race for both fork/execvp and
// posix_spawn paths.
static void v_os_execute_set_cloexec(int fd) {
	int flags = fcntl(fd, F_GETFD, 0);
	if (flags >= 0) {
		fcntl(fd, F_SETFD, flags | FD_CLOEXEC);
	}
}

#if defined(__ANDROID__) && (!defined(__ANDROID_API__) || __ANDROID_API__ < 28)
// Android API levels below 28 do not provide posix_spawn(). Fall back to
// fork()/execvp() with a pipe; this is what popen() does internally and
// is sufficient for capturing the merged stdout+stderr of a shell command.
static int v_os_execute_capture_start(const char *cmd, int *child_pid, int *read_fd) {
	int pipefd[2];
	if (pipe(pipefd) != 0) {
		return -1;
	}
	v_os_execute_set_cloexec(pipefd[0]);
	v_os_execute_set_cloexec(pipefd[1]);
	pid_t pid = fork();
	if (pid < 0) {
		close(pipefd[0]);
		close(pipefd[1]);
		return -1;
	}
	if (pid == 0) {
		// child: redirect stdout+stderr to the pipe, then exec the shell.
		// dup2 clears FD_CLOEXEC on the destination, so STDOUT/STDERR stay
		// open across exec while the original pipe fds are auto-closed.
		dup2(pipefd[1], STDOUT_FILENO);
		dup2(pipefd[1], STDERR_FILENO);
		close(pipefd[0]);
		close(pipefd[1]);
		execlp("sh", "sh", "-c", cmd, (char *)NULL);
		_exit(127);
	}
	close(pipefd[1]);
	*child_pid = (int)pid;
	*read_fd = pipefd[0];
	return 0;
}
#else
// Use opaque void* declarations for posix_spawn instead of #include <spawn.h>.
// Including <spawn.h> transitively pulls in <features.h>/<sys/cdefs.h>, which
// breaks under musl-gcc on the Ubuntu docker image where <sys/cdefs.h> is not
// on the include path. The 128-byte buffer is comfortably larger than the
// real posix_spawn_file_actions_t on glibc (~80 bytes) and musl (~40 bytes);
// posix_spawn_file_actions_init() initializes the buffer, so its true layout
// is not needed here. We pass NULL for posix_spawnattr_t, so it is not
// declared. Calling these via void* is ABI-compatible: pointer parameters
// are passed identically regardless of pointee type.
typedef struct { unsigned char _opaque[128]; } v_posix_spawn_file_actions_t;
extern int posix_spawn(pid_t *, const char *, const void *, const void *, char *const [], char *const []);
extern int posix_spawn_file_actions_init(void *);
extern int posix_spawn_file_actions_destroy(void *);
extern int posix_spawn_file_actions_adddup2(void *, int, int);
extern int posix_spawn_file_actions_addclose(void *, int);

extern char **environ;

static int v_os_execute_capture_start(const char *cmd, int *child_pid, int *read_fd) {
	int pipefd[2];
	if (pipe(pipefd) != 0) {
		return -1;
	}
	v_os_execute_set_cloexec(pipefd[0]);
	v_os_execute_set_cloexec(pipefd[1]);
	v_posix_spawn_file_actions_t actions;
	if (posix_spawn_file_actions_init(&actions) != 0) {
		close(pipefd[0]);
		close(pipefd[1]);
		return -1;
	}
	int err = 0;
	if ((err = posix_spawn_file_actions_adddup2(&actions, pipefd[1], STDOUT_FILENO)) != 0
		|| (err = posix_spawn_file_actions_adddup2(&actions, pipefd[1], STDERR_FILENO)) != 0
		|| (err = posix_spawn_file_actions_addclose(&actions, pipefd[0])) != 0
		|| (err = posix_spawn_file_actions_addclose(&actions, pipefd[1])) != 0) {
		posix_spawn_file_actions_destroy(&actions);
		close(pipefd[0]);
		close(pipefd[1]);
		return -1;
	}
	char *const argv[] = {(char *)"/bin/sh", (char *)"-c", (char *)cmd, NULL};
	err = posix_spawn(child_pid, "/bin/sh", &actions, NULL, argv, environ);
	posix_spawn_file_actions_destroy(&actions);
	if (err != 0) {
		close(pipefd[0]);
		close(pipefd[1]);
		return -1;
	}
	close(pipefd[1]);
	*read_fd = pipefd[0];
	return 0;
}
#endif
