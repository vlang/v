extern char **environ;

static int v_os_execute_capture_start(const char *cmd, int *child_pid, int *read_fd) {
	int pipefd[2];
	if (pipe(pipefd) != 0) {
		return -1;
	}
	posix_spawn_file_actions_t actions;
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
