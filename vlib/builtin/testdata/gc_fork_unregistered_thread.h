// A thread the collector never registered forks the process, the way a C library does
// on a thread it created itself (libdbus autolaunching the session bus on a libmpv
// thread). gc.h redirects pthread_create and pthread_join to the collector's wrappers,
// which would register the thread, so this header calls the real ones.
#include <pthread.h>
#include <signal.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#undef pthread_create
#undef pthread_join

// 0 ok, 2 child hung, 3 child killed by a signal, 4 fork failed, else the child's exit code.
static int gc_fork_unregistered_verdict = -1;

static void *gc_fork_unregistered_thread(void *arg) {
	(void)arg;
	pid_t pid = fork();
	if (pid < 0) {
		gc_fork_unregistered_verdict = 4;
		return NULL;
	}
	if (pid == 0) {
		_exit(0);
	}
	for (int i = 0; i < 500; i++) {
		int status = 0;
		if (waitpid(pid, &status, WNOHANG) == pid) {
			gc_fork_unregistered_verdict = WIFSIGNALED(status) ? 3 : WEXITSTATUS(status);
			return NULL;
		}
		struct timespec pause = {0, 10 * 1000 * 1000};
		nanosleep(&pause, NULL);
	}
	kill(pid, SIGKILL);
	waitpid(pid, NULL, 0);
	gc_fork_unregistered_verdict = 2;
	return NULL;
}

static int gc_fork_from_unregistered_thread(void) {
	pthread_t thread;
	if (pthread_create(&thread, NULL, gc_fork_unregistered_thread, NULL) != 0) {
		return 4;
	}
	pthread_join(thread, NULL);
	return gc_fork_unregistered_verdict;
}
