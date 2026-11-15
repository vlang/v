// A FastC "unit zygote" compiles the translation units of one program with a
// single parse of their shared head.
//
// The compiler forks the zygote while it is still small. The zygote waits for a
// job on its control pipe: the TinyCC options, and then the C text that all
// units share. That text ends in a `#warning` line that carries
// v_fastc_zygote_marker, the number of units and the prefix of their object
// paths. TinyCC reports the warning through its error callback in the middle of
// the translation unit, with every declaration of the head parsed, and the
// callback forks one process per remaining unit there. Each process then
// replaces its standard input with its own unit's pipe, so TinyCC goes on
// reading only that unit's definitions and writes that unit's object.
//
// The head may arrive long before its marker: a zygote can parse the head of
// the previous build while the compiler generates the next one, and is given
// the marker only when both heads turn out to be the same text.
//
// No V code runs in these processes: only this file, TinyCC and the C library.
#include <libtcc.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#define V_FASTC_ZYGOTE_MAX_UNITS 64
#define V_FASTC_ZYGOTE_MAGIC 0x7a79676fu

static const char v_fastc_zygote_marker[] = "__v_fastc_zygote_units";

typedef struct {
	int unit_count;
	int unit_index;
	int forked;
	int fork_failed;
	int child_count;
	int body_fds[V_FASTC_ZYGOTE_MAX_UNITS];
	pid_t children[V_FASTC_ZYGOTE_MAX_UNITS];
	// Unit N is written to "<object_prefix>N.o".
	char object_prefix[1024];
} v_fastc_zygote_state;

static v_fastc_zygote_state v_fastc_zygote;

static void v_fastc_zygote_cloexec(int fd) {
	int flags = fcntl(fd, F_GETFD);
	if (flags >= 0) {
		fcntl(fd, F_SETFD, flags | FD_CLOEXEC);
	}
}

static int v_fastc_zygote_read_all(int fd, void *data, size_t len) {
	char *at = (char *)data;
	while (len > 0) {
		ssize_t got = read(fd, at, len);
		if (got > 0) {
			at += got;
			len -= (size_t)got;
		} else if (got < 0 && errno == EINTR) {
			continue;
		} else {
			return 0;
		}
	}
	return 1;
}

static void v_fastc_zygote_write_all(int fd, const char *data, size_t len) {
	while (len > 0) {
		ssize_t written = write(fd, data, len);
		if (written > 0) {
			data += written;
			len -= (size_t)written;
		} else if (written < 0 && errno == EINTR) {
			continue;
		} else {
			return;
		}
	}
}

static void v_fastc_zygote_report(const char *message) {
	v_fastc_zygote_write_all(STDERR_FILENO, message, strlen(message));
	v_fastc_zygote_write_all(STDERR_FILENO, "\n", 1);
}

// v_fastc_zygote_units reads "<count> <object prefix>" after the marker.
static int v_fastc_zygote_units(const char *text) {
	while (*text == ' ') {
		text++;
	}
	int count = 0;
	while (*text >= '0' && *text <= '9' && count <= V_FASTC_ZYGOTE_MAX_UNITS) {
		count = count * 10 + (*text - '0');
		text++;
	}
	if (count < 1 || count > V_FASTC_ZYGOTE_MAX_UNITS || *text != ' ') {
		return 0;
	}
	text++;
	size_t len = strlen(text);
	while (len > 0 && (text[len - 1] == ' ' || text[len - 1] == '\r' || text[len - 1] == '\n')) {
		len--;
	}
	if (len == 0 || len >= sizeof(v_fastc_zygote.object_prefix)) {
		return 0;
	}
	memcpy(v_fastc_zygote.object_prefix, text, len);
	v_fastc_zygote.object_prefix[len] = 0;
	return count;
}

// v_fastc_zygote_diagnostic receives every TinyCC warning and error. The marker
// warning is the point at which the units part.
static void v_fastc_zygote_diagnostic(void *opaque, const char *message) {
	(void)opaque;
	const char *marker = v_fastc_zygote.forked ? NULL : strstr(message, v_fastc_zygote_marker);
	if (marker != NULL) {
		v_fastc_zygote.forked = 1;
		v_fastc_zygote.unit_count = v_fastc_zygote_units(marker + sizeof(v_fastc_zygote_marker) - 1);
		for (int unit = 1; unit < v_fastc_zygote.unit_count; unit++) {
			pid_t pid = fork();
			if (pid < 0) {
				v_fastc_zygote.fork_failed = 1;
				break;
			}
			if (pid == 0) {
				v_fastc_zygote.unit_index = unit;
				v_fastc_zygote.child_count = 0;
				break;
			}
			v_fastc_zygote.children[v_fastc_zygote.child_count++] = pid;
		}
		// The rest of this translation unit is this process's own unit. The other
		// units' pipes are closed so that a unit that fails leaves its writer
		// with a closed pipe instead of a full one.
		if (v_fastc_zygote.unit_count > 0) {
			dup2(v_fastc_zygote.body_fds[v_fastc_zygote.unit_index], STDIN_FILENO);
		}
		for (int unit = 0; unit < V_FASTC_ZYGOTE_MAX_UNITS; unit++) {
			if (v_fastc_zygote.body_fds[unit] >= 0) {
				close(v_fastc_zygote.body_fds[unit]);
				v_fastc_zygote.body_fds[unit] = -1;
			}
		}
		return;
	}
	v_fastc_zygote_report(message);
}

static char *v_fastc_zygote_read_text(int fd) {
	uint32_t len = 0;
	if (!v_fastc_zygote_read_all(fd, &len, sizeof(len))) {
		return NULL;
	}
	char *text = (char *)malloc((size_t)len + 1);
	if (text == NULL || !v_fastc_zygote_read_all(fd, text, len)) {
		return NULL;
	}
	text[len] = 0;
	return text;
}

// v_fastc_zygote_environment applies the search paths that the tcc executable
// reads from its environment.
static void v_fastc_zygote_environment(TCCState *state) {
	const char *path = getenv("C_INCLUDE_PATH");
	if (path != NULL) {
		tcc_add_sysinclude_path(state, path);
	}
	path = getenv("CPATH");
	if (path != NULL) {
		tcc_add_include_path(state, path);
	}
}

static void v_fastc_zygote_run(int control_fd, int diagnostics_fd, int done_fd) {
	uint32_t magic = 0;
	// A build that needs no zygote closes the control pipe without a job.
	if (!v_fastc_zygote_read_all(control_fd, &magic, sizeof(magic))
		|| magic != V_FASTC_ZYGOTE_MAGIC) {
		_exit(0);
	}
	dup2(diagnostics_fd, STDOUT_FILENO);
	dup2(diagnostics_fd, STDERR_FILENO);
	close(diagnostics_fd);
	char *options = v_fastc_zygote_read_text(control_fd);
	if (options == NULL) {
		v_fastc_zygote_report("FastC unit zygote: incomplete job");
		_exit(1);
	}
	// The shared head follows the job on the control pipe.
	dup2(control_fd, STDIN_FILENO);
	close(control_fd);
	TCCState *state = tcc_new();
	if (state == NULL) {
		v_fastc_zygote_report("FastC unit zygote: could not create a TinyCC state");
		_exit(1);
	}
	tcc_set_error_func(state, NULL, v_fastc_zygote_diagnostic);
	int failed = tcc_set_options(state, options) != 0;
	if (!failed) {
		v_fastc_zygote_environment(state);
		failed = tcc_set_output_type(state, TCC_OUTPUT_OBJ) != 0;
	}
	if (!failed) {
		failed = tcc_add_file(state, "-") != 0;
	}
	if (!failed && (!v_fastc_zygote.forked || v_fastc_zygote.unit_count == 0)) {
		v_fastc_zygote_report("FastC unit zygote: the shared declarations did not reach the units");
		failed = 1;
	}
	if (!failed && v_fastc_zygote.fork_failed) {
		v_fastc_zygote_report("FastC unit zygote: could not start a process for every unit");
		failed = 1;
	}
	if (!failed) {
		char object[sizeof(v_fastc_zygote.object_prefix) + 16];
		snprintf(object, sizeof(object), "%s%d.o", v_fastc_zygote.object_prefix, v_fastc_zygote.unit_index);
		failed = tcc_output_file(state, object) != 0;
	}
	if (!failed) {
		unsigned char unit = (unsigned char)v_fastc_zygote.unit_index;
		v_fastc_zygote_write_all(done_fd, (const char *)&unit, 1);
	}
	for (int child = 0; child < v_fastc_zygote.child_count; child++) {
		int status = 0;
		while (waitpid(v_fastc_zygote.children[child], &status, 0) < 0 && errno == EINTR) {
		}
		if (status != 0) {
			failed = 1;
		}
	}
	_exit(failed ? 1 : 0);
}

// v_fastc_zygote_start forks the unit zygote. It returns the zygote's process
// id and the parent's ends of its pipes, or -1 when it could not be started.
int v_fastc_zygote_start(int max_units, int *control_fd, int *diagnostics_fd, int *done_fd, int *body_fds) {
	if (max_units < 2 || max_units > V_FASTC_ZYGOTE_MAX_UNITS) {
		return -1;
	}
	// Pipe 0 carries the job and the shared head, 1 the diagnostics, 2 one byte
	// per finished unit, and the rest one unit each.
	int pipes[V_FASTC_ZYGOTE_MAX_UNITS + 3][2];
	int pipe_count = max_units + 3;
	for (int i = 0; i < pipe_count; i++) {
		if (pipe(pipes[i]) != 0) {
			for (int j = 0; j < i; j++) {
				close(pipes[j][0]);
				close(pipes[j][1]);
			}
			return -1;
		}
		// No other process that the compiler starts may hold these pipes open.
		v_fastc_zygote_cloexec(pipes[i][0]);
		v_fastc_zygote_cloexec(pipes[i][1]);
	}
	pid_t pid = fork();
	if (pid < 0) {
		for (int i = 0; i < pipe_count; i++) {
			close(pipes[i][0]);
			close(pipes[i][1]);
		}
		return -1;
	}
	if (pid == 0) {
		// The compiler's signal handlers are V code, which does not run here.
		for (int sig = 1; sig < NSIG; sig++) {
			if (sig != SIGKILL && sig != SIGSTOP) {
				signal(sig, SIG_DFL);
			}
		}
		sigset_t no_signals;
		sigemptyset(&no_signals);
		sigprocmask(SIG_SETMASK, &no_signals, NULL);
		for (int unit = 0; unit < V_FASTC_ZYGOTE_MAX_UNITS; unit++) {
			v_fastc_zygote.body_fds[unit] = -1;
		}
		for (int unit = 0; unit < max_units; unit++) {
			v_fastc_zygote.body_fds[unit] = pipes[unit + 3][0];
		}
		// The copy keeps the standard streams and its own ends of these pipes. An
		// open copy of anything else, such as another zygote's pipes, would keep
		// that pipe's reader from seeing its end.
		int limit = getdtablesize();
		if (limit > 4096) {
			limit = 4096;
		}
		for (int fd = 3; fd < limit; fd++) {
			int keep = fd == pipes[0][0] || fd == pipes[1][1] || fd == pipes[2][1];
			for (int unit = 0; !keep && unit < max_units; unit++) {
				keep = fd == pipes[unit + 3][0];
			}
			if (!keep) {
				close(fd);
			}
		}
		v_fastc_zygote_run(pipes[0][0], pipes[1][1], pipes[2][1]);
		_exit(1);
	}
	close(pipes[0][0]);
	close(pipes[1][1]);
	close(pipes[2][1]);
	// A unit that fails early closes its pipe: its writer sees EPIPE, not SIGPIPE.
	fcntl(pipes[0][1], F_SETNOSIGPIPE, 1);
	*control_fd = pipes[0][1];
	*diagnostics_fd = pipes[1][0];
	*done_fd = pipes[2][0];
	for (int unit = 0; unit < max_units; unit++) {
		close(pipes[unit + 3][0]);
		fcntl(pipes[unit + 3][1], F_SETNOSIGPIPE, 1);
		body_fds[unit] = pipes[unit + 3][1];
	}
	return (int)pid;
}

// v_fastc_zygote_kill ends a zygote that was not given its marker, and reaps it.
void v_fastc_zygote_kill(int pid) {
	int status = 0;
	kill((pid_t)pid, SIGKILL);
	while (waitpid((pid_t)pid, &status, 0) < 0 && errno == EINTR) {
	}
}

// v_fastc_zygote_wait reaps the zygote and returns its exit code.
int v_fastc_zygote_wait(int pid) {
	int status = 0;
	while (waitpid((pid_t)pid, &status, 0) < 0) {
		if (errno != EINTR) {
			return -1;
		}
	}
	if ((status & 0x7f) == 0) {
		return (status >> 8) & 0xff;
	}
	return 128 + (status & 0x7f);
}
