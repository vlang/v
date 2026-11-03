module main

import os
import time

fn test_run_and_inline_forward_termination_and_clean_files() ! {
	for signal_number in [1, 15] {
		for inline in [false, true] {
			root := os.join_path(os.vtmp_dir(), 'run_signal_${os.getpid()}_${time.now().unix_nano()}')
			os.mkdir_all(root)!
			defer { os.rmdir_all(root) or {} }
			ready := os.join_path(root, 'ready')
			program := "import os\nimport time\nfn main() { os.write_file('${ready}', os.getpid().str()) or { panic(err) }; time.sleep(30 * time.second) }\n"
			mut args := ['-nocache', '-gc', 'none']
			if inline {
				args << ['-e', program]
			} else {
				source := os.join_path(root, 'main.v')
				os.write_file(source, program)!
				args << ['run', source]
			}
			mut process := os.new_process(@VEXE)
			process.set_args(args)
			process.set_work_folder(root)
			mut environment := os.environ()
			environment.delete('VEXE')
			environment.delete('VFLAGS')
			environment['V_MACOS_V3_NO_FALLBACK'] = '1'
			process.set_environment(environment)
			process.set_redirect_stdio()
			process.run()
			defer {
				if process.is_alive() { process.signal_kill() }
				process.wait()
				process.close()
			}
			deadline := time.now().add(45 * time.second)
			for !os.exists(ready) && process.is_alive() && time.now() < deadline {
				time.sleep(10 * time.millisecond)
			}
			assert os.exists(ready), 'child did not become ready'
			child_pid := os.read_file(ready)!.int()
			mut child_completed := false
			defer {
				if !child_completed { C.kill(child_pid, 9) }
			}
			assert C.kill(process.pid, signal_number) == 0
			stop_deadline := time.now().add(10 * time.second)
			for process.is_alive() && time.now() < stop_deadline {
				time.sleep(10 * time.millisecond)
			}
			assert !process.is_alive(), 'compiler did not stop after signal ${signal_number}'
			process.wait()
			assert process.code == 128 + signal_number, process.stderr_slurp()
			assert C.kill(child_pid, 0) == -1, 'run child still exists'
			child_completed = true
			for entry in os.ls(root)! {
				assert !entry.starts_with('.v3_inline_'), entry
				assert entry != 'main', 'implicit executable survived termination'
			}
		}
	}
}

fn test_run_signal_helper_has_a_portable_windows_stub() {
	cc := os.find_abs_path_of_executable('clang') or { return }
	source := os.join_path(@VEXEROOT, 'vlib', 'v', 'driver', 'run_signals_nix.c')
	result := os.exec([cc, '-D_WIN32', '-Wall', '-Wextra', '-Werror', '-fsyntax-only', source])
	assert result.exit_code == 0, result.output
}

fn test_run_signal_forwarded_before_exec_terminates_child() ! {
	cc := os.find_abs_path_of_executable('clang') or {
		os.find_abs_path_of_executable('cc') or { return }
	}
	root := os.join_path(os.vtmp_dir(), 'run_signal_preexec_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'preexec.c')
	binary := os.join_path(root, 'preexec')
	os.write_file(source, run_signal_preexec_harness)!
	helper := os.join_path(@VEXEROOT, 'vlib', 'v', 'driver', 'run_signals_nix.c')
	compiled := os.exec([cc, '-Wall', '-Wextra', '-Werror', '-o', binary, source, helper])
	assert compiled.exit_code == 0, compiled.output
	for signal_number in [1, 15] {
		result := os.exec([binary, signal_number.str()])
		assert result.exit_code == 0, result.output
	}
}

// Keep the child between fork and exec so signal forwarding exercises its inherited handler.
// The parent's alarm kills and reaps a stuck child, making failures bounded.
const run_signal_preexec_harness = r'
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int v3_run_signals_begin(void);
int v3_run_signals_set_pid(int child_pid);
int v3_run_signals_finish(void);
static volatile sig_atomic_t timeout_child;
static volatile sig_atomic_t timed_out;
static void timeout_cleanup(int signo) {
    (void)signo;
    timed_out = 1;
    if (timeout_child > 0) kill((int)timeout_child, SIGKILL);
}
int main(int argc, char **argv) {
    int signo = argc > 1 ? atoi(argv[1]) : SIGTERM;
    int ready[2], gate[2];
    if (pipe(ready) || pipe(gate) || v3_run_signals_begin()) return 3;
    pid_t child = fork();
    if (child < 0) return 4;
    if (child == 0) {
        unsigned char byte = 1;
        close(ready[0]);
        close(gate[1]);
        if (write(ready[1], &byte, 1) != 1) _exit(5);
        close(ready[1]);
        while (read(gate[0], &byte, 1) < 0 && errno == EINTR) {}
        execl("/bin/sh", "sh", "-c", "exit 99", (char *)0);
        _exit(6);
    }
    close(ready[1]);
    close(gate[0]);
    timeout_child = child;
    signal(SIGALRM, timeout_cleanup);
    alarm(2);
    unsigned char byte;
    ssize_t read_result;
    do { read_result = read(ready[0], &byte, 1); } while (read_result < 0 && errno == EINTR);
    close(ready[0]);
    v3_run_signals_set_pid((int)child);
    if (read_result != 1 || kill(getpid(), signo)) kill(child, SIGKILL);
    int status;
    pid_t waited;
    do { waited = waitpid(child, &status, 0); } while (waited < 0 && errno == EINTR);
    alarm(0);
    timeout_child = 0;
    close(gate[1]);
    int received = v3_run_signals_finish();
    if (timed_out || waited != child || received != signo || !WIFEXITED(status)
        || WEXITSTATUS(status) != 128 + signo) {
        fprintf(stderr, "signal=%d timeout=%d status=%d received=%d\n", signo, timed_out, status, received);
        return 1;
    }
    return 0;
}
'

fn test_run_signal_dispositions_restart_and_finish() ! {
	cc := os.find_abs_path_of_executable('clang') or {
		os.find_abs_path_of_executable('cc') or { return }
	}
	root := os.join_path(os.vtmp_dir(), 'run_signal_actions_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'actions.c')
	binary := os.join_path(root, 'actions')
	helper := os.join_path(@VEXEROOT, 'vlib', 'v', 'driver', 'run_signals_nix.c')
	os.write_file(source, run_signal_actions_harness.replace('@RUN_SIGNAL_HELPER@', helper))!
	for mode in ['', '-DV3_TEST_HEADERLESS'] {
		mut args := [cc, '-Wall', '-Wextra', '-Werror', '-o', binary, source]
		if mode != '' {
			args << mode
		}
		compiled := os.exec(args)
		assert compiled.exit_code == 0, compiled.output
		for check in ['ignore-hup', 'ignore-term', 'ignore-int', 'ignore-quit', 'ignore-tstp',
			'ignore-ttin', 'ignore-ttou', 'pending', 'restart', 'finish', 'group-hup', 'group-term',
			'group-int', 'group-quit', 'debugger-stop', 'setup-retry', 'setup-rejected', 'setup-failure'] {
			result := os.exec([binary, check])
			assert result.exit_code == 0, '${mode} ${check}: ${result.output}'
		}
	}
}

// Interpose restoration to deliver SIGTERM exactly inside the old finish race.
// An alarm kills and reaps either child if a wait or signal regression stalls.
const run_signal_actions_harness = r'
#define _DEFAULT_SOURCE 1
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

static volatile sig_atomic_t previous_calls, child_signal, inject_finish, timed_out;
static volatile sig_atomic_t timeout_child, timeout_sender;
static void previous_handler(int signo) { (void)signo; previous_calls++; }
static void child_handler(int signo) { child_signal = signo; }
static void timeout_cleanup(int signo) {
    (void)signo;
    timed_out = 1;
    if (timeout_child > 0) kill((int)timeout_child, SIGKILL);
    if (timeout_sender > 0) kill((int)timeout_sender, SIGKILL);
}
static int fail_pipe;
static int test_pipe(int fds[2]) {
    if (fail_pipe) { errno = EMFILE; return -1; }
    return pipe(fds);
}
static int stop_notification = -1;
static int fail_setpgid;
static int test_setpgid(pid_t child, pid_t group) {
    if (child > 0 && fail_setpgid > 0) {
        fail_setpgid--;
        errno = EPERM;
        return -1;
    }
    return setpgid(child, group);
}
static pid_t test_waitpid(pid_t child, int *status, int options) {
    pid_t result = waitpid(child, status, options);
    if (result > 0 && WIFSTOPPED(*status) && stop_notification >= 0) {
        unsigned char byte = 1;
        write(stop_notification, &byte, 1);
    }
    return result;
}
static int test_sigaction(int signo, const struct sigaction *action, struct sigaction *previous) {
    if (inject_finish && signo == SIGTERM && action && action->sa_handler == previous_handler) {
        inject_finish = 0;
        if (kill(getpid(), SIGTERM)) return -1;
    }
    return sigaction(signo, action, previous);
}
#ifdef V3_TEST_HEADERLESS
#define V3_RUN_SIGNALS_HEADERLESS 1
static int test_sigaction_opaque(int signo, const void *action, void *previous) {
    return test_sigaction(signo, action, previous);
}
#define v3_run_sigaction test_sigaction_opaque
#ifdef __APPLE__
int test_siginterrupt(int, int) __asm__("_siginterrupt");
#else
int test_siginterrupt(int, int) __asm__("siginterrupt");
#endif
#define v3_run_siginterrupt test_siginterrupt
#define v3_run_sigprocmask sigprocmask
#define v3_run_sigemptyset sigemptyset
#define v3_run_sigaddset sigaddset
#define v3_run_setpgid test_setpgid
#define v3_run_getpgid getpgid
#define v3_run_getpgrp getpgrp
#define v3_run_tcgetpgrp tcgetpgrp
#define v3_run_tcsetpgrp tcsetpgrp
#define v3_run_open open
#define v3_run_close close
#define v3_run_fcntl fcntl
#define v3_run_waitpid test_waitpid
#define v3_run_fork fork
#define v3_run_pipe test_pipe
#define v3_run_read read
#define v3_run_write write
#else
#define sigaction(...) test_sigaction(__VA_ARGS__)
#define waitpid(...) test_waitpid(__VA_ARGS__)
#define pipe(...) test_pipe(__VA_ARGS__)
#define setpgid(...) test_setpgid(__VA_ARGS__)
#endif
#include "@RUN_SIGNAL_HELPER@"
#undef sigaction
#undef waitpid
#undef pipe
#undef setpgid
#ifdef V3_TEST_HEADERLESS
_Static_assert(sizeof(struct sigaction) <= sizeof(v3_run_action), "sigaction storage too small");
_Static_assert(__alignof__(struct sigaction) <= __alignof__(v3_run_action), "sigaction alignment");
#endif

static pid_t reap(pid_t child, int *status) {
    pid_t result;
    do { result = waitpid(child, status, 0); } while (result < 0 && errno == EINTR);
    return result;
}
static int check_ignore(const char *binary, int signo) {
    struct sigaction action = {0};
    action.sa_handler = SIG_IGN;
    sigemptyset(&action.sa_mask);
    if (sigaction(signo, &action, 0) || v3_run_signals_begin()) return 3;
    if (sigaction(signo, 0, &action) || action.sa_handler != SIG_IGN) return 4;
    pid_t child = fork();
    if (child < 0) return 5;
    if (child == 0) {
        char number[16];
        snprintf(number, sizeof(number), "%d", signo);
        execl(binary, binary, "check-ignore", number, (char *)0);
        _exit(6);
    }
    timeout_child = child;
    int status = 0;
    pid_t waited = reap(child, &status);
    timeout_child = 0;
    int received = v3_run_signals_finish();
    return waited != child || !WIFEXITED(status) || WEXITSTATUS(status) || received;
}
static int check_pending(void) {
    struct sigaction action = {0};
    action.sa_handler = previous_handler;
    sigemptyset(&action.sa_mask);
    sigset_t block, previous_mask, pending;
    sigemptyset(&block);
    sigaddset(&block, SIGTERM);
    if (sigaction(SIGTERM, &action, 0) || sigprocmask(SIG_BLOCK, &block, &previous_mask)
        || kill(getpid(), SIGTERM) || v3_run_signals_begin()) return 3;
    if (sigpending(&pending) || !sigismember(&pending, SIGTERM)) return 4;
    if (sigprocmask(SIG_SETMASK, &previous_mask, 0)) return 5;
    return v3_run_signals_finish() != SIGTERM || previous_calls;
}
static volatile sig_atomic_t group_calls, continue_calls;
static int group_notification;
static void group_handler(int signo) {
    (void)signo;
    group_calls++;
}
static void continue_handler(int signo) { (void)signo; continue_calls++; }
static int read_exact(int fd, void *data, int size) {
    unsigned char *bytes = data;
    while (size > 0) {
        ssize_t count = read(fd, bytes, (size_t)size);
        if (count < 0 && errno == EINTR) continue;
        if (count <= 0) return -1;
        bytes += count;
        size -= (int)count;
    }
    return 0;
}
static int check_group(int signo) {
    int notification[2], command[2];
    struct sigaction action = {0};
    action.sa_handler = SIG_DFL;
    sigemptyset(&action.sa_mask);
    sigset_t blocked, original_mask;
    sigemptyset(&blocked);
    sigaddset(&blocked, signo);
    if (setpgid(0, 0) || sigaction(signo, &action, 0)
        || sigprocmask(SIG_BLOCK, &blocked, &original_mask) || pipe(notification)
        || pipe(command) || v3_run_signals_begin()) return 3;
    pid_t child = fork();
    if (child < 0) return 4;
    if (!child) {
        close(notification[0]); close(command[1]);
        group_notification = notification[1];
        action.sa_handler = group_handler;
        action.sa_flags = SA_RESTART;
        if (sigaction(signo, &action, 0)) _exit(5);
        action.sa_handler = continue_handler;
        if (sigaction(SIGCONT, &action, 0)) _exit(6);
        unsigned char byte = 0;
        write(group_notification, &byte, 1);
        if (read_exact(command[0], &byte, 1)) _exit(7);
        /* This barrier proves startup did not deliver an unsolicited SIGCONT. */
        byte = (unsigned char)continue_calls;
        write(group_notification, &byte, 1);
        if (read_exact(command[0], &byte, 1)) _exit(8);
        /* Drain direct group delivery before allowing the compiler to forward.
         * Separate unblock phases prevent standard-signal coalescing from hiding
         * the duplicate that a shared parent/child process group would deliver. */
        if (sigprocmask(SIG_UNBLOCK, &blocked, 0)
            || sigprocmask(SIG_BLOCK, &blocked, 0)) _exit(9);
        byte = (unsigned char)group_calls;
        write(group_notification, &byte, 1);
        if (read_exact(command[0], &byte, 1)
            || sigprocmask(SIG_UNBLOCK, &blocked, 0)) _exit(10);
        unsigned char counts[2] = {(unsigned char)group_calls, (unsigned char)continue_calls};
        write(group_notification, counts, sizeof(counts));
        _exit(42);
    }
    timeout_child = child;
    close(notification[1]); close(command[0]);
    unsigned char byte;
    if (read_exact(notification[0], &byte, 1) || v3_run_signals_set_pid((int)child)) return 11;
    int separate_group = getpgid(child) != getpgrp();
    byte = 0;
    if (write(command[1], &byte, 1) != 1 || read_exact(notification[0], &byte, 1) || byte) return 12;
    if (kill(-getpgrp(), signo) || write(command[1], &byte, 1) != 1
        || read_exact(notification[0], &byte, 1)) return 13;
    int direct_calls = byte;
    if (sigprocmask(SIG_UNBLOCK, &blocked, 0)
        || sigprocmask(SIG_BLOCK, &blocked, 0) || write(command[1], &byte, 1) != 1) return 14;
    unsigned char counts[2];
    if (read_exact(notification[0], counts, sizeof(counts))) return 15;
    int code, child_signal;
    if (v3_run_signals_wait(&code, &child_signal)) return 16;
    timeout_child = 0;
    int received = v3_run_signals_finish();
    if (sigprocmask(SIG_SETMASK, &original_mask, 0)) return 17;
    int expected_signal = signo == SIGHUP || signo == SIGTERM ? signo : 0;
    if (counts[0] != 1 || counts[1]) {
        fprintf(stderr, "direct=%d total=%d continues=%d\n", direct_calls, counts[0], counts[1]);
        return 18;
    }
    return !separate_group || code != 42 || child_signal || received != expected_signal;
}
/* A debugger stopping only the child must not strand a waiter without a tty. */
static int check_debugger_stop(void) {
    int report[2];
    if (pipe(report)) return 3;
    pid_t worker = fork();
    if (worker < 0) return 4;
    if (!worker) {
        close(report[0]);
        if (setsid() < 0 || v3_run_signals_begin()) _exit(5);
        pid_t child = fork();
        if (child < 0) _exit(6);
        if (!child) { raise(SIGSTOP); _exit(42); }
        if (write(report[1], &child, sizeof(child)) != sizeof(child)) _exit(7);
        stop_notification = report[1];
        if (v3_run_signals_set_pid((int)child)) _exit(8);
        int code, child_signal;
        if (v3_run_signals_wait(&code, &child_signal)) _exit(9);
        _exit(v3_run_signals_finish() || code != 42 || child_signal);
    }
    timeout_sender = worker;
    close(report[1]);
    pid_t child;
    unsigned char byte;
    if (read_exact(report[0], &child, sizeof(child))) return 10;
    timeout_child = child;
    if (read_exact(report[0], &byte, 1) || kill(child, SIGCONT)) return 11;
    int status;
    pid_t waited;
    do { waited = waitpid(worker, &status, WUNTRACED); } while (waited < 0 && errno == EINTR);
    int stopped = waited == worker && WIFSTOPPED(status);
    if (stopped) { kill(worker, SIGCONT); reap(worker, &status); }
    timeout_child = timeout_sender = 0;
    return stopped || waited != worker || !WIFEXITED(status) || WEXITSTATUS(status);
}
/* Keep the child in its inherited group until the parent completes setup.
 * One transient EPERM must retry; a repeated failure must remain unpublished. */
static int check_setup_retry(int failures) {
    int gate[2];
    if (pipe(gate) || v3_run_signals_begin()) return 3;
    pid_t child = fork();
    if (child < 0) return 4;
    if (!child) {
        close(gate[1]);
        unsigned char byte;
        _exit(read_exact(gate[0], &byte, 1) ? 5 : 42);
    }
    timeout_child = child;
    close(gate[0]);
    fail_setpgid = failures;
    int result = v3_run_signals_set_pid((int)child);
    fail_setpgid = 0;
    if (failures == 1) {
        if (result || getpgid(child) != child) return 6;
    } else if (!result || v3_run_child_pid != 0) {
        return 7;
    }
    unsigned char byte = 1;
    if (write(gate[1], &byte, 1) != 1) return 8;
    close(gate[1]);
    int code, child_signal;
    if (failures == 1) {
        if (v3_run_signals_wait(&code, &child_signal) || code != 42 || child_signal) return 9;
    } else {
        int status;
        if (reap(child, &status) != child || !WIFEXITED(status) || WEXITSTATUS(status) != 42) return 10;
    }
    timeout_child = 0;
    return v3_run_signals_finish();
}
/* Exhaust observer descriptors after the child has forked a descendant. Both
 * retain report[1], so EOF proves that group cleanup removed both processes. */
static int check_setup_failure(void) {
    int report[2];
    if (pipe(report) || v3_run_signals_begin()) return 3;
    pid_t child = fork();
    if (child < 0) return 4;
    if (!child) {
        close(report[0]);
        if (setpgid(0, 0)) _exit(5);
        pid_t descendant = fork();
        if (descendant < 0) _exit(6);
        if (descendant) write(report[1], &descendant, sizeof(descendant));
        for (;;) pause();
    }
    close(report[1]);
    timeout_child = child;
    pid_t descendant;
    if (read_exact(report[0], &descendant, sizeof(descendant))) return 7;
    timeout_sender = descendant;
    v3_run_handoff_terminal = 1;
    fail_pipe = 1;
    if (!v3_run_signals_set_pid((int)child)) return 8;
    v3_run_signals_kill((int)child);
    int status;
    if (reap(child, &status) != child) return 9;
    timeout_child = 0;
    unsigned char byte;
    long count;
    do { count = read(report[0], &byte, 1); } while (count < 0 && errno == EINTR);
    close(report[0]);
    timeout_sender = 0;
    return v3_run_signals_finish() || count != 0 || !WIFSIGNALED(status)
        || WTERMSIG(status) != SIGKILL;
}
static int check_restart(void) {
    int ready[2];
    if (pipe(ready) || v3_run_signals_begin()) return 3;
    struct sigaction action;
    if (sigaction(SIGTERM, 0, &action) || !(action.sa_flags & SA_RESTART)) return 4;
    pid_t child = fork();
    if (child < 0) return 5;
    if (child == 0) {
        action.sa_handler = child_handler;
        sigemptyset(&action.sa_mask);
        action.sa_flags = 0;
        sigset_t block, previous_mask;
        sigemptyset(&block);
        sigaddset(&block, SIGTERM);
        if (sigaction(SIGTERM, &action, 0) || sigprocmask(SIG_BLOCK, &block, &previous_mask)) _exit(6);
        close(ready[0]);
        if (write(ready[1], "r", 1) != 1) _exit(7);
        close(ready[1]);
        while (!child_signal) sigsuspend(&previous_mask);
        struct timespec delay = {0, 150000000};
        nanosleep(&delay, 0);
        _exit(42);
    }
    timeout_child = child;
    close(ready[1]);
    char byte;
    if (read(ready[0], &byte, 1) != 1) return 8;
    close(ready[0]);
    v3_run_signals_set_pid((int)child);
    pid_t sender = fork();
    if (sender < 0) return 9;
    if (sender == 0) {
        struct timespec delay = {0, 50000000};
        nanosleep(&delay, 0);
        kill(getppid(), SIGTERM);
        _exit(0);
    }
    timeout_sender = sender;
    int status = 0, sender_status;
    /* Deliberately wait once, just like os.Process.wait. SA_RESTART must keep waiting. */
    pid_t waited = waitpid(child, &status, 0);
    if (waited != child) { kill(child, SIGKILL); reap(child, &status); }
    timeout_child = 0;
    reap(sender, &sender_status);
    timeout_sender = 0;
    int received = v3_run_signals_finish();
    return waited != child || !WIFEXITED(status) || WEXITSTATUS(status) != 42 || received != SIGTERM;
}
static int check_finish(void) {
    struct sigaction action = {0}, restored;
    action.sa_handler = previous_handler;
    action.sa_flags = SA_RESTART;
    sigemptyset(&action.sa_mask);
    sigaddset(&action.sa_mask, SIGUSR1);
    sigset_t mask, restored_mask;
    sigemptyset(&mask);
    sigaddset(&mask, SIGUSR2);
    if (sigaction(SIGTERM, &action, 0) || sigprocmask(SIG_BLOCK, &mask, 0)
        || v3_run_signals_begin()) return 3;
    inject_finish = 1;
    int received = v3_run_signals_finish();
    if (sigaction(SIGTERM, 0, &restored) || sigprocmask(SIG_BLOCK, 0, &restored_mask)) return 4;
    return received || previous_calls != 1 || restored.sa_handler != previous_handler
        || !(restored.sa_flags & SA_RESTART) || !sigismember(&restored.sa_mask, SIGUSR1)
        || !sigismember(&restored_mask, SIGUSR2) || sigismember(&restored_mask, SIGTERM)
        || sigismember(&restored_mask, SIGHUP);
}
int main(int argc, char **argv) {
    if (argc > 2 && !strcmp(argv[1], "check-ignore")) {
        struct sigaction action;
        return sigaction(atoi(argv[2]), 0, &action) || action.sa_handler != SIG_IGN;
    }
    struct sigaction timeout = {0};
    timeout.sa_handler = timeout_cleanup;
    sigemptyset(&timeout.sa_mask);
    if (sigaction(SIGALRM, &timeout, 0)) return 2;
    alarm(3);
    int result = 2;
    if (argc > 1 && !strcmp(argv[1], "ignore-hup")) result = check_ignore(argv[0], SIGHUP);
    if (argc > 1 && !strcmp(argv[1], "ignore-term")) result = check_ignore(argv[0], SIGTERM);
    if (argc > 1 && !strcmp(argv[1], "ignore-int")) result = check_ignore(argv[0], SIGINT);
    if (argc > 1 && !strcmp(argv[1], "ignore-quit")) result = check_ignore(argv[0], SIGQUIT);
    if (argc > 1 && !strcmp(argv[1], "ignore-tstp")) result = check_ignore(argv[0], SIGTSTP);
    if (argc > 1 && !strcmp(argv[1], "ignore-ttin")) result = check_ignore(argv[0], SIGTTIN);
    if (argc > 1 && !strcmp(argv[1], "ignore-ttou")) result = check_ignore(argv[0], SIGTTOU);
    if (argc > 1 && !strcmp(argv[1], "group-hup")) result = check_group(SIGHUP);
    if (argc > 1 && !strcmp(argv[1], "group-term")) result = check_group(SIGTERM);
    if (argc > 1 && !strcmp(argv[1], "group-int")) result = check_group(SIGINT);
    if (argc > 1 && !strcmp(argv[1], "group-quit")) result = check_group(SIGQUIT);
    if (argc > 1 && !strcmp(argv[1], "debugger-stop")) result = check_debugger_stop();
    if (argc > 1 && !strcmp(argv[1], "setup-retry")) result = check_setup_retry(1);
    if (argc > 1 && !strcmp(argv[1], "setup-rejected")) result = check_setup_retry(2);
    if (argc > 1 && !strcmp(argv[1], "pending")) result = check_pending();
    if (argc > 1 && !strcmp(argv[1], "setup-failure")) result = check_setup_failure();
    if (argc > 1 && !strcmp(argv[1], "restart")) result = check_restart();
    if (argc > 1 && !strcmp(argv[1], "finish")) result = check_finish();
    alarm(0);
    if (timeout_child > 0) { kill((int)timeout_child, SIGKILL); waitpid((int)timeout_child, 0, 0); }
    if (timeout_sender > 0) { kill((int)timeout_sender, SIGKILL); waitpid((int)timeout_sender, 0, 0); }
    if (result || timed_out) fprintf(stderr, "result=%d timeout=%d previous_calls=%d\n", result, timed_out, previous_calls);
    return result || timed_out;
}
'

fn test_run_group_signal_reaches_nonterminating_child_once() ! {
	for signal_number in [1, 15] {
		root := os.join_path(os.vtmp_dir(), 'run_group_${os.getpid()}_${time.now().unix_nano()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		ready := os.join_path(root, 'ready')
		counts := os.join_path(root, 'counts')
		helper := os.join_path(root, 'counter.c')
		source := os.join_path(root, 'main.c.v')
		os.write_file(helper, run_signal_counter_source)!
		os.write_file(source, "import os\nimport time\n#flag ${helper}\nfn C.run_signal_counter_install(i32) i32\nfn C.run_signal_counter_get() i32\nfn C.run_signal_counter_continues() i32\nfn main() { assert C.run_signal_counter_install(i32(os.args[1].int())) == 0; os.write_file(os.args[2], os.getpid().str())!; _ = os.input(''); for C.run_signal_counter_get() == 0 { time.sleep(time.millisecond) }; os.write_file(os.args[3], C.run_signal_counter_get().str() + ',' + C.run_signal_counter_continues().str())! }\n")!
		mut process := os.new_process(@VEXE)
		process.use_pgroup = true
		process.set_work_folder(root)
		process.set_redirect_stdio()
		process.set_args(['-nocache', '-gc', 'none', 'run', source, signal_number.str(), ready,
			counts])
		process.run()
		mut child_pid := 0
		defer {
			if process.is_alive() { process.signal_pgkill() }
			if child_pid > 0 { C.kill(-child_pid, 9) }
			process.wait()
			process.close()
		}
		deadline := time.now().add(45 * time.second)
		for !os.exists(ready) && process.is_alive() && time.now() < deadline {
			time.sleep(10 * time.millisecond)
		}
		assert os.exists(ready), 'run process did not become ready'
		child_pid = os.read_file(ready)!.int()
		assert C.kill(-process.pid, signal_number) == 0
		process.stdin_write('done\n')
		stop_deadline := time.now().add(10 * time.second)
		for process.is_alive() && time.now() < stop_deadline {
			time.sleep(10 * time.millisecond)
		}
		assert !process.is_alive(), 'run process did not exit'
		process.wait()
		assert process.code == 128 + signal_number, process.stderr_slurp()
		assert os.read_file(counts)! == '1,0'
		assert !os.exists(os.join_path(root, 'main')), 'implicit executable survived'
		child_pid = 0
	}
}

const run_signal_counter_source = r'
#include <signal.h>
static volatile sig_atomic_t run_signal_counter, run_continue_counter;
static void run_count_signal(int signo) { (void)signo; run_signal_counter++; }
static void run_count_continue(int signo) { (void)signo; run_continue_counter++; }
int run_signal_counter_install(int signo) {
    struct sigaction action = {0};
    sigemptyset(&action.sa_mask);
    action.sa_flags = SA_RESTART;
    action.sa_handler = run_count_signal;
    if (sigaction(signo, &action, 0)) return -1;
    action.sa_handler = run_count_continue;
    return sigaction(SIGCONT, &action, 0);
}
int run_signal_counter_get(void) { return (int)run_signal_counter; }
int run_signal_counter_continues(void) { return (int)run_continue_counter; }
'

fn test_run_terminal_input_and_job_control() ! {
	python := os.find_abs_path_of_executable('python3') or { return }
	root := os.join_path(os.vtmp_dir(), 'run_tty_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	script := os.join_path(@VEXEROOT, 'vlib', 'v', 'tests', 'compiler_processes', 'testdata', 'run_terminal_nix.py')
	result := os.exec([python, script, @VEXE, root])
	assert result.exit_code == 0, result.output
}
