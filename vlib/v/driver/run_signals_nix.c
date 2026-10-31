#ifdef _WIN32
int v3_run_signals_begin(void) { return 0; }
int v3_run_signals_set_pid(int child_pid) { (void)child_pid; return 0; }
void v3_run_signals_kill(int child_pid) { (void)child_pid; }
int v3_run_signals_wait(int *code, int *signal_number) { *code = 0; *signal_number = 0; return 0; }
int v3_run_signals_finish(void) { return 0; }
#else
#if !defined(_POSIX_C_SOURCE)
#define _POSIX_C_SOURCE 200809L
#endif
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

static volatile sig_atomic_t v3_run_owner_pid;
static volatile sig_atomic_t v3_run_child_pid;
static volatile sig_atomic_t v3_run_received_signal;
static volatile sig_atomic_t v3_run_termination_signal;
static volatile sig_atomic_t v3_run_forwarded_stop;
static volatile sig_atomic_t v3_run_bridging_stop;
static volatile sig_atomic_t v3_run_bridging_interrupt;
static volatile sig_atomic_t v3_run_observer_pid;
static volatile sig_atomic_t v3_run_observer_forwarded_int;
static volatile sig_atomic_t v3_run_observer_forwarded_quit;
static volatile sig_atomic_t v3_run_observer_forwarded_stop;
static int v3_run_observed_stop;
static int v3_run_observer_control = -1;
static int v3_run_observer_stopped;
static volatile sig_atomic_t v3_run_observer_notice_read = -1;
static volatile sig_atomic_t v3_run_observer_notice_write = -1;
static int v3_run_owner_group;
static int v3_run_terminal_fd = -1;
static int v3_run_handoff_terminal;
#ifdef V3_RUN_SIGNALS_HEADERLESS
#ifndef v3_run_set_signal
#define v3_run_set_signal signal
#endif
/* Supported Unix sigaction records fit in 512 bytes; pointer/long-double alignment
 * also covers their native alignment. libc owns the opaque layout. */
typedef union {
    unsigned char bytes[512];
    void (*handler_alignment)(int);
    long double alignment;
} v3_run_action;
#else
typedef struct sigaction v3_run_action;
#define v3_run_sigaction sigaction
#define v3_run_sigprocmask sigprocmask
#define v3_run_sigemptyset sigemptyset
#define v3_run_sigaddset sigaddset
#define v3_run_setpgid setpgid
#define v3_run_getpgid getpgid
#define v3_run_getpgrp getpgrp
#define v3_run_tcgetpgrp tcgetpgrp
#define v3_run_tcsetpgrp tcsetpgrp
#define v3_run_open open
#define v3_run_close close
#define v3_run_fcntl fcntl
#define v3_run_waitpid waitpid
#define v3_run_fork fork
#define v3_run_pipe pipe
#define v3_run_read read
#define v3_run_write write
#define v3_run_set_signal signal
#endif
static v3_run_action v3_run_previous_term;
static v3_run_action v3_run_previous_hup;
static v3_run_action v3_run_previous_int;
static v3_run_action v3_run_previous_quit;
static v3_run_action v3_run_previous_tstp;
static v3_run_action v3_run_previous_ttin;
static v3_run_action v3_run_previous_ttou;
/* These match os.Signal on every supported Unix target. */
enum { v3_run_hup = 1, v3_run_int = 2, v3_run_quit = 3, v3_run_term = 15 };
#if defined(V3_RUN_SIGNALS_HEADERLESS) && (defined(__QNX__) || defined(__QNXNTO__))
#define V3_RUN_BLOCK 0
#define V3_RUN_SETMASK 2
#define V3_RUN_UNBLOCK 1
#else
#define V3_RUN_BLOCK SIG_BLOCK
#define V3_RUN_SETMASK SIG_SETMASK
#if defined(V3_RUN_SIGNALS_HEADERLESS)
#if defined(__linux__) || defined(__ANDROID__)
#define V3_RUN_UNBLOCK 1
#else
#define V3_RUN_UNBLOCK 2
#endif
#else
#define V3_RUN_UNBLOCK SIG_UNBLOCK
#endif
#endif
#ifdef V3_RUN_SIGNALS_HEADERLESS
#ifndef WUNTRACED
#if defined(__sun) || defined(__QNX__) || defined(__QNXNTO__)
#define WUNTRACED 4
#else
#define WUNTRACED 2
#endif
#endif
#ifndef WIFSTOPPED
#define WIFSTOPPED(status) (((status) & 0xff) == 0x7f)
#endif
#ifndef WSTOPSIG
#define WSTOPSIG(status) (((status) >> 8) & 0xff)
#endif
/* Solaris and QNX use different job-control signal numbers from BSD. */
#if defined(__sun) || defined(__QNX__) || defined(__QNXNTO__)
#define V3_RUN_STOP 23
#define V3_RUN_CONT 25
#define V3_RUN_TTOU 27
#define V3_RUN_TTIN 26
#define V3_RUN_TSTP 24
#else
#define V3_RUN_STOP SIGSTOP
#define V3_RUN_CONT SIGCONT
#define V3_RUN_TTOU SIGTTOU
#define V3_RUN_TTIN 21
#if defined(__linux__) || defined(__ANDROID__)
#define V3_RUN_TSTP 20
#else
#define V3_RUN_TSTP 18
#endif
#endif
#else
#define V3_RUN_STOP SIGSTOP
#define V3_RUN_CONT SIGCONT
#define V3_RUN_TTOU SIGTTOU
#define V3_RUN_TTIN SIGTTIN
#define V3_RUN_TSTP SIGTSTP
#endif

/* Signals forwarded by the parent must not be relayed back by its observer. */
static void v3_run_send_child_signal(int signal_number) {
    if (v3_run_observer_pid > 0 && v3_run_observer_notice_write >= 0) {
        volatile sig_atomic_t *announced = signal_number == v3_run_int
            ? &v3_run_observer_forwarded_int : signal_number == v3_run_quit
            ? &v3_run_observer_forwarded_quit : &v3_run_observer_forwarded_stop;
        if ((signal_number == v3_run_int || signal_number == v3_run_quit
            || signal_number == V3_RUN_TSTP) && !*announced) {
            *announced = 1;
            unsigned char notice = (unsigned char)signal_number;
            while (v3_run_write((int)v3_run_observer_notice_write, &notice, 1) < 0
                && errno == EINTR) {}
        }
    }
    if (v3_run_child_pid > 0) kill(-(int)v3_run_child_pid, signal_number);
}

static void v3_run_forward_signal(int signal_number) {
    int saved_errno = errno;
    int is_stop = signal_number == V3_RUN_TSTP || signal_number == V3_RUN_TTIN
        || signal_number == V3_RUN_TTOU;
    /* A forked child inherits this handler until exec replaces its image. */
    if (getpid() != v3_run_owner_pid) {
        if (is_stop) {
            kill(getpid(), V3_RUN_STOP);
            errno = saved_errno;
            return;
        }
        _exit(128 + signal_number);
    }
    if ((is_stop && v3_run_bridging_stop) || signal_number == v3_run_bridging_interrupt) {
        errno = saved_errno;
        return;
    }
    if (is_stop) v3_run_forwarded_stop = signal_number;
    v3_run_received_signal = signal_number;
    if (signal_number == v3_run_hup || signal_number == v3_run_term) {
        v3_run_termination_signal = signal_number;
    }
    v3_run_send_child_signal(signal_number);
    errno = saved_errno;
}

static int v3_run_block_signals(sigset_t *previous_mask) {
    sigset_t mask;
    if (v3_run_sigemptyset(&mask) || v3_run_sigaddset(&mask, v3_run_term)
        || v3_run_sigaddset(&mask, v3_run_hup) || v3_run_sigaddset(&mask, v3_run_int)
        || v3_run_sigaddset(&mask, v3_run_quit) || v3_run_sigaddset(&mask, V3_RUN_TSTP)
        || v3_run_sigaddset(&mask, V3_RUN_TTIN) || v3_run_sigaddset(&mask, V3_RUN_TTOU)) {
        return -1;
    }
    return v3_run_sigprocmask(V3_RUN_BLOCK, &mask, previous_mask);
}

static int v3_run_install_signal(int signal_number, v3_run_action *previous) {
    if (v3_run_sigaction(signal_number, 0, previous)) {
        return -1;
    }
#ifdef V3_RUN_SIGNALS_HEADERLESS
    /* Signals are blocked here. Never probe with SIG_IGN, which discards pending
     * signals; restore inherited ignores before unblocking or forking. */
    void (*previous_handler)(int) = (void (*)(int))v3_run_set_signal(signal_number, v3_run_forward_signal);
    if (previous_handler == (void (*)(int))-1) {
        return -1;
    }
    if (previous_handler == (void (*)(int))SIG_IGN) {
        return v3_run_sigaction(signal_number, previous, 0);
    }
    if (v3_run_siginterrupt(signal_number, 0)) {
        int saved_errno = errno;
        v3_run_sigaction(signal_number, previous, 0);
        errno = saved_errno;
        return -1;
    }
    return 0;
#else
    /* Inherited ignores must survive fork/exec, as with nohup. */
    if (previous->sa_handler == SIG_IGN) {
        return 0;
    }
    struct sigaction action = {0};
    action.sa_handler = v3_run_forward_signal;
    action.sa_flags = SA_RESTART;
    sigemptyset(&action.sa_mask);
    return sigaction(signal_number, &action, 0);
#endif
}

/* Terminal ownership changes from a background group must not stop the compiler. */
static int v3_run_set_foreground(int group) {
    sigset_t mask, previous_mask;
    if (v3_run_sigemptyset(&mask) || v3_run_sigaddset(&mask, V3_RUN_TTOU)
        || v3_run_sigprocmask(V3_RUN_BLOCK, &mask, &previous_mask)) return -1;
    int result = v3_run_tcsetpgrp(v3_run_terminal_fd, group);
    int saved_errno = errno;
    v3_run_sigprocmask(V3_RUN_SETMASK, &previous_mask, 0);
    errno = saved_errno;
    return result;
}

static void v3_run_restore_terminal(void) {
    if (v3_run_terminal_fd >= 0
        && v3_run_tcgetpgrp(v3_run_terminal_fd) == v3_run_child_pid) {
        v3_run_set_foreground(v3_run_owner_group);
    }
}

static int v3_run_foreground_child(void) {
    if (v3_run_handoff_terminal && v3_run_terminal_fd >= 0
        && v3_run_tcgetpgrp(v3_run_terminal_fd) == v3_run_owner_group) {
        return v3_run_set_foreground((int)v3_run_child_pid);
    }
    return 0;
}

static void v3_run_observer_interrupt(int signal_number) {
    unsigned char notices[3];
    int forwarded = 0;
    for (;;) {
        long count = v3_run_read((int)v3_run_observer_notice_read, notices, sizeof(notices));
        if (count < 0 && errno == EINTR) continue;
        if (count <= 0) break;
        for (long i = 0; i < count; i++) if (notices[i] == signal_number) forwarded = 1;
    }
    /* Classification commits here: later parent notifications cannot change it. */
    _exit((forwarded ? 64 : 128) + signal_number);
}

/* The app is already forked, so it cannot inherit this control pipe. EOF also
 * shuts down the observer if its parent exits unexpectedly. Signals stay blocked
 * until its group and dispositions are ready, including a fast terminal handoff. */
static int v3_run_start_observer(const sigset_t *previous_mask) {
    if (!v3_run_handoff_terminal) return 0;
    int control[2];
    if (v3_run_pipe(control)) return -1;
    if (v3_run_fcntl(control[0], F_SETFD, FD_CLOEXEC) < 0
        || v3_run_fcntl(control[1], F_SETFD, FD_CLOEXEC) < 0) {
        v3_run_close(control[0]); v3_run_close(control[1]);
        return -1;
    }
    int notices[2];
    if (v3_run_pipe(notices)) {
        v3_run_close(control[0]); v3_run_close(control[1]);
        return -1;
    }
    int flags = v3_run_fcntl(notices[0], F_GETFL, 0);
    if (flags < 0 || v3_run_fcntl(notices[0], F_SETFL, flags | O_NONBLOCK) < 0
        || v3_run_fcntl(notices[0], F_SETFD, FD_CLOEXEC) < 0
        || v3_run_fcntl(notices[1], F_SETFD, FD_CLOEXEC) < 0) {
        v3_run_close(control[0]); v3_run_close(control[1]);
        v3_run_close(notices[0]); v3_run_close(notices[1]);
        return -1;
    }
    int observer = v3_run_fork();
    if (observer < 0) {
        v3_run_close(control[0]); v3_run_close(control[1]);
        v3_run_close(notices[0]); v3_run_close(notices[1]);
        return -1;
    }
    if (!observer) {
        v3_run_close(control[1]);
        v3_run_close(notices[1]);
        v3_run_observer_notice_read = notices[0];
        if (v3_run_setpgid(0, (int)v3_run_child_pid)) _exit(125);
        int ignored[] = {v3_run_hup, v3_run_term, V3_RUN_TTIN, V3_RUN_TTOU};
        for (int i = 0; i < 4; i++) {
            if (v3_run_set_signal(ignored[i], SIG_IGN) == (void (*)(int))-1) _exit(125);
        }
        int interrupts[] = {v3_run_int, v3_run_quit, V3_RUN_TSTP};
        for (int i = 0; i < 3; i++) {
            void (*previous)(int) = v3_run_set_signal(interrupts[i], v3_run_observer_interrupt);
            if (previous == (void (*)(int))-1) _exit(125);
            if (previous == (void (*)(int))SIG_IGN) v3_run_set_signal(interrupts[i], SIG_IGN);
        }
        if (v3_run_terminal_fd >= 0) v3_run_close(v3_run_terminal_fd);
        for (int fd = 0; fd < 3; fd++) {
            if (fd != control[0] && fd != notices[0]) v3_run_close(fd);
        }
        if (v3_run_sigprocmask(V3_RUN_SETMASK, previous_mask, 0)) _exit(125);
        unsigned char byte;
        for (;;) {
            long count = v3_run_read(control[0], &byte, 1);
            if (count < 0 && errno == EINTR) continue;
            if (count <= 0) _exit(count < 0 ? 125 : 0);
        }
    }
    v3_run_close(control[0]);
    if (v3_run_setpgid(observer, (int)v3_run_child_pid)
        && v3_run_getpgid(observer) != v3_run_child_pid) {
        v3_run_close(control[1]);
        v3_run_close(notices[0]); v3_run_close(notices[1]);
        kill(observer, SIGKILL);
        while (v3_run_waitpid(observer, 0, 0) < 0 && errno == EINTR) {}
        return -1;
    }
    v3_run_observer_pid = observer;
    v3_run_observer_control = control[1];
    /* Retain a reader so a late handler write cannot generate SIGPIPE after exit. */
    v3_run_observer_notice_read = notices[0];
    v3_run_observer_notice_write = notices[1];
    v3_run_observer_stopped = 0;
    v3_run_observer_forwarded_int = 0;
    v3_run_observer_forwarded_quit = 0;
    v3_run_observer_forwarded_stop = 0;
    return 0;
}

/* A tty interrupt reaches the app's foreground group. Relay it to the original
 * pipeline even when the app handles the signal and keeps running. */
static int v3_run_observer_exit(int status) {
    sigset_t previous_mask, selected;
    if (v3_run_block_signals(&previous_mask)) return -1;
    int signal_number = WIFEXITED(status) ? WEXITSTATUS(status) - 128 : 0;
    v3_run_observer_pid = 0;
    v3_run_observer_stopped = 0;
    if (v3_run_observer_control >= 0) v3_run_close(v3_run_observer_control);
    v3_run_observer_control = -1;
    if (v3_run_observer_notice_read >= 0) v3_run_close((int)v3_run_observer_notice_read);
    if (v3_run_observer_notice_write >= 0) v3_run_close((int)v3_run_observer_notice_write);
    v3_run_observer_notice_read = v3_run_observer_notice_write = -1;
    if (signal_number == V3_RUN_TSTP) v3_run_observed_stop = 1;
    if (signal_number == v3_run_int || signal_number == v3_run_quit) {
        if (v3_run_sigemptyset(&selected) || v3_run_sigaddset(&selected, signal_number)) {
            v3_run_sigprocmask(V3_RUN_SETMASK, &previous_mask, 0);
            return -1;
        }
        v3_run_bridging_interrupt = signal_number;
        kill(-v3_run_owner_group, signal_number);
        /* Drain our own group notification under the existing handler, including
         * when finish entered with all forwarded signals blocked. */
        v3_run_sigprocmask(V3_RUN_UNBLOCK, &selected, 0);
        v3_run_bridging_interrupt = 0;
    }
    v3_run_sigprocmask(V3_RUN_SETMASK, &previous_mask, 0);
    return WIFEXITED(status) && WEXITSTATUS(status) == 125 ? -1 : 0;
}

static int v3_run_close_observer(void) {
    if (v3_run_observer_control >= 0) v3_run_close(v3_run_observer_control);
    v3_run_observer_control = -1;
    if (v3_run_observer_pid > 0 && v3_run_observer_stopped) {
        kill((int)v3_run_observer_pid, V3_RUN_CONT);
    }
    while (v3_run_observer_pid > 0) {
        int status;
        int waited = v3_run_waitpid((int)v3_run_observer_pid, &status, WUNTRACED);
        if (waited < 0) {
            if (errno == EINTR) continue;
            v3_run_observer_pid = 0;
            v3_run_observer_stopped = 0;
            if (v3_run_observer_notice_read >= 0) v3_run_close((int)v3_run_observer_notice_read);
            if (v3_run_observer_notice_write >= 0) v3_run_close((int)v3_run_observer_notice_write);
            v3_run_observer_notice_read = v3_run_observer_notice_write = -1;
            return -1;
        }
        if (WIFSTOPPED(status)) {
            kill(waited, V3_RUN_CONT);
            continue;
        }
        return v3_run_observer_exit(status);
    }
    return 0;
}

int v3_run_signals_begin(void) {
    sigset_t previous_mask;
    if (v3_run_block_signals(&previous_mask)) return -1;
    v3_run_owner_pid = getpid();
    v3_run_owner_group = v3_run_getpgrp();
    v3_run_child_pid = 0;
    v3_run_received_signal = 0;
    v3_run_termination_signal = 0;
    v3_run_forwarded_stop = 0;
    v3_run_observed_stop = 0;
    v3_run_bridging_stop = 0;
    v3_run_bridging_interrupt = 0;
    v3_run_observer_pid = 0;
    v3_run_observer_control = -1;
    v3_run_observer_stopped = 0;
    v3_run_terminal_fd = v3_run_open("/dev/tty", O_RDWR);
    /* Keep tty-reading pipeline peers foreground when the program reads a pipe. */
    v3_run_handoff_terminal = v3_run_terminal_fd >= 0
        && v3_run_tcgetpgrp(0) >= 0;
    if (v3_run_terminal_fd >= 0
        && v3_run_fcntl(v3_run_terminal_fd, F_SETFD, FD_CLOEXEC) < 0) {
        int saved_errno = errno;
        v3_run_close(v3_run_terminal_fd);
        v3_run_terminal_fd = -1;
        v3_run_sigprocmask(V3_RUN_SETMASK, &previous_mask, 0);
        errno = saved_errno;
        return -1;
    }
    int installed = 0;
    int signals[] = {v3_run_term, v3_run_hup, v3_run_int, v3_run_quit,
        V3_RUN_TSTP, V3_RUN_TTIN, V3_RUN_TTOU};
    v3_run_action *previous[] = {&v3_run_previous_term, &v3_run_previous_hup,
        &v3_run_previous_int, &v3_run_previous_quit, &v3_run_previous_tstp,
        &v3_run_previous_ttin, &v3_run_previous_ttou};
    while (installed < 7 && !v3_run_install_signal(signals[installed], previous[installed])) {
        installed++;
    }
    int result = installed == 7 ? 0 : -1;
    int saved_errno = errno;
    if (result) {
        while (installed > 0) {
            installed--;
            v3_run_sigaction(signals[installed], previous[installed], 0);
        }
        if (v3_run_terminal_fd >= 0) v3_run_close(v3_run_terminal_fd);
        v3_run_terminal_fd = -1;
    }
    v3_run_sigprocmask(V3_RUN_SETMASK, &previous_mask, 0);
    errno = saved_errno;
    return result;
}

int v3_run_signals_set_pid(int child_pid) {
    sigset_t previous_mask;
    if (v3_run_block_signals(&previous_mask)) return -1;
    /* The child also sets its group before exec. Verify it before publishing the
     * forwarding target, including when exec won the parent-side setpgid race. */
    int result = 0;
    if (child_pid <= 0) {
        result = -1;
    } else {
        for (int attempt = 0; attempt < 2; attempt++) {
            if (!v3_run_setpgid(child_pid, child_pid)) break;
            int set_error = errno;
            int group = v3_run_getpgid(child_pid);
            if (group == child_pid || set_error == ESRCH || (group < 0 && errno == ESRCH)) break;
            /* Darwin can report EPERM while the child creates this same group.
             * Retry once, still verifying the group before publishing it. */
            if (set_error == EPERM && attempt == 0) continue;
            result = -1;
            break;
        }
    }
    if (!result) {
        v3_run_child_pid = child_pid;
        result = v3_run_start_observer(&previous_mask);
        if (!result) result = v3_run_foreground_child();
        if (result && kill(child_pid, 0) < 0 && errno == ESRCH) result = 0;
        if (!result) {
            sig_atomic_t signal_number = v3_run_received_signal;
            if (signal_number != 0) v3_run_send_child_signal((int)signal_number);
        }
    }
    int saved_errno = errno;
    v3_run_sigprocmask(V3_RUN_SETMASK, &previous_mask, 0);
    errno = saved_errno;
    return result;
}

/* A terminal stop can be caught by the app. The observer still suspends the
 * shell's original job and restores its terminal, without re-signalling the app. */
static int v3_run_suspend_owner(int stop_signal, int forwarded_stop) {
    v3_run_restore_terminal();
    /* Only the owner's own copy of this group stop is swallowed. A forwarded stop
     * sends none, and a stop that arrives as the job resumes is a new one. */
    if (v3_run_terminal_fd >= 0 && !forwarded_stop
        && (stop_signal == V3_RUN_TSTP || stop_signal == V3_RUN_TTIN
            || stop_signal == V3_RUN_TTOU)) {
        v3_run_bridging_stop = 1;
        kill(-v3_run_owner_group, stop_signal);
    }
    sigset_t previous_mask;
    for (;;) {
        v3_run_forwarded_stop = 0;
        kill((int)v3_run_owner_pid, V3_RUN_STOP);
        v3_run_bridging_stop = 0;
        /* A later stop must reach the app after its SIGCONT, not before it. */
        if (v3_run_block_signals(&previous_mask)) return -1;
        /* A stop forwarded while the app was still stopped, such as a pipeline
         * peer reading the terminal right after `bg`, suspends the job again. */
        if (!v3_run_forwarded_stop) break;
        v3_run_sigprocmask(V3_RUN_SETMASK, &previous_mask, 0);
    }
    int result = v3_run_foreground_child();
    if (!result) kill(-(int)v3_run_child_pid, V3_RUN_CONT);
    int saved_errno = errno;
    v3_run_sigprocmask(V3_RUN_SETMASK, &previous_mask, 0);
    errno = saved_errno;
    return result ? -1 : 0;
}

void v3_run_signals_kill(int child_pid) {
    /* Publish the group only after setpgid/getpgid verified it. A setup failure
     * before that point must never target an unverified process group. */
    if (child_pid > 0 && v3_run_child_pid == child_pid) kill(-child_pid, SIGKILL);
    if (child_pid > 0) kill(child_pid, SIGKILL);
}

int v3_run_signals_wait(int *code, int *signal_number) {
    if (v3_run_child_pid <= 0) return -1;
    int status;
    for (;;) {
        /* The driver enters this phase after build children have finished. Wait
         * for either owned PID even if the app changes its process group. */
        int target = v3_run_observer_pid > 0 ? -1 : (int)v3_run_child_pid;
        int waited = v3_run_waitpid(target, &status, WUNTRACED);
        if (waited < 0) {
            if (errno == EINTR) continue;
            return -1;
        }
        if (waited == v3_run_observer_pid) {
            if (WIFSTOPPED(status)) { v3_run_observer_stopped = 1; continue; }
            if (v3_run_observer_exit(status)) return -1;
            if (v3_run_observed_stop) {
                v3_run_observed_stop = 0;
                if (v3_run_suspend_owner(V3_RUN_TSTP, 0)) return -1;
            }
            sigset_t previous_mask;
            if (v3_run_block_signals(&previous_mask)) return -1;
            int result = v3_run_start_observer(&previous_mask);
            v3_run_sigprocmask(V3_RUN_SETMASK, &previous_mask, 0);
            if (result) return -1;
            continue;
        }
        if (waited != v3_run_child_pid) continue;
        if (WIFSTOPPED(status)) {
            /* Terminal reads and TOSTOP writes can race the initial handoff. */
            if ((WSTOPSIG(status) == V3_RUN_TTIN || WSTOPSIG(status) == V3_RUN_TTOU)
                && !v3_run_forwarded_stop && v3_run_handoff_terminal
                && v3_run_terminal_fd >= 0
                && v3_run_tcgetpgrp(v3_run_terminal_fd) == v3_run_child_pid) {
                kill(-(int)v3_run_child_pid, V3_RUN_CONT);
                continue;
            }
            int forwarded_stop = v3_run_forwarded_stop;
            v3_run_forwarded_stop = 0;
            /* A debugger can stop/resume only the child without stopping its waiter. */
            if (v3_run_terminal_fd < 0 && !forwarded_stop) continue;
            int stop_signal = WSTOPSIG(status);
            /* Reap the observer before suspending: otherwise its pending stop
             * notification could suspend the same job again after fg. */
            if (v3_run_close_observer()) return -1;
            v3_run_observed_stop = 0;
            if (v3_run_suspend_owner(stop_signal, forwarded_stop)) return -1;
            sigset_t previous_mask;
            if (v3_run_block_signals(&previous_mask)) return -1;
            int result = v3_run_start_observer(&previous_mask);
            v3_run_sigprocmask(V3_RUN_SETMASK, &previous_mask, 0);
            if (result) return -1;
            continue;
        }
        v3_run_restore_terminal();
        if (v3_run_close_observer()) return -1;
        *signal_number = WIFSIGNALED(status) ? WTERMSIG(status) : 0;
        *code = *signal_number ? 128 + *signal_number : WEXITSTATUS(status);
        return 0;
    }
}

int v3_run_signals_finish(void) {
    sigset_t previous_mask;
    if (v3_run_block_signals(&previous_mask)) return -1;
    v3_run_restore_terminal();
    v3_run_close_observer();
    if (v3_run_terminal_fd >= 0) v3_run_close(v3_run_terminal_fd);
    v3_run_terminal_fd = -1;
    v3_run_child_pid = 0;
    int received_signal = v3_run_termination_signal;
    v3_run_sigaction(V3_RUN_TTOU, &v3_run_previous_ttou, 0);
    v3_run_sigaction(V3_RUN_TTIN, &v3_run_previous_ttin, 0);
    v3_run_sigaction(V3_RUN_TSTP, &v3_run_previous_tstp, 0);
    v3_run_sigaction(v3_run_quit, &v3_run_previous_quit, 0);
    v3_run_sigaction(v3_run_int, &v3_run_previous_int, 0);
    v3_run_sigaction(v3_run_hup, &v3_run_previous_hup, 0);
    v3_run_sigaction(v3_run_term, &v3_run_previous_term, 0);
    /* Pending signals now use the restored disposition instead of a stale snapshot. */
    v3_run_sigprocmask(V3_RUN_SETMASK, &previous_mask, 0);
    return received_signal;
}
#endif
