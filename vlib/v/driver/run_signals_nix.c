#ifdef _WIN32
int v3_run_signals_begin(void) { return 0; }
void v3_run_signals_set_pid(int child_pid) { (void)child_pid; }
int v3_run_signals_finish(void) { return 0; }
#else
#if !defined(_POSIX_C_SOURCE)
#define _POSIX_C_SOURCE 200809L
#endif
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>

static volatile sig_atomic_t v3_run_owner_pid;
static volatile sig_atomic_t v3_run_child_pid;
static volatile sig_atomic_t v3_run_received_signal;
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
#endif
static v3_run_action v3_run_previous_term;
static v3_run_action v3_run_previous_hup;
/* These match os.Signal on every supported Unix target. */
enum { v3_run_hup = 1, v3_run_term = 15 };

static void v3_run_forward_signal(int signal_number) {
    int saved_errno = errno;
    /* A forked child inherits this handler until exec replaces its image. */
    if (getpid() != v3_run_owner_pid) {
        _exit(128 + signal_number);
    }
    v3_run_received_signal = signal_number;
    sig_atomic_t child_pid = v3_run_child_pid;
    if (child_pid > 0) {
        kill((int)child_pid, signal_number);
    }
    errno = saved_errno;
}

static int v3_run_block_signals(sigset_t *previous_mask) {
    sigset_t mask;
    if (v3_run_sigemptyset(&mask) || v3_run_sigaddset(&mask, v3_run_term)
        || v3_run_sigaddset(&mask, v3_run_hup)) {
        return -1;
    }
    return v3_run_sigprocmask(SIG_BLOCK, &mask, previous_mask);
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

int v3_run_signals_begin(void) {
    sigset_t previous_mask;
    if (v3_run_block_signals(&previous_mask)) {
        return -1;
    }
    v3_run_owner_pid = getpid();
    v3_run_child_pid = 0;
    v3_run_received_signal = 0;
    int result = v3_run_install_signal(v3_run_term, &v3_run_previous_term);
    if (result == 0 && v3_run_install_signal(v3_run_hup, &v3_run_previous_hup)) {
        int saved_errno = errno;
        v3_run_sigaction(v3_run_term, &v3_run_previous_term, 0);
        errno = saved_errno;
        result = -1;
    }
    int saved_errno = errno;
    v3_run_sigprocmask(SIG_SETMASK, &previous_mask, 0);
    errno = saved_errno;
    return result;
}

void v3_run_signals_set_pid(int child_pid) {
    v3_run_child_pid = child_pid;
    sig_atomic_t signal_number = v3_run_received_signal;
    if (signal_number != 0 && child_pid > 0) {
        kill(child_pid, signal_number);
    }
}

int v3_run_signals_finish(void) {
    sigset_t previous_mask;
    if (v3_run_block_signals(&previous_mask)) {
        return -1;
    }
    v3_run_child_pid = 0;
    int received_signal = v3_run_received_signal;
    v3_run_sigaction(v3_run_hup, &v3_run_previous_hup, 0);
    v3_run_sigaction(v3_run_term, &v3_run_previous_term, 0);
    /* Pending signals now use the restored disposition instead of a stale snapshot. */
    v3_run_sigprocmask(SIG_SETMASK, &previous_mask, 0);
    return received_signal;
}
#endif
