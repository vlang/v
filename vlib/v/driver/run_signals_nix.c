#ifdef _WIN32
int v3_run_signals_begin(void) { return 0; }
void v3_run_signals_set_pid(int child_pid) { (void)child_pid; }
int v3_run_signals_finish(void) { return 0; }
#else
/* Keep BSD signal semantics even when a consumer selects strict POSIX features. */
#if defined(__linux__) && !defined(_DEFAULT_SOURCE)
#define _DEFAULT_SOURCE 1
#endif
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>

typedef void (*v3_run_signal_handler)(int);
static volatile sig_atomic_t v3_run_owner_pid;
static volatile sig_atomic_t v3_run_child_pid;
static volatile sig_atomic_t v3_run_received_signal;
static v3_run_signal_handler v3_run_previous_term;
static v3_run_signal_handler v3_run_previous_hup;
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

int v3_run_signals_begin(void) {
    v3_run_owner_pid = getpid();
    v3_run_child_pid = 0;
    v3_run_received_signal = 0;
    v3_run_previous_term = (v3_run_signal_handler)signal(v3_run_term, v3_run_forward_signal);
    if (v3_run_previous_term == (v3_run_signal_handler)-1) {
        return -1;
    }
    v3_run_previous_hup = (v3_run_signal_handler)signal(v3_run_hup, v3_run_forward_signal);
    if (v3_run_previous_hup == (v3_run_signal_handler)-1) {
        int saved_errno = errno;
        signal(v3_run_term, v3_run_previous_term);
        errno = saved_errno;
        return -1;
    }
    return 0;
}

void v3_run_signals_set_pid(int child_pid) {
    v3_run_child_pid = child_pid;
    sig_atomic_t signal_number = v3_run_received_signal;
    if (signal_number != 0 && child_pid > 0) {
        kill(child_pid, signal_number);
    }
}

int v3_run_signals_finish(void) {
    v3_run_child_pid = 0;
    int received_signal = v3_run_received_signal;
    signal(v3_run_hup, v3_run_previous_hup);
    signal(v3_run_term, v3_run_previous_term);
    return received_signal;
}
#endif
