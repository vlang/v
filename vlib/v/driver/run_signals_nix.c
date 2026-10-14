#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>

static volatile sig_atomic_t v3_run_child_pid;
static volatile sig_atomic_t v3_run_received_signal;
static struct sigaction v3_run_previous_term;
static struct sigaction v3_run_previous_hup;

static void v3_run_forward_signal(int signal_number) {
    int saved_errno = errno;
    v3_run_received_signal = signal_number;
    sig_atomic_t child_pid = v3_run_child_pid;
    if (child_pid > 0) {
        kill((pid_t)child_pid, signal_number);
    }
    errno = saved_errno;
}

int v3_run_signals_begin(void) {
    struct sigaction action = {0};
    action.sa_handler = v3_run_forward_signal;
    action.sa_flags = SA_RESTART;
    sigemptyset(&action.sa_mask);
    sigaddset(&action.sa_mask, SIGTERM);
    sigaddset(&action.sa_mask, SIGHUP);
    v3_run_child_pid = 0;
    v3_run_received_signal = 0;
    if (sigaction(SIGTERM, &action, &v3_run_previous_term) != 0) {
        return -1;
    }
    if (sigaction(SIGHUP, &action, &v3_run_previous_hup) != 0) {
        int saved_errno = errno;
        sigaction(SIGTERM, &v3_run_previous_term, 0);
        errno = saved_errno;
        return -1;
    }
    return 0;
}

void v3_run_signals_set_pid(int child_pid) {
    v3_run_child_pid = child_pid;
    sig_atomic_t signal_number = v3_run_received_signal;
    if (signal_number != 0 && child_pid > 0) {
        kill((pid_t)child_pid, signal_number);
    }
}

int v3_run_signals_finish(void) {
    v3_run_child_pid = 0;
    int received_signal = v3_run_received_signal;
    sigaction(SIGHUP, &v3_run_previous_hup, 0);
    sigaction(SIGTERM, &v3_run_previous_term, 0);
    return received_signal;
}
