#define _POSIX_C_SOURCE 200809L
#include <signal.h>
#include <pthread.h>
#include <stdatomic.h>
#include <sched.h>
#include <unistd.h>
#include <stdio.h>
static _Atomic int worker_ready, worker_seen, release_worker, worker_done;
static pthread_t worker_thread;
static int selected_signal;
static int fixture_kill(pid_t, int);
#define kill fixture_kill
static int fixture_sigprocmask(int, const sigset_t *, sigset_t *);
#define sigprocmask fixture_sigprocmask
#include "run_signals_nix.c"
#undef sigprocmask
#undef kill
static int fixture_kill(pid_t pid, int signo) {
    if (pid == getpid() && signo == SIGSTOP) {
        /* Model resuming the owner before its delayed worker handler completes. */
        while (!atomic_load(&worker_seen)) sched_yield();
        return 0;
    }
    return kill(pid, signo);
}
static int fixture_sigprocmask(int how, const sigset_t *set, sigset_t *old) {
    /* Force process-directed delivery to the worker before the owner unblocks. */
    if (how == SIG_UNBLOCK && set && sigismember(set, selected_signal)) {
        while (!atomic_load(&worker_seen)) sched_yield();
    }
    return sigprocmask(how, set, old);
}
/* Hold the worker handler until the owner has completed the group relay. */
static void delayed_handler(int signo) {
    atomic_store(&worker_seen, 1);
    while (!atomic_load(&release_worker)) {}
    v3_run_forward_signal(signo);
    atomic_store(&worker_done, 1);
}
static void *worker(void *unused) {
    (void)unused;
    sigset_t set;
    sigemptyset(&set); sigaddset(&set, selected_signal);
    pthread_sigmask(SIG_UNBLOCK, &set, 0);
    atomic_store(&worker_ready, 1);
    while (!atomic_load(&worker_done)) sched_yield();
    return 0;
}
int main(int argc, char **argv) {
    (void)argv;
    selected_signal = argc > 1 ? SIGTSTP : SIGQUIT;
    alarm(10);
    if (setsid() < 0) return 10;
    struct sigaction action = {0};
    action.sa_handler = delayed_handler;
    sigemptyset(&action.sa_mask);
    if (sigaction(selected_signal, &action, 0)) return 11;
    sigset_t set;
    sigemptyset(&set); sigaddset(&set, selected_signal);
    pthread_sigmask(SIG_BLOCK, &set, 0);
    if (pthread_create(&worker_thread, 0, worker, 0)) return 12;
    while (!atomic_load(&worker_ready)) sched_yield();
    v3_run_owner_pid = getpid();
    v3_run_owner_group = getpgrp();
    if (selected_signal == SIGTSTP) {
        v3_run_terminal_fd = 0;
        if (v3_run_suspend_owner(SIGTSTP, 0)) return 13;
    } else if (v3_run_observer_exit((128 + selected_signal) << 8)) return 13;
    atomic_store(&release_worker, 1);
    pthread_join(worker_thread, 0);
    printf("received=%d bridging=%d\n", (int)v3_run_received_signal, (int)v3_run_bridging_interrupt);
    return v3_run_received_signal != 0 || v3_run_bridging_interrupt != 0
        || v3_run_bridging_stop != 0;
}
