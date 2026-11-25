#ifndef V_RUNTIME_SHELL_PROBE_H
#define V_RUNTIME_SHELL_PROBE_H
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

static volatile sig_atomic_t shell_signal_count;
static void shell_signal_handler(int number) { (void)number; ++shell_signal_count; }
static void *shell_parallel_worker(void *argument) {
    int code = (int)(intptr_t)argument;
    char command[32]; snprintf(command, sizeof(command), "exit %d", code);
    for (int i = 0; i < 4; ++i) {
        int status = system(command);
        if (!WIFEXITED(status) || WEXITSTATUS(status) != code) return (void *)1;
    }
    return NULL;
}
static void *shell_interrupt_worker(void *unused) {
    (void)unused;
    sigset_t set; sigemptyset(&set); sigaddset(&set, SIGUSR2);
    if (sigprocmask(SIG_BLOCK, &set, NULL) || usleep(10000) || kill(getpid(), SIGUSR2)) return (void *)1;
    return NULL;
}
#define SHELL_CHECK(test) do { if (!(test)) { result = __LINE__; goto cleanup; } } while (0)
static int shell_native_probe(void) {
    struct sigaction old_int, old_quit, old_usr2, action = {0}, current;
    sigset_t old_mask, block, after;
    if (sigaction(SIGINT, NULL, &old_int) || sigaction(SIGQUIT, NULL, &old_quit) ||
        sigaction(SIGUSR2, NULL, &old_usr2) || sigprocmask(SIG_SETMASK, NULL, &old_mask)) return __LINE__;
    int result = 0, status = 0, workers = 0;
    pthread_t threads[2]; void *thread_result = NULL;
    FILE *reader = NULL, *writer = NULL, *file = NULL;
    char path[] = "/tmp/vlibc-shell-XXXXXX", command[512], text[128] = {0};
    int fd = mkstemp(path);
    SHELL_CHECK(fd >= 0);
    SHELL_CHECK(!close(fd)); fd = -1;
    action.sa_handler = shell_signal_handler; action.sa_flags = SA_RESTART;
    sigemptyset(&action.sa_mask); sigaddset(&action.sa_mask, SIGUSR1);
    SHELL_CHECK(!sigaction(SIGINT, &action, NULL) && !sigaction(SIGQUIT, &action, NULL));
    sigemptyset(&block); sigaddset(&block, SIGUSR1);
    SHELL_CHECK(!sigprocmask(SIG_BLOCK, &block, NULL));
    SHELL_CHECK(system(NULL) != 0);
    status = system("exit 7");
    SHELL_CHECK(WIFEXITED(status) && WEXITSTATUS(status) == 7);
    status = system("kill -INT $$");
    SHELL_CHECK(WIFSIGNALED(status) && WTERMSIG(status) == SIGINT && !shell_signal_count);
    SHELL_CHECK(!sigaction(SIGINT, NULL, &current) && current.sa_handler == shell_signal_handler);
    SHELL_CHECK((current.sa_flags & SA_RESTART) && sigismember(&current.sa_mask, SIGUSR1) == 1);
    sigemptyset(&after);
    SHELL_CHECK(!sigprocmask(SIG_SETMASK, NULL, &after));
    SHELL_CHECK(sigismember(&after, SIGUSR1) == 1 && sigismember(&after, SIGCHLD) == sigismember(&old_mask, SIGCHLD));
    action.sa_handler = SIG_IGN;
    SHELL_CHECK(!sigaction(SIGQUIT, &action, NULL));
    status = system("kill -QUIT $$; exit 9");
    SHELL_CHECK(WIFEXITED(status) && WEXITSTATUS(status) == 9);
    for (int i = 0; i < 2; ++i) {
        SHELL_CHECK(!pthread_create(&threads[i], NULL, shell_parallel_worker, (void *)(intptr_t)(i + 4)));
        ++workers;
    }
    while (workers) {
        --workers;
        SHELL_CHECK(!pthread_join(threads[workers], &thread_result) && !thread_result);
    }
    SHELL_CHECK(!sigaction(SIGINT, NULL, &current) && current.sa_handler == shell_signal_handler);
    SHELL_CHECK(!sigaction(SIGQUIT, NULL, &current) && current.sa_handler == SIG_IGN);

    errno = 0;
    SHELL_CHECK(!popen("exit 0", "rw") && errno == EINVAL);
    errno = 0;
    SHELL_CHECK(!popen("exit 0", "r+") && errno == EINVAL);
    reader = popen("printf 'native pipe'; exit 7", "er");
    SHELL_CHECK(reader && (fcntl(fileno(reader), F_GETFD) & FD_CLOEXEC));
    SHELL_CHECK(fread(text, 1, sizeof(text), reader) == 11 && !memcmp(text, "native pipe", 11));
    status = pclose(reader); reader = NULL;
    SHELL_CHECK(WIFEXITED(status) && WEXITSTATUS(status) == 7);
    snprintf(command, sizeof(command), "cat >'%s'", path);
    writer = popen(command, "w");
    SHELL_CHECK(writer && !(fcntl(fileno(writer), F_GETFD) & FD_CLOEXEC));
    snprintf(command, sizeof(command), "if [ -e /proc/self/fd/%d ]; then exit 13; else printf closed; fi", fileno(writer));
    reader = popen(command, "r");
    SHELL_CHECK(reader);
    memset(text, 0, sizeof(text));
    SHELL_CHECK(fread(text, 1, sizeof(text), reader) == 6 && !strcmp(text, "closed"));
    status = pclose(reader); reader = NULL;
    SHELL_CHECK(WIFEXITED(status) && WEXITSTATUS(status) == 0);
    SHELL_CHECK(fprintf(writer, "native buffered %d\n", 83) == 19);
    status = pclose(writer); writer = NULL;
    SHELL_CHECK(WIFEXITED(status) && WEXITSTATUS(status) == 0);
    file = fopen(path, "r");
    SHELL_CHECK(file && fgets(text, sizeof(text), file) && !strcmp(text, "native buffered 83\n"));
    SHELL_CHECK(!fclose(file)); file = NULL;

    action.sa_handler = shell_signal_handler; action.sa_flags = 0; sigemptyset(&action.sa_mask);
    SHELL_CHECK(!sigaction(SIGUSR2, &action, NULL));
    sigemptyset(&block); sigaddset(&block, SIGUSR2);
    SHELL_CHECK(!sigprocmask(SIG_UNBLOCK, &block, NULL));
    reader = popen("sleep 0.06; exit 6", "r");
    SHELL_CHECK(reader && !pthread_create(&threads[0], NULL, shell_interrupt_worker, NULL));
    workers = 1;
    status = pclose(reader); reader = NULL;
    SHELL_CHECK(WIFEXITED(status) && WEXITSTATUS(status) == 6 && shell_signal_count > 0);
    SHELL_CHECK(!pthread_join(threads[0], &thread_result) && !thread_result); workers = 0;
cleanup:
    while (workers) { --workers; pthread_join(threads[workers], NULL); }
    if (reader) pclose(reader);
    if (writer) pclose(writer);
    if (file) fclose(file);
    if (fd >= 0) close(fd);
    unlink(path);
    sigaction(SIGINT, &old_int, NULL); sigaction(SIGQUIT, &old_quit, NULL);
    sigaction(SIGUSR2, &old_usr2, NULL); sigprocmask(SIG_SETMASK, &old_mask, NULL);
    if (!result) puts("shell status, signal restoration, concurrent commands, pipe ownership and interrupted close passed");
    return result;
}
#undef SHELL_CHECK
#endif
