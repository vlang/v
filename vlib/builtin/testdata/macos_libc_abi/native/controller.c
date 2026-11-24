/* Hosted controller validates actual wait status, not shell 128+signal codes. */
#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <unistd.h>
#include <util.h>

static void run(const char *binary, char mode) {
    int input[2], output[2], errors[2], master, slave;
    assert(pipe(input) == 0 && pipe(output) == 0 && pipe(errors) == 0);
    assert(openpty(&master, &slave, NULL, NULL, NULL) == 0);
    assert(write(input[1], "xyz", 3) == 3);
    close(input[1]);
    pid_t child = fork(); assert(child >= 0);
    if (child == 0) {
        assert(dup2(input[0], 0) == 0 && dup2(output[1], 1) == 1 && dup2(errors[1], 2) == 2);
        assert(dup2(slave, 3) == 3);
        /* Do not assume the shell/test runner's inherited fatal disposition. */
        assert(signal(SIGABRT, SIG_DFL) != SIG_ERR && signal(SIGILL, SIG_DFL) != SIG_ERR);
        struct rlimit limit = {0, 0}; assert(setrlimit(RLIMIT_CORE, &limit) == 0);
        alarm(10);
        char option[] = {mode, 0};
        execl(binary, binary, option, (char *)NULL);
        _exit(90);
    }
    close(input[0]); close(output[1]); close(errors[1]); close(master); close(slave);
    int status = 0;
    pid_t waited = 0;
    for (unsigned attempt = 0; attempt < 10000; ++attempt) {
        waited = waitpid(child, &status, WNOHANG);
        if (waited == child) break;
        assert(waited == 0 || (waited == -1 && errno == EINTR));
        usleep(1000);
    }
    if (waited != child) {
        (void)kill(child, SIGKILL);
        (void)waitpid(child, &status, 0);
        fprintf(stderr, "native mode %c timed out\n", mode);
        exit(1);
    }
    char observed[32] = {0}, diagnostic[512] = {0};
    assert(read(output[0], observed, sizeof observed - 1) >= 0);
    assert(read(errors[0], diagnostic, sizeof diagnostic - 1) >= 0);
    close(output[0]); close(errors[0]);
    if (mode == '0') {
        if (!WIFEXITED(status) || WEXITSTATUS(status) || strcmp(observed, "xyz") || *diagnostic) {
            fprintf(stderr, "normal probe failed: status=%d stdout=%s stderr=%s\n", status, observed, diagnostic);
            exit(1);
        }
        return;
    }
    if (!WIFSIGNALED(status) || WTERMSIG(status) != SIGABRT) {
        fprintf(stderr, "mode %c failed: status=%d stdout=%s stderr=%s\n", mode, status, observed, diagnostic);
        exit(1);
    }
    assert(strchr(observed, 'A') == NULL && strchr(observed, 'R') == NULL);
    if (mode == 'h' || mode == 'w') assert(strcmp(observed, "H") == 0);
    else if (mode == 'c') assert(strcmp(observed, "H") == 0 || strcmp(observed, "HH") == 0);
    else assert(*observed == 0);
    if (mode == 'n') assert(strcmp(diagnostic, "Assertion failed: (value != 0), file native.c, line 123.\n") == 0);
    if (mode == 'l') assert(strcmp(diagnostic, "native.c:-2147483648: failed assertion `value != 0'\n") == 0);
    if (mode == 'f') assert(strcmp(diagnostic, "Assertion failed: (value != 0), function native_function, file native.c, line 0.\n") == 0);
    if (mode == 'e') assert(strcmp(diagnostic, "Assertion failed: (early assertion), file constructor.c, line 17.\n") == 0);
}

int main(int argc, char **argv) {
    assert(argc == 2);
    const char *modes = "0dihnlfwec";
    for (const char *mode = modes; *mode; ++mode) run(argv[1], *mode);
    puts("V Darwin native libc ABI and abnormal termination ok");
    return 0;
}
