/* Use the ordinary optimized GNU header bodies in production builds. */
#undef __NO_INLINE__
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <sys/syscall.h>
#include <time.h>
#include <unistd.h>
#include <wchar.h>

#define CHECK(condition) do { if (!(condition)) return __LINE__; } while (0)
static FILE *v_character_stream;
static int v_character_ready, v_character_acquired;

static int check_prompt_read(int output_fd, char stdout_buffer[BUFSIZ]) {
    char input_buffer[128], received[6];
    int input[2];
    CHECK(!pipe(input));
    CHECK(!setvbuf(stdout, stdout_buffer, _IOLBF, BUFSIZ));
    FILE *reader = fdopen(input[0], "r");
    CHECK(reader && !setvbuf(reader, input_buffer, _IOLBF, sizeof input_buffer));
    CHECK(write(input[1], "AB", 2) == 2 && !close(input[1]));
    CHECK(fputs("prompt", stdout) >= 0 && read(output_fd, received, sizeof received) < 0 && errno == EAGAIN);
    CHECK(fgetc(reader) == 'A' && read(output_fd, received, sizeof received) == 6 &&
          !memcmp(received, "prompt", 6));
    CHECK(fputs("next", stdout) >= 0 && fgetc(reader) == 'B');
    CHECK(read(output_fd, received, sizeof received) < 0 && errno == EAGAIN);
    CHECK(fgetc(reader) == EOF && read(output_fd, received, sizeof received) == 4 &&
          !memcmp(received, "next", 4));
    CHECK(!fclose(reader));
    return 0;
}

#ifdef __GLIBC__
static ssize_t prompt_cookie_write(void *cookie, const char *data, size_t size) {
    return write(*(int *)cookie, data, size);
}
#endif

static int check_prompt_flush(void) {
    static char buffers[3][BUFSIZ];
    int output[2], saved = dup(STDOUT_FILENO);
    CHECK(saved >= 0 && !pipe2(output, O_NONBLOCK | O_CLOEXEC));
    CHECK(dup2(output[1], STDOUT_FILENO) == STDOUT_FILENO);
    int result = check_prompt_read(output[0], buffers[0]);
    if (result) return result;
    CHECK(dup2(saved, STDOUT_FILENO) == STDOUT_FILENO && !close(saved));
#ifdef __GLIBC__
    FILE *original = stdout;
    for (int cookie = 0; cookie <= 1; ++cookie) {
        FILE *redirected = cookie
            ? fopencookie(&output[1], "w", (cookie_io_functions_t){.write = prompt_cookie_write})
            : fdopen(dup(output[1]), "w");
        CHECK(redirected);
        stdout = redirected;
        result = check_prompt_read(output[0], buffers[cookie + 1]);
        stdout = original;
        if (result) return result;
        CHECK(!fclose(redirected));
    }
#endif
    CHECK(!close(output[0]) && !close(output[1]));
    return 0;
}

int v_stdio_character_prepare(const char *path) {
    int result = check_prompt_flush();
    if (result) return result;
    int count = -1;
    int (*volatile put_string)(const char *, FILE *) = fputs;
    FILE *fresh = tmpfile();
    CHECK(fresh && put_string("", fresh) >= 0 && fwide(fresh, 0) < 0 && !fclose(fresh));
    fresh = tmpfile();
    CHECK(fresh && fscanf(fresh, "%n", &count) == 0 && count == 0 &&
          fwide(fresh, 0) < 0 && !fclose(fresh));
    FILE *file = fopen(path, "w+b");
    CHECK(file != NULL);
    for (unsigned int i = 0; i < 32768; ++i) CHECK(putc_unlocked(i & 255, file) == (int)(i & 255));
    CHECK(fseek(file, 0, SEEK_SET) == 0);
    v_character_stream = file;
    flockfile(file);
    flockfile(file);
    CHECK(ftrylockfile(file) == 0);
    return 0;
}

int v_stdio_character_contender(void) {
    FILE *file = v_character_stream;
    CHECK(ftrylockfile(file) != 0);
    __atomic_store_n(&v_character_ready, 1, __ATOMIC_RELEASE);
    flockfile(file);
    __atomic_store_n(&v_character_acquired, 1, __ATOMIC_RELEASE);
    for (unsigned int i = 0; i < 32768; ++i) CHECK(fgetc_unlocked(file) == (int)(i & 255));
    CHECK(fgetc_unlocked(file) == EOF && feof_unlocked(file) && !ferror_unlocked(file));
    CHECK(ungetc(255, file) == 255 && !feof(file));
    CHECK(fgetc(file) == 255 && fgetc(file) == EOF && feof(file));
    CHECK(ungetc(EOF, file) == EOF && feof(file));
    funlockfile(file);
    return 0;
}

int v_stdio_character_release(void) {
    struct timespec pause = {0, 1000000};
    for (int i = 0; i < 5000 && !__atomic_load_n(&v_character_ready, __ATOMIC_ACQUIRE); ++i)
        nanosleep(&pause, NULL);
    CHECK(__atomic_load_n(&v_character_ready, __ATOMIC_ACQUIRE));
    funlockfile(v_character_stream);
    funlockfile(v_character_stream);
    CHECK(!__atomic_load_n(&v_character_acquired, __ATOMIC_ACQUIRE));
    funlockfile(v_character_stream);
    return 0;
}

int v_stdio_character_finish(const char *path) {
    FILE *file = v_character_stream;
    CHECK(__atomic_load_n(&v_character_acquired, __ATOMIC_ACQUIRE));
    clearerr(file);
    CHECK(!feof_unlocked(file) && !ferror_unlocked(file));
    rewind(file);
    CHECK(fgetc(file) == 0 && ungetc(254, file) == 254 && ungetc(255, file) == 255);
    CHECK(fgetc_unlocked(file) == 255 && fgetc_unlocked(file) == 254 && fgetc(file) == 1);
    CHECK(fseek(file, 0, SEEK_SET) == 0 && fputc('Z', file) == 'Z');
    CHECK(fflush(file) == 0 && fgetc(file) == 1);
    CHECK(fseek(file, 0, SEEK_SET) == 0 && fgetc(file) == 'Z');
    flockfile(file);
    /* The forking thread keeps its TLS identity and recursive stream ownership. */
    long child = syscall(SYS_clone, SIGCHLD, 0, NULL, NULL, 0);
    CHECK(child >= 0);
    if (!child) {
        int result = ftrylockfile(file) == 0 && fgetc(file) == 1;
        syscall(SYS_exit_group, result ? 0 : 1);
        __builtin_unreachable();
    }
    int status = -1;
    long waited;
    do { waited = syscall(SYS_wait4, child, &status, 0, NULL); } while (waited < 0 && errno == EINTR);
    CHECK(waited == child && status == 0);
    funlockfile(file);
    CHECK(fclose(file) == 0);
    file = fopen(path, "rb");
    CHECK(file != NULL && fgetc(file) == 'Z');
    CHECK(putc_unlocked('x', file) == EOF && ferror_unlocked(file) && errno == EBADF);
    CHECK(fgetc(file) == 1 && ferror(file));
    clearerr(file);
    CHECK(!ferror(file) && fgetc(file) == 2);
    CHECK(fclose(file) == 0);
    file = fopen(path, "rb");
    CHECK(file && setvbuf(file, NULL, _IONBF, 0) == 0);
    CHECK(fgetc(file) == 'Z' && ungetc(255, file) == 255 && fgetc_unlocked(file) == 255);
    CHECK(fgetc_unlocked(file) == 1 && fclose(file) == 0);
    CHECK(unlink(path) == 0);
    return 0;
}
