#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include "probe.h"
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <unistd.h>

#define CHECK(condition) do { if (!(condition)) return __LINE__; } while (0)
int v_freestanding_stdio_check(const char *directory) {
    char path[1024], buffer[32], storage[16];
    CHECK(snprintf(path, sizeof path, "%s/buffered", directory) > 0);
    FILE *file = fopen(path, "w+b");
    CHECK(file != NULL && setvbuf(file, storage, _IOFBF, sizeof storage) == 0);
    CHECK(fwrite("abcdef", 1, 6, file) == 6 && ftell(file) == 6);
    struct stat metadata;
    CHECK(stat(path, &metadata) == 0 && metadata.st_size == 0);
    CHECK(fflush(file) == 0 && fstat(fileno(file), &metadata) == 0 && metadata.st_size == 6);
    CHECK(fseek(file, 0, SEEK_SET) == 0);
    CHECK(fread(buffer, 1, 2, file) == 2 && !memcmp(buffer, "ab", 2) && ftell(file) == 2);
    CHECK(fseek(file, 1, SEEK_CUR) == 0 && fgetc(file) == 'd' && ftell(file) == 4);
    CHECK(fflush(file) == 0 && lseek(fileno(file), 0, SEEK_CUR) == 4);
    CHECK(fread(buffer, 1, sizeof buffer, file) == 2 && feof(file));
    rewind(file);
    CHECK(!feof(file) && !ferror(file) && ftell(file) == 0);
    CHECK(fgets(buffer, sizeof buffer, file) == buffer && !strcmp(buffer, "abcdef"));
    CHECK(fclose(file) == 0);

    file = fopen(path, "ab");
    CHECK(file && setvbuf(file, NULL, _IOFBF, 64) == 0);
    CHECK(fwrite("ghi", 1, 3, file) == 3 && ftell(file) == 9);
    CHECK(fclose(file) == 0);
    int fd = open(path, O_RDWR | O_CLOEXEC);
    CHECK(fd >= 0);
    file = fdopen(fd, "rb");
    CHECK(file && fwrite("bad", 1, 3, file) == 0 && ferror(file) && errno == EBADF);
    clearerr(file);
    CHECK(fread(buffer, 1, 9, file) == 9 && !memcmp(buffer, "abcdefghi", 9));
    CHECK(fclose(file) == 0);
    fd = open(path, O_RDONLY);
    CHECK(fd >= 0 && close(fd) == 0);
    file = fopen(path, "wbx");
    CHECK(!file && errno == EEXIST);
    file = fopen(path, "w+b");
    CHECK(file && fseeko(file, (off_t)1 << 33, SEEK_SET) == 0);
    CHECK(fputc('x', file) == 'x' && ftello(file) == ((off_t)1 << 33) + 1);
    CHECK(fflush(file) == 0 && fstat(fileno(file), &metadata) == 0 && metadata.st_size == ((off_t)1 << 33) + 1);
    CHECK(fclose(file) == 0 && remove(path) == 0);

    int pair[2];
    CHECK(syscall(SYS_pipe2, pair, O_NONBLOCK | O_CLOEXEC) == 0);
    file = fdopen(pair[1], "w");
    CHECK(file && setvbuf(file, storage, _IOLBF, sizeof storage) == 0);
    CHECK(fputs("ab", file) >= 0 && read(pair[0], buffer, sizeof buffer) == -1 && errno == EAGAIN);
    CHECK(fputs("c\ndef", file) >= 0 && read(pair[0], buffer, sizeof buffer) == 4 && !memcmp(buffer, "abc\n", 4));
    CHECK(read(pair[0], buffer, sizeof buffer) == -1 && errno == EAGAIN);
    CHECK(fflush(NULL) == 0 && read(pair[0], buffer, sizeof buffer) == 3 && !memcmp(buffer, "def", 3));
    CHECK(fclose(file) == 0 && close(pair[0]) == 0);

    CHECK(syscall(SYS_pipe2, pair, O_NONBLOCK | O_CLOEXEC) == 0);
    CHECK(write(pair[1], "unread bytes", 12) == 12);
    file = fdopen(pair[0], "r");
    CHECK(file && fgetc(file) == 'u');
    CHECK(fclose(file) == 0 && close(pair[1]) == 0);

    CHECK(syscall(SYS_pipe2, pair, O_NONBLOCK | O_CLOEXEC) == 0);
    CHECK(write(pair[1], "first\nsecond\n", 13) == 13 && close(pair[1]) == 0);
    file = fdopen(pair[0], "r");
    CHECK(file != NULL);
    char *line = NULL;
    size_t capacity = 0;
    CHECK(getline(&line, &capacity, file) == 6 && !strcmp(line, "first\n"));
    CHECK(getline(&line, &capacity, file) == 7 && !strcmp(line, "second\n"));
    CHECK(getline(&line, &capacity, file) == -1 && feof(file));
    free(line);
    CHECK(fclose(file) == 0);

    file = fopen("/dev/full", "wb");
    CHECK(file && setvbuf(file, storage, _IOFBF, sizeof storage) == 0);
    CHECK(fwrite("retained", 1, 8, file) == 8);
    CHECK(fflush(file) == EOF && ferror(file) && errno == ENOSPC);
    clearerr(file);
    CHECK(!ferror(file) && fclose(file) == EOF && errno == ENOSPC);

    file = fopen("/dev/null", "wb");
    CHECK(file != NULL);
    flockfile(file); flockfile(file);
    CHECK(ftrylockfile(file) == 0);
    CHECK(fprintf(file, "%s:%08x:%.17g", "locked", 42u, 1.0 / 7.0) > 0);
    funlockfile(file); funlockfile(file); funlockfile(file);
    CHECK(fclose(file) == 0);
    return 0;
}

static int v_stdio_exit_mode;
static void v_stdio_exit_first(void) { fputs("atexit first\n", stdout); }
static void v_stdio_exit_second(void) { fputs("atexit second\n", stdout); }
__attribute__((destructor)) static void v_stdio_elf_finalizer(void) {
    if (v_stdio_exit_mode) fputs("ELF finalizer\n", stdout);
}
void v_freestanding_stdio_exit_output(int explicit_exit) {
    v_stdio_exit_mode = 1;
    if (atexit(v_stdio_exit_first) || atexit(v_stdio_exit_second)) exit(3);
    printf("buffered stdout on exit\n");
    if (explicit_exit) exit(0);
}
