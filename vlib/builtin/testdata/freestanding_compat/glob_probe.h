#ifndef V_RUNTIME_GLOB_PROBE_H
#define V_RUNTIME_GLOB_PROBE_H
#include <glob.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

static unsigned glob_opens, glob_closes, glob_reads, glob_stats, glob_errors;
static void *glob_open(const char *path) {
    ++glob_opens;
    if (strstr(path, "/blocked")) { errno = EACCES; return NULL; }
    return opendir(path);
}
static void glob_close(void *dir) { ++glob_closes; closedir(dir); }
static struct dirent64 *glob_read(void *dir) { ++glob_reads; return readdir64(dir); }
static int glob_stat(const char *p, struct stat64 *s) { ++glob_stats; return stat64(p, s); }
static int glob_lstat(const char *p, struct stat64 *s) { ++glob_stats; return lstat64(p, s); }
static int glob_error(const char *path, int code) {
    if (strstr(path, "/blocked") && code == EACCES) ++glob_errors;
    return 1;
}
#define GLOB_CHECK(x) do { if (!(x)) { result = __LINE__; goto cleanup; } } while (0)
static int glob_native_probe(void) {
    char directory[] = "/tmp/vlibc-glob-XXXXXX", pattern[512], path[512], expected[512];
    const char *names[] = {"a.mp4", "b.mp4", ".hidden.mp4", "literal*.mp4", "other.txt"};
    glob_t g = {0}; glob64_t big = {0};
    int result = 0, fd = -1;
    const char *old_home = getenv("HOME"); char *saved_home = old_home ? strdup(old_home) : NULL;
    GLOB_CHECK(!old_home || saved_home);
    GLOB_CHECK(mkdtemp(directory));
    for (unsigned i = 0; i < sizeof(names) / sizeof(*names); ++i) {
        snprintf(path, sizeof(path), "%s/%s", directory, names[i]);
        fd = open(path, O_CREAT | O_EXCL | O_RDWR, 0600); GLOB_CHECK(fd >= 0);
        if (!i) {
            const off64_t offset = (off64_t)5 * 1024 * 1024 * 1024 + 37;
            GLOB_CHECK(!ftruncate64(fd, offset + 1));
            GLOB_CHECK(pwrite64(fd, "Z", 1, offset) == 1);
            char byte = 0; GLOB_CHECK(pread64(fd, &byte, 1, offset) == 1 && byte == 'Z');
            GLOB_CHECK(lseek64(fd, 0, SEEK_CUR) == 0);
            GLOB_CHECK(!ftruncate64(fd, 0));
        }
        GLOB_CHECK(!close(fd)); fd = -1;
    }
    snprintf(path, sizeof(path), "%s/branch", directory); GLOB_CHECK(!mkdir(path, 0700));
    snprintf(path, sizeof(path), "%s/branch/nested.mp4", directory);
    fd = open(path, O_CREAT | O_EXCL | O_WRONLY, 0600); GLOB_CHECK(fd >= 0);
    GLOB_CHECK(!close(fd)); fd = -1;
    snprintf(path, sizeof(path), "%s/dangling", directory); GLOB_CHECK(!symlink("absent", path));
    snprintf(path, sizeof(path), "%s/link", directory); GLOB_CHECK(!symlink("branch", path));
    snprintf(pattern, sizeof(pattern), "%s/*.mp4", directory);
    GLOB_CHECK(!glob(pattern, 0, NULL, &g) && g.gl_pathc == 3 && !g.gl_pathv[3]);
    for (size_t i = 1; i < g.gl_pathc; ++i) GLOB_CHECK(strcmp(g.gl_pathv[i - 1], g.gl_pathv[i]) < 0);
    globfree(&g); memset(&g, 0, sizeof(g));
    GLOB_CHECK(!glob(pattern, GLOB_PERIOD, NULL, &g) && g.gl_pathc == 4);
    globfree(&g); memset(&g, 0, sizeof(g));
    snprintf(pattern, sizeof(pattern), "%s/{b*,{a*,missing*}}.mp4", directory);
    GLOB_CHECK(!glob(pattern, GLOB_BRACE | GLOB_NOCHECK, NULL, &g) && g.gl_pathc == 2);
    snprintf(expected, sizeof(expected), "%s/b.mp4", directory); GLOB_CHECK(!strcmp(g.gl_pathv[0], expected));
    snprintf(expected, sizeof(expected), "%s/a.mp4", directory); GLOB_CHECK(!strcmp(g.gl_pathv[1], expected));
    globfree(&g); memset(&g, 0, sizeof(g));
    snprintf(pattern, sizeof(pattern), "%s/literal\\*.mp4", directory);
    GLOB_CHECK(!glob(pattern, 0, NULL, &g) && g.gl_pathc == 1);
    snprintf(expected, sizeof(expected), "%s/literal*.mp4", directory); GLOB_CHECK(!strcmp(g.gl_pathv[0], expected));
    globfree(&g); memset(&g, 0, sizeof(g));
    snprintf(pattern, sizeof(pattern), "%s/*/nested.mp4", directory);
    GLOB_CHECK(!glob(pattern, 0, NULL, &g) && g.gl_pathc == 2);
    globfree(&g); memset(&g, 0, sizeof(g));
    snprintf(pattern, sizeof(pattern), "%s/dangling", directory);
    GLOB_CHECK(!glob(pattern, 0, NULL, &g) && g.gl_pathc == 1);
    globfree(&g); memset(&g, 0, sizeof(g));
    snprintf(pattern, sizeof(pattern), "%s/*", directory);
    GLOB_CHECK(!glob(pattern, GLOB_ONLYDIR | GLOB_MARK, NULL, &g) && g.gl_pathc == 2);
    GLOB_CHECK(g.gl_pathv[0][strlen(g.gl_pathv[0]) - 1] == '/' && g.gl_pathv[1][strlen(g.gl_pathv[1]) - 1] == '/');
    globfree(&g); memset(&g, 0, sizeof(g));
    g.gl_offs = 2;
    snprintf(pattern, sizeof(pattern), "%s/b.mp4", directory);
    GLOB_CHECK(!glob(pattern, GLOB_DOOFFS, NULL, &g) && g.gl_pathc == 1 && !g.gl_pathv[0] && !g.gl_pathv[1]);
    g.gl_pathv[0] = (char *)"command";
    snprintf(pattern, sizeof(pattern), "%s/a.mp4", directory);
    GLOB_CHECK(!glob(pattern, GLOB_DOOFFS | GLOB_APPEND, NULL, &g) && g.gl_pathc == 2);
    GLOB_CHECK(!strcmp(g.gl_pathv[0], "command") && !g.gl_pathv[1] && !g.gl_pathv[4]);
    snprintf(expected, sizeof(expected), "%s/b.mp4", directory); GLOB_CHECK(!strcmp(g.gl_pathv[2], expected));
    snprintf(expected, sizeof(expected), "%s/a.mp4", directory); GLOB_CHECK(!strcmp(g.gl_pathv[3], expected));
    globfree(&g); memset(&g, 0, sizeof(g));
    snprintf(pattern, sizeof(pattern), "%s/{no-such-one,no-such-two}", directory);
    GLOB_CHECK(!glob(pattern, GLOB_BRACE | GLOB_NOCHECK, NULL, &g) && g.gl_pathc == 1 && !strcmp(g.gl_pathv[0], pattern));
    globfree(&g); memset(&g, 0, sizeof(g));
    GLOB_CHECK(glob(pattern, GLOB_BRACE, NULL, &g) == GLOB_NOMATCH && !g.gl_pathc);
    globfree(&g); memset(&g, 0, sizeof(g));
    GLOB_CHECK(!setenv("HOME", directory, 1));
    GLOB_CHECK(!glob("~/*.mp4", GLOB_TILDE, NULL, &g) && g.gl_pathc == 3);
    globfree(&g); memset(&g, 0, sizeof(g));
    GLOB_CHECK(glob("~vlibc_user_does_not_exist_091817/file", GLOB_TILDE_CHECK | GLOB_NOCHECK, NULL, &g) == GLOB_NOMATCH);
    globfree(&g); memset(&g, 0, sizeof(g));
    big.gl_opendir = glob_open; big.gl_closedir = glob_close; big.gl_readdir = glob_read;
    big.gl_stat = glob_stat; big.gl_lstat = glob_lstat;
    snprintf(pattern, sizeof(pattern), "%s/*.mp4", directory);
    GLOB_CHECK(!glob64(pattern, GLOB_ALTDIRFUNC | GLOB_MARK, NULL, &big) && big.gl_pathc == 3);
    GLOB_CHECK(glob_opens == 1 && glob_closes == 1 && glob_reads > 3 && glob_stats >= 3);
    globfree64(&big); big.gl_pathc = 0; big.gl_pathv = NULL;
    snprintf(pattern, sizeof(pattern), "%s/blocked/*", directory);
    GLOB_CHECK(glob64(pattern, GLOB_ALTDIRFUNC, glob_error, &big) == GLOB_ABORTED && glob_errors == 1);
    globfree64(&big); big.gl_pathc = 0; big.gl_pathv = NULL;
    for (int i = 0; i < 128; ++i) {
        snprintf(pattern, sizeof(pattern), "%s/*", directory);
        GLOB_CHECK(!glob64(pattern, 0, NULL, &big) && big.gl_pathc == 7);
        globfree64(&big); big.gl_pathc = 0; big.gl_pathv = NULL;
    }
cleanup:
    if (fd >= 0) close(fd);
    globfree(&g); globfree64(&big);
    if (saved_home) setenv("HOME", saved_home, 1); else unsetenv("HOME");
    free(saved_home);
    for (unsigned i = 0; i < sizeof(names) / sizeof(*names); ++i) {
        snprintf(path, sizeof(path), "%s/%s", directory, names[i]); unlink(path);
    }
    snprintf(path, sizeof(path), "%s/branch/nested.mp4", directory); unlink(path);
    snprintf(path, sizeof(path), "%s/branch", directory); rmdir(path);
    snprintf(path, sizeof(path), "%s/link", directory); unlink(path);
    snprintf(path, sizeof(path), "%s/dangling", directory); unlink(path);
    rmdir(directory);
    if (!result) puts("glob expansion, callbacks, append ownership and sparse 64-bit I/O passed");
    return result;
}
#undef GLOB_CHECK
static int strerror_native_probe(void) {
#ifdef __GLIBC__
    extern int __xpg_strerror_r(int, char *, size_t);
    char text[100]; errno = EDOM;
    if (__xpg_strerror_r(ENOENT, text, sizeof(text)) || strcmp(text, "No such file or directory") || errno != EDOM) return __LINE__;
    memset(text, 'X', sizeof(text));
    if (__xpg_strerror_r(ENOENT, text, 5) != ERANGE || strcmp(text, "No s") || text[5] != 'X' || errno != EDOM) return __LINE__;
    if (__xpg_strerror_r(ENOENT, NULL, 0) != ERANGE || errno != EDOM) return __LINE__;
    if (__xpg_strerror_r(999, text, sizeof(text)) != EINVAL || strcmp(text, "Unknown error 999")) return __LINE__;
    if (__xpg_strerror_r(-2147483647 - 1, text, sizeof(text)) != EINVAL || strcmp(text, "Unknown error -2147483648")) return __LINE__;
    memset(text, 'X', sizeof(text));
    if (__xpg_strerror_r(999, text, 5) != EINVAL || strcmp(text, "Unkn") || text[5] != 'X') return __LINE__;
    if (strcmp(strerror_r(EACCES, text, 1), "Permission denied") || errno != EDOM) return __LINE__;
#endif
    puts("POSIX and GNU reentrant error formatting passed"); return 0;
}
#endif
