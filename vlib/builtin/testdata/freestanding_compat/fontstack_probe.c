#define _GNU_SOURCE 1
#include "probe.h"
#include <arpa/inet.h>
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <float.h>
#include <langinfo.h>
#include <limits.h>
#include <locale.h>
#include <libintl.h>
#include <sys/random.h>
#include <math.h>
#include <pthread.h>
#include <pwd.h>
#include <setjmp.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/select.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <unistd.h>
#include <wchar.h>
extern void arc4random_buf(void *, size_t);

#define CHECK(x)                                                                                   \
    do {                                                                                           \
        if (!(x))                                                                                  \
            return __LINE__;                                                                       \
    } while (0)
#ifdef V_PROBE_SANITIZED
#define V_PROBE_ABI(name) "v_bare_" name
#else
#define V_PROBE_ABI(name) name
#endif
extern long v_probe_legacy_strtol(const char *, char **, int) __asm__(V_PROBE_ABI("strtol"));
extern long v_probe_c23_strtol(const char *, char **, int) __asm__(V_PROBE_ABI("__isoc23_strtol"));
extern int v_probe_c23_sscanf(const char *, const char *,
                              ...) __asm__(V_PROBE_ABI("__isoc23_sscanf"));
extern int __cxa_atexit(void (*)(void *), void *, void *);
extern void __cxa_finalize(void *);
static int v_integer_compare(const void *a, const void *b) {
    int x = *(const int *)a, y = *(const int *)b;
    return (x > y) - (x < y);
}
static int v_context_compare(const void *a, const void *b, void *context) {
    return v_integer_compare(a, b) * *(const int *)context;
}
static int v_callback_total;
static void v_callback(void *argument) {
    v_callback_total = v_callback_total * 10 + *(int *)argument;
}
static int v_numeric_checks(void) {
    CHECK(__fpclassifyl(-0.0L) == FP_ZERO && __signbitl(-0.0L));
    CHECK(__fpclassifyl(1.0L) == FP_NORMAL && !__signbitl(1.0L));
    CHECK(__fpclassifyl((long double)INFINITY) == FP_INFINITE);
    CHECK(__fpclassifyl((long double)NAN) == FP_NAN);
    CHECK(__fpclassifyl(LDBL_TRUE_MIN) == FP_SUBNORMAL);
    char *end;
    errno = 0;
    CHECK(v_probe_legacy_strtol("-9223372036854775808!", &end, 10) == LONG_MIN && *end == '!' &&
          !errno);
    CHECK(v_probe_legacy_strtol("9223372036854775808!", &end, 10) == LONG_MAX && *end == '!' &&
          errno == ERANGE);
    CHECK(v_probe_legacy_strtol("0x!", &end, 0) == 0 && *end == 'x');
    CHECK(v_probe_legacy_strtol("0b101!", &end, 0) == 0 && *end == 'b');
    CHECK(v_probe_c23_strtol("0b101!", &end, 0) == 5 && *end == '!');
    CHECK(v_probe_c23_strtol("-0B101!", &end, 2) == -5 && *end == '!');
    CHECK(v_probe_c23_strtol("0b!", &end, 0) == 0 && *end == 'b');
    errno = 0;
    CHECK(strtoull("18446744073709551616z", &end, 10) == ULLONG_MAX && *end == 'z' &&
          errno == ERANGE);
    CHECK(strtoull("-1z", &end, 10) == ULLONG_MAX && *end == 'z');
    int x = -1, n = -1;
    CHECK(v_probe_c23_sscanf("0b101!", "%i%n", &x, &n) == 1 && x == 5 && n == 5);
    CHECK(v_probe_c23_sscanf("101!", "%b%n", &x, &n) == 1 && x == 5 && n == 3);
    char b[5];
    double d;
    CHECK(sscanf("12 1.25 abcd!", "%d %lf %4[a-z]%n", &x, &d, b, &n) == 3 && x == 12 && d == 1.25 &&
          !strcmp(b, "abcd") && n == 12);
    CHECK(sscanf("", "%d", &x) == EOF);
    char *allocated = NULL;
    CHECK(sscanf("hello!", "%m[a-z]", &allocated) == 1 && !strcmp(allocated, "hello"));
    free(allocated);
    const char *cases[] = {"1e", "1e+", "0x1p", "infinX", "nan(foo"};
    const int offsets[] = {1, 1, 3, 3, 3};
    for (size_t i = 0; i < sizeof cases / sizeof *cases; ++i) {
        d = strtod(cases[i], &end);
        CHECK(end - cases[i] == offsets[i]);
    }
    CHECK(strtod("0x.8p-4!", &end) == 0.03125 && *end == '!');
    CHECK(strtod("4.9406564584124654e-324", &end) == 0x1p-1074 && !*end);
    errno = 0;
    CHECK(isinf(strtod("1e5000!", &end)) && *end == '!' && errno == ERANGE);
    errno = 0;
    CHECK(strtod("1e-5000!", &end) == 0 && *end == '!' && errno == ERANGE);
    CHECK(signbit(strtod("-0!", &end)) && *end == '!');
    char *text = NULL;
    CHECK(asprintf(&text, "%02000d/%n%s", 7, &n, "done") == 2005 && n == 2001 &&
          strlen(text) == 2005 && !strcmp(text + 2000, "/done"));
    free(text);
    char *retained = malloc(4);
    CHECK(retained);
    retained[0] = 'Z';
    volatile size_t impossible = SIZE_MAX;
    errno = 0;
    CHECK(!reallocarray(retained, impossible, 2) && errno == ENOMEM && retained[0] == 'Z');
    free(retained);
#ifndef V_PROBE_SANITIZED
    errno = 0;
    CHECK(!malloc(impossible) && errno == ENOMEM);
    errno = 0;
    CHECK(!calloc(impossible, 2) && errno == ENOMEM);
#endif
    int values[1024];
    for (int i = 0; i < 1024; ++i)
        values[i] = (i * 137) % 127;
    qsort(values, 1024, sizeof *values, v_integer_compare);
    for (int i = 1; i < 1024; ++i)
        CHECK(values[i - 1] <= values[i]);
    int key = 73;
    CHECK(bsearch(&key, values, 1024, sizeof *values, v_integer_compare) != NULL);
    key = 200;
    CHECK(bsearch(&key, values, 1024, sizeof *values, v_integer_compare) == NULL);
    int direction = -1;
    qsort_r(values, 1024, sizeof *values, v_context_compare, &direction);
    for (int i = 1; i < 1024; ++i)
        CHECK(values[i - 1] >= values[i]);
    return 0;
}
static int v_locale_checks(void) {
    CHECK(setlocale(LC_ALL, "C.UTF-8"));
    CHECK(!strcmp(nl_langinfo(CODESET), "UTF-8"));
    char wide_text[16];
    CHECK(snprintf(wide_text, sizeof wide_text, "%lc/%ls", (wint_t)0xe9, L"\U0001f600") == 7);
    CHECK(!strcmp(wide_text, "\xc3\xa9/\xf0\x9f\x98\x80"));
    CHECK(MB_CUR_MAX == 4 && isalpha('a') && isspace('\t') && !isalpha(0xff) &&
          tolower('Z') == 'z');
    mbstate_t st = {0};
    wchar_t wc = 0;
    CHECK(mbrtowc(&wc, "\xf0\x9f", 2, &st) == (size_t)-2 && !mbsinit(&st));
    CHECK(mbrtowc(&wc, "\x98\x80", 2, &st) == 2 && wc == 0x1f600 && mbsinit(&st));
    CHECK(mbrtowc(&wc, "\xed\xa0\x80", 3, &st) == (size_t)-1 && errno == EILSEQ && mbsinit(&st));
    CHECK(mbrtowc(&wc, "\xc0\x80", 2, &st) == (size_t)-1 && errno == EILSEQ);
    CHECK(mbrtowc(&wc, "\xf4\x90\x80\x80", 4, &st) == (size_t)-1 && errno == EILSEQ);
    CHECK(mbrlen("\xe2", 1, NULL) == (size_t)-2);
    CHECK(mbrtowc(&wc, "A", 1, NULL) == 1 && wc == 'A');
    CHECK(mbrlen("\x82\xac", 2, NULL) == 2);
    char utf[8];
    CHECK(wcrtomb(utf, 0x1f600, &st) == 4 && !memcmp(utf, "\xf0\x9f\x98\x80", 4));
    CHECK(wcrtomb(utf, 0xd800, &st) == (size_t)-1 && errno == EILSEQ);
    const char *input = "A\xf0\x9f\x98\x80";
    wchar_t wide[4] = {0};
    CHECK(mbsrtowcs(NULL, &input, 0, &st) == 2 && *input == 'A' && mbsinit(&st));
    CHECK(mbsrtowcs(wide, &input, 1, &st) == 1 && wide[0] == L'A' && (unsigned char)*input == 0xf0);
    CHECK(mbsrtowcs(wide + 1, &input, 3, &st) == 1 && !input && wide[1] == 0x1f600 && !wide[2]);
    const wchar_t *reverse = wide;
    CHECK(wcsrtombs(NULL, &reverse, 0, &st) == 5 && reverse == wide);
    CHECK(wcsrtombs(utf, &reverse, 3, &st) == 1 && reverse == wide + 1 && utf[0] == 'A');
    CHECK(wcsrtombs(utf + 1, &reverse, 7, &st) == 4 && !reverse &&
          !memcmp(utf, "A\xf0\x9f\x98\x80", 6));
    CHECK(mbstowcs(wide, "\xc3\xa9", 4) == 1 && wide[0] == 0xe9 && !wide[1]);
    CHECK(wcstombs(utf, L"\u00e9", sizeof utf) == 2 && !memcmp(utf, "\xc3\xa9", 3));
    const wchar_t invalid[] = {0xd800, 0};
    reverse = invalid;
    errno = 0;
    CHECK(wcsrtombs(utf, &reverse, 0, &st) == 0 && reverse == invalid && !errno);
    CHECK(wcsrtombs(utf, &reverse, sizeof utf, &st) == (size_t)-1 && errno == EILSEQ);
    CHECK(!strcmp(dcgettext("fontconfig", "source message", LC_MESSAGES), "source message"));
    locale_t c = newlocale(LC_ALL_MASK, "C", (locale_t)0);
    CHECK(c);
    locale_t previous = uselocale(c);
    CHECK(MB_CUR_MAX == 1 && !strcmp(nl_langinfo(CODESET), "ANSI_X3.4-1968"));
    CHECK(mbrtowc(&wc, "\xc3\xa9", 2, &st) == (size_t)-1);
    locale_t duplicate = duplocale(c);
    CHECK(duplicate);
    CHECK(!newlocale(LC_CTYPE_MASK, "V_missing_locale", (locale_t)0) && errno == ENOENT);
    uselocale(previous);
    freelocale(duplicate);
    freelocale(c);
    CHECK(!strcmp(localeconv()->decimal_point, ".") && localeconv()->frac_digits == CHAR_MAX);
    struct tm t = {.tm_year = 121,
                   .tm_mon = 0,
                   .tm_mday = 1,
                   .tm_wday = 5,
                   .tm_yday = 0,
                   .tm_isdst = 0,
                   .tm_zone = "UTC"};
    char date[128];
    CHECK(strftime(date, sizeof date, "%F %G-W%V-%u %z %Z %s", &t) > 0);
    CHECK(!strcmp(date, "2021-01-01 2020-W53-5 +0000 UTC 1609459200"));
    CHECK(strftime(date, 3, "%Y", &t) == 0);
    CHECK(setlocale(LC_ALL, "C"));
    CHECK(setlocale(LC_CTYPE, "C.UTF-8"));
    char *saved = strdup(setlocale(LC_ALL, NULL));
    CHECK(saved);
    CHECK(setlocale(LC_ALL, "C") && MB_CUR_MAX == 1);
    CHECK(setlocale(LC_ALL, saved) && MB_CUR_MAX == 4);
    free(saved);
    CHECK(setlocale(LC_ALL, "C"));
    return 0;
}
static int v_jump_checks(void) {
    jmp_buf jump;
    volatile int stage = 0;
    switch (setjmp(jump)) {
    case 0: stage = 1; longjmp(jump, 0);
    case 1: CHECK(stage == 1); break;
    default: return __LINE__;
    }
    sigset_t before, blocked, after;
    CHECK(sigprocmask(SIG_SETMASK, NULL, &before) == 0);
    sigemptyset(&blocked);
    sigaddset(&blocked, SIGUSR1);
    CHECK(sigprocmask(SIG_BLOCK, &blocked, NULL) == 0);
    sigjmp_buf masked_jump;
    switch (sigsetjmp(masked_jump, 1)) {
    case 0:
        CHECK(sigprocmask(SIG_UNBLOCK, &blocked, NULL) == 0);
        siglongjmp(masked_jump, 27);
    case 27: break;
    default: return __LINE__;
    }
    CHECK(sigprocmask(SIG_SETMASK, NULL, &after) == 0 &&
          sigismember(&after, SIGUSR1) == 1);
    CHECK(sigprocmask(SIG_SETMASK, &before, NULL) == 0);
    return 0;
}
static int v_system_checks(const char *directory) {
    char path[1024];
    CHECK(snprintf(path, sizeof path, "%s/fontstack-XXXXXX", directory) > 0);
    int fd = mkstemp(path);
    CHECK(fd >= 0);
    struct iovec out[2] = {{(void *)"ab", 2}, {(void *)"cd", 2}};
    CHECK(writev(fd, out, 2) == 4 && lseek(fd, 0, SEEK_SET) == 0);
    char contents[4];
    struct iovec in[2] = {{contents, 1}, {contents + 1, 3}};
    CHECK(readv(fd, in, 2) == 4 && !memcmp(contents, "abcd", 4));
    FILE *stream = fdopen(fd, "r+");
    CHECK(stream && fseek(stream, 0, SEEK_SET) == 0);
    CHECK(fgetc(stream) == 'a' && ungetc('X', stream) == 'X' && ftell(stream) == 0 &&
          fgetc(stream) == 'X' && fgetc(stream) == 'b');
    CHECK(fseek(stream, 4, SEEK_SET) == 0 && fgetc(stream) == EOF && feof(stream));
    CHECK(ungetc('Q', stream) == 'Q' && !feof(stream) && fgetc(stream) == 'Q');
    CHECK(fseek(stream, 0, SEEK_SET) == 0);
    char two[3];
    CHECK(fscanf(stream, "%2c", two) == 1 && two[0] == 'a' && two[1] == 'b' &&
          fgetc(stream) == 'c');
    CHECK(fclose(stream) == 0);
    char *canonical = realpath(path, NULL);
    CHECK(canonical && canonical[0] == '/');
    free(canonical);
    char linkpath[1100];
    snprintf(linkpath, sizeof linkpath, "%s.link", path);
    CHECK(symlink(path, linkpath) == 0);
    struct stat st;
    CHECK(lstat(linkpath, &st) == 0 && S_ISLNK(st.st_mode));
    CHECK(stat(linkpath, &st) == 0 && st.st_size == 4);
    char *a = realpath(path, NULL), *b = realpath(linkpath, NULL);
    CHECK(a && b && !strcmp(a, b));
    free(a);
    free(b);
    CHECK(unlink(linkpath) == 0 && unlink(path) == 0);
    DIR *d = opendir(directory);
    CHECK(d);
    int seen = 0;
    struct dirent *entry;
    errno = 0;
    while ((entry = readdir(d))) {
        CHECK(entry->d_reclen >= 20);
        ++seen;
    }
    CHECK(!errno && seen >= 2);
    rewinddir(d);
    CHECK(readdir(d));
    CHECK(closedir(d) == 0);
    char *cwd = getcwd(NULL, 0);
    CHECK(cwd && cwd[0] == '/');
    free(cwd);
    void *mapping = mmap(NULL, 8192, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    CHECK(mapping != MAP_FAILED);
    memset(mapping, 0x5a, 8192);
    CHECK(munmap(mapping, 8192) == 0);
    int pipefd[2];
    CHECK(pipe(pipefd) == 0);
    CHECK(write(pipefd[1], "z", 1) == 1);
    fd_set fds;
    FD_ZERO(&fds);
    FD_SET(pipefd[0], &fds);
    struct timeval timeout = {0};
    CHECK(select(pipefd[0] + 1, &fds, NULL, NULL, &timeout) == 1 && FD_ISSET(pipefd[0], &fds));
    int available = -1;
    CHECK(ioctl(pipefd[0], FIONREAD, &available) == 0 && available == 1);
    close(pipefd[0]);
    close(pipefd[1]);
    char address[INET6_ADDRSTRLEN];
    unsigned char ip[16];
    CHECK(inet_pton(AF_INET6, "2001:db8::1", ip) == 1);
    CHECK(inet_ntop(AF_INET6, ip, address, sizeof address) && !strcmp(address, "2001:db8::1"));
    errno = 0;
    CHECK(!inet_ntop(AF_INET6, ip, address, 3) && errno == ENOSPC);
    struct passwd pwd, *found = NULL;
    char buffer[16384];
    CHECK(getpwuid_r(getuid(), &pwd, buffer, sizeof buffer, &found) == 0 && found &&
          pwd.pw_uid == getuid());
    CHECK(getpwnam_r(pwd.pw_name, &pwd, buffer, sizeof buffer, &found) == 0 && found);
    CHECK(getpwnam_r("V_compat_no_such_user_861952", &pwd, buffer, sizeof buffer, &found) == 0 &&
          !found);
    CHECK(setenv("V_NATIVE_ENV_INTEROP", "C-side", 1) == 0 &&
          !strcmp(getenv("V_NATIVE_ENV_INTEROP"), "C-side"));
    return 0;
}
int v_freestanding_fontstack_check(const char *directory) {
    int result = v_numeric_checks();
    if (result)
        return result;
    result = v_locale_checks();
    if (result)
        return result;
    result = v_jump_checks();
    if (result)
        return result;
    result = v_system_checks(directory);
    if (result)
        return result;
    int first = 1, second = 2, dso;
    CHECK(__cxa_atexit(v_callback, &first, &dso) == 0 &&
          __cxa_atexit(v_callback, &second, &dso) == 0);
    __cxa_finalize(&dso);
    CHECK(v_callback_total == 21);
    __cxa_finalize(&dso);
    CHECK(v_callback_total == 21);
    time_t epoch = 1609459200;
    struct tm *calendar = gmtime(&epoch);
    CHECK(calendar && calendar->tm_year == 121 && calendar->tm_mon == 0 &&
          calendar->tm_mday == 1 && calendar->tm_wday == 5);
    CHECK(sleep(0) == 0);
    char bytes[] = {'a', (char)0x80, 'z', 0}, accepted[] = {(char)0x80, 0};
    CHECK(strpbrk(bytes, accepted) == bytes + 1 && !strpbrk(bytes, "") && !strpbrk("", accepted));
#if defined(__GLIBC__)
    char sparse[PATH_MAX];
    CHECK(snprintf(sparse, sizeof sparse, "%s/sparse-XXXXXX", directory) > 0);
    int sparse_fd = mkstemp(sparse);
    CHECK(sparse_fd >= 0 && lseek64(sparse_fd, (off64_t)1 << 33, SEEK_SET) == ((off64_t)1 << 33));
    CHECK(write(sparse_fd, "X", 1) == 1);
    struct stat64 sparse_stat;
    CHECK(fstat64(sparse_fd, &sparse_stat) == 0 && sparse_stat.st_size == (((off64_t)1 << 33) + 1));
    CHECK(close(sparse_fd) == 0 && unlink(sparse) == 0);
#endif
    unsigned char random[64], zero[64] = {0};
    CHECK(getentropy(random, sizeof random) == 0 && memcmp(random, zero, sizeof random));
    errno = 0;
    unsigned char excessive[257] = {0};
    CHECK(getentropy(excessive, sizeof excessive) == -1 && errno == EIO);
    CHECK(getrandom(random, sizeof random, 0) == sizeof random);
    CHECK(pathconf(directory, _PC_PATH_MAX) == PATH_MAX);
    CHECK(pathconf(directory, _PC_NAME_MAX) > 0);
    errno = 0;
    CHECK(fpathconf(-1, _PC_PATH_MAX) == -1 && errno == EBADF);
    errno = 0;
    CHECK(pathconf(directory, -1) == -1 && errno == EINVAL);
    arc4random_buf(random, sizeof random);
    CHECK(memcmp(random, zero, sizeof random));
    srand(1);
    CHECK(rand() == 1804289383 && rand() == 846930886);
    return 0;
}
static pthread_mutex_t v_condition_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t v_condition = PTHREAD_COND_INITIALIZER;
static int v_condition_waiting, v_condition_release, v_condition_done, v_condition_errors;
void v_freestanding_condition_worker(void) {
    if (pthread_mutex_lock(&v_condition_mutex)) {
        __atomic_add_fetch(&v_condition_errors, 1, __ATOMIC_RELAXED);
        return;
    }
    ++v_condition_waiting;
    pthread_cond_broadcast(&v_condition);
    while (!v_condition_release)
        if (pthread_cond_wait(&v_condition, &v_condition_mutex)) {
            ++v_condition_errors;
            break;
        }
    ++v_condition_done;
    pthread_cond_broadcast(&v_condition);
    pthread_mutex_unlock(&v_condition_mutex);
}
int v_freestanding_condition_release(void) {
    CHECK(pthread_mutex_lock(&v_condition_mutex) == 0);
    while (v_condition_waiting < 8)
        CHECK(pthread_cond_wait(&v_condition, &v_condition_mutex) == 0);
    CHECK(pthread_cond_destroy(&v_condition) == EBUSY);
    v_condition_release = 1;
    CHECK(pthread_cond_broadcast(&v_condition) == 0);
    while (v_condition_done < 8)
        CHECK(pthread_cond_wait(&v_condition, &v_condition_mutex) == 0);
    CHECK(pthread_mutex_unlock(&v_condition_mutex) == 0 && !v_condition_errors);
    pthread_condattr_t attr;
    CHECK(pthread_condattr_init(&attr) == 0 &&
          pthread_condattr_setclock(&attr, CLOCK_MONOTONIC) == 0);
    pthread_cond_t timed;
    CHECK(pthread_cond_init(&timed, &attr) == 0 && pthread_condattr_destroy(&attr) == 0);
    struct timespec deadline;
    CHECK(clock_gettime(CLOCK_MONOTONIC, &deadline) == 0);
    deadline.tv_nsec += 2000000;
    if (deadline.tv_nsec >= 1000000000) {
        deadline.tv_nsec -= 1000000000;
        ++deadline.tv_sec;
    }
    CHECK(pthread_mutex_lock(&v_condition_mutex) == 0);
    CHECK(pthread_cond_timedwait(&timed, &v_condition_mutex, &deadline) == ETIMEDOUT);
    CHECK(pthread_mutex_unlock(&v_condition_mutex) == 0 && pthread_cond_destroy(&timed) == 0);
    CHECK(pthread_cond_destroy(&v_condition) == 0);
    return 0;
}

int v_freestanding_fontstack_environment(void) {
    const char *s = getenv("V_NATIVE_ENV_INTEROP");
    return s && !strcmp(s, "V-side");
}
