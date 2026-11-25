#ifndef V_RUNTIME_DIRECTORY_SCAN_PROBE_H
#define V_RUNTIME_DIRECTORY_SCAN_PROBE_H
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

static int scan_visible(const struct dirent *entry) { errno = EINTR; return entry->d_name[0] != '.'; }
static int scan_items64(const struct dirent64 *entry) { errno = EINTR; return !strncmp(entry->d_name, "item", 4); }
static int scan_none64(const struct dirent64 *entry) { (void)entry; return 0; }
static int scan_reverse(const struct dirent **a, const struct dirent **b) { return strcmp((*b)->d_name, (*a)->d_name); }
#define DIRECTORY_CHECK(test) do { if (!(test)) { result = __LINE__; goto cleanup; } } while (0)
static int directory_scan_native_probe(void) {
    int result = 0, count = -1;
    struct dirent **entries = NULL;
    struct dirent64 **large = NULL;
    char directory[] = "/tmp/vlibc-directory-XXXXXX", path[512];
    DIRECTORY_CHECK(mkdtemp(directory));
    for (int i = 0; i < 73; ++i) {
        const char *extra[] = {"item10", "item2", "item01"};
        if (i < 70) snprintf(path, sizeof(path), "%s/entry_%03d", directory, i);
        else snprintf(path, sizeof(path), "%s/%s", directory, extra[i - 70]);
        int fd = open(path, O_CREAT | O_WRONLY | O_EXCL, 0600);
        DIRECTORY_CHECK(fd >= 0);
        DIRECTORY_CHECK(!close(fd));
    }
    errno = EDOM;
    count = scandir(directory, &entries, scan_visible, alphasort);
    DIRECTORY_CHECK(count == 73 && errno == EDOM);
    for (int i = 1; i < count; ++i) DIRECTORY_CHECK(strcmp(entries[i - 1]->d_name, entries[i]->d_name) < 0);
    for (int i = 0; i < count; ++i) free(entries[i]);
    free(entries); entries = NULL; count = -1;
    count = scandir(directory, &entries, scan_visible, scan_reverse);
    DIRECTORY_CHECK(count == 73);
    for (int i = 1; i < count; ++i) DIRECTORY_CHECK(strcmp(entries[i - 1]->d_name, entries[i]->d_name) > 0);
    for (int i = 0; i < count; ++i) free(entries[i]);
    free(entries); entries = NULL; count = -1;
    count = scandir64(directory, &large, scan_items64, versionsort64);
    DIRECTORY_CHECK(count == 3 && !strcmp(large[0]->d_name, "item01"));
    DIRECTORY_CHECK(!strcmp(large[1]->d_name, "item2") && !strcmp(large[2]->d_name, "item10"));
    for (int i = 0; i < count; ++i) free(large[i]);
    free(large); large = NULL; count = -1;
    count = scandir64(directory, &large, scan_none64, alphasort64);
    DIRECTORY_CHECK(count == 0);
    free(large); large = NULL; count = -1;
    errno = 0;
    DIRECTORY_CHECK(scandir64("/vlibc-directory-missing", &large, NULL, NULL) == -1 && errno == ENOENT);
    snprintf(path, sizeof(path), "%s/item2", directory);
    DIRECTORY_CHECK(!eaccess(path, R_OK | W_OK) && !euidaccess(path, R_OK));
    errno = 0;
    DIRECTORY_CHECK(eaccess(path, X_OK) == -1 && errno == EACCES);
    errno = 0;
    DIRECTORY_CHECK(eaccess("/vlibc-effective-access-missing", R_OK) == -1 && errno == ENOENT);
#ifndef V_RUNTIME_HOSTED_ORACLE
    /* Out-of-contract mode bits are rejected by the kernel-backed runtime;
     * the GNU hosted eaccess implementation masks these bits instead. */
    errno = 0;
    DIRECTORY_CHECK(eaccess(path, 8) == -1 && errno == EINVAL);
#endif
cleanup:
    if (entries) { for (int i = 0; i < count; ++i) free(entries[i]); free(entries); }
    if (large) { for (int i = 0; i < count; ++i) free(large[i]); free(large); }
    for (int i = 0; i < 73; ++i) {
        const char *extra[] = {"item10", "item2", "item01"};
        if (i < 70) snprintf(path, sizeof(path), "%s/entry_%03d", directory, i);
        else snprintf(path, sizeof(path), "%s/%s", directory, extra[i - 70]);
        unlink(path);
    }
    rmdir(directory);
    if (!result) puts("directory growth, native callbacks, 64-bit records and version sorting passed");
    return result;
}
#undef DIRECTORY_CHECK

static int media_string_process_probe(void) {
    char *haystack = malloc(32769), *needle = malloc(1025);
    if (!haystack || !needle) { free(haystack); free(needle); return __LINE__; }
    memset(haystack, 'A', 32768); haystack[32767] = 'b'; haystack[32768] = 0;
    memset(needle, 'a', 1024); needle[1023] = 'B'; needle[1024] = 0;
    int valid = strcasestr(haystack, needle) == haystack + 32768 - 1024
        && strcasestr(haystack, "") == haystack && !strcasestr(haystack, "C");
    free(haystack); free(needle);
    if (!valid) return __LINE__;
    char tokens[] = ",one;;two:three";
    char *token = strtok(tokens, ",;");
    if (!token || strcmp(token, "one")) return __LINE__;
    token = strtok(NULL, ":");
    if (!token || strcmp(token, ";two")) return __LINE__;
    token = strtok(NULL, ":");
    if (!token || strcmp(token, "three") || strtok(NULL, ":")) return __LINE__;
    if (getpgid(0) != getpgrp() || getsid(0) <= 0) return __LINE__;
    pid_t child = fork();
    if (child < 0) return __LINE__;
    if (!child) _exit(setpgrp() || getpgrp() != getpid() || setpgid(0, 0) ? 95 : 0);
    int status = 0;
    if (waitpid(child, &status, 0) != child || !WIFEXITED(status) || WEXITSTATUS(status)) return __LINE__;
    struct timespec before, after;
    if (clock_gettime(CLOCK_MONOTONIC, &before) || usleep(2000) || clock_gettime(CLOCK_MONOTONIC, &after)) return __LINE__;
    if ((after.tv_sec - before.tv_sec) * 1000000000LL + after.tv_nsec - before.tv_nsec < 1000000) return __LINE__;
    puts("case-insensitive search, token state, process groups and microsecond sleep passed");
    return 0;
}
#endif
