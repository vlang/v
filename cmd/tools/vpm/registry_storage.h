#ifndef VPM_REGISTRY_STORAGE_H
#define VPM_REGISTRY_STORAGE_H

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#ifdef _WIN32
#include <io.h>
#include <windows.h>
#else
#include <unistd.h>
#endif

static inline int vpm_registry_create_file(void *path, void **stream) {
#ifdef _WIN32
    int fd = _wopen((const wchar_t *)path, _O_WRONLY | _O_CREAT | _O_EXCL | _O_BINARY,
                    _S_IREAD | _S_IWRITE);
#else
    int fd = open((const char *)path, O_WRONLY | O_CREAT | O_EXCL, 0600);
#endif
    if (fd < 0) return errno ? errno : EIO;
#ifdef _WIN32
    FILE *file = _fdopen(fd, "wb");
#else
    FILE *file = fdopen(fd, "wb");
#endif
    if (!file) {
        int failure = errno ? errno : EIO;
#ifdef _WIN32
        _close(fd);
        _wunlink((const wchar_t *)path);
#else
        close(fd);
        unlink((const char *)path);
#endif
        return failure;
    }
    *stream = file;
    return 0;
}

static inline int vpm_registry_write_file(void *stream, void *data, size_t length) {
    FILE *file = (FILE *)stream;
    size_t written = 0;
    while (written < length) {
        errno = 0;
        size_t count = fwrite((const unsigned char *)data + written, 1, length - written, file);
        written += count;
        if (ferror(file) || count == 0) {
            int failure = errno ? errno : EIO;
            if (failure == EINTR) {
                clearerr(file);
                continue;
            }
            return failure;
        }
    }
    return 0;
}

static inline int vpm_registry_close_file(void *stream) {
    if (fclose((FILE *)stream) != 0) return errno ? errno : EIO;
    return 0;
}

static inline int vpm_registry_sync_file(void *stream) {
    FILE *file = (FILE *)stream;
    if (fflush(file) != 0) return errno ? errno : EIO;
#ifdef _WIN32
    if (_commit(_fileno(file)) != 0) return errno ? errno : EIO;
#else
    int fd = fileno(file);
    int result;
    do { result = fsync(fd); } while (result != 0 && errno == EINTR);
    if (result != 0) return errno ? errno : EIO;
#ifdef __APPLE__
    do { result = fcntl(fd, F_FULLFSYNC); } while (result != 0 && errno == EINTR);
    if (result != 0) return errno ? errno : EIO;
#endif
#endif
    return 0;
}

static inline int vpm_registry_replace_file(void *source, void *target) {
#ifdef _WIN32
    if (!MoveFileExW((const wchar_t *)source, (const wchar_t *)target,
                     MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH)) return (int)GetLastError();
#else
    if (rename((const char *)source, (const char *)target) != 0) return errno ? errno : EIO;
#endif
    return 0;
}

static inline int vpm_registry_sync_directory(void *path) {
#ifdef _WIN32
    /* Windows has no portable directory fsync. Replacements use
       MOVEFILE_WRITE_THROUGH; ancestor creation remains filesystem-dependent. */
    (void)path;
    return 0;
#else
    int flags = O_RDONLY;
#ifdef O_DIRECTORY
    flags |= O_DIRECTORY;
#endif
    int fd = open((const char *)path, flags);
    if (fd < 0) return errno ? errno : EIO;
    int failure = 0;
    struct stat status;
    if (fstat(fd, &status) != 0) failure = errno ? errno : EIO;
    else if (!S_ISDIR(status.st_mode)) failure = ENOTDIR;
    if (!failure) {
        int result;
        do { result = fsync(fd); } while (result != 0 && errno == EINTR);
        if (result != 0) failure = errno ? errno : EIO;
#ifdef __APPLE__
        if (!failure) {
            do { result = fcntl(fd, F_FULLFSYNC); } while (result != 0 && errno == EINTR);
            if (result != 0) failure = errno ? errno : EIO;
        }
#endif
    }
    if (close(fd) != 0 && !failure) failure = errno ? errno : EIO;
    return failure;
#endif
}

#endif
