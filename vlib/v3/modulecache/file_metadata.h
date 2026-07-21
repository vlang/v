#ifndef V3_MODULECACHE_FILE_METADATA_H
#define V3_MODULECACHE_FILE_METADATA_H

#include <stdint.h>

#if defined(__APPLE__) || defined(__linux__)
#include <sys/stat.h>

static int v3_modulecache_file_metadata(const char *path, uint64_t *device, uint64_t *inode,
	uint64_t *size, uint64_t *mtime_seconds, uint64_t *mtime_nanoseconds,
	uint64_t *ctime_seconds, uint64_t *ctime_nanoseconds) {
	struct stat info;
	if (stat(path, &info) != 0) {
		return 0;
	}
	*device = (uint64_t)info.st_dev;
	*inode = (uint64_t)info.st_ino;
	*size = (uint64_t)info.st_size;
#if defined(__APPLE__)
	*mtime_seconds = (uint64_t)info.st_mtimespec.tv_sec;
	*mtime_nanoseconds = (uint64_t)info.st_mtimespec.tv_nsec;
	*ctime_seconds = (uint64_t)info.st_ctimespec.tv_sec;
	*ctime_nanoseconds = (uint64_t)info.st_ctimespec.tv_nsec;
#else
	*mtime_seconds = (uint64_t)info.st_mtim.tv_sec;
	*mtime_nanoseconds = (uint64_t)info.st_mtim.tv_nsec;
	*ctime_seconds = (uint64_t)info.st_ctim.tv_sec;
	*ctime_nanoseconds = (uint64_t)info.st_ctim.tv_nsec;
#endif
	return 1;
}
#else
static int v3_modulecache_file_metadata(const char *path, uint64_t *device, uint64_t *inode,
	uint64_t *size, uint64_t *mtime_seconds, uint64_t *mtime_nanoseconds,
	uint64_t *ctime_seconds, uint64_t *ctime_nanoseconds) {
	(void)path;
	(void)device;
	(void)inode;
	(void)size;
	(void)mtime_seconds;
	(void)mtime_nanoseconds;
	(void)ctime_seconds;
	(void)ctime_nanoseconds;
	return 0;
}
#endif

#endif
