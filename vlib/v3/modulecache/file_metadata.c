#include <stdint.h>

#if defined(__APPLE__) || defined(__linux__)
#include <sys/stat.h>

static int v3_modulecache_file_metadata(const char *path, u64 *device, u64 *inode,
	u64 *size, u64 *mtime_seconds, u64 *mtime_nanoseconds,
	u64 *ctime_seconds, u64 *ctime_nanoseconds) {
	struct stat info;
	if (stat(path, &info) != 0) {
		return 0;
	}
	*device = (u64)info.st_dev;
	*inode = (u64)info.st_ino;
	*size = (u64)info.st_size;
#if defined(__APPLE__)
	*mtime_seconds = (u64)info.st_mtimespec.tv_sec;
	*mtime_nanoseconds = (u64)info.st_mtimespec.tv_nsec;
	*ctime_seconds = (u64)info.st_ctimespec.tv_sec;
	*ctime_nanoseconds = (u64)info.st_ctimespec.tv_nsec;
#else
	*mtime_seconds = (u64)info.st_mtim.tv_sec;
	*mtime_nanoseconds = (u64)info.st_mtim.tv_nsec;
	*ctime_seconds = (u64)info.st_ctim.tv_sec;
	*ctime_nanoseconds = (u64)info.st_ctim.tv_nsec;
#endif
	return 1;
}
#else
static int v3_modulecache_file_metadata(const char *path, u64 *device, u64 *inode,
	u64 *size, u64 *mtime_seconds, u64 *mtime_nanoseconds,
	u64 *ctime_seconds, u64 *ctime_nanoseconds) {
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
