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
#elif defined(_WIN32)
#include <stdlib.h>
#include <windows.h>

// v3_modulecache_filetime_parts splits a FILETIME into whole seconds since the
// Unix epoch plus the leftover nanoseconds, matching the stat() fields above.
static void v3_modulecache_filetime_parts(FILETIME value, u64 *seconds, u64 *nanoseconds) {
	u64 ticks = ((u64)value.dwHighDateTime << 32) | (u64)value.dwLowDateTime;
	const u64 ticks_1601_to_1970 = 116444736000000000ULL;
	if (ticks < ticks_1601_to_1970) {
		*seconds = 0;
		*nanoseconds = 0;
		return;
	}
	ticks -= ticks_1601_to_1970;
	*seconds = ticks / 10000000ULL;
	*nanoseconds = (ticks % 10000000ULL) * 100ULL;
}

// The CRT's stat() reports st_ino == 0 on Windows, so a stat-based signature
// would make every file on the volume share one identity. Go to the file handle
// instead: the volume serial number and the 64 bit file index are the real
// device/inode pair, and they come from the same call as the size and times.
static int v3_modulecache_file_metadata(const char *path, u64 *device, u64 *inode,
	u64 *size, u64 *mtime_seconds, u64 *mtime_nanoseconds,
	u64 *ctime_seconds, u64 *ctime_nanoseconds) {
	int wide_length = MultiByteToWideChar(CP_UTF8, 0, path, -1, NULL, 0);
	if (wide_length <= 0) {
		return 0;
	}
	WCHAR *wide_path = (WCHAR *)malloc((size_t)wide_length * sizeof(WCHAR));
	if (wide_path == NULL) {
		return 0;
	}
	if (MultiByteToWideChar(CP_UTF8, 0, path, -1, wide_path, wide_length) <= 0) {
		free(wide_path);
		return 0;
	}
	// No access bits: this opens for metadata only, so it cannot fail on a file
	// another process holds exclusively, and FILE_FLAG_BACKUP_SEMANTICS lets the
	// same call describe a directory.
	HANDLE handle = CreateFileW(wide_path, 0,
		FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, OPEN_EXISTING,
		FILE_FLAG_BACKUP_SEMANTICS, NULL);
	free(wide_path);
	if (handle == INVALID_HANDLE_VALUE) {
		return 0;
	}
	BY_HANDLE_FILE_INFORMATION info;
	if (!GetFileInformationByHandle(handle, &info)) {
		CloseHandle(handle);
		return 0;
	}
	CloseHandle(handle);
	u64 file_index = ((u64)info.nFileIndexHigh << 32) | (u64)info.nFileIndexLow;
	if (file_index == 0) {
		// Some network redirectors report no file index. Reporting a zero inode
		// would make distinct files compare equal, so decline and let the caller
		// compare the contents instead.
		return 0;
	}
	*device = (u64)info.dwVolumeSerialNumber;
	*inode = file_index;
	*size = ((u64)info.nFileSizeHigh << 32) | (u64)info.nFileSizeLow;
	v3_modulecache_filetime_parts(info.ftLastWriteTime, mtime_seconds, mtime_nanoseconds);
	// Windows has no inode change time. Creation time is the closest stable
	// companion field; the write time and size above are what actually move when
	// a source file is edited.
	v3_modulecache_filetime_parts(info.ftCreationTime, ctime_seconds, ctime_nanoseconds);
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
