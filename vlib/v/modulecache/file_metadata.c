#include <stdint.h>

#if defined(__APPLE__) || defined(__linux__)
#include <sys/stat.h>

int v3_modulecache_file_metadata(const char *path, uint64_t *device, uint64_t *inode,
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
#elif defined(_WIN32)
#include <stdlib.h>
#include <windows.h>

// v3_modulecache_filetime_parts splits a Windows file time (100 ns ticks since
// 1601) into whole seconds since the Unix epoch plus the leftover nanoseconds,
// matching the stat() fields above.
static void v3_modulecache_filetime_parts(uint64_t ticks, uint64_t *seconds, uint64_t *nanoseconds) {
	const uint64_t ticks_1601_to_1970 = 116444736000000000ULL;
	if (ticks < ticks_1601_to_1970) {
		*seconds = 0;
		*nanoseconds = 0;
		return;
	}
	ticks -= ticks_1601_to_1970;
	*seconds = ticks / 10000000ULL;
	*nanoseconds = (ticks % 10000000ULL) * 100ULL;
}

// v3_modulecache_file_basic_info mirrors FILE_BASIC_INFO, which the headers
// bundled with tcc do not declare.
typedef struct {
	LARGE_INTEGER creation_time;
	LARGE_INTEGER last_access_time;
	LARGE_INTEGER last_write_time;
	LARGE_INTEGER change_time;
	DWORD file_attributes;
} v3_modulecache_file_basic_info;

typedef BOOL (WINAPI *v3_modulecache_get_file_information_by_handle_ex_fn)(HANDLE, int, LPVOID, DWORD);

// v3_modulecache_change_time returns the handle's change time: unlike the write
// time, it also moves when the write time itself is set back, so a same-size
// edit that restores the old write time is still visible. It returns 0 when no
// change time is available: FAT and exFAT report 0, and the query itself can
// fail. The function is looked up at run time because the kernel32 import list
// bundled with tcc has no GetFileInformationByHandleEx.
static uint64_t v3_modulecache_change_time(HANDLE handle) {
	HMODULE kernel32 = GetModuleHandleW(L"kernel32.dll");
	if (kernel32 == NULL) {
		return 0;
	}
	v3_modulecache_get_file_information_by_handle_ex_fn get_info =
		(v3_modulecache_get_file_information_by_handle_ex_fn)GetProcAddress(kernel32,
		"GetFileInformationByHandleEx");
	if (get_info == NULL) {
		return 0;
	}
	v3_modulecache_file_basic_info info;
	// 0 is FileBasicInfo in FILE_INFO_BY_HANDLE_CLASS.
	if (!get_info(handle, 0, &info, sizeof(info)) || info.change_time.QuadPart <= 0) {
		return 0;
	}
	return (uint64_t)info.change_time.QuadPart;
}

// The CRT's stat() reports st_ino == 0 on Windows, so a stat-based signature
// would make every file on the volume share one identity. Go to the file handle
// instead: the volume serial number and the 64 bit file index are the real
// device/inode pair, and they come from the same call as the size and times.
int v3_modulecache_file_metadata(const char *path, uint64_t *device, uint64_t *inode,
	uint64_t *size, uint64_t *mtime_seconds, uint64_t *mtime_nanoseconds,
	uint64_t *ctime_seconds, uint64_t *ctime_nanoseconds) {
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
	uint64_t change_time = v3_modulecache_change_time(handle);
	CloseHandle(handle);
	uint64_t file_index = ((uint64_t)info.nFileIndexHigh << 32) | (uint64_t)info.nFileIndexLow;
	if (file_index == 0 || change_time == 0) {
		// Some network redirectors report no file index, and FAT and exFAT keep no
		// change time. A zero inode would make distinct files compare equal, and
		// without a change time an edit that restores the old write time goes
		// unseen, so decline and let the caller compare the contents instead.
		return 0;
	}
	*device = (uint64_t)info.dwVolumeSerialNumber;
	*inode = file_index;
	*size = ((uint64_t)info.nFileSizeHigh << 32) | (uint64_t)info.nFileSizeLow;
	v3_modulecache_filetime_parts(((uint64_t)info.ftLastWriteTime.dwHighDateTime << 32)
		| (uint64_t)info.ftLastWriteTime.dwLowDateTime, mtime_seconds, mtime_nanoseconds);
	v3_modulecache_filetime_parts(change_time, ctime_seconds, ctime_nanoseconds);
	return 1;
}
#else
int v3_modulecache_file_metadata(const char *path, uint64_t *device, uint64_t *inode,
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
