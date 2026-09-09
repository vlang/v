#ifndef V_OS_FILELOCK_HELPERS_H
#define V_OS_FILELOCK_HELPERS_H

#include <string.h>

#ifdef _WIN32
#include <windows.h>

static int v_filelock_lock(HANDLE handle, int exclusive, int immediate,
	unsigned long long start, unsigned long long len) {
	OVERLAPPED overlap;
	memset(&overlap, 0, sizeof(overlap));
	overlap.Offset = (DWORD)(start & 0xffffffffULL);
	overlap.OffsetHigh = (DWORD)(start >> 32);
	DWORD flags = immediate ? LOCKFILE_FAIL_IMMEDIATELY : 0;
	if (exclusive) {
		flags |= LOCKFILE_EXCLUSIVE_LOCK;
	}
	DWORD low = len == 0 ? MAXDWORD : (DWORD)(len & 0xffffffffULL);
	DWORD high = len == 0 ? MAXDWORD : (DWORD)(len >> 32);
	return LockFileEx(handle, flags, 0, low, high, &overlap) ? 0 : -1;
}

static int v_filelock_unlock(HANDLE handle, unsigned long long start,
	unsigned long long len) {
	OVERLAPPED overlap;
	memset(&overlap, 0, sizeof(overlap));
	overlap.Offset = (DWORD)(start & 0xffffffffULL);
	overlap.OffsetHigh = (DWORD)(start >> 32);
	DWORD low = len == 0 ? MAXDWORD : (DWORD)(len & 0xffffffffULL);
	DWORD high = len == 0 ? MAXDWORD : (DWORD)(len >> 32);
	return UnlockFileEx(handle, 0, low, high, &overlap) ? 0 : -1;
}

#else
#include <fcntl.h>
#include <unistd.h>

static int v_filelock_lock(int fd, int exclusive, int immediate,
	unsigned long long start, unsigned long long len) {
	struct flock fl;
	memset(&fl, 0, sizeof(fl));
	fl.l_type = exclusive ? F_WRLCK : F_RDLCK;
	fl.l_whence = SEEK_SET;
	fl.l_start = (off_t)start;
	fl.l_len = len == 0 ? 0 : (off_t)len;
	return fcntl(fd, immediate ? F_SETLK : F_SETLKW, &fl);
}

static int v_filelock_unlock(int fd, unsigned long long start,
	unsigned long long len) {
	struct flock fl;
	memset(&fl, 0, sizeof(fl));
	fl.l_type = F_UNLCK;
	fl.l_whence = SEEK_SET;
	fl.l_start = (off_t)start;
	fl.l_len = len == 0 ? 0 : (off_t)len;
	return fcntl(fd, F_SETLK, &fl);
}

#endif

#endif
