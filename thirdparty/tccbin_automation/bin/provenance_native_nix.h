#ifndef TCCBIN_AUTOMATION_PROVENANCE_NATIVE_NIX_H
#define TCCBIN_AUTOMATION_PROVENANCE_NATIVE_NIX_H

#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#define TCCBIN_FILE_NAMES_STATUS_SUCCESS ((int32_t) 0L)
#define TCCBIN_FILE_NAMES_STATUS_NO_MORE_FILES ((int32_t) 0x80000006UL)

enum tccbin_file_names_constant {
	TCCBIN_FILE_NAMES_BUFFER_SIZE = 64 * 1024,
	TCCBIN_FILE_NAMES_INFORMATION = 12,
	TCCBIN_FILE_NAMES_HEADER_SIZE = 12,
	TCCBIN_FILE_NAMES_NEXT_OFFSET = 0,
	TCCBIN_FILE_NAMES_INDEX_OFFSET = 4,
	TCCBIN_FILE_NAMES_LENGTH_OFFSET = 8
};

enum tccbin_file_names_result {
	TCCBIN_FILE_NAMES_FINISHED = 0,
	TCCBIN_FILE_NAMES_READY = 1,
	TCCBIN_FILE_NAMES_SKIPPED = 2,
	TCCBIN_FILE_NAMES_ERROR_STATUS = -1,
	TCCBIN_FILE_NAMES_ERROR_INFORMATION = -2,
	TCCBIN_FILE_NAMES_ERROR_HEADER = -3,
	TCCBIN_FILE_NAMES_ERROR_NAME = -4,
	TCCBIN_FILE_NAMES_ERROR_NEXT = -5,
	TCCBIN_FILE_NAMES_ERROR_CAPACITY = -6,
	TCCBIN_FILE_NAMES_ERROR_ARGUMENT = -7
};

static int tccbin_file_names_apply_batch_status(int32_t status,
	uint64_t information, uint64_t capacity, uint64_t *valid_bytes,
	uint64_t *offset, int *batch_ready, int *finished) {
	if (valid_bytes == NULL || offset == NULL || batch_ready == NULL ||
		finished == NULL || capacity == 0 ||
		capacity > TCCBIN_FILE_NAMES_BUFFER_SIZE) {
		return TCCBIN_FILE_NAMES_ERROR_ARGUMENT;
	}
	if (status == TCCBIN_FILE_NAMES_STATUS_NO_MORE_FILES) {
		if (information != 0) {
			return TCCBIN_FILE_NAMES_ERROR_INFORMATION;
		}
		*valid_bytes = 0;
		*offset = 0;
		*batch_ready = 0;
		*finished = 1;
		return TCCBIN_FILE_NAMES_FINISHED;
	}
	if (status != TCCBIN_FILE_NAMES_STATUS_SUCCESS) {
		return TCCBIN_FILE_NAMES_ERROR_STATUS;
	}
	if (information == 0 || information > capacity ||
		information > TCCBIN_FILE_NAMES_BUFFER_SIZE) {
		return TCCBIN_FILE_NAMES_ERROR_INFORMATION;
	}
	*valid_bytes = information;
	*offset = 0;
	*batch_ready = 1;
	*finished = 0;
	return TCCBIN_FILE_NAMES_READY;
}

static int tccbin_file_names_decode_record(const unsigned char *batch,
	uint64_t valid_bytes, uint64_t *offset, int *batch_ready,
	uint16_t *output, uint64_t output_capacity, uint64_t *output_length) {
	uint32_t next;
	uint32_t file_index;
	uint32_t filename_length_field;
	uint64_t remaining;
	uint64_t filename_bytes;
	uint64_t filename_length;
	uint64_t next_offset;
	if (batch == NULL || offset == NULL || batch_ready == NULL || output == NULL ||
		output_length == NULL || output_capacity == 0 || *batch_ready == 0) {
		return TCCBIN_FILE_NAMES_ERROR_ARGUMENT;
	}
	if (*offset > valid_bytes ||
		valid_bytes - *offset < TCCBIN_FILE_NAMES_HEADER_SIZE) {
		return TCCBIN_FILE_NAMES_ERROR_HEADER;
	}
	remaining = valid_bytes - *offset;
	memcpy(&next, batch + (size_t) *offset + TCCBIN_FILE_NAMES_NEXT_OFFSET,
		sizeof(next));
	memcpy(&file_index, batch + (size_t) *offset + TCCBIN_FILE_NAMES_INDEX_OFFSET,
		sizeof(file_index));
	memcpy(&filename_length_field,
		batch + (size_t) *offset + TCCBIN_FILE_NAMES_LENGTH_OFFSET,
		sizeof(filename_length_field));
	(void) file_index;
	filename_bytes = (uint64_t) filename_length_field;
	if (filename_bytes == 0 || filename_bytes % sizeof(uint16_t) != 0 ||
		filename_bytes > remaining - TCCBIN_FILE_NAMES_HEADER_SIZE) {
		return TCCBIN_FILE_NAMES_ERROR_NAME;
	}
	filename_length = filename_bytes / sizeof(uint16_t);
	if (filename_length + 1 > output_capacity || filename_length > INT_MAX) {
		return TCCBIN_FILE_NAMES_ERROR_CAPACITY;
	}
	if (next == 0) {
		next_offset = *offset;
	} else {
		if ((uint64_t) next < TCCBIN_FILE_NAMES_HEADER_SIZE + filename_bytes ||
			(uint64_t) next > remaining - TCCBIN_FILE_NAMES_HEADER_SIZE ||
			next % sizeof(uint32_t) != 0) {
			return TCCBIN_FILE_NAMES_ERROR_NEXT;
		}
		next_offset = *offset + (uint64_t) next;
	}
	memcpy(output, batch + (size_t) *offset + TCCBIN_FILE_NAMES_HEADER_SIZE,
		(size_t) filename_bytes);
	output[filename_length] = 0;
	*output_length = filename_length;
	if (next == 0) {
		*batch_ready = 0;
	} else {
		*offset = next_offset;
	}
	if ((filename_length == 1 && output[0] == (uint16_t) '.') ||
		(filename_length == 2 && output[0] == (uint16_t) '.' &&
		 output[1] == (uint16_t) '.')) {
		return TCCBIN_FILE_NAMES_SKIPPED;
	}
	return TCCBIN_FILE_NAMES_READY;
}

#ifdef _WIN32

#include <windows.h>
#include <stdlib.h>
#include <wchar.h>

/* Keep the reviewed NT ABI layouts inside C. V sees only opaque HANDLE/enumerator pointers. */
typedef LONG tccbin_windows_ntstatus;

typedef struct tccbin_windows_unicode_string {
	USHORT Length;
	USHORT MaximumLength;
	PWSTR Buffer;
} tccbin_windows_unicode_string;

typedef struct tccbin_windows_object_attributes {
	ULONG Length;
	HANDLE RootDirectory;
	tccbin_windows_unicode_string *ObjectName;
	ULONG Attributes;
	PVOID SecurityDescriptor;
	PVOID SecurityQualityOfService;
} tccbin_windows_object_attributes;

typedef union tccbin_windows_io_status_value {
	tccbin_windows_ntstatus Status;
	PVOID Pointer;
} tccbin_windows_io_status_value;

typedef struct tccbin_windows_io_status_block {
	tccbin_windows_io_status_value Value;
	ULONG_PTR Information;
} tccbin_windows_io_status_block;

#define TCCBIN_WINDOWS_ABI_ASSERT(name, expression) \
	typedef char name[(expression) ? 1 : -1]

TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_wchar_is_two_bytes,
	sizeof(WCHAR) == 2);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_long_is_four_bytes,
	sizeof(LONG) == 4);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_ulong_is_four_bytes,
	sizeof(ULONG) == 4);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_ushort_is_two_bytes,
	sizeof(USHORT) == 2);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_boolean_is_one_byte,
	sizeof(BOOLEAN) == 1);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_handle_is_pointer_sized,
	sizeof(HANDLE) == sizeof(void *));
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_ulong_ptr_is_pointer_sized,
	sizeof(ULONG_PTR) == sizeof(void *));
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_unicode_length_offset,
	offsetof(tccbin_windows_unicode_string, Length) == 0);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_unicode_maximum_length_offset,
	offsetof(tccbin_windows_unicode_string, MaximumLength) == 2);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_object_length_offset,
	offsetof(tccbin_windows_object_attributes, Length) == 0);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_io_value_offset,
	offsetof(tccbin_windows_io_status_block, Value) == 0);

#if defined(_WIN64)
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_pointer_is_eight_bytes,
	sizeof(void *) == 8);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_unicode_size_x64,
	sizeof(tccbin_windows_unicode_string) == 16);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_unicode_buffer_offset_x64,
	offsetof(tccbin_windows_unicode_string, Buffer) == 8);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_object_size_x64,
	sizeof(tccbin_windows_object_attributes) == 48);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_object_root_offset_x64,
	offsetof(tccbin_windows_object_attributes, RootDirectory) == 8);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_object_name_offset_x64,
	offsetof(tccbin_windows_object_attributes, ObjectName) == 16);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_object_attributes_offset_x64,
	offsetof(tccbin_windows_object_attributes, Attributes) == 24);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_object_security_offset_x64,
	offsetof(tccbin_windows_object_attributes, SecurityDescriptor) == 32);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_object_quality_offset_x64,
	offsetof(tccbin_windows_object_attributes, SecurityQualityOfService) == 40);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_io_size_x64,
	sizeof(tccbin_windows_io_status_block) == 16);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_io_information_offset_x64,
	offsetof(tccbin_windows_io_status_block, Information) == 8);
#else
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_pointer_is_four_bytes,
	sizeof(void *) == 4);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_unicode_size_x86,
	sizeof(tccbin_windows_unicode_string) == 8);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_unicode_buffer_offset_x86,
	offsetof(tccbin_windows_unicode_string, Buffer) == 4);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_object_size_x86,
	sizeof(tccbin_windows_object_attributes) == 24);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_object_root_offset_x86,
	offsetof(tccbin_windows_object_attributes, RootDirectory) == 4);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_object_name_offset_x86,
	offsetof(tccbin_windows_object_attributes, ObjectName) == 8);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_object_attributes_offset_x86,
	offsetof(tccbin_windows_object_attributes, Attributes) == 12);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_object_security_offset_x86,
	offsetof(tccbin_windows_object_attributes, SecurityDescriptor) == 16);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_object_quality_offset_x86,
	offsetof(tccbin_windows_object_attributes, SecurityQualityOfService) == 20);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_io_size_x86,
	sizeof(tccbin_windows_io_status_block) == 8);
TCCBIN_WINDOWS_ABI_ASSERT(tccbin_windows_io_information_offset_x86,
	offsetof(tccbin_windows_io_status_block, Information) == 4);
#endif

#undef TCCBIN_WINDOWS_ABI_ASSERT

typedef VOID (NTAPI *tccbin_windows_io_apc_routine_fn)(PVOID,
	tccbin_windows_io_status_block *, ULONG);

typedef tccbin_windows_ntstatus (NTAPI *tccbin_nt_open_file_fn)(PHANDLE,
	ACCESS_MASK, tccbin_windows_object_attributes *,
	tccbin_windows_io_status_block *, ULONG, ULONG);

typedef tccbin_windows_ntstatus (NTAPI *tccbin_nt_query_directory_file_fn)(HANDLE,
	HANDLE, tccbin_windows_io_apc_routine_fn, PVOID,
	tccbin_windows_io_status_block *, PVOID, ULONG, LONG, BOOLEAN,
	tccbin_windows_unicode_string *, BOOLEAN);

enum tccbin_windows_native_constant {
	TCCBIN_WINDOWS_OBJECT_CASE_INSENSITIVE = 0x00000040,
	TCCBIN_WINDOWS_FILE_DIRECTORY_FILE = 0x00000001,
	TCCBIN_WINDOWS_FILE_SEQUENTIAL_ONLY = 0x00000004,
	TCCBIN_WINDOWS_FILE_SYNCHRONOUS_IO_NONALERT = 0x00000020,
	TCCBIN_WINDOWS_FILE_NON_DIRECTORY_FILE = 0x00000040,
	TCCBIN_WINDOWS_FILE_OPEN_REPARSE_POINT = 0x00200000
};

typedef struct tccbin_windows_directory_enumerator {
	HANDLE directory;
	tccbin_nt_query_directory_file_fn query_directory;
	unsigned char buffer[TCCBIN_FILE_NAMES_BUFFER_SIZE];
	uint64_t valid_bytes;
	uint64_t offset;
	int batch_ready;
	int restart;
	int finished;
} tccbin_windows_directory_enumerator;

enum tccbin_windows_child_mode {
	TCCBIN_WINDOWS_CHILD_DOCUMENT = 0,
	TCCBIN_WINDOWS_CHILD_DIRECTORY = 1,
	TCCBIN_WINDOWS_CHILD_SNAPSHOT = 2
};

static int tccbin_windows_child_name_is_safe(const wchar_t *name) {
	const wchar_t *cursor;
	if (name == NULL || name[0] == L'\0' || wcscmp(name, L".") == 0 ||
		wcscmp(name, L"..") == 0) {
		return 0;
	}
	for (cursor = name; *cursor != L'\0'; ++cursor) {
		if (*cursor == L'/' || *cursor == L'\\') {
			return 0;
		}
	}
	return 1;
}

static void *tccbin_windows_open_directory_path_no_follow(const wchar_t *path) {
	HANDLE handle = CreateFileW(path,
		FILE_LIST_DIRECTORY | FILE_READ_ATTRIBUTES | SYNCHRONIZE,
		FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
		FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL);
	return (void *) handle;
}

static void *tccbin_windows_open_child_no_follow(void *parent_pointer,
	const wchar_t *name, int mode) {
	HANDLE parent = (HANDLE) parent_pointer;
	HMODULE module;
	tccbin_nt_open_file_fn nt_open_file;
	tccbin_windows_unicode_string unicode_name;
	tccbin_windows_object_attributes attributes;
	tccbin_windows_io_status_block status_block;
	HANDLE handle = INVALID_HANDLE_VALUE;
	ACCESS_MASK access;
	ULONG share;
	ULONG options;
	size_t name_length;
	tccbin_windows_ntstatus status;
	if (parent == NULL || parent == INVALID_HANDLE_VALUE ||
		!tccbin_windows_child_name_is_safe(name) ||
		(mode != TCCBIN_WINDOWS_CHILD_DOCUMENT &&
		 mode != TCCBIN_WINDOWS_CHILD_DIRECTORY &&
		 mode != TCCBIN_WINDOWS_CHILD_SNAPSHOT)) {
		return (void *) INVALID_HANDLE_VALUE;
	}
	name_length = wcslen(name);
	if (name_length == 0 || name_length > (USHRT_MAX / sizeof(wchar_t))) {
		return (void *) INVALID_HANDLE_VALUE;
	}
	module = GetModuleHandleW(L"ntdll.dll");
	if (module == NULL) {
		return (void *) INVALID_HANDLE_VALUE;
	}
	nt_open_file = (tccbin_nt_open_file_fn) GetProcAddress(module, "NtOpenFile");
	if (nt_open_file == NULL) {
		return (void *) INVALID_HANDLE_VALUE;
	}
	unicode_name.Length = (USHORT) (name_length * sizeof(wchar_t));
	unicode_name.MaximumLength = unicode_name.Length;
	unicode_name.Buffer = (PWSTR) name;
	memset(&attributes, 0, sizeof(attributes));
	attributes.Length = sizeof(attributes);
	attributes.RootDirectory = parent;
	attributes.ObjectName = &unicode_name;
	attributes.Attributes = TCCBIN_WINDOWS_OBJECT_CASE_INSENSITIVE;
	memset(&status_block, 0, sizeof(status_block));
	access = FILE_READ_ATTRIBUTES | SYNCHRONIZE;
	share = FILE_SHARE_READ | FILE_SHARE_WRITE;
	options = TCCBIN_WINDOWS_FILE_OPEN_REPARSE_POINT |
		TCCBIN_WINDOWS_FILE_SYNCHRONOUS_IO_NONALERT;
	if (mode == TCCBIN_WINDOWS_CHILD_DOCUMENT) {
		access |= FILE_READ_DATA;
		share = FILE_SHARE_READ;
		options |= TCCBIN_WINDOWS_FILE_NON_DIRECTORY_FILE |
			TCCBIN_WINDOWS_FILE_SEQUENTIAL_ONLY;
	} else if (mode == TCCBIN_WINDOWS_CHILD_DIRECTORY) {
		access |= FILE_LIST_DIRECTORY;
		options |= TCCBIN_WINDOWS_FILE_DIRECTORY_FILE;
	}
	status = nt_open_file(&handle, access, &attributes, &status_block, share, options);
	if ((int32_t) status != TCCBIN_FILE_NAMES_STATUS_SUCCESS || handle == NULL ||
		handle == INVALID_HANDLE_VALUE) {
		if (handle != NULL && handle != INVALID_HANDLE_VALUE) {
			CloseHandle(handle);
		}
		return (void *) INVALID_HANDLE_VALUE;
	}
	return (void *) handle;
}

static void *tccbin_windows_open_directory_enumerator(void *parent_pointer) {
	HANDLE parent = (HANDLE) parent_pointer;
	HMODULE module;
	tccbin_nt_query_directory_file_fn query_directory;
	tccbin_windows_directory_enumerator *enumerator;
	if (parent == NULL || parent == INVALID_HANDLE_VALUE) {
		return NULL;
	}
	module = GetModuleHandleW(L"ntdll.dll");
	if (module == NULL) {
		return NULL;
	}
	query_directory = (tccbin_nt_query_directory_file_fn)
		GetProcAddress(module, "NtQueryDirectoryFile");
	if (query_directory == NULL) {
		return NULL;
	}
	enumerator = (tccbin_windows_directory_enumerator *)
		calloc(1, sizeof(tccbin_windows_directory_enumerator));
	if (enumerator == NULL) {
		return NULL;
	}
	enumerator->directory = parent;
	enumerator->query_directory = query_directory;
	enumerator->restart = 1;
	return (void *) enumerator;
}

static int tccbin_windows_load_directory_batch(
	tccbin_windows_directory_enumerator *enumerator) {
	tccbin_windows_io_status_block status_block;
	tccbin_windows_ntstatus status;
	int batch_result;
	if (enumerator == NULL || enumerator->finished) {
		return 0;
	}
	memset(enumerator->buffer, 0, sizeof(enumerator->buffer));
	memset(&status_block, 0, sizeof(status_block));
	status = enumerator->query_directory(enumerator->directory, NULL, NULL, NULL,
		&status_block, enumerator->buffer, (ULONG) sizeof(enumerator->buffer),
		(LONG) TCCBIN_FILE_NAMES_INFORMATION, FALSE, NULL,
		enumerator->restart ? TRUE : FALSE);
	batch_result = tccbin_file_names_apply_batch_status((int32_t) status,
		(uint64_t) status_block.Information, (uint64_t) sizeof(enumerator->buffer),
		&enumerator->valid_bytes, &enumerator->offset, &enumerator->batch_ready,
		&enumerator->finished);
	if (batch_result == TCCBIN_FILE_NAMES_READY) {
		enumerator->restart = 0;
	}
	return batch_result;
}

static int tccbin_windows_read_directory_entry(void *enumerator_pointer,
	wchar_t *buffer, uint64_t capacity) {
	tccbin_windows_directory_enumerator *enumerator =
		(tccbin_windows_directory_enumerator *) enumerator_pointer;
	uint64_t filename_length;
	int decoded;
	int loaded;
	if (enumerator == NULL || buffer == NULL || capacity == 0) {
		return -1;
	}
	for (;;) {
		if (enumerator->finished) {
			return 0;
		}
		if (!enumerator->batch_ready) {
			loaded = tccbin_windows_load_directory_batch(enumerator);
			if (loaded <= 0) {
				return loaded;
			}
		}
		decoded = tccbin_file_names_decode_record(enumerator->buffer,
			enumerator->valid_bytes, &enumerator->offset, &enumerator->batch_ready,
			(uint16_t *) buffer, capacity, &filename_length);
		if (decoded == TCCBIN_FILE_NAMES_SKIPPED) {
			continue;
		}
		if (decoded != TCCBIN_FILE_NAMES_READY) {
			return decoded;
		}
		return (int) filename_length;
	}
}

static int tccbin_windows_close_directory_enumerator(void *enumerator_pointer) {
	tccbin_windows_directory_enumerator *enumerator =
		(tccbin_windows_directory_enumerator *) enumerator_pointer;
	int result = 0;
	if (enumerator == NULL) {
		return -1;
	}
	free(enumerator);
	return result;
}

#else

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

static inline int tccbin_sigchld_read(struct sigaction *previous) {
	if (previous == NULL) {
		errno = EINVAL;
		return -1;
	}
	return sigaction(SIGCHLD, NULL, previous);
}

typedef struct tccbin_native_stat_snapshot {
	uint64_t volume;
	uint64_t index;
	uint64_t links;
	uint64_t size;
	uint32_t mode;
	int64_t mtime_sec;
	int64_t mtime_nsec;
	int64_t ctime_sec;
	int64_t ctime_nsec;
	int regular;
} tccbin_native_stat_snapshot;

static void tccbin_snapshot_from_stat(const struct stat *information,
	tccbin_native_stat_snapshot *snapshot) {
	snapshot->volume = (uint64_t) information->st_dev;
	snapshot->index = (uint64_t) information->st_ino;
	snapshot->links = (uint64_t) information->st_nlink;
	snapshot->size = (uint64_t) information->st_size;
	snapshot->mode = (uint32_t) information->st_mode;
#if defined(__APPLE__)
	snapshot->mtime_sec = (int64_t) information->st_mtimespec.tv_sec;
	snapshot->mtime_nsec = (int64_t) information->st_mtimespec.tv_nsec;
	snapshot->ctime_sec = (int64_t) information->st_ctimespec.tv_sec;
	snapshot->ctime_nsec = (int64_t) information->st_ctimespec.tv_nsec;
#else
	snapshot->mtime_sec = (int64_t) information->st_mtim.tv_sec;
	snapshot->mtime_nsec = (int64_t) information->st_mtim.tv_nsec;
	snapshot->ctime_sec = (int64_t) information->st_ctim.tv_sec;
	snapshot->ctime_nsec = (int64_t) information->st_ctim.tv_nsec;
#endif
	snapshot->regular = S_ISREG(information->st_mode) ? 1 : 0;
}

static int tccbin_lstat_snapshot(const char *path, tccbin_native_stat_snapshot *snapshot) {
	struct stat information;
	if (lstat(path, &information) != 0) {
		return -1;
	}
	tccbin_snapshot_from_stat(&information, snapshot);
	return 0;
}

static int tccbin_fstat_snapshot(int fd, tccbin_native_stat_snapshot *snapshot) {
	struct stat information;
	if (fstat(fd, &information) != 0) {
		return -1;
	}
	tccbin_snapshot_from_stat(&information, snapshot);
	return 0;
}

static int tccbin_fstatat_snapshot(int parent_fd, const char *name,
	tccbin_native_stat_snapshot *snapshot) {
#if defined(AT_SYMLINK_NOFOLLOW)
	struct stat information;
	if (fstatat(parent_fd, name, &information, AT_SYMLINK_NOFOLLOW) != 0) {
		return -1;
	}
	tccbin_snapshot_from_stat(&information, snapshot);
	return 0;
#else
	(void) parent_fd;
	(void) name;
	(void) snapshot;
	errno = ENOTSUP;
	return -1;
#endif
}

static int tccbin_open_document_no_follow(const char *path) {
	int flags = O_RDONLY;
#ifdef O_NONBLOCK
	flags |= O_NONBLOCK;
#endif
#ifdef O_NOFOLLOW
	flags |= O_NOFOLLOW;
#endif
#ifdef O_CLOEXEC
	flags |= O_CLOEXEC;
#endif
#ifdef O_NOCTTY
	flags |= O_NOCTTY;
#endif
	int fd = open(path, flags);
#ifndef O_CLOEXEC
	if (fd >= 0) {
		int descriptor_flags = fcntl(fd, F_GETFD);
		if (descriptor_flags < 0 || fcntl(fd, F_SETFD, descriptor_flags | FD_CLOEXEC) != 0) {
			close(fd);
			return -1;
		}
	}
#endif
	return fd;
}

static int tccbin_open_directory_no_follow(const char *path) {
#if defined(O_DIRECTORY) && defined(O_NOFOLLOW)
	int flags = O_RDONLY | O_DIRECTORY | O_NOFOLLOW;
#ifdef O_CLOEXEC
	flags |= O_CLOEXEC;
#endif
	int fd = open(path, flags);
#ifndef O_CLOEXEC
	if (fd >= 0) {
		int descriptor_flags = fcntl(fd, F_GETFD);
		if (descriptor_flags < 0 ||
			fcntl(fd, F_SETFD, descriptor_flags | FD_CLOEXEC) != 0) {
			close(fd);
			return -1;
		}
	}
#endif
	return fd;
#else
	(void) path;
	errno = ENOTSUP;
	return -1;
#endif
}

static int tccbin_openat_no_follow(int parent_fd, const char *name, int directory) {
#if defined(O_DIRECTORY) && defined(O_NOFOLLOW)
	int flags = O_RDONLY | O_NOFOLLOW;
	if (directory) {
		flags |= O_DIRECTORY;
	} else {
#ifdef O_NONBLOCK
		flags |= O_NONBLOCK;
#endif
#ifdef O_NOCTTY
		flags |= O_NOCTTY;
#endif
	}
#ifdef O_CLOEXEC
	flags |= O_CLOEXEC;
#endif
	int fd = openat(parent_fd, name, flags);
#ifndef O_CLOEXEC
	if (fd >= 0) {
		int descriptor_flags = fcntl(fd, F_GETFD);
		if (descriptor_flags < 0 ||
			fcntl(fd, F_SETFD, descriptor_flags | FD_CLOEXEC) != 0) {
			close(fd);
			return -1;
		}
	}
#endif
	return fd;
#else
	(void) parent_fd;
	(void) name;
	(void) directory;
	errno = ENOTSUP;
	return -1;
#endif
}

static void *tccbin_open_directory_enumerator(int directory_fd) {
	int duplicated_fd;
	DIR *directory;
#if defined(O_DIRECTORY) && defined(O_NOFOLLOW)
	{
		int flags = O_RDONLY | O_DIRECTORY | O_NOFOLLOW;
#ifdef O_CLOEXEC
		flags |= O_CLOEXEC;
#endif
		duplicated_fd = openat(directory_fd, ".", flags);
#ifndef O_CLOEXEC
		if (duplicated_fd >= 0) {
			int descriptor_flags = fcntl(duplicated_fd, F_GETFD);
			if (descriptor_flags < 0 ||
				fcntl(duplicated_fd, F_SETFD, descriptor_flags | FD_CLOEXEC) != 0) {
				close(duplicated_fd);
				return NULL;
			}
		}
#endif
	}
#else
	(void) directory_fd;
	errno = ENOTSUP;
	return NULL;
#endif
	if (duplicated_fd < 0) {
		return NULL;
	}
	directory = fdopendir(duplicated_fd);
	if (directory == NULL) {
		close(duplicated_fd);
		return NULL;
	}
	rewinddir(directory);
	return (void *) directory;
}

static int tccbin_read_directory_entry(void *directory_pointer, char *buffer,
	uint64_t capacity) {
	DIR *directory = (DIR *) directory_pointer;
	struct dirent *entry;
	size_t length;
	if (directory == NULL || buffer == NULL || capacity == 0) {
		return -1;
	}
	for (;;) {
		errno = 0;
		entry = readdir(directory);
		if (entry == NULL) {
			return errno == 0 ? 0 : -1;
		}
		if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
			continue;
		}
		length = strlen(entry->d_name);
		if (length + 1 > capacity) {
			return -2;
		}
		memcpy(buffer, entry->d_name, length + 1);
		return (int) length;
	}
}

static int tccbin_close_directory_enumerator(void *directory_pointer) {
	if (directory_pointer == NULL) {
		return -1;
	}
	return closedir((DIR *) directory_pointer);
}

static int64_t tccbin_read_document(int fd, void *buffer, uint64_t length) {
	return (int64_t) read(fd, buffer, (size_t) length);
}

static int tccbin_close_document(int fd) {
	return close(fd);
}

#endif

#endif
