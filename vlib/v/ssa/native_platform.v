module ssa

// native_darwin_constant resolves macros used by the native compiler's OS runtime.
// These values come from the Darwin system ABI, independently of the bootstrap OS.
fn native_darwin_constant(name string) ?i64 {
	return match name {
		'O_RDONLY', 'F_OK', 'PROT_NONE', 'SEEK_SET', 'CLOCK_REALTIME' { i64(0) }
		'O_WRONLY', 'X_OK', 'PROT_READ', 'SEEK_CUR', 'RTLD_LAZY', 'EPERM',
		'PTHREAD_CREATE_JOINABLE', 'POLLIN' {
			i64(1)
		}
		'O_RDWR', 'W_OK', 'PROT_WRITE', 'MAP_PRIVATE', 'SEEK_END', 'RTLD_NOW',
		'ENOENT', 'PTHREAD_CREATE_DETACHED', 'PTHREAD_PROCESS_PRIVATE' {
			i64(2)
		}
		'O_ACCMODE', 'ESRCH' { i64(3) }
		'O_NONBLOCK', 'O_NDELAY', 'R_OK', 'PROT_EXEC', 'RTLD_LOCAL', 'EINTR' { i64(4) }
		'CLOCK_MONOTONIC' { i64(6) }
		'O_APPEND', 'RTLD_GLOBAL' { i64(8) }
		'ECHILD' { i64(10) }
		'ENOMEM' { i64(12) }
		'EACCES' { i64(13) }
		'O_SHLOCK' { i64(16) }
		'EEXIST' { i64(17) }
		'ENOTDIR' { i64(20) }
		'EINVAL' { i64(22) }
		'_SC_PAGESIZE', '_SC_PAGE_SIZE' { i64(29) }
		'O_EXLOCK', 'EPIPE', 'POLLNVAL' { i64(32) }
		'ERANGE' { i64(34) }
		'EAGAIN' { i64(35) }
		'_SC_NPROCESSORS_CONF' { i64(57) }
		'_SC_NPROCESSORS_ONLN' { i64(58) }
		'ETIMEDOUT' { i64(60) }
		'O_ASYNC' { i64(64) }
		'O_SYNC', 'O_FSYNC', 'S_IWUSR' { i64(128) }
		'_SC_PHYS_PAGES' { i64(200) }
		'O_NOFOLLOW', 'S_IRUSR' { i64(256) }
		'O_CREAT' { i64(512) }
		'O_TRUNC', 'PATH_MAX' { i64(1024) }
		'O_EXCL' { i64(2048) }
		'MAP_ANON', 'MAP_ANONYMOUS', 'S_IFIFO' { i64(4096) }
		'S_IFCHR' { i64(8192) }
		'S_IFDIR' { i64(16384) }
		'S_IFBLK' { i64(24576) }
		'S_IFREG', 'O_EVTONLY' { i64(32768) }
		'S_IFLNK' { i64(40960) }
		'S_IFSOCK' { i64(49152) }
		'S_IFMT' { i64(61440) }
		'O_NOCTTY' { i64(131072) }
		'O_DIRECTORY' { i64(1048576) }
		'O_SYMLINK' { i64(2097152) }
		'O_DSYNC' { i64(4194304) }
		'O_CLOEXEC' { i64(16777216) }
		'EOF', 'MAP_FAILED' { i64(-1) }
		else { return none }
	}
}

// native_errno_addr accesses Darwin's thread-local errno through libc.
fn (mut b Builder) native_errno_addr() ValueID {
	ptr_i32 := b.m.type_store.get_ptr(b.i32_type)
	if '__error' !in b.fn_ids {
		b.register_extern('__error', ptr_i32, [])
	}
	return b.emit_runtime_call('__error', ptr_i32, [])
}
