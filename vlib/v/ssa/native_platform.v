module ssa

// native_platform_constant returns the native runtime constants for the host
// ABI used by direct code generation.
fn native_platform_constant(name string) ?i64 {
	$if linux {
		return native_linux_constant(name)
	} $else {
		return native_darwin_constant(name)
	}
}

fn native_linux_constant(name string) ?i64 {
	match name {
		'CLOCK_MONOTONIC' { return i64(1) }
		'EAGAIN' { return i64(11) }
		'ETIMEDOUT' { return i64(110) }
		'O_NONBLOCK', 'O_NDELAY' { return i64(2048) }
		'O_ASYNC' { return i64(8192) }
		'O_CREAT' { return i64(64) }
		'O_EXCL' { return i64(128) }
		'O_NOCTTY' { return i64(256) }
		'O_TRUNC' { return i64(512) }
		'O_APPEND' { return i64(1024) }
		'O_DSYNC' { return i64(4096) }
		'O_DIRECTORY' { return i64(65536) }
		'O_NOFOLLOW' { return i64(131072) }
		'O_CLOEXEC' { return i64(524288) }
		'O_SYNC' { return i64(1052672) }
		'MAP_ANON', 'MAP_ANONYMOUS' { return i64(32) }
		'RTLD_LOCAL' { return i64(0) }
		'RTLD_GLOBAL' { return i64(256) }
		'RAND_MAX' { return i64(2147483647) }
		'FIONREAD' { return i64(21531) }
		'_SC_PAGESIZE', '_SC_PAGE_SIZE' { return i64(30) }
		'_SC_NPROCESSORS_CONF' { return i64(83) }
		'_SC_NPROCESSORS_ONLN' { return i64(84) }
		'_SC_PHYS_PAGES' { return i64(85) }
		else { return native_darwin_constant(name) }
	}
}

// native_darwin_constant resolves macros used by the native compiler's OS runtime.
// These values come from the Darwin system ABI, independently of the bootstrap OS.
fn native_darwin_constant(name string) ?i64 {
	return match name {
		'O_RDONLY', 'F_OK', 'PROT_NONE', 'SEEK_SET', 'CLOCK_REALTIME', 'STDIN_FILENO' {
			i64(0)
		}
		'O_WRONLY', 'X_OK', 'PROT_READ', 'SEEK_CUR', 'RTLD_LAZY', 'EPERM',
		'PTHREAD_CREATE_JOINABLE', 'POLLIN', 'STDOUT_FILENO' {
			i64(1)
		}
		'O_RDWR', 'W_OK', 'PROT_WRITE', 'MAP_PRIVATE', 'SEEK_END', 'RTLD_NOW',
		'ENOENT', 'PTHREAD_CREATE_DETACHED', 'PTHREAD_PROCESS_PRIVATE', 'STDERR_FILENO' {
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
		'RAND_MAX' { i64(2147483647) }
		'EOF', 'MAP_FAILED' { i64(-1) }
		else { return none }
	}
}

// native_errno_addr accesses the host libc's thread-local errno.
fn (mut b Builder) native_errno_addr() ValueID {
	ptr_i32 := b.m.type_store.get_ptr(b.i32_type)
	symbol := $if linux { '__errno_location' } $else { '__error' }
	if symbol !in b.fn_ids {
		b.register_extern(symbol, ptr_i32, [])
	}
	return b.emit_runtime_call(symbol, ptr_i32, [])
}
