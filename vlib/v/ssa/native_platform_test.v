module ssa

fn test_native_linux_constants_do_not_inherit_different_darwin_values() {
	for name, value in {
		'PTHREAD_CREATE_JOINABLE': i64(0)
		'PTHREAD_CREATE_DETACHED': i64(1)
		'PTHREAD_PROCESS_PRIVATE': i64(0)
		'PTHREAD_PROCESS_SHARED':  i64(1)
		'PATH_MAX':                i64(4096)
		'O_SYNC':                  i64(1052672)
		'O_FSYNC':                 i64(1052672)
	} {
		assert native_linux_constant(name)? == value, name
	}
	assert native_darwin_constant('PTHREAD_CREATE_JOINABLE')? == 1
	assert native_darwin_constant('PTHREAD_CREATE_DETACHED')? == 2
	assert native_darwin_constant('PTHREAD_PROCESS_PRIVATE')? == 2
	assert native_darwin_constant('PATH_MAX')? == 1024
	assert native_darwin_constant('O_FSYNC')? == 128
}

// The values are those of the kernel's headers: include/uapi/asm-generic/fcntl.h for amd64, and
// arch/arm64/include/uapi/asm/fcntl.h, which gives O_DIRECTORY and O_NOFOLLOW other bits.
fn test_native_linux_open_flags_follow_the_target_architecture() {
	amd64 := TargetData{
		os:   'linux'
		arch: 'amd64'
	}
	arm64 := TargetData{
		os:   'linux'
		arch: 'arm64'
	}
	assert native_platform_constant('O_DIRECTORY', amd64)? == 0x10000
	assert native_platform_constant('O_NOFOLLOW', amd64)? == 0x20000
	assert native_platform_constant('O_DIRECTORY', arm64)? == 0x4000
	assert native_platform_constant('O_NOFOLLOW', arm64)? == 0x8000
	// On arm64 the amd64 values are O_DIRECT and O_LARGEFILE.
	assert native_platform_constant('O_DIRECTORY', arm64)? | native_platform_constant('O_NOFOLLOW',
		arm64)? | native_platform_constant('O_CLOEXEC', arm64)? == 0x8c000
	for name in ['O_RDONLY', 'O_WRONLY', 'O_RDWR', 'O_CREAT', 'O_EXCL', 'O_NOCTTY', 'O_TRUNC',
		'O_APPEND', 'O_NONBLOCK', 'O_NDELAY', 'O_DSYNC', 'O_ASYNC', 'O_CLOEXEC', 'O_SYNC', 'O_FSYNC'] {
		assert native_platform_constant(name, arm64)? == native_platform_constant(name,
			amd64)?, name
	}
	// A macOS target is not affected by its architecture.
	assert native_platform_constant('O_DIRECTORY', TargetData{ os: 'macos', arch: 'arm64' })? == 0x100000
	assert native_platform_constant('O_NOFOLLOW', TargetData{ os: 'macos', arch: 'arm64' })? == 0x100
}

fn test_native_fcntl_and_signal_constants_are_resolved() {
	for name, value in {
		'F_SETFD':    i64(2)
		'FD_CLOEXEC': i64(1)
		'SIGKILL':    i64(9)
		'SIGTERM':    i64(15)
		'NULL':       i64(0)
	} {
		assert native_linux_constant(name)? == value, name
		assert native_darwin_constant(name)? == value, name
	}
}

fn test_native_sync_constants_match_the_compiler_runtime() {
	for name, value in {
		'memory_order_relaxed': i64(0)
		'memory_order_acquire': i64(2)
		'memory_order_release': i64(3)
		'memory_order_acq_rel': i64(4)
		'memory_order_seq_cst': i64(5)
	} {
		assert native_linux_constant(name)? == value, name
		assert native_darwin_constant(name)? == value, name
	}
	assert native_darwin_constant('PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP')? == 0
	assert native_linux_constant('PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP')? == 2
	assert native_darwin_constant('UNRECOGNIZED_SYNC_CONSTANT') == none
	assert native_linux_constant('UNRECOGNIZED_SYNC_CONSTANT') == none
}
