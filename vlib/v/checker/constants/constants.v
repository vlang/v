module constants

pub const (
	valid_comptime_if_os             = ['windows', 'ios', 'macos', 'mach', 'darwin', 'hpux', 'gnu',
		'qnx', 'linux', 'freebsd', 'openbsd', 'netbsd', 'bsd', 'dragonfly', 'android', 'termux',
		'solaris', 'haiku', 'serenity', 'vinix']
	valid_comptime_compression_types = ['none', 'zlib']
	valid_comptime_if_compilers      = ['gcc', 'tinyc', 'clang', 'mingw', 'msvc', 'cplusplus']
	valid_comptime_if_platforms      = ['amd64', 'i386', 'aarch64', 'arm64', 'arm32', 'rv64', 'rv32']
	valid_comptime_if_cpu_features   = ['x64', 'x32', 'little_endian', 'big_endian']
	valid_comptime_if_other          = ['apk', 'js', 'debug', 'prod', 'test', 'glibc', 'prealloc',
		'no_bounds_checking', 'freestanding', 'threads', 'js_node', 'js_browser', 'js_freestanding',
		'interpreter', 'es5', 'profile', 'wasm32_emscripten']
	valid_comptime_not_user_defined  = all_valid_comptime_idents()
)

fn all_valid_comptime_idents() []string {
	mut res := []string{}
	res << constants.valid_comptime_if_os
	res << constants.valid_comptime_if_compilers
	res << constants.valid_comptime_if_platforms
	res << constants.valid_comptime_if_cpu_features
	res << constants.valid_comptime_if_other
	return res
}
