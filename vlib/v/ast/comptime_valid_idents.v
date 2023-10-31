module ast

pub const (
	valid_comptime_if_os             = ['windows', 'ios', 'macos', 'mach', 'darwin', 'hpux', 'gnu',
		'qnx', 'linux', 'freebsd', 'openbsd', 'netbsd', 'bsd', 'dragonfly', 'android', 'termux',
		'solaris', 'haiku', 'serenity', 'vinix', 'plan9']
	valid_comptime_if_compilers      = ['gcc', 'tinyc', 'clang', 'mingw', 'msvc', 'cplusplus']
	valid_comptime_if_platforms      = ['amd64', 'i386', 'aarch64', 'arm64', 'arm32', 'rv64', 'rv32']
	valid_comptime_if_cpu_features   = ['x64', 'x32', 'little_endian', 'big_endian']
	valid_comptime_if_other          = ['apk', 'js', 'debug', 'prod', 'test', 'glibc', 'prealloc',
		'no_bounds_checking', 'freestanding', 'threads', 'js_node', 'js_browser', 'js_freestanding',
		'interpreter', 'es5', 'profile', 'wasm32', 'wasm32_emscripten', 'wasm32_wasi', 'fast_math',
		'native']
	valid_comptime_not_user_defined  = all_valid_comptime_idents()
	valid_comptime_compression_types = ['none', 'zlib']
)

fn all_valid_comptime_idents() []string {
	mut res := []string{}
	res << ast.valid_comptime_if_os
	res << ast.valid_comptime_if_compilers
	res << ast.valid_comptime_if_platforms
	res << ast.valid_comptime_if_cpu_features
	res << ast.valid_comptime_if_other
	return res
}
