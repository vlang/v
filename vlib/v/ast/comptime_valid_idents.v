module ast

pub const valid_comptime_if_os = ['windows', 'ios', 'macos', 'mach', 'darwin', 'hpux', 'gnu', 'qnx',
	'linux', 'freebsd', 'openbsd', 'netbsd', 'bsd', 'dragonfly', 'android', 'termux', 'solaris',
	'haiku', 'serenity', 'vinix', 'plan9', 'wasm32_emscripten']
pub const valid_comptime_if_compilers = ['gcc', 'tinyc', 'clang', 'mingw', 'msvc', 'cplusplus']
pub const valid_comptime_if_platforms = ['amd64', 'i386', 'aarch64', 'arm64', 'arm32', 'rv64',
	'rv32', 's390x', 'ppc64le', 'loongarch64']
pub const valid_comptime_if_cpu_features = ['x64', 'x32', 'little_endian', 'big_endian']
pub const valid_comptime_if_other = ['apk', 'js', 'debug', 'prod', 'test', 'glibc', 'prealloc',
	'no_bounds_checking', 'freestanding', 'threads', 'js_node', 'js_browser', 'js_freestanding',
	'interpreter', 'es5', 'profile', 'wasm32', 'wasm32_wasi', 'fast_math', 'native', 'autofree']
pub const valid_comptime_not_user_defined = all_valid_comptime_idents()
pub const valid_comptime_compression_types = ['none', 'zlib']

fn all_valid_comptime_idents() []string {
	mut res := []string{}
	res << valid_comptime_if_os
	res << valid_comptime_if_compilers
	res << valid_comptime_if_platforms
	res << valid_comptime_if_cpu_features
	res << valid_comptime_if_other
	return res
}
