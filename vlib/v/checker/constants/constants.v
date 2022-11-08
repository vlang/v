module constants

// TODO: move all constants from `checker` to here, and replace the hardcoded one with the computed one
pub const valid_comptime_not_user_defined = ['windows', 'ios', 'macos', 'mach', 'darwin', 'hpux',
	'gnu', 'qnx', 'linux', 'freebsd', 'openbsd', 'netbsd', 'bsd', 'dragonfly', 'android', 'termux',
	'solaris', 'haiku', 'serenity', 'vinix', 'gcc', 'tinyc', 'clang', 'mingw', 'msvc', 'cplusplus',
	'amd64', 'i386', 'aarch64', 'arm64', 'arm32', 'rv64', 'rv32', 'x64', 'x32', 'little_endian',
	'big_endian', 'apk', 'js', 'debug', 'prod', 'test', 'glibc', 'prealloc', 'no_bounds_checking',
	'freestanding', 'threads', 'js_node', 'js_browser', 'js_freestanding', 'interpreter', 'es5',
	'profile', 'wasm32_emscripten']
