module ast

import v.pref

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

pub fn eval_comptime_not_user_defined_ident(ident string, the_pref &pref.Preferences) !bool {
	mut is_true := false
	if ident in valid_comptime_if_os {
		if ident_enum_val := pref.os_from_string(ident) {
			if ident_enum_val == the_pref.os {
				is_true = true
			}
		}
	} else if ident in valid_comptime_if_compilers {
		is_true = pref.cc_from_string(ident) == the_pref.ccompiler_type
	} else if ident in valid_comptime_if_platforms {
		match ident {
			'amd64' {
				is_true = the_pref.arch == .amd64
			}
			'i386' {
				is_true = the_pref.arch == .i386
			}
			'aarch64' {
				is_true = the_pref.arch == .arm64
			}
			'arm64' {
				is_true = the_pref.arch == .arm64
			}
			'arm32' {
				is_true = the_pref.arch == .arm32
			}
			'rv64' {
				is_true = the_pref.arch == .rv64
			}
			'rv32' {
				is_true = the_pref.arch == .rv32
			}
			's390x' {
				is_true = the_pref.arch == .s390x
			}
			'ppc64le' {
				is_true = the_pref.arch == .ppc64le
			}
			'loongarch64' {
				is_true = the_pref.arch == .loongarch64
			}
			else {
				return error('invalid \$if condition: unknown platforms `${ident}`')
			}
		}
	} else if ident in valid_comptime_if_cpu_features {
		match ident {
			'x64' {
				is_true = the_pref.m64
			}
			'x32' {
				is_true = !the_pref.m64
			}
			'little_endian' {
				is_true = $if little_endian { true } $else { false }
			}
			'big_endian' {
				is_true = $if big_endian { true } $else { false }
			}
			else {
				return error('invalid \$if condition: unknown cpu_features `${ident}`')
			}
		}
	} else if ident in valid_comptime_if_other {
		match ident {
			'apk' {
				is_true = the_pref.is_apk
			}
			'js' {
				is_true = the_pref.backend.is_js()
			}
			'debug' {
				is_true = the_pref.is_debug
			}
			'prod' {
				is_true = the_pref.is_prod
			}
			'test' {
				is_true = the_pref.is_test
			}
			'glibc' {
				is_true = the_pref.is_glibc
			}
			'prealloc' {
				is_true = the_pref.prealloc
			}
			'no_bounds_checking' {
				is_true = the_pref.no_bounds_checking
			}
			'freestanding' {
				is_true = the_pref.is_bare && !the_pref.output_cross_c
			}
			'threads' {
				return error('threads should handle outside of `check_valid_ident()`')
			}
			'js_node' {
				is_true = the_pref.backend == .js_node
			}
			'js_browser' {
				is_true = the_pref.backend == .js_browser
			}
			'js_freestanding' {
				is_true = the_pref.backend == .js_freestanding
			}
			'interpreter' {
				is_true = the_pref.backend == .interpret
			}
			'es5' {
				is_true = the_pref.output_es5
			}
			'profile' {
				is_true = the_pref.is_prof
			}
			'wasm32' {
				is_true = the_pref.arch == .wasm32
			}
			'wasm32_wasi' {
				is_true = the_pref.os == .wasm32_wasi
			}
			'fast_math' {
				is_true = the_pref.fast_math
			}
			'native' {
				is_true = the_pref.backend == .native
			}
			'autofree' {
				is_true = the_pref.autofree
			}
			else {
				return error('invalid \$if condition: unknown other indent `${ident}`')
			}
		}
	} else if ident in the_pref.compile_defines {
		is_true = true
	} else {
		return error('invalid \$if condition: unknown indent `${ident}`')
	}
	return is_true
}

pub const system_ident_map = {
	// OS
	'windows':            '_WIN32'
	'ios':                '__TARGET_IOS__'
	'macos':              '__APPLE__'
	'mach':               '__MACH__'
	'darwin':             '__DARWIN__'
	'hpux':               '__HPUX__'
	'gnu':                '__GNU__'
	'qnx':                '__QNX__'
	'linux':              '__linux__'
	'serenity':           '__serenity__'
	'plan9':              '__plan9__'
	'vinix':              '__vinix__'
	'freebsd':            '__FreeBSD__'
	'openbsd':            '__OpenBSD__'
	'netbsd':             '__NetBSD__'
	'bsd':                '__BSD__'
	'dragonfly':          '__DragonFly__'
	'android':            '__ANDROID__'
	'termux':             '__TERMUX__'
	'solaris':            '__sun'
	'haiku':              '__HAIKU__'
	// Backend
	'js':                 '_VJS'
	'wasm32_emscripten':  '__EMSCRIPTEN__'
	'native':             '_VNATIVE'
	// Compiler
	'gcc':                '__V_GCC__'
	'tinyc':              '__TINYC__'
	'clang':              '__clang__'
	'mingw':              '__MINGW32__'
	'msvc':               '_MSC_VER'
	'cplusplus':          '__cplusplus'
	// Others
	'threads':            '__VTHREADS__'
	'gcboehm':            '_VGCBOEHM'
	'debug':              '_VDEBUG'
	'prod':               '_VPROD'
	'profile':            '_VPROFILE'
	'test':               '_VTEST'
	'glibc':              '__GLIBC__'
	'prealloc':           '_VPREALLOC'
	'no_bounds_checking': 'CUSTOM_DEFINE_no_bounds_checking'
	'freestanding':       '_VFREESTANDING'
	'autofree':           '_VAUTOFREE'
	// CPU
	'amd64':              '__V_amd64'
	'aarch64':            '__V_arm64'
	'arm64':              '__V_arm64' // aarch64 alias
	'arm32':              '__V_arm32'
	'i386':               '__V_x86'
	'rv64':               '__V_rv64'
	'riscv64':            '__V_rv64' // rv64 alias
	'rv32':               '__V_rv32'
	'riscv32':            '__V_rv32' // rv32 alias
	's390x':              '__V_s390x'
	'ppc64le':            '__V_ppc64le'
	'loongarch64':        '__V_loongarch64'
	'x64':                'TARGET_IS_64BIT'
	'x32':                'TARGET_IS_32BIT'
	'little_endian':      'TARGET_ORDER_IS_LITTLE'
	'big_endian':         'TARGET_ORDER_IS_BIG'
}

pub fn comptime_if_to_ifdef(name string, the_pref &pref.Preferences) !string {
	if name == 'fast_math' {
		return if the_pref.ccompiler_type == .msvc {
			// turned on by: `-cflags /fp:fast`
			'_M_FP_FAST'
		} else {
			// turned on by: `-cflags -ffast-math`
			'__FAST_MATH__'
		}
	}
	if ifdef := system_ident_map[name] {
		return ifdef
	}
	return error('bad os ifdef name `${name}`')
}
