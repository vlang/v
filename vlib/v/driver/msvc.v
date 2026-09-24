module driver

import os
import v.gen.c as cgen
import v.pref

// MSVC's `cl` does not accept the gcc-style command lines that the rest of the driver
// builds. msvc_cl_args translates them, and msvc_lower_c_file makes the generated C
// itself acceptable to `cl` (see cgen.msvc_compat_c_source).

const msvc_default_libs = ['kernel32.lib', 'user32.lib', 'advapi32.lib', 'ws2_32.lib']

// Libraries that gcc-style toolchains link separately, but that are part of the MSVC
// C runtime or have no Windows counterpart.
const msvc_ignored_libs = ['m', 'pthread', 'dl', 'rt', 'c', 'gcc', 'gcc_s', 'stdc++', 'mingw32',
	'mingwex', 'moldname', 'msvcrt', 'ucrt']

// Options that take the following argument as their operand and have no MSVC meaning.
const msvc_ignored_options_with_operand = ['-arch', '-target', '-isysroot', '--sysroot', '-MT',
	'-MF', '-MQ', '-framework']

const msvc_source_extensions = ['.c', '.cc', '.cpp', '.cxx']

const msvc_linker_input_extensions = ['.o', '.obj', '.lib', '.a', '.res', '.def']

// c_compiler_is_msvc reports whether a C compiler command is MSVC's `cl`, judging by its
// name only (like effective_c_compiler_name, but without running the compiler).
fn c_compiler_is_msvc(compiler string) bool {
	name := os.file_name(compiler).to_lower_ascii()
	return name in ['cl', 'cl.exe'] || name.contains('msvc')
}

// v3_msvc_link_flags returns the linker options for a Windows program linked by MSVC,
// in `-Wl,` form so msvc_cl_args passes them after `/link`.
fn v3_msvc_link_flags(target_os string, is_shared bool, is_o bool, subsystem pref.Subsystem, windows_gui_app bool) []string {
	if target_os != 'windows' || is_shared || is_o {
		return []
	}
	// Match the 32 MiB main thread stack that gcc-style Windows builds reserve.
	mut flags := ['-Wl,/STACK:33554432']
	match subsystem {
		.console {
			flags << '-Wl,/SUBSYSTEM:CONSOLE'
		}
		.windows {
			flags << '-Wl,/SUBSYSTEM:WINDOWS'
		}
		.auto {
			if windows_gui_app {
				flags << '-Wl,/SUBSYSTEM:WINDOWS'
			}
		}
	}
	return flags
}

// msvc_cl_args translates the gcc-style arguments for V's generated C into arguments
// for MSVC's `cl`. Options without an MSVC counterpart (warning selection, code
// generation tuning like `-fwrapv`, or `-std=gnu11`) are dropped.
fn msvc_cl_args(args []string, target_os string) []string {
	return msvc_translate_cl_args(args, target_os, true)
}

// msvc_cl_object_args is msvc_cl_args for third-party C sources, which are compiled in
// `cl`'s own default mode, as V1 did.
fn msvc_cl_object_args(args []string, target_os string) []string {
	return msvc_translate_cl_args(args, target_os, false)
}

fn msvc_translate_cl_args(args []string, target_os string, generated bool) []string {
	// `/bigobj`: the program is one translation unit with tens of thousands of functions.
	mut compile := ['/nologo', '/volatile:ms', '/bigobj', '/MD']
	if generated {
		// V's C must declare everything it calls; its string literals are UTF-8.
		compile << ['/we4013', '/utf-8']
	}
	mut inputs := []string{}
	mut libs := []string{}
	mut link := []string{}
	mut output := ''
	mut compile_only := false
	mut is_shared := false
	mut is_debug := false
	mut has_cpp := false
	mut i := 0
	for i < args.len {
		arg := args[i]
		i++
		if arg.len == 0 {
			continue
		}
		next := if i < args.len { args[i] } else { '' }
		match arg {
			'-o' {
				output = next
				i++
				continue
			}
			'-c' {
				compile_only = true
				continue
			}
			'-shared' {
				is_shared = true
				continue
			}
			'-g', '-g3', '-ggdb' {
				is_debug = true
				continue
			}
			'-w' {
				compile << '/w'
				continue
			}
			'-x' {
				i++
				continue
			}
			'-I', '-isystem', '-iquote', '-idirafter' {
				compile << '/I${next}'
				i++
				continue
			}
			'-D' {
				compile << '/D${next}'
				i++
				continue
			}
			'-U' {
				compile << '/U${next}'
				i++
				continue
			}
			'-include' {
				compile << '/FI${next}'
				i++
				continue
			}
			'-L' {
				link << '/LIBPATH:${next}'
				i++
				continue
			}
			'-l' {
				msvc_add_lib(mut libs, next)
				i++
				continue
			}
			'-Xlinker' {
				msvc_add_linker_option(mut link, next)
				i++
				continue
			}
			'-mwindows' {
				link << '/SUBSYSTEM:WINDOWS'
				continue
			}
			'-mconsole' {
				link << '/SUBSYSTEM:CONSOLE'
				continue
			}
			else {}
		}
		if arg in msvc_ignored_options_with_operand {
			i++
			continue
		}
		if arg.starts_with('@') {
			compile << arg
			continue
		}
		lower := arg.to_lower_ascii()
		if msvc_source_extensions.any(lower.ends_with(it)) && !arg.starts_with('-') {
			if !lower.ends_with('.c') {
				has_cpp = true
			}
			inputs << arg
			continue
		}
		if msvc_linker_input_extensions.any(lower.ends_with(it)) && !arg.starts_with('-') {
			if lower.ends_with('.def') {
				link << '/DEF:${arg}'
			} else {
				inputs << arg
			}
			continue
		}
		if arg.starts_with('-I') {
			compile << '/I${arg[2..]}'
		} else if arg.starts_with('-D') {
			compile << '/D${arg[2..]}'
		} else if arg.starts_with('-U') {
			compile << '/U${arg[2..]}'
		} else if arg.starts_with('-L') {
			link << '/LIBPATH:${arg[2..]}'
		} else if arg.starts_with('-l') {
			msvc_add_lib(mut libs, arg[2..])
		} else if arg.starts_with('-Wl,') {
			for part in arg[4..].split(',') {
				msvc_add_linker_option(mut link, part)
			}
		} else if arg.starts_with('-O') {
			level := arg[2..]
			compile << match level {
				'0' { '/Od' }
				's', 'z', '1' { '/O1' }
				else { '/O2' }
			}
		} else if arg == '-Werror=implicit-function-declaration' {
			// Already enabled by the default `/we4013`.
		} else if arg.starts_with('/') && !os.exists(arg) {
			// An MSVC option given directly, for example with `-cflags`.
			compile << arg
		} else if !arg.starts_with('-') {
			// Another linker input, such as an extensionless library path.
			inputs << arg
		}
		// Any other gcc-style option (`-W...`, `-f...`, `-m...`, `-std=...`, `-pthread`,
		// `-municode`, `-M...`) has no MSVC equivalent that V relies on.
	}
	if generated && !has_cpp {
		// C11 mode also enables MSVC's conforming preprocessor. It defines `__STDC__`,
		// which hides the POSIX names (`popen`, `fileno`, ...) that V calls.
		compile << ['/std:c11', '/D_CRT_DECLARE_NONSTDC_NAMES=1']
	}
	if is_debug {
		compile << '/Zi'
		link << '/DEBUG'
	}
	if compile_only {
		compile << '/c'
		if output.len > 0 {
			compile << '/Fo${output}'
		}
	} else {
		if is_shared {
			compile << '/LD'
		}
		if output.len > 0 {
			compile << '/Fe${output}'
		}
	}
	mut res := compile.clone()
	res << inputs
	if !compile_only {
		if target_os == 'windows' {
			for lib in msvc_default_libs {
				if lib !in libs {
					res << lib
				}
			}
		}
		res << libs
		if link.len > 0 {
			res << '/link'
			res << link
		}
	}
	return res
}

fn msvc_add_lib(mut libs []string, name string) {
	mut lib := name.trim_space()
	if lib.starts_with(':') {
		lib = lib[1..]
	}
	if lib.len == 0 || lib in msvc_ignored_libs {
		return
	}
	if !lib.to_lower_ascii().ends_with('.lib') {
		lib += '.lib'
	}
	if lib !in libs {
		libs << lib
	}
}

// msvc_add_linker_option translates one gcc-style linker option (the operand of
// `-Wl,` or `-Xlinker`) for MSVC's `link`.
fn msvc_add_linker_option(mut link []string, option string) {
	if option.len == 0 {
		return
	}
	if option.starts_with('/') {
		link << option
		return
	}
	if option.starts_with('--stack=') {
		link << '/STACK:${option.all_after('=')}'
	} else if option.starts_with('--subsystem=') || option.starts_with('-subsystem=') {
		link << '/SUBSYSTEM:${option.all_after('=').to_upper_ascii()}'
	} else if option.starts_with('-L') {
		link << '/LIBPATH:${option[2..]}'
	}
	// Other GNU linker options (`-rpath`, `--as-needed`, ...) have no MSVC equivalent.
}

// msvc_lower_c_file rewrites a generated C file in place, so MSVC can compile it.
fn msvc_lower_c_file(path string) ! {
	source := os.read_file(path)!
	os.write_file(path, cgen.msvc_compat_c_source(source))!
}

// msvc_require_cl exits with an explanation when MSVC's compiler cannot be run.
fn msvc_require_cl(c_compiler string, host_os string) {
	os.find_abs_path_of_executable(c_compiler) or {
		if host_os == 'windows' {
			eprintln('`-cc msvc` could not find `${c_compiler}`. Run V from a Visual Studio Developer Command Prompt (or after `vcvars64.bat`), so that `cl` and its INCLUDE/LIB environment are available.')
		} else {
			eprintln('`-cc msvc` can only compile on Windows; use `-o file.c` to generate C for MSVC on ${host_os}.')
		}
		exit(1)
	}
}
