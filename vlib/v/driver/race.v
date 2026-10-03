module driver

import os
import v.pref

// `-race` builds a program with the data race detector. Like Go's `-race`, it relies
// on ThreadSanitizer: the C compiler instruments every memory access of the generated
// C, and the TSan runtime tracks the happens-before relation between threads through
// the pthread, semaphore and atomic operations that V's `spawn`, `go`, channels,
// `lock`/`rlock` and `sync` are built on.

// v3_race_c_flags are passed to both the C compiler and the linker of a race build.
// Debug info lets the TSan symbolizer turn stack frames into V file:line positions,
// frame pointers keep its stack unwinding reliable.
const v3_race_c_flags = ['-fsanitize=thread', '-g', '-fno-omit-frame-pointer']

// v3_race_supported_targets lists the targets for which clang/gcc ship a ThreadSanitizer
// runtime. It follows Go's list of `-race` platforms, minus windows/amd64: Go uses its
// own build of the TSan runtime there, while clang and gcc do not provide one.
const v3_race_supported_targets = {
	'linux':   ['amd64', 'arm64', 'ppc64le', 's390x', 'loongarch64', 'riscv64']
	'macos':   ['amd64', 'arm64']
	'freebsd': ['amd64']
	'netbsd':  ['amd64']
}

// v3_race_gc_mode returns the garbage collection mode of a race build. ThreadSanitizer
// only sees heap memory that is allocated and freed through the C allocator. Boehm GC
// reuses the memory of collected objects behind its back, which TSan reports as races
// between the old and the new owner of the memory, and the GC's internal locks add
// happens-before edges between threads that hide real races. Go avoids both by telling
// the race runtime about every allocation and free (racemalloc/racefree); Boehm has no
// such hook, so race builds use the C allocator, like `-gc none`.
fn v3_race_gc_mode(gc_mode string) !string {
	if gc_mode in ['', 'none'] {
		return 'none'
	}
	return error('`-race` cannot be combined with `-gc ${gc_mode}`: the race detector tracks heap memory through the C allocator, so race builds do not use a garbage collector (like `-gc none`)')
}

// v3_race_check_prealloc reports an error for `-race` with the arena allocator (`-prealloc`),
// for the reason of v3_race_gc_mode: the arenas hand out and reuse memory without the C
// allocator, so the race detector would not see where the lifetime of an object begins and
// ends.
fn v3_race_check_prealloc(user_defines []string) ! {
	if user_defines.any(it.all_before('=').trim_space() == 'prealloc') {
		return error('`-race` cannot be combined with `-prealloc`: the race detector tracks heap memory through the C allocator, which the arena allocator bypasses')
	}
}

// v3_race_check_reserved_define reports an error for `-d race` without `-race`. The `race`
// define turns on the race detector hooks of the V runtime, which need the ThreadSanitizer
// runtime that only `-race` links.
fn v3_race_check_reserved_define(race bool, user_defines []string) ! {
	if !race && user_defines.any(it.all_before('=').trim_space() == 'race') {
		return error('`-d race` is reserved for race builds: use `-race` to build with the race detector')
	}
}

// v3_race_check_backend reports an error when `-race` is used with a backend that does not
// emit C for clang/gcc.
fn v3_race_check_backend(backend string) ! {
	if backend != 'c' {
		return error('`-race` is only supported by the C backend')
	}
}

// v3_race_check_target reports an error when there is no ThreadSanitizer runtime for the
// target, or when the output is not a native program that can load one.
fn v3_race_check_target(target pref.Target, output_cross_c bool) ! {
	if output_cross_c {
		return error('`-race` cannot be combined with portable cross output (`-os cross`/`-cross`)')
	}
	archs := v3_race_supported_targets[target.os] or { []string{} }
	if target.arch !in archs {
		return error('`-race` is not supported on ${target.os}/${target.arch}; supported targets: ${v3_race_supported_target_names()}')
	}
}

fn v3_race_supported_target_names() string {
	mut names := []string{}
	for target_os, archs in v3_race_supported_targets {
		for arch in archs {
			names << '${target_os}/${arch}'
		}
	}
	return names.join(', ')
}

// v3_race_default_c_compiler returns the C compiler of a race build without `-cc`: clang,
// when it is installed. gcc's ThreadSanitizer instrumentation misses the reads and writes of
// whole struct values in call arguments and results, which V uses for strings, arrays and
// maps, so races on them would go unreported. On macOS, `cc` is clang.
fn v3_race_default_c_compiler(default_c_compiler string) string {
	$if macos {
		return default_c_compiler
	}
	os.find_abs_path_of_executable('clang') or { return default_c_compiler }
	return 'clang'
}

// v3_race_check_c_compiler reports an error for C compilers that have no ThreadSanitizer
// support. The effective name is the one the generated code is written for (`tinyc`,
// `clang`, `gcc`, ...).
fn v3_race_check_c_compiler(c_compiler string, effective_c_compiler string) ! {
	if effective_c_compiler == 'tinyc' || c_compiler_is_msvc(c_compiler) {
		return error('`-race` needs a C compiler with ThreadSanitizer support (clang or gcc), not `${os.file_name(c_compiler)}`')
	}
}

// v3_race_c_compiler_hint explains a failed race build whose C toolchain lacks the
// ThreadSanitizer runtime, which distributions often package separately from gcc.
fn v3_race_c_compiler_hint(output string) string {
	lower := output.to_lower()
	if lower.contains('tsan') || lower.contains('fsanitize=thread') {
		return 'The race detector needs the ThreadSanitizer runtime of your C compiler. Install it (for example the `libtsan` package for gcc), or select a C compiler that provides it with `-cc clang`.'
	}
	return ''
}

// v3_keep_macos_debug_symbols keeps the DWARF line tables of a macOS debug build next
// to the final binary. On macOS they stay in the object files, and `clang -g` writes a
// `.dSYM` bundle beside its temporary output only when it compiles and links in one step.
// Without them debuggers and the TSan symbolizer (atos) cannot recover file:line positions.
fn v3_keep_macos_debug_symbols(staged_binary string, bin_file string) {
	target_dsym := bin_file + '.dSYM'
	os.rmdir_all(target_dsym) or {}
	staged_dsym := staged_binary + '.dSYM'
	if os.is_dir(staged_dsym) {
		os.mv(staged_dsym, target_dsym) or {}
		return
	}
	dsymutil := os.find_abs_path_of_executable('dsymutil') or { return }
	os.exec(['${dsymutil}', bin_file, '-o', '${target_dsym}'])
}

// v3_remove_macos_debug_symbols removes the debug symbols of a macOS debug binary that
// `v run` deletes after running it.
fn v3_remove_macos_debug_symbols(bin_file string) {
	os.rmdir_all(bin_file + '.dSYM') or {}
}
