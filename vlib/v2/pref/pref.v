// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

import os
import os.cmdline

pub enum Backend {
	v      // V source output (default)
	eval   // AST interpreter
	cleanc // Clean C backend (AST -> C)
	c      // SSA -> C backend
	x64    // Native x64/AMD64 backend
	arm64  // Native ARM64 backend
}

pub enum Arch {
	auto // Auto-detect based on OS
	x64
	arm64
}

// GarbageCollectionMode controls which garbage collector is used.
// Translated from Go's runtime GC, the `vgc` mode provides a concurrent
// tri-color mark-and-sweep collector written in pure V.
pub enum GarbageCollectionMode {
	no_gc // no garbage collection
	vgc   // V GC: concurrent tri-color mark-and-sweep (translated from Go's runtime GC)
	boehm // Boehm-Demers-Weiser conservative GC (legacy)
}

pub struct Preferences {
pub mut:
	debug                 bool
	verbose               bool
	ownership             bool // -ownership: enable ownership checking for strings
	skip_genv             bool
	skip_builtin          bool
	skip_imports          bool
	skip_type_check       bool                  // Skip type checking phase (for backends that don't need it yet)
	no_parallel           bool                  // when true, run type check sequentially (default: parallel)
	no_parallel_transform bool                  // when true, run transform sequentially (default: parallel)
	no_cache              bool                  // Disable build cache
	no_markused           bool                  // Disable markused stage and dead-function pruning
	show_cc               bool                  // Print C compiler command(s)
	stats                 bool                  // Print extended statistics
	print_parsed_files    bool                  // Print all parsed files grouped by full/.vh parse mode
	keep_c                bool                  // Keep generated C file after compilation
	use_context_allocator bool                  // Use context allocator for heap allocations (enables profiling)
	is_shared_lib         bool                  // Compile to shared library (.dylib/.so) for live reload
	no_optimize           bool                  // -O0: skip SSA optimization (mem2reg, phi elimination)
	is_prod               bool                  // -prod: use -O3 optimization for C compiler
	prealloc              bool                  // -prealloc: use arena allocation (bump-pointer, not thread-safe)
	gc_mode               GarbageCollectionMode // Garbage collection mode (-gc flag)
	backend               Backend
	arch                  Arch = .auto
	output_file           string
	printfn_list          []string // List of function names whose generated C source should be printed
	user_defines          []string // User-defined comptime flags via -d <name>
	hot_fn                string   // Extract raw machine code for this function only (hot reload)
	single_backend        bool     // Only include the selected backend (strip other backends from binary)
	eval_runtime_args     []string // Program argv exposed to the eval backend
	ccompiler             string   // C compiler override (-cc flag)
pub:
	vroot         string = detect_vroot()
	vmodules_path string = os.vmodules_dir()
}

fn dir_exists(path string) bool {
	if path.len == 0 {
		return false
	}
	if os.is_dir(path) {
		return true
	}
	_ := os.ls(path) or { return false }
	return true
}

fn detect_vroot_from(start string) string {
	if start.len == 0 {
		return ''
	}
	mut dir := start
	// Avoid os.abs_path during bootstrap; normalize relative paths manually.
	if !os.is_abs_path(dir) {
		cwd := os.getwd()
		if cwd.len > 0 {
			dir = os.join_path(cwd, dir)
		}
	}
	if dir_exists(os.join_path(dir, 'vlib', 'builtin')) {
		return dir
	}
	dir = os.dir(dir)
	if dir_exists(os.join_path(dir, 'vlib', 'builtin')) {
		return dir
	}
	dir = os.dir(dir)
	if dir_exists(os.join_path(dir, 'vlib', 'builtin')) {
		return dir
	}
	dir = os.dir(dir)
	if dir_exists(os.join_path(dir, 'vlib', 'builtin')) {
		return dir
	}
	dir = os.dir(dir)
	if dir_exists(os.join_path(dir, 'vlib', 'builtin')) {
		return dir
	}
	dir = os.dir(dir)
	if dir_exists(os.join_path(dir, 'vlib', 'builtin')) {
		return dir
	}
	dir = os.dir(dir)
	if dir_exists(os.join_path(dir, 'vlib', 'builtin')) {
		return dir
	}
	dir = os.dir(dir)
	if dir_exists(os.join_path(dir, 'vlib', 'builtin')) {
		return dir
	}
	dir = os.dir(dir)
	if dir_exists(os.join_path(dir, 'vlib', 'builtin')) {
		return dir
	}
	return ''
}

fn detect_vroot() string {
	// First prefer the compile-time module root when it points to a valid
	// V source tree. This keeps self-host binaries stable during bootstrapping.
	compile_time_root := @VMODROOT
	if compile_time_root.len > 0 {
		// Trust @VMODROOT without runtime validation. During bootstrapping
		// (v2 → v3 via ARM64), os.join_path/os.is_dir may not work correctly
		// in the native binary, but the compile-time root is always correct.
		return compile_time_root
	}
	// Prefer deriving from executable path: <vroot>/cmd/v2/v3.
	if os.args.len > 0 && os.args[0].len > 0 {
		vroot := detect_vroot_from(os.args[0])
		if vroot.len > 0 {
			return vroot
		}
	}
	// Fallback to current directory ancestry.
	cwd := os.getwd()
	vroot := detect_vroot_from(cwd)
	if vroot.len > 0 {
		return vroot
	}
	// Final fallback: keep old behavior for unusual bootstrap contexts.
	if os.args.len > 0 && os.args[0].len > 0 {
		if os.is_abs_path(os.args[0]) {
			return os.dir(os.args[0])
		}
		return cwd
	}
	return cwd
}

pub fn new_preferences() Preferences {
	return Preferences{
		backend: .cleanc
	}
}

// new_preferences_from_args parses full args list including option values
pub fn new_preferences_from_args(args []string) Preferences {
	// Default backend is cleanc
	default_backend := 'cleanc'
	mut backend_str_long := cmdline.option(args, '-backend', '')
	if backend_str_long.len == 0 {
		backend_str_long = ''
	}
	mut backend_str_short := cmdline.option(args, '-b', '')
	if backend_str_short.len == 0 {
		backend_str_short = ''
	}
	backend_str := if backend_str_long.len > 0 {
		backend_str_long
	} else if backend_str_short.len > 0 {
		backend_str_short
	} else {
		default_backend
	}
	mut backend := Backend.cleanc
	match backend_str {
		'eval' {
			backend = .eval
		}
		'cleanc' {
			backend = .cleanc
		}
		'c' {
			backend = .c
		}
		'v' {
			backend = .v
		}
		'arm64' {
			backend = .arm64
		}
		'x64' {
			backend = .x64
		}
		else {
			eprintln('error: unknown backend `${backend_str}`. Valid backends: eval, cleanc, c, v, arm64, x64')
			exit(1)
		}
	}

	mut arch_str := cmdline.option(args, '-arch', 'auto')
	if arch_str.len == 0 {
		arch_str = 'auto'
	}
	mut arch := Arch.auto
	match arch_str {
		'auto' {
			arch = .auto
		}
		'x64' {
			arch = .x64
		}
		'arm64' {
			arch = .arm64
		}
		else {
			eprintln('error: unknown architecture `${arch_str}`. Valid architectures: auto, x64, arm64')
			exit(1)
		}
	}

	output_file := cmdline.option(args, '-o', cmdline.option(args, '-output', ''))
	ccompiler := cmdline.option(args, '-cc', '')

	// Parse -printfn option (comma-separated list of function names to print)
	mut printfn_str := cmdline.option(args, '-printfn', '')
	if printfn_str.len == 0 {
		printfn_str = ''
	}
	printfn_list := if printfn_str.len > 0 { printfn_str.split(',') } else { []string{} }

	// Parse -d <name> comptime defines (can be specified multiple times)
	user_defines := cmdline.options(args, '-d')

	// Parse -hot-fn <name> for hot code reloading
	mut hot_fn_str := cmdline.option(args, '-hot-fn', '')
	if hot_fn_str.len == 0 {
		hot_fn_str = ''
	}

	// Parse -gc <mode> for garbage collection
	gc_mode_str := cmdline.option(args, '-gc', '')
	mut gc_mode := GarbageCollectionMode.no_gc
	mut gc_defines := []string{}
	match gc_mode_str {
		'', 'none' {
			gc_mode = .no_gc
		}
		'vgc' {
			gc_mode = .vgc
			gc_defines << 'vgc'
		}
		'boehm' {
			gc_mode = .boehm
			gc_defines << 'gcboehm'
			gc_defines << 'gcboehm_full'
			gc_defines << 'gcboehm_opt'
		}
		else {
			eprintln('error: unknown garbage collection mode `-gc ${gc_mode_str}`')
			eprintln('  `-gc vgc` .............. V GC: concurrent tri-color mark-and-sweep (translated from Go)')
			eprintln('  `-gc boehm` ............ Boehm-Demers-Weiser conservative GC')
			eprintln('  `-gc none` ............. no garbage collection (default)')
			exit(1)
		}
	}

	mut all_defines := user_defines.clone()
	all_defines << gc_defines

	options := cmdline.only_options(args)

	// Parse -ownership flag (must be after options is declared)
	mut ownership := false
	$if ownership ? {
		ownership = '-ownership' in options
		if ownership {
			all_defines << 'ownership'
		}
	}

	// Validate flags: error on unknown options
	known_flags_with_values := ['-backend', '-b', '-o', '-output', '-arch', '-printfn', '-gc',
		'-d', '-hot-fn', '-cc']
	mut known_boolean_flags := ['--debug', '--verbose', '-v', '--skip-genv', '--skip-builtin',
		'--skip-imports', '--skip-type-check', '--no-parallel', '-nocache', '--nocache',
		'-nomarkused', '--nomarkused', '-showcc', '--showcc', '-stats', '--stats',
		'-print-parsed-files', '--print-parsed-files', '-keepc', '--profile-alloc', '-profile-alloc',
		'-enable-globals', '--enable-globals', '-shared', '--shared', '-O0', '--single-backend',
		'-single-backend', '-prod', '-prealloc']
	$if ownership ? {
		known_boolean_flags << '-ownership'
	}
	for opt in options {
		if opt !in known_flags_with_values && opt !in known_boolean_flags {
			eprintln('error: unknown flag `${opt}`')
			eprintln('')
			eprintln('Usage: v2 [options] <file.v>')
			eprintln('')
			eprintln('Options:')
			eprintln('  -o <file>              Output file name')
			eprintln('  -backend <name>        Backend: eval, cleanc, c, v, arm64, x64 (default: cleanc)')
			eprintln('  -b <name>              Short for -backend')
			eprintln('  -arch <name>           Architecture: auto, x64, arm64 (default: auto)')
			eprintln('  -printfn <names>       Print generated C for functions (comma-separated)')
			eprintln('  -stats, --stats        Print compilation statistics')
			eprintln('  -nocache, --nocache    Disable build cache')
			eprintln('  -d <name>              Define a comptime flag')
			eprintln('  -enable-globals        Accepted for v1 compatibility')
			eprintln('  -prod                  Production build: optimize with -O3 -flto')
			eprintln('  -prealloc              Use arena allocation (faster, not thread-safe)')
			eprintln('  -O0                    Skip SSA optimization (faster compile, slower code)')
			$if ownership ? {
				eprintln('  -ownership             Enable ownership checking for strings')
			}
			eprintln('  --debug                Enable debug mode')
			eprintln('  -v, --verbose          Enable verbose output')
			eprintln('  -cc <compiler>         C compiler to use (default: tcc, fallback: cc)')
			eprintln('  -showcc, --showcc      Print C compiler command')
			eprintln('  -keepc                 Keep generated C file')
			eprintln('  --no-parallel          Disable parallel type check and transform')
			exit(1)
		}
	}

	return Preferences{
		debug:                 '--debug' in options
		verbose:               '--verbose' in options || '-v' in options
		skip_genv:             '--skip-genv' in options
		skip_builtin:          '--skip-builtin' in options
		skip_imports:          '--skip-imports' in options
		skip_type_check:       '--skip-type-check' in options
		no_parallel:           '--no-parallel' in options
		no_parallel_transform: '--no-parallel' in options
		no_cache:              '-nocache' in options || '--nocache' in options
		no_markused:           '-nomarkused' in options || '--nomarkused' in options
		show_cc:               '-showcc' in options || '--showcc' in options
		stats:                 '-stats' in options || '--stats' in options
		print_parsed_files:    '-print-parsed-files' in options || '--print-parsed-files' in options
		keep_c:                '-keepc' in options
		use_context_allocator: '--profile-alloc' in options || '-profile-alloc' in options
		is_shared_lib:         '-shared' in options || '--shared' in options
		no_optimize:           '-O0' in options
		is_prod:               '-prod' in options
		prealloc:              '-prealloc' in options
		single_backend:        '--single-backend' in options || '-single-backend' in options
		ownership:             ownership
		gc_mode:               gc_mode
		backend:               backend
		arch:                  arch
		output_file:           output_file
		printfn_list:          printfn_list
		user_defines:          all_defines
		hot_fn:                hot_fn_str
		ccompiler:             ccompiler
		vroot:                 detect_vroot()
		vmodules_path:         os.vmodules_dir()
	}
}

pub fn new_preferences_using_options(options []string) Preferences {
	// Default backend based on OS: macOS defaults to arm64, others to x64
	mut backend := if os.user_os() == 'macos' { Backend.arm64 } else { Backend.x64 }
	if '--eval' in options || 'eval' in options {
		backend = .eval
	} else if '--cleanc' in options || 'cleanc' in options {
		backend = .cleanc
	} else if '--c' in options || 'c' in options {
		backend = .c
	} else if '--v' in options || 'v' in options {
		backend = .v
	} else if '--arm64' in options || 'arm64' in options {
		backend = .arm64
	} else if '--x64' in options || 'x64' in options {
		backend = .x64
	}

	mut arch := Arch.auto
	if '--arch-x64' in options {
		arch = .x64
	} else if '--arch-arm64' in options {
		arch = .arm64
	}

	return Preferences{
		// config flags
		debug:                 '--debug' in options
		verbose:               '--verbose' in options || '-v' in options
		skip_genv:             '--skip-genv' in options
		skip_builtin:          '--skip-builtin' in options
		skip_imports:          '--skip-imports' in options
		skip_type_check:       '--skip-type-check' in options
		no_parallel:           '--no-parallel' in options
		no_parallel_transform: '--no-parallel' in options
		no_cache:              '-nocache' in options || '--nocache' in options
		no_markused:           '-nomarkused' in options || '--nomarkused' in options
		show_cc:               '-showcc' in options || '--showcc' in options
		stats:                 '-stats' in options || '--stats' in options
		print_parsed_files:    '-print-parsed-files' in options || '--print-parsed-files' in options
		use_context_allocator: '--profile-alloc' in options || '-profile-alloc' in options
		is_shared_lib:         '-shared' in options || '--shared' in options
		no_optimize:           '-O0' in options
		is_prod:               '-prod' in options
		prealloc:              '-prealloc' in options
		single_backend:        '--single-backend' in options || '-single-backend' in options
		backend:               backend
		arch:                  arch
	}
}

// get_effective_arch returns the architecture to use based on preferences and OS
pub fn (p &Preferences) get_effective_arch() Arch {
	if p.arch != .auto {
		return p.arch
	}
	// Auto-detect: macOS defaults to arm64, others to x64
	return if os.user_os() == 'macos' { Arch.arm64 } else { Arch.x64 }
}
