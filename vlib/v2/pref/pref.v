// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

import os
import os.cmdline

pub enum Backend {
	v      // V source output (default)
	cleanc // Clean C backend (AST -> C)
	x64    // Native x64/AMD64 backend
	arm64  // Native ARM64 backend
}

pub enum Arch {
	auto // Auto-detect based on OS
	x64
	arm64
}

pub struct Preferences {
pub mut:
	debug                 bool
	verbose               bool
	skip_genv             bool
	skip_builtin          bool
	skip_imports          bool
	skip_type_check       bool // Skip type checking phase (for backends that don't need it yet)
	no_parallel           bool = true // default to sequential parsing until parallel is fixed
	keep_c                bool // Keep generated C file after compilation
	use_context_allocator bool // Use context allocator for heap allocations (enables profiling)
	backend               Backend
	arch                  Arch = .auto
	output_file           string
	printfn_list          []string // List of function names whose generated C source should be printed
pub:
	vroot         string = detect_vroot()
	vmodules_path string = os.vmodules_dir()
}

fn detect_vroot() string {
	// Prefer deriving from executable path: <vroot>/cmd/v2/v3
	if os.args.len > 0 && os.args[0].len > 0 {
		exe_path := os.abs_path(os.args[0])
		dir1 := os.dir(exe_path)
		p1 := os.join_path(dir1, 'vlib', 'builtin')
		if os.is_dir(p1) {
			return dir1
		}
		dir2 := os.dir(dir1)
		if os.is_dir(os.join_path(dir2, 'vlib', 'builtin')) {
			return dir2
		}
		dir3 := os.dir(dir2)
		if os.is_dir(os.join_path(dir3, 'vlib', 'builtin')) {
			return dir3
		}
	}
	// Fallback to cwd if already at repository root.
	cwd := os.getwd()
	if cwd.len > 0 && os.is_dir(os.join_path(cwd, 'vlib', 'builtin')) {
		return cwd
	}
	// Final fallback preserves previous behavior.
	vexe_dir := os.dir(@VEXE)
	return vexe_dir
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
		'cleanc' { backend = .cleanc }
		'v' { backend = .v }
		'arm64' { backend = .arm64 }
		'x64' { backend = .x64 }
		else {}
	}

	mut arch_str := cmdline.option(args, '-arch', 'auto')
	if arch_str.len == 0 {
		arch_str = 'auto'
	}
	mut arch := Arch.auto
	match arch_str {
		'x64' { arch = .x64 }
		'arm64' { arch = .arm64 }
		else {}
	}

	output_file := cmdline.option(args, '-o', cmdline.option(args, '-output', ''))

	// Parse -printfn option (comma-separated list of function names to print)
	mut printfn_str := cmdline.option(args, '-printfn', '')
	if printfn_str.len == 0 {
		printfn_str = ''
	}
	printfn_list := if printfn_str.len > 0 { printfn_str.split(',') } else { []string{} }

	options := cmdline.only_options(args)
	// Default to sequential parsing (no_parallel=true) unless --parallel is specified
	use_parallel := '--parallel' in options
	return Preferences{
		debug:                 '--debug' in options || '-d' in options
		verbose:               '--verbose' in options || '-v' in options
		skip_genv:             '--skip-genv' in options
		skip_builtin:          '--skip-builtin' in options
		skip_imports:          '--skip-imports' in options
		skip_type_check:       '--skip-type-check' in options
		no_parallel:           !use_parallel
		keep_c:                '-keepc' in options
		use_context_allocator: '--profile-alloc' in options || '-profile-alloc' in options
		backend:               backend
		arch:                  arch
		output_file:           output_file
		printfn_list:          printfn_list
		vroot:                 detect_vroot()
		vmodules_path:         os.vmodules_dir()
	}
}

pub fn new_preferences_using_options(options []string) Preferences {
	// Default backend based on OS: macOS defaults to arm64, others to x64
	mut backend := if os.user_os() == 'macos' { Backend.arm64 } else { Backend.x64 }
	if '--cleanc' in options || 'cleanc' in options {
		backend = .cleanc
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

	// Default to sequential parsing (no_parallel=true) unless --parallel is specified
	use_parallel := '--parallel' in options
	return Preferences{
		// config flags
		debug:                 '--debug' in options || '-d' in options
		verbose:               '--verbose' in options || '-v' in options
		skip_genv:             '--skip-genv' in options
		skip_builtin:          '--skip-builtin' in options
		skip_imports:          '--skip-imports' in options
		skip_type_check:       '--skip-type-check' in options
		no_parallel:           !use_parallel
		use_context_allocator: '--profile-alloc' in options || '-profile-alloc' in options
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
