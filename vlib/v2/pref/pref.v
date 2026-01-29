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
	debug           bool
	verbose         bool
	skip_genv       bool
	skip_builtin    bool
	skip_imports    bool
	skip_type_check bool // Skip type checking phase (for backends that don't need it yet)
	no_parallel     bool = true // default to sequential parsing until parallel is fixed
	backend         Backend
	arch            Arch = .auto
	output_file     string
pub:
	vroot         string = os.dir(@VEXE)
	vmodules_path string = os.vmodules_dir()
}

pub fn new_preferences() Preferences {
	return Preferences{
		backend: if os.user_os() == 'macos' { .arm64 } else { .x64 }
	}
}

// new_preferences_from_args parses full args list including option values
pub fn new_preferences_from_args(args []string) Preferences {
	// Default backend based on OS: macOS defaults to arm64, others to x64
	default_backend := if os.user_os() == 'macos' { 'arm64' } else { 'x64' }
	backend_str := cmdline.option(args, '-backend', default_backend)
	mut backend := if os.user_os() == 'macos' { Backend.arm64 } else { Backend.x64 }
	match backend_str {
		'cleanc' { backend = .cleanc }
		'v' { backend = .v }
		'arm64' { backend = .arm64 }
		'x64' { backend = .x64 }
		else {}
	}

	arch_str := cmdline.option(args, '-arch', 'auto')
	mut arch := Arch.auto
	match arch_str {
		'x64' { arch = .x64 }
		'arm64' { arch = .arm64 }
		else {}
	}

	output_file := cmdline.option(args, '-o', cmdline.option(args, '-output', ''))

	options := cmdline.only_options(args)
	// Default to sequential parsing (no_parallel=true) unless --parallel is specified
	use_parallel := '--parallel' in options
	return Preferences{
		debug:           '--debug' in options || '-d' in options
		verbose:         '--verbose' in options || '-v' in options
		skip_genv:       '--skip-genv' in options
		skip_builtin:    '--skip-builtin' in options
		skip_imports:    '--skip-imports' in options
		skip_type_check: '--skip-type-check' in options
		no_parallel:     !use_parallel
		backend:         backend
		arch:            arch
		output_file:     output_file
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
		debug:           '--debug' in options || '-d' in options
		verbose:         '--verbose' in options || '-v' in options
		skip_genv:       '--skip-genv' in options
		skip_builtin:    '--skip-builtin' in options
		skip_imports:    '--skip-imports' in options
		skip_type_check: '--skip-type-check' in options
		no_parallel:     !use_parallel
		backend:         backend
		arch:            arch
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
