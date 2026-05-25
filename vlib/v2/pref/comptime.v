// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

// comptime_flag_value evaluates a single comptime flag identifier (as it would
// appear in `$if name {` or `@[if name ?]`). Shared between the parser (struct
// field conditionals) and the transformer (statement / expression $if).
//
// `pref` may be `nil` for early uses (some tests construct partial state);
// flags that depend on backend / user_defines then evaluate to `false`.
pub fn comptime_flag_value(pref &Preferences, name string) bool {
	match name {
		'macos', 'darwin' {
			$if macos {
				return true
			}
			return false
		}
		'linux' {
			$if linux {
				return true
			}
			return false
		}
		'windows' {
			$if windows {
				return true
			}
			return false
		}
		'bsd' {
			$if macos || freebsd || openbsd || netbsd || dragonfly {
				return true
			}
			return false
		}
		'freebsd' {
			$if freebsd {
				return true
			}
			return false
		}
		'openbsd' {
			$if openbsd {
				return true
			}
			return false
		}
		'netbsd' {
			$if netbsd {
				return true
			}
			return false
		}
		'dragonfly' {
			$if dragonfly {
				return true
			}
			return false
		}
		'x64', 'amd64' {
			$if amd64 {
				return true
			}
			return false
		}
		'arm64', 'aarch64' {
			$if arm64 {
				return true
			}
			return false
		}
		'little_endian' {
			$if little_endian {
				return true
			}
			return false
		}
		'big_endian' {
			$if big_endian {
				return true
			}
			return false
		}
		'debug' {
			$if debug {
				return true
			}
			return false
		}
		'native' {
			return pref != unsafe { nil } && (pref.backend == .arm64 || pref.backend == .x64)
		}
		// Native backend cannot resolve C.stdout/C.stderr data symbols through GOT,
		// so use C.write() instead of fwrite() for I/O operations.
		'builtin_write_buf_to_fd_should_use_c_write' {
			return pref != unsafe { nil } && (pref.backend == .arm64 || pref.backend == .x64)
		}
		'tinyc' {
			// For native backends, inline assembly from V source is not supported
			// by the SSA builder. Pretend we're TinyCC so that `$if arm64 && !tinyc`
			// guards select the software fallback path instead of inline asm.
			return pref != unsafe { nil } && (pref.backend == .arm64 || pref.backend == .x64)
		}
		'prealloc' {
			return pref != unsafe { nil } && pref.prealloc
		}
		'new_int', 'gcboehm', 'autofree', 'ppc64' {
			return false
		}
		else {
			if pref != unsafe { nil } && name in pref.user_defines {
				return true
			}
			return false
		}
	}
}
