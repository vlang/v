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
			return normalize_current_os_name(pref.target_os_or_host()) == 'macos'
		}
		'linux' {
			return normalize_current_os_name(pref.target_os_or_host()) == 'linux'
		}
		'windows' {
			return normalize_current_os_name(pref.target_os_or_host()) == 'windows'
		}
		'bsd' {
			return normalize_current_os_name(pref.target_os_or_host()) in [
				'macos',
				'freebsd',
				'openbsd',
				'netbsd',
				'dragonfly',
			]
		}
		'freebsd' {
			return normalize_current_os_name(pref.target_os_or_host()) == 'freebsd'
		}
		'openbsd' {
			return normalize_current_os_name(pref.target_os_or_host()) == 'openbsd'
		}
		'netbsd' {
			return normalize_current_os_name(pref.target_os_or_host()) == 'netbsd'
		}
		'dragonfly' {
			return normalize_current_os_name(pref.target_os_or_host()) == 'dragonfly'
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
		'v2_native_windows_pe_minimal' {
			return pref != unsafe { nil } && pref.backend == .x64
				&& pref.get_effective_arch() == .x64
				&& normalize_current_os_name(pref.target_os_or_host()) == 'windows'
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
