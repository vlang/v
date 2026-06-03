// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

import os

fn is_target_or_mode_comptime_flag(name string) bool {
	return name in [
		'macos',
		'darwin',
		'mac',
		'linux',
		'windows',
		'bsd',
		'freebsd',
		'openbsd',
		'netbsd',
		'dragonfly',
		'android',
		'termux',
		'ios',
		'solaris',
		'qnx',
		'serenity',
		'plan9',
		'vinix',
		'cross',
		'none',
		'freestanding',
		'bare',
	]
}

pub fn define_list_contains(defines []string, name string) bool {
	lower_name := name.to_lower()
	return name in defines || lower_name in defines
}

// pkgconfig_result runs pkg-config with `args` and returns stdout on success.
pub fn pkgconfig_result(args []string) ?string {
	if args.len == 0 {
		return none
	}
	mut quoted_args := []string{cap: args.len}
	for arg in args {
		quoted_args << os.quoted_path(arg)
	}
	result := os.execute('pkg-config ${quoted_args.join(' ')}')
	if result.exit_code != 0 {
		return none
	}
	return result.output.trim_space()
}

// comptime_pkgconfig_value evaluates `$pkgconfig('name')` in compile-time
// conditions. Missing pkg-config packages are false, matching v1 behavior.
pub fn comptime_pkgconfig_value(name string) bool {
	if _ := pkgconfig_result(['--exists', name]) {
		return true
	}
	return false
}

// comptime_flag_value evaluates a plain comptime flag identifier, as it would
// appear in `$if name {` or `@[if name]`. Use comptime_optional_flag_value for
// the optional `name ?` form. Shared between the parser (struct field
// conditionals) and the transformer (statement / expression $if).
//
// `pref` may be `nil` for early uses (some tests construct partial state);
// flags that depend on backend / user_defines then evaluate to `false`.
pub fn comptime_flag_value(pref &Preferences, name string) bool {
	match name {
		'macos', 'darwin', 'mac' {
			return pref.normalized_target_os() == 'macos'
		}
		'linux' {
			return pref.normalized_target_os() == 'linux'
		}
		'windows' {
			return pref.normalized_target_os() == 'windows'
		}
		'bsd' {
			return pref.normalized_target_os() in [
				'macos',
				'freebsd',
				'openbsd',
				'netbsd',
				'dragonfly',
			]
		}
		'freebsd' {
			return pref.normalized_target_os() == 'freebsd'
		}
		'openbsd' {
			return pref.normalized_target_os() == 'openbsd'
		}
		'netbsd' {
			return pref.normalized_target_os() == 'netbsd'
		}
		'dragonfly' {
			return pref.normalized_target_os() == 'dragonfly'
		}
		'android' {
			return pref.normalized_target_os() == 'android'
		}
		'termux' {
			return pref.normalized_target_os() == 'termux'
		}
		'ios' {
			return pref.normalized_target_os() == 'ios'
		}
		'solaris' {
			return pref.normalized_target_os() == 'solaris'
		}
		'qnx' {
			return pref.normalized_target_os() == 'qnx'
		}
		'serenity' {
			return pref.normalized_target_os() == 'serenity'
		}
		'plan9' {
			return pref.normalized_target_os() == 'plan9'
		}
		'vinix' {
			return pref.normalized_target_os() == 'vinix'
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
				&& pref.get_effective_arch() == .x64 && pref.normalized_target_os() == 'windows'
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
		'no_backtrace' {
			return pref != unsafe { nil } && ((pref.backend == .arm64 || pref.backend == .x64)
				|| name in pref.user_defines)
		}
		'prealloc' {
			return pref != unsafe { nil } && pref.prealloc
		}
		'cross' {
			return pref != unsafe { nil } && (pref.is_cross_target() || name in pref.user_defines)
		}
		'none' {
			return pref != unsafe { nil } && pref.normalized_target_os() == 'none'
		}
		'freestanding' {
			return pref != unsafe { nil } && (pref.is_freestanding() || name in pref.user_defines)
		}
		'freestanding_hooks' {
			return pref != unsafe { nil }
				&& (pref.has_freestanding_hooks() || name in pref.user_defines)
		}
		'freestanding_output' {
			return pref != unsafe { nil }
				&& (pref.has_freestanding_hook('output') || name in pref.user_defines)
		}
		'freestanding_panic' {
			return pref != unsafe { nil }
				&& (pref.has_freestanding_hook('panic') || name in pref.user_defines)
		}
		'freestanding_alloc' {
			return pref != unsafe { nil }
				&& (pref.has_freestanding_hook('alloc') || name in pref.user_defines)
		}
		'new_int', 'gcboehm', 'autofree', 'ppc64' {
			return false
		}
		else {
			if pref != unsafe { nil } && define_list_contains(pref.user_defines, name) {
				return true
			}
			return false
		}
	}
}

pub fn comptime_optional_define_value(name string, user_defines []string, explicit_user_defines []string) bool {
	if define_list_contains(explicit_user_defines, name) {
		return true
	}
	if is_target_or_mode_comptime_flag(name.to_lower()) {
		return false
	}
	return define_list_contains(user_defines, name)
}

// comptime_optional_flag_value evaluates the `name ?` form. Unlike a plain
// `$if name`, target OS and target-mode flags must not become true implicitly.
// Non-target compiler capability flags keep their normal value, because V's
// builtin/runtime code uses the optional form for guarded internal fallbacks.
pub fn comptime_optional_flag_value(pref &Preferences, name string) bool {
	if pref == unsafe { nil } {
		return false
	}
	if comptime_optional_define_value(name, pref.user_defines, pref.explicit_user_defines) {
		return true
	}
	if is_target_or_mode_comptime_flag(name.to_lower()) {
		return false
	}
	return comptime_flag_value(pref, name)
}
