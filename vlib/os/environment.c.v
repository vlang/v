// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

fn C.getenv(&char) &char

// C.GetEnvironmentStringsW & C.FreeEnvironmentStringsW are defined only on windows
fn C.GetEnvironmentStringsW() &u16

fn C.FreeEnvironmentStringsW(&u16) int

// getenv returns the value of the environment variable named by the key.
// If there is not one found, it returns an empty string ''.
pub fn getenv(key string) string {
	return getenv_opt(key) or { '' }
}

// getenv_opt returns the value of a given environment variable.
// Returns `none` if the environment variable does not exist.
@[manualfree]
pub fn getenv_opt(key string) ?string {
	unsafe {
		$if windows {
			kw := key.to_wide()
			defer {
				free(voidptr(kw))
			}
			s := C._wgetenv(kw)
			if s == 0 {
				return none
			}
			return string_from_wide(s)
		} $else {
			s := C.getenv(&char(key.str))
			if s == nil {
				return none
			}
			// Note: C.getenv *requires* that the result be copied.
			return cstring_to_vstring(s)
		}
	}
}

// setenv sets the value of an environment variable with `name` to `value`.
pub fn setenv(name string, value string, overwrite bool) int {
	$if windows {
		format := '${name}=${value}'.to_wide()
		defer {
			unsafe { free(voidptr(format)) }
		}
		if overwrite {
			unsafe {
				return C._wputenv(format)
			}
		} else {
			if getenv(name).len == 0 {
				unsafe {
					return C._wputenv(format)
				}
			}
		}
		return -1
	} $else {
		unsafe {
			return C.setenv(&char(name.str), &char(value.str), overwrite)
		}
	}
}

// unsetenv clears an environment variable with `name`.
pub fn unsetenv(name string) int {
	$if windows {
		format := '${name}='.to_wide()
		defer {
			unsafe { free(voidptr(format)) }
		}
		return C._wputenv(format)
	} $else {
		return C.unsetenv(&char(name.str))
	}
}

// environ returns a map of all the current environment variables.
// See: https://linux.die.net/man/5/environ for Unix platforms.
// See: https://docs.microsoft.com/bg-bg/windows/win32/api/processenv/nf-processenv-getenvironmentstrings
// for Windows OS.
pub fn environ() map[string]string {
	// TODO how to declare Virtual C globals?
	// const C.environ &&char
	mut res := map[string]string{}
	$if windows {
		mut estrings := C.GetEnvironmentStringsW()
		mut eline := ''
		for c := estrings; *c != 0; {
			eline = unsafe { string_from_wide(c) }
			eq_index := eline.index_u8(`=`)
			if eq_index > 0 {
				res[eline[0..eq_index]] = eline[eq_index + 1..]
			}
			unsafe {
				c = c + eline.len + 1
			}
		}
		C.FreeEnvironmentStringsW(estrings)
	} $else {
		start := &&char(voidptr(C.environ))
		mut i := 0
		for {
			x := unsafe { start[i] }
			if x == 0 {
				break
			}
			eline := unsafe { cstring_to_vstring(x) }
			eq_index := eline.index_u8(`=`)
			if eq_index > 0 {
				res[eline[0..eq_index]] = eline[eq_index + 1..]
			}
			i++
		}
	}
	return res
}
