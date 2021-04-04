// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

fn C.getenv(&char) &char

// C.GetEnvironmentStringsW & C.FreeEnvironmentStringsW are defined only on windows
fn C.GetEnvironmentStringsW() &u16

fn C.FreeEnvironmentStringsW(&u16) int

// `getenv` returns the value of the environment variable named by the key.
pub fn getenv(key string) string {
	unsafe {
		$if windows {
			s := C._wgetenv(key.to_wide())
			if s == 0 {
				return ''
			}
			return string_from_wide(s)
		} $else {
			s := C.getenv(&char(key.str))
			if s == voidptr(0) {
				return ''
			}
			// NB: C.getenv *requires* that the result be copied.
			return cstring_to_vstring(&byte(s))
		}
	}
}

// os.setenv sets the value of an environment variable with `name` to `value`.
pub fn setenv(name string, value string, overwrite bool) int {
	$if windows {
		format := '$name=$value'
		if overwrite {
			unsafe {
				return C._putenv(&char(format.str))
			}
		} else {
			if getenv(name).len == 0 {
				unsafe {
					return C._putenv(&char(format.str))
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

// os.unsetenv clears an environment variable with `name`.
pub fn unsetenv(name string) int {
	$if windows {
		format := '$name='
		return C._putenv(&char(format.str))
	} $else {
		return C.unsetenv(&char(name.str))
	}
}

// See: https://linux.die.net/man/5/environ for unix platforms.
// See: https://docs.microsoft.com/bg-bg/windows/win32/api/processenv/nf-processenv-getenvironmentstrings
// os.environ returns a map of all the current environment variables
type PChar = &char
type PPChar = &&char

fn deref_ppchar(x &PChar) &char {
	return &char(*x)
}

pub fn environ() map[string]string {
	mut res := map[string]string{}
	$if windows {
		mut estrings := C.GetEnvironmentStringsW()
		mut eline := ''
		for c := estrings; *c != 0; {
			eline = unsafe { string_from_wide(c) }
			eq_index := eline.index_byte(`=`)
			if eq_index > 0 {
				res[eline[0..eq_index]] = eline[eq_index + 1..]
			}
			unsafe {
				c = c + eline.len + 1
			}
		}
		C.FreeEnvironmentStringsW(estrings)
	} $else {
		e := unsafe { PPChar(voidptr(C.environ)) }
		size_of_pchar := int(sizeof(PChar))
		for i := 0; true; i++ {
			z := voidptr(unsafe { &byte(voidptr(e)) + size_of_pchar * i })
			current_ptr := &PChar(z)
			derefed := deref_ppchar(current_ptr)
			if isnil(derefed) {
				break
			}
			eline := unsafe { cstring_to_vstring(derefed) }
			eq_index := eline.index_byte(`=`)
			if eq_index > 0 {
				res[eline[0..eq_index]] = eline[eq_index + 1..]
			}
		}
	}
	return res
}
