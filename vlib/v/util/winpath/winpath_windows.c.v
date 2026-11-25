// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module winpath

import os

// full_path gives the path that Windows makes of `path` before it acts on it
// (`GetFullPathNameW`): absolute, with `.` and `..` taken as text, without the dots and spaces at the
// end of each name, and a name that this Windows takes as a device, such as `nul`, as that device,
// `\\.\nul`. Links are not followed. It gives `path` itself when Windows makes none.
pub fn full_path(path string) string {
	mut buffer := [32768]u16{}
	wide := path.to_wide()
	defer {
		// Without a garbage collector nothing else frees what `to_wide` allocates.
		unsafe { free(voidptr(wide)) }
	}
	length := int(C.GetFullPathName(voidptr(wide), u32(buffer.len), voidptr(&buffer[0]),
		unsafe { nil }))
	if length <= 0 || length >= buffer.len {
		return path
	}
	// Windows wrote `length` characters into `buffer`, which `string_from_wide2` reads.
	return unsafe { string_from_wide2(&buffer[0], length) }
}

// absolute_path gives the absolute path that Windows makes of `path` (see `full_path`): a relative
// path from the working folder, `C:cache` from the folder that is current on drive `C:`, and `\cache`
// from the root of the current drive. When Windows makes none, it gives `path` made absolute by
// `os.abs_path`.
pub fn absolute_path(path string) string {
	full := full_path(path)
	if os.is_abs_path(full) {
		return full
	}
	return full_path(os.abs_path(path))
}
