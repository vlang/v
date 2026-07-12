// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

#include <dirent.h>

fn C.opendir(path &char) &C.DIR
fn C.readdir(dir &C.DIR) &C.dirent
fn C.closedir(dir &C.DIR) int

// list_dir_entries_nix returns one-level directory entries without using
// result-or blocks, so SSA/C bootstrap does not depend on option guard lowering.
@[manualfree]
fn list_dir_entries_nix(path string) []string {
	if path.len == 0 {
		return []string{}
	}
	mut res := []string{cap: 50}
	dir := unsafe { C.opendir(path.str) }
	if isnil(dir) {
		return res
	}
	for {
		ent := C.readdir(dir)
		if isnil(ent) {
			break
		}
		unsafe {
			bptr := &u8(&ent.d_name[0])
			if bptr[0] == 0 || (bptr[0] == `.` && bptr[1] == 0)
				|| (bptr[0] == `.` && bptr[1] == `.` && bptr[2] == 0) {
				continue
			}
			res << tos_clone(bptr)
		}
	}
	C.closedir(dir)
	return res
}
