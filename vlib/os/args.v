// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

// args_after returns all os.args, located *after* a specified `cut_word`.
// When `cut_word` is NOT found, os.args is returned unmodified.
pub fn args_after(cut_word string) []string {
	if args.len == 0 {
		return []string{}
	}
	mut cargs := []string{}
	if cut_word !in args {
		cargs = args.clone()
	} else {
		mut found := false
		cargs << args[0]
		for a in args[1..] {
			if a == cut_word {
				found = true
				continue
			}
			if !found {
				continue
			}
			cargs << a
		}
	}
	return cargs
}

// args_after returns all os.args, located *before* a specified `cut_word`.
// When `cut_word` is NOT found, os.args is returned unmodified.
pub fn args_before(cut_word string) []string {
	if args.len == 0 {
		return []string{}
	}
	mut cargs := []string{}
	if cut_word !in args {
		cargs = args.clone()
	} else {
		cargs << args[0]
		for a in args[1..] {
			if a == cut_word {
				break
			}
			cargs << a
		}
	}
	return cargs
}
