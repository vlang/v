// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import v2.pref
import v2.builder

fn main() {
	args := os.args[1..]

	prefs := pref.new_preferences_from_args(args)

	files := get_files(args)
	if files.len == 0 {
		eprintln('At least 1 .v file expected')
		exit(1)
	}
	$if debug {
		eprintln('v files: ${files}')
	}

	mut b := builder.new_builder(prefs)
	b.build(files)
}

// get_files extracts source files from args, excluding options and their values
fn get_files(args []string) []string {
	options_with_values := ['-backend', '-o', '-output', '-arch']
	mut files := []string{}
	mut skip_next := false
	for arg in args {
		if skip_next {
			skip_next = false
			continue
		}
		if arg.starts_with('-') {
			if arg in options_with_values {
				skip_next = true
			}
			continue
		}
		files << arg
	}
	return files
}
