// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import os.cmdline
import v2.pref
import v2.builder

fn main() {
	args := os.args[1..]

	options := cmdline.only_options(args)
	prefs := pref.new_preferences_using_options(options)

	files := cmdline.only_non_options(args)
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
