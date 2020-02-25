// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import (
	os
	v.pref
)

fn check_for_common_mistake(args []string, p &pref.Preferences) {
	if p.out_name.ends_with('.c') && p.backend == .c {
		println('HINT: `-o $p.out_name` implies `-csource keep` and does not results in an executable currently.')
		//println('      To overwrite this, specify `-csource drop` explicitly.')
	}
	if p.out_name.ends_with('.js') && p.backend != .js {
		println('HINT: `-o $p.out_name` implies `-backend js` currently.')
		//println('      To overwrite this, specify the intended backend explicitly.')
	}
	if p.path == 'vlib/compiler' || p.path == 'v.v' {
		println('HINT: The V compiler is now located in `cmd/v`.')
		println('      `$p.path` is no longer the correct path if you are intending to do so.')
	}
	if !p.path.ends_with('.v') && !os.is_dir(p.path) && os.is_dir(p.path + os.path_separator) {
		println('HINT: `$p.path` is not a directory nor a file suffixed with `.v`.')
		println('      Did you perhaps accidentally reference the compiled executable?')
		println('      To make sure V detects the directory correctly, add the path separator to the end of the path like so:')
		println('      `v $p.path$os.path_separator`')
	}
}
