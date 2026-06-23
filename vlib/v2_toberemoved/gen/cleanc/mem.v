// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module cleanc

import os

fn (g &Gen) print_cgen_mem(stage string) {
	if os.getenv('V2_MEM') == '' {
		return
	}
	$if macos {
		eprintln('  [mem]   cleanc/${stage}: live ${darwin_cleanc_live_mb()} MB')
	} $else {
		eprintln('  [mem]   cleanc/${stage}')
	}
}
