// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import os

// t_print_mem reports live malloc bytes at a transformer sub-phase boundary.
// Gated on V2_MEM. Under -gc none the value is monotonic on macOS, so deltas
// between stages are the exact bytes each sub-phase allocated. Defined for all
// platforms (transform_files calls it everywhere); the malloc-statistics probe
// is macOS-only, so other platforms just print the stage marker.
fn t_print_mem(stage string) {
	if os.getenv('V2_MEM') == '' {
		return
	}
	$if macos {
		eprintln('  [mem]   transform/${stage}: live ${darwin_transform_live_mb()} MB')
	} $else {
		eprintln('  [mem]   transform/${stage}')
	}
}
