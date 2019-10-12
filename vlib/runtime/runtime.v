// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module runtime

import os

$if linux {
	#include <sys/sysinfo.h>
}
//$if linux {
fn C.get_nprocs() int
//}

//$if windows {
fn C.GetCurrentProcessorNumber() u32
//}

pub fn nr_cpus() int {
	$if linux {
		return C.get_nprocs()
	}
	$if windows {
		mut nr := int(C.GetCurrentProcessorNumber())
		if nr == 0 {
			nr = os.getenv('NUMBER_OF_PROCESSORS').int()
		}
		return nr
	}
	return 1
}
