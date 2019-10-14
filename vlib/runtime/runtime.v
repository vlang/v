// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module runtime

import os

//$if linux {
fn C.sysconf(name int) i64
//}

//$if windows {
fn C.GetCurrentProcessorNumber() u32
//}

pub fn nr_cpus() int {
	$if linux {
		return int(C.sysconf(C._SC_NPROCESSORS_ONLN))
	}
	$if mac {
		return int(C.sysconf(C._SC_NPROCESSORS_ONLN))
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
