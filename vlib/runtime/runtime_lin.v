// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module runtime

#include <sys/sysinfo.h>

fn C.get_nprocs() int

pub fn nr_cpus() int {
	return C.get_nprocs()
}
