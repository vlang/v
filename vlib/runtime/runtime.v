// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
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

pub fn nr_jobs() int {
	mut cpus := nr_cpus()
	// allow for overrides, for example using `VJOBS=32 ./v test .`
	vjobs := os.getenv('VJOBS').int()
	if vjobs > 0 {
		cpus = vjobs
	}
	return cpus
}

pub fn is_32bit() bool {
	$if x32 { return true }
	return false
}

pub fn is_64bit() bool {
	$if x64 { return true }
	return false
}

pub fn is_little_endian() bool {
	$if little_endian { return true }
	return false
}

pub fn is_big_endian() bool {
	$if big_endian { return true }
	return false
}
