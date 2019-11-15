// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module runtime

//$if linux {
fn C.sysconf(name int) i64
//}

//$if windows {
fn C.GetCurrentProcessorNumber() u32
//}

pub fn nr_cpus() int {
	$if windows {
		return nr_cpus_win()
	}
	return nr_cpus_nix()
}

pub fn is_32bit() bool {
	mut x := false
	$if x32 { x = true }
	return x
}

pub fn is_64bit() bool {
	mut x := false
	$if x64 { x = true }
	return x
}

pub fn is_little_endian() bool {
	mut x := false
	$if little_endian { x = true }
	return x
}

pub fn is_big_endian() bool {
	mut x := false
	$if big_endian { x = true }
	return x
}
