// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module runtime

import os

// nr_jobs returns the same as `nr_cpus`, but influenced by the env variable `VJOBS`.
// If the environment variable `VJOBS` is set, and has a value > 0,
// then `nr_jobs` will return that number instead.
// This is useful for runtime tweaking of e.g. threaded or concurrent code.
pub fn nr_jobs() int {
	$if cross ? {
		// A single thread is *more likely* to work consistently everywhere during bootstrapping.
		// NB: the compiler itself uses runtime.nr_jobs() and sync.pool to process things in parallel
		// in its cgen stage. Returning 1 here, increases the chances of it working on non linux systems.
		return 1
	}
	mut cpus := nr_cpus() - 1
	// allow for overrides, for example using `VJOBS=32 ./v test .`
	vjobs := os.getenv('VJOBS').int()
	if vjobs > 0 {
		cpus = vjobs
	}
	if cpus == 0 {
		return 1
	}
	return cpus
}

// is_32bit returns true if the current executable is running on a 32 bit system.
pub fn is_32bit() bool {
	$if x32 {
		return true
	}
	return false
}

// is_64bit returns true if the current executable is running on a 64 bit system.
pub fn is_64bit() bool {
	$if x64 {
		return true
	}
	return false
}

// is_little_endian returns true if the current executable is running on a little-endian system.
pub fn is_little_endian() bool {
	$if little_endian {
		return true
	}
	return false
}

// is_big_endian returns true if the current executable is running on a big-endian system.
pub fn is_big_endian() bool {
	$if big_endian {
		return true
	}
	return false
}
