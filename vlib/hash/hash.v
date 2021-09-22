// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module hash

interface Hasher {
	// Sum appends the current hash to b and returns the resulting array.
	// It does not change the underlying hash state.
	sum(b []byte) []byte
	size() int
	block_size() int
}

interface Hash32er {
	sum32() u32
}

interface Hash64er {
	sum64() u64
}
