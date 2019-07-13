// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module hash

interface Hash {
	Sum(b []byte) []byte
}


interface Hash32 {
	Sum32() uint32
}

interface Hash64 {
	Sum64() uint64
}