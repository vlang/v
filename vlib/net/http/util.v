// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// A fast version that avoids allocations in s.split()
// returns the locations of the 2 spaces
// "GET / HTTP/1.1" => ["GET" "/" "HTTP/1.1"]
fn fast_request_words(line string) (int, int) {
	space1 := line.index(' ') or { return 0, 0 }
	space2 := line.index_after(' ', space1 + 1) or { return 0, 0 }
	return space1, space2
}
