// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module winpath

// full_path gives `path` as it is: only Windows makes another path of it before it acts on it.
pub fn full_path(path string) string {
	return path
}

// absolute_path gives `path` as it is (see `full_path`).
pub fn absolute_path(path string) string {
	return path
}
