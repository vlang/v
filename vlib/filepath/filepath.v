// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module filepath

// OS Path Separators
const(
	PATH_SEPARATOR_NIX = '/'
	PATH_SEPARATOR_WIN = '\\'
)


pub fn separator() string {
	$if windows {
		return PATH_SEPARATOR_WIN
	} 
	$else {
		return PATH_SEPARATOR_NIX
	}
}

pub fn join(path []string) string {
    return path.join(separator())
}

pub fn split(path string) []string {
    return path.split(separator())
}
