module obj

import os
import os.asset

// read a file as single lines
pub fn read_lines_from_file(file_path string) []string {
	if os.exists(file_path) {
		return os.read_lines(file_path) or { [] }
	}
	return read_bytes_from_file(file_path).bytestr().split_into_lines()
}

// read a file as []u8
pub fn read_bytes_from_file(file_path string) []u8 {
	if os.exists(file_path) {
		return os.read_bytes(file_path) or { [] }
	}
	mpath := 'models/${file_path}'
	return asset.read_bytes('assets', mpath) or {
		eprintln('Model file NOT FOUND: `${mpath}`')
		exit(0)
	}
}
