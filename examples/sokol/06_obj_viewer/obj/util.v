module obj

import os

// read a file as single lines
pub fn read_lines_from_file(file_path string) []string {
	mut path := ''
	mut rows := []string{}
	$if android {
		path = 'models/' + file_path
		bts := os.read_apk_asset(path) or {
			eprintln('File [$path] NOT FOUND!')
			return rows
		}
		rows = bts.bytestr().split_into_lines()
	} $else {
		path = 'assets/models/' + file_path
		rows = os.read_lines(path) or {
			eprintln('File [$path] NOT FOUND!')
			return rows
		}
	}
	return rows
}

// read a file as []byte
pub fn read_bytes_from_file(file_path string) []byte {
	mut path := ''
	mut buffer := []byte{}
	$if android {
		path = 'models/' + file_path
		buffer = os.read_apk_asset(path) or {
			eprintln('Texure file: [$path] NOT FOUND!')
			exit(0)
		}
	} $else {
		path = 'assets/models/' + file_path
		buffer = os.read_bytes(path) or {
			eprintln('Texure file: [$path] NOT FOUND!')
			exit(0)
		}
	}
	return buffer
}
