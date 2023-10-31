module obj

import os

// read a file as single lines
pub fn read_lines_from_file(file_path string) []string {
	mut path := ''
	mut rows := []string{}
	$if android {
		path = 'models/' + file_path
		bts := os.read_apk_asset(path) or {
			eprintln('File [${path}] NOT FOUND!')
			return rows
		}
		rows = bts.bytestr().split_into_lines()
	} $else {
		path = os.resource_abs_path('assets/models/' + file_path)
		rows = os.read_lines(path) or {
			eprintln('File [${path}] NOT FOUND! file_path: ${file_path}')
			return rows
		}
	}
	return rows
}

// read a file as []u8
pub fn read_bytes_from_file(file_path string) []u8 {
	mut path := ''
	mut buffer := []u8{}
	$if android {
		path = 'models/' + file_path
		buffer = os.read_apk_asset(path) or {
			eprintln('Texure file: [${path}] NOT FOUND!')
			exit(0)
		}
	} $else {
		path = os.resource_abs_path('assets/models/' + file_path)
		buffer = os.read_bytes(path) or {
			eprintln('Texure file: [${path}] NOT FOUND!')
			exit(0)
		}
	}
	return buffer
}
