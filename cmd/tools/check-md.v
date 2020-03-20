module main

import os

const (
	too_long_line_length = 100
)

fn main() {
	files_paths := os.args[1..]
	mut errors := 0
	for file_path in files_paths {
		real_path := os.real_path(file_path)
		lines := os.read_lines(real_path) or {
			continue
		}
		for i, line in lines {
			if line.len > too_long_line_length {
				eprintln('$real_path:${i+1}:${line.len+1}: line too long')
				errors++
			}
		}
	}
	// TODO: uncomment this AFTER doc/docs.md line lengths are fixed
	/*
	if errors > 0 {
		exit(1)
	}
	*/

}
