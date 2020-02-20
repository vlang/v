module main

import os

const (
	too_long_line_length = 100
)

fn main() {
	files_paths := os.args[1..]
	mut errors := 0

	for file_path in files_paths {
		real_path := os.realpath(file_path)
		lines := os.read_lines(real_path) or { continue }
		mut line_num := 1

		for line in lines {
			if line.len > too_long_line_length {
				eprintln('$real_path:$line_num:${line.len+1}: line too long')
				errors++
			}
			line_num++
		}
	}

	if errors > 0 {
		exit(1)
	}
}
