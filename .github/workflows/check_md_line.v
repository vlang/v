module main

import os

fn main() {
	files_paths := os.args[1..]
	mut errors := 0

	for file_path in files_paths {
		real_path := os.realpath(file_path)
		lines := os.read_lines(real_path) or { continue }
		mut line_num := 1

		for line in lines {
			if line.len > 100 {
				eprintln('$real_path:$line_num with $line.len column width')
				errors++
			}
			line_num++
		}
	}

	if errors > 0 {
		exit(1)
	}
}
