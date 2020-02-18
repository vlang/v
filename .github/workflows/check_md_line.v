module main

import os
import filepath

fn main() {
	args := os.args
	exec_path := args[0]
	vroot := filepath.dir(filepath.dir(filepath.dir(exec_path)))

	docs_md_path := filepath.join(vroot, 'doc', 'docs.md')
	docs_md := os.read_file(docs_md_path) or { return }
	lines := docs_md.split('\n')
	mut i := 0
	for i = 0; i < lines.len; i++ {
		if lines[i].len >= 100 {
			eprintln('The number of columns in line $i exceeds the limit: 100')
		}
	}
	if i > 0 {
		exit(1)
	}
}
