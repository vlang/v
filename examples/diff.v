module main

import arrays.diff
import os

// diff_files generate diff for two files.
fn diff_files(src_file string, dst_file string) !string {
	src := os.read_lines(src_file)!
	dst := os.read_lines(dst_file)!

	mut ctx := diff.diff(src, dst)
	return ctx.generate_patch(colorful: true, block_header: true)
}

fn main() {
	f1 := "Module{
	name: 'Foo'
	description: 'Awesome V module.'
	version: '0.0.0'
	dependencies: []
}
"
	f2 := "Module{
	name: 'foo'
	description: 'Awesome V module.'
	version: '0.1.0'
	license: 'MIT'
	dependencies: []
}
"
	p1 := 'diff_f1.txt'
	p2 := 'diff_f2.txt'
	os.write_file(p1, f1)!
	os.write_file(p2, f2)!

	str := diff_files(p1, p2)!
	println(str)

	os.rm(p1)!
	os.rm(p2)!
}
