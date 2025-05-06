module main

import arrays.diff
import os

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

	src := os.read_lines(p1)!
	dst := os.read_lines(p2)!

	mut ctx := diff.diff(src, dst)
	str := ctx.gen_str(colorful: true, block_header: true)
	println(str)

	os.rm(p1)!
	os.rm(p2)!
}
