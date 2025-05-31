module main

import os

fn (mut c Create) set_bin_project_files() {
	base := if c.new_dir { c.name } else { '' }
	c.files << ProjectFiles{
		path:    os.join_path(base, 'main.v')
		content: "module main

fn main() {
	println('Hello World!')
}
"
	}
}
