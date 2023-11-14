module main

import os

fn (mut c Create) set_bin_project_files() {
	main_path := os.join_path('src', 'main.v')
	c.files << ProjectFiles{
		path: if c.new_dir { os.join_path(c.name, main_path) } else { main_path }
		content: "module main

fn main() {
	println('Hello World!')
}
"
	}
}
