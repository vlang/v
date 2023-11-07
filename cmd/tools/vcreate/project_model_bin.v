module main

fn (mut c Create) set_bin_project_files() {
	c.files << ProjectFiles{
		path: if c.new_dir { '${c.name}/src/main.v' } else { 'src/main.v' }
		content: "module main

fn main() {
	println('Hello World!')
}
"
	}
}
