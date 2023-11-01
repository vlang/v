module main

fn (mut c Create) set_bin_project_files(new bool) {
	c.files << ProjectFiles{
		path: if new { '${c.name}/src/main.v' } else { 'src/main.c.v' }
		content: "module main

fn main() {
	println('Hello World!')
}
"
	}
}
