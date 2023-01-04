import os

fn main() {
	files := os.ls('.') or {
		println(err)
		return
	}
	mut f := os.create('file_list.txt') or {
		println(err)
		return
	}
	for file in files {
		if os.is_file(file) {
			f.write_string(file + '\r\n') or { println(err) }
		}
	}
	f.close()
}
