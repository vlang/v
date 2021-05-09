import os

fn main() {
	if true {
		mut f := os.open('file.txt') or {
			panic('error')
		}
		defer { f.close() }
	}
}
