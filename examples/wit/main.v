import os
import wit

fn main() {
	src := os.read_file('example.wit') or { panic(err) }
	doc := wit.parse(src) or { panic(err) }
	println(doc.to_v())
}