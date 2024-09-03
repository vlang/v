module main

struct Blob {
	label int
}

fn (mut b Blob) str() string {
	return ''
}

struct Labels {
	blobs map[int]Blob
}

fn break_it() Labels {
	mut a := Labels{}
	return a
}

fn test_main() {
	mut a := break_it()
	assert a.blobs.len == 0
}
