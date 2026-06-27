module main

struct Entry {
	name         string
	initial_data []u8
	size         int
}

fn collect(entries []Entry) []u8 {
	mut out := []u8{}
	for entry in entries {
		if entry.initial_data.len > 0 {
			out << entry.initial_data
			continue
		}
		for _ in 0 .. entry.size {
			out << u8(0)
		}
	}
	return out
}

fn main() {
	entries := [
		Entry{
			name:         'a'
			initial_data: [u8(1), 2, 3]
			size:         0
		},
		Entry{
			name:         'b'
			initial_data: []u8{}
			size:         4
		},
		Entry{
			name:         'c'
			initial_data: [u8(9)]
			size:         0
		},
	]
	out := collect(entries)
	println(out.len)
	println(out[0])
	println(out[5])
	println(out[7])
}
