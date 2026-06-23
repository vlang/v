module main

const page_size = 64

@[direct_array_access]
fn fill_hash(data &u8, data_len int, out &u8) {
	for i in 0 .. 32 {
		unsafe {
			out[i] = if i < data_len { data[i] + 1 } else { u8(i) }
		}
	}
}

fn hash_pages_shape(data &u8, mut hashes []u8, page_start int, page_end int, code_limit int) {
	mut hash_buf := [32]u8{}
	for page := page_start; page < page_end; page++ {
		start := page * page_size
		mut end := start + page_size
		if end > code_limit {
			end = code_limit
		}
		unsafe {
			fill_hash(data + start, end - start, &hash_buf[0])
		}
		hash_offset := page * 32
		for i in 0 .. 32 {
			hashes[hash_offset + i] = hash_buf[i]
		}
	}
}

fn main() {
	mut data := []u8{len: 150}
	for i in 0 .. data.len {
		data[i] = u8(i % 251)
	}
	mut hashes := []u8{len: 96}
	unsafe {
		hash_pages_shape(data.data, mut hashes, 0, 3, data.len)
	}
	println(hashes[0])
	println(hashes[32])
	println(hashes[64])
	println(hashes[95])
}
