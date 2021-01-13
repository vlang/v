/*
import os

const (
	const_file = $embed_file('v.png')
)

fn main() {
	mut file := const_file
	mut out := os.join_path(os.temp_dir(), 'const_v_out.png')
	mut fw := os.create(out) or { panic('failed to create file $filename') }
	fw.write_bytes(file.data(), file.len)
	fw.close()

	mut size := os.file_size(out)
	assert size == file.len

	file = $embed_file('v.png')
	out = os.join_path(os.temp_dir(), 'v_out.png')
	fw = os.create(out) or { panic('failed to create file $filename') }
	fw.write_bytes(file.data(), file.len)
	fw.close()

	size = os.file_size(out)
	assert size == file.len
}
*/
