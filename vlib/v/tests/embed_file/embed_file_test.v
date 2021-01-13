import os

const (
	const_file = $embed_file('/home/runner/work/v/v/vlib/v/tests/embed_file/v.png')
)

fn test_const_embed_file() {
	mut file := const_file
	out := os.join_path(os.temp_dir(), 'v_embed_const_out.png')
	os.rm(out)
	mut fw := os.create(out) or { panic('failed to create file $out') }
	fw.write_bytes(file.data(), file.len)
	fw.close()

	size := os.file_size(out)
	assert size == file.len
}

fn test_embed_file() {
	mut file := $embed_file('/home/runner/work/v/v/vlib/v/tests/embed_file/v.png')
	out := os.join_path(os.temp_dir(), 'v_embed_out.png')
	os.rm(out)
	mut fw := os.create(out) or { panic('failed to create file $out') }
	fw.write_bytes(file.data(), file.len)
	fw.close()

	size := os.file_size(out)
	assert size == file.len
}
