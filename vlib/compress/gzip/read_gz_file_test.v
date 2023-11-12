import os
import compress.gzip

const samples_folder = os.join_path(os.dir(@FILE), 'samples')

fn read_and_decode_file(fpath string) !([]u8, string) {
	compressed := os.read_bytes(fpath)!
	decoded := gzip.decompress(compressed)!
	content := decoded.bytestr()
	return compressed, content
}

fn test_reading_and_decoding_a_known_gziped_file() {
	compressed, content := read_and_decode_file(os.join_path(samples_folder, 'known.gz'))!
	assert compressed#[0..3] == [u8(31), 139, 8]
	assert compressed#[-5..] == [u8(127), 115, 1, 0, 0]
	assert content.contains('## Description:')
	assert content.contains('## Examples:')
	assert content.ends_with('```\n')
}

fn test_decoding_all_samples_files() {
	for gz_file in os.walk_ext(samples_folder, '.gz') {
		_, content := read_and_decode_file(gz_file)!
		assert content.len > 0, 'decoded content should not be empty: `${content}`'
	}
}
