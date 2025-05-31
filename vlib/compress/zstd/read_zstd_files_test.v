import os
import compress.zstd

const samples_folder = os.join_path(os.dir(@FILE), 'samples')

fn s(fname string) string {
	return os.join_path(samples_folder, fname)
}

fn read_and_decode_file(fpath string) !([]u8, string) {
	compressed := os.read_bytes(fpath)!
	decoded := zstd.decompress(compressed)!
	content := decoded.bytestr()
	return compressed, content
}

fn test_reading_and_decoding_a_known_zstded_file() {
	compressed, content := read_and_decode_file(s('known.zst'))!
	assert compressed#[0..3] == [u8(40), 181, 47]
	assert compressed#[-5..] == [u8(10), 78, 32, 170, 44]
	assert content.contains('## Description')
	assert content.contains('## Examples:')
	assert content.ends_with('```')
}

fn test_decoding_all_samples_files() {
	for zstd_file in os.walk_ext(samples_folder, '.zst') {
		_, content := read_and_decode_file(zstd_file)!
		assert content.len > 0, 'decoded content should not be empty: `${content}`'
	}
}

fn test_reading_zstd_files_compressed_with_different_compress_level() {
	_, content1 := read_and_decode_file(s('readme_level_1.zst'))!
	_, content5 := read_and_decode_file(s('readme_level_5.zst'))!
	_, content9 := read_and_decode_file(s('readme_level_9.zst'))!
	_, content19 := read_and_decode_file(s('readme_level_19.zst'))!
	assert content19 == content9
	assert content9 == content5
	assert content5 == content1
}
