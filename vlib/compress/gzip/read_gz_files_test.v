import os
import compress.gzip

const samples_folder = os.join_path(os.dir(@FILE), 'samples')

fn s(fname string) string {
	return os.join_path(samples_folder, fname)
}

fn read_and_decode_file(fpath string) !([]u8, string) {
	compressed := os.read_bytes(fpath)!
	decoded := gzip.decompress(compressed)!
	content := decoded.bytestr()
	return compressed, content
}

fn test_reading_and_decoding_a_known_gziped_file() {
	compressed, content := read_and_decode_file(s('known.gz'))!
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

fn test_reading_gzip_files_compressed_with_different_options() {
	_, content1 := read_and_decode_file(s('readme_level_1.gz'))!
	_, content5 := read_and_decode_file(s('readme_level_5.gz'))!
	_, content9 := read_and_decode_file(s('readme_level_9.gz'))!
	_, content9_rsyncable := read_and_decode_file(s('readme_level_9_rsyncable.gz'))!
	assert content9_rsyncable == content9
	assert content9 == content5
	assert content5 == content1
}
