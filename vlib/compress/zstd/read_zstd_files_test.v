import os
import compress.zstd

const samples_folder = os.join_path(os.dir(@FILE), 'samples')

fn s(fname string) string {
	return os.join_path(samples_folder, fname)
}

fn test_decoding_all_samples_files() {
	known := s('known.zst')
	readme_samples := [1, 5, 9, 19].map(s('readme_level_${it}.zst'))
	files := os.walk_ext(samples_folder, '.zst')
	assert known in files
	mut readme_contents := map[string][]u8{}
	for path in files {
		compressed := os.read_bytes(path)!
		decoded := zstd.decompress(compressed)!
		assert decoded.len > 0, 'decoded sample is empty: ${path}'
		if path == known {
			content := decoded.bytestr()
			assert compressed#[0..3] == [u8(40), 181, 47]
			assert compressed#[-5..] == [u8(10), 78, 32, 170, 44]
			assert content.contains('## Description')
			assert content.contains('## Examples:')
			assert content.ends_with('```')
		}
		if path in readme_samples {
			readme_contents[path] = decoded
		}
	}
	reference := readme_contents[readme_samples[0]]!
	for path in readme_samples[1..] {
		assert readme_contents[path]! == reference, path
	}
}
