module main

import os
import compress.zstd

const sample_file = os.join_path(os.dir(@FILE), '..', 'samples', 'known.zst')

fn main() {
	compressed := os.read_bytes(sample_file) or { panic(err) }
	decompressed := zstd.decompress(compressed) or { panic(err) }
	println('decoded `${sample_file}`:')
	println(decompressed.bytestr())
}
