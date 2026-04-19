module snappy

import os
import benchmark

const samples_folder = os.join_path(os.dir(@FILE), 'samples')

fn s(fname string) string {
	return os.join_path(samples_folder, fname)
}

fn test_framing_compress_decompress_alice() {
	alice := os.read_bytes(s('alice29.txt'))!

	alice_compressed := encode_stream(alice)
	assert decode_stream(alice_compressed)! == alice, 'compressed alice should be the same as decompressed'
}

fn test_framing_decompress_block_alice() {
	alice_compressed := os.read_bytes(s('alice29_framing.snappy'))!
	alice := os.read_bytes(s('alice29.txt'))!

	assert decode_stream(alice_compressed)! == alice, 'compressed alice should be the same as decompressed'
}

fn test_benchmark_framing_alice() {
	alice := os.read_bytes(s('alice29.txt'))!

	mut b := benchmark.start()

	encode_stream(alice)

	b.measure('framing_alice')
}

fn test_benchmark_block_alice() {
	alice := os.read_bytes(s('alice29.txt'))!

	mut b := benchmark.start()

	compress(alice)

	b.measure('block_alice')
}

fn test_block_compress_decompress_alice() {
	alice := os.read_bytes(s('alice29.txt'))!

	alice_compressed := compress(alice)
	assert decompress(alice_compressed)! == alice, 'compressed alice should be the same as decompressed'
}

fn test_block_decompress_block_alice() {
	alice_compressed := os.read_bytes(s('alice29_block.snappy'))!
	alice := os.read_bytes(s('alice29.txt'))!

	assert decompress(alice_compressed)! == alice, 'compressed alice should be the same as decompressed'
}
