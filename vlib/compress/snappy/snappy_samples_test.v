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

fn test_precompressed_framing_alice() {
	alice_compressed := os.read_bytes(s('alice29_framing.snappy'))!
	alice := os.read_bytes(s('alice29.txt'))!

	// Normalize line endings before comparing
	decompressed_str := decode_stream(alice_compressed)!.bytestr().replace('\r\n', '\n')
	alice_str := alice.bytestr().replace('\r\n', '\n')

	assert decompressed_str == alice_str, 'compressed alice should be the same as decompressed'
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

fn test_precompressed_block_alice() {
	alice_compressed := os.read_bytes(s('alice29_block.snappy'))!
	alice := os.read_bytes(s('alice29.txt'))!

	decompressed := decompress(alice_compressed)!

	// Normalize line endings before comparing
	decompressed_str := decompressed.bytestr().replace('\r\n', '\n')
	alice_str := alice.bytestr().replace('\r\n', '\n')

	assert decompressed_str == alice_str, 'compressed alice should be the same as decompressed'
}
