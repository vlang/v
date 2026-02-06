// This example shows how to use io.cp, in combination with os.open and crypto.sha256,
// to read and hash files chunk by chunk, without loading them completely in memory (which may
// require too much RAM with big files).
//
// Usage: examples/sha256sum_with_io_cp [FILE]...
//
// Note: to compile the program, use:
// `v -prod -cflags "-march=native -mtune=native" examples/sha256sum_with_io_cp.v`
// After that, to compare it with say `sha256sum`, you can run:
// `v repeat -R 5 "sha256sum v" "examples/sha256sum_with_io_cp v`
import os
import io
import crypto.sha256

fn hash_file(path string) !string {
	mut file := os.open(path)!
	mut digest := sha256.new()
	io.cp(mut file, mut digest, buffer_size: 256 * 1024)!
	file.close()
	return digest.sum([]).hex()
}

fn main() {
	for fpath in os.args#[1..] {
		h := hash_file(fpath)!
		println('${h}  ${fpath}')
	}
}
