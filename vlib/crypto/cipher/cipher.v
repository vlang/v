// The source code refers to the go standard library, which will be combined with AES in the future.

module cipher

// A Block represents an implementation of block cipher
// using a given key. It provides the capability to encrypt
// or decrypt individual blocks. The mode implementations
// extend that capability to streams of blocks.
interface Block {
	block_size int // block_size returns the cipher's block size.
	encrypt(mut dst []u8, src []u8) // Encrypt encrypts the first block in src into dst.
	// Dst and src must overlap entirely or not at all.
	decrypt(mut dst []u8, src []u8) // Decrypt decrypts the first block in src into dst.
	// Dst and src must overlap entirely or not at all.
}

// A Stream represents a stream cipher.
interface Stream {
	// xor_key_stream XORs each byte in the given slice with a byte from the
	// cipher's key stream. Dst and src must overlap entirely or not at all.
	//
	// If len(dst) < len(src), xor_key_stream should panic. It is acceptable
	// to pass a dst bigger than src, and in that case, xor_key_stream will
	// only update dst[:len(src)] and will not touch the rest of dst.
	//
	// Multiple calls to xor_key_stream behave as if the concatenation of
	// the src buffers was passed in a single run. That is, Stream
	// maintains state and does not reset at each xor_key_stream call.
	xor_key_stream(mut dst []u8, src []u8)
}

// A BlockMode represents a block cipher running in a block-based mode (CBC,
// ECB etc).
interface BlockMode {
	block_size int // block_size returns the mode's block size.
	crypt_blocks(mut dst []u8, src []u8) // crypt_blocks encrypts or decrypts a number of blocks. The length of
	// src must be a multiple of the block size. Dst and src must overlap
	// entirely or not at all.
	//
	// If len(dst) < len(src), crypt_blocks should panic. It is acceptable
	// to pass a dst bigger than src, and in that case, crypt_blocks will
	// only update dst[:len(src)] and will not touch the rest of dst.
	//
	// Multiple calls to crypt_blocks behave as if the concatenation of
	// the src buffers was passed in a single run. That is, BlockMode
	// maintains state and does not reset at each crypt_blocks call.
}

// Utility routines

// fn dup(p []u8) []u8 {
// 	q := make([]u8, p.len)
// 	copy(mut q, p)
// 	return q
// }
