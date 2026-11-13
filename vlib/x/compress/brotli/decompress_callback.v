module brotli

// DecompressCallback receives decoded chunks and returns the chunk length to
// continue, or 0 to stop early. It matches compress.brotli.DecompressCallback
// and compress.deflate.ChunkCallback so callbacks written for those entry
// points can be reused here.
pub type DecompressCallback = fn (chunk []u8, userdata voidptr) int

const callback_chunk_size = 32 * 1024

// decompress_with_callback decodes `data` with default DecompressParams and
// delivers the output in 32 KiB chunks, mirroring the native
// compress.brotli entry point. Returning a short count from `cb` stops
// delivery and returns the bytes delivered so far without an error, so
// callers can enforce output limits the same way vlib/net/http does.
// Callers that need concatenated streams, metadata comments, or custom
// dictionaries should use decompress() directly.
pub fn decompress_with_callback(data []u8, cb DecompressCallback, userdata voidptr) !int {
	decoded := decompress(data)!
	mut delivered := 0
	mut offset := 0
	for offset < decoded.len {
		mut end := offset + callback_chunk_size
		if end > decoded.len {
			end = decoded.len
		}
		chunk := decoded[offset..end]
		if cb(chunk, userdata) != chunk.len {
			return delivered
		}
		delivered += chunk.len
		offset = end
	}
	return delivered
}
