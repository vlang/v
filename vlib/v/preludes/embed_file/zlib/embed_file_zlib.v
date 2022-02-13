module zlib

import compress.zlib
import v.embed_file

struct ZLibDecoder {}

fn (_ ZLibDecoder) decompress(data []byte) ?[]byte {
	return zlib.decompress(data)
}

fn init() {
	embed_file.register_decoder('zlib', embed_file.Decoder(ZLibDecoder{}))
}
