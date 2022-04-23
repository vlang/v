[has_globals]
module embed_file

interface Decoder {
	decompress([]u8) ?[]u8
}

struct EmbedFileDecoders {
mut:
	decoders map[string]Decoder
}

__global g_embed_file_decoders = &EmbedFileDecoders{}

pub fn register_decoder(compression_type string, decoder Decoder) {
	g_embed_file_decoders.decoders[compression_type] = decoder
}
