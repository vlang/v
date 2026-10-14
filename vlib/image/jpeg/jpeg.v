module jpeg

import image
import image.internal
import io

const signature = '\xff\xd8\xff'

// Options selects JPEG encoding quality from 1 (smallest) to 100 (highest).
@[params]
pub struct Options {
pub:
	quality int = 75
}

fn init() {
	image.register_format('jpeg', signature, decode_registered, decode_config_registered)
}

fn decode_registered(mut reader image.PeekReader) !image.Image {
	return decode(reader)
}

fn decode_config_registered(mut reader image.PeekReader) !image.Config {
	return decode_config(reader)
}

// decode reads a JPEG into an eight-bit, non-premultiplied NRGBA image.
pub fn decode(reader io.Reader) !image.Image {
	return internal.decode(reader, signature)
}

// decode_config reads the color model and dimensions without decompressing pixels.
pub fn decode_config(reader io.Reader) !image.Config {
	return internal.decode_config(reader, signature)
}

// encode_to_bytes encodes img as a JPEG; alpha is discarded.
pub fn encode_to_bytes(img image.Image, options Options) ![]u8 {
	if options.quality < 1 || options.quality > 100 {
		return error('jpeg: quality must be between 1 and 100')
	}
	return internal.encode(img, true, options.quality)
}

// encode writes img as a JPEG to writer; alpha is discarded.
pub fn encode(mut writer io.Writer, img image.Image, options Options) ! {
	internal.write_encoded(mut writer, encode_to_bytes(img, options)!)!
}
