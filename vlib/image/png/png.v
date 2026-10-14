module png

import image
import image.internal
import io

const signature = '\x89PNG\r\n\x1a\n'

fn init() {
	image.register_format('png', signature, decode_registered, decode_config_registered)
}

fn decode_registered(mut reader image.PeekReader) !image.Image {
	return decode(reader)
}

fn decode_config_registered(mut reader image.PeekReader) !image.Config {
	return decode_config(reader)
}

// decode reads a PNG into an eight-bit, non-premultiplied NRGBA image.
pub fn decode(reader io.Reader) !image.Image {
	return internal.decode(reader, signature)
}

// decode_config reads the color model and dimensions without decompressing pixels.
pub fn decode_config(reader io.Reader) !image.Config {
	return internal.decode_config(reader, signature)
}

// encode_to_bytes encodes img as a lossless, eight-bit RGBA PNG.
pub fn encode_to_bytes(img image.Image) ![]u8 {
	return internal.encode(img, false, 0)
}

// encode writes img as a lossless, eight-bit RGBA PNG to writer.
pub fn encode(mut writer io.Writer, img image.Image) ! {
	internal.write_encoded(mut writer, encode_to_bytes(img)!)!
}
