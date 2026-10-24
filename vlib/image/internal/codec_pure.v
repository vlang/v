// Pure V PNG encoder (replaces stb_image_write C backend for PNG path)
import hash.crc32
import compress.deflate
import image

fn png_chunk(type_str string, data []u8) []u8 {
	mut chunk := []u8{len: 4 + 4 + data.len + 4}
	n := u32(data.len)
	chunk[0] = u8((n >> 24) & 0xff)
	chunk[1] = u8((n >> 16) & 0xff)
	chunk[2] = u8((n >> 8) & 0xff)
	chunk[3] = u8(n & 0xff)
	for i in 0 .. 4 {
		chunk[4 + i] = u8(type_str[i])
	}
	for i in 0 .. data.len {
		chunk[8 + i] = data[i]
	}
	crc := crc32.sum(chunk[4..8 + data.len])
	chunk[8 + data.len] = u8((crc >> 24) & 0xff)
	chunk[9 + data.len] = u8((crc >> 16) & 0xff)
	chunk[10 + data.len] = u8((crc >> 8) & 0xff)
	chunk[11 + data.len] = u8(crc & 0xff)
	return chunk
}

pub fn encode_png(img image.Image) ![]u8 {
	bounds := img.bounds()
	w := bounds.dx()
	h := bounds.dy()
	if w <= 0 || h <= 0 || w > max_i32 / 4 / h {
		return error('image: invalid dimensions')
	}
	mut pixels := []u8{len: w * h * 4}
	for y in 0 .. h {
		for x in 0 .. w {
			c := color.to_nrgba(img.at(bounds.min.x + x, bounds.min.y + y))
			i := (y * w + x) * 4
			pixels[i] = c.r
			pixels[i + 1] = c.g
			pixels[i + 2] = c.b
			pixels[i + 3] = c.a
		}
	}
	mut out := []u8{}
	// PNG signature
	out << u8(0x89) << u8(0x50) << u8(0x4e) << u8(0x47) << u8(0x0d) << u8(0x0a) << u8(0x1a) << u8(0x0a)
	// IHDR chunk
	mut ihdr := []u8{len: 13}
	ihdr[0] = u8((w >> 24) & 0xff)
	ihdr[1] = u8((w >> 16) & 0xff)
	ihdr[2] = u8((w >> 8) & 0xff)
	ihdr[3] = u8(w & 0xff)
	ihdr[4] = u8((h >> 24) & 0xff)
	ihdr[5] = u8((h >> 16) & 0xff)
	ihdr[6] = u8((h >> 8) & 0xff)
	ihdr[7] = u8(h & 0xff)
	ihdr[8] = 8 // bit depth
	ihdr[9] = 6 // color type RGBA
	ihdr[10] = 0 // compression
	ihdr[11] = 0 // filter method
	ihdr[12] = 0 // interlace
	out << png_chunk('IHDR', ihdr)
	// Image data with filter byte 0 per row
	row_size := w * 4 + 1 // +1 for filter byte
	mut raw := []u8{len: h * row_size}
	for y in 0 .. h {
		raw[y * row_size] = 0 // filter type 0 (none)
		for x in 0 .. w {
			src := (y * w + x) * 4
			dst := y * row_size + 1 + x * 4
			raw[dst] = pixels[src]
			raw[dst + 1] = pixels[src + 1]
			raw[dst + 2] = pixels[src + 2]
			raw[dst + 3] = pixels[src + 3]
		}
	}
	compressed := deflate.deflate_compress_fixed(raw) or { return err }
	out << png_chunk('IDAT', compressed)
	out << png_chunk('IEND', []u8{})
	return out
}
