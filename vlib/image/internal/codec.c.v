module internal

import image
import image.color
import io
import os
import stbi

#include "stb_image_write.h"

fn C.stbi_write_png_to_func(fn (voidptr, voidptr, i32), voidptr, i32, i32, i32, voidptr, i32) i32
fn C.stbi_write_jpg_to_func(fn (voidptr, voidptr, i32), voidptr, i32, i32, i32, voidptr, i32) i32

struct EncodedImage {
mut:
	bytes []u8
}

fn collect_encoded(context voidptr, data voidptr, size i32) {
	// stb invokes this synchronously while the context and byte span are valid.
	mut encoded := unsafe { &EncodedImage(context) }
	encoded.bytes << unsafe { (&u8(data)).vbytes(int(size)) }
}

fn read_encoded(reader io.Reader, magic string) ![]u8 {
	mut r := reader
	mut bytes := []u8{}
	mut chunk := []u8{len: 16 * 1024}
	for {
		n := r.read(mut chunk) or {
			if err is io.Eof || err is os.Eof {
				break
			}
			return err
		}
		if n == 0 {
			break
		}
		if n < 0 || n > chunk.len {
			return error('image: invalid reader byte count')
		}
		if bytes.len > max_i32 - n {
			return error('image: encoded image is too large')
		}
		bytes << chunk[..n]
	}
	if bytes.len < magic.len || bytes[..magic.len].bytestr() != magic {
		return error('image: invalid image signature')
	}
	if magic == '\x89PNG\r\n\x1a\n' {
		reject_cgbi(bytes)!
	}
	return bytes
}

fn reject_cgbi(bytes []u8) ! {
	mut offset := 8
	for offset <= bytes.len - 12 {
		length := (u32(bytes[offset]) << 24) | (u32(bytes[offset + 1]) << 16) | (u32(bytes[offset + 2]) << 8) | u32(bytes[offset + 3])
		// Check the complete chunk before converting its unsigned length or advancing.
		if length > u32(bytes.len - offset - 12) {
			return
		}
		chunk_type := bytes[offset + 4..offset + 8].bytestr()
		if chunk_type == 'CgBI' {
			// stb can return premultiplied BGRA for this Apple extension.
			return error('image: Apple CgBI PNG is not supported')
		}
		if chunk_type == 'IEND' {
			return
		}
		offset += int(length) + 12
	}
}

fn checked_info(bytes []u8) !stbi.ImageInfo {
	info := stbi.info_from_memory(bytes.data, bytes.len)!
	if info.width <= 0 || info.height <= 0 || info.width > max_i32 / 4 / info.height {
		return error('image: invalid or oversized image dimensions')
	}
	return info
}

// decode reads an eight-bit PNG or JPEG into an independent NRGBA pixel buffer.
pub fn decode(reader io.Reader, magic string) !image.Image {
	bytes := read_encoded(reader, magic)!
	_ := checked_info(bytes)!
	loaded := stbi.load_from_memory(bytes.data, bytes.len)!
	defer {
		loaded.free()
	}
	mut decoded := image.new_nrgba(image.rect(0, 0, loaded.width, loaded.height))
	// The decoded C buffer remains alive until the deferred free above.
	copy(mut decoded.pix, unsafe { loaded.data.vbytes(decoded.pix.len) })
	return decoded
}

// decode_config reads a PNG or JPEG header without decompressing its pixels.
pub fn decode_config(reader io.Reader, magic string) !image.Config {
	bytes := read_encoded(reader, magic)!
	info := checked_info(bytes)!
	return image.Config{
		color_model: color.nrgba_model
		width:       info.width
		height:      info.height
	}
}

// encode converts an image to PNG, or JPEG with the given quality when jpeg is true.
pub fn encode(img image.Image, jpeg bool, quality int) ![]u8 {
	bounds := img.bounds()
	w := bounds.dx()
	h := bounds.dy()
	if w <= 0 || h <= 0 || w > max_i32 / 4 / h {
		return error('image: invalid or oversized image dimensions')
	}
	// stb stores one PNG filter byte per row in addition to the RGBA pixels.
	if !jpeg && w > (max_i32 / h - 1) / 4 {
		return error('image: invalid or oversized image dimensions')
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
	if jpeg {
		mut encoded := EncodedImage{}
		ok := C.stbi_write_jpg_to_func(collect_encoded, &encoded, i32(w), i32(h), 4, pixels.data,
			i32(quality))
			i32(quality))
		if ok == 0 {
			return error('image: encoding failed')
		}
		return encoded.bytes
	}
	return encode_png(img)
}

// write_encoded writes every encoded byte, including when the writer accepts partial writes.
pub fn write_encoded(mut writer io.Writer, bytes []u8) ! {
	mut offset := 0
	for offset < bytes.len {
		n := writer.write(bytes[offset..])!
		if n <= 0 || n > bytes.len - offset {
			return error('image: invalid writer byte count')
		}
		offset += n
	}
}
