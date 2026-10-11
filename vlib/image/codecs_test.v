import encoding.hex
import image
import image.color
import image.draw
import image.jpeg
import image.png
import io
import os

struct CodecReader {
	bytes []u8
mut:
	pos int
}

fn (mut r CodecReader) read(mut buf []u8) !int {
	if r.pos >= r.bytes.len {
		return io.Eof{}
	}
	n := copy(mut buf, r.bytes[r.pos..])
	r.pos += n
	return n
}

struct CodecWriter {
mut:
	bytes []u8
}

fn (mut w CodecWriter) write(bytes []u8) !int {
	// Exercise partial writes, which file and network writers may return.
	n := if bytes.len > 7 { 7 } else { bytes.len }
	w.bytes << bytes[..n]
	return n
}

struct FailingCodecReader {
	bytes   []u8
	fail_at int
mut:
	pos    int
	failed bool
}

fn (mut r FailingCodecReader) read(mut buf []u8) !int {
	if r.pos == r.fail_at && !r.failed {
		r.failed = true
		return error('codec reader failure')
	}
	if r.pos >= r.bytes.len {
		return io.Eof{}
	}
	end := if r.pos < r.fail_at { r.fail_at } else { r.bytes.len }
	n := copy(mut buf, r.bytes[r.pos..end])
	r.pos += n
	return n
}

struct FailingCodecWriter {
	zero_count bool
mut:
	calls int
}

fn (mut w FailingCodecWriter) write(_ []u8) !int {
	w.calls++
	if w.calls == 1 {
		return 1
	}
	if w.zero_count {
		return 0
	}
	return error('codec writer failure')
}

struct OversizedPngImage {}

fn (img OversizedPngImage) color_model() color.Model {
	return color.nrgba_model
}

fn (img OversizedPngImage) bounds() image.Rectangle {
	// The pixel buffer would fit an i32, but adding PNG row filter bytes would not.
	return image.rect(0, 0, 1, max_i32 / 4)
}

fn (img OversizedPngImage) at(_ int, _ int) color.Color {
	panic('PNG dimensions must be rejected before accessing pixels')
}

fn test_registered_codecs_propagate_reader_errors() {
	src := image.new_nrgba(image.rect(0, 0, 1, 1))
	bytes := png.encode_to_bytes(src)!
	// Exercise failures during format sniffing and after the buffered signature is consumed.
	for fail_at in [0, 8] {
		if _, _ := image.decode(FailingCodecReader{ bytes: bytes, fail_at: fail_at }) {
			assert false, 'registered decode swallowed a reader error'
		} else {
			assert err.msg() == 'codec reader failure'
		}
		if _, _ := image.decode_config(FailingCodecReader{ bytes: bytes, fail_at: fail_at }) {
			assert false, 'registered decode_config swallowed a reader error'
		} else {
			assert err.msg() == 'codec reader failure'
		}
	}
}

fn test_codecs_decode_files() {
	src := image.new_nrgba(image.rect(0, 0, 1, 1))
	for format in ['png', 'jpeg'] {
		bytes := if format == 'png' {
			png.encode_to_bytes(src)!
		} else {
			jpeg.encode_to_bytes(src)!
		}
		path := os.join_path(os.temp_dir(), 'v_image_codec_${format}_${os.getpid()}')
		os.write_file_array(path, bytes)!
		defer { os.rm(path) or {} }
		mut file := os.open(path)!
		decoded, name := image.decode(file)!
		file.close()
		assert name == format
		assert decoded.bounds() == src.bounds()
		file = os.open(path)!
		config, config_name := image.decode_config(file)!
		file.close()
		assert config_name == format
		assert config.width == 1
		assert config.height == 1
	}
}

fn test_codecs_propagate_writer_errors_and_reject_zero_writes() {
	src := image.new_nrgba(image.rect(0, 0, 1, 1))
	for zero_count in [false, true] {
		mut png_writer := FailingCodecWriter{ zero_count: zero_count }
		if _ := png.encode(mut png_writer, src) {
			assert false, 'PNG encoder swallowed a writer failure'
		} else {
			assert err.msg() == if zero_count {
				'image: invalid writer byte count'
			} else {
				'codec writer failure'
			}
		}
		mut jpeg_writer := FailingCodecWriter{ zero_count: zero_count }
		if _ := jpeg.encode(mut jpeg_writer, src) {
			assert false, 'JPEG encoder swallowed a writer failure'
		} else {
			assert err.msg() == if zero_count {
				'image: invalid writer byte count'
			} else {
				'codec writer failure'
			}
		}
	}
}

fn test_png_rejects_filter_buffer_overflow_before_allocating() {
	if _ := png.encode_to_bytes(OversizedPngImage{}) {
		assert false, 'PNG encoder accepted an overflowing filter buffer'
	} else {
		assert err.msg() == 'image: invalid or oversized image dimensions'
	}
}

fn test_png_external_fixture_and_registration() {
	bytes := hex.decode('89504e470d0a1a0a0000000d49484452000000010000000108060000001f15c4890000000d49444154789c63f8cfc0f01f00050001ff89993d1d0000000049454e44ae426082')!
	img, format := image.decode(CodecReader{ bytes: bytes })!
	assert format == 'png'
	assert img.bounds() == image.rect(0, 0, 1, 1)
	assert color.to_nrgba(img.at(0, 0)) == color.NRGBA{255, 0, 0, 255}
	config, config_format := image.decode_config(CodecReader{ bytes: bytes })!
	assert config_format == 'png'
	assert config.width == 1
	assert config.height == 1
}

fn test_png_rejects_apple_cgbi_images() {
	// Independently encoded premultiplied BGRA fixtures: opaque and half-alpha red.
	for fixture in [
		'89504e470d0a1a0a0000000443674249500020022bd5b37f0000000d49484452000000010000000108060000001f15c4890000000749444154636060f8ff1f0052def1700000000049454e44ae426082',
		'89504e470d0a1a0a0000000443674249500020022bd5b37f0000000d49484452000000010000000108060000001f15c489000000074944415463606068680000da98b03f0000000049454e44ae426082',
	] {
		bytes := hex.decode(fixture)!
		if _ := png.decode(CodecReader{ bytes: bytes }) {
			assert false, 'PNG decoder accepted premultiplied BGRA pixels'
		} else {
			assert err.msg() == 'image: Apple CgBI PNG is not supported'
		}
		if _ := png.decode_config(CodecReader{ bytes: bytes }) {
			assert false, 'PNG configuration accepted an unsupported format'
		} else {
			assert err.msg() == 'image: Apple CgBI PNG is not supported'
		}
		if _, _ := image.decode(CodecReader{ bytes: bytes }) {
			assert false, 'registered PNG decoder accepted premultiplied BGRA pixels'
		} else {
			assert err.msg() == 'image: Apple CgBI PNG is not supported'
		}
		if _, _ := image.decode_config(CodecReader{ bytes: bytes }) {
			assert false, 'registered PNG configuration accepted an unsupported format'
		} else {
			assert err.msg() == 'image: Apple CgBI PNG is not supported'
		}
	}
}

fn test_png_accepts_cgbi_text_in_ordinary_chunk_data() {
	// "CgBI" in ancillary data is not a CgBI chunk. stb does not check chunk CRCs.
	bytes := hex.decode('89504e470d0a1a0a0000000d49484452000000010000000108060000001f15c48900000009744558746e6f74650043674249000000000000000d49444154789c63f8cfc0f01f00050001ff89993d1d0000000049454e44ae426082')!
	img, format := image.decode(CodecReader{ bytes: bytes })!
	assert format == 'png'
	assert color.to_nrgba(img.at(0, 0)) == color.NRGBA{255, 0, 0, 255}
	config := png.decode_config(CodecReader{ bytes: bytes })!
	assert config.width == 1
	assert config.height == 1
}

fn test_png_preserves_alpha_and_source_bounds() {
	mut src := image.new_nrgba(image.rect(5, 7, 7, 8))
	src.set_nrgba(5, 7, color.NRGBA{120, 60, 30, 128})
	src.set_nrgba(6, 7, color.NRGBA{10, 20, 30, 255})
	mut writer := CodecWriter{}
	png.encode(mut writer, src)!
	decoded := png.decode(CodecReader{ bytes: writer.bytes })!
	assert decoded.bounds() == image.rect(0, 0, 2, 1)
	assert color.to_nrgba(decoded.at(0, 0)) == src.nrgba_at(5, 7)
	assert color.to_nrgba(decoded.at(1, 0)) == src.nrgba_at(6, 7)
	config := png.decode_config(CodecReader{ bytes: writer.bytes })!
	assert config.width == 2
	assert config.height == 1
}

fn test_resize_then_jpeg_encode_and_registered_decode() {
	mut src := image.new_nrgba(image.rect(0, 0, 4, 4))
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			src.set_nrgba(x, y, color.NRGBA{240, 30, 10, 255})
		}
	}
	resized := draw.resize(src, 2, 3)!
	mut writer := CodecWriter{}
	jpeg.encode(mut writer, resized, quality: 95)!
	img, format := image.decode(CodecReader{ bytes: writer.bytes })!
	assert format == 'jpeg'
	assert img.bounds() == image.rect(0, 0, 2, 3)
	c := color.to_nrgba(img.at(0, 0))
	assert c.r >= 235 && c.r <= 245
	assert c.g >= 25 && c.g <= 35
	assert c.b >= 5 && c.b <= 15
	assert c.a == 255
	config, name := image.decode_config(CodecReader{ bytes: writer.bytes })!
	assert name == 'jpeg'
	assert config.width == 2
	assert config.height == 3
}

fn test_codecs_reject_wrong_formats_and_invalid_quality() {
	mut src := image.new_rgba(image.rect(0, 0, 1, 1))
	bytes := png.encode_to_bytes(src)!
	if _ := jpeg.decode(CodecReader{ bytes: bytes }) {
		assert false, 'JPEG decoder accepted a PNG'
	}
	if _ := png.decode(CodecReader{ bytes: 'not an image'.bytes() }) {
		assert false, 'PNG decoder accepted an invalid signature'
	}
	for quality in [0, 101] {
		if _ := jpeg.encode_to_bytes(src, quality: quality) {
			assert false, 'accepted invalid JPEG quality'
		}
	}
	if _ := png.encode_to_bytes(image.NRGBA{}) {
		assert false, 'accepted an empty image'
	}
}
