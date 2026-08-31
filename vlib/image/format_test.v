module image

import image.color
import io

struct BytesReader {
	data []u8
mut:
	pos int
}

fn (mut r BytesReader) read(mut buf []u8) !int {
	if r.pos >= r.data.len {
		return io.Eof{}
	}
	n := copy(mut buf, r.data[r.pos..])
	r.pos += n
	return n
}

fn fake_decode(mut r PeekReader) !Image {
	mut header := []u8{len: 5}
	n := r.read(mut header)!
	assert n == 5
	assert header.bytestr() == 'VIMG1'
	mut img := new_rgba(rect(0, 0, 1, 1))
	img.set_rgba(0, 0, color.RGBA{
		r: 10
		g: 20
		b: 30
		a: 255
	})
	return img
}

fn fake_decode_config(mut r PeekReader) !Config {
	mut header := []u8{len: 5}
	n := r.read(mut header)!
	assert n == 5
	assert header.bytestr() == 'VIMG1'
	return Config{
		color_model: color.rgba_model
		width:       1
		height:      1
	}
}

fn test_match_with_wildcard() {
	assert match_magic('V?MG', 'VIMG'.bytes())
	assert match_magic('V?MG', 'VXMG'.bytes())
	assert !match_magic('V?MG', 'VIM'.bytes())
	assert !match_magic('V?MG', 'WIMG'.bytes())
}

fn test_register_decode_and_decode_config() {
	register_format('vtest', 'VIMG?', fake_decode, fake_decode_config)

	mut reader := BytesReader{
		data: 'VIMG1payload'.bytes()
	}
	img, name := decode(reader)!
	assert name == 'vtest'
	assert img.bounds() == rect(0, 0, 1, 1)
	assert img.at(0, 0) == color.Color(color.RGBA{
		r: 10
		g: 20
		b: 30
		a: 255
	})

	mut config_reader := BytesReader{
		data: 'VIMG1payload'.bytes()
	}
	config, config_name := decode_config(config_reader)!
	assert config_name == 'vtest'
	assert config.width == 1
	assert config.height == 1

	mut unknown_reader := BytesReader{
		data: 'NOPE'.bytes()
	}
	if _, _ := decode(unknown_reader) {
		assert false, 'decode should reject unknown formats'
	} else {
		assert err.msg() == err_format
	}
}
