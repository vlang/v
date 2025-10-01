// Copyright (c) 2025 Delyan Angelov. All rights reserved. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.
module vorbis

pub struct VorbisData {
pub mut:
	path        string
	channels    i32
	sample_rate i32
	len         i32
	data        &i16 = unsafe { nil }
}

pub fn (mut vd VorbisData) free() {
	unsafe {
		C.free(vd.data)
		vd.data = nil
		vd.path.free()
		vd.path.str = nil
	}
}

pub fn decode_file(path string) !VorbisData {
	mut res := VorbisData{
		path: path
	}
	unsafe {
		size := C.stb_vorbis_decode_filename(&char(res.path.str), &res.channels, &res.sample_rate,
			&res.data)
		if size == -1 {
			return error('could not decode ogg file')
		}
		res.len = size
	}
	return res
}

pub fn decode_memory(ptr &u8, len i32) !VorbisData {
	mut res := VorbisData{
		path: ':memory:'
	}
	unsafe {
		size := C.stb_vorbis_decode_memory(ptr, len, &res.channels, &res.sample_rate,
			&res.data)
		if size == -1 {
			return error('could not decode ogg/vorbis memory block')
		}
		res.len = size
	}
	return res
}
