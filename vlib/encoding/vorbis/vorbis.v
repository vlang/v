// Copyright (c) 2025 Delyan Angelov. All rights reserved. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.
module vorbis

// DecodedSong contains the information about a fully decoded song, that was loaded either by decode_file/1 or by decode_memory/1 .
pub struct DecodedSong {
pub mut:
	path        string
	channels    i32
	sample_rate i32
	sample_len  i32
	data        &i16 = unsafe { nil }
}

// free frees all the resources allocated for the decoded song
pub fn (mut song DecodedSong) free() {
	unsafe {
		C.free(song.data)
		song.data = nil
		song.path.free()
		song.path.str = nil
	}
}

// decode_file completely decodes the given file from `path` in memory.
// NOTE: for bigger songs, the memory usage may be disproportionate, since vorbis/ogg songs
// are usually highly compressed with a lossy encoder.
pub fn decode_file(path string) !DecodedSong {
	mut res := DecodedSong{
		path: path
	}
	unsafe {
		size := C.stb_vorbis_decode_filename(&char(res.path.str), &res.channels, &res.sample_rate,
			&res.data)
		if size == -1 {
			return error('could not decode ogg file')
		}
		res.sample_len = size
	}
	return res
}

// decode_memory completely decodes the given encoded memory block that is pointed by `ptr`.
// NOTE: for bigger songs, the memory usage may be disproportionate. Consider using decoding while
// playing instead, which can conserve the used RAM, at the cost of slightly higher CPU usage.
pub fn decode_memory(ptr &u8, len i32) !DecodedSong {
	mut res := DecodedSong{
		path: ':memory:'
	}
	unsafe {
		size := C.stb_vorbis_decode_memory(ptr, len, &res.channels, &res.sample_rate,
			&res.data)
		if size == -1 {
			return error('could not decode ogg/vorbis memory block')
		}
		res.sample_len = size
	}
	return res
}
