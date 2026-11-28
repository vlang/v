module audio

import encoding.vorbis

// load_ogg decodes an OGG/Vorbis file into a Buffer using the bundled
// stb_vorbis decoder from vlib/encoding/vorbis. Only 16-bit PCM is produced.
pub fn load_ogg(path string) !Buffer {
	song := vorbis.decode_file(path) or { return error('audio: cannot decode ogg "${path}": ${err}') }
	mut samples := []i16{len: song.sample_len}
	unsafe {
		for i in 0 .. song.sample_len {
			samples[i] = song.data[i]
		}
		song.free()
	}
	return Buffer{samples, int(song.channels), int(song.sample_rate), 0}
}

// decode_ogg_memory decodes an OGG/Vorbis byte block into a Buffer.
pub fn decode_ogg_memory(data []u8) !Buffer {
	if data.len == 0 {
		return error('audio: empty ogg block')
	}
	song := vorbis.decode_memory(data.data, i32(data.len)) or {
		return error('audio: cannot decode ogg memory: ${err}')
	}
	mut samples := []i16{len: song.sample_len}
	unsafe {
		for i in 0 .. song.sample_len {
			samples[i] = song.data[i]
		}
		song.free()
	}
	return Buffer{samples, int(song.channels), int(song.sample_rate), 0}
}

// load_compressed loads a WAV or OGG file based on its extension.
pub fn load_compressed(path string) !Buffer {
	if path.ends_with('.ogg') || path.ends_with('.oga') {
		return load_ogg(path)
	}
	return load_wav(path)
}
