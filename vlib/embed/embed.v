module embed

import os

// https://github.com/vlang/rfcs/blob/master/embedding_resources.md
// EmbeddedData encapsulates functionality for the `$embed_file()` compile time call.
pub struct EmbeddedData {
	path  string
	apath string
mut:
	compressed        byteptr
	uncompressed      byteptr
	free_compressed   bool
	free_uncompressed bool
pub:
	len int
}

pub fn (ed EmbeddedData) str() string {
	return 'embed.EmbeddedData{ len: $ed.len, path: "$ed.path", path: "$ed.apath", uncompressed: ${ptr_str(ed.uncompressed)} }'
}

[unsafe]
pub fn (mut ed EmbeddedData) free() {
	unsafe {
		ed.path.free()
		ed.apath.free()
		if ed.free_compressed {
			free(ed.compressed)
		}
		if ed.free_uncompressed {
			free(ed.uncompressed)
		}
	}
}

pub fn (mut ed EmbeddedData) data() byteptr {
	if !isnil(ed.uncompressed) {
		return ed.uncompressed
	} else {
		if isnil(ed.uncompressed) && !isnil(ed.compressed) {
			// TODO implement uncompression
			// See also C Gen.gen_embedded_data() where the compression should occur.
			ed.uncompressed = ed.compressed
		} else {
			mut path := os.resource_abs_path(ed.path)
			if !os.is_file(path) {
				path = ed.apath
				if !os.is_file(path) {
					panic('EmbeddedData error: files "$ed.path" and "$ed.apath" do not exist')
				}
			}
			bytes := os.read_bytes(path) or {
				panic('EmbeddedData error: "$path" could not be read: $err')
			}
			ed.uncompressed = bytes.data
			ed.free_uncompressed = true
		}
	}
	return ed.uncompressed
}
