module embed_file

import os

pub const (
	is_used = 1
)

// https://github.com/vlang/rfcs/blob/master/embedding_resources.md
// EmbedFileData encapsulates functionality for the `$embed_file()` compile time call.
pub struct EmbedFileData {
	apath            string
	compression_type string
mut:
	compressed        &u8 = unsafe { nil }
	uncompressed      &u8 = unsafe { nil }
	free_compressed   bool
	free_uncompressed bool
pub:
	len  int
	path string
}

pub fn (ed EmbedFileData) str() string {
	return 'embed_file.EmbedFileData{ len: $ed.len, path: "$ed.path", apath: "$ed.apath", uncompressed: ${ptr_str(ed.uncompressed)} }'
}

[unsafe]
pub fn (mut ed EmbedFileData) free() {
	unsafe {
		ed.path.free()
		ed.apath.free()
		ed.compression_type.free()
		if ed.free_compressed {
			free(ed.compressed)
			ed.compressed = &u8(0)
		}
		if ed.free_uncompressed {
			free(ed.uncompressed)
			ed.uncompressed = &u8(0)
		}
	}
}

pub fn (original &EmbedFileData) to_string() string {
	unsafe {
		mut ed := &EmbedFileData(original)
		the_copy := &u8(memdup(ed.data(), ed.len))
		return the_copy.vstring_with_len(ed.len)
	}
}

pub fn (original &EmbedFileData) to_bytes() []u8 {
	unsafe {
		mut ed := &EmbedFileData(original)
		the_copy := memdup(ed.data(), ed.len)
		return the_copy.vbytes(ed.len)
	}
}

pub fn (mut ed EmbedFileData) data() &u8 {
	if ed.uncompressed != unsafe { nil } {
		return ed.uncompressed
	}
	if ed.uncompressed == unsafe { nil } && ed.compressed != unsafe { nil } {
		decoder := g_embed_file_decoders.decoders[ed.compression_type] or {
			panic('EmbedFileData error: unknown compression of "$ed.path": "$ed.compression_type"')
		}
		compressed := unsafe { ed.compressed.vbytes(ed.len) }
		decompressed := decoder.decompress(compressed) or {
			panic('EmbedFileData error: decompression of "$ed.path" failed: $err')
		}
		unsafe {
			ed.uncompressed = &u8(memdup(decompressed.data, ed.len))
		}
	} else {
		mut path := os.resource_abs_path(ed.path)
		if !os.is_file(path) {
			path = ed.apath
			if !os.is_file(path) {
				panic('EmbedFileData error: files "$ed.path" and "$ed.apath" do not exist')
			}
		}
		bytes := os.read_bytes(path) or {
			panic('EmbedFileData error: "$path" could not be read: $err')
		}
		ed.uncompressed = bytes.data
		ed.free_uncompressed = true
	}
	return ed.uncompressed
}

//////////////////////////////////////////////////////////////////////////////
// EmbedFileIndexEntry is used internally by the V compiler when you compile a
// program that uses $embed_file('file.bin') in -prod mode.
// V will generate a static index of all embedded files, and will call the
// find_index_entry_by_path over the index and the relative paths of the embeds.
// Note: these are public on purpose, to help -usecache.
pub struct EmbedFileIndexEntry {
	id   int
	path string
	algo string
	data &u8 = unsafe { nil }
}

// find_index_entry_by_path is used internally by the V compiler:
pub fn find_index_entry_by_path(start voidptr, path string, algo string) &EmbedFileIndexEntry {
	mut x := &EmbedFileIndexEntry(start)
	for x.id >= 0 && x.data != 0 && (x.algo != algo || x.path != path) {
		unsafe {
			x++
		}
	}
	$if trace_embed_file ? {
		eprintln('>> v.embed_file find_index_entry_by_path ${ptr_str(start)}, id: $x.id, path: "$path", algo: "$algo" => ${ptr_str(x)}')
	}
	return x
}
