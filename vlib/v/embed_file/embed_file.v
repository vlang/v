module embed_file

// EmbedFileChunk is one piece of an embedded file. C only guarantees that an
// implementation accepts 65535 bytes in a single object, so a payload past that
// is emitted as several of these, and join_chunks puts them back together.
//
// A table of them ends with a zero length entry. If that entry still carries a
// pointer, it is not the end but a link to the next table, which is how a table
// stays an acceptable size itself.
pub struct EmbedFileChunk {
pub:
	data &u8 = unsafe { nil }
	len  int
}

// join_chunks_buffer reserves the memory a joined payload lives in.
//
// Under `-prealloc` it deliberately does not come from the preallocator. The
// buffer is filled at the top of `_vinit`, before `prealloc_vinit()` has
// installed the first arena, so allocating it there would create an arena that
// the installation then orphans, throwing off both the statistics and the
// cleanup. It is also the wrong home for it either way: the buffer lives for the
// whole program, while the preallocator is reset.
@[unsafe]
fn join_chunks_buffer(size int) &u8 {
	$if prealloc {
		unsafe {
			buffer := join_chunks_raw_alloc(size)
			if buffer == nil {
				panic('EmbedFileData error: could not reserve ${size} bytes for a joined payload')
			}
			return buffer
		}
	} $else {
		return unsafe { &u8(malloc(isize(size))) }
	}
}

// join_chunks copies a payload that was embedded in pieces into one buffer, and
// is called by generated code while the `EmbedFileData` is being built, not when
// its bytes are first read. Materializing it there and not later is what keeps
// `data()` free of the lazy mutation that two threads reading the same embedded
// constant would race on.
//
// The buffer lives as long as the program, like the static bytes it replaces.
@[markused]
pub fn join_chunks(chunks &EmbedFileChunk, len int) &u8 {
	unsafe {
		buffer := join_chunks_buffer(if len > 0 { len } else { 1 })
		mut offset := 0
		mut chunk := chunks
		for offset < len {
			if chunk.len == 0 {
				if chunk.data == nil {
					break
				}
				// A link to the table that continues this one.
				chunk = &EmbedFileChunk(chunk.data)
				continue
			}
			mut n := chunk.len
			if offset + n > len {
				n = len - offset
			}
			vmemcpy(buffer + offset, chunk.data, isize(n))
			offset += n
			chunk++
		}
		return buffer
	}
}

// EmbedFileData encapsulates functionality for the `$embed_file()` compile time call.
pub struct EmbedFileData {
	apath            string
	compression_type string
mut:
	compressed        &u8 = unsafe { nil }
	compressed_len    int
	uncompressed      &u8 = unsafe { nil }
	free_compressed   bool
	free_uncompressed bool
pub:
	len  int
	path string
}

pub fn (ed EmbedFileData) str() string {
	return 'embed_file.EmbedFileData{ len: ${ed.len}, path: "${ed.path}", apath: "${ed.apath}", uncompressed: ${ptr_str(ed.uncompressed)} }'
}

@[unsafe]
pub fn (mut ed EmbedFileData) free() {
	unsafe {
		ed.path.free()
		ed.apath.free()
		ed.compression_type.free()
		if ed.free_compressed {
			free(ed.compressed)
			ed.compressed = &u8(nil)
		}
		if ed.free_uncompressed {
			free(ed.uncompressed)
			ed.uncompressed = &u8(nil)
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
			panic('EmbedFileData error: unknown compression of "${ed.path}": "${ed.compression_type}"')
		}
		compressed := unsafe { ed.compressed.vbytes(ed.compressed_len) }
		decompressed := decoder.decompress(compressed) or {
			panic('EmbedFileData error: decompression of "${ed.path}" failed: ${err}')
		}
		unsafe {
			ed.uncompressed = &u8(memdup(decompressed.data, ed.len))
		}
	} else {
		$if !freestanding {
			reload_from_file_at_runtime(mut ed)
		}
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
@[markused]
pub fn find_index_entry_by_path(start voidptr, path string, algo string) &EmbedFileIndexEntry {
	unsafe {
		mut x := &EmbedFileIndexEntry(start)
		for x.id >= 0 && x.data != 0 && (x.algo != algo || x.path != path) {
			x++
		}
		$if trace_embed_file ? {
			eprintln('>> v.embed_file find_index_entry_by_path ${ptr_str(start)}, id: ${x.id}, path: "${path}", algo: "${algo}" => ${ptr_str(x)}')
		}
		return x
	}
}
