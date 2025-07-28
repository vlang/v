module tar

import compress.gzip
import os

// read_tar_gz_file decompresses a given local file and reads all the blocks
// with a given reader.
pub fn read_tar_gz_file(path string, reader Reader) ! {
	tar_gz := os.read_bytes(path) or { return error('read bytes error: ${err}') }
	all_blocks := gzip.decompress(tar_gz) or { return error('gzip decompress error: ${err}') }
	mut untar := Untar{
		reader: reader
	}
	untar.read_all_blocks(all_blocks)!
}

// Read is used by Untar to call Reader implemented methods.
// The implementor can read the block's `get_block_number()` and `get_path()`
// and can set the field `stop_early` to true to suspend the reading.
pub struct Read {
mut:
	block_number int
	special      BlockSpecial
	prefix_len   int
	prefix_buf   [131]u8
	separator    bool
	path_len     int
	path_buf     [100]u8

	long_path &LongPath = unsafe { nil }
pub mut:
	stop_early bool
}

// set_short_path sets Read path with the tar block strings `prefix` and `path`.
// Block's `prefix` C string max length is 131 but most of the time is 0.
// Block's `path` C string max length is 100. Both `prefix` and `path` are
// linked to a V string but converted until is needed, see `get_path()`.
fn (mut b Read) set_short_path(buffer [512]u8, separator_after_prefix bool) {
	// first check if TAR block has a prefix string (0 to 131 chars). The
	// prefix will be other than '' the TAR block filepath len is > 100.
	b.prefix_len = 0
	for i := 345; i < 345 + 131; i++ {
		letter := buffer[i]
		if letter == 0 {
			break // first 0 found means prefix C string is complete.
		}
		b.prefix_buf[b.prefix_len] = letter
		b.prefix_len++
	}

	b.separator = separator_after_prefix

	// most of the time there is path for blocks like dirs and regular files:
	b.path_len = 0
	for i := 0; i < 100; i++ {
		letter := buffer[i]
		if letter == 0 {
			break // first 0 found means path C string is complete.
		}
		b.path_buf[b.path_len] = letter
		b.path_len++
	}
}

// set_long_path sets Read path with the long path reference.
fn (mut b Read) set_long_path(long_path &LongPath) {
	b.long_path = unsafe { long_path }
}

// get_path returns the path of this read. The path is valid for blocks of types
// directory, file and file data.
pub fn (b Read) get_path() string {
	if b.long_path != unsafe { nil } {
		return b.long_path.get_path()
	}

	mut str := []u8{}
	if b.prefix_len > 0 {
		str << b.prefix_buf[0..b.prefix_len]
	}
	if b.prefix_len > 0 && b.separator {
		str << `/`
	}
	if b.path_len > 0 {
		str << b.path_buf[0..b.path_len]
	}
	return str.bytestr()
}

// get_block_number returns the consecutive number of this read.
pub fn (b Read) get_block_number() int {
	return b.block_number
}

pub fn (b Read) get_special() BlockSpecial {
	return b.special
}

pub fn (r Read) str() string {
	return '(block_number:${r.block_number} path:${r.get_path()} special:${r.special} stop_early:${r.stop_early})'
}

// Reader is used to read by Untar to parse the blocks.
pub interface Reader {
mut:
	// dir_block is called when untar reads a block of type directory.
	// Call `Read.get_path()` to get the full name of the directory.
	// `size` field is zero for directories.
	// The implementor can set Read's field `stop_early` to suspend the reader.
	dir_block(mut read Read, size u64)

	// file_block is called when untar reads a block of type filename.
	// Call `Read.get_path()` to get the full name of the file.
	// `size` is the expected file size in bytes to be read later.
	// The implementor can set Read's field `stop_early` to suspend the reader.
	file_block(mut read Read, size u64)

	// file_block is called when untar reads a block of type filedata.
	// Call `Read.get_path()` to get the full name of the file data belongs to.
	// The `data` size is 512 bytes or less. `pending` indicates how many bytes are left to read.
	// The implementor can inspect the data and use the pending value
	// to set Read's field `stop_early` to suspend the reader.
	data_block(mut read Read, data []u8, pending int)

	// other_block is called when untar reads a block type other than directory,
	// filename or filedata. `Read.get_header()` and 'details' give more info about the block.
	// `block device` or `FIFO`.
	// The implementor can set Read's field `stop_early` to suspend the reader.
	other_block(mut read Read, details string)
}

// DebugReader implements a Reader and prints rows for blocks read
// as directories, files, file data blocks and special blocks.
pub struct DebugReader implements Reader {
}

// new_debug_reader returns a DebugReader
pub fn new_debug_reader() &DebugReader {
	return &DebugReader{}
}

fn (mut t DebugReader) dir_block(mut read Read, size u64) {
	println('DIR   #${read.get_block_number()} ${read.get_path()}')
}

fn (mut t DebugReader) file_block(mut read Read, size u64) {
	println('FILE  #${read.get_block_number()} path:${read.get_path()} size:${size}')
}

fn (mut t DebugReader) data_block(mut read Read, data []u8, pending int) {
	println('DATA  #${read.get_block_number()} ${read.get_path()} size:${data.len} pending:${pending}')
}

fn (mut t DebugReader) other_block(mut read Read, details string) {
	println('OTHER #${read.get_block_number()} special:${read.special} ${details}')
}

// ReadResult is returned by ReadResultFn
pub enum ReadResult {
	@continue
	stop_early
	end_of_file
	end_archive
	overflow
}

type ReadResultFn = fn (block []u8) !ReadResult

@[heap]
pub struct Decompressor {
mut:
	untar &Untar
}

// new_decompressor returns a Decompressor to decompress a tar.gz file
// A given Untar with a registered Reader will read the blocks.
pub fn new_decompresor(untar &Untar) &Decompressor {
	return &Decompressor{
		untar: untar
	}
}

// read_all decompresses the given `tar_gz` array with all the tar blocks.
// Then calls untar method `read_all` to read all the blocks at once.
// A read result is returned which can be of the type stop early or an error.
pub fn (mut d Decompressor) read_all(tar_gz []u8) !ReadResult {
	all_blocks := gzip.decompress(tar_gz) or { return error('gzip decompress error: ${err}') }
	return d.untar.read_all_blocks(all_blocks)!
}

// read_chunks decompresses the given `tar_gz` array by chunks of
// 32768 bytes which can hold up to 64 tar blocks of 512 bytes each.
// Then calls untar method read_block with ChunksReader dispatcher.
// A read result is returned which can be of the type stop early or an error.
pub fn (mut d Decompressor) read_chunks(tar_gz []u8) !ReadResult {
	mut reader := &ChunksReader{
		read_block_fn: d.untar.read_single_block
	}
	callback := fn (chunk []u8, mut reader ChunksReader) int {
		result := reader.read_blocks(chunk)
		if result == .continue {
			return chunk.len // go for more
		}
		return 0 // suspend
	}
	gzip.decompress_with_callback(tar_gz, callback, reader) or {
		if reader.result == .continue {
			return error('${err}')
		}
		return reader.result
	}
	return reader.result
}

// ChunkReader has a reusable fixed buffer with maximum length of decompressed chunk
// of 32768 bytes plus a maximum previous pending tar block of 512 bytes.
struct ChunksReader {
mut:
	read_block_fn  ReadResultFn = unsafe { nil }
	buffer         [32768 + 512]u8
	chunks_counter int
	pending        int // position of the last not sent buffer byte
	result         ReadResult
}

// read_blocks receives a chunk like those of 32k from a gzip decompressor. The chunk is
// assumed to be a TAR archive section and is cut in 512 bytes blocks that are sent to
// the untar reader one by one. The untar reader result informs this process to continue or
// stop early. This process can keep in the buffer the remaining bytes of an incomplete
// block and will be send to the untar reader prepended to a next chunk cuts.
fn (mut d ChunksReader) read_blocks(chunk []u8) ReadResult {
	d.chunks_counter++
	total := d.pending + chunk.len
	if total > d.buffer.len {
		eprintln('Should not occur buffer overflow ${total}')
		return .overflow
	}

	// append new chunk after previous incomplete block bytes not sent yet
	for i, ch in chunk {
		d.buffer[i + d.pending] = ch
	}
	d.pending += chunk.len

	mut cut := 0
	for {
		if cut + 512 > d.pending {
			// after sending all complete blocks move the remaining not sent bytes
			// to the start of the reused buffer to be prepended before next chunk
			for i := cut; i < d.pending; i++ {
				d.buffer[cut - 512] = d.buffer[i]
			}
			d.pending -= cut
			return .continue
		}

		// send a complete block
		block := d.buffer[cut..cut + 512]
		cut += 512
		d.result = d.read_block_fn(block) or {
			eprintln('Should not occur buffer overflow')
			return .overflow
		}
		match d.result {
			.continue {
				// try next cut or leave a remaining
			}
			else {
				break // untar error or stop_early
			}
		}
	}
	return d.result
}
