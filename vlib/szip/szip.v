module szip

import os

#flag -I @VROOT/thirdparty/zip
#include "zip.c"
#include "zip.h"

struct C.zip_t {
}

type Zip = C.zip_t

fn C.zip_open(byteptr, int, byte) &Zip

fn C.zip_close(&Zip)

fn C.zip_entry_open(&Zip, byteptr) int

fn C.zip_entry_close(&Zip) int

fn C.zip_entry_name(&Zip) byteptr

fn C.zip_entry_index(&Zip) int

fn C.zip_entry_isdir(&Zip) int

fn C.zip_entry_size(&Zip) u64

fn C.zip_entry_crc32(&Zip) u32

fn C.zip_entry_write(&Zip, voidptr, int) int

fn C.zip_entry_fwrite(&Zip, byteptr) int

fn C.zip_entry_read(&Zip, byteptr, int) int

fn C.zip_entry_fread(&Zip, byteptr) int

fn C.zip_total_entries(&Zip) int

// CompressionLevel lists compression levels, see in "thirdparty/zip/miniz.h"
pub enum CompressionLevel {
	no_compression = 0
	best_speed = 1
	best_compression = 9
	uber_compression = 10
	default_level = 6
	default_compression = -1
}

// OpenMode lists the opening modes
// .write: opens a file for reading/extracting (the file must exists).
// .read_only: creates an empty file for writing.
// .append: appends to an existing archive.
pub enum OpenMode {
	write
	read_only
	append
}

[inline]
fn (om OpenMode) to_byte() byte {
	return match om {
		.write {
			`w`
		}
		.read_only {
			`r`
		}
		.append {
			`a`
		}
	}
}

// open opens zip archive with compression level using the given mode.
// name: the name of the zip file to open.
// level: can be any value of the CompressionLevel enum.
// mode: can be any value of the OpenMode enum.
pub fn open(name string, level CompressionLevel, mode OpenMode) ?&Zip {
	if name.len == 0 {
		return error('szip: name of file empty')
	}
	p_zip := &Zip(C.zip_open(name.str, int(level), mode.to_byte()))
	if isnil(p_zip) {
		return error('szip: cannot open/create/append new zip archive')
	}
	return p_zip
}

// close closes the zip archive, releases resources - always finalize.
[inline]
pub fn (mut z Zip) close() {
	C.zip_close(z)
}

// open_entry opens an entry by name in the zip archive.
// For zip archive opened in 'w' or 'a' mode the function will append
// a new entry. In readonly mode the function tries to locate the entry
// in global dictionary.
pub fn (mut zentry Zip) open_entry(name string) ? {
	res := C.zip_entry_open(zentry, name.str)
	if res == -1 {
		return error('szip: cannot open archive entry')
	}
}

// close_entry closes a zip entry, flushes buffer and releases resources.
[inline]
pub fn (mut zentry Zip) close_entry() {
	C.zip_entry_close(zentry)
}

// name returns a local name of the current zip entry.
// The main difference between user's entry name and local entry name
// is optional relative path.
// Following .ZIP File Format Specification - the path stored MUST not contain
// a drive or device letter, or a leading slash.
// All slashes MUST be forward slashes '/' as opposed to backwards slashes '\'
// for compatibility with Amiga and UNIX file systems etc.
pub fn (mut zentry Zip) name() string {
	name := C.zip_entry_name(zentry)
	if name == 0 {
		return ''
	}
	return unsafe { name.vstring() }
}

// index returns an index of the current zip entry.
pub fn (mut zentry Zip) index() ?int {
	index := int(C.zip_entry_index(zentry))
	if index == -1 {
		return error('szip: cannot get current index of zip entry')
	}
	return index // must be check for INVALID_VALUE
}

// is_dir determines if the current zip entry is a directory entry.
pub fn (mut zentry Zip) is_dir() ?bool {
	isdir := C.zip_entry_isdir(zentry)
	if isdir < 0 {
		return error('szip: cannot check entry type')
	}
	return isdir == 1
}

// size returns an uncompressed size of the current zip entry.
[inline]
pub fn (mut zentry Zip) size() u64 {
	return C.zip_entry_size(zentry)
}

// crc32 returns CRC-32 checksum of the current zip entry.
[inline]
pub fn (mut zentry Zip) crc32() u32 {
	return C.zip_entry_crc32(zentry)
}

// write_entry compresses an input buffer for the current zip entry.
pub fn (mut zentry Zip) write_entry(data []byte) ? {
	if (data[0] & 0xff) == -1 {
		return error('szip: cannot write entry')
	}
	res := C.zip_entry_write(zentry, data.data, data.len)
	if res != 0 {
		return error('szip: failed to write entry')
	}
}

// create_entry compresses a file for the current zip entry.
pub fn (mut zentry Zip) create_entry(name string) ? {
	res := C.zip_entry_fwrite(zentry, name.str)
	if res != 0 {
		return error('szip: failed to create entry')
	}
}

// read_entry extracts the current zip entry into output buffer.
// The function allocates sufficient memory for an output buffer.
// NOTE: remember to release the memory allocated for an output buffer.
// for large entries, please take a look at zip_entry_extract function.
pub fn (mut zentry Zip) read_entry() ?voidptr {
	mut buf := voidptr(0)
	mut bsize := i64(0)
	res := C.zip_entry_read(zentry, &buf, &bsize)
	if res == -1 {
		return error('szip: cannot read properly data from entry')
	}
	return buf
}

// extract_entry extracts the current zip entry into output file.
pub fn (mut zentry Zip) extract_entry(path string) ? {
	if !os.is_file(path) {
		return error('szip: cannot open file for extracting, "$path" not exists')
	}
	res := C.zip_entry_fread(zentry, path.str)
	if res != 0 {
		return error('szip: failed to extract entry')
	}
}

/*
extract extracts the current zip entry using a callback function (on_extract).
fn (mut zentry Zip) extract(path string) bool {
	if C.access(path.str, 0) == -1 {
		return false
		// return error('szip: cannot open directory for extracting, directory not exists')
	}
	res := C.zip_extract(zentry, path.str, 0, 0)
	return res == 0
}
*/

// total returns the number of all entries (files and directories) in the zip archive.
pub fn (mut zentry Zip) total() ?int {
	tentry := int(C.zip_total_entries(zentry))
	if tentry == -1 {
		return error('szip: cannot count total entries')
	}
	return tentry
}
