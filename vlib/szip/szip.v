module szip

import os

#flag -I @VEXEROOT/thirdparty/zip
#include "zip.c"
#include "zip.h"

struct C.zip_t {
}

type Zip = C.zip_t

fn C.zip_open(&char, int, char) &Zip

fn C.zip_close(&Zip)

fn C.zip_entry_open(&Zip, &u8) int

fn C.zip_entry_openbyindex(&Zip, int) int

fn C.zip_entry_close(&Zip) int

fn C.zip_entry_name(&Zip) &u8

fn C.zip_entry_index(&Zip) int

fn C.zip_entry_isdir(&Zip) int

fn C.zip_entry_size(&Zip) u64

fn C.zip_entry_crc32(&Zip) u32

fn C.zip_entry_write(&Zip, voidptr, usize) int

fn C.zip_entry_fwrite(&Zip, &char) int

fn C.zip_entry_read(&Zip, &voidptr, &usize) int

fn C.zip_entry_noallocread(&Zip, voidptr, usize) int

fn C.zip_entry_fread(&Zip, &char) int

fn C.zip_total_entries(&Zip) int

fn C.zip_extract_without_callback(&char, &char) int

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
fn (om OpenMode) to_u8() u8 {
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
	p_zip := unsafe { &Zip(C.zip_open(&char(name.str), int(level), char(mode.to_u8()))) }
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
	res := C.zip_entry_open(zentry, &char(name.str))
	if res == -1 {
		return error('szip: cannot open archive entry')
	}
}

// open_entry_by_index opens an entry by index in the archive.
pub fn (mut z Zip) open_entry_by_index(index int) ? {
	res := C.zip_entry_openbyindex(z, index)
	if res == -1 {
		return error('szip: cannot open archive entry at index $index')
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
	name := unsafe { &u8(C.zip_entry_name(zentry)) }
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
pub fn (mut zentry Zip) write_entry(data []u8) ? {
	if int(data[0] & 0xff) == -1 {
		return error('szip: cannot write entry')
	}
	res := C.zip_entry_write(zentry, data.data, data.len)
	if res != 0 {
		return error('szip: failed to write entry')
	}
}

// create_entry compresses a file for the current zip entry.
pub fn (mut zentry Zip) create_entry(name string) ? {
	res := C.zip_entry_fwrite(zentry, &char(name.str))
	if res != 0 {
		return error('szip: failed to create entry')
	}
}

// read_entry extracts the current zip entry into output buffer.
// The function allocates sufficient memory for an output buffer.
// NOTE: remember to release the memory allocated for an output buffer.
// for large entries, please take a look at zip_entry_extract function.
pub fn (mut zentry Zip) read_entry() ?voidptr {
	mut buf := &u8(0)
	mut bsize := usize(0)
	res := C.zip_entry_read(zentry, unsafe { &voidptr(&buf) }, &bsize)
	if res == -1 {
		return error('szip: cannot read properly data from entry')
	}
	return buf
}

// read_entry_buf extracts the current zip entry into user specified buffer
pub fn (mut zentry Zip) read_entry_buf(buf voidptr, in_bsize int) ?int {
	bsize := usize(in_bsize)
	res := C.zip_entry_noallocread(zentry, buf, bsize)
	if res == -1 {
		return error('szip: cannot read properly data from entry')
	}
	return res
}

// extract_entry extracts the current zip entry into output file.
pub fn (mut zentry Zip) extract_entry(path string) ? {
	if !os.is_file(path) {
		return error('szip: cannot open file for extracting, "$path" not exists')
	}
	res := C.zip_entry_fread(zentry, &char(path.str))
	if res != 0 {
		return error('szip: failed to extract entry')
	}
}

// extract zip file to directory
pub fn extract_zip_to_dir(file string, dir string) ?bool {
	if C.access(&char(dir.str), 0) == -1 {
		return error('szip: cannot open directory for extracting, directory not exists')
	}
	res := C.zip_extract_without_callback(&char(file.str), &char(dir.str))
	return res == 0
}

// zip files (full path) to zip file
pub fn zip_files(path_to_file []string, path_to_export_zip string) ? {
	// open or create new zip
	mut zip := open(path_to_export_zip, .no_compression, .write) or { panic(err) }

	// add all files from the directory to the archive
	for file in path_to_file {
		// add file to zip
		zip.open_entry(os.base(file)) or { panic(err) }
		file_as_byte := os.read_bytes(file) or { panic(err) }
		zip.write_entry(file_as_byte) or { panic(err) }

		zip.close_entry()
	}

	// close zip
	defer {
		zip.close()
	}
}

/*
TODO add
// zip all files in directory to zip file
pub fn zip_folder(path_to_dir string, path_to_export_zip string) {

	// get list files from directory
	files := os.ls(path_to_dir) or { panic(err) }

	// open or create new zip
	mut zip := szip.open(path_to_export_zip, .no_compression, .write) or { panic(err) }

	// add all files from the directory to the archive
	for file in files {
		eprintln('Zipping $file to ${path_to_export_zip}...')
		println(path_to_dir + file)

		// add file to zip
		zip.open_entry(file) or { panic(err) }
		file_as_byte := os.read_bytes(path_to_dir + '/'+ file) or { panic(err) }
        zip.write_entry(file_as_byte) or { panic(err) }

        zip.close_entry()
	}

	// close zip
	zip.close()

	eprintln('Successfully')
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
