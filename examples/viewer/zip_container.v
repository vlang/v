/**********************************************************************
*
* Zip archive container support for the image viewer
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module main

// Zip container support for the viewer.
//
// Each zip entry is read into a V `[]u8` and the format-agnostic
// `decoder` module handles it.
//
// Unicode note: the underlying miniz library opens files with the ANSI C
// `fopen`, which fails on Windows when the path contains non-ASCII
// characters (e.g. a user profile like `C:\Users\Łukasz\...`). V's own
// `os.read_bytes` uses the wide-char Windows APIs and handles such paths, so
// when the requested path is not pure ASCII we copy the archive to an
// ASCII-only temp file and open that instead.

import os
import time
import compress.szip

// is_ascii reports whether every byte of s is < 128.
fn is_ascii(s string) bool {
	for b in s.bytes() {
		if b >= 128 {
			return false
		}
	}
	return true
}

// ascii_temp_dir returns a temp directory whose path is guaranteed ASCII.
fn ascii_temp_dir() string {
	td := os.temp_dir()
	if is_ascii(td) {
		return td
	}
	$if windows {
		return 'C:\\Windows\\Temp'
	} $else {
		return '/tmp'
	}
}

// ensure_ascii_zip returns a path that miniz can open. If `path` is already
// ASCII it is returned unchanged; otherwise the bytes are copied (via the
// Unicode-safe `os.read_bytes`) to an ASCII temp file and that path returned.
fn ensure_ascii_zip(path string) !string {
	if is_ascii(path) {
		return path
	}
	data := os.read_bytes(path) or {
		return error('cannot read zip `${path}`: ${err}')
	}
	tmp := os.join_path(ascii_temp_dir(), 'vviewer_${os.getpid()}_${time.now().unix_nano()}.zip')
	os.write_bytes(tmp, data) or {
		return error('cannot write temp zip `${tmp}`: ${err}')
	}
	return tmp
}

// scan_zip enumerates the image entries of a zip archive and adds them to the
// item list as extractable (need_extract) drawables. The stored `path` is an
// ASCII-safe path so later extraction can reopen the archive on Windows.
fn (mut il Item_list) scan_zip(orig_path string, in_index int) ! {
	println('Scanning ZIP [${orig_path}]')
	path := ensure_ascii_zip(orig_path)!
	mut zp := szip.open(path, szip.CompressionLevel.no_compression, szip.OpenMode.read_only)!
	n_entries := zp.total()!
	for index in 0 .. n_entries {
		zp.open_entry_by_index(index)!
		is_dir := zp.is_dir()!
		name := zp.name()
		size := zp.size()
		if !is_dir {
			ext := get_extension(name)
			if is_image(ext) {
				il.n_item += 1
				il.lst << Item{
					need_extract:         true
					path:                 path
					name:                 name.clone()
					container_index:      in_index
					container_item_index: index
					i_type:               ext
					n_item:               il.n_item
					drawable:             true
					size:                 size
				}
			}
		}
		// Keep the entry open until we are done reading it later.
		zp.close_entry()
	}
	zp.close()
}

// load_current_bytes returns the raw bytes of the currently selected item,
// reading it from disk or extracting it from the open zip archive.
fn (mut app App) load_current_bytes() ![]u8 {
	if app.item_list.lst.len <= 0 || app.item_list.n_item <= 0 {
		return error('no item selected')
	}
	item := app.item_list.lst[app.item_list.item_index]

	if !item.need_extract {
		full := app.item_list.get_file_path()
		return os.read_bytes(full) or {
			return error('cannot read `${full}`')
		}
	}

	// Reopen the zip only when the selected item belongs to a different archive
	// than the one currently open. `item.path` is already ASCII-safe (set by
	// scan_zip), so miniz can open it on Windows.
	if app.zip_index != item.container_index {
		if app.zip_index >= 0 {
			app.zip.close()
		}
		app.zip = szip.open(item.path, szip.CompressionLevel.no_compression,
			szip.OpenMode.read_only)!
		app.zip_index = item.container_index
	}

	app.zip.open_entry_by_index(item.container_item_index)!
	mut buf := []u8{len: int(item.size)}
	app.zip.read_entry_buf(buf.data, buf.len)!
	app.zip.close_entry()
	return buf
}
