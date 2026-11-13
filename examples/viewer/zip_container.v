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

// max_image_bytes caps the uncompressed size the viewer will allocate for a
// single archive entry. A lazy open trusts the central directory, and a
// corrupt archive can declare gigabytes over a tiny payload, so the bound is
// checked before any allocation happens.
const max_image_bytes = u64(512 * 1024 * 1024)

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
// `uid` makes the temp name unique per scan job: two workers copying the
// same non-ASCII archive within the same nanosecond must not collide.
fn ensure_ascii_zip(path string, uid int) !string {
	if is_ascii(path) {
		return path
	}
	data := os.read_bytes(path) or {
		return error('cannot read zip `${path}`: ${err}')
	}
	tmp := os.join_path(ascii_temp_dir(), 'vviewer_${os.getpid()}_${time.now().unix_nano()}_${uid}.zip')
	os.write_bytes(tmp, data) or {
		return error('cannot write temp zip `${tmp}`: ${err}')
	}
	return tmp
}

// zip_format_hint inspects the first bytes of a file that failed to open as
// a zip archive and names a known foreign format, so the scan warning says
// what the file actually is (empty, RAR, 7-Zip) instead of only why the
// zip parse failed.
fn zip_format_hint(path string) string {
	size := os.file_size(path)
	if size == 0 {
		return ' [empty file]'
	}
	if size < 8 {
		return ''
	}
	mut f := os.open(path) or {
		return ''
	}
	defer {
		f.close()
	}
	head := f.read_raw_at[[8]u8](0) or {
		return ''
	}
	if head[0] == 0x52 && head[1] == 0x61 && head[2] == 0x72 && head[3] == 0x21 {
		return ' [actually a RAR archive]'
	}
	if head[0] == 0x37 && head[1] == 0x7a && head[2] == 0xbc && head[3] == 0xaf {
		return ' [actually a 7-Zip archive]'
	}
	return ''
}

// scan_zip_items enumerates the image entries of a zip archive into a pure
// Job_result: it touches no shared viewer state because it runs on a worker
// thread. The stored `path` is an ASCII-safe path so later extraction can
// reopen the archive on Windows. A single bad entry is recorded as a warning
// and skipped; it never aborts the scan of the remaining entries.
fn scan_zip_items(mut p &Scan_pool, orig_path string, uid int) !Job_result {
	println('Scanning ZIP [${orig_path}]')
	path := ensure_ascii_zip(orig_path, uid)!
	// Lazy open: only the central directory is parsed, no entry is decoded.
	mut zp := szip.open_lazy(path) or {
		return error('${err}${zip_format_hint(orig_path)}')
	}
	n_entries := zp.total()!
	// Progress accounting: the archive's entries become known now (the
	// total grows) and are reported one by one as they are enumerated, so
	// even a huge archive animates the bar instead of jumping at the end.
	p.add_progress(0, n_entries)
	mut res := Job_result{
		is_zip:   true
		key:      orig_path.clone()
		zip_path: path
		n_total:  n_entries
	}
	for index in 0 .. n_entries {
		scan_zip_entry(mut zp, index, path, mut res) or {
			p.add_scan_warning('zip entry ${index} in `${orig_path}`: ${err}')
		}
		p.add_progress(1, 0)
	}
	zp.close()
	return res
}

// scan_zip_entry reads one archive entry and appends it to the job result
// when it is a viewable image. Errors are returned so the caller can skip
// the single bad entry (encrypted, truncated header, ...) and keep scanning.
fn scan_zip_entry(mut zp &szip.Zip, index int, path string, mut res Job_result) ! {
	zp.open_entry_by_index(index)!
	is_dir := zp.is_dir()!
	name := zp.name()
	size := zp.size()
	// The entry is re-opened by index when its bytes are actually needed.
	zp.close_entry()
	if is_dir {
		return
	}
	ext := get_extension(name)
	if is_image(ext) {
		if size > max_image_bytes {
			// The central directory claims more than any viewable image can
			// need: treat the entry as corrupt instead of listing an item
			// whose load would try a huge allocation.
			return error('implausible uncompressed size ${size}')
		}
		res.items << Item{
			need_extract:         true
			path:                 path
			name:                 name.clone()
			container_item_index: index
			i_type:               ext
			drawable:             true
			size:                 size
		}
	}
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
	// scan_zip), so miniz can open it on Windows. The lazy open decodes only
	// the entry that is actually read, not the whole archive.
	if app.zip_index != item.container_index {
		if app.zip_index >= 0 {
			app.zip.close()
		}
		app.zip = szip.open_lazy(item.path)!
		app.zip_index = item.container_index
	}

	// The destination buffer is NOT sized from the untrusted central-directory
	// `item.size`: a corrupt archive can declare a plausible size there while
	// its local header disagrees, and allocating from it before validation is
	// what let a crafted archive exhaust the address space. read_entry_bytes()
	// validates the entry against its local header, decompresses and verifies
	// the checksum first, and only then allocates the real decoded length.
	app.zip.open_entry_by_index(item.container_item_index)!
	buf := app.zip.read_entry_bytes() or {
		app.zip.close_entry()
		return error('zip entry `${item.name}`: ${err}')
	}
	app.zip.close_entry()
	return buf
}
