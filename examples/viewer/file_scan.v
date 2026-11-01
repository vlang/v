/**********************************************************************
*
* File, folder, and zip archive scanner for the image viewer
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module main

// File / folder / zip scanner for the viewer.
//
// It builds a
// flat list of "items" (files, folders, zip archives, and the images inside
// zips) and provides navigation helpers. Decoding itself is format-agnostic
// (handled by the `decoder` module via magic bytes); here we only need to
// know which entries are drawable images.

import os

// Item_type classifies a scanned entry. Image formats MUST stay grouped at the
// bottom (>= .bmp) so `is_image` can be a single range check.
enum Item_type {
	file = 0
	folder
	// containers
	zip = 16
	// image formats (drawable)
	bmp = 32
	jpg
	png
	gif
	tiff
	webp
}

// Item is one entry in the navigation list.
pub struct Item {
pub mut:
	path                 string
	name                 string
	size                 u64
	mtime                i64 // last-modified time in seconds (0 for zip entries)
	i_type               Item_type = .file
	container_index      int  // index of the owning zip/folder item (-1 if none)
	container_item_index int  // index of this entry inside the zip
	need_extract         bool // true when the bytes live inside a zip
	drawable             bool // true when this item is a viewable image
	n_item               int  // 1-based position among drawable images
	rotation             int  // number of 90-degree rotations applied
	orig_index           int  // position in the original load order (for "as loaded" sort)
}

// Sort_mode selects how the hierarchy is ordered.
enum Sort_mode {
	loaded = 0 // original scan order (default)
	name   = 1 // by file name (case-insensitive)
	date   = 2 // by last-modified time
}

// Item_list holds the scanned list and the current selection.
struct Item_list {
pub mut:
	lst        []Item
	path_sep   string
	item_index int = -1 // index into lst of the currently shown item
	n_item     int    // number of drawable images
	loaded     bool   // true once scanning finished
	last_error string // last scan error (shown in the overlay; eprintln is invisible in a GUI exe)
	sort_mode  Sort_mode = .loaded
	// Non-fatal scan problems (unreadable subfolder, corrupt zip entry, ...).
	// The scan always continues past such entries; the overlay shows the count
	// and `W` toggles the full list.
	scan_warnings []string
	// Incremental scan state. `scanning` is true from scan_begin() until
	// scan_finish(). Progress is reported as scan_done/scan_total entries:
	// every filesystem entry (file, folder, archive) and archive entry
	// discovered so far counts once in each counter.
	scanning   bool
	scan_done  int
	scan_total int
	scan_jobs  []Scan_job
}

// Scan_job is one resumable unit of the incremental scan: either a directory
// listing that can be processed in chunks (folder) or a single zip archive
// pass (zip). Jobs live in a LIFO stack so the produced item order matches
// the old recursive depth-first scan exactly.
struct Scan_job {
mut:
	is_zip bool
	path   string // directory or archive to scan
	in_index int  // container index of the listing (folder) / zip item
	// Sub-folder jobs create their folder Item when popped, exactly where
	// the old recursive scan created it (just before descending).
	pending_item bool
	item_path    string
	item_name    string
	item_in      int
	item_mtime   i64
	// Resumable listing state (folder jobs only).
	entries []string
	listed  bool
	pos     int
	subdirs []string
}

// scan_chunk_entries is how many directory entries one scan_step() pass
// processes before yielding, so huge folders cannot stall a frame.
const scan_chunk_entries = 512

// add_scan_warning records a non-fatal scan problem. The report is capped so
// a pathological tree (thousands of unreadable entries) cannot grow it
// without bound.
fn (mut il Item_list) add_scan_warning(msg string) {
	if il.scan_warnings.len < 100 {
		il.scan_warnings << msg
	}
}

@[inline]
fn modulo(x int, n int) int {
	return (x % n + n) % n
}

// get_extension maps a filename to its Item_type by extension.
fn get_extension(x string) Item_type {
	if x.len > 4 {
		match x[x.len - 4..].to_lower() {
			'.zip' {
				return .zip
			}
			'.jpg' {
				return .jpg
			}
			'.png' {
				return .png
			}
			'.bmp' {
				return .bmp
			}
			'.gif' {
				return .gif
			}
			else {}
		}
	}
	if x.len > 5 {
		match x[x.len - 5..].to_lower() {
			'.jpeg' {
				return .jpg
			}
			'.tiff' {
				return .tiff
			}
			'.webp' {
				return .webp
			}
			else {}
		}
	}
	return .file
}

@[inline]
fn is_image(t Item_type) bool {
	return int(t) >= int(Item_type.bmp)
}

@[inline]
fn is_container(t Item_type) bool {
	return t in [.zip, .folder]
}

// is_inside_a_container reports whether the current item must be extracted
// from a zip before it can be decoded.
fn (il &Item_list) is_inside_a_container() bool {
	if il.lst.len <= 0 || il.n_item <= 0 {
		return false
	}
	return il.lst[il.item_index].need_extract
}

// get_file_path returns the on-disk path of the current item. For direct
// command-line files `path` is empty and `name` is the full path (matching
// V's example viewer). For folder-scanned files `path` is the directory.
fn (il &Item_list) get_file_path() string {
	if il.lst.len <= 0 || il.n_item <= 0 {
		return ''
	}
	it := il.lst[il.item_index]
	if it.need_extract {
		return it.name
	}
	if it.path != '' {
		return '${it.path}${il.path_sep}${it.name}'
	}
	return it.name
}

// scan_begin starts an incremental scan of the given paths. It only
// validates and enqueues the top-level paths (fast); the actual walking is
// done by repeated scan_step() calls so a UI stays responsive and can show
// progress. scan_finish() must be called once scan_step() reports done.
fn (mut il Item_list) scan_begin(args []string) {
	il.loaded = false
	il.scanning = true
	il.path_sep = $if windows { '\\' } $else { '/' }
	il.scan_done = 0
	il.scan_total = args.len
	for x in args {
		// Explicitly passed paths that vanished (or were mistyped) are reported
		// instead of being silently ignored.
		if !os.exists(x) {
			il.last_error = 'missing path `${x}`'
			il.add_scan_warning('missing path `${x}`')
			il.scan_done += 1
			continue
		}
		if os.is_dir(x) {
			il.lst << Item{
				path:            x
				name:            x
				container_index: il.lst.len
				i_type:          .folder
				mtime:           os.file_last_mod_unix(x)
			}
			il.scan_jobs << Scan_job{ path: x.clone(), in_index: il.lst.len - 1 }
			il.scan_done += 1
			continue
		}
		mut item := Item{
			path:            ''
			name:            x
			container_index: -1
		}
		ext := get_extension(x)
		if ext == .zip {
			item.i_type = .zip
			item.mtime = os.file_last_mod_unix(x)
			il.lst << item
			il.scan_jobs << Scan_job{ is_zip: true, path: x.clone(), in_index: il.lst.len - 1 }
			il.scan_done += 1
			continue
		}
		if is_image(ext) {
			il.n_item += 1
			item.n_item = il.n_item
			item.i_type = ext
			item.drawable = true
			item.mtime = os.file_last_mod_unix(x)
			il.lst << item
		} else {
			// A top-level path the viewer cannot show (files found inside
			// scanned folders are skipped silently: that is normal).
			il.add_scan_warning('skipped, not a supported image: `${x}`')
		}
		il.scan_done += 1
	}
}

// free_job_scratch releases a finished job's scratch arrays. Only the array
// backings are freed: the Items already copied the string headers they keep
// (the per-entry strings themselves leak exactly like the old recursive
// scan did under `-gc none`).
fn free_job_scratch(mut job Scan_job) {
	unsafe { job.entries.free() }
	unsafe { job.subdirs.free() }
}

// scan_step processes one chunk of the pending scan work and reports whether
// more work remains. Folder listings are cut every scan_chunk_entries
// entries; a zip found mid-listing suspends its folder so the archive's
// entries land right after the zip item, preserving the old recursive order.
fn (mut il Item_list) scan_step() bool {
	if il.scan_jobs.len == 0 {
		return false
	}
	mut job := il.scan_jobs.pop()
	if job.is_zip {
		// A corrupt or unreadable archive must not abort the scan of the
		// remaining entries: record the problem and continue.
		il.scan_zip(job.path, job.in_index) or {
			il.add_scan_warning('zip `${job.path}`: ${err}')
		}
		free_job_scratch(mut job)
		return il.scan_jobs.len > 0
	}
	if job.pending_item {
		il.lst << Item{
			path:            job.item_path
			name:            job.item_name
			i_type:          .folder
			container_index: job.item_in
			mtime:           job.item_mtime
		}
		job.in_index = il.lst.len - 1
	}
	if !job.listed {
		println('Scanning [${job.path}]')
		// One unreadable folder (permissions, locked, deleted mid-scan) must
		// not abort the scan of its siblings: record and continue.
		mut ls_failed := false
		entries := os.ls(job.path) or {
			ls_failed = true
			il.add_scan_warning('folder `${job.path}`: ${err}')
			[]string{}
		}
		if ls_failed {
			free_job_scratch(mut job)
			return il.scan_jobs.len > 0
		}
		job.entries = entries
		job.listed = true
		il.scan_total += job.entries.len
	}
	mut processed := 0
	for job.pos < job.entries.len && processed < scan_chunk_entries {
		x := job.entries[job.pos]
		c := job.pos
		job.pos += 1
		processed += 1
		il.scan_done += 1
		pt := '${job.path}${il.path_sep}${x}'
		mut item := Item{
			path:                 job.path
			name:                 x
			container_index:      job.in_index
			container_item_index: c
		}
		if os.is_dir(pt) {
			job.subdirs << x
			continue
		}
		ext := get_extension(x)
		if ext == .zip {
			item.i_type = .zip
			item.mtime = os.file_last_mod_unix(pt)
			il.lst << item
			// Suspend the folder: the archive entries must be appended
			// immediately after the zip item, before the next sibling.
			il.scan_jobs << job
			il.scan_jobs << Scan_job{ is_zip: true, path: pt, in_index: il.lst.len - 1 }
			return true
		}
		if is_image(ext) {
			il.n_item += 1
			item.n_item = il.n_item
			item.i_type = ext
			item.drawable = true
			item.mtime = os.file_last_mod_unix(pt)
			il.lst << item
		}
	}
	if job.pos < job.entries.len {
		il.scan_jobs << job // suspended mid-listing
		return true
	}
	// All entries done: enqueue the sub-folders. Push in reverse so the
	// stack pops the first sub-folder first, like the old recursion did.
	// (A plain `len - 1 .. -1` range is deliberately avoided: it does not
	// iterate when the end bound is negative.)
	mut si := job.subdirs.len - 1
	for si >= 0 {
		x := job.subdirs[si]
		si--
		pt := '${job.path}${il.path_sep}${x}'
		il.scan_jobs << Scan_job{
			pending_item: true
			path:         pt
			item_path:    job.path
			item_name:    x
			item_in:      job.in_index
			item_mtime:   os.file_last_mod_unix(pt)
		}
	}
	free_job_scratch(mut job)
	return il.scan_jobs.len > 0
}

// scan_finish closes an incremental scan: records the original load order
// (so the "as loaded" sort can restore it even after reordering), selects the
// first item and marks the list loaded.
fn (mut il Item_list) scan_finish() {
	for i in 0 .. il.lst.len {
		il.lst[i].orig_index = i
	}
	println('Items: ${il.n_item}')
	il.get_next_item(1)
	il.loaded = true
	il.scanning = false
}

// get_items_list builds the list from the command-line / dropped paths,
// synchronously: it drives the same incremental engine the GUI scan uses.
fn (mut il Item_list) get_items_list(args []string) {
	il.scan_begin(args)
	for il.scan_step() {}
	il.scan_finish()
}

// get_next_item advances the selection by `in_inc`, skipping non-drawable
// entries and wrapping around.
fn (mut il Item_list) get_next_item(in_inc int) {
	if il.lst.len <= 0 || il.n_item <= 0 {
		return
	}
	inc := if in_inc > 0 { 1 } else { -1 }
	mut i := modulo(il.item_index + in_inc, il.lst.len)
	start := i
	for {
		if il.lst[i].drawable {
			il.item_index = i
			break
		}
		i = modulo(i + inc, il.lst.len)
		if i == start {
			break
		}
	}
}

// go_to_next_container jumps to the next folder/zip boundary.
fn (mut il Item_list) go_to_next_container(in_inc int) {
	if il.lst.len <= 0 || il.n_item <= 0 {
		return
	}
	inc := if in_inc > 0 { 1 } else { -1 }
	cur_container := il.lst[il.item_index].container_index
	mut i := modulo(il.item_index + in_inc, il.lst.len)
	start := i
	for {
		if is_container(il.lst[i].i_type) && i != cur_container {
			il.item_index = i
			il.get_next_item(1)
			break
		}
		i = modulo(i + inc, il.lst.len)
		if i == start {
			break
		}
	}
}

// rotate applies a 90-degree rotation step to the current item.
fn (mut il Item_list) rotate(in_inc int) {
	mut r := il.lst[il.item_index].rotation + in_inc
	r = modulo(r, 4)
	il.lst[il.item_index].rotation = r
}

// resort_merge is a merge sort of `order` by the string key `keys[order[i]]`.
// `tmp` is a pre-allocated scratch buffer of the same length. A hand-written
// sort is used because V 0.5.2's `.sort()` sugar cannot compare through an
// external key array, and a capturing closure would allocate a context that
// leaks under `-gc none`.
fn resort_merge(mut order []int, keys []string, mut tmp []int, lo int, hi int) {
	if hi - lo <= 1 {
		return
	}
	mid := (lo + hi) / 2
	resort_merge(mut order, keys, mut tmp, lo, mid)
	resort_merge(mut order, keys, mut tmp, mid, hi)
	mut i := lo
	mut j := mid
	mut k := lo
	for i < mid && j < hi {
		if keys[order[i]] <= keys[order[j]] {
			tmp[k] = order[i]
			i++
		} else {
			tmp[k] = order[j]
			j++
		}
		k++
	}
	for i < mid {
		tmp[k] = order[i]
		i++
		k++
	}
	for j < hi {
		tmp[k] = order[j]
		j++
		k++
	}
	for x in lo .. hi {
		order[x] = tmp[x]
	}
}

// resort reorders the navigation hierarchy by `mode` while preserving the
// folder/zip tree: siblings inside each container are ordered by the chosen key
// and the tree is re-flattened in depth-first order. The current selection
// follows the same file to its new position. Every temporary allocation is
// released because the Windows target runs without a garbage collector.
fn (mut il Item_list) resort(mode Sort_mode) {
	if il.lst.len == 0 {
		return
	}
	il.sort_mode = mode
	n := il.lst.len
	mut cur_orig := -1
	if il.item_index >= 0 && il.item_index < n {
		cur_orig = il.lst[il.item_index].orig_index
	}
	// Build a full-path sort key per item. The list is in DFS order, so a
	// parent's key is computed before its children's; the low separator byte
	// keeps every child grouped under its parent's prefix.
	mut keys := []string{len: n}
	for i in 0 .. n {
		it := il.lst[i]
		mut own := ''
		match mode {
			.name {
				own = it.name.to_lower()
			}
			.date {
				own = '${it.mtime:020}'
			}
			else {
				own = '${it.orig_index:020}'
			}
		}
		if it.container_index >= 0 && it.container_index != i {
			// Two 2-operand concats avoid the intermediate that `a + b + c` leaks.
			mut tail := '\x01' + own
			keys[i] = keys[it.container_index] + tail
			unsafe { tail.free() }
			unsafe { own.free() }
		} else {
			keys[i] = own
		}
	}
	// Sort item indices by their full-path key (hierarchy-preserving).
	mut order := []int{len: n}
	for i in 0 .. n {
		order[i] = i
	}
	mut tmp := []int{len: n}
	resort_merge(mut order, keys, mut tmp, 0, n)
	unsafe { tmp.free() }
	// Map old index -> new position.
	mut old_to_new := []int{len: n}
	for pos in 0 .. n {
		old_to_new[order[pos]] = pos
	}
	// Re-flatten: rebuild the list in the new order, recomputing the container
	// links and the 1-based drawable counter.
	mut new_lst := []Item{len: n}
	mut new_n := 0
	for pos in 0 .. n {
		oi := order[pos]
		mut it := il.lst[oi]
		if it.container_index >= 0 && it.container_index != oi {
			it.container_index = old_to_new[it.container_index]
		} else {
			it.container_index = -1
		}
		if it.drawable {
			new_n += 1
			it.n_item = new_n
		}
		new_lst[pos] = it
	}
	// Follow the previously selected file to its new position (before the maps
	// are released).
	mut new_item_index := il.item_index
	if cur_orig >= 0 && il.item_index >= 0 && il.item_index < n {
		new_item_index = old_to_new[il.item_index]
	}
	// Release all temporaries (no GC on the Windows target).
	// `keys.free()` deep-frees every key string and the backing array, so the
	// key strings must NOT be freed individually here (that would double-free).
	unsafe { keys.free() }
	unsafe { order.free() }
	unsafe { old_to_new.free() }
	// Swap in the new list. The old backing array is freed; the strings it held
	// are shallow-copied into new_lst and therefore survive.
	unsafe { il.lst.free() }
	unsafe { il.lst = new_lst }
	il.n_item = new_n
	il.item_index = new_item_index
}
