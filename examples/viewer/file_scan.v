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
//
// The scan runs on a pool of worker threads (all CPU cores but two). Each
// job (one directory listing, or one zip archive) is processed by a worker
// with pure functions that touch no shared viewer state; workers publish
// their results to the main thread, which assembles the final flat list in
// `scan_finish`. The assembly reproduces the old single-threaded recursive
// order exactly (depth-first: folder entries in listing order, zip entries
// right after their archive, sub-folders last), so the "as loaded" order
// and the existing tests are unchanged.

import os
import runtime
import sync
import time

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
	// Shared state of the worker pool (nil between scans).
	pool &Scan_pool = unsafe { nil }
	// Top-level paths enqueued by scan_begin, in argument order; assembly
	// appends each root's Item and its subtree in that same order.
	roots []Root_entry
}

// path_sep returns the platform directory separator.
fn path_sep() string {
	return $if windows { '\\' } $else { '/' }
}

// Scan_job is one unit of scan work: a single directory listing or a single
// zip archive pass. Jobs are discovered while other jobs run (a folder
// listing yields its sub-folder and zip jobs), like work "stealing" from the
// queue: any worker may pick any job, so completion order is arbitrary; the
// deterministic item order is restored later by the assembly pass.
struct Scan_job {
mut:
	is_zip bool
	id     int    // unique job id (folder map key / zip temp-file discriminator)
	path   string // directory or archive to scan
}

// Sub_dir is one sub-folder discovered by a folder job: enough to create its
// folder Item and to enqueue its scan job during assembly.
struct Sub_dir {
mut:
	name  string // folder entry name
	path  string // full path
	mtime i64
	id    int // folder id given to its job
}

// Root_entry remembers a top-level path and the Item it contributes to the
// flat list. The Item is appended during assembly (not in scan_begin) so
// every root lands right before its own subtree, in argument order, exactly
// like the original recursive scan did.
struct Root_entry {
mut:
	is_zip bool
	id     int  // folder id (folder roots)
	item   Item // the root's own Item, appended during assembly
}

// Job_result is the pure output of one job. Workers fill one on their stack
// and hand it to the main thread by value through the results channel; the
// main thread files it in the pool maps and frees the array backings in
// scan_finish. The Items copy the string headers they keep (the per-entry
// strings themselves leak exactly like the old recursive scan did under
// `-gc none`).
struct Job_result {
mut:
	is_zip bool
	// folder results
	id      int    // folder id
	path    string // the directory itself (parent path for its sub-folders)
	items   []Item // images and zip items, in listing order
	subdirs []Sub_dir
	// zip results
	key      string // archive path as discovered (map key)
	zip_path string // ASCII-safe path stored on the entries
	n_total  int    // total archive entries (progress accounting)
}

// Scan_pool is the state shared between the main thread and the workers.
// Every mutable field lives behind `mtx`, including the result handover
// (`results_buf`) and the worker exit counter (`exited`): workers append
// finished results and bump `exited`, the main thread moves the results out
// and polls `exited` to join. No `chan` is used on purpose: V channels own
// internal ring buffers and synchronisation primitives that cannot be released
// under `-gc none`, so a per-scan channel pair would leak on every replacement
// scan, while plain arrays and the pool itself free cleanly.
struct Scan_pool {
mut:
	mtx       sync.Mutex
	queue     []Scan_job // pending jobs (LIFO: better cache locality)
	cancelled bool       // set by scan_abort(): workers stop early and drop results
	in_flight int        // enqueued but not yet completed jobs
	next_id   int        // folder id generator
	done      int        // progress: entries processed so far
	total     int        // progress: entries discovered so far
	warnings  []string   // capped at 100
	// Completed results, filed by the main thread while draining `results_buf`.
	folders map[int]Job_result
	zips    map[string]Job_result
	// Worker handover (guarded by `mtx`).
	results_buf []Job_result
	exited      int
	n_workers   int
}

// scan_progress_flush is how many folder entries a worker walks before
// reporting progress to the shared counters. The lock stays off the hot
// path (one lock per 512 entries) while the bar still advances smoothly;
// zip jobs report per entry because each entry already costs file I/O.
const scan_progress_flush = 512

// scan_worker_count is how many worker threads a scan uses: every logical
// CPU core but two, so the UI and whatever else is running keep their cores.
// At least one worker is always used.
fn scan_worker_count() int {
	mut n := runtime.nr_cpus() - 2
	if n < 1 {
		n = 1
	}
	return n
}

fn (mut p Scan_pool) add_scan_warning(msg string) {
	p.mtx.lock()
	if p.warnings.len < 100 {
		p.warnings << msg
	}
	p.mtx.unlock()
}

// add_progress advances the shared progress counters (done <= total is
// preserved: totals are always added before their matching dones).
fn (mut p Scan_pool) add_progress(done_inc int, total_inc int) {
	p.mtx.lock()
	p.done += done_inc
	p.total += total_inc
	p.mtx.unlock()
}

// new_job_id hands out a unique id for any scan job (folder or zip). Zip
// jobs use it as the temp-file discriminator in `ensure_ascii_zip`, so two
// workers must never build the same temp path in the same clock tick.
fn (mut p Scan_pool) new_job_id() int {
	p.mtx.lock()
	id := p.next_id
	p.next_id += 1
	p.mtx.unlock()
	return id
}

fn (mut p Scan_pool) push_job(job Scan_job) {
	p.mtx.lock()
	p.queue << job
	p.in_flight += 1
	p.mtx.unlock()
}

fn (mut p Scan_pool) job_finished() {
	p.mtx.lock()
	p.in_flight -= 1
	p.mtx.unlock()
}

// add_scan_warning records a non-fatal scan problem on the list itself
// (used before the pool exists, i.e. for the top-level paths). The report is
// capped so a pathological tree cannot grow it without bound.
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

// scan_begin starts a parallel scan of the given paths. It validates and
// enqueues the top-level paths (fast), then launches the worker pool; the
// actual walking happens on the workers. The main thread collects results
// with repeated scan_step() calls so the UI stays responsive and can show
// progress. scan_finish() must be called once scan_step() reports done.
fn (mut il Item_list) scan_begin(args []string) {
	il.loaded = false
	il.scanning = true
	il.path_sep = path_sep()
	mut p := &Scan_pool{}
	p.mtx.init()
	p.folders = map[int]Job_result{}
	p.zips = map[string]Job_result{}
	il.pool = p
	p.total = args.len
	for x in args {
		// Explicitly passed paths that vanished (or were mistyped) are reported
		// instead of being silently ignored.
		if !os.exists(x) {
			il.last_error = 'missing path `${x}`'
			il.add_scan_warning('missing path `${x}`')
			p.done += 1
			continue
		}
		if os.is_dir(x) {
			id := p.new_job_id()
			p.push_job(Scan_job{ id: id, path: x.clone() })
			il.roots << Root_entry{
				id:   id
				item: Item{
					path:   x
					name:   x
					i_type: .folder
					mtime:  os.file_last_mod_unix(x)
				}
			}
			p.done += 1
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
			p.push_job(Scan_job{ is_zip: true, id: p.new_job_id(), path: x.clone() })
			il.roots << Root_entry{ is_zip: true, item: item }
			p.done += 1
			continue
		}
		if is_image(ext) {
			item.i_type = ext
			item.drawable = true
			item.mtime = os.file_last_mod_unix(x)
			il.roots << Root_entry{ item: item }
		} else {
			// A top-level path the viewer cannot show (files found inside
			// scanned folders are skipped silently: that is normal).
			il.add_scan_warning('skipped, not a supported image: `${x}`')
		}
		p.done += 1
	}
	il.scan_done = p.done
	il.scan_total = p.total
	// Launch the workers: they drain the job queue until every job (and
	// everything it discovers) is done.
	p.n_workers = scan_worker_count()
	for _ in 0 .. p.n_workers {
		spawn scan_worker(p)
	}
}

// scan_worker is one pool worker: it runs jobs until the queue empties with
// no work in flight (or the scan is cancelled), publishes each result to the
// main thread, then signals its own exit. It never touches Item_list or any
// UI state.
fn scan_worker(p &Scan_pool) {
	mut mp := &Scan_pool(p)
	for {
		mut has_job := false
		mut job := Scan_job{}
		mp.mtx.lock()
		if mp.cancelled {
			mp.mtx.unlock()
			break
		}
		if mp.queue.len > 0 {
			job = mp.queue.pop()
			has_job = true
		} else if mp.in_flight == 0 {
			mp.mtx.unlock()
			break
		}
		mp.mtx.unlock()
		if !has_job {
			time.sleep(1 * time.millisecond)
			continue
		}
		if job.is_zip {
			run_zip_job(mut mp, job)
		} else {
			run_folder_job(mut mp, job)
		}
		mp.job_finished()
	}
	mp.mtx.lock()
	mp.exited += 1
	mp.mtx.unlock()
}

// publish_result hands a finished job's result to the main thread by appending
// it to the shared handover buffer under the pool lock. If the scan was
// cancelled while the job ran, the result is dropped (its scratch arrays freed)
// and false is returned; the aborting thread keeps draining `results_buf`
// until every worker has exited, so a worker never blocks.
fn publish_result(mut p &Scan_pool, mut res Job_result) bool {
	p.mtx.lock()
	if p.cancelled {
		p.mtx.unlock()
		free_job_result(mut res)
		return false
	}
	p.results_buf << res
	p.mtx.unlock()
	return true
}

// run_folder_job lists one directory and classifies its entries. It only
// reads the filesystem (one stat per entry: the old engine paid two) and
// enqueues the discovered sub-folder and zip jobs; the produced Items get
// their container links later, during assembly. A folder that cannot be
// listed (permissions, deleted mid-scan) is a warning, never an abort. The
// finished result is handed to the main thread through the results channel.
fn run_folder_job(mut p &Scan_pool, job Scan_job) {
	entries := os.ls(job.path) or {
		p.add_scan_warning('folder `${job.path}`: ${err}')
		return
	}
	println('Scanning [${job.path}]')
	p.add_progress(0, entries.len)
	mut res := Job_result{
		id:   job.id
		path: job.path
	}
	sep := path_sep()
	// Progress advances as the listing is walked (in batches), like the
	// old chunked main-thread engine did, so a huge folder animates the
	// bar instead of jumping when the job ends.
	mut flushed := 0
	for c, x in entries {
		if c - flushed >= scan_progress_flush {
			p.add_progress(c - flushed, 0)
			flushed = c
		}
		pt := '${job.path}${sep}${x}'
		st := os.stat(pt) or {
			// The entry vanished between ls and stat: counted, skipped.
			continue
		}
		if st.get_filetype() == .directory {
			id := p.new_job_id()
			res.subdirs << Sub_dir{
				name:  x.clone()
				path:  pt
				mtime: st.mtime
				id:    id
			}
			p.push_job(Scan_job{ id: id, path: pt })
			continue
		}
		ext := get_extension(x)
		if ext == .zip {
			res.items << Item{
				path:                 job.path
				name:                 x.clone()
				container_item_index: c
				i_type:               .zip
				mtime:                st.mtime
			}
			p.push_job(Scan_job{ is_zip: true, id: p.new_job_id(), path: pt })
		} else if is_image(ext) {
			res.items << Item{
				path:                 job.path
				name:                 x.clone()
				container_item_index: c
				i_type:               ext
				mtime:                st.mtime
				drawable:             true
			}
		}
	}
	p.add_progress(entries.len - flushed, 0)
	// Release the os.ls() listing. The retained Items/Sub_dirs own cloned name
	// strings (see the x.clone() below), so a deep entries.free() is safe here
	// and reclaims both the array backing and the strings of the skipped
	// (non-image, non-archive) entries. Under the Boehm build free is a no-op,
	// so this only matters for the `-gc none` target.
	unsafe { entries.free() }
	publish_result(mut p, mut res)
}

// run_zip_job scans one archive and hands the result to the main thread.
// A corrupt or unreadable archive is a warning, never an abort. Progress
// is reported inside scan_zip_items while the entries are enumerated.
fn run_zip_job(mut p &Scan_pool, job Scan_job) {
	mut zr := scan_zip_items(mut p, job.path, job.id) or {
		p.add_scan_warning('zip `${job.path}`: ${err}')
		return
	}
	publish_result(mut p, mut zr)
}

// scan_step collects the results the workers have completed so far and
// reports whether the scan still has work in flight. Cheap by design: the
// filesystem walking happens on the workers, so the UI frame budget is only
// spent on moving finished results into the pool maps.
fn (mut il Item_list) scan_step() bool {
	if il.pool == unsafe { nil } {
		return false
	}
	mut p := &Scan_pool(il.pool)
	// Move the finished results out of the shared buffer under the lock, then
	// file them into the pool maps outside it (the maps are main-thread only).
	p.mtx.lock()
	mut buf := unsafe { p.results_buf }
	p.results_buf = []Job_result{}
	il.scan_done = p.done
	il.scan_total = p.total
	more := p.in_flight > 0
	p.mtx.unlock()
	for r in buf {
		if r.is_zip {
			p.zips[r.key] = r
		} else {
			p.folders[r.id] = r
		}
	}
	unsafe { buf.free() }
	return more
}

// scan_finish closes a scan: it waits for the workers to exit, assembles the
// flat item list in the exact order the old single-threaded recursive scan
// produced, records the original load order, selects the first item and
// releases the pool.
fn (mut il Item_list) scan_finish() {
	if il.pool == unsafe { nil } {
		return
	}
	mut p := &Scan_pool(il.pool)
	// Join: every worker bumps `exited` after publishing its last result, so
	// once all have exited the handover buffer holds whatever is still left.
	mut exited := 0
	for exited < p.n_workers {
		p.mtx.lock()
		exited = p.exited
		p.mtx.unlock()
		if exited < p.n_workers {
			time.sleep(1 * time.millisecond)
		}
	}
	// All workers are gone: no one can append any more, so drain without locking.
	mut buf := unsafe { p.results_buf }
	p.results_buf = []Job_result{}
	for r in buf {
		if r.is_zip {
			p.zips[r.key] = r
		} else {
			p.folders[r.id] = r
		}
	}
	unsafe { buf.free() }
	il.assemble_scan(mut p)
	for w in p.warnings {
		// Clone: `p.warnings.free()` below deep-frees the pool's strings, so a
		// shallow-copied header would dangle in the retained report.
		il.scan_warnings << w.clone()
	}
	il.scan_done = p.total
	release_pool(mut p)
	il.pool = unsafe { nil }
	for i in 0 .. il.lst.len {
		il.lst[i].orig_index = i
	}
	println('Items: ${il.n_item}')
	il.get_next_item(1)
	il.loaded = true
	il.scanning = false
}

// release_pool frees a finished scan's pool and everything it owns: the
// results' scratch arrays, the maps, the handover buffer, the mutex and the
// pool itself. The per-entry strings leak exactly like the old recursive scan
// did under `-gc none`; the Items that survived assembly already copied them.
fn release_pool(mut p &Scan_pool) {
	for _, mut fr in p.folders {
		free_job_result(mut fr)
	}
	for _, mut zr in p.zips {
		free_job_result(mut zr)
	}
	// Drain any result still in the handover buffer (normally empty: the join
	// in scan_finish/scan_abort already moved them out).
	for mut r in p.results_buf {
		free_job_result(mut r)
	}
	unsafe { p.folders.free() }
	unsafe { p.zips.free() }
	unsafe { p.results_buf.free() }
	unsafe { p.queue.free() }
	unsafe { p.warnings.free() }
	p.mtx.destroy()
	// `p.free()` is a no-op for a reference under `-gc none`; the builtin
	// `free` emits a real v_free and releases the pool allocation itself.
	unsafe { free(p) }
}

// scan_abort cancels a running scan and empties the list, so a new scan can
// start on the same Item_list. Dropping a new path set during a scan must not
// leave the old workers traversing the tree and publishing into a buffer nobody
// drains: they are told to stop, their handed-over results are drained and
// freed, and the main thread joins every worker before the pool is released.
// Safe to call at any time (idle, scanning, or finished).
fn (mut il Item_list) scan_abort() {
	if il.pool != unsafe { nil } {
		mut p := &Scan_pool(il.pool)
		p.mtx.lock()
		p.cancelled = true
		n := p.n_workers
		p.mtx.unlock()
		// A worker notices the flag between jobs and before publishing; one
		// that is mid-job still finishes it (a listing or an archive parse
		// cannot be interrupted) and then drops its result. Drain whatever the
		// workers already handed over and wait for every exit.
		mut exited := 0
		for exited < n {
			p.mtx.lock()
			mut buf := unsafe { p.results_buf }
			p.results_buf = []Job_result{}
			exited = p.exited
			p.mtx.unlock()
			for mut r in buf {
				free_job_result(mut r)
			}
			unsafe { buf.free() }
			if exited < n {
				time.sleep(1 * time.millisecond)
			}
		}
		// Whatever the last workers handed over just before exiting.
		mut last := unsafe { p.results_buf }
		p.results_buf = []Job_result{}
		for mut r in last {
			free_job_result(mut r)
		}
		unsafe { last.free() }
		release_pool(mut p)
		il.pool = unsafe { nil }
	}
	il.reset()
}

// reset empties the item list and all scan state (the pool must already be
// released). Every owned allocation is freed for the `-gc none` target; the
// per-entry Item strings leak exactly like the old recursive scan did.
fn (mut il Item_list) reset() {
	unsafe { il.lst.free() }
	unsafe { il.roots.free() }
	il.lst = []Item{}
	il.roots = []Root_entry{}
	unsafe { il.scan_warnings.free() } // deep-free: the report owns its strings
	il.scan_warnings = []string{}
	il.item_index = -1
	il.n_item = 0
	il.loaded = false
	il.scanning = false
	il.scan_done = 0
	il.scan_total = 0
	il.last_error = ''
	// A replacement scan always rebuilds the list in "as loaded" order, so the
	// sort mode must match it. Without this, App.set_sort() would early-return
	// when the user re-selects the stale mode (e.g. sort by name, drop a new
	// folder, press `1` again: the new list would stay unsorted).
	il.sort_mode = .loaded
}

// free_job_result releases one finished job's scratch arrays. Only the array
// backings are freed: the Items already copied the string headers they keep
// (the per-entry strings themselves leak exactly like the old recursive
// scan did under `-gc none`).
fn free_job_result(mut r Job_result) {
	unsafe { r.items.free() }
	unsafe { r.subdirs.free() }
}

// assemble_scan flattens the per-job results into the final list. The roots
// are walked in argument order and each root's own Item is appended right
// before its subtree, exactly like the original recursive scan did.
fn (mut il Item_list) assemble_scan(mut p &Scan_pool) {
	for r in il.roots {
		mut item := r.item
		if r.is_zip {
			il.lst << item
			il.append_zip_entries(mut p, item.name, il.lst.len - 1)
		} else if item.i_type == .folder {
			item.container_index = il.lst.len // a folder is its own container
			il.lst << item
			il.assemble_folder(mut p, r.id, il.lst.len - 1)
		} else {
			// A direct image file passed on the command line / dropped.
			il.n_item += 1
			item.n_item = il.n_item
			il.lst << item
		}
	}
	unsafe { il.roots.free() }
}

// assemble_folder appends the contents of one scanned folder after its own
// folder Item (already in the list at `self_index`): entries in listing
// order with each archive's entries right after the archive item, then the
// sub-folder subtrees.
fn (mut il Item_list) assemble_folder(mut p &Scan_pool, fid int, self_index int) {
	r := p.folders[fid] or {
		return // folder could not be listed: warning already recorded
	}
	for it in r.items {
		mut item := it
		item.container_index = self_index
		if item.i_type == .zip {
			il.lst << item
			zi := il.lst.len - 1
			il.append_zip_entries(mut p, '${item.path}${il.path_sep}${item.name}', zi)
		} else {
			il.n_item += 1
			item.n_item = il.n_item
			il.lst << item
		}
	}
	for sd in r.subdirs {
		il.lst << Item{
			path:            r.path
			name:            sd.name
			i_type:          .folder
			container_index: self_index
			mtime:           sd.mtime
		}
		il.assemble_folder(mut p, sd.id, il.lst.len - 1)
	}
}

// append_zip_entries appends the drawable entries of a scanned archive
// right after the archive's own item (already in the list at `zip_index`).
fn (mut il Item_list) append_zip_entries(mut p &Scan_pool, key string, zip_index int) {
	zr := p.zips[key] or {
		return // unreadable archive: warning already recorded
	}
	for e in zr.items {
		mut ei := e
		ei.container_index = zip_index
		il.n_item += 1
		ei.n_item = il.n_item
		il.lst << ei
	}
}

// get_items_list builds the list from the command-line / dropped paths,
// synchronously: it drives the same parallel engine the GUI scan uses.
fn (mut il Item_list) get_items_list(args []string) {
	il.scan_begin(args)
	for il.scan_step() {
	}
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
