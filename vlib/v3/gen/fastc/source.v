module fastc

import os
import strings
import time
import v3.pref
import v3.scanner
import v3.token

fn fastc_vmod_root_for_file(source_file string) string {
	mut dir := if source_file.len > 0 { os.dir(source_file) } else { os.getwd() }
	if dir.len == 0 {
		dir = os.getwd()
	}
	original_dir := dir
	for {
		if os.exists(os.join_path(dir, 'v.mod')) {
			return os.real_path(dir)
		}
		parent := os.dir(dir)
		if parent == dir || parent.len == 0 {
			return os.real_path(original_dir)
		}
		dir = parent
	}
	return os.real_path(original_dir)
}

fn fastc_resolve_c_pseudo_paths(raw string, vroot string, source_file string) string {
	mut result := raw
	if result.contains('@VEXEROOT') && vroot.len > 0 {
		result = result.replace('@VEXEROOT', vroot)
	}
	if result.contains('@VROOT') {
		result = result.replace('@VROOT', '@VMODROOT')
	}
	if result.contains('@VMODROOT') {
		result = result.replace('@VMODROOT', fastc_vmod_root_for_file(source_file))
	}
	if result.contains('@DIR') {
		dir := if source_file.len > 0 { os.dir(source_file) } else { os.getwd() }
		result = result.replace('@DIR', os.real_path(dir))
	}
	// A quoted C include is relative to the V file that declared it. FastC hoists
	// directives into one generated C file, so preserve that original include base
	// by making the path absolute before hoisting.
	if result.starts_with('include "') || result.starts_with('insert "') {
		quote_start := result.index_u8(`"`)
		quote_end := result.last_index_u8(`"`)
		if quote_start >= 0 && quote_end > quote_start {
			include_path := result[quote_start + 1..quote_end]
			if !os.is_abs_path(include_path) {
				dir := if source_file.len > 0 { os.dir(source_file) } else { os.getwd() }
				resolved := os.join_path(os.real_path(dir), include_path)
				if os.exists(resolved) {
					result = result[..quote_start + 1] + resolved + result[quote_end..]
				}
			}
		}
	}
	return result
}

fn fastc_load_source(path string, prefs &pref.Preferences) FastcLoadedSource {
	source := os.read_file(path) or {
		return FastcLoadedSource{
			path: path
			failed: true
			error_message: err.msg()
		}
	}
	header := fastc_scan_source_header(source, path, prefs) or {
		return FastcLoadedSource{
			path: path
			failed: true
			error_message: err.msg()
		}
	}
	return FastcLoadedSource{
		path: path
		source: source
		header: header
	}
}

fn fastc_resolve_source_files(paths []string, prefs &pref.Preferences) !([]FastcSourceFile, map[string]string) {
	mut timer := fastc_new_phase_timer()
	memo_path := fastc_resolve_memo_path(paths, prefs)
	memo_text := if memo_path != '' { os.read_file(memo_path) or { '' } } else { '' }
	mut memo := fastc_parse_resolve_memo(memo_text)
	if memo.files.len > 0 {
		memo.blob = fastc_read_memo_blob(memo_path, memo)
	}
	builtin_dir := if prefs.building_v {
		os.real_path(prefs.get_vlib_module_path('builtin'))
	} else {
		''
	}
	// vroot is canonical, so a module directory found below its vlib is too and
	// needs no realpath call of its own.
	canonical_vlib := if prefs.building_v { os.dir(builtin_dir) } else { '' }
	// Modules are re-discovered once per importing file; canonicalization and
	// directory enumeration are syscalls, so both are memoized for the walk and
	// shared with the preload below.
	mut real_path_cache := map[string]string{}
	mut module_dir_files := map[string][]string{}
	mut module_path_cache := map[string]string{}
	mut preloaded := map[string]FastcLoadedSource{}
	if memo.files.len > 0 {
		// A memo from an earlier run names everything this entry touched, so
		// all of it is listed, read and looked up in one parallel batch.
		preloaded = fastc_preload_memo(memo, prefs, canonical_vlib, mut module_path_cache, mut module_dir_files)
		timer.mark('resolve.memo_preload')
	}
	mut queue := []FastcQueuedSource{}
	if prefs.building_v {
		for builtin_file in fastc_module_source_files(builtin_dir, prefs, mut module_dir_files) {
			queue << FastcQueuedSource{
				path: builtin_file
				module_name: 'builtin'
				is_canonical: true
				listed: true
			}
		}
	}
	for path in paths {
		entry_path := if prefs.building_v { os.real_path(path) } else { path }
		queue << FastcQueuedSource{
			path: entry_path
			is_canonical: prefs.building_v
		}
		// A V module spans every source file in its directory, plus any `subdirs`
		// its v.mod lists that belong to the same module (e.g. gitly's `ssh/`,
		// `repo/`, ... all declare `module main`). The entry file names only one of
		// them, so gather the rest of the entry module here. Only the real-builtin
		// path does this: the bootstrap runtime compiles single entry files, and its
		// tests place several independent programs in one scratch directory.
		if prefs.building_v {
			for module_file in fastc_entry_module_files(entry_path, prefs) {
				if fastc_source_file_matches_backend(module_file) {
					queue << FastcQueuedSource{
						path: module_file
						is_canonical: true
						listed: true
					}
				}
			}
		}
	}
	timer.mark('resolve.queue_init')
	if memo.files.len == 0 {
		// Every reachable file is read and header-scanned up front on worker
		// threads, following imports as soon as they are known. The wave walk
		// below then only orders and validates what was preloaded, so its
		// dependency depth no longer serializes the file reads.
		preloaded = fastc_preload_sources(queue, prefs, canonical_vlib, mut module_path_cache, mut module_dir_files, mut real_path_cache)
		timer.mark('resolve.preload')
	}
	mut memo_lookup_modules := []string{}
	mut memo_lookup_sources := []string{}
	mut seen := map[string]bool{}
	mut sources := []FastcSourceFile{}
	// path -> the module_name a file was first loaded under, and the resulting alias map:
	// the SAME file reached via a second import name (a module re-exported at another path,
	// e.g. `json2` also importable as `x.json2` via `@[alias]`) records that second name as
	// an alias of the first, so `<second>.<sym>` references resolve to the loaded `<first>`.
	mut path_module := map[string]string{}
	mut module_aliases := map[string]string{}
	mut scheduled_path_modules := map[string]string{}
	mut queue_index := 0
	for queue_index < queue.len {
		// Freeze the current discovery wave. Its files are independent until their
		// headers have been scanned, so load them concurrently and merge in queue order.
		wave_end := queue.len
		mut wave_paths := []string{cap: wave_end - queue_index}
		mut wave_modules := []string{cap: wave_end - queue_index}
		mut pending_alias_paths := []string{}
		mut pending_alias_modules := []string{}
		for queue_index < wave_end {
			queued := queue[queue_index]
			queue_index++
			mut path := ''
			if queued.is_canonical {
				path = queued.path
			} else if cached := real_path_cache[queued.path] {
				path = cached
			} else {
				path = os.real_path(queued.path)
				real_path_cache[queued.path] = path
			}
			if seen[path] {
				loaded := path_module[path] or { '' }
				if queued.module_name != '' && loaded != queued.module_name {
					if loaded != '' {
						module_aliases[queued.module_name] = loaded
					} else {
						// The duplicate belongs to this same wave; resolve its alias after
						// the first occurrence's header has supplied the canonical module.
						pending_alias_paths << path
						pending_alias_modules << queued.module_name
					}
				}
				continue
			}
			if !queued.listed && !os.is_file(path) {
				return error('fastc source file `${path}` does not exist')
			}
			seen[path] = true
			scheduled_path_modules[path] = queued.module_name
			wave_paths << path
			wave_modules << queued.module_name
		}
		timer.mark('resolve.wave_scan')
		loaded_sources := fastc_preloaded_source_headers(wave_paths, prefs, preloaded)
		timer.mark('resolve.wave_load(${wave_paths.len})')
		wave_source_start := sources.len
		for i, loaded_source in loaded_sources {
			if loaded_source.failed {
				return error(loaded_source.error_message)
			}
			path := wave_paths[i]
			queued_module := wave_modules[i]
			mut header := loaded_source.header
			if queued_module != '' {
				expected_module_name := queued_module.all_after_last('.')
				if header.module_name != expected_module_name {
					return error('fastc imported source `${path}` declares module `${header.module_name}` instead of `${expected_module_name}`')
				}
				header = FastcSourceHeader{
					module_name: queued_module
					imports: header.imports
					import_order: header.import_order
					blank_imports: header.blank_imports
					has_globals: header.has_globals
					has_constants: header.has_constants
					has_global_declarations: header.has_global_declarations
					has_interfaces: header.has_interfaces
					has_comptime_if: header.has_comptime_if
					has_type_keywords: header.has_type_keywords
					has_generic_fn_syntax: header.has_generic_fn_syntax
				}
			}
			sources << FastcSourceFile{
				path: path
				source: loaded_source.source
				header: header
			}
			path_module[path] = header.module_name
		}
		for i, alias_path in pending_alias_paths {
			loaded := path_module[alias_path] or { '' }
			alias_module := pending_alias_modules[i]
			if loaded != '' && loaded != alias_module {
				module_aliases[alias_module] = loaded
			}
		}
		for source_file in sources[wave_source_start..] {
			mut discovered_imports := map[string]bool{}
			for imported_module in fastc_header_imported_modules(source_file.header) {
				if discovered_imports[imported_module] {
					continue
				}
				discovered_imports[imported_module] = true
				module_cache_key := fastc_module_cache_key(prefs, source_file.path, imported_module)
				module_dir := fastc_resolve_module_dir(module_cache_key, imported_module, source_file.path, prefs, canonical_vlib, mut module_path_cache)
				memo_lookup_modules << imported_module
				memo_lookup_sources << source_file.path
				if module_dir == '' {
					return error('fastc cannot resolve imported module `${imported_module}` from `${source_file.path}`')
				}
				module_files := fastc_module_source_files(module_dir, prefs, mut module_dir_files)
				for module_file in module_files {
					mut module_file_real := module_file
					if !prefs.building_v {
						if cached := real_path_cache[module_file] {
							module_file_real = cached
						} else {
							module_file_real = os.real_path(module_file)
							real_path_cache[module_file] = module_file_real
						}
					}
					if scheduled_module := scheduled_path_modules[module_file_real] {
						loaded_module := path_module[module_file_real] or { scheduled_module }
						if loaded_module != '' && loaded_module != imported_module {
							module_aliases[imported_module] = loaded_module
						}
						continue
					}
					scheduled_path_modules[module_file_real] = imported_module
					queue << FastcQueuedSource{
						path: module_file_real
						module_name: imported_module
						is_canonical: prefs.building_v
						listed: true
					}
				}
			}
		}
	}
	timer.mark('resolve.imports')
	if memo_path != '' {
		fastc_store_resolve_memo(memo_path, memo_text, sources, builtin_dir, memo_lookup_modules, memo_lookup_sources, prefs, module_path_cache, preloaded)
		timer.mark('resolve.memo_store')
	}
	return sources, module_aliases
}

// FastcResolveMemo records what an earlier resolution of the same entry
// touched: the module directories it listed, the files it loaded and the
// module lookups it made. Only the names are reused: the directories are
// listed and the files read again and the lookups are recomputed, all in one
// parallel batch instead of level by level along the import chain, and the
// ordering walk then replays over that data exactly as it would have without
// the memo (anything it needs that the memo missed is loaded on demand).
struct FastcResolveMemo {
mut:
	dirs           []string
	files          []string
	stamps         []FastcFileStamp
	offsets        []int
	lookup_modules []string
	lookup_sources []string
	// written is the memo's own write time (unix seconds); cached contents
	// are trusted only for files modified at least two seconds before it.
	written i64
	blob_token string
	// blob holds every memoized file's content, each followed by a NUL, when
	// the companion blob file matched the memo; empty otherwise.
	blob string
}

// FastcFileStamp identifies a file version: a cached content is reused only
// when the file's current stamp equals the memoized one.
struct FastcFileStamp {
	size  i64
	mtime i64
	ctime i64
	inode u64
}

fn fastc_same_stamp(a FastcFileStamp, b FastcFileStamp) bool {
	return a.size == b.size && a.mtime == b.mtime && a.ctime == b.ctime && a.inode == b.inode
}

fn fastc_file_stamp(path string) ?FastcFileStamp {
	st := os.stat(path) or { return none }
	return FastcFileStamp{
		size:  i64(st.size)
		mtime: st.mtime
		ctime: st.ctime
		inode: st.inode
	}
}

// fastc_memo_token_width is the fixed width of the token line that starts a
// memo blob, so the file offsets recorded in the memo are absolute and stable.
const fastc_memo_token_width = 40

// fastc_memo_blob_path names the content file that accompanies a memo.
fn fastc_memo_blob_path(memo_path string) string {
	return memo_path + '.src'
}

// fastc_read_memo_blob reads the memo's content blob and returns it when it
// belongs to this memo (same token) and has the expected size.
fn fastc_read_memo_blob(memo_path string, memo FastcResolveMemo) string {
	if memo.blob_token == '' || memo.stamps.len != memo.files.len || memo.offsets.len != memo.files.len {
		return ''
	}
	blob := os.read_file(fastc_memo_blob_path(memo_path)) or { return '' }
	mut expected := fastc_memo_token_width + 1
	for stamp in memo.stamps {
		expected += int(stamp.size) + 1
	}
	if blob.len != expected || !blob.starts_with(memo.blob_token) || blob[fastc_memo_token_width] != `\n` {
		return ''
	}
	return blob
}

// FastcMemoTask is one unit of the memo preload: a module lookup, a directory
// listing or a chunk of file reads.
struct FastcMemoTask {
	kind        int
	module_name string
	source      string
	dir         string
	files       []string
	// For read tasks: the memoized stamps and blob offsets of `files`, and
	// the memo they came from (its blob and trust window).
	stamps  []FastcFileStamp
	offsets []int
	blob    string
	trusted_before i64
}

struct FastcMemoResult {
	index   int
	dir     string
	files   []string
	sources []FastcLoadedSource
	stamps  []FastcFileStamp
}

fn fastc_fnv_hash(seed u64, text string) u64 {
	mut hash := seed
	for i in 0 .. text.len {
		hash ^= u64(text[i])
		hash *= u64(1099511628211)
	}
	hash ^= u64(0xff)
	hash *= u64(1099511628211)
	return hash
}

// fastc_resolve_memo_path names the memo file of an entry set and build
// configuration; it is empty when the memo is disabled.
fn fastc_resolve_memo_path(paths []string, prefs &pref.Preferences) string {
	if os.getenv('V3_FASTC_NO_RESOLVE_MEMO') != '' {
		return ''
	}
	mut hash := u64(14695981039346656037)
	// The format version keeps compilers that write different memo layouts
	// from overwriting each other's memo.
	hash = fastc_fnv_hash(hash, 'fastc-resolve-memo-2')
	hash = fastc_fnv_hash(hash, prefs.vroot)
	hash = fastc_fnv_hash(hash, prefs.target.os)
	hash = fastc_fnv_hash(hash, prefs.target.arch)
	hash = fastc_fnv_hash(hash, if prefs.building_v { 'v' } else { '' })
	for define in prefs.user_defines {
		hash = fastc_fnv_hash(hash, define)
	}
	for path in paths {
		hash = fastc_fnv_hash(hash, os.real_path(path))
	}
	return os.join_path_single(os.vtmp_dir(), 'fastc_resolve_${hash.hex()}.memo')
}

fn fastc_parse_resolve_memo(text string) FastcResolveMemo {
	mut memo := FastcResolveMemo{}
	for line in text.split_into_lines() {
		if line.len < 3 {
			continue
		}
		fields := line[2..].split('\t')
		kind := line[0]
		if kind == `D` {
			memo.dirs << fields[0]
		} else if kind == `F` {
			memo.files << fields[0]
			if fields.len == 6 {
				memo.stamps << FastcFileStamp{
					size:  fields[1].i64()
					mtime: fields[2].i64()
					ctime: fields[3].i64()
					inode: fields[4].u64()
				}
				memo.offsets << fields[5].int()
			}
		} else if kind == `M` && fields.len == 2 {
			memo.lookup_modules << fields[0]
			memo.lookup_sources << fields[1]
		} else if kind == `T` {
			memo.written = fields[0].i64()
		} else if kind == `B` {
			memo.blob_token = fields[0]
		}
	}
	return memo
}

// fastc_memo_tasks turns a memo into preload tasks: lookups and listings
// first, then the file reads in chunks.
fn fastc_memo_tasks(memo FastcResolveMemo) []FastcMemoTask {
	mut tasks := []FastcMemoTask{cap: memo.dirs.len + memo.lookup_modules.len + memo.files.len / fastc_source_load_chunk_size + 1}
	for i, module_name in memo.lookup_modules {
		tasks << FastcMemoTask{
			kind:   0
			module_name: module_name
			source: memo.lookup_sources[i]
		}
	}
	for dir in memo.dirs {
		tasks << FastcMemoTask{
			kind: 1
			dir:  dir
		}
	}
	with_blob := memo.blob.len > 0
	// Files whose content is in the blob only need a stat each, so those
	// chunks are larger.
	chunk := if with_blob { fastc_source_load_chunk_size * 2 } else { fastc_source_load_chunk_size }
	mut start := 0
	for start < memo.files.len {
		mut end := start + chunk
		if end > memo.files.len {
			end = memo.files.len
		}
		mut task := FastcMemoTask{
			kind:  2
			files: memo.files[start..end]
		}
		if with_blob {
			task = FastcMemoTask{
				kind:           2
				files:          memo.files[start..end]
				stamps:         memo.stamps[start..end]
				offsets:        memo.offsets[start..end]
				blob:           memo.blob
				trusted_before: memo.written - 1
			}
		}
		tasks << task
		start = end
	}
	return tasks
}

// fastc_run_memo_task performs one memo preload task with the same helpers
// the ordering walk uses, so the results are what the walk would compute.
fn fastc_run_memo_task(task FastcMemoTask, index int, prefs &pref.Preferences, canonical_vlib string) FastcMemoResult {
	if task.kind == 0 {
		mut local_cache := map[string]string{}
		key := fastc_module_cache_key(prefs, task.source, task.module_name)
		return FastcMemoResult{
			index: index
			dir:   fastc_resolve_module_dir(key, task.module_name, task.source, prefs, canonical_vlib, mut local_cache)
		}
	}
	if task.kind == 1 {
		return FastcMemoResult{
			index: index
			dir:   task.dir
			files: fastc_list_module_sources(task.dir, prefs)
		}
	}
	mut sources := []FastcLoadedSource{cap: task.files.len}
	mut stamps := []FastcFileStamp{cap: task.files.len}
	for i, path in task.files {
		stamp := fastc_file_stamp(path) or {
			sources << fastc_load_source(path, prefs)
			stamps << FastcFileStamp{}
			continue
		}
		stamps << stamp
		if task.blob.len > 0 && fastc_same_stamp(stamp, task.stamps[i]) && stamp.mtime < task.trusted_before {
			// The file is the memoized version: its content is in the blob.
			offset := task.offsets[i]
			text := unsafe { tos(task.blob.str + offset, int(stamp.size)) }.clone()
			sources << fastc_load_source_text(path, text, prefs)
			continue
		}
		sources << fastc_load_source(path, prefs)
	}
	return FastcMemoResult{
		index:   index
		files:   task.files
		sources: sources
		stamps:  stamps
	}
}

// fastc_load_source_text header-scans already read source text.
fn fastc_load_source_text(path string, source string, prefs &pref.Preferences) FastcLoadedSource {
	header := fastc_scan_source_header(source, path, prefs) or {
		return FastcLoadedSource{
			path:          path
			failed:        true
			error_message: err.msg()
		}
	}
	return FastcLoadedSource{
		path:   path
		source: source
		header: header
	}
}

// fastc_apply_memo_results stores the preload results in the walk's caches
// and returns the loaded files keyed by path.
fn fastc_apply_memo_results(tasks []FastcMemoTask, results []FastcMemoResult, prefs &pref.Preferences, mut module_path_cache map[string]string, mut module_dir_files map[string][]string) map[string]FastcLoadedSource {
	mut loaded := map[string]FastcLoadedSource{}
	for index, task in tasks {
		result := results[index]
		if task.kind == 0 {
			module_path_cache[fastc_module_cache_key(prefs, task.source, task.module_name)] = result.dir
		} else if task.kind == 1 {
			module_dir_files[task.dir] = result.files
		} else {
			for i, source in result.sources {
				stamp := if i < result.stamps.len { result.stamps[i] } else { FastcFileStamp{} }
				loaded[task.files[i]] = FastcLoadedSource{
					path:          source.path
					source:        source.source
					header:        source.header
					failed:        source.failed
					error_message: source.error_message
					stamp:         stamp
				}
			}
		}
	}
	return loaded
}

// fastc_store_resolve_memo writes the memo of this resolution when it differs
// from the one read at the start, atomically, so a concurrent compiler sees
// either version.
fn fastc_store_resolve_memo(memo_path string, previous_text string, sources []FastcSourceFile, builtin_dir string, lookup_modules []string, lookup_sources []string, prefs &pref.Preferences, module_path_cache map[string]string, preloaded map[string]FastcLoadedSource) {
	mut out := strings.new_builder(4096)
	mut seen_dirs := map[string]bool{}
	if builtin_dir != '' {
		seen_dirs[builtin_dir] = true
		out.writeln('D\t' + builtin_dir)
	}
	for i, module_name in lookup_modules {
		dir := module_path_cache[fastc_module_cache_key(prefs, lookup_sources[i], module_name)] or { '' }
		if dir == '' || seen_dirs[dir] {
			continue
		}
		seen_dirs[dir] = true
		out.writeln('D\t' + dir)
	}
	// The stamps and the blob layout: the previous memo's token and time are
	// not part of the comparison, so an unchanged program rewrites nothing.
	mut stamp_lines := strings.new_builder(8192)
	mut blob_size := fastc_memo_token_width + 1
	mut stamps := []FastcFileStamp{cap: sources.len}
	for source_file in sources {
		mut stamp := FastcFileStamp{}
		if loaded := preloaded[source_file.path] {
			stamp = loaded.stamp
		}
		if stamp.size == 0 && stamp.mtime == 0 {
			stamp = fastc_file_stamp(source_file.path) or { FastcFileStamp{} }
		}
		stamps << stamp
		stamp_lines.writeln('F\t' + source_file.path + '\t${stamp.size}\t${stamp.mtime}\t${stamp.ctime}\t${stamp.inode}\t${blob_size}')
		blob_size += source_file.source.len + 1
	}
	out.write_string(stamp_lines.str())
	for i, module_name in lookup_modules {
		out.writeln('M\t' + module_name + '\t' + lookup_sources[i])
	}
	body := out.str()
	if body == fastc_memo_body(previous_text) {
		return
	}
	mut blob_token := '${os.getpid()}.${time.now().unix_nano()}'
	for blob_token.len < fastc_memo_token_width {
		blob_token += '.'
	}
	blob_token = blob_token[..fastc_memo_token_width]
	mut blob := strings.new_builder(blob_size)
	blob.writeln(blob_token)
	for source_file in sources {
		blob.write_string(source_file.source)
		blob.write_u8(0)
	}
	blob_path := fastc_memo_blob_path(memo_path)
	staged_blob := '${blob_path}.${os.getpid()}.tmp'
	os.write_file(staged_blob, blob.str()) or { return }
	os.mv(staged_blob, blob_path) or {
		os.rm(staged_blob) or {}
		return
	}
	text := body + 'T\t${time.now().unix()}\nB\t' + blob_token + '\n'
	staged := '${memo_path}.${os.getpid()}.tmp'
	os.write_file(staged, text) or { return }
	os.mv(staged, memo_path) or {
		os.rm(staged) or {}
		return
	}
}

// fastc_memo_body strips the write time and token lines from a memo text, so
// two memos of the same program compare equal.
fn fastc_memo_body(text string) string {
	mut out := strings.new_builder(text.len)
	for line in text.split_into_lines() {
		if line.len >= 2 && (line[0] == `T` || line[0] == `B`) && line[1] == `\t` {
			continue
		}
		out.writeln(line)
	}
	return out.str()
}

// fastc_source_load_chunk_size is the number of files one loader thread reads;
// reads are short syscalls, so a few per spawn amortize the thread start.
const fastc_source_load_chunk_size = 4

// fastc_resolve_module_dir resolves and memoizes the directory of an imported
// module. A directory below the canonical vlib root is canonical already.
// fastc_module_cache_key keys the module directory cache by importing
// directory and module name; when building V the vlib lookup is global.
// (vfmt rewrites '\x00' as '\0', which the FastC parser rejects, so this
// helper stays in a file vfmt does not verify.)
fn fastc_module_cache_key(prefs &pref.Preferences, source_path string, imported_module string) string {
	if prefs.building_v {
		return imported_module
	}
	return os.dir(source_path) + '\x00' + imported_module
}

fn fastc_resolve_module_dir(module_cache_key string, imported_module string, source_path string, prefs &pref.Preferences, canonical_vlib string, mut module_path_cache map[string]string) string {
	if cached := module_path_cache[module_cache_key] {
		return cached
	}
	mut module_dir := ''
	mut canonical := false
	if prefs.building_v {
		vlib_module_dir := prefs.get_vlib_module_path(imported_module)
		// Alias modules need the generic resolver to follow `alias.v`.
		if os.is_dir(vlib_module_dir) && !os.is_file(os.join_path_single(vlib_module_dir, 'alias.v')) {
			module_dir = vlib_module_dir
			canonical = canonical_vlib != '' && module_dir.starts_with(canonical_vlib + '/')
		} else {
			module_dir = prefs.get_module_path(imported_module, source_path)
		}
	} else {
		module_dir = prefs.get_module_path(imported_module, source_path)
	}
	if prefs.building_v && !canonical {
		module_dir = os.real_path(module_dir)
	}
	module_path_cache[module_cache_key] = module_dir
	return module_dir
}

// fastc_module_source_files lists and memoizes the backend-compatible source
// files of a module directory.
fn fastc_module_source_files(module_dir string, prefs &pref.Preferences, mut module_dir_files map[string][]string) []string {
	if cached := module_dir_files[module_dir] {
		return cached.clone()
	}
	module_files := fastc_list_module_sources(module_dir, prefs)
	module_dir_files[module_dir] = module_files
	return module_files
}

// fastc_list_module_sources lists the backend-relevant .v files of `dir`.
fn fastc_list_module_sources(dir string, prefs &pref.Preferences) []string {
	mut module_files := []string{}
	for module_file in pref.get_v_files_from_dir_for_target(dir, prefs.user_defines, prefs.target) {
		if fastc_source_file_matches_backend(module_file) {
			module_files << module_file
		}
	}
	return module_files
}

// fastc_preloaded_source_headers returns the preloaded sources for `paths`,
// reading any that the preload did not reach synchronously.
fn fastc_preloaded_source_headers(paths []string, prefs &pref.Preferences, preloaded map[string]FastcLoadedSource) []FastcLoadedSource {
	mut result := []FastcLoadedSource{cap: paths.len}
	for path in paths {
		if source := preloaded[path] {
			result << source
		} else {
			result << fastc_load_source(path, prefs)
		}
	}
	return result
}

// fastc_entry_module_files returns the other source files of the entry file's
// module: its directory siblings, plus the files of any `subdirs` its v.mod
// declares (which V compiles as part of the same module). The entry file itself
// and duplicates are filtered by the caller's seen-set.
fn fastc_entry_module_files(entry_path string, prefs &pref.Preferences) []string {
	entry_dir := os.dir(os.real_path(entry_path))
	if entry_dir == '' {
		return []string{}
	}
	mut files := pref.get_v_files_from_dir_for_target(entry_dir, prefs.user_defines, prefs.target)
	// Only the module root (where v.mod lives) pulls in the declared subdirs, so
	// an entry file already inside a subdir does not re-expand the whole project.
	vmod_root := fastc_vmod_root_for_file(entry_path)
	if vmod_root != '' && os.real_path(vmod_root) == entry_dir {
		for subdir in fastc_vmod_subdirs(vmod_root) {
			subdir_path := os.join_path(vmod_root, subdir)
			if os.is_dir(subdir_path) {
				files << pref.get_v_files_from_dir_for_target(subdir_path, prefs.user_defines, prefs.target)
			}
		}
	}
	return files
}

// fastc_vmod_subdirs extracts the `subdirs: ['a', 'b']` list from a v.mod file.
fn fastc_vmod_subdirs(vmod_root string) []string {
	content := os.read_file(os.join_path(vmod_root, 'v.mod')) or { return []string{} }
	subdirs_index := content.index('subdirs') or { return []string{} }
	rest := content[subdirs_index..]
	open_bracket := rest.index('[') or { return []string{} }
	close_bracket := rest.index(']') or { return []string{} }
	if close_bracket <= open_bracket {
		return []string{}
	}
	mut subdirs := []string{}
	for part in rest[open_bracket + 1..close_bracket].split(',') {
		name := part.trim_space().trim("'").trim('"')
		if name != '' {
			subdirs << name
		}
	}
	return subdirs
}

fn fastc_header_imported_modules(header FastcSourceHeader) []string {
	if header.import_order.len > 0 {
		return header.import_order.clone()
	}
	mut fallback := header.imports.values()
	fallback << header.blank_imports
	return fallback
}

fn fastc_sources_in_dependency_order(sources []FastcSourceFile) ![]FastcSourceFile {
	mut module_order := []string{}
	for source_file in sources {
		module_name := source_file.header.module_name
		if module_name !in module_order {
			module_order << module_name
		}
	}
	mut visiting := []string{}
	mut visited := []string{}
	mut ordered := []FastcSourceFile{cap: sources.len}
	for module_name in module_order {
		fastc_append_module_sources(module_name, sources, mut visiting, mut visited, mut ordered)!
	}
	return ordered
}

// fastc_module_init_calls lists the `init` hooks of `ordered_sources`, which
// must already be in module dependency order.
fn fastc_module_init_calls(ordered_sources []FastcSourceFile, functions map[string]FastcFunctionSignature) ![]string {
	return fastc_module_lifecycle_calls(ordered_sources, functions, 'init', false)
}

fn fastc_module_cleanup_calls(ordered_sources []FastcSourceFile, functions map[string]FastcFunctionSignature) ![]string {
	return fastc_module_lifecycle_calls(ordered_sources, functions, 'cleanup', true)
}

fn fastc_module_lifecycle_calls(ordered_sources []FastcSourceFile, functions map[string]FastcFunctionSignature, hook_name string, reverse bool) ![]string {
	mut seen_modules := map[string]bool{}
	mut ordered_modules := []string{}
	for source_file in ordered_sources {
		module_name := source_file.header.module_name
		if seen_modules[module_name] {
			continue
		}
		seen_modules[module_name] = true
		ordered_modules << module_name
	}
	modules := if reverse { ordered_modules.reverse() } else { ordered_modules }
	mut calls := []string{}
	for module_name in modules {
		function_key := fastc_function_key(module_name, hook_name)
		if signature := functions[function_key] {
			if signature.parameter_types.len > 0 {
				return error('fastc parser does not support module `${hook_name}` with parameters in ${signature.path}')
			}
			calls << fastc_c_function_name(module_name, hook_name)
		}
	}
	return calls
}

fn fastc_generate_startup_initializers(ordered_sources []FastcSourceFile, constant_initializers map[string]string, global_initializers map[string]string, module_init_calls []string) !string {
	mut seen_modules := map[string]bool{}
	mut out := strings.new_builder(1024)
	for source_file in ordered_sources {
		module_name := source_file.header.module_name
		if seen_modules[module_name] {
			continue
		}
		seen_modules[module_name] = true
		constant_initializer := constant_initializers[module_name] or { '' }
		global_initializer := global_initializers[module_name] or { '' }
		out.write_string(constant_initializer)
		out.write_string(global_initializer)
		init_call := fastc_c_function_name(module_name, 'init')
		if init_call in module_init_calls {
			out.writeln('\t${init_call}();')
		}
	}
	return out.str()
}

fn fastc_append_module_sources(module_name string, sources []FastcSourceFile, mut visiting []string, mut visited []string, mut ordered []FastcSourceFile) ! {
	if module_name in visited {
		return
	}
	if module_name in visiting {
		return error('fastc parser does not support cyclic module dependency involving `${module_name}`')
	}
	visiting << module_name
	mut dependencies := []string{}
	for source_file in sources {
		if source_file.header.module_name != module_name {
			continue
		}
		for dependency in fastc_header_imported_modules(source_file.header) {
			if dependency != module_name && dependency !in dependencies {
				dependencies << dependency
			}
		}
	}
	for dependency in dependencies {
		fastc_append_module_sources(dependency, sources, mut visiting, mut visited, mut ordered)!
	}
	for source_file in sources {
		if source_file.header.module_name == module_name {
			ordered << source_file
		}
	}
	visiting.delete(visiting.len - 1)
	visited << module_name
}

fn fastc_source_file_matches_backend(path string) bool {
	return !path.ends_with('.arm64.v') && !path.ends_with('.amd64.v') && !path.ends_with('.native.v') && !path.ends_with('.wasm.v') && !path.ends_with('.rv64.v') && !path.ends_with('.js.v')
}

fn fastc_scan_source_header(source string, path string, prefs &pref.Preferences) !FastcSourceHeader {
	file := token.File.unindexed(path, source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut module_name := ''
	mut imports := map[string]string{}
	mut import_order := []string{}
	mut blank_imports := []string{}
	mut has_globals := false
	mut brace_depth := 0
	mut tok := scan.scan()
	for tok != .eof {
		if module_name == '' && tok == .attribute {
			mut attribute_depth := 1
			tok = scan.scan()
			for attribute_depth > 0 && tok != .eof {
				if tok == .name && scan.lit == 'has_globals' {
					has_globals = true
				}
				if tok == .lsbr {
					attribute_depth++
				} else if tok == .rsbr {
					attribute_depth--
				}
				tok = scan.scan()
			}
			continue
		}
		if module_name == '' && tok == .key_module {
			tok = scan.scan()
			if tok != .name {
				return error('fastc parser does not support module declaration in ${path}')
			}
			module_name = scan.lit
			tok = scan.scan()
			continue
		}
		if brace_depth == 0 && tok == .dollar {
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				if selected.source != '' {
					selected_header := fastc_scan_source_header(selected.source, path, prefs)!
					fastc_merge_source_header_imports(selected_header, path, mut imports, mut import_order, mut blank_imports)!
					has_globals = has_globals || selected_header.has_globals
				}
				tok = selected.tok
				continue
			}
		}
		if brace_depth == 0 && tok in [.key_fn, .key_struct, .key_enum, .key_interface, .key_type,
			.key_const, .key_global] {
			break
		}
		if tok != .key_import || brace_depth > 0 {
			if tok == .lcbr {
				brace_depth++
			} else if tok == .rcbr && brace_depth > 0 {
				brace_depth--
			}
			tok = scan.scan()
			continue
		}
		tok = scan.scan()
		if tok == .lpar {
			tok = scan.scan()
			for tok != .rpar && tok != .eof {
				if tok == .semicolon || tok == .comma {
					tok = scan.scan()
					continue
				}
				import_path, alias, selected_names, next_token := fastc_scan_import(mut scan, tok, path)!
				fastc_register_import_alias(import_path, alias, path, mut imports, mut blank_imports)!
				fastc_register_selective_imports(import_path, selected_names, path, mut imports)!
				if import_path !in import_order {
					import_order << import_path
				}
				tok = next_token
			}
			if tok == .rpar {
				tok = scan.scan()
			}
			continue
		}
		import_path, alias, selected_names, next_token := fastc_scan_import(mut scan, tok, path)!
		fastc_register_import_alias(import_path, alias, path, mut imports, mut blank_imports)!
		fastc_register_selective_imports(import_path, selected_names, path, mut imports)!
		if import_path !in import_order {
			import_order << import_path
		}
		tok = next_token
	}
	if module_name == '' {
		module_name = 'main'
	}
	// V auto-injects `import orm` for files that use `sql <conn> { ... }` blocks.
	// Mirror that so the ORM lowering resolves `orm.Table`/`orm.QueryData`/... and the
	// `orm` module is pulled into the source set. Only the real-builtin path has the
	// runtime the ORM needs; the toy runtime cannot compile `orm`.
	// The compiler's own sources never use the ORM, and the probe rescans every
	// file that mentions `sql` anywhere (FastC's ORM lowering does), so self-host
	// builds skip it.
	if prefs.building_v && !prefs.selfhost && module_name != 'orm' && 'orm' !in imports && fastc_source_uses_sql(source, prefs) {
		imports['orm'] = 'orm'
		if 'orm' !in import_order {
			import_order << 'orm'
		}
	}
	if prefs.building_v && prefs.backend == 'fastc' && imports['driver'] == 'v3.driver' && 'fastcdriver' in imports {
		fastcdriver_module := imports['fastcdriver']
		imports['driver'] = fastcdriver_module
		for i, imported_module in import_order {
			if imported_module == 'v3.driver' {
				import_order[i] = fastcdriver_module
			}
		}
	}
	// The declaration keyword flags are filled in by fastc_collect_generic_method_sources,
	// the first parallel pass, so discovery waves only scan imports.
	return FastcSourceHeader{
		module_name: module_name
		imports: imports
		import_order: import_order
		blank_imports: blank_imports
		has_globals: has_globals
	}
}

// fastc_header_with_scan_flags copies a header with the declaration keyword
// flags of its source filled in.
// fastc_apply_scan_flags stores per-file scan flags into the source headers
// starting at `start`.
fn fastc_apply_scan_flags(mut sources []FastcSourceFile, flags []FastcSourceScanFlags, start int) {
	for i, file_flags in flags {
		source_file := sources[start + i]
		sources[start + i] = FastcSourceFile{
			path: source_file.path
			source: source_file.source
			source_offset: source_file.source_offset
			header: fastc_header_with_scan_flags(source_file.header, file_flags)
		}
	}
}

fn fastc_header_with_scan_flags(header FastcSourceHeader, flags FastcSourceScanFlags) FastcSourceHeader {
	return FastcSourceHeader{
		module_name: header.module_name
		imports: header.imports
		import_order: header.import_order
		blank_imports: header.blank_imports
		has_globals: header.has_globals
		has_constants: flags.has_constants
		has_global_declarations: flags.has_global_declarations
		has_interfaces: flags.has_interfaces
		has_comptime_if: flags.has_comptime_if
		has_type_keywords: flags.has_type_keywords
		has_generic_fn_syntax: flags.has_generic_fn_syntax
	}
}

// FastcSourceScanFlags records which declaration keywords a source mentions
// anywhere in its bytes (comments and strings included), so later collection
// passes can skip files that cannot contain the declarations they look for.
struct FastcSourceScanFlags {
mut:
	has_constants           bool
	has_global_declarations bool
	has_interfaces          bool
	has_comptime_if         bool
	has_type_keywords       bool
	has_generic_fn_syntax   bool
}

@[direct_array_access]
fn fastc_source_word_at(source string, i int, word string) bool {
	if i + word.len > source.len {
		return false
	}
	for j := 0; j < word.len; j++ {
		if source[i + j] != word[j] {
			return false
		}
	}
	if i > 0 && fastc_identifier_byte(source[i - 1]) {
		return false
	}
	return i + word.len == source.len || !fastc_identifier_byte(source[i + word.len])
}

@[direct_array_access]
fn fastc_source_space_byte(c u8) bool {
	return c == ` ` || c == `\t` || c == `\r` || c == `\n`
}

// fastc_source_skip_blank returns the offset of the first byte at or after
// `start` that is neither whitespace nor part of a comment. The scanner treats
// comments as whitespace between tokens, so the byte probes must too, or a
// comment between two tokens would turn a superset test into a false negative.
@[direct_array_access]
fn fastc_source_skip_blank(source string, start int) int {
	mut j := start
	for j < source.len {
		c := source[j]
		if fastc_source_space_byte(c) {
			j++
		} else if c == `/` && j + 1 < source.len && source[j + 1] == `/` {
			for j < source.len && source[j] != `\n` {
				j++
			}
		} else if c == `/` && j + 1 < source.len && source[j + 1] == `*` {
			// Block comments nest, as in the scanner.
			mut depth := 1
			j += 2
			for j < source.len && depth > 0 {
				if source[j] == `/` && j + 1 < source.len && source[j + 1] == `*` {
					depth++
					j += 2
				} else if source[j] == `*` && j + 1 < source.len && source[j + 1] == `/` {
					depth--
					j += 2
				} else {
					j++
				}
			}
		} else {
			break
		}
	}
	return j
}

// fastc_source_comptime_if_at reports whether `$` at `i` starts a `$if`.
@[direct_array_access]
fn fastc_source_comptime_if_at(source string, i int) bool {
	return fastc_source_word_at(source, fastc_source_skip_blank(source, i + 1), 'if')
}

// fastc_source_generic_fn_at reports whether the `fn` keyword ending at `i`
// could declare a function or method with its own type parameter, i.e. is
// followed by an optional receiver clause and a name that a `[` follows.
@[direct_array_access]
fn fastc_source_generic_fn_at(source string, i int) bool {
	mut j := fastc_source_skip_blank(source, i)
	if j < source.len && source[j] == `(` {
		j++
		for {
			j = fastc_source_skip_blank(source, j)
			if j >= source.len {
				return false
			}
			if source[j] == `)` {
				break
			}
			j++
		}
		j = fastc_source_skip_blank(source, j + 1)
	}
	name_start := j
	for j < source.len && (fastc_identifier_byte(source[j]) || source[j] == `.`) {
		j++
	}
	if j == name_start {
		return false
	}
	j = fastc_source_skip_blank(source, j)
	return j < source.len && source[j] == `[`
}

// fastc_source_scan_flags computes every header flag in one pass over the
// bytes. It runs on every file of every discovery wave, so it must stay cheap
// for large files.
@[direct_array_access]
fn fastc_source_scan_flags(source string) FastcSourceScanFlags {
	mut flags := FastcSourceScanFlags{}
	for i := 0; i < source.len; i++ {
		c := source[i]
		// Only a word start can begin a keyword, so after checking one, skip
		// the rest of the identifier; the byte that ends it cannot start a
		// keyword either, since its previous byte is part of the word.
		if fastc_identifier_byte(c) {
			mut end := i + 1
			for end < source.len && fastc_identifier_byte(source[end]) {
				end++
			}
			if !flags.has_constants || !flags.has_type_keywords || !flags.has_global_declarations
				|| !flags.has_interfaces || !flags.has_generic_fn_syntax {
				fastc_source_scan_word_flags(source, i, c, mut flags)
			}
			i = end
			continue
		}
		if c == `$` {
			if !flags.has_comptime_if && fastc_source_comptime_if_at(source, i) {
				flags.has_comptime_if = true
			}
		}
	}
	return flags
}

// fastc_source_scan_word_flags checks the identifier starting at `i` (whose
// first byte is `c`) for the declaration keywords the flags track.
@[direct_array_access]
fn fastc_source_scan_word_flags(source string, i int, c u8, mut flags FastcSourceScanFlags) {
	if c == `c` {
		if !flags.has_constants && fastc_source_word_at(source, i, 'const') {
			flags.has_constants = true
		}
	} else if c == `_` {
		if !flags.has_global_declarations && fastc_source_word_at(source, i, '__global') {
			flags.has_global_declarations = true
		}
	} else if c == `i` {
		if !flags.has_interfaces && fastc_source_word_at(source, i, 'interface') {
			flags.has_interfaces = true
			flags.has_type_keywords = true
		}
	} else if c == `s` {
		if !flags.has_type_keywords && fastc_source_word_at(source, i, 'struct') {
			flags.has_type_keywords = true
		}
	} else if c == `e` {
		if !flags.has_type_keywords && fastc_source_word_at(source, i, 'enum') {
			flags.has_type_keywords = true
		}
	} else if c == `t` {
		if !flags.has_type_keywords && fastc_source_word_at(source, i, 'type') {
			flags.has_type_keywords = true
		}
	} else if c == `u` {
		if !flags.has_type_keywords && fastc_source_word_at(source, i, 'union') {
			flags.has_type_keywords = true
		}
	} else if c == `f` {
		if !flags.has_generic_fn_syntax && fastc_source_word_at(source, i, 'fn')
			&& fastc_source_generic_fn_at(source, i + 2) {
			flags.has_generic_fn_syntax = true
		}
	}
}

fn fastc_identifier_byte(value u8) bool {
	return value == `_` || (value >= `a` && value <= `z`) || (value >= `A` && value <= `Z`) || (value >= `0` && value <= `9`)
}

// fastc_source_uses_sql reports whether a file contains a `sql <conn> { ... }` ORM
// block (statement or expression form). `sql` is not a keyword, so it is matched as a
// name not preceded by `.` and followed by a connection expression and then `{`.
fn fastc_source_uses_sql(source string, prefs &pref.Preferences) bool {
	if !source.contains('sql') {
		return false
	}
	file := token.File.unindexed('orm_sql_probe', source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut previous := token.Token.eof
	mut tok := scan.scan()
	for tok != .eof {
		if tok == .name && scan.lit == 'sql' && previous != .dot {
			mut look := scan
			mut next := look.scan()
			if next == .name {
				// Scan the connection expression (`db`, `app.db`, ...) to its `{`.
				for next !in [token.Token.eof, .lcbr, .semicolon, .rcbr] {
					next = look.scan()
				}
				if next == .lcbr {
					return true
				}
			}
		}
		previous = tok
		tok = scan.scan()
	}
	return false
}

fn fastc_merge_source_header_imports(header FastcSourceHeader, path string, mut destination_imports map[string]string, mut destination_import_order []string, mut destination_blank_imports []string) ! {
	for alias, imported_module in header.imports {
		if alias.starts_with('#select#') {
			fastc_register_selective_imports(imported_module, [
				alias['#select#'.len..],
			], path, mut destination_imports)!
		} else {
			fastc_register_import_alias(imported_module, alias, path, mut destination_imports, mut destination_blank_imports)!
		}
	}
	for imported_module in header.blank_imports {
		fastc_register_import_alias(imported_module, '_', path, mut destination_imports, mut destination_blank_imports)!
	}
	for imported_module in header.import_order {
		if imported_module !in destination_import_order {
			destination_import_order << imported_module
		}
	}
}

fn fastc_register_import_alias(import_path string, alias string, path string, mut imports map[string]string, mut blank_imports []string) ! {
	if alias == '_' {
		blank_imports << import_path
		return
	}
	if existing_module := imports[alias] {
		if existing_module != import_path {
			return error('fastc parser cannot reuse import alias `${alias}` for `${import_path}` after `${existing_module}` in ${path}')
		}
	}
	imports[alias] = import_path
}

fn fastc_scan_import(mut scan scanner.Scanner, first token.Token, path string) !(string, string, []string, token.Token) {
	mut tok := first
	if tok != .name {
		return error('fastc parser does not support import `${tok.str()}` in ${path}')
	}
	mut parts := [scan.lit]
	tok = scan.scan()
	for tok == .dot {
		tok = scan.scan()
		if tok != .name {
			return error('fastc parser does not support import path in ${path}')
		}
		parts << scan.lit
		tok = scan.scan()
	}
	mut alias := parts.last()
	if tok == .key_as {
		tok = scan.scan()
		if tok != .name {
			return error('fastc parser does not support import alias in ${path}')
		}
		alias = scan.lit
		tok = scan.scan()
	}
	mut selected_names := []string{}
	if tok == .lcbr {
		mut depth := 1
		for depth > 0 {
			tok = scan.scan()
			if tok == .eof {
				return error('fastc parser does not support unfinished selective import in ${path}')
			}
			if tok == .lcbr {
				depth++
			} else if tok == .rcbr {
				depth--
			} else if depth == 1 && tok == .name {
				selected_names << scan.lit
			}
		}
		tok = scan.scan()
	}
	return parts.join('.'), alias, selected_names, tok
}

fn fastc_selective_import_key(name string) string {
	return '#select#${name}'
}

fn fastc_register_selective_imports(import_path string, selected_names []string, path string, mut imports map[string]string) ! {
	for name in selected_names {
		key := fastc_selective_import_key(name)
		if existing_module := imports[key] {
			if existing_module != import_path {
				return error('fastc parser cannot resolve ambiguous selective import `${name}` in ${path}')
			}
		}
		imports[key] = import_path
	}
}
