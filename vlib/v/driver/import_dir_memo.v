@[has_globals]
module driver

import os
import v.flat
import v.pref
import v.workers

// ImportDirMemo is what the wave loop of resolve_imports learned of the
// directories it searched. Resolving one import lists the module's directory to
// tell that it holds V sources, the wave prediction lists it for the files it
// expects, and the resolver lists it once more for the files it parses; every
// probe of an ancestor or a sibling directory resolves the same few paths again.
// The loop reads no directory that its own parses change, and it runs with one
// set of defines, so each answer holds for as long as the loop runs.
@[heap]
struct ImportDirMemo {
mut:
	v_files     map[string][]string
	real_paths  map[string]string
	has_sources map[string]bool
	symlinks    map[string]flat.ListedSymlinks
}

// The memo of the running wave loop, or nil outside of one. Only the thread
// that runs resolve_imports reads and writes it: the pool tasks that the loop
// starts (parse chunks, header scans) resolve no module directory.
__global v3_import_dir_memo &ImportDirMemo

// begin_import_dir_memo starts the memo of a wave loop.
fn begin_import_dir_memo() {
	v3_import_dir_memo = &ImportDirMemo{}
}

// end_import_dir_memo drops the memo: what follows the wave loop reads the
// directories again.
fn end_import_dir_memo() {
	v3_import_dir_memo = unsafe { nil }
}

// module_dir_v_files returns the V sources of `dir` that this build compiles.
// Callers only read the list.
fn module_dir_v_files(prefs &pref.Preferences, dir string) []string {
	if isnil(v3_import_dir_memo) {
		return prefs.without_excluded(pref.get_v_files_from_dir_for_backend_target(dir, prefs.user_defines,
			prefs.backend, prefs.target))
	}
	if files := v3_import_dir_memo.v_files[dir] {
		return files
	}
	files := prefs.without_excluded(pref.get_v_files_from_dir_for_backend_target(dir, prefs.user_defines,
		prefs.backend, prefs.target))
	v3_import_dir_memo.v_files[dir] = files
	return files
}

// import_dir_real_path is os.real_path for the paths that import resolution
// compares.
fn import_dir_real_path(path string) string {
	if isnil(v3_import_dir_memo) {
		return os.real_path(path)
	}
	if real := v3_import_dir_memo.real_paths[path] {
		return real
	}
	real := os.real_path(path)
	v3_import_dir_memo.real_paths[path] = real
	return real
}

// module_dir_symlinks is flat.scan_listed_symlinks for a module directory.
fn module_dir_symlinks(dir string) flat.ListedSymlinks {
	if isnil(v3_import_dir_memo) {
		return flat.scan_listed_symlinks(dir)
	}
	if listed := v3_import_dir_memo.symlinks[dir] {
		return listed
	}
	listed := flat.scan_listed_symlinks(dir)
	v3_import_dir_memo.symlinks[dir] = listed
	return listed
}

// ImportWarm is an import that a wave is about to resolve, and what resolving it
// reads from the file system: the task of import_warm_thread.
struct ImportWarm {
	prefs          voidptr // &pref.Preferences
	path           string
	importing_file string
	project_root   string
mut:
	// What resolving the module and its identity looked up, as the resolver's
	// path cache holds it.
	lookups    map[string]string
	dir        string
	found      bool
	real_dir   string
	files      []string
	source_dir string
	// Of source_dir, when the module's sources are not in `dir` itself.
	source_real_dir string
	source_files    []string
	symlinks        flat.ListedSymlinks
	identity        string
}

// import_warm_thread resolves one import the way the wave loop does, with a path
// cache of its own, and reads what the loop then asks of the module's directory.
// It runs while the memo of the loop is set aside, so it shares nothing.
fn import_warm_thread(arg voidptr) voidptr {
	mut warm := unsafe { &ImportWarm(arg) }
	prefs := unsafe { &pref.Preferences(warm.prefs) }
	warm.dir = resolve_project_or_pref_module_path(prefs, warm.path, warm.importing_file,
		warm.project_root, mut warm.lookups)
	if warm.dir.len == 0 || !os.is_dir(warm.dir) {
		return unsafe { nil }
	}
	warm.found = true
	warm.real_dir = os.real_path(warm.dir)
	warm.files = prefs.without_excluded(pref.get_v_files_from_dir_for_backend_target(warm.dir, prefs.user_defines,
		prefs.backend, prefs.target))
	warm.source_dir = v3_directory_source_root_of(warm.dir, warm.real_dir)
	if warm.source_dir != warm.dir {
		warm.source_real_dir = os.real_path(warm.source_dir)
		warm.source_files = prefs.without_excluded(pref.get_v_files_from_dir_for_backend_target(warm.source_dir,
			prefs.user_defines, prefs.backend, prefs.target))
	}
	warm.symlinks = flat.scan_listed_symlinks(warm.source_dir)
	warm.identity = import_module_identity_with_path_cache(prefs, warm.path, warm.importing_file,
		warm.project_root, warm.dir, mut warm.lookups)
	return unsafe { nil }
}

// import_warm_min_requests is the number of imports from which the pool resolves
// them ahead of the wave loop.
const import_warm_min_requests = 4

// warm_import_resolutions resolves the imports `requests` across the worker pool
// and leaves what that read in the caches of the wave loop: the path cache, the
// identity cache and the directory memo. The loop then finds there what it would
// have read from the file system itself, one import after another: the answers
// are the same, since each task asks the questions of the loop for its import
// and nothing changes the directories meanwhile. The tasks use caches of their
// own; the memo, which only the loop's thread may touch, is set aside while they
// run. Only a build of the compiler's own tree asks for this: nothing there
// reads the path cache afterwards (a diagnostics server would), so the lookups
// that a task adds to it ahead of the loop are never seen.
fn warm_import_resolutions(a &flat.FlatAst, prefs &pref.Preferences, requests []EagerSelfhostImport, project_root string, mut module_path_cache map[string]string, mut module_identity_cache map[string]string) {
	if requests.len < import_warm_min_requests || isnil(v3_import_dir_memo)
		|| isnil(a.worker_pool) || a.worker_pool.size() == 0 {
		return
	}
	mut warms := []ImportWarm{cap: requests.len}
	for request in requests {
		warms << ImportWarm{
			prefs:          voidptr(prefs)
			path:           request.path
			importing_file: request.importing_file
			project_root:   project_root
		}
	}
	mut tasks := []workers.Task{cap: warms.len}
	for i in 0 .. warms.len {
		tasks << workers.Task{
			run:        import_warm_thread
			arg:        unsafe { voidptr(&warms[i]) }
			force_sync: i == 0
		}
	}
	mut memo := v3_import_dir_memo
	v3_import_dir_memo = unsafe { nil }
	a.worker_pool.run(tasks)
	v3_import_dir_memo = memo
	for warm in warms {
		for key, value in warm.lookups {
			if key !in module_path_cache {
				module_path_cache[key] = value
			}
		}
		importing_dir := if warm.importing_file.len > 0 { os.dir(warm.importing_file) } else { '' }
		path_key := '${importing_dir}\n${warm.path}'
		if path_key !in module_path_cache {
			module_path_cache[path_key] = warm.dir
		}
		if !warm.found {
			continue
		}
		if warm.dir !in memo.v_files {
			memo.v_files[warm.dir] = warm.files
		}
		if warm.dir !in memo.real_paths {
			memo.real_paths[warm.dir] = warm.real_dir
		}
		if warm.source_dir != warm.dir {
			if warm.source_dir !in memo.v_files {
				memo.v_files[warm.source_dir] = warm.source_files
			}
			if warm.source_dir !in memo.real_paths {
				memo.real_paths[warm.source_dir] = warm.source_real_dir
			}
		}
		if warm.source_dir !in memo.symlinks {
			memo.symlinks[warm.source_dir] = warm.symlinks
		}
		identity_key := '${importing_dir}\n${warm.path}\n${warm.dir}'
		if identity_key !in module_identity_cache {
			module_identity_cache[identity_key] = warm.identity
		}
	}
}
