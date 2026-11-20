@[has_globals]
module driver

import os
import v.pref

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
