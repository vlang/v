module flat

import os

// record_source_path returns os.real_path(path) through the table of resolved
// source paths. Until resolve_source_paths freezes the table it also adds the
// missing answers, so only the thread that owns the AST may call it, outside any
// disposable allocation scope. Once the table is frozen it only reads it, like
// real_source_path.
pub fn (mut a FlatAst) record_source_path(path string) string {
	if a.source_paths_frozen {
		return a.real_source_path(path)
	}
	if resolved := a.resolved_source_paths[path] {
		return resolved
	}
	resolved := os.real_path(path)
	a.resolved_source_paths[path] = resolved
	return resolved
}

// record_listed_source_path is record_source_path for `path`, an entry of the
// directory listing that `real_dir` is the os.real_path of. Unless the entry is
// a symbolic link, os.real_path(path) is `real_dir` joined with the entry's own
// name, which one lstat confirms: resolving every listed file anew walked all of
// its parent directories once per file.
pub fn (mut a FlatAst) record_listed_source_path(path string, real_dir string) string {
	if a.source_paths_frozen {
		return a.real_source_path(path)
	}
	if resolved := a.resolved_source_paths[path] {
		return resolved
	}
	resolved := listed_real_path(path, real_dir)
	a.resolved_source_paths[path] = resolved
	return resolved
}

// listed_real_path returns os.real_path(path) for an entry of the directory
// that `real_dir` is the os.real_path of.
fn listed_real_path(path string, real_dir string) string {
	$if windows {
		return os.real_path(path)
	} $else {
		base := os.file_name(path)
		if base in ['', '.', '..'] || real_dir.len == 0 || !os.is_abs_path(real_dir) {
			return os.real_path(path)
		}
		st := os.lstat(path) or { return os.real_path(path) }
		if st.get_filetype() == .symbolic_link {
			return os.real_path(path)
		}
		return if real_dir.ends_with(os.path_separator) {
			real_dir + base
		} else {
			real_dir + os.path_separator + base
		}
	}
}

// resolve_source_paths completes the table of resolved source paths with every
// parsed source file, reusing the answers record_source_path already recorded,
// and freezes it: nothing adds to it afterwards, including another call to
// resolve_source_paths. Call it on the thread that owns the AST, outside any
// disposable allocation scope and before other threads read the AST.
pub fn (mut a FlatAst) resolve_source_paths() {
	if a.source_paths_frozen {
		return
	}
	for _, file in a.source_files {
		if file.name.len > 0 && file.name !in a.resolved_source_paths {
			a.resolved_source_paths[file.name] = os.real_path(file.name)
		}
	}
	a.source_paths_frozen = true
}

// real_source_path returns the same result as os.real_path(path), answering
// from the table of resolved source paths when it can. It never adds to the
// table, so any thread may call it once resolve_source_paths has frozen it.
pub fn (a &FlatAst) real_source_path(path string) string {
	if resolved := a.resolved_source_paths[path] {
		return resolved
	}
	return os.real_path(path)
}
