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

// record_listed_source_path is record_source_path for a path whose last
// component is a directory entry spelled the way the listing of its directory
// returned it. The resolved form of such a path is the resolved directory
// followed by the entry, unless the entry is itself a link, so one os.real_path
// per directory answers for all of its regular files.
pub fn (mut a FlatAst) record_listed_source_path(path string) string {
	if a.source_paths_frozen {
		return a.real_source_path(path)
	}
	if resolved := a.resolved_source_paths[path] {
		return resolved
	}
	resolved := a.resolve_listed_source_path(path)
	a.resolved_source_paths[path] = resolved
	return resolved
}

fn (mut a FlatAst) resolve_listed_source_path(path string) string {
	$if windows {
		return os.real_path(path)
	}
	sep := path.last_index_u8(`/`)
	if sep <= 0 || sep == path.len - 1 {
		return os.real_path(path)
	}
	name := path[sep + 1..]
	if name == '.' || name == '..' {
		return os.real_path(path)
	}
	// A link, or anything else that is not a plain file, resolves the slow way.
	attr := os.lstat(path) or { return os.real_path(path) }
	if attr.get_filetype() != .regular {
		return os.real_path(path)
	}
	real_dir := a.record_source_path(path[..sep])
	if real_dir.len == 0 || real_dir[0] != `/` {
		return os.real_path(path)
	}
	if real_dir.len == 1 {
		return '/' + name
	}
	return real_dir + '/' + name
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
