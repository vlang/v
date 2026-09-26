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
