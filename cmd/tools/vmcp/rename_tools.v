// The rename implementation.
//
// A rename is planned first and applied second, and the plan is what makes the
// tool safe: the caller sees every position that will change, and the write is
// refused outright if the file no longer lines up with the plan.
module main

import os
import v.astjson
import v.astquery

// RenameHit is one place a rename has to write.
pub struct RenameHit {
pub:
	line   int
	column int
	// length is how many characters the old name occupies at that position.
	length int
}

// rename_targets resolves the files a rename applies to, defaulting to every V
// file of the project.
fn (ws &Workspace) rename_targets(requested []string) ![]string {
	if requested.len == 0 {
		return ws.v_files(ws.root)
	}
	mut out := []string{}
	for file in requested {
		out << ws.resolve(file)!
	}
	return out
}

// rename_hits returns every place in `file` where `name` is really mentioned.
//
// The hits come back in the order they must be applied: later lines first, and
// within a line, later columns first. A shorter new name shifts every position
// after it, so applying them in this order keeps the recorded columns valid.
fn rename_hits(file string, name string) []RenameHit {
	mut hits := []RenameHit{}
	mut seen := map[string]bool{}
	for occ in astquery.references(astquery.parse(file), name) {
		key := '${occ.line}:${occ.column}'
		if seen[key] {
			continue
		}
		seen[key] = true
		hits << RenameHit{
			line:   occ.line
			column: occ.column
			length: name.len
		}
	}
	sort_hits_last_first(mut hits)
	return hits
}

// sort_hits_last_first orders hits so that applying them one by one never
// invalidates a position that has not been reached yet.
fn sort_hits_last_first(mut hits []RenameHit) {
	mut ordered := []RenameHit{cap: hits.len}
	mut remaining := hits.clone()
	for remaining.len > 0 {
		mut best := 0
		for i in 1 .. remaining.len {
			if comes_last(remaining[i], remaining[best]) {
				best = i
			}
		}
		ordered << remaining[best]
		remaining.delete(best)
	}
	hits = ordered.clone()
}

// comes_last reports whether `a` sits after `b` in the file.
fn comes_last(a RenameHit, b RenameHit) bool {
	return a.line > b.line || (a.line == b.line && a.column > b.column)
}

// apply_rename returns the contents of `file` with every hit replaced by
// `new_name`.
//
// A hit that no longer fits its line is a hard failure rather than a silent skip:
// the file changed between the plan and the write, and writing half of the rename
// would leave the project in a state neither the caller nor the compiler expects.
//
// The text at the span must still be `name`. Without that check a column that
// drifted off the name writes over whatever happens to be there, which is how a
// method rename turned `fn (h Host) hello()` into `fn (h Host) hellogreett`.
fn apply_rename(file string, hits []RenameHit, name string, new_name string) !string {
	lines := os.read_file(file) or { return error('could not read the file') }.split_into_lines()
	mut out := lines.clone()
	for hit in hits {
		if hit.line < 1 || hit.line > out.len {
			return error('line ${hit.line} is past the end of the file')
		}
		start := hit.column - 1
		end := start + hit.length
		if start < 0 || end > out[hit.line - 1].len {
			return error('column ${hit.column} on line ${hit.line} no longer holds the old name')
		}
		there := out[hit.line - 1][start..end]
		if hit.length != name.len || there != name {
			return error('column ${hit.column} on line ${hit.line} holds `${there}`, not `${name}`')
		}
		out[hit.line - 1] = out[hit.line - 1][..start] + new_name + out[hit.line - 1][end..]
	}
	return join_lines(out)
}

// rename_file_json applies the rename to one file, or reports the plan when
// `dry_run` is set.
fn rename_file_json(ws &Workspace, file string, hits []RenameHit, name string,
	new_name string, dry_run bool) string {
	result := if dry_run {
		os.read_file(file) or { return error_json(err.msg()) }
	} else {
		apply_rename(file, hits, name, new_name) or {
			return error_json(err.msg())
		}
	}
	if !dry_run {
		os.write_file(file, result) or { return error_json(err.msg()) }
	}
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('path')
	w.string(ws.relative(file))
	w.key('edit_count')
	w.number(hits.len)
	w.key('hits')
	w.begin_array()
	for hit in hits {
		w.array_item()
		w.begin_object()
		w.key('line')
		w.number(hit.line)
		w.key('column')
		w.number(hit.column)
		w.key('length')
		w.number(hit.length)
		w.end_object()
	}
	w.end_array()
	w.end_object()
	return w.str()
}
