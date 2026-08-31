// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import time
import db.sqlite

// cmd_import migrates the accumulated history produced by the old static-site
// pipeline (the `table.html` / gh-pages `index.html` files) into fast.db, so the
// dashboard keeps its per-commit history when the deployment switches over.
//
//   v run . import [--since YYYY-MM-DD] path/to/table.html [more.html ...]
//
// It is idempotent: commits already present in the database are skipped.
// With --since, rows older than the given date are not imported.
fn cmd_import(args []string) ! {
	mut files := []string{}
	mut since := i64(0)
	mut ref := '' // history the imported rows belong to; defaults to the repo default
	mut i := 0
	for i < args.len {
		a := args[i]
		if a == '--since' && i + 1 < args.len {
			since = parse_since(args[i + 1])
			i += 2
			continue
		}
		if a == '--ref' && i + 1 < args.len {
			ref = args[i + 1]
			i += 2
			continue
		}
		if !a.starts_with('-') {
			files << a
		}
		i++
	}
	if files.len == 0 {
		return error('usage: fast import [--since YYYY-MM-DD] [--ref <ref>] <table.html> [more.html ...]')
	}
	if ref == '' {
		ref = resolve_history_ref('') // the old fast.vlang.io history was the default branch
	}
	mut db := open_db()!
	defer {
		db.close() or {}
	}
	// Claim the history and insert the rows in one transaction, so an empty or failed
	// import (unreadable file, or no usable rows) rolls back the claim too — otherwise
	// it would permanently claim an empty database and reject a later valid import or
	// sampler run for another ref. Re-importing into an already-claimed database with
	// nothing new is still fine (idempotent), so only reject a freshly-made claim.
	already := history_claimed(db)
	// A failed BEGIN would leave the inserts running in autocommit, where ROLLBACK
	// cannot undo them, so validate it up front like the migration code does.
	migrate_exec(mut db, 'BEGIN')!
	total := do_import(mut db, files, since, ref) or {
		db.exec_none('ROLLBACK')
		return err
	}
	if total == 0 && !already {
		db.exec_none('ROLLBACK')
		return error('no benchmark rows found in ${files.join(', ')}; nothing imported (history not claimed)')
	}
	// A failed COMMIT (disk full, I/O error, ...) leaves the transaction to be rolled
	// back when the connection closes, so it must not be reported as a successful
	// import — validate its result code and surface the failure instead of logging done.
	migrate_exec(mut db, 'COMMIT') or {
		db.exec_none('ROLLBACK')
		return err
	}
	elog('import done: ${total} rows inserted. Start the web app with: v run . serve')
}

// do_import claims the history and imports all files, returning the number of rows
// inserted. Errors (or a zero result) let the caller roll the transaction back.
fn do_import(mut db sqlite.DB, files []string, since i64, ref string) !int {
	history := claim_history(mut db, ref)!
	// The per-commit membership check (commit_off_history) can only reject a commit when
	// git resolves both it and the claimed ref. If the claimed ref cannot be resolved
	// (misspelled, deleted, or never fetched), every merge-base exits 128 and no commit
	// is ever rejected, so an unrelated archive would import wholesale under a nonexistent
	// identity. Fail up front instead of treating an unresolvable ref as confirmed history.
	if !ref_resolvable(history) {
		return error('claimed history ref `${history}` cannot be resolved in this checkout; refusing to import — fetch it, or pass a correct --ref')
	}
	mut total := 0
	for f in files {
		content := os.read_file(f) or { return error('cannot read ${f}: ${err}') }
		n := import_table_html(mut db, content, since, history)!
		elog('imported ${n} rows from ${f} (history: ${history})')
		total += n
	}
	return total
}

// parse_since converts a YYYY-MM-DD string into a unix timestamp (0 == no filter).
fn parse_since(s string) i64 {
	p := s.split('-')
	if p.len < 3 {
		return 0
	}
	return time.new(year: p[0].int(), month: p[1].int(), day: p[2].int()).unix()
}

// import_table_html parses the `<tr>` rows of an old fast.vlang.io table and
// inserts them, returning the number of newly inserted rows. The historic row
// layout is 14 `<td>` cells: date, commit(link), message, v.c, v, native(unused),
// hello, v.c size, parse, check, cgen, scan, V lines, V lines/s.
fn import_table_html(mut db sqlite.DB, html string, since i64, git_ref string) !int {
	mut inserted := 0
	for row in html.split('<tr>') {
		// data rows link to a commit; the header row (with `<th>`) does not
		if !row.contains('/commit/') {
			continue
		}
		cells := extract_cells(row.all_before('</tr>'))
		if cells.len < 14 {
			continue
		}
		raw := cells[1].trim_space()
		// Only accept a real hex commit hash. A crafted HTML file could otherwise put
		// shell metacharacters here, which later reach `git`/`oldv` shell commands.
		if !is_commit_hash(raw) {
			continue
		}
		// Resolve to the canonical 8-char abbreviation that `run` stores (the first 8
		// chars of the full hash) plus the committer date (%ct, monotonic along
		// first-parent ancestry, unlike the old table's author date), from the local
		// checkout — so an imported 7/40-char id is neither stored as a duplicate point
		// nor placed out of ancestry order. Commits absent from the checkout are skipped.
		commit, cdate := resolve_commit(raw) or {
			elog('  skipping ${raw}: not in the local checkout, so its ancestry order cannot be preserved')
			continue
		}
		if benchmark_exists(db, commit) {
			continue
		}
		if since != 0 && cdate.unix() < since {
			continue
		}
		// Verify the commit actually belongs to the claimed history before tagging it
		// with git_ref. On a fresh database the claim has no stored tip for
		// history_diverged to validate, so pairing the wrong archive with --ref (e.g. a
		// branch's commits with `--ref origin/master`) would otherwise mix unrelated
		// points into one history and only be caught later when a sampler is rejected.
		if commit_off_history(commit, git_ref) {
			elog('  skipping ${commit}: not on the claimed history ${git_ref} (wrong archive for this --ref?)')
			continue
		}
		b := Benchmark{
			commit_hash: commit
			git_ref:     git_ref
			message:     unescape_html(cells[2])
			commit_date: cdate
			created_at:  time.now()
			v_c_ms:      cell_int(cells[3])
			v_self_ms:   cell_int(cells[4])
			hello_ms:    cell_int(cells[6])
			vc_size_kb:  cell_int(cells[7])
			parse_ms:    cell_int(cells[8])
			check_ms:    cell_int(cells[9])
			cgen_ms:     cell_int(cells[10])
			scan_ms:     cell_int(cells[11])
			vlines:      cell_int(cells[12])
			lines_per_s: cell_int(cells[13])
		}
		insert_benchmark(mut db, b)!
		inserted++
	}
	return inserted
}

// extract_cells returns the text content of each `<td>` in a row, tags stripped.
fn extract_cells(row string) []string {
	mut cells := []string{}
	mut rest := row
	for {
		open := rest.index('<td') or { break }
		gt := rest[open..].index('>') or { break }
		rest = rest[open + gt + 1..]
		close := rest.index('</td>') or { break }
		cells << strip_tags(rest[..close])
		rest = rest[close + 5..]
	}
	return cells
}

// strip_tags removes any `<...>` tags from a cell and trims the result.
fn strip_tags(s string) string {
	mut out := []u8{}
	mut in_tag := false
	for c in s {
		if c == `<` {
			in_tag = true
		} else if c == `>` {
			in_tag = false
		} else if !in_tag {
			out << c
		}
	}
	return out.bytestr().trim_space()
}

// cell_int parses a numeric cell, dropping units and separators (`ms`, `KB`, `,`).
fn cell_int(s string) int {
	return s.replace('ms', '').replace('KB', '').replace(',', '').trim_space().int()
}

fn unescape_html(s string) string {
	return s.replace('&lt;', '<').replace('&gt;', '>').replace('&amp;', '&').trim_space()
}

// is_commit_hash reports whether `s` looks like an abbreviated or full git commit
// hash (7-40 hex chars), used to reject untrusted commit cells before they reach
// any shell command.
fn is_commit_hash(s string) bool {
	if s.len < 7 || s.len > 40 {
		return false
	}
	for c in s {
		if !(c.is_hex_digit()) {
			return false
		}
	}
	return true
}

// resolve_commit maps an imported commit id to the canonical 8-char hash that `run`
// stores (the first 8 chars of the full hash) and its committer date (%ct), from the
// local checkout, in a single git call. It returns `none` when the commit is not
// present: an absent commit has no reliable committer date, and the legacy table's
// timestamp came from the Git author date, which is not guaranteed to follow
// first-parent order. Since load_benchmarks sorts by commit_date and treats adjacent
// rows as ancestry neighbours, importing an out-of-order date would show misleading
// deltas — so unresolved rows are skipped instead. On a full checkout every historical
// commit resolves; only a shallow/partial clone drops rows, which a full fetch fixes.
// ref_resolvable reports whether `git_ref` names a commit in this checkout. An import
// against an unresolvable claimed ref must fail rather than silently accept every row.
fn ref_resolvable(git_ref string) bool {
	return os.execute('git -C ${os.quoted_path(vdir)} rev-parse --verify --quiet ${os.quoted_path('${git_ref}^{commit}')}').exit_code == 0
}

// commit_off_history reports whether `commit` is *proven* not to belong to `git_ref`'s
// history — git resolves both and reports the commit is not an ancestor of the ref
// (merge-base --is-ancestor exit 1). It returns false when membership holds (exit 0) or
// cannot be determined (exit >1, e.g. an unresolvable ref on a shallow clone), matching
// history_diverged's lenient-when-unknown stance so a partial checkout is not
// over-rejected. `commit` has already been resolved in the local checkout by the caller.
fn commit_off_history(commit string, git_ref string) bool {
	res :=
		os.execute('git -C ${os.quoted_path(vdir)} merge-base --is-ancestor ${os.quoted_path(commit)} ${os.quoted_path(git_ref)}')
	return res.exit_code == 1
}

fn resolve_commit(raw string) ?(string, time.Time) {
	res :=
		os.execute("git -C ${os.quoted_path(vdir)} log -n1 --pretty=format:'%H %ct' ${os.quoted_path(raw)}")
	if res.exit_code == 0 {
		parts := res.output.trim_space().split(' ')
		if parts.len == 2 && parts[0].len >= 8 {
			ts := parts[1].i64()
			if ts > 0 {
				return parts[0][..8], time.unix(ts)
			}
		}
	}
	return none
}
