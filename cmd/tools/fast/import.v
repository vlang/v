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
	db.exec_none('BEGIN')
	total := do_import(mut db, files, since, ref) or {
		db.exec_none('ROLLBACK')
		return err
	}
	if total == 0 && !already {
		db.exec_none('ROLLBACK')
		return error('no benchmark rows found in ${files.join(', ')}; nothing imported (history not claimed)')
	}
	db.exec_none('COMMIT')
	elog('import done: ${total} rows inserted. Start the web app with: v run . serve')
}

// do_import claims the history and imports all files, returning the number of rows
// inserted. Errors (or a zero result) let the caller roll the transaction back.
fn do_import(mut db sqlite.DB, files []string, since i64, ref string) !int {
	history := claim_history(mut db, ref)!
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
		// ancestry, unlike the old table's %at), in one git call — so an imported
		// 7/40-char id for the same commit is not stored as a duplicate point.
		commit, cdate := resolve_commit(raw, parse_old_date(cells[0].trim_space()))
		if benchmark_exists(db, commit) {
			continue
		}
		if since != 0 && cdate.unix() < since {
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

// resolve_commit maps an imported commit id to the canonical 8-char hash that
// `run` stores (the first 8 chars of the full hash) and its committer date, in a
// single git call. Falls back to a bounded abbreviation and the given table
// timestamp when the commit is not present in the local checkout.
fn resolve_commit(raw string, fallback_date time.Time) (string, time.Time) {
	res :=
		os.execute("git -C ${os.quoted_path(vdir)} log -n1 --pretty=format:'%H %ct' ${os.quoted_path(raw)}")
	if res.exit_code == 0 {
		parts := res.output.trim_space().split(' ')
		if parts.len == 2 && parts[0].len >= 8 {
			ts := parts[1].i64()
			date := if ts > 0 { time.unix(ts) } else { fallback_date }
			return parts[0][..8], date
		}
	}
	return short_hash(raw), fallback_date
}

// parse_old_date reads the old `time.format()` output (`YYYY-MM-DD HH:MM`). If it
// cannot be parsed, it returns the unix epoch so the row still imports (it simply
// sorts to the bottom) rather than being dropped.
fn parse_old_date(s string) time.Time {
	parts := s.split(' ')
	ymd := parts[0].split('-')
	if ymd.len < 3 {
		return time.unix(0)
	}
	mut hour, mut minute := 0, 0
	if parts.len > 1 {
		hm := parts[1].split(':')
		hour = hm[0].int()
		if hm.len > 1 {
			minute = hm[1].int()
		}
	}
	return time.new(
		year:   ymd[0].int()
		month:  ymd[1].int()
		day:    ymd[2].int()
		hour:   hour
		minute: minute
	)
}
