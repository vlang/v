// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import time
import db.sqlite

// Benchmark is a single measured data point for one V commit.
// It is persisted with V's ORM into the local SQLite database at `db_path`.
@[table: 'benchmarks']
struct Benchmark {
mut:
	id          int @[primary; sql: serial]
	commit_hash string    // short (8 char) commit hash
	message     string    // commit subject line
	commit_date time.Time // committer date (%ct); monotonic along first-parent
	created_at  time.Time // when this benchmark was actually run
	v_c_ms      int       // `v -o v.c cmd/v`   : self compile to C
	v_self_ms   int       // `v -o v cmd/v`     : self compile to a binary
	hello_ms    int       // `v hello_world.v`  : compile a tiny program
	vc_size_kb  int       // size of the generated v.c, in KB
	scan_ms     int       // scanner time
	parse_ms    int       // parser time
	check_ms    int       // checker time
	cgen_ms     int       // C generation time
	vlines      int       // number of V source lines compiled
	lines_per_s int       // V lines / second for the `v -o v.c` step
}

// open_db opens (creating if needed) the SQLite database and makes sure the
// `benchmarks` table exists. `create table` maps to `CREATE TABLE IF NOT EXISTS`,
// so calling this repeatedly is safe.
fn open_db() !sqlite.DB {
	mut db := sqlite.connect(db_path)!
	sql db {
		create table Benchmark
	}!
	return db
}

fn insert_benchmark(mut db sqlite.DB, b Benchmark) ! {
	sql db {
		insert b into Benchmark
	}!
}

// benchmark_exists reports whether a commit has already been measured, so that
// re-runs of the sampler are idempotent.
fn benchmark_exists(db sqlite.DB, hash string) bool {
	rows := sql db {
		select from Benchmark where commit_hash == hash
	} or { return false }
	return rows.len > 0
}

// load_benchmarks returns every stored benchmark, newest commit first.
fn load_benchmarks(db sqlite.DB) []Benchmark {
	return sql db {
		select from Benchmark order by commit_date desc
	} or { []Benchmark{} }
}

// Delta is a rendered difference badge for one column: `text` is what to show
// (e.g. `-12`) and `cls` is the CSS class controlling its colour.
struct Delta {
	text string
	cls  string
}

// Row is the view-model rendered by templates/index.html. All diffing is done
// here in V (server side), so the template and the browser stay dumb.
struct Row {
	num         int
	timestamp   string
	commit_hash string
	commit_url  string
	message     string
	v_c         int
	v_self      int
	hello       int
	vc_size     int
	scan        int
	parse       int
	check       int
	cgen        int
	vlines      int
	lines_per_s int
	d_v_c       Delta
	d_v_self    Delta
	d_hello     Delta
}

fn iabs(x int) int {
	return if x < 0 { -x } else { x }
}

// delta compares a measurement to the previous (older) one. Lower millisecond
// numbers mean a faster compiler, so a negative delta is rendered green.
fn delta(cur int, prev int, threshold int) Delta {
	d := cur - prev
	if d == 0 || iabs(d) <= threshold {
		return Delta{}
	}
	cls := if d < 0 { 'plus' } else { 'minus' }
	text := if d > 0 { '+${d}' } else { '${d}' }
	return Delta{
		text: text
		cls:  cls
	}
}

// build_rows turns stored benchmarks (newest first) into view rows, computing
// the coloured deltas against each row's older neighbour.
fn build_rows(list []Benchmark) []Row {
	mut rows := []Row{cap: list.len}
	for i, b in list {
		// the list is newest-first, so the older commit is the next element
		prev := if i + 1 < list.len { list[i + 1] } else { b }
		rows << Row{
			num:         list.len - i
			timestamp:   b.commit_date.format()
			commit_hash: b.commit_hash
			commit_url:  'https://github.com/vlang/v/commit/${b.commit_hash}'
			message:     b.message
			v_c:         b.v_c_ms
			v_self:      b.v_self_ms
			hello:       b.hello_ms
			vc_size:     b.vc_size_kb
			scan:        b.scan_ms
			parse:       b.parse_ms
			check:       b.check_ms
			cgen:        b.cgen_ms
			vlines:      b.vlines
			lines_per_s: b.lines_per_s
			d_v_c:       delta(b.v_c_ms, prev.v_c_ms, 18)
			d_v_self:    delta(b.v_self_ms, prev.v_self_ms, 36)
			d_hello:     delta(b.hello_ms, prev.hello_ms, 36)
		}
	}
	return rows
}
