// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import time
import db.sqlite

// Benchmark is a single measured data point for one V commit.
// It is persisted with V's ORM into the local SQLite database at `db_path`.
@[table: 'benchmarks']
struct Benchmark {
mut:
	id           int    @[primary; sql: serial]
	commit_hash  string @[unique] // short (8 char) hash; unique so concurrent runs cannot insert duplicates
	git_ref      string    // the history this row belongs to (e.g. origin/master); mixing is rejected
	message      string    // commit subject line
	commit_date  time.Time // committer date (%ct); monotonic along first-parent
	created_at   time.Time // when this benchmark was actually run
	v_c_ms       int       // self compile to C (`cmd/v` historically, `vlib/v3/v3.v` now)
	v_self_ms    int       // self compile to a binary (same source selection as v_c_ms)
	hello_ms     int       // `v hello_world.v`  : compile a tiny program
	vc_size_kb   int       // size of the generated v.c, in KB
	scan_ms      int       // scanner time
	parse_ms     int       // parser time
	check_ms     int       // checker time
	cgen_ms      int       // C generation time
	scan_rss_kb  int       // RSS reported after the scan/setup stage (v3 output)
	parse_rss_kb int       // RSS reported after parsing (v3 output)
	check_rss_kb int       // RSS reported after checking (v3 output)
	cgen_rss_kb  int       // RSS reported after C generation (v3 output)
	vlines       int       // number of V source lines compiled
	lines_per_s  int       // V lines / second for the `v -o v.c` step
	// peak resident-set-size (RSS) five-number summary (KB) across rss_samples runs,
	// for a box-and-whisker view. self_* = V self-compiling, hello_* = hello.v.
	self_rss_min_kb  int
	self_rss_q1_kb   int
	self_rss_med_kb  int
	self_rss_q3_kb   int
	self_rss_max_kb  int
	hello_rss_min_kb int
	hello_rss_q1_kb  int
	hello_rss_med_kb int
	hello_rss_q3_kb  int
	hello_rss_max_kb int
}

// open_db opens (creating if needed) the SQLite database and makes sure the
// `benchmarks` table exists. `create table` maps to `CREATE TABLE IF NOT EXISTS`,
// so calling this repeatedly is safe.
fn open_db() !sqlite.DB {
	mut db := sqlite.connect(db_path)!
	// Close the just-opened connection if schema init/migration fails (e.g. the db
	// is locked), so the per-request open_db() callers do not leak file descriptors.
	sql db {
		create table Benchmark
	} or {
		db.close() or {}
		return err
	}
	migrate_schema(mut db) or {
		db.close() or {}
		return err
	}
	return db
}

// migrate_exec runs one migration statement and errors on any SQLite failure code
// (locked, disk full, constraint, ...), so migration problems are not swallowed.
fn migrate_exec(mut db sqlite.DB, query string) ! {
	code := db.exec_none(query)
	if code != sqlite.sqlite_ok && code != sqlite.sqlite_done {
		return error('schema migration failed (sqlite code ${code}) for: ${query}')
	}
}

// schema_version is bumped whenever the migration changes. It is tracked with
// PRAGMA user_version rather than the presence of a table/column, so a database
// left half-migrated by an intermediate release (e.g. one that created fast_meta
// but never seeded/canonicalized the history) is still upgraded.
const schema_version = 3

// migrate_schema upgrades a database created by an older version of the tool. It
// is transactional and idempotent (a no-op once applied), and propagates errors
// so open_db() fails loudly rather than returning an inconsistent schema.
fn migrate_schema(mut db sqlite.DB) ! {
	uv := db.exec('PRAGMA user_version')!
	if uv.len > 0 && uv[0].vals[0].int() >= schema_version {
		return
	}
	// Discover the existing columns so we only ALTER genuinely-missing ones — that
	// way a failing ALTER is a real error, not the benign "duplicate column name".
	info := db.exec('PRAGMA table_info(benchmarks)')!
	mut existing := map[string]bool{}
	for row in info {
		if row.vals.len > 1 {
			existing[row.vals[1]] = true // table_info column 1 is the name
		}
	}
	migrate_exec(mut db, 'BEGIN')!
	apply_migration(mut db, existing) or {
		migrate_exec(mut db, 'ROLLBACK') or {}
		return err
	}
	migrate_exec(mut db, 'PRAGMA user_version = ${schema_version}')!
	migrate_exec(mut db, 'COMMIT')!
}

fn apply_migration(mut db sqlite.DB, existing map[string]bool) ! {
	rss_columns := ['self_rss_min_kb', 'self_rss_q1_kb', 'self_rss_med_kb', 'self_rss_q3_kb',
		'self_rss_max_kb', 'hello_rss_min_kb', 'hello_rss_q1_kb', 'hello_rss_med_kb',
		'hello_rss_q3_kb', 'hello_rss_max_kb', 'scan_rss_kb', 'parse_rss_kb', 'check_rss_kb',
		'cgen_rss_kb']
	for c in rss_columns {
		if c !in existing {
			migrate_exec(mut db,
				'ALTER TABLE benchmarks ADD COLUMN ${c} INTEGER NOT NULL DEFAULT 0')!
		}
	}
	if 'git_ref' !in existing {
		migrate_exec(mut db, "ALTER TABLE benchmarks ADD COLUMN git_ref TEXT NOT NULL DEFAULT ''")!
	}
	// A database created before commit_hash gained @[unique] has no uniqueness
	// enforcement (CREATE TABLE IF NOT EXISTS won't add it), so overlapping runs
	// could insert duplicate commits and the ORM upsert's ON CONFLICT would have no
	// index to target. Deduplicate (keep the newest row per hash) and add the index.
	migrate_exec(mut db,
		'DELETE FROM benchmarks WHERE id NOT IN (SELECT MAX(id) FROM benchmarks GROUP BY commit_hash)')!
	migrate_exec(mut db,
		'CREATE UNIQUE INDEX IF NOT EXISTS idx_benchmarks_commit_hash ON benchmarks(commit_hash)')!
	// single-row key/value store that holds this database's history identity
	migrate_exec(mut db,
		'CREATE TABLE IF NOT EXISTS fast_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)')!
	canonicalize_history(mut db)!
}

// canonicalize_history seeds/rewrites the database's history identity in the
// current normalized form, from the stored fast_meta value and existing rows, so
// an upgraded database stays usable even when normalize_ref's output changed
// between versions (e.g. `master` -> `origin/master`). It errors if the rows/meta
// already identify more than one history.
fn canonicalize_history(mut db sqlite.DB) ! {
	// the single raw stored identity (from meta, else the rows)
	mut raw := ''
	m := db.exec("SELECT value FROM fast_meta WHERE key = 'history_ref'")!
	if m.len > 0 && m[0].vals[0] != '' {
		raw = m[0].vals[0]
	}
	rows := db.exec("SELECT DISTINCT git_ref FROM benchmarks WHERE git_ref != ''")!
	for r in rows {
		v := r.vals[0]
		if v == '' {
			continue
		}
		if raw == '' {
			raw = v
		} else if v != raw && normalize_ref(v) != normalize_ref(raw) {
			return error('cannot migrate fast.db: it already contains rows from multiple histories (${raw}, ${v})')
		}
	}
	if raw == '' {
		// No recorded identity, but there may be legacy rows (git_ref='') from before
		// the column existed. Leaving the db unclaimed would let the next run/bench/
		// import claim an arbitrary ref and mix histories, so infer it: if the rows
		// belong to the repository default branch, adopt it; otherwise refuse and make
		// the operator choose (re-import with --ref, or start a fresh database).
		cnt := db.exec('SELECT count(*) FROM benchmarks')!
		if cnt.len == 0 || cnt[0].vals[0].int() == 0 {
			return
		}
		def := normalize_ref(resolve_history_ref(''))
		if def == '' || def == 'HEAD' || !stored_tip_on(db, def) {
			return error('fast.db has ${cnt[0].vals[0]} rows with no recorded git history; re-import with `--ref <ref>`, or start a fresh database.')
		}
		raw = def
	}
	mut canon := normalize_ref(raw)
	// If normalization qualified a bare name with a remote (the fallback for a ref
	// with no local upstream, e.g. `release` -> `origin/release`), only adopt it
	// once the stored commits are proven to belong to that history — otherwise a
	// local branch that diverged from the same-named remote would be silently
	// relabeled and future remote measurements would mix two histories. The newest
	// stored commit is the tip of this single first-parent history, so its ancestry
	// covers the rest.
	if canon != raw && !stored_tip_on(db, canon) {
		canon = raw
	}
	// tag every row (including legacy git_ref='' rows) with the single validated
	// history, and record it in fast_meta.
	safe := canon.replace("'", "''")
	migrate_exec(mut db, "DELETE FROM fast_meta WHERE key = 'history_ref'")!
	migrate_exec(mut db, "INSERT INTO fast_meta (key, value) VALUES ('history_ref', '${safe}')")!
	migrate_exec(mut db, "UPDATE benchmarks SET git_ref = '${safe}'")!
}

// stored_tip_on reports whether the newest stored commit is contained in `git_ref`
// (an ancestor of it). Returns false if it cannot be resolved, so migration stays
// conservative and does not remap onto an unproven history.
fn stored_tip_on(db sqlite.DB, git_ref string) bool {
	tip := db.exec('SELECT commit_hash FROM benchmarks ORDER BY commit_date DESC LIMIT 1') or {
		return false
	}
	if tip.len == 0 {
		return true // no rows to contradict the candidate
	}
	c := tip[0].vals[0]
	res :=
		os.execute('git -C ${os.quoted_path(vdir)} merge-base --is-ancestor ${os.quoted_path(c)} ${os.quoted_path(git_ref)}')
	return res.exit_code == 0
}

// normalize_ref reduces a ref to a stable history identity, so a local branch and
// its remote-tracking ref share one identity (e.g. `master` and `origin/master`),
// WITHOUT collapsing same-named branches on different remotes (`origin/release`
// vs `upstream/release`, which stay distinct).
fn normalize_ref(ref string) string {
	mut r := ref.trim_space()
	r = r.trim_string_left('refs/heads/')
	r = r.trim_string_left('refs/remotes/')
	if r == '' {
		return r
	}
	remotes := git_lines('remote')
	// already remote-qualified (origin/master, upstream/release): keep as-is
	if r.all_before('/') in remotes {
		return r
	}
	// bare branch name: canonicalize to its remote-tracking ref. Prefer the local
	// branch's upstream; if there is no local branch (detached/shallow checkout, or
	// a legacy stored `master`), qualify it with a remote that has the branch,
	// preferring the repository default, so the identity does not depend on a local
	// branch existing.
	up :=
		os.execute('git -C ${os.quoted_path(vdir)} rev-parse --abbrev-ref ${os.quoted_path('${r}@{upstream}')}')
	if up.exit_code == 0 && up.output.trim_space() != '' {
		upstream := up.output.trim_space()
		// Collapse the local branch onto its upstream when the two are on one line of
		// history — either is an ancestor of the other. That keeps the identity stable
		// across a normal push: a branch that is ahead of its upstream (unpushed commits)
		// still normalizes to the upstream, and continues to after those commits are
		// pushed, so claim_history does not reject the unchanged history. A genuinely
		// diverged branch (neither an ancestor, e.g. a force-push onto unrelated history)
		// keeps its own identity, so it is never mixed with the remote's revisions.
		if same_line(r, upstream) {
			return upstream
		}
		return r
	}
	// No configured upstream. Distinguish a *missing* local branch (a bare/legacy name we
	// are merely qualifying, e.g. a stored `master` with no local branch) from a local
	// branch that simply lacks an upstream: the former can adopt a remote ref freely, but
	// the latter must be proven on the same line of history before being qualified, or a
	// diverged local branch would be silently stored under the remote's identity.
	local_exists := os.execute('git -C ${os.quoted_path(vdir)} rev-parse --verify --quiet ${os.quoted_path('refs/heads/${r}')}').exit_code == 0
	for remote in ordered_remotes(remotes) {
		cand := '${remote}/${r}'
		if os.execute('git -C ${os.quoted_path(vdir)} rev-parse --verify --quiet ${os.quoted_path(cand)}').exit_code != 0 {
			continue
		}
		if !local_exists || same_line(r, cand) {
			return cand
		}
	}
	return r
}

// same_line reports whether refs `a` and `b` lie on one line of history — either is an
// ancestor of the other. It returns false when they have genuinely diverged, and also
// when either cannot be resolved (merge-base then exits >1), so an unresolvable ref is
// never treated as sharing history.
fn same_line(a string, b string) bool {
	gitc := 'git -C ${os.quoted_path(vdir)}'
	if os.execute('${gitc} merge-base --is-ancestor ${os.quoted_path(a)} ${os.quoted_path(b)}').exit_code == 0 {
		return true
	}
	return os.execute('${gitc} merge-base --is-ancestor ${os.quoted_path(b)} ${os.quoted_path(a)}').exit_code == 0
}

// git_lines runs a git subcommand against the checkout and returns its non-empty
// output lines.
fn git_lines(subcmd string) []string {
	mut out := []string{}
	res := os.execute('git -C ${os.quoted_path(vdir)} ${subcmd}')
	if res.exit_code == 0 {
		for line in res.output.split_into_lines() {
			l := line.trim_space()
			if l != '' {
				out << l
			}
		}
	}
	return out
}

// ordered_remotes lists the remotes with the repository default (the remote of
// origin/HEAD) first, so a bare branch present on several remotes canonicalizes
// deterministically to the default one.
fn ordered_remotes(remotes []string) []string {
	mut def := 'origin'
	res := os.execute('git -C ${os.quoted_path(vdir)} rev-parse --abbrev-ref origin/HEAD')
	if res.exit_code == 0 && res.output.trim_space().contains('/') {
		def = res.output.trim_space().all_before('/')
	}
	mut order := []string{}
	if def in remotes {
		order << def
	}
	for rem in remotes {
		if rem !in order {
			order << rem
		}
	}
	return order
}

// claim_history atomically records this database's single history identity (the
// normalized ref) the first time, and returns it. It errors if the database is
// already claimed for a different history. The claim uses INSERT OR IGNORE on a
// PRIMARY KEY, so two concurrent processes racing on an empty database cannot each
// claim a different ref — exactly one INSERT wins and the loser sees the mismatch.
// history_claimed reports whether this database already has a history identity.
fn history_claimed(db sqlite.DB) bool {
	rows := db.exec("SELECT 1 FROM fast_meta WHERE key = 'history_ref'") or { return false }
	return rows.len > 0
}

fn claim_history(mut db sqlite.DB, ref string) !string {
	norm := normalize_ref(ref)
	// `HEAD` (or empty) is not a stable history identity — e.g. a detached shallow
	// checkout with no origin/HEAD or master/main. Refuse it, since otherwise
	// unrelated detached commits would all share the `HEAD` history and corrupt the
	// ancestry-based chart. The user must check out a named branch or pass an
	// explicit ref (`run -branch <ref>` / `import --ref <ref>`).
	if norm == '' || norm == 'HEAD' {
		return error('could not resolve a stable git history for this checkout (`${ref}`). Check out a named branch, or pass an explicit ref: `run -branch <ref>` / `import --ref <ref>`.')
	}
	safe := norm.replace("'", "''")
	migrate_exec(mut db,
		"INSERT OR IGNORE INTO fast_meta (key, value) VALUES ('history_ref', '${safe}')")!
	rows := db.exec("SELECT value FROM fast_meta WHERE key = 'history_ref'")!
	stored := if rows.len > 0 { rows[0].vals[0] } else { norm }
	if stored != norm {
		return error('fast.db already tracks history `${stored}`; refusing to record `${norm}` — mixing branches would corrupt the ancestry-based chart. Use a separate database.')
	}
	// The ref string matching is not enough: a tracked branch can be force-pushed or
	// reset onto unrelated history while keeping the same normalized name. Its stored
	// tip would then no longer be an ancestor of the ref, and appending new samples
	// beside the old ones makes the dashboard compute deltas across unrelated revisions.
	// Refuse only when divergence is *proven* (both resolve and git reports not-an-
	// ancestor), so a transient/shallow checkout that cannot resolve the tip is not
	// rejected.
	if history_diverged(db, norm) {
		return error('fast.db history `${norm}` was force-pushed or reset onto unrelated history (its newest stored commit is no longer an ancestor of `${norm}`); refusing to append — the ancestry-based chart would compare unrelated revisions. Use a separate database.')
	}
	return norm
}

// history_diverged reports whether the newest stored commit is *definitively* not an
// ancestor of `ref` — i.e. both resolve and git proves they are on unrelated history.
// It returns false when ancestry holds, and also when it cannot be determined (e.g. a
// shallow clone missing the stored commit), so a partial checkout does not reject an
// otherwise valid reuse. `merge-base --is-ancestor` exits 0 (ancestor), 1 (proven not
// ancestor), or >1 (could not resolve).
fn history_diverged(db sqlite.DB, ref string) bool {
	tip := db.exec('SELECT commit_hash FROM benchmarks ORDER BY commit_date DESC LIMIT 1') or {
		return false
	}
	if tip.len == 0 {
		return false // nothing stored yet — the first claim cannot diverge
	}
	c := tip[0].vals[0]
	res :=
		os.execute('git -C ${os.quoted_path(vdir)} merge-base --is-ancestor ${os.quoted_path(c)} ${os.quoted_path(ref)}')
	return res.exit_code == 1
}

fn insert_benchmark(mut db sqlite.DB, b Benchmark) ! {
	sql db {
		insert b into Benchmark
	}!
}

// upsert_benchmark inserts `b`, or updates the existing row with the same
// commit_hash (the unique key). Being a single atomic statement, a failure
// cannot lose the previous measurement the way a delete-then-insert can.
fn upsert_benchmark(mut db sqlite.DB, b Benchmark) ! {
	sql db {
		upsert b into Benchmark
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

// load_benchmarks returns every stored benchmark, newest commit first. It
// propagates query errors so a database outage (locked/corrupt/incompatible
// schema) surfaces as a server error rather than masquerading as "no data".
fn load_benchmarks(db sqlite.DB) ![]Benchmark {
	return sql db {
		select from Benchmark order by commit_date desc
	}!
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
mut:
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
	scan_rss    int
	parse_rss   int
	check_rss   int
	cgen_rss    int
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
			timestamp:   b.commit_date.custom_format('MMM D HH:mm')
			commit_hash: short_hash(b.commit_hash)
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
			scan_rss:    b.scan_rss_kb / 1024
			parse_rss:   b.parse_rss_kb / 1024
			check_rss:   b.check_rss_kb / 1024
			cgen_rss:    b.cgen_rss_kb / 1024
			vlines:      b.vlines
			lines_per_s: b.lines_per_s
			d_v_c:       delta(b.v_c_ms, prev.v_c_ms, 18)
			d_v_self:    delta(b.v_self_ms, prev.v_self_ms, 36)
			d_hello:     delta(b.hello_ms, prev.hello_ms, 36)
		}
	}
	return rows
}
