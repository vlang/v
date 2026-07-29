// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import time

// where the `oldv` tool caches historic V checkouts: <cache>/oldv/v_at_<commit>/
const oldv_cache = os.join_path(os.cache_dir(), 'oldv')
const oldv_src = os.join_path(vdir, 'cmd', 'tools', 'oldv.v')

// vexe returns the V compiler of the main checkout, used to drive `oldv`.
fn vexe() string {
	return os.join_path(vdir, exe_name('v'))
}

// resolve_history_ref picks the git revision to walk for the backfill. Not every
// checkout has a local `master` (the reviewed one only had `main` + `work`), so
// prefer an explicit -branch, then the remote's default branch, then a local
// master/main, and finally HEAD, which always exists.
fn resolve_history_ref(user_branch string) string {
	if user_branch != '' {
		return user_branch
	}
	gitc := 'git -C ${os.quoted_path(vdir)}'
	origin := os.execute('${gitc} symbolic-ref --quiet --short refs/remotes/origin/HEAD')
	if origin.exit_code == 0 && origin.output.trim_space() != '' {
		return origin.output.trim_space()
	}
	for cand in ['master', 'main'] {
		if os.execute('${gitc} rev-parse --verify --quiet ${cand}').exit_code == 0 {
			return cand
		}
	}
	return 'HEAD'
}

// cmd_run samples every <step>th commit of <year> (default: every 50th commit of
// 2026), builds V for each sampled commit with `oldv` and benchmarks it, storing
// the results. Pass -dry-run to only list what would be measured.
fn cmd_run(args []string) ! {
	mut year := 2026
	mut step := 50
	mut latest := 0
	mut branch := ''
	dry := args.contains('-dry-run')
	for i, a in args {
		if a == '-year' && i + 1 < args.len {
			year = args[i + 1].int()
		}
		if a == '-step' && i + 1 < args.len {
			step = args[i + 1].int()
		}
		if a == '-latest' && i + 1 < args.len {
			latest = args[i + 1].int()
		}
		if a == '-branch' && i + 1 < args.len {
			branch = args[i + 1]
		}
	}
	if step < 1 {
		step = 1
	}

	ref := resolve_history_ref(branch)
	mut selected := []string{}
	if latest > 0 {
		// benchmark the N most recent first-parent commits (newest last, so they
		// are stored oldest-first)
		res :=
			os.execute('git -C ${os.quoted_path(vdir)} log ${os.quoted_path(ref)} --first-parent -n ${latest} --pretty=format:%H')
		if res.exit_code != 0 {
			return error('could not read history from `${ref}`: ${res.output.trim_space()}')
		}
		// git log lists newest-first; append in reverse so they are stored oldest-first
		lines := res.output.split_into_lines().filter(it.len > 0)
		for i := lines.len - 1; i >= 0; i-- {
			selected << lines[i]
		}
		elog('benchmarking the ${selected.len} latest commits on ${ref}')
	} else {
		// Anchor both boundaries at midnight: git reads a bare `2026-01-01` as that
		// date at the current time of day, which drops early Jan 1 commits and shifts
		// every Nth sample. The `T00:00:00` form has no space, so it stays shell-safe.
		from := '${year}-01-01T00:00:00'
		to := '${year + 1}-01-01T00:00:00'
		log_cmd := 'git -C ${os.quoted_path(vdir)} log ${os.quoted_path(ref)} --first-parent --reverse --since=${from} --until=${to} --pretty=format:%H'
		res := os.execute(log_cmd)
		if res.exit_code != 0 {
			return error('could not read history from `${ref}`: ${res.output.trim_space()}')
		}
		commits := res.output.split_into_lines().filter(it.len > 0)
		if commits.len == 0 {
			elog('no commits found for ${year} on ${ref}')
			return
		}
		// Start at step-1 so "every 50th commit" actually samples the 50th, 100th,
		// ... commits (1-based), not the 1st, 51st, 101st.
		for i := step - 1; i < commits.len; i += step {
			selected << commits[i]
		}
		elog('year ${year}: ${commits.len} commits on ${ref}, sampling every ${step}th => ${selected.len} benchmarks')
	}

	// A dry run only lists what would be measured; keep it read-only and never
	// touch the database (the source dir may not even be writable).
	if dry {
		for idx, c in selected {
			short := c[..8]
			message := git(vdir, 'log -n1 --pretty=format:%s ${c}')
			ts := git(vdir, 'log -n1 --pretty=format:%ct ${c}')
			date := time.unix(ts.i64())
			elog('[${idx + 1:2}/${selected.len}] ${short} ${date.format()} ${message}')
		}
		elog('dry run: nothing was built or stored')
		return
	}

	// Refresh the shared oldv cache once up front. oldv only auto-syncs when
	// ~/.cache/oldv/{v,vc} is absent, so without this a stale cache cannot build
	// commits newer than the last sync - breaking the scheduled/resumable use.
	sync_oldv_cache()

	mut db := open_db()!
	defer {
		db.close() or {}
	}

	mut ok, mut failed, mut skipped := 0, 0, 0
	for idx, c in selected {
		short := c[..8]
		message := git(vdir, 'log -n1 --pretty=format:%s ${c}')
		ts := git(vdir, 'log -n1 --pretty=format:%ct ${c}')
		date := time.unix(ts.i64())
		elog('[${idx + 1:2}/${selected.len}] ${short} ${date.format()} ${message}')
		if benchmark_exists(db, short) {
			elog('  already benchmarked, skipping')
			skipped++
			continue
		}
		b := benchmark_commit(c, short, message, date, args) or {
			elog('  FAILED to benchmark ${short}: ${err}')
			failed++
			continue
		}
		// The `commit_hash` UNIQUE constraint makes this insert the atomic claim:
		// if a concurrent run (e.g. cron overlapping a manual backfill) already
		// stored this commit, the insert is rejected instead of duplicating the row.
		insert_benchmark(mut db, b) or {
			elog('  ${short} was stored by a concurrent run, skipping: ${err}')
			skipped++
			continue
		}
		ok++
		elog('  stored ${short}: v.c ${b.v_c_ms}ms, v ${b.v_self_ms}ms, hello ${b.hello_ms}ms (${ok} done, ${failed} failed, ${skipped} skipped)')
	}
	elog('run done: ${ok} stored, ${failed} failed, ${skipped} skipped. Start the web app with: v run . serve')
}

// cmd_remeasure re-measures every commit already in the database, replacing each
// row with a fresh measurement. Used to backfill new metrics (e.g. RSS) onto
// existing rows without changing which commits are tracked. It resolves each
// stored short hash to a full hash so the cached oldv build is reused.
fn cmd_remeasure(args []string) ! {
	mut db := open_db()!
	defer {
		db.close() or {}
	}
	rows := load_benchmarks(db)!
	if rows.len == 0 {
		elog('remeasure: database is empty, nothing to do')
		return
	}
	elog('remeasuring ${rows.len} commits (refreshes timings, adds RSS) ...')
	sync_oldv_cache()

	mut ok, mut failed := 0, 0
	for idx, r in rows {
		short := r.commit_hash
		mut full := git(vdir, 'rev-parse ${short}')
		if full == '' {
			full = short
		}
		elog('[${idx + 1:2}/${rows.len}] ${short} ${r.commit_date.format()} ${r.message}')
		b := benchmark_commit(full, short, r.message, r.commit_date, args) or {
			elog('  FAILED to remeasure ${short}: ${err}')
			failed++
			continue
		}
		delete_benchmark(mut db, short)!
		insert_benchmark(mut db, b)!
		ok++
		elog('  updated ${short}: v.c ${b.v_c_ms}ms, self RSS med ${b.self_rss_med_kb / 1024}MB peak ${b.self_rss_max_kb / 1024}MB')
	}
	elog('remeasure done: ${ok} updated, ${failed} failed')
}

// benchmark_commit builds V exactly as it was at `commit` using the `oldv` tool,
// then builds an optimized vprod and runs the measurements in that historic
// checkout. The main repo is never touched — oldv works entirely inside its own
// cache folder.
fn benchmark_commit(commit string, short string, message string, date time.Time, args []string) !Benchmark {
	dir := build_with_oldv(commit, args)!
	build_vprod(dir, args)!
	return run_measurements(dir, short, message, date, args)!
}

// sync_oldv_cache updates the shared oldv source cache (~/.cache/oldv/{v,vc}).
// It is best-effort: if the network is down but the cache already covers the
// selected commits, the per-commit builds can still succeed from the local copy.
fn sync_oldv_cache() {
	elog('  oldv: syncing source cache in ${oldv_cache} ...')
	lexec('${os.quoted_path(vexe())} run ${os.quoted_path(oldv_src)} --cache-sync')
}

// build_with_oldv builds (or reuses a cached build of) V at `commit` via the
// oldv tool, and returns the path to that historic V checkout.
//
// oldv handles the hard parts: it finds the matching vc bootstrap commit, cleans
// the checkout, prepares tcc, compiles v.c with the C compiler and self-builds a
// working `./v` — the same way fast.vlang.io always produced historic compilers.
fn build_with_oldv(commit string, args []string) !string {
	dir := os.join_path(oldv_cache, 'v_at_' + commit)
	built_v := os.join_path(dir, exe_name('v'))
	if os.is_executable(built_v) {
		elog('  oldv: reusing cached build at ${dir}')
		return dir
	}
	cc := if args.contains('-clang') { 'clang' } else { 'cc' }
	elog('  oldv: building V @ ${commit[..8]} (cc=${cc}) ...')
	// `v run cmd/tools/oldv.v <commit>` clones v+vc into <cache>/oldv on first
	// use, then checks out and bootstraps the requested commit.
	cmd := '${os.quoted_path(vexe())} run ${os.quoted_path(oldv_src)} --cc ${cc} ${commit}'
	code := os.system(cmd)
	if code != 0 || !os.is_executable(built_v) {
		return error('oldv could not build ${commit[..8]} (expected ${built_v})')
	}
	return dir
}
