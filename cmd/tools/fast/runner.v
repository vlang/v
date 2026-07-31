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

	// Serialize the whole run behind the global oldv build lock before touching any
	// shared state: sync_oldv_cache and every per-commit build reuse oldv's shared
	// source cache and `v_at_vc` checkout, so a second run must not proceed in parallel.
	mut blk := acquire_build_lock() or {
		elog('another benchmark run holds the build lock; exiting (nothing to do)')
		return
	}
	defer {
		blk.release()
	}

	// Refresh the shared oldv cache once up front. oldv only auto-syncs when
	// ~/.cache/oldv/{v,vc} is absent, so without this a stale cache cannot build
	// commits newer than the last sync - breaking the scheduled/resumable use.
	sync_oldv_cache()

	mut db := open_db()!
	defer {
		db.close() or {}
	}
	// claim this database's single history — mixing refs breaks the chart's
	// ancestry assumption (see claim_history). `history` is the normalized ref. Record
	// whether the database was already claimed, so a claim this call makes on a fresh
	// database can be rolled back if the run ends up storing nothing.
	already := history_claimed(db)
	history := claim_history(mut db, ref)!

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
		mut b := benchmark_commit(c, short, message, date, args) or {
			elog('  FAILED to benchmark ${short}: ${err}')
			failed++
			continue
		}
		b.git_ref = history
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
	// Every sampled build/insert failed on a fresh database: drop the claim we made, so
	// a later run/bench/import for another ref is not rejected by an otherwise empty
	// database (as the import and bench paths do). The delete is conditional on the table
	// still being empty *in one statement*, so a concurrent import that commits rows
	// between here and the delete keeps its claim (no separate count-then-delete race). A
	// failed DELETE (db full/locked/I/O) is surfaced, not logged as released.
	if ok == 0 && !already {
		migrate_exec(mut db,
			"DELETE FROM fast_meta WHERE key = 'history_ref' AND NOT EXISTS (SELECT 1 FROM benchmarks)")!
		if !history_claimed(db) {
			elog('no benchmarks stored; released the history claim on the empty database')
		}
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
	// Serialize against any concurrent run/remeasure before touching oldv's shared
	// source cache and `v_at_vc` checkout (see acquire_build_lock).
	mut blk := acquire_build_lock() or {
		elog('another benchmark run holds the build lock; exiting (nothing to do)')
		return
	}
	defer {
		blk.release()
	}
	elog('remeasuring ${rows.len} commits (refreshes timings, adds RSS) ...')
	sync_oldv_cache()

	mut ok, mut failed := 0, 0
	for idx, r in rows {
		short := r.commit_hash
		// Resolve the short hash to a full one so the cached oldv build is reused.
		// `git()` ignores the exit status and returns combined output, and a failed
		// rev-parse still echoes the input token before its fatal diagnostic, so
		// check the exit code explicitly and fall back to the short hash only on a
		// genuine resolution failure (e.g. a shallow clone).
		rp := os.execute('git -C ${os.quoted_path(vdir)} rev-parse ${os.quoted_path(short)}')
		full := if rp.exit_code == 0 { rp.output.trim_space() } else { short }
		elog('[${idx + 1:2}/${rows.len}] ${short} ${r.commit_date.format()} ${r.message}')
		mut b := benchmark_commit(full, short, r.message, r.commit_date, args) or {
			elog('  FAILED to remeasure ${short}: ${err}')
			failed++
			continue
		}
		b.git_ref = r.git_ref // preserve the history this row already belongs to
		// atomic replace: an upsert on the unique commit_hash cannot lose the
		// existing row if it fails, unlike a delete followed by a separate insert
		upsert_benchmark(mut db, b) or {
			elog('  FAILED to store ${short}: ${err}')
			failed++
			continue
		}
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

// The build lock serializes a whole build+measure against every other benchmark
// process on this machine. It is global — one lock, not per-commit — for two reasons:
// `run` has every commit's oldv build check the single shared `v_at_vc` checkout out to
// a commit-specific VC revision (see cmd/tools/oldv.v and vgit.prepare_vc_source), so
// two runs building *different* commits still race on that shared checkout and its
// `v.c`; and `bench` rebuilds vprod and overwrites `v.c` / `v2` / the hello binary
// directly in the main checkout. Holding it across the measurement, and around the
// shared `sync_oldv_cache`, also keeps concurrent runs from contending for the CPU and
// contaminating each other's timings.
const build_lock_dir = os.join_path(oldv_cache, 'fast-build.lock')
const build_lock_owner = os.join_path(build_lock_dir, 'owner')

// A lock whose owner heartbeat has not been refreshed for longer than this is treated
// as belonging to a crashed run and may be reclaimed.
const build_lock_stale_secs = i64(60 * 60)

// The background heartbeat refreshes the owner file this often while the lock is held.
// It is kept well under the stale threshold so a healthy but slow build — one oldv
// checkout plus the full measurement suite can exceed an hour on a slow machine — is
// never mistaken for a crash and reclaimed out from under it.
const build_lock_heartbeat_secs = 5 * 60

// BuildLock is a held global build lock. It owns a background thread that keeps the
// owner heartbeat fresh for the lock's whole lifetime, so a long build/measurement is
// not reclaimed as stale, and its release() only removes the directory while this
// process still owns it. `owner` is the owner file, kept open so heartbeats can refresh
// it through the descriptor rather than by path (see refresh_owner).
struct BuildLock {
mut:
	token string // unique per acquisition; identifies this owner in the owner file
	owner os.File
	stop  chan bool
	hb    thread
}

// acquire_build_lock takes the global build lock, returning none if another live run
// holds it. Mutual exclusion is an atomic mkdir. A lock left by a crashed run is
// reclaimed by atomically renaming the stale directory aside and then deleting *that*
// copy — never build_lock_dir itself. So two processes recovering from the same crash
// cannot both delete a fresh lock: rename() on one source is atomic, only one reclaimer
// captures the stale directory, and a replacement lock another owner recreates at
// build_lock_dir in the meantime is left untouched. On success a heartbeat thread is
// started; the caller must release() the returned lock.
fn acquire_build_lock() ?BuildLock {
	os.mkdir_all(oldv_cache) or {}
	for attempt in 0 .. 5 {
		if dir_created(build_lock_dir) {
			// A token unique to this acquisition (pid is not enough: a reused pid after
			// this process dies must not look like the same owner). Everything downstream
			// checks the token, so a reclaimed lock is never mistaken for still ours.
			token := '${os.getpid()}-${time.sys_mono_now()}'
			// Keep the owner file open: heartbeats refresh it through THIS descriptor, so
			// after a reclaim renames our directory aside the writes land on our orphaned
			// inode and cannot overwrite the successor's owner file.
			mut owner := os.open_file(build_lock_owner, 'w') or {
				os.rmdir_all(build_lock_dir) or {}
				return none
			}
			refresh_owner(mut owner, token) // record ownership + first heartbeat
			stop := chan bool{}
			hb := spawn build_lock_heartbeat(owner, token, stop)
			return BuildLock{
				token: token
				owner: owner
				stop:  stop
				hb:    hb
			}
		}
		if build_lock_age() <= build_lock_stale_secs {
			return none // held by a live run (or one still stamping a fresh lock)
		}
		// Stale: capture the directory by moving it aside, then discard our captured
		// copy. Losers of the rename race fall through and re-evaluate — by then the
		// winner may have already recreated a fresh, live lock.
		aside := '${build_lock_dir}.stale.${os.getpid()}.${attempt}'
		if renamed(build_lock_dir, aside) {
			os.rmdir_all(aside) or {}
		}
	}
	return none
}

// heartbeat refreshes the owner file every build_lock_heartbeat_secs until release()
// signals it to stop, so a build/measurement that runs longer than the stale threshold
// keeps the lock alive instead of letting a second invocation reclaim it as stale. It
// refreshes through the held descriptor, which is atomic with respect to a reclaim: if
// this process was suspended past the stale threshold and a successor took over, our
// descriptor points at the orphaned inode, so the refresh cannot overwrite the
// successor's owner file (nor make us later delete its live lock).
fn build_lock_heartbeat(owner_file os.File, token string, stop chan bool) {
	mut owner := owner_file // shares the underlying descriptor with the returned lock
	for {
		select {
			_ := <-stop {
				return
			}
			build_lock_heartbeat_secs * time.second {
				refresh_owner(mut owner, token)
			}
		}
	}
}

// build_lock_age reports the seconds since the lock was last refreshed. It prefers the
// heartbeat `owner` file, but falls back to the directory's own creation time while a
// just-created lock has not been stamped yet, so the tiny window between mkdir and the
// first stamp counts as live rather than stale (never reclaimable out from under a new
// owner). A vanished lock reads as reclaimable.
fn build_lock_age() i64 {
	now := time.now().unix()
	if os.exists(build_lock_owner) {
		return now - os.file_last_mod_unix(build_lock_owner)
	}
	if os.exists(build_lock_dir) {
		return now - os.file_last_mod_unix(build_lock_dir)
	}
	return build_lock_stale_secs + 1
}

// refresh_owner (re)writes `token` through the held owner descriptor and flushes it, so
// the owner file's mtime advances (marking the lock live). Because it writes through the
// descriptor rather than reopening by path, a write after the directory was reclaimed
// lands on the orphaned inode and can never overwrite the successor's owner file — the
// property that makes the heartbeat safe against a resumed, previously suspended owner.
fn refresh_owner(mut owner os.File, token string) {
	owner.write_to(0, token.bytes()) or { return }
	owner.flush()
}

// release stops the heartbeat, closes the owner descriptor, and drops the lock — but only
// while this process still owns it. If the heartbeat ever stalled past the stale
// threshold and another run reclaimed the directory, the owner file now holds a different
// token; deleting it would clobber that live replacement lock, so leave it in place.
fn (mut bl BuildLock) release() {
	bl.stop <- true
	bl.hb.wait()
	owned := build_lock_owned_by(bl.token)
	bl.owner.close()
	if owned {
		os.rmdir_all(build_lock_dir) or {}
	}
}

// build_lock_owned_by reports whether the owner file still holds `token`, i.e. no other
// run has reclaimed the lock and stamped its own token in the meantime.
fn build_lock_owned_by(token string) bool {
	content := os.read_file(build_lock_owner) or { return false }
	return content.trim_space() == token
}

// dir_created reports whether it created `path`. mkdir is atomic, so exactly one of
// several racers creating the same directory succeeds.
fn dir_created(path string) bool {
	os.mkdir(path) or { return false }
	return true
}

// renamed reports whether it atomically moved `src` to `dst`.
fn renamed(src string, dst string) bool {
	os.rename(src, dst) or { return false }
	return true
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
	elog('  oldv: building V @ ${short_hash(commit)} (cc=${cc}) ...')
	// `v run cmd/tools/oldv.v <commit>` clones v+vc into <cache>/oldv on first
	// use, then checks out and bootstraps the requested commit.
	cmd := '${os.quoted_path(vexe())} run ${os.quoted_path(oldv_src)} --cc ${cc} ${os.quoted_path(commit)}'
	code := os.system(cmd)
	if code != 0 || !os.is_executable(built_v) {
		return error('oldv could not build ${short_hash(commit)} (expected ${built_v})')
	}
	return dir
}
