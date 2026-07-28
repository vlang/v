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
	mut branch := ''
	dry := args.contains('-dry-run')
	for i, a in args {
		if a == '-year' && i + 1 < args.len {
			year = args[i + 1].int()
		}
		if a == '-step' && i + 1 < args.len {
			step = args[i + 1].int()
		}
		if a == '-branch' && i + 1 < args.len {
			branch = args[i + 1]
		}
	}
	if step < 1 {
		step = 1
	}

	ref := resolve_history_ref(branch)
	from := '${year}-01-01'
	to := '${year + 1}-01-01'
	log_cmd := 'git -C ${os.quoted_path(vdir)} log ${ref} --first-parent --reverse --since=${from} --until=${to} --pretty=format:%H'
	res := os.execute(log_cmd)
	if res.exit_code != 0 {
		return error('could not read history from `${ref}`: ${res.output.trim_space()}')
	}
	commits := res.output.split_into_lines().filter(it.len > 0)
	if commits.len == 0 {
		elog('no commits found for ${year} on ${ref}')
		return
	}

	mut selected := []string{}
	for i := 0; i < commits.len; i += step {
		selected << commits[i]
	}
	elog('year ${year}: ${commits.len} commits on ${ref}, sampling every ${step}th => ${selected.len} benchmarks')

	// A dry run only lists what would be measured; keep it read-only and never
	// touch the database (the source dir may not even be writable).
	if dry {
		for idx, c in selected {
			short := c[..8]
			message := git(vdir, 'log -n1 --pretty=format:%s ${c}')
			ts := git(vdir, 'log -n1 --pretty=format:%at ${c}')
			date := time.unix(ts.i64())
			elog('[${idx + 1:2}/${selected.len}] ${short} ${date.format()} ${message}')
		}
		elog('dry run: nothing was built or stored')
		return
	}

	mut db := open_db()!
	defer {
		db.close() or {}
	}

	mut ok, mut failed, mut skipped := 0, 0, 0
	for idx, c in selected {
		short := c[..8]
		message := git(vdir, 'log -n1 --pretty=format:%s ${c}')
		ts := git(vdir, 'log -n1 --pretty=format:%at ${c}')
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
		insert_benchmark(mut db, b)!
		ok++
		elog('  stored ${short}: v.c ${b.v_c_ms}ms, v ${b.v_self_ms}ms, hello ${b.hello_ms}ms (${ok} done, ${failed} failed, ${skipped} skipped)')
	}
	elog('run done: ${ok} stored, ${failed} failed, ${skipped} skipped. Start the web app with: v run . serve')
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
