// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import time
import log

// fast powers https://fast.vlang.io — the "Is V still fast?" dashboard.
//
// It measures how long the V compiler needs to compile itself (and a couple of
// small programs) for a given commit, stores every measurement in a local
// SQLite database via V's ORM, and serves a small veb web app that visualises
// the history.
//
// Usage:
//   v run . serve [-port 8080]              start the web app (default command)
//   v run . bench [-clang] [-noprod]        benchmark the current HEAD commit
//   v run . run [-year 2026] [-step 50] [-latest N] [-branch <ref>] [-dry-run]
//                                           benchmark every <step>th commit of <year>,
//                                           or the N most recent commits with -latest
//   v run . seed                            insert demo rows (to preview the UI)
//   v run . import [--since D] [--ref R] <table.html> [...]  migrate old history
//   v run . export [-o <dir>]               render a static site for GitHub Pages
//   v run . help

const fast_dir = os.real_path(os.dir(@FILE))
const vdir = os.real_path(os.dir(os.dir(os.dir(fast_dir))))
const db_path = os.join_path(fast_dir, 'fast.db')
const log_path = os.join_path(fast_dir, 'fast.log')

// measurement sampling parameters. Tuned for a local backfill: fewer samples
// than the historic AWS setup (which used 2/20/16), so 56 commits finish in a
// reasonable time. We still discard the slowest samples to cut noise.
const warmup_samples = 1
const max_samples = 8
const discard_highest_samples = 3
const rss_samples = 5 // runs used for the peak-RSS five-number summary
const voptions = ' -skip-unused -show-timings -stats '

fn elog(msg string) {
	line := '${time.now().format_ss_micro()} ${msg}\n'
	if mut f := os.open_append(log_path) {
		f.write_string(line) or {}
		f.close()
	}
	log.info(msg)
}

fn lexec(cmd string) string {
	elog('  lexec: ${cmd}')
	res := os.execute(cmd)
	if res.exit_code != 0 {
		elog('  lexec FAILED, exit_code: ${res.exit_code}, output:\n${res.output}')
	}
	return res.output.trim_right('\r\n')
}

// git runs a git subcommand against `dir` and returns its trimmed output.
fn git(dir string, subcmd string) string {
	return lexec('git -C ${os.quoted_path(dir)} ${subcmd}')
}

// short_hash abbreviates a commit hash to 6 characters for display, without
// panicking on shorter hashes that `import` accepts.
fn short_hash(h string) string {
	return if h.len > 6 { h[..6] } else { h }
}

// exe_name returns the platform-specific executable file name (V and its
// builds are named `v`/`vprod` on unix, `v.exe`/`vprod.exe` on Windows).
fn exe_name(base string) string {
	$if windows {
		return base + '.exe'
	} $else {
		return base
	}
}

fn main() {
	// ensure all log messages are visible, even if the program later panics
	log.use_stdout()
	log.set_always_flush(true)

	args := arguments()
	cmd := if args.len > 1 { args[1] } else { 'serve' }
	rest := if args.len > 2 { args[2..] } else { []string{} }

	match cmd {
		'serve', 'server', 'web' {
			serve(rest) or { fatal('serve failed: ${err}') }
		}
		'bench' {
			cmd_bench(rest) or { fatal('bench failed: ${err}') }
		}
		'run', 'run-2026', 'backfill' {
			cmd_run(rest) or { fatal('run failed: ${err}') }
		}
		'remeasure' {
			cmd_remeasure(rest) or { fatal('remeasure failed: ${err}') }
		}
		'seed' {
			cmd_seed() or { fatal('seed failed: ${err}') }
		}
		'import' {
			cmd_import(rest) or { fatal('import failed: ${err}') }
		}
		'export' {
			cmd_export(rest) or { fatal('export failed: ${err}') }
		}
		'help', '-h', '--help' {
			print_help()
		}
		else {
			eprintln('unknown command: `${cmd}`\n')
			print_help()
			exit(1)
		}
	}
}

fn fatal(msg string) {
	elog(msg)
	exit(1)
}

fn print_help() {
	println('fast — the "Is V still fast?" benchmark dashboard

Commands:
  serve [-port 8080]                 start the veb web app (default)
  bench [-clang] [-noprod]           benchmark the current HEAD commit
  run [-year 2026] [-step 50] [-latest N] [-branch <ref>] [-dry-run]
                                     benchmark every <step>th commit of a year,
                                     or the N most recent commits with -latest
  remeasure                          re-measure every stored commit (backfill new metrics)
  seed                               insert demo rows (to preview the UI)
  import [--since YYYY-MM-DD] [--ref <ref>] <table.html> [...]
                                     migrate old fast.vlang.io history into fast.db
                                     (--ref: the history the rows belong to;
                                      defaults to the repo default branch)
  export [-o <dir>]                  render a static site (index.html + json) for GitHub Pages
  help                               show this help

Database: ${db_path}')
}

// cmd_seed inserts a handful of synthetic rows so the web UI can be previewed
// without waiting for real (slow) compiler benchmarks.
fn cmd_seed() ! {
	mut db := open_db()!
	defer {
		db.close() or {}
	}
	// Never mix synthetic demo rows with real benchmark data: refuse a non-empty
	// database, and claim a distinct `seed-demo` history so a later real `run`/
	// `bench` is rejected. Use a separate/empty database to preview the UI.
	existing := load_benchmarks(db)!
	if existing.len > 0 {
		return error('fast.db already contains ${existing.len} rows; refusing to add demo data. Use a separate, empty database for `seed`.')
	}
	demo_ref := claim_history(mut db, 'seed-demo')!
	base := time.new(year: 2026, month: 1, day: 5, hour: 12)
	mut samples := [
		Benchmark{
			commit_hash: 'e1e6ddce'
			message:     'markused: add array method map and filter support'
			commit_date: base
			v_c_ms:      1420
			v_self_ms:   3980
			hello_ms:    170
			vc_size_kb:  5400
			scan_ms:     95
			parse_ms:    210
			check_ms:    360
			cgen_ms:     540
			vlines:      412000
		},
		Benchmark{
			commit_hash: 'a1b2c3d4'
			message:     'checker: speed up generic instantiation'
			commit_date: base.add_days(40)
			v_c_ms:      1360
			v_self_ms:   3910
			hello_ms:    166
			vc_size_kb:  5420
			scan_ms:     93
			parse_ms:    205
			check_ms:    330
			cgen_ms:     535
			vlines:      418000
		},
		Benchmark{
			commit_hash: 'f9e8d7c6'
			message:     'cgen: reduce duplicated string temporaries'
			commit_date: base.add_days(95)
			v_c_ms:      1405
			v_self_ms:   3960
			hello_ms:    172
			vc_size_kb:  5390
			scan_ms:     96
			parse_ms:    208
			check_ms:    345
			cgen_ms:     520
			vlines:      423000
		},
		Benchmark{
			commit_hash: 'd8abccbd'
			message:     'checker, cgen: fix fixed array struct initialization'
			commit_date: base.add_days(160)
			v_c_ms:      1338
			v_self_ms:   3875
			hello_ms:    161
			vc_size_kb:  5455
			scan_ms:     92
			parse_ms:    201
			check_ms:    322
			cgen_ms:     511
			vlines:      431000
		},
	]
	for mut s in samples {
		if benchmark_exists(db, s.commit_hash) {
			elog('seed: ${s.commit_hash} already present, skipping')
			continue
		}
		s.created_at = time.now()
		s.git_ref = demo_ref
		s.lines_per_s = if s.v_c_ms > 0 { int(f64(s.vlines) / f64(s.v_c_ms) * 1000.0) } else { 0 }
		insert_benchmark(mut db, s)!
		elog('seed: inserted ${s.commit_hash}')
	}
	elog('seed done. Run `v run . serve` and open http://localhost:8080')
}
