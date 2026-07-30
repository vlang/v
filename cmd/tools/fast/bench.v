// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import time
import arrays
import db.sqlite

// history_ref_for_head returns the history (branch) that the current HEAD belongs
// to. On a normal checkout that is the branch name; on a detached HEAD (common in
// CI / pinned deployments) `--abbrev-ref` yields the literal `HEAD`, so resolve it
// to the branch/history that contains the commit — preferring the repository
// default — instead of the meaningless `HEAD`.
fn history_ref_for_head() string {
	branch := git(vdir, 'rev-parse --abbrev-ref HEAD')
	if branch != 'HEAD' {
		return branch
	}
	// detached: if HEAD is on the default branch's history, use that. Skip when the
	// default is itself an unresolved `HEAD` (shallow checkout with no refs), since
	// `merge-base --is-ancestor HEAD HEAD` trivially succeeds and would claim `HEAD`.
	default_ref := resolve_history_ref('')
	if default_ref != 'HEAD'
		&& os.execute('git -C ${os.quoted_path(vdir)} merge-base --is-ancestor HEAD ${os.quoted_path(default_ref)}').exit_code == 0 {
		return default_ref
	}
	// otherwise pick a remote/local branch that contains the commit
	for pattern in ['refs/remotes', 'refs/heads'] {
		res :=
			os.execute('git -C ${os.quoted_path(vdir)} for-each-ref --contains HEAD --format=${os.quoted_path('%(refname)')} ${pattern}')
		if res.exit_code == 0 {
			for line in res.output.split_into_lines() {
				r := line.trim_space()
				if r != '' && !r.ends_with('/HEAD') {
					return r
				}
			}
		}
	}
	return 'HEAD' // truly unattached commit
}

// cmd_bench benchmarks the current HEAD of the main repo and stores the result.
fn cmd_bench(args []string) ! {
	commit := git(vdir, 'rev-parse HEAD')[..8]
	message := git(vdir, 'log -n1 --pretty=format:%s ${commit}')
	// committer date (%ct), not author date (%at): it is monotonic along
	// first-parent history, so ordering by it preserves ancestry order.
	ts := git(vdir, 'log -n1 --pretty=format:%ct ${commit}')
	date := time.unix(ts.i64())
	// the history this HEAD belongs to (its branch, resolved even when detached)
	head_ref := history_ref_for_head()
	elog('Benchmarking HEAD ${commit} on ${head_ref} "${message}" (${date.format()})')

	mut db := open_db()!
	defer {
		db.close() or {}
	}

	// Serialize against any other benchmark process before touching the shared main
	// checkout: bench rebuilds vprod and overwrites v.c / v2 / the hello binary in
	// `vdir`, which a concurrent bench (or run) would clobber — corrupting the build
	// or contaminating the timings. run's oldv builds take the same global lock.
	mut blk := acquire_build_lock() or {
		elog('another benchmark build is in progress; skipping bench for ${commit}')
		return
	}
	defer {
		blk.release()
	}

	// claim/validate this database's single history (normalized, so `master` and
	// `origin/master` are the same); rejects mixing another branch's history. Remember
	// whether it was already claimed, so a claim made by this call can be undone if the
	// benchmark below fails and leaves the database empty.
	already := history_claimed(db)
	history := claim_history(mut db, head_ref)!
	// Bail out before the expensive rebuild + measurement suite, so repeat
	// invocations for an unchanged HEAD stay cheap.
	if benchmark_exists(db, commit) && !args.contains('-force') {
		elog('commit ${commit} is already benchmarked (use -force to re-run)')
		return
	}

	measure_and_store(mut db, commit, message, date, history, args) or {
		build_err := err
		// The build or measurement failed. If this call freshly claimed a previously
		// empty database, drop the claim: leaving it would permanently reject a later
		// successful bench for another ref (the same empty-database condition the
		// importer rolls back). The delete is conditional on the table still being empty
		// in one statement, so a concurrent import that commits rows first keeps its
		// claim (no count-then-delete race). A failed rollback is surfaced too, so a
		// full/locked database is not silently left claimed.
		if !already {
			migrate_exec(mut db,
				"DELETE FROM fast_meta WHERE key = 'history_ref' AND NOT EXISTS (SELECT 1 FROM benchmarks)") or {
				return error('${build_err}; also failed to release the empty history claim: ${err}')
			}
		}
		return build_err
	}
	elog('stored benchmark for ${commit}')
}

// measure_and_store builds vprod, runs the measurement suite in the main checkout, and
// stores the row. It is split out of cmd_bench so the caller can roll back a fresh,
// still-empty history claim if any step here fails.
fn measure_and_store(mut db sqlite.DB, commit string, message string, date time.Time, history string, args []string) ! {
	build_vprod(vdir, args)!
	mut b := run_measurements(vdir, commit, message, date, args)!
	b.git_ref = history
	// Atomic replace via upsert on the unique commit_hash: a `-force` re-measure
	// updates the row in a single statement, so an interrupted or failed store can
	// never lose the previous valid measurement (unlike a delete then insert).
	upsert_benchmark(mut db, b)!
}

// build_vprod builds an optimized `vprod` binary inside `dir` using that
// checkout's own `./v`.
fn build_vprod(dir string, args []string) ! {
	os.chdir(dir)!
	v := os.join_path('.', exe_name('v'))
	// V appends `.exe` to the output name on Windows, so use the same.
	vprod := os.join_path(dir, exe_name('vprod'))
	// Remove any stale vprod from a previous commit first: if the rebuild below
	// fails, a leftover binary must not be benchmarked under the new commit hash.
	os.rm(vprod) or {}
	mut build_cmd := '${v} -o vprod -prod cmd/v'
	if args.contains('-noprod') {
		build_cmd = '${v} -o vprod cmd/v'
		elog('  building vprod (fast, non-prod) in ${dir} ...')
	} else {
		// Note: intentionally NOT using -prealloc: on historic commits the
		// prealloc allocator can crash at startup (SIGBUS), which would make
		// every measurement time an instant crash instead of a real compile.
		elog('  building vprod (-prod) in ${dir} ...')
	}
	res := os.execute(build_cmd)
	if res.exit_code != 0 {
		return error('vprod build failed (exit ${res.exit_code}) in ${dir}:\n${res.output}')
	}
	if !os.exists(vprod) {
		return error('failed to build vprod in ${dir}')
	}
}

// run_measurements runs the full battery of measurements for the built checkout
// at `dir`, returning a populated Benchmark (id/created_at aside).
fn run_measurements(dir string, commit string, message string, date time.Time, args []string) !Benchmark {
	// Use the system C compiler (cc). tcc is not available on every machine
	// (e.g. arm64 macOS), and `-cc tcc` there silently falls back to cc anyway.
	ccompiler := if args.contains('-clang') { 'clang' } else { 'cc' }
	vprod := os.join_path(dir, exe_name('vprod'))
	os.chdir(dir)!
	stage_v := build_v3_stage_compiler(dir, date, args)!

	self_c_cmd := if stage_v == '' {
		'${os.quoted_path(vprod)} ${voptions} -o v.c cmd/v'
	} else {
		'${os.quoted_path(stage_v)} -selfhost -no-memory-limit -show-timings -stats -o v.c vlib/v3/v3.v'
	}
	self_bin_cmd := if stage_v == '' {
		'${os.quoted_path(vprod)} ${voptions} -cc ${ccompiler} -o v2 cmd/v'
	} else {
		'${os.quoted_path(stage_v)} -selfhost -no-memory-limit -show-timings -stats -cc ${ccompiler} -o v2 vlib/v3/v3.v'
	}
	hello_cmd := if stage_v == '' {
		'${os.quoted_path(vprod)} ${voptions} -cc ${ccompiler} examples/hello_world.v'
	} else {
		'${os.quoted_path(stage_v)} -no-memory-limit -show-timings -stats -cc ${ccompiler} -o fast_hello examples/hello_world.v'
	}

	// Sanity probe: make sure the compiler actually self-compiles to a real v.c
	// before timing anything. A broken build (e.g. it crashes at startup) must
	// fail the commit, not record a ~2ms "instant crash" as a great result.
	os.rm('v.c') or {}
	probe := os.execute(self_c_cmd)
	if probe.exit_code != 0 || !os.exists('v.c') || os.file_size('v.c') < 100_000 {
		return error('self-compile probe failed in ${dir} (exit ${probe.exit_code}); skipping commit')
	}

	v_c := measure(self_c_cmd, 'v -o v.c')!
	v_self := measure(self_bin_cmd, 'v -o v')!
	hello := measure(hello_cmd, 'v hello.v')!
	vc_size := int(os.file_size('v.c') / 1000)
	stages := measure_steps_minimal(vprod, stage_v)!
	mut vlines := parse_vlines(probe.output)
	if vlines == 0 {
		vlines = stages.vlines
	}
	lines_per_s := if v_c > 0 { int(f64(vlines) / f64(v_c) * 1000.0) } else { 0 }

	// peak RSS (memory) five-number summaries for the box-and-whisker view
	self_rss := measure_rss(self_c_cmd, 'v self-compile RSS')
	hello_rss := measure_rss(hello_cmd, 'v hello.v RSS')

	return Benchmark{
		commit_hash:  commit
		message:      message
		commit_date:  date
		created_at:   time.now()
		v_c_ms:       v_c
		v_self_ms:    v_self
		hello_ms:     hello
		vc_size_kb:   vc_size
		scan_ms:      stages.scan_ms
		parse_ms:     stages.parse_ms
		check_ms:     stages.check_ms
		cgen_ms:      stages.cgen_ms
		scan_rss_kb:  stages.scan_rss_kb
		parse_rss_kb: stages.parse_rss_kb
		check_rss_kb: stages.check_rss_kb
		cgen_rss_kb:  stages.cgen_rss_kb
		vlines:       vlines
		lines_per_s:  lines_per_s

		self_rss_min_kb:  self_rss.min
		self_rss_q1_kb:   self_rss.q1
		self_rss_med_kb:  self_rss.med
		self_rss_q3_kb:   self_rss.q3
		self_rss_max_kb:  self_rss.max
		hello_rss_min_kb: hello_rss.min
		hello_rss_q1_kb:  hello_rss.q1
		hello_rss_med_kb: hello_rss.med
		hello_rss_q3_kb:  hello_rss.q3
		hello_rss_max_kb: hello_rss.max
	}
}

// build_v3_stage_compiler builds the standalone v3 driver used to self-compile
// vlib/v3/v3.v after the phase-RSS rollout. Older rows deliberately keep zeroes
// for these new fields, preserving the historical phase series.
fn build_v3_stage_compiler(dir string, date time.Time, args []string) !string {
	// 2026-07-30 00:00 Europe/Moscow, the day v3 became the macOS default.
	if date.unix() < 1785358800 {
		return ''
	}
	$if !macos {
		return ''
	}
	vprod := os.join_path(dir, exe_name('vprod'))
	stage_v := os.join_path(dir, exe_name('fastv3'))
	os.rm(stage_v) or {}
	prod := if args.contains('-noprod') { '' } else { '-prod' }
	cmd := '${os.quoted_path(vprod)} -gc none ${prod} -o ${os.quoted_path(stage_v)} vlib/v3/v3.v'
	elog('  building standalone v3 self-compiler ...')
	res := os.execute(cmd)
	if res.exit_code != 0 || !os.is_executable(stage_v) {
		return error('standalone v3 build failed (exit ${res.exit_code}) in ${dir}:\n${res.output}')
	}
	return stage_v
}

// RssStats is a five-number summary (KB) of peak RSS across several runs.
struct RssStats {
	min int
	q1  int
	med int
	q3  int
	max int
}

// measure_rss runs `cmd` rss_samples times, capturing each run's peak resident
// set size, and returns the five-number summary (KB) for a box-and-whisker plot.
fn measure_rss(cmd string, description string) RssStats {
	elog('  Measuring ${description}, samples: ${rss_samples}')
	mut vals := []int{}
	for _ in 0 .. rss_samples {
		kb := peak_rss_kb(cmd)
		if kb > 0 {
			vals << kb
		}
	}
	if vals.len == 0 {
		return RssStats{}
	}
	vals.sort()
	n := vals.len
	return RssStats{
		min: vals[0]
		q1:  vals[n / 4]
		med: vals[n / 2]
		q3:  vals[(3 * n) / 4]
		max: vals[n - 1]
	}
}

// peak_rss_kb runs `cmd` under the platform's `time` tool and returns its peak
// resident set size in KB. The return value distinguishes three cases:
//   >0  a valid measurement
//    0  this platform has no supported way to measure RSS (a real zero-data case)
//   -1  the timing binary is missing, or the measured command exited non-zero
//       (the run failed, so the sample must be rejected — `/usr/bin/time` can
//       still print a maxrss line for a process that crashed)
fn peak_rss_kb(cmd string) int {
	mut time_flag := ''
	$if macos {
		time_flag = '-l' // prints "<bytes>  maximum resident set size" to stderr
	} $else $if linux {
		time_flag = '-v' // prints "Maximum resident set size (kbytes): <kb>"
	} $else {
		return 0 // unsupported platform: legitimately no RSS data
	}
	if !os.exists('/usr/bin/time') {
		return -1 // timing binary unavailable
	}
	// Per-process + per-invocation file name, so overlapping sampler/remeasure
	// runs never clobber each other's `time` output (which would drop samples or
	// attribute another commit's RSS to this one).
	tmp := os.join_path(os.temp_dir(), 'fast_rss_${os.getpid()}_${time.sys_mono_now()}.txt')
	defer {
		os.rm(tmp) or {}
	}
	if os.system('/usr/bin/time ${time_flag} ${cmd} > /dev/null 2>${os.quoted_path(tmp)}') != 0 {
		return -1 // the measured command failed; reject this sample
	}
	out := os.read_file(tmp) or { return -1 }
	mut kb := i64(0)
	for line in out.split_into_lines() {
		$if macos {
			if line.contains('maximum resident set size') {
				kb = line.trim_space().all_before(' ').i64() / 1024
			}
		} $else {
			if line.contains('Maximum resident set size') {
				kb = line.all_after(':').trim_space().i64()
			}
		}
	}
	return int(kb)
}

// measure returns the average wall-clock time (ms) for `cmd`, discarding the
// highest samples to reduce noise from random load spikes. It errors if any run
// of `cmd` exits non-zero, so a commit whose compilation fails (e.g. an old
// revision that emits v.c but cannot complete the C compile with the selected
// cc) is skipped instead of recording the short failure time as a great result.
fn measure(cmd string, description string) !int {
	elog('  Measuring ${description}, warmups: ${warmup_samples}, samples: ${max_samples}, discard: ${discard_highest_samples}')
	for _ in 0 .. warmup_samples {
		res := os.execute(cmd)
		if res.exit_code != 0 {
			return error('warmup failed (exit ${res.exit_code}): `${cmd}`\n${res.output}')
		}
	}
	mut runs := []int{}
	for r in 0 .. max_samples {
		sw := time.new_stopwatch()
		res := os.execute(cmd)
		sample := int(sw.elapsed().milliseconds())
		if res.exit_code != 0 {
			return error('command failed (exit ${res.exit_code}): `${cmd}`\n${res.output}')
		}
		runs << sample
		elog('    sample ${r + 1:2}/${max_samples:2} ... ${sample} ms')
	}
	runs.sort()
	for _ in 0 .. discard_highest_samples {
		runs.pop()
	}
	if runs.len == 0 {
		return 0
	}
	return int(f64(arrays.sum(runs) or { 0 }) / runs.len)
}

struct StageMeasurements {
mut:
	scan_ms      int
	parse_ms     int
	check_ms     int
	cgen_ms      int
	scan_rss_kb  int
	parse_rss_kb int
	check_rss_kb int
	cgen_rss_kb  int
	vlines       int
}

// measure_steps_minimal runs the compiler several times, capturing the minimum
// time and RSS reported for each compiler stage.
fn measure_steps_minimal(vprod string, stage_v string) !StageMeasurements {
	mut scans, mut parses, mut checks, mut cgens := []int{}, []int{}, []int{}, []int{}
	mut scan_rss, mut parse_rss, mut check_rss, mut cgen_rss := []int{}, []int{}, []int{}, []int{}
	mut vliness := []int{}
	for _ in 0 .. max_samples {
		sample := measure_steps_one_sample(vprod, stage_v)!
		scans << sample.scan_ms
		parses << sample.parse_ms
		checks << sample.check_ms
		cgens << sample.cgen_ms
		scan_rss << sample.scan_rss_kb
		parse_rss << sample.parse_rss_kb
		check_rss << sample.check_rss_kb
		cgen_rss << sample.cgen_rss_kb
		vliness << sample.vlines
	}
	return StageMeasurements{
		scan_ms:      arrays.min(scans) or { 0 }
		parse_ms:     arrays.min(parses) or { 0 }
		check_ms:     arrays.min(checks) or { 0 }
		cgen_ms:      arrays.min(cgens) or { 0 }
		scan_rss_kb:  arrays.min(scan_rss) or { 0 }
		parse_rss_kb: arrays.min(parse_rss) or { 0 }
		check_rss_kb: arrays.min(check_rss) or { 0 }
		cgen_rss_kb:  arrays.min(cgen_rss) or { 0 }
		vlines:       arrays.max(vliness) or { 0 }
	}
}

fn measure_steps_one_sample(vprod string, stage_v string) !StageMeasurements {
	cmd := if stage_v == '' {
		'${os.quoted_path(vprod)} ${voptions} -o v.c cmd/v'
	} else {
		'${os.quoted_path(stage_v)} -selfhost -no-memory-limit -show-timings -stats -o v.c vlib/v3/v3.v'
	}
	resp := os.execute(cmd)
	if resp.exit_code != 0 {
		return error('stage-timing run failed (exit ${resp.exit_code}): `${cmd}`\n${resp.output}')
	}
	return parse_stage_measurements(resp.output)
}

fn parse_stage_measurements(output string) !StageMeasurements {
	if output.contains(' MB RSS') {
		return parse_v3_stage_measurements(output)
	}

	mut result := StageMeasurements{
		vlines: parse_vlines(output)
	}
	lines := output.split_into_lines()
	if lines.len == 3 {
		result.parse_ms = lines[0].before('.').int()
		result.check_ms = lines[1].before('.').int()
		result.cgen_ms = lines[2].before('.').int()
	} else {
		ms_lines := lines.map(it.split('  ms '))
		for line in ms_lines {
			if line.len == 2 {
				match line[1] {
					'SCAN' { result.scan_ms = line[0].int() }
					'PARSE' { result.parse_ms = line[0].int() }
					'CHECK' { result.check_ms = line[0].int() }
					'C GEN' { result.cgen_ms = line[0].int() }
					else {}
				}
			}
		}
	}
	// Both output formats set parse/check/cgen on success; if none parsed, the
	// output was unusable (e.g. a crash that still exited 0), so reject it rather
	// than letting arrays.min pick these zeroes into the stored row.
	if result.parse_ms == 0 && result.check_ms == 0 && result.cgen_ms == 0 {
		return error('could not parse stage timings from output:\n${output}')
	}
	return result
}

fn parse_v3_stage_measurements(output string) !StageMeasurements {
	mut result := StageMeasurements{
		vlines: parse_vlines(output)
	}
	for raw_line in output.split_into_lines() {
		line := raw_line.trim_space()
		ms_pos := line.index(' ms') or { continue }
		left := line[..ms_pos].trim_space()
		parts := left.fields()
		if parts.len < 2 {
			continue
		}
		label := parts[..parts.len - 1].join(' ')
		ms := int(parts.last().f64())
		rss_mb := rss_mb_from_v3_line(line)
		match true {
			label == 'parse setup/cache' {
				// v3 scans while setting up its parse pipeline, so this is the
				// closest equivalent to the old compiler's standalone SCAN stage.
				result.scan_ms = ms
				result.scan_rss_kb = rss_mb * 1024
			}
			label == 'parse .vh' || label.starts_with('parse .v') || label == 'resolve imports' {
				result.parse_ms += ms
				result.parse_rss_kb = rss_mb * 1024
			}
			label == 'check' || label.starts_with('check (') {
				result.check_ms = ms
				result.check_rss_kb = rss_mb * 1024
			}
			label == 'cgen' || label.starts_with('cgen (') {
				result.cgen_ms = ms
				result.cgen_rss_kb = rss_mb * 1024
			}
			else {}
		}
	}
	if result.parse_ms == 0 && result.check_ms == 0 && result.cgen_ms == 0 {
		return error('could not parse v3 stage timings from output:\n${output}')
	}
	return result
}

fn rss_mb_from_v3_line(line string) int {
	rss_end := line.index(' MB RSS') or { return 0 }
	return line[..rss_end].trim_space().all_after_last(' ').int()
}

fn parse_vlines(output string) int {
	for raw_line in output.split_into_lines() {
		line := raw_line.trim_space()
		if line.starts_with('parsed .v lines') {
			return line.trim_string_left('parsed .v lines').trim_space().all_before(' ').int()
		}
		if line.contains('V') && line.contains('source') && line.contains('size:') {
			start := line.index(':') or { continue }
			end := line.index('lines,') or { continue }
			return line[start + 1..end].trim_space().int()
		}
	}
	return 0
}
