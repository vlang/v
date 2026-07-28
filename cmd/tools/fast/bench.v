// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import time
import arrays

// cmd_bench benchmarks the current HEAD of the main repo and stores the result.
fn cmd_bench(args []string) ! {
	commit := git(vdir, 'rev-parse HEAD')[..8]
	message := git(vdir, 'log -n1 --pretty=format:%s ${commit}')
	// committer date (%ct), not author date (%at): it is monotonic along
	// first-parent history, so ordering by it preserves ancestry order.
	ts := git(vdir, 'log -n1 --pretty=format:%ct ${commit}')
	date := time.unix(ts.i64())
	elog('Benchmarking HEAD ${commit} "${message}" (${date.format()})')

	mut db := open_db()!
	defer {
		db.close() or {}
	}
	// Bail out before the expensive rebuild + measurement suite, so repeat
	// invocations for an unchanged HEAD stay cheap.
	exists := benchmark_exists(db, commit)
	if exists && !args.contains('-force') {
		elog('commit ${commit} is already benchmarked (use -force to re-run)')
		return
	}

	build_vprod(vdir, args)!
	b := run_measurements(vdir, commit, message, date, args)!
	// Replace the old row only after a successful rebuild+measurement, so a
	// failed -force run never destroys the existing data (commit_hash is UNIQUE).
	if exists {
		delete_benchmark(mut db, commit)!
	}
	insert_benchmark(mut db, b)!
	elog('stored benchmark for ${commit}')
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

	// Sanity probe: make sure vprod actually compiles cmd/v to a real v.c before
	// timing anything. A broken build (e.g. it crashes at startup) must fail the
	// commit, not silently record a ~2ms "instant crash" as a great result.
	os.rm('v.c') or {}
	qvprod := os.quoted_path(vprod)
	probe := os.execute('${qvprod} ${voptions} -o v.c cmd/v')
	if probe.exit_code != 0 || !os.exists('v.c') || os.file_size('v.c') < 100_000 {
		return error('vprod probe failed in ${dir} (exit ${probe.exit_code}); skipping commit')
	}

	v_c := measure('${qvprod} ${voptions} -o v.c cmd/v', 'v -o v.c')!
	v_self := measure('${qvprod} ${voptions} -cc ${ccompiler} -o v2 cmd/v', 'v -o v')!
	hello := measure('${qvprod} ${voptions} -cc ${ccompiler} examples/hello_world.v', 'v hello.v')!
	vc_size := int(os.file_size('v.c') / 1000)
	scan, parse, check, cgen, vlines := measure_steps_minimal(vprod)!
	lines_per_s := if v_c > 0 { int(f64(vlines) / f64(v_c) * 1000.0) } else { 0 }

	return Benchmark{
		commit_hash: commit
		message:     message
		commit_date: date
		created_at:  time.now()
		v_c_ms:      v_c
		v_self_ms:   v_self
		hello_ms:    hello
		vc_size_kb:  vc_size
		scan_ms:     scan
		parse_ms:    parse
		check_ms:    check
		cgen_ms:     cgen
		vlines:      vlines
		lines_per_s: lines_per_s
	}
}

// measure returns the average wall-clock time (ms) for `cmd`, discarding the
// highest samples to reduce noise from random load spikes. It errors if any run
// of `cmd` exits non-zero, so a commit whose compilation fails (e.g. an old
// revision that emits v.c but cannot complete the C compile with the selected
// cc) is skipped instead of recording the short failure time as a great result.
fn measure(cmd string, description string) !int {
	elog('  Measuring ${description}, warmups: ${warmup_samples}, samples: ${max_samples}, discard: ${discard_highest_samples}')
	for _ in 0 .. warmup_samples {
		os.system(cmd)
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

// measure_steps_minimal runs `vprod` several times, capturing the minimum time
// reported by `-show-timings` for each compiler stage.
fn measure_steps_minimal(vprod string) !(int, int, int, int, int) {
	mut scans, mut parses, mut checks, mut cgens, mut vliness := []int{}, []int{}, []int{}, []int{}, []int{}
	for _ in 0 .. max_samples {
		scan, parse, check, cgen, vlines := measure_steps_one_sample(vprod)!
		scans << scan
		parses << parse
		checks << check
		cgens << cgen
		vliness << vlines
	}
	scan := arrays.min(scans) or { 0 }
	parse := arrays.min(parses) or { 0 }
	check := arrays.min(checks) or { 0 }
	cgen := arrays.min(cgens) or { 0 }
	vlines := arrays.max(vliness) or { 0 }
	return scan, parse, check, cgen, vlines
}

fn measure_steps_one_sample(vprod string) !(int, int, int, int, int) {
	cmd := '${os.quoted_path(vprod)} ${voptions} -o v.c cmd/v'
	resp := os.execute(cmd)
	if resp.exit_code != 0 {
		return error('stage-timing run failed (exit ${resp.exit_code}): `${cmd}`\n${resp.output}')
	}

	mut scan, mut parse, mut check, mut cgen, mut vlines := 0, 0, 0, 0, 0
	lines := resp.output.split_into_lines()
	if lines.len == 3 {
		parse = lines[0].before('.').int()
		check = lines[1].before('.').int()
		cgen = lines[2].before('.').int()
	} else {
		ms_lines := lines.map(it.split('  ms '))
		for line in ms_lines {
			if line.len == 2 {
				match line[1] {
					'SCAN' { scan = line[0].int() }
					'PARSE' { parse = line[0].int() }
					'CHECK' { check = line[0].int() }
					'C GEN' { cgen = line[0].int() }
					else {}
				}
			} else if line[0].contains('V') && line[0].contains('source')
				&& line[0].contains('size') {
				start := line[0].index(':') or { 0 }
				end := line[0].index('lines,') or { 0 }
				s := line[0][start + 1..end]
				vlines = s.trim_space().int()
			}
		}
	}
	// Both output formats set parse/check/cgen on success; if none parsed, the
	// output was unusable (e.g. a crash that still exited 0), so reject it rather
	// than letting arrays.min pick these zeroes into the stored row.
	if parse == 0 && check == 0 && cgen == 0 {
		return error('could not parse stage timings from output:\n${resp.output}')
	}
	return scan, parse, check, cgen, vlines
}
