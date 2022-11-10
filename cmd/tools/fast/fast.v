// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import os
import time
import arrays

const warmup_samples = 2

const max_samples = 10

const discard_highest_samples = 6

const voptions = ' -skip-unused -show-timings -stats '

const fast_dir = os.dir(@FILE)

const vdir = os.dir(os.dir(os.dir(fast_dir)))

fn elog(msg string) {
	eprintln('$time.now().format_ss_micro() $msg')
}

fn main() {
	total_sw := time.new_stopwatch()
	elog('fast.html generator start')
	defer {
		elog('fast.html generator end, total: ${total_sw.elapsed().milliseconds():6} ms')
	}
	//
	mut ccompiler_path := 'tcc'
	if vdir.contains('/tmp/cirrus-ci-build') {
		ccompiler_path = 'clang'
	}
	if os.args.contains('-clang') {
		ccompiler_path = 'clang'
	}
	elog('fast_dir: $fast_dir | vdir: $vdir | compiler: $ccompiler_path')

	os.chdir(fast_dir)!
	if !os.exists('$vdir/v') && !os.is_dir('$vdir/vlib') {
		elog('fast.html generator needs to be located in `v/cmd/tools/fast`')
		exit(1)
	}
	if !os.exists('table.html') {
		os.create('table.html')!
	}

	if !os.args.contains('-noupdate') {
		elog('Fetching updates...')
		ret := os.system('$vdir/v up')
		if ret != 0 {
			elog('failed to update V, exit_code: $ret')
			return
		}
	}

	// fetch the last commit's hash
	commit := exec('git rev-parse HEAD')[..8]
	if os.exists('website/index.html') {
		uploaded_index := os.read_file('website/index.html')!
		if uploaded_index.contains('>$commit<') {
			elog('NOTE: commit $commit had been benchmarked already.')
			if !os.args.contains('-force') {
				elog('nothing more to do')
				return
			}
		}
	}

	os.chdir(vdir)!
	message := exec('git log --pretty=format:"%s" -n1 $commit')
	commit_date := exec('git log -n1 --pretty="format:%at" $commit')
	date := time.unix(commit_date.i64())

	elog('Benchmarking commit $commit , with commit message: "$message", commit_date: $commit_date, date: $date')

	// build an optimized V
	if os.args.contains('-do-not-rebuild-vprod') {
		if !os.exists('vprod') {
			elog('Exiting, since if you use `-do-not-rebuild-vprod`, you should already have a `$vdir/vprod` executable, but it is missing!')
			return
		}
	} else {
		elog('  Building vprod...')
		if os.args.contains('-noprod') {
			exec('./v -o vprod cmd/v') // for faster debugging
		} else {
			exec('./v -o vprod -prod -prealloc cmd/v')
		}
	}

	if !os.args.contains('-do-not-rebuild-caches') {
		elog('clearing caches...')
		// cache vlib modules
		exec('$vdir/v wipe-cache')
		exec('$vdir/v -o vwarm_caches -cc $ccompiler_path cmd/v')
	}

	// measure
	diff1 := measure('$vdir/vprod $voptions -o v.c cmd/v', 'v.c')
	diff2 := measure('$vdir/vprod $voptions -cc $ccompiler_path -o v2 cmd/v', 'v2')
	diff3 := 0 // measure('$vdir/vprod -native $vdir/cmd/tools/1mil.v', 'native 1mil')
	diff4 := measure('$vdir/vprod $voptions -cc $ccompiler_path examples/hello_world.v',
		'hello.v')
	vc_size := os.file_size('v.c') / 1000
	scan, parse, check, cgen, vlines := measure_steps_minimal(vdir)!

	html_message := message.replace_each(['<', '&lt;', '>', '&gt;'])

	os.chdir(fast_dir)!
	// place the new row on top
	table := os.read_file('table.html')!
	new_table :=
		'	<tr>
		<td>$date.format()</td>
		<td><a target=_blank href="https://github.com/vlang/v/commit/$commit">$commit</a></td>
		<td>$html_message</td>
		<td>${diff1}ms</td>
		<td>${diff2}ms</td>
		<td>${diff3}ms</td>
		<td>${diff4}ms</td>
		<td>$vc_size KB</td>
		<td>${parse}ms</td>
		<td>${check}ms</td>
		<td>${cgen}ms</td>
		<td>${scan}ms</td>
		<td>$vlines</td>
		<td>${int(f64(vlines) / f64(diff1) * 1000.0)}</td>
	</tr>\n' +
		table.trim_space() + '\n'
	os.write_file('table.html', new_table)!

	// regenerate index.html
	header := os.read_file('header.html')!
	footer := os.read_file('footer.html')!
	mut res := os.create('index.html')!
	res.writeln(header)!
	res.writeln(new_table)!
	res.writeln(footer)!
	res.close()

	// upload the result to github pages
	if os.args.contains('-upload') {
		elog('uploading...')
		os.chdir('website')!
		os.execute_or_exit('git checkout gh-pages')
		os.mv('../index.html', 'index.html')!
		os.system('git commit -am "update benchmark"')
		os.system('git push origin gh-pages')
		elog('uploading done')
	}
}

fn exec(s string) string {
	e := os.execute_or_exit(s)
	return e.output.trim_right('\r\n')
}

// measure returns milliseconds
fn measure(cmd string, description string) int {
	elog('  Measuring $description, warmups: $warmup_samples, samples: $max_samples, discard: $discard_highest_samples, with cmd: `$cmd`')
	for _ in 0 .. warmup_samples {
		exec(cmd)
	}
	mut runs := []int{}
	for r in 0 .. max_samples {
		print('  Sample ${r + 1:2}/${max_samples:2} ... ')
		sw := time.new_stopwatch()
		exec(cmd)
		sample := int(sw.elapsed().milliseconds())
		runs << sample
		println('$sample ms')
		flush_stdout()
	}
	runs.sort()
	elog('   runs before discarding: $runs, avg: ${f64(arrays.sum(runs) or { 0 }) / runs.len:5.2f}')
	// Discard the highest times, since on AWS, they are caused by random load spikes,
	// that are unpredictable, add noise and skew the statistics, without adding useful
	// insights:
	for _ in 0 .. discard_highest_samples {
		runs.pop()
	}
	elog('   runs  after discarding: $runs, avg: ${f64(arrays.sum(runs) or { 0 }) / runs.len:5.2f}')
	return int(f64(arrays.sum(runs) or { 0 }) / runs.len)
}

fn measure_steps_minimal(vdir string) !(int, int, int, int, int) {
	elog('measure_steps_minimal $vdir, samples: $max_samples')
	mut scans, mut parses, mut checks, mut cgens, mut vliness := []int{}, []int{}, []int{}, []int{}, []int{}
	for i in 0 .. max_samples {
		scan, parse, check, cgen, vlines := measure_steps_one_sample(vdir)
		scans << scan
		parses << parse
		checks << check
		cgens << cgen
		vliness << vlines
		elog('    [${i:2}/${max_samples:2}] scan: $scan ms, min parse: $parse ms, min check: $check ms, min cgen: $cgen ms, min vlines: $vlines ms')
	}
	scan, parse, check, cgen, vlines := arrays.min(scans)!, arrays.min(parses)!, arrays.min(checks)!, arrays.min(cgens)!, arrays.min(vliness)!
	elog('measure_steps_minimal => min scan: $scan ms, min parse: $parse ms, min check: $check ms, min cgen: $cgen ms, min vlines: $vlines ms')
	return scan, parse, check, cgen, vlines
}

fn measure_steps_one_sample(vdir string) (int, int, int, int, int) {
	resp := os.execute_or_exit('$vdir/vprod $voptions -o v.c cmd/v')

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
				if line[1] == 'SCAN' {
					scan = line[0].int()
				}
				if line[1] == 'PARSE' {
					parse = line[0].int()
				}
				if line[1] == 'CHECK' {
					check = line[0].int()
				}
				if line[1] == 'C GEN' {
					cgen = line[0].int()
				}
			} else {
				// fetch number of V lines
				if line[0].contains('V') && line[0].contains('source') && line[0].contains('size') {
					start := line[0].index(':') or { 0 }
					end := line[0].index('lines,') or { 0 }
					s := line[0][start + 1..end]
					vlines = s.trim_space().int()
				}
			}
		}
	}
	return scan, parse, check, cgen, vlines
}
