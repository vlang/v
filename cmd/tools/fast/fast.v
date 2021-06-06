// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import os
import time

// TODO -usecache
const voptions = ' -skip-unused -show-timings -stats '

fn main() {
	exe := os.executable()
	fast_dir := os.dir(exe)
	vdir := os.dir(os.dir(os.dir(fast_dir)))
	os.chdir(fast_dir)
	if !os.exists('$vdir/v') && !os.is_dir('$vdir/vlib') {
		println('fast.html generator needs to be located in `v/cmd/tools/fast`')
	}
	println('fast.html generator\n')
	println('Fetching updates...')
	ret := os.system('$vdir/v up')
	if ret != 0 {
		println('failed to update V')
		return
	}
	// Fetch the last commit's hash
	commit := exec('git rev-parse HEAD')[..8]
	if !os.exists('table.html') {
		os.create('table.html') ?
	}
	mut table := os.read_file('table.html') ?
	if table.contains('>$commit<') {
		println('nothing to benchmark')
		exit(1)
		return
	}
	// for i, commit in commits {
	message := exec('git log --pretty=format:"%s" -n1 $commit')
	// println('\n${i + 1}/$commits.len Benchmarking commit $commit "$message"')
	println('\nBenchmarking commit $commit "$message"')
	// Build an optimized V
	// println('Checking out ${commit}...')
	// exec('git checkout $commit')
	println('  Building vprod...')
	os.chdir(vdir)
	exec('v -o $vdir/vprod -prod -prealloc cmd/v')
	// exec('v -o $vdir/vprod $vdir/cmd/v') // for faster debugging
	// cache vlib modules
	exec('v wipe-cache')
	exec('v -o v2 -prod -usecache cmd/v')
	// measure
	diff1 := measure('$vdir/vprod $voptions -o v.c cmd/v', 'v.c')
	mut tcc_path := 'tcc'
	$if freebsd {
		tcc_path = '/usr/local/bin/tcc'
	}
	diff2 := measure('$vdir/vprod $voptions -cc $tcc_path -o v2 cmd/v', 'v2')
	diff3 := 0 // measure('$vdir/vprod -native $vdir/cmd/tools/1mil.v', 'native 1mil')
	diff4 := measure('$vdir/vprod -usecache $voptions -cc clang examples/hello_world.v',
		'hello.v')
	vc_size := os.file_size('v.c') / 1000
	// scan/parse/check/cgen
	scan, parse, check, cgen, vlines := measure_steps(vdir)
	// println('Building V took ${diff}ms')
	commit_date := exec('git log -n1 --pretty="format:%at" $commit')
	date := time.unix(commit_date.int())
	//
	os.chdir(fast_dir)
	mut out := os.create('table.html') ?
	// Place the new row on top
	html_message := message.replace_each(['<', '&lt;', '>', '&gt;'])
	table =
		'<tr>
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
		<td>${int(f64(vlines) / f64(diff1) * 1000.0)} lines/s</td>
	</tr>\n' +
		table.trim_space()
	out.writeln(table) ?
	out.close()
	// Regenerate index.html
	header := os.read_file('header.html') ?
	footer := os.read_file('footer.html') ?
	mut res := os.create('index.html') ?
	res.writeln(header) ?
	res.writeln(table) ?
	res.writeln(footer) ?
	res.close()
	//}
	// exec('git checkout master')
	// os.write_file('last_commit.txt', commits[commits.len - 1]) ?
}

fn exec(s string) string {
	e := os.execute_or_panic(s)
	return e.output.trim_right('\r\n')
}

// returns milliseconds
fn measure(cmd string, description string) int {
	println('  Measuring $description')
	println('  Warming up...')
	for _ in 0 .. 3 {
		exec(cmd)
	}
	println('  Building...')
	mut runs := []int{}
	for r in 0 .. 5 {
		println('  Sample ${r + 1}/5')
		sw := time.new_stopwatch({})
		exec(cmd)
		runs << int(sw.elapsed().milliseconds())
	}
	// discard lowest and highest time
	runs.sort()
	runs = runs[1..4]
	mut sum := 0
	for run in runs {
		sum += run
	}
	return int(sum / 3)
}

fn measure_steps(vdir string) (int, int, int, int, int) {
	resp := os.execute_or_panic('$vdir/vprod $voptions -o v.c cmd/v')

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
				// Fetch number of V lines
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
