// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import os
import time

fn main() {
	exe := os.executable()
	dir := os.dir(exe)
	vdir := os.dir(os.dir(os.dir(dir)))
	if !os.exists('$vdir/v') && !os.is_dir('$vdir/vlib') {
		println('fast.html generator needs to be located in `v/cmd/tools/fast`')
	}
	println('fast.html generator\n')
	// Fetch the last commit's hash
	println('Fetching updates...')
	ret := os.system('$vdir/v up')
	if ret != 0 {
		println('failed to update V')
		return
	}
	mut commit_hash := exec('git rev-parse HEAD')
	commit_hash = commit_hash[..8]
	if os.exists('last_commit.txt') {
		last_commit := os.read_file('last_commit.txt') ?
		if last_commit.trim_space() == commit_hash.trim_space() {
			println('No new commits to benchmark. Commit $commit_hash has already been processed.')
			return
		}
		commit_hash = last_commit.trim_space()
	}
	if !os.exists('table.html') {
		os.create('table.html') ?
	}
	mut table := os.read_file('table.html') ?
	/*
	// Do nothing if it's already been processed.
	if table.contains(commit_hash) {
		println('Commit $commit_hash has already been processed')
		return
	}
	*/
	last_commits := exec('git log --pretty=format:"%h" -n 50').split('\n')
	// Fetch all unprocessed commits (commits after the last processed commit)
	mut commits := []string{}
	println('!last_commit="$commit_hash"')
	for i, c in last_commits {
		if c == commit_hash {
			commits = last_commits[..i].reverse()
			break
		}
	}
	println(last_commits)
	println('Commits to benchmark:')
	println(commits)
	for i, commit in commits {
		message := exec('git log --pretty=format:"%s" -n1 $commit')
		println('\n${i + 1}/$commits.len Benchmarking commit $commit "$message"')
		// Build an optimized V
		println('Checking out ${commit}...')
		exec('git checkout $commit')
		println('  Building vprod...')
		exec('v -o $vdir/vprod -prod $vdir/cmd/v')
		diff1 := measure('$vdir/vprod -cc clang -o v.c $vdir/cmd/v', 'v.c')
		diff2 := measure('$vdir/vprod -cc clang -o v2 $vdir/cmd/v', 'v2')
		diff3 := measure('$vdir/vprod -x64 $vdir/cmd/tools/1mil.v', 'x64 1mil')
		diff4 := measure('$vdir/vprod -cc clang $vdir/examples/hello_world.v', 'hello.v')
		// println('Building V took ${diff}ms')
		commit_date := exec('git log -n1 --pretty="format:%at" $commit')
		date := time.unix(commit_date.int())
		mut out := os.create('table.html') ?
		// Place the new row on top
		table = '<tr>
		<td>$date.format()</td>
		<td><a target=_blank href="https://github.com/vlang/v/commit/$commit">$commit</a></td>
		<td>$message</td>
		<td>${diff1}ms</td>
		<td>${diff2}ms</td>
		<td>${diff3}ms</td>
		<td>${diff4}ms</td>
	</tr>\n' +
			table.trim_space()
		out.writeln(table)
		out.close()
		// Regenerate index.html
		header := os.read_file('header.html') ?
		footer := os.read_file('footer.html') ?
		mut res := os.create('index.html') ?
		res.writeln(header)
		res.writeln(table)
		res.writeln(footer)
		res.close()
	}
	exec('git checkout master')
	os.write_file('last_commit.txt', commits[commits.len - 1]) ?
}

fn exec(s string) string {
	e := os.exec(s) or {
		panic(err)
	}
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
		println('  Sample ${r+1}/5')
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
