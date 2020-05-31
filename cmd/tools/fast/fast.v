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
	ret := os.system('git pull --rebase')
	if ret != 0 {
		println('failed to git pull')
		return
	}
	mut commit_hash := exec('git rev-parse HEAD')
	commit_hash = commit_hash[..7]
	if !os.exists('table.html') {
		os.create('table.html') or { panic(err) }
	}
	mut table := os.read_file('table.html') or { panic(err) }
	// Do nothing if it's already been processed.
	if table.contains(commit_hash) {
		println('Commit $commit_hash has already been processed')
		return
	}
	// Build an optimized V
	println('Building vprod...')
	exec('v -o $vdir/vprod -prod $vdir/cmd/v')
	println('Measuring...')
	diff1 := measure('$vdir/vprod -cc clang -o v.c $vdir/cmd/v')
	diff2 := measure('$vdir/vprod -cc clang -o v2 $vdir/cmd/v')
	diff3 := measure('$vdir/vprod -x64 $vdir/cmd/tools/1mil.v')
	diff4 := measure('$vdir/vprod -cc clang $vdir/examples/hello_world.v')
	//println('Building V took ${diff}ms')
	commit_date := exec('git log -n1 --pretty="format:%at"')
	message := exec('git log -n1 --pretty="format:%s"')
	date := time.unix(commit_date.int())
	mut out := os.create('table.html') or { panic(err) }
	// Place the new row on top
	table =
'<tr>
	<td>${date.format()}</td>
	<td><a target=_blank href="https://github.com/vlang/v/commit/$commit_hash">$commit_hash</a></td>
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
	header := os.read_file('header.html') or { panic(err) }
	footer := os.read_file('footer.html') or { panic(err) }
	mut res := os.create('index.html') or { panic(err) }
	res.writeln(header)
	res.writeln(table)
	res.writeln(footer)
	res.close()
}

fn exec(s string) string {
	e := os.exec(s) or { panic(err) }
	return e.output
}

// returns milliseconds
fn measure(cmd string) int {
	println('Warming up...')
	for _ in 0..3 {
		exec(cmd)
	}
	println('Building...')
	sw := time.new_stopwatch({})
	exec(cmd)
	return int(sw.elapsed().milliseconds())
}
