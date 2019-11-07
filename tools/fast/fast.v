// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

import os
import time

fn main() {
	exe := os.executable()
	dir := os.dir(exe)
	vdir := os.dir(dir)
	if !os.file_exists('$vdir/v') && !os.dir_exists('$vdir/vlib') {
		println('fast.html generator needs to be located in `v/tools/`')
	}	
	println('fast.html generator\n')
	// Fetch the last commit's hash
	mut commit_hash := exec('git rev-parse HEAD')
	commit_hash = commit_hash[..7]
	mut table := os.read_file('table.html') or { '' }
	// Do nothing if it's already been processed.
	if table.contains(commit_hash) {
		println('Commit $commit_hash has already been processed')
		return
	}	
	// Build an optimized V
	println('Building vprod...')
	exec('v -o $vdir/vprod -prod $vdir/v.v')
	cmd := '$vdir/vprod -o v.c $vdir/v.v'
	println('Warming up...')
	for i in 0..3 {
		os.exec(cmd) or { panic(err) }
	}	
	println('Building...')
	ticks := time.ticks()
	os.exec(cmd) or { panic(err) }
	diff := time.ticks() - ticks
	println('Building V took ${diff}ms')
	commit_date := exec('git log -n1 --pretty="format:%at"')
	message := exec('git log -n1 --pretty="format:%s"')
	date := time.unix(commit_date.int())
	out := os.create('table.html') or { panic(err) }
	// Place the new row on top
	table =
'<tr>
	<td>${date.format()}</td>
	<td><a target=_blank href="https://github.com/vlang/v/commit/$commit_hash">$commit_hash</a></td>
	<td>$message</td>
	<td>${diff}ms</td>
	<td>${diff}ms</td>
	<td>${diff}ms</td>
</tr>\n' +
	table.trim_space()
	out.writeln(table)
	out.close()
	// Regenerate index.html
	header := os.read_file('header.html') or { panic(err) }
	res := os.create('index.html') or { panic(err) }
	res.writeln(header)
	res.writeln(table)
	res.close()
}	

fn exec(s string) string {
	e := os.exec(s) or { panic(err) }
	return e.output
}	
	

