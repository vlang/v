// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import os
import time

fn elog(msg string) {
	eprintln('$time.now().format_ss_micro() $msg')
}

// A job that runs in the background, checks for repo updates,
// runs fast.v, pushes the HTML result to the fast.vlang.io GH pages repo.
fn main() {
	elog('fast_job start')
	defer {
		elog('fast_job end')
	}
	if !os.exists('website') {
		println('cloning the website repo...')
		os.system('git clone git@github.com:/vlang/website.git')
	}
	if !os.exists('fast') {
		println('"fast" binary (built with `v fast.v`) was not found')
		return
	}
	for {
		eprintln('$time.now().format_ss_micro() checking for updates ...')
		res_pull := os.execute('git pull --rebase')
		if res_pull.exit_code != 0 {
			println('failed to git pull. uncommitted changes?')
			return
		}
		// println('running ./fast')
		elog('running ./fast -upload')
		resp := os.execute('./fast -upload')
		if resp.exit_code < 0 {
			println(resp.output)
			return
		}
		if resp.exit_code != 0 {
			println('resp != 0, skipping')
			println(resp.output)
		}
		elog('sleeping for 180 seconds...')
		time.sleep(180 * time.second)
	}
}
