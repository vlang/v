// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import os
import time

// A job that runs in the background, checks for repo updates,
// runs fast.v, pushes the HTML result to the fast.vlang.io GH pages repo.
fn main() {
	println(time.now())
	if !os.exists('website') {
		println('cloning the website repo...')
		os.system('git clone git@github.com:/vlang/website.git')
	}
	if !os.exists('fast') {
		println('"fast" binary (built with `v fast.v`) was not found')
		return
	}
	for {
		res_pull := os.execute('git pull --rebase')
		if res_pull.exit_code != 0 {
			println('failed to git pull. uncommitted changes?')
			return
		}
		// println('running ./fast')
		resp := os.execute('./fast -upload')
		if resp.exit_code < 0 {
			println(resp.output)
			return
		}
		if resp.exit_code != 0 {
			println('resp != 0, skipping')
			println(resp.output)
		}
		time.sleep(180 * time.second)
	}
}
