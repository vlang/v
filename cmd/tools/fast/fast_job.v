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
		os.exec('git pull --rebase') or {
			println('failed to git pull. uncommitted changes?')
			return
		}
		// println('running fast')
		resp := os.exec('./fast') or {
			println(err)
			return
		}
		if resp.exit_code != 0 {
			println('resp != 0, skipping')
		} else {
			os.chdir('website')
			os.exec('git checkout gh-pages') ?
			os.cp('../index.html', 'index.html') ?
			os.system('git commit -am "update benchmark"')
			os.system('git push origin gh-pages')
			os.chdir('..')
		}
		time.sleep(60 * time.second)
	}
}
