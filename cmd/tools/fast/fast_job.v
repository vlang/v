// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import os
import time

const vexe = @VEXE

const sleep_period = 120

fn elog(msg string) {
	eprintln('$time.now().format_ss_micro() $msg')
}

fn delay() {
	elog('Sleeping for $sleep_period seconds...')
	time.sleep(sleep_period * time.second)
}

// A job that runs in the background, checks for repo updates,
// runs fast.v, pushes the HTML result to the fast.vlang.io GH pages repo.
fn main() {
	os.chdir(os.dir(@FILE))!
	os.setenv('LANG', 'C', true)
	elog('fast_job start in os.getwd(): $os.getwd()')
	defer {
		elog('fast_job end')
	}

	if !os.exists('website') {
		println('cloning the website repo...')
		os.system('git clone git@github.com:/vlang/website.git')
	}
	for {
		elog('------------------- Checking for updates ... -------------------')
		res_pull := os.execute('git pull --rebase')
		if res_pull.exit_code != 0 {
			println('failed to git pull. uncommitted changes?')
			println('res_pull.output: $res_pull.output')
			delay()
			continue
		}
		if res_pull.output.contains('Already up to date.') {
			if os.args[1] or { '' } == '-force-update' {
				elog('The repository was already updated, but -force-update was passed too.')
			} else {
				elog('Already updated.')
				delay()
				continue
			}
		}

		elog('recompiling V')
		os.system('${os.quoted_path(vexe)} self')

		elog('recompiling ./fast')
		os.execute('${os.quoted_path(vexe)} fast.v')
		os.system('ls -la fast fast.v')

		elog('running ./fast -upload')
		resp := os.execute('./fast -upload')
		if resp.exit_code != 0 {
			println('resp.exit_code = $resp.exit_code != 0')
			println(resp.output)
		}

		delay()
	}
}
