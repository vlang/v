// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import os
import time

const fast_dir = os.dir(@FILE)

const vdir = os.dir(os.dir(os.dir(fast_dir)))

const vexe = os.join_path(vdir, 'v')

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
	os.setenv('LANG', 'C', true)
	elog('fast_job fast_dir: $fast_dir | vdir: $vdir | vexe: $vexe')

	os.chdir(fast_dir)!

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
		elog('> res_pull.output: $res_pull.output')
		if res_pull.exit_code != 0 {
			elog('Git pull failed. You may have uncommitted changes?')
			delay()
			continue
		}
		if res_pull.output.contains('Already up to date.') {
			if os.args.contains('-force-update') {
				elog('The repository was already updated, but -force-update was passed too.')
			} else {
				delay()
				continue
			}
		}

		elog('recompiling V')
		os.system('${os.quoted_path(vexe)} self')
		os.system('ls -la ${os.quoted_path(vexe)}')

		elog('recompiling ./fast')
		recompile_fast_v_code := os.system('${os.quoted_path(vexe)} fast.v')
		if recompile_fast_v_code != 0 {
			elog('WARNING: could not recompile fast.v')
			delay()
			continue
		}
		os.system('ls -la fast fast.v')

		elog('running ./fast -upload')
		fast_exit_code := os.system('./fast -upload')
		if fast_exit_code != 0 {
			println('fast_exit_code = $fast_exit_code, != 0')
		}

		delay()
	}
}
