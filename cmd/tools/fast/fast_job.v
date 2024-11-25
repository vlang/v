// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import os
import time

// This program acts as a service/monitor/daemon, that runs in the background, checks for repo updates,
// and runs fast.v, when there are changes. It should *never fail*, even if fast.v can fail. In that case,
// this program should just loop, trying again on the next iteration of the loop, and hoping that the repo
// was fixed enough, so that fast.v can succeed.

// It is the responsibility of * fast.v * , to do all measurements, process them, and to pushes the HTML result
// of that processing to the fast.vlang.io GH pages repo.

const sleep_period = 60

const fast_dir = os.dir(@FILE)

const vdir = os.real_path(os.dir(os.dir(os.dir(fast_dir))))

const vexe = os.real_path(os.join_path(vdir, 'v'))

const old_path = os.getenv('PATH')

fn elog(msg string) {
	eprintln('${time.now().format_ss_micro()} ${msg}')
}

fn delay() {
	elog('Sleeping for ${sleep_period} seconds...')
	time.sleep(sleep_period * time.second)
}

fn check_output_repo(url string, branch string, folder string) {
	if os.exists(folder) {
		elog('Note: ${folder} already exists; using it.')
		return
	}
	cmd := 'git clone --filter=blob:none --branch=${branch}  ${url}  ${folder}'
	elog('Note: ${folder} is missing. Cloning to `${folder}`, with: `${cmd}` ...')
	res := os.system(cmd)
	elog('... cloning done, result: ${res}')
}

fn main() {
	elog('fast_job start setup ...')
	// ensure a more stable working environment for the used tools, independent on how this executable was started:
	os.setenv('LANG', 'C', true)
	os.setenv('PATH', '${vdir}:${old_path}', true)
	elog('fast_job fast_dir: ${fast_dir}')
	elog('fast_job vdir: ${vdir}')
	elog('fast_job vexe: ${vexe}')
	elog('fast_job PATH: ${os.getenv('PATH')}')

	os.chdir(fast_dir)!
	elog('fast_job start in os.getwd(): ${os.getwd()}')

	defer {
		elog('fast_job end')
	}

	elog('fast_job clone output repos...')
	check_output_repo('https://github.com/vlang/website', 'gh-pages', 'fast.vlang.io/')
	check_output_repo('https://github.com/vlang/docs/', 'main', 'docs.vlang.io/')
	check_output_repo('https://github.com/vlang/docs/', 'generator', 'docs.vlang.io/docs_generator/')

	mut i := 0
	for {
		i++
		elog('------------------- Checking for updates, loop: ${i} ... -------------------')
		os.chdir(fast_dir)!

		res_pull := os.execute('git pull --rebase')
		elog('> res_pull.output: ${res_pull.output}')
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
		recompile_fast_v_code := os.system('${os.quoted_path(vexe)} -keepc -g fast.v')
		if recompile_fast_v_code != 0 {
			elog('WARNING: could not recompile fast.v')
			delay()
			continue
		}
		os.system('ls -la fast fast.v')

		elog('running ./fast -upload')
		fast_exit_code := os.system('./fast -upload')
		if fast_exit_code != 0 {
			println('fast_exit_code = ${fast_exit_code}, != 0')
		}

		delay()
	}
}
