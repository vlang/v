// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import os
import time
import arrays
import log

const args = arguments()
const warmup_samples = 2

const max_samples = 20

const discard_highest_samples = 16

const voptions = ' -skip-unused -show-timings -stats '

const fast_dir = os.real_path(os.dir(@FILE))

const fast_log_path = os.real_path(os.join_path(fast_dir, 'fast.log'))

const vdir = os.real_path(os.dir(os.dir(os.dir(fast_dir))))

fn elog(msg string) {
	line := '${time.now().format_ss_micro()} ${msg}\n'
	if mut f := os.open_append(fast_log_path) {
		f.write_string(line) or {}
		f.close()
	}
	log.info(msg)
}

fn lsystem(cmd string) int {
	elog('lsystem: ${cmd}')
	return os.system(cmd)
}

fn lexec(cmd string) string {
	elog('  lexec: ${cmd}')
	return os.execute(cmd).output.trim_right('\r\n')
}

fn main() {
	// ensure all log messages will be visible to the observers, even if the program panics
	log.use_stdout()
	log.set_always_flush(true)

	total_sw := time.new_stopwatch()
	elog('fast.html generator start')
	defer {
		elog('fast.html generator end, total: ${total_sw.elapsed().milliseconds():6} ms')
	}

	mut ccompiler_path := 'tcc'
	if vdir.contains('/tmp/cirrus-ci-build') {
		ccompiler_path = 'clang'
	}
	if args.contains('-clang') {
		ccompiler_path = 'clang'
	}
	elog('fast_dir: ${fast_dir} | vdir: ${vdir} | compiler: ${ccompiler_path}')

	os.chdir(fast_dir)!
	if !os.exists('${vdir}/v') && !os.is_dir('${vdir}/vlib') {
		elog('fast.html generator needs to be located in `v/cmd/tools/fast`')
		exit(1)
	}
	if !os.exists('table.html') {
		os.create('table.html')!
	}

	if !args.contains('-noupdate') {
		elog('Fetching updates...')
		ret := lsystem('${vdir}/v up')
		if ret != 0 {
			elog('failed to update V, exit_code: ${ret}')
			return
		}
	}

	// fetch the last commit's hash
	commit := lexec('git rev-parse HEAD')[..8]
	if os.exists('fast.vlang.io/index.html') {
		uploaded_index := os.read_file('fast.vlang.io/index.html')!
		if uploaded_index.contains('>${commit}<') {
			elog('NOTE: commit ${commit} had been benchmarked already.')
			if !args.contains('-force') {
				elog('nothing more to do')
				return
			}
		}
	}

	os.chdir(vdir)!
	message := lexec('git log --pretty=format:"%s" -n1 ${commit}')
	commit_date := lexec('git log -n1 --pretty="format:%at" ${commit}')
	date := time.unix(commit_date.i64())

	elog('Benchmarking commit ${commit} , with commit message: "${message}", commit_date: ${commit_date}, date: ${date}')

	// build an optimized V
	if args.contains('-do-not-rebuild-vprod') {
		if !os.exists('vprod') {
			elog('Exiting, since if you use `-do-not-rebuild-vprod`, you should already have a `${vdir}/vprod` executable, but it is missing!')
			return
		}
	} else {
		elog('  Building vprod...')
		if args.contains('-noprod') {
			lexec('./v -o vprod cmd/v') // for faster debugging
		} else {
			lexec('./v -o vprod -prod -prealloc cmd/v')
		}
	}

	if !args.contains('-do-not-rebuild-caches') {
		elog('clearing caches...')
		// cache vlib modules
		lexec('${vdir}/v wipe-cache')
		lexec('${vdir}/v -o vwarm_caches -cc ${ccompiler_path} cmd/v')
	}

	// measure
	diff1 := measure('${vdir}/vprod ${voptions} -o v.c cmd/v', 'v.c')
	diff2 := measure('${vdir}/vprod ${voptions} -cc ${ccompiler_path} -o v2 cmd/v', 'v2')
	diff3 := 0 // measure('$vdir/vprod -native $vdir/cmd/tools/1mil.v', 'native 1mil')
	diff4 := measure('${vdir}/vprod ${voptions} -cc ${ccompiler_path} examples/hello_world.v',
		'hello.v')
	vc_size := os.file_size('v.c') / 1000
	scan, parse, check, cgen, vlines := measure_steps_minimal(vdir)!

	html_message := message.replace_each(['<', '&lt;', '>', '&gt;'])

	os.chdir(fast_dir)!
	// place the new row on top
	table := os.read_file('table.html')!
	new_table :=
		'	<tr>
		<td>${date.format()}</td>
		<td><a target=_blank href="https://github.com/vlang/v/commit/${commit}">${commit}</a></td>
		<td>${html_message}</td>
		<td>${diff1}ms</td>
		<td>${diff2}ms</td>
		<td>${diff3}ms</td>
		<td>${diff4}ms</td>
		<td>${vc_size} KB</td>
		<td>${parse}ms</td>
		<td>${check}ms</td>
		<td>${cgen}ms</td>
		<td>${scan}ms</td>
		<td>${vlines}</td>
		<td>${int(f64(vlines) / f64(diff1) * 1000.0)}</td>
	</tr>\n' +
		table.trim_space() + '\n'
	os.write_file('table.html', new_table)!

	// regenerate index.html
	header := os.read_file('header.html')!
	footer := os.read_file('footer.html')!
	mut res := os.create('index.html')!
	res.writeln(header)!
	res.writeln(new_table)!
	res.writeln(footer)!
	res.close()

	// upload the result to github pages
	if args.contains('-upload') {
		$if freebsd {
			// Note: tcc currently can not compile vpm on FreeBSD, due to its dependence on net.ssl and net.mbedtls, so force using clang instead:
			elog('FreeBSD: compiling the VPM tool with clang...')
			lexec('${vdir}/vprod -cc clang ${vdir}/cmd/tools/vpm/')
			os.chdir('${fast_dir}/docs.vlang.io/docs_generator/')!
			elog('FreeBSD: installing the dependencies for the docs generator...')
			lexec('${vdir}/vprod install')
			os.chdir(fast_dir)!
		}

		os.chdir('${fast_dir}/fast.vlang.io/')!
		elog('Uploading to fast.vlang.io/ ...')
		lexec('git checkout gh-pages')
		os.mv('../index.html', 'index.html')!
		elog('   adding changes...')
		lexec('git commit -am "update fast.vlang.io for commit ${commit}"')
		elog('   pushing...')
		lexec('git push origin gh-pages')
		elog('   uploading to fast.vlang.io/ done')
		os.chdir(fast_dir)!

		os.chdir('${fast_dir}/docs.vlang.io/')!
		elog('Uploading to docs.vlang.io/ ...')
		elog('   pulling upstream changes...')
		lexec('git pull')
		elog('   running build.vsh...')
		lexec('${vdir}/vprod run build.vsh')
		elog('   adding new docs...')
		lexec('git add .')
		elog('   commiting...')
		lexec('git commit -am "update docs for commit ${commit}"')
		elog('   pushing...')
		lexec('git push')
		elog('   uploading to fast.vlang.io/ done')
		os.chdir(fast_dir)!
	}
}

// measure returns milliseconds
fn measure(cmd string, description string) int {
	elog('  Measuring ${description}, warmups: ${warmup_samples}, samples: ${max_samples}, discard: ${discard_highest_samples}, with cmd: `${cmd}`')
	for _ in 0 .. warmup_samples {
		os.system(cmd)
	}
	mut runs := []int{}
	for r in 0 .. max_samples {
		sw := time.new_stopwatch()
		os.execute(cmd)
		sample := int(sw.elapsed().milliseconds())
		runs << sample
		elog('  Sample ${r + 1:2}/${max_samples:2} ... ${sample} ms')
	}
	runs.sort()
	elog('   runs before discarding: ${runs}, avg: ${f64(arrays.sum(runs) or { 0 }) / runs.len:5.2f}')
	// Discard the highest times, since on AWS, they are caused by random load spikes,
	// that are unpredictable, add noise and skew the statistics, without adding useful
	// insights:
	for _ in 0 .. discard_highest_samples {
		runs.pop()
	}
	elog('   runs  after discarding: ${runs}, avg: ${f64(arrays.sum(runs) or { 0 }) / runs.len:5.2f}')
	return int(f64(arrays.sum(runs) or { 0 }) / runs.len)
}

fn measure_steps_minimal(vdir string) !(int, int, int, int, int) {
	elog('measure_steps_minimal ${vdir}, samples: ${max_samples}')
	mut scans, mut parses, mut checks, mut cgens, mut vliness := []int{}, []int{}, []int{}, []int{}, []int{}
	for i in 0 .. max_samples {
		scan, parse, check, cgen, vlines, cmd := measure_steps_one_sample(vdir)
		scans << scan
		parses << parse
		checks << check
		cgens << cgen
		vliness << vlines
		elog('    [${i:2}/${max_samples:2}] scan: ${scan} ms, min parse: ${parse} ms, min check: ${check} ms, min cgen: ${cgen} ms, min vlines: ${vlines} ms, cmd: ${cmd}')
	}
	scan, parse, check, cgen, vlines := arrays.min(scans)!, arrays.min(parses)!, arrays.min(checks)!, arrays.min(cgens)!, arrays.min(vliness)!
	elog('measure_steps_minimal => min scan: ${scan} ms, min parse: ${parse} ms, min check: ${check} ms, min cgen: ${cgen} ms, min vlines: ${vlines} ms')
	return scan, parse, check, cgen, vlines
}

fn measure_steps_one_sample(vdir string) (int, int, int, int, int, string) {
	cmd := '${vdir}/vprod ${voptions} -o v.c cmd/v'
	resp := os.execute(cmd)

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
				// fetch number of V lines
				if line[0].contains('V') && line[0].contains('source') && line[0].contains('size') {
					start := line[0].index(':') or { 0 }
					end := line[0].index('lines,') or { 0 }
					s := line[0][start + 1..end]
					vlines = s.trim_space().int()
				}
			}
		}
	}
	return scan, parse, check, cgen, vlines, cmd
}
