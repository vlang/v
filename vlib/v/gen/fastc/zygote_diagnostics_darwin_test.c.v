// vtest build: macos

module fastc

import os
import time
import v.cmdexec

const fastc_zygote_diagnostics_probe = 'fastc-zygote-diagnostics-probe'

fn testsuite_begin() {
	$if macos && !tinyc && !fastc_selfhost ?&& !v3_backend ? {
		if os.args.len == 4 && os.args[1] == fastc_zygote_diagnostics_probe {
			fastc_zygote_diagnostics_scenario(os.args[2], os.args[3]) or {
				eprintln(err.msg())
				exit(1)
			}
			println('zygote diagnostics complete')
			exit(0)
		}
	}
}

fn fastc_zygote_diagnostics_assert_reaped(pids []int) ! {
	for pid in pids {
		mut status := 0
		if C.waitpid(pid, &status, C.WNOHANG) != -1 || C.errno != C.ECHILD {
			return error('zygote ${pid} was not reaped')
		}
	}
}

fn fastc_zygote_diagnostics_compile(mut zygote FastcZygote, prefix string, head string, body_error bool) ! {
	$if macos && !tinyc && !fastc_selfhost ?&& !v3_backend ? {
		pieces := [head, if body_error {
			'#error fastc_zygote_body_error\n'
		} else {
			'int first(void) { return 11; }\n'
		}, 'int second(void) { return 22; }\n']
		units := FastcUnitLayout{
			head_end:    1
			solo_end:    1
			solo_starts: [1]
			unit_starts: [1, 2, 3]
		}
		plan := fastc_zygote_plan(prefix, pieces, units, 2)
		if plan.paths.len != 2 {
			return error('expected two zygote units')
		}
		objects := plan.paths.map(it[..it.len - 2] + '.o')
		mut prepared_link := FastcPreparedLink{}
		fastc_compile_libtcc_zygote_c_units(mut zygote, ['-nostdinc', '-nostdlib'], objects,
			pieces, units, plan, mut prepared_link, false)!
		for object in objects {
			if !os.is_file(object) || os.file_size(object) == 0 {
				return error('missing zygote object ${object}')
			}
		}
	}
}

fn fastc_zygote_diagnostics_scenario(mode string, root string) ! {
	$if macos && !tinyc && !fastc_selfhost ?&& !v3_backend ? {
		head_path := os.join_path(root, 'cached.head')
		// More diagnostics than a pipe can hold, followed by more source than
		// another pipe can hold. The feeder must not be joined before its reader.
		warning := '#warning fastc_zygote_cached_head_${'x'.repeat(240)}\n'
		head := warning.repeat(4096) + 'int first(void);\nint second(void);\n'
		if head.len < fastc_zygote_head_bytes {
			return error('the fixture must be large enough to retain its head')
		}
		// Seed the cache through a real fresh compilation and its normal writer.
		mut fresh := fastc_start_libtcc_zygote(2, head_path)
		defer { fastc_discard_libtcc_zygote(mut fresh) }
		if !fresh.active || fresh.guess.active {
			return error('expected a fresh zygote without a cached head')
		}
		fresh_pid := fresh.fresh.pid
		fastc_zygote_diagnostics_compile(mut fresh, os.join_path(root, 'seed'), head, false)!
		fastc_zygote_diagnostics_assert_reaped([fresh_pid])!
		kept := fastc_parse_zygote_head(os.read_file(head_path)!) or {
			return error('the fresh compilation did not retain its head')
		}
		if kept.text != head {
			return error('the retained head differs from the compiled head')
		}
		mut cached := fastc_start_libtcc_zygote(2, head_path)
		defer { fastc_discard_libtcc_zygote(mut cached) }
		if !cached.active || !cached.guess.active {
			return error('expected the next compilation to start a speculative zygote')
		}
		pids := [cached.fresh.pid, cached.guess.pid]
		if mode == 'discard' {
			fastc_discard_libtcc_zygote(mut cached)
		} else {
			current_head := if mode == 'miss' { head + 'typedef int changed_head;\n' } else { head }
			mut failure := ''
			fastc_zygote_diagnostics_compile(mut cached, os.join_path(root, 'build'), current_head,
				mode == 'body_error') or { failure = err.msg() }
			if mode == 'body_error' {
				if !failure.contains('fastc_zygote_body_error')
					|| !failure.contains('fastc_zygote_cached_head_') {
					return error('cached zygote lost its head or body diagnostics: ${failure}')
				}
			} else if failure != '' {
				return error(failure)
			}
		}
		fastc_zygote_diagnostics_assert_reaped(pids)!
	}
}

fn test_cached_zygote_drains_diagnostics_while_feeding_its_head() ! {
	$if macos && !tinyc && !fastc_selfhost ?&& !v3_backend ? {
		root := os.join_path(os.vtmp_dir(), 'fastc_zygote_diagnostics_${os.getpid()}_${time.now().unix_nano()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		for mode in ['hit', 'miss', 'body_error', 'discard'] {
			work := os.join_path(root, mode)
			os.mkdir_all(work)!
			// A regression must fail with a deadline, not hang the test runner.
			// cmdexec also kills the zygotes in the child's process group on timeout.
			result := cmdexec.run_with_timeout(os.executable(), [
				fastc_zygote_diagnostics_probe,
				mode,
				work,
			], 30_000)
			assert result.exit_code == 0, '${mode}: ${result.output}'
			assert result.output.contains('zygote diagnostics complete'), '${mode}: ${result.output}'
		}
	}
}
