module main

import os
import flag

// gen_vc regenerates V's bootstrap C snapshots, v.c and v_win.c in vlang/vc, from vlang/v master.
// `serve` runs it as the vcgen.vlang.io service, `run-once` makes a single run, and `snapshot`
// only generates and checks the snapshot of an already built checkout, which is what
// .github/workflows/gen_vc_ci.yml uses. See doc/vcgen_service.md.

const usage = 'usage:
  gen_vc serve [options]      run the vcgen.vlang.io service
  gen_vc run-once [options]   make one run and exit (1 when it failed)
  gen_vc snapshot <v-root> <out-dir>
                              write and check v.c and v_win.c of a built V checkout

Runs are dry runs unless `--mode push` is given. Secrets come from the environment only:
  VCGEN_PUSH_TOKEN       token that may push to the https vlang/vc remote (push mode)
  VCGEN_WEBHOOK_SECRET   secret of the GitHub push webhook (serve)
Run `gen_vc serve --help` for the options.'

// Config is the configuration of runs and of the service.
struct Config {
	work_dir              string
	v_dir                 string
	mode                  Mode
	host                  string
	port                  int
	poll_minutes          int
	v_url                 string
	vc_url                string
	repository            string
	push_user             string
	webhook_secret        string
	min_available_mib     int
	build_timeout_minutes int
	retry_failed_minutes  int
}

// github_repository returns `owner/name` for a github.com URL, or '' for any other URL.
fn github_repository(url string) string {
	prefix := 'https://github.com/'
	if !url.starts_with(prefix) {
		return ''
	}
	return url[prefix.len..].trim_string_right('.git').trim_right('/')
}

// parse_config reads the options of `serve` and `run-once`, and returns the config and `--force`.
fn parse_config(command string, args []string) !(Config, bool) {
	mut fp := flag.new_flag_parser(args)
	fp.application('gen_vc ${command}')
	fp.description('Regenerates vlang/vc from vlang/v master. See doc/vcgen_service.md.')
	work_dir := fp.string('work-dir', 0, '/var/lib/vcgen',
		'directory for the bare vlang/vc clone and the generated snapshot')
	v_dir := fp.string('v-dir', 0, '',
		'the vlang/v checkout to build in, empty for <work-dir>/v; its path is embedded in the snapshot')
	mode := fp.string('mode', 0, 'dry-run', '`dry-run` never pushes; `push` publishes to vlang/vc')
	host := fp.string('host', 0, '127.0.0.1', 'address to listen on')
	port := fp.int('port', 0, 7171, 'port to listen on')
	poll_minutes := fp.int('poll-minutes', 0, 5, 'minutes between polls of vlang/v master; 0 disables polling')
	v_url := fp.string('v-url', 0, 'https://github.com/vlang/v', 'vlang/v repository')
	vc_url := fp.string('vc-url', 0, 'https://github.com/vlang/vc', 'vlang/vc repository')
	push_user := fp.string('push-user', 0, bot_name, 'user name sent with VCGEN_PUSH_TOKEN')
	min_available_mib := fp.int('min-available-mib', 0, 2048,
		'skip a build while MemAvailable plus SwapFree is lower; 0 disables the check')
	build_timeout_minutes := fp.int('build-timeout-minutes', 0, 60,
		'time limit of `make` and of each generation')
	retry_failed_minutes := fp.int('retry-failed-minutes', 0, 60,
		'minutes before a failed run of the same commits is retried')
	force := fp.bool('force', 0, false, 'build even if nothing changed since the last run')
	rest := fp.finalize()!
	if rest.len > 0 {
		return error('unexpected arguments: ${rest.join(' ')}')
	}
	parsed_mode := match mode {
		'dry-run' { Mode.dry_run }
		'push' { Mode.push }
		else { return error('--mode must be `dry-run` or `push`') }
	}
	if push_user == '' || !push_user.bytes().all(it.is_alnum() || it == `-`) {
		return error('--push-user must be a GitHub user name')
	}
	if !os.is_abs_path(work_dir) || (v_dir != '' && !os.is_abs_path(v_dir)) {
		return error('--work-dir and --v-dir must be absolute paths')
	}
	if parsed_mode == .push && vc_url.starts_with('https://') && os.getenv('VCGEN_PUSH_TOKEN') == '' {
		return error('push mode needs VCGEN_PUSH_TOKEN in the environment')
	}
	return Config{
		work_dir:              work_dir
		v_dir:                 if v_dir == '' { os.join_path(work_dir, 'v') } else { v_dir }
		mode:                  parsed_mode
		host:                  host
		port:                  port
		poll_minutes:          poll_minutes
		v_url:                 v_url
		vc_url:                vc_url
		repository:            github_repository(v_url)
		push_user:             push_user
		webhook_secret:        os.getenv('VCGEN_WEBHOOK_SECRET')
		min_available_mib:     min_available_mib
		build_timeout_minutes: build_timeout_minutes
		retry_failed_minutes:  retry_failed_minutes
	}, force
}

fn main() {
	args := os.args[1..]
	if args.len == 0 || args[0] in ['help', '-h', '--help'] {
		println(usage)
		exit(if args.len == 0 { 2 } else { 0 })
	}
	// Never wait for a password prompt; a missing credential is an error.
	os.setenv('GIT_TERMINAL_PROMPT', '0', true)
	match args[0] {
		'snapshot' {
			if args.len != 3 {
				eprintln(usage)
				exit(2)
			}
			v_root := os.real_path(args[1])
			mut log := Log{}
			commit := log.run(['git', '-C', v_root, 'rev-parse', '--verify', 'HEAD^{commit}'],
				'${v_root} is not a Git checkout') or {
				eprintln(err.msg())
				exit(1)
			}
			generate_snapshot(mut log, v_root, commit.trim_space(), args[2], 0) or {
				eprintln(err.msg())
				exit(1)
			}
		}
		'run-once', 'serve' {
			config, force := parse_config(args[0], args[1..]) or {
				eprintln(err.msg())
				exit(2)
			}
			if args[0] == 'serve' {
				serve(config) or {
					eprintln(err.msg())
					exit(1)
				}
				return
			}
			mut runner := Runner{
				config: config
			}
			report := runner.run(force)
			println('outcome=${report.outcome} v_commit=${report.v_commit} vc_commit=${report.vc_commit}')
			if report.outcome == .failed {
				exit(1)
			}
		}
		else {
			eprintln(usage)
			exit(2)
		}
	}
}
