import os
import json
import term
import time

const c = term.colorize
const tg = term.green
const tm = term.magenta
const tb = term.bold

struct Check {
	name     string
	bucket   string
	state    string
	link     string
	workflow string
}

fn (c Check) is_failed() bool {
	return c.bucket == 'fail' || c.state in ['FAILURE', 'TIMED_OUT']
}

fn (c Check) is_cancelled() bool {
	return c.bucket == 'cancel' || c.state == 'CANCELLED'
}

struct GhCheckRun {
	name       string
	status     string
	conclusion string
	html_url   string @[json: html_url]
}

struct GhCheckRunsResponse {
	check_runs []GhCheckRun @[json: check_runs]
}

fn main() {
	unbuffer_stdout()
	arg := os.args[1] or {
		println('Usage: v run gh_restart_failed.v <PR_NUMBER|REF>')
		return
	}
	is_pr := arg.len < 6 && arg.bytes().all(it.is_digit())
	checks := if is_pr { get_checks_for_pr(arg.int()) } else { get_checks_for_commit(arg) }
	mut failed := []Check{}
	mut cancelled := []Check{}
	mut succeeded := 0
	mut in_progress := 0
	for check in checks {
		if check.is_failed() {
			failed << check
		} else if check.is_cancelled() {
			cancelled << check
		} else if check.bucket == 'pass' || check.state == 'SUCCESS' {
			succeeded++
		} else {
			in_progress++
		}
	}
	mut to_restart := []Check{}
	to_restart << failed
	to_restart << cancelled
	mut restarted_count := 0
	if to_restart.len > 0 {
		println('Found ${to_restart.len} failed or cancelled jobs:')
		for job in to_restart {
			println('- ${job.workflow} / ${job.name} (${job.state})')
		}
		println('\n' + c(tg, 'Do you want to restart these ${to_restart.len} jobs? [y/N]: '))
		if os.input('').to_lower().trim_space() == 'y' {
			println('')
			for job in to_restart {
				run_id, job_id := parse_ids(job.link)
				if run_id == '' || job_id == '' {
					println('Could not parse IDs from link: ${job.link} (Skipping)')
					continue
				}
				print('Restarting ${job.name} (Run: ${run_id}, Job: ${job_id})... ')
				res := execute_with_progress('gh run rerun ${run_id} --job ${job_id}')
				if res.exit_code == 0 {
					restarted_count++
				} else {
					println('  Error: ${res.output.trim_space()}')
				}
			}
		} else {
			println('Aborted restart.')
		}
	} else {
		println('No failed or cancelled jobs found.')
	}
	println('\n' + c(tg, 'Summary:'))
	println('Total jobs:   ${m(checks.len)}')
	println('Failed:       ${m(failed.len)}')
	println('Cancelled:    ${m(cancelled.len)}')
	println('Succeeded:    ${m(succeeded)}')
	println('In Progress:  ${m(in_progress)}')
	println('Restarted:    ${m(restarted_count)}')
}

fn m(n int) string {
	return c(tb, n.str())
}

fn parse_ids(link string) (string, string) {
	parts := link.split('/')
	runs_idx := parts.index('runs')
	job_idx := parts.index('job')
	mut run_id := ''
	mut job_id := ''
	if runs_idx != -1 && runs_idx + 1 < parts.len {
		run_id = parts[runs_idx + 1]
	}
	if job_idx != -1 && job_idx + 1 < parts.len {
		job_id = parts[job_idx + 1]
	}
	return run_id, job_id
}

fn get_checks_for_pr(pr_number int) []Check {
	println(c(tg, 'Fetching checks for PR ${m(pr_number)}...'))
	res := execute_with_progress('gh pr checks ${pr_number} --json name,bucket,state,link,workflow')
	if res.exit_code != 0 {
		println('Error: ${res.output}')
		exit(1)
	}
	return json.decode([]Check, res.output) or { exit(1) }
}

fn get_checks_for_commit(commit string) []Check {
	println(c(tg, 'Fetching checks for ref ${c(tb, commit)}...'))
	res := execute_with_progress('gh api repos/:owner/:repo/commits/${commit}/check-runs?per_page=100')
	if res.exit_code != 0 {
		println('Error: ${res.output}')
		exit(1)
	}
	resp := json.decode(GhCheckRunsResponse, res.output) or { exit(1) }
	mut checks := []Check{}
	for cr in resp.check_runs {
		checks << Check{
			name:     cr.name
			bucket:   if cr.conclusion == 'failure' {
				'fail'
			} else if cr.conclusion == 'cancelled' {
				'cancel'
			} else if cr.conclusion == 'success' {
				'pass'
			} else {
				'pending'
			}
			state:    if cr.conclusion != '' {
				cr.conclusion.to_upper()
			} else {
				cr.status.to_upper()
			}
			link:     cr.html_url
			workflow: 'Actions'
		}
	}
	return checks
}

fn execute_with_progress(cmd string) os.Result {
	start := time.now()
	mut stop := false
	spawn fn (cmd string, start time.Time, mut stop &bool) {
		start_str := start.hhmmss()
		for !*stop {
			elapsed := time.since(start).seconds()
			print('\rRunning ${c(tm, cmd)} [${start_str}] ... ${elapsed:.1f}s')
			os.flush()
			time.sleep(100 * time.millisecond)
		}
	}(cmd, start, mut &stop)
	res := os.execute(cmd)
	elapsed := time.since(start).seconds()
	stop = true
	time.sleep(100 * time.millisecond)
	status := if res.exit_code == 0 { 'OK' } else { 'Failed' }
	print('\r')
	println('Command ${c(tm, cmd)} done in ${elapsed:.1f}s. ${status}')
	return res
}
