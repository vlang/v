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

struct GhRun {
	database_id   i64    @[json: databaseId]
	workflow_name string @[json: workflowName]
}

struct GhJob {
	name        string
	status      string
	conclusion  string
	url         string
	database_id i64 @[json: databaseId]
}

struct GhRunView {
	jobs          []GhJob
	workflow_name string @[json: workflowName]
}

fn main() {
	unbuffer_stdout()
	if os.args.len != 2 {
		println('Usage: v run gh_restart_failed.v <PR_NUMBER|REF>')
		return
	}
	arg := os.args[1]
	mut is_pr := true
	if arg.len > 5 {
		is_pr = false
	} else {
		for r in arg {
			if !r.is_digit() {
				is_pr = false
				break
			}
		}
	}
	mut failed := []Check{}
	mut cancelled := []Check{}
	mut succeeded := 0
	mut in_progress := 0
	mut total := 0
	checks := if is_pr { get_checks_for_pr(arg.int()) } else { get_checks_for_commit(arg) }
	for check in checks {
		total++
		match check.bucket {
			'fail' {
				failed << check
			}
			'cancel' {
				cancelled << check
			}
			'pass' {
				succeeded++
			}
			'pending' {
				in_progress++
			}
			else {
				// Fallback to state if bucket is ambiguous
				if check.state in ['FAILURE', 'TIMED_OUT'] {
					failed << check
				} else if check.state == 'CANCELLED' {
					cancelled << check
				} else if check.state == 'SUCCESS' {
					succeeded++
				} else {
					in_progress++
				}
			}
		}
	}
	mut to_restart := []Check{}
	to_restart << failed
	to_restart << cancelled
	// List failed/cancelled jobs
	if to_restart.len > 0 {
		println('Found ${to_restart.len} failed or cancelled jobs:')
		for job in to_restart {
			println('- ${job.workflow} / ${job.name} (${job.state})')
		}
	} else {
		println('No failed or cancelled jobs found.')
	}

	mut restarted_count := 0
	// Ask for confirmation if there are jobs to restart
	if to_restart.len > 0 {
		println('')
		print(c(tg, 'Do you want to restart these ${to_restart.len} jobs? [y/N]: '))
		answer := os.input('').to_lower().trim_space()
		if answer == 'y' {
			println('')
			for job in to_restart {
				// Extract run_id and job_id from link
				// Link format: https://.../runs/<run_id>/job/<job_id>
				parts := job.link.split('/')
				runs_idx := parts.index('runs')
				job_kw_idx := parts.index('job')
				mut run_id := ''
				mut job_id := ''
				if runs_idx != -1 && runs_idx + 1 < parts.len {
					run_id = parts[runs_idx + 1]
				}
				if job_kw_idx != -1 && job_kw_idx + 1 < parts.len {
					job_id = parts[job_kw_idx + 1]
				}
				if run_id == '' || job_id == '' {
					println('Could not parse IDs from link: ${job.link} (Skipping)')
					continue
				}
				print('Restarting ${job.name} (Run: ${run_id}, Job: ${job_id})... ')
				// Attempt restart
				// Using --job <job_id> with run_id
				restart_cmd := 'gh run rerun ${run_id} --job ${job_id}'
				restart_res := execute_with_progress(restart_cmd)
				if restart_res.exit_code == 0 {
					restarted_count++
				} else {
					println('  Error: ${restart_res.output.trim_space()}')
				}
			}
		} else {
			println('Aborted restart.')
		}
	}
	// Final Summary
	println('')
	println(c(tg, 'Summary:'))
	println('Total jobs found: ${m(total)}')
	println('Failed: ${m(failed.len)}')
	println('Cancelled: ${m(cancelled.len)}')
	println('Succeeded: ${m(succeeded)}')
	println('In Progress: ${m(in_progress)}')
	println('Restarted: ${m(restarted_count)}')
}

fn m(metric int) string {
	return c(tb, metric.str())
}

fn get_checks_for_pr(pr_number int) []Check {
	mut checks := []Check{}
	println(c(tg, 'Fetching checks for PR ${m(pr_number)}...'))
	cmd := 'gh pr checks ${pr_number} --json name,bucket,state,link,workflow'
	res := execute_with_progress(cmd)
	if res.exit_code != 0 {
		println('Error fetching checks: ${res.output}')
		exit(1)
	}
	checks = json.decode([]Check, res.output) or {
		println('Failed to decode JSON: ${err}')
		exit(1)
	}
	return checks
}

fn get_checks_for_commit(commit string) []Check {
	mut checks := []Check{}
	println(c(tg, 'Fetching checks for ref ${c(tb, commit)}...'))
	runs_res := execute_with_progress('gh run list --commit ${commit} --limit 100 --json databaseId,workflowName')
	if runs_res.exit_code != 0 {
		println('Error fetching runs: ${runs_res.output}')
		exit(1)
	}
	runs := json.decode([]GhRun, runs_res.output) or {
		println('Failed to decode runs JSON: ${err}')
		exit(1)
	}
	for run in runs {
		view_res := execute_with_progress('gh run view ${run.database_id} --json jobs,workflowName')
		if view_res.exit_code != 0 {
			continue
		}
		view := json.decode(GhRunView, view_res.output) or { continue }
		for job in view.jobs {
			mut bucket := 'pass'
			mut state := job.conclusion.to_upper()
			if state == '' {
				state = job.status.to_upper()
			}
			if job.conclusion == 'failure' {
				bucket = 'fail'
			} else if job.conclusion == 'cancelled' {
				bucket = 'cancel'
			} else if job.status in ['in_progress', 'queued', 'waiting'] {
				bucket = 'pending'
				if state == '' {
					state = 'PENDING'
				}
			}
			checks << Check{
				name:     job.name
				bucket:   bucket
				state:    state
				link:     job.url
				workflow: view.workflow_name
			}
		}
	}
	return checks
}

fn execute_with_progress(cmd string) os.Result {
	start := time.now()
	start_str := start.hhmmss()
	mut stop := false
	spawn fn (cmd string, start_str string, start time.Time, mut stop &bool) {
		for !*stop {
			elapsed := time.since(start).seconds()
			print('\rRunning ${c(tm, cmd)} [started at ${start_str}] ... elapsed ${elapsed:.1f}s')
			os.flush()
			time.sleep(100 * time.millisecond)
		}
	}(cmd, start_str, start, mut &stop)

	res := os.execute(cmd)
	stop = true
	time.sleep(150 * time.millisecond)
	elapsed := time.since(start).seconds()
	status := if res.exit_code == 0 { 'OK' } else { 'Failed' }
	println('\rCommand ${c(tm, cmd)} [started at ${start_str}] ... done in ${elapsed:.1f}s. Status: ${status}.')
	return res
}
