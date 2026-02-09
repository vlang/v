import os
import json
import term

const c = term.colorize
const tg = term.green
const tb = term.bold

struct Check {
	name     string
	bucket   string
	state    string
	link     string
	workflow string
}

fn main() {
	unbuffer_stdout()
	if os.args.len != 2 {
		println('Usage: v run gh_restart_failed.v <PR_NUMBER>')
		return
	}
	pr_number := os.args[1].int()
	println(c(tg, 'Fetching checks for PR ${m(pr_number)}...'))
	// Fetch checks using gh CLI
	cmd := 'gh pr checks ${pr_number} --json name,bucket,state,link,workflow'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		println('Error fetching checks: ${res.output}')
		return
	}
	checks := json.decode([]Check, res.output) or {
		println('Failed to decode JSON: ${err}')
		return
	}
	mut failed := []Check{}
	mut cancelled := []Check{}
	mut succeeded := 0
	mut in_progress := 0
	mut total := 0
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
				restart_res := os.execute(restart_cmd)
				if restart_res.exit_code == 0 {
					println('OK')
					restarted_count++
				} else {
					println('Failed')
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
