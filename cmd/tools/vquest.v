module main

import os
import cli
import net.http
import net.urllib
import json
import rand
import term

const search_endpoint = 'https://api.github.com/search/issues'
const issue_endpoint = 'https://api.github.com/repos/vlang/v/issues'
const confirm_search_query = 'repo:vlang/v is:issue is:open -label:"Status: Confirmed"'
const fix_search_query = 'repo:vlang/v is:issue is:open'
const feature_search_query = 'repo:vlang/v is:issue state:open label:"Feature/Enhancement Request"'
const per_page = 100
const max_search_results = 1000

struct SearchResponse {
	total_count int
	items       []Issue
}

struct Issue {
	number   int
	title    string
	html_url string
	body     string
	labels   []Label
}

struct Label {
	name string
}

struct IssueDetails {
	number   int
	title    string
	html_url string
	body     string
	labels   []Label
	state    string
}

fn main() {
	// the 0th arg is /path/to/vquest, the 1st is `quest`; the args after that are the subcommands
	mut args := []string{}
	args << os.args[0]
	args << os.args#[2..]
	mut app := cli.Command{
		name:        'v quest'
		description: 'A tool to help make V better for everyone, by spending some time each day, on random tasks/missions like:\n * documenting public APIs\n * issue confirmation reviewing and triage\n * testing'
		execute:     cli.print_help_for_command
		posix_mode:  true
		defaults:    struct {
			man: false
		}
		commands:    [
			cli.Command{
				name:        'document'
				description: 'Print a random missing doc entry from the V standard library.'
				execute:     run_document
			},
			cli.Command{
				name:        'confirm'
				description: 'Open a random vlang/v issue, that is still unconfirmed in your browser.'
				flags:       issue_flags.clone()
				execute:     run_confirm
			},
			cli.Command{
				name:        'fix'
				description: 'Open a random vlang/v issue (but still open) in your browser.'
				flags:       issue_flags.clone()
				execute:     run_fix
			},
			cli.Command{
				name:        'implement'
				description: 'Open a random vlang/v feature request issue in your browser.'
				flags:       issue_flags.clone()
				execute:     run_implement
			},
			cli.Command{
				name:        'solve'
				description: 'Find a random bug reproducible on your OS, print it to stdout and save to bug-<issue_id>.md.'
				flags:       solve_flags.clone()
				execute:     run_solve
			},
		]
	}
	app.setup()
	if args.len <= 1 {
		if rcmd := rand.element(app.commands) {
			rcmd.execute(rcmd)!
			return
		}
	}
	app.parse(args)
}

const issue_flags = [
	cli.Flag{
		description: 'Print the issue URL without opening a browser.'
		flag:        .bool
		name:        'print-only'
		abbrev:      'p'
	},
	cli.Flag{
		description:   'Start page for issues (default -1: auto). Must be > 0.'
		flag:          .int
		name:          'from'
		abbrev:        'f'
		default_value: ['-1']
	},
	cli.Flag{
		description:   'End page for issues (default -1: auto). Must be > 0 and >= the from page (see -f).'
		flag:          .int
		name:          'to'
		abbrev:        't'
		default_value: ['-1']
	},
]

const solve_flags = [
	cli.Flag{
		description:   'Override OS detection (macos, windows, linux).'
		flag:          .string
		name:          'os'
		abbrev:        'o'
		default_value: ['']
	},
	cli.Flag{
		description:   'Start page for issues (default -1: auto). Must be > 0.'
		flag:          .int
		name:          'from'
		abbrev:        'f'
		default_value: ['-1']
	},
	cli.Flag{
		description:   'End page for issues (default -1: auto). Must be > 0 and >= the from page (see -f).'
		flag:          .int
		name:          'to'
		abbrev:        't'
		default_value: ['-1']
	},
	cli.Flag{
		description:   'Output file for the bug report (default: bug-<issue_id>.md).'
		flag:          .string
		name:          'output'
		abbrev:        'O'
		default_value: ['']
	},
]

fn run_confirm(cmd cli.Command) ! {
	run_issue(cmd, confirm_search_query, 'still unconfirmed', 'Help us by confirming and triaging this issue:')!
}

fn run_fix(cmd cli.Command) ! {
	run_issue(cmd, fix_search_query, 'open', 'Help us by fixing or confirming this issue:')!
}

fn run_implement(cmd cli.Command) ! {
	run_issue(cmd, feature_search_query, 'feature request', 'Help us by implementing the issue in a PR, or triage it:')!
}

fn run_solve(cmd cli.Command) ! {
	// Clean up old bug report files
	for f in os.glob('bug-*.md') or { []string{} } {
		os.rm(f) or {}
	}

	user_os := get_target_os(cmd)
	os_label := get_os_label(user_os)
	output_override := cmd.flags.get_string('output') or { '' }

	// Build query that excludes issues for other OSes
	// We look for open bugs that are NOT exclusive to other operating systems
	excluded_os_labels := get_excluded_os_labels(user_os)
	mut query := 'repo:vlang/v is:issue is:open label:Bug'
	for label in excluded_os_labels {
		query += ' -label:"${label}"'
	}

	total := fetch_total_count(query)!
	max_pages := total_to_max_pages(total)
	if max_pages == 0 {
		return error('no bugs found for ${os_label}')
	}
	start_page, end_page := resolve_page_range(cmd, max_pages)!
	page := start_page + (rand.intn(end_page - start_page + 1) or { 0 })
	eprintln(term.colorize(term.gray, 'Found: ${total} bugs reproducible on ${os_label}. Fetching from page: ${page} in [${start_page}, ${end_page}] ...'))

	issue := fetch_issue_from_page(query, page)!
	details := fetch_issue_details(issue.number)!

	// Format the bug report
	report := format_bug_report(details, user_os)

	// Print to stdout
	println(report)

	// Determine output filename (default: bug-<issue_id>.md)
	output_file := if output_override != '' { output_override } else { 'bug-${details.number}.md' }

	// Write to file
	os.write_file(output_file, report) or {
		return error('failed to write to ${output_file}: ${err}')
	}
	eprintln(term.colorize(term.green, '\nBug report saved to: ${output_file}'))
}

fn get_target_os(cmd cli.Command) string {
	override := cmd.flags.get_string('os') or { '' }
	if override != '' {
		return override.to_lower()
	}
	return os.user_os()
}

fn get_os_label(user_os string) string {
	return match user_os {
		'macos' { 'macOS' }
		'windows' { 'Windows' }
		'linux' { 'Linux' }
		'freebsd' { 'FreeBSD' }
		else { user_os }
	}
}

fn get_excluded_os_labels(user_os string) []string {
	// Return labels for OSes that should be EXCLUDED
	// i.e., if user is on macOS, exclude Windows-only and Linux-only bugs
	all_os_labels := ['OS: Windows', 'OS: Linux', 'OS: macOS', 'OS: FreeBSD']
	user_label := match user_os {
		'macos' { 'OS: macOS' }
		'windows' { 'OS: Windows' }
		'linux' { 'OS: Linux' }
		'freebsd' { 'OS: FreeBSD' }
		else { '' }
	}
	return all_os_labels.filter(it != user_label)
}

fn fetch_issue_details(issue_number int) !IssueDetails {
	url := '${issue_endpoint}/${issue_number}'
	body := api_get(url)!
	return json.decode(IssueDetails, body)!
}

fn format_bug_report(issue IssueDetails, user_os string) string {
	mut report := '# Bug #${issue.number}: ${issue.title}\n\n'
	report += '**URL:** ${issue.html_url}\n\n'
	report += '**Target OS:** ${get_os_label(user_os)}\n\n'

	if issue.labels.len > 0 {
		labels := issue.labels.map(it.name).join(', ')
		report += '**Labels:** ${labels}\n\n'
	}

	report += '## Description\n\n'
	report += issue.body
	report += '\n'

	return report
}

fn run_issue(cmd cli.Command, issue_query string, issue_label string, help_label string) ! {
	print_only := cmd.flags.get_bool('print-only') or { false }
	total := fetch_total_count(issue_query)!
	max_pages := total_to_max_pages(total)
	if max_pages == 0 {
		return error('no unconfirmed issues found')
	}
	start_page, end_page := resolve_page_range(cmd, max_pages)!
	page := start_page + (rand.intn(end_page - start_page + 1) or { 0 })
	eprintln(term.colorize(term.gray, 'Found: ${total} ${issue_label} issues. Fetching issue from page: ${page} in [${start_page}, ${end_page}] ...'))
	issue := fetch_issue_from_page(issue_query, page)!
	println(term.colorize(term.green, help_label))
	println(issue.html_url)
	if print_only {
		return
	}
	open_uri(issue.html_url)!
}

fn run_document(cmd cli.Command) ! {
	res := os.execute('v missdoc --exclude vlib/v --exclude /linux_bare/ --exclude /wasm_bare/ @vlib')
	if res.exit_code != 0 {
		return error('v missdoc failed: ${res.output}')
	}
	lines := res.output.split_into_lines().filter(it.trim_space() != '')
	if lines.len == 0 {
		return error('no missing doc entries found')
	}
	idx := rand.intn(lines.len) or { 0 }
	eprintln(term.colorize(term.green, 'Help us document this public API:'))
	println(term.colorize(term.bold, lines[idx]))
}

fn fetch_total_count(query string) !int {
	url := build_search_url(query, 1, 1)
	body := api_get(url)!
	resp := json.decode(SearchResponse, body)!
	return resp.total_count
}

fn resolve_page_range(cmd cli.Command, max_pages int) !(int, int) {
	from, from_set := read_page_limit(cmd, 'from', '-f')!
	to, to_set := read_page_limit(cmd, 'to', '-t')!
	if from_set && to_set && to < from {
		return error('invalid page range: -t (${to}) is smaller than -f (${from})')
	}
	mut start_page := 1
	mut end_page := max_pages
	if from_set {
		start_page = from
	}
	if to_set {
		end_page = to
	}
	if start_page < 1 {
		start_page = 1
	}
	if end_page > max_pages {
		end_page = max_pages
	}
	if end_page < start_page {
		return error('no issues found in the requested page range')
	}
	return start_page, end_page
}

fn read_page_limit(cmd cli.Command, name string, flag_label string) !(int, bool) {
	value := cmd.flags.get_int(name)!
	is_set := flag_is_set(cmd, name)
	if is_set && value < 0 {
		return error('${flag_label} must be >= 0')
	}
	return value, is_set
}

fn flag_is_set(cmd cli.Command, name string) bool {
	for flag in cmd.flags.get_all_found() {
		if flag.name == name {
			return true
		}
	}
	return false
}

fn fetch_issue_from_page(query string, page int) !Issue {
	url := build_search_url(query, page, per_page)
	body := api_get(url)!
	resp := json.decode(SearchResponse, body)!
	if resp.items.len == 0 {
		return error('no issues returned for page ${page}')
	}
	idx := rand.intn(resp.items.len) or { 0 }
	return resp.items[idx]
}

fn build_search_url(query string, page int, per_page int) string {
	mut values := urllib.new_values()
	values.add('q', query)
	values.add('per_page', per_page.str())
	values.add('page', page.str())
	return '${search_endpoint}?${values.encode()}'
}

fn api_get(url string) !string {
	resp := http.fetch(
		url:    url
		method: .get
		header: http.new_header_from_map({
			http.CommonHeader.accept:     'application/vnd.github+json'
			http.CommonHeader.user_agent: 'v quest'
		})
	)!
	if resp.status_code != 200 {
		return error('GitHub API error ${resp.status_code}: ${resp.body}')
	}
	return resp.body
}

fn total_to_max_pages(total int) int {
	if total <= 0 {
		return 0
	}
	max_pages := (total + per_page - 1) / per_page
	limit := max_search_results / per_page
	return if max_pages < limit { max_pages } else { limit }
}

fn open_uri(uri string) ! {
	$if !termux {
		os.open_uri(uri)!
	}
}
