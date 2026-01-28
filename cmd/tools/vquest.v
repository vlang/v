module main

import os
import cli
import net.http
import net.urllib
import json
import rand
import term

const search_endpoint = 'https://api.github.com/search/issues'
const search_query = 'repo:vlang/v is:issue is:open -label:"Status: Confirmed"'
const per_page = 100
const max_search_results = 1000

struct SearchResponse {
	total_count int
	items       []Issue
}

struct Issue {
	html_url string
}

fn main() {
	// the 0th arg is /path/to/vquest, the 1st is `quest`; the args after that are the subcommands
	mut args := []string{}
	args << os.args[0]
	args << os.args#[2..]
	mut app := cli.Command{
		name:        'quest'
		description: 'A tool to help make V better for everyone, by spending some time each day, on random tasks/missions like:\n * documenting public APIs\n * issue confirmation reviewing and triage\n * testing'
		execute:     cli.print_help_for_command
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
				description: 'Open a random unconfirmed vlang/v issue in your browser.'
				flags:       [
					cli.Flag{
						flag:        .bool
						name:        'print-only'
						abbrev:      'p'
						description: 'Print the issue URL without opening a browser.'
					},
				]
				execute:     run_confirm
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

fn run_confirm(cmd cli.Command) ! {
	print_only := cmd.flags.get_bool('print-only') or { false }
	total := fetch_total_count()!
	max_pages := total_to_max_pages(total)
	if max_pages == 0 {
		return error('no unconfirmed issues found')
	}
	page := (rand.intn(max_pages) or { 0 }) + 1
	eprintln(term.colorize(term.gray, 'Found: ${total} still unconfirmed issues. Fetching issue from page: ${page} ...'))
	issue := fetch_issue_from_page(page)!
	if print_only {
		println(issue.html_url)
		return
	}
	os.open_uri(issue.html_url)!
	println(term.colorize(term.green, 'Help us by confirming and triaging this issue:'))
	println(issue.html_url)
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

fn fetch_total_count() !int {
	url := build_search_url(1, 1)
	body := api_get(url)!
	resp := json.decode(SearchResponse, body)!
	return resp.total_count
}

fn fetch_issue_from_page(page int) !Issue {
	url := build_search_url(page, per_page)
	body := api_get(url)!
	resp := json.decode(SearchResponse, body)!
	if resp.items.len == 0 {
		return error('no issues returned for page ${page}')
	}
	idx := rand.intn(resp.items.len) or { 0 }
	return resp.items[idx]
}

fn build_search_url(page int, per_page int) string {
	mut values := urllib.new_values()
	values.add('q', search_query)
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
