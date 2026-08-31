module main

import flag
import json2
import net.http
import os
import time

struct CreateBugReportResponse {
pub:
	id           string
	delete_url   string
	delete_token string
	message      string
}

struct SendConfig {
	report_url  string
	report_file string
}

fn args_without_command() []string {
	args := os.args#[1..]
	if args.len > 0 && args[0] == 'bug-report-send' {
		return args[1..]
	}
	return args
}

fn config_from_args() !SendConfig {
	mut fp := flag.new_flag_parser(args_without_command())
	fp.application('v bug-report-send')
	fp.version('0.0.1')
	fp.description('Send a V compiler bug report JSON payload.')
	fp.arguments_description('')
	show_help := fp.bool('help', `h`, false, 'Show this help screen.')
	report_url := fp.string('url', 0, '', 'Bug report endpoint URL.')
	report_file := fp.string('file', `f`, '', 'JSON report file to send.')
	if show_help {
		println(fp.usage())
		exit(0)
	}
	remaining := fp.finalize()!
	if remaining.len > 0 {
		return error('unexpected arguments: ${remaining.join(' ')}')
	}
	if report_url == '' {
		return error('missing required -url')
	}
	if report_file == '' {
		return error('missing required -file')
	}
	return SendConfig{
		report_url:  report_url.trim_space().trim_right('/')
		report_file: report_file
	}
}

fn send_bug_report(config SendConfig) !CreateBugReportResponse {
	report_json := os.read_file(config.report_file)!
	mut header := http.new_header(key: .content_type, value: 'application/json')
	header.set(.accept, 'application/json')
	response := http.fetch(
		method:        .post
		url:           config.report_url
		data:          report_json
		header:        header
		max_retries:   1
		read_timeout:  3 * time.second
		write_timeout: 3 * time.second
	)!
	if response.status_code < 200 || response.status_code >= 300 {
		return error('server responded with HTTP ${response.status_code}')
	}
	return json2.decode[CreateBugReportResponse](response.body)!
}

fn main() {
	config := config_from_args() or {
		eprintln('v bug-report-send: ${err}')
		exit(1)
	}
	response := send_bug_report(config) or {
		eprintln('v bug-report-send: ${err}')
		exit(1)
	}
	println('Sent C compiler bug report to ${config.report_url}.')
	if response.id != '' {
		println('Report id: ${response.id}')
	}
	delete_url := if response.delete_url != '' {
		response.delete_url
	} else if response.id != '' && response.delete_token != '' {
		'${config.report_url}/${response.id}?token=${response.delete_token}'
	} else {
		''
	}
	if delete_url != '' {
		println('Delete this report from the server with:')
		println('  curl -X DELETE ${os.quoted_path(delete_url)}')
	}
}
