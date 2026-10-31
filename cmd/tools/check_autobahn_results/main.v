module main

import os
import json2

struct ReportCount {
	total  int
	failed int
}

fn check_report(content string, group string) !ReportCount {
	decoded := json2.decode[json2.Any](content)!
	if decoded !is map[string]json2.Any {
		return error('report must be an object')
	}
	groups := decoded.as_map()
	cases := groups[group] or { return error('missing report group ${group}') }
	if cases !is map[string]json2.Any {
		return error('report group ${group} must be an object')
	}
	mut failed := 0
	for name, entry in cases.as_map() {
		if entry !is map[string]json2.Any {
			return error('case ${name} must be an object')
		}
		fields := entry.as_map()
		behavior := fields['behavior'] or { return error('case ${name} has no behavior') }
		close := fields['behaviorClose'] or { return error('case ${name} has no behaviorClose') }
		if behavior !is string || close !is string {
			return error('case ${name} behaviors must be strings')
		}
		if behavior.str() == 'FAILED' || close.str() == 'FAILED' {
			failed++
		}
	}
	return ReportCount{ total: cases.as_map().len, failed: failed }
}

fn check_reports(root string, clients_only bool) !bool {
	clients := check_report(os.read_file(os.join_path(root, 'clients', 'index.json'))!, 'v-client')!
	if clients_only {
		println('CLIENT ERRORS ${clients.failed} of ${clients.total}')
		return clients.failed == 0
	}
	servers := check_report(os.read_file(os.join_path(root, 'servers', 'index.json'))!, 'AutobahnServer')!
	println('CLIENT ERRORS ${clients.failed} of ${clients.total}, SERVER ERRORS ${servers.failed} of ${servers.total}')
	return clients.failed == 0 && servers.failed == 0
}

fn main() {
	if os.args.len < 2 || os.args.len > 3 || (os.args.len == 3 && os.args[2] != '--clients-only') {
		eprintln('usage: check_autobahn_results <reports_dir> [--clients-only]')
		exit(1)
	}
	ok := check_reports(os.args[1], os.args.len == 3) or {
		eprintln(err)
		exit(1)
	}
	if !ok {
		exit(1)
	}
}
