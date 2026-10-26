module main

import os

fn test_compact_reordered_json_counts_each_case_once() {
	count := check_report('{"other":{},"v-client":{"2":{"behaviorClose":"FAILED","behavior":"FAILED"},"1":{"behavior":"OK","behaviorClose":"OK"}}}', 'v-client')!
	assert count.total == 2
	assert count.failed == 1
}

fn test_invalid_reports_are_rejected() {
	for content in ['{', '{}', '[]', '{"v-client":[]}', '{"v-client":{"1":{"behavior":"OK"}}}',
		'{"v-client":{"1":{"behavior":1,"behaviorClose":"OK"}}}'] {
		mut rejected := false
		check_report(content, 'v-client') or { rejected = true }
		assert rejected, content
	}
}

fn test_wss_reports_need_only_clients() {
	root := os.join_path(os.vtmp_dir(), 'autobahn_reports_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'clients'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'clients', 'index.json'), '{"v-client":{"1":{"behavior":"OK","behaviorClose":"OK"}}}')!
	assert check_reports(root, true)!
	mut rejected := false
	check_reports(root, false) or { rejected = true }
	assert rejected
}
