module main

import os

fn show_release_tag_for(now string) string {
	script_path := os.join_path(os.dir(@FILE), 'workflows', 'show_manual_release_cmd.vsh')
	cmd := 'V_RELEASE_TAG_NOW="${now}" ${os.quoted_path(@VEXE)} run ${os.quoted_path(script_path)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, res.output
	for line in res.output.split_into_lines() {
		if line.contains('current release_tag: ') {
			return line.all_after('current release_tag: ')
		}
	}
	panic('missing release tag in output:\n${res.output}')
}

fn test_show_manual_release_cmd_uses_iso_week_year() {
	test_cases := {
		'2025-12-28 00:00:00': 'weekly.2025.52'
		'2025-12-29 00:00:00': 'weekly.2026.01'
		'2025-12-31 00:00:00': 'weekly.2026.01'
		'2026-01-01 00:00:00': 'weekly.2026.01'
		'2026-01-04 00:00:00': 'weekly.2026.01'
		'2026-01-05 00:00:00': 'weekly.2026.02'
	}
	for input, expected in test_cases {
		assert show_release_tag_for(input) == expected
	}
}
