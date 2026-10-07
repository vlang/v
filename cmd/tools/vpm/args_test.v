module main

fn test_parse_vpm_command_skips_option_values() {
	args := ['-m', 'https://mirror.example', 'install', '--once', 'nedpals.args']
	assert parse_vpm_command(args) == 'install'
}

fn test_parse_query_args_skips_option_values() {
	args := ['install', '-m', 'https://mirror.example', 'nedpals.args']
	assert parse_query_args(args, 'install') == ['nedpals.args']
}

fn test_parse_query_args_empty_with_only_mirror() {
	args := ['install', '-m', 'https://mirror.example']
	assert parse_query_args(args, 'install') == []string{}
}

fn test_merge_server_urls_appends_custom_urls_after_defaults() {
	default_urls := ['https://official-a.example', 'https://official-b.example']
	custom_urls := ['https://official-b.example', 'https://mirror.example']
	assert merge_server_urls(default_urls, custom_urls) == [
		'https://official-a.example',
		'https://official-b.example',
		'https://mirror.example',
	]
}

fn test_precise_update_option_values_are_not_module_queries() {
	args := ['update', '-p', 'publisher.pkg', '--precise', 'v1.2.3']
	assert parse_vpm_command(args) == 'update'
	assert parse_query_args(args, 'update') == []string{}
	assert parse_query_args(['update', 'pkg', '--precise', 'v1.2.3'], 'update') == ['pkg']
}

fn test_update_and_release_policy_option_values_are_not_package_queries() {
	args := ['update', '-p', 'pkg', '--precise', 'v1.2.3', '--dry-run']
	assert parse_query_args(args, 'update') == []string{}
	assert parse_query_args(['update', 'pkg', '--precise', 'v1.2.3'], 'update') == ['pkg']
	assert parse_query_args(['update', '--pin', 'pkg', '--precise', 'v1.2.3'], 'update') == []string{}
	assert parse_query_args(['install', '--exclude-newer', '2026-01-01', '--minimum-release-age',
		'2d', 'pkg@^1'], 'install') == ['pkg@^1']
}
