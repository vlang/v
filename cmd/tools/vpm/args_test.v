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
