module main

fn test_get_server_urls_from_args_supports_all_flags() {
	args := ['install', '-server-url', 'https://one.example/', '--server-url',
		' https://two.example ', '--server-urls', 'https://one.example']
	server_urls := get_server_urls_from_args(args)
	assert server_urls == ['https://one.example', 'https://two.example']
}

fn test_get_mirror_urls_from_args_supports_short_and_long_flags() {
	args := ['install', '-m', 'https://mirror1.example/', '--mirror', 'https://mirror2.example',
		'-m', 'https://mirror1.example']
	mirror_urls := get_mirror_urls_from_args(args)
	assert mirror_urls == ['https://mirror1.example', 'https://mirror2.example']
}

fn test_build_install_server_urls_prioritizes_default_servers() {
	server_urls := build_install_server_urls(['https://official1.example',
		'https://official2.example'], ['https://mirror1.example', 'https://official2.example'])
	assert server_urls == ['https://official1.example', 'https://official2.example',
		'https://mirror1.example']
}

fn test_metadata_server_urls_uses_selected_server() {
	mut selector := VpmInstallServerSelector{
		candidate_urls: ['https://official.example', 'https://mirror.example']
	}
	assert selector.metadata_server_urls() == ['https://official.example', 'https://mirror.example']
	selector.selected_url = 'https://mirror.example'
	assert selector.metadata_server_urls() == ['https://mirror.example']
}
