module main

import os
import rand

fn test_get_server_urls_from_args_supports_all_flags() {
	args := ['install', '-server-url', 'https://one.example/', '--server-url', ' https://two.example ',
		'--server-urls', 'https://one.example']
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
	server_urls := build_install_server_urls(['https://official1.example', 'https://official2.example'], [
		'https://mirror1.example',
		'https://official2.example',
	])
	assert server_urls == ['https://official1.example', 'https://official2.example',
		'https://mirror1.example']
}

fn test_metadata_server_urls_prefers_selected_server_with_fallbacks() {
	mut selector := VpmInstallServerSelector{
		candidate_urls: ['https://official.example', 'https://mirror.example']
	}
	assert selector.metadata_server_urls() == ['https://official.example', 'https://mirror.example']
	selector.selected_url = 'https://mirror.example'
	assert selector.metadata_server_urls() == ['https://mirror.example', 'https://official.example']
}

// `v install --local` has to put a package where the compiler looks for it. The
// module lookup root is the nearest v.mod folder, and a module's import path is
// its path under that root, so the package belongs there directly — the virtual
// `modules/` directory it used to be hidden in is no longer searched.
fn test_local_install_root_is_the_module_lookup_root() {
	root := os.join_path(os.vtmp_dir(), 'vpm_local_root_${rand.ulid()}')
	nested := os.join_path(root, 'sub', 'deep')
	os.mkdir_all(nested) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'vpm_local_root' }\n") or {
		panic(err)
	}
	assert os.real_path(local_vmodules_path(nested)) == os.real_path(root)
}

// Without a v.mod there is no lookup root above the working directory, so the
// working directory itself is the one place an install can still be imported from.
fn test_local_install_root_without_a_manifest_is_the_working_directory() {
	root := os.join_path(os.vtmp_dir(), 'vpm_local_no_manifest_${rand.ulid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	assert os.real_path(local_vmodules_path(root)) == os.real_path(root)
}
