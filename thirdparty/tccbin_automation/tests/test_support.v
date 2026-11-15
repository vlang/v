module tests

import os

fn automation_root() string {
	return os.real_path(os.join_path(os.getwd(), 'thirdparty', 'tccbin_automation'))
}

const dormant_toolchain_binding = '"toolchain_profile": {\n        "profile_id": null,\n        "profile_path": null,\n        "profile_sha256": null\n      }'

const production_toolchain_bindings = [
	'"toolchain_profile": {\n        "profile_id": "macos-arm64-github-hosted-clang-v1",\n        "profile_path": "toolchain-profiles/macos-arm64.profile.json",\n        "profile_sha256": "f8022035063578f6102896ed1fb0046dd3f286880f44dab6173cac8e6711b4bc"\n      }',
]

// dormant_registry_source returns the registry at `root` with every reviewed production toolchain
// binding reset to null. Synthetic automation roots copy only `schemas/` and bind their own
// synthetic profiles, so they must not inherit a binding whose profile file they do not carry.
fn dormant_registry_source(root string) string {
	mut source := os.read_file(os.join_path(root, 'targets.json')) or { panic(err) }
	for binding in production_toolchain_bindings {
		assert source.count(binding) == 1
		source = source.replace_once(binding, dormant_toolchain_binding)
	}
	return source
}

// write_cmd_git_wrapper writes a `git.cmd` that runs the installed git.exe and returns its directory.
// `tag` keeps the directory name apart for each caller.
fn write_cmd_git_wrapper(tag string) !string {
	real_git := os.find_abs_path_of_executable('git')!
	wrapper_dir := os.join_path(os.temp_dir(), 'tccbin-git-cmd-${tag}-${os.getpid()}')
	os.rmdir_all(wrapper_dir) or {}
	os.mkdir_all(wrapper_dir)!
	os.write_file(os.join_path(wrapper_dir, 'git.cmd'), '@"${real_git}" %*\r\n')!
	return wrapper_dir
}
