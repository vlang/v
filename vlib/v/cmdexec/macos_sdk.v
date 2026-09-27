module cmdexec

import os

const macos_sdk_cache_version = '1'

fn macos_sdk_cache_escape(value string) string {
	return value.replace('\\', '\\\\').replace('|', '\\|').replace('\n', '\\n')
}

fn macos_sdk_selection_marker() string {
	mut values := [os.getenv('SDKROOT'), os.getenv('DEVELOPER_DIR'), os.getenv('TOOLCHAINS'),
		os.getenv('PATH')]
	xcode_select_link := '/var/db/xcode_select_link'
	selection := if os.is_link(xcode_select_link) {
		os.readlink(xcode_select_link) or { '' }
	} else {
		''
	}
	values << selection
	for developer_dir in ['/Library/Developer/CommandLineTools',
		'/Applications/Xcode.app/Contents/Developer'] {
		values << '${os.is_dir(developer_dir)}:${os.file_last_mod_unix(developer_dir)}'
	}
	return values.map(macos_sdk_cache_escape(it)).join('|')
}

fn macos_sdk_read_cache(path string, marker string) string {
	content := os.read_file(path) or { return '' }
	lines := content.split_into_lines()
	if lines.len != 3 || lines[0] != macos_sdk_cache_version || lines[1] != marker
		|| !os.is_dir(lines[2]) {
		return ''
	}
	return lines[2]
}

fn macos_sdk_write_cache(path string, marker string, root string) {
	if !os.is_dir(root) {
		return
	}
	tmp_path := '${path}.${os.getpid()}.tmp'
	os.write_file(tmp_path, '${macos_sdk_cache_version}\n${marker}\n${root}\n') or { return }
	os.rename(tmp_path, path) or { os.rm(tmp_path) or {} }
}

// macos_sdk_root finds and caches the SDK selected by Apple's command-line tools.
pub fn macos_sdk_root() string {
	env_root := os.getenv('SDKROOT')
	if os.is_dir(env_root) {
		return env_root
	}
	marker := macos_sdk_selection_marker()
	cache_path := os.join_path_single(os.vtmp_dir(), 'v3_macos_sdk_root')
	cached_root := macos_sdk_read_cache(cache_path, marker)
	if cached_root != '' {
		return cached_root
	}
	result := run('xcrun', ['--show-sdk-path'])
	if result.exit_code == 0 {
		found := result.output.trim_space()
		if os.is_dir(found) {
			macos_sdk_write_cache(cache_path, marker, found)
			return found
		}
	}
	for candidate in ['/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk',
		'/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk'] {
		if os.is_dir(candidate) {
			macos_sdk_write_cache(cache_path, marker, candidate)
			return candidate
		}
	}
	return ''
}
