// Coverage for the two directory-walking entry points of `veb.assets`, which
// `assets_test.v` and `asset_manager_lookup_test.v` never call: both drive `add`
// directly, so nothing asserts how a tree of files becomes `include_name`s.
//
// The tree is built under `os.temp_dir()`, which V resolves from TMPDIR first,
// so a private build environment keeps this off the shared temp directory.

import os
import veb.assets
import time

const fixture_name = 'veb_assets_handle_test_${os.getpid()}'

fn write_fixture(path string, contents string) {
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, contents) or { panic(err) }
}

fn build_tree() string {
	root := os.join_path(os.temp_dir(), fixture_name)
	os.rmdir_all(root) or {}
	write_fixture(os.join_path(root, 'a.css'), '.a {\n\tcolor: red;\n}\n')
	write_fixture(os.join_path(root, 'b.js'), '{"b": 2}\n')
	write_fixture(os.join_path(root, 'notes.txt'), 'not an asset\n')
	write_fixture(os.join_path(root, 'sub', 'c.css'), '.c {\n\tcolor: blue;\n}\n')
	write_fixture(os.join_path(root, 'sub', 'd.js'), '{"d": 4}\n')
	write_fixture(os.join_path(root, 'sub', 'e.png'), 'not an asset either\n')
	return root
}

fn include_names(items []assets.Asset) []string {
	mut names := []string{}
	for a in items {
		names << a.include_name
	}
	names.sort()
	return names
}

fn test_handle_assets_collects_css_and_js_recursively() {
	root := build_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	mut am := assets.AssetManager{}
	am.handle_assets(root) or {
		assert false, 'handle_assets failed: ${err}'
		return
	}
	// .txt and .png are ignored by the extension match.
	assert include_names(am.get_assets(.css)) == ['a.css', os.join_path('sub', 'c.css')]
	assert include_names(am.get_assets(.js)) == ['b.js', os.join_path('sub', 'd.js')]
	assert am.get_assets(.all).len == 4
}

fn test_handle_assets_records_the_file_it_read() {
	root := build_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	mut am := assets.AssetManager{}
	am.handle_assets(root) or {
		assert false, 'handle_assets failed: ${err}'
		return
	}
	for a in am.get_assets(.all) {
		assert os.exists(a.file_path), 'missing ${a.file_path}'
		assert a.file_path.ends_with(a.include_name), '${a.file_path} does not end with ${a.include_name}'
		// `add` stamps the mtime it read, so the two must agree.
		assert a.last_modified == time.unix(os.file_last_mod_unix(a.file_path))
	}
}

fn test_handle_assets_on_an_empty_directory_registers_nothing() {
	root := os.join_path(os.temp_dir(), fixture_name + '_empty')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	mut am := assets.AssetManager{}
	am.handle_assets(root) or {
		assert false, 'handle_assets failed: ${err}'
		return
	}
	assert am.get_assets(.all).len == 0
}

fn test_handle_assets_reports_a_missing_directory() {
	missing := os.join_path(os.temp_dir(), fixture_name + '_does_not_exist')
	os.rmdir_all(missing) or {}
	mut am := assets.AssetManager{}
	am.handle_assets(missing) or { return }
	assert false, 'handle_assets should have failed on a missing directory'
}

fn test_handle_assets_at_prefixes_the_include_names() {
	root := build_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	mut am := assets.AssetManager{}
	am.handle_assets_at(root, 'bundles/') or {
		assert false, 'handle_assets_at failed: ${err}'
		return
	}
	assert include_names(am.get_assets(.css)) == [
		os.join_path('bundles', 'a.css'),
		os.join_path('bundles', 'sub', 'c.css'),
	]
	assert include_names(am.get_assets(.js)) == [
		os.join_path('bundles', 'b.js'),
		os.join_path('bundles', 'sub', 'd.js'),
	]
}

fn test_handle_assets_at_trims_a_trailing_slash() {
	root := build_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	mut with_slash := assets.AssetManager{}
	with_slash.handle_assets_at(root, 'bundles/') or { panic(err) }
	mut without_slash := assets.AssetManager{}
	without_slash.handle_assets_at(root, 'bundles') or { panic(err) }
	assert include_names(with_slash.get_assets(.all)) == include_names(without_slash.get_assets(.all))
}

fn test_handle_assets_at_with_an_empty_prefix_matches_handle_assets() {
	root := build_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	mut plain := assets.AssetManager{}
	plain.handle_assets(root) or { panic(err) }
	mut prefixed := assets.AssetManager{}
	prefixed.handle_assets_at(root, '') or { panic(err) }
	assert include_names(plain.get_assets(.all)) == include_names(prefixed.get_assets(.all))
	assert include_names(plain.get_assets(.all)) == ['a.css', 'b.js', os.join_path('sub', 'c.css'),
		os.join_path('sub', 'd.js')]
}

// NOTE: `include` emits `asset.file_path`, not `include_name`, and
// `add_asset_directory` stores an absolute path there. This matches what
// `assets_test.v` already asserts for a path passed straight to `add`, so the
// directory-walking entry points produce hrefs that are not URLs at all.
fn test_include_emits_the_absolute_file_path_of_a_walked_asset() {
	root := build_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	mut am := assets.AssetManager{}
	am.handle_assets_at(root, 'static') or { panic(err) }
	mut css_path := ''
	mut js_path := ''
	for a in am.get_assets(.all) {
		if a.include_name.ends_with('a.css') {
			css_path = a.file_path
		}
		if a.include_name.ends_with('b.js') {
			js_path = a.file_path
		}
	}
	assert '${am.include(.css, os.join_path('static', 'a.css'))}' == '<link rel="stylesheet" href="${css_path}">'
	assert '${am.include(.js, os.join_path('static', 'b.js'))}' == '<script src="${js_path}"></script>'
	$if windows {
		assert '${am.include(.css, 'static/a.css')}' == ''
	} $else {
		assert '${am.include(.css, 'static/a.css')}' == '<link rel="stylesheet" href="${css_path}">'
	}
}
