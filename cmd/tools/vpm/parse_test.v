module main

import os
import rand

const parse_test_root = os.join_path(os.vtmp_dir(), 'vpm_parse_test_${rand.ulid()}')
const parse_tmp_path = os.join_path(parse_test_root, 'vpm_modules')

fn testsuite_begin() {
	os.mkdir_all(parse_tmp_path) or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(parse_test_root) or {}
}

fn tmp_path_error(relative_path string) string {
	get_tmp_path(parse_tmp_path, relative_path) or { return err.msg() }
	return ''
}

fn test_parent_segment_is_rejected() {
	msg := tmp_path_error('pkg/../../victim')
	assert msg.contains('`..` segment'), msg
}

fn test_slash_in_version_is_not_a_parent_segment() {
	path := get_tmp_path(parse_tmp_path, 'release/1.0') or { panic(err) }
	assert path.ends_with('1.0'), path
}

fn test_dots_inside_one_segment_are_not_a_parent_segment() {
	msg := tmp_path_error('foo..bar')
	assert !msg.contains('`..` segment'), msg
}

fn test_version_tag_and_module_path_are_accepted() {
	versioned := get_tmp_path(parse_tmp_path, 'v0.1.0') or { panic(err) }
	assert versioned.ends_with('v0.1.0'), versioned
	plain := get_tmp_path(parse_tmp_path, 'group/name') or { panic(err) }
	assert plain.ends_with('name'), plain
	assert !relative_path_has_parent_segment('v0.1.0')
	assert !relative_path_has_parent_segment('group/name')
}

fn test_existing_dir_inside_tmp_root_is_removed() {
	inner := os.join_path(parse_tmp_path, 'mod')
	os.mkdir_all(inner) or { panic(err) }
	os.write_file(os.join_path(inner, 'x.txt'), 'x') or { panic(err) }
	got := get_tmp_path(parse_tmp_path, 'mod') or { panic(err) }
	assert !os.exists(os.join_path(got, 'x.txt'))
}

fn test_resolved_path_outside_tmp_root_is_not_deleted() {
	outside := os.join_path(parse_test_root, 'outside')
	os.mkdir_all(outside) or { panic(err) }
	marker := os.join_path(outside, 'keep.txt')
	os.write_file(marker, 'keep') or { panic(err) }
	os.symlink(outside, os.join_path(parse_tmp_path, 'linked')) or { panic(err) }
	msg := tmp_path_error('linked')
	assert msg.contains('outside'), msg
	got := os.read_file(marker) or { panic(err) }
	assert got == 'keep'
	assert os.is_dir(outside)
}

fn test_missing_child_through_outside_symlink_is_rejected() {
	outside := os.join_path(parse_test_root, 'outside')
	os.mkdir_all(outside) or { panic(err) }
	marker := os.join_path(outside, 'keep.txt')
	os.write_file(marker, 'keep') or { panic(err) }
	os.symlink(outside, os.join_path(parse_tmp_path, 'link')) or { panic(err) }
	msg := tmp_path_error('link/child')
	assert msg != '', msg
	got := os.read_file(marker) or { panic(err) }
	assert got == 'keep'
	assert os.is_dir(outside)
	assert !os.exists(os.join_path(outside, 'child'))
}
