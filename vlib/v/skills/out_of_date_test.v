module skills

import os

const out_of_date_entry = '---
name: probe
description: a skill used to probe staleness
---

# Probe

'

fn write_skill(directory string, body string, extras map[string]string) {
	os.mkdir_all(directory) or { panic(err) }
	os.write_file(os.join_path(directory, entry_file), body) or { panic(err) }
	for relative, content in extras {
		os.mkdir_all(os.dir(os.join_path(directory, relative))) or { panic(err) }
		os.write_file(os.join_path(directory, relative), content) or { panic(err) }
	}
}

fn out_of_date_fixture() (string, string) {
	root := os.join_path(os.vtmp_dir(), 'skills_out_of_date_${os.getpid()}')
	os.rmdir_all(root) or {}
	vroot := os.join_path(root, 'vroot')
	install_dir := os.join_path(root, 'installed')
	os.mkdir_all(vroot) or { panic(err) }
	os.mkdir_all(install_dir) or { panic(err) }
	return vroot, install_dir
}

// A skill that was installed from this very bundle is not stale, and one the
// bundle has since changed is, whether by edit or by a new file.
fn test_out_of_date_reports_only_the_skills_that_left_the_bundle() {
	vroot, install_dir := out_of_date_fixture()
	defer {
		os.rmdir_all(os.dir(vroot)) or {}
	}
	bundled := os.join_path(vroot, 'vlib', 'v', 'skills', 'probe')
	installed := os.join_path(install_dir, 'probe')
	write_skill(bundled, out_of_date_entry, {
		'references/one.md': 'one\n'
	})
	write_skill(installed, out_of_date_entry, {
		'references/one.md': 'one\n'
	})
	assert out_of_date(vroot, install_dir) == []
	// A changed file in the bundle makes the installation stale.
	write_skill(bundled, out_of_date_entry, {
		'references/one.md': 'one changed\n'
	})
	assert out_of_date(vroot, install_dir) == ['probe']
	// Installing the new content brings it back to current.
	write_skill(installed, out_of_date_entry, {
		'references/one.md': 'one changed\n'
	})
	assert out_of_date(vroot, install_dir) == []
	// A file the bundle gained and the installation lacks is also stale.
	os.write_file(os.join_path(bundled, 'references', 'two.md'), 'two\n') or { panic(err) }
	assert out_of_date(vroot, install_dir) == ['probe']
	// A file only the installation has is not: the file counts are what differ here.
	os.write_file(os.join_path(bundled, 'references', 'three.md'), 'three\n') or { panic(err) }
	assert out_of_date(vroot, install_dir) == ['probe']
}

// A skill that disappears from the bundle is not reported: there is nothing to
// compare it to, so removing it is left to the caller's own report.
fn test_out_of_date_ignores_a_skill_the_bundle_no_longer_has() {
	vroot, install_dir := out_of_date_fixture()
	defer {
		os.rmdir_all(os.dir(vroot)) or {}
	}
	bundled := os.join_path(vroot, 'vlib', 'v', 'skills', 'probe')
	installed := os.join_path(install_dir, 'probe')
	write_skill(bundled, out_of_date_entry, {})
	write_skill(installed, out_of_date_entry, {})
	assert out_of_date(vroot, install_dir) == []
	os.rmdir_all(bundled) or { panic(err) }
	assert out_of_date(vroot, install_dir) == []
	// A directory installed with no `SKILL.md` is not a skill either.
	os.mkdir_all(os.join_path(install_dir, 'not_a_skill')) or { panic(err) }
	assert out_of_date(vroot, install_dir) == []
}

// An install directory that does not exist yet is simply empty, not an error.
fn test_out_of_date_handles_an_absent_or_empty_install_directory() {
	vroot, install_dir := out_of_date_fixture()
	defer {
		os.rmdir_all(os.dir(vroot)) or {}
	}
	write_skill(os.join_path(vroot, 'vlib', 'v', 'skills', 'probe'), out_of_date_entry, {})
	assert out_of_date(vroot, os.join_path(install_dir, 'nowhere')) == []
	os.rmdir_all(install_dir) or { panic(err) }
	assert out_of_date(vroot, install_dir) == []
	// A vroot with no bundled skills folder reports nothing either.
	assert out_of_date(os.join_path(vroot, 'nowhere'), install_dir) == []
}
