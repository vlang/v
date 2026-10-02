module main

import os
import v.skills

fn test_forced_install_refuses_symlinks_without_changing_their_targets() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'skills_install_symlink_review_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	bundle := os.join_path(root, 'bundled', 'alpha')
	os.mkdir_all(bundle)!
	os.write_file(os.join_path(bundle, 'SKILL.md'), '---\nname: alpha\ndescription: Test installation.\n---\n\n# Alpha\n')!
	skill := skills.Skill{
		name:        'alpha'
		description: 'Test installation.'
		directory:   bundle
		files:       ['SKILL.md']
	}
	dest := os.join_path(root, 'installed')
	victim := os.join_path(root, 'victim')
	os.mkdir_all(dest)!
	os.mkdir_all(victim)!
	marker := os.join_path(victim, 'keep.txt')
	os.write_file(marker, 'unrelated contents')!
	link := os.join_path(dest, 'alpha')
	os.symlink(victim, link)!
	for force in [false, true] {
		for dry_run in [false, true] {
			result := skills.install(skill, dest, skills.InstallOptions{ force: force, dry_run: dry_run }) or {
				assert err.msg().contains('symlink skill directory'), err.msg()
				assert os.is_link(link)
				assert os.read_file(marker)! == 'unrelated contents'
				continue
			}
			assert false, 'symlink installation was accepted: ${result}'
		}
	}
	os.rm(link)!
	os.symlink(os.join_path(root, 'absent'), link)!
	assert skills.install(skill, dest, skills.InstallOptions{ force: true }) == none
	assert os.is_link(link)
	os.rm(link)!
	skills.install(skill, dest, skills.InstallOptions{})!
	entry := os.join_path(link, 'SKILL.md')
	os.write_file(entry, 'local edit')!
	skills.install(skill, dest, skills.InstallOptions{ force: true, dry_run: true })!
	assert os.read_file(entry)! == 'local edit'
	skills.install(skill, dest, skills.InstallOptions{ force: true })!
	assert os.read_file(entry)! == os.read_file(os.join_path(bundle, 'SKILL.md'))!
}
