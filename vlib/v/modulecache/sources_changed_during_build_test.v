module modulecache

import os
import time

fn test_a_source_changed_during_the_build_is_not_published() ! {
	root := os.join_path(os.vtmp_dir(), 'sources_changed_during_build_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'source.v')
	os.write_file(source, 'module m\npub fn value() int { return 1 }\n')!
	time.sleep(20 * time.millisecond)
	// The build starts after the source was saved.
	enable_file_hash_memo(os.join_path(root, 'memo'))
	time.sleep(20 * time.millisecond)
	manager := new_manager(@VEXEROOT, 'sources_changed_salt', true, '', '')
	manager_at_root := Manager{
		...manager
		dir: os.join_path(root, 'cache')
	}
	dependency := os.join_path(root, 'dependency.h')
	os.write_file(dependency, '#define A 1\n')!
	entry := manager_at_root.write_header('m', [source], 'module m\n')!
	assert os.exists(entry.header_stamp)
	manager_at_root.write_stamp('m', [source], {
		dependency: file_signature(dependency)
	}, 'compile')!
	stamp := os.read_file(entry.object_stamp)!
	// The dependency changed during the build, so its metadata cannot vouch for it.
	assert stamp.contains('dependency=${dependency}\t${file_signature(dependency)}\t\n')

	// A save during the build leaves nothing that vouches for the new contents.
	os.write_file(source, 'module m\npub fn value() int { return 2 }\n')!
	again := manager_at_root.write_header('m', [source], 'module m\n')!
	assert !os.exists(again.header_stamp)
	manager_at_root.write_stamp('m', [source], map[string]string{}, 'compile')!
	assert !os.exists(entry.object_stamp)
	assert !file_changed_during_build(@FILE)
}
