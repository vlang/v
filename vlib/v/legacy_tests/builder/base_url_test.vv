module builder

import os

fn test_find_module_path_from_vmod_root_honors_base_url() {
	root := os.join_path(os.vtmp_dir(), 'v_builder_base_url_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'source', 'feature'))!
	os.write_file(os.join_path(root, 'v.mod'),
		"Module {\n\tname: 'example.pkg'\n\tbase_url: 'source'\n}\n")!
	assert find_module_path_from_vmod_root(root, 'example.pkg.feature')! == os.join_path(root,
		'source', 'feature')
}
