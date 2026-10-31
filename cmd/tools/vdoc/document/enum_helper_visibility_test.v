module document

import os

fn test_private_static_enum_helpers_replace_generated_public_helpers() {
	root := os.join_path(os.vtmp_dir(), 'vdoc_private_enum_helpers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	enum_source := 'pub enum Shade { light dark }\n'
	method_source := 'fn Shade.from(name string) Shade { return .light }\n'
	for method_first in [false, true] {
		source := 'module sample\n' + if method_first {
			method_source + enum_source
		} else {
			enum_source + method_source
		}
		path := os.join_path(root, 'sample.v')
		os.write_file(path, source)!
		mut public_doc := new(root)
		public_doc.parse_file(path)!
		assert 'Shade' in public_doc.contents
		assert 'Shade.from' !in public_doc.contents
		mut all_doc := new(root)
		all_doc.pub_only = false
		all_doc.parse_file(path)!
		method := all_doc.contents['Shade.from']!
		assert !method.is_pub
		assert method.content == 'fn Shade.from(name string) Shade'
	}
}

fn test_private_static_enum_helpers_replace_generated_helpers_across_files() {
	root := os.join_path(os.vtmp_dir(), 'vdoc_private_enum_files_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	enum_path := os.join_path(root, 'enum.v')
	method_path := os.join_path(root, 'method.v')
	os.write_file(enum_path, 'module sample\npub enum Shade { light dark }\n')!
	os.write_file(method_path, 'module sample\nfn Shade.from(name string) Shade { return .light }\n')!
	for paths in [[enum_path, method_path], [method_path, enum_path]] {
		mut public_doc := new(root)
		for path in paths {
			public_doc.parse_file(path)!
		}
		assert 'Shade' in public_doc.contents
		assert 'Shade.from' !in public_doc.contents
	}
}
