module types

import os

fn test_veb_metadata_names_are_known_and_case_sensitive() {
	root := os.join_path(os.vtmp_dir(), 'veb_attribute_names_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'main.v')
	source := 'struct Payload {
		name string @[description: \'A name\'; example: \'"V"\']
	}
	@[connect]
	fn connect_route() {}
	@[trace]
	fn trace_route() {}
	fn main() {
		println(Payload{})
		connect_route()
		trace_route()
	}'
	os.write_file(path, source) or { panic(err) }
	accepted := os.exec([@VEXE, '-W', '-check', path])
	assert accepted.exit_code == 0, accepted.output
	for name in ['description', 'example', 'connect', 'trace'] {
		unknown := name.capitalize()
		os.write_file(path, source.replace(name, unknown)) or { panic(err) }
		rejected := os.exec([@VEXE, '-W', '-check', path])
		assert rejected.exit_code != 0, rejected.output
		assert rejected.output.contains('unknown attribute `${unknown}`'), rejected.output
	}
}
