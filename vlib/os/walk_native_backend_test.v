import os

// The native backend lays `C.dirent` out from the fields that V declares for it, where the C
// backend leaves that to the header of the system. A walk on Linux reads the name and the kind
// of an entry from that struct, so a program built by either backend must report the same paths.
fn test_a_walk_built_by_the_native_backend_reports_the_same_paths() {
	$if !linux || !amd64 {
		eprintln('> skipping ${@FN}: it is for the native backend on Linux, amd64')
		return
	}
	folder := os.join_path(os.vtmp_dir(), 'walk_native_backend_${os.getpid()}')
	os.rmdir_all(folder) or {}
	tree := os.join_path_single(folder, 'tree')
	os.mkdir_all(os.join_path(tree, 'inner', '.hidden_dir'))!
	defer {
		os.rmdir_all(folder) or {}
	}
	os.write_file(os.join_path_single(tree, 'top.txt'), 'x')!
	os.write_file(os.join_path(tree, 'inner', 'deep.txt'), 'x')!
	os.write_file(os.join_path(tree, 'inner', '.hidden_dir', '.dot'), 'x')!
	os.symlink(os.join_path_single(tree, 'inner'), os.join_path_single(tree, 'link'))!
	source := os.join_path_single(folder, 'walker.v')
	os.write_file(source, "import os\n\nfn main() {\n\tos.walk(os.args[1], fn (path string) {\n\t\tprintln('W ' + path)\n\t})\n\tos.walk_with_context(os.args[1], unsafe { nil }, fn (ctx voidptr, path string) {\n\t\tprintln('C ' + path)\n\t})\n}\n")!
	mut outputs := []string{}
	for backend in ['-native', '-b c'] {
		exe := os.join_path_single(folder, 'walker' + backend.replace(' ', '_'))
		mut build := [@VEXE]
		build << backend.split(' ')
		build << ['-o', exe, source]
		built := os.exec(build)
		assert built.exit_code == 0, '${backend}: ${built.output}'
		ran := os.exec([exe, tree])
		assert ran.exit_code == 0, '${backend}: ${ran.output}'
		mut lines := ran.output.split_into_lines()
		lines.sort()
		outputs << lines.join('\n')
	}
	mut expected := ['C inner', 'C inner/.hidden_dir', 'C inner/.hidden_dir/.dot', 'C inner/deep.txt',
		'C link', 'C top.txt', 'W inner/.hidden_dir/.dot', 'W inner/deep.txt', 'W link', 'W top.txt']
	expected = expected.map(it[..2] + os.join_path_single(tree, it[2..]))
	expected.sort()
	assert outputs[1] == expected.join('\n')
	assert outputs[0] == outputs[1]
}
