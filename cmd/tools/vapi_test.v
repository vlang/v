import os

fn build_vapi_tool(root string) string {
	mut tool := os.join_path(root, 'vapi_tool')
	$if windows {
		tool += '.exe'
	}
	build := os.exec([@VEXE, '-o', tool, os.join_path(@VEXEROOT, 'cmd', 'tools', 'vapi.v')])
	assert build.exit_code == 0, build.output
	return tool
}

// test_vapi_module_isolation_and_iface_mutability covers two regressions:
// snapshots must not merge sibling submodules into the parent module, and
// interface method mutability must be recorded and diffed.
fn test_vapi_module_isolation_and_iface_mutability() {
	root := os.join_path(os.vtmp_dir(), 'vapi_p1_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	tool := build_vapi_tool(root)

	// A submodule `child` declares its own `hello`; the parent snapshot must
	// stay `collision` with a single `hello`, so removing the parent's `hello`
	// is reported as breaking instead of silently matching the child's.
	mod_dir := os.join_path(root, 'collision')
	os.mkdir_all(os.join_path(mod_dir, 'child'))!
	os.write_file(os.join_path(mod_dir, 'collision.v'), 'module collision\n\npub fn hello() int {\n\treturn 1\n}\n')!
	os.write_file(os.join_path(mod_dir, 'child', 'child.v'), 'module child\n\npub fn hello() int {\n\treturn 2\n}\n')!
	snap := os.exec([tool, 'snapshot', mod_dir])
	assert snap.exit_code == 0, snap.output
	assert snap.output.contains('"collision"'), snap.output
	assert snap.output.count('"hello"') == 1, snap.output
	base := os.join_path(root, 'base.json')
	os.write_file(base, snap.output)!
	os.write_file(os.join_path(mod_dir, 'collision.v'), 'module collision\n')!
	check := os.exec([tool, 'check', '--baseline', base, mod_dir])
	assert check.exit_code == 1, check.output
	assert check.output.contains('removed function `hello`'), check.output

	// Adding `mut:` to an interface method changes the snapshot and must be
	// reported as a breaking mutability change.
	iface_dir := os.join_path(root, 'iface')
	os.mkdir_all(iface_dir)!
	os.write_file(os.join_path(iface_dir, 'iface.v'), 'module iface\n\npub interface Speaker {\n\tspeak() string\n}\n')!
	isnap := os.exec([tool, 'snapshot', iface_dir])
	assert isnap.exit_code == 0, isnap.output
	assert isnap.output.contains('"speak"'), isnap.output
	ibase := os.join_path(root, 'iface_base.json')
	os.write_file(ibase, isnap.output)!
	same := os.exec([tool, 'check', '--baseline', ibase, iface_dir])
	assert same.exit_code == 0, same.output
	os.write_file(os.join_path(iface_dir, 'iface.v'), 'module iface\n\npub interface Speaker {\nmut:\n\tspeak() string\n}\n')!
	msnap := os.exec([tool, 'snapshot', iface_dir])
	assert msnap.exit_code == 0, msnap.output
	assert msnap.output != isnap.output, msnap.output
	mcheck := os.exec([tool, 'check', '--baseline', ibase, iface_dir])
	assert mcheck.exit_code == 1, mcheck.output
	assert mcheck.output.contains('mutability changed'), mcheck.output
}
