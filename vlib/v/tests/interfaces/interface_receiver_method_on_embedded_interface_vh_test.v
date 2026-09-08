import os

fn run_v_in_dir(workdir string, args []string) os.Result {
	mut process := os.new_process(@VEXE)
	process.set_work_folder(workdir)
	process.set_args(args)
	process.set_redirect_stdio()
	process.wait()
	stdout := process.stdout_slurp()
	stderr := process.stderr_slurp()
	exit_code := process.code
	process.close()
	return os.Result{
		exit_code: exit_code
		output:    stdout + stderr
	}
}

fn test_external_interface_receiver_method_from_vh() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'interface_receiver_method_vh_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'nodes'))!
	os.write_file(os.join_path(root, 'v.mod'), "Module {\n\tname: 'iface_vh'\n}\n")!
	// `build-module` emits bodyless receiver method declarations in `.vh` files.
	os.write_file(os.join_path(root, 'nodes', 'nodes.vh'),
		'module nodes\n\npub interface Node {}\n\npub fn (node Node) accept(child &Node)\n\npub interface Element {\n\tNode\n}\n')!
	os.write_file(os.join_path(root, 'main.v'),
		'module main\n\nimport nodes\n\nstruct ElementImpl {}\n\nstruct Child {}\n\nfn main() {\n\tmut element := &nodes.Element(&ElementImpl{})\n\telement.accept(&nodes.Node(&Child{}))\n\tprintln(1)\n}\n')!
	run_result := run_v_in_dir(root, ['-gc', 'none', '-check', '.'])
	assert run_result.exit_code == 0, run_result.output
}

fn test_mutable_external_interface_receiver_method_from_vh_is_rejected() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'mutable_interface_receiver_method_vh_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'nodes'))!
	os.write_file(os.join_path(root, 'v.mod'), "Module {\n\tname: 'mutable_iface_vh'\n}\n")!
	os.write_file(os.join_path(root, 'nodes', 'nodes.vh'),
		'module nodes\n\npub interface Node {}\n\npub fn (mut node Node) replace(next Node)\n\npub interface Element {\n\tNode\n}\n')!
	os.write_file(os.join_path(root, 'main.v'),
		'module main\n\nimport nodes\n\nstruct Item {}\n\nfn main() {\n\tmut element := nodes.Element(Item{})\n\telement.replace(nodes.Node(Item{}))\n}\n')!
	run_result := run_v_in_dir(root, ['-gc', 'none', '-check', '.'])
	assert run_result.exit_code == 1, run_result.output
	assert run_result.output.contains('its body is unavailable and may replace its receiver'), run_result.output
}

fn test_implicitly_mutable_receiver_argument_is_rejected() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'implicit_mut_interface_receiver_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(root)!
	os.write_file(os.join_path(root, 'v.mod'), "Module {\n\tname: 'implicit_mut_iface'\n}\n")!
	os.write_file(os.join_path(root, 'main.v'),
		'module main\n\ninterface Node {}\n\ninterface Element {\n\tNode\n}\n\nstruct Item {}\n\nfn main() {\n\tmut element := Element(Item{})\n\telement.replace_implicitly(Node(Item{}))\n}\n\nfn replace_node(mut node Node, next Node) {\n\tnode = next\n}\n\nfn (mut node Node) replace_implicitly(next Node) {\n\treplace_node(node, next)\n}\n')!
	run_result := run_v_in_dir(root, ['-gc', 'none', '-disable-explicit-mutability', '-check',
		'.'])
	assert run_result.exit_code == 1, run_result.output
	assert run_result.output.contains('it can replace its receiver through a mutable call'), run_result.output
}

fn test_external_closure_receiver_replacement_is_rejected() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'external_closure_interface_receiver_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'nodes'))!
	os.write_file(os.join_path(root, 'v.mod'), "Module {\n\tname: 'external_closure_iface'\n}\n")!
	os.write_file(os.join_path(root, 'nodes', 'nodes.v'),
		'module nodes\n\npub interface Node {}\n\npub fn (mut node Node) replace_in_closure(next Node) {\n\treplace := fn [mut node, next] () {\n\t\tnode = next\n\t}\n\treplace()\n}\n\npub interface Element {\n\tNode\n}\n\npub struct Item {}\n')!
	os.write_file(os.join_path(root, 'main.v'),
		'module main\n\nimport nodes\n\nfn main() {\n\tmut element := nodes.Element(nodes.Item{})\n\telement.replace_in_closure(nodes.Node(nodes.Item{}))\n}\n')!
	run_result := run_v_in_dir(root, ['-gc', 'none', '-check', '.'])
	assert run_result.exit_code == 1, run_result.output
	assert run_result.output.contains('it captures its receiver mutably and can replace it'), run_result.output
}
