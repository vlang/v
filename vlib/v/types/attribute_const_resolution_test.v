module types

import os

fn attribute_const_resolution_root(suffix string) string {
	root := os.join_path(os.vtmp_dir(), 'attribute_const_${suffix}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'attribute_const_${suffix}' }\n") or {
		panic(err)
	}
	return root
}

fn attribute_const_check(root string) os.Result {
	return os.exec([@VEXE, '-check', root])
}

fn test_attribute_const_private_is_rejected() {
	root := attribute_const_resolution_root('private')
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'provider')) or { panic(err) }
	os.write_file(os.join_path(root, 'provider', 'provider.v'), "module provider\n\nconst private_path = '/private'\n\npub const path = '/public'\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main\n\nimport provider\n\n@[attribute]\nstruct Route {\n\tpath string\n}\n\n@[Route(provider.private_path)]\nfn foo() {}\n\nfn main() {}\n') or {
		panic(err)
	}
	result := attribute_const_check(root)
	assert result.exit_code != 0, result.output
	assert result.output.contains('is private'), result.output
	// Public constant from the same provider passes.
	os.write_file(os.join_path(root, 'main.v'), 'module main\n\nimport provider\n\n@[attribute]\nstruct Route {\n\tpath string\n}\n\n@[Route(provider.path)]\nfn foo() {}\n\nfn main() {}\n') or {
		panic(err)
	}
	ok := attribute_const_check(root)
	assert ok.exit_code == 0, ok.output
}

fn test_attribute_const_import_alias_resolves() {
	root := attribute_const_resolution_root('alias')
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'provider')) or { panic(err) }
	os.write_file(os.join_path(root, 'provider', 'provider.v'), "module provider\n\npub const path = '/public'\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main\n\nimport provider as aliasp\n\n@[attribute]\nstruct Route {\n\tpath string\n}\n\n@[Route(aliasp.path)]\nfn foo() {}\n\nfn main() {}\n') or {
		panic(err)
	}
	result := attribute_const_check(root)
	assert result.exit_code == 0, result.output
	// Typed comptime reflection sees the aliased literal.
	os.write_file(os.join_path(root, 'main.v'), 'module main\n\nimport provider as aliasp\n\n@[attribute]\nstruct Route {\n\tpath string\n}\n\n@[Route(aliasp.path)]\nfn foo() {}\n\nfn main() {\n\t\$for attr in foo.attributes {\n\t\t\$if attr is Route {\n\t\t\tprintln(attr.path)\n\t\t}\n\t}\n}\n') or {
		panic(err)
	}
	run := os.exec([@VEXE, 'run', root])
	assert run.exit_code == 0, run.output
	assert run.output.contains('/public'), run.output
}

fn test_attribute_const_enum_nominal_identity() {
	root := attribute_const_resolution_root('enum')
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'one')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'two')) or { panic(err) }
	os.write_file(os.join_path(root, 'one', 'one.v'), 'module one\n\npub enum Mode {\n\tfirst\n\tsecond\n}\n\npub const chosen = Mode.first\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'two', 'two.v'), 'module two\n\npub enum Mode {\n\tfirst\n\tsecond\n}\n\npub const selected = Mode.first\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main\n\nimport one\nimport two\n\n@[attribute]\nstruct Tag {\n\tmode one.Mode\n}\n\n@[Tag(two.selected)]\nfn foo() {}\n\nfn main() {}\n') or {
		panic(err)
	}
	result := attribute_const_check(root)
	assert result.exit_code != 0, result.output
	assert result.output.contains('is not a value of enum'), result.output
	// Same-module nominal enum passes.
	os.write_file(os.join_path(root, 'main.v'), 'module main\n\nimport one\n\n@[attribute]\nstruct Tag {\n\tmode one.Mode\n}\n\n@[Tag(one.chosen)]\nfn foo() {}\n\nfn main() {}\n') or {
		panic(err)
	}
	ok := attribute_const_check(root)
	assert ok.exit_code == 0, ok.output
}
