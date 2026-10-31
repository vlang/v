// vtest build: !windows
import os

fn run_agents_create(args []string) os.Result {
	mut command := [@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', 'clang', '-gc', 'none']
	command << args
	return os.exec(['sh', '-c', command.map(os.quoted_path(it)).join(' ') + ' < /dev/null'])
}

fn test_agents_generation_is_opt_in_and_matches_each_template() {
	root := os.join_path(os.vtmp_dir(), 'vcreate_agents_templates_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	old_wd := os.getwd()
	os.chdir(root) or { panic(err) }
	defer {
		os.chdir(old_wd) or {}
	}
	default_result := run_agents_create(['new', 'without_agents'])
	assert default_result.exit_code == 0, default_result.output
	assert !os.exists(os.join_path(root, 'without_agents', 'AGENTS.md'))
	for template in ['bin', 'lib', 'web'] {
		name := 'agents_${template}'
		result := run_agents_create(['new', '--${template}', '--agents-md', name])
		assert result.exit_code == 0, result.output
		path := os.join_path(root, name, 'AGENTS.md')
		content := os.read_file(path) or { panic(err) }
		assert content.contains('# AGENTS.md - ${name}')
		assert content.contains('v test .')
		if template == 'lib' {
			assert !content.contains('v run .')
			assert content.contains('`${name}.v` is the module root')
		} else {
			assert content.contains('v run .')
		}
		checked := run_agents_create(['check-md', path])
		assert checked.exit_code == 0, checked.output
		if template == 'lib' {
			os.chdir(os.join_path(root, name)) or { panic(err) }
			tested := run_agents_create(['test', '.'])
			assert tested.exit_code == 0, tested.output
			os.chdir(root) or { panic(err) }
		}
	}
}

fn test_agents_init_preserves_existing_files_and_dangling_links() {
	root := os.join_path(os.vtmp_dir(), 'vcreate_agents_preservation_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	old_wd := os.getwd()
	os.chdir(root) or { panic(err) }
	defer {
		os.chdir(old_wd) or {}
	}
	contract := '# Keep this contributor contract\n'
	os.write_file('AGENTS.md', contract) or { panic(err) }
	result := run_agents_create(['init', '--agents-md'])
	assert result.exit_code == 0, result.output
	assert os.read_file('AGENTS.md') or { panic(err) } == contract
	os.rm('AGENTS.md') or { panic(err) }
	target := os.join_path(root, 'missing_contract.md')
	os.symlink(target, 'AGENTS.md') or { panic(err) }
	linked := run_agents_create(['init', '--agents-md'])
	assert linked.exit_code == 0, linked.output
	assert os.is_link('AGENTS.md')
	assert os.readlink('AGENTS.md') or { panic(err) } == target
	assert !os.exists(target)
}

fn test_agents_init_describes_existing_library_sources() {
	root := os.join_path(os.vtmp_dir(), 'vcreate_agents_existing_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	old_wd := os.getwd()
	os.chdir(root) or { panic(err) }
	defer {
		os.chdir(old_wd) or {}
	}
	manifest := "Module { name: 'stored_name' }\n"
	source := 'module stored_name\n\npub fn value() int { return 7 }\n'
	os.write_file('v.mod', manifest) or { panic(err) }
	os.write_file('custom_api.v', source) or { panic(err) }
	result := run_agents_create(['init', '--lib', '--agents-md'])
	assert result.exit_code == 0, result.output
	content := os.read_file('AGENTS.md') or { panic(err) }
	assert content.contains('# AGENTS.md - stored_name')
	assert content.contains('Library V sources define the public API.')
	assert !content.contains('`stored_name.v`')
	assert !content.contains('v run .')
	assert !os.exists('stored_name.v')
	assert os.read_file('v.mod') or { panic(err) } == manifest
	assert os.read_file('custom_api.v') or { panic(err) } == source
}
