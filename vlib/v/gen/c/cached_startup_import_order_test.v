module c

import os

fn startup_cache_source(imports []string, message string) string {
	return 'module main\n' + imports.map('import ' + it).join('\n') + '\nfn main() {\n\tinitfirst.marker()\n\tinitsecond.marker()\n\tprintln("${message}")\n}\n'
}

fn startup_cache_build(root string, cache string, name string, flags []string) string {
	mut args := ['env', 'V3CACHE=' + cache, @VEXE, '-cc', 'cc', '-show-timings']
	args << flags
	args << ['-o', os.join_path(root, name), os.join_path(root, 'main.v')]
	compiled := os.exec(args)
	assert compiled.exit_code == 0, compiled.output
	return compiled.output
}

fn startup_cache_output(root string, name string) string {
	result := os.exec([os.join_path(root, name)])
	assert result.exit_code == 0, result.output
	return result.output.trim_space()
}

fn test_cached_module_initializers_follow_reordered_imports_and_uncached_cleanup() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_startup_import_order_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for name in ['initfirst', 'initsecond'] {
		os.mkdir_all(os.join_path(root, name))!
		word := name.all_after('init')
		os.write_file(os.join_path(root, name, name + '.v'), 'module ${name}\nfn init() { println("${word}") }\nfn cleanup() { println("cleanup ${word}") }\npub fn marker() {}\n')!
	}
	main := os.join_path(root, 'main.v')
	cache := os.join_path(root, 'cache')
	os.write_file(main, startup_cache_source(['initfirst', 'initsecond'], 'cold'))!
	startup_cache_build(root, cache, 'cold', [])
	assert startup_cache_output(root, 'cold') == 'first\nsecond\ncold\ncleanup second\ncleanup first'
	os.write_file(main, startup_cache_source(['initfirst', 'initsecond'], 'done'))!
	incremental := startup_cache_build(root, cache, 'warm', [])
	assert incremental.contains('cgen (incremental)'), incremental
	assert startup_cache_output(root, 'warm') == 'first\nsecond\ndone\ncleanup second\ncleanup first'
	os.write_file(main, startup_cache_source(['initsecond', 'initfirst'], 'done'))!
	startup_cache_build(root, cache, 'uncached', ['-nocache'])
	expected := startup_cache_output(root, 'uncached')
	assert expected == 'second\nfirst\ndone\ncleanup first\ncleanup second'
	startup_cache_build(root, cache, 'reordered', [])
	assert startup_cache_output(root, 'reordered') == expected
	startup_cache_build(root, os.join_path(root, 'fresh-cache'), 'fresh', [])
	assert startup_cache_output(root, 'fresh') == expected
}

fn test_wrapper_dependencies_outlive_cleanup_in_cached_and_uncached_builds() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_startup_wrapper_cleanup_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for name in ['initfirst', 'initsecond', 'wrapper', 'library'] {
		os.mkdir_all(os.join_path(root, name))!
	}
	os.write_file(os.join_path(root, 'initfirst', 'initfirst.v'), 'module initfirst\nimport os\nfn init() { println("first") }\nfn cleanup() { assert os.getenv("V_STARTUP_WRAPPER_DEPENDENCY") == "alive"; println("cleanup first") }\npub fn marker() {}\n')!
	os.write_file(os.join_path(root, 'initsecond', 'initsecond.v'), 'module initsecond\nimport os\nfn init() { os.setenv("V_STARTUP_WRAPPER_DEPENDENCY", "alive", true); println("second") }\nfn cleanup() { os.unsetenv("V_STARTUP_WRAPPER_DEPENDENCY"); println("cleanup second") }\npub fn marker() {}\n')!
	os.write_file(os.join_path(root, 'wrapper', 'wrapper.v'), 'module wrapper\nimport initsecond\npub fn marker() { initsecond.marker() }\n')!
	library := os.join_path(root, 'library', 'library.v')
	main := os.join_path(root, 'main.v')
	cache := os.join_path(root, 'cache')
	os.write_file(library, 'module library\nimport wrapper\nimport initfirst\nimport initsecond\npub fn marker() { wrapper.marker(); initfirst.marker(); initsecond.marker() }\n')!
	os.write_file(main, 'import library\nfn main() { library.marker(); println("cold") }\n')!
	startup_cache_build(root, cache, 'cold', [])
	assert startup_cache_output(root, 'cold') == 'second\nfirst\ncold\ncleanup first\ncleanup second'
	os.write_file(main, 'import library\nfn main() { library.marker(); println("done") }\n')!
	incremental := startup_cache_build(root, cache, 'warm', [])
	assert incremental.contains('cgen (incremental)'), incremental
	expected := 'second\nfirst\ndone\ncleanup first\ncleanup second'
	assert startup_cache_output(root, 'warm') == expected
	// The wrapper still visits initsecond first when independent imports are reordered.
	os.write_file(library, 'module library\nimport wrapper\nimport initsecond\nimport initfirst\npub fn marker() { wrapper.marker(); initfirst.marker(); initsecond.marker() }\n')!
	startup_cache_build(root, cache, 'uncached', ['-nocache'])
	assert startup_cache_output(root, 'uncached') == expected
	startup_cache_build(root, cache, 'reordered', [])
	assert startup_cache_output(root, 'reordered') == expected
	startup_cache_build(root, os.join_path(root, 'fresh-cache'), 'fresh', [])
	assert startup_cache_output(root, 'fresh') == expected
}
