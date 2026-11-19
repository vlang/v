import os

// The files that `$tmpl` reads are inputs of the program, like its sources. Each fixture
// has its own cache, changes nothing but a template and runs what the next build links.

const restored_executable = 'V3 program executable restored'

const page_program = "module main

fn page() string {
	return \$tmpl('page.txt')
}

fn main() {
	print(page())
}
"

const module_page_program = 'module main

import pages

fn main() {
	print(pages.page())
}
'

const module_page_source = "module pages

pub fn page() string {
	return \$tmpl('page.txt')
}
"

struct TemplateProject {
	root   string
	main   string
	output string
	cache  string
}

struct TemplateBuild {
	trace  string
	output string
}

fn tmpl_cache_vtmp_path() string {
	return os.join_path(os.temp_dir(), 'v3_tmpl_cache_vtmp_${os.getpid()}')
}

fn testsuite_begin() {
	vtmp := tmpl_cache_vtmp_path()
	os.rmdir_all(vtmp) or {}
	os.mkdir_all(vtmp) or { panic(err) }
	os.setenv('VTMP', vtmp, true)
}

fn testsuite_end() {
	os.rmdir_all(tmpl_cache_vtmp_path()) or {}
}

fn new_template_project(name string) TemplateProject {
	root := os.join_path(os.real_path(os.temp_dir()), 'v3_tmpl_cache_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'templates' }\n") or { panic(err) }
	return TemplateProject{
		root:   root
		main:   os.join_path(root, 'main.v')
		output: os.join_path(root, 'program')
		cache:  os.join_path(root, 'cache')
	}
}

fn (p TemplateProject) write(relative_path string, content string) {
	path := os.join_path(p.root, relative_path)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, content) or { panic(err) }
}

fn (p TemplateProject) build_with_executable_cache(use_cache bool, compiler_flags ...string) TemplateBuild {
	disabled := if use_cache { '0' } else { '1' }
	built := os.exec(['env', 'V3CACHE=${p.cache}', 'V3_CACHE_TRACE=1',
		'V3_DISABLE_UNCHANGED_BUILD_CACHE=1', 'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE=${disabled}',
		@VEXE, '-cc', 'cc', '-gc', 'none', '-d', 'no_segfault_handler', ...compiler_flags, '-o',
		p.output, p.main])
	assert built.exit_code == 0, built.output
	run := os.exec([p.output])
	assert run.exit_code == 0, run.output
	return TemplateBuild{
		trace:  built.output
		output: run.output
	}
}

fn (p TemplateProject) build() TemplateBuild {
	return p.build_with_executable_cache(false)
}

fn test_a_changed_template_rebuilds_the_program() {
	p := new_template_project('root')
	defer { os.rmdir_all(p.root) or {} }
	p.write('main.v', page_program)
	p.write('page.txt', 'first\n')
	assert p.build().output == 'first\n'

	p.write('page.txt', 'other\n')
	assert p.build().output == 'other\n'
}

fn test_template_include_preserves_path_components() {
	p := new_template_project('include_path')
	defer { os.rmdir_all(p.root) or {} }
	p.write('main.v', page_program)
	p.write('page.txt', '@include "partials.txt/header.txt.txt"\n@include "partials.html/footer"\n')
	p.write('partials.txt/header.txt.txt', 'header\n')
	p.write('partials.html/footer.html', 'footer\n')
	assert p.build().output == 'header\nfooter\n'
}

fn test_executable_cache_follows_template_inputs_when_eligible() {
	check_template_executable_cache('executable', false)
	$if linux {
		// Hosted GCC reads stdc-predef.h implicitly. The freestanding fixture
		// verifies a real executable hit separately from that bypass case.
		check_template_executable_cache('freestanding', true, '-cflags', '-ffreestanding')
	}
}

fn check_template_executable_cache(name string, require_restore bool, compiler_flags ...string) {
	p := new_template_project(name)
	defer { os.rmdir_all(p.root) or {} }
	p.write('main.v', page_program)
	native_header_bypass := 'V3 program executable not cached: native headers require C compiler processing'
	for expected in ['first\n', 'other\n'] {
		p.write('page.txt', expected)
		cold := p.build_with_executable_cache(true, ...compiler_flags)
		assert cold.output == expected
		assert !cold.trace.contains(restored_executable), cold.trace
		warm := p.build_with_executable_cache(true, ...compiler_flags)
		assert warm.output == expected
		if !require_restore && cold.trace.contains(native_header_bypass) {
			assert warm.trace.contains(native_header_bypass), warm.trace
			assert !warm.trace.contains(restored_executable), warm.trace
		} else {
			assert warm.trace.contains(restored_executable), warm.trace
		}
	}
}

fn test_a_changed_include_of_a_template_is_not_served_from_the_caches() {
	p := new_template_project('include')
	defer { os.rmdir_all(p.root) or {} }
	p.write('main.v', page_program)
	p.write('page.txt', 'page\n@include "nested.txt"\n')
	p.write('nested.txt', 'nested\n@include "leaf.txt"\n')
	p.write('leaf.txt', 'leaf\n')
	assert p.build().output == 'page\nnested\nleaf\n'

	p.write('leaf.txt', 'changed leaf\n')
	changed := p.build()
	assert changed.output == 'page\nnested\nchanged leaf\n'

	p.write('nested.txt', 'changed nested\n@include "leaf.txt"\n')
	assert p.build().output == 'page\nchanged nested\nchanged leaf\n'
}

fn test_a_changed_template_of_a_cached_module_rebuilds_its_object() {
	p := new_template_project('module')
	defer { os.rmdir_all(p.root) or {} }
	p.write('main.v', module_page_program)
	p.write('pages/pages.v', module_page_source)
	p.write('pages/page.txt', 'first\n')
	assert p.build().output == 'first\n'
	objects := os.walk_ext(p.cache, '.o').filter(os.file_name(it).starts_with('pages_'))
	assert objects.len > 0, 'the cold build did not produce a cached pages object'
	mods := objects.map(os.file_last_mod_unix(it))
	unchanged := p.build()
	assert unchanged.output == 'first\n'
	assert !unchanged.trace.contains('module=pages reason='), unchanged.trace
	assert os.walk_ext(p.cache, '.o').filter(os.file_name(it).starts_with('pages_')) == objects
	assert objects.map(os.file_last_mod_unix(it)) == mods

	p.write('pages/page.txt', 'changed\n')
	changed := p.build()
	assert changed.trace.contains('module=pages reason=object dependency changed'), changed.trace
	assert changed.output == 'changed\n'

	// With a changed program source too, only the dependencies of the module object
	// can tell that its template is another one.
	p.write('pages/page.txt', 'changed again\n')
	p.write('main.v', module_page_program + '\n// edited\n')
	assert p.build().output == 'changed again\n'
}

fn test_dump_files_lists_the_templates_of_a_program() {
	p := new_template_project('dump_files')
	defer { os.rmdir_all(p.root) or {} }
	p.write('main.v', page_program)
	p.write('page.txt', 'page\n@include "nested.txt"\n')
	p.write('nested.txt', 'nested\n')
	listing := os.join_path(p.root, 'files.txt')
	dumped := os.exec(['env', 'V3CACHE=${p.cache}', @VEXE, '-cc', 'cc', '-gc', 'none', '-dump-files',
		listing, '-o', p.output, p.main])
	assert dumped.exit_code == 0, dumped.output
	files := os.read_lines(listing) or { panic(err) }
	assert os.join_path(p.root, 'page.txt') in files, files.str()
	assert os.join_path(p.root, 'nested.txt') in files, files.str()
}
