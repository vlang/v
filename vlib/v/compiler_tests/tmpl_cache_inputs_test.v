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

fn (p TemplateProject) build() TemplateBuild {
	built := os.exec(['env', 'V3CACHE=${p.cache}', 'V3_CACHE_TRACE=1', @VEXE, '-cc', 'cc', '-gc',
		'none', '-o', p.output, p.main])
	assert built.exit_code == 0, built.output
	run := os.exec([p.output])
	assert run.exit_code == 0, run.output
	return TemplateBuild{
		trace:  built.output
		output: run.output
	}
}

fn test_a_changed_template_is_not_served_from_the_executable_cache() {
	p := new_template_project('root')
	defer { os.rmdir_all(p.root) or {} }
	p.write('main.v', page_program)
	p.write('page.txt', 'first\n')
	assert p.build().output == 'first\n'
	unchanged := p.build()
	assert unchanged.trace.contains(restored_executable), unchanged.trace
	assert unchanged.output == 'first\n'

	p.write('page.txt', 'changed\n')
	changed := p.build()
	assert !changed.trace.contains(restored_executable), changed.trace
	assert changed.output == 'changed\n'
	again := p.build()
	assert again.trace.contains(restored_executable), again.trace
	assert again.output == 'changed\n'
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
	assert !changed.trace.contains(restored_executable), changed.trace
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
	// The module's body is not parsed from here on. Its template must keep the identity
	// that the build which did parse it gave to the executable.
	unchanged := p.build()
	assert unchanged.trace.contains(restored_executable), unchanged.trace
	assert unchanged.output == 'first\n'

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
