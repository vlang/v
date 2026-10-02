import os
import v.vmod

const quote = '\x22'

const apos = '\x27'

fn test_ok() {
	ok_source := "Module {
	name: 'V'
	description: 'The V programming language.'
	version: '0.7.7'
	license: 'MIT'
	repo_url: 'https://github.com/vlang/v'
	dependencies: []
}"
	for s in [ok_source, ok_source.replace(apos, quote), ok_source.replace('\n', '\r\n'),
		ok_source.replace('\n', '\r\n '), ok_source.replace('\n', '\n ')] {
		content := vmod.decode(s)!
		assert content.name == 'V'
		assert content.base_url == ''
		assert content.description == 'The V programming language.'
		assert content.version == '0.7.7'
		assert content.license == 'MIT'
		assert content.repo_url == 'https://github.com/vlang/v'
		assert content.dependencies == []
		assert content.unknown == {}
	}
	e := vmod.decode('Module{}')!
	assert e.name == ''
	assert e.base_url == ''
	assert e.description == ''
	assert e.version == ''
	assert e.license == ''
	assert e.repo_url == ''
	assert e.dependencies == []
	assert e.unknown == {}
}

fn test_invalid_start() {
	vmod.decode('\n\nXYZ') or {
		assert err.msg() == 'vmod: v.mod files should start with Module, at line 3'
		return
	}
	assert false
}

fn test_base_url() {
	content := vmod.decode("Module {\n\tname: 'V'\n\tbase_url: 'source'\n}")!
	assert content.base_url == 'source'
}

fn test_legacy_dependencies() {
	for name in ['ui', "'ui'", '"ui"'] {
		for version in ['0.1', '1', '0.1.2', "'0.1'", '"0.1"', "'^0.1.2'"] {
			for separator in [':', ': ', ' : ', '\n:\n'] {
				for trailing_comma in ['', ','] {
					source := "Module {\n name: 'x'\n dependencies: [${name}${separator}${version}${trailing_comma}]\n base_url: 'src'\n}"
					content := vmod.decode(source)!
					assert content.name == 'x'
					assert content.base_url == 'src'
					assert content.dependencies == ['ui'], source
				}
			}
		}
	}
}

fn test_compact_legacy_dependencies() {
	content := vmod.decode("Module{name:'x'\n dependencies:[ui:0.1,'gg':'0.2.0']\n base_url:'src'}")!
	assert content.name == 'x'
	assert content.base_url == 'src'
	assert content.dependencies == ['ui', 'gg']
}

fn test_dependency_names() {
	for dependencies in ['[]', '[ui]', '[ui,]', "['ui']", '["ui"]'] {
		content := vmod.decode('Module { dependencies: ${dependencies} }')!
		assert content.dependencies == if dependencies == '[]' { []string{} } else { ['ui'] }
	}
	content := vmod.decode("Module {
		name: 'x'
		dependencies: [
			ui: 0.1,
			'gg': '0.2.0',
			nedpals.args: 0.3.0,
			lib2,
			'other',
		]
		license: 'MIT'
		subdirs: ['internal']
	}")!
	assert content.dependencies == ['ui', 'gg', 'nedpals.args', 'lib2', 'other']
	assert content.license == 'MIT'
	assert content.unknown['subdirs'] == ['internal']
	assert vmod.decode(vmod.encode(content))!.dependencies == content.dependencies
}

fn test_legacy_dependencies_from_file() {
	test_dir := os.join_path(os.vtmp_dir(), '${@FN}_${os.getpid()}')
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	path := os.join_path(test_dir, 'v.mod')
	os.write_file(path, "Module{\n\tname: 'x'\n\tdependencies: [\n\t\tui: 0.1,\n\t]\n}\n")!
	content := vmod.from_file(path)!
	assert content.name == 'x'
	assert content.dependencies == ['ui']
	assert content.source_root(test_dir) == test_dir
}

fn test_invalid_dependencies() {
	for dependencies in ['[ui:]', '[ui:,]', "['ui':]", '[ui: 0.1 gg: 0.2]', '[ui: : 0.1]', '[ui: []]',
		'[ui: {}]', '[0.1]', "['ui' 'gg']", '[ui: 0.1', '[ui:'] {
		vmod.decode('Module { dependencies: ${dependencies} }') or { continue }
		assert false, dependencies
	}
}

fn test_unknown_arrays_require_strings() {
	for values in ['[ui]', '[ui: 0.1]', "['ui': '0.1']", '[0.1]'] {
		vmod.decode('Module { subdirs: ${values} }') or { continue }
		assert false, values
	}
}

fn test_invalid_end() {
	vmod.decode('\nModule{\n \nname: ${quote}zzzz}') or {
		assert err.msg() == 'vmod: invalid token ${quote}eof${quote}, at line 4'
		return
	}
	assert false
}
