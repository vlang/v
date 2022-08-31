import v.vmod

const quote = '\x22'

const apos = '\x27'

fn test_ok() ? {
	ok_source := "Module {
	name: 'V'
	description: 'The V programming language.'
	version: '0.3.1'
	license: 'MIT'
	repo_url: 'https://github.com/vlang/v'
	dependencies: []
}"
	for s in [ok_source, ok_source.replace(apos, quote), ok_source.replace('\n', '\r\n'),
		ok_source.replace('\n', '\r\n '), ok_source.replace('\n', '\n ')] {
		content := vmod.decode(s)?
		assert content.name == 'V'
		assert content.description == 'The V programming language.'
		assert content.version == '0.3.1'
		assert content.license == 'MIT'
		assert content.repo_url == 'https://github.com/vlang/v'
		assert content.dependencies == []
		assert content.unknown == {}
	}
	e := vmod.decode('Module{}')?
	assert e.name == ''
	assert e.description == ''
	assert e.version == ''
	assert e.license == ''
	assert e.repo_url == ''
	assert e.dependencies == []
	assert e.unknown == {}
}

fn test_invalid_start() ? {
	vmod.decode('\n\nXYZ') or {
		assert err.msg() == 'vmod: v.mod files should start with Module, at line 3'
		return
	}
	assert false
}

fn test_invalid_end() ? {
	vmod.decode('\nModule{\n \nname: ${quote}zzzz}') or {
		assert err.msg() == 'vmod: invalid token ${quote}eof$quote, at line 4'
		return
	}
	assert false
}
