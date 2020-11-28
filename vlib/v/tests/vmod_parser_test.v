import vmod
import os

fn test_from_file() {
	os.chdir(os.dir(os.getenv('VEXE')))
	data := vmod.from_file('./v.mod') or {
		panic(err)
	}
	assert data.name == 'V'
	assert data.description == 'The V programming language.'
	assert data.version == '0.1.30'
	assert data.dependencies.len == 0
}

fn test_decode() {
	content := "
	  Module {
		name: \'foobar\',
		description: \'Just a sample module\'
		version: \'0.2.0\',
		repo_url: \'https://gitlab.com\',
		author: \'Fooz Bar\',
		license: \'GPL-2.0\',
		dependencies: [\'hello\'],
		test: \'foo\'
	  }
	"
	data := vmod.decode(content) or {
		println(err)
		exit(1)
	}
	assert data.name == 'foobar'
	assert data.version == '0.2.0'
	assert data.description == 'Just a sample module'
	assert data.repo_url == 'https://gitlab.com'
	assert data.author == 'Fooz Bar'
	assert data.license == 'GPL-2.0'
	assert data.dependencies[0] == 'hello'
	assert data.unknown['test'][0] == 'foo'
	vmod.decode('') or {
		assert err == 'vmod: no content.'
		exit(0)
	}
}
