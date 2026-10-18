import v.vmod

const mf_no_deps = "Module {
	name: 'V'
	description: 'The V programming language.'
	version: '0.4.10'
	license: 'MIT'
	repo_url: 'https://github.com/vlang/v'
	dependencies: []
}"

fn test_encode_vmod_with_no_deps() {
	mf := vmod.decode(mf_no_deps)!
	assert vmod.encode(mf) == mf_no_deps
}

const mf_with_deps_1 = "Module {
	name: 'V'
	description: 'The V programming language.'
	version: '0.4.10'
	license: 'MIT'
	repo_url: 'https://github.com/vlang/v'
	dependencies: ['hello', 'world']
}"

const mf_with_deps_2 = 'Module {
	name: \'V\'
	description: "The V\' programming language."
	version: \'0.4.10\'
	license: \'MIT\'
	repo_url: \'https://github.com/vlang/v\'
	dependencies: [
		\'hello\',
		\'world\',
		\'fdsfgarhrhregwegewgeage\',
		\'geageaegeggegagaghjuktktytrrtrehitroorekfwepokooe\',
	]
}'

fn test_encode_vmod_with_multiple_deps() {
	mf1 := vmod.decode(mf_with_deps_1)!
	assert vmod.encode(mf1) == mf_with_deps_1
	mf2 := vmod.decode(mf_with_deps_2)!
	assert vmod.encode(mf2) == mf_with_deps_2
}

const mf_with_extra_fields = "Module {
	name: 'V'
	base_url: 'src'
	description: 'The V programming language.'
	version: '0.4.10'
	license: 'MIT'
	repo_url: 'https://github.com/vlang/v'
	author: 'Bilbo Baggins'
	dependencies: ['hello', 'world']
	extra_a: ['a', 'b', 'c']
	extra_b: [
		'aaaaaaaaaaaaaaaaaaaaa',
		'bbbbbbbbbbbbbbbbbbbbb',
		'ccccccccccccccccccccc',
	]
}"

fn test_encode_vmod_with_extra_fields() {
	mf := vmod.decode(mf_with_extra_fields)!
	assert vmod.encode(mf) == mf_with_extra_fields
}

fn test_encode_preserves_catalog_and_workspaces_in_stable_key_order() {
	manifest := vmod.decode("Module { name: 'app' dependencies: ['foo@^1'] catalog: { zed: '^2', 'publisher.foo': '^1' } workspaces: ['packages/*'] }")!
	encoded := vmod.encode(manifest)
	decoded := vmod.decode(encoded)!
	assert decoded.catalog == manifest.catalog
	assert decoded.workspaces == manifest.workspaces
	assert decoded.dependencies == manifest.dependencies
	publisher_index := encoded.index("'publisher.foo':") or { panic('expected catalog key in encoded manifest') }
	zed_index := encoded.index("'zed':") or { panic('expected catalog key in encoded manifest') }
	assert publisher_index < zed_index
}

fn test_encode_preserves_repo_branch() {
	manifest := vmod.decode("Module { name: 'app' repo_url: 'https://example.com/o/p' repo_branch: 'main' }")!
	encoded := vmod.encode(manifest)
	assert encoded.contains("repo_branch: 'main'")
	assert vmod.decode(encoded)!.repo_branch == 'main'

	// The parser default must not be written back out, so that a manifest
	// without the field keeps its current text.
	defaulted := vmod.decode("Module { name: 'app' }")!
	assert !vmod.encode(defaulted).contains('repo_branch')
}

fn test_encode_escapes_values_holding_both_quote_kinds() {
	for value in ['it\'s a "quoted" name', 'plain', "has ' only", 'has " only'] {
		mut manifest := vmod.decode("Module { name: 'app' }")!
		manifest.description = value
		decoded := vmod.decode(vmod.encode(manifest)) or {
			panic('encoded manifest did not parse: ${err.msg()}\n${vmod.encode(manifest)}')
		}
		assert decoded.description == value
	}
}
