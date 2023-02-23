import json

struct Node {
	location NodeLocation [json: 'loc']
}

struct NodeLocation {
	source_file ?SourceFile [json: 'includedFrom']
}

struct SourceFile {
	path string [json: 'file']
}

fn test_main() {
	node := json.decode(Node, '{"loc": { "includedFrom": { "file": "/bin/foo" } } }')!

	mut source_file := node.location.source_file or { none }
	assert source_file.path == ''

	source_file = node.location.source_file or {
		SourceFile{
			path: '-'
		}
	}

	assert source_file.path == '-'
}
