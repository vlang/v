// vtest build: !sanitize-memory-gcc && !sanitize-address-gcc && !sanitize-address-clang
// vtest vflags: -autofree
import json2

struct Config {
	bbb bool
}

struct RenamedConfig {
	renamed bool @[json: 'renamed_key']
}

fn test_compilation_with_autofree() {
	cfg := Config{}
	s := json2.encode(cfg, prettify: true)
	assert s == '{\n    "bbb": false\n}'
}

fn test_autofree_preserves_json_renamed_key() {
	assert json2.encode(RenamedConfig{}) == '{"renamed_key":false}'
	assert json2.encode(RenamedConfig{}) == '{"renamed_key":false}'
	assert json2.decode[RenamedConfig]('{"renamed_key":true}')!.renamed
	assert json2.decode[RenamedConfig]('{"renamed_key":true}')!.renamed
}
