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

struct RenamedTextConfig {
	text string @[json: 'renamed_key']
}

fn test_autofree_retains_renamed_keys_for_escaped_names_and_buffer_reuse() {
	mut buffer := json2.DecodeBuffer{}
	retained := json2.decode_reuse[RenamedTextConfig]('{"renamed_key":"first"}', mut buffer)!
	for _ in 0 .. 20 {
		assert json2.decode[RenamedTextConfig](r'{"renamed_\u006bey":"second"}')!.text == 'second'
		assert json2.decode_reuse[RenamedTextConfig](r'{"renamed_\u006bey":"\n"}', mut buffer)!.text == '\n'
		assert json2.decode_reuse[RenamedTextConfig]('{"renamed_key":"plain"}', mut buffer)!.text == 'plain'
	}
	assert retained.text == 'first'
}
