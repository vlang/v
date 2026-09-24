import toml
import x.json2

struct TomlJson2Config {
	key string
}

// `toml.Any` has no `str` for its `[]Any` variant while `json2.Any` does; using
// both modules in one program must not route `toml.Any` through `json2.[]Any.str`.
fn test_toml_decode_and_json2_encode_in_one_program() {
	t := toml.decode[TomlJson2Config]('key = "val"')!
	assert t.key == 'val'
	assert t.str().contains("key: 'val'")
	assert json2.encode[TomlJson2Config](t) == '{"key":"val"}'
}
