import json2
import toml
import toml.to

fn test_toml_json_any_array_return_codegen() {
	value := to.json_any(toml.Any([toml.Any('one'), toml.Any('two')]))
	assert value == json2.Any([json2.Any('one'), json2.Any('two')])
}
