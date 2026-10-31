module main

import config
import json2
import net.http as _

fn reflected_field_names[T]() []string {
	mut names := []string{}
	$for field in T.fields {
		names << field.name
	}
	return names
}

fn test_config_fields_keep_their_canonical_module_with_http_imported() {
	assert reflected_field_names[config.Cfg]() == ['name', 'n']
	cfg := json2.decode[config.Cfg]('{"name":"vails","n":3}')!
	assert cfg.name == 'vails'
	assert cfg.n == 3
	assert json2.encode(cfg).contains('"name":"vails"')
}
