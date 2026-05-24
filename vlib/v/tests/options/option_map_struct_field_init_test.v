struct Config {
	values ?map[string]int
}

fn test_option_map_struct_field_init_with_literal() {
	cfg := Config{
		values: {
			'one': 1
			'two': 2
		}
	}
	assert cfg.values?['one']! == 1
	assert cfg.values?['two']! == 2
}
