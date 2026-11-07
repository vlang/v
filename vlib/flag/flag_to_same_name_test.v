import flag

struct FlagConfig {
	label string = 'initial'
}

fn test_to_struct_keeps_callers_flag_config_type() {
	config, rest := flag.to_struct[FlagConfig](['app', '-label=parsed', 'tail'],
		skip:  1
		style: .go_flag
	) or { panic(err) }
	assert config.label == 'parsed'
	assert rest == ['tail']
}

fn test_using_keeps_callers_flag_config_type() {
	config, rest := flag.using[FlagConfig](FlagConfig{ label: 'default' }, ['app'],
		skip:  1
		style: .go_flag
	)!
	assert config.label == 'default'
	assert rest.len == 0
	library_config, _ := flag.using[flag.FlagConfig](flag.FlagConfig{ val_desc: 'library' },
		['app'],
		skip:  1
		style: .go_flag
	)!
	assert library_config.val_desc == 'library'
}
