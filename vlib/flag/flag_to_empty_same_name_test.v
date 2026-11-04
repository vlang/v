import flag

struct FlagConfig {}

fn test_empty_flag_config_keeps_its_identity_in_result_tuple() {
	config, rest := flag.to_struct[FlagConfig](['app', 'extra'], skip: 1, style: .go_flag) or {
		panic(err)
	}
	assert config == FlagConfig{}
	assert rest == ['extra']
}
