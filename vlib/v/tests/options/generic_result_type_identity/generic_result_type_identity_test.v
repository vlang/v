import flag
import multi_return_models

struct FlagConfig {}

fn test_empty_caller_config_in_imported_generic_result() {
	config, rest := flag.to_struct[FlagConfig](['app', 'tail'], skip: 1, style: .go_flag) or {
		panic(err)
	}
	assert config == FlagConfig{}
	assert rest == ['tail']
	library_config, _ := flag.using[flag.FlagConfig](flag.FlagConfig{ val_desc: 'library' },
		['app'],
		skip:  1
		style: .go_flag
	)!
	assert library_config.val_desc == 'library'
}

fn test_result_wrapper_for_tuple_named_module() {
	config := multi_return_models.load_config('value')!
	assert config.value == 'value'
	assert multi_return_models.load_mode()! == .enabled
}

fn test_option_wrapper_for_tuple_named_module() {
	config := multi_return_models.optional_config('value') or { panic('missing config') }
	assert config.value == 'value'
	assert multi_return_models.optional_config('') == none
}
