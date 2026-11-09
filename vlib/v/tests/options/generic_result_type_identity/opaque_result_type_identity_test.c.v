import multi_return_models

fn test_header_only_native_payload_keeps_option_and_result_wrappers() {
	optional := multi_return_models.optional_handle(true) or { panic('missing handle') }
	assert multi_return_models.handle_value(optional) == 7
	assert multi_return_models.optional_handle(false) == none
	result := multi_return_models.result_handle()!
	assert multi_return_models.handle_value(result) == 7
}
