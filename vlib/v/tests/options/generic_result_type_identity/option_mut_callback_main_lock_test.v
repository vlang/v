import multi_return_models

struct Config {
	step int
}

fn update_point(mut point ?multi_return_models.CallbackPoint, config Config) {
	if mut value := point {
		value.x += config.step
	}
}

fn test_optional_callback_preserves_mut_option_parameter_with_main_type() {
	callbacks := multi_return_models.Callbacks[Config]{ callback: update_point }
	mut point := ?multi_return_models.CallbackPoint(multi_return_models.CallbackPoint{ x: 1 })
	callbacks.apply(mut point, Config{ step: 2 })
	assert (point or { panic('missing point') }).x == 3
	mut plain := multi_return_models.CallbackPoint{ x: 4 }
	callbacks.apply(mut plain, Config{ step: 5 })
	assert plain.x == 9
}
