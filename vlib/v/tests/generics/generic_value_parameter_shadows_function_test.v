import json2

struct ShadowUser {
	name string
}

type ShadowCallback = fn (int) int

fn initial(value int) int {
	return value + 1
}

fn duplicate_initial[T](initial T) T {
	mut element := initial
	return element
}

fn test_generic_value_parameter_shadows_function() {
	assert duplicate_initial(7) == 7
	assert duplicate_initial('value') == 'value'
	assert duplicate_initial(true)
	assert duplicate_initial(ShadowUser{ name: 'Ada' }).name == 'Ada'
	assert duplicate_initial([4, 5]!) == [4, 5]!
}

fn test_generic_callback_parameter_keeps_its_own_signature() {
	callback := duplicate_initial(fn (value string) string {
		return value.to_upper()
	})
	assert callback('ada') == 'ADA'
	alias := duplicate_initial(ShadowCallback(initial))
	assert alias(5) == 6
	callbacks := [initial]
	assert callbacks[0](5) == 6
}

fn test_imported_generic_parameter_shadows_main_function() {
	users := json2.decode[[]ShadowUser]('[{"name":"Ada"}]') or { panic(err) }
	assert users.len == 1
	assert users[0].name == 'Ada'
	values := json2.decode[[]int]('[2,3]') or { panic(err) }
	assert values == [2, 3]
}
