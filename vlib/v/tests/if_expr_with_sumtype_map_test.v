module main

type ConfigValue = bool | int | string
type ConfigMap = map[string]ConfigValue

fn foo(conf ConfigMap) bool {
	mut bar := false
	// Check type
	bar = if conf['baz'] or { false } is bool {
		conf['baz'] or { false } as bool
	} else {
		false
	} // Default value
	return bar
}

fn test_if_expr_with_sumtype_map() {
	conf := {
		'baz': ConfigValue(123)
	}
	ret := foo(conf)
	println(ret)
	assert !ret
}
