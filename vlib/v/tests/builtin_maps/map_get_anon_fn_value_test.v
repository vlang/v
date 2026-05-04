@[strict_map_index]
module main

const numbers = {
	'one': fn () int {
		return 1
	}
}

struct Tree {
	value string
}

type TreeBelt = map[string]fn (input string) []string

fn test_map_get_anon_fn_value() {
	num1 := numbers['one'] or {
		fn () int {
			return 2
		}
	}
	ret1 := num1()
	println(ret1)
	assert ret1 == 1

	num2 := numbers['two'] or {
		fn () int {
			return 2
		}
	}
	ret2 := num2()
	println(ret2)
	assert ret2 == 2
}

fn test_map_get_anon_fn_value_if_guard() {
	values := {
		'one': 1
	}
	mut found := false
	if value := values['one'] {
		found = true
		assert value == 1
	}
	assert found
	mut missing_found := false
	if _ := values['two'] {
		missing_found = true
	}
	assert !missing_found
}

fn test_cast_map_literal_with_closure_value() {
	tree := Tree{
		value: 'he he'
	}

	belt := TreeBelt({
		'bar': fn [tree] (input string) []string {
			return [tree.value + input]
		}
	})

	mut belt2 := TreeBelt(map[string]fn (string) []string{})
	belt2['bar'] = fn [tree] (input string) []string {
		return [tree.value + input]
	}

	bar := belt['bar'] or { panic('missing `bar` in belt') }
	bar2 := belt2['bar'] or { panic('missing `bar` in belt2') }
	assert bar('foo') == ['he hefoo']
	assert bar2('fo') == ['he hefo']
}
