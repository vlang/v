module main

import term

fn abc() {
	println('xyz')
}

fn def() string {
	return 'xyz'
}

const a_const_that_is_fn = abc

const a_const_that_is_fn_returning_value = def

const a_const_same_as_fn_in_another_module = term.yellow

fn test_simple_fn_assigned_to_const_can_be_called() {
	a_const_that_is_fn()
	assert true
}

fn test_simple_fn_assigned_to_const_can_be_called_and_returns_value() {
	assert def == a_const_that_is_fn_returning_value
	assert def() == 'xyz'
	assert a_const_that_is_fn_returning_value() == 'xyz'
	assert def() == a_const_that_is_fn_returning_value()
	assert a_const_that_is_fn_returning_value() == def()
}

//

fn test_a_const_that_is_alias_to_fn_from_module() {
	assert a_const_same_as_fn_in_another_module == term.yellow
	assert term.yellow('x') == a_const_same_as_fn_in_another_module('x')
	assert ptr_str(term.yellow) == ptr_str(a_const_same_as_fn_in_another_module)
}

//

const pg = fn_generator()

const pg2 = main.fn_generator()()

fn fn_generator() fn () string {
	return fn () string {
		println('hello')
		return 'ok'
	}
}

fn test_a_const_can_be_assigned_a_fn_produced_by_a_fn_generator_and_the_const_can_be_used() {
	assert main.fn_generator()() == 'ok'

	x := fn_generator()
	assert x() == 'ok'

	y := pg
	assert y() == 'ok'

	assert pg() == 'ok'

	assert pg2 == 'ok'
}
