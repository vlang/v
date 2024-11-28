import os
import rand

const my_random_letter_const = u8(65) + (rand.u8() % 26)

fn test_rand_is_initialized_before_main() {
	eprintln('random letter: ${my_random_letter_const.str()} | ASCII code: ${my_random_letter_const}')
	assert my_random_letter_const.is_capital()
}

//

const last_constant = fn_that_calls_a_method_on_a_constant()
const a_constant = os.join_path(@VROOT, 'a')

fn fn_that_calls_a_method_on_a_constant() string {
	return a_constant.replace('\\', '/')
}

fn test_consts_initialised_with_a_function_that_uses_other_consts_as_receivers_are_properly_ordered() {
	assert last_constant != ''
}
