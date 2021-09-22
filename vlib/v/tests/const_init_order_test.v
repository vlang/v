import rand

const (
	my_random_letter_const = byte(65 + rand.u32n(25))
)

fn test_rand_is_initialized_before_main() {
	eprintln('random letter: $my_random_letter_const.str() | ASCII code: $my_random_letter_const')
	assert my_random_letter_const.is_capital()
}
