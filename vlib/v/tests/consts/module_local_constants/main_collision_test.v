module main

struct Record {
	answer string
}

const main = Record{ answer: 'field' }
const answer = 42

fn test_main_module_constant_wins_over_matching_const_field() {
	assert main.answer == 42
	assert answer == 42
}
