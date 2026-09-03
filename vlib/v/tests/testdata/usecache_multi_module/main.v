module main

import mymod

fn main() {
	// sumtype value tagged inside the cached module, `is`-checked in the program TU
	s := mymod.make_circle(3)
	assert s is mymod.Circle
	// `is`-check inside the cached module on a program-TU-held value
	assert mymod.classify(s) == 'circle'
	// interface built in the cached module, dispatched from both TUs
	sp := mymod.make_speaker()
	assert sp.speak() == 'woof'
	assert mymod.greet(sp) == 'woof'
	assert mymod.speaker_type(sp).len > 0
	// closures created in the cached module, called from both TUs
	g := mymod.make_getter(42)
	assert g() == 42
	assert mymod.call_getter_here(7) == 7
	// module consts + array growth (max_int const) inside the cached module
	assert mymod.push_many() == 10
	assert mymod.big_cap == 1000
	// float formatting reads strconv's const tables inside the cached builtin object
	assert '${2.5 + 0.25}' == '2.75'
	assert 12345.678.str() == '12345.678'
	println('OK')
}
