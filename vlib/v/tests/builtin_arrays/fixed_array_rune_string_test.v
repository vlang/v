fn test_fixed_array_rune_string() {
	bytes := [72, 101, 108, 108, 111]!
	assert bytes.map(|h| rune(h)).string() == 'Hello'
	assert bytes.map(rune(it)).string() == 'Hello'

	values := [`H`, `é`, `世`, `🌎`]!
	assert values.string() == 'Hé世🌎'
	assert values.map(it).string() == 'Hé世🌎'
	assert values.map(it).filter(it != `H`).string() == 'é世🌎'
	assert values.map(it).reverse().string() == '🌎世éH'
	assert values.map(it).str() == '[`H`, `é`, `世`, `🌎`]'
	assert values.string() == 'Hé世🌎'

	dynamic := [`H`, `é`, `世`, `🌎`]
	assert dynamic.string() == 'Hé世🌎'
	assert dynamic.map(it).string() == 'Hé世🌎'
}

fn test_empty_fixed_array_rune_string() {
	values := [`a`]!
	assert values[..0].string() == ''
	assert values.map(it).filter(it != `a`).string() == ''
	assert []rune{}.string() == ''
}

struct RuneSource {
mut:
	calls int
}

fn (mut source RuneSource) values() [2]int {
	source.calls++
	return [0xe9, 0x1f30e]!
}

fn test_fixed_array_rune_string_evaluates_receiver_once() {
	mut source := RuneSource{}
	assert source.values().map(rune(it)).string() == 'é🌎'
	assert source.calls == 1
}

type RunePair = [2]rune

fn (values RunePair) string() string {
	return 'custom'
}

fn test_fixed_rune_array_alias_preserves_user_method() {
	values := RunePair([`a`, `b`]!)
	assert values.string() == 'custom'
	assert values.map(it).string() == 'ab'
}
