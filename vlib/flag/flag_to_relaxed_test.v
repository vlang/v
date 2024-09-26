import flag

const one_ok_gnu_arg_no_tail = ['-flip', '--g', '/path/to', '--mix', '-ver']
const two_ok_gnu_arg_no_tail = ['-flip', '--g', '--sound=blop', '/path/to', '--mix', '-ver']

const one_ok_gnu_arg_tail = ['-flip', '--g', '/path/to', '--mix', '-ver', 'tail']
const two_ok_gnu_arg_tail = ['-flip', '--g', '--sound=blop', '/path/to', '--mix', '-ver', 'tail']

struct Config {
	mix   bool
	sound string
	beep  bool
	path  string @[tail]
}

fn test_flag_relaxed_mode() {
	// Test `mode: .relaxed`
	config1, no_matches1 := flag.to_struct[Config](one_ok_gnu_arg_no_tail, mode: .relaxed)!
	assert config1.mix == true
	assert config1.sound == ''
	assert config1.beep == false
	assert config1.path == ''
	assert no_matches1 == ['-flip', '--g', '/path/to', '-ver']

	config2, no_matches2 := flag.to_struct[Config](two_ok_gnu_arg_no_tail, mode: .relaxed)!
	assert config2.mix == true
	assert config2.sound == 'blop'
	assert config2.beep == false
	assert config2.path == ''
	assert no_matches2 == ['-flip', '--g', '/path/to', '-ver']

	config3, no_matches3 := flag.to_struct[Config](one_ok_gnu_arg_tail, mode: .relaxed)!
	assert config3.mix == true
	assert config3.sound == ''
	assert config3.beep == false
	assert config3.path == 'tail'
	assert no_matches3 == ['-flip', '--g', '/path/to', '-ver']

	config4, no_matches4 := flag.to_struct[Config](two_ok_gnu_arg_tail, mode: .relaxed)!
	assert config4.mix == true
	assert config4.sound == 'blop'
	assert config4.beep == false
	assert config4.path == 'tail'
	assert no_matches4 == ['-flip', '--g', '/path/to', '-ver']
}
