import os
import v.util.vflags

fn test_tokenize_to_args() {
	assert vflags.tokenize_to_args('-cc gcc -ldflags ""') == ['-cc', 'gcc', '-ldflags', '']
	assert vflags.tokenize_to_args('abc def xyz') == ['abc', 'def', 'xyz']
	assert vflags.tokenize_to_args('abc -def --xyz') == ['abc', '-def', '--xyz']
	assert vflags.tokenize_to_args('abc  -def   --xyz') == ['abc', '-def', '--xyz']
	assert vflags.tokenize_to_args('abc  "a b"   --xyz') == ['abc', 'a b', '--xyz'], 'Using double quote should work'
	assert vflags.tokenize_to_args("abc  'a b'   --xyz") == ['abc', 'a b', '--xyz'], 'Using single quote instead of a double " should work too'
	assert vflags.tokenize_to_args('abc  "a quote: \\" . The end."   --xyz') == [
		'abc',
		'a quote: " . The end.',
		'--xyz',
	], 'escaping " should work'
}

fn test_join_env_vflags_and_os_args() {
	os.unsetenv('VFLAGS')
	os.unsetenv('VOSARGS')

	start := vflags.join_env_vflags_and_os_args()
	// dump(start)
	assert start.len > 0

	os.setenv('VFLAGS', r'-cc gcc -ldflags ""', true)
	x := vflags.join_env_vflags_and_os_args()
	// dump(x)
	assert x.len > 0
	assert 'gcc' in x
	assert '-cc' in x
	assert '-ldflags' in x
	assert x.last() == ''

	os.setenv('VFLAGS', r'-cc   gcc -ldflags   "-lpthread -lm"', true)
	y := vflags.join_env_vflags_and_os_args()
	// dump(y)
	assert '-ldflags' in y
	assert y.last() == '-lpthread -lm'

	os.setenv('VFLAGS', r'-cc gcc -ldflags "-lpthread -lm"    -showcc', true)
	z := vflags.join_env_vflags_and_os_args()
	// dump(z)
	assert '-ldflags' in z
	assert z#[-2..-1][0] == '-lpthread -lm'
	assert z.last() == '-showcc'
}
