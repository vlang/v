import flag

const posix_and_gnu_args_no_tail = ['-f']

const posix_and_gnu_args_tail = ['-f', 'tail']
const posix_and_gnu_args_tails = ['-f', 'tail0', 'tail1', 'tail2']

const skip_posix_and_gnu_args_tail = ['skipme', '-f', 'tail']
const skip_posix_and_gnu_args_tails = ['skipme', '-f', 'tail0', 'tail1', 'tail2']

struct ConfigTail {
	mix  bool
	path string @[tail]
}

struct ConfigTails {
	mix   bool
	paths []string @[tail]
}

fn test_flag_tail() {
	// Test `@[tail]` edge-cases
	config1, no_matches1 := flag.to_struct[ConfigTail](posix_and_gnu_args_tail)!
	assert config1.mix == false
	assert config1.path == 'tail'
	assert no_matches1 == ['-f']

	config2, no_matches2 := flag.to_struct[ConfigTails](posix_and_gnu_args_tails)!
	assert config2.mix == false
	assert config2.paths == ['tail0', 'tail1', 'tail2']
	assert no_matches2 == ['-f']

	config3, no_matches3 := flag.to_struct[ConfigTail](skip_posix_and_gnu_args_tail, skip: 1)!
	assert config3.mix == false
	assert config3.path == 'tail'
	assert no_matches3 == ['-f']

	config4, no_matches4 := flag.to_struct[ConfigTails](skip_posix_and_gnu_args_tails, skip: 1)!
	assert config4.mix == false
	assert config4.paths == ['tail0', 'tail1', 'tail2']
	assert no_matches4 == ['-f']

	config5, no_matches5 := flag.to_struct[ConfigTail](posix_and_gnu_args_tail, skip: 1)!
	assert config5.mix == false
	assert config5.path == 'tail'
	assert no_matches5 == []

	config6, no_matches6 := flag.to_struct[ConfigTails](posix_and_gnu_args_tails, skip: 1)!
	assert config6.mix == false
	assert config6.paths == ['tail0', 'tail1', 'tail2']
	assert no_matches6 == []

	config7, no_matches7 := flag.to_struct[ConfigTail](posix_and_gnu_args_no_tail)!
	assert config7.mix == false
	assert config7.path == ''
	assert no_matches7 == ['-f']

	config8, no_matches8 := flag.to_struct[ConfigTail](posix_and_gnu_args_no_tail, skip: 1)!
	assert config8.mix == false
	assert config8.path == ''
	assert no_matches8 == []
}
