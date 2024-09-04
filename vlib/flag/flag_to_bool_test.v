import flag

const gnu_args_bool_flags = ['--no-parallel', '--nocache', '--stay', '--nix']

struct BoolConfig {
	mix      bool
	nix      bool = true
	parallel bool = true @[long: 'no-parallel']
	cache    bool = true @[long: nocache]
	no_stay  bool @[long: 'stay']
}

fn test_bool_flags() {
	bf, _ := flag.to_struct[BoolConfig](gnu_args_bool_flags, style: .long)!

	assert bf.mix == false
	assert bf.nix == true // This should not fail?
	assert bf.parallel == false
	assert bf.cache == true
	assert bf.no_stay == true
}
