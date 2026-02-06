import flag

const some_args_1 = ['--mix', '-m', 'ok', '-d', 'one', '--test=abc', '-d', 'two', '/path/to/a',
	'path/to/b']

struct Config {
	am       string   @[only: m]
	def_test string = 'def'   @[long: test; short: t]
	device   []string @[short: d]
	paths    []string @[tail]
mut:
	amount   int = 1
	mix      bool
	mix_hard bool = true
}

fn test_using() {
	mut config := Config{
		mix_hard: false
		amount:   8
	}

	config, _ = flag.using[Config](config, some_args_1)!
	assert config.mix
	assert config.mix_hard == false
	assert config.am == 'ok'
	assert config.def_test == 'abc'
	assert config.device[0] == 'one'
	assert config.device[1] == 'two'
	assert config.amount == 8
	assert config.paths.len == 2
	assert config.paths[0] == '/path/to/a'
	assert config.paths[1] == 'path/to/b'

	config.mix = false // is changed to true via `--mix`
	config.mix_hard = true // should be kept as `true`, since no flags changed it
	config.amount = 888

	config2, _ := flag.using[Config](config, some_args_1)!
	assert config2.mix
	assert config2.mix_hard
	assert config2.am == 'ok'
	assert config2.def_test == 'abc'
	assert config2.device[0] == 'one'
	assert config2.device[1] == 'two'
	assert config2.device[2] == 'one' // `config` already had items pushed from `some_args_1` so this grows when using `using[T](struct,...)`
	assert config2.device[3] == 'two'
	assert config2.device.len == 4
	assert config2.amount == 888
	assert config2.paths.len == 4
	assert config2.paths[0] == '/path/to/a'
	assert config2.paths[1] == 'path/to/b'
	assert config2.paths[2] == '/path/to/a'
	assert config2.paths[3] == 'path/to/b'
}
