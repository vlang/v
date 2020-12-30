struct Config {
	token string
}

struct Client {
	x u64
	y u64
}

fn new(config Config, shard_count ...int) ?&Client {
	return &Client{1, 2}
}

fn test_can_compile_an_empty_var_arg() {
	x := new(Config{
		token: 'xyz'
	}) or { panic(err) }
	assert x.x == 1
	assert x.y == 2
}
