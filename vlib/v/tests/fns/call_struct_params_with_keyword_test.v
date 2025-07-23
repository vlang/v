@[params]
struct Config {
	dump bool
}

fn test_main() {
	opt := Config{}
	run('x', dump: opt.dump)!
}

fn run(source string, config Config) ! {
	dump(config)
}
