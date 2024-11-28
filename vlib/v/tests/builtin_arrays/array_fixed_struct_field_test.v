struct Args {
	bytes [2]int
}

fn test_main() {
	make_args() or { assert err.msg() == 'a' }
}

fn make_args() !Args {
	return Args{
		bytes: get_range() or { return error('a') }
	}
}

fn get_range() ![2]int {
	return error('')
}
