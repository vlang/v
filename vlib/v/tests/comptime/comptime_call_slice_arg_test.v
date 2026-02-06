struct Dummy {}

fn (d Dummy) sample(x int) int {
	return x + 1
}

fn test_main() {
	args := ['0', '5']
	$for method in Dummy.methods {
		if args.len > 1 {
			assert Dummy{}.$method(args[1 ..]) == 6

			tmp := args[1..]
			assert Dummy{}.$method(tmp) == 6
		}
	}
}
