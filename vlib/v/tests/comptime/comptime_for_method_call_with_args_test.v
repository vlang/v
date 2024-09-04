import os

struct Dummy {}

fn (d Dummy) sample(file_name string) {
	println(file_name)
}

fn test_comptime_for_method_call_with_args() {
	$for method in Dummy.methods {
		if os.args.len > 1 {
			d := Dummy{}
			d.$method(os.args)
		}
	}
	assert true
}
