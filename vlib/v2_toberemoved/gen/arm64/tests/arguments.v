import os

fn main() {
	println(os.args.len > 0)
	if os.args.len > 0 {
		println(os.args[0].len > 0)
	}
}
