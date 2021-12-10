import os
import strconv

fn main() {
	if n := strconv.atoi(os.args[0]) {
		println(n)
	} else {
		println(err)
	}
	assert true
}
