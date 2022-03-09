[noreturn]
fn no_return() {
	exit(0)
}

fn abc() bool {
	$if x64 {
		no_return()
	} $else {
		no_return()
	}
}

fn main() {
	abc()
}
