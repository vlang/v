[manualfree]
module main

fn abc() {
	x := 'abc should be autofreed'
	println(x)
}

[manualfree]
fn xyz() {
	x := 'xyz should do its own memory management'
	println(x)
}

fn main() {
	abc()
	xyz()
}
