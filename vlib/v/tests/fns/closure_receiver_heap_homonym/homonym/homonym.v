module homonym

@[heap]
pub struct Receiver {
pub mut:
	n int
}

// change_alias retains the captured heap receiver's reference.
pub fn (mut receiver Receiver) change_alias() {
	fn [mut receiver] () {
		mut alias := receiver
		alias.n = 9
	}()
}
