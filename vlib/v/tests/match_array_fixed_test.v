fn test_main() {
	on_event()
}

fn on_event() {
	match [0, 0]! {
		[0, 1]! {
			assert false
		}
		else {
			assert true
		}
	}
}
