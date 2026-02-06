const msg = c'test'

fn test_main() {
	unsafe {
		assert msg.vstring() == 'test'
	}
}
