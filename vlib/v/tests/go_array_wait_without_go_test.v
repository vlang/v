fn test_go_array_wait_without_go() {
	sa := SA{}
	sb := SA{}

	sa.wait()
	sb.wait()

	assert true
}

struct SA {
mut:
	threads []thread
}

pub fn (self SA) wait() {
	self.threads.wait()
}

struct SB {
mut:
	threads []thread
}

pub fn (self SB) wait() {
	self.threads.wait()
}
