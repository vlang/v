import time

fn test_main() {
	select {
		1 * time.second {}
	}
	assert true
}
