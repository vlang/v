import time

fn test_unbuffer_stdout() {
	// TODO: add a more extensive test, perhaps using expect,
	// that does not require an active user to verify its validity.
	// Currently this just tests that unbuffer_stdout/0 can be called,
	// and its effects can be observed by commenting the call to it,
	// on the next line, and then re-running the test:
	unbuffer_stdout()
	print('#---')
	for i in 0 .. 21 {
		print('\b\b\b> ${i:02}')
		time.sleep(20 * time.millisecond)
	}
	println('\b\b\b done')
	assert true
}
