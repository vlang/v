import os

// vtest retry: 2

// This tests whether the V test runner, can handle retries.
//
// The comment above, should make it try re-running the same test,
// a maximum of 2 times. It will fail for all, but the last retry.
// This is useful for reducing false positives on the CI, due to
// flakyness of specific tests like `vlib/v/live/live_test.v` for example.

// Note: this test is supposed to be run with `v test retry_test.v`.
// Running just `v retry_test.v` WILL fail.

fn test_test_runner_retrying_failing_tests() {
	n := os.getenv('VTEST_RETRY').int()
	maxn := os.getenv('VTEST_RETRY_MAX').int()
	eprintln('> n: ${n} | maxn: ${maxn}')
	if n > 0 && n == maxn {
		assert true
		return
	}
	assert false
}
