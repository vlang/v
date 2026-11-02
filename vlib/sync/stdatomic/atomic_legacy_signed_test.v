// vtest vflags: -w
// Deliberate deprecated-type coverage must also run in suites using -W.
import sync.stdatomic

fn test_legacy_signed_atomic_operations() {
	mut value := stdatomic.new_atomic(isize(-42))
	value.store(-50)
	assert value.load() == -50
	assert value.add(8) == -50
	assert value.sub(2) == -42
	assert value.swap(-100) == -44
	assert !value.compare_and_swap(-42, -1)
	assert value.compare_and_swap(-100, -1)
	assert value.load() == -1
}
