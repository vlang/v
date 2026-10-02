module types

// A scoped batch costs a fork of the checker and a promotion of its results:
// a small program is checked in one batch, a large one in at most the limit.

fn test_a_small_program_is_checked_in_one_batch() {
	assert scoped_check_batch_count(11, 440, scoped_check_serial_batches) == 1
	assert scoped_check_batch_count(1, 5, scoped_check_serial_batches) == 1
}

fn test_a_batch_holds_a_minimum_of_work() {
	assert scoped_check_batch_count(201, 8200, scoped_check_serial_batches) == 4
	assert scoped_check_batch_count(2001, 82_000, scoped_check_serial_batches) == 40
}

fn test_a_large_program_keeps_the_batch_limit() {
	assert scoped_check_batch_count(20_000, 800_000, scoped_check_serial_batches) == scoped_check_serial_batches
	assert scoped_check_batch_count(3, 1_000_000, scoped_check_serial_batches) == 3
}
