module bench

// current_instruction_count returns none: retired instruction counters are
// only read on macOS.
fn current_instruction_count() ?u64 {
	return none
}
