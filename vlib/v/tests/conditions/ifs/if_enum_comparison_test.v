enum ComparisonState {
	bound     = 11
	abandoned = 29
	other     = 47
}

type ComparisonStateAlias = ComparisonState

struct ComparisonTicket {
	state ComparisonState
}

fn matches_conditional_state(ticket ComparisonTicket, abandoned bool, other bool) bool {
	return ticket.state == if abandoned {
		.abandoned
	} else if other {
		.other
	} else {
		.bound
	}
}

fn test_enum_comparison_with_conditional_shorthand() {
	for abandoned in [false, true] {
		state := if abandoned { ComparisonState.abandoned } else { ComparisonState.bound }
		ticket := ComparisonTicket{state}
		assert ticket.state == if abandoned { .abandoned } else { .bound }
		assert (if abandoned { .abandoned } else { .bound }) == ticket.state
		assert ticket.state != if abandoned { .bound } else { .abandoned }
		assert (if abandoned { .bound } else { .abandoned }) != ticket.state
		assert matches_conditional_state(ticket, abandoned, false)
		alias_state := ComparisonStateAlias(state)
		assert alias_state == (if abandoned { .abandoned } else { .bound })
		assert (if abandoned { .abandoned } else { .bound }) == alias_state
		assert ticket.state == match abandoned {
			true { .abandoned }
			else { .bound }
		}
		assert (match abandoned {
			true { .abandoned }
			else { .bound }
		}) == ticket.state
	}
	assert matches_conditional_state(ComparisonTicket{.other}, false, true)
}

@[flag]
enum ComparisonAccess {
	read
	write
}

fn test_flag_enum_comparison_with_conditional_shorthand() {
	for writable in [false, true] {
		access := if writable {
			ComparisonAccess.read | .write
		} else {
			ComparisonAccess.read
		}
		assert access == if writable { .read | .write } else { .read }
		assert (if writable { .read | (.write) } else { .read }) == access
	}
}
