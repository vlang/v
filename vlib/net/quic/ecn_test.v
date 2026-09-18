module quic

// test_ecn_state_parses_counts_without_erroring_and_never_validates is the
// plan's own explicitly named test: ECN counts from a received ACK frame
// are recorded without erroring, and NEVER treated as validated -- so no
// future congestion-control code path could react to them.
fn test_ecn_state_parses_counts_without_erroring_and_never_validates() {
	mut s := new_ecn_state()
	assert !s.is_validated()

	s.note_ack_ecn_counts(EcnCounts{
		ect0:   10
		ect1:   0
		ecn_ce: 3
	})
	assert s.last_ect0 == 10
	assert s.last_ect1 == 0
	assert s.last_ecn_ce == 3

	// Regardless of what was recorded -- including a non-zero ECN-CE
	// count, which a real ECN-reacting implementation would treat as a
	// congestion signal -- v1 never reports validated.
	assert !s.is_validated()
}

fn test_ecn_state_records_latest_cumulative_totals() {
	mut s := new_ecn_state()
	s.note_ack_ecn_counts(EcnCounts{
		ect0:   1
		ect1:   2
		ecn_ce: 3
	})
	s.note_ack_ecn_counts(EcnCounts{
		ect0:   5
		ect1:   6
		ecn_ce: 7
	})
	assert s.last_ect0 == 5
	assert s.last_ect1 == 6
	assert s.last_ecn_ce == 7
}
