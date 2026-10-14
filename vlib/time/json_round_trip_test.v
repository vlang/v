module time

fn test_json_string_round_trips_and_accepts_unquoted_values() {
	reference := parse_iso8601('2026-02-14T09:07:06.123Z')!
	for input in [reference.to_json(), reference.format_rfc3339()] {
		mut decoded := Time{}
		decoded.from_json_string(input)!
		assert decoded.unix() == reference.unix()
		assert decoded.format_rfc3339() == reference.format_rfc3339()
	}
	mut timestamp := Time{}
	timestamp.from_json_string('42')!
	assert timestamp.unix() == 42
	timestamp.from_json_string('"42"')!
	assert timestamp.unix() == 42
}

fn test_json_string_rejects_empty_and_short_invalid_values() {
	for input in ['', 'null', '"', '""', 'bad', '-', '--', '1-2', '+42'] {
		mut decoded := Time{}
		if _ := decoded.from_json_string(input) {
			assert false, 'accepted invalid time: ${input}'
		}
	}
}

fn test_json_string_accepts_signed_unix_timestamp_boundaries() {
	for value in [i64(0), -42, max_i64, min_i64] {
		for input in [value.str(), '"${value}"'] {
			mut decoded := Time{}
			decoded.from_json_string(input)!
			assert decoded.unix() == value
		}
	}
}

fn test_json_string_rejects_unix_timestamp_overflow() {
	for value in ['9223372036854775808', '-9223372036854775809'] {
		for input in [value, '"${value}"'] {
			mut decoded := Time{}
			if _ := decoded.from_json_string(input) {
				assert false, 'accepted overflowing timestamp: ${input}'
			}
		}
	}
}
