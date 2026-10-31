module strconv

fn test_parse_bool_true_values() {
	for s in ['1', 't', 'T', 'true', 'TRUE', 'True'] {
		v := parse_bool(s) or {
			assert false, '`${s}` must parse'
			false
		}
		assert v == true, '`${s}` must be true'
	}
}

fn test_parse_bool_false_values() {
	for s in ['0', 'f', 'F', 'false', 'FALSE', 'False'] {
		v := parse_bool(s) or {
			assert false, '`${s}` must parse'
			false
		}
		assert v == false, '`${s}` must be false'
	}
}

fn test_parse_bool_rejects_everything_else() {
	for s in ['', 'yes', 'no', '2', 'TrUe', ' true', 'true ', 'on', 'tRUE'] {
		parse_bool(s) or {
			assert err.msg().contains('invalid syntax')
			continue
		}
		assert false, '`${s}` must not parse'
	}
}
