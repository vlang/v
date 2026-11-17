type SharedIsValue = int | string

type SharedIsFirst = SharedIsValue

type SharedIsSecond = SharedIsFirst

type SharedIsThird = SharedIsSecond

fn test_shared_alias_chain_is_checks_both_variants() {
	shared integer := SharedIsThird(SharedIsSecond(SharedIsFirst(SharedIsValue(42))))
	rlock integer {
		is_int := integer is int
		is_not_string := integer !is string
		assert is_int
		assert is_not_string
	}
	shared text := SharedIsThird(SharedIsSecond(SharedIsFirst(SharedIsValue('text'))))
	rlock text {
		is_string := text is string
		is_not_int := text !is int
		assert is_string
		assert is_not_int
	}
}

fn shared_alias_is_int(shared value SharedIsThird) bool {
	rlock value {
		return value is int
	}
}

fn shared_alias_is_not_int(shared value SharedIsThird) bool {
	rlock value {
		return value !is int
	}
}

fn test_shared_alias_parameter_is_checks_both_variants() {
	shared integer := SharedIsThird(SharedIsSecond(SharedIsFirst(SharedIsValue(42))))
	shared text := SharedIsThird(SharedIsSecond(SharedIsFirst(SharedIsValue('text'))))
	assert shared_alias_is_int(shared integer)
	assert !shared_alias_is_not_int(shared integer)
	assert !shared_alias_is_int(shared text)
	assert shared_alias_is_not_int(shared text)
}
