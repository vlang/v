struct GenericCallEnvelope {
	value int
}

fn make_generic_call_envelope[I, R](id I, result R) GenericCallEnvelope {
	_ = id
	_ = result
	return GenericCallEnvelope{
		value: 7
	}
}

fn (envelope GenericCallEnvelope) value_copy() int {
	return envelope.value
}

fn test_multi_arg_generic_call_chained_method_emits_all_specializations() {
	values := [
		make_generic_call_envelope(1, true).value_copy(),
		make_generic_call_envelope('server', u64(2)).value_copy(),
	]
	assert values == [7, 7]
}
