type PointerPayloadSum = int | string

type PlainPayloadSum = f64 | u8

fn pointer_payload_sum_once(mut calls []int) &PointerPayloadSum {
	calls[0]++
	return &PointerPayloadSum('payload')
}

fn plain_payload_sum_once(mut calls []int) &PlainPayloadSum {
	calls[0]++
	return &PlainPayloadSum(u8(7))
}

fn test_sum_pointer_typeof_reads_each_tag_width_once() {
	mut calls := [0]
	pointer_name := unsafe { typeof(pointer_payload_sum_once(mut calls)) }
	assert pointer_name == 'string'
	assert calls[0] == 1
	plain_name := unsafe { typeof(plain_payload_sum_once(mut calls)) }
	assert plain_name == 'u8'
	assert calls[0] == 2
	no_pointer_payload := unsafe { &PointerPayloadSum(nil) }
	no_plain_payload := unsafe { &PlainPayloadSum(nil) }
	assert unsafe { typeof(no_pointer_payload) } == 'unknown PointerPayloadSum'
	assert unsafe { typeof(no_plain_payload) } == 'unknown PlainPayloadSum'
}

fn optional_gc_array(present bool) ?[]string {
	if !present {
		return none
	}
	return ['first'.clone(), 'second'.clone()]
}

fn result_gc_array(present bool) ![]string {
	if !present {
		return error('missing array')
	}
	return ['third'.clone(), 'fourth'.clone()]
}

fn test_option_and_result_pointer_payload_roundtrip() {
	optional := optional_gc_array(true) or { panic('expected an array') }
	assert optional == ['first', 'second']
	absent := optional_gc_array(false) or { ['fallback'] }
	assert absent == ['fallback']
	result := result_gc_array(true) or { panic(err) }
	assert result == ['third', 'fourth']
	failed := result_gc_array(false) or {
		assert err.msg() == 'missing array'
		['fallback']
	}
	assert failed == ['fallback']
}
