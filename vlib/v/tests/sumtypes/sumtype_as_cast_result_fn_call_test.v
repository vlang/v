module main

type MsvcAsCastValue = MsvcAsCastRangeValue | string

struct MsvcAsCastRangeValue {
	start int
	end   int
}

struct MsvcAsCastCounter {
mut:
	calls int
}

fn make_msvc_as_cast_value() !MsvcAsCastValue {
	return MsvcAsCastRangeValue{
		start: 2
		end:   5
	}
}

fn make_counted_msvc_as_cast_value(mut counter MsvcAsCastCounter) !MsvcAsCastValue {
	counter.calls++
	return MsvcAsCastRangeValue{
		start: 7
		end:   9
	}
}

fn test_sumtype_as_cast_of_result_fn_call() {
	value := make_msvc_as_cast_value()! as MsvcAsCastRangeValue
	assert value.start == 2
	assert value.end == 5
}

fn test_parenthesized_sumtype_as_cast_of_result_fn_call() {
	value := (make_msvc_as_cast_value()!) as MsvcAsCastRangeValue
	assert value.start == 2
	assert value.end == 5
}

fn test_sumtype_as_cast_of_result_fn_call_keeps_short_circuit() {
	mut counter := MsvcAsCastCounter{}
	left := counter.calls != 0
	if left && (make_counted_msvc_as_cast_value(mut counter)! as MsvcAsCastRangeValue).start == 7 {
		assert false
	}
	assert counter.calls == 0

	right := counter.calls == 0
	if right || (make_counted_msvc_as_cast_value(mut counter)! as MsvcAsCastRangeValue).end == 9 {
		assert true
	} else {
		assert false
	}
	assert counter.calls == 0
}
