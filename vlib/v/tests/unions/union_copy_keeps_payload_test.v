// A union is copied bit for bit: a copy cannot clone its `string` member, because
// another member may hold the live value (as in `vsql.Value`).
union Payload {
mut:
	f f64
	i i64
	s string
}

struct Value {
mut:
	kind int
	v    Payload
}

fn new_int(x i64) Value {
	return Value{
		kind: 1
		v:    Payload{
			i: x
		}
	}
}

fn new_float(x f64) Value {
	return Value{
		kind: 2
		v:    Payload{
			f: x
		}
	}
}

fn (v Value) int_value() i64 {
	return unsafe { v.v.i }
}

fn (v Value) float_value() f64 {
	return unsafe { v.v.f }
}

fn test_array_push_and_clone_keep_the_union_payload() {
	i := new_int(123)
	f := new_float(1.25)
	mut values := []Value{}
	values << i
	values << f
	assert values[0].int_value() == 123
	assert values[1].float_value() == 1.25
	copies := values.clone()
	assert copies[0].int_value() == 123
	assert copies[1].float_value() == 1.25
}

fn test_map_store_keeps_the_union_payload() {
	i := new_int(-7)
	f := new_float(2.5)
	mut values := map[string]Value{}
	values['i'] = i
	values['f'] = f
	copies := values.clone()
	assert copies['i'] or { panic('missing') }.int_value() == -7
	assert copies['f'] or { panic('missing') }.float_value() == 2.5
}
