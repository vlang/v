interface FixedSamples {
mut:
	samples [4]f32
	param   f32
	sum() f32
}

struct FixedLowpass {
mut:
	samples [4]f32
	param   f32
}

fn (mut l FixedLowpass) sum() f32 {
	return l.samples[1] + l.param
}

fn test_interface_with_fixed_array_field_from_reference() {
	mut l := &FixedLowpass{}
	l.samples[1] = 2.0
	l.param = 0.5
	mut fx := FixedSamples(l)
	assert fx.sum() == 2.5
	assert fx.samples[1] == 2.0
	fx.samples[1] = 4.0
	assert l.samples[1] == 4.0
	assert fx.sum() == 4.5
}

fn test_interface_with_fixed_array_field_from_value() {
	mut l := FixedLowpass{}
	l.samples[1] = 3.0
	l.param = 1.5
	mut fx := FixedSamples(l)
	assert fx.sum() == 4.5
	assert fx.samples[1] == 3.0
}

fn test_interface_fixed_array_field_from_pointer_cast() {
	mut l := &FixedLowpass{}
	l.samples[1] = 2.0
	l.param = 0.5
	mut fx := &FixedSamples(l)
	assert fx.samples[1] == 2.0
	assert fx.sum() == 2.5
	fx.samples[1] = 4.0
	assert l.samples[1] == 4.0
	assert fx.sum() == 4.5
}
