// A `panic(...)` branch of a returned match produces no value, so it does not
// have to match the returned sum type.
struct AngleValue {
	deg f64
}

struct LengthValue {
	px f64
}

struct KeywordValue {
	name string
}

type DimensionValue = AngleValue | LengthValue

type StyleValue = AngleValue | KeywordValue | LengthValue

fn (style_value StyleValue) as_dimension() DimensionValue {
	return match style_value {
		AngleValue { DimensionValue(style_value) }
		LengthValue { DimensionValue(style_value) }
		else { panic('VERIFY_NOT_REACHED') }
	}
}

fn test_return_match_sumtype_with_panic_else() {
	angle := StyleValue(AngleValue{
		deg: 1.5
	}).as_dimension()
	assert angle is AngleValue
	if angle is AngleValue {
		assert angle.deg == 1.5
	}
	length := StyleValue(LengthValue{
		px: 3
	}).as_dimension()
	assert length is LengthValue
	if length is LengthValue {
		assert length.px == 3.0
	}
}
