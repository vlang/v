// vtest vflags: -no-parallel -autofree
type SmartcastOptionalValue = string | i64

fn (value SmartcastOptionalValue) i64() ?i64 {
	match value {
		string {
			return value.i64()
		}
		i64 {
			return value
		}
	}
	return none
}

fn test_scalar_smartcast_method_is_wrapped_as_optional_success() {
	value := SmartcastOptionalValue('42')
	assert value.i64()? == 42
}
