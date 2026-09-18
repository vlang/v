module comptime_accessor_helper

pub fn zero_option_payload[T](x ?T) {
	_ := '__v3_comptime_zero'
}
