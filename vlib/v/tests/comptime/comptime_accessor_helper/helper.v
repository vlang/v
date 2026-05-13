module comptime_accessor_helper

pub fn zero_option_payload[T](x ?T) {
	_ := typeof(x).payload_type{}
}
