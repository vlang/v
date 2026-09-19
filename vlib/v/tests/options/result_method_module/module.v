module result_method_module

pub struct Value {
pub:
	text string
}

pub fn (value Value) str() string {
	return value.text
}
