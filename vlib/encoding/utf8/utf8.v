module utf8

import encoding.utf8.validate as impl

// validate_str reports if str consists of valid UTF-8 runes
pub fn validate_str(str string) bool {
	return impl.utf8_string(str)
}

// validate reports if data consists of valid UTF-8 runes
pub fn validate(data &u8, len int) bool {
	return impl.utf8_data(data, len)
}
