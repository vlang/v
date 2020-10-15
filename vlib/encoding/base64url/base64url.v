module base64url

import encoding.base64

// decode decodes base64url string to string
pub fn decode(data string) string {
	mut result := data.replace('-', '+') // 62nd char of encoding
	result = data.replace('_', '/') // 63rd char of encoding
	match result.len % 4 { // Pad with trailing '='s
		2 { result += "==" }  // 2 pad chars
		3 { result += "=" }   // 1 pad char
		else { }              // no padding
	}
	return base64.decode(data)
}

// encode encodes given string to base64url string
pub fn encode(data string) string {
	mut result := base64.encode(data)
	result = result.replace('+', '-') // 62nd char of encoding
	result = result.replace('/', '_') // 63rd char of encoding
	result = result.replace('=', '')  // Remove any trailing '='s
	return result
}
