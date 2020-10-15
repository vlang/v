import encoding.base64url

fn test_encode() {
	test := base64url.encode('Hello Base64Url encoding!')
	assert test == 'SGVsbG8gQmFzZTY0VXJsIGVuY29kaW5nIQ'
}

fn test_decode() {
	test := base64url.decode("SGVsbG8gQmFzZTY0VXJsIGVuY29kaW5nIQ")
	assert test == 'Hello Base64Url encoding!'
}
