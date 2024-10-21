import encoding.uri

fn test_encode_decode() {
	uri_str := 'Some data!That Needs Encoding/'
	uri_encoded_str := 'Some%20data%21That%20Needs%20Encoding%2F'
	assert uri.encode(uri_str) == uri_encoded_str
	assert uri.decode(uri_encoded_str) == uri_str
}
