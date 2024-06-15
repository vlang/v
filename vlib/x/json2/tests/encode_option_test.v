import x.json2

struct JoseHeader {
pub mut:
	cty ?string
	alg string
	typ string = 'JWT'
}

fn test_main() {
	res := json2.encode(JoseHeader{ alg: 'HS256' })
	assert res == '{"alg":"HS256","typ":"JWT"}'
}
