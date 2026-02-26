import x.json2

fn test_decode_map_any_keeps_numeric_values() {
	payload := {
		'sub': json2.Any('453636')
		'aud': json2.Any('Coachonko')
		'iss': json2.Any('Coachonko')
		'nbf': json2.Any(1688229809)
		'iat': json2.Any(1688229809)
		'jti': json2.Any('e0a26a81-9cc0-4f52-b672-4f6f3d3b44a5')
		'exp': json2.Any(1688273009)
	}
	encoded := json2.encode(payload)
	decoded := json2.decode[map[string]json2.Any](encoded)!

	assert decoded.len == payload.len
	assert 'sub' in decoded
	assert 'aud' in decoded
	assert 'iss' in decoded
	assert 'nbf' in decoded
	assert 'iat' in decoded
	assert 'jti' in decoded
	assert 'exp' in decoded

	assert decoded['sub']!.str() == '453636'
	assert decoded['aud']!.str() == 'Coachonko'
	assert decoded['iss']!.str() == 'Coachonko'
	assert decoded['jti']!.str() == 'e0a26a81-9cc0-4f52-b672-4f6f3d3b44a5'
	assert decoded['nbf']!.f64() == f64(1688229809)
	assert decoded['iat']!.f64() == f64(1688229809)
	assert decoded['exp']!.f64() == f64(1688273009)
}
