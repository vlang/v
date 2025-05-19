module main

import json

pub struct JwtHeader {
pub:
	alg string
	typ string
	cty string
}

const f1 = json.encode(JwtHeader{ alg: '1', typ: '2', cty: '3' })
