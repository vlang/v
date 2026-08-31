module main

import json2

pub struct JwtHeader {
pub:
	alg string
	typ string
	cty string
}

const f1 = json2.encode(JwtHeader{ alg: '1', typ: '2', cty: '3' })
