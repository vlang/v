import x.json2
type AliasTest = int
struct JoseHeader {
mut:
	cty ?string = none
	alg ?string
	typ string = 'JWT'
	aa AliasTest
	gg ?f32 = 12
	jj []byte = [byte(0),1,2,3,4]
}

fn main() {
	result := json2.encode(JoseHeader{cty: none, alg: 'HS256',aa:24})
	dump(result)
}