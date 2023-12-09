## Description:

This is an example of a an .md file, used for adding more rich text
documentation in a project or module.

This is a [link](https://vlang.io/) to the main V site.

This is a <b>bold text</b>.

This is a script <script>console.log('hi from README.md');</script> .

## Examples:

### Processing command line args:
```v
import os

fn main() {
	dump(os.args)
	dump(os.args.len)
	assert os.args.len > 0
}
```

### A JWT example (test syntax highlighting):
```v
import crypto.hmac
import crypto.sha256
import encoding.base64
import json
import time

struct JwtHeader {
	alg string
	typ string
}

struct JwtPayload {
	sub  string
	name string
	iat  int
}

fn main() {
	sw := time.new_stopwatch()
	secret := 'your-256-bit-secret'
	token := make_token(secret)
	ok := auth_verify(secret, token)
	dt := sw.elapsed().microseconds()
	println('token: ${token}')
	println('auth_verify(secret, token): ${ok}')
	println('Elapsed time: ${dt} uS')
}

fn make_token(secret string) string {
	header := base64.url_encode(json.encode(JwtHeader{'HS256', 'JWT'}).bytes())
	payload := base64.url_encode(json.encode(JwtPayload{'1234567890', 'John Doe', 1516239022}).bytes())
	signature := base64.url_encode(hmac.new(secret.bytes(), '${header}.${payload}'.bytes(),
		sha256.sum, sha256.block_size))
	jwt := '${header}.${payload}.${signature}'
	return jwt
}

fn auth_verify(secret string, token string) bool {
	token_split := token.split('.')
	signature_mirror := hmac.new(secret.bytes(), '${token_split[0]}.${token_split[1]}'.bytes(),
		sha256.sum, sha256.block_size)
	signature_from_token := base64.url_decode(token_split[2])
	return hmac.equal(signature_from_token, signature_mirror)
}
```
