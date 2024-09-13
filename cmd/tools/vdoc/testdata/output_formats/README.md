## Description

This is an example of a an .md file, used for adding more rich text
documentation in a project or module.

This is a [link](https://vlang.io/) to the main V site.

This is a <b>bold text</b>.

This is a script `<script>console.log('hi from README.md');</script>` .

## Examples

### Processing command line args

```v
import os

fn main() {
	dump(os.args)
	dump(os.args.len)
	assert os.args.len > 0

	// Test escape characters like for `&` and `<`
	mut arr := [1, 2, 3]
	mut ref := &arr
	arr << 4

	ch := chan bool{cap: 1}
	ch <- true
}
```

### A JWT example (test syntax highlighting)

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

### Other language specifiers

```cpp
#include <iostream>
#include <map>

std::map<std::string, int> my_map {
	{"KEY_1", 0},
	{"KEY_2", 10},
};

for (const auto &[key, value] : my_map) {
	std::cout << key << ": " << value << ", ";
}
std::cout << "\n";
```

```v ignore
doc1 := toml.parse_text(<string content>) or { panic(err) }
doc2 := toml.parse_file(<file path>) or { panic(err) }
```

### Escape html in strings

```v
const html = '<!DOCTYPE html>
<html lang="en">
  <head>
    <style>
      body {
        background: linear-gradient(to right, #274060, #1B2845);
        color: GhostWhite;
        font-family: sans-serif;
        text-align: center;
      }
    </style>
  </head>
  <body>
    <h1>Your App Content!</h1>
    <button onclick="callV()">Call V!</button>
  </body>
  <script>
    async function callV() {
      // Call a V function that takes an argument and returns a value.
      const res = await window.my_v_func(\'Hello from JS!\');
      console.log(res);
    }
  </script>
</html>'
```

- Regular markdown list point 1
- List point 2
- List point 3

1. Numbered markdown list point 1
2. List point 2
3. List point 3

A code block without a specific language should be rendered verbatim:
```
.
├── static/
│   ├── css/
│   │   └── main.css
│   └── js/
│       └── main.js
└── main.v
```

The s tags here in the code block, should be rendered verbatim, not interpreted as HTML ones:
```
h:m:s      // 5:02:33
m:s.mi<s>  // 2:33.015
s.mi<s>    // 33.015s
mi.mc<ms>  // 15.007ms
mc.ns<ns>  // 7.234us
ns<ns>     // 234ns
```

The End.
