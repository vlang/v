## Description:

`json` provides encoding/decoding of V data structures to/from JSON.

## Examples:

```v
import json

enum JobTitle {
	manager
	executive
	worker
}

struct Employee {
	name   string
	age    int
	salary f32
	title  JobTitle
}

fn main() {
	x := Employee{'Peter', 28, 95000.5, .worker}
	println(x)
	//
	s := json.encode(x)
	println('Employee x: $s')
	assert s == '{"name":"Peter","age":28,"salary":95000.5,"title":2}'
	//
	y := json.decode(Employee, s)?
	//
	println(y)
	assert y == x
}
```
