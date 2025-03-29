# HTTP Status Code Response API

This module is the HTTP status code response protocol for V Web apps.
You can send status codes in JSON with this module.
It is compatible with front-end servers written in other programming languages.
(e.g.:Node.js front-end server)

## Examples

```v
import os
import veb
import veb.status { Status, new_status }

pub struct RespStatus {
	Status
}

@[heap]
pub struct App {
	veb.Controller
}

pub struct Context {
	veb.Context
}

@['/'; get]
pub fn (mut app App) status_handle() veb.Result {
	resp := RespStatus{
		Status: new_status('Success.', .ok)
	}
	return ctx.json(resp)
}

pub fn main() {
	os.chdir(os.dir(@FILE))!
	mut app := &App{}
	veb.run[App, Context](mut app, 8082)
}
```
