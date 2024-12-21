module main

import json

pub struct Response {
pub:
	results []Result = []Result{len: 0} @[json: list]
	tags    []string = []string{len: 0} @[json: tags]
	kind    string   @[json: result_type]
}

pub struct Result {
pub:
	id         int    @[json: defid]
	author     string @[json: author]
	definition string @[json: definition]
}

fn test_main() {
	resp := '{"list": [{"defid":123, "author": "", "definition": "" }]}'
	response := json.decode(Response, resp)!
	assert response.results.len > 0
	assert response.results[0].id == 123
}
