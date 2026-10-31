module main

fn test_extract_run_id_validates_response() {
	assert extract_run_id('{"result":{"id":42}}')! == 42
	for input in ['{', '{}', '{"error":"failed"}', '{"result":{}}', '{"result":{"id":0}}',
		'{"result":{"id":-1}}'] {
		mut rejected := false
		extract_run_id(input) or { rejected = true }
		assert rejected, input
	}
}
