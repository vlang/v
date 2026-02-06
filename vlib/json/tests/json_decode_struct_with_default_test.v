import json

pub struct Response {
pub:
	results []Result = []Result{len: 0} @[json: list]
	tags    []string = []string{len: 0} @[json: tags]
	kind    string   @[json: result_type]
}

pub struct Result {
pub:
	id            int      @[json: defid]
	author        string   @[json: author]
	definition    string   @[json: definition]
	link          string   @[json: permalink]
	thumbs_down   int      @[json: thumbs_down]
	thumbs_up     int      @[json: thumbs_up]
	word          string   @[json: word]
	date          string   @[json: written_on]
	audio_samples []string @[json: sound_urls]
	example       string   @[json: example]
}

pub fn define(word string) !&Response {
	resp := '{"list":[{"defid":3439287}]}'
	response := json.decode(Response, resp) or { return err }
	return &response
}

fn test_main() {
	response := define('')!
	assert response.results.len > 0
	assert response.results[0].id == 3439287
}
