import json
import vweb

struct VwebApp {
	vweb.Context
}

fn test_encode_struct_embedding_vweb_context() {
	encoded := json.encode(VwebApp{})
	assert encoded.contains('"done":false')
	assert !encoded.contains('"ctx"')
	assert !encoded.contains('"req"')
}
