module decoder2

fn test_nodes() {
	mut nodes := []Node{}

	mut decoder := Decoder{
		json: '{"val": "2"}'
	}

	decoder.fulfill_nodes(mut nodes)

	assert nodes.len == 1
	assert nodes[0].key_pos == 2
	assert nodes[0].key_len == 3
	assert nodes[0].children == none
	nodes = []

	decoder = Decoder{
		json: '{"val": 0, "val1": 1}'
	}
	decoder.fulfill_nodes(mut nodes)

	assert nodes.len == 2
	assert nodes[0].key_pos == 2
	assert nodes[0].key_len == 3

	assert nodes[1].key_pos == 12
	assert nodes[1].key_len == 4

	nodes = []

	decoder = Decoder{
		json: '{"val": {"val": 2}}'
	}
	decoder.fulfill_nodes(mut nodes)

	assert nodes.len == 1
	assert nodes[0].children != none
	assert nodes[0].children?.len == 1
	assert nodes[0].children?[0].key_pos == 10
	assert nodes[0].children?[0].children == none
}
