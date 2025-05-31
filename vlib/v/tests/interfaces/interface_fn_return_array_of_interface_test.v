import os

interface Node {
	name string
}

struct NodeEmpty {
	name string = 'abc'
}

fn pusher(lines []string) []Node {
	mut nodes := []Node{}

	for line in lines {
		nodes << NodeEmpty{}
	}

	return nodes
}

fn test_fn_return_array_of_interface() {
	lines := os.read_lines(@FILE) or { panic(err) }
	pushed := pusher(lines)
	println(pushed)
	assert pushed.len > 0
}
