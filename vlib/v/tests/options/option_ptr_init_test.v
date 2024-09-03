struct Queue {
	head ?&Node
}

struct Node {
	next ?&Node
}

fn test_main() {
	q := Queue{}
	assert q.head == none
}
