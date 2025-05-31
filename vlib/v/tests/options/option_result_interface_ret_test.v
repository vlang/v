struct Node1 {
	data int
}

struct Node2 {
	data int
}

interface INode {
	data int
}

fn make_node(c int) !INode {
	match c {
		1 {
			return make_node1()!
		}
		2 {
			return make_node2()!
		}
		else {
			return error('invalid node type')
		}
	}
}

fn make_node_opt(c int) ?INode {
	match c {
		1 {
			return *make_node1_opt()?
		}
		2 {
			return *make_node2_opt()?
		}
		else {
			return none
		}
	}
}

fn make_node1() !&Node1 {
	return &Node1{
		data: 1
	}
}

fn make_node2() !&Node2 {
	return &Node2{
		data: 2
	}
}

fn make_node1_opt() ?&Node1 {
	return &Node1{
		data: 1
	}
}

fn make_node2_opt() ?&Node2 {
	return &Node2{
		data: 2
	}
}

fn test_main() {
	a := make_node(1)!
	dump(a)
	assert a.data == 1

	b := make_node_opt(1)?
	dump(b)
	assert a.data == 1
}
