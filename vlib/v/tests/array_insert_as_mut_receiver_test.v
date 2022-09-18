fn test_array_insert_as_mut_receiver() {
	mut ns := []Node{cap: 2}
	other(mut ns)
	println(ns)
	assert ns.len == 1
	assert ns[0].id == 1
	assert ns[0].a == 0
}

fn other(mut ns []Node) {
	n1 := Node{
		id: 1
	}
	ns.insert(0, n1)
}

pub struct Node {
pub:
	id u32 [required]
	a  int
}
