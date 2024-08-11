type MySumType = S1 | S2

struct Info {
	name string
}

struct Node {
	left MySumType
}

struct S1 {
	is_info bool
	info    Info
}

fn (s1 S1) is_info() bool {
	return s1.is_info
}

struct S2 {
	field2 string
}

fn get_name(s1 S1) string {
	return s1.info.name
}

fn test_autocast_in_if_conds() {
	node := Node{
		left: MySumType(S1{
			is_info: false
			info:    Info{'foo'}
		})
	}

	a := 22

	if a > 0 && node.left is S1 && !node.left.is_info && get_name(node.left) == 'foo'
		&& !node.left.is_info() {
		println('ok')
		assert true
	} else {
		assert false
	}
}
