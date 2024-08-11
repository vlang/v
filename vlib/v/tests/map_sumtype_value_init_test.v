type Child = Tree | string

struct Tree {
	name  string
	child map[string]Child
}

fn test_map_sumtype_value_init() {
	tree := Tree{
		name:  'foo'
		child: {
			'a': 'value'
		}
	}
	println(tree)
	ret := tree.child['a'] or { Child('') }
	assert ret == Child('value')
}
