type Child = Tree | string

type RecursiveChild = int | []RecursiveChild

struct Tree {
	name  string
	child map[string]Child
}

struct RecursiveFields {
	a int
	b []int
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

fn test_recursive_sumtype_array_variant_assignment_in_map() {
	ints := [1, 2, 3]
	mut got := map[string]RecursiveChild{}
	got['nums'] = ints
	assert got == {
		'nums': RecursiveChild([RecursiveChild(1), RecursiveChild(2), RecursiveChild(3)])
	}
}

fn test_recursive_sumtype_array_variant_comptime_selector_assignment_in_map() {
	data := RecursiveFields{120, [130, 140]}
	mut got := map[string]RecursiveChild{}
	$for field in RecursiveFields.fields {
		$if field.typ is int {
			got[field.name] = data.$(field.name)
		} $else $if field.typ is []int {
			got[field.name] = data.$(field.name)
		}
	}
	assert got == {
		'a': RecursiveChild(120)
		'b': RecursiveChild([RecursiveChild(130), RecursiveChild(140)])
	}
}
