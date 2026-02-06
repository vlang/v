pub type Any = string | []Any | map[string]Any

pub fn decode_struct[T](value map[string]Any, mut t T) ! {
	$for f in T.fields {
		key := f.name
		vals := value[key] or { '' }
		if vals is string && vals != '' {
			$if f is string {
				t.$(f.name) = vals
			}
		} else if vals is map[string]Any {
			if f.is_struct {
				decode_struct(vals, mut t.$(f.name))!
			}
		} else if vals is []Any {
			$if f is $array {
				for val in vals {
					if val is map[string]Any {
						mut elem := array_element(t.$(f.name))
						$if elem is $struct {
							decode_struct(val, mut elem)!
							t.$(f.name) << elem
						}
					}
				}
			}
		}
	}
}

fn array_element[T](a []T) T {
	return T{}
}

struct Element {
	name string
}

struct Set {
	name  string
	elems []Element
}

struct Tree {
	name     string
	children []Tree
}

const set_any = {
	'name':  Any('1')
	'elems': Any([
		Any({
			'name': Any('2')
		}),
		{
			'name': Any('3')
		},
	])
}
const tree_any = {
	'name':     Any('1')
	'children': Any([
		Any({
			'name': Any('2')
		}),
		{
			'name': Any('3')
		},
	])
}

fn test_main() {
	mut set := Set{}
	decode_struct(set_any, mut set)!
	assert set.name == '1'
	assert set.elems[0].name == '2'
	assert set.elems[1].name == '3'

	mut tree := Tree{}
	decode_struct(tree_any, mut tree)!
	assert tree.name == '1'
	assert tree.children[0].name == '2'
	assert tree.children[1].name == '3'
}
