struct IntInfo {
	value int
}

struct StructInfo {
	name  string
	value int
}

type TypeInfo = IntInfo | StructInfo

fn name_contains_dot(name string) bool {
	return name.contains('.')
}

fn has_dot(info TypeInfo) bool {
	mut cur := info
	if cur is StructInfo {
		return name_contains_dot(cur.name)
	}
	return false
}

fn main() {
	with_dot := TypeInfo(StructInfo{
		name:  'foo.bar'
		value: 1
	})
	without_dot := TypeInfo(StructInfo{
		name:  'foobar'
		value: 2
	})
	println(has_dot(with_dot))
	println(has_dot(without_dot))
}
