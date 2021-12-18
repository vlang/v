struct Array<T> {
pub mut:
	elements []T
}

struct String {
	str string
}

interface IObject {
	equals(IObject) bool
}

pub fn (s1 String) equals(s2 IObject) bool {
	if s2 is String {
		return s1.str == s2.str
	}
	return false
}

pub fn (mut m Array<T>) contains(e T) bool {
	for mut element in m.elements {
		if element.equals(e) {
			return true
		}
	}
	return false
}

fn test_generic_array_of_interface_method_call() {
	s := String{'hello'}
	mut a := Array<IObject>{[s]}
	ret := a.contains(IObject(s))
	println(ret)
	assert ret
}
