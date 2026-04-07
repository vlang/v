struct Info[T] {
	data T
}

fn get_info[T](res Info[T]) string {
	return '${res}'
}

fn test_generic_struct_to_string() {
	mut ret := get_info(Info[bool]{true})
	println(ret)
	assert ret.contains('data: true')

	ret = get_info(Info[int]{123})
	println(ret)
	assert ret.contains('data: 123')

	ret = get_info(Info[f32]{f32(2.2)})
	println(ret)
	assert ret.contains('data: 2.2')

	ret = get_info(Info[f64]{2.2})
	println(ret)
	assert ret.contains('data: 2.2')

	ret = get_info(Info[string]{'aaa'})
	println(ret)
	assert ret.contains("data: 'aaa'")

	ret = get_info(Info[u64]{u64(234)})
	println(ret)
	assert ret.contains('data: 234')
}

@[heap]
struct RefNode[T] {
pub mut:
	value T
}

struct RefList[T] {
pub mut:
	node ?&RefNode[T]
}

fn (mut l RefList[T]) add(value T) {
	l.node = &RefNode[T]{
		value: value
	}
}

fn (l RefList[T]) array() []T {
	mut a := []T{}
	mut n := l.node or { return a }
	a << n.value
	return a
}

@[heap]
struct ReferencedValue {
	n int
}

fn test_generic_struct_to_string_with_reference_values() {
	mut list := RefList[&ReferencedValue]{}
	list.add(&ReferencedValue{
		n: 5
	})
	assert list.array()[0].n == 5
	assert '${list}' == 'RefList[&ReferencedValue]{
    node: &Option(RefNode[&ReferencedValue]{
        value: &ReferencedValue{
            n: 5
        }
    })
}'
	assert '${list.array()}' == '[&ReferencedValue{
    n: 5
}]'
}
