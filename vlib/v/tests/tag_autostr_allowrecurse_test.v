@[autostr: allowrecurse]
struct Node {
	a &Node = unsafe { nil }
	b &Node = unsafe { nil }
}

fn test_stringifying_a_value_of_a_struct_tagged_with_autostr_allowrecurse() {
	abc := &Node{
		a: &Node{
			b: &Node{}
		}
		b: &Node{
			a: &Node{}
		}
	}
	// println(abc)
	s := abc.str()
	assert !s.contains('&<circular>')
	assert s.contains('a: &Node{')
	assert s.contains('a: &nil')
	assert s.contains('b: &Node{')
	assert s.contains('b: &nil')
}
