pub struct Embed {
	foo int
}

pub interface Node {
	id u32
mut:
	parent &Node
}

pub struct Item {
	Embed
pub:
	id u32
mut:
	parent &Node = unsafe { nil }
}

fn test_struct_with_both_an_embed_and_a_pointer_to_interface_value_fields__initialises_properly() {
	i := Item{}
	si := i.str()
	dump(si)
	assert si.contains('Item{')
	assert si.contains('Embed: Embed{')
	assert si.contains('foo: 0')
	assert si.contains('id: 0')
	assert si.contains('parent: &nil')
	assert si.contains('}')
}
