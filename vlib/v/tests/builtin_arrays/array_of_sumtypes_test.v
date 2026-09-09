struct Node {
mut:
	tag     string
	content []Content
}

struct Text {
}

type Content = Node | Text

struct Left {
	error string
}

struct Right {
	value int
}

type Either = Left | Right

fn test_push_prepend_insert() {
	mut body := Node{}
	body.content << Node{
		tag: 'a'
	}
	body.content.prepend(Node{ tag: 'b' })
	body.content.insert(1, Node{ tag: 'c' })
	assert body.content.len == 3
}

fn test_nested_selector_access_on_sumtype_array_elements() {
	mut body := Node{
		tag: 'body'
	}
	body.content << Node{
		tag: 'p'
	}
	body.content[0].content << Node{
		tag: 'i'
	}
	body.content[0].content[0].content << Node{
		tag: 'j'
	}
	body.content[0].content[0].content[0].content << Node{
		tag: 'k'
	}
	body.content[0].content[0].content[0].content[0].content << Text{}
	assert ((body.content[0] as Node).content[0] as Node).content.len == 1
}

fn test_selector_access_on_indexed_sumtype_variants() {
	items := [Either(Left{
		error: 'boom'
	})]
	assert items[0].error == 'boom'
}
