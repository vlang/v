struct Node {
mut:
	tag     string
	content []Content
}

struct Text {
}

type Content = Node | Text

fn test_push_prepend_insert() {
	mut body := Node{}
	body.content << Node{
		tag: 'a'
	}
	body.content.prepend(Node{ tag: 'b' })
	body.content.insert(1, Node{ tag: 'c' })
	assert body.content.len == 3
}
