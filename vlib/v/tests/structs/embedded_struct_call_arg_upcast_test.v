// Like V1, a function call may pass a struct, or a reference to one, where one of
// its (possibly nested) embedded structs is expected.
struct UpcastNode {
	name string
}

struct UpcastStyled {
	UpcastNode
	style int
}

struct UpcastMetrics {
	UpcastStyled
	margin int
}

@[heap]
struct UpcastBox {
	UpcastMetrics
	width int
}

struct UpcastTag {
	id int
}

struct UpcastTagged {
	UpcastTag
	UpcastNode
}

fn upcast_node_name(n &UpcastNode) string {
	return n.name
}

fn upcast_style(n &UpcastStyled) int {
	return n.style + upcast_node_name(n).len
}

fn upcast_node_value_name(n UpcastNode) string {
	return n.name
}

fn (b &UpcastBox) containing_block() ?&UpcastBox {
	return b
}

fn test_embedded_struct_reference_call_arg() {
	b := &UpcastBox{
		UpcastMetrics: UpcastMetrics{
			UpcastStyled: UpcastStyled{
				UpcastNode: UpcastNode{
					name: 'box'
				}
				style:      7
			}
		}
		width:         10
	}
	containing := b.containing_block() or { panic('no containing block') }
	assert upcast_node_name(containing) == 'box'
	assert upcast_style(containing) == 10
	assert upcast_node_value_name(*b) == 'box'
}

fn test_second_embedded_struct_call_arg() {
	tagged := &UpcastTagged{
		UpcastTag:  UpcastTag{
			id: 3
		}
		UpcastNode: UpcastNode{
			name: 'second'
		}
	}
	assert upcast_node_name(tagged) == 'second'
}
