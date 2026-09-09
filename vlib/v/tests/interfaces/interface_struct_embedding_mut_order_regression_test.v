struct EmbeddedBase {
	next ?&EmbeddedBase
}

interface EmbeddedNode {
mut:
	is_valid() bool
}

struct EmbeddedState {
mut:
	valid bool
}

struct EmbeddedAccount {
	EmbeddedBase
	EmbeddedState
}

fn (mut self EmbeddedState) is_valid() bool {
	return self.valid
}

fn test_interface_cast_with_mut_method_on_later_embedded_field() {
	mut acc := EmbeddedAccount{}
	mut node := EmbeddedNode(acc)
	assert node.is_valid() == acc.valid
}
