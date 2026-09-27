enum Alignment {
	@none = -10
	left
	@type
}

struct Layout {
	alignment Alignment = .@none
}

fn alignment_name(value Alignment) string {
	return match value {
		.@none { 'unset' }
		.left { 'left' }
		.@type { 'type' }
	}
}

fn alignment_score(value Alignment) int {
	match value {
		.@none { return 0 }
		.left { return 1 }
		.@type { return 2 }
	}
}

fn test_escaped_enum_members_keep_their_type_and_value() {
	assert Layout{}.alignment == .@none
	assert int(Alignment.@none) == -10
	mut value := Alignment.left
	value = .@type
	assert value == Alignment.@type
	assert alignment_name(value) == 'type'
	value = .@none
	assert alignment_name(value) == 'unset'
	assert [Alignment.@none, .left, .@type].map(alignment_name(it)) == ['unset', 'left', 'type']
	assert alignment_score(.@none) == 0
	assert alignment_score(.@type) == 2
}
