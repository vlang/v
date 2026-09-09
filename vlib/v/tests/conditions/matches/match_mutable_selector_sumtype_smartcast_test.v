struct MatchMutableSelectorStandard {}

struct MatchMutableSelectorCustom {
mut:
	name string
}

type MatchMutableSelectorIdentifier = MatchMutableSelectorStandard | MatchMutableSelectorCustom

struct MatchMutableSelectorOwner {
mut:
	identifier MatchMutableSelectorIdentifier
}

fn (mut c MatchMutableSelectorCustom) rename_to(value string) string {
	c.name = value
	return c.name
}

fn (mut o MatchMutableSelectorOwner) update_identifier(value string) !string {
	match o.identifier {
		MatchMutableSelectorStandard {
			return error('not allowed')
		}
		MatchMutableSelectorCustom {
			return o.identifier.rename_to(value)
		}
	}
}

fn (o MatchMutableSelectorOwner) identifier_name() !string {
	match o.identifier {
		MatchMutableSelectorStandard {
			return error('not allowed')
		}
		MatchMutableSelectorCustom {
			return o.identifier.name
		}
	}
}

fn test_match_smartcast_on_mutable_selector_without_match_mut() {
	mut owner := MatchMutableSelectorOwner{
		identifier: MatchMutableSelectorCustom{
			name: 'before'
		}
	}
	assert owner.update_identifier('after')! == 'after'
	assert owner.identifier_name()! == 'after'
}
