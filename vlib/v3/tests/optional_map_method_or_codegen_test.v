struct MapProvider {}

fn (p &MapProvider) values() ?map[string]string {
	return {
		'answer': '42'
	}
}

struct MapOwner {
	provider &MapProvider
}

fn test_optional_map_method_or_uses_declared_value_type() {
	provider := &MapProvider{}
	owner := MapOwner{
		provider: provider
	}
	values := owner.provider.values() or { return }
	assert values['answer'] == '42'
}
