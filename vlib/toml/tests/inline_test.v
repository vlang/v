import toml

struct Address {
	street string
	city   string
}

fn test_inline() {
	a := Address{'Elm Street', 'Springwood'}

	mut mp := map[string]toml.Any{}
	mp['street'] = toml.Any(a.street)
	mp['city'] = toml.Any(a.city)

	assert mp.to_inline_toml() == '{ street = "Elm Street", city = "Springwood" }'
}
