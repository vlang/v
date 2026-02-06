type ReleaseTarget = string

const production = ReleaseTarget('production')
const testing = ReleaseTarget('testing')

fn test_alias_string_match() {
	target := match ReleaseTarget('testing') {
		production { production }
		testing { testing }
		else { panic(error('Invalid target')) }
	}

	assert target == ReleaseTarget('testing')
	assert target == testing
}
