module main

pub fn TopLevelDomain.cases() []TopLevelDomain {
	mut cases := []TopLevelDomain{}

	$for top_level_domain in TopLevelDomain.values {
		case := TopLevelDomain.from(top_level_domain.name) or { panic(err) }
		cases << case
	}
	return cases
}
