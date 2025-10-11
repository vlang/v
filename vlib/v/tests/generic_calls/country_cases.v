module main

pub fn Country.cases() []Country {
	mut cases := []Country{}

	$for country in Country.values {
		case := Country.from(country.name) or { panic(err) }
		cases << case
	}
	return cases
}
