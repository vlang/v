module main

pub fn Currency.cases() []Currency {
	mut cases := []Currency{}

	$for currency in Currency.values {
		case := Currency.from(currency.name) or { panic(err) }
		cases << case
	}
	return cases
}
