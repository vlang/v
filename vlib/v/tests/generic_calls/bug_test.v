// import faker { Faker, Currency }
module main

fn test_main() {
	mut fake := Faker{}

	code := fake.currency_code()
	codes := Currency.cases().map(it.to_code())
	// dump(code)
	// dump(codes)
	assert codes.contains(code)
}

fn test_it_generates_currency_code() {
	mut fake := Faker{}

	code := fake.currency_code()
	codes := Currency.cases().map(it.to_code())

	assert codes.contains(code)
}
