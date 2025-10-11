module main

fn test_main() {
	mut fake := Faker{}

	code := fake.currency_code()
	codes := Currency.cases().map(it.to_code())
	assert codes.contains(code)
}
