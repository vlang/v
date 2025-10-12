module main

pub fn (mut this Faker) currency_code() string {
	return this.random_element(Currency.cases()).to_code()
}
