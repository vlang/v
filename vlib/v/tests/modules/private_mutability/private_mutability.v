module private_mutability

pub struct Counter {
mut:
	hidden int
pub:
	label string
}

fn increment_hidden(mut counter Counter) {
	counter.hidden++
}

fn (mut counter Counter) bump_hidden() {
	counter.hidden++
}

pub fn (mut counter Counter) bump_hidden_via_method() {
	counter.bump_hidden()
}

pub fn (mut counter Counter) bump_hidden_via_helper() {
	increment_hidden(mut counter)
}

pub fn (counter Counter) label_text() string {
	return counter.label
}
