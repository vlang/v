// Function signatures can be declared as types:

type Filter = fn (string) string

// Functions can accept function types as arguments:

fn filter(s string, f Filter) string {
	return f(s)
}

// Declare a function with a matching signature:

fn uppercase(s string) string {
	return s.to_upper()
}

fn main() {
	// A function can be assigned to a matching type:

	my_filter := Filter(uppercase)

	// You don't strictly need the `Filter` cast - it's only used
	// here to illustrate how these types are compatible.

	// All of the following prints "HELLO WORLD!":

	println(filter('Hello World!', my_filter))
	println(filter('Hello World!', uppercase))
	println(filter('Hello World!', fn (s string) string {
		return s.to_upper()
	}))
}
