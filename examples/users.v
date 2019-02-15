module main

import json // V will automatically insert missing imports (like the goimports tool)
import http

// Right now V requires all consts to be upper case.
// I'm still not certain about this.
const API_URL = 'https://vlang.io/users.json'

// [json] attribute makes the compiler generate json.encode and json.decode
// functions for this type. This results in better performance, since no
// reflection is needed.
[json] 
struct User {
	name          string // V will automatically format and align your code.
	age           int    // No need to use an additional tool.
	is_registered bool
}

fn main() {
	// `http.get()` returns an optional string.
	// V optionals combine the features of Rust's Option<T> and Result<T>.
	// We must unwrap all optionals with `or`, otherwise V will complain.
	s := http.get(API_URL) or {
		// `err` is a reserved variable (not a global) that
		// contains an error message if there is one
		eprintln('Failed to fetch "users.json": $err')
		// `or` blocks must end with `return`, `break`, or `continue`
		return

	}
	// Types can be passed as arguments
	users := json.decode([]User, s)
	// Encoding JSON doesn't require a type, since V knows what type
	// `user` has
	println(json.encode(users))
	// Please note the difference between V and Go:
	// when there's only one variable, it's a value, not an index.
	for user in users {
		println('$user.name: $user.age')
	}
	// `for` loop has an alternative form when an index is required:
	for i, user in users {
		println('$i) $user')
		if !user.can_register() {
			// V allows both ' and " to denote strings
			// However, for consistency V will replace " with '
			// unless the string contains an apostrophe.
			println("Can't register")
		}
	}
}

// The method declration is the same as in Go.
// There is one big difference. Here `u` can be either passed by value (User)
// or by reference (&User). The compiler will make the right decision
// depending on the size of the User struct. You no longer have to rember
// which one to use. It works here because `u` can't be modified (it's not
// marked as `mut`)
fn (u User) can_register() bool {
	return u.age >= 16
}


// Here `u` can be modified and it will always be a reference.
fn (u mut User) register() {
	u.is_registered = true
}




