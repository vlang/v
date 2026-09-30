module main

import amod

// `amod` spells `foo` only in a skipped branch, which cannot use this private `foo`.
fn foo() int {
	return 1
}

fn main() {
	println(amod.value())
}
