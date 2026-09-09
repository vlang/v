const used_const = 1
const used_by_pub_const = 3
const const_used_by_unused_fn = 2
const unused_const = 4

fn used_fn() int {
	return used_const
}

fn helper_used_by_pub() int {
	return used_by_pub_const
}

fn unused_fn() int {
	return const_used_by_unused_fn
}

pub fn pub_fn() int {
	return helper_used_by_pub()
}

$if never_defined ? {
	const skipped_const = 4

	fn skipped_fn() int {
		return skipped_const
	}
}

fn main() {
	println(used_fn())
}
