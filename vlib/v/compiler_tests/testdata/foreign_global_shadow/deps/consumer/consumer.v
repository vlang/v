module consumer

pub struct Item {
pub:
	count int
}

// local_array returns a function-local array with a foreign global's short name.
pub fn local_array() []int {
	mut sources := []int{}
	sources << 3
	sources << 7
	return sources
}

// parameter iterates an array parameter whose name also belongs to a foreign global.
pub fn parameter(sources []int) int {
	mut total := 0
	for source in sources { total += source }
	return total
}

// loop_binding uses the foreign global's name for an iteration binding.
pub fn loop_binding() int {
	mut total := 0
	for sources in [1, 2, 3] { total += sources }
	return total
}

// closure returns an anonymous function whose parameter shares a foreign global's name.
pub fn closure() fn ([]int) int {
	return fn (sources []int) int { return sources[0] }
}

// callback invokes a function parameter with a foreign global's short name.
pub fn callback(sources fn (int) int) int { return sources(4) }

// value reads the receiver binding rather than another module's global.
pub fn (sources Item) value() int { return sources.count }
