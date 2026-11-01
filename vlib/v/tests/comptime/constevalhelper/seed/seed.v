module seed

@[comptime]
fn wide_value() int {
	return 256
}

pub const value = wide_value()
