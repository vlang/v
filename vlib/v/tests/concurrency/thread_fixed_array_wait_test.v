type Triple = [3]u32

struct Holder {
mut:
	ids [3]u32
}

fn ids(base u32) [3]u32 {
	return [base, base + 1, base + 2]!
}

fn triple() Triple {
	return Triple([u32(7), 8, 9]!)
}

fn sum(values [3]u32) u32 {
	return values[0] + values[1] + values[2]
}

fn joined(base u32) [3]u32 {
	handle := spawn ids(base)
	return handle.wait()
}

fn test_wait_assigns_a_fixed_array_result() {
	mut out := [3]u32{}
	handle := spawn ids(1)
	out = handle.wait()
	assert out == [u32(1), 2, 3]!
	declared_handle := spawn ids(4)
	declared := declared_handle.wait()
	assert declared == [u32(4), 5, 6]!
}

fn test_wait_result_is_used_as_a_fixed_array_value() {
	assert joined(10) == [u32(10), 11, 12]!
	arg := spawn ids(1)
	assert sum(arg.wait()) == 6
	indexed := spawn ids(20)
	assert indexed.wait()[2] == 22
	mut holder := Holder{}
	field := spawn ids(30)
	holder.ids = field.wait()
	assert holder.ids == [u32(30), 31, 32]!
	compared := spawn ids(70)
	assert compared.wait() == [u32(70), 71, 72]!
}

fn test_wait_returns_an_aliased_fixed_array() {
	handle := spawn triple()
	assert handle.wait() == Triple([u32(7), 8, 9]!)
}

fn test_wait_on_an_array_of_fixed_array_threads() {
	mut threads := []thread [3]u32{}
	threads << spawn ids(40)
	threads << spawn ids(50)
	assert threads.wait() == [[u32(40), 41, 42]!, [u32(50), 51, 52]!]
}
