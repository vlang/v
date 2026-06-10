module main

fn to_i64_direct(x u64) i64 {
	return i64(x)
}

fn to_i64_if_expr(x u64, neg bool) i64 {
	return if neg { -i64(x) } else { i64(x) }
}

fn to_i64_result_direct(x u64) !i64 {
	return i64(x)
}

fn to_i64_result_if_expr(x u64, neg bool) !i64 {
	return if neg { -i64(x) } else { i64(x) }
}

fn to_i64_result_if_var(x u64, neg bool) !i64 {
	y := if neg { -i64(x) } else { i64(x) }
	return y
}

fn main() {
	println(to_i64_direct(6))
	println(to_i64_if_expr(6, false))
	println(to_i64_result_direct(6) or { 99 })
	println(to_i64_result_if_expr(6, false) or { 99 })
	println(to_i64_result_if_var(6, false) or { 99 })
}
