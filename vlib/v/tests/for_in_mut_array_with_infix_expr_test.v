import net

fn test_for_in_mut_array_with_infix_expr() {
	ignore := net.TcpConn{}
	mut array := []&net.TcpConn{}

	for mut socket in array {
		if ignore != socket {
			println('not eq')
		}
	}
	assert true
}
