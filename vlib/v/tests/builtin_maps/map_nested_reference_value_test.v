import net

fn accept_mut_tcp_conn(mut conn net.TcpConn) bool {
	return true
}

fn test_nested_map_tcpconn_reference_value_for_in_mut_type() {
	mut con_list := map[string]map[string]&net.TcpConn{}
	conn := &net.TcpConn{}
	con_list['foo'] = {
		'bar': conn
	}

	for _, mut connection in unsafe { con_list['foo'] } {
		assert typeof(connection).name == '&net.TcpConn'
		assert accept_mut_tcp_conn(mut connection)
		assert voidptr(connection) == voidptr(conn)
	}
}
