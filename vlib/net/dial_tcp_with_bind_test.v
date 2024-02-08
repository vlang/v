import net

fn test_bind() {
	$if !network ? {
		return
	}
	eprintln(@LOCATION)
	mut conn := net.dial_tcp_with_bind('vlang.io:80', '0.0.0.0:0')!
	dump(conn)
	conn.close()!
	eprintln(@LOCATION)
}
