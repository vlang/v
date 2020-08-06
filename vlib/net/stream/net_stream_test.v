import net
import net.stream

struct Test {
	a int
	b f32
	c string
}

fn setup() (net.Socket, net.Socket, net.Socket) {
	server := net.listen(0) or { panic(err)	}
	server_port := server.get_port()
	client := net.dial('127.0.0.1', server_port) or { panic(err) }
	socket := server.accept() or { panic(err) }
	return server, client, socket
}

fn test_arrays() {
	_, c, s := setup()

	mut nos := stream.new_net_output_stream(s)
	mut nis := stream.new_net_input_stream(c)

	//bytes
	/*a := [byte(76), 7, 43, 5]
	nos.write_bytes(a) or { assert false }
	c_a := nis.get_bytes(4)
	assert a == c_a
*/
	//u16s
	b := [u16(546), 3434, 33]
	nos.write_u16s(b) or { assert false }
	c_b := nis.get_u16s(3)
	assert b == c_b
/*
	//u32s
	d := [u32(10), 34324, 454, 34, 352]
	nos.write_u32s(d) or { assert false }
	c_d := nis.get_u32s(5)
	assert d == c_d

	//u64s
	e := [u64(32542), 23213, 23244353, 324534534]
	nos.write_u64s(e) or { assert false }
	c_e := nis.get_u64s(4)
	assert e == c_e

	//i8s
*/
	//i16s

	//i32s

	//i64s
}
