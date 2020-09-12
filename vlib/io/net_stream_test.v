import net
import io

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

	mut nos := io.new_net_output_stream(s)
	mut nis := io.new_net_input_stream(c)

	//bytes
	a := [byte(76), 7, 43, 5]
	nos.write_bytes(a) or { assert false }
	c_a := nis.read_bytes(4)
	assert a == c_a

	//u16s
	b := [u16(546), 3434, 33]
	nos.write_u16s(b) or { assert false }
	c_b := nis.read_u16s(3)
	assert b == c_b

	//u32s
	d := [u32(10), 34324, 454, 34, 352]
	nos.write_u32s(d) or { assert false }
	c_d := nis.read_u32s(5)
	assert d == c_d

	//u64s
	e := [u64(32542), 23213, 23244353, 324534534]
	nos.write_u64s(e) or { assert false }
	c_e := nis.read_u64s(4)
	assert e == c_e

	//i8s
	f := [i8(20), 40, 5, 10]
	nos.write_i8s(f) or { assert false }
	c_f := nis.read_i8s(4)
	assert f == c_f

	//i16s
	g := [i16(3434), 3242, 655, 445, 23]
	nos.write_i16s(g) or { assert false }
	c_g := nis.read_i16s(5)
	assert g == c_g

	//i32s
	h := [342, 32424, 565, 343, 7676, 34]
	nos.write_ints(h) or { assert false }
	c_h := nis.read_ints(6)
	assert h == c_h
	//i64s
	i := [i64(354345), 45435564, 54645654, 3242342]
	nos.write_i64s(i) or { assert false }
	c_i := nis.read_i64s(4)
	assert i  == c_i
}

fn test_primitives() {
	_, c, s := setup()

	mut nos := io.new_net_output_stream(s)
	mut nis := io.new_net_input_stream(c)

	// bytes
	a := byte(45)
	nos.write_byte(a) or { assert false }
	c_a := nis.read_byte()
	assert a == c_a

	// u16
	b := u16(345)
	nos.write_u16(b) or { assert false }
	c_b := nis.read_u16()
	assert b == c_b

	// u32
	d := u32(353453)
	nos.write_u32(d) or { assert false }
	c_d := nis.read_u32()
	assert d == c_d

	// u64
	e := u64(43645645654)
	nos.write_u64(e) or { assert false }
	c_e := nis.read_u64()
	assert e == c_e

	// i8
	f := i8(43)
	nos.write_i8(f) or { assert false }
	c_f := nis.read_i8()
	assert f == c_f

	// i16
	g := i16(676)
	nos.write_i16(g) or { assert false }
	c_g := nis.read_i16()
	assert g == c_g

	// int
	h := 4543565
	nos.write_int(h) or { assert false }
	c_h := nis.read_int()
	assert h == c_h

	// i64
	i := i64(4343654654654)
	nos.write_i64(i) or { assert false }
	c_i := nis.read_i64()
	assert i == c_i
}

fn test_floats() {
	_, c, s := setup()

	mut nos := io.new_net_output_stream(s)
	mut nis := io.new_net_input_stream(c)

	a := f32(7.5)
	nos.write_f32(a) or { assert false }
	c_a := nis.read_f32()
	assert a == c_a

	b := f64(43587349857834579834.45435435)
	nos.write_f64(b) or { assert false }
	c_b := nis.read_f64()
	assert b == c_b

	d := [f32(7.3), 3.45, 546.3, 4545.3]
	nos.write_f32s(d) or { assert false }
	c_d := nis.read_f32s(4)
	assert d == c_d

	e := [f64(345324324.3242342), 3243242.342, 344564.343242423, 43543.43534, 34234.34324]
	nos.write_f64s(e) or { assert false }
	c_e := nis.read_f64s(5)
	assert e == c_e
}

fn test_string() {
	_, c, s := setup()

	mut nos := io.new_net_output_stream(s)
	mut nis := io.new_net_input_stream(c)

	a := 'hello'
	nos.write_string(a) or { assert false }
	c_a := nis.read_string(5)
	assert a == c_a
}

fn test_struct() {
	_, c, s := setup()

	mut nos := io.new_net_output_stream(s)
	mut nis := io.new_net_input_stream(c)

	a := Test{
		a: 1
		b: 2.0
		c: 'test'
	}
	nos.write_struct(a, sizeof(Test)) or { assert false }
	got := &Test(nis.read_struct(sizeof(Test)))
	de_ref := *got
	assert a.a == de_ref.a && a.b == de_ref.b && a.c == de_ref.c
}
