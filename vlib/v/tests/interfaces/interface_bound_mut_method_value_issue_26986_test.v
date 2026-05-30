struct ProtoReader {
	value string
}

fn (pr &ProtoReader) read() string {
	return pr.value
}

struct Conn {
	rd &ProtoReader
}

fn (c &Conn) with_proto_reader(f fn (rd &ProtoReader) !) ! {
	f(c.rd)!
}

interface Cmder {
mut:
	read_reply(rd &ProtoReader) !
}

struct SomeCmd {
mut:
	value string
}

fn (mut sc SomeCmd) read_reply(rd &ProtoReader) ! {
	sc.value = rd.read()
}

fn process_via_interface_pointer(mut cmd Cmder) ! {
	c := &Conn{
		rd: &ProtoReader{
			value: 'from pointer'
		}
	}
	mut cmd_ref := unsafe { &cmd }
	c.with_proto_reader(cmd_ref.read_reply)!
}

fn process_via_interface_value(mut cmd Cmder) ! {
	c := &Conn{
		rd: &ProtoReader{
			value: 'from value'
		}
	}
	c.with_proto_reader(cmd.read_reply)!
}

fn test_bound_mut_interface_method_value_mutates_underlying_object() ! {
	mut cmd := &SomeCmd{}
	process_via_interface_pointer(mut cmd)!
	assert cmd.value == 'from pointer'
	process_via_interface_value(mut cmd)!
	assert cmd.value == 'from value'
}
