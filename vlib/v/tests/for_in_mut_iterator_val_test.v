const (
	packets = {
		0: &Packet{
			pid: 2
		}
		1: &Packet{
			pid: 1
		}
	}
)

struct Packet {
	pid        int
	handle     fn () string
	restricted bool
}

struct Reader {
	foo int
	bar string
mut:
	index int
}

fn (mut p Reader) next() ?&Packet {
	if p.index + 1 > packets.len {
		return error('')
	}
	if p.index !in packets {
		return error('')
	}

	p.index++
	return packets[p.index - 1]
}

fn test_for_in_mut_interator_val() {
	r := Reader{}
	mut rets := []string{}

	for mut packet in r {
		println(packet.pid)
		rets << '${packet.pid}'
	}

	println(rets)
	assert rets.len == 2
	assert rets[0] == '2'
	assert rets[1] == '1'
}
