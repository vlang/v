import ifacepkt { Packet, Ping, Pong }
import sumpkt as _

fn test_interface_cast_through_selective_import() {
	p := Packet(&Ping{})
	assert p.id() == 1
	q := Packet(Pong{4})
	assert q.id() == 4
}
