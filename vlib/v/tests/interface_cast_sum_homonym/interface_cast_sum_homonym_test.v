import ifacepkt
import sumpkt as _

fn test_unqualified_interface_cast_inside_its_module() {
	assert ifacepkt.from_closure().id() == 1
	assert ifacepkt.from_value(7).id() == 7
	assert ifacepkt.from_local() == 1
}

fn test_qualified_interface_cast_from_main() {
	p := ifacepkt.Packet(&ifacepkt.Ping{})
	assert p.id() == 1
	assert p is &ifacepkt.Ping
	q := ifacepkt.Packet(ifacepkt.Pong{3})
	assert q.id() == 3
}

fn test_interface_without_homonym() {
	assert ifacepkt.frame().id() == 1
}
