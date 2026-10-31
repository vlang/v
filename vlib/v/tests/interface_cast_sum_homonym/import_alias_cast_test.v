import ifacepkt as ip
import sumpkt as sp

fn test_interface_cast_through_import_alias() {
	p := ip.Packet(&ip.Ping{})
	assert p.id() == 1
	assert p is &ip.Ping
	s := sp.Packet(sp.Goodbye{})
	assert s is sp.Goodbye
}
